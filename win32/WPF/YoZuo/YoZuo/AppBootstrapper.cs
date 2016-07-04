using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.ComponentModel.Composition.Hosting;
using System.ComponentModel.Composition.Primitives;
using System.Linq;
using System.Windows;
using System.Reflection;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo
{
	public class AppBootstrapper : Bootstrapper<IShell>
	{
		CompositionContainer container;

		/// <summary>
		/// By default, we are configured to use MEF
		/// </summary>
		protected override void Configure()
		{
			//1. First init catalog for start up.
			var catalog = new AggregateCatalog(
				AssemblySource.Instance.Select(x => new AssemblyCatalog(x)).OfType<ComposablePartCatalog>()
				);

			catalog.Catalogs.Add(CreateDirectoryCatalog(Environment.CurrentDirectory + "/Presentation"));
			catalog.Catalogs.Add(CreateDirectoryCatalog(Environment.CurrentDirectory + "/Workbench"));

			container = new CompositionContainer(catalog);
			var batch = new CompositionBatch();

			//2. Inject export values to IoC container and resolve their dependencies if needed.
			var windowManager = new YoZuoWindowManager();
			var presentationService = new PresentationService();
			var resourceService = new ResourceService();

			batch.AddExportedValue<IEventAggregator>(new EventAggregator());
			batch.AddExportedValue<IResourceService>(resourceService);
			batch.AddExportedValue<IWindowManager>(windowManager);
			batch.AddExportedValue<IYoZuoWindowManager>(windowManager);
			batch.AddExportedValue<IPresentationService>(presentationService);
			batch.AddExportedValue(container);
			batch.AddExportedValue(catalog);

			container.Compose(batch);
			container.ComposeParts(resourceService);
			container.ComposeParts(presentationService);

			//3. Init the startup environment : Presentation -> Ribbon ; Language -> Chinese
			presentationService.ChangePresentation("PART_OPTION_PRESENTATION_RIBBON");
			resourceService.ChangeLanguage("zh-cn");
		}

		private DirectoryCatalog CreateDirectoryCatalog(string path)
		{
			var catalog = new DirectoryCatalog(path, "YoZuo*.dll");
			AssemblySource.Instance.AddRange(catalog.LoadedFiles.Select(file => Assembly.LoadFile(file)));

			return catalog;
		}

		protected override IEnumerable<Assembly> SelectAssemblies()
		{
			return new Assembly[] { Assembly.GetEntryAssembly() };
		}

		protected override void StartRuntime()
		{
			Execute.InitializeWithDispatcher();
			AssemblySource.Instance.AddRange(SelectAssemblies());

			Application = Application.Current;
			PrepareApplication();

			//Reorder to put configure at the last step.
			IoC.GetInstance = GetInstance;
			IoC.GetAllInstances = GetAllInstances;
			IoC.BuildUp = BuildUp;
			Configure();
		}

		protected override object GetInstance(Type serviceType, string key)
		{
			string contract = string.IsNullOrEmpty(key) ? AttributedModelServices.GetContractName(serviceType) : key;
			var exports = container.GetExportedValues<object>(contract);

			if (exports.Any())
				return exports.First();

			throw new Exception(string.Format("Could not locate any instances of contract {0}.", contract));
		}

		protected override IEnumerable<object> GetAllInstances(Type serviceType)
		{
			return container.GetExportedValues<object>(AttributedModelServices.GetContractName(serviceType));
		}

		protected override void BuildUp(object instance)
		{
			container.SatisfyImportsOnce(instance);
		}
	}
}
