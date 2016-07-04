using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.ComponentModel.Composition;

using Illusion;

namespace YoZuo.Workbench
{
	[MenuPart(BaseMenu = WorkbenchName.PresentationGroup)]
	public class DefaultPresentationPart : MenuPart
	{
		public DefaultPresentationPart()
			: base(WorkbenchName.DefaultPresentationPart)
		{
			Icon = "PinkLarge.png";
		}

		public override void Execute()
		{
			IoC.Get<IPresentationService>().ChangePresentation(Name);

			Application.Current.ShutdownMode = ShutdownMode.OnExplicitShutdown;
			var window = Application.Current.MainWindow;
			window.Close();
			IoC.Get<IWindowManager>().ShowWindow(window.DataContext);
			Application.Current.ShutdownMode = ShutdownMode.OnMainWindowClose;
		}
	}
}
