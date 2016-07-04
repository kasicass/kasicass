using System;
using System.ComponentModel;
using System.Windows;
using System.Windows.Media;
using System.Windows.Data;
using System.Windows.Markup;

namespace Illusion.Views
{
    /// <summary>
    /// Custom <see cref="MarkupExtension"/> used to provide <see cref="System.Windows.Controls.Image"/>, 
    /// it internally use <see cref="IResourceService"/> to query the image source from name key.
    /// </summary>
    /// <example>
    /// public class DataModel
    /// {
    ///     public string ImageSource {get;set;}
    /// }
    /// 
    /// Window window = new Window() { DataContext = new DataModel() };
    /// Using ImgaeBinding to set Icon In Window.xaml:  <Window Icon="{ImageBinding ImageSource}" />
    /// </example>
	[MarkupExtensionReturnType(typeof(BindingExpression))]
	public class ImageBindingExtension : MarkupExtension
	{
		public ImageBindingExtension(string path)
			: this()
		{
			Path = path;
		}

		public ImageBindingExtension()
		{
		}

		[ConstructorArgument("path")]
		public string Path
		{
			get;
			set;
		}


		public override object ProvideValue(IServiceProvider serviceProvider)
		{
			IProvideValueTarget target = serviceProvider.GetService(typeof(IProvideValueTarget)) as IProvideValueTarget;

			if (target.TargetObject is Setter)
			{
				return new Binding(Path) { Converter = ImgaeResourceConverter.Default };
			}
			else
			{
				Binding binding = new Binding(Path) { Converter = ImgaeResourceConverter.Default };
				return binding.ProvideValue(serviceProvider);
			}
		}
	}

    /// <summary>
    /// Custom <see cref="MarkupExtension"/> used to provide <see cref="System.Windows.Controls.Image"/>, 
    /// it internally use <see cref="IResourceService"/> to query the image source from name key.
    /// </summary>
    /// <example>
    /// public class DataModel
    /// {
    ///     public string ImageSource {get;set;}
    /// }
    /// 
    /// Image image = new Imgae() { DataContext = new DataModel() };
    /// Using ImgaeBinding to set <see cref="ImageSource"/> to Image:  <Image Source="{ImageSourceBinding ImageSource}" />
    /// </example>
	[MarkupExtensionReturnType(typeof(BindingExpression))]
	public class ImageSourceBindingExtension : MarkupExtension
	{
		public ImageSourceBindingExtension(string path)
			: this()
		{
			Path = path;
		}

		public ImageSourceBindingExtension()
		{
		}

		[ConstructorArgument("path")]
		public string Path
		{
			get;
			set;
		}


		public override object ProvideValue(IServiceProvider serviceProvider)
		{
			IProvideValueTarget target = serviceProvider.GetService(typeof(IProvideValueTarget)) as IProvideValueTarget;

			if (target.TargetObject is Setter)
			{
				return new Binding(Path) { Converter = ImgaeSourceResourceConverter.Default };
			}
			else
			{
				Binding binding = new Binding(Path) { Converter = ImgaeSourceResourceConverter.Default };
				return binding.ProvideValue(serviceProvider);
			}
		}
	}
    /// <summary>
    /// Custom <see cref="MarkupExtension"/> used to provide <see cref="ImageSource"/>, 
    /// it internally use <see cref="IResourceService"/> to query the image source from name key.
    /// </summary>
	[MarkupExtensionReturnType(typeof(ImageSource))]
	public class ImageResourceExtension : MarkupExtension
	{
		static ILog Log = LogManager.GetLog(typeof(ImageResourceExtension));

		public ImageResourceExtension(string key)
			: this()
		{
			Key = key;
		}

		public ImageResourceExtension()
		{
		}

		[ConstructorArgument("key")]
		public string Key
		{
			get;
			set;
		}

		public override object ProvideValue(IServiceProvider serviceProvider)
		{
			return IoC.Get<IResourceService>().GetImage(Key);
		}
	}
}
