using System;
using System.ComponentModel;
using System.Windows;
using System.Windows.Data;
using System.Windows.Markup;

namespace Illusion.Views
{
    /// <summary>
    /// Custom <see cref="MarkupExtension"/> used to provide string, 
    /// it internally use <see cref="IResourceService"/> to query the string source from name key.
    /// </summary>
	[MarkupExtensionReturnType(typeof(BindingExpression))]
	public class StringBindingExtension : StringResourceExtension
	{
		FrameworkElement element = new FrameworkElement();

		public StringBindingExtension(string path)
			: this()
		{
			Path = path;
		}

		public StringBindingExtension()
			: base()
		{
			DependencyPropertyDescriptor dpd = DependencyPropertyDescriptor.FromProperty(FrameworkElement.TagProperty, typeof(FrameworkElement));
			dpd.AddValueChanged(element, new EventHandler((s, e) =>
				{
					Key = element.Tag != null ? element.Tag.ToString() : null;
					NotifyValueChanged();
				}));
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

			if (target.TargetObject is DependencyObject)
			{
				Binding bind = new Binding(string.Format("DataContext.{0}", Path));
				bind.Source = target.TargetObject;
				element.SetBinding(FrameworkElement.TagProperty, bind);

				Binding binding = new Binding("Value") { Source = this, Mode = BindingMode.OneWay };
				return binding.ProvideValue(serviceProvider);
			}
			else
			{
				return this;
			}
		}
	}

    /// <summary>
    /// Custom <see cref="MarkupExtension"/> used to provide string, 
    /// it internally use <see cref="IResourceService"/> to query the string source from name key.
    /// </summary>
	[MarkupExtensionReturnType(typeof(BindingExpression))]
	public class StringResourceExtension : MarkupExtension, INotifyPropertyChanged, IHandle<LanguageChangedMessage>
	{
		static ILog Log = LogManager.GetLog(typeof(StringResourceExtension));

		public StringResourceExtension(string key)
			: this()
		{
			Key = key;
		}

		public StringResourceExtension()
		{
			if (!Execute.InDesignMode)
			{
				IoC.Get<IEventAggregator>().Subscribe(this);
			}
		}

		[ConstructorArgument("key")]
		public string Key
		{
			get;
			set;
		}

		public string Value
		{
			get
			{
				if (Key == null)
				{
					return null;
				}

				try
				{
					string result = IoC.Get<IResourceService>().GetString(Key);
					return result;
				}
				catch (Exception e)
				{
					Log.Error(e);
					return null;
				}
			}
		}

		public override object ProvideValue(IServiceProvider serviceProvider)
		{
			IProvideValueTarget target = serviceProvider.GetService(typeof(IProvideValueTarget)) as IProvideValueTarget;

			Setter setter = target.TargetObject as Setter;
			if (setter != null)
			{
				return new Binding("Value") { Source = this, Mode = BindingMode.OneWay };
			}
			else
			{
				Binding binding = new Binding("Value") { Source = this, Mode = BindingMode.OneWay };
				return binding.ProvideValue(serviceProvider);
			}
		}

		#region INotifyPropertyChanged

		public event PropertyChangedEventHandler PropertyChanged;

		static readonly PropertyChangedEventArgs ValueChangedEventArgs = new PropertyChangedEventArgs("Value");

		protected void NotifyValueChanged()
		{
			if (PropertyChanged != null)
				PropertyChanged(this, ValueChangedEventArgs);
		}

		#endregion

		#region IHandle<LanguageChangedMessage> Members

		public void Handle(LanguageChangedMessage message)
		{
			NotifyValueChanged();
		}

		#endregion
	}
}
