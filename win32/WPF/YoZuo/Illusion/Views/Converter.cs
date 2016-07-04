using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Media;

namespace Illusion
{
	public class GenericBindingConverter : IValueConverter
	{
		#region IValueConverter Members

		public object Convert(object value, Type targetType, object parameter, System.Globalization.CultureInfo culture)
		{
			if (!(value is IPart) ||
				parameter == null)
			{
				return null;
			}

			var interfaceName = parameter.ToString().Split('.');
			if (interfaceName == null && interfaceName.Length != 2)
			{
				throw new ArgumentException("The parameter of GenericBindingConverter is not valid, it should be as Interface.Property");
			}

			return value.GetType().GetInterfaceProperty(interfaceName[0], interfaceName[1]).GetValue(value, null);
		}

		public object ConvertBack(object value, Type targetType, object parameter, System.Globalization.CultureInfo culture)
		{
			throw new NotImplementedException();
		}

		#endregion
	}

	public class ImgaeResourceConverter : IValueConverter
	{
		public static ImgaeResourceConverter Default = new ImgaeResourceConverter();

		#region IValueConverter Members

		public object Convert(object value, Type targetType, object parameter, System.Globalization.CultureInfo culture)
		{
			if (value == null || value == DependencyProperty.UnsetValue)
			{
				return DependencyProperty.UnsetValue;
			}

			return new Image() { Source = ImgaeSourceResourceConverter.Default.Convert(value, targetType, parameter, culture) as ImageSource };
		}

		public object ConvertBack(object value, Type targetType, object parameter, System.Globalization.CultureInfo culture)
		{
			throw new NotImplementedException();
		}

		#endregion
	}

	public class ImgaeSourceResourceConverter : IValueConverter
	{
		public static ImgaeSourceResourceConverter Default = new ImgaeSourceResourceConverter();

		#region IValueConverter Members

		public object Convert(object value, Type targetType, object parameter, System.Globalization.CultureInfo culture)
		{
			if (value == null || value == DependencyProperty.UnsetValue)
			{
				return DependencyProperty.UnsetValue;
			}

			return IoC.Get<IResourceService>().GetImage(value.ToString());
		}

		public object ConvertBack(object value, Type targetType, object parameter, System.Globalization.CultureInfo culture)
		{
			throw new NotImplementedException();
		}

		#endregion
	}
}
