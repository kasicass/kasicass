using Petzold.CircleTheButtons;
using System;
using System.Reflection;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Shapes;

namespace Petzold.SelectColorFromWheel
{
	class ColorWheel : ListBox
	{
		public ColorWheel()
		{
			FrameworkElementFactory factoryRadialPanel = new FrameworkElementFactory(typeof(RadialPanel));
			ItemsPanel = new ItemsPanelTemplate(factoryRadialPanel);

			DataTemplate template = new DataTemplate(typeof(Brush));
			ItemTemplate = template;

			FrameworkElementFactory elRectangle = new FrameworkElementFactory(typeof(Rectangle));
			elRectangle.SetValue(Rectangle.WidthProperty, 4.0);
			elRectangle.SetValue(Rectangle.HeightProperty, 12.0);
			elRectangle.SetValue(Rectangle.MarginProperty, new Thickness(1, 8, 1, 8));
			elRectangle.SetBinding(Rectangle.FillProperty, new Binding(""));
			template.VisualTree = elRectangle;

			PropertyInfo[] props = typeof(Brushes).GetProperties();
			foreach (PropertyInfo prop in props)
				Items.Add((Brush)prop.GetValue(null,null));
		}
	}
}

