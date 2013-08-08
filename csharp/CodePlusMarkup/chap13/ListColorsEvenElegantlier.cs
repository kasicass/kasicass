using Petzold.ListNamedBrushes;
using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Shapes;

namespace Petzold.ListColorsEvenElegantlier
{
	public class ListColorsEvenElegantlier : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new ListColorsEvenElegantlier());
		}

		public ListColorsEvenElegantlier()
		{
			Title = "List Colors Even Elegantlier";

			DataTemplate template = new DataTemplate(typeof(NamedBrush));
			FrameworkElementFactory factoryStack = new FrameworkElementFactory(typeof(StackPanel));
			factoryStack.SetValue(StackPanel.OrientationProperty, Orientation.Horizontal);
			template.VisualTree = factoryStack;

			FrameworkElementFactory factoryRectangle = new FrameworkElementFactory(typeof(Rectangle));
			factoryRectangle.SetValue(Rectangle.WidthProperty, 16.0);
			factoryRectangle.SetValue(Rectangle.HeightProperty, 16.0);
			factoryRectangle.SetValue(Rectangle.MarginProperty, new Thickness(2));
			factoryRectangle.SetValue(Rectangle.StrokeProperty, SystemColors.WindowTextBrush);
			factoryRectangle.SetBinding(Rectangle.FillProperty, new Binding("Brush"));
			factoryStack.AppendChild(factoryRectangle);

			FrameworkElementFactory factoryTextBlock = new FrameworkElementFactory(typeof(TextBlock));
			factoryTextBlock.SetValue(TextBlock.VerticalAlignmentProperty, VerticalAlignment.Center);
			factoryTextBlock.SetValue(TextBlock.TextProperty, new Binding("Name"));
			factoryStack.AppendChild(factoryTextBlock);

			ListBox lstbox = new ListBox();
			lstbox.Width = 150;
			lstbox.Height = 150;
			Content = lstbox;

			lstbox.ItemTemplate = template;
			lstbox.ItemsSource = NamedBrush.All;

			lstbox.SelectedValuePath = "Brush";
			lstbox.SetBinding(ListBox.SelectedValueProperty, "Background");
			lstbox.DataContext = this;
		}
	}
}

