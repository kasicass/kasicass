using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.SelectColorFromWheel
{
	public class SelectColorFromWheel : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new SelectColorFromWheel());
		}

		public SelectColorFromWheel()
		{
			Title = "Select Color from Wheel";
			SizeToContent = SizeToContent.WidthAndHeight;

			StackPanel stack = new StackPanel();
			stack.Orientation = Orientation.Horizontal;
			Content = stack;

			Button btn = new Button();
			btn.Content = "Do-nothing button\nto test tabbing";
			btn.Margin = new Thickness(24);
			btn.HorizontalAlignment = HorizontalAlignment.Center;
			btn.VerticalAlignment = VerticalAlignment.Center;
			stack.Children.Add(btn);

			ColorWheel clrwheel = new ColorWheel();
			clrwheel.Margin = new Thickness(24);
			clrwheel.HorizontalAlignment = HorizontalAlignment.Center;
			clrwheel.VerticalAlignment = VerticalAlignment.Center;
			stack.Children.Add(clrwheel);
			clrwheel.SetBinding(ColorWheel.SelectedValueProperty, "Background");
			clrwheel.DataContext = this;

			btn = new Button();
			btn.Content = "Do-nothing button\nto test tabbing";
			btn.Margin = new Thickness(24);
			btn.HorizontalAlignment = HorizontalAlignment.Center;
			btn.VerticalAlignment = VerticalAlignment.Center;
			stack.Children.Add(btn);
		}
	}
}

