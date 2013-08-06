using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.SelectColor
{
	public class SelectColor : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new SelectColor());
		}

		public SelectColor()
		{
			Title = "Select Color";
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

			ColorGrid clrgrid = new ColorGrid();
			clrgrid.Margin = new Thickness(24);
			clrgrid.HorizontalAlignment = HorizontalAlignment.Center;
			clrgrid.VerticalAlignment = VerticalAlignment.Center;
			clrgrid.SelectedColorChanged += ColorGridOnSelectedColorChanged;
			stack.Children.Add(clrgrid);

			btn = new Button();
			btn.Content = "Do-nothing button\nto test tabbing";
			btn.Margin = new Thickness(24);
			btn.HorizontalAlignment = HorizontalAlignment.Center;
			btn.VerticalAlignment = VerticalAlignment.Center;
			stack.Children.Add(btn);
		}

		void ColorGridOnSelectedColorChanged(object sender, EventArgs args)
		{
			ColorGrid clrgrid = sender as ColorGrid;
			Background = new SolidColorBrush(clrgrid.SelectedColor);
		}
	}
}

