using Petzold.SelectColorFromGrid;
using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.SelectColorFromMenuGrid
{
	public class SelectColorFromMenuGrid : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new SelectColorFromMenuGrid());
		}

		public SelectColorFromMenuGrid()
		{
			Title = "Select Color from Menu Grid";

			DockPanel dock = new DockPanel();
			Content = dock;

			Menu menu = new Menu();
			dock.Children.Add(menu);
			DockPanel.SetDock(menu, Dock.Top);

			TextBlock text = new TextBlock();
			text.Text = Title;
			text.FontSize = 32;
			text.TextAlignment = TextAlignment.Center;
			dock.Children.Add(text);

			MenuItem itemColor = new MenuItem();
			itemColor.Header = "_Color";
			menu.Items.Add(itemColor);

			MenuItem itemForeground = new MenuItem();
			itemForeground.Header = "_Foreground";
			itemColor.Items.Add(itemForeground);

			ColorGridBox clrbox = new ColorGridBox();
			clrbox.SetBinding(ColorGridBox.SelectedValueProperty, "Foreground");
			clrbox.DataContext = this;
			itemForeground.Items.Add(clrbox);

			MenuItem itemBackground = new MenuItem();
			itemBackground.Header = "_Background";
			itemColor.Items.Add(itemBackground);

			clrbox = new ColorGridBox();
			clrbox.SetBinding(ColorGridBox.SelectedValueProperty, "Background");
			clrbox.DataContext = this;
			itemBackground.Items.Add(clrbox);
		}
	}
}

