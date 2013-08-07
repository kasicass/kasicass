using System;
using System.Reflection;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.ListColoredLabels
{
	class ListColoredLabels : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new ListColoredLabels());
		}

		public ListColoredLabels()
		{
			Title = "List Colored Labels";

			ListBox lstbox = new ListBox();
			lstbox.Height = 150;
			lstbox.Width = 150;
			lstbox.SelectionChanged += ListBoxOnSelectionChanged;
			Content = lstbox;

			PropertyInfo[] props = typeof(Colors).GetProperties();
			foreach (PropertyInfo prop in props)
			{
				Color clr = (Color)prop.GetValue(null, null);
				bool isBlack = .222*clr.R + .707*clr.G + .071*clr.B > 125;
				Label lbl = new Label();
				lbl.Content = prop.Name;
				lbl.Background = new SolidColorBrush(clr);
				lbl.Foreground = isBlack ? Brushes.Black : Brushes.White;
				lbl.Width = 100;
				lbl.Margin = new Thickness(15, 0, 0, 0);
				lbl.Tag = clr;
				lstbox.Items.Add(lbl);
			}
		}

		void ListBoxOnSelectionChanged(object sender, SelectionChangedEventArgs args)
		{
			ListBox lstbox = sender as ListBox;
			Label lbl = lstbox.SelectedItem as Label;
			if (lbl != null)
			{
				Color clr = (Color)lbl.Tag;
				Background = new SolidColorBrush(clr);
			}
		}
	}
}

