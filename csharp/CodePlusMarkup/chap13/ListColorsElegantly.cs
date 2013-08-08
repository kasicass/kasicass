using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.ListColorsElegantly
{
	public class ListColorsElegantly : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new ListColorsElegantly());
		}

		public ListColorsElegantly()
		{
			Title = "List Colors Elegantly";
	
			ColorListBox lstbox = new ColorListBox();
			lstbox.Height = 150;
			lstbox.Width = 150;
			lstbox.SelectionChanged += ListBoxOnSelectionChanged;
			Content = lstbox;
	
			lstbox.SelectedColor = SystemColors.WindowColor;
		}

		void ListBoxOnSelectionChanged(object sender, SelectionChangedEventArgs args)
		{
			ColorListBox lstbox = sender as ColorListBox;
			Background = new SolidColorBrush(lstbox.SelectedColor);
		}
	}
}

