using System;
using System.Reflection;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.CompileXamlWindow
{
	public partial class CompileXamlWindow : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new CompileXamlWindow());
		}

		public CompileXamlWindow()
		{
			InitializeComponent();

			foreach (PropertyInfo prop in typeof(Brushes).GetProperties())
				lstbox.Items.Add(prop.Name);
		}

		void ButtonOnClick(object sender, RoutedEventArgs args)
		{
			Button btn = sender as Button;
			MessageBox.Show("The button labeled '" + btn.Content + "' has beed clicked.");
		}

		void ListBoxOnSelection(object sender, SelectionChangedEventArgs args)
		{
			ListBox lstbox = sender as ListBox;
			string strItem = lstbox.SelectedItem as string;
			PropertyInfo prop = typeof(Brushes).GetProperty(strItem);
			elips.Fill = (Brush)prop.GetValue(null, null);
		}
	}
}

