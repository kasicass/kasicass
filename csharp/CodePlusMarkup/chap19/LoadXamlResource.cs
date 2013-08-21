using System;
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Markup;
using System.Xml;

namespace Petzold.LoadXamlResource
{
	public class LoadXamlResource : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new LoadXamlResource());
		}

		public LoadXamlResource()
		{
			Title = "Load Xaml Resource";

			StreamReader sr = new StreamReader("LoadXamlResource.xml");
			XmlTextReader xmlreader = new XmlTextReader(sr);
			FrameworkElement el = XamlReader.Load(xmlreader) as FrameworkElement;
			Content = el;

			Button btn = el.FindName("MyButton") as Button;
			if (btn != null)
				btn.Click += ButtonOnClick;
		}

		void ButtonOnClick(object sender, RoutedEventArgs args)
		{
			MessageBox.Show("The button labeled '" + (args.Source as Button).Content + "' has been clicked");
		}
	}
}

