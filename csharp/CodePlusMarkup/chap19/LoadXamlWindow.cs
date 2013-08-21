using System;
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Markup;
using System.Xml;

namespace Petzold.LoadXamlWindow
{
	public class LoadXamlWindow : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			StreamReader sr = new StreamReader("LoadXamlWindow.xml");
			XmlTextReader xmlreader = new XmlTextReader(sr);
			Window win = XamlReader.Load(xmlreader) as Window;
			win.AddHandler(Button.ClickEvent, new RoutedEventHandler(ButtonOnClick));
			app.Run(win);
		}

		static void ButtonOnClick(object sender, RoutedEventArgs args)
		{
			MessageBox.Show("The button labeled '" + (args.Source as Button).Content + "' has been clicked");
		}
	}
}

