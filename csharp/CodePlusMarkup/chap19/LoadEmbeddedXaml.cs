using System;
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Markup;
using System.Xml;

namespace Petzold.LoadEmbeddedXaml
{
	public class LoadEmbeddedXaml : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new LoadEmbeddedXaml());
		}

		public LoadEmbeddedXaml()
		{
			Title = "Load Embedded Xaml";

			string strXaml = "<Button xmlns='http://schemas.microsoft.com/winfx/2006/xaml/presentation' " +
				"Foreground='LightSeaGreen' FontSize='24pt'>" +
				"  Click me!" +
				"</Button>";

			StringReader strreader = new StringReader(strXaml);
			XmlTextReader xmlreader = new XmlTextReader(strreader);
			object obj = XamlReader.Load(xmlreader);

			Content = obj;
		}
	}
}

