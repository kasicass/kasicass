using System;
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Markup;
using System.Xml;

namespace Petzold.XamlViewer
{
	public class XamlViewer : Window
	{
		[STAThread]
		public static void Main(string[] args)
		{
			Application app = new Application();
			app.Run(new XamlViewer());
		}

		public XamlViewer()
		{
			Title = "Xaml Viewer";
			AllowDrop = true;

			TextBlock tb = new TextBlock();
			tb.Text = "Drag a xaml file into the window!";
			tb.HorizontalAlignment = HorizontalAlignment.Center;
			tb.VerticalAlignment = VerticalAlignment.Center;
			tb.FontSize = 24;

			Content = tb;
		}

		protected override void OnDrop(DragEventArgs e)
		{
			base.OnDrop(e);

			if (e.Data.GetDataPresent(DataFormats.FileDrop))
			{
				string[] files = (string[])e.Data.GetData(DataFormats.FileDrop);
				StreamReader sr = new StreamReader(files[0]);
				XmlTextReader xmlreader = new XmlTextReader(sr);
				object obj = XamlReader.Load(xmlreader);

				if (obj is Window)
				{
					Window win = obj as Window;
					win.Owner = this;
					win.Show();
				}
				else
				{
					Content = obj;
				}
			}
		}
	}
}

