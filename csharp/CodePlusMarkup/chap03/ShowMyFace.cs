using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;

namespace Petzold.ShowMyFace
{
	class ShowMyFace : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new ShowMyFace());
		}

		public ShowMyFace()
		{
			Title = "Show My Face";

			Uri uri = new Uri("http://www.charlespetzold.com/PetzoldTattoo.jpg");
			BitmapImage bitmap = new BitmapImage(uri);
			Image img = new Image();
			img.Source = bitmap;
			Content = img;
		}
	}
}

