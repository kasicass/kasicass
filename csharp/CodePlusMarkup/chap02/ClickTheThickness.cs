using System;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.ClickTheThickness
{
	public class ClickTheThickness : Window
	{
		int thickness = 1;

		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new ClickTheThickness());
		}

		public ClickTheThickness()
		{
			BorderThickness = new Thickness(thickness);
		}

		protected override void OnMouseDown(MouseButtonEventArgs args)
		{
			thickness += 1;
			thickness %= 100;

			BorderThickness = new Thickness(thickness);
		}
	}
}
