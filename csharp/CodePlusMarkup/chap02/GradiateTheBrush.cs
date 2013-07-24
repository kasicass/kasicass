using System;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.GradiateTheBrush
{
	public class GradiateTheBrush : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new GradiateTheBrush());
		}

		public GradiateTheBrush()
		{
			Title = "Gradiate the Brush";

			LinearGradientBrush brush = new LinearGradientBrush(Colors.Red, Colors.Blue, new Point(0,0), new Point(1,1));
			Background = brush;
		}
	}
}
