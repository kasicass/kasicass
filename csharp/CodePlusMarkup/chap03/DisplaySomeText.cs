using System;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.DisplaySomeText
{
	public class DisplaySomeText : Window
	{
		int method = 0;

		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new DisplaySomeText());
		}

		public DisplaySomeText()
		{
			Title = "Display Some Text";
			Content = "Content can be simple text!";

			FontFamily = new FontFamily("Comic Sans MS");
			FontSize = 48;

			Brush brush = new LinearGradientBrush(Colors.Black, Colors.White, new Point(0, 0), new Point(1, 1));
			Background = brush;
			Foreground = brush;
		}

		protected override void OnMouseDown(MouseButtonEventArgs args)
		{
			method += 1;
			method %= 4;

			if (method == 0)
			{
				Content = "Content can be simple text!";
			}
			else if (method == 1)
			{
				Content = Math.PI;
			}
			else if (method == 2)
			{
				Content = DateTime.Now;
			}
			else
			{
				Content = EventArgs.Empty;
			}
		}
	}
}

