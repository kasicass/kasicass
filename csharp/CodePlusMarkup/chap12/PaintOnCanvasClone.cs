using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Shapes;

namespace Petzold.PaintOnCanvasClone
{
	public class PaintOnCanvasClone : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new PaintOnCanvasClone());
		}

		public PaintOnCanvasClone()
		{
			Title = "Paint on Canvas Clone";

			CanvasClone canv = new CanvasClone();
			Content = canv;

			SolidColorBrush[] brushes = { Brushes.Red, Brushes.Green, Brushes.Blue };
			for (int i = 0; i < brushes.Length; i++)
			{
				Rectangle rect = new Rectangle();
				rect.Fill = brushes[i];
				rect.Width = 200;
				rect.Height = 200;
				canv.Children.Add(rect);
				CanvasClone.SetLeft(rect, 100*(i+1));
				CanvasClone.SetTop(rect, 100*(i+1));
			}
		}
	}
}

