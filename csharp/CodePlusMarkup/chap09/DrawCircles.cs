using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Shapes;

namespace Petzold.DrawCircles
{
	public class DrawCircles : Window
	{
		Canvas canv;

		bool isDrawing;
		Ellipse elips;
		Point ptCenter;

		bool isDragging;
		FrameworkElement elDragging;
		Point ptMouseStart, ptElementStart;

		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new DrawCircles());
		}

		public DrawCircles()
		{
			Title = "Draw Circles";
			Content = canv = new Canvas();
		}

		protected override void OnMouseLeftButtonDown(MouseButtonEventArgs args)
		{
			base.OnMouseLeftButtonDown(args);

			if (isDragging)
				return;

			ptCenter = args.GetPosition(canv);
			elips = new Ellipse();
			elips.Stroke = SystemColors.WindowTextBrush;
			elips.StrokeThickness = 1;
			elips.Width = 0;
			elips.Height = 0;
			canv.Children.Add(elips);
			Canvas.SetLeft(elips, ptCenter.X);
			Canvas.SetTop(elips, ptCenter.Y);

			CaptureMouse();
			isDrawing = true;
		}

		protected override void OnMouseRightButtonDown(MouseButtonEventArgs args)
		{
			base.OnMouseRightButtonDown(args);

			if (isDrawing)
				return;

			ptMouseStart = args.GetPosition(canv);
			elDragging = canv.InputHitTest(ptMouseStart) as FrameworkElement;
			if (elDragging != null)
			{
				ptElementStart = new Point(Canvas.GetLeft(elDragging), Canvas.GetTop(elDragging));
				isDragging = true;
			}
		}

		protected override void OnMouseDown(MouseButtonEventArgs args)
		{
			base.OnMouseDown(args);

			if (args.ChangedButton == MouseButton.Middle)
			{
				Shape shape = canv.InputHitTest(args.GetPosition(canv)) as Shape;
				if (shape != null)
				{
					shape.Fill = (shape.Fill == Brushes.Red ? Brushes.Transparent : Brushes.Red);
				}
			}
		}

		protected override void OnMouseMove(MouseEventArgs args)
		{
			base.OnMouseMove(args);
			Point ptMouse = args.GetPosition(canv);

			if (isDrawing)
			{
				double dRadius = Math.Sqrt(Math.Pow(ptCenter.X - ptMouse.X, 2) +
					Math.Pow(ptCenter.Y - ptMouse.Y, 2));

				Canvas.SetLeft(elips, ptCenter.X - dRadius);
				Canvas.SetTop(elips, ptCenter.Y - dRadius);

				elips.Width = 2*dRadius;
				elips.Height = 2*dRadius;
			}
			else if (isDragging)
			{
				Canvas.SetLeft(elDragging, ptElementStart.X + ptMouse.X - ptMouseStart.X);
				Canvas.SetTop(elDragging, ptElementStart.Y + ptMouse.Y - ptMouseStart.Y);
			}
		}

		protected override void OnMouseUp(MouseButtonEventArgs args)
		{
			base.OnMouseUp(args);

			if (isDrawing && args.ChangedButton == MouseButton.Left)
			{
				elips.Stroke = Brushes.Blue;
				elips.StrokeThickness = Math.Min(24, elips.Width/2);
				elips.Fill = Brushes.Red;

				isDrawing = false;
				ReleaseMouseCapture();
			}
			else if (isDragging && args.ChangedButton == MouseButton.Right)
			{
				isDragging = false;
			}
		}

		protected override void OnTextInput(TextCompositionEventArgs args)
		{
			base.OnTextInput(args);

			// esc key
			if (args.Text.IndexOf('\x1B') != -1)
			{
				if (isDrawing)
				{
					ReleaseMouseCapture();
				}
				else if (isDragging)
				{
					Canvas.SetLeft(elDragging, ptElementStart.X);
					Canvas.SetTop(elDragging, ptElementStart.Y);
					isDragging = false;
				}
			}
		}

		protected override void OnLostMouseCapture(MouseEventArgs args)
		{
			base.OnLostMouseCapture(args);

			if (isDrawing)
			{
				canv.Children.Remove(elips);
				isDrawing = false;
			}
		}
	}
}
