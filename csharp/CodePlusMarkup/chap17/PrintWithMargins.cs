using System;
using System.Globalization;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Printing;

namespace Petzold.PrintWithMargins
{
	public class PrintWithMargins : Window
	{
		PrintQueue prnqueue;
		PrintTicket prntkt;
		Thickness marginPage = new Thickness(96);

		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new PrintWithMargins());
		}

		public PrintWithMargins()
		{
			Title = "Print with Margins";
			FontSize = 24;

			StackPanel stack = new StackPanel();
			Content = stack;

			Button btn = new Button();
			btn.Content = "Page Set_up...";
			btn.HorizontalAlignment = HorizontalAlignment.Center;
			btn.Margin = new Thickness(24);
			btn.Click += SetupOnClick;
			stack.Children.Add(btn);

			btn = new Button();
			btn.Content = "_Print...";
			btn.HorizontalAlignment = HorizontalAlignment.Center;
			btn.Margin = new Thickness(24);
			btn.Click += PrintOnClick;
			stack.Children.Add(btn);
		}

		void SetupOnClick(object sender, RoutedEventArgs args)
		{
			PageMarginsDialog dlg = new PageMarginsDialog();
			dlg.Owner = this;
			dlg.PageMargins = marginPage;

			if (dlg.ShowDialog().GetValueOrDefault())
			{
				marginPage = dlg.PageMargins;
			}
		}

		void PrintOnClick(object sender, RoutedEventArgs args)
		{
			PrintDialog dlg = new PrintDialog();

			if (prnqueue != null)
				dlg.PrintQueue = prnqueue;

			if (prntkt != null)
				dlg.PrintTicket = prntkt;

			if (dlg.ShowDialog().GetValueOrDefault())
			{
				prnqueue = dlg.PrintQueue;
				prntkt = dlg.PrintTicket;

				DrawingVisual vis = new DrawingVisual();
				DrawingContext dc = vis.RenderOpen();
				Pen pn = new Pen(Brushes.Black, 1);

				Rect rectPage = new Rect(marginPage.Left, marginPage.Top,
					dlg.PrintableAreaWidth - (marginPage.Left + marginPage.Right),
					dlg.PrintableAreaHeight - (marginPage.Top + marginPage.Bottom));
				dc.DrawRectangle(null, pn, rectPage);

				FormattedText formtxt = new FormattedText(
					String.Format("Hello, Printer! {0} x {1}", dlg.PrintableAreaWidth/96, dlg.PrintableAreaHeight/96),
					CultureInfo.CurrentCulture, FlowDirection.LeftToRight, new Typeface(
					new FontFamily("Times New Roman"), FontStyles.Italic, FontWeights.Normal, FontStretches.Normal),
					48, Brushes.Black);

				Size sizeText = new Size(formtxt.Width, formtxt.Height);
				Point ptText = new Point(rectPage.Left + (rectPage.Width - formtxt.Width) / 2,
					rectPage.Top + (rectPage.Height - formtxt.Height) / 2);

				dc.DrawText(formtxt, ptText);
				dc.DrawRectangle(null,pn, new Rect(ptText, sizeText));

				dc.Close();
				dlg.PrintVisual(vis, Title);
			}
		}
	}
}

