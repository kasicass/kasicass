using System;
using System.Printing;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.PrintBanner
{
	public class PrintBanner : Window
	{
		TextBox txtbox;

		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new PrintBanner());
		}

		public PrintBanner()
		{
			Title = "Print Banner";
			SizeToContent = SizeToContent.WidthAndHeight;

			StackPanel stack = new StackPanel();
			Content = stack;

			txtbox = new TextBox();
			txtbox.Width = 250;
			txtbox.Margin = new Thickness(12);
			stack.Children.Add(txtbox);

			Button btn = new Button();
			btn.Content = "_Print...";
			btn.Margin = new Thickness(12);
			btn.Click += PrintOnClick;
			btn.HorizontalAlignment = HorizontalAlignment.Center;
			stack.Children.Add(btn);

			txtbox.Focus();
		}

		void PrintOnClick(object sender, RoutedEventArgs args)
		{
			PrintDialog dlg = new PrintDialog();
			if (dlg.ShowDialog().GetValueOrDefault())
			{
				PrintTicket prntkt = dlg.PrintTicket;
				prntkt.PageOrientation = PageOrientation.Portrait;
				dlg.PrintTicket = prntkt;

				BannerDocumentPaginator paginator = new BannerDocumentPaginator();
				paginator.Text = txtbox.Text;
				paginator.PageSize = new Size(dlg.PrintableAreaWidth, dlg.PrintableAreaHeight);
				dlg.PrintDocument(paginator, "Banner: " + txtbox.Text);
			}
		}
	}
}

