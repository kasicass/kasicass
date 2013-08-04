using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.GetMedieval
{
	public class GetMedieval : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new GetMedieval());
		}

		public GetMedieval()
		{
			Title = "Get Medieval";

			MedievalButton btn = new MedievalButton();
			btn.Text = "Click this button";
			btn.FontSize = 24;
			btn.HorizontalAlignment = HorizontalAlignment.Center;
			btn.VerticalAlignment = VerticalAlignment.Center;
			btn.Padding = new Thickness(5, 20, 5, 20);
			btn.Knock += ButtonOnKnock;

			Content = btn;
		}

		void ButtonOnKnock(object sender, RoutedEventArgs args)
		{
			MedievalButton btn = args.Source as MedievalButton;
			MessageBox.Show("The button labeled \"" + btn.Text + "\" has been knocked.", Title);
		}
	}
}

