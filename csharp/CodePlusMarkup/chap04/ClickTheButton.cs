using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.ClickTheButton
{
	public class ClickTheButton : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new ClickTheButton());
		}

		public ClickTheButton()
		{
			Title = "Click the Button";

			Button btn = new Button();
			btn.Content = "_Click me, please!";
			btn.Click += ButtonOnClick;
			btn.Margin = new Thickness(96);
			btn.Padding = new Thickness(96);

			Content = btn;
		}

		void ButtonOnClick(object sender, RoutedEventArgs args)
		{
			MessageBox.Show("The button has been clicked and all is well.", Title);
		}
	}
}

