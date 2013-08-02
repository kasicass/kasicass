using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.ScrollFiftyButtons
{
	class ScrollFiftyButtons : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new ScrollFiftyButtons());
		}

		public ScrollFiftyButtons()
		{
			Title = "Scroll Fifty Buttons";
			SizeToContent = SizeToContent.Width;
			AddHandler(Button.ClickEvent, new RoutedEventHandler(ButtonOnClick));

			ScrollViewer scroll = new ScrollViewer();
			Content = scroll;

			StackPanel stack = new StackPanel();
			stack.Margin = new Thickness(5);
			scroll.Content = stack;

			for (int i = 0; i < 50; i++)
			{
				Button btn = new Button();
				btn.Name = "Button" + (i+1);
				btn.Content = btn.Name + " says 'Click me'";
				btn.Margin = new Thickness(5);
				stack.Children.Add(btn);
			}
		}

		void ButtonOnClick(object sender, RoutedEventArgs args)
		{
			Button btn = args.Source as Button;
			if (btn != null)
				MessageBox.Show(btn.Name + " has been clicked", "Button Click");
		}
	}
}

