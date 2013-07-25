using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.BindTheButton
{
	public class BindTheButton : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new BindTheButton());
		}

		public BindTheButton()
		{
			Title = "Bind the Button";

			ToggleButton btn = new ToggleButton();
			btn.Content = "Make _Topmost";
			btn.HorizontalAlignment = HorizontalAlignment.Center;
			btn.VerticalAlignment = VerticalAlignment.Center;
			btn.SetBinding(ToggleButton.IsCheckedProperty, "Topmost");
			btn.DataContext = this;
			Content = btn;

			ToolTip tip = new ToolTip();
			tip.Content = "Toggle the button on to make the window topmost on the desktop";
			btn.ToolTip = tip;
		}
	}
}

