using System;
using System.Windows;
using System.Windows.Input;

namespace Petzold.HandleAnEvent
{
	class HandleAnEvent
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();

			Window win = new Window();
			win.Title = "Handle An Event";
			win.MouseDown += WindowOnMouseDown;

			app.Run(win);
		}

		static void WindowOnMouseDown(object sender, MouseButtonEventArgs args)
		{
			Window win = sender as Window;
			string msg = string.Format("Window clicked with {0} button at point {1}",
				args.ChangedButton, args.GetPosition(win));
			MessageBox.Show(msg, win.Title);
		}
	}
}

