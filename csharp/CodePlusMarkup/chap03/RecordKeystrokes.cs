using System;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.RecordKeystrokes
{
	public class RecordKeystrokes : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new RecordKeystrokes());
		}

		public RecordKeystrokes()
		{
			Title = "Record Keystrokes";
			Content = "";
		}

		protected override void OnTextInput(TextCompositionEventArgs args)
		{
			base.OnTextInput(args);
			string s = Content as String;

			if (args.Text == "\b")
			{
				if (s.Length > 0)
					s = s.Substring(0, s.Length - 1);
			}
			else
			{
				s += args.Text;
			}
			Content = s;
		}
	}
}

