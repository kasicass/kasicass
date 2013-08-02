using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.ExamineKeystrokes
{
	class ExamineKeystrokes : Window
	{
		StackPanel stack;
		ScrollViewer scroll;
		string strHeader = "Event Key Sys-Key Text Ctrl-Text Sys-Text Ime KeyStates IsDown IsUp isToggled IsRepeat";
		string strFormatKey  = "{0,-10}{1,-20}{2,-10}      {3,-10}{4,-15}{5,-8}{6,-7}{7,-10}{8,-10}";
		string strFormatText = "{0,-10}     {1,-10}{2,-10}{3,-10}";

		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new ExamineKeystrokes());
		}

		public ExamineKeystrokes()
		{
			Title = "Examine Keystrokes";
			FontFamily = new FontFamily("Courier New");

			Grid grid = new Grid();
			Content = grid;

			RowDefinition rowdef = new RowDefinition();
			rowdef.Height = GridLength.Auto;
			grid.RowDefinitions.Add(rowdef);
			grid.RowDefinitions.Add(new RowDefinition());

			TextBlock textHeader = new TextBlock();
			textHeader.FontWeight = FontWeights.Bold;
			textHeader.Text = strHeader;
			grid.Children.Add(textHeader);

			scroll = new ScrollViewer();
			grid.Children.Add(scroll);
			Grid.SetRow(scroll, 1);

			stack = new StackPanel();
			scroll.Content = stack;
		}

		protected override void OnKeyDown(KeyEventArgs args)
		{
			base.
		}
	}
}

