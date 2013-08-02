using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.ExamineRoutedEvents
{
	public class ExamineRoutedEvents : Application
	{
		static readonly FontFamily fontfam = new FontFamily("Lucida Console");
		const string strFormat = "{0,-30} {1,-15} {2,-15} {3,-15}";
		StackPanel stackOutput;
		DateTime dtLast;

		[STAThread]
		public static void Main()
		{
			ExamineRoutedEvents app = new ExamineRoutedEvents();
			app.Run();
		}

		protected override void OnStartup(StartupEventArgs args)
		{
			base.OnStartup(args);

			Window win = new Window();
			win.Title = "Examine Routed Events";

			Grid grid = new Grid();
			win.Content = grid;

			RowDefinition rowdef = new RowDefinition();
			rowdef.Height = GridLength.Auto;
			grid.RowDefinitions.Add(rowdef);

			rowdef = new RowDefinition();
			rowdef.Height = GridLength.Auto;
			grid.RowDefinitions.Add(rowdef);

			Button btn = new Button();
			btn.HorizontalAlignment = HorizontalAlignment.Center;
			btn.Margin = new Thickness(24);
			btn.Padding = new Thickness(24);
			grid.Children.Add(btn);

			TextBlock text = new TextBlock();
			text.FontSize = 24;
			text.Text = win.Title;
			btn.Content = text;

			TextBlock textHeadings = new TextBlock();
			textHeadings.FontFamily = fontfam;
			textHeadings.Inlines.Add(new Underline(new Run(
				String.Format(strFormat, "Routed Event", "sender", "Source", "OriginalSource"))));
			grid.Children.Add(textHeadings);
			Grid.SetRow(textHeadings, 1);

			ScrollViewer scroll = new ScrollViewer();
			grid.Children.Add(scroll);
			Grid.SetRow(scroll, 2);

			stackOutput = new StackPanel();
			scroll.Content = stackOutput;

			UIElement[] els = {win, grid, btn, text};
			foreach (UIElement el in els)
			{
				el.PreviewKeyDown += AllPurposeEventHandler;
				el.PreviewKeyUp += AllPurposeEventHandler;
				el.PreviewTextInput += AllPurposeEventHandler;
				el.KeyDown += AllPurposeEventHandler;
				el.KeyUp += AllPurposeEventHandler;
				el.TextInput += AllPurposeEventHandler;

				el.MouseDown += AllPurposeEventHandler;
				el.MouseUp += AllPurposeEventHandler;
				el.PreviewMouseDown += AllPurposeEventHandler;
				el.PreviewMouseUp += AllPurposeEventHandler;

				el.StylusDown += AllPurposeEventHandler;
				el.StylusUp += AllPurposeEventHandler;
				el.PreviewStylusDown += AllPurposeEventHandler;
				el.PreviewStylusUp += AllPurposeEventHandler;

				el.AddHandler(Button.ClickEvent, new RoutedEventHandler(AllPurposeEventHandler));
			}
			win.Show();
		}

		void AllPurposeEventHandler(object sender, RoutedEventArgs args)
		{
			DateTime dtNow = DateTime.Now;
			if (dtNow - dtLast > TimeSpan.FromMilliseconds(100))
				stackOutput.Children.Add(new TextBlock(new Run(" ")));
			dtLast = dtNow;

			TextBlock text = new TextBlock();
			text.FontFamily = fontfam;
			text.Text = String.Format(strFormat, args.RoutedEvent.Name, TypeWithoutNamespace(sender),
				TypeWithoutNamespace(args.Source), TypeWithoutNamespace(args.OriginalSource));
			stackOutput.Children.Add(text);
			(stackOutput.Parent as ScrollViewer).ScrollToBottom();
		}

		string TypeWithoutNamespace(object obj)
		{
			string[] astr = obj.GetType().ToString().Split('.');
			return astr[astr.Length - 1];
		}
	}
}

