using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Threading;

namespace Petzold.CalculateInHex
{
	public class CalculateInHex : Window
	{
		RoundedButton btnDisplay;
		ulong numDisplay;
		ulong numFirst;
		bool bNewNumber = true;
		char chOperation = '=';

		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new CalculateInHex());
		}

		public CalculateInHex()
		{
			Title = "Calculate in Hex";
			SizeToContent = SizeToContent.WidthAndHeight;
			ResizeMode = ResizeMode.CanMinimize;

			Grid grid = new Grid();
			grid.Margin = new Thickness(4);
			Content = grid;

			for (int i = 0; i < 5; i++)
			{
				ColumnDefinition col = col = new ColumnDefinition();
				col.Width = GridLength.Auto;
				grid.ColumnDefinitions.Add(col);
			}

			for (int i = 0; i < 7; i++)
			{
				RowDefinition row = new RowDefinition();
				row.Height = GridLength.Auto;
				grid.RowDefinitions.Add(row);
			}

			string[] strButtons = { "0",
				"D", "E", "F", "+", "&",
				"A", "B", "C", "-", "|",
				"7", "8", "9", "*", "^",
				"4", "5", "6", "/", "<<",
				"1", "2", "3", "%", ">>",
				"0", "Back", "Equals" };
			int iRow = 0, iCol = 0;

			foreach (string str in strButtons)
			{
				RoundedButton btn = new RoundedButton();
				btn.Focusable = false;
				btn.Height = 32;
				btn.Margin = new Thickness(4);
				btn.Click += ButtonOnClick;

				TextBlock txt = new TextBlock();
				txt.Text = str;
				btn.Child = txt;

				grid.Children.Add(btn);
				Grid.SetRow(btn, iRow);
				Grid.SetColumn(btn, iCol);

				if (iRow == 0 && iCol == 0)
				{
					btnDisplay = btn;
					btn.Margin = new Thickness(4,4,4,6);
					Grid.SetColumnSpan(btn, 5);
					iRow = 1;
				}
				else if (iRow == 6 && iCol > 0)
				{
					Grid.SetColumnSpan(btn, 2);
					iCol += 2;
				}
				else
				{
					btn.Width = 32;
					if (0 == (iCol = (iCol + 1) % 5))
						iRow++;
				}
			}
		}

		void ButtonOnClick(object sender, RoutedEventArgs args)
		{
			RoundedButton btn = args.Source as RoundedButton;
			if (btn == null)
				return;

			string strButton = (btn.Child as TextBlock).Text;
			char chButton = strButton[0];

			if (strButton == "Equals")
				chButton = '=';

			if (btn == btnDisplay)
				numDisplay = 0;
			else if (strButton == "Back")
				numDisplay /= 16;
			else if (Char.IsLetterOrDigit(chButton))
			{
				if (bNewNumber)
				{
					numFirst = numDisplay;
					numDisplay = 0;
					bNewNumber = false;
				}

				if (numDisplay <= ulong.MaxValue >> 4)
					numDisplay = 16*numDisplay + (ulong)(chButton - (Char.IsDigit(chButton) ? '0' : 'A' - 10));
			}
			else
			{
				if (!bNewNumber)
				{
					switch (chOperation)
					{
					case '=': break;
					case '+': numDisplay = numFirst + numDisplay; break;
					case '-': numDisplay = numFirst - numDisplay; break;
					case '*': numDisplay = numFirst * numDisplay; break;
					case '&': numDisplay = numFirst & numDisplay; break;
					case '|': numDisplay = numFirst | numDisplay; break;
					case '^': numDisplay = numFirst ^ numDisplay; break;
					case '<': numDisplay = numFirst << (int)numDisplay; break;
					case '>': numDisplay = numFirst >> (int)numDisplay; break;
					case '/': numDisplay = numDisplay != 0 ? numFirst / numDisplay : ulong.MaxValue; break;
					case '%': numDisplay = numDisplay != 0 ? numFirst % numDisplay : ulong.MaxValue; break;
					default: numDisplay = 0; break;
					}
				}

				bNewNumber = true;
				chOperation = chButton;
			}

			TextBlock text = new TextBlock();
			text.Text = String.Format("{0:X}", numDisplay);
			btnDisplay.Child = text;
		}

		protected override void OnTextInput(TextCompositionEventArgs args)
		{
			base.OnTextInput(args);

			if (args.Text.Length == 0)
				return;

			char chKey = Char.ToUpper(args.Text[0]);
			foreach (UIElement child in (Content as Grid).Children)
			{
				RoundedButton btn = child as RoundedButton;
				string strButton = (btn.Child as TextBlock).Text;

				if ((chKey == strButton[0] && btn != btnDisplay && strButton != "Equals" && strButton != "Back") ||
					(chKey == '=' && strButton == "Equals") ||
					(chKey == '\r' && strButton == "Equals") ||
					(chKey == '\b' && strButton == "Back") ||
					(chKey == '\x1B' && btn == btnDisplay))
				{
					RoutedEventArgs argsClick = new RoutedEventArgs(RoundedButton.ClickEvent, btn);
					btn.RaiseEvent(argsClick);

					btn.IsPressed = true;

					DispatcherTimer tmr = new DispatcherTimer();
					tmr.Interval = TimeSpan.FromMilliseconds(100);
					tmr.Tag = btn;
					tmr.Tick += TimerOnTick;
					tmr.Start();

					args.Handled = true;
				}
			}
		}

		void TimerOnTick(object sender, EventArgs args)
		{
			DispatcherTimer tmr = sender as DispatcherTimer;
			RoundedButton btn = tmr.Tag as RoundedButton;
			btn.IsPressed = false;

			tmr.Stop();
			tmr.Tick -= TimerOnTick;
		}
	}
}

