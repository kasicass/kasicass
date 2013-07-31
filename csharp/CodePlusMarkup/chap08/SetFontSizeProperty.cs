using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.SetFontSizeProperty
{
	public class SetFontSizeProperty : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new SetFontSizeProperty());
		}

		public SetFontSizeProperty()
		{
			Title = "Set FontSize Property";
			SizeToContent = SizeToContent.WidthAndHeight;
			ResizeMode = ResizeMode.CanMinimize;
			FontSize = 16;
			double[] fntsizes = { 8, 16, 32 };

			Grid grid = new Grid();
			Content = grid;

			for (int i = 0; i < 2; i++)
			{
				RowDefinition row = new RowDefinition();
				row.Height = GridLength.Auto;
				grid.RowDefinitions.Add(row);
			}

			for (int i = 0; i < fntsizes.Length; i++)
			{
				ColumnDefinition col = new ColumnDefinition();
				col.Width = GridLength.Auto;
				grid.ColumnDefinitions.Add(col);
			}

			for (int i = 0; i < fntsizes.Length; i++)
			{
				Button btn = new Button();
				btn.Content = new TextBlock(new Run("Set window FontSize to " + fntsizes[i]));
				btn.Tag = fntsizes[i];
				btn.HorizontalAlignment = HorizontalAlignment.Center;
				btn.VerticalAlignment = VerticalAlignment.Center;
				btn.Click += WindowFontSizeOnClick;
				grid.Children.Add(btn);
				Grid.SetRow(btn, 0);
				Grid.SetColumn(btn, i);

				btn = new Button();
				btn.Content = new TextBlock(new Run("Set button FontSize to " + fntsizes[i]));
				btn.Tag = fntsizes[i];
				btn.HorizontalAlignment = HorizontalAlignment.Center;
				btn.VerticalAlignment = VerticalAlignment.Center;
				btn.Click += ButtonFontSizeOnClick;
				grid.Children.Add(btn);
				Grid.SetRow(btn, 1);
				Grid.SetColumn(btn, i);
			}
		}

		void WindowFontSizeOnClick(object sender, RoutedEventArgs args)
		{
			Button btn = args.Source as Button;
			FontSize = (double)btn.Tag;
		}

		void ButtonFontSizeOnClick(object sender, RoutedEventArgs args)
		{
			Button btn = args.Source as Button;
			btn.FontSize = (double)btn.Tag;
		}
	}
}

