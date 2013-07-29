using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.SplitTheClient
{
	class SplitTheClient : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new SplitTheClient());
		}

		public SplitTheClient()
		{
			Title = "Split the Client";

			Grid grid1 = new Grid();
			grid1.ColumnDefinitions.Add(new ColumnDefinition());
			grid1.ColumnDefinitions.Add(new ColumnDefinition());
			grid1.ColumnDefinitions.Add(new ColumnDefinition());
			grid1.ColumnDefinitions[1].Width = GridLength.Auto;
			Content = grid1;

			Button btn = new Button();
			btn.Content = "Button No.1";
			grid1.Children.Add(btn);
			Grid.SetRow(btn, 0);
			Grid.SetColumn(btn, 0);

			GridSplitter split = new GridSplitter();
			split.ShowsPreview = true;
			split.HorizontalAlignment = HorizontalAlignment.Center;
			split.VerticalAlignment = VerticalAlignment.Stretch;
			split.Width = 6;
			grid1.Children.Add(split);
			Grid.SetRow(split, 0);
			Grid.SetColumn(split, 1);

			Grid grid2 = new Grid();
			grid2.RowDefinitions.Add(new RowDefinition());
			grid2.RowDefinitions.Add(new RowDefinition());
			grid2.RowDefinitions.Add(new RowDefinition());
			grid2.RowDefinitions[1].Height = GridLength.Auto;
			grid1.Children.Add(grid2);
			Grid.SetRow(grid2, 0);
			Grid.SetColumn(grid2, 2);

			btn = new Button();
			btn.Content = "Button No.2";
			grid2.Children.Add(btn);
			Grid.SetRow(btn, 0);
			Grid.SetColumn(btn, 0);

			split = new GridSplitter();
			split.ShowsPreview = true;
			split.HorizontalAlignment = HorizontalAlignment.Stretch;
			split.VerticalAlignment = VerticalAlignment.Center;
			split.Height = 6;
			grid2.Children.Add(split);
			Grid.SetRow(split, 1);
			Grid.SetColumn(split, 0);

			btn = new Button();
			btn.Content = "Button No.3";
			grid2.Children.Add(btn);
			Grid.SetRow(btn, 2);
			Grid.SetColumn(btn, 0);
		}
	}
}
