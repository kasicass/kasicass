using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.EnterTheGrid
{
	public class EnterTheGrid : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new EnterTheGrid());
		}

		public EnterTheGrid()
		{
			Title = "Enter the Grid";
			MinWidth = 300;
			SizeToContent = SizeToContent.WidthAndHeight;

			StackPanel stack = new StackPanel();
			Content = stack;

			Grid grid1 = new Grid();
			grid1.Margin = new Thickness(5);
			grid1.ShowGridLines = true;
			stack.Children.Add(grid1);

			for (int i = 0; i < 5; i++)
			{
				RowDefinition rowdef = new RowDefinition();
				rowdef.Height = GridLength.Auto;
				grid1.RowDefinitions.Add(rowdef);
			}

			ColumnDefinition coldef = new ColumnDefinition();
			coldef.Width = GridLength.Auto;
			grid1.ColumnDefinitions.Add(coldef);

			coldef = new ColumnDefinition();
			coldef.Width = new GridLength(100, GridUnitType.Star);
			grid1.ColumnDefinitions.Add(coldef);

			string[] strLabels = { "_First name:", "_Last name:", "_Social security number:",
				"_Credit card number:", "_Other personal stuff:" };

			for (int i = 0; i < strLabels.Length; i++)
			{
				Label lbl = new Label();
				lbl.Content = strLabels[i];
				lbl.VerticalContentAlignment = VerticalAlignment.Center;
				grid1.Children.Add(lbl);
				Grid.SetRow(lbl, i);
				Grid.SetColumn(lbl, 0);

				TextBox txtbox = new TextBox();
				txtbox.Margin = new Thickness(5);
				grid1.Children.Add(txtbox);
				Grid.SetRow(txtbox, i);
				Grid.SetColumn(txtbox, 1);
			}

			Grid grid2 = new Grid();
			grid2.Margin = new Thickness(10);
			grid2.ShowGridLines = true;
			stack.Children.Add(grid2);

			grid2.ColumnDefinitions.Add(new ColumnDefinition());
			grid2.ColumnDefinitions.Add(new ColumnDefinition());

			Button btn = new Button();
			btn.Content = "Submit";
			btn.HorizontalAlignment = HorizontalAlignment.Center;
			btn.IsDefault = true;
			btn.Click += delegate { Close(); };
			grid2.Children.Add(btn); // row & column are 0

			btn = new Button();
			btn.Content = "Cancel";
			btn.HorizontalAlignment = HorizontalAlignment.Center;
			btn.IsCancel = true;
			btn.Click += delegate { Close(); };
			grid2.Children.Add(btn);
			Grid.SetColumn(btn, 1); // row is 0

			(stack.Children[0] as Panel).Children[1].Focus();
		}
	}
}

