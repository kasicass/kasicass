using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.SetSpaceProperty
{
	public class SetSpaceProperty : SpaceWindow
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new SetSpaceProperty());
		}

		public SetSpaceProperty()
		{
			Title = "Set Space Property";
			SizeToContent = SizeToContent.WidthAndHeight;
			ResizeMode = ResizeMode.CanMinimize;
			int[] iSpaces = {0, 1, 2};

			Grid grid = new Grid();
			Content = grid;

			for (int i = 0; i < 2; i++)
			{
				RowDefinition row = new RowDefinition();
				row.Height = GridLength.Auto;
				grid.RowDefinitions.Add(row);
			}

			for (int i = 0; i < iSpaces.Length; i++)
			{
				ColumnDefinition col = new ColumnDefinition();
				col.Width = GridLength.Auto;
				grid.ColumnDefinitions.Add(col);
			}

			for (int i = 0; i < iSpaces.Length; i++)
			{
				SpaceButton btn = new SpaceButton();
				btn.Text = "Set window Space to " + iSpaces[i];
				btn.Tag = iSpaces[i];
				btn.HorizontalAlignment = HorizontalAlignment.Center;
				btn.VerticalAlignment = VerticalAlignment.Center;
				btn.Click += WindowPropertyOnClick;
				grid.Children.Add(btn);
				Grid.SetRow(btn, 0);
				Grid.SetColumn(btn, i);

				btn = new SpaceButton();
				btn.Text = "Set button Space to " + iSpaces[i];
				btn.Tag = iSpaces[i];
				btn.HorizontalAlignment = HorizontalAlignment.Center;
				btn.VerticalAlignment = VerticalAlignment.Center;
				btn.Click += ButtonPropertyOnClick;
				grid.Children.Add(btn);
				Grid.SetRow(btn, 1);
				Grid.SetColumn(btn, i);
			}
		}

		void WindowPropertyOnClick(object sender, RoutedEventArgs args)
		{
			SpaceButton btn = args.Source as SpaceButton;
			Space = (int)btn.Tag;
		}

		void ButtonPropertyOnClick(object sneder, RoutedEventArgs args)
		{
			SpaceButton btn = args.Source as SpaceButton;
			btn.Space = (int)btn.Tag;
		}
	}
}

