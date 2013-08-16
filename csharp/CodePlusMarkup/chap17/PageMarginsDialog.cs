using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;

namespace Petzold.PrintWithMargins
{
	class PageMarginsDialog : Window
	{
		enum Side
		{
			Left, Right, Top, Bottom
		}

		TextBox[] txtbox = new TextBox[4];
		Button btnOk;

		public Thickness PageMargins
		{
			set
			{
				txtbox[(int)Side.Left].Text = (value.Left/96).ToString("F3");
				txtbox[(int)Side.Right].Text = (value.Right/96).ToString("F3");
				txtbox[(int)Side.Top].Text = (value.Top/96).ToString("F3");
				txtbox[(int)Side.Bottom].Text = (value.Bottom/96).ToString("F3");
			}
			get
			{
				return new Thickness(Double.Parse(txtbox[(int)Side.Left].Text)*96,
					Double.Parse(txtbox[(int)Side.Top].Text),
					Double.Parse(txtbox[(int)Side.Right].Text),
					Double.Parse(txtbox[(int)Side.Bottom].Text));
			}
		}

		public PageMarginsDialog()
		{
			Title = "Page Setup";
			ShowInTaskbar = false;
			WindowStyle = WindowStyle.ToolWindow;
			WindowStartupLocation = WindowStartupLocation.CenterOwner;
			SizeToContent = SizeToContent.WidthAndHeight;
			ResizeMode = ResizeMode.NoResize;

			StackPanel stack = new StackPanel();
			Content = stack;

			GroupBox grpbox = new GroupBox();
			grpbox.Header = "Margins (inches)";
			grpbox.Margin = new Thickness(12);
			stack.Children.Add(grpbox);

			Grid grid = new Grid();
			grid.Margin = new Thickness(6);
			grpbox.Content = grid;

			for (int i = 0; i < 2; i++)
			{
				RowDefinition rowdef = new RowDefinition();
				rowdef.Height = GridLength.Auto;
				grid.RowDefinitions.Add(rowdef);
			}

			for (int i = 0; i < 4; i++)
			{
				ColumnDefinition coldef = new ColumnDefinition();
				coldef.Width = GridLength.Auto;
				grid.ColumnDefinitions.Add(coldef);
			}

			for (int i = 0; i < 4; i++)
			{
				Label lbl = new Label();
				lbl.Content = "_" + Enum.GetName(typeof(Side), i) + ":";
				lbl.Margin = new Thickness(6);
				lbl.VerticalAlignment = VerticalAlignment.Center;
				grid.Children.Add(lbl);
				Grid.SetRow(lbl, i/2);
				Grid.SetColumn(lbl, 2*(i%2));

				txtbox[i] = new TextBox();
				txtbox[i].TextChanged += TextBoxOnTextChanged;
				txtbox[i].MinWidth = 48;
				txtbox[i].Margin = new Thickness(6);
				grid.Children.Add(txtbox[i]);
				Grid.SetRow(txtbox[i], i/2);
				Grid.SetColumn(txtbox[i], 2*(i%2)+1);
			}

			UniformGrid unigrid = new UniformGrid();
			unigrid.Rows = 1;
			unigrid.Columns = 2;
			stack.Children.Add(unigrid);

			btnOk = new Button();
			btnOk.Content = "OK";
			btnOk.IsDefault = true;
			btnOk.IsEnabled = false;
			btnOk.MinWidth = 60;
			btnOk.Margin = new Thickness(12);
			btnOk.HorizontalAlignment = HorizontalAlignment.Center;
			btnOk.Click += OkButtonOkClick;
			unigrid.Children.Add(btnOk);

			Button btnCancel = new Button();
			btnCancel.Content = "Cancel";
			btnCancel.IsCancel = true;
			btnCancel.MinWidth = 60;
			btnCancel.Margin = new Thickness(12);
			btnCancel.HorizontalAlignment = HorizontalAlignment.Center;
			unigrid.Children.Add(btnCancel);
		}

		void TextBoxOnTextChanged(object sender, TextChangedEventArgs args)
		{
			double result;
			btnOk.IsEnabled = Double.TryParse(txtbox[(int)Side.Left].Text, out result) &&
				Double.TryParse(txtbox[(int)Side.Right].Text, out result) &&
				Double.TryParse(txtbox[(int)Side.Top].Text, out result) &&
				Double.TryParse(txtbox[(int)Side.Bottom].Text, out result);
		}

		void OkButtonOkClick(object sender, RoutedEventArgs args)
		{
			DialogResult = true;
		}
	}
}

