using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Shapes;

namespace Petzold.FormatRichText
{
	public partial class FormatRichText : Window
	{
		ToggleButton[] btnAlignment = new ToggleButton[4];

		void AddParaToolBar(ToolBarTray tray, int band, int index)
		{
			ToolBar toolbar = new ToolBar();
			toolbar.Band = band;
			toolbar.BandIndex = index;
			tray.ToolBars.Add(toolbar);

			toolbar.Items.Add(btnAlignment[0] = CreateButton(TextAlignment.Left, "Align Left", 0, 4));
			toolbar.Items.Add(btnAlignment[1] = CreateButton(TextAlignment.Center, "Center", 2, 2));
			toolbar.Items.Add(btnAlignment[2] = CreateButton(TextAlignment.Right, "Align Right", 4, 0));
			toolbar.Items.Add(btnAlignment[3] = CreateButton(TextAlignment.Justify, "Justify", 0, 0));

			txtbox.SelectionChanged += TextBoxOnSelectionChanged2;
		}

		ToggleButton CreateButton(TextAlignment align, string strToolTip, int offsetLeft, int offsetRight)
		{
			ToggleButton btn = new ToggleButton();
			btn.Tag = align;
			btn.Click += ButtonOnClick;

			Canvas canv = new Canvas();
			canv.Width = 16;
			canv.Height = 16;
			btn.Content = canv;

			for (int i = 0; i < 5; i++)
			{
				Polyline poly = new Polyline();
				poly.Stroke = SystemColors.WindowTextBrush;
				poly.StrokeThickness = 1;

				if ((i & 1) == 0)
				{
					poly.Points = new PointCollection(new Point[] {
						new Point(2, 2+3*i), new Point(14, 2+3*i)
					});
				}
				else
				{
					poly.Points = new PointCollection(new Point[] {
						new Point(2+offsetLeft, 2+3*i), new Point(14-offsetRight, 2+3*i)
					});
				}

				canv.Children.Add(poly);
			}

			ToolTip tip = new ToolTip();
			tip.Content = strToolTip;
			btn.ToolTip = tip;

			return btn;
		}

		void TextBoxOnSelectionChanged2(object sender, RoutedEventArgs args)
		{
			object obj = txtbox.Selection.GetPropertyValue(Paragraph.TextAlignmentProperty);
			if (obj != null && obj is TextAlignment)
			{
				TextAlignment align = (TextAlignment)obj;
				foreach (ToggleButton btn in btnAlignment)
				{
					btn.IsChecked = (align == (TextAlignment)btn.Tag);
				}
			}
			else
			{
				foreach (ToggleButton btn in btnAlignment)
				{
					btn.IsChecked = false;
				}
			}
		}

		void ButtonOnClick(object sender, RoutedEventArgs args)
		{
			ToggleButton btn = args.Source as ToggleButton;
			foreach (ToggleButton btnAlign in btnAlignment)
			{
				btnAlign.IsChecked = (btn == btnAlign);
			}

			TextAlignment align = (TextAlignment)btn.Tag;
			txtbox.Selection.ApplyPropertyValue(Paragraph.TextAlignmentProperty, align);
		}
	}
}

