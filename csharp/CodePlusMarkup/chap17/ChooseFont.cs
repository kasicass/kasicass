using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.ChooseFont
{
	public class ChooseFont : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new ChooseFont());
		}

		public ChooseFont()
		{
			Title = "Choose Font";
			Button btn = new Button();
			btn.Content = Title;
			btn.HorizontalAlignment = HorizontalAlignment.Center;
			btn.VerticalAlignment = VerticalAlignment.Center;
			btn.Click += ButtonOnClick;
			Content = btn;
		}

		void ButtonOnClick(object sender, RoutedEventArgs args)
		{
			FontDialog dlg = new FontDialog();
			dlg.Owner = this;
			dlg.Typeface = new Typeface(FontFamily, FontStyle, FontWeight, FontStretch);
			dlg.FontSize = FontSize;
			if (dlg.ShowDialog().GetValueOrDefault())
			{
				FontFamily = dlg.Typeface.FontFamily;
				FontStyle = dlg.Typeface.Style;
				FontWeight = dlg.Typeface.Weight;
				FontStretch = dlg.Typeface.Stretch;
				FontSize = dlg.FaceSize;
			}
		}
	}
}

