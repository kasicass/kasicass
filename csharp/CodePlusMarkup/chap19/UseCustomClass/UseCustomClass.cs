using Petzold.SelectColorFromGrid;
using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.UseCustomClass
{
	public partial class UseCustomClass : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new UseCustomClass());
		}

		public UseCustomClass()
		{
			InitializeComponent();
		}

		void ColorGridBoxOnSelectionChanged(object sender, SelectionChangedEventArgs args)
		{
			ColorGridBox clrbox = args.Source as ColorGridBox;
			Background = (Brush) clrbox.SelectedValue;
		}
	}
}

