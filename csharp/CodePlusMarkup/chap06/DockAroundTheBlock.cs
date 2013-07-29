using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.DockAroundTheBlock
{
	class DockAroundTheBlock : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new DockAroundTheBlock());
		}

		public DockAroundTheBlock()
		{
			Title = "Dock Around the Block";

			DockPanel dock = new DockPanel();
			Content = dock;
			for (int i = 0; i < 17; i++)
			{
				Button btn = new Button();
				btn.Content = "Button No. " + (i+1);
				dock.Children.Add(btn);
				btn.SetValue(DockPanel.DockProperty, (Dock)(i%4));
			}
		}
	}
}

