using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.MeetTheDockers
{
	public class MeetTheDockers : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new MeetTheDockers());
		}

		public MeetTheDockers()
		{
			Title = "Meet the Dockers";

			DockPanel dock = new DockPanel();
			Content = dock;

			Menu menu = new Menu();
			MenuItem item = new MenuItem();
			item.Header = "Menu";
			menu.Items.Add(item);

			DockPanel.SetDock(menu, Dock.Top);
			dock.Children.Add(menu);

			ToolBar tool = new ToolBar();
			tool.Header = "Toolbar";

			DockPanel.SetDock(tool, Dock.Top);
			dock.Children.Add(tool);

			StatusBar status = new StatusBar();
			StatusBarItem statitem = new StatusBarItem();
			statitem.Content = "Status";
			status.Items.Add(statitem);

			DockPanel.SetDock(status, Dock.Bottom);
			dock.Children.Add(status);

			ListBox lstbox = new ListBox();
			lstbox.Items.Add("List Box Item");

			DockPanel.SetDock(lstbox, Dock.Left);
			dock.Children.Add(lstbox);

			TextBox txtbox = new TextBox();
			txtbox.AcceptsReturn = true;

			dock.Children.Add(txtbox);
			txtbox.Focus();
		}
	}
}

