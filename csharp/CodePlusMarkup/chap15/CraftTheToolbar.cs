using System;
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;

namespace Petzold.CraftTheToolbar
{
	public class CraftTheToolbar : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new CraftTheToolbar());
		}

		public CraftTheToolbar()
		{
			Title = "Craft the Toolbar";

			RoutedUICommand[] comm = {
				ApplicationCommands.New,
				ApplicationCommands.Open,
				ApplicationCommands.Save,
				ApplicationCommands.Print,
				ApplicationCommands.Cut,
				ApplicationCommands.Copy,
				ApplicationCommands.Paste,
				ApplicationCommands.Delete
			};

			string[] strImages = {
				"NewDocumentHS.png", "OpenHS.png", "SaveHS.png", "PrintHS.png",
				"CutHS.png", "CopyHS.png", "PasteHS.png", "DeleteHS.png"
			};

			DockPanel dock = new DockPanel();
			dock.LastChildFill = false;
			Content = dock;

			ToolBar toolbar = new ToolBar();
			dock.Children.Add(toolbar);
			DockPanel.SetDock(toolbar, Dock.Top);

			for (int i = 0; i < 8; i++)
			{
				if (i == 4)
					toolbar.Items.Add(new Separator());

				Button btn = new Button();
				btn.Command = comm[i];
				toolbar.Items.Add(btn);

				Image img = new Image();
				string fileName = Path.Combine(Directory.GetCurrentDirectory(), strImages[i]);
				img.Source = new BitmapImage(new Uri(fileName));
				img.Stretch = Stretch.None;
				btn.Content = img;

				ToolTip tip = new ToolTip();
				tip.Content = comm[i].Text;
				btn.ToolTip = tip;

				CommandBindings.Add(new CommandBinding(comm[i], ToolBarButtonOnClick));
			}
		}

		void ToolBarButtonOnClick(object sender, ExecutedRoutedEventArgs args)
		{
			RoutedUICommand comm = args.Command as RoutedUICommand;
			MessageBox.Show(comm.Name + " command not yet implemented", Title);
		}
	}
}

