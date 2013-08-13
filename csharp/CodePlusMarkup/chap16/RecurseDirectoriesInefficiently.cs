using System;
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.RecurseDirectoriesInefficiently
{
	public class RecurseDirectoriesInefficiently : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new RecurseDirectoriesInefficiently());
		}

		public RecurseDirectoriesInefficiently()
		{
			Title = "Recurse Directories Inefficiently";

			TreeView tree = new TreeView();
			Content = tree;

			TreeViewItem item = new TreeViewItem();
			item.Header = Path.GetPathRoot(Environment.SystemDirectory);
			item.Tag = new DirectoryInfo(item.Header as string);
			tree.Items.Add(item);

			GetSubdirectories(item);
		}

		void GetSubdirectories(TreeViewItem item)
		{
			DirectoryInfo dir = item.Tag as DirectoryInfo;
			DirectoryInfo[] subdirs;

			try
			{
				subdirs = dir.GetDirectories();
			}
			catch
			{
				return;
			}

			foreach (DirectoryInfo subdir in subdirs)
			{
				TreeViewItem subitem = new TreeViewItem();
				subitem.Header = subdir.Name;
				subitem.Tag = subdir;
				item.Items.Add(subitem);

				GetSubdirectories(subitem);
			}
		}
	}
}

