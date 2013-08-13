using System;
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;

namespace Petzold.RecurseDirectoriesIncrementally
{
	public class DirectoryTreeViewItem : ImagedTreeViewItem
	{
		DirectoryInfo dir;

		public DirectoryTreeViewItem(DirectoryInfo dir)
		{
			this.dir = dir;
			Text = dir.Name;

			SelectedImage = new BitmapImage(new Uri(Path.Combine(Directory.GetCurrentDirectory(), "OpenFolder.bmp")));
			UnselectedImage = new BitmapImage(new Uri(Path.Combine(Directory.GetCurrentDirectory(), "ClosedFolder.bmp")));
		}

		public DirectoryInfo DirectoryInfo
		{
			get { return dir; }
		}

		public void Populate()
		{
			DirectoryInfo[] dirs;

			try
			{
				dirs = dir.GetDirectories();
			}
			catch
			{
				return;
			}

			foreach (DirectoryInfo dirChild in dirs)
			{
				Items.Add(new DirectoryTreeViewItem(dirChild));
			}
		}

		protected override void OnExpanded(RoutedEventArgs args)
		{
			base.OnExpanded(args);
			foreach (object obj in Items)
			{
				DirectoryTreeViewItem item = obj as DirectoryTreeViewItem;
				item.Populate();
			}
		}
	}
}

