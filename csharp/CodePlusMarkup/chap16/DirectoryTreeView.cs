using System;
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;

namespace Petzold.RecurseDirectoriesIncrementally
{
	public class DirectoryTreeView : TreeView
	{
		public DirectoryTreeView()
		{
			RefreshTree();
		}

		public void RefreshTree()
		{
			BeginInit();
			Items.Clear();

			DriveInfo[] drives = DriveInfo.GetDrives();
			foreach (DriveInfo drive in drives)
			{
				char chDrive = drive.Name.ToUpper()[0];
				DirectoryTreeViewItem item = new DirectoryTreeViewItem(drive.RootDirectory);

				if (chDrive != 'A' && chDrive != 'B' && drive.IsReady && drive.VolumeLabel.Length > 0)
					item.Text = String.Format("{0}({1})", drive.VolumeLabel, drive.Name);
				else
					item.Text = String.Format("{0}({1})", drive.DriveType, drive.Name);

				if (chDrive == 'A' || chDrive == 'B')
					item.SelectedImage = item.UnselectedImage = new BitmapImage(new Uri(Path.Combine(Directory.GetCurrentDirectory(), "35Floppy.bmp")));
				else if (drive.DriveType == DriveType.CDRom)
					item.SelectedImage = item.UnselectedImage = new BitmapImage(new Uri(Path.Combine(Directory.GetCurrentDirectory(), "CDDrive.bmp")));
				else
					item.SelectedImage = item.UnselectedImage = new BitmapImage(new Uri(Path.Combine(Directory.GetCurrentDirectory(), "Drive.bmp")));

				Items.Add(item);
				if (chDrive != 'A' && chDrive != 'B' && drive.IsReady)
					item.Populate();
			}
			EndInit();
		}
	}
}

