using System;
using System.Collections.Generic;
using System.IO;

namespace Petzold.TemplateTheTree
{
	public class DiskDirectory
	{
		DirectoryInfo dirinfo;

		public DiskDirectory(DirectoryInfo dirinfo)
		{
			this.dirinfo = dirinfo;
		}

		public string Name
		{
			get { return dirinfo.Name; }
		}

		public List<DiskDirectory> Subdirectories
		{
			get
			{
				List<DiskDirectory> dirs = new List<DiskDirectory>();
				DirectoryInfo[] subdirs;

				try
				{
					subdirs = dirinfo.GetDirectories();
				}
				catch
				{
					return dirs;
				}

				foreach (DirectoryInfo subdir in subdirs)
					dirs.Add(new DiskDirectory(subdir));
				return dirs;
			}
		}
	}
}

