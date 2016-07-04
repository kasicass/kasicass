using System;
using System.IO;
using System.Linq;
using System.ComponentModel.Composition;
using System.Windows;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.ViewModels
{
	[Export(typeof(IShell))]
	public class ShellViewModel : Screen, IShell
	{
		public ShellViewModel()
		{
			CacheViews = false;
		}

		[Import]
		public IPartManager<IMenuPart> TabRegion { get; set; }

		[Import]
		public IStatusBarService StatusBarRegion { get; set; }
	}
}
