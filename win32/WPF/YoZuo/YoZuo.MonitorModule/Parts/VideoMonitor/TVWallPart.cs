using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorModule
{
	[MenuPart(BaseMenu = MonitorModuleWorkbenchName.VideoMonitoGroup)]
	public class TVWallPart : MenuPart
	{
		public TVWallPart()
			: base(MonitorModuleWorkbenchName.TVWallPart)
		{
			Icon = "BlueLarge.png";
		}
	}
}
