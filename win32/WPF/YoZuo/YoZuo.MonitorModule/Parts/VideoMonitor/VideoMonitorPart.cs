using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorModule
{
	[MenuPart(BaseMenu = WorkbenchName.MonitorModuleTab)]
	public class VideoMonitorPart : MenuPart
	{
		public VideoMonitorPart()
			: base(MonitorModuleWorkbenchName.VideoMonitoGroup)
		{
			Icon = "BlueLarge.png";
		}
	}
}
