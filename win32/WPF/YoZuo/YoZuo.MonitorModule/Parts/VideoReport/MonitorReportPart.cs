using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorModule
{
	[MenuPart(BaseMenu = MonitorModuleWorkbenchName.VideoReportPart)]
	public class MonitorReportPart : MenuPart
	{
		public MonitorReportPart()
			: base(MonitorModuleWorkbenchName.MonitorReportPart)
		{
			Icon = "BlueLarge.png";
		}
	}
}
