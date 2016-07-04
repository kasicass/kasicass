using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorManagement
{
	[MenuPart(BaseMenu = MonitorManagementWorkbenchName.MonitorManagementGroup, PreviousMenu = MonitorManagementWorkbenchName.VideoSearchManagementPart)]
	public class MonitorReportManagementPart : MenuPart
	{
		public MonitorReportManagementPart()
			: base(MonitorManagementWorkbenchName.MonitorReportManagementPart)
		{
			Icon = "OrangeLarge.png";
		}
	}
}
