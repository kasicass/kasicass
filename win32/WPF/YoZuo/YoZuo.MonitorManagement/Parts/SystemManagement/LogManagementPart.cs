using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorManagement
{
	[MenuPart(BaseMenu = MonitorManagementWorkbenchName.SystemManagementGroup, PreviousMenu = MonitorManagementWorkbenchName.SystemManagementPart)]
	public class LogManagementPart : MenuPart
	{
		public LogManagementPart()
			: base(MonitorManagementWorkbenchName.LogManagementPart)
		{
			Icon = "BlueLarge.png";
		}
	}
}
