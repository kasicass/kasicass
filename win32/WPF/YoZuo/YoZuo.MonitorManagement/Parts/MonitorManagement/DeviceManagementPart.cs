using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorManagement
{
	[MenuPart(BaseMenu = MonitorManagementWorkbenchName.MonitorManagementGroup, PreviousMenu = MonitorManagementWorkbenchName.TVWallManagementPart)]
	public class DeviceManagementPart : MenuPart
	{
		public DeviceManagementPart()
			: base(MonitorManagementWorkbenchName.DeviceManagementPart)
		{
			Icon = "BlueLarge.png";
		}
	}
}
