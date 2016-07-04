using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorManagement
{
	[MenuPart(BaseMenu = MonitorManagementWorkbenchName.SystemManagementGroup, PreviousMenu = MonitorManagementWorkbenchName.UserManagementPart)]
	public class SystemManagementPart : MenuPart
	{
		public SystemManagementPart()
			: base(MonitorManagementWorkbenchName.SystemManagementPart)
		{
			Icon = "BrownLarge.png";
		}
	}
}
