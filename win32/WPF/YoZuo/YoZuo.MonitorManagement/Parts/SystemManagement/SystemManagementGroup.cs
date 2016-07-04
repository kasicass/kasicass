using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorManagement
{
	[MenuPart(BaseMenu = WorkbenchName.MonitorManagementTab, PreviousMenu = MonitorManagementWorkbenchName.MonitorManagementGroup)]
	public class SystemManagementGroup : MenuPart
	{
		public SystemManagementGroup()
			: base(MonitorManagementWorkbenchName.SystemManagementGroup)
		{

		}
	}
}
