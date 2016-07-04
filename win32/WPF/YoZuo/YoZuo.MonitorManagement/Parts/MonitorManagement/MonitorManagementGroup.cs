using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorManagement
{
	[MenuPart(BaseMenu = WorkbenchName.MonitorManagementTab)]
	public class MonitorManagementGroup : MenuPart
	{
		public MonitorManagementGroup()
			: base(MonitorManagementWorkbenchName.MonitorManagementGroup)
		{
			Icon = "BlueLarge.png";
		}
	}
}
