using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorManagement
{
	[MenuPart(BaseMenu = MonitorManagementWorkbenchName.MonitorManagementGroup, PreviousMenu = MonitorManagementWorkbenchName.DeviceManagementPart)]
	public class IntelligentAnalysisManagementPart : MenuPart
	{
		public IntelligentAnalysisManagementPart()
			: base(MonitorManagementWorkbenchName.IntelligentAnalysisManagementPart)
		{
			Icon = "BlueLarge.png";
		}
	}
}
