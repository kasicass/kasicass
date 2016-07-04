using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorManagement
{
	[MenuPart(BaseMenu = MonitorManagementWorkbenchName.MonitorManagementGroup, PreviousMenu = MonitorManagementWorkbenchName.EMapManagementPart)]
	public class TVWallManagementPart : MenuPart
	{
		public TVWallManagementPart()
			: base(MonitorManagementWorkbenchName.TVWallManagementPart)
		{
			Icon = "BrownLarge.png";
		}
	}
}
