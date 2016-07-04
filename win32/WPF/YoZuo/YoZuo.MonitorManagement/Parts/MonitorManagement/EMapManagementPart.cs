using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorManagement
{
	[MenuPart(BaseMenu = MonitorManagementWorkbenchName.MonitorManagementGroup)]
	public class EMapManagementPart : MenuPart
	{
		public EMapManagementPart()
			: base(MonitorManagementWorkbenchName.EMapManagementPart)
		{
			Icon = "BlueLarge.png";
		}
	}
}
