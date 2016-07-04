using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorManagement
{
	[MenuPart(BaseMenu = MonitorManagementWorkbenchName.SystemManagementGroup)]
	public class UserManagementPart : MenuPart
	{
		public UserManagementPart()
			: base(MonitorManagementWorkbenchName.UserManagementPart)
		{
			Icon = "GrayLarge.png";
		}
	}
}
