using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;

namespace YoZuo.Workbench
{
	[MenuPart(PreviousMenu = WorkbenchName.MonitorManagementTab)]
	public class MonitorSystemTab : MenuPart
	{
		public MonitorSystemTab()
			: base(WorkbenchName.MonitorSystemTab)
		{

		}
	}
}
