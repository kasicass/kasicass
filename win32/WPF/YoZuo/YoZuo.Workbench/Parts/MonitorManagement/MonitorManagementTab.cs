using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;

namespace YoZuo.Workbench
{
	[MenuPart(PreviousMenu = WorkbenchName.MonitorLayoutTab)]
	public class MonitorManagementTab : MenuPart
	{
		public MonitorManagementTab()
			: base(WorkbenchName.MonitorManagementTab)
		{

		}
	}
}
