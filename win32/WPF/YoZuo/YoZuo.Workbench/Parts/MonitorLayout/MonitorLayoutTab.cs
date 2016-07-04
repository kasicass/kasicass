using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;

namespace YoZuo.Workbench
{
	[MenuPart(PreviousMenu = WorkbenchName.MonitorModuleTab)]
	public class MonitorLayoutTab : MenuPart
	{
		public MonitorLayoutTab()
			: base(WorkbenchName.MonitorLayoutTab)
		{

		}
	}
}
