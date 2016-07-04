using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorModule
{
	[MenuPart(BaseMenu = MonitorModuleWorkbenchName.VideoMonitoGroup)]
	public class EMapPart : MenuPart
	{
		public EMapPart()
			: base(MonitorModuleWorkbenchName.EMapPart)
		{
			Icon = "BlueLarge.png";
		}
	}
}
