using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorModule
{
	[MenuPart(BaseMenu = MonitorModuleWorkbenchName.VideoHandleGroup)]
	public class VideoIncreasePart : MenuPart
	{
		public VideoIncreasePart()
			: base(MonitorModuleWorkbenchName.VideoIncreasePart)
		{
			Icon = "BlueLarge.png";
		}
	}
}
