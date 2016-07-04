using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorModule
{
	[MenuPart(BaseMenu=WorkbenchName.MonitorModuleTab)]
	public class VideoHandleGroup : MenuPart
	{
		public VideoHandleGroup()
			: base(MonitorModuleWorkbenchName.VideoHandleGroup)
		{
			Icon = "BlueLarge.png";
		}
	}
}
