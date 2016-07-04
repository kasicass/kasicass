using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorManagement
{
	[MenuPart(BaseMenu = MonitorManagementWorkbenchName.MonitorManagementGroup, PreviousMenu = MonitorManagementWorkbenchName.IntelligentAnalysisManagementPart)]
	public class VideoCompressManagementPart : MenuPart
	{
		public VideoCompressManagementPart()
			: base(MonitorManagementWorkbenchName.VideoCompressManagementPart)
		{
			Icon = "BlueLarge.png";
		}
	}
}
