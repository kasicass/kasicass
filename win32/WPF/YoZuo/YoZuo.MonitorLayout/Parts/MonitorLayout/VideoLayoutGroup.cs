using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorLayout
{
	[MenuPart(BaseMenu = WorkbenchName.MonitorLayoutTab)]
	public class VideoLayoutGroup : MenuPart
	{
		public VideoLayoutGroup()
			: base(MonitorLayoutWorkbenchName.VideoLayoutGroup)
		{
			Icon = "BlueLarge.png";
		}
	}
}
