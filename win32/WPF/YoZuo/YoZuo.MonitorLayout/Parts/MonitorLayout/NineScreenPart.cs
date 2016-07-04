using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorLayout
{
	[MenuPart(BaseMenu = MonitorLayoutWorkbenchName.VideoLayoutGroup, PreviousMenu = MonitorLayoutWorkbenchName.FourScreenPart)]
	public class NineScreenPart : MenuPart
	{
		public NineScreenPart()
			: base(MonitorLayoutWorkbenchName.NineScreenPart)
		{
			Icon = "PinkLarge.png";
		}
	}
}
