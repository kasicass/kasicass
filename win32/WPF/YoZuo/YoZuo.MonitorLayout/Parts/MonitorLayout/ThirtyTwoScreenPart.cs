using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorLayout
{
	[MenuPart(BaseMenu = MonitorLayoutWorkbenchName.VideoLayoutGroup, PreviousMenu = MonitorLayoutWorkbenchName.TwentyFiveScreenPart)]
	public class ThirtyTwoScreenPart : MenuPart
	{
		public ThirtyTwoScreenPart()
			: base(MonitorLayoutWorkbenchName.ThirtyTwoScreenPart)
		{
			Icon = "BrownLarge.png";
		}
	}
}
