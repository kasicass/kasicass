using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorLayout
{
	[MenuPart(BaseMenu = MonitorLayoutWorkbenchName.VideoLayoutGroup, PreviousMenu = MonitorLayoutWorkbenchName.SixteenScreenPart)]
	public class TwentyFiveScreenPart : MenuPart
	{
		public TwentyFiveScreenPart()
			: base(MonitorLayoutWorkbenchName.TwentyFiveScreenPart)
		{
			Icon = "GrayLarge.png";
		}
	}
}
