using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorLayout
{
	[MenuPart(BaseMenu = MonitorLayoutWorkbenchName.VideoLayoutGroup, PreviousMenu = MonitorLayoutWorkbenchName.SingleScreenPart)]
	public class FourScreenPart : MenuPart
	{
		public FourScreenPart()
			: base(MonitorLayoutWorkbenchName.FourScreenPart)
		{
			Icon = "GrayLarge.png";
		}
	}
}
