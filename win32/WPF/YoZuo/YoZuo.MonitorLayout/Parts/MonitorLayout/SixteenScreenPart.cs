using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorLayout
{
	[MenuPart(BaseMenu = MonitorLayoutWorkbenchName.VideoLayoutGroup, PreviousMenu = MonitorLayoutWorkbenchName.NineScreenPart)]
	public class SixteenScreenPart : MenuPart
	{
		public SixteenScreenPart()
			: base(MonitorLayoutWorkbenchName.SixteenScreenPart)
		{
			Icon = "OrangeLarge.png";
		}
	}
}
