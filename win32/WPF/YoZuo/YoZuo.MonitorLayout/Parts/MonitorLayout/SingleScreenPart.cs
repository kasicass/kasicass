using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;
using YoZuo.Workbench;

namespace YoZuo.MonitorLayout
{
	[MenuPart(BaseMenu = MonitorLayoutWorkbenchName.VideoLayoutGroup)]
	public class SingleScreenPart : MenuPart
	{
		public SingleScreenPart()
			: base(MonitorLayoutWorkbenchName.SingleScreenPart)
		{
			Icon = "BlueLarge.png";
		}
	}
}
