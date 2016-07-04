using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.ComponentModel.Composition;

using Illusion;

namespace YoZuo.Workbench
{
	[MenuPart(BaseMenu = WorkbenchName.LanguageGroup, PreviousMenu = WorkbenchName.EnglishPart)]
	public class ChinesePart : MenuPart
	{
		public ChinesePart()
			: base(WorkbenchName.ChinesePart)
		{
			Icon = "PinkLarge.png";
		}

		public override void Execute()
		{
			IoC.Get<IResourceService>().ChangeLanguage("zh-cn");
		}
	}
}
