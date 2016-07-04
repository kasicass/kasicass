using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.ComponentModel.Composition;

using Illusion;

namespace YoZuo.Workbench
{
	[MenuPart(BaseMenu = WorkbenchName.LanguageGroup)]
	public class EnglishPart : MenuPart
	{
		public EnglishPart()
			: base(WorkbenchName.EnglishPart)
		{
			Icon = "OrangeLarge.png";
		}

        public override void Execute()
        {
			IoC.Get<IResourceService>().ChangeLanguage("en-us");
		}
	}
}
