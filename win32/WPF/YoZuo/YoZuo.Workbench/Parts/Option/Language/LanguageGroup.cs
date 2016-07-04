using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.ComponentModel.Composition;

using Illusion;

namespace YoZuo.Workbench
{
	[MenuPart(BaseMenu = WorkbenchName.OptionTab)]
	public class LanguageGroup : MenuPart
	{
		public LanguageGroup()
			: base(WorkbenchName.LanguageGroup)
		{

		}
	}
}
