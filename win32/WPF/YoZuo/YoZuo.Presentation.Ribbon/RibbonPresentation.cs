using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Text;

using Illusion;

namespace YoZuo.Presentation.Ribbon
{
	[Export(typeof(IPresentation))]
	public class RibbonPresentation : IPresentation
	{
		private NameTransformer.Rule _rule;

		public RibbonPresentation()
		{
			_rule = new NameTransformer.Rule()
			{
				ReplacePattern = @"(?<nsbefore>([A-Za-z_]\w*\.)*)?(?<nsvm>ViewModels\.)(?<nsafter>[A-Za-z_]\w*\.)*(?<basename>[A-Za-z_]\w*)(?<suffix>ViewModel$)",
				ReplacementValues = new[] { @"YoZuo.Presentation.Ribbon.${nsafter}${basename}View" },
				GlobalFilterPattern = @"(([A-Za-z_]\w*\.)*)?ViewModels\.([A-Za-z_]\w*\.)*[A-Za-z_]\w*ViewModel$"
			};
		}

		#region IPresentation Members

		public void Attach()
		{
			ViewLocator.NameTransformer.AddRule(_rule.ReplacePattern, _rule.ReplacementValues, _rule.GlobalFilterPattern);
		}

		public void Detach()
		{
			var rule = ViewLocator.NameTransformer.FirstOrDefault(item => 
				(item.ReplacePattern == _rule.ReplacePattern) && (item.GlobalFilterPattern == _rule.GlobalFilterPattern));
			if (rule != null)
			{
				ViewLocator.NameTransformer.Remove(rule);
			}
		}

		#endregion

		#region IHaveName Members

		public string Name
		{
			get
			{
				return "PART_OPTION_PRESENTATION_RIBBON";
			}
		}

		#endregion
	}
}
