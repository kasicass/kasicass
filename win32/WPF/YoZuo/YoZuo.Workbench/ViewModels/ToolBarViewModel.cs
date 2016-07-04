using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;

namespace YoZuo.Workbench.ViewModels
{
	[Export(typeof(IPartManager<IToolBarPart>))]
	public class ToolBarViewModel : PartManager<IToolBarPart, IToolBarPartMetaData>
	{
	}
}
