using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;
using System.Windows;
using Illusion;

namespace YoZuo.Workbench.ViewModels
{
	[Export(typeof(IPartManager<IMenuPart>))]
	public class TabViewModel : PartManager<IMenuPart, IMenuPartMetaData>
	{
	}
}
