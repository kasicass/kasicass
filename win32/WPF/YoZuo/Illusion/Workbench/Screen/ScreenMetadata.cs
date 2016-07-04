using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Text;

namespace Illusion
{
	public interface IDockScreenMetaData
	{
        DockType Type { get; }
        DockSide Side { get; }
    }

	[MetadataAttribute]
	[AttributeUsage(AttributeTargets.Class, AllowMultiple = false)]
    public class DockScreenAttribute : ExportAttribute, IDockScreenMetaData
	{
		public DockScreenAttribute() : base(typeof(IDockScreen)) { }
        public DockScreenAttribute(Type contractType) : base(contractType) { }

        #region IDockScreenMetaData Members

        public DockType Type { get; set; }
        public DockSide Side { get; set; }

		#endregion
	}
}
