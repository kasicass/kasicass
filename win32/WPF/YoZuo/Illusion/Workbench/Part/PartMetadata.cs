using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Text;

namespace Illusion
{
	public interface IPartMetaData
	{
	}

	public interface IMenuPartMetaData : IPartMetaData
	{
		string MenuName { get; }
		string BaseMenu { get; }
		string PreviousMenu { get; }
		string NextMenu { get; }
	}

	public interface IToolBarPartMetaData : IPartMetaData
	{
		string ToolBarName { get; }
		string BaseToolBar { get; }
		string PreviousToolBar { get; }
		string NextToolBar { get; }
	}

	[MetadataAttribute]
	[AttributeUsage(AttributeTargets.Class, AllowMultiple = false)]
	public class MenuPartAttribute : ExportAttribute, IMenuPartMetaData
	{
		public MenuPartAttribute() : base(typeof(IMenuPart)) { }
		public MenuPartAttribute(Type contractType) : base(contractType) { }

		#region IMenuPartMetaData Members

		public string MenuName { get; set; }

		public string BaseMenu { get; set; }

		public string PreviousMenu { get; set; }

		public string NextMenu { get; set; }

		#endregion
	}

	[MetadataAttribute]
	[AttributeUsage(AttributeTargets.Class, AllowMultiple = false)]
	public class ToolBarPartAttribute : ExportAttribute, IToolBarPartMetaData
	{
		public ToolBarPartAttribute() : base(typeof(IToolBarPart)) { }
		public ToolBarPartAttribute(Type contractType) : base(contractType) { }

		#region IToolBarPartMetaData Members

		public string ToolBarName { get; set; }
	
		public string BaseToolBar { get; set; }

		public string PreviousToolBar { get; set; }

		public string NextToolBar { get; set; }

		#endregion
	}
}
