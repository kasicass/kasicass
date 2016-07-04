using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Illusion
{
    /// <summary>
    /// A toolbar part for various implementations of <see cref="IToolBarPart"/>.
    /// </summary>
	public class ToolBarPart : PartBase, IToolBarPart, IObservableParent<IToolBarPart>
	{
		#region Constructor
		
		public ToolBarPart(string name)
			: base(name)
		{
		}
		
		#endregion

		#region IToolBarPart Members

		private string toolTip;
		public string ToolTip
		{
			get { return toolTip; }
			set { toolTip = value; NotifyOfPropertyChange(() => ToolTip); }
		}

		private IObservableCollection<IToolBarPart> items = new BindableCollection<IToolBarPart>();
		IObservableCollection<IToolBarPart> IObservableParent<IToolBarPart>.Items
		{
			get { return items; }
		}

		#endregion

		#region ISeparaterPart Members

		private bool _isSeparator = false;
		public bool IsSeparator
		{
			get
			{
				return _isSeparator;
			}
			protected set
			{
				_isSeparator = value;
				NotifyOfPropertyChange(() => IsSeparator);
			}
		}

		#endregion
	}

	public class ToolBarButton : PartBase, IToolBarPart
	{
		#region Constructor
		
		public ToolBarButton(string name)
			: base(name)
		{
		}
		
		#endregion

		#region IToolBarPart Members

		private string toolTip;
		public string ToolTip
		{
			get { return toolTip; }
			set { toolTip = value; NotifyOfPropertyChange(() => ToolTip); }
		}

		#endregion

		#region ISeparaterPart Members

		public bool IsSeparator
		{
			get
			{
				return false;
			}
		}

		#endregion
	}
}
