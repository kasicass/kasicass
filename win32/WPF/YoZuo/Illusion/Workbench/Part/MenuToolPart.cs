using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Illusion
{
    /// <summary>
    /// A part that can be used as both <see cref="MenuPart"/> and <see cref="ToolBarPart"/>.
    /// </summary>
	public class MenuToolPart : PartBase, IMenuPart, IToolBarPart, IObservableParent<IMenuPart>, IObservableParent<IToolBarPart>
	{
		#region Constructor
		
		public MenuToolPart()
			: base()
		{
		}

		public MenuToolPart(string name)
			: base(name)
		{
		}
		
		#endregion

		#region PartBase

		public override void Execute()
		{
			IStatusBarService statusBar = IoC.Get<IStatusBarService>();
			statusBar.ShowMessage(DisplayName);
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

		#region IPartParent<IMenuPart> Members

		private BindableCollection<IMenuPart> menus = new BindableCollection<IMenuPart>();
		IObservableCollection<IMenuPart> IObservableParent<IMenuPart>.Items
		{
			get { return menus; }
		}
		
		#endregion

		#region IPartParent<IToolBarPart> Memebers

		private BindableCollection<IToolBarPart> toolbars = new BindableCollection<IToolBarPart>();
		IObservableCollection<IToolBarPart> IObservableParent<IToolBarPart>.Items
		{
			get { return toolbars; }
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
			private set
			{
				_isSeparator = value;
				NotifyOfPropertyChange(() => IsSeparator);
			}
		}

		#endregion

		#region ICheckable Members

		public bool IsCheckable
		{
			get;
			protected set;
		}

		private bool _isChecked = false;
		public bool IsChecked
		{
			get
			{
				return _isChecked;
			}
			set
			{
				_isChecked = value;
				NotifyOfPropertyChange(() => IsChecked);
			}
		}

		#endregion
	}
}
