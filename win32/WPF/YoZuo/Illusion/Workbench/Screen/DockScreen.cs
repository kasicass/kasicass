using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Illusion
{
    public enum DockSide
    {
        None,
        Left,
        Top,
        Right,
        Bottom
    }

	public enum DockType
	{
		Floating,
		Document,
		DockableContent,
	}

    /// <summary>
    /// Denotes the docked <see cref="IScreen"/> that can display is the side of window.
    /// </summary>
	public interface IDockScreen : IScreen, ILocalizableDisplay
	{
		string Icon { get; set; }
        DockType Type { get; set; }
        DockSide Side { get; set; }
		void Show();
		void Close();
	}

    /// <summary>
    /// Denotes the document screen.
    /// </summary>
	public interface IDockDocumentScreen : IDockScreen
	{

	}

    /// <summary>
    /// A base class for various implementations of <see cref="IDockScreen"/>.
    /// </summary>
	public abstract class DockScreenBase : Screen, IDockScreen
	{
		#region Constructor
		
		public DockScreenBase(string name)
		{
			Name = name;
			this.AutoUpdate(true);
		}
		
		#endregion

		#region IDockScreen

		private string icon;
		public string Icon
		{
			get { return icon; }
			set { icon = value; NotifyOfPropertyChange(() => Icon); }
		}

		private string name;
		public string Name
		{
			get { return name; }
			set { name = value; NotifyOfPropertyChange(() => Name); }
		}

        private DockType type;
        public DockType Type
        {
            get { return type; }
            set
            {
                if (type != value)
                {
                    type = value;
                    NotifyOfPropertyChange(() => Type);
                }
            }
        }

        private DockSide side;
        public DockSide Side
        {
            get { return side; }
            set
            {
                if (side != value)
                {
                    side = value;
                    NotifyOfPropertyChange(() => Side);
                }
            }
        }

		public abstract void Show();
		public abstract void Close();

		#endregion

		#region IHandle<LanguageChangedEventArgs> Members

		public void Handle(LanguageChangedMessage message)
		{
			this.UpdateDisplayName();
		}

		#endregion
	}
}
