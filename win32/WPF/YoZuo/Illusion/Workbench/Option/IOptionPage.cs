using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Illusion
{
    /// <summary>
    /// Denotes an option page which contains the options.
    /// </summary>
	public interface IOptionPage : ILocalizableDisplay
	{
		void Cancel();
		void Commit();
		void Load(IConfigurationObject value);
	}

    /// <summary>
    /// A base class for various implementations of <see cref="IOptionPage"/>.
    /// </summary>
	public abstract class OptionPageBase : PropertyChangedBase, IOptionPage
	{
		public OptionPageBase(string name)
		{
			Name = name;
			this.AutoUpdate(true);
		}

		#region IOptionPage Members

		public virtual void Cancel()
		{
			//Do nothing by default
		}

		public abstract void Commit();

		public void Load(IConfigurationObject value)
		{
			throw new NotImplementedException();
		}

		#endregion

		#region IHaveName Members

		private string _name = string.Empty;
		public string Name
		{
			get
			{
				return _name;
			}
			set
			{
				_name = value;
				NotifyOfPropertyChange(() => Name);
			}
		}

		#endregion

		#region IHaveDisplayName Members

		private string _displayName = string.Empty;
		public string DisplayName
		{
			get
			{
				return _displayName;
			}
			set
			{
				_displayName = value;
				NotifyOfPropertyChange(() => DisplayName);
			}
		}

		#endregion

		#region IHandle<LanguageChangedMessage> Members

		public void Handle(LanguageChangedMessage message)
		{
			this.UpdateDisplayName();
		}

		#endregion
	}
}
