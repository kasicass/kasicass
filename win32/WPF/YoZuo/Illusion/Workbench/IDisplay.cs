using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Illusion
{
	/// <summary>
	/// Denotes an instance which has a name.
	/// </summary>
	public interface IHaveName
	{
		string Name { get; }
	}

	/// <summary>
    /// Denotes an instance which implements <see cref="IHaveName"/> and <see cref="IHaveDisplayName"/>.
	/// </summary>
	public interface IDisplay : IHaveName, IHaveDisplayName
	{
	}

	/// <summary>
	/// Denotes an extend <see cref="IDisplay"/> which is localizable to update its DisplayName.
	/// </summary>
	public interface ILocalizableDisplay : IDisplay, IHandle<LanguageChangedMessage>
	{

	}

	public static class ILocalizableDisplayExtension
	{
		public static void AutoUpdate(this ILocalizableDisplay display, bool isAutoUpdate)
		{
			IEventAggregator eventAggregator = IoC.Get<IEventAggregator>();
			if (isAutoUpdate)
			{
				eventAggregator.Subscribe(display);
				display.UpdateDisplayName();
			}
			else
			{
				eventAggregator.Unsubscribe(display);
			}

		}

		public static void UpdateDisplayName(this ILocalizableDisplay display)
		{
			display.DisplayName = IoC.Get<IResourceService>().GetString(display.Name);
		}
	}
}
