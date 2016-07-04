using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;
using System.Windows;

namespace Illusion
{
	/// <summary>
	/// Denotes the theme object hold the theme resources.
	/// </summary>
	public interface ITheme : ILocalizableDisplay
	{
		IEnumerable<ResourceDictionary> Resources { get; }
	}

	public static class ThemeExtension
	{
		public static ResourceDictionary LoadResourceDictionary(this ITheme theme, Uri uri)
		{
			return Application.LoadComponent(uri) as ResourceDictionary;
		}

		public static bool HaveResource(this ITheme theme)
		{
			return theme.Resources != null && theme.Resources.Count() > 0;
		}
	}

	/// <summary>
	/// Base <see cref="ITheme"/> class for various implementations of <see cref="ITheme"/>.
	/// </summary>
	public abstract class ThemeBase : PropertyChangedBase, ITheme
	{
		public ThemeBase(string name)
		{
			this.AutoUpdate(true);
		}

		#region IDisplay Members

		private string name;
		public string Name
		{
			get { return name; }
			protected set { name = value; NotifyOfPropertyChange(() => Name); }
		}

		#endregion

		#region IHaveDisplayName Members

		private string displayName;
		public string DisplayName
		{
			get { return displayName; }
			set { displayName = value; NotifyOfPropertyChange(() => DisplayName); }
		}

		#endregion

		#region IHandle<LanguageChangedEventArgs> Members

		public void Handle(LanguageChangedMessage message)
		{
			this.UpdateDisplayName();
		}

		#endregion

		#region ITheme Members

		public virtual IEnumerable<ResourceDictionary> Resources
		{
			get { return null; }
		}

		protected ResourceDictionary LoadResourceDictionary(Uri uri)
		{
			return Application.LoadComponent(uri) as ResourceDictionary;
		}

		#endregion
	}

	/// <summary>
	/// Defines the theme service to manage UI theme, theme resources are WPF <see cref="ResourceDictionary"/>.
	/// </summary>
	public interface IThemeService
	{
		ResourceDictionary MainResource { get; }
		IEnumerable<ITheme> Themes { get; }
		ITheme CurrentTheme { get; }
		void ChangeTheme(ITheme theme);
		void ChangeTheme(string themeName);
	}

	/// <summary>
	/// Default implementation of <see cref="IThemeService"/>.
	/// </summary>
	public class ThemeService : IThemeService
	{
		private static ResourceDictionary DefaultResource = new ResourceDictionary();

		[ImportMany]
		public IEnumerable<ITheme> Themes { get; set; }

		public ResourceDictionary MainResource
		{
			get
			{
				if (Application.Current != null)
				{
					return Application.Current.Resources;
				}
				return DefaultResource;
			}
		}

		private ITheme _theme;
		public ITheme CurrentTheme
		{
			get
			{
				return _theme;
			}
			protected set
			{
				if (_theme != value)
				{
					_theme = value;
				}
			}
		}

		public void ChangeTheme(ITheme theme)
		{
			if (theme != CurrentTheme)
			{
				//1. Unload the current theme.
				if (CurrentTheme != null)
				{
					if (CurrentTheme.HaveResource())
					{
						CurrentTheme.Resources.Apply(i => MainResource.MergedDictionaries.Remove(i));
					}

					ICheckable check = CurrentTheme as ICheckable;
					if (check != null && check.IsCheckable)
					{
						check.IsChecked = false;
					}
				}

				//2. Load the new theme.
				if (theme != null)
				{
					if (theme.HaveResource())
					{
						theme.Resources.Apply(i => MainResource.MergedDictionaries.Add(i));
					}

					ICheckable check = theme as ICheckable;
					if (check != null && check.IsCheckable)
					{
						check.IsChecked = true;
					}
				}

				//3. Restore the current theme.
				CurrentTheme = theme;
			}
		}

		public void ChangeTheme(string themeName)
		{
			if (Themes != null)
			{
				var theme = Themes.FirstOrDefault(i => string.Equals(i.Name, themeName));
				if (theme != null)
				{
					ChangeTheme(theme);
				}
			}
		}
	}
}
