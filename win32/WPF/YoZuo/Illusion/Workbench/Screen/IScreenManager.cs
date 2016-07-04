using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

namespace Illusion
{
    /// <summary>
    /// Denotes a manager class that manage the <see cref="IDockScreen"/>s.
    /// </summary>
	public interface IDockScreenManager
	{
		IObservableCollection<IDockScreen> Screens { get; }
		IObservableCollection<IDockScreen> Documents { get; }

		IDockScreen this[string name] { get; }
	}

    /// <summary>
    /// Default concrete <see cref="IDockScreenManager"/> with manages <see cref="IDockScreen"/> items, it uses MEF to construct the <see cref="IDockScreen"/>s.
    /// </summary>
	public class DockScreenManager : ViewAware, IDockScreenManager, IPartImportsSatisfiedNotification
	{
		#region Constructor

		public DockScreenManager()
		{
		}

		#endregion

		#region Property

		[ImportMany]
        protected Lazy<IDockScreen, IDockScreenMetaData>[] InternalItems { get; set; }

		#endregion

		#region Method

		protected virtual void ConfigScreens()
		{
			if (InternalItems == null || InternalItems.Length == 0)
			{
				return;
			}

			Screens.Clear();
			Screens.AddRange(InternalItems.Select(item =>
                {
                    item.Value.Side = item.Metadata.Side;
                    item.Value.Type = item.Metadata.Type;
                    return item.Value;
                }));
		}

		#endregion

		#region IDockScreenManager Members

		private IObservableCollection<IDockScreen> screens = new BindableCollection<IDockScreen>();
		public IObservableCollection<IDockScreen> Screens
		{
			get { return screens; }
		}

		private IObservableCollection<IDockScreen> documents = new BindableCollection<IDockScreen>();
		public IObservableCollection<IDockScreen> Documents
		{
			get { return documents; }
		}

		public IDockScreen this[string name]
		{
			get
			{
				var screen = Screens.FirstOrDefault(item => string.Equals(item.Name, name));
				if (screen == null)
				{
					throw new ArgumentException("The name of DockScreen is not found, please check the name");
				}
				return screen;
			}
		}

		#endregion

		#region IPartImportsSatisfiedNotification Members

		public void OnImportsSatisfied()
		{
			ConfigScreens();
		}

		#endregion
	}
}
