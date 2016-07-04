using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Windows.Media;

namespace Illusion
{
	/// <summary>
	/// Base <see cref="IPart"/> class for various implementations of <see cref="IPart"/>.
	/// </summary>
	public abstract class PartBase : PropertyChangedBase, IPart
	{
		protected PartBase()
			:this(null)
		{
		}

		protected PartBase(string name)
		{
			this.Name = name ?? GetType().Name.Replace("Part", string.Empty);
			this.execute = ((i) => { });
			this.canExecute = (() => IsActive);

		    this.AutoUpdate(true);
		}

		public PartBase(string name, System.Action<PartBase> execute, Func<bool> canExecute = null)
			: this(name)
		{
			this.execute = execute ?? ((i) => { });
			this.canExecute = canExecute ?? (() => IsActive);
		}

		private string name;
		public string Name
		{
			get { return name; }
			protected set { name = value; NotifyOfPropertyChange(() => Name); }
		}

		private string displayName;
		public string DisplayName
		{
			get { return displayName; }
			set { displayName = value; NotifyOfPropertyChange(() => DisplayName); }
		}

		private string icon;
		public string Icon
		{
			get { return icon; }
			set { icon = value; NotifyOfPropertyChange(() => Icon); }
		}

		private string inputGestureText;
		public string InputGestureText
		{
			get { return inputGestureText; }
			set { inputGestureText = value; NotifyOfPropertyChange(() => InputGestureText); }
		}

		private bool isVisible = true;
		public bool IsVisible
		{
			get { return isVisible; }
			set { isVisible = value; NotifyOfPropertyChange(() => IsVisible); }
		}

		public virtual void OnAttached()
		{ }

		#region IExecutable

		private readonly System.Action<PartBase> execute;
		/// <summary>
		/// The action associated to the ActionItem
		/// </summary>
		public virtual void Execute()
		{
			this.execute(this);
		}

		private readonly Func<bool> canExecute;
		/// <summary>
		/// Calls the underlying canExecute function.
		/// </summary>
		public virtual bool CanExecute
		{
			get { return canExecute(); }
		}
		#endregion

		#region Activation & Deactivation
		public event EventHandler<ActivationEventArgs> Activated;
		public event EventHandler<DeactivationEventArgs> AttemptingDeactivation;
		public event EventHandler<DeactivationEventArgs> Deactivated;

		private bool isActive = true;
		public bool IsActive
		{
			get { return isActive; }
		}

		public void Activate()
		{
			if (IsActive)
				return;

			isActive = true;
			OnActivate();
			if (Activated != null)
				Activated(this, new ActivationEventArgs { WasInitialized = false });
			NotifyOfPropertyChange(() => CanExecute);
		}
		protected virtual void OnActivate() { }

		public virtual void Deactivate(bool close)
		{
			if (!IsActive)
				return;

			if (AttemptingDeactivation != null)
				AttemptingDeactivation(this, new DeactivationEventArgs { WasClosed = close });

			isActive = false;
			OnDeactivate(close);
			NotifyOfPropertyChange(() => CanExecute);
			if (Deactivated != null)
				Deactivated(this, new DeactivationEventArgs { WasClosed = close });
		}
		protected virtual void OnDeactivate(bool close) { }

		#endregion

		#region IHandle<LanguageChangedEventArgs> Members

		public void Handle(LanguageChangedMessage message)
		{
			this.UpdateDisplayName();
		}

		#endregion
	}
}
