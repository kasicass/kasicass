using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Input;
using System.Windows.Interactivity;

namespace Illusion.Views
{
    /// <summary>
    /// Global Behavior class used to attach behaviors.
    /// </summary>
	public class Behavior
	{
		#region AutoMergeStyle

		/// <summary>
		/// AutoMergeStyle Attached Dependency Property
		/// </summary>
		public static readonly DependencyProperty AutoMergeStyleProperty =
			DependencyProperty.RegisterAttached("AutoMergeStyle", typeof(bool), typeof(Behavior),
				new FrameworkPropertyMetadata((bool)false,
					new PropertyChangedCallback(OnAutoMergeStyleChanged)));

		/// <summary>
		/// Gets the AutoMergeStyle property.  This dependency property 
		/// indicates ....
		/// </summary>
		public static bool GetAutoMergeStyle(DependencyObject d)
		{
			return (bool)d.GetValue(AutoMergeStyleProperty);
		}

		/// <summary>
		/// Sets the AutoMergeStyle property.  This dependency property 
		/// indicates ....
		/// </summary>
		public static void SetAutoMergeStyle(DependencyObject d, bool value)
		{
			d.SetValue(AutoMergeStyleProperty, value);
		}

		/// <summary>
		/// Handles changes to the AutoMergeStyle property.
		/// </summary>
		private static void OnAutoMergeStyleChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
		{
			if (e.OldValue == e.NewValue)
			{
				return;
			}

			Control control = d as Control;
			if (control == null)
			{
				throw new NotSupportedException("AutoMergeStyle can only used in Control");
			}

			if ((bool)e.NewValue)
			{
				Type type = d.GetType();
				control.SetResourceReference(Behavior.BaseOnStyleProperty, type);
			}
			else
			{
				control.ClearValue(Behavior.BaseOnStyleProperty);
			}
		}

		#endregion

		#region BaseOnStyle

		/// <summary>
		/// BaseOnStyle Attached Dependency Property
		/// </summary>
		public static readonly DependencyProperty BaseOnStyleProperty =
			DependencyProperty.RegisterAttached("BaseOnStyle", typeof(Style), typeof(Behavior),
				new FrameworkPropertyMetadata((Style)null,
					new PropertyChangedCallback(OnBaseOnStyleChanged)));

		/// <summary>
		/// Gets the BaseOnStyle property.  This dependency property 
		/// indicates ....
		/// </summary>
		public static Style GetBaseOnStyle(DependencyObject d)
		{
			return (Style)d.GetValue(BaseOnStyleProperty);
		}

		/// <summary>
		/// Sets the BaseOnStyle property.  This dependency property 
		/// indicates ....
		/// </summary>
		public static void SetBaseOnStyle(DependencyObject d, Style value)
		{
			d.SetValue(BaseOnStyleProperty, value);
		}

		/// <summary>
		/// Handles changes to the BaseOnStyle property.
		/// </summary>
		private static void OnBaseOnStyleChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
		{
			if (e.OldValue == e.NewValue)
			{
				return;
			}

			Control control = d as Control;
			if (control == null)
			{
				throw new NotSupportedException("BaseOnStyle can only used in Control");
			}

			Style baseOnStyle = e.NewValue as Style;
			Style originalStyle = GetOriginalStyle(control);
			if (originalStyle == null)
			{
				originalStyle = control.Style;
				SetOriginalStyle(control, originalStyle);
			}
			Style newStyle = originalStyle;

			if (originalStyle.IsSealed)
			{
				newStyle = new Style();
				newStyle.TargetType = originalStyle.TargetType;

				//1. Copy resources, setters, triggers
				newStyle.Resources = originalStyle.Resources;
				foreach (var st in originalStyle.Setters)
				{
					newStyle.Setters.Add(st);
				}
				foreach (var tg in originalStyle.Triggers)
				{
					newStyle.Triggers.Add(tg);
				}

				//2. Set BaseOn Style
				newStyle.BasedOn = baseOnStyle;
			}
			else
			{
				originalStyle.BasedOn = baseOnStyle;
			}

			control.Style = newStyle;
		}

		#endregion

		#region OriginalStyle

		/// <summary>
		/// OriginalStyle Attached Dependency Property
		/// </summary>
		public static readonly DependencyProperty OriginalStyleProperty =
			DependencyProperty.RegisterAttached("OriginalStyle", typeof(Style), typeof(Behavior),
				new FrameworkPropertyMetadata((Style)null));

		/// <summary>
		/// Gets the OriginalStyle property.  This dependency property 
		/// indicates ....
		/// </summary>
		public static Style GetOriginalStyle(DependencyObject d)
		{
			return (Style)d.GetValue(OriginalStyleProperty);
		}

		/// <summary>
		/// Sets the OriginalStyle property.  This dependency property 
		/// indicates ....
		/// </summary>
		public static void SetOriginalStyle(DependencyObject d, Style value)
		{
			d.SetValue(OriginalStyleProperty, value);
		}

		#endregion
	}

	/// <summary>
	/// Registers KeyGesture InputBindings at application scope.
	/// </summary>
	/// <remarks>
	/// This sets the associated object as the command target automatically.
	/// This only calls the command when the associated object is visible.
	/// This only supports Gestures that include modifier keys (Ctrl, Alt, Shift)
	/// This passes the InputBinding.CommandParameter when executing the command.
	/// </remarks>
	public class GlobalShortcutKeysBehavior : Behavior<FrameworkElement>
	{
		private FrameworkElement frameworkElement;

		private static Dictionary<Window, List<FrameworkElement>> associatedWindows = new Dictionary<Window, List<FrameworkElement>>();
		private static object associatedWindowsLock = new object();
		private static List<InputBinding> appInputBindings = new List<InputBinding>();
		private static InputBinding[] readOnlyAppInputBindings = new InputBinding[0];
		private static object appInputBindingsLock = new object();

		/// <summary>
		/// Initializes a new instance of the <see cref="GlobalShortcutKeysBehavior"/> class.
		/// </summary>
		public GlobalShortcutKeysBehavior()
		{
			this.InputBindings = new InputBindingCollection();
		}

		/// <summary>
		/// Gets or sets the input bindings.
		/// </summary>
		/// <value>The input bindings.</value>
		public InputBindingCollection InputBindings
		{
			get { return (InputBindingCollection)GetValue(InputBindingsProperty); }
			set { SetValue(InputBindingsProperty, value); }
		}

		public static readonly DependencyProperty InputBindingsProperty = DependencyProperty.Register("InputBindings", typeof(InputBindingCollection), typeof(GlobalShortcutKeysBehavior), new FrameworkPropertyMetadata(null));

		/// <summary>
		/// Registers the specified keyboard input bindings for the specified command target.
		/// </summary>
		/// <param name="inputBindings">The input bindings.</param>
		/// <param name="commandTarget">The command target.</param>
		/// <remarks>
		/// It is OK if the command target has not been loaded yet.
		/// Bindings are automatically removed when the command target is unloaded.
		/// </remarks>
		public static void Register(InputBindingCollection inputBindings, FrameworkElement commandTarget)
		{
			if (inputBindings == null)
			{
				throw new ArgumentNullException("inputBindings");
			}

			if (commandTarget == null)
			{
				throw new ArgumentNullException("commandTarget");
			}

			RegisterBindings(inputBindings, commandTarget);

			// I register the window if the target has been loaded, otherwise I wait until it is visible.
			if (!RegisterAssociatedWindow(commandTarget))
			{
				commandTarget.Loaded += new RoutedEventHandler(CommandTarget_Loaded);
			}

			commandTarget.Unloaded += new RoutedEventHandler(CommandTarget_Unloaded);
		}

		/// <summary>
		/// Unregisters all keyboard input binding for the specified command target.
		/// </summary>
		/// <param name="commandTarget">The command target.</param>
		public static void Unregister(FrameworkElement commandTarget)
		{
			if (commandTarget == null)
			{
				throw new ArgumentNullException("commandTarget");
			}

			UnregisterAssociatedWindow(commandTarget);
			UnregisterBindings(commandTarget);
		}

		private static void CommandTarget_Loaded(object sender, RoutedEventArgs e)
		{
			FrameworkElement commandTarget = sender as FrameworkElement;
			if (commandTarget != null)
			{
				commandTarget.Loaded -= CommandTarget_Loaded;
				RegisterAssociatedWindow(commandTarget);
			}
		}

		private static void CommandTarget_Unloaded(object sender, RoutedEventArgs e)
		{
			FrameworkElement commandTarget = sender as FrameworkElement;
			if (commandTarget != null)
			{
				UnregisterAssociatedWindow(commandTarget);
				UnregisterBindings(commandTarget);
			}
		}

		/// <summary>
		/// Called after the behavior is attached to an AssociatedObject.
		/// </summary>
		/// <remarks>Override this to hook up functionality to the AssociatedObject.</remarks>
		protected override void OnAttached()
		{
			base.OnAttached();

			this.frameworkElement = this.AssociatedObject as FrameworkElement;

			if (this.frameworkElement != null)
			{
				if (this.InputBindings != null)
				{
					RegisterBindings(this.InputBindings, this.frameworkElement);
				}
				RegisterAssociatedWindow(this.frameworkElement);
			}
		}

		/// <summary>
		/// Called when the behavior is being detached from its AssociatedObject, but before it has actually occurred.
		/// </summary>
		/// <remarks>Override this to unhook functionality from the AssociatedObject.</remarks>
		protected override void OnDetaching()
		{
			if (this.frameworkElement != null)
			{
				UnregisterAssociatedWindow(this.frameworkElement);

			}

			base.OnDetaching();
		}

		#region Registration Helper Methods

		private static void RegisterBindings(InputBindingCollection inputBindings, FrameworkElement commandTarget)
		{
			Debug.Assert(inputBindings != null);
			Debug.Assert(commandTarget != null);

			lock (appInputBindingsLock)
			{
				foreach (InputBinding inputBinding in inputBindings)
				{
					// I only add input bindings that are KeyGestures and have a command.
					if ((inputBinding.Gesture is KeyGesture) && (inputBinding.Command != null))
					{
						inputBinding.CommandTarget = commandTarget;
						appInputBindings.Add(inputBinding);
					}
				}

				readOnlyAppInputBindings = appInputBindings.ToArray();
			}
		}

		private static void UnregisterBindings(InputBindingCollection inputBindings)
		{
			Debug.Assert(inputBindings != null);

			lock (appInputBindingsLock)
			{
				foreach (InputBinding inputBinding in inputBindings)
				{
					appInputBindings.Remove(inputBinding);
				}

				readOnlyAppInputBindings = appInputBindings.ToArray();
			}
		}

		private static void UnregisterBindings(object commandTarget)
		{
			Debug.Assert(commandTarget != null);

			lock (appInputBindingsLock)
			{
				foreach (InputBinding inputBinding in appInputBindings)
				{
					if (object.ReferenceEquals(inputBinding.CommandTarget, commandTarget))
					{
						appInputBindings.Remove(inputBinding);
					}
				}

				readOnlyAppInputBindings = appInputBindings.ToArray();
			}
		}

		private static bool RegisterAssociatedWindow(FrameworkElement commandTarget)
		{
			Debug.Assert(commandTarget != null);

			Window window = Window.GetWindow(commandTarget);
			if (window != null)
			{
				lock (associatedWindowsLock)
				{
					// I reference count the windows to prevent double subscribing to PreviewKeyDown
					List<FrameworkElement> frameworkElements;
					if (!associatedWindows.TryGetValue(window, out frameworkElements))
					{
						frameworkElements = new List<FrameworkElement>();
						associatedWindows[window] = frameworkElements;
						window.PreviewKeyDown += Window_PreviewKeyDown;
					}

					if (!frameworkElements.Contains(commandTarget))
					{
						frameworkElements.Add(commandTarget);
					}
				}
			}

			return (window != null);
		}

		private static void UnregisterAssociatedWindow(FrameworkElement commandTarget)
		{
			Debug.Assert(commandTarget != null);

			Window window = Window.GetWindow(commandTarget);
			if (window != null)
			{
				lock (associatedWindowsLock)
				{
					List<FrameworkElement> frameworkElements;
					if (associatedWindows.TryGetValue(window, out frameworkElements))
					{
						frameworkElements.Remove(commandTarget);
						if (frameworkElements.Count == 0)
						{
							associatedWindows.Remove(window);
							window.PreviewKeyDown -= Window_PreviewKeyDown;
						}
					}
				}
			}
		}

		#endregion

		private static void Window_PreviewKeyDown(object sender, KeyEventArgs e)
		{
			if (Keyboard.Modifiers != ModifierKeys.None)
			{
				Window window = sender as Window;
				if (window != null)
				{
					// I hold the lock for the shortest time possible
					InputBinding[] inputBindings;
					lock (appInputBindingsLock)
					{
						inputBindings = readOnlyAppInputBindings;
					}

					foreach (InputBinding inputBinding in inputBindings)
					{
						if (inputBinding.Gesture.Matches(inputBinding.CommandTarget, e))
						{
							if (InvokeCommandIfTargetVisible(inputBinding))
							{
								e.Handled = true;
							}
						}
					}
				}
			}
		}

		private static bool InvokeCommandIfTargetVisible(InputBinding inputBinding)
		{
			Debug.Assert(inputBinding != null);

			FrameworkElement frameworkElement = inputBinding.CommandTarget as FrameworkElement;

			// I only execute when the command target is visible.
			if ((frameworkElement != null) && (frameworkElement.IsVisible))
			{
				return InvokeCommand(inputBinding);
			}

			return false;
		}

		private static bool InvokeCommand(InputBinding inputBinding)
		{
			Debug.Assert(inputBinding != null);

			// Routed commands are static, so we need to provide the command target
			// otherwise the default is to use the focused element.
			RoutedCommand routedCommand = inputBinding.Command as RoutedCommand;
			if (routedCommand != null)
			{
				if (routedCommand.CanExecute(inputBinding.CommandParameter, inputBinding.CommandTarget))
				{
					routedCommand.Execute(inputBinding.CommandParameter, inputBinding.CommandTarget);
					return true;
				}
			}
			else
			{
				// Other commands are instance commands and can be called directly.
				if (inputBinding.Command.CanExecute(inputBinding.CommandParameter))
				{
					inputBinding.Command.Execute(inputBinding.CommandParameter);
					return true;
				}
			}

			return false;
		}
	}

	public class InputBindingTrigger : TriggerBase<FrameworkElement>, ICommand
	{
		public InputBindingTrigger()
		{

		}
		public InputBinding InputBinding
		{
			get { return (InputBinding)GetValue(InputBindingProperty); }
			set { SetValue(InputBindingProperty, value); }
		}
		public static readonly DependencyProperty InputBindingProperty =
			DependencyProperty.Register("InputBinding", typeof(InputBinding)
			, typeof(InputBindingTrigger)
			, new UIPropertyMetadata(null));

		protected override void OnAttached()
		{
			if (InputBinding != null)
			{
				InputBinding.Command = this;
				AssociatedObject.InputBindings.Add(InputBinding);
			}
			base.OnAttached();
		}

		# region ICommand Members

		public bool CanExecute(object parameter)
		{
			return true;
		}

		public event EventHandler CanExecuteChanged;

		public void Execute(object parameter)
		{
			InvokeActions(parameter);
		}

		# endregion
	}
}
