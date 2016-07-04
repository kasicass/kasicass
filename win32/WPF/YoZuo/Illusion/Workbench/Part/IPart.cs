using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Media;

namespace Illusion
{
	/// <summary>
	/// Denotes an instance which can be executed.
	/// </summary>
	public interface IExecutable
	{
		bool CanExecute { get; }
		void Execute();
	}

	/// <summary>
	/// Denotes an instance which can be checked.
	/// </summary>
	public interface ICheckable
	{
		bool IsCheckable { get; }
		bool IsChecked { get; set; }
	}

    /// <summary>
    /// Denotes a small UI widget that can display and interact.
    /// </summary>
	public interface IPart : ILocalizableDisplay, IExecutable, IActivate, IDeactivate
	{
		string InputGestureText { get; set; }

		void OnAttached();
		
		string Icon { get; set; }

		bool IsVisible { get; set; }
	}

    /// <summary>
    /// Concrete <see cref="IPart"/> which denotes a <see cref="System.Window.Controls.MenuItem"/> instance.
    /// </summary>
	public interface IMenuPart : IPart, ISeparaterPart, ICheckable
	{

	}

    /// <summary>
    /// Concrete <see cref="IPart"/> which denotes a <see cref="System.Window.Controls.ToolBar"/> item instance.
    /// </summary>
	public interface IToolBarPart : IPart, ISeparaterPart
	{
		string ToolTip { get; set; }
	}

    /// <summary>
    /// Denotes a <see cref="System.Window.Controls.Separater"/> instance.
    /// </summary>
	public interface ISeparaterPart
	{
		bool IsSeparator { get; }
	}

    /// <summary>
    /// Denotes a manager class that manage the <see cref="IPart"/>s.
    /// </summary>
	public interface IPartManager<T> where T : IPart
	{
		IObservableCollection<T> Items { get; }
	}

    /// <summary>
    /// Denotes a manager node that holds the <see cref="IObservableCollection"/> item.
    /// </summary>
	public interface IObservableParent<T>
	{
		IObservableCollection<T> Items { get; }
	}
}
