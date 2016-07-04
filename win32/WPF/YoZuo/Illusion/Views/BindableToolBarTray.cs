using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Collections.Specialized;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;

namespace Illusion.Views
{
    /// <summary>
    ///  Bindable concrete <see cref="ToolBarTray"/>.
    /// </summary>
	public class BindableToolBarTray : ToolBarTray
	{
		#region Field

		public static readonly DependencyProperty ContainsVisibleToolBarsProperty = DependencyProperty.Register("ContainsVisibleToolBars", typeof(bool), typeof(BindableToolBarTray), new FrameworkPropertyMetadata(false));

		#endregion

		#region Constructor

		static BindableToolBarTray()
		{
			FrameworkElement.DefaultStyleKeyProperty.OverrideMetadata(typeof(BindableToolBarTray), new FrameworkPropertyMetadata(typeof(BindableToolBarTray)));
			ItemsControl.ItemsSourceProperty.AddOwner(typeof(BindableToolBarTray), new FrameworkPropertyMetadata(new PropertyChangedCallback(BindableToolBarTray.OnItemsSourceChanged)));
		}

		#endregion

		#region Method
		
		private void AddCollectionChangedHandler(INotifyCollectionChanged observableCollection)
		{
			if (observableCollection != null)
			{
				observableCollection.CollectionChanged += new NotifyCollectionChangedEventHandler(this.ToolBarCollectionChangedHandler);
			}
		}

		protected override Size ArrangeOverride(Size arrangeSize)
		{
			Size size = base.ArrangeOverride(arrangeSize);
			this.ContainsVisibleToolBars = base.ToolBars.FirstOrDefault<ToolBar>(t => (t.Visibility == Visibility.Visible)) != null;
			return size;
		}

		private void OnItemsSourceChanged(DependencyPropertyChangedEventArgs args)
		{
			if (args.OldValue != null)
			{
				this.RemoveCollectionChangedHandler(args.OldValue as INotifyCollectionChanged);
				base.ToolBars.Clear();
			}
			if (args.NewValue != null)
			{
				this.AddCollectionChangedHandler(args.NewValue as INotifyCollectionChanged);
				this.ProcessToolBarModels((IEnumerable)args.NewValue);
			}
		}

		private static void OnItemsSourceChanged(DependencyObject sender, DependencyPropertyChangedEventArgs args)
		{
			((BindableToolBarTray)sender).OnItemsSourceChanged(args);
		}

		private void ProcessToolBarModels(IEnumerable toolBarModels)
		{
			foreach (var source in toolBarModels)
			{
				base.ToolBars.Add(new ToolBar() { DataContext = source });
			}
		}

		private void RemoveCollectionChangedHandler(INotifyCollectionChanged observableCollection)
		{
			if (observableCollection != null)
			{
				observableCollection.CollectionChanged -= new NotifyCollectionChangedEventHandler(this.ToolBarCollectionChangedHandler);
			}
		}

		private void ToolBarCollectionChangedHandler(object sender, NotifyCollectionChangedEventArgs e)
		{
			switch (e.Action)
			{
				case NotifyCollectionChangedAction.Add:
					this.ProcessToolBarModels(e.NewItems);
					return;

				case NotifyCollectionChangedAction.Remove:
					{
						int num = (e.OldStartingIndex + e.OldItems.Count) - 1;
						int num2 = 0;
						while (num2 != e.OldItems.Count)
						{
							base.ToolBars.Remove(base.ToolBars[num]);
							num2++;
							num--;
						}
						return;
					}
			}
			base.ToolBars.Clear();
			this.ProcessToolBarModels((IEnumerable)sender);
		}
		
		#endregion

		#region Property
		
		public bool ContainsVisibleToolBars
		{
			get
			{
				return (bool)base.GetValue(ContainsVisibleToolBarsProperty);
			}
			set
			{
				base.SetValue(ContainsVisibleToolBarsProperty, value);
			}
		}

		public IEnumerable ItemsSource
		{
			get
			{
				return (IEnumerable)base.GetValue(ItemsControl.ItemsSourceProperty);
			}
			set
			{
				base.SetValue(ItemsControl.ItemsSourceProperty, value);
			}
		}
		
		#endregion
	}
}
