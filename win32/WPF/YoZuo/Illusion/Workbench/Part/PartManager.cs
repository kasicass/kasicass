using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

namespace Illusion
{
	/// <summary>
    /// Concrete <see cref="IPartManager"/> with manages <see cref="IPart"/> items, it uses MEF to construct the <see cref="IPart"/>s.
	/// </summary>
	public class PartManager<T> : IPartManager<T>, IPartImportsSatisfiedNotification where T : IPart
	{
		#region Constructor
		
		public PartManager()
		{
		}
		
		#endregion

		#region Property
		
		[ImportMany]
		protected T[] InternalItems { get; set; }
		
		#endregion

		#region Method
		
		protected virtual void ConfigParts()
		{
			if (InternalItems == null || InternalItems.Length == 0)
			{
				return;
			}

			items.Clear();
			items.AddRange(InternalItems);
		}
		
		#endregion

		#region IPartManager Members

		private IObservableCollection<T> items = new BindableCollection<T>();
		public IObservableCollection<T> Items
		{
			get { return items; }
		}

		#endregion

		#region IPartImportsSatisfiedNotification Members

		public void OnImportsSatisfied()
		{
			ConfigParts();
		}

		#endregion
	}

    /// <summary>
    /// Extends the <see cref="IPartManager"/> that support <see cref="IPartMetaData"/>.
    /// </summary>
    /// <typeparam name="T">IPart</typeparam>
    /// <typeparam name="TMetadata">The type of the metadata.</typeparam>
	public class PartManager<T, TMetadata> : IPartManager<T>, IPartImportsSatisfiedNotification
		where T : IPart
		where TMetadata : IPartMetaData
	{
		#region Field
		
		protected static readonly Func<TMetadata, string> BasePart;
		protected static readonly Func<TMetadata, string> PreviousPart;
		protected static readonly Func<TMetadata, string> NextPart;
		
		#endregion

		#region Constructor
		
		static PartManager()
		{
			var props = typeof(TMetadata).GetProperties();
			BasePart = DynamicAccessEngine.GetPropertyDelegate<TMetadata, string>(props.FirstOrDefault(it => it.Name.Contains("Base")).Name);
			PreviousPart = DynamicAccessEngine.GetPropertyDelegate<TMetadata, string>(props.FirstOrDefault(it => it.Name.Contains("Previous")).Name);
			NextPart = DynamicAccessEngine.GetPropertyDelegate<TMetadata, string>(props.FirstOrDefault(it => it.Name.Contains("Next")).Name);
		}

		public PartManager()
		{
		}
		
		#endregion

		#region Property

        [ImportMany]
        protected Lazy<T, TMetadata>[] InternalItems
        {
            get;
            set;
        }
		
		#endregion

		#region Method
		
		protected virtual void ConfigParts()
		{
			if (InternalItems == null || InternalItems.Length == 0)
			{
				return;
			}

			items.Clear();

			//Sort items according to metadata's Base , Previous, Next value
			SortItems();
		}

		protected virtual void SortItems()
		{
			var items = InternalItems.Select((it) =>
			{
				return new OrderItem<T>()
				{
					Base = BasePart(it.Metadata),
					Before = PreviousPart(it.Metadata),
					After = NextPart(it.Metadata),
					Value = it.Value
				};
			}).ToList();

			var roots = SortAndAttachItems(items.Where(it => string.IsNullOrEmpty(it.Base)).ToList());

			foreach (var item in items)
			{
				var baseItem = items.FirstOrDefault(it => string.Equals(it.Value.Name, item.Base));
				if (baseItem != null)
				{
					baseItem.Children.Add(item);
				}
			}

			foreach (var item in roots)
			{
				SortItem(item);
			}

			Items.AddRange(roots.Select(it => it.Value));
		}

		private void SortItem(OrderItem<T> item)
		{
			if (item.Children.Count == 0)
			{
				return;
			}

			//1. Child recursion.
			foreach (var it in item.Children)
			{
				SortItem(it);
			}

			//2. Sort
			var sortedItems = SortAndAttachItems(item.Children);

			foreach (var it in sortedItems)
			{
				IObservableParent<T> parent = item.Value as IObservableParent<T>;
				if (parent != null)
				{
					parent.Items.Add(it.Value);
				}
			}
		}

		private List<OrderItem<T>> SortAndAttachItems(List<OrderItem<T>> items)
		{
			//1. Sort
			var sortedItems = new List<OrderItem<T>>();
			var unsortedItems = new List<OrderItem<T>>();
			foreach (var newItem in items)
			{
				if (string.IsNullOrEmpty(newItem.Before) && string.IsNullOrEmpty(newItem.After))
				{
					sortedItems.Add(newItem);
				}
				else
				{
					unsortedItems.Add(newItem);
				}
			}

			while (unsortedItems.Count > 0)
			{
				List<OrderItem<T>> stillUnsortedItems = new List<OrderItem<T>>();
				int startingCount = unsortedItems.Count;
				foreach (var newItem in unsortedItems)
				{
					if (!string.IsNullOrEmpty(newItem.After))
					{
						var beforeItem = sortedItems.FirstOrDefault(it => it.Value.Name == newItem.After);
						if (beforeItem != null)
						{
							sortedItems.Insert(sortedItems.IndexOf(beforeItem), newItem);
						}
						else
						{
							stillUnsortedItems.Add(newItem);
						}
					}
					else
					{
						var afterItem = sortedItems.FirstOrDefault(it => it.Value.Name == newItem.Before);
						if (afterItem != null)
						{
							int index = sortedItems.IndexOf(afterItem);
							if (index == sortedItems.Count - 1)
							{
								sortedItems.Add(newItem);
							}
							else
							{
								sortedItems.Insert(index + 1, newItem);
							}
						}
						else
						{
							stillUnsortedItems.Add(newItem);
						}
					}
				}
				if (startingCount == stillUnsortedItems.Count)
				{
					sortedItems.Add(stillUnsortedItems[0]);
					stillUnsortedItems.RemoveAt(0);
				}
				unsortedItems = stillUnsortedItems;
			}

			//2. Call Attached method of IPart
			sortedItems.Apply(o => o.Value.OnAttached());

			return sortedItems;
		}
		
		#endregion

		#region IPartManager Members

		private IObservableCollection<T> items = new BindableCollection<T>();
		public IObservableCollection<T> Items
		{
			get { return items; }
		}

		#endregion

		#region IPartImportsSatisfiedNotification Members

		public void OnImportsSatisfied()
		{
			ConfigParts();
		}

		#endregion

		#region Private OrderItem Class

		private class OrderItem<U>
		{
			public string Base { get; set; }
			public string Before { get; set; }
			public string After { get; set; }
			public U Value { get; set; }

			private List<OrderItem<U>> children = new List<OrderItem<U>>();
			public List<OrderItem<U>> Children
			{
				get
				{
					return children;
				}
			}
		}

		#endregion
	}
}
