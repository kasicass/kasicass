using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Windows;

namespace Illusion
{
	/// <summary> 
	/// Monitors the PropertyChanged event of an object that implements INotifyPropertyChanged,
	/// and executes callback methods (i.e. handlers) registered for properties of that object.
	/// </summary>
	/// <typeparam name="T">The type of object to monitor for property changes.</typeparam>
	public class PropertyObserver<T> : IWeakEventListener where T : class, INotifyPropertyChanged
	{
		private readonly Dictionary<string, Action<T>> _propertyNameToHandlerMap;
		private readonly WeakReference _propertySourceRef;

		/// <summary>
		/// Initializes a new instance of PropertyObserver, which
		/// observes the 'propertySource' object for property changes.
		/// </summary>
		/// <param name="propertySource">The object to monitor for property changes.</param>
		public PropertyObserver(T propertySource)
		{
			if (propertySource == null)
				throw new ArgumentNullException("propertySource");

			_propertySourceRef = new WeakReference(propertySource);
			_propertyNameToHandlerMap = new Dictionary<string, Action<T>>();
		}

		/// <summary>
		/// Registers a callback to be invoked when the PropertyChanged event has been raised for the specified property.
		/// </summary>
		/// <param name="expression">A lambda expression like 'n => n.PropertyName'.</param>
		/// <param name="handler">The callback to invoke when the property has changed.</param>
		/// <returns>The object on which this method was invoked, to allow for multiple invocations chained together.</returns>
		public PropertyObserver<T> RegisterHandler(Expression<Func<T, object>> expression, Action<T> handler)
		{
			if (expression == null)
				throw new ArgumentNullException("expression");

			string propertyName = GetPropertyName(expression);
			if (String.IsNullOrEmpty(propertyName))
				throw new ArgumentException("'expression' did not provide a property name.");

			if (handler == null)
				throw new ArgumentNullException("handler");

			return RegisterHandler(propertyName, handler);
		}

		/// <summary>
		/// Registers a callback to be invoked when the PropertyChanged event has been raised for the specified property.
		/// </summary>
		/// <param name="propertyName">Name of the property.</param>
		/// <param name="handler">The callback to invoke when the property has changed.</param>
		/// <returns>
		/// The object on which this method was invoked, to allow for multiple invocations chained together.
		/// </returns>
		public PropertyObserver<T> RegisterHandler(string propertyName, Action<T> handler)
		{
			var propertySource = this.GetPropertySource();
			if (propertySource != null)
			{
				_propertyNameToHandlerMap[propertyName] = handler;
				PropertyChangedEventManager.AddListener(propertySource, this, propertyName);
			}

			return this;
		}

		/// <summary>
		/// Removes the callback associated with the specified property.
		/// </summary>
		/// <param name="expression">A lambda expression like 'n => n.PropertyName'.</param>
		/// <returns>The object on which this method was invoked, to allow for multiple invocations chained together.</returns>
		public PropertyObserver<T> UnregisterHandler(Expression<Func<T, object>> expression)
		{
			if (expression == null)
				throw new ArgumentNullException("expression");

			string propertyName = GetPropertyName(expression);
			if (String.IsNullOrEmpty(propertyName))
				throw new ArgumentException("'expression' did not provide a property name.");

			return UnregisterHandler(propertyName);
		}


		/// <summary>
		/// Removes the callback associated with the specified property.
		/// </summary>
		/// <param name="propertyName">Name of the property.</param>
		/// <returns>
		/// The object on which this method was invoked, to allow for multiple invocations chained together.
		/// </returns>
		public PropertyObserver<T> UnregisterHandler(string propertyName)
		{
			T propertySource = GetPropertySource();
			if (propertySource != null)
			{
				if (_propertyNameToHandlerMap.ContainsKey(propertyName))
				{
					_propertyNameToHandlerMap.Remove(propertyName);
					PropertyChangedEventManager.RemoveListener(propertySource, this, propertyName);
				}
			}

			return this;
		}

		/// <summary>
		/// Gets the name of the property.
		/// </summary>
		/// <param name="expression">The expression.</param>
		/// <returns></returns>
		private static string GetPropertyName(Expression<Func<T, object>> expression)
		{
			var lambda = expression as LambdaExpression;
			MemberExpression memberExpression;
			if (lambda.Body is UnaryExpression)
			{
				var unaryExpression = lambda.Body as UnaryExpression;
				memberExpression = unaryExpression.Operand as MemberExpression;
			}
			else
			{
				memberExpression = lambda.Body as MemberExpression;
			}

			Debug.Assert(memberExpression != null, "Please provide a lambda expression like 'n => n.PropertyName'");

			if (memberExpression != null)
			{
				var propertyInfo = memberExpression.Member as PropertyInfo;
				if (propertyInfo != null)
					return propertyInfo.Name;
			}

			return null;
		}

		/// <summary>
		/// Gets the property source.
		/// </summary>
		/// <returns></returns>
		private T GetPropertySource()
		{
			try
			{
				return (T)_propertySourceRef.Target;
			}
			catch
			{
				return default(T);
			}
		}

		/// <summary>
		/// Receives events from the centralized event manager.
		/// </summary>
		/// <param name="managerType">The type of the <see cref="T:System.Windows.WeakEventManager"/> calling this method.</param>
		/// <param name="sender">Object that originated the event.</param>
		/// <param name="e">Event data.</param>
		/// <returns>
		/// true if the listener handled the event. It is considered an error by the <see cref="T:System.Windows.WeakEventManager"/> handling in WPF to register a listener for an event that the listener does not handle. Regardless, the method should return false if it receives an event that it does not recognize or handle.
		/// </returns>
		bool IWeakEventListener.ReceiveWeakEvent(Type managerType, object sender, EventArgs e)
		{
			if (managerType == typeof(PropertyChangedEventManager))
			{
				string propertyName = ((PropertyChangedEventArgs)e).PropertyName;
				var propertySource = (T)sender;

				if (String.IsNullOrEmpty(propertyName))
				{
					// When the property name is empty, all properties are considered to be invalidated.
					// Iterate over a copy of the list of handlers, in case a handler is registered by a callback.
					_propertyNameToHandlerMap
						.Values
						.ToList()
						.ForEach(h => h(propertySource));
					return true;
				}

				Action<T> handler;
				if (_propertyNameToHandlerMap.TryGetValue(propertyName, out handler))
				{
					handler(propertySource);
					return true;
				}
			}

			return false;
		}
	}

	public class CollectionSyncer<T, U> : IWeakEventListener
		where T : class
		where U : class
	{
		#region Field

		private Func<T, U> _create;
		private IObservableCollection<U> _syncCollection;

		#endregion

		public CollectionSyncer(ICollection<T> t, Func<T, U> create)
		{
			_create = create;
			INotifyCollectionChanged collection = t as INotifyCollectionChanged;
			if (collection == null)
			{
				throw new ArgumentException("The ICollection<T> must be INotifyCollectionChanged");
			}
			CollectionChangedEventManager.AddListener(collection, this);
			_syncCollection = new BindableCollection<U>();
			t.Apply(item => _syncCollection.Add(_create(item)));
		}

		public IObservableCollection<U> SyncCollection
		{
			get
			{
				return _syncCollection;
			}
		}

		#region IWeakEventListener Members

		public bool ReceiveWeakEvent(Type managerType, object sender, EventArgs e)
		{
			if (managerType == typeof(CollectionChangedEventManager))
			{
				NotifyCollectionChangedEventArgs arg = e as NotifyCollectionChangedEventArgs;
				if (arg != null)
				{
					switch (arg.Action)
					{
						case NotifyCollectionChangedAction.Add:
							if (arg.NewItems != null)
							{
								for (int itemIndex = 0; itemIndex < arg.NewItems.Count; itemIndex++)
								{
									_syncCollection.Insert(arg.NewStartingIndex +
									  itemIndex, _create(arg.NewItems[itemIndex] as T));
								}
								break;
							}
							break;
						case NotifyCollectionChangedAction.Move:
							_syncCollection.RemoveAt(arg.OldStartingIndex);
							_syncCollection.Insert(arg.NewStartingIndex, _create(arg.NewItems[0] as T));
							break;
						case NotifyCollectionChangedAction.Remove:
							if (arg.OldItems != null && _syncCollection.Count > 0)
							{
								for (int itemIndex = 0; itemIndex < arg.OldItems.Count; itemIndex++)
								{
									_syncCollection.RemoveAt(arg.OldStartingIndex);
								}
							}
							break;
						case NotifyCollectionChangedAction.Replace:
							if (arg.NewItems == null)
							{
								for (int itemIndex = 0; itemIndex < arg.NewItems.Count; itemIndex++)
								{
									_syncCollection[arg.NewStartingIndex + itemIndex] = _create(arg.NewItems[itemIndex] as T);
								}
							}
							break;
						case NotifyCollectionChangedAction.Reset:
							_syncCollection.Clear();
							break;
						default:
							break;
					}
				}
				return true;
			}
			return false;
		}

		#endregion
	}

	public class CollectionMerger<T> : IWeakEventListener where T : class
	{
		#region Field

		private readonly WeakReference _source;
		private readonly int _insertIndex;

		#endregion

		public CollectionMerger(IList<T> source, ICollection<T> target, int index = -1)
		{
			if (source == null || target == null)
			{
				throw new ArgumentNullException("The collections of CollectionMerge should not be null");
			}
			if (index < 0)
			{
				_insertIndex = source.Count;
				target.Apply(item => source.Add(item));
			}
			else
			{
				_insertIndex = index;
				target.Apply(item =>
					{
						source.Insert(index, item);
						index++;
					});
			}

			INotifyCollectionChanged collection = target as INotifyCollectionChanged;
			if (collection != null)
			{
				_source = new WeakReference(source);
				CollectionChangedEventManager.AddListener(collection, this);
			}
		}

		private IList<T> Source
		{
			get
			{
				if (_source.IsAlive)
				{
					return _source.Target as IList<T>;
				}
				else
				{
					return default(IList<T>);
				}
			}
		}

		#region IWeakEventListener Members

		private int FindIndex(IList<T> source, IList<T> target)
		{
			if (target == null || target.Count == 0)
			{
				return _insertIndex;
			}

			var contains = source.Intersect(target);
			if (contains == null || contains.Count() == 0)
			{
				return _insertIndex;
			}

			return source.IndexOf(contains.First());
		}

		public bool ReceiveWeakEvent(Type managerType, object sender, EventArgs e)
		{
			if (managerType == typeof(CollectionChangedEventManager))
			{
				NotifyCollectionChangedEventArgs arg = e as NotifyCollectionChangedEventArgs;
				if (arg != null)
				{
					int startIndex = FindIndex(Source, sender as IList<T>);

					switch (arg.Action)
					{
						case NotifyCollectionChangedAction.Add:
							if (arg.NewItems != null)
							{
								for (int itemIndex = 0; itemIndex < arg.NewItems.Count; itemIndex++)
								{
									Source.Insert(startIndex + itemIndex, arg.NewItems[itemIndex] as T);
								}
								break;
							}
							break;
						case NotifyCollectionChangedAction.Move:
							Source.RemoveAt(startIndex + arg.OldStartingIndex);
							Source.Insert(startIndex + arg.NewStartingIndex, arg.NewItems[0] as T);
							break;
						case NotifyCollectionChangedAction.Remove:
						case NotifyCollectionChangedAction.Reset:
							if (arg.OldItems != null)
							{
								for (int itemIndex = 0; itemIndex < arg.OldItems.Count; itemIndex++)
								{
									Source.Remove(arg.OldItems[itemIndex] as T);
								}
							}
							break;
						case NotifyCollectionChangedAction.Replace:
							if (arg.NewItems == null)
							{
								for (int itemIndex = 0; itemIndex < arg.NewItems.Count; itemIndex++)
								{
									Source[startIndex + arg.NewStartingIndex + itemIndex] = arg.NewItems[itemIndex] as T;
								}
							}
							break;
						default:
							break;
					}
				}
				return true;
			}
			return false;
		}

		#endregion
	}

}