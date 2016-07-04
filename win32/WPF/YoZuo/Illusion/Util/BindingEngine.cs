using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Linq.Expressions;

namespace Illusion
{
	using Expression = System.Linq.Expressions.Expression;

	/// <summary>
	///  A bind engine supports custom data binding.
	/// </summary>
	/// <remarks>
	/// It exports two methods for data binding:
	/// 1.SetPropertyBinding
	/// 2.ClearPropertyBinding
	/// The binding engine use weak reference to hold the dependent, that means if u miss to clear data binding,
	/// the dependent will not leak memory.
	/// </remarks>
	/// <author>
	///  yohan zhou 
	///  http://www.cnblogs.com/zhouyongh
	/// </author>
	public class BindingEngine
	{
		#region Field

		private static Dictionary<WeakEntry, Delegate> _expressionSources = new Dictionary<WeakEntry, Delegate>();

		#endregion

		/// <summary>
		/// Sets the property binding.
		/// </summary>
		/// <param name="source">The source.</param>
		/// <param name="target">The target.</param>
		/// <param name="sourceProp">The source prop.</param>
		/// <param name="targetProp">The target prop.</param>
		/// <param name="notify">if set to <c>true</c> update immediately.</param>
		/// <param name="converter">The converter.</param>
		/// <param name="parameter">The converter parameter.</param>
		public static void SetPropertyBinding(Object source, INotifyPropertyChanged target, string sourceProp, string targetProp, bool notify = true, IDataConverter converter = null, object parameter = null)
		{
			WeakEntry entry = new WeakEntry(source.GetType(), target.GetType(), sourceProp, targetProp);
			Delegate setAction = GetExpressionAction(entry, source, true, converter);
			WeakSource wSource = WeakSource.Register(source, target, setAction, targetProp, converter, parameter);
			if (notify)
			{
				wSource.NotifyPropertyChanged(target, targetProp);
			}
		}

		/// <summary>
		/// Clears the property binding.
		/// </summary>
		/// <param name="source">The source.</param>
		/// <param name="target">The target.</param>
		/// <param name="sourceProp">The source prop.</param>
		/// <param name="targetProp">The target prop.</param>
		public static void ClearPropertyBinding(Object source, INotifyPropertyChanged target, string sourceProp, string targetProp)
		{
			WeakEntry entry = new WeakEntry(source.GetType(), target.GetType(), sourceProp, targetProp);
			Delegate setAction = GetExpressionAction(entry, source, true);

			WeakSource.UnRegister(source, setAction, targetProp);
		}

		/// <summary>
		/// Gets the expression action.
		/// </summary>
		/// <param name="entry">The entry.</param>
		/// <param name="source">The source.</param>
		/// <param name="createNew">if set to <c>true</c> [create new].</param>
		/// <param name="converter">The converter.</param>
		/// <returns></returns>
		private static Delegate GetExpressionAction(WeakEntry entry, object source, bool createNew, IDataConverter converter = null)
		{
			Delegate action = null;
			if (_expressionSources.ContainsKey(entry))
			{
				action = _expressionSources[entry];
			}
			else if (createNew)
			{
				/////  Code of the below Expression Tree  ////////////////////////////////
				//if (converter != null)
				//{
				//    target.Property = converter.Convert(source.Property, parameter);
				//}
				//else
				//{
				//    if (target.Property.GetType() == source.Property.GetType())
				//    {
				//        target.Property = source.Property;
				//    }
				//    else
				//    {
				//        throw new InvalidOperationException("The property type between binding source and target does not match, please use IDataConverter to do custom convert.");
				//    }  
				//}
				//////////////////////////////////////////////////////////////////////////

				//Set Property
				var prop = entry.SourceType.GetProperty(entry.SourceProp);
				var paraObj = Expression.Parameter(entry.SourceType);

				//Get Property
				var targetProperty = entry.TargetType.GetProperty(entry.TargetProp);
				var paraTarget = Expression.Parameter(entry.TargetType);
				var getter = Expression.Property(paraTarget, targetProperty);

				//Combine
				Expression boy;
				var paraConvert = Expression.Variable(typeof(IDataConverter));
				var paraParameter = Expression.Variable(typeof(object));

				boy = Expression.IfThenElse(
						Expression.NotEqual(paraConvert, Expression.Constant(null)),
						Expression.Call(paraObj, prop.GetSetMethod(), Expression.Convert(Expression.Call(paraConvert, typeof(IDataConverter).GetMethod("Convert"),
											Expression.Convert(getter, typeof(object)), Expression.Convert(paraParameter, typeof(object))), prop.PropertyType)),
						Expression.IfThenElse(
							Expression.Equal(Expression.Constant(prop.PropertyType, typeof(Type)), Expression.Constant(getter.Type, typeof(Type))),
							Expression.Call(paraObj, prop.GetSetMethod(), Expression.Convert(Expression.Convert(getter, typeof(object)), prop.PropertyType)),
							Expression.Throw(Expression.Constant(new InvalidOperationException(
								"The property type between binding source and target does not match, please use IDataConverter to do custom convert.")))));

				action = Expression.Lambda(boy, paraObj, paraTarget, paraConvert, paraParameter).Compile();

				_expressionSources.Add(entry, action);
			}
			return action;
		}

		private struct WeakEntry
		{
			public Type SourceType;
			public Type TargetType;
			public string SourceProp;
			public string TargetProp;

			/// <summary>
			/// Initializes a new instance of the <see cref="WeakEntry"/> struct.
			/// </summary>
			/// <param name="sourceType">Type of the source.</param>
			/// <param name="targetType">Type of the target.</param>
			/// <param name="sourceProp">The source prop.</param>
			/// <param name="targetProp">The target prop.</param>
			public WeakEntry(Type sourceType, Type targetType, string sourceProp, string targetProp)
			{
				SourceType = sourceType;
				TargetType = targetType;
				SourceProp = sourceProp;
				TargetProp = targetProp;
			}

			/// <summary>
			/// Returns the hash code for this instance.
			/// </summary>
			/// <returns>
			/// A 32-bit signed integer that is the hash code for this instance.
			/// </returns>
			public override int GetHashCode()
			{
				return SourceType.GetHashCode() ^ TargetType.GetHashCode() ^ SourceProp.GetHashCode() ^ TargetProp.GetHashCode();
			}

			/// <summary>
			/// Indicates whether this instance and a specified object are equal.
			/// </summary>
			/// <param name="obj">Another object to compare to.</param>
			/// <returns>
			/// true if <paramref name="obj"/> and this instance are the same type and represent the same value; otherwise, false.
			/// </returns>
			public override bool Equals(object obj)
			{
				WeakEntry entry = (WeakEntry)obj;
				return ((this.SourceType == entry.SourceType) && (this.TargetType == entry.TargetType) &&
					(this.SourceProp == entry.SourceProp) && (this.TargetProp == entry.TargetProp));
			}

			/// <summary>
			/// Implements the operator ==.
			/// </summary>
			/// <param name="obj1">The obj1.</param>
			/// <param name="obj2">The obj2.</param>
			/// <returns>The result of the operator.</returns>
			public static bool operator ==(WeakEntry obj1, WeakEntry obj2)
			{
				return obj1.Equals(obj2);
			}

			/// <summary>
			/// Implements the operator !=.
			/// </summary>
			/// <param name="obj1">The obj1.</param>
			/// <param name="obj2">The obj2.</param>
			/// <returns>The result of the operator.</returns>
			public static bool operator !=(WeakEntry obj1, WeakEntry obj2)
			{
				return !(obj1 == obj2);
			}
		}

		private struct WeakAction
		{
			public Delegate Action;
			public IDataConverter Converter;
			public object Parameter;

			public WeakAction(Delegate action, IDataConverter converter, object parameter)
			{
				System.Diagnostics.Debug.Assert(action != null);
				Action = action;
				Converter = converter;
				Parameter = parameter;
			}

			/// <summary>
			/// Returns the hash code for this instance.
			/// </summary>
			/// <returns>
			/// A 32-bit signed integer that is the hash code for this instance.
			/// </returns>
			public override int GetHashCode()
			{
				return Action.GetHashCode();
			}

			/// <summary>
			/// Indicates whether this instance and a specified object are equal.
			/// </summary>
			/// <param name="obj">Another object to compare to.</param>
			/// <returns>
			/// true if <paramref name="obj"/> and this instance are the same type and represent the same value; otherwise, false.
			/// </returns>
			public override bool Equals(object obj)
			{
				WeakAction action = (WeakAction)obj;
				return ((this.Action == action.Action) && (this.Converter == action.Converter) &&
					(this.Parameter == action.Parameter));
			}

			/// <summary>
			/// Implements the operator ==.
			/// </summary>
			/// <param name="obj1">The obj1.</param>
			/// <param name="obj2">The obj2.</param>
			/// <returns>The result of the operator.</returns>
			public static bool operator ==(WeakAction obj1, WeakAction obj2)
			{
				return obj1.Equals(obj2);
			}

			/// <summary>
			/// Implements the operator !=.
			/// </summary>
			/// <param name="obj1">The obj1.</param>
			/// <param name="obj2">The obj2.</param>
			/// <returns>The result of the operator.</returns>
			public static bool operator !=(WeakAction obj1, WeakAction obj2)
			{
				return !(obj1 == obj2);
			}
		}

		private class WeakSource : WeakReference
		{
			private static Dictionary<int, WeakSource> _weakSources = new Dictionary<int, WeakSource>();

			public Dictionary<string, IList<WeakAction>> Actions = new Dictionary<string, IList<WeakAction>>();
			public Dictionary<int, IList<string>> Targets = new Dictionary<int, IList<string>>();

			/// <summary>
			/// Registers the specified source.
			/// </summary>
			/// <param name="source">The source.</param>
			/// <param name="target">The target.</param>
			/// <param name="action">The action.</param>
			/// <param name="targetProp">The target prop.</param>
			/// <param name="converter">The converter.</param>
			/// <param name="parameter">The converter parameter.</param>
			/// <returns></returns>
			public static WeakSource Register(Object source, INotifyPropertyChanged target, Delegate action, string targetProp, IDataConverter converter = null, object parameter = null)
			{
				WeakSource wSource = _weakSources.ContainsKey(source.GetHashCode()) ? _weakSources[source.GetHashCode()] : null;
				if (wSource == null)
				{
					wSource = new WeakSource(source);
					_weakSources.Add(source.GetHashCode(), wSource);
				}

				IList<WeakAction> actions;
				if (wSource.Actions.ContainsKey(targetProp))
				{
					actions = wSource.Actions[targetProp];
				}
				else
				{
					actions = new List<WeakAction>();
					wSource.Actions.Add(targetProp, actions);
				}
				actions.Add(new WeakAction(action, converter, parameter));

				IList<string> props;
				if (!wSource.Targets.ContainsKey(target.GetHashCode()))
				{
					props = new List<string>();
					props.Add(targetProp);
					wSource.Targets.Add(target.GetHashCode(), props);
					target.PropertyChanged += new PropertyChangedEventHandler(wSource.HandlePropertyChanged);
				}
				else
				{
					props = wSource.Targets[target.GetHashCode()];
					if (!props.Contains(targetProp))
					{
						props.Add(targetProp);
					}
				}
				return wSource;
			}

			/// <summary>
			/// Unregister the specified source.
			/// </summary>
			/// <param name="source">The source.</param>
			/// <param name="action">The action.</param>
			/// <param name="targetProp">The target prop.</param>
			public static void UnRegister(Object source, Delegate action, string targetProp)
			{
				WeakSource wSource = _weakSources.ContainsKey(source.GetHashCode()) ? _weakSources[source.GetHashCode()] : null;
				if (wSource != null && wSource.Actions.ContainsKey(targetProp))
				{
					IList<WeakAction> actions = wSource.Actions[targetProp];
					var wAction = actions.FirstOrDefault(item => item.Action == action);
					if (wAction != null)
					{
						actions.Remove(wAction);
						if (actions.Count == 0)
						{
							wSource.Actions.Remove(targetProp);
						}
					}
					if (wSource.Actions.Count() == 0)
					{
						_weakSources.Remove(wSource.GetHashCode());
					}
				}
			}

			/// <summary>
			/// Initializes a new instance of the <see cref="WeakSource"/> class.
			/// </summary>
			/// <param name="source">The source.</param>
			private WeakSource(Object source)
				: base(source)
			{

			}

			/// <summary>
			/// Notifies the property changed.
			/// </summary>
			/// <param name="target">The target.</param>
			/// <param name="targetProp">The target prop.</param>
			public void NotifyPropertyChanged(INotifyPropertyChanged target, string targetProp)
			{
				if (!Actions.ContainsKey(targetProp))
				{
					return;
				}

				IList<WeakAction> actions = Actions[targetProp];
				if (actions != null)
				{
					foreach (WeakAction action in actions)
					{
						action.Action.DynamicInvoke(this.Target, target, action.Converter, action.Parameter);
					}
				}
			}

			/// <summary>
			/// Handles the property changed.
			/// </summary>
			/// <param name="sender">The sender.</param>
			/// <param name="args">The <see cref="System.ComponentModel.PropertyChangedEventArgs"/> instance containing the event data.</param>
			public void HandlePropertyChanged(object sender, PropertyChangedEventArgs args)
			{
				INotifyPropertyChanged target = sender as INotifyPropertyChanged;
				if (target == null)
				{
					throw new NotSupportedException("WeakSource can only work on INotifyPropertyChanged");
				}

				string name = args.PropertyName;
				if (!Actions.ContainsKey(name))
				{
					return;
				}

				if (IsAlive)
				{
					NotifyPropertyChanged(target, name);
				}
				else
				{
					target.PropertyChanged -= new PropertyChangedEventHandler(HandlePropertyChanged);
					Targets.Remove(target.GetHashCode());
				}
			}
		}
	}

	public interface IDataConverter
	{
		object Convert(object value, object parameter);
	}
}
