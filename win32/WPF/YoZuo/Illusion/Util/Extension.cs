using System;
using System.Collections.Generic;
using System.Timers;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.IO;
using System.Reflection;
using System.Windows.Interop;
using System.Resources;
using System.Windows.Threading;

namespace Illusion
{
    /// <summary>
    /// Wpf extensions.
    /// </summary>
	public static class WpfExtensions
	{
		/// <summary>
		/// Finds a Child of a given item in the visual tree. 
		/// </summary>
		/// <param name="parent">A direct parent of the queried item.</param>
		/// <typeparam name="T">The type of the queried item.</typeparam>
		/// <param name="childName">x:Name or Name of child. </param>
		/// <returns>The first parent item that matches the submitted type parameter. 
		/// If not matching item can be found, a null parent is being returned.</returns>
		public static T FindChild<T>(this DependencyObject parent, string childName = null)
		   where T : DependencyObject
		{
			// Confirm parent and childName are valid. 
			if (parent == null) return null;

			T foundChild = null;

			int childrenCount = VisualTreeHelper.GetChildrenCount(parent);
			for (int i = 0; i < childrenCount; i++)
			{
				var child = VisualTreeHelper.GetChild(parent, i);
				// If the child is not of the request child type child
				T childType = child as T;
				if (childType == null)
				{
					// recursively drill down the tree
					foundChild = FindChild<T>(child, childName);

					// If the child is found, break so we do not overwrite the found child. 
					if (foundChild != null) break;
				}
				else if (!string.IsNullOrEmpty(childName))
				{
					var frameworkElement = child as FrameworkElement;
					// If the child's name is set for search
					if (frameworkElement != null && frameworkElement.Name == childName)
					{
						// if the child's name is of the request name
						foundChild = (T)child;
						break;
					}
				}
				else
				{
					// child element found.
					foundChild = (T)child;
					break;
				}
			}

			return foundChild;
		}
	}

    /// <summary>
    /// String extensions.
    /// </summary>
	public static class StringExtension
	{
		public static string Right(this string str, string content)
		{
			if (str == null)
			{
				return null;
			}

			if (!str.Contains(content))
			{
				return string.Empty;
			}
			else
			{
				int index = str.LastIndexOf(content);
				return index < str.Length - 1 ? str.Substring(index + content.Length, str.Length - index - content.Length) : string.Empty;
			}
		}

		public static string Left(this string str, string content)
		{
			if (str == null)
			{
				return null;
			}

			if (!str.Contains(content))
			{
				return string.Empty;
			}
			else
			{
				int index = str.IndexOf(content);
				return str.Substring(0, index);
			}
		}
	}

    /// <summary>
    /// Type extensions.
    /// </summary>
	public static class TypeExtension
	{
		/// <summary>
		/// Gets the interface explicit property.
		/// </summary>
		/// <param name="type">The type.</param>
		/// <param name="interfaceName">Name of the interface.</param>
		/// <param name="propertyName">Name of the property.</param>
		/// <returns></returns>
		public static PropertyInfo GetInterfaceProperty(this Type type, string interfaceName, string propertyName)
		{
			Type typeInterface;
			if (interfaceName != null && interfaceName.Contains('<'))
			{
				var names = interfaceName.Trim().Split('<', '>');
				typeInterface = type.GetInterfaces().Where(i => names.All(j => i.FullName.Contains(j))).FirstOrDefault();
			}
			else
			{
				typeInterface = type.GetInterfaces().Where(i => i.Name == interfaceName).FirstOrDefault();
			}

			if (typeInterface != null)
			{
				return typeInterface.GetProperty(propertyName);
			}
			return null;
		}

		/// <summary>
		/// Determines whether the specific type is simple type..
		/// </summary>
		/// <param name="type">The type.</param>
		/// <returns>
		/// 	<c>true</c> if is simple type; otherwise, <c>false</c>.
		/// </returns>
		public static bool IsSimpleType(this Type type)
		{
			return (type.IsPrimitive ||
				type.IsEnum ||
				type == typeof(string));
		}

		/// <summary>
		/// Determines whether the specific type is this type or subclass.
		/// </summary>
		/// <param name="type">The type.</param>
		/// <param name="c">The c.</param>
		/// <returns>
		/// 	<c>true</c> if it is; otherwise, <c>false</c>.
		/// </returns>
		public static bool IsThisOrSubclassOf(this Type type, Type c)
		{
			return type == c || type.IsSubclassOf(c);
		}
	}

    /// <summary>
    /// ResourceManager extensions.
    /// </summary>
	public static class ResourceManagerExtension
	{
		/// <summary>
		/// Get string from resource, return string.Empty if failed.
		/// </summary>
		/// <param name="manager">The manager.</param>
		/// <param name="name">The name.</param>
		/// <returns></returns>
		public static string TryGetString(this ResourceManager manager, string name)
		{
			try
			{
				return manager.GetString(name);
			}
			catch
			{
				return string.Empty;
			}
		}

		/// <summary>
		/// Get object from resource, return null if failed.
		/// </summary>
		/// <param name="manager">The manager.</param>
		/// <param name="name">The name.</param>
		/// <returns></returns>
		public static object TryGetObject(this ResourceManager manager, string name)
		{
			try
			{
				return manager.GetObject(name);
			}
			catch
			{
				return null;
			}
		}

		public static ImageSource GetImageSource(this ResourceManager manager, string name)
		{
			object bmp = null;

			//1. Get Image from resource manager
			if (manager != null)
			{
				bmp = manager.TryGetObject(name);
			}

			//2. Get Image from embedded resource(name = uri)
			if (bmp == null)
			{
				try
				{
					bmp = new System.Drawing.Bitmap(Application.GetResourceStream(
						new Uri(string.Format(@"/{0};component/Resources/Images/{1}", Assembly.GetCallingAssembly().GetName().Name, name), UriKind.Relative)).Stream);
				}
				catch
				{
					return null;
				}
			}

			IntPtr hBitmap = ((System.Drawing.Bitmap)bmp).GetHbitmap();
			BitmapSource bs;
			try
			{
				bs = Imaging.CreateBitmapSourceFromHBitmap(hBitmap, IntPtr.Zero,
														   Int32Rect.Empty, BitmapSizeOptions.FromEmptyOptions());
				bs.Freeze();
			}
			finally
			{
				NativeMethods.DeleteObject(hBitmap);
			}
			return bs;
		}
	}

    /// <summary>
    /// Action extensions.
    /// </summary>
	public static class ActionExtension
	{
		/// <summary>
		/// Executes the action on the UI thread.
		/// </summary>
		/// <param name="action">The action to execute.</param>
		/// <param name="delay">The delay.</param>
		public static void OnUIThread(this System.Action action, TimeSpan delay)
		{
			if (delay.TotalMilliseconds < 0)
				throw new ArgumentOutOfRangeException("delay", delay, "Value must be positive");

			Execute.OnUIThread(
				delegate
				{
					DispatcherTimer t = new DispatcherTimer();
					t.Interval = delay;
					t.Tick += delegate
					{
						t.Stop();
						action();
					};
					t.Start();
				});
		}
	}
}