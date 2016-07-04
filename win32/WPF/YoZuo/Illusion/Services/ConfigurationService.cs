using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel;
using System.Xml;
using System.Xml.Linq;
using System.IO;
using System.Globalization;
using System.Linq.Expressions;
using System.Windows;

namespace Illusion
{
	/// <summary>
	/// Denotes the basic object for configuration, each <see cref="IConfigurationObject"/> persists as a xml node.
	/// </summary>
	/// <example>
	/// 1. Save
	/// Window window = new Window();
	/// IConfigurationObject configObj = IoC.Get<IConfigurationService>()["MainWindow"];
	/// configObj.SetProperty("Width", window.Width);
	/// 
	/// 2. Load
	/// window.Width = configObj.GetProperty<double>("Width");
	/// 
	/// 3. Use lambda expression to load/Save
	/// configObj.SaveProperty<Window>(window, i=> i.Width, i => i.Height);
	/// configObj.LoadProperty<Window>(window, i=> i.Width, i => i.Height);
	/// </example>
	public interface IConfigurationObject : INotifyPropertyChanged
	{
		void Clear();
		void ClearProperty(string name);
		//TODO: It should support children nodes, not implement yet. 
		IConfigurationObject CreateConfigurationObject();

		bool HasProperty(string name);
		object GetProperty(string name);
		object GetProperty(string name, object defaultValue);
		T GetProperty<T>(string name);
		T GetProperty<T>(string name, T defaultValue);
		IConfigurationObject SetProperty(string name, object value);
		IConfigurationObject SetProperty(string name, object value, object defaultValue);

		//Load & Save all specific properties
		void LoadProperty<T>(T t, params Expression<Func<T, object>>[] props);
		void SaveProperty<T>(T t, params Expression<Func<T, object>>[] props);
	}

	/// <summary>
	/// Defines the service that provide configuration, internally use <see cref="IConfigurationObject"/> to do the save/load logic.
	/// </summary>
	public interface IConfigurationService
	{
		void Load();
		void Save();

		string ConfigurationDirectoryPath { get; }
		IConfigurationObject this[string name] { get; set; }
		IConfigurationObject this[IHaveName display] { get; set; }
	}

	/// <summary>
	/// Constants for Configuration
	/// </summary>
	internal class ConfigurationConstant
	{
		internal const string CONFIG_OBJECT = "ConfigurationObject";
		internal const string CONFIG_PROPERTY = "Property";
		internal const string CONFIG_TYPE = "Type";
		internal const string CONFIG_NAME = "Name";
	}

	/// <summary>
	/// Default implementation of <see cref="IConfigurationObject"/>.
	/// </summary>
	internal class ConfigurationObject : IConfigurationObject, INotifyPropertyChanged
	{
		#region Field

		private static ILog Log = LogManager.GetLog(typeof(ConfigurationObject));
		private SortedList properties = new SortedList();

		#endregion

		#region IConfigurationObject Memebers

		public void Clear()
		{
			string[] array = new string[this.properties.Count];
			this.properties.Keys.CopyTo(array, 0);
			array.Apply(item => this.ClearProperty(item));
		}

		public void ClearProperty(string name)
		{
			if (this.properties.Contains(name))
			{
				this.properties.Remove(name);
				this.OnPropertyChanged(new PropertyChangedEventArgs(name));
			}
		}

		public IConfigurationObject CreateConfigurationObject()
		{
			return new ConfigurationObject();
		}

		public object GetProperty(string name)
		{
			return this.GetProperty(name, null);
		}

		public object GetProperty(string name, object defaultValue)
		{
			if (this.properties.Contains(name))
			{
				return this.properties[name];
			}
			return defaultValue;
		}

		public T GetProperty<T>(string name)
		{
			Type type = typeof(T);
			object obj = GetProperty(name);
			if (obj == null)
			{
				return default(T);
			}
			if (obj.GetType().IsThisOrSubclassOf(type))
			{
				return (T)obj;
			}
			throw new InvalidCastException("The property does not match the specify type");
		}

		public T GetProperty<T>(string name, T defaultValue)
		{
			Type type = typeof(T);
			object obj = GetProperty(name, defaultValue as object);
			if (obj == null)
			{
				return defaultValue;
			}
			if (obj.GetType().IsThisOrSubclassOf(type))
			{
				return (T)obj;
			}
			throw new InvalidCastException("The property does not match the specify type");
		}

		public bool HasProperty(string name)
		{
			return this.properties.Contains(name);
		}

		public IConfigurationObject SetProperty(string name, object value)
		{
			return this.SetProperty(name, value, null);
		}

		public IConfigurationObject SetProperty(string name, object value, object defaultValue)
		{
			if ((value == null) || object.Equals(value, defaultValue))
			{
				this.ClearProperty(name);
			}
			else if (!this.properties.Contains(name) || !object.Equals(this.properties[name], value))
			{
				this.properties[name] = value;
				this.OnPropertyChanged(new PropertyChangedEventArgs(name));
			}
			return this;
		}

		public void LoadProperty<T>(T t, params Expression<Func<T, object>>[] props)
		{
			if (props != null)
			{
				foreach (var prop in props)
				{
					string name = prop.GetMemberInfo().Name;
					if (HasProperty(name))
					{
						DynamicAccessEngine.SetProperty(t, name, GetProperty(name));
					}
				}
			}
		}
		public void SaveProperty<T>(T t, params Expression<Func<T, object>>[] props)
		{
			if (props != null)
			{
				foreach (var prop in props)
				{
					string name = prop.GetMemberInfo().Name;
					SetProperty(name, DynamicAccessEngine.GetProperty(t, name));
				}
			}
		}

		internal void Load(XElement element)
		{
			foreach (var node in element.Elements(ConfigurationConstant.CONFIG_PROPERTY))
			{
				string name = node.Attribute(XName.Get(ConfigurationConstant.CONFIG_NAME)).Value;
				Type type = typeof(string);
				var tp = node.Attribute(XName.Get(ConfigurationConstant.CONFIG_TYPE));
				if (tp != null && !string.IsNullOrEmpty(tp.Value))
				{
					type = Type.GetType(tp.Value);
				}

				TypeConverter converter = TypeDescriptor.GetConverter(type);
				if (converter != null)
				{
					try
					{
						object obj2 = converter.ConvertFromString(null, CultureInfo.InvariantCulture, node.Value);
						this.SetProperty(name, obj2);
					}
					catch (Exception e)
					{
						Log.Error(e);
					}
				}
			}
		}

		internal void Save(XElement element)
		{
			foreach (var key in properties.Keys)
			{
				object obj = this.properties[key];
				ConfigurationObject configObject = obj as ConfigurationObject;
				if (configObject != null)
				{
					configObject.Save(element);
				}
				else
				{
					XElement node = new XElement(ConfigurationConstant.CONFIG_PROPERTY);
					node.SetAttributeValue(ConfigurationConstant.CONFIG_NAME, key);
					Type type = obj.GetType();
					if (type != typeof(string))
					{
						node.SetAttributeValue(ConfigurationConstant.CONFIG_TYPE, type.AssemblyQualifiedName);
					}
					string text = TypeDescriptor.GetConverter(type).ConvertToString(null, CultureInfo.InvariantCulture, obj);
					node.Value = text;
					element.Add(node);
				}
			}
		}

		#endregion

		#region INotifyPropertyChanged Members

		public event PropertyChangedEventHandler PropertyChanged;

		protected void OnPropertyChanged(PropertyChangedEventArgs e)
		{
			if (this.PropertyChanged != null)
			{
				this.PropertyChanged(this, e);
			}
		}

		#endregion
	}

	/// <summary>
	/// Default implementation of <see cref="IConfigurationService"/>.
	/// </summary>
	public class ConfigurationService : IConfigurationService
	{
		#region Field

		static ILog Log = LogManager.GetLog(typeof(ConfigurationService));
		private SortedList configurations = new SortedList();
		private Version version = new Version(0, 1, 0, 0);
		private string configurationPath;

		#endregion

		#region Constructor

		public ConfigurationService(string configurationPath)
		{
			string folderPath = Environment.CurrentDirectory;
			this.configurationPath = Path.Combine(folderPath, configurationPath);
			this.Load();
		}

		#endregion

		#region Property

		public string ConfigurationDirectoryPath
		{
			get
			{
				return configurationPath;
			}
		}

		private string ConfigurationFileName
		{
			get
			{
				return Path.Combine(this.ConfigurationDirectoryPath, "user.config");
			}
		}

		protected SortedList Configurations
		{
			get
			{
				return this.configurations;
			}
		}

		#endregion

		#region IConfigurationService Members

		public void Load()
		{
			this.Configurations.Clear();
			try
			{
				if (File.Exists(this.ConfigurationFileName))
				{
					XElement root = XElement.Load(this.ConfigurationFileName);
					LoadInternal(root);
				}
			}
			catch (Exception e)
			{
				Log.Error(e);
			}
		}

		public void Save()
		{
			try
			{
				Directory.CreateDirectory(this.ConfigurationDirectoryPath);
				XElement element = new XElement(ConfigurationConstant.CONFIG_OBJECT);
				SaveInternal(element);
				XDocument document = new XDocument(element);
				document.Save(this.ConfigurationFileName);
			}
			catch (Exception e)
			{
				Log.Error(e);
			}
		}

		public IConfigurationObject this[string name]
		{
			get
			{
				if (this.configurations.Contains(name))
				{
					return (IConfigurationObject)this.configurations[name];
				}
				ConfigurationObject obj2 = new ConfigurationObject();
				this.configurations[name] = obj2;
				return obj2;
			}
			set
			{
				if (value == null)
				{
					if (this.configurations.Contains(name))
					{
						this.configurations.Remove(name);
					}
				}
				else
				{
					this.configurations[name] = value;
				}
			}
		}

		public IConfigurationObject this[IHaveName display]
		{
			get
			{
				if (display == null)
				{
					throw new ArgumentNullException("The specific display is null");
				}
				return this[display.Name];
			}
			set
			{
				if (display == null)
				{
					throw new ArgumentNullException("The specific display is null");
				}
				this[display.Name] = value;
			}
		}


		protected void LoadInternal(XElement element)
		{
			if (element != null)
			{
				foreach (var node in element.Elements())
				{
					LoadInternal(node);
				}

				string name = element.Attribute(XName.Get(ConfigurationConstant.CONFIG_NAME)).Value;
				((ConfigurationObject)this[name]).Load(element);
			}
		}

		protected void SaveInternal(XElement element)
		{
			if (element != null)
			{
				string[] array = new string[this.configurations.Count];
				this.configurations.Keys.CopyTo(array, 0);
				foreach (string str in array)
				{
					var node = new XElement(ConfigurationConstant.CONFIG_OBJECT);
					node.SetAttributeValue(ConfigurationConstant.CONFIG_NAME, str);
					((ConfigurationObject)this.configurations[str]).Save(node);
					element.Add(node);
				}
			}
		}

		#endregion
	}

	/// <summary>
	/// Denotes the target site for monitor configuration.
	/// </summary>
	public interface IConfigurationTarget
	{
		PropertyObserver Monitor { get; }
	}

	public static class IConfigurationTargetExtension
	{
		public static void Bind<T>(this IConfigurationTarget target, IConfigurationObject configObject, Expression<Func<T, object>> sourceProp, Action<IConfigurationObject> handler)
		{
			string configProp = sourceProp.GetMemberInfo().Name;
			Bind(target, configObject, configProp, handler);
		}

		public static void Bind(this IConfigurationTarget target, IConfigurationObject configObject, string configProp, Action<IConfigurationObject> handler)
		{
			if (target.Monitor != null)
			{
				var observer = target.Monitor.GetObserver(configObject);
				observer.RegisterHandler(configProp, handler);
			}
		}

		public static void UnBind<T>(this IConfigurationTarget target, IConfigurationObject configObject, Expression<Func<T, object>> sourceProp)
		{
			string configProp = sourceProp.GetMemberInfo().Name;
			UnBind(target, configObject, configProp);
		}

		public static void UnBind(this IConfigurationTarget target, IConfigurationObject configObject, string configProp)
		{
			if (target.Monitor != null)
			{
				var observer = target.Monitor.GetObserver(configObject);
				observer.UnregisterHandler(configProp);
			}
		}
	}

	/// <summary>
	/// Observer class to monitor PropertyChanged.
	/// </summary>
	public class PropertyObserver
	{
		private Dictionary<int, PropertyObserver<IConfigurationObject>> observers = new Dictionary<int, PropertyObserver<IConfigurationObject>>();

		public void RemoveObserver(IConfigurationObject obj)
		{
			if (ContainsObserver(obj))
			{
				observers.Remove(obj.GetHashCode());
			}
		}

		public PropertyObserver<IConfigurationObject> GetObserver(IConfigurationObject obj)
		{
			int key = obj.GetHashCode();
			if (!ContainsObserver(obj))
			{
				PropertyObserver<IConfigurationObject> pb = new PropertyObserver<IConfigurationObject>(obj);
				observers.Add(key, pb);
			}

			return observers[key];
		}

		private bool ContainsObserver(IConfigurationObject obj)
		{
			if (observers.ContainsKey(obj.GetHashCode()))
			{
				return true;
			}
			return false;
		}

		protected virtual PropertyObserver<IConfigurationObject> CreateObserver(IConfigurationObject obj)
		{
			ConfigurationObject configObject = obj as ConfigurationObject;
			if (configObject != null)
			{
				return new PropertyObserver<IConfigurationObject>(configObject);
			}
			return null;
		}
	}
}
