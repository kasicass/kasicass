using Petzold.ListSystemParameters;
using System;
using System.Collections.Generic;
using System.Reflection;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.ListSortedSystemParameters
{
	public class ListSortedSystemParameters : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new ListSortedSystemParameters());
		}

		public ListSortedSystemParameters()
		{
			Title = "List Sorted System Parameters";

			ListView lstvue = new ListView();
			Content = lstvue;

			GridView grdvue = new GridView();
			lstvue.View = grdvue;

			GridViewColumn col = new GridViewColumn();
			col.Header = "Property Name";
			col.Width = 200;
			col.DisplayMemberBinding = new Binding("Name");
			grdvue.Columns.Add(col);

			col = new GridViewColumn();
			col.Header = "Value";
			col.Width = 200;
			grdvue.Columns.Add(col);

			DataTemplate template = new DataTemplate(typeof(string));
			FrameworkElementFactory factoryTextBlock = new FrameworkElementFactory(typeof(TextBlock));
			factoryTextBlock.SetValue(TextBlock.HorizontalAlignmentProperty, HorizontalAlignment.Right);
			factoryTextBlock.SetBinding(TextBlock.TextProperty, new Binding("Value"));
			template.VisualTree = factoryTextBlock;
			col.CellTemplate = template;

			PropertyInfo[] props = typeof(SystemParameters).GetProperties();
			SortedList<string, SystemParam> sortlist = new SortedList<string, SystemParam>();
			foreach (PropertyInfo prop in props)
			{
				if (prop.PropertyType != typeof(ResourceKey))
				{
					SystemParam sysparam = new SystemParam();
					sysparam.Name = prop.Name;
					sysparam.Value = prop.GetValue(null, null);
					sortlist.Add(prop.Name, sysparam);
				}
			}

			lstvue.ItemsSource = sortlist.Values;
		}
	}
}

