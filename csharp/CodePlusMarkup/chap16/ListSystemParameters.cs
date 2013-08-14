using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Reflection;
using System.Windows;
using System.Windows.Data;
using System.Windows.Input;
using System.Windows.Controls;
using System.Windows.Media;

namespace Petzold.ListSystemParameters
{
	class ListSystemParameters : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new ListSystemParameters());
		}

		public ListSystemParameters()
		{
			Title = "List System Parameters";

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
			col.DisplayMemberBinding = new Binding("Value");
			grdvue.Columns.Add(col);

			PropertyInfo[] props = typeof(SystemParameters).GetProperties();
			foreach (PropertyInfo prop in props)
			{
				if (prop.PropertyType != typeof(ResourceKey))
				{
					SystemParam sysparam = new SystemParam();
					sysparam.Name = prop.Name;
					sysparam.Value = prop.GetValue(null, null);
					lstvue.Items.Add(sysparam);
				}
			}
		}
	}
}

