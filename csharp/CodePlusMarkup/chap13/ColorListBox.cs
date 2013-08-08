using System;
using System.Reflection;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.ListColorsElegantly
{
	class ColorListBox : ListBox
	{
		public ColorListBox()
		{
			PropertyInfo[] props = typeof(Colors).GetProperties();
			foreach (PropertyInfo prop in props)
			{
				ColorListBoxItem item = new ColorListBoxItem();
				item.Text = prop.Name;
				item.Color = (Color)prop.GetValue(null, null);
				Items.Add(item);
			}
			SelectedValuePath = "Color";
		}

		public Color SelectedColor
		{
			set { SelectedValue = value; }
			get { return SelectedValue == null ? Colors.White : (Color)SelectedValue; }
		}
	}
}

