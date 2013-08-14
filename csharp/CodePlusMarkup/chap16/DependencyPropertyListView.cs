using System;
using System.Collections.Generic;
using System.Reflection;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;

namespace Petzold.ExploreDependencyProperties
{
	public class DependencyPropertyListView : ListView
	{
		public static DependencyProperty TypeProperty;

		static DependencyPropertyListView()
		{
			TypeProperty = DependencyProperty.Register("Type", typeof(Type), typeof(DependencyPropertyListView),
				new PropertyMetadata(null, new PropertyChangedCallback(OnTypePropertyChanged)));
		}

		static void OnTypePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
		{
			DependencyPropertyListView lstvue = obj as DependencyPropertyListView;
			Type type = args.NewValue as Type;
			lstvue.ItemsSource = null;
			if (type != null)
			{
				SortedList<string, DependencyProperty> list = new SortedList<string, DependencyProperty>();
				FieldInfo[] infos = type.GetFields();
				foreach (FieldInfo info in infos)
				{
					if (info.FieldType == typeof(DependencyProperty))
					{
						list.Add(info.Name, (DependencyProperty)info.GetValue(null));
					}
				}
				lstvue.ItemsSource = list.Values;
			}
		}

		public Type Type
		{
			set { SetValue(TypeProperty, value); }
			get { return (Type)GetValue(TypeProperty); }
		}

		public DependencyPropertyListView()
		{
			GridView grdvue = new GridView();
			this.View = grdvue;

			GridViewColumn col = new GridViewColumn();
			col.Header = "Name";
			col.Width = 150;
			col.DisplayMemberBinding = new Binding("Name");
			grdvue.Columns.Add(col);
	
			col = new GridViewColumn();
			col.Header = "Owner";
			col.Width = 100;
			grdvue.Columns.Add(col);

			DataTemplate template = new DataTemplate();
			col.CellTemplate = template;

			FrameworkElementFactory elTextBlock = new FrameworkElementFactory(typeof(TextBlock));
			template.VisualTree = elTextBlock;

			Binding bind = new Binding("OwnerType");
			bind.Converter = new TypeToString();
			elTextBlock.SetBinding(TextBlock.TextProperty, bind);

			col = new GridViewColumn();
			col.Header = "Type";
			col.Width = 100;
			grdvue.Columns.Add(col);

			template = new DataTemplate();
			col.CellTemplate = template;
			elTextBlock = new FrameworkElementFactory(typeof(TextBlock));
			template.VisualTree = elTextBlock;
			bind = new Binding("PropertyType");
			bind.Converter = new TypeToString();
			elTextBlock.SetBinding(TextBlock.TextProperty, bind);

			col = new GridViewColumn();
			col.Header = "Default";
			col.Width = 75;
			col.DisplayMemberBinding = new Binding("DefaultMetadata.DefaultValue");
			grdvue.Columns.Add(col);

			col = new GridViewColumn();
			col.Header = "Raed-Only";
			col.Width = 75;
			col.DisplayMemberBinding = new Binding("DefaultMetadata.ReadOnly");
			grdvue.Columns.Add(col);

			col = new GridViewColumn();
			col.Header = "Usage";
			col.Width = 75;
			col.DisplayMemberBinding = new Binding("DefaultMetadata.AttachedPropertyUsage");
			grdvue.Columns.Add(col);

			col = new GridViewColumn();
			col.Header = "Flags";
			col.Width = 250;
			grdvue.Columns.Add(col);

			template = new DataTemplate();
			col.CellTemplate = template;
			elTextBlock = new FrameworkElementFactory(typeof(TextBlock));
			template.VisualTree = elTextBlock;
			bind = new Binding("DefaultMetadata");
			bind.Converter = new MetadataToFlags();
			elTextBlock.SetBinding(TextBlock.TextProperty, bind);
		}
	}
}

