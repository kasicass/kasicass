using System;
using System.Collections.Generic;
using System.Reflection;
using System.Windows;
using System.Windows.Controls;

namespace Petzold.ShowClassHierarchy
{
	public class ClassHierarchyTreeView : TreeView
	{
		public ClassHierarchyTreeView(Type typeRoot)
		{
			UIElement dummy = new UIElement();
			List<Assembly> assemblies = new List<Assembly>();

			AssemblyName[] anames = Assembly.GetExecutingAssembly().GetReferencedAssemblies();
			foreach (AssemblyName aname in anames)
			{
				assemblies.Add(Assembly.Load(aname));
			}

			SortedList<string, Type> classes = new SortedList<string, Type>();
			foreach (Assembly assembly in assemblies)
			{
				foreach (Type typ in assembly.GetTypes())
				{
					if (typ.IsPublic && typ.IsSubclassOf(typeRoot))
					{
						classes.Add(typ.Name, typ);
					}
				}
			}

			TypeTreeViewItem item = new TypeTreeViewItem(typeRoot);
			Items.Add(item);

			CreateLinkedItems(item, classes);
		}

		void CreateLinkedItems(TypeTreeViewItem itemBase, SortedList<string, Type> list)
		{
			foreach (KeyValuePair<string, Type> kvp in list)
			{
				if (kvp.Value.BaseType == itemBase.Type)
				{
					TypeTreeViewItem item = new TypeTreeViewItem(kvp.Value);
					itemBase.Items.Add(item);
					CreateLinkedItems(item, list);
				}
			}
		}
	}
}

