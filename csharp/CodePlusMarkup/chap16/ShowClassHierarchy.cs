using System;
using System.Collections.Generic;
using System.Reflection;
using System.Windows;
using System.Windows.Controls;

namespace Petzold.ShowClassHierarchy
{
	class ShowClassHierarchy : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new ShowClassHierarchy());
		}

		public ShowClassHierarchy()
		{
			Title = "Show Class Hierarchy";

			ClassHierarchyTreeView tree = new ClassHierarchyTreeView(typeof(System.Windows.Threading.DispatcherObject));
			Content = tree;
		}
	}
}

