using System;
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.TemplateTheTree
{
	public class TemplateTheTree : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new TemplateTheTree());
		}

		public TemplateTheTree()
		{
			Title = "Template the Tree";

			TreeView tree = new TreeView();
			Content = tree;

			HierarchicalDataTemplate template = new HierarchicalDataTemplate(typeof(DiskDirectory));
			template.ItemsSource = new Binding("Subdirectories");
	
			FrameworkElementFactory factoryTextBlock = new FrameworkElementFactory(typeof(TextBlock));
			factoryTextBlock.SetBinding(TextBlock.TextProperty, new Binding("Name"));

			template.VisualTree = factoryTextBlock;

			DiskDirectory dir = new DiskDirectory(new DirectoryInfo(Path.GetPathRoot(Environment.SystemDirectory)));
			TreeViewItem item = new TreeViewItem();
			item.Header = dir.Name;
			item.ItemsSource = dir.Subdirectories;
			item.ItemTemplate = template;

			tree.Items.Add(item);
			item.IsExpanded = true;
		}
	}
}

