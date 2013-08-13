using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.ManuallyPopulateTreeView
{
	public class ManuallyPopulateTreeView : Window
	{
		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new ManuallyPopulateTreeView());
		}

		public ManuallyPopulateTreeView()
		{
			Title = "Manually Populate TreeView";

			TreeView tree = new TreeView();
			Content = tree;

			TreeViewItem itemAnimal = new TreeViewItem();
			itemAnimal.Header = "Animal";
			tree.Items.Add(itemAnimal);

			TreeViewItem itemDog = new TreeViewItem();
			itemDog.Header = "Dog";
			itemDog.Items.Add("Poodle");
			itemDog.Items.Add("Irish Setter");
			itemDog.Items.Add("German Shepherd");
			itemAnimal.Items.Add(itemDog);

			TreeViewItem itemCat = new TreeViewItem();
			itemCat.Header = "Cat";
			itemCat.Items.Add("Calico");

			TreeViewItem item = new TreeViewItem();
			item.Header = "Alley Cat";
			itemCat.Items.Add(item);
			Button btn = new Button();
			btn.Content = "Noodles";
			itemCat.Items.Add(btn);

			itemCat.Items.Add("Siamese");
			itemAnimal.Items.Add(itemCat);

			TreeViewItem itemPrimate = new TreeViewItem();
			itemPrimate.Header = "Primate";
			itemPrimate.Items.Add("Chimpanzee");
			itemPrimate.Items.Add("Bonobo");
			itemPrimate.Items.Add("Human");
			itemAnimal.Items.Add(itemPrimate);

			TreeViewItem itemMineral = new TreeViewItem();
			itemMineral.Header = "Mineral";
			itemMineral.Items.Add("Calcium");
			itemMineral.Items.Add("Zinc");
			itemMineral.Items.Add("Iron");
			tree.Items.Add(itemMineral);

			TreeViewItem itemVegetable = new TreeViewItem();
			itemVegetable.Header = "Vegetable";
			itemVegetable.Items.Add("Carrot");
			itemVegetable.Items.Add("Asparagus");
			itemVegetable.Items.Add("Broccoli");
			tree.Items.Add(itemVegetable);
		}
	}
}

