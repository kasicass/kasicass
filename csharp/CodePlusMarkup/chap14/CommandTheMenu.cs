using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.CommandTheMenu
{
	public class CommandTheMenu : Window
	{
		TextBlock text;

		[STAThread]
		public static void Main()
		{
			Application app = new Application();
			app.Run(new CommandTheMenu());
		}

		public CommandTheMenu()
		{
			Title = "Command The Menu";

			DockPanel dock = new DockPanel();
			Content = dock;

			Menu menu = new Menu();
			dock.Children.Add(menu);
			DockPanel.SetDock(menu, Dock.Top);

			text = new TextBlock();
			text.Text = "Sample clipboard text";
			text.HorizontalAlignment = HorizontalAlignment.Center;
			text.VerticalAlignment = VerticalAlignment.Center;
			text.FontSize = 32;
			text.TextWrapping = TextWrapping.Wrap;
			dock.Children.Add(text);
	
			MenuItem itemEdit = new MenuItem();
			itemEdit.Header = "_Edit";
			menu.Items.Add(itemEdit);

			MenuItem itemCut = new MenuItem();
			itemCut.Header = "Cu_t";
			itemCut.Command = ApplicationCommands.Cut;
			itemEdit.Items.Add(itemCut);

			MenuItem itemCopy = new MenuItem();
			itemCopy.Header = "_Copy";
			itemCopy.Command = ApplicationCommands.Copy;
			itemEdit.Items.Add(itemCopy);

			MenuItem itemPaste = new MenuItem();
			itemPaste.Header = "_Paste";
			itemPaste.Command = ApplicationCommands.Paste;
			itemEdit.Items.Add(itemPaste);

			MenuItem itemDelete = new MenuItem();
			itemDelete.Header = "_Delete";
			itemDelete.Command = ApplicationCommands.Delete;
			itemEdit.Items.Add(itemDelete);

			// new command
			InputGestureCollection collGestures = new InputGestureCollection();
			collGestures.Add(new KeyGesture(Key.R, ModifierKeys.Control));
			RoutedUICommand commRestore = new RoutedUICommand("_Restore", "Restore", GetType(), collGestures);

			MenuItem itemRestore = new MenuItem();
			itemRestore.Header = "_Restore";
			itemRestore.Command = commRestore;
			itemEdit.Items.Add(itemRestore);

			CommandBindings.Add(new CommandBinding(ApplicationCommands.Cut, CutOnExecute, CutCanExecute));
			CommandBindings.Add(new CommandBinding(ApplicationCommands.Copy, CopyOnExecute, CutCanExecute));
			CommandBindings.Add(new CommandBinding(ApplicationCommands.Paste, PasteOnExecute, PasteCanExecute));
			CommandBindings.Add(new CommandBinding(ApplicationCommands.Delete, DeleteOnExecute, CutCanExecute));
			CommandBindings.Add(new CommandBinding(commRestore, RestoreOnExecute));
		}

		void CutCanExecute(object sender, CanExecuteRoutedEventArgs args)
		{
			args.CanExecute = text.Text != null && text.Text.Length > 0;
		}

		void PasteCanExecute(object sender, CanExecuteRoutedEventArgs args)
		{
			args.CanExecute = Clipboard.ContainsText();
		}

		void CutOnExecute(object sender, ExecutedRoutedEventArgs args)
		{
			ApplicationCommands.Copy.Execute(null, this);
			ApplicationCommands.Delete.Execute(null, this);
		}

		void CopyOnExecute(object sender, ExecutedRoutedEventArgs args)
		{
			Clipboard.SetText(text.Text);
		}

		void PasteOnExecute(object sender, ExecutedRoutedEventArgs args)
		{
			text.Text = Clipboard.GetText();
		}

		void DeleteOnExecute(object sender, ExecutedRoutedEventArgs args)
		{
			text.Text = null;
		}

		void RestoreOnExecute(object sender, ExecutedRoutedEventArgs args)
		{
			text.Text = "Sample clipboard text";
		}
	}
}

