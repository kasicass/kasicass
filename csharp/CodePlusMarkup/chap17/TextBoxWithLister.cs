using System;
using System.Collections;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.ChooseFont
{
	class TextBoxWithLister : ContentControl
	{
		TextBox txtbox;
		Lister lister;
		bool isReadOnly;

		public event EventHandler SelectionChanged;
		public event TextChangedEventHandler TextChanged;

		public TextBoxWithLister()
		{
			DockPanel dock = new DockPanel();
			Content = dock;

			txtbox = new TextBox();
			txtbox.TextChanged += TextBoxOnTextChanged;
			dock.Children.Add(txtbox);
			DockPanel.SetDock(txtbox, Dock.Top);

			lister = new Lister();
			lister.SelectionChanged += ListerOnSelectionChanged;
			dock.Children.Add(lister);
		}

		public string Text
		{
			get { return txtbox.Text; }
			set { txtbox.Text = value; }
		}

		public bool IsReadOnly
		{
			set { isReadOnly = value; }
			get { return isReadOnly; }
		}

		public object SelectedItem
		{
			set
			{
				lister.SelectedItem = value;
				if (lister.SelectedItem != null)
					txtbox.Text = lister.SelectedItem.ToString();
				else
					txtbox.Text = "";
			}
			get
			{
				return lister.SelectedItem;
			}
		}

		public int SelectedIndex
		{
			set
			{
				lister.SelectedIndex = value;
				if (lister.SelectedIndex == -1)
					txtbox.Text = "";
				else
					txtbox.Text = lister.SelectedItem.ToString();
			}
			get
			{
				return lister.SelectedIndex;
			}
		}

		public void Add(object obj)
		{
			lister.Add(obj);
		}

		public void Insert(int index, object obj)
		{
			lister.Insert(index, obj);
		}

		public void Clear()
		{
			lister.Clear();
		}

		public bool Contains(object obj)
		{
			return lister.Contains(obj);
		}
	
		protected override void OnMouseDown(MouseButtonEventArgs args)
		{
			base.OnMouseDown(args);
			Focus();
		}

		protected override void OnGotKeyboardFocus(KeyboardFocusChangedEventArgs args)
		{
			base.OnGotKeyboardFocus(args);
			if (args.NewFocus == this)
			{
				txtbox.Focus();
				if (SelectedIndex == -1 && lister.Count > 0)
					SelectedIndex = 0;
			}
		}

		protected override void OnPreviewTextInput(TextCompositionEventArgs args)
		{
			base.OnPreviewTextInput(args);
			if (IsReadOnly)
			{
				lister.GoToLetter(args.Text[0]);
				args.Handled = true;
			}
		}

		protected override void OnPreviewKeyDown(KeyEventArgs args)
		{
			base.OnKeyDown(args);
			if (SelectedIndex == -1)
				return;

			switch (args.Key)
			{
			case Key.Home:
				if (lister.Count > 0)
					SelectedIndex = 0;
				break;

			case Key.End:
				if (lister.Count > 0)
					SelectedIndex = lister.Count - 1;
				break;

			case Key.Up:
				if (SelectedIndex > 0)
					SelectedIndex++;
				break;

			case Key.Down:
				if (SelectedIndex < lister.Count - 1)
					SelectedIndex++;
				break;

			case Key.PageUp:
				lister.PageUp();
				break;

			case Key.PageDown:
				lister.PageDown();
				break;

			default:
				return;
			}
			args.Handled = true;
		}

		void ListerOnSelectionChanged(object sender, EventArgs args)
		{
			if (SelectedIndex == -1)
				txtbox.Text = "";
			else
				txtbox.Text = lister.SelectedItem.ToString();

			OnSelectionChanged(args);
		}

		void TextBoxOnTextChanged(object sender, TextChangedEventArgs args)
		{
			if (TextChanged != null)
				TextChanged(this, args);
		}

		protected virtual void OnSelectionChanged(EventArgs args)
		{
			if (SelectionChanged != null)
				SelectionChanged(this, args);
		}
	}
}

