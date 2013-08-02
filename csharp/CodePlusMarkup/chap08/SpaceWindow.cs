using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.SetSpaceProperty
{
	public class SpaceWindow : Window
	{
		public static readonly DependencyProperty SpaceProperty;

		public int Space
		{
			set
			{
				SetValue(SpaceProperty, value);
			}
			get
			{
				return (int)GetValue(SpaceProperty);
			}
		}

		static SpaceWindow()
		{
			FrameworkPropertyMetadata metadata = new FrameworkPropertyMetadata();
			metadata.Inherits = true;

			SpaceProperty = SpaceButton.SpaceProperty.AddOwner(typeof(SpaceWindow));
			SpaceProperty.OverrideMetadata(typeof(SpaceWindow), metadata);
		}
	}
}

