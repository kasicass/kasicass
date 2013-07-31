using System;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.SetSpaceProperty
{
	public class SpaceButton : Button
	{
		string txt;

		public string Text
		{
			set
			{
				txt = value;
				Content = SpaceOutText(txt);
			}
			get
			{
				return txt;
			}
		}

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

		static SpaceButton()
		{
			FrameworkPropertyMetadata metadata = new FrameworkPropertyMetadata();
			metadata.DefaultValue = 1;
			metadata.AffectsMeasure = true;
			metadata.Inherits = true;
			metadata.PropertyChangedCallback += OnSpacePropertyChanged;
		}
	}
}

