using System;
using System.Text;
using System.Windows;
using System.Windows.Controls;

namespace ButtonTest02
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
            metadata.DefaultValue = 0;
            metadata.PropertyChangedCallback += OnSpacePropertyChanged;
            metadata.Inherits = true;
            SpaceProperty = DependencyProperty.Register("Space", typeof(int), typeof(SpaceButton), metadata, ValidateSpaceValue);
        }

        static bool ValidateSpaceValue(object obj)
        {
            int i = (int)obj;
            return i >= 0;
        }

        static void OnSpacePropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args)
        {
            SpaceButton btn = obj as SpaceButton;
            string txt = btn.Content as string;
            if (txt == null) return;
            btn.Content = btn.SpaceOutText(txt);
        }

        string SpaceOutText(string str)
        {
            if (str == null)
                return null;

            StringBuilder builder = new StringBuilder();

            foreach (char ch in str)
            {
                builder.Append(ch + new string(' ', Space));
            }

            return builder.ToString();
        }
    }
}
