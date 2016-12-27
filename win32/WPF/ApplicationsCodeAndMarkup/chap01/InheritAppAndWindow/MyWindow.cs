using System;
using System.Windows;
using System.Windows.Input;

namespace Petzold.InheritAppAndWindow
{
    public class MyWindow : Window
    {
        public MyWindow()
        {
            Title = "Inhrit App & Window";
        }

        protected override void OnMouseDown(MouseButtonEventArgs args)
        {
            base.OnMouseDown(args);

            string message = string.Format("Window clicked with {0} button at point ({1})",
                args.ChangedButton, args.GetPosition(this));
            MessageBox.Show(message, Title);
        }
    }
}