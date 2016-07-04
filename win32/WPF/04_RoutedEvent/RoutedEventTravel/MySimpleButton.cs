using System;
using System.Windows;
using System.Windows.Controls;

namespace RoutedEventTravel
{
    public class MySimpleButton : Button
    {
        static MySimpleButton()
        {
            EventManager.RegisterClassHandler(typeof(MySimpleButton), CustomClickEvent, new RoutedEventHandler(CustomClickClassHandler), false);
        }

        public event EventHandler ClassHandlerProcessed;
        public static void CustomClickClassHandler(object sender, RoutedEventArgs e)
        {
            MySimpleButton simpleBtn = sender as MySimpleButton;
            EventArgs args = new EventArgs();
            simpleBtn.ClassHandlerProcessed(simpleBtn, args);
        }

        public static readonly RoutedEvent CustomClickEvent = EventManager.RegisterRoutedEvent("CustomClick", RoutingStrategy.Bubble, typeof(RoutedEventHandler), typeof(MySimpleButton));
        public event RoutedEventHandler CustomClick
        {
            add { AddHandler(CustomClickEvent, value); }
            remove { RemoveHandler(CustomClickEvent, value); }
        }

        void RaiseCustomClickEvent()
        {
            RoutedEventArgs e = new RoutedEventArgs(MySimpleButton.CustomClickEvent);
            RaiseEvent(e);
        }

        protected override void OnClick()
        {
            RaiseCustomClickEvent();
        }
    }
}
