using System;
using System.Windows;

namespace Petzold.InheritAppAndWindow
{
    class MyApplication : Application
    {
        protected override void OnStartup(StartupEventArgs args)
        {
            base.OnStartup(args);

            MyWindow win = new MyWindow();
            win.Show();
        }
    }
}