using System;
using System.Windows;

namespace Petzold.InheritAppAndWindow
{
    class InheritAppAndWindow
    {
        [STAThread]
        public static void Main()
        {
            MyApplication app = new MyApplication();
            app.Run();
        }
    }
}