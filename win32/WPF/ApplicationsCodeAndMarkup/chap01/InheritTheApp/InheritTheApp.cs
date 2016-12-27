using System;
using System.Windows;

namespace Petzold.InheritTheApp
{
    class InheritTheApp : Application
    {
        [STAThread]
        public static void Main()
        {
            InheritTheApp app = new InheritTheApp();
            app.Run();
        }

        protected override void OnStartup(StartupEventArgs args)
        {
            base.OnStartup(args);

            Window win = new Window();
            win.Title = "Inherit the App";
            win.Show();
        }

        protected override void OnSessionEnding(SessionEndingCancelEventArgs args)
        {
            base.OnSessionEnding(args);

            MessageBoxResult result = MessageBox.Show("Do you want to save your data?",
                MainWindow.Title, MessageBoxButton.YesNoCancel,
                MessageBoxImage.Question, MessageBoxResult.Yes);
        }
    }
}