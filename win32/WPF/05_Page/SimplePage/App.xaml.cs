using System;
using System.Windows;
using System.Windows.Navigation;

namespace SimplePage
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application
    {
        private void Application_Startup(object sender, StartupEventArgs e)
        {
            NavigationWindow win = new NavigationWindow();
            win.Content = new MyPage();
            win.Show();
        }
    }
}
