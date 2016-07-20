using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Linq;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Navigation;

namespace LifeCycle
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application
    {
        private void Application_Navigating(object sender, NavigatingCancelEventArgs e)
        {
            System.Console.WriteLine("Application_Navigating, Uri: {0}", e.Uri.ToString());
        }

        private void Application_NavigationFailed(object sender, NavigationFailedEventArgs e)
        {
            System.Console.WriteLine("Application_NavigationFailed, Exception: {0}", e.Exception.ToString());
            e.Handled = true;
        }

        private void Application_Navigated(object sender, NavigationEventArgs e)
        {
            System.Console.WriteLine("Application_Navigated, Uri: {0}", e.Uri.ToString());
        }

        private void Application_NavigationProgress(object sender, NavigationProgressEventArgs e)
        {
            System.Console.WriteLine("Application_Navigated, Uri: {0}, BytesRead: {1}", e.Uri.ToString(), e.BytesRead);
        }

        private void Application_NavigationStopped(object sender, NavigationEventArgs e)
        {
            System.Console.WriteLine("Application_NavigationStopped, Uri: {0}", e.Uri.ToString());
        }

        private void Application_LoadCompleted(object sender, NavigationEventArgs e)
        {
            System.Console.WriteLine("Application_LoadCompleted, Uri: {0}", e.Uri.ToString());
        }

        private void Application_FragmentNavigation(object sender, FragmentNavigationEventArgs e)
        {
            System.Console.WriteLine("Application_FragmentNavigation, Fragment: {0}", e.Fragment);
        }
    }
}
