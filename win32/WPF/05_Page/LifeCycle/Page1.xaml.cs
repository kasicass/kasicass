using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Threading;

namespace LifeCycle
{
    /// <summary>
    /// Interaction logic for Page1.xaml
    /// </summary>
    public partial class Page1 : Page
    {
        public Page1()
        {
            System.Console.WriteLine("Page1 Created");
            InitializeComponent();
        }

        private void Hyperlink_Click(object sender, RoutedEventArgs e)
        {
            NavigationService.Navigate(new Uri("pack://application:,,,/Page2.xaml"));
            Thread.Sleep(500);
            NavigationService.StopLoading();
        }

        private void Hyperlink_Refresh(object sender, RoutedEventArgs e)
        {
            NavigationService.Refresh();
        }

        private void Page_Loaded(object sender, RoutedEventArgs e)
        {
            System.Console.WriteLine("Page1 Loaded");
        }

        private void Page_Unloaded(object sender, RoutedEventArgs e)
        {
            System.Console.WriteLine("Page1 UnLoaded");
        }
    }
}
