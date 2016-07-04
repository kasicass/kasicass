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

namespace RoutedEventTravel
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            this.simpleBtn.ClassHandlerProcessed += simpleBtn_RaisedClass;
        }

        protected int eventCounter = 0;

        private void InsertList(object sender, RoutedEventArgs e)
        {
            eventCounter++;
            string message = "#" + eventCounter.ToString() + ":\r\n" +
                "InsertList\r\n" +
                "  Sender: " + sender.ToString() + "\r\n" +
                "  Source: " + e.Source + "\r\n" +
                "  Origianl Source: " + e.OriginalSource;
            firstMessages.Items.Add(message);
            e.Handled = (bool)chkHandle.IsChecked;
        }

        private void cmdClear_Click(object sender, RoutedEventArgs e)
        {
            eventCounter = 0;
            firstMessages.Items.Clear();
        }

        private void simpleBtn_RaisedClass(object sender, EventArgs e)
        {
            eventCounter++;
            string message = "#" + eventCounter.ToString() + ":\r\n" +
                "Windows Class Handler\r\n" +
                "  Sender: " + sender.ToString();
            firstMessages.Items.Add(message);
        }

        private void ProcessHandlersToo(object sender, RoutedEventArgs e)
        {
            eventCounter++;
            string message = "#" + eventCounter.ToString() + ":\r\n" +
                "ProcessHandlersToo\r\n" +
                "  Sender: " + sender.ToString() + "\r\n" +
                "  Source: " + e.Source + "\r\n" +
                "  Original Source: " + e.OriginalSource;
            firstMessages.Items.Add(message);
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            grid1.AddHandler(MySimpleButton.CustomClickEvent, new RoutedEventHandler(ProcessHandlersToo), true);
        }
    }
}
