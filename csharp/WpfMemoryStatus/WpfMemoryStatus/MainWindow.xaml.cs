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

namespace WpfMemoryStatus
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            FillStatus();
        }

        private void FillStatus()
        {
            var status = new MemoryStatus();
            ((Label)this.FindName("lblTotalPhys")).Content = status.AvailPhys;
            ((Label)this.FindName("lblAvailPhys")).Content = status.AvailPhys;
            ((Label)this.FindName("lblTotalPageFile")).Content = status.TotalPageFile;
            ((Label)this.FindName("lblAvailPageFile")).Content = status.AvailPageFile;
            ((Label)this.FindName("lblTotalVirtual")).Content = status.TotalVirtual;
            ((Label)this.FindName("lblAvailVirtual")).Content = status.AvailVirtual;
        }
    }
}
