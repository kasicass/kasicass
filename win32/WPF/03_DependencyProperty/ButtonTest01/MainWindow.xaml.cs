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

namespace ButtonTest01
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private double _oldFontSize = 0;

        public MainWindow()
        {
            InitializeComponent();
            _oldFontSize = FontSize;
        }

        private void FontSizeWinBtn_Click(object sender, RoutedEventArgs e)
        {
            FontSize = 16;
        }

        private void FontSizeBtn_Click(object sender, RoutedEventArgs e)
        {
            this.FontSizeBtn.FontSize = 8;
        }

        private void ResetFontSizeBtn_Click(object sender, RoutedEventArgs e)
        {
            FontSize = _oldFontSize;
            this.FontSizeBtn.FontSize = _oldFontSize;
        }
    }
}
