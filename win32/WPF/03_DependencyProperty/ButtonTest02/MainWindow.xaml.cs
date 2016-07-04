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

namespace ButtonTest02
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public static readonly DependencyProperty SpaceProperty;
        public int Space
        {
            set { SetValue(SpaceProperty, value); }
            get { return (int)GetValue(SpaceProperty); }
        }

        static MainWindow()
        {
            FrameworkPropertyMetadata metadata = new FrameworkPropertyMetadata();
            metadata.Inherits = true;
            SpaceProperty = SpaceButton.SpaceProperty.AddOwner(typeof(MainWindow));
            SpaceProperty.OverrideMetadata(typeof(Window), metadata);
        }

        public MainWindow()
        {
            InitializeComponent();
        }

        private void BtnSpace_Click(object sender, RoutedEventArgs e)
        {
            this.btnSpace.Space = 2;
        }

        private void WinSpace_Click(object sender, RoutedEventArgs e)
        {
            this.Space = 2;
        }
    }
}
