using System;
using System.Windows;
using System.Windows.Controls;

namespace kcode.wpf {
	public partial class HelloWPF : Window {
		public HelloWPF() {
			InitializeComponent();
		}

		void button_Click(object sender, RoutedEventArgs e) {
			MessageBox.Show("HelloWPF");
		}
	}
}
