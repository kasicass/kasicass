using System;
using System.Windows;

namespace kcode.wpf {
	public partial class MyApp : Application {
		void AppStartup(object sender, StartupEventArgs e) {
			Window window = new HelloWPF();
			window.Show();
		}
	}
}
