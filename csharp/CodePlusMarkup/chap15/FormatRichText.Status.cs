using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Threading;

namespace Petzold.FormatRichText
{
	public partial class FormatRichText : Window
	{
		StatusBarItem itemDateTime;

		void AddStatusBar(DockPanel dock)
		{
			StatusBar status = new StatusBar();
			dock.Children.Add(status);
			DockPanel.SetDock(status, Dock.Bottom);

			itemDateTime = new StatusBarItem();
			itemDateTime.HorizontalAlignment = HorizontalAlignment.Right;
			status.Items.Add(itemDateTime);

			DispatcherTimer tmr = new DispatcherTimer();
			tmr.Interval = TimeSpan.FromSeconds(1);
			tmr.Tick += TimerOnTick;
			tmr.Start();
		}

		void TimerOnTick(object sender, EventArgs args)
		{
			DateTime dt = DateTime.Now;
			itemDateTime.Content = dt.ToLongDateString() + " " + dt.ToLongTimeString();
		}
	}
}

