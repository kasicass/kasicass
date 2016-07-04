using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;

using Illusion;

namespace YoZuo.Workbench.ViewModels
{
	[Export(typeof(IStatusBarService))]
	public class StatusBarViewModel : ViewAware, IStatusBarService
	{
		#region IStatusBarService Members

		public void ShowMessage(string message)
		{
			Message = message;
		}

		public void ShowProgress(double currentValue, double allValue)
		{
			Value = currentValue;
			Maximum = allValue;
		}

		#endregion

		private string message;
		public string Message
		{
			get { return message; }
			set { message = value; NotifyOfPropertyChange(() => Message); }
		}

		private double value;
		public double Value
		{
			get { return value; }
			set { this.value = value; NotifyOfPropertyChange(() => Value); }
		}

		private double maximum;
		public double Maximum
		{
			get { return maximum; }
			set { maximum = value; NotifyOfPropertyChange(() => Maximum); }
		}

	}
}
