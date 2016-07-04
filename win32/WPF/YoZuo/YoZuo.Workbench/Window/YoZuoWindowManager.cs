using System;
using Illusion;

namespace YoZuo.Workbench
{
	public interface IYoZuoWindowManager : IWindowManager
	{
		void ShowContent(object dataModel, object context = null);
	}

	public class YoZuoWindowManager : WindowManager, IYoZuoWindowManager
	{
		#region IYoZuoWindowManager Members

		public void ShowContent(object dataModel, object context = null)
		{
			throw new NotImplementedException();
		}

		#endregion
	}
}
