namespace Illusion
{
	public interface IStatusBarService
	{
		void ShowMessage(string message);
		void ShowProgress(double currentValue, double allValue);
	}
}
