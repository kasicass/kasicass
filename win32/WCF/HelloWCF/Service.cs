namespace MyService
{
	public class HelloWCFService : IHelloWCFService
	{
		public string HelloWCF()
		{
			return "Hello WCF!";
		}
	}
}