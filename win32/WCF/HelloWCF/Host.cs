using System;
using System.ServiceModel;

public class MyHost
{
	static public void Main(string[] args)
	{
		using (ServiceHost host = new ServiceHost(typeof(MyService.HelloWCFService), new Uri("http://localhost:8007/HelloWCF")))
		{
			host.AddServiceEndpoint(typeof(MyService.IHelloWCFService), new BasicHttpBinding(), "HelloWCFService");
			host.Open();
			
			Console.WriteLine("Press <ENTER> to terminate the service host");
			Console.ReadLine();
		}
	}
}