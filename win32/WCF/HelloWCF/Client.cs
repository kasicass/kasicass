using System;
using System.ServiceModel;
using MyService;

public class MyClient
{
	static public void Main(string[] args)
	{
		EndpointAddress ep = new EndpointAddress("http://localhost:8007/HelloWCF/HelloWCFService");
		
		IHelloWCFService proxy = ChannelFactory<IHelloWCFService>.CreateChannel(new BasicHttpBinding(), ep);
		string s = proxy.HelloWCF();
		Console.WriteLine("output: " + s);
	}
}