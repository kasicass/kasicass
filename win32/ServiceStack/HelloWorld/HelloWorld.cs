using System;
using ServiceStack;

class Program
{
	[Route("/hello/{Name}")]
	public class Hello {
		public string Name { get; set; }
	}
	
	public class HelloResponse {
		public string Result { get; set; }
	}
	
	public class HelloService : Service {
		public object Any(Hello request) {
			return new HelloResponse { Result = "Hello, " + request.Name };
		}
	}
	
	public class AppHost : AppSelfHostBase {
		public AppHost() : base("HttpListener Self-Host", typeof(HelloService).Assembly) {}
		
		public override void Configure(Funq.Container continaer) {}
	}
	
	static void Main(string[] args) {
		var listeningOn = args.Length == 0 ? "http://*:8007/" : args[0];
		var appHost = new AppHost().Init().Start(listeningOn);
		
		Console.WriteLine("AppHost Created at {0}, listening on {1}", DateTime.Now, listeningOn);
		Console.ReadKey();
	}
}