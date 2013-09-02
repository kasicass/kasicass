using System;
using System.IO;
using System.IO.Pipes;

namespace AnonymousPipeLocalIPC
{
	public class PipeClient
	{
		public static void Main(string[] args)
		{
			if (args.Length > 0)
			{
				using (var pipeClient = new AnonymousPipeClientStream(PipeDirection.In, args[0]))
				{
					try
					{
						Console.WriteLine("[CLIENT] Setting ReadMode to \"Message\".");
						pipeClient.ReadMode = PipeTransmissionMode.Message;
					}
					catch (NotSupportedException e)
					{
						Console.WriteLine("[CLIENT] Exception:\n  {0}", e.Message);
					}

					Console.WriteLine("[CLIENT Current TransmissionMode: {0}.", pipeClient.TransmissionMode);

					using (var sr = new StreamReader(pipeClient))
					{
						string temp;

						do
						{
							Console.WriteLine("[CLIENT] Wait for sync...");
							temp = sr.ReadLine();
						}
						while (!temp.StartsWith("SYNC"));

						while ((temp = sr.ReadLine()) != null)
						{
							Console.WriteLine("[CLIENT] Echo: " + temp);
						}
					}
				}
			}

			Console.Write("[CLIENT] Press Enter to continue...");
			Console.ReadLine();
		}
	}
}

