using System;
using System.IO;
using System.IO.Pipes;
using System.Diagnostics;

namespace AnonymousPipeLocalIPC
{
	public class PipeServer
	{
		public static void Main()
		{
			Process pipeClient = new Process();
			pipeClient.StartInfo.FileName = "PipeClient.exe";

			using (var pipeServer = new AnonymousPipeServerStream(PipeDirection.Out,
				HandleInheritability.Inheritable))
			{
				try
				{
					Console.Write("[SERVER] Setting ReadMode to \"Message\".");
					pipeServer.ReadMode = PipeTransmissionMode.Message;
				}
				catch (NotSupportedException e)
				{
					Console.WriteLine("[SERVER] Exception:\n  {0}", e.Message);
				}

				Console.WriteLine("[SERVER] Current TransmissionMode: {0}.", pipeServer.TransmissionMode);

				pipeClient.StartInfo.Arguments = pipeServer.GetClientHandleAsString();
				pipeClient.StartInfo.UseShellExecute = false;
				pipeClient.Start();

				pipeServer.DisposeLocalCopyOfClientHandle();
				try
				{
					using (var sw = new StreamWriter(pipeServer))
					{
						sw.AutoFlush = true;
						sw.WriteLine("SYNC"); // Send a 'sync message' and wait for client to receive it.
						pipeServer.WaitForPipeDrain();
						Console.Write("[SERVER] Enter text: ");
						sw.WriteLine(Console.ReadLine());
					}
				}
				catch (IOException e) // if the pipe is broken or disconnected.
				{
					Console.WriteLine("[SERVER] Error: {0}", e.Message);
				}
			}

			pipeClient.WaitForExit();
			pipeClient.Close();
			Console.WriteLine("[SERVER] Client quit. Server terminating.");
		}
	}
}

