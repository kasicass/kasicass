using System;
using System.IO;
using System.IO.Pipes;
using System.Text;
using System.Security.Principal;
using System.Diagnostics;
using System.Threading;

namespace NamedPipeNetworkIPC
{
	public class PipeClient
	{
		private static int numClients = 4;

		public static void Main(string[] args)
		{
			if (args.Length > 0)
			{
				if (args[0] == "spawnclient")
				{
					var pipeClient = new NamedPipeClientStream(".", "testpipe", PipeDirection.InOut,
						PipeOptions.None, TokenImpersonationLevel.Impersonation);

					Console.WriteLine("Connecting to server...\n");
					pipeClient.Connect();

					StreamString ss = new StreamString(pipeClient);
					if (ss.ReadString() == "I am the one true server!")
					{
						ss.WriteString(Environment.CurrentDirectory + "\\testdata.txt");
						Console.Write(ss.ReadString());
					}
					else
					{
						Console.WriteLine("Server could not be verified.");
					}

					pipeClient.Close();
					Thread.Sleep(4000); // Give the client process some time to display results before exiting.
				}
			}
			else
			{
				Console.WriteLine("\n*** Named pipe client stream with impersonation example ***\n");
				StartClients();
			}
		}

		private static void StartClients()
		{
			int i;
			string currentProcessName = Environment.CommandLine;
			Process[] plist = new Process[numClients];

			Console.WriteLine("Spawning client processes...\n");

			if (currentProcessName.Contains(Environment.CurrentDirectory))
			{
				currentProcessName = currentProcessName.Replace(Environment.CurrentDirectory, String.Empty);
			}

			currentProcessName = currentProcessName.Replace("\\", String.Empty);
			currentProcessName = currentProcessName.Replace("\"", String.Empty);

			for (i = 0; i < numClients; i++)
			{
				plist[i] = Process.Start(currentProcessName, "spawnclient");
			}

			while (i > 0)
			{
				for (int j = 0; j < numClients; j++)
				{
					if (plist[j] != null)
					{
						if (plist[j].HasExited)
						{
							Console.WriteLine("Client process[{0}] has exited.", plist[j].Id);
							plist[j] = null;
							i--;
						}
						else
						{
							Thread.Sleep(250);
						}
					}
				}
			}
			Console.WriteLine("\nClient processes finished, exiting.");
		}
	}

	public class StreamString
	{
		private Stream ioStream;
		private UnicodeEncoding streamEncoding;

		public StreamString(Stream ioStream)
		{
			this.ioStream = ioStream;
			streamEncoding = new UnicodeEncoding();
		}

		public string ReadString()
		{
			int len = 0;

			len = ioStream.ReadByte() * 256;
			len += ioStream.ReadByte();
			byte[] inBuffer = new byte[len];
			ioStream.Read(inBuffer, 0, len);

			return streamEncoding.GetString(inBuffer);
		}

		public int WriteString(string outString)
		{
			byte[] outBuffer = streamEncoding.GetBytes(outString);
			int len = outBuffer.Length;
			if (len > UInt16.MaxValue)
			{
				len = (int)UInt16.MaxValue;
			}
			ioStream.WriteByte((byte)(len/256));
			ioStream.WriteByte((byte)(len&255));
			ioStream.Write(outBuffer, 0, len);
			ioStream.Flush();

			return outBuffer.Length + 2;
		}
	}
}

