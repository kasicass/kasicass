using System;
using System.IO;
using System.IO.Pipes;
using System.Text;
using System.Threading;

namespace NamedPipeNetworkIPC
{
	public class PipeServer
	{
		private static int numThreads = 4;

		public static void Main()
		{
			int i;
			Thread[] servers = new Thread[numThreads];

			Console.WriteLine("\n*** Named pipe server stream with impresonation example ***\n");
			Console.WriteLine("Waiting for client connect...\n");
			for (i = 0; i < numThreads; i++)
			{
				servers[i] = new Thread(ServerThread);
				servers[i].Start();
			}
			Thread.Sleep(250);

			while (i > 0)
			{
				for (int j = 0; j < numThreads; j++)
				{
					if (servers[j] != null)
					{
						if (servers[j].Join(250))
						{
							Console.WriteLine("Server thread[{0}] finished.", servers[j].ManagedThreadId);
							servers[j] = null;
							i--;
						}
					}
				}
			}
			Console.WriteLine("\nServer threads exhausted, exiting.");
		}

		private static void ServerThread(object data)
		{
			var pipeServer = new NamedPipeServerStream("testpipe", PipeDirection.InOut, numThreads);
			int threadId = Thread.CurrentThread.ManagedThreadId;
	
			pipeServer.WaitForConnection();
	
			Console.WriteLine("Client connected on thread[{0}].", threadId);
			try
			{
				StreamString ss = new StreamString(pipeServer);
				ss.WriteString("I am the one true server!");
				string filename = ss.ReadString();
				ReadFileToStream fileReader = new ReadFileToStream(ss, filename);
				Console.WriteLine("Reading file: {0} on thread[{1}] as user: {2}.",
					filename, threadId, pipeServer.GetImpersonationUserName());
				pipeServer.RunAsClient(fileReader.Start);
			}
			catch (IOException e)
			{
				Console.WriteLine("ERROR: {0}", e.Message);
			}
			pipeServer.Close();
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

	public class ReadFileToStream
	{
		private string fn;
		private StreamString ss;

		public ReadFileToStream(StreamString str, string filename)
		{
			fn = filename;
			ss = str;
		}

		public void Start()
		{
			string contents = File.ReadAllText(fn);
			ss.WriteString(contents);
		}
	}
}

