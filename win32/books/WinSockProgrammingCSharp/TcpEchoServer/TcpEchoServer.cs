using System;               // For Console, Int32, ArgumentException, Environment
using System.Net;           // For IPAddress
using System.Net.Sockets;   // For TcpListener, TcpClient

namespace TcpEchoServer
{
    class TcpEchoServer
    {
        private const int BUFSIZE = 32;

        static void Main(string[] args)
        {
            if (args.Length > 1)
            {
                throw new ArgumentException("Parameters: [<Port>]");
            }

            int servPort = (args.Length == 1) ? Int32.Parse(args[0]) : 7;

            TcpListener listener = null;

            try
            {
                listener = new TcpListener(IPAddress.Any, servPort);
                listener.Start();
            }
            catch (SocketException e)
            {
                Console.WriteLine(e.ErrorCode + ": " + e.Message);
                Environment.Exit(e.ErrorCode);
            }

            byte[] rcvBuffer = new byte[BUFSIZE];
            int bytesRcvd;

            for (; ; )
            {
                TcpClient client = null;
                NetworkStream netStream = null;

                try
                {
                    client = listener.AcceptTcpClient();
                    netStream = client.GetStream();
                    Console.Write("Handling client - ");

                    // Receive until client closes connection, indicated by 0 return value
                    int totalBytesEchoed = 0;
                    while ((bytesRcvd = netStream.Read(rcvBuffer, 0, rcvBuffer.Length)) > 0)
                    {
                        netStream.Write(rcvBuffer, 0, bytesRcvd);
                        totalBytesEchoed += bytesRcvd;
                    }
                    Console.WriteLine("echoed {0} bytes.", totalBytesEchoed);

                    netStream.Close();
                    client.Close();
                }
                catch (Exception e)
                {
                    Console.WriteLine(e.Message);
                    netStream.Close();
                }
            }
        }
    }
}
