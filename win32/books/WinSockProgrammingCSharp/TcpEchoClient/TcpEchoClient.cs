using System;                // For String, Int32, Console, ArgumentException
using System.Text;           // For Encoding
using System.IO;             // For IOException
using System.Net.Sockets;    // For TcpClient, NetworkStream, SocketException

namespace TcpEchoClient
{
    class TcpEchoClient
    {
        static void Main(string[] args)
        {
            if ((args.Length < 2) || (args.Length > 3))
            {
                throw new ArgumentException("Parameters: <Server> <Word> [<Port>]");
            }

            String server = args[0];

            // Convert input String to bytes
            byte[] byteBuffer = Encoding.ASCII.GetBytes(args[1]);

            // Use port argument if supplied, otherwise default to 7
            int servPort = (args.Length == 3) ? Int32.Parse(args[2]) : 7;

            TcpClient client = null;
            NetworkStream netStream = null;

            try
            {
                client = new TcpClient(server, servPort);

                Console.WriteLine("Connected to server... sending echo string");

                netStream = client.GetStream();

                // Send the encoded string to the server
                netStream.Write(byteBuffer, 0, byteBuffer.Length);

                Console.WriteLine("Sent {0} bytes to server...", byteBuffer.Length);

                int totalBytesRcvd = 0;
                int bytesRcvd = 0;

                while (totalBytesRcvd < byteBuffer.Length)
                {
                    if ((bytesRcvd = netStream.Read(byteBuffer, totalBytesRcvd, byteBuffer.Length - totalBytesRcvd)) == 0)
                    {
                        Console.WriteLine("Connection closed prematurely.");
                        break;
                    }
                    totalBytesRcvd += bytesRcvd;
                }

                Console.WriteLine("Received {0} bytes from server: {1}", totalBytesRcvd,
                    Encoding.ASCII.GetString(byteBuffer, 0, totalBytesRcvd));
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }
            finally
            {
                netStream.Close();
                client.Close();
            }
        }
    }
}
