using System;
using NetMQ;
using NetMQ.Sockets;

namespace EchoClient
{
    class Program
    {
        static void Main(string[] args)
        {
            using (var client = new RequestSocket(">tcp://localhost:5555"))
            {
                client.SendFrame("I'm a Message.");
                string s = client.ReceiveFrameString();
                Console.WriteLine("Echo: {0}", s);
            }
        }
    }
}
