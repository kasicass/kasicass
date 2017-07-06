using System;
using NetMQ;
using NetMQ.Sockets;

namespace EchoServer
{
    class Program
    {
        static void Main(string[] args)
        {
            using (var server = new ResponseSocket("@tcp://localhost:5555"))
            {
                while (true)
                {
                    string s = server.ReceiveFrameString();
                    Console.WriteLine("Recv: {0}", s);
                    server.SendFrame(s);
                }
            }
        }
    }
}
