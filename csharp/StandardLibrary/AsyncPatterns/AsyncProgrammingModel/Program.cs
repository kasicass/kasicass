using System;
using System.IO;
using System.Text;
using System.Threading;

namespace AsyncProgrammingModel
{
    class Program
    {
        static void Main(string[] args)
        {
            byte[] buf = new byte[128];

            FileStream file = new FileStream(@"D:\hello.py", FileMode.Open);
            IAsyncResult ar = file.BeginRead(buf, 0, 128, null, null);
            Thread.Sleep(1000); // do other things
            int n = file.EndRead(ar);
            file.Close();

            string s = Encoding.UTF8.GetString(buf, 0, n);
            Console.WriteLine("Readed: {0}, {1}", n, s);
        }
    }
}
