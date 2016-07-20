using System;
using System.IO;
using System.Text;

namespace RunRunRun
{
    class Program
    {
        static void Main(string[] args)
        {
            Stream stdin = System.Console.OpenStandardInput();
            byte[] buf = new byte[128];
            try
            {
                int n = stdin.Read(buf, 0, 128);
                char[] chars = Encoding.UTF7.GetChars(buf, 0, n);
                string s = new String(chars);
                s = s.Trim();
                System.Console.Write(s);
            }
            catch (Exception e)
            {
                System.Console.WriteLine("Err: " + e.Message);
            }
        }
    }
}
