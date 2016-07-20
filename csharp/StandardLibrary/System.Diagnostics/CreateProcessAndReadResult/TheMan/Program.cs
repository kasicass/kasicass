using System;
using System.Text;
using System.Diagnostics;

namespace TheMan
{
    class Program
    {
        static void Main(string[] args)
        {
            Process my = new Process();

            try
            {
                my.StartInfo.UseShellExecute = false;
                my.StartInfo.FileName = "RunRunRun.exe";
                my.StartInfo.CreateNoWindow = true;
                my.StartInfo.RedirectStandardInput = true;
                my.StartInfo.RedirectStandardOutput = true;
                my.Start();

                my.StandardInput.WriteLine("Hello Man");

                char[] chars = new char[128];
                int n = my.StandardOutput.Read(chars, 0, 128);
                String s = new String(chars);
                s = s.Trim();
                System.Console.WriteLine("Output: " + s);
            }
            catch (Exception e)
            {
                Console.WriteLine("Err: " + e.Message);
            }
        }
    }
}
