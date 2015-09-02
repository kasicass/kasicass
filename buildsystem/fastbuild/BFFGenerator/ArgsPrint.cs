using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;
using System.IO;

namespace ArgsPrint
{
    class Program
    {
        [DllImport("kernel32.dll", CharSet = CharSet.Auto)]
        public static extern void OutputDebugString(string message);

        static void Main(string[] args)
        {
            string toPrint = "[clhooker] " + System.AppDomain.CurrentDomain.FriendlyName;
            for (int i = 0; i < args.Length; ++i)
            {
                string s = args[i];
                if (s[0] == '@')
                {
                    s = s.Substring(1);
                    StreamReader sr = new StreamReader(s);
                    string content = sr.ReadToEnd();
                    content = content.Replace("\r\n", " ");
                    //content.Replace("\n", " ");
                    toPrint += (" " + content);
                    sr.Close();
                }
                else
                {
                    toPrint += (" " + s);
                }
            }

            OutputDebugString(toPrint);




            //Console.WriteLine("[clhooker] " + string.Join(" ", args));
        }
    }
}
