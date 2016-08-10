using System;
using System.IO;
using System.Reflection;

namespace BaddyNet
{
    internal struct MyType
    {
        public int v;
        public float f;
        public string s;
    }

    public class PacketBaseTest
    {
        public static void Main(string[] args)
        {
            /*
            MyType my = new MyType() { v = 10, f = 2.1f, s = "Hello" };
            FieldInfo[] fields = my.GetType().GetFields();
            foreach (var fi in fields)
            {
                var v = fi.GetValue(my);
                Console.WriteLine(v);
            }
            */

            PacketA a = new PacketA { v = 10, f = 2.1f, s = "Hello" };
            MemoryStream ms = new MemoryStream(128);
            a.marshal(ms);

            ms.Position = 0;
            PacketA b = new PacketA();
            b.unmarshal(ms);

            Console.WriteLine(b.v);
            Console.WriteLine(b.f);
            Console.WriteLine(b.s);
        }
    }
}