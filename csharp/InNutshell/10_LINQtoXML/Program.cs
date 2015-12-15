using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml.Linq;

namespace _10_LINQtoXML
{
    class Program
    {
        static void PrintQueryResult(IEnumerable<string> query)
        {
            Console.Write("[ ");
            foreach (string value in query)
            {
                Console.Write("\"{0}\", ", value);
            }
            Console.WriteLine("]");
        }

        static void ChildNodeNavigation()
        {
            var bench = new XElement("bench",
                            new XElement("toolbox",
                                new XElement("handtool", "Hammer"),
                                new XElement("handtool", "Rasp")
                            ),
                            new XElement("toolbox",
                                new XElement("handtool", "Saw"),
                                new XElement("powertool", "Nailgun")
                            ),
                            new XComment("Be careful with the nailgun")
                        );

            foreach (XNode node in bench.Nodes())
            {
                Console.WriteLine(node.ToString(SaveOptions.DisableFormatting) + ".");
            }

            // Retrieving elements
            foreach (XElement e in bench.Elements())
            {
                Console.WriteLine(e.Name + "=" + e.Value);
            }

            foreach (var tool in bench.Elements("toolbox").Elements("handtool"))
            {
                Console.WriteLine("tool: {0} = {1}", tool.Name, tool.Value);
            }

            // query
            IEnumerable<string> query =
                from toolbox in bench.Elements()
                where toolbox.Elements().Any(tool => tool.Value == "Nailgun")
                select toolbox.Value;
            PrintQueryResult(query);

            query =
                from toolbox in bench.Elements()
                from tool in toolbox.Elements()
                where tool.Name == "handtool"
                select tool.Value;
            PrintQueryResult(query);

            // counts
            int x = bench.Elements("toolbox").Count();
            int y = bench.Elements().Where(e => e.Name == "toolbox").Count();
            Console.WriteLine("count1: {0}, count2: {1}", x, y);

            // Retrieving descendants
            Console.WriteLine(bench.Descendants("handtool").Count());

            foreach (XNode node in bench.DescendantNodes())
            {
                Console.WriteLine(node.ToString(SaveOptions.DisableFormatting));
            }
        }

        static void Main(string[] args)
        {
            ChildNodeNavigation();
        }
    }
}
