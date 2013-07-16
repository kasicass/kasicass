using System;
using KCode;

public static class Program
{
	public static void Main()
	{
		SqliteConsole console = new SqliteConsole();
		while (true)
		{
			Console.Write("> ");

			string line = Console.ReadLine();
			string result = console.RunSQL(line);
			Console.WriteLine(result);
		}
	}
}
