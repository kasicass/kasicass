using System;

class HelloWorld
{
	static void Main()
	{
		#if DebugConfig
		Console.WriteLine("In Debug");
		#endif

		Console.WriteLine("Hello World!");
	}
}