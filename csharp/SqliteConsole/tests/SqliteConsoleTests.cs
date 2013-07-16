using Xunit;

namespace KCode
{
	public class SqliteConsoleTests
	{
		[Fact]
		public void ConsoleInit()
		{
			SqliteConsole console = new SqliteConsole();
			console.RunSQL("CREATE DATABASE mytest.db3");
			console.RunSQL("CREATE DATABASE mytest.db3");
		}
	}
}
