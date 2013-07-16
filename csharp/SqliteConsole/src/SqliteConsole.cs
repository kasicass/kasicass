using System;

namespace KCode
{
	public class SqliteConsole
	{
		CommandFactory cmdFactory_ = new CommandFactory();

		public SqliteConsole()
		{
			cmdFactory_.RegisterCommand("CREATE DATABASE", new CommandCreateDatabase());
		}

		public string RunSQL(string sqlText)
		{
			ICommand cmd = cmdFactory_.FindCommand(sqlText);
			return cmd.Run(sqlText.Split(' '));
		}
	}
}
