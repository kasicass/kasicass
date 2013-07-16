using System;
using System.IO;
using Mono.Data.Sqlite;

namespace KCode
{
	internal class CommandCreateDatabase : ICommand
	{
		public string Run(string[] args)
		{
			string dbName = args[2]+".db3";
			if (File.Exists(dbName))
			{
				return "Already have one!";
			}
			else
			{
				SqliteConnection.CreateFile(dbName);
				return "Create ok!";
			}
		}
	}
}
