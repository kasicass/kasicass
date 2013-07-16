using System;

namespace KCode
{
	interface ICommand
	{
		string Run(string[] args);
	}
}
