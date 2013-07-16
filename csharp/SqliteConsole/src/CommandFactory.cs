using System;
using System.Linq;
using System.Collections.Generic;

namespace KCode
{
	internal class CommandFactory
	{
		Dictionary<string, ICommand> cmdDict_ = new Dictionary<string, ICommand>();

		public void RegisterCommand(string cmdText, ICommand cmd)
		{
			cmdDict_[cmdText] = cmd;
		}
		
		public ICommand FindCommand(string sqlText)
		{
			var textList  = sqlText.Split(' ');
			// var textList = from s in tmpList where s.Length > 0 select s;
			int i = 0;
			string cmdText = textList[0];
			while (true)
			{
				if (cmdDict_.ContainsKey(cmdText))
					return cmdDict_[cmdText];

				i++;
				if (i == textList.Length)
					break;

				cmdText = cmdText + " " + textList[i];
			}

			return null;
		}
	}
}
