// usage:
//  IPAddressExample www.163.com
//  IPAddressExample 183.60.187.42   # www.sina.com.cn

using System;									// for String and Console
using System.Net;							// for Dns, IPHostEntry, IPAddress
using System.Net.Sockets;			// for SocketException

class IPAddressExample
{
	static void PrintHostInfo(String host)
	{
		try
		{
			IPHostEntry hostInfo;
			// hostInfo = Dns.Resolve(host);  // obsolete
			hostInfo = Dns.GetHostEntry(host);

			Console.WriteLine("\tCanonial Name: " + hostInfo.HostName);
			Console.Write("\tIP Addresses: ");
			foreach (IPAddress ipaddr in hostInfo.AddressList)
			{
				Console.Write(ipaddr.ToString() + " ");
			}
			Console.WriteLine();

			Console.Write("\tAliases: ");
			foreach (String alias in hostInfo.Aliases)
			{
				Console.Write(alias + " ");
			}
			Console.WriteLine("\n");
		}
		catch (Exception)
		{
			Console.WriteLine("\tUnable to resolve host: " + host + "\n");
		}
	}

	static void Main(string[] args)
	{
		try
		{
			Console.WriteLine("Local Host:");
			String localHostName = Dns.GetHostName();
			Console.WriteLine("\tHost Name: " + localHostName);

			PrintHostInfo(localHostName);
		}
		catch (Exception)
		{
			Console.WriteLine("Unable to resolve local host\n");
		}

		foreach (String arg in args)
		{
			Console.WriteLine(arg + ":");
			PrintHostInfo(arg);
		}
	}
}

