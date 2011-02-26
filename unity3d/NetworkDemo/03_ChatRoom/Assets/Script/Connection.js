var ServerAddr : String = "127.0.0.1";
var ServerPort : int = 2224;

function OnGUI()
{
	GUILayout.BeginArea(Rect(Screen.width-200,0,200,200));

	if (Network.peerType == NetworkPeerType.Disconnected)
	{
		GUILayout.Label("Connection status: Disconnected");

		ServerAddr = GUILayout.TextField(ServerAddr, GUILayout.MinWidth(100));
		ServerPort = parseInt(GUILayout.TextField(ServerPort.ToString()));

		if (GUILayout.Button("Connect as client"))
		{
			Network.Connect(ServerAddr, ServerPort);
		}

		if (GUILayout.Button("Start Server"))
		{
			Network.InitializeServer(32, ServerPort, false);
		}
	}
	else
	{
		if (Network.peerType == NetworkPeerType.Connecting)
		{
			GUILayout.Label("Connection status: Connecting");
		}
		else if (Network.peerType == NetworkPeerType.Client)
		{
			GUILayout.Label("Connection status: Connected");
		}
		else if (Network.peerType == NetworkPeerType.Server)
		{
			GUILayout.Label("Connection status: Server!");
			GUILayout.Label("Connection: " + Network.connections.length);
		}

		if (GUILayout.Button("Disconnect"))
		{
			Network.Disconnect(200);
		}
	}
	
	GUILayout.EndArea();
}