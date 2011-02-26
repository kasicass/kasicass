
var connectToIP : String = "127.0.0.1";
var connectPort : int = 2224;

function OnGUI ()
{
	if (Network.peerType == NetworkPeerType.Disconnected)
	{
		GUILayout.Label("Connection status: Disconnected");
		
		connectToIP = GUILayout.TextField(connectToIP, GUILayout.MinWidth(100));
		connectPort = parseInt(GUILayout.TextField(connectPort.ToString()));
		
		if (GUILayout.Button("Start Server"))
		{
			Network.InitializeServer(32, connectPort, false);
		}
	}
	else
	{
		if (Network.peerType == NetworkPeerType.Server)
		{
			GUILayout.Label("Connection status: Server!");
			GUILayout.Label("Connection: " + Network.connections.length);
			if (Network.connections.length >= 1)
			{
				GUILayout.Label("Ping to first player: " + Network.GetAveragePing(Network.connections[0]));
			}
		}
		
		if (GUILayout.Button("Disconnect"))
		{
			Network.Disconnect(200);
		}
	}
}

// Server functions called by Unity
function OnPlayerConnected(player: NetworkPlayer) {
	Debug.Log("Player connected from: " + player.ipAddress + ":" + player.port);
}

function OnServerInitialized() {
	Debug.Log("Server initialized and ready");
}

function OnPlayerDisconnected(player: NetworkPlayer) {
	Debug.Log("Player disconnected from: " + player.ipAddress + ":" + player.port);
}

