
var connectToIP : String = "127.0.0.1";
var connectPort : int = 2224;

function OnGUI () {
	if (Network.peerType == NetworkPeerType.Disconnected)
	{
		GUILayout.Label("Connection status: Disconnected");
		
		connectToIP = GUILayout.TextField(connectToIP, GUILayout.MinWidth(100));
		connectPort = parseInt(GUILayout.TextField(connectPort.ToString()));
		
		if (GUILayout.Button("Connect as client"))
		{
			Network.Connect(connectToIP, connectPort);
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
			GUILayout.Label("Connection status: Client!");
			GUILayout.Label("Ping to server: " + Network.GetAveragePing(Network.connections[0]));
		}
		
		if (GUILayout.Button("Disconnect"))
		{
			Network.Disconnect(200);
		}
	}
}

// Client functions called by Unity
function OnConnectedToServer()
{
	Debug.Log("This CLIENT has connected to a server");	
}

function OnDisconnectedFromServer(info : NetworkDisconnection)
{
	Debug.Log("This SERVER OR CLIENT has disconnected from a server");
}

function OnFailedToConnect(error: NetworkConnectionError)
{
	Debug.Log("Could not connect to server: " + error);
}