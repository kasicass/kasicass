var chatWords : String = "";
var inputWord : String = "input words here.";

function OnGUI ()
{
	if (Network.isClient)
	{
	    GUILayout.BeginArea(Rect(0,0,300,600));
	    GUILayout.TextArea(chatWords);
	    GUILayout.BeginHorizontal();
	    inputWord = GUILayout.TextArea(inputWord);
	    if (GUILayout.Button("Send"))
	    {
	    	networkView.RPC("SomeoneSay", RPCMode.All, inputWord);
	    	inputWord = "";
	    }
	    GUILayout.EndHorizontal();
	    
	  	if (GUILayout.Button("Disconnect"))
	    {
    	    Network.Disconnect(200);
    	}
	    GUILayout.EndArea();
	}
}

@RPC
function SomeoneSay(sayWord : String, info : NetworkMessageInfo)
{
	chatWords = chatWords + "\n" + info.sender + ":" + sayWord;
}