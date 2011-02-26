
function Update ()
{
    if (Network.isServer)
    {
        var moveDirection : Vector3 = new Vector3(0, 0, -1);
        var speed : float = 10;
        transform.Translate(speed * moveDirection * Time.deltaTime);
    }
}