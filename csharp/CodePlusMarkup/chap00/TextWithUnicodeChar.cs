using System.Windows.Forms;

class TextWithUnicodeChar
{
	public static void Main()
	{
		MessageBox.Show("Hello, Microsoft\x00AE .NET Framework");
	}
}

