using System;
using System.IO;
using System.Windows;
using System.Windows.Media;
using System.Xml.Serialization;

namespace Petzold.NotepadClone
{
	public class NotepadCloneSettings
	{
		public WindowState WindowState = WindowState.Normal;
		public Rect RestoreBounds = Rect.Empty;
		public TextWrapping TextWrapping = TextWrapping.NoWrap;
		public string FontFamily = "";
		public string FontStyle = new FontStyleConverter().ConvertToString(FontStyles.Normal);
		public string FontWeight = new FontWeightConverter().ConvertToString(FontWeights.Normal);
		public string FontStretch = new FontStretchConverter().ConvertToString(FontStretches.Normal);
		public double FontSize = 11;

		public virtual bool Save(string strAppData)
		{
			
		}
	}
}

