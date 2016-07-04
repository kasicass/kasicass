using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.ComponentModel.Composition;
using System.Resources;
using System.Globalization;

using Illusion;
using System.Windows.Media;

namespace YoZuo.MonitorLayout
{
	[Export(typeof(IResource))]
	public class Resource : IResource
	{
		private ResourceManager stringResource;
		private CultureInfo _culture = new CultureInfo("en-us");

		public Resource()
		{
			stringResource = new ResourceManager("YoZuo.MonitorLayout.Resources.StringResource", typeof(Resource).Assembly);
		}

		#region IResource Members

		public CultureInfo CurrentCulture 
		{ 
			set
			{
				_culture = value;
			}
		}

		public string GetString(string name)
		{
			return stringResource.GetString(name, _culture);
		}

		public ImageSource GetImage(string name)
		{
			return null;
		}

		#endregion
	}
}
