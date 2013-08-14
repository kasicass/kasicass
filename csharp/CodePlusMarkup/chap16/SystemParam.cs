namespace Petzold.ListSystemParameters
{
	public class SystemParam
	{
		string strName;
		object objValue;

		public string Name
		{
			set { strName = value; }
			get { return strName; }
		}

		public object Value
		{
			set { objValue = value; }
			get { return objValue; }
		}

		public override string ToString()
		{
			return Name + "=" + Value;
		}
	}
}
