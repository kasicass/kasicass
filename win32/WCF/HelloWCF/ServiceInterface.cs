using System.ServiceModel;

namespace MyService
{
	[ServiceContract(Namespace="http://kasicass.me/hellowcf")]
	public interface IHelloWCFService
	{
		[OperationContract]
		string HelloWCF();
	}
}
