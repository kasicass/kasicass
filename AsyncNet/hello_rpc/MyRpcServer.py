import time
import msgpack
from AsyncNet import AsyncCore
from AsyncNet import HEADER_WORDLSB
from AsyncNet import ASYNC_EVT_NEW, ASYNC_EVT_LEAVE, ASYNC_EVT_ESTAB, ASYNC_EVT_DATA
from AsyncNet import ASYNC_MODE_IN

class RpcService(object):
	def __init__(self, ip, port):
		self.core = AsyncCore()
		self.listenHid = self.core.new_listen(ip, port, HEADER_WORDLSB)

		print 'listen on localhost:8888 hid=%xh' % self.listenHid

	def run(self):
		core = self.core
		while True:
			core.wait(0.05)
			while True:
				event, hid, tag, data = core.read()
				if event == None:
					break

				if event == ASYNC_EVT_NEW:
					print time.strftime('[%Y-%m-%d %H:%M:%S]'), 'new hid=%xh'%hid
				elif event == ASYNC_EVT_LEAVE:
					print time.strftime('[%Y-%m-%d %H:%M:%S]'), 'leave hid=%xh'%hid
				elif event == ASYNC_EVT_DATA:
					print time.strftime('[%Y-%m-%d %H:%M:%S]'), 'recv hid=%xh'%hid
					fn, args = msgpack.unpackb(data, use_list=False)
					fn = getattr(self, fn, None)
					if fn:
						print time.strftime('[%Y-%m-%d %H:%M:%S]'), 'call method %s(%s)'%(fn,str(args))
						if args:
							fn(*args)
						else:
							fn()

class MyService(RpcService):
	def __init__(self, ip, port):
		super(MyService, self).__init__(ip, port)

	def sayHello(self):
		print 'Hello RPC'

def main():
	svc = MyService('127.0.0.1', 8888)
	svc.run()

if __name__ == '__main__':
	main()

