import time
import msgpack
from AsyncNet import AsyncCore
from AsyncNet import HEADER_WORDLSB
from AsyncNet import ASYNC_EVT_NEW, ASYNC_EVT_LEAVE, ASYNC_EVT_ESTAB, ASYNC_EVT_DATA
from AsyncNet import ASYNC_MODE_IN

class RpcService(object):
	def __init__(self, name):
		self.name = name

class MyService1(RpcService):
	def __init__(self, name):
		super(MyService1, self).__init__(name)
		self.count = 0

	def incCount(self, n):
		print 'incCount', self.count, n
		self.count += n

class MyService2(RpcService):
	def __init__(self, name):
		super(MyService2, self).__init__(name)

	def sayHello(self):
		print 'Hello'

class ServiceManager(object):
	def __init__(self, ip, port):
		self.serviceMap = {}
		self.core = AsyncCore()
		self.listenHid = self.core.new_listen(ip, port, HEADER_WORDLSB)
		print 'listen on localhost:8888 hid=%xh' % self.listenHid

	def addService(self, svc):
		self.serviceMap[svc.name] = svc

	def callService(self, name, fn, args):
		svc = self.serviceMap.get(name, None)
		if not svc:
			return

		fn = getattr(svc, fn, None)
		if not fn:
			return

		# print time.strftime('[%Y-%m-%d %H:%M:%S]'), 'call method %s.%s(%s)'%(svc.__class__.__name__,fn,str(args))
		if args:
			fn(*args)
		else:
			fn()

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
					name, fn, args = msgpack.unpackb(data, use_list=False)
					self.callService(name, fn, args)

def main():
	mgr = ServiceManager('127.0.0.1', 8888)
	mgr.addService(MyService1('counter'))
	mgr.addService(MyService2('hello'))
	mgr.run()

if __name__ == '__main__':
	main()

