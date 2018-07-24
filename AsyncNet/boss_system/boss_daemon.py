import msgpack
import boss_config
import boss_logger
import boss_network

class HelloStub(object):
	def __init__(self):
		self.logger = boss_logger.getLogger('HelloStub')

	def sayHello(self):
		self.logger.info('Hello')

class BossServer(boss_network.BaseServer):
	def __init__(self):
		super(BossServer, self).__init__()
		self.logger = boss_logger.getLogger('BossServer')
		self.stubs  = {}

	def add_stub(self, stub):
		self.stubs[stub.__class__.__name__] = stub

	def handle_client_new(self, client_hid, client_addr):
		self.logger.info('new conn: %d, %s', client_hid, str(client_addr))

	def handle_client_data(self, client_hid, data):
		data = msgpack.unpackb(data)
		self.logger.info('data: %d, %s', client_hid, str(data))
		stub_name, func_name, args = data
		stub = self.stubs.get(stub_name, None)
		if stub and getattr(stub, func_name, None):
			getattr(stub, func_name)(*args)

	def handle_client_leave(self, client_hid):
		self.logger.info('conn leave: %d', client_hid)

if __name__ == '__main__':
	server = BossServer()
	server.add_stub(HelloStub())

	server.add_endpoint(boss_config.BOSS_ENDPOINT)
	server.logger.info('BossServer starts: %s', str(boss_config.BOSS_ENDPOINT))
	server.run()

