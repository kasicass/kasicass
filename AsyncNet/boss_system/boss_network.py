from AsyncNet import AsyncCore
from AsyncNet import HEADER_WORDLSB
from AsyncNet import ASYNC_EVT_NEW, ASYNC_EVT_LEAVE, ASYNC_EVT_ESTAB, ASYNC_EVT_DATA
from AsyncNet import ASYNC_MODE_IN
import boss_logger

class BaseServer(object):
	def __init__(self):
		self.core = AsyncCore()
		self.listen_hids = []
		self.client_hids = []

	def add_endpoint(self, endpoint):
		ip, port = endpoint
		hid = self.core.new_listen(ip, port, HEADER_WORDLSB)
		self.core.option(hid, 'REUSEADDR', 1)
		self.listen_hids.append(hid)

	def dispatch_event(self, event, hid, data):
		if event == ASYNC_EVT_NEW:
			if self.core.get_mode(hid) == ASYNC_MODE_IN:
				self.client_hids.append(hid)
				_, port, ip = self.core.parse_remote(data)
				self.handle_client_new(hid, (ip, port))
		elif event == ASYNC_EVT_LEAVE:
			if hid in self.client_hids:
				self.client_hids.remove(hid)
				self.handle_client_leave(hid)
		elif event == ASYNC_EVT_DATA:
			if hid in self.client_hids:
				self.handle_client_data(hid, data)

	def run_internal(self):
		while True:
			self.core.wait(0.1)
			while True:
				event, hid, tag, data = self.core.read()
				if event == None:
					break

				self.dispatch_event(event, hid, data)

	def run(self):
		try:
			self.run_internal()
		except KeyboardInterrupt:
			pass

	def send(self, hid, data):
		self.core.send(hid, data)

	def handle_client_new(self, client_hid, client_addr):
		raise ValueError('please implement this method')

	def handle_client_data(self, child_hid, data):
		raise ValueError('please implement this method')

	def handle_client_leave(self, child_hid):
		raise ValueError('please implement this method')

class BaseClient(object):
	def __init__(self):
		self.core = AsyncCore()
		self.client_hid = None

	def new_connect(self, endpoint):
		if self.client_hid != None:
			return

		ip, port = endpoint
		self.client_hid = self.core.new_connect(ip, port, HEADER_WORDLSB)

	def dispatch_event(self, event, hid, data):
		if event == ASYNC_EVT_DATA:
			self.handle_data(data)
		elif event == ASYNC_EVT_ESTAB:
			self.handle_connected()
		elif event == ASYNC_EVT_LEAVE:
			self.handle_disconnected()
			self.client_hid = None

	def run(self, wait_time):
		self.core.wait(wait_time)
		while True:
			event, hid, tag, data = self.core.read()
			if event == None:
				break

			self.dispatch_event(event, hid, data)

	def hid(self):
		return self.client_hid

	def send(self, data):
		self.core.send(self.client_hid, data)

	def handle_connected(self):
		raise ValueError('please implement this method')

	def handle_disconnected(self):
		raise ValueError('please implement this method')

	def handle_data(self, data):
		raise ValueError('please implement this method')

if __name__ == '__main__':
	class MyServer(BaseServer):
		def __init__(self):
			super(MyServer, self).__init__()
			self.logger = boss_logger.getLogger('MyServer')

		def handle_client_new(self, client_hid, client_addr):
			self.logger.info('new conn: %d, %s', client_hid, str(client_addr))

		def handle_client_data(self, client_hid, data):
			self.logger.info('echo: %d, %s', client_hid, data)
			self.send(client_hid, data)

		def handle_client_leave(self, client_hid):
			self.logger.info('conn leave: %d', client_hid)

	class MyClient(BaseClient):
		def __init__(self):
			super(MyClient, self).__init__()
			self.logger = boss_logger.getLogger('MyClient')

		def handle_connected(self):
			self.logger.info('connected %d', self.hid())
			self.send('hello')

		def handle_disconnected(self):
			self.logger.info('disconnected %d', self.hid())

		def handle_data(self, data):
			self.logger.info('recv %s', data)

	import sys
	import time

	if len(sys.argv) != 2:
		print 'usage: python %s <server/client>' % sys.argv[0]
		sys.exit(0)

	if sys.argv[1] == 'server':
		server = MyServer()
		server.add_endpoint(('127.0.0.1', 8886))
		server.logger.info('MyServer starts at %s:%d', '127.0.0.1', 8886)
		server.run()
	else:
		client = MyClient()
		client.new_connect(('127.0.0.1', 8886))

		count = 0
		index = 0
		while True:
			client.run(0.1)
			time.sleep(0.1)

			count += 1
			while count > 10:
				index += 1
				client.send('baby = %d' % index)
				count = 0

