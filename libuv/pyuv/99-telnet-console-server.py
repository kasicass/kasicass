# -*- encoding: utf-8 -*-

from __future__ import print_function

import signal
import pyuv

class Connection(object):
	def __init__(self, server, handle):
		self.server = server
		self.clientHandle = handle
		self.clientHandle.start_read(self.onRead)

	def onRead(self, handle, data, error):
		if data is None:
			print('client closed:', self.clientHandle.getpeername())
			self.clientHandle.close()
			self.server.removeConnection(self)
			return

		self.clientHandle.write(data)

	def close(self):
		self.clientHandle.close()

class Acceptor(object):
	def __init__(self, server, endpoint):
		self.server = server

		loop = pyuv.Loop.default_loop()
		self.listenHandle = pyuv.TCP(loop)
		self.listenHandle.bind(endpoint)
		self.listenHandle.listen(self.onConnection)

	def close(self):
		self.listenHandle.close()

	def onConnection(self, handle, error):
		loop = pyuv.Loop.default_loop()
		clientHandle = pyuv.TCP(loop)
		self.listenHandle.accept(clientHandle)

		conn = Connection(self.server, clientHandle)
		self.server.addConnection(conn)

		print('new client from:', clientHandle.getpeername())

class TelnetConsoleServer(object):
	def __init__(self, endpoint):
		loop = pyuv.Loop.default_loop()
		self.acceptor = Acceptor(self, endpoint)

		self.signalHandle = pyuv.Signal(loop)
		self.signalHandle.start(self.onSignal, signal.SIGINT)

		self.connections = []
	
	def run(self):
		loop = pyuv.Loop.default_loop()
		loop.run()

	def onSignal(self, handle, signum):
		[conn.close() for conn in self.connections]
		self.acceptor.close()
		self.signalHandle.close()
		print('bye!')

	def addConnection(self, conn):
		self.connections.append(conn)

	def removeConnection(self, conn):
		self.connections.remove(conn)

if __name__ == '__main__':
	server = TelnetConsoleServer(('127.0.0.1', 8888))
	server.run()

