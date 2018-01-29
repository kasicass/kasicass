# -*- encoding: utf-8 -*-

from __future__ import print_function

import sys
import code
import signal
import pyuv
from cStringIO import StringIO

class TelnetConsole(code.InteractiveInterpreter):
	def __init__(self, conn):
		#super(TelnetConsole, self).__init__()
		code.InteractiveInterpreter.__init__(self)
		self.resetbuffer()
		self.conn = conn

	def resetbuffer(self):
		self.buf = []

	def push(self, line):
		self.buf.append(line)
		source = "\n".join(self.buf)
		more = self.runsource(source, "<console>")
		if not more:
			self.resetbuffer()
		return more

	def write(self, data):
		self.conn.write(data)

	def writeWelcome(self):
		cprt = 'Type "help", "copyright", "credits" or "license" for more information.'
		self.write("Python %s on %s\n%s\n(%s)\n" % (sys.version, sys.platform, cprt, self.__class__.__name__))
		self.writePrompt(0)

	def writePrompt(self, more):
		if more:
			prompt = '... '
		else:
			prompt = '>>> '
		self.write(prompt)

	def handleData(self, line):
		encoding = getattr(sys.stdin, "encoding", None)
		if encoding and not isinstance(line, unicode):
			line = line.decode(encoding)

		stdoutBuff = StringIO()
		old = sys.stdout
		sys.stdout = stdoutBuff
		more = self.push(line)
		self.write(stdoutBuff.getvalue())
		sys.stdout = old

		self.writePrompt(more)

class Connection(object):
	def __init__(self, server, handle):
		self.server = server

		self.clientHandle = handle
		self.clientHandle.start_read(self.onRead)

		self.console = TelnetConsole(self)
		self.console.writeWelcome()

	def onRead(self, handle, data, error):
		if data is None:
			print('client closed:', self.clientHandle.getpeername())
			self.clientHandle.close()
			self.server.removeConnection(self)
			return

		self.console.handleData(data)

	def write(self, data):
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
		self.endpoint    = endpoint
	
	def run(self):
		print('TelnetConsole runs on %s' % str(self.endpoint))
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

