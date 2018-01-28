import os
import sys
from pyuv import Loop
from pyuv import fs
from pyuv import errno

class UvCat(object):
	BYTES_READ_ONCE = 8           # 8 bytes, test multi-read

	def __init__(self, loop):
		self.loop = loop
		self.fd = None
		self.readOffset = 0

	def start(self, filename):
		self.fd = None
		self.readOffset = 0

		# args = (loop, path, flags, mode, callback)
		fs.open(self.loop, filename, os.O_RDONLY, 0, self.onFileOpen)

	def onFileOpen(self, response):
		error = response.error
		if error:
			print 'open fail:', errno.strerror(error), ',fd =', fd
			return

		fd = response.result
		self.fd = fd

		# fs.read(loop, fd, length, offset[, callback])
		fs.read(loop, fd, self.BYTES_READ_ONCE, self.readOffset, self.onFileRead)

	def onFileRead(self, response):
		error = response.error
		if error:
			print 'open fail:', errno.strerror(error)
			return

		result = response.result
		if len(result) == 0:
			fs.close(self.loop, self.fd)
			return

		# sys.stdout.write(result)
		# fs.write(loop, fd, write_data, offset[, callback])
		fs.write(self.loop, 1, result, -1, self.onStdoutWrite)

	def onStdoutWrite(self, response):
		self.readOffset += self.BYTES_READ_ONCE
		fs.read(self.loop, self.fd, self.BYTES_READ_ONCE, self.readOffset, self.onFileRead)
		

if __name__ == '__main__':
	loop = Loop.default_loop()
	uvcat = UvCat(loop)
	uvcat.start(sys.argv[1])
	loop.run()

