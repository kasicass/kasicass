import sys
from pyuv import Loop
from pyuv import fs

def onchange_callback(handle, filename, events, error):
	print filename,
	if events & fs.UV_RENAME:
		print 'renamed'
	if events & fs.UV_CHANGE:
		print 'changed'	

if __name__ == '__main__':
	if len(sys.argv) <= 1:
		print 'Usage: %s <file1> [file2 ...]'%sys.argv[0]
		sys.exit(0)

	loop = Loop.default_loop()
	for filename in sys.argv[1:]:
		handle = fs.FSEvent(loop)
		handle.start(filename, 0, onchange_callback)
	loop.run()

