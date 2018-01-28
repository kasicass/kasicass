import time
from pyuv import Loop
from pyuv import Timer

counter = 0
def timer_callback(handle):
	global counter
	counter += 1
	if counter < 10:
		print 'counter = %d, %s' % (counter, time.strftime('%H:%M:%S'))
	else:
		handle.stop()

loop = Loop.default_loop()

handle = Timer(loop)
handle.start(timer_callback, 3.5, 2) # timeout = 3.5s, repeat = 2s

print 'timer start,', time.strftime('%H:%M:%S')
loop.run()

