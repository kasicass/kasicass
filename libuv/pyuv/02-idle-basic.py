# Idle handles will run the given callback once per loop iteration,
# right before the Prepare handles.
#
# Warning Despite the name, Idle handles will get their callbacks
# called on every loop iteration, not when the loop is actually "idle".

from pyuv import Loop
from pyuv import Idle

counter = 0
def idle_callback(handle):
	global counter
	counter += 1
	if counter >= 100000:
		handle.stop()

loop = Loop.default_loop()

myidle = Idle(loop)
myidle.start(idle_callback)

print 'Idling...'
loop.run()

print 'counter =', counter

