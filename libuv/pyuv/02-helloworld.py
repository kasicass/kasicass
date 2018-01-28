from pyuv import Loop
from pyuv import UV_RUN_DEFAULT, UV_RUN_ONCE, UV_RUN_NOWAIT

loop = Loop()

print "Now quitting."
loop.run(UV_RUN_DEFAULT)

