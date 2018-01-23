# -*- encoding: utf-8 -*-
# setDaemon(True) 的线程，如果没有显式 join()，则不会影响 python vm 的退出

import threading
import time
import logging

logging.basicConfig(
	level=logging.DEBUG,
	format='(%(threadName)-10s) %(message)s'
	)

def daemon():
	logging.debug('Starting')
	time.sleep(2)
	logging.debug('Exiting')

d = threading.Thread(name='daemon', target=daemon)
d.setDaemon(True)

def non_daemon():
	logging.debug('Starting')
	logging.debug('Exiting')

t = threading.Thread(name='non-daemon', target=non_daemon)

d.start()
t.start()

d.join(1)
print 'd.isAlive()', d.isAlive()
t.join()

