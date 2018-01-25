# -*- encoding: utf-8 -*-

import logging
import traceback

logging.basicConfig(
	level=logging.DEBUG
)

class MyLoggerAdapter(object):
	def __init__(self, logger):
		self.logger = logger

	def debug(self, msg, stack_info=None, *args, **kwargs):
		self.logger.debug(msg, *args, **kwargs)
		if stack_info:
			self.logger.debug(''.join(traceback.format_stack()))

	def debug1(self, msg, *args, **kwargs):
		if kwargs.has_key('stack_info'):
			stack_info = kwargs.pop('stack_info')
		else:
			stack_info = 0

		self.logger.debug(msg, *args, **kwargs)

		if stack_info:
			self.logger.debug(''.join(traceback.format_stack()))

logger = logging.getLogger('phay')
logger = MyLoggerAdapter(logger)

def foo():
	logger.debug('Woo!', stack_info=1)
	#for line in traceback.format_stack():
	#	logging.debug(line)
	#logging.debug(''.join(traceback.format_stack()))

def bar():
	foo()

bar()

