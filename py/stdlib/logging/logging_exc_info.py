# -*- encoding: utf-8 -*-
# logging.exception() 会记录调用栈

import logging

logging.basicConfig(
	level='DEBUG',
)

def foo():
	try:
		a = 1 / 0
	except:
		logging.exception('woo!')

def bar():
	foo()

bar()

