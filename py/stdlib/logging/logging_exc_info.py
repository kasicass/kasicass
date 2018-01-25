# -*- encoding: utf-8 -*-
# 在 except 的时候，可以用 logging.exception() 来捕获 exception 的内容
# logging 模块内部用了 sys.exc_info() 的内容

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

