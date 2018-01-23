# -*- encoding: utf-8 -*-
# args, kwargs 在 class Thread 中用了 __ 变量去保存，继承类无法直接访问
# 如果想用 args, kwargs，要重写继承类的 __init__()，将 args kwargs 记录
# 下来

import threading
import logging

logging.basicConfig(
	level=logging.DEBUG,
	format='(%(threadName)-10s) %(message)s'
	)

class MyThreadWithArgs(threading.Thread):

	def __init__(self, group=None, target=None, name=None, args=(), kwargs=None, verbose=None):
		super(MyThreadWithArgs, self).__init__(group=group, target=target, name=name, verbose=verbose)
		self.args = args
		self.kwargs = kwargs

	def run(self):
		logging.debug('running with %s and %s', self.args, self.kwargs)

for i in range(5):
	t = MyThreadWithArgs(args=(i,), kwargs={'a':'A', 'b':'B'})
	t.start()

