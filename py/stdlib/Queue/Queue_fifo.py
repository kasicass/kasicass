# -*- encoding: utf-8 -*-
# thread-safe queue

import Queue

q = Queue.Queue()
for i in range(5):
	q.put(i)

while not q.empty():
	print q.get(),
print

