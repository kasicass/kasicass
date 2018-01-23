# -*- encoding: utf-8 -*-

from AsyncNet import AsyncNotify
from AsyncNet import ASYNC_NOTIFY_EVT_DATA

def main():
	n1 = AsyncNotify(2001)
	n1.listen('127.0.0.1', 8888)

	n1.login_token('abcd')                 # 设置互连的密钥
	n1.sid_add(2002, '127.0.0.1', 8889)    # 添加对方 sid

	n1.send(2002, 1, 'hello')
	n1.send(2002, 2, 'world!')

	n1.trace(None, True, 5)
	n1.option('logmask', 0xff)

	import time
	ts = time.time() + 5
	index = 0

	while True:
		n1.wait(0.01)
		while True:
			e, w, l, d = n1.read()
			if e == None:
				break
			if e == ASYNC_NOTIFY_EVT_DATA:
				print '[N1] RECV cmd=%d data=%s'%(l, repr(d))

		if time.time() > ts:
			print '[N1] send to [N2]', 'index=%d'%index
			ts = time.time() + 5
			n1.send(2002, 3, 'index:\x00 %d'%index)
			index += 1

	return 0	

if __name__ == '__main__':
	main()

