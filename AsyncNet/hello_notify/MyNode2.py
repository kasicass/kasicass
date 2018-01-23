# -*- encoding: utf-8 -*-

from AsyncNet import AsyncNotify
from AsyncNet import ASYNC_NOTIFY_EVT_DATA

def main():
	n2 = AsyncNotify(2002)
	n2.listen('127.0.0.1', 8889)

	n2.login_token('abcd')                 # 设置互连的密钥
	n2.sid_add(2001, '127.0.0.1', 8888)    # 添加对方 sid

	n2.trace(None, True, 5)
	n2.option('logmask', 0xff)

	while True:
		n2.wait(0.01)
		while True:
			e, w, l, d = n2.read()
			if e == None:
				break
			if e == ASYNC_NOTIFY_EVT_DATA:
				n2.send(w, l, d)
				print '[N2] RECV cmd=%d data=%s'%(l, repr(d))

	return 0	

if __name__ == '__main__':
	main()

