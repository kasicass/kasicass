import time
from AsyncNet import AsyncCore
from AsyncNet import HEADER_WORDLSB
from AsyncNet import ASYNC_EVT_NEW, ASYNC_EVT_LEAVE, ASYNC_EVT_ESTAB, ASYNC_EVT_DATA
from AsyncNet import ASYNC_MODE_IN

def main():
	core = AsyncCore()
	client_hid = core.new_connect('127.0.0.1', 8888, HEADER_WORDLSB)
	if client_hid < 0:
		print 'can not connect to localhost:8888'
		return -1

	while True:
		core.wait(0.1)
		while True:
			event, hid, tag, data = core.read()
			if event == None:
				break

			if event == ASYNC_EVT_NEW:
				print time.strftime('[%Y-%m-%d %H:%M:%S]'), 'new hid=%xh'%hid
			elif event == ASYNC_EVT_LEAVE:
				print time.strftime('[%Y-%m-%d %H:%M:%S]'), 'leave hid=%xh'%hid
			elif event == ASYNC_EVT_ESTAB:
				print time.strftime('[%Y-%m-%d %H:%M:%S]'), 'estab hid=%xh'%hid
				core.send(hid, 'START')
			elif event == ASYNC_EVT_DATA:
				print time.strftime('[%Y-%m-%d %H:%M:%S]'), 'recv hid=%xh'%hid, 'data', data
		

if __name__ == '__main__':
	main()

