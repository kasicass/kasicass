import time
from AsyncNet import AsyncCore
from AsyncNet import HEADER_WORDLSB
from AsyncNet import ASYNC_EVT_NEW, ASYNC_EVT_LEAVE, ASYNC_EVT_ESTAB, ASYNC_EVT_DATA
from AsyncNet import ASYNC_MODE_IN

def main():
	core = AsyncCore()
	hid_listen = core.new_listen('127.0.0.1', 8888, HEADER_WORDLSB)
	if hid_listen < 0:
		print 'can not listen on port 8888'
		return -1

	print 'listen on localhost:8888 hid=%xh' % hid_listen

	index = 0
	clients = set()
	timeslap = time.time()
	while True:
		core.wait(0.1)
		while True:
			event, hid, tag, data = core.read()
			if event == None:
				break

			if event == ASYNC_EVT_NEW:
				print time.strftime('[%Y-%m-%d %H:%M:%S]'), 'new hid=%xh'%hid
				if core.get_mode(hid) == ASYNC_MODE_IN:
					clients.add(hid)
					print time.strftime('[%Y-%m-%d %H:%M:%S]'), 'accept hid=%xh'%hid
			elif event == ASYNC_EVT_LEAVE:
				print time.strftime('[%Y-%m-%d %H:%M:%S]'), 'leave hid=%xh'%hid
				clients.remove(hid)
			elif event == ASYNC_EVT_ESTAB:
				print time.strftime('[%Y-%m-%d %H:%M:%S]'), 'estab hid=%xh'%hid
			elif event == ASYNC_EVT_DATA:
				print time.strftime('[%Y-%m-%d %H:%M:%S]'), 'recv hid=%xh'%hid, 'data', data

		if clients:
			current = time.time()
			if current > timeslap:
				timeslap = current + 5
				for hid in clients:
					core.send(hid, 'ECHO\x00%d'%index)
					index += 1

	return 0

if __name__ == '__main__':
	main()

