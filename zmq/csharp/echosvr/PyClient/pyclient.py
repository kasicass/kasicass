# python -m pip install zmq

import zmq

if __name__ == '__main__':
	ctx = zmq.Context(1)
	sock_client = ctx.socket(zmq.REQ)
	sock_client.connect('tcp://localhost:5555')
	sock_client.send('From Python')

	s = sock_client.recv()
	print 'echo:', s

