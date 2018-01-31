import socket
from pyuv import Loop
from pyuv import dns

def dns_callback(result, error):
	for info in result:
		# addrinfo_result(family=2, socktype=1, proto=6, canonname='', sockaddr=('71.11.84.232', 6667))
		print '-------------------'
		print 'addr:', info.sockaddr
		print 'cname:', info.canonname
		print 'family:', info.family
		print 'socktype:', info.socktype
		print 'proto:', info.proto

loop = Loop.default_loop()

dns.getaddrinfo(loop, 'irc.freenode.net', 6667, 0, 0, socket.IPPROTO_TCP, 0, callback=dns_callback)
loop.run()

