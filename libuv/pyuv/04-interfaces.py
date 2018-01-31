from pyuv import util

addrs = util.interface_addresses()
print "Number of interfaces:", len(addrs)
for addr in addrs:
	print "---------------"
	print "Name:", addr.name
	print "  Internal:", addr.is_internal
	print "  Netmask:", addr.netmask
	print "  Mac:", addr.mac

