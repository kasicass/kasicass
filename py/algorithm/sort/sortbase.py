import random

def getDataList():
	a = []
	for i in xrange(0, 100):
		a.append(int(random.uniform(0, 10000)))
	return a

