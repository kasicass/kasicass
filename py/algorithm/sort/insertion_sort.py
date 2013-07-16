# randomly ordered array - uses ~N^2/4 compares and ~N^2/4 exchanges
# worst - ~N^2/2 compares and ~N^2/2 exchanges
# best  - N-1 compares and 0 exchange, best sorting method for sorted array among other methods

import sortbase

def insertion_sort(dataList):
	for i in xrange(1, len(dataList)):
		for j in xrange(i, 0, -1):
			if dataList[j] < dataList[j-1]:
				dataList[j], dataList[j-1] = dataList[j-1], dataList[j]
	return dataList

if __name__ == '__main__':
	print insertion_sort(sortbase.getDataList())

