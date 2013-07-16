# uses ~N^2/2 compares and N exchanges to sort an array of length N

import sortbase

def selection_sort(dataList):
	for i in xrange(0, len(dataList)):
		k = i
		for j in xrange(i+1, len(dataList)):
			if dataList[j] < dataList[k]:
				k = j
		if k != i:
			dataList[i], dataList[k] = dataList[k], dataList[i]
	return dataList

if __name__ == '__main__':
	print selection_sort(sortbase.getDataList())

