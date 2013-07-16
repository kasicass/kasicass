import sortbase

def shell_sort(dataList):
	N = len(dataList)
	h = 1

	while h < N/3:
		h = 3*h + 1		# 1, 4, 13, 40, 121, 364, 1093, ...

	while h >= 1:
		# h-sort the array
		for i in xrange(h, N):
			# insert a[i] amont a[i-h], a[i-2*h], a[i-3*h] ...
			for j in xrange(i, h-1, -h):
				if dataList[j] < dataList[j-h]:
					dataList[j], dataList[j-h] = dataList[j-h], dataList[j]
		h /= 3
	return dataList

if __name__ == '__main__':
	print shell_sort(sortbase.getDataList())

