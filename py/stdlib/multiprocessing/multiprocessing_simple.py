import time
import multiprocessing

def worker(i):
	print 'Worker #%d start' % i
	time.sleep(i)
	print 'Worker #%d end' % i
	return

if __name__ == '__main__':
	jobs = []
	for i in range(5):
		p = multiprocessing.Process(target=worker, args=(i,))
		jobs.append(p)
		p.start()

