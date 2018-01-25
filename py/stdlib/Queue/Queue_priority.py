import Queue
import threading

class Job(object):
	def __init__(self, priority, description):
		self.priority = priority
		self.description = description
		print 'New job:', description

	def __cmp__(self, other):
		return cmp(self.priority, other.priority)

q = Queue.PriorityQueue()

q.put( Job(3, 'Mid-level job') )
q.put( Job(10, 'Low-level job') )
q.put( Job(1, 'Important job') )

def process_job(q):
	while True:
		next_job = q.get()
		print 'Processing job:', next_job.description
		q.task_done()

workers = [
	threading.Thread(target=process_job, args=(q,)),
	threading.Thread(target=process_job, args=(q,)),
	]

for w in workers:
	w.setDaemon(True)
	w.start()

q.join()

