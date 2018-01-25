from Queue import Queue
from threading import Thread
import time
import urllib
import urlparse

import feedparser

num_fetch_threads = 2
enclosure_queue   = Queue()

feed_urls = [
	'https://blog.codingnow.com/atom.xml'
]

def downloadEnclosures(i, q):
	while True:
		print '%s: Looking for the next enclosure' % i
		url = q.get()
		parsed_url = urlparse.urlparse(url)

