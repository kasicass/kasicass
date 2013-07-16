import sys
import os
from os import path

def usage():
	print "python filediff.py <file1 or dir1> <file2 or dir2>"

def filediff(file1, file2):
	f1 = open(file1, 'rb')
	f2 = open(file2, 'rb')

	while True: 
		s1 = f1.read(8196)
		s2 = f2.read(8196)
		if len(s1) == 0 and len(s2) == 0:
			break

	if s1 != s2:
		return False

	return True

def dirdiff(dir1, dir2):
	pathmap = {}   # 'subfilename' : {'p1':path1, 'p2':path2},
	for root, dirs, files in os.walk(dir1):
		for filename in files:
			filename = path.join(root, filename)
			subfilename = filename.replace(dir1, '')
			pathmap.setdefault(subfilename, {})['p1'] = filename

	for root, dirs, files in os.walk(dir2):
		for filename in files:
			filename = path.join(root, filename)
			subfilename = filename.replace(dir2, '')
			pathmap.setdefault(subfilename, {})['p2'] = filename

	for k, v in pathmap.iteritems():
		if v.has_key('p1') and v.has_key('p2'):
			identical = filediff(v['p1'], v['p2'])
			if not identical:
				print '%s%*s not-identical' % (k, 40-len(k), '')
			elif v.has_key('p1'):
				print '%s%*s only in %s' % (k, 40-len(k), '', dir1)
			else:
				print '%s%*s only in %s' % (k, 40-len(k), '', dir2)

if __name__ == '__main__':
	if len(sys.argv) != 3:
		usage()
		sys.exit(1)

	file1 = path.realpath(sys.argv[1])
	file2 = path.realpath(sys.argv[2])
	if file1 == file2:
		print 'file/dir name equal!', file1
	sys.exit(0)
  
	if path.isfile(file1) and path.isfile(file2):
		print '-------------------------------------------'
		print file1, file2, "equal" if filediff(file1, file2) else "not-equal"
		print '-------------------------------------------'
		print 'done!'
	elif path.isdir(file1) and path.isdir(file2):
		print '-------------------------------------------'
		dirdiff(file1, file2)
		print '-------------------------------------------'
		print 'done!'
	else:
		print 'no diff! are you sure dir/file exists?'

