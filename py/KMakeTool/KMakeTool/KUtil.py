import types
import uuid

class InfoObject(object):
	pass

def readPyFileInfo(pyfile):
	with open(pyfile, 'r') as f:
		data = f.read()
	co = compile(data, pyfile, 'exec')
	info = InfoObject()
	exec co in info.__dict__
	return info

def newGuid():
	return '{' + str(uuid.uuid4()).upper() + '}'

