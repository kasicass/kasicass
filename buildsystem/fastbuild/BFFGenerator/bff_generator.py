import fileinput

# ObjectList('Name')
# {
#   .Compiler = 'Compiler-x86'
#   .CompilerOutputPath = 'Debug\'
#   .CompilerInputFiles = { 'Source.cpp', 'Source1.cpp' }
#   .CompilerOptions = '%1 /Fo"%2" '
#                    + '...'
# }
class ObjectList(object):
	def __init__(self):
		self.Name = '??'
		self.Compiler = 'Compiler-x86'
		self.CompilerOutputPath = '??'
		self.CompilerInputFiles = []
		self.CompilerOptions = '??'

	def toBFF(self):
		files = "{ '" + "', '".join(self.CompilerInputFiles) + "' }"
		return """
ObjectList('%s')
{
	.Compiler = '%s'
	.CompilerOutputPath = '%s'
	.CompilerInputFiles = %s
	.CompilerOptions = '%%1 /Fo"%%2" '
                   + '%s'
}
""" % (self.Name, self.Compiler, self.CompilerOutputPath, files, self.CompilerOptions)


def tokenlizeArgs(args):
	# '/Zc:forScope /Fo"Debug\\" /Fd"Debug\vc120.pdb"' => ['/Zc:forScope', '/Fo"Debug\\"', '/Fd"Debug\vc120.pdb"']
	# TODO: '/Fo"Out Put\\"' - space character
	return args.split(' ')

def parseCL(args):
	tokens = tokenlizeArgs(args)
	objlist = ObjectList()
	otherOptions = []
	isOptions = False
	for t in tokens:
		if isOptions:
			otherOptions.append(t)
			isOptions = False
			continue

		if t[0] == '/':
			if t.startswith('/Fo'):
				objlist.CompilerOutputPath = t[3:].replace('"', '')
			elif t.startswith('/Fd'):
				otherOptions.append(t)
				otherOptions.append('/FS')  # multi cl.exe write .pdb at the same time, need this
			elif t.startswith('/D'):
				otherOptions.append(t)
				isOptions = True
			else:
				otherOptions.append(t)
		else:
			objlist.CompilerInputFiles.append(t)
	objlist.CompilerOptions = ' '.join(otherOptions)
	return objlist

if __name__ == '__main__':
	for line in fileinput.input():
		line = line.strip()
		n = line.find(' ')
		cmd  = line[:n].lower()
		args = line[n+1:]
		if cmd == "cl.exe":
			objlist = parseCL(args)
			print objlist.toBFF()

