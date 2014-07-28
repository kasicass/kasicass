import os
import sys
sys.path.append(os.getcwd() + '/KMakeTool/')

from KMakeTool.KSolutionMaker import KSolutionMaker

if __name__ == '__main__':
	maker = KSolutionMaker(os.getcwd())
	maker.setCompiler('vs2012')
	maker.run()

