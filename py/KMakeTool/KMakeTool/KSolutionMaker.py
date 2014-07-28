import os
import KUtil
from KCompilerVs2012 import KCompilerVs2012

class KSolutionMaker(object):
	def __init__(self, solutionDir):
		self.compiler = 'vs2012'
		self.solutionDir = solutionDir

	def setCompiler(self, compiler):
		self.compiler = compiler

	def run(self):
		solutionFile = os.getcwd() + os.sep + 'KMakefile.py'
		solutionInfo = KUtil.readPyFileInfo(solutionFile)

		print 'Solution File:', solutionFile
		print 'Generating \'%s\' ...' % solutionInfo.SOLUTION_NAME

		solutionInfo.SOLUTION_DIR = os.getcwd()
		compiler = KCompilerVs2012(solutionInfo)
		compiler.run()

		print 'Generating done.'

