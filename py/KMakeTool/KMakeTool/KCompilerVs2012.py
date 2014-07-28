import os
import KUtil

class KProjectTagVs2012(object):
	def __init__(self, projectDir):
		projectDir  = projectDir.replace('/', '\\')
		projectFile = projectDir + r'\KProject.py'
		projectInfo = KUtil.readPyFileInfo(projectFile)
		self.name = projectInfo.PROJECT_NAME
		self.vcxprojPath = '..\\' + projectDir + '\\' + self.name + '.vcxproj'
		self.guid = KUtil.newGuid()

	def projTag(self):
		return 'Project("{8BC9CEB8-8B4A-11D0-8D11-00A0C91BC942}") = "%s", "%s", "%s"\nEndProject\n' % (self.name, self.vcxprojPath, self.guid)

	def globalTag(self):
		return """		%s.Debug|Win32.ActiveCfg = Debug|Win32
		%s.Debug|Win32.Build.0 = Debug|Win32
		%s.Release|Win32.ActiveCfg = Release|Win32
		%s.Release|Win32.Build.0 = Release|Win32
""" % (self.guid, self.guid, self.guid, self.guid)

class KCompilerVs2012(object):

	def __init__(self, solutionInfo):
		self.solutionInfo = solutionInfo
		self.projTags = []
		for projPath in solutionInfo.PROJECTS:
			self.projTags.append( KProjectTagVs2012(projPath) )

	def run(self):
		outDir = self.solutionInfo.SOLUTION_DIR + os.sep + 'vs2012'
		try:
			os.mkdir(outDir)
		except WindowsError:
			pass

		outSolutionFile = outDir + os.sep + self.solutionInfo.SOLUTION_NAME + '.sln'
		f = open(outSolutionFile, 'w+')
		f.write('\n')
		f.write('Microsoft Visual Studio Solution File, Format Version 12.00\n')
		f.write('# Visual Studio 2012\n')

		# projects
		for projTag in self.projTags:
			f.write(projTag.projTag())

		f.write('Global\n')
		f.write('	GlobalSection(SolutionConfigurationPlatforms) = preSolution\n')
		f.write('		Debug|Win32 = Debug|Win32\n')
		f.write('		Release|Win32 = Release|Win32\n')
		f.write('	EndGlobalSection\n')

		# global tag
		for projTag in self.projTags:
			f.write(projTag.globalTag())
		
		f.write('	GlobalSection(SolutionProperties) = preSolution\n')
		f.write('		HideSolutionNode = FALSE\n')
		f.write('	EndGlobalSection\n')
		f.write('EndGlobal\n')
		f.close()

