from direct.showbase.ShowBase import ShowBase
from panda3d.core import *

class Application(ShowBase):
	def __init__(self):
		ShowBase.__init__(self)
		cm = CardMaker("plane")
		cm.setFrame(-1, 1, -1, 1)

		plane = render2d.attachNewNode(cm.generate())
		movie = loader.loadTexture("movie.avi")
		
		plane.setTexture(movie)
		plane.setTexScale(TextureStage.getDefault(), movie.getTexScale())
		
		movie.setLoop(1)
		movie.play()

if __name__ == '__main__':
	gameApp = Application()
	gameApp.run()

