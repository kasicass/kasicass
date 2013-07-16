from direct.showbase.ShowBase import ShowBase
from direct.gui.OnscreenImage import OnscreenImage
from direct.actor.Actor import Actor
from panda3d.core import *
import random

class Application(ShowBase):
	def __init__(self):
		ShowBase.__init__(self)

		self.panda = Actor("panda", {"walk":"panda-walk"})
		self.panda.reparentTo(render)
		self.panda.loop("walk")
		self.cam.setPos(0, -30, 5)

		files = ("head1.jpg", "head2.png")
		for i in range(10):
			OnscreenImage(random.choice(files), scale = Vec3(0.15,0,0.15),
				pos = Vec3(random.uniform(-1,1), 0, random.uniform(-1,1)), hpr = Vec3(0, 0, random.uniform(0, 360)))

if __name__ == '__main__':
	gameApp = Application()
	gameApp.run()

