# -*- encoding:utf-8 -*-

from direct.showbase.ShowBase import ShowBase
from direct.gui.OnscreenText import OnscreenText
from panda3d.core import *

class Application(ShowBase):
    def __init__(self):
        ShowBase.__init__(self)
        font = loader.loadFont("simsun.ttc")
        props = TextProperties()
        props.setTextColor(1, 1, 0, 0.5)
        tp = TextPropertiesManager.getGlobalPtr()
        tp.setProperties("yellow", props)
        OnscreenText(text = "Panda3D Rocks!!中文", frame = Vec4(1, 0, 0, 1), bg = Vec4(1, 1, 0, 1),
            pos = Vec2(-0.5, 0.5), scale = 0.2, font = font)

        wrapWidth = 6
        text = OnscreenText(text = "So long... \1yellow\1And thanks for all the bamboo\2!!", wordwrap = wrapWidth,
            fg = Vec4(1,1,1,1), shadow = Vec4(0,0,0,1), scale = 0.07, font = font)
        wrap = text.getScale()[0] * wrapWidth
        print "Word wrap after", wrap, "screen units"


if __name__ == '__main__':
    gameApp = Application()
    gameApp.run()

