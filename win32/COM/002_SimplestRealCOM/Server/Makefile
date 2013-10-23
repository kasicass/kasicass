SRC = CoCat.cpp CoCatFactory.cpp iids.cpp DllMain.cpp
OBJ = CoCat.obj CoCatFactory.obj iids.obj DllMain.obj
CFLAGS = /c /nologo /Od /MTd /EHsc /D "_MBCS" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "_USERDLL" /D "_WINDLL"
LDFLAGS = /nologo /OUT:"CoCat.dll" /DLL /SUBSYSTEM:WINDOWS /DEF:".\CoCat.def"

all:
	cl $(CFLAGS) $(SRC)
	link $(LDFLAGS) $(OBJ) OleAut32.lib

clean:
	del *.obj *.dll *.exp *.lib

