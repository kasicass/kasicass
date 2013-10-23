# a simplest hard-coded COM server emulation
SRC = main.cpp CoCat.cpp CoCatFactory.cpp iids.cpp
OBJ = main.obj CoCat.obj CoCatFactory.obj iids.obj
CFLAGS = /c /EHsc /Zc:wchar_t /fp:precise /D_WIN32_WINNT=0x0500 /D "_MBCS" /D "_CONSOLE" /MD
LDFLAGS = /SUBSYSTEM:CONSOLE

all:
	cl $(CFLAGS) $(SRC)
	link /OUT:cat.exe $(LDFLAGS) $(OBJ) OleAut32.lib

clean:
	del *.obj *.exe

