DLL_SRC = PrintMe.c
DLL_OBJ = PrintMe.obj
DLL_CFLAGS  = /c /nologo /Od /MTd /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "_USERDLL" /D "_WINDLL" /D "MY_BUILD_CODE"
DLL_LDFLAGS = /nologo /OUT:"PrintMe.dll" /DLL /SUBSYSTEM:WINDOWS

EXE_SRC = DllClient.cpp
EXE_OBJ = DllClient.obj
EXE_CFLAGS  = /c /nologo /Od /MTd /D "_DEBUG" /D "WIN32" /D "_WINDOWS"
EXE_LDFLAGS = /nologo /OUT:"DllClient.exe" /SUBSYSTEM:CONSOLE

all:
	cl $(DLL_CFLAGS) $(DLL_SRC)
	link $(DLL_LDFLAGS) $(DLL_OBJ)
	cl $(EXE_CFLAGS) $(EXE_SRC)
	link $(EXE_LDFLAGS) $(EXE_OBJ) PrintMe.lib

clean:
	del *.exe *.obj *.dll *.exp *.lib

