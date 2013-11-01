SRC = kill.cpp
OBJ = kill.obj
CFLAGS = /c /EHsc /Zc:wchar_t /fp:precise /D_WIN32_WINNT=0x0500 /D "_MBCS" /D "_CONSOLE" /D "_DEBUG" /MDd
LDFLAGS = /SUBSYSTEM:CONSOLE user32.lib

all:
	cl $(CFLAGS) $(SRC)
	link /OUT:kill.exe $(LDFLAGS) $(OBJ)

clean:
	del *.obj *.exe *.res
