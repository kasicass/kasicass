SRC = filetime.cpp
OBJ = filetime.obj
CFLAGS = /c /EHsc /Zc:wchar_t /fp:precise /D_WIN32_WINNT=0x0500 /D "_MBCS" /D "_CONSOLE" /D "_DEBUG" /MDd
LDFLAGS = /SUBSYSTEM:CONSOLE

all:
	cl $(CFLAGS) $(SRC)
	link /OUT:filetime.exe $(LDFLAGS) $(OBJ)

clean:
	del *.obj *.exe *.res
