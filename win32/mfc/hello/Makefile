SRC = KApp.cpp KMainWindow.cpp
OBJ = KApp.obj KMainWindow.obj
CFLAGS = /c /EHsc /Zc:wchar_t /fp:precise /D_WIN32_WINNT=0x0500 /D "_MBCS" /D "_WINDOWS" /D "_AFXDLL" /MD
LDFLAGS = /SUBSYSTEM:WINDOWS

all:
	cl $(CFLAGS) $(SRC)
	link /OUT:app.exe $(LDFLAGS) $(OBJ)

clean:
	del *.obj *.exe *.res
