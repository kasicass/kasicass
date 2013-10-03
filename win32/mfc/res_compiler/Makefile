# http://msdn.microsoft.com/en-us/library/windows/desktop/aa381042(v=vs.85).aspx

SRC = KApp.cpp KMainWindow.cpp KGdiPlusBitmap.cpp
OBJ = KApp.obj KMainWindow.obj KGdiPlusBitmap.obj
CFLAGS = /c /EHsc /Zc:wchar_t /fp:precise /D_WIN32_WINNT=0x0500 /D "_MBCS" /D "_WINDOWS" /D "_AFXDLL" /MD
LDFLAGS = /SUBSYSTEM:WINDOWS gdiplus.lib

all:
	rc app.rc
	cl $(CFLAGS) $(SRC)
	link /OUT:app.exe $(LDFLAGS) $(OBJ) app.res

clean:
	del *.obj *.exe *.res
