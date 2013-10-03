# /c, compile only
# /EHsc, c++ exception
# /fp:precise, floating point computation mode
# /D "_WINDOWS" + /SUBSYSTEM:WINDOWS, win32 app
# /D "_CONSOLE" + /SUBSYSTEM:CONSOLE, console app
# /D "_UNICODE" /D "UNICODE", unicode compile
# /D "_MBCS", 

SRC = KApp.cpp KMainWindow.cpp KGdiPlusBitmap.cpp
OBJ = KApp.obj KMainWindow.obj KGdiPlusBitmap.obj
#CFLAGS = /c /EHsc /Zc:wchar_t /fp:precise /D_WIN32_WINNT=0x0500 /D "_UNICODE" /D "UNICODE" /D "_WINDOWS" /D "_AFXDLL" /MD
CFLAGS = /c /EHsc /Zc:wchar_t /fp:precise /D_WIN32_WINNT=0x0500 /D "_MBCS" /D "_WINDOWS" /D "_AFXDLL" /MD
LDFLAGS = /SUBSYSTEM:WINDOWS gdiplus.lib

all:
	cl $(CFLAGS)  $(SRC)
	link /OUT:app.exe $(LDFLAGS) $(OBJ)

clean:
	del *.obj *.exe
