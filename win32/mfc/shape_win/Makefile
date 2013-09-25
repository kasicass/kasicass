# http://www.nuonsoft.com/blog/2009/05/27/how-to-use-updatelayeredwindow/
# http://www.sineysoft.com/blog/post/77.html
# http://www.codeproject.com/Articles/3537/Loading-JPG-PNG-resources-using-GDI

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
