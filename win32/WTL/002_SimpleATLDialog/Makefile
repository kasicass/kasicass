SRC = Main.cpp
OBJ = Main.obj
CFLAGS = /nologo /c /EHsc /Zc:wchar_t /fp:precise /D "_UNICODE" /D "UNICODE" /D "_WINDOWS" /MD
LDFLAGS = /nologo /SUBSYSTEM:WINDOWS

all:
	rc app.rc
	cl $(CFLAGS) $(SRC)
	link /OUT:app.exe $(LDFLAGS) $(OBJ) app.res

clean:
	del *.obj *.exe *.res
