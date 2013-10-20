# /c, compile only
# /EHsc, c++ exception
# /fp:precise, floating point computation mode
# /D "_WINDOWS" + /SUBSYSTEM:WINDOWS, win32 app
# /D "_CONSOLE" + /SUBSYSTEM:CONSOLE, console app
# /D "_UNICODE" /D "UNICODE", unicode compile
# /D "_MBCS", 

SRC = Main.cpp
OBJ = Main.obj
CFLAGS = /nologo /c /EHsc /Zc:wchar_t /fp:precise /D "_UNICODE" /D "UNICODE" /D "_WINDOWS" /MD
LDFLAGS = /SUBSYSTEM:WINDOWS

all:
	cl $(CFLAGS) $(SRC)
	link /OUT:app.exe $(LDFLAGS) $(OBJ)

clean:
	del *.obj *.exe
