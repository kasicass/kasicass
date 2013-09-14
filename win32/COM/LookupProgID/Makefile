SRC = LookupProgID.cpp
OBJ = LookupProgID.obj
CFLAGS = /nologo /c /EHsc /Zc:wchar_t /fp:precise /D "_CONSOLE" /D /MD
LDFLAGS = /nologo /SUBSYSTEM:CONSOLE

all:
	cl.exe 
	cl $(CFLAGS) $(SRC)
	link /OUT:LookupProgID.exe $(LDFLAGS) $(OBJ)

clean:
	del *.obj *.exe
