SRC = heaptype.c
OBJ = heaptype.obj
CFLAGS = /c /EHsc /Zc:wchar_t /fp:precise /D "_MBCS" /D "_CONSOLE" /D "_DEBUG" /MDd
LDFLAGS = /SUBSYSTEM:CONSOLE /LARGEADDRESSAWARE kernel32.lib

all:
	cl $(CFLAGS) $(SRC)
	link /OUT:heaptype.exe $(LDFLAGS) $(OBJ)

clean:
	del *.obj *.exe *.res
