SRC = CriticalSectionMemTest.c
OBJ = CriticalSectionMemTest.obj
CFLAGS = /c /EHsc /Zc:wchar_t /fp:precise /D "_MBCS" /D "_CONSOLE" /D "PSAPI_VERSION=1" /MT
LDFLAGS = /SUBSYSTEM:CONSOLE /LARGEADDRESSAWARE psapi.lib

all:
	cl $(CFLAGS) $(SRC)
	link /OUT:csmemtest.exe $(LDFLAGS) $(OBJ)

clean:
	del *.obj *.exe *.res
