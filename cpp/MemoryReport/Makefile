SRC = mm_config.cpp mm_record.cpp mm_mallocfree.cpp test.cpp
OBJ = mm_config.obj mm_record.obj mm_mallocfree.obj test.obj
CFLAGS = /c /EHsc /Zc:wchar_t /fp:precise /D_WIN32_WINNT=0x0500 /D "_MBCS" /D "_WINDOWS" /D "MEMORY_REPORT_ENABLE" /MD
#CFLAGS = /c /EHsc /Zc:wchar_t /fp:precise /D_WIN32_WINNT=0x0500 /D "_MBCS" /D "_WINDOWS" /MD
LDFLAGS = /SUBSYSTEM:CONSOLE

all:
	cl $(CFLAGS) $(SRC)
	link /OUT:test.exe $(LDFLAGS) $(OBJ)

clean:
	del *.obj *.exe *.res
