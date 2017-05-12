SRC = example1.c example1_driver.c erl_comm.c
OBJ = example1.obj example1_driver.obj erl_comm.obj
CFLAGS = /c /Zc:wchar_t /fp:precise /D "_MBCS" /D "_CONSOLE" /MD
LDFLAGS = /SUBSYSTEM:CONSOLE

all:
	cl $(CFLAGS) $(SRC)
	link /OUT:example1.exe $(LDFLAGS) $(OBJ)

clean:
	del *.obj *.exe *.beam

