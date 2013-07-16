all: prepare ConsoleExe

prepare:
	-mkdir -p bin

LIB_SRC=src/ICommand.cs src/CommandFactory.cs src/CommandCreateDatabase.cs src/SqliteConsole.cs
SqliteConsole:
	dmcs -debug /out:bin/SqliteConsole.dll /r:Mono.Data.Sqlite.dll /t:library $(LIB_SRC)

EXE_SRC=src/Main.cs
ConsoleExe: SqliteConsole
	dmcs -debug /out:bin/myconsole.exe /r:bin/SqliteConsole.dll /t:winexe $(EXE_SRC)

clean:
	-rm -dRf bin
