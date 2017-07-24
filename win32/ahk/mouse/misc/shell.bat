@echo off

REM
REM  To run this at startup, use this as your shortcut target:
REM  %windir%\system32\cmd.exe /k w:\handmade\misc\shell.bat
REM

IF EXIST w: subst /d w:
subst w: D:\myprj\kasicass\win32\ahk

call w:\mouse\misc\setenv.bat
w:
