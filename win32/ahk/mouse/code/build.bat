@echo off

IF NOT EXIST ..\..\build mkdir ..\..\build

pushd ..\..\build
cl -Zi ..\mouse\code\win32_mouse.cpp user32.lib gdi32.lib
cl -Zi ..\mouse\code\win32_post.cpp user32.lib gdi32.lib
popd
