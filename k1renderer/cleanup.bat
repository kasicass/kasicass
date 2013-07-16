rmdir /Q /S bin
rmdir /Q /S build
rmdir /Q /S lib

rmdir /Q /S vs2010\ipch
del /F /Q vs2010\samples\*.user
del /F /Q vs2010\src\*.user
del /F /Q vs2010\*.sdf
del /F /Q /AH vs2010\*.suo