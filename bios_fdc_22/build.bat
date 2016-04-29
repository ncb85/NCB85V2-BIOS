@echo off

set FILENAME=ncb85bios
set HOME=%CD%
set PROJPATH=%HOME%
set ASW_PATH=D:\Users\roman\hobby\hardware_projects\8085\asw\bin
set SORT_PATH=D:\cygwin\bin

set CYGWIN=nodosfilewarning

cd /D %ASW_PATH%

REM FDTYPEs 360,720,120,144
call:build 360
call:build 720
call:build 120
call:build 144
goto:end

:build
set FDTYPE=%~1
asw -cpu 8080 -L -OLIST %PROJPATH%\%FILENAME%_%FDTYPE%.lst -D Floppy=%FDTYPE% -i %PROJPATH% %PROJPATH%\%FILENAME%.asm
if ERRORLEVEL 1 goto:end

p2hex %PROJPATH%\%FILENAME%.p %PROJPATH%\%FILENAME%_%FDTYPE%.ihx -k -r $-$1FFF -R 57344 -F Intel -i 0 > NUL
%SORT_PATH%\sort -k1.8,1.9 -k1.4,1.7 %HOME%/%FILENAME%_%FDTYPE%.ihx > %HOME%/%FILENAME%_%FDTYPE%.hex
del %HOME%\%FILENAME%_%FDTYPE%.ihx
goto:eof

:end
cd /D %HOME%
