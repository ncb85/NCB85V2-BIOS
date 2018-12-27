rem @echo off

set FILENAME=ncb85bios
set HOME=%CD%
set PROJPATH=%HOME%
set ASW_PATH=D:\Users\roman\hobby\hardware_projects\8085\asw\bin
set SORT_PATH=D:\cygwin\bin

set CYGWIN=nodosfilewarning

cd /D %ASW_PATH%

REM FDTYPEs 360k,720k,1.2MB,1.44MB,1.0MB IBM
REM optional second parameter 50 denotes an extra 8" C: SS/DD drive, capacity 500kB
REM call:build 360
REM call:build 720
REM call:build 120
REM call:build 120 50
REM call:build 120 120
call:build 144
REM call:build 144 50
REM call:build 144 144
REM call:build 100
goto:end

:build
set FDTYPE=%~1
set PARAM2=%~2
set EXTRA="0"
IF NOT "%PARAM2%"=="" (
	SET EXTRA=%PARAM2%
)
asw -cpu 8080 -L -OLIST %PROJPATH%\%FILENAME%_%FDTYPE%%PARAM2%.lst -D Floppy=%FDTYPE%,Extra=%EXTRA% -i %PROJPATH% %PROJPATH%\%FILENAME%.asm
if ERRORLEVEL 1 goto:end

p2hex %PROJPATH%\%FILENAME%.p %PROJPATH%\%FILENAME%_%FDTYPE%%PARAM2%.ihx -k -r $-$1FFF -R 57344 -F Intel -i 0 > NUL
rem p2hex %PROJPATH%\%FILENAME%.p %PROJPATH%\%FILENAME%_%FDTYPE%%PARAM2%.ihx -k -r $-$1FFF -R 32768 -F Intel -i 0 > NUL
%SORT_PATH%\sort -k1.8,1.9 -k1.4,1.7 %HOME%/%FILENAME%_%FDTYPE%%PARAM2%.ihx > %HOME%/%FILENAME%_%FDTYPE%%PARAM2%.hex
del %HOME%\%FILENAME%_%FDTYPE%%PARAM2%.ihx
goto:eof

:end
cd /D %HOME%
