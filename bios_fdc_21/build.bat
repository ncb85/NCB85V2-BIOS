@echo off

set FILENAME=ncb85bios21
set HOME=%CD%
set PROJPATH=%HOME%
set ASW_PATH=D:\Users\roman\hobby\hardware_projects\8085\asw\bin
set SORT_PATH=D:\cygwin\bin
set HEX2BIN_PATH=D:\Users\roman\hobby\hardware_projects\8085\ncb85\Utils

set CYGWIN=nodosfilewarning

cd /D %ASW_PATH%

asw -cpu 8080 -L -i %PROJPATH% %PROJPATH%\%FILENAME%.asm

if ERRORLEVEL 1 goto error

p2hex %PROJPATH%\%FILENAME%.p %PROJPATH%\%FILENAME%.ihx -k -r $-$1FFF -R 57344 -F Intel -i 0 > NUL

%SORT_PATH%\sort -k1.8,1.9 -k1.4,1.7 %HOME%/%FILENAME%.ihx > %HOME%/%FILENAME%.hex

goto end

:error
echo Chyba kompilacie.

:end
cd /D %HOME%
