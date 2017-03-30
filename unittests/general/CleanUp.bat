@echo off

echo Cleaning...

del /f /q /s *.bak
del /f /q /s *.dcu
del /f /q /s *.ddp
del /f /q /s *.~*
del /f /q /s *.local
del /f /q /s *.identcache
del /f /q /s *.tvsconfig

del /f /q /s *.bpl
del /f /q /s *.cbk
del /f /q /s *.dcp
del /f /q /s *.dsk
del /f /q /s *.o
del /f /q /s *.rsm
del /f /q /s *.skincfg
del /f /q /s *.log
del /f /q /s *.stat

for /f "tokens=* delims=" %%i in ('dir /s /b /a:d __history') do (
  rd /s /q "%%i"
)

for /f "tokens=* delims=" %%i in ('dir /s /b /a:d __recovery') do (
  rd /s /q "%%i"
)

for /f "tokens=* delims=" %%i in ('dir /s /b /a:d Win32') do (
  rd /s /q "%%i"
)

for /f "tokens=* delims=" %%i in ('dir /s /b /a:d Win64') do (
  rd /s /q "%%i"
)

if "%1"=="" goto :eof
pause