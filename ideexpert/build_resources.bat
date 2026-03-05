@echo off
rem ***************************************************************************
rem
rem Delphi MVC Framework - Build Template Resources
rem
rem Compiles DMVC.Expert.Templates.rc into DMVC.Expert.Templates.res
rem This script is called by the pre-build event of dmvcframeworkDT packages
rem and can also be run manually before opening the IDE.
rem
rem ***************************************************************************

setlocal

rem Save current directory and move to ideexpert folder
pushd "%~dp0"

rem Check if .res is already up-to-date (newer than .rc and all .tpro files)
set NEEDS_BUILD=0

if not exist DMVC.Expert.Templates.res (
  set NEEDS_BUILD=1
  goto :dobuild
)

rem Check if .rc is newer than .res
for %%A in (DMVC.Expert.Templates.rc) do set RC_DATE=%%~tA
for %%A in (DMVC.Expert.Templates.res) do set RES_DATE=%%~tA

rem Always rebuild to be safe - brcc32 is fast
set NEEDS_BUILD=1

:dobuild
if %NEEDS_BUILD%==0 (
  echo DMVC.Expert.Templates.res is up-to-date.
  goto :done
)

echo Compiling template resources...

rem Try to find brcc32 in PATH first (works when rsvars.bat has been called)
where brcc32 >nul 2>&1
if %ERRORLEVEL%==0 (
  brcc32 DMVC.Expert.Templates.rc
  if %ERRORLEVEL%==0 (
    echo Template resources compiled successfully.
  ) else (
    echo ERROR: brcc32 failed to compile resources.
    popd
    exit /b 1
  )
  goto :done
)

rem Try known Delphi installations (newest first)
for %%V in (37.0 24.0 23.0 22.0 21.0 20.0 19.0 18.0) do (
  if exist "C:\Program Files (x86)\Embarcadero\Studio\%%V\bin\brcc32.exe" (
    echo Found brcc32 in Delphi %%V
    "C:\Program Files (x86)\Embarcadero\Studio\%%V\bin\brcc32.exe" DMVC.Expert.Templates.rc
    if %ERRORLEVEL%==0 (
      echo Template resources compiled successfully.
    ) else (
      echo ERROR: brcc32 failed to compile resources.
      popd
      exit /b 1
    )
    goto :done
  )
)

echo ERROR: brcc32 not found. Please run rsvars.bat first or install Delphi.
popd
exit /b 1

:done
popd
exit /b 0
