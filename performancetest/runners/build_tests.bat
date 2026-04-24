@echo off
REM Crystallised benchmark runner: build the test CLIENT (bench driver).
REM Used during the 3.5.0-silicon ITDEVCON performance run.
REM Paths are resolved relative to this script so it works from any
REM clone of the repo.
cd /d "%~dp0..\..\unittests\general\TestClient"
call "c:\program files (x86)\embarcadero\studio\37.0\bin\rsvars.bat"
set DCC_Define=CONSOLE_TESTRUNNER;CI
msbuild /t:Build /p:Config=Debug /p:Platform=Win64 DMVCFrameworkTests.dproj
