@echo off
REM Crystallised benchmark runner: build the Indy Direct test SERVER
REM (one of the three backends compared for ITDEVCON 2026).
cd /d "%~dp0..\..\unittests\general\TestServer"
call "c:\program files (x86)\embarcadero\studio\37.0\bin\rsvars.bat"
msbuild /t:Build /p:Config=Debug /p:Platform=Win64 TestServerIndyDirect.dproj
