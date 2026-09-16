@echo off
cd /d %~dp0
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
msbuild ErrorPageTest.dproj /p:Config=Debug /p:Platform=Win32 /t:Build /v:minimal
if errorlevel 1 (
    echo Build failed!
    exit /b 1
)
echo Build successful, running tests...
.\bin\ErrorPageTest.exe
exit /b %errorlevel%
