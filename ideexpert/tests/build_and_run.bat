@echo off
cd /d C:\DEV\dmvcframework\ideexpert\tests
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
msbuild TestTemplateGenerator.dproj /p:Config=Debug /p:Platform=Win32 /t:Build /v:minimal
if errorlevel 1 (
    echo Build failed!
    exit /b 1
)
echo Build successful, running tests...
%CD%\TestTemplateGenerator.exe
