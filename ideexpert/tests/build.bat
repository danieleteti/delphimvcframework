@echo off
cd /d "%~dp0"
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
msbuild TestTemplateGenerator.dproj /p:Config=DEBUG /p:Platform=Win32 /t:Build
