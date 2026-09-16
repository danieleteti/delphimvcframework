@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
msbuild "C:\DEV\dmvcframework\samples\console_themes_demo\ConsoleThemesDemo.dproj" /t:Build /p:Config=Debug /p:Platform=Win32 /v:minimal
