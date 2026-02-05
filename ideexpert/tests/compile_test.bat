@echo off
cd /d C:\DEV\dmvcframework\ideexpert\tests
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
dcc32 TestTemplateGenerator.dpr
pause
