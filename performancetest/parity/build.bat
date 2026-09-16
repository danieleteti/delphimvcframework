@echo off
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
msbuild "%~dp0ParityCheck.dproj" /p:Config=Release /p:Platform=Win64 /t:Build /v:minimal
