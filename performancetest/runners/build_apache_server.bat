@echo off
REM Crystallised benchmark runner: build the Apache ISAPI test SERVER
REM (one of the three backends compared for ITDEVCON 2026). Builds in
REM CI config, strips stale output, then drops the module into the
REM bundled Apache24/modules as mod_dmvctest.dll so Apache can load it
REM without a restart.
setlocal
set REPO_ROOT=%~dp0..\..
cd /d "%REPO_ROOT%\unittests\general\TestServer"
del /q bin\TestServerApache.* 2>nul
del /q Win64\CI\TestServerApache.* 2>nul
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
msbuild /t:Rebuild /p:Config=CI /p:Platform=Win64 TestServerApache.dproj /v:minimal >nul
copy /y bin\TestServerApache.dll "%REPO_ROOT%\unittests\apache\Apache24\modules\mod_dmvctest.dll" >nul
echo built
endlocal
