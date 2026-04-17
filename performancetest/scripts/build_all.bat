@echo off
REM ============================================================================
REM  build_all.bat  -  Build all 3 bench server variants.
REM
REM  Usage:
REM    build_all.bat [Platform] [Config]
REM      Platform : Win64 (default) ^| Win32
REM      Config   : Release (default) ^| Debug
REM
REM  Output .exe files land next to their .dproj inside bench_server\.
REM ============================================================================

setlocal

set MY_PLATFORM=%~1
set MY_CONFIG=%~2
if "%MY_PLATFORM%"=="" set MY_PLATFORM=Win64
if "%MY_CONFIG%"=="" set MY_CONFIG=Release

call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
if errorlevel 1 (
  echo [ERROR] rsvars.bat not found or failed.
  echo Edit build_all.bat and point the call to your Delphi install.
  exit /b 1
)

REM rsvars overwrites PLATFORM/CONFIG; restore ours after.
set PLATFORM=%MY_PLATFORM%
set CONFIG=%MY_CONFIG%

pushd "%~dp0..\bench_server"

echo.
echo === Building PerfBenchServer_WebBroker ^| %CONFIG% ^| %PLATFORM% ===
msbuild PerfBenchServer_WebBroker.dproj /p:Config=%CONFIG% /p:Platform=%PLATFORM% /t:Build /v:minimal
if errorlevel 1 goto fail

echo.
echo === Building PerfBenchServer_IndyDirect ^| %CONFIG% ^| %PLATFORM% ===
msbuild PerfBenchServer_IndyDirect.dproj /p:Config=%CONFIG% /p:Platform=%PLATFORM% /t:Build /v:minimal
if errorlevel 1 goto fail

echo.
echo === Building PerfBenchServer_HttpSys ^| %CONFIG% ^| %PLATFORM% ===
msbuild PerfBenchServer_HttpSys.dproj /p:Config=%CONFIG% /p:Platform=%PLATFORM% /t:Build /v:minimal
if errorlevel 1 goto fail

popd
echo.
echo === All 3 variants built in performancetest\bench_server\ ===
exit /b 0

:fail
echo [FAIL]
popd
exit /b 1
