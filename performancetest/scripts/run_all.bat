@echo off
REM ============================================================================
REM  run_all.bat  -  Full benchmark run across 3 backends x 5 scenarios
REM
REM  Usage:
REM    run_all.bat [SECONDS] [CONNECTIONS] [THREADS]
REM
REM  Prerequisites:
REM    - The 3 server executables built in Release:
REM        ..\bench_server\Win64\Release\PerfBenchServer_WebBroker.exe
REM        ..\bench_server\Win64\Release\PerfBenchServer_IndyDirect.exe
REM        ..\bench_server\Win64\Release\PerfBenchServer_HttpSys.exe
REM    - wrk in PATH (or bombardier as fallback).
REM
REM  The script starts each backend on port 9999, waits, runs all scenarios,
REM  stops the backend, moves on to the next. Results land in ..\results\.
REM ============================================================================

setlocal enabledelayedexpansion

set DURATION=%~1
set CONN=%~2
if "%DURATION%"=="" set DURATION=30
if "%CONN%"=="" set CONN=200

set BINDIR=%~dp0..\bench_server
set RESULTS=%~dp0..\results
if not exist "%RESULTS%" mkdir "%RESULTS%"

set STAMP=%DATE:~-4%%DATE:~3,2%%DATE:~0,2%_%TIME:~0,2%%TIME:~3,2%
set STAMP=%STAMP: =0%
set SUMMARY=%RESULTS%\summary_%STAMP%.md

echo # DMVCFramework Perf Baseline %STAMP% > "%SUMMARY%"
echo. >> "%SUMMARY%"
echo Params: c=%CONN% duration=%DURATION%s >> "%SUMMARY%"
echo. >> "%SUMMARY%"

call :RUN_BACKEND webbroker   PerfBenchServer_WebBroker.exe
call :RUN_BACKEND indydirect  PerfBenchServer_IndyDirect.exe
call :RUN_BACKEND httpsys     PerfBenchServer_HttpSys.exe

echo.
echo === Done. Summary: %SUMMARY% ===
goto :eof

:RUN_BACKEND
set BNAME=%~1
set BEXE=%~2
set BPATH=%BINDIR%\%BEXE%

if not exist "%BPATH%" (
  echo [SKIP] %BNAME%: binary missing at %BPATH%
  goto :eof
)

echo.
echo ############################################################
echo ##  Backend: %BNAME%
echo ############################################################

start "DMVCBench_%BNAME%" /B "%BPATH%"
REM Poll the health endpoint until the server is up (max ~10s).
set /a TRIES=0
:WAIT
set /a TRIES+=1
powershell -NoProfile -Command "try { $r = Invoke-WebRequest -UseBasicParsing -Uri 'http://localhost:9999/bench/health' -TimeoutSec 1 -ErrorAction Stop; exit 0 } catch { exit 1 }" >nul 2>&1
if errorlevel 1 (
  if %TRIES% LSS 20 (
    timeout /t 1 /nobreak >nul
    goto WAIT
  )
  echo [ERROR] %BNAME% did not become reachable.
  taskkill /FI "WINDOWTITLE eq DMVCBench_%BNAME%*" /F >nul 2>&1
  goto :eof
)
echo [OK] %BNAME% reachable after %TRIES% tries.

for %%S in (health json_small json_large heavy upload) do (
  call "%~dp0run_scenario.bat" %BNAME% %%S %DURATION% %CONN%
)

echo. >> "%SUMMARY%"
echo ## %BNAME% >> "%SUMMARY%"
for %%S in (health json_small json_large heavy upload) do (
  echo. >> "%SUMMARY%"
  echo ### %%S >> "%SUMMARY%"
  echo ``` >> "%SUMMARY%"
  if exist "%RESULTS%\%BNAME%_%%S.json" (
    type "%RESULTS%\%BNAME%_%%S.json" >> "%SUMMARY%"
  ) else (
    type "%RESULTS%\%BNAME%_%%S.log" >> "%SUMMARY%" 2>nul
  )
  echo ``` >> "%SUMMARY%"
)

taskkill /FI "WINDOWTITLE eq DMVCBench_%BNAME%*" /F >nul 2>&1
REM Also kill by image name in case window title didn't match.
taskkill /IM %BEXE% /F >nul 2>&1
timeout /t 2 /nobreak >nul
goto :eof
