@echo off
REM ============================================================================
REM  run_all_stable.bat  -  Full re-baseline harness.
REM
REM  Usage:
REM    run_all_stable.bat [SECONDS] [CONNECTIONS] [REPEATS]
REM
REM  Runs 3 backends x 7 scenarios x N repeats. With defaults (30s, 100, 3)
REM  the full run takes ~33 minutes. WebBroker is flaky at c=100 on this
REM  machine (see BASELINE.md); data is still collected but some scenarios
REM  may show partial successRate.
REM
REM  Raw per-run JSONs go to ..\results\<backend>_<scenario>_r<N>.json and
REM  parse_results.py aggregates them via median.
REM ============================================================================

setlocal enabledelayedexpansion

set DURATION=%~1
set CONN=%~2
set REPEATS=%~3
if "%DURATION%"=="" set DURATION=30
if "%CONN%"=="" set CONN=100
if "%REPEATS%"=="" set REPEATS=3

set BINDIR=%~dp0..\bench_server
set RESULTS=%~dp0..\results
if not exist "%RESULTS%" mkdir "%RESULTS%"

REM Wipe prior run samples so the median is clean.
del /q "%RESULTS%\*_r*.json" 2>nul
del /q "%RESULTS%\summary*.md" 2>nul
del /q "%RESULTS%\summary.csv" 2>nul

call :RUN_BACKEND webbroker   PerfBenchServer_WebBroker.exe
call :RUN_BACKEND indydirect  PerfBenchServer_IndyDirect.exe
call :RUN_BACKEND httpsys     PerfBenchServer_HttpSys.exe

echo.
echo === Done. Parse: python scripts\parse_results.py ===
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

REM Poll /bench/health via PowerShell (timeout tool on this shell is GNU, not cmd).
set /a TRIES=0
:WAIT
set /a TRIES+=1
powershell -NoProfile -Command "try { $r = Invoke-WebRequest -UseBasicParsing -Uri 'http://localhost:9999/bench/health' -TimeoutSec 1 -ErrorAction Stop; exit 0 } catch { exit 1 }" >nul 2>&1
if errorlevel 1 (
  if %TRIES% LSS 20 (
    powershell -NoProfile -Command "Start-Sleep -Milliseconds 500" >nul
    goto WAIT
  )
  echo [ERROR] %BNAME% did not become reachable.
  taskkill /IM %BEXE% /F >nul 2>&1
  goto :eof
)
echo [OK] %BNAME% reachable after %TRIES% tries.

for %%S in (health json_small json_large heavy upload pods_small pods_large) do (
  call "%~dp0run_stable.bat" %BNAME% %%S %DURATION% %CONN% %REPEATS%
)

taskkill /IM %BEXE% /F >nul 2>&1
powershell -NoProfile -Command "Start-Sleep -Seconds 2" >nul
goto :eof
