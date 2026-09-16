@echo off
REM ============================================================================
REM  run_stable.bat  -  Run one scenario N times for statistical stability.
REM
REM  Usage:
REM    run_stable.bat BACKEND SCENARIO [SECONDS] [CONNECTIONS] [REPEATS]
REM
REM  Writes ..\results\<BACKEND>_<SCENARIO>_r1.json, _r2.json, ...
REM  parse_results.py auto-aggregates them via median.
REM
REM  The bench machine shows ~3x per-run variance on short samples, so
REM  single-point measurements mislead. Use this to compare optimizations.
REM ============================================================================

setlocal enabledelayedexpansion

if "%~2"=="" (
  echo Usage: run_stable.bat BACKEND SCENARIO [SECONDS] [CONNECTIONS] [REPEATS]
  exit /b 1
)

set BACKEND=%~1
set SCENARIO=%~2
set DURATION=%~3
set CONN=%~4
set REPEATS=%~5
if "%DURATION%"=="" set DURATION=30
if "%CONN%"=="" set CONN=100
if "%REPEATS%"=="" set REPEATS=3

set OUTDIR=%~dp0..\results
if not exist "%OUTDIR%" mkdir "%OUTDIR%"

REM Wipe old run files for this backend+scenario so old data doesn't skew.
del /q "%OUTDIR%\%BACKEND%_%SCENARIO%_r*.json" 2>nul
del /q "%OUTDIR%\%BACKEND%_%SCENARIO%.json" 2>nul

for /L %%I in (1,1,%REPEATS%) do (
  echo.
  echo === Repeat %%I/%REPEATS% ===
  call "%~dp0run_scenario.bat" %BACKEND% %SCENARIO% %DURATION% %CONN%
  if exist "%OUTDIR%\%BACKEND%_%SCENARIO%.json" (
    move /y "%OUTDIR%\%BACKEND%_%SCENARIO%.json" "%OUTDIR%\%BACKEND%_%SCENARIO%_r%%I.json" >nul
  )
)

echo.
echo === Done. %REPEATS% samples at %OUTDIR%\%BACKEND%_%SCENARIO%_r*.json ===
