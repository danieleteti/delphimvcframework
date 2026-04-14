@echo off
REM ============================================================================
REM  run_scenario.bat  -  Run one benchmark scenario against a running server.
REM
REM  Usage:
REM    run_scenario.bat BACKEND SCENARIO [SECONDS] [CONNECTIONS]
REM
REM  Example:
REM    run_scenario.bat indydirect json_small 30 200
REM
REM  Driver resolution (first match wins):
REM    1. %~dp0..\tools\oha.exe  (shipped with the repo)
REM    2. oha in PATH
REM    3. wrk in PATH            (Lua-script fallback)
REM
REM  Results land in ..\results\<BACKEND>_<SCENARIO>.json (oha) or .log (wrk).
REM ============================================================================

setlocal enabledelayedexpansion

if "%~1"=="" (
  echo Usage: run_scenario.bat BACKEND SCENARIO [SECONDS] [CONNECTIONS]
  echo   BACKEND    = webbroker ^| indydirect ^| httpsys
  echo   SCENARIO   = health ^| json_small ^| json_large ^| upload ^| heavy
  exit /b 1
)

set BACKEND=%~1
set SCENARIO=%~2
set DURATION=%~3
set CONN=%~4
if "%DURATION%"=="" set DURATION=30
if "%CONN%"=="" set CONN=200

set HOST=http://localhost:9999
set OUTDIR=%~dp0..\results
if not exist "%OUTDIR%" mkdir "%OUTDIR%"

REM --- Resolve scenario to path + method + body -------------------------------
set URL=
set METHOD=GET
set BODYFILE=
if /I "%SCENARIO%"=="health"     set URL=%HOST%/bench/health
if /I "%SCENARIO%"=="json_small" set URL=%HOST%/bench/json/small
if /I "%SCENARIO%"=="json_large" set URL=%HOST%/bench/json/large
if /I "%SCENARIO%"=="heavy"      set URL=%HOST%/bench/heavy
if /I "%SCENARIO%"=="upload" (
  set URL=%HOST%/bench/upload
  set METHOD=POST
  set BODYFILE=%~dp0upload_body_1mb.bin
  if not exist "!BODYFILE!" (
    echo [setup] Generating 1 MB upload body at !BODYFILE! ...
    powershell -NoProfile -Command "[System.IO.File]::WriteAllBytes('!BODYFILE!', (New-Object byte[] 1048576))"
  )
)

if "%URL%"=="" (
  echo [ERROR] Unknown scenario '%SCENARIO%'.
  exit /b 2
)

REM --- Resolve oha: local binary wins over PATH ------------------------------
set OHA=%~dp0..\tools\oha.exe
if not exist "%OHA%" (
  where oha >nul 2>nul
  if %ERRORLEVEL% EQU 0 (
    for /f "delims=" %%O in ('where oha') do set OHA=%%O
  ) else (
    set OHA=
  )
)

echo.
echo === %BACKEND% / %SCENARIO%  (t=%DURATION%s c=%CONN%) ===

REM --- Primary: oha -----------------------------------------------------------
if not "%OHA%"=="" (
  set OUT=%OUTDIR%\%BACKEND%_%SCENARIO%.json
  echo Driver: %OHA%
  echo Output: !OUT!
  if /I "%METHOD%"=="POST" (
    "%OHA%" -z %DURATION%s -c %CONN% --no-tui --output-format json ^
        -m POST -T application/octet-stream ^
        -D "%BODYFILE%" ^
        %URL% > "!OUT!"
  ) else (
    "%OHA%" -z %DURATION%s -c %CONN% --no-tui --output-format json %URL% > "!OUT!"
  )
  goto :eof
)

REM --- Fallback: wrk ----------------------------------------------------------
where wrk >nul 2>nul
if %ERRORLEVEL% EQU 0 (
  set SCRIPT=%~dp0bench_%SCENARIO%.lua
  set OUT=%OUTDIR%\%BACKEND%_%SCENARIO%.log
  echo Driver: wrk
  echo Output: !OUT!
  wrk -t8 -c%CONN% -d%DURATION%s -s "!SCRIPT!" --latency %HOST% > "!OUT!" 2>&1
  type "!OUT!"
  goto :eof
)

echo [ERROR] No load driver found.
echo   Expected: %~dp0..\tools\oha.exe  (ships with the repo)
echo   Or install: winget install hatoo.oha  ^|  cargo install oha
echo   Or: wrk via WSL (sudo apt install wrk)
exit /b 3
