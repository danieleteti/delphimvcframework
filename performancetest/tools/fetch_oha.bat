@echo off
REM Download oha (https://github.com/hatoo/oha) Windows binary into this folder.
REM Run this once after cloning; run_scenario.bat picks it up automatically.

set OHA_URL=https://github.com/hatoo/oha/releases/download/v1.14.0/oha-windows-amd64-pgo.exe
set OHA_DEST=%~dp0oha.exe

if exist "%OHA_DEST%" (
  echo [skip] already present: %OHA_DEST%
  "%OHA_DEST%" --version
  goto :eof
)

echo Downloading oha from %OHA_URL% ...
powershell -NoProfile -Command "Invoke-WebRequest -UseBasicParsing -Uri '%OHA_URL%' -OutFile '%OHA_DEST%'"
if errorlevel 1 (
  echo [error] download failed.
  exit /b 1
)
echo OK: %OHA_DEST%
"%OHA_DEST%" --version
