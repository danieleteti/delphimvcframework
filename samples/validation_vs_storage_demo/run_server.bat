@echo off
rem Start the demo server (Indy Direct, port 8989).
rem Press CTRL+C to stop.

cd /d "%~dp0"

if not exist ValidationVsStorageDemo.exe (
  echo ValidationVsStorageDemo.exe not found - build the project first.
  pause
  exit /b 1
)

ValidationVsStorageDemo.exe
