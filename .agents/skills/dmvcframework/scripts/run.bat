@echo off
setlocal EnableDelayedExpansion

:: ============================================================
:: DMVCFramework — Run Server Script
::
:: Starts the compiled DMVCFramework server executable.
:: The executable is expected at the standard MSBuild output path:
::   <project-dir>\<Platform>\<Config>\<ProjectName>.exe
::
:: Usage:
::   run.bat <project.dproj>
::   run.bat <project.dproj> [Config]    (Debug* | Release)
::   run.bat <project.dproj> [Config] [Platform]  (Win32* | Win64)
:: ============================================================

set "PROJ=%~1"
set "CFG=%~2"
set "PLT=%~3"

if "!PROJ!"=="" (
    echo Usage: run.bat ^<project.dproj^> [Debug^|Release] [Win32^|Win64]
    exit /b 1
)
if not exist "!PROJ!" (
    echo ERROR: Project file not found: !PROJ!
    exit /b 1
)

if "!CFG!"=="" set "CFG=Debug"
if "!PLT!"=="" set "PLT=Win32"

set "PROJ_DIR=%~dp1"
if "!PROJ_DIR:~-1!"=="\" set "PROJ_DIR=!PROJ_DIR:~0,-1!"
set "PROJ_NAME=%~n1"

set "EXE=!PROJ_DIR!\!PLT!\!CFG!\!PROJ_NAME!.exe"

if not exist "!EXE!" (
    echo ERROR: Executable not found: !EXE!
    echo Run build.bat first:
    echo   build.bat "!PROJ!" !CFG! !PLT!
    exit /b 1
)

echo Starting: !EXE!
echo Press Ctrl+C to stop.
echo.
"!EXE!"
