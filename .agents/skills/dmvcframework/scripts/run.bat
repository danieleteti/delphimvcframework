@echo off
setlocal EnableDelayedExpansion

:: ============================================================
:: DMVCFramework — Run Server Script
::
:: Starts the compiled DMVCFramework server executable.
:: The executable is looked for in the wizard's output folder first (it sets the
:: output to .\bin, where bin\.env lives too), then at the MSBuild default:
::   <project-dir>\bin\<ProjectName>.exe
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

set "EXE=!PROJ_DIR!\bin\!PROJ_NAME!.exe"
if not exist "!EXE!" set "EXE=!PROJ_DIR!\!PLT!\!CFG!\!PROJ_NAME!.exe"

if not exist "!EXE!" (
    echo ERROR: Executable not found in !PROJ_DIR!\bin nor at !EXE!
    echo Run build.bat first:
    echo   build.bat "!PROJ!" !CFG! !PLT!
    exit /b 1
)

echo Starting: !EXE!
echo Press Ctrl+C to stop.
echo.
for %%E in ("!EXE!") do pushd "%%~dpE"
"!EXE!"
popd
