@echo off
setlocal EnableDelayedExpansion

:: ============================================================
:: DMVCFramework — Run DUnitX Test Suite
::
:: Builds the test project (always Debug) then runs the DUnitX
:: console runner. Outputs a JUnit-compatible XML report to
:: <project-dir>\TestResults\results.xml for CI integration.
::
:: Usage:
::   run-tests.bat <test-project.dproj>
::   run-tests.bat <test-project.dproj> [Platform]  (Win32* | Win64)
::   run-tests.bat <test-project.dproj> [Platform] [bds-version]
::
:: The Delphi version is resolved the same way as build.bat:
::   saved .delphi-version file > single install > ask user (exit 2)
:: ============================================================

set "PROJ=%~1"
set "PLT=%~2"
set "VER=%~3"

if "!PROJ!"=="" (
    echo Usage: run-tests.bat ^<test-project.dproj^> [Win32^|Win64] [bds-version]
    exit /b 1
)
if not exist "!PROJ!" (
    echo ERROR: Project file not found: !PROJ!
    exit /b 1
)

if "!PLT!"=="" set "PLT=Win32"

set "PROJ_DIR=%~dp1"
if "!PROJ_DIR:~-1!"=="\" set "PROJ_DIR=!PROJ_DIR:~0,-1!"
set "PROJ_NAME=%~n1"
set "EXE=!PROJ_DIR!\!PLT!\Debug\!PROJ_NAME!.exe"
set "RESULTS_DIR=!PROJ_DIR!\TestResults"

:: ---- Step 1: Build ----
echo [1/2] Building test project...
echo.

set "BUILD_SCRIPT=%~dp0..\..\dmvcframework\scripts\build.bat"
if not exist "!BUILD_SCRIPT!" (
    :: Fallback: look for build.bat relative to Claude skills install
    set "BUILD_SCRIPT=%USERPROFILE%\.claude\skills\dmvcframework\scripts\build.bat"
)
if not exist "!BUILD_SCRIPT!" (
    echo ERROR: build.bat not found. Install the dmvcframework skill first.
    exit /b 1
)

if "!VER!"=="" (
    call "!BUILD_SCRIPT!" "!PROJ!" Debug !PLT!
) else (
    call "!BUILD_SCRIPT!" "!PROJ!" Debug !PLT! !VER!
)

if !ERRORLEVEL! EQU 2 (
    :: build.bat found multiple versions and needs user input — propagate
    exit /b 2
)
if !ERRORLEVEL! NEQ 0 (
    echo.
    echo BUILD FAILED — tests not run.
    exit /b !ERRORLEVEL!
)

if not exist "!EXE!" (
    echo ERROR: Executable not found after build: !EXE!
    exit /b 1
)

:: ---- Step 2: Run tests ----
if not exist "!RESULTS_DIR!" mkdir "!RESULTS_DIR!"

echo.
echo [2/2] Running tests...
echo.

"!EXE!" --format:progress --format:xml --output:"!RESULTS_DIR!\results.xml"
set "TEST_EXIT=!ERRORLEVEL!"

echo.
if !TEST_EXIT! EQU 0 (
    echo ALL TESTS PASSED
) else (
    echo TESTS FAILED — see !RESULTS_DIR!\results.xml
)

exit /b !TEST_EXIT!
