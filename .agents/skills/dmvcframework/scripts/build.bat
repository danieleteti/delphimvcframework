@echo off
setlocal EnableDelayedExpansion

:: ============================================================
:: DMVCFramework — Delphi Build Script
::
:: Usage:
::   build.bat <project.dproj>
::   build.bat <project.dproj> [Config]    (Debug* | Release)
::   build.bat <project.dproj> [Config] [Platform]  (Win32* | Win64)
::   build.bat <project.dproj> [Config] [Platform] [bds-version]
::   build.bat <project.dproj> list        (show installed versions)
::
:: Version selection:
::   1. Explicit bds-version parameter wins (and saves preference)
::   2. .delphi-version file in the project directory
::   3. Single installed version: used automatically
::   4. Multiple installed versions: prints list and exits with code 2
::      — Claude asks the user which version to use, then re-runs with it
::
:: The chosen version is saved in <project-dir>\.delphi-version so it
:: is remembered for every subsequent build without asking again.
:: ============================================================

set "PROJ=%~1"
set "CFG=%~2"
set "PLT=%~3"
set "VER=%~4"

if "!PROJ!"=="" (
    echo Usage: build.bat ^<project.dproj^> [Debug^|Release] [Win32^|Win64] [bds-version^|list]
    exit /b 1
)
if not exist "!PROJ!" (
    echo ERROR: Project file not found: !PROJ!
    exit /b 1
)

if "!CFG!"=="" set "CFG=Debug"
if "!PLT!"=="" set "PLT=Win32"

:: .delphi-version lives next to the .dproj
set "PROJ_DIR=%~dp1"
if "!PROJ_DIR:~-1!"=="\" set "PROJ_DIR=!PROJ_DIR:~0,-1!"
set "VER_FILE=!PROJ_DIR!\.delphi-version"

:: ---- Known Delphi / RAD Studio BDS versions ----
set /A I=0
set /A I+=1 & set "V!I!.BDS=37.0" & set "V!I!.NAME=Delphi 13 Florence"
set /A I+=1 & set "V!I!.BDS=23.0" & set "V!I!.NAME=Delphi 12 Athens"
set /A I+=1 & set "V!I!.BDS=22.0" & set "V!I!.NAME=Delphi 11 Alexandria"
set /A I+=1 & set "V!I!.BDS=21.0" & set "V!I!.NAME=Delphi 10.4 Sydney"
set /A I+=1 & set "V!I!.BDS=20.0" & set "V!I!.NAME=Delphi 10.3 Rio"
set /A I+=1 & set "V!I!.BDS=19.0" & set "V!I!.NAME=Delphi 10.2 Tokyo"
set /A I+=1 & set "V!I!.BDS=18.0" & set "V!I!.NAME=Delphi 10.1 Berlin"
set /A I+=1 & set "V!I!.BDS=17.0" & set "V!I!.NAME=Delphi 10 Seattle"
set "KNOWN=!I!"

:: ---- Scan for installed versions ----
set "FOUND=0"
for /L %%K in (1,1,!KNOWN!) do (
    set "BDS=!V%%K.BDS!"
    for %%D in (
        "C:\Program Files (x86)\Embarcadero\Studio\!BDS!\bin\rsvars.bat"
        "C:\Program Files\Embarcadero\Studio\!BDS!\bin\rsvars.bat"
    ) do (
        if exist %%~D (
            set /A FOUND+=1
            set "F!FOUND!.BDS=!V%%K.BDS!"
            set "F!FOUND!.NAME=!V%%K.NAME!"
            set "F!FOUND!.PATH=%%~D"
        )
    )
)

if !FOUND! EQU 0 (
    echo ERROR: No Delphi installation found in standard paths.
    echo Set the BDS environment variable and invoke MSBuild directly.
    exit /b 1
)

:: ---- 'list' command ----
if /I "!CFG!"=="list" (
    echo Installed Delphi versions:
    for /L %%I in (1,1,!FOUND!) do (
        echo   !F%%I.BDS!  !F%%I.NAME!
    )
    exit /b 0
)

:: ---- Resolve which version to use ----
set "RSVARS="
set "CHOSEN_BDS="
set "CHOSEN_NAME="

:: Priority 1: explicit version from command line
if not "!VER!"=="" (
    for /L %%I in (1,1,!FOUND!) do (
        if "!F%%I.BDS!"=="!VER!" (
            set "RSVARS=!F%%I.PATH!"
            set "CHOSEN_BDS=!VER!"
            set "CHOSEN_NAME=!F%%I.NAME!"
        )
    )
    if "!RSVARS!"=="" (
        echo ERROR: Delphi !VER! not installed. Available:
        for /L %%I in (1,1,!FOUND!) do echo   !F%%I.BDS!  !F%%I.NAME!
        exit /b 1
    )
    echo !CHOSEN_BDS!>"!VER_FILE!"
    goto :build
)

:: Priority 2: saved preference
if exist "!VER_FILE!" (
    set /p SAVED=<"!VER_FILE!"
    set "SAVED=!SAVED: =!"
    if not "!SAVED!"=="" (
        for /L %%I in (1,1,!FOUND!) do (
            if "!F%%I.BDS!"=="!SAVED!" (
                set "RSVARS=!F%%I.PATH!"
                set "CHOSEN_BDS=!F%%I.BDS!"
                set "CHOSEN_NAME=!F%%I.NAME!"
            )
        )
        if not "!RSVARS!"=="" (
            echo Using saved version: !CHOSEN_NAME! ^(!CHOSEN_BDS!^)
            goto :build
        )
        echo WARNING: Saved version !SAVED! no longer found. Rescanning...
    )
)

:: Priority 3: single version installed
if !FOUND! EQU 1 (
    set "RSVARS=!F1.PATH!"
    set "CHOSEN_BDS=!F1.BDS!"
    set "CHOSEN_NAME=!F1.NAME!"
    echo !CHOSEN_BDS!>"!VER_FILE!"
    echo Using: !CHOSEN_NAME!
    goto :build
)

:: Priority 4: multiple versions — Claude/user must choose
echo MULTIPLE_DELPHI_VERSIONS_FOUND
echo.
echo Multiple Delphi installations found. Choose one:
echo.
for /L %%I in (1,1,!FOUND!) do (
    echo   %%I.  !F%%I.NAME!  ^(BDS !F%%I.BDS!^)
)
echo.
echo Re-run passing the BDS version number as 4th argument:
echo   build.bat "!PROJ!" !CFG! !PLT! ^<bds-version^>
echo.
echo Example:
echo   build.bat "!PROJ!" !CFG! !PLT! !F1.BDS!
exit /b 2

:build
echo.
echo Project:  !PROJ!
echo Config:   !CFG!  /  Platform: !PLT!
echo Delphi:   !CHOSEN_NAME! ^(!CHOSEN_BDS!^)
echo.

call "!RSVARS!" >nul 2>&1
msbuild "!PROJ!" /t:Build /p:Config=!CFG! /p:Platform=!PLT! /nologo /verbosity:minimal

if !ERRORLEVEL! NEQ 0 (
    echo.
    echo BUILD FAILED
    exit /b !ERRORLEVEL!
)

echo.
echo BUILD OK
exit /b 0
