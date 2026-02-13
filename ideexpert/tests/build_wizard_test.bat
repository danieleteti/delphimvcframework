@echo off
echo ========================================
echo Building DMVC Wizard Standalone Test
echo ========================================
echo.

REM Find Delphi installation (Delphi 13 Florence)
set DELPHI_PATH=C:\Program Files (x86)\Embarcadero\Studio\37.0
if not exist "%DELPHI_PATH%\bin\rsvars.bat" (
    echo ERROR: Delphi 13 not found at %DELPHI_PATH%
    echo Please edit this batch file and set the correct DELPHI_PATH
    pause
    exit /b 1
)

REM Setup Delphi environment
call "%DELPHI_PATH%\bin\rsvars.bat"

REM Compile resources first
echo.
echo Compiling template resources...
cd ..
brcc32 DMVC.Expert.Templates.rc
if errorlevel 1 (
    echo ERROR: Failed to compile resources
    pause
    exit /b 1
)

REM Build the project
cd tests
echo.
echo Building WizardTestStandalone.dpr...
msbuild WizardTestStandalone.dpr /p:Config=Debug /p:Platform=Win32 /t:Build /v:minimal

if errorlevel 1 (
    echo.
    echo ERROR: Build failed!
    pause
    exit /b 1
)

echo.
echo ========================================
echo Build completed successfully!
echo ========================================
echo.
echo Executable: Win32\Debug\WizardTestStandalone.exe
echo.
pause
