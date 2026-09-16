@echo off
setlocal enabledelayedexpansion
set EXAMPLES_DIR=%~dp0
set MVCENTGEN=%EXAMPLES_DIR%..\MVCEntGen.exe
set DPROJ=%EXAMPLES_DIR%TestGeneratedUnits.dproj
set BDS=C:\Program Files (x86)\Embarcadero\Studio\37.0
call "%BDS%\bin\rsvars.bat"
set MSBUILD=msbuild
set ERRORS=0

echo ============================================================
echo  DMVCFramework EntGen - Generate all examples and verify
echo ============================================================
echo.

rem --- Generate all examples ---

echo [1/8] Generating ex01_basic...
"%MVCENTGEN%" --config "%EXAMPLES_DIR%ex01_basic.env" --no-color
if errorlevel 1 ( echo   FAILED & set /a ERRORS+=1 ) else echo   OK

echo [2/8] Generating ex02_audit...
"%MVCENTGEN%" --config "%EXAMPLES_DIR%ex02_audit.env" --no-color
if errorlevel 1 ( echo   FAILED & set /a ERRORS+=1 ) else echo   OK

echo [3/8] Generating ex03_softdelete...
"%MVCENTGEN%" --config "%EXAMPLES_DIR%ex03_softdelete.env" --no-color
if errorlevel 1 ( echo   FAILED & set /a ERRORS+=1 ) else echo   OK

echo [4/8] Generating ex04_readonly_refresh...
"%MVCENTGEN%" --config "%EXAMPLES_DIR%ex04_readonly_refresh.env" --no-color
if errorlevel 1 ( echo   FAILED & set /a ERRORS+=1 ) else echo   OK

echo [5/8] Generating ex05_novalidation...
"%MVCENTGEN%" --config "%EXAMPLES_DIR%ex05_novalidation.env" --no-color
if errorlevel 1 ( echo   FAILED & set /a ERRORS+=1 ) else echo   OK

echo [6/8] Generating ex06_abstract...
"%MVCENTGEN%" --config "%EXAMPLES_DIR%ex06_abstract.env" --no-color
if errorlevel 1 ( echo   FAILED & set /a ERRORS+=1 ) else echo   OK

echo [7/8] Generating ex07_filtering...
"%MVCENTGEN%" --config "%EXAMPLES_DIR%ex07_filtering.env" --no-color
if errorlevel 1 ( echo   FAILED & set /a ERRORS+=1 ) else echo   OK

echo [8/8] Generating ex08_fullfeatures...
"%MVCENTGEN%" --config "%EXAMPLES_DIR%ex08_fullfeatures.env" --no-color
if errorlevel 1 ( echo   FAILED & set /a ERRORS+=1 ) else echo   OK

echo.

if !ERRORS! gtr 0 (
  echo GENERATION ERRORS: !ERRORS! example(s^) failed - skipping compilation.
  echo.
  pause
  exit /b 1
)

rem --- Compile the test project ---

echo Compiling TestGeneratedUnits.dproj...
echo.

where msbuild >nul 2>&1
if not errorlevel 1 goto :msbuild_ok
echo ERROR: MSBuild not found on PATH. rsvars.bat may have failed.
echo   BDS=%BDS%
echo.
pause
exit /b 1
:msbuild_ok

rd /s /q "%EXAMPLES_DIR%Win32" 2>nul

"%MSBUILD%" "%DPROJ%" -p:Config=Debug -p:Platform=Win32 -p:BDS="%BDS%" -v:minimal -nologo
if errorlevel 1 (
  echo.
  echo COMPILATION FAILED.
  echo.
  pause
  exit /b 1
)

echo.
echo ============================================================
echo  All examples generated and compiled successfully.
echo ============================================================
echo.
pause
