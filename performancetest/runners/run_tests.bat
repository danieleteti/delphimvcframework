@echo off
REM Crystallised benchmark runner: invoke the compiled test client
REM with any arguments passed on the command line.
REM   run_tests.bat --run:SomeFixture.SomeTest
cd /d "%~dp0..\..\unittests\general\TestClient\bin64"
.\DMVCFrameworkTests.exe %*
