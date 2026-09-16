@echo off
REM Crystallised benchmark runner: execute only the Indy Direct
REM single-flush test suite. Used to verify the single-flush response
REM feature under stable load during the 3.5.0-silicon performance work.
cd /d "%~dp0..\..\unittests\general\TestClient\bin64"
.\DMVCFrameworkTests.exe --run:IndyDirectSingleFlushTestU.TIndyDirectSingleFlushTests
