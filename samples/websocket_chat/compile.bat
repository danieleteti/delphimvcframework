@echo off
echo Compiling WebSocket Chat Server...

set DELPHI_BIN=C:\Program Files (x86)\Embarcadero\Studio\37.0\bin
set PROJECT_ROOT=..\..
set SOURCES=%PROJECT_ROOT%\sources
set LIBS=%PROJECT_ROOT%\lib

"%DELPHI_BIN%\dcc32.exe" ^
  -B ^
  -NSSystem;Vcl;Data;Web;Winapi ^
  -U"%SOURCES%;%LIBS%\loggerpro;%LIBS%\dmustache;%LIBS%\swagdoc\Source" ^
  -I"%SOURCES%" ^
  -R"%SOURCES%" ^
  -E".\bin" ^
  -N".\dcu" ^
  WebSocketChatServer.dpr

if %ERRORLEVEL% == 0 (
    echo.
    echo Compilation successful!
    echo Executable: bin\WebSocketChatServer.exe
) else (
    echo.
    echo Compilation failed with error code: %ERRORLEVEL%
)

pause
