@echo off
set BDS=C:\Program Files (x86)\Embarcadero\Studio\37.0
set PATH=%BDS%\bin;%PATH%
set LIB=%BDS%\lib\Win32\release;%LIB%
dcc32 -B -U"..\..\sources;..\..\lib\loggerpro;..\..\lib\dmustache;..\..\lib\swagdoc\Source;%BDS%\lib\Win32\release;%BDS%\lib\Win32\release\RTL" WebSocketChatServer.dpr
