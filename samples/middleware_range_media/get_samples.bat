@echo off
rem ---------------------------------------------------------------------------
rem  get_samples.bat
rem  Downloads royalty-free sample media files for the Range Media Middleware demo.
rem
rem  Output:
rem    media\sample.ogg  - Beethoven: Moonlight Sonata (Wikimedia Commons, Public Domain)
rem    media\sample.mp4  - Big Buck Bunny 320x180, CC BY 3.0, (c) Blender Foundation
rem
rem  Requirements: curl.exe (built into Windows 10 / 11)
rem ---------------------------------------------------------------------------

where curl >nul 2>&1
if errorlevel 1 (
  echo ERROR: curl not found.
  echo curl.exe is built into Windows 10 and later.
  echo If you are on Windows 8, install curl from https://curl.se/windows/
  exit /b 1
)

if not exist media mkdir media

echo Downloading media\sample.ogg ...
echo   Beethoven: Moonlight Sonata - Wikimedia Commons ^(Public Domain^)
curl -L --progress-bar -o media\sample.ogg ^
  "https://upload.wikimedia.org/wikipedia/commons/9/96/Beethoven_Moonlight_sonata_sequenced.ogg"
if errorlevel 1 goto :fail

echo.
echo Downloading media\sample.mp4 ...
echo   Big Buck Bunny 320x180, CC BY 3.0, ^(c^) Blender Foundation
curl -L --progress-bar -o media\sample.mp4 ^
  "https://download.blender.org/peach/bigbuckbunny_movies/BigBuckBunny_320x180.mp4"
if errorlevel 1 goto :fail

echo.
echo Done.  Files saved in the media\ folder.
echo.
echo  Attribution:
echo   audio: Beethoven - Moonlight Sonata, Wikimedia Commons (Public Domain)
echo          https://commons.wikimedia.org/wiki/File:Beethoven_Moonlight_Sonata.ogg
echo   video: Big Buck Bunny, CC BY 3.0, (c) copyright 2008 Blender Foundation
echo          https://www.bigbuckbunny.org
echo.
echo Open http://localhost:8080 after starting the server.
exit /b 0

:fail
echo.
echo ERROR: download failed.  Check your internet connection and try again.
exit /b 1
