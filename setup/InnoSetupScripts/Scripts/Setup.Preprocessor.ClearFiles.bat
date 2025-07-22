@echo off

rem #
rem # Copyright (c) 2024-2025 (Ethea S.r.l.)
rem #
rem # Original code is Copyright (c) 2021-2024 Skia4Delphi Project.
rem #
rem # Use of this source code is governed by the MIT license that can be
rem # found in the LICENSE file.
rem #

echo Cleaning Library Path (Lib)...
for /f "tokens=* delims=" %%i in ('dir "..\..\..\Lib\" /s /b /a:-d ^| find /v "\Win32\Release\" ^| find /v "\Win64\Release\"') do (
  echo Deleting "%%i"
  del "%%i"
)
