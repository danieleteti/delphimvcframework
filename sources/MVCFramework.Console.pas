// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// *************************************************************************** }

unit MVCFramework.Console;

interface

uses
  System.SysUtils
{$IFDEF MSWINDOWS}
  ,WinApi.Windows
{$ENDIF}
{$IFDEF LINUX}
{$ENDIF}
    ;

type
  // https://stackoverflow.com/questions/17125440/c-win32-console-color
  // https://docs.microsoft.com/en-us/dotnet/api/system.consolecolor?view=netcore-3.1
  TConsoleColor = (Black = 0, // The color black.
    DarkBlue = 1, // The color dark blue.
    DarkGreen = 2, // The color dark green.
    DarkCyan = 3, // The color dark cyan (dark blue-green).
    DarkRed = 4, // The color dark red.
    DarkMagenta = 5, // The color dark magenta (dark purplish-red).
    DarkYellow = 6, // The color dark yellow (ochre).
    Gray = 7, // The color gray.
    DarkGray = 8, // The color dark gray.
    Blue = 9, // The color blue.
    Green = 10, // The color green.
    Cyan = 11, // The color Cyan(Blue - green).
    Red = 12, // The color red.
    Magenta = 13, // The color magenta (purplish-red).
    Yellow = 14, // The color yellow.
    White = 15 // The color white.
    );

  EMVCConsole = class(Exception)

  end;

  TMVCConsoleSize = record
    Columns: Word;
    Rows: Word;
  end;

procedure ResetConsole;
procedure TextColor(const color: TConsoleColor);
procedure TextBackground(const color: TConsoleColor);
procedure GotoXY(const X, Y: Byte);
function GetConsoleSize: TMVCConsoleSize;
function GetConsoleBufferSize: TMVCConsoleSize;
procedure ClrScr;
function GetCh: Char;
procedure WaitForReturn;
procedure SaveColors;
procedure RestoreSavedColors;
procedure Init;
procedure SetDefaults;
function ConsoleAttr: Integer;
procedure SetConsoleAttr(const TextAttr: Integer);
function TextAttr: Word;
procedure SetTextAttr(const TextAttr: Word);
function BackgroundAttr: Word;
procedure SetBackgroundAttr(const BackgroundAttr: Word);


function ColorName(const color: TConsoleColor): String;

implementation

uses
  System.TypInfo;

const
  ESC = Chr(27);

var
  GForeGround, GSavedForeGround: Int16;
  GBackGround, GSavedBackGround: Int16;
  GOutHandle: THandle = INVALID_HANDLE_VALUE;
  GInputHandle: THandle = INVALID_HANDLE_VALUE;

function ColorName(const color: TConsoleColor): String;
begin
  Result := GetEnumName(TypeInfo(TConsoleColor), Ord(color));
end;


{$IFDEF LINUX}
procedure WaitForReturn;
begin
  ReadLn;
end;

procedure Init; inline;
begin

end;

procedure UpdateMode;
begin

end;

function GetCh: Char;
begin
  raise EMVCConsole.Create('Not Implemented');
end;

procedure GotoXY(const X, Y: Byte);
begin
  raise EMVCConsole.Create('Not Implemented');
end;

function GetConsoleSize: TMVCConsoleSize;
begin
  raise EMVCConsole.Create('Not Implemented');
end;

function GetConsoleBufferSize: TMVCConsoleSize;
begin
  raise EMVCConsole.Create('Not Implemented');
end;

procedure ClrScr;
begin
  raise EMVCConsole.Create('Not Implemented');
end;

{$ENDIF}
{$IFDEF MSWINDOWS}

procedure WinCheck(const Value: LongBool);
begin
  if not Value then
    raise EMVCConsole.CreateFmt('GetLastError() = %d', [GetLastError]);
end;

procedure WaitForReturn;
begin
  while GetCh <> #13 do;
end;

procedure ClrScr;
var
  lSize: TMVCConsoleSize;
  dwConSize: UInt32;
  lStartCoord: _COORD;
  lCharsWritten: UInt32;
  lConsoleScreenBufferInfo: _CONSOLE_SCREEN_BUFFER_INFO;
begin
  // https://docs.microsoft.com/en-us/windows/console/clearing-the-screen
  lSize := GetConsoleBufferSize;
  dwConSize := lSize.Columns * lSize.Rows;
  lStartCoord.X := 0;
  lStartCoord.Y := 0;
  if not FillConsoleOutputCharacter(GOutHandle, ' ', dwConSize, lStartCoord, lCharsWritten) then
    raise EMVCConsole.CreateFmt('Cannot fill console with blank char - GetLastError() = %d', [GetLastError]);

  if not GetConsoleScreenBufferInfo(GOutHandle, lConsoleScreenBufferInfo) then
    raise EMVCConsole.CreateFmt('Cannot GetConsoleScreenBufferInfo - GetLastError() = %d', [GetLastError]);

  if not FillConsoleOutputAttribute(GOutHandle, lConsoleScreenBufferInfo.wAttributes, dwConSize, lStartCoord,
    lCharsWritten) then
    raise EMVCConsole.CreateFmt('Cannot FillConsoleOutputAttribute - GetLastError() = %d', [GetLastError]);

  GotoXY(0, 0);
end;

function GetConsoleSize: TMVCConsoleSize;
var
  lConsoleScreenBufferInfo: _CONSOLE_SCREEN_BUFFER_INFO;
begin
  if not GetConsoleScreenBufferInfo(GOutHandle, lConsoleScreenBufferInfo) then
    raise EMVCConsole.CreateFmt('Cannot Get Console Size - GetLastError() = %d', [GetLastError]);
  Result.Columns := lConsoleScreenBufferInfo.srWindow.Right - lConsoleScreenBufferInfo.srWindow.Left + 1;
  Result.Rows := lConsoleScreenBufferInfo.srWindow.Bottom - lConsoleScreenBufferInfo.srWindow.Top + 1;
end;

procedure EnsureStdInput;
begin
  if GInputHandle = INVALID_HANDLE_VALUE then
  begin
    GInputHandle := GetStdHandle(STD_INPUT_HANDLE);
    if GInputHandle = INVALID_HANDLE_VALUE then
    begin
      raise EMVCConsole.CreateFmt('Cannot Get STD_INPUT_HANDLE - GetLastError() = %d', [GetLastError]);
    end;
  end;
end;

function GetCh: Char;
var
  lMode, lCC: DWORD;
  C: Char;
begin
  EnsureStdInput;
  C := #0;
  WinCheck(GetConsoleMode(GInputHandle, lMode));
  WinCheck(SetConsoleMode(GInputHandle, lMode and (not(ENABLE_LINE_INPUT or ENABLE_ECHO_INPUT))));
  try
    lCC := 0;
    WinCheck(ReadConsole(GInputHandle, @C, SizeOf(Char), lCC, nil));
  finally
    WinCheck(SetConsoleMode(GInputHandle, lMode));
  end;
  Result := C;
end;

function GetConsoleBufferSize: TMVCConsoleSize;
var
  lConsoleScreenBufferInfo: _CONSOLE_SCREEN_BUFFER_INFO;
begin
  if not GetConsoleScreenBufferInfo(GOutHandle, lConsoleScreenBufferInfo) then
    raise EMVCConsole.CreateFmt('Cannot Get Console Buffer Size - GetLastError() = %d', [GetLastError]);
  Result.Columns := lConsoleScreenBufferInfo.dwSize.X;
  Result.Rows := lConsoleScreenBufferInfo.dwSize.Y;
end;

procedure Init;
begin
  GOutHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  if GOutHandle = INVALID_HANDLE_VALUE then
    raise EMVCConsole.CreateFmt('Cannot Get STD_OUTPUT_HANDLE - GetLastError() = %d', [GetLastError]);
end;

procedure UpdateMode;
begin
  SetConsoleTextAttribute(GOutHandle, Ord(GForeGround) or Ord(GBackGround));
end;

procedure GotoXY(const X, Y: Byte);
var
  lCoord: _COORD;
begin
  lCoord.X := X;
  lCoord.Y := Y;
  if not SetConsoleCursorPosition(GOutHandle, lCoord) then
  begin
    raise EMVCConsole.Create('Invalid Coordinates');
  end;
end;

{$ENDIF}

procedure ResetConsole;
begin
  // write(ESC + '[0m');
  GForeGround := Ord(TConsoleColor.DarkGray);
  GBackGround := Ord(TConsoleColor.Black);
  UpdateMode;
end;

procedure TextColor(const color: TConsoleColor);
begin
  GForeGround := Ord(color);
  UpdateMode;
  // write(ESC + GetColorString);
end;

procedure TextBackground(const color: TConsoleColor);
begin
  GBackGround := Ord(color) shl 4;
  UpdateMode;
  // write(ESC + GetColorString);
end;

procedure SetDefaults;
begin
  GForeGround := Ord(TConsoleColor.White);
  GBackGround := Ord(TConsoleColor.Black);
  UpdateMode;
end;

procedure SaveColors;
begin
  GSavedForeGround := GForeGround;
  GSavedBackGround := GBackGround;
end;

procedure RestoreSavedColors;
begin
  GForeGround := GSavedForeGround;
  GBackGround := GSavedBackGround;
  UpdateMode;
end;

function ConsoleAttr: Integer;
begin
  Result := GForeGround;
  Result := (Result shl 16) or GBackGround;
end;

procedure SetConsoleAttr(const TextAttr: Integer);
var
  lAttr: Integer;
begin
  lAttr := TextAttr;
  GBackGround := lAttr and $0000FFFF;
  GForeGround := lAttr shr 16;
  UpdateMode;
end;


function TextAttr: Word;
begin
  Result := GForeGround;
end;

procedure SetTextAttr(const TextAttr: Word);
begin
  GForeGround := TextAttr;
  UpdateMode;
end;

function BackgroundAttr: Word;
begin
  Result := GBackGround;
end;

procedure SetBackgroundAttr(const BackgroundAttr: Word);
begin
  GBackGround := BackgroundAttr;
  UpdateMode;
end;



initialization

if IsConsole then
begin
  Init;
  SetDefaults;
end;

finalization

end.
