// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team
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

type
  TConsoleMode = (Normal, Bright);
  TConsoleColor = (Black = 30, Red = 31, Green = 32, Yellow = 33, Blue = 34, Magenta = 35, Cyan = 36, White = 37);

procedure ResetConsole;
procedure TextColor(const Color: TConsoleColor);
procedure TextBackground(const Color: TConsoleColor);
procedure SetMode(const ConsoleMode: TConsoleMode);

implementation

uses

{$IFDEF MSWINDOWS}
  WinApi.Windows,

{$ENDIF}
  System.SysUtils;

const
  ESC = Chr(27);

var
  GForeGround: TConsoleColor;
  GBackGround: TConsoleColor;
  GMode: TConsoleMode = TConsoleMode.Normal;

function ToBackGround(const ForeGround: Byte): Byte;
begin
  if (GMode = TConsoleMode.Bright) and (ForeGround <> Byte(TConsoleColor.Black)) then
  begin
    Result := ForeGround + 10 + 60;
  end
  else
  begin
    Result := ForeGround + 10;
  end;
end;

{$IFDEF LINUX}

procedure EnableVirtualTerminalProcessing; inline;
begin
  // do nothing
end;

{$ELSE}

procedure EnableVirtualTerminalProcessing; inline;

const
  ENABLE_VIRTUAL_TERMINAL_PROCESSING = $0004;

var
  hOut: THandle;
  dwMode: UInt32;
begin
  hOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if hOut = INVALID_HANDLE_VALUE then
    raise Exception.CreateFmt('GetLastError() = %d', [GetLastError]);

  dwMode := 0;
  if (not GetConsoleMode(hOut, &dwMode)) then
    raise Exception.CreateFmt('GetLastError() = %d', [GetLastError]);

  dwMode := dwMode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
  if (not SetConsoleMode(hOut, dwMode)) then
    raise Exception.CreateFmt('GetLastError() = %d', [GetLastError]);
end;

{$ENDIF}

procedure ResetConsole;
begin
  write(ESC + '[0m');
end;

function GetColorString: string;
begin
  if GMode = TConsoleMode.Bright then
    Result := Format('[%d;1;%dm', [Byte(GForeGround), ToBackGround(Byte(GBackGround))])
  else
    Result := Format('[%d;%dm', [Byte(GForeGround), ToBackGround(Byte(GBackGround))]);
end;

procedure TextColor(const Color: TConsoleColor);
begin
  GForeGround := Color;
  write(ESC + GetColorString);
end;

procedure TextBackground(const Color: TConsoleColor);
begin
  GBackGround := Color;
  write(ESC + GetColorString);
end;

procedure SetMode(const ConsoleMode: TConsoleMode);
begin
  GMode := ConsoleMode;
end;

procedure InitDefault;
begin
  GForeGround := TConsoleColor.White;
  GBackGround := TConsoleColor.Black;
end;

initialization

EnableVirtualTerminalProcessing;
InitDefault;

finalization

end.
