// ***************************************************************************
//
// LoggerPro
//
// Copyright (c) 2010-2026 Daniele Teti
//
// https://github.com/danieleteti/loggerpro
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
// ***************************************************************************

unit LoggerPro.AnsiColors;

{ ANSI terminal color / style helpers for LoggerPro console rendering.
  Self-contained: no dependencies on other LoggerPro units.
  Safe to use on Windows 10+ (ENABLE_VIRTUAL_TERMINAL_PROCESSING), Linux, macOS. }

interface

const
  ESC = #27;

  { Foreground color escape sequences (bright variants where applicable) }
  FORE_DEFAULT  = ESC + '[39m';
  FORE_BLACK    = ESC + '[30m';
  FORE_DARKRED  = ESC + '[31m';
  FORE_DARKGREEN= ESC + '[32m';
  FORE_DARKYELLOW= ESC + '[33m';
  FORE_DARKBLUE = ESC + '[34m';
  FORE_DARKMAGENTA= ESC + '[35m';
  FORE_DARKCYAN = ESC + '[36m';
  FORE_GRAY     = ESC + '[37m';
  FORE_DARKGRAY = ESC + '[90m';
  FORE_RED      = ESC + '[91m';
  FORE_GREEN    = ESC + '[92m';
  FORE_YELLOW   = ESC + '[93m';
  FORE_BLUE     = ESC + '[94m';
  FORE_MAGENTA  = ESC + '[95m';
  FORE_CYAN     = ESC + '[96m';
  FORE_WHITE    = ESC + '[97m';

  { Background color escape sequences }
  BACK_DEFAULT     = ESC + '[49m';
  BACK_BLACK       = ESC + '[40m';
  BACK_DARKRED     = ESC + '[41m';
  BACK_DARKGREEN   = ESC + '[42m';
  BACK_DARKYELLOW  = ESC + '[43m';
  BACK_DARKBLUE    = ESC + '[44m';
  BACK_DARKMAGENTA = ESC + '[45m';
  BACK_DARKCYAN    = ESC + '[46m';
  BACK_GRAY        = ESC + '[47m';
  BACK_DARKGRAY    = ESC + '[100m';
  BACK_RED         = ESC + '[101m';
  BACK_GREEN       = ESC + '[102m';
  BACK_YELLOW      = ESC + '[103m';
  BACK_BLUE        = ESC + '[104m';
  BACK_MAGENTA     = ESC + '[105m';
  BACK_CYAN        = ESC + '[106m';
  BACK_WHITE       = ESC + '[107m';

  { Style escape sequences }
  STYLE_BRIGHT  = ESC + '[1m';
  STYLE_DIM     = ESC + '[2m';
  STYLE_NORMAL  = ESC + '[22m';
  STYLE_RESETALL= ESC + '[0m';

type
  /// <summary>
  /// ANSI foreground color escape sequences. Colorama-style namespaced
  /// access: Fore.Red, Fore.DarkGray, Fore.Reset, etc.
  /// Usage: WriteLn(Fore.Red + 'error text' + Style.ResetAll);
  /// Requires EnableANSIColorConsole on Windows 10+. No-op needed on Linux.
  /// Values delegate to the flat FORE_* constants above so the escape
  /// code literals live in one place only.
  /// </summary>
  Fore = record
  const
    Black       = FORE_BLACK;
    DarkRed     = FORE_DARKRED;
    DarkGreen   = FORE_DARKGREEN;
    DarkYellow  = FORE_DARKYELLOW;
    DarkBlue    = FORE_DARKBLUE;
    DarkMagenta = FORE_DARKMAGENTA;
    DarkCyan    = FORE_DARKCYAN;
    Gray        = FORE_GRAY;
    DarkGray    = FORE_DARKGRAY;
    Red         = FORE_RED;
    Green       = FORE_GREEN;
    Yellow      = FORE_YELLOW;
    Blue        = FORE_BLUE;
    Magenta     = FORE_MAGENTA;
    Cyan        = FORE_CYAN;
    White       = FORE_WHITE;
    Reset       = FORE_DEFAULT;
  end;

  /// <summary>
  /// ANSI background color escape sequences.
  /// Usage: WriteLn(Back.Red + 'highlighted' + Style.ResetAll);
  /// </summary>
  Back = record
  const
    Black       = BACK_BLACK;
    DarkRed     = BACK_DARKRED;
    DarkGreen   = BACK_DARKGREEN;
    DarkYellow  = BACK_DARKYELLOW;
    DarkBlue    = BACK_DARKBLUE;
    DarkMagenta = BACK_DARKMAGENTA;
    DarkCyan    = BACK_DARKCYAN;
    Gray        = BACK_GRAY;
    DarkGray    = BACK_DARKGRAY;
    Red         = BACK_RED;
    Green       = BACK_GREEN;
    Yellow      = BACK_YELLOW;
    Blue        = BACK_BLUE;
    Magenta     = BACK_MAGENTA;
    Cyan        = BACK_CYAN;
    White       = BACK_WHITE;
    Reset       = BACK_DEFAULT;
  end;

  /// <summary>
  /// ANSI style escape sequences.
  /// Style.ResetAll resets foreground, background and style.
  /// </summary>
  Style = record
  const
    Bright   = STYLE_BRIGHT;
    Dim      = STYLE_DIM;
    Normal   = STYLE_NORMAL;
    ResetAll = STYLE_RESETALL;
  end;

{ Wrap aText in aColor + STYLE_RESETALL (colorama-style autoreset).
  When aColor is empty, returns aText unchanged - so LogColorSchemes.Monochrome
  passed through the renderer emits zero escape codes automatically. }
function Colorize(const aText, aColor: string): string; inline;

{ Enable ANSI virtual terminal processing on Windows 10+.
  No-op on Linux / macOS (ANSI natively supported).
  Idempotent: safe to call multiple times. }
procedure EnableANSIColorConsole;

{ True if EnableANSIColorConsole succeeded (Windows) or on Unix platforms. }
function IsANSIColorConsoleEnabled: Boolean;

{ True when stdout is an interactive terminal. False when redirected to
  a file or piped to another process. Used to degrade colored output to
  monochrome automatically, avoiding escape-code pollution in log files. }
function IsStdoutTerminal: Boolean;

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows
{$ELSE}
  Posix.Unistd
{$ENDIF}
  ;

var
  gANSIEnabled: Boolean = {$IFDEF MSWINDOWS}False{$ELSE}True{$ENDIF};

function Colorize(const aText, aColor: string): string;
begin
  if aColor = '' then
    Result := aText
  else
    Result := aColor + aText + STYLE_RESETALL;
end;

procedure EnableANSIColorConsole;
{$IFDEF MSWINDOWS}
const
  ENABLE_VIRTUAL_TERMINAL_PROCESSING = $0004;
var
  hOut: THandle;
  dwMode: DWORD;
begin
  if gANSIEnabled then
    Exit;
  hOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if hOut = INVALID_HANDLE_VALUE then
    Exit;
  dwMode := 0;
  if not GetConsoleMode(hOut, dwMode) then
    Exit;
  if SetConsoleMode(hOut, dwMode or ENABLE_VIRTUAL_TERMINAL_PROCESSING) then
    gANSIEnabled := True;
end;
{$ELSE}
begin
  // Unix terminals handle ANSI natively; nothing to turn on.
end;
{$ENDIF}

function IsANSIColorConsoleEnabled: Boolean;
begin
  Result := gANSIEnabled;
end;

function IsStdoutTerminal: Boolean;
{$IFDEF MSWINDOWS}
var
  hOut: THandle;
  ft: DWORD;
begin
  hOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if hOut = INVALID_HANDLE_VALUE then
    Exit(False);
  ft := GetFileType(hOut);
  // FILE_TYPE_CHAR = console / serial device. FILE_TYPE_DISK / _PIPE = redirect.
  Result := ft = FILE_TYPE_CHAR;
end;
{$ELSE}
begin
  Result := isatty(STDOUT_FILENO) <> 0;
end;
{$ENDIF}

end.
