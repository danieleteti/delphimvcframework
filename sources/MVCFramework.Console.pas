// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
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
// ***************************************************************************
//
// Cross-platform console library:
// - Colored text output
// - Tables, boxes, progress bars, spinners, and interactive menus
// - Keyboard input handling (arrows, special keys)
// - Windows and Linux support
//
// *************************************************************************** }

unit MVCFramework.Console;

{$I dmvcframework.inc}
{$WARN UNIT_PLATFORM OFF}

interface

uses
  System.SysUtils,
  System.SyncObjs,
  System.Classes,
  System.Character
{$IFDEF MSWINDOWS}
  ,WinApi.Windows
{$ENDIF}
{$IFDEF LINUX}
  ,Posix.Unistd,
  Posix.Termios,
  Posix.SysStat,
  Posix.Fcntl,
  Posix.SysSelect,
  Posix.StrOpts,
  Posix.Stdlib
{$ENDIF}
    ;

const
  KEY_UP    = 256 + 38;  // VK_UP
  KEY_DOWN  = 256 + 40;  // VK_DOWN
  KEY_LEFT  = 256 + 37;  // VK_LEFT
  KEY_RIGHT = 256 + 39;  // VK_RIGHT
  KEY_ESCAPE = 27;
  KEY_ENTER = 13;

type
  TConsoleColor = (
    Black = 0,
    DarkBlue = 1,
    DarkGreen = 2,
    DarkCyan = 3,
    DarkRed = 4,
    DarkMagenta = 5,
    DarkYellow = 6,
    Gray = 7,
    DarkGray = 8,
    Blue = 9,
    Green = 10,
    Cyan = 11,
    Red = 12,
    Magenta = 13,
    Yellow = 14,
    White = 15,
    UseDefault = 16
    );

  TBoxStyle = (bsSingle, bsDouble, bsRounded, bsThick, bsUseDefault);

  TSpinnerStyle = (
    ssLine,       // - \ | /
    ssDots,       // Braille dots
    ssBounce,     // Braille bounce
    ssGrow,       // Block elements
    ssArrow,      // Arrow rotation
    ssCircle,     // Circle quarters
    ssClock,      // Clock faces
    ssEarth,      // Globe rotation
    ssMoon,       // Moon phases
    ssWeather     // Weather emoji
  );

  TProgressBarStyle = (pbsSimple, pbsBlocks, pbsArrows, pbsCircles);
  TAlignment = (taLeft, taCenter, taRight);
  TListStyle = (lsBullet, lsNumbered, lsDash, lsArrow);

  EMVCConsole = class(Exception)
  end;

  TMVCConsoleSize = record
    Columns: Word;
    Rows: Word;
  end;

  TMVCConsolePoint = record
    X: Word;
    Y: Word;
  end;

  TConsoleColorStyle = record
    TextColor, BackgroundColor, DrawColor, SymbolsColor,
    BackgroundHighlightColor, TextHighlightColor: TConsoleColor;
    BoxStyle: TBoxStyle;
  end;

  TStringArray = array of string;
  TStringMatrix = array of TStringArray;

var
  ConsoleTheme: TConsoleColorStyle = (
    TextColor : TConsoleColor.Cyan;
    BackgroundColor : TConsoleColor.Black;
    DrawColor : TConsoleColor.White;
    SymbolsColor : TConsoleColor.Gray;
    BackgroundHighlightColor: TConsoleColor.Cyan;
    TextHighlightColor: TConsoleColor.Blue;
    BoxStyle: TBoxStyle.bsRounded;
  );


// ============================================================================
// LOW-LEVEL CONSOLE FUNCTIONS
// ============================================================================

procedure ResetConsole;
procedure TextColor(const Color: TConsoleColor);
procedure TextBackground(const Color: TConsoleColor);
procedure GotoXY(const X, Y: Word);
function GetConsoleSize: TMVCConsoleSize;
function GetConsoleBufferSize: TMVCConsoleSize;
function GetCursorPosition: TMVCConsolePoint;
procedure ClrScr;
function GetCh: Char;
function GetKey: Integer;
function IsSpecialKey(KeyCode: Integer): Boolean; inline;
procedure WaitForReturn;
procedure SaveColors;
procedure RestoreSavedColors;
procedure SetDefaultColors;
function ConsoleAttr: Integer;
procedure SetConsoleAttr(const TextAttr: Integer);
function TextAttr: Word;
procedure SetTextAttr(const TextAttr: Word);
function BackgroundAttr: Word;
procedure SetBackgroundAttr(const BackgroundAttr: Word);
procedure HideCursor;
procedure ShowCursor;
function KeyPressed: Boolean;
procedure EnableUTF8Console;

// ============================================================================
// TEXT OUTPUT
// ============================================================================

procedure WriteColoredText(const Text: string;
  ForeColor: TConsoleColor = UseDefault;
  BackColor: TConsoleColor = UseDefault);

procedure WriteLine(const Text: string); overload;
procedure WriteLine(const Text: string; ForeColor: TConsoleColor); overload;
procedure WriteLine(const Text: string; ForeColor: TConsoleColor;
  BackColor: TConsoleColor); overload;

procedure WriteAlignedText(const Text: string; Width: Integer;
  Alignment: TAlignment = taCenter; TextColor: TConsoleColor = UseDefault);
procedure CenterInScreen(const Text: String);

// ============================================================================
// STATUS MESSAGES
// ============================================================================

procedure WriteHeader(const Text: string; Width: Integer = 80;
  HeaderColor: TConsoleColor = UseDefault);
procedure WriteSeparator(Width: Integer = 60; CharSymbol: Char = '-');
procedure WriteSuccess(const Message: string);
procedure WriteWarning(const Message: string);
procedure WriteError(const Message: string);
procedure WriteInfo(const Message: string);
procedure WriteFormattedList(const Title: string; const Items: TStringArray;
  ListStyle: TListStyle);

// ============================================================================
// DRAWING PRIMITIVES
// ============================================================================

procedure DrawBox(X, Y, Width, Height: Word; Style: TBoxStyle = bsRounded;
  const Title: string = '');
procedure DrawHorizontalLine(X, Y, Length: Word; Style: TBoxStyle = bsUseDefault);
procedure DrawVerticalLine(X, Y, Length: Word; Style: TBoxStyle = bsUseDefault);
procedure ClearRegion(X, Y, Width, Height: Word);
procedure SaveCursorPosition;
procedure RestoreCursorPosition;

// ============================================================================
// INFORMATION
// ============================================================================

function ColorName(const Color: TConsoleColor): String;
function IsTerminalCapable: Boolean;
function GetTerminalName: string;
procedure Beep;
procedure FlashScreen;

// ============================================================================
// HIGH-LEVEL API
// ============================================================================

type
  ISpinner = interface
    ['{A1B2C3D4-5E6F-7890-ABCD-EF1234567890}']
    procedure Hide;
  end;

  IProgress = interface
    ['{8F5E3C2A-1B4D-4E9F-A3C7-9D2E6F1B8A4C}']
    procedure Update(Value: Integer);
    procedure Increment(Amount: Integer = 1);
    procedure SetMessage(const Msg: string);
    procedure Complete;
  end;

/// <summary>
/// Interactive menu with keyboard navigation. Returns selected index or -1 if cancelled.
/// </summary>
function Menu(const Items: TStringArray): Integer; overload;
function Menu(const Title: string; const Items: TStringArray): Integer; overload;
function Menu(const Title: string; const Items: TStringArray;
  DefaultIndex: Integer): Integer; overload;

/// <summary>
/// Displays a formatted table with auto-sizing columns.
/// </summary>
procedure Table(const Headers: TStringArray; const Data: TStringMatrix); overload;
procedure Table(const Headers: TStringArray; const Data: TStringMatrix;
  const Title: string); overload;

/// <summary>
/// Interactive table with row selection. Returns selected row index or -1.
/// </summary>
function TableMenu(const Headers: TStringArray;
  const Data: TStringMatrix): Integer; overload;
function TableMenu(const Title: string; const Headers: TStringArray;
  const Data: TStringMatrix): Integer; overload;
function TableMenu(const Title: string; const Headers: TStringArray;
  const Data: TStringMatrix; DefaultIndex: Integer): Integer; overload;

/// <summary>
/// Displays a box with optional title and content lines.
/// </summary>
procedure Box(const Content: TStringArray); overload;
procedure Box(const Title: string; const Content: TStringArray); overload;
procedure Box(const Title: string; const Content: TStringArray;
  Width: Integer); overload;

/// <summary>
/// Progress bar with auto-cleanup. MaxValue > 0: determinate. MaxValue = 0: indeterminate.
/// </summary>
function Progress(const Title: string; MaxValue: Integer): IProgress; overload;
function Progress(const Title: string): IProgress; overload;

/// <summary>
/// Yes/no confirmation prompt. Returns True if user confirms.
/// </summary>
function Confirm(const Question: string): Boolean; overload;
function Confirm(const Question: string; DefaultYes: Boolean): Boolean; overload;

/// <summary>
/// Quick single-choice prompt. Returns selected index or -1.
/// </summary>
function Choose(const Question: string; const Options: TStringArray): Integer;

/// <summary>
/// Non-blocking background spinner. Call Hide or release the interface to stop.
/// </summary>
function Spinner(AStyle: TSpinnerStyle = ssLine;
  AColor: TConsoleColor = DarkGray): ISpinner; overload;
function Spinner(const AMessage: string; AStyle: TSpinnerStyle = ssLine;
  AColor: TConsoleColor = DarkGray): ISpinner; overload;

// ============================================================================
// UTILITY
// ============================================================================

function PadRight(const S: string; Len: Integer): string;


implementation

uses
  System.TypInfo,
  System.Math;

const
  ESC = Chr(27);

type
  TStyleColorComponent = (sccText, sccBackground, sccHighLightBackground, sccHighLightText, sccDraw, sccSymbol);
  TBoxChars = record
    TopLeft, TopRight, BottomLeft, BottomRight: Char;
    Vertical, Horizontal: Char;
    LeftJoin, RightJoin, TopJoin, BottomJoin, Cross: Char;
  end;

var
  GForeGround, GSavedForeGround: Int16;
  GBackGround, GSavedBackGround: Int16;
  GOutHandle: THandle = INVALID_HANDLE_VALUE;
  GInputHandle: THandle = INVALID_HANDLE_VALUE;
  GIsConsoleAllocated: Boolean = False;
  GLock: TObject = nil;
  GSavedCursorX, GSavedCursorY: Word;
{$IFDEF MSWINDOWS}
  hConsoleInput: THandle;
{$ENDIF}

{$IFDEF LINUX}
type
  TLinuxWinSize = record
    ws_row: Word;
    ws_col: Word;
    ws_xpixel: Word;
    ws_ypixel: Word;
  end;

  TLinuxTimeVal = record
    tv_sec: Int64;
    tv_usec: Int64;
  end;

const
  TIOCGWINSZ = $5413;

function __select(nfds: Integer; readfds, writefds, exceptfds: Pointer;
  timeout: Pointer): Integer; cdecl; external 'libc.so.6' name 'select';

var
  GOriginalTermios: termios;
  GTerminalSetup: Boolean = False;

const
  ANSI_COLORS: array[TConsoleColor] of string = (
    '30',     // Black
    '34',     // DarkBlue
    '32',     // DarkGreen
    '36',     // DarkCyan
    '31',     // DarkRed
    '35',     // DarkMagenta
    '33',     // DarkYellow
    '37',     // Gray
    '90',     // DarkGray
    '94',     // Blue
    '92',     // Green
    '96',     // Cyan
    '91',     // Red
    '95',     // Magenta
    '93',     // Yellow
    '97',     // White
    '0'       // UseDefault (reset)
  );

  ANSI_BG_COLORS: array[TConsoleColor] of string = (
    '40',     // Black
    '44',     // DarkBlue
    '42',     // DarkGreen
    '46',     // DarkCyan
    '41',     // DarkRed
    '45',     // DarkMagenta
    '43',     // DarkYellow
    '47',     // Gray
    '100',    // DarkGray
    '104',    // Blue
    '102',    // Green
    '106',    // Cyan
    '101',    // Red
    '105',    // Magenta
    '103',    // Yellow
    '107',    // White
    '0'       // UseDefault (reset)
  );

{$ENDIF}

// ============================================================================
// INTERNAL HELPERS
// ============================================================================

function GetBoxStyleOrDefault(BoxStyle: TBoxStyle): TBoxStyle;
begin
  Result := BoxStyle;
  if Result = bsUseDefault then
    Result := ConsoleTheme.BoxStyle;
end;

function GetBoxChars(Style: TBoxStyle): TBoxChars;
begin
  Style := GetBoxStyleOrDefault(Style);
  case Style of
    bsSingle: begin
      Result.TopLeft := #$250C; Result.TopRight := #$2510;
      Result.BottomLeft := #$2514; Result.BottomRight := #$2518;
      Result.Vertical := #$2502; Result.Horizontal := #$2500;
      Result.LeftJoin := #$251C; Result.RightJoin := #$2524;
      Result.TopJoin := #$252C; Result.BottomJoin := #$2534;
      Result.Cross := #$253C;
    end;
    bsDouble: begin
      Result.TopLeft := #$2554; Result.TopRight := #$2557;
      Result.BottomLeft := #$255A; Result.BottomRight := #$255D;
      Result.Vertical := #$2551; Result.Horizontal := #$2550;
      Result.LeftJoin := #$2560; Result.RightJoin := #$2563;
      Result.TopJoin := #$2566; Result.BottomJoin := #$2569;
      Result.Cross := #$256C;
    end;
    bsRounded: begin
      Result.TopLeft := #$256D; Result.TopRight := #$256E;
      Result.BottomLeft := #$2570; Result.BottomRight := #$256F;
      Result.Vertical := #$2502; Result.Horizontal := #$2500;
      Result.LeftJoin := #$251C; Result.RightJoin := #$2524;
      Result.TopJoin := #$252C; Result.BottomJoin := #$2534;
      Result.Cross := #$253C;
    end;
    bsThick: begin
      Result.TopLeft := #$250F; Result.TopRight := #$2513;
      Result.BottomLeft := #$2517; Result.BottomRight := #$251B;
      Result.Vertical := #$2503; Result.Horizontal := #$2501;
      Result.LeftJoin := #$2523; Result.RightJoin := #$252B;
      Result.TopJoin := #$2533; Result.BottomJoin := #$253B;
      Result.Cross := #$254B;
    end;
  end;
end;

function GetColorOrDefault(Color: TConsoleColor; StyleColorComponent: TStyleColorComponent): TConsoleColor;
begin
  if Color = TConsoleColor.UseDefault then
  begin
    case StyleColorComponent of
      sccText: Result := ConsoleTheme.TextColor;
      sccBackground: Result := ConsoleTheme.BackgroundColor;
      sccDraw: Result := ConsoleTheme.DrawColor;
      sccSymbol: Result := ConsoleTheme.SymbolsColor;
      sccHighLightBackground: Result := ConsoleTheme.BackgroundHighlightColor;
      sccHighLightText: Result := ConsoleTheme.TextHighlightColor;
      else
        raise EMVCConsole.Create('Unknown StyleColorComponent');
    end;
  end
  else
    Result := Color;
end;

procedure FlushOutput; inline;
begin
  Flush(Output);
end;

function CalcColumnWidths(const Headers: TStringArray; const Data: TStringMatrix): TArray<Integer>;
var
  I, J: Integer;
begin
  SetLength(Result, Length(Headers));
  for I := 0 to High(Headers) do
  begin
    Result[I] := Length(Headers[I]);
    for J := 0 to High(Data) do
      if (I < Length(Data[J])) and (Length(Data[J][I]) > Result[I]) then
        Result[I] := Length(Data[J][I]);
    Inc(Result[I], 2); // padding
  end;
end;

// ============================================================================
// UTILITY
// ============================================================================

function PadRight(const S: string; Len: Integer): string;
begin
  Result := S;
  if Length(Result) < Len then
    Result := Result + StringOfChar(' ', Len - Length(Result))
  else if Length(Result) > Len then
    Result := Copy(Result, 1, Len);
end;

// ============================================================================
// PLATFORM: LINUX
// ============================================================================

function ColorName(const Color: TConsoleColor): String;
begin
  Result := GetEnumName(TypeInfo(TConsoleColor), Ord(Color));
end;

{$IFDEF LINUX}

procedure SetupTerminal;
var
  NewTermios: termios;
begin
  if not GTerminalSetup then
  begin
    tcgetattr(STDIN_FILENO, GOriginalTermios);
    NewTermios := GOriginalTermios;
    NewTermios.c_lflag := NewTermios.c_lflag and not (ICANON or ECHO);
    tcsetattr(STDIN_FILENO, TCSANOW, NewTermios);
    GTerminalSetup := True;
  end;
end;

procedure RestoreTerminal;
begin
  if GTerminalSetup then
  begin
    tcsetattr(STDIN_FILENO, TCSANOW, GOriginalTermios);
    GTerminalSetup := False;
  end;
end;

function KeyPressed: Boolean;
var
  FDSet: fd_set;
  TimeVal: TLinuxTimeVal;
begin
  SetupTerminal;
  __FD_ZERO(FDSet);
  __FD_SET(STDIN_FILENO, FDSet);
  TimeVal.tv_sec := 0;
  TimeVal.tv_usec := 0;
  Result := __select(STDIN_FILENO + 1, @FDSet, nil, nil, @TimeVal) > 0;
end;

procedure EnableUTF8Console;
begin
  WriteLn(ESC + '[?1049h');
end;

procedure HideCursor;
begin
  Write(ESC + '[?25l');
end;

procedure ShowCursor;
begin
  Write(ESC + '[?25h');
end;

procedure Init; inline;
begin
  SetupTerminal;
end;

procedure WaitForReturn;
var
  Ch: Char;
begin
  SetupTerminal;
  repeat
    Ch := GetCh;
  until (Ch = #13) or (Ch = #10);
end;

procedure UpdateMode;
begin
  Write(ESC + '[' + ANSI_COLORS[TConsoleColor(GForeGround)] + ';' +
        ANSI_BG_COLORS[TConsoleColor(GBackGround shr 4)] + 'm');
end;

function GetCh: Char;
var
  Buffer: array[0..0] of Char;
begin
  SetupTerminal;
  if __read(STDIN_FILENO, @Buffer, 1) = 1 then
    Result := Buffer[0]
  else
    Result := #0;
end;

procedure GotoXY(const X, Y: Word);
begin
  Write(ESC + '[' + IntToStr(Y + 1) + ';' + IntToStr(X + 1) + 'H');
end;

function GetConsoleSize: TMVCConsoleSize;
var
  WinSize: TLinuxWinSize;
begin
  if ioctl(STDOUT_FILENO, TIOCGWINSZ, @WinSize) = 0 then
  begin
    Result.Columns := WinSize.ws_col;
    Result.Rows := WinSize.ws_row;
  end
  else
  begin
    Result.Columns := 80;
    Result.Rows := 25;
  end;
end;

function GetConsoleBufferSize: TMVCConsoleSize;
begin
  Result := GetConsoleSize;
end;

function GetCursorPosition: TMVCConsolePoint;
var
  Response: string;
  Ch: Char;
  I: Integer;
  Numbers: array[0..1] of Integer;
  NumberIndex: Integer;
  CurrentNumber: string;
begin
  Write(ESC + '[6n');
  Response := '';
  NumberIndex := 0;
  CurrentNumber := '';
  repeat
    Ch := GetCh;
    Response := Response + Ch;
  until (Ch = 'R') or (Length(Response) > 20);

  for I := 1 to Length(Response) do
  begin
    Ch := Response[I];
    if CharInSet(Ch, ['0'..'9']) then
      CurrentNumber := CurrentNumber + Ch
    else if (Ch = ';') or (Ch = 'R') then
    begin
      if CurrentNumber <> '' then
      begin
        Numbers[NumberIndex] := StrToIntDef(CurrentNumber, 1);
        Inc(NumberIndex);
        CurrentNumber := '';
      end;
    end;
  end;

  if NumberIndex >= 2 then
  begin
    Result.Y := Numbers[0] - 1;
    Result.X := Numbers[1] - 1;
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

procedure ClrScr;
begin
  Write(ESC + '[2J');
  Write(ESC + '[H');
end;

function GetKey: Integer;
var
  Buffer: array[0..0] of Char;
  Ch: Char;
begin
  Result := 0;
  SetupTerminal;
  if __read(STDIN_FILENO, @Buffer, 1) = 1 then
  begin
    Ch := Buffer[0];
    Result := Ord(Ch);
    if Ch = #27 then
    begin
      if KeyPressed then
      begin
        if __read(STDIN_FILENO, @Buffer, 1) = 1 then
        begin
          if Buffer[0] = '[' then
          begin
            if __read(STDIN_FILENO, @Buffer, 1) = 1 then
            begin
              case Buffer[0] of
                'A': Result := KEY_UP;
                'B': Result := KEY_DOWN;
                'C': Result := KEY_RIGHT;
                'D': Result := KEY_LEFT;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{$ENDIF}

// ============================================================================
// PLATFORM: WINDOWS
// ============================================================================

{$IFDEF MSWINDOWS}

const
  ATTACH_PARENT_PROCESS = DWORD(-1);
function AttachConsole(dwProcessId: DWORD): BOOL; stdcall; external kernel32 name 'AttachConsole';

procedure EnableUTF8Console;
begin
  SetConsoleOutputCP(CP_UTF8);
end;

procedure KeyInit;
var
  mode: DWORD;
begin
  Reset(Input);
  GInputHandle := TTextRec(Input).Handle;
  hConsoleInput := GInputHandle;
  SetActiveWindow(0);
  GetConsoleMode(hConsoleInput, mode);
  if (mode and ENABLE_MOUSE_INPUT) = ENABLE_MOUSE_INPUT then
    SetConsoleMode(hConsoleInput, mode xor ENABLE_MOUSE_INPUT);
end;

procedure Init;
begin
  if not GIsConsoleAllocated then
  begin
    TMonitor.Enter(GLock);
    try
      KeyInit;
      if not GIsConsoleAllocated then
      begin
        if not IsConsole then
        begin
          if not AttachConsole(ATTACH_PARENT_PROCESS) then
            AllocConsole;
        end;
        GOutHandle := GetStdHandle(STD_OUTPUT_HANDLE);
        if GOutHandle = INVALID_HANDLE_VALUE then
          raise EMVCConsole.CreateFmt('Cannot Get STD_OUTPUT_HANDLE - GetLastError() = %d', [GetLastError]);
        GIsConsoleAllocated := True;
      end;
    finally
      TMonitor.Exit(GLock);
    end;
  end;
end;

function KeyPressed: Boolean;
var
  InputRecord: INPUT_RECORD;
  NumRead: DWORD;
begin
  Result := False;
  Init;
  if PeekConsoleInput(GInputHandle, InputRecord, 1, NumRead) and (NumRead > 0) then
  begin
    if (InputRecord.EventType = KEY_EVENT) and InputRecord.Event.KeyEvent.bKeyDown then
      Result := True
    else
      ReadConsoleInput(GInputHandle, InputRecord, 1, NumRead);
  end;
end;

procedure InternalShowCursor(const AShowCursor: Boolean);
var
  info: CONSOLE_CURSOR_INFO;
begin
  Init;
  GetConsoleCursorInfo(GOutHandle, info);
  info.bVisible := AShowCursor;
  SetConsoleCursorInfo(GOutHandle, info);
end;

procedure WaitForReturn;
begin
  Init;
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
  Init;
  lSize := GetConsoleBufferSize;
  dwConSize := lSize.Columns * lSize.Rows;
  lStartCoord.X := 0;
  lStartCoord.Y := 0;
  if not FillConsoleOutputCharacter(GOutHandle, ' ', dwConSize, lStartCoord, lCharsWritten) then
    raise EMVCConsole.CreateFmt('Cannot fill console - GetLastError() = %d', [GetLastError]);
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

function GetCursorPosition: TMVCConsolePoint;
var
  lConsoleScreenBufferInfo: _CONSOLE_SCREEN_BUFFER_INFO;
begin
  Init;
  if not GetConsoleScreenBufferInfo(GOutHandle, lConsoleScreenBufferInfo) then
    raise EMVCConsole.CreateFmt('Cannot Get Cursor Position - GetLastError() = %d', [GetLastError]);
  Result.X := lConsoleScreenBufferInfo.dwCursorPosition.X;
  Result.Y := lConsoleScreenBufferInfo.dwCursorPosition.Y;
end;

function GetCh: Char;
var
  Key: Integer;
begin
  Key := GetKey;
  if Key < 256 then
    Result := Chr(Key)
  else
    Result := #0;
end;

function GetKey: Integer;
var
  InputRecord: INPUT_RECORD;
  NumRead: DWORD;
  KeyEvent: KEY_EVENT_RECORD;
begin
  Init;
  repeat
    if ReadConsoleInput(GInputHandle, InputRecord, 1, NumRead) then
    begin
      if (InputRecord.EventType = KEY_EVENT) then
      begin
        KeyEvent := InputRecord.Event.KeyEvent;
        if KeyEvent.bKeyDown then
        begin
          if KeyEvent.AsciiChar <> #0 then
          begin
            Result := Ord(KeyEvent.AsciiChar);
            Exit;
          end
          else
          begin
            Result := 256 + KeyEvent.wVirtualKeyCode;
            Exit;
          end;
        end;
      end;
    end;
  until False;
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

procedure UpdateMode;
begin
  Init;
  SetConsoleTextAttribute(GOutHandle, Ord(GForeGround) or Ord(GBackGround));
end;

procedure GotoXY(const X, Y: Word);
var
  lCoord: _COORD;
begin
  Init;
  lCoord.X := X;
  lCoord.Y := Y;
  if not SetConsoleCursorPosition(GOutHandle, lCoord) then
    raise EMVCConsole.Create('Invalid Coordinates');
end;

procedure HideCursor;
begin
  InternalShowCursor(False);
end;

procedure ShowCursor;
begin
  InternalShowCursor(True);
end;

{$ENDIF}

// ============================================================================
// CROSS-PLATFORM HIGH-LEVEL FUNCTIONS
// ============================================================================

procedure CenterInScreen(const Text: String);
var
  Size: TMVCConsoleSize;
begin
  Init;
  Size := GetConsoleSize;
  GotoXY(Size.Columns div 2 - Length(Text) div 2, Size.Rows div 2 - 1);
  Write(Text);
end;

procedure ResetConsole;
begin
  SetDefaultColors;
{$IFDEF LINUX}
  Write(ESC + '[0m');
{$ENDIF}
end;

procedure TextColor(const Color: TConsoleColor);
begin
  GForeGround := Ord(Color);
  UpdateMode;
end;

procedure TextBackground(const Color: TConsoleColor);
begin
  GBackGround := Ord(Color) shl 4;
  UpdateMode;
end;

procedure SetDefaultColors;
begin
  GForeGround := Ord(TConsoleColor.Gray);
  GBackGround := Ord(TConsoleColor.Black);
  UpdateMode;
end;

procedure SaveColors;
begin
  Init;
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

function IsSpecialKey(KeyCode: Integer): Boolean;
begin
  Result := KeyCode > 255;
end;

// ============================================================================
// TEXT OUTPUT
// ============================================================================

procedure WriteColoredText(const Text: string; ForeColor: TConsoleColor;
                          BackColor: TConsoleColor);
begin
  Init;
  SaveColors;
  try
    TextColor(GetColorOrDefault(ForeColor, sccText));
    TextBackground(GetColorOrDefault(BackColor, sccBackground));
    Write(Text);
  finally
    RestoreSavedColors;
  end;
end;

procedure WriteLine(const Text: string);
begin
  WriteLn(Text);
end;

procedure WriteLine(const Text: string; ForeColor: TConsoleColor);
begin
  WriteColoredText(Text, ForeColor, UseDefault);
  WriteLn;
end;

procedure WriteLine(const Text: string; ForeColor: TConsoleColor; BackColor: TConsoleColor);
begin
  WriteColoredText(Text, ForeColor, BackColor);
  WriteLn;
end;

procedure WriteAlignedText(const Text: string; Width: Integer; Alignment: TAlignment; TextColor: TConsoleColor);
var
  PaddingLeft, PaddingRight: Integer;
  AlignedText: string;
begin
  TextColor := GetColorOrDefault(TextColor, sccText);
  if Length(Text) >= Width then
  begin
    WriteLine(Text, TextColor);
    Exit;
  end;
  case Alignment of
    taLeft:
      AlignedText := Text + StringOfChar(' ', Width - Length(Text));
    taRight:
      AlignedText := StringOfChar(' ', Width - Length(Text)) + Text;
    taCenter:
    begin
      PaddingLeft := (Width - Length(Text)) div 2;
      PaddingRight := Width - Length(Text) - PaddingLeft;
      AlignedText := StringOfChar(' ', PaddingLeft) + Text + StringOfChar(' ', PaddingRight);
    end;
  end;
  WriteLine(AlignedText, TextColor);
end;

// ============================================================================
// STATUS MESSAGES
// ============================================================================

procedure WriteHeader(const Text: string; Width: Integer; HeaderColor: TConsoleColor);
var
  Line: string;
  PaddingSize: Integer;
  CharSymbol: Char;
begin
  HeaderColor := GetColorOrDefault(HeaderColor, sccHighLightText);
  CharSymbol := GetBoxChars(ConsoleTheme.BoxStyle).Horizontal;
  Line := StringOfChar(CharSymbol, Width);
  WriteLine(Line, ConsoleTheme.DrawColor);

  if Text <> '' then
  begin
    PaddingSize := (Width - Length(Text) - 2) div 2;
    Line := StringOfChar(' ', PaddingSize) + ' ' + Text + ' ' +
            StringOfChar(' ', Width - PaddingSize - Length(Text) - 2);
    WriteLine(Line, HeaderColor);
    Line := StringOfChar(CharSymbol, Width);
    WriteLine(Line, ConsoleTheme.DrawColor);
  end;
end;

procedure WriteSeparator(Width: Integer; CharSymbol: Char);
begin
  WriteLine(StringOfChar(CharSymbol, Width), Gray);
end;

procedure WriteSuccess(const Message: string);
begin
  WriteColoredText('[SUCCESS] ', Green);
  WriteLine(Message, White);
end;

procedure WriteWarning(const Message: string);
begin
  WriteColoredText('[WARNING] ', Yellow);
  WriteLine(Message, White);
end;

procedure WriteError(const Message: string);
begin
  WriteColoredText('[ERROR] ', Red);
  WriteLine(Message, White);
end;

procedure WriteInfo(const Message: string);
begin
  WriteColoredText('[INFO] ', Cyan);
  WriteLine(Message, White);
end;

procedure WriteFormattedList(const Title: string; const Items: TStringArray;
  ListStyle: TListStyle);
var
  I: Integer;
  Prefix: string;
begin
  if Title <> '' then
    WriteLine(Title, ConsoleTheme.TextColor);
  for I := 0 to High(Items) do
  begin
    case ListStyle of
      lsBullet: Prefix := '  * ';
      lsNumbered: Prefix := Format('%3d. ', [I + 1]);
      lsDash: Prefix := '  - ';
      lsArrow: Prefix := '  > ';
    end;
    WriteColoredText(Prefix, ConsoleTheme.SymbolsColor);
    WriteLine(Items[I], ConsoleTheme.TextColor);
  end;
end;

// ============================================================================
// DRAWING PRIMITIVES
// ============================================================================

procedure SaveCursorPosition;
var
  Pos: TMVCConsolePoint;
begin
  Pos := GetCursorPosition;
  GSavedCursorX := Pos.X;
  GSavedCursorY := Pos.Y;
end;

procedure RestoreCursorPosition;
begin
  GotoXY(GSavedCursorX, GSavedCursorY);
end;

procedure DrawBox(X, Y, Width, Height: Word; Style: TBoxStyle; const Title: string);
var
  I, J: Integer;
  TitleStart: Integer;
  BoxChars: TBoxChars;
begin
  BoxChars := GetBoxChars(Style);
  GotoXY(X, Y);
  Write(BoxChars.TopLeft);
  TitleStart := (Width - Length(Title)) div 2;
  J := 1;
  while J <= Width - 2 do
  begin
    if (Title <> '') and (J = TitleStart) then
    begin
      Write(Title);
      J := J + Length(Title);
    end
    else
    begin
      Write(BoxChars.Horizontal);
      Inc(J);
    end;
  end;
  Write(BoxChars.TopRight);

  for I := 1 to Height - 2 do
  begin
    GotoXY(X, Y + I);
    Write(BoxChars.Vertical);
    GotoXY(X + Width - 1, Y + I);
    Write(BoxChars.Vertical);
  end;

  GotoXY(X, Y + Height - 1);
  Write(BoxChars.BottomLeft);
  for I := 1 to Width - 2 do
    Write(BoxChars.Horizontal);
  Write(BoxChars.BottomRight);
end;

procedure DrawHorizontalLine(X, Y, Length: Word; Style: TBoxStyle);
var
  I: Integer;
  BoxChars: TBoxChars;
begin
  BoxChars := GetBoxChars(Style);
  GotoXY(X, Y);
  for I := 0 to Length - 1 do
    Write(BoxChars.Horizontal);
end;

procedure DrawVerticalLine(X, Y, Length: Word; Style: TBoxStyle);
var
  I: Integer;
  BoxChars: TBoxChars;
begin
  BoxChars := GetBoxChars(Style);
  for I := 0 to Length - 1 do
  begin
    GotoXY(X, Y + I);
    Write(BoxChars.Vertical);
  end;
end;

procedure ClearRegion(X, Y, Width, Height: Word);
var
  I: Integer;
  Spaces: string;
begin
  Spaces := StringOfChar(' ', Width);
  for I := 0 to Height - 1 do
  begin
    GotoXY(X, Y + I);
    Write(Spaces);
  end;
end;

// ============================================================================
// INFORMATION
// ============================================================================

function IsTerminalCapable: Boolean;
begin
{$IFDEF LINUX}
  Result := isatty(STDOUT_FILENO) = 1;
{$ELSE}
  Result := True;
{$ENDIF}
end;

function GetTerminalName: string;
{$IFDEF LINUX}
var
  TermVar: PAnsiChar;
{$ENDIF}
begin
{$IFDEF LINUX}
  TermVar := getenv('TERM');
  if TermVar <> nil then
    Result := string(TermVar)
  else
    Result := 'unknown';
{$ELSE}
  Result := 'Windows Console';
{$ENDIF}
end;

procedure Beep;
begin
{$IFDEF LINUX}
  Write(#7);
{$ELSE}
  WinApi.Windows.Beep(800, 200);
{$ENDIF}
end;

procedure FlashScreen;
begin
{$IFDEF LINUX}
  Write(ESC + '[?5h');
  Sleep(100);
  Write(ESC + '[?5l');
{$ELSE}
  SaveColors;
  try
    TextColor(Black);
    TextBackground(White);
    ClrScr;
    Sleep(100);
  finally
    RestoreSavedColors;
    ClrScr;
  end;
{$ENDIF}
end;

// ============================================================================
// HIGH-LEVEL API: TABLE
// ============================================================================

procedure InternalDrawTable(const Headers: TStringArray; const Data: TStringMatrix;
  const ColWidths: TArray<Integer>; const lBoxChars: TBoxChars;
  HighlightRow: Integer = -1);
var
  I, J: Integer;
  Cell, Line: string;
begin
  // Top border
  WriteColoredText(lBoxChars.TopLeft, ConsoleTheme.DrawColor);
  Line := '';
  for I := 0 to High(ColWidths) do
  begin
    Line := Line + StringOfChar(lBoxChars.Horizontal, ColWidths[I]);
    if I < High(ColWidths) then
      Line := Line + lBoxChars.TopJoin
    else
      Line := Line + lBoxChars.TopRight;
  end;
  WriteLine(Line, ConsoleTheme.DrawColor);

  // Headers
  WriteColoredText(lBoxChars.Vertical, ConsoleTheme.DrawColor);
  for I := 0 to High(Headers) do
  begin
    Cell := ' ' + PadRight(Headers[I], ColWidths[I] - 2) + ' ';
    WriteColoredText(Cell, ConsoleTheme.TextHighlightColor);
    WriteColoredText(lBoxChars.Vertical, ConsoleTheme.DrawColor);
  end;
  WriteLn;

  // Header separator
  WriteColoredText(lBoxChars.LeftJoin, ConsoleTheme.DrawColor);
  for I := 0 to High(ColWidths) do
  begin
    WriteColoredText(StringOfChar(lBoxChars.Horizontal, ColWidths[I]), ConsoleTheme.DrawColor);
    if I < High(ColWidths) then
      WriteColoredText(lBoxChars.Cross, ConsoleTheme.DrawColor)
    else
      WriteColoredText(lBoxChars.RightJoin, ConsoleTheme.DrawColor);
  end;
  WriteLn;

  // Data rows
  for I := 0 to High(Data) do
  begin
    WriteColoredText(lBoxChars.Vertical, ConsoleTheme.DrawColor);
    for J := 0 to High(Headers) do
    begin
      if J < Length(Data[I]) then
        Cell := ' ' + PadRight(Data[I][J], ColWidths[J] - 2) + ' '
      else
        Cell := StringOfChar(' ', ColWidths[J]);

      if I = HighlightRow then
        WriteColoredText(Cell, ConsoleTheme.TextHighlightColor, ConsoleTheme.BackgroundHighlightColor)
      else
        WriteColoredText(Cell, ConsoleTheme.TextColor);

      WriteColoredText(lBoxChars.Vertical, ConsoleTheme.DrawColor);
    end;
    WriteLn;
  end;

  // Bottom border
  WriteColoredText(lBoxChars.BottomLeft, ConsoleTheme.DrawColor);
  Line := '';
  for I := 0 to High(ColWidths) do
  begin
    Line := Line + StringOfChar(lBoxChars.Horizontal, ColWidths[I]);
    if I < High(ColWidths) then
      Line := Line + lBoxChars.BottomJoin
    else
      Line := Line + lBoxChars.BottomRight;
  end;
  WriteLine(Line, ConsoleTheme.DrawColor);
end;

procedure Table(const Headers: TStringArray; const Data: TStringMatrix);
begin
  Table(Headers, Data, '');
end;

procedure Table(const Headers: TStringArray; const Data: TStringMatrix; const Title: string);
var
  ColWidths: TArray<Integer>;
begin
  if Length(Headers) = 0 then Exit;

  ColWidths := CalcColumnWidths(Headers, Data);

  if Title <> '' then
    WriteLine(Title, ConsoleTheme.TextHighlightColor);

  InternalDrawTable(Headers, Data, ColWidths, GetBoxChars(bsUseDefault));
end;

// ============================================================================
// HIGH-LEVEL API: TABLE MENU
// ============================================================================

function TableMenu(const Headers: TStringArray; const Data: TStringMatrix): Integer;
begin
  Result := TableMenu('', Headers, Data, 0);
end;

function TableMenu(const Title: string; const Headers: TStringArray; const Data: TStringMatrix): Integer;
begin
  Result := TableMenu(Title, Headers, Data, 0);
end;

function TableMenu(const Title: string; const Headers: TStringArray;
  const Data: TStringMatrix; DefaultIndex: Integer): Integer;
var
  ColWidths: TArray<Integer>;
  lBoxChars: TBoxChars;
  SelectedIndex: Integer;
  Key: Integer;
  StartY: Integer;

  procedure DrawTable;
  begin
    GotoXY(0, StartY);
    if Title <> '' then
      WriteLine(Title, ConsoleTheme.TextHighlightColor);
    InternalDrawTable(Headers, Data, ColWidths, lBoxChars, SelectedIndex);
    WriteLine('Use arrows to navigate, Enter to select, ESC to cancel', DarkGray);
  end;

begin
  if (Length(Data) = 0) or (Length(Headers) = 0) then Exit(-1);

  lBoxChars := GetBoxChars(bsUseDefault);
  ColWidths := CalcColumnWidths(Headers, Data);

  ClrScr;
  StartY := 0;
  SelectedIndex := EnsureRange(DefaultIndex, 0, High(Data));

  HideCursor;
  try
    repeat
      DrawTable;
      Key := GetKey;
      case Key of
        KEY_UP: if SelectedIndex > 0 then Dec(SelectedIndex);
        KEY_DOWN: if SelectedIndex < High(Data) then Inc(SelectedIndex);
        KEY_ENTER: Exit(SelectedIndex);
        KEY_ESCAPE: Exit(-1);
      end;
    until False;
  finally
    ShowCursor;
  end;
end;

// ============================================================================
// HIGH-LEVEL API: BOX
// ============================================================================

procedure Box(const Content: TStringArray);
begin
  Box('', Content, 60);
end;

procedure Box(const Title: string; const Content: TStringArray);
begin
  Box(Title, Content, 60);
end;

procedure Box(const Title: string; const Content: TStringArray; Width: Integer);
var
  I: Integer;
  Line, ContentLine: string;
  lBoxChars: TBoxChars;
begin
  lBoxChars := GetBoxChars(bsUseDefault);

  // Top border
  Line := lBoxChars.TopLeft + StringOfChar(lBoxChars.Horizontal, Width - 2) + lBoxChars.TopRight;
  WriteLine(Line, ConsoleTheme.DrawColor);

  // Title
  if Title <> '' then
  begin
    ContentLine := ' ' + PadRight(Title, Width - 4) + ' ';
    WriteColoredText(lBoxChars.Vertical, ConsoleTheme.DrawColor);
    WriteColoredText(ContentLine, ConsoleTheme.TextHighlightColor);
    WriteLine(lBoxChars.Vertical, ConsoleTheme.DrawColor);

    Line := lBoxChars.LeftJoin + StringOfChar(lBoxChars.Horizontal, Width - 2) + lBoxChars.RightJoin;
    WriteLine(Line, ConsoleTheme.DrawColor);
  end;

  // Content
  for I := 0 to High(Content) do
  begin
    WriteColoredText(lBoxChars.Vertical, ConsoleTheme.DrawColor);
    ContentLine := ' ' + PadRight(Content[I], Width - 4) + ' ';
    WriteColoredText(ContentLine, ConsoleTheme.TextColor);
    WriteLine(lBoxChars.Vertical, ConsoleTheme.DrawColor);
  end;

  // Bottom border
  Line := lBoxChars.BottomLeft + StringOfChar(lBoxChars.Horizontal, Width - 2) + lBoxChars.BottomRight;
  WriteLine(Line, ConsoleTheme.DrawColor);
end;

// ============================================================================
// HIGH-LEVEL API: MENU
// ============================================================================

function Menu(const Items: TStringArray): Integer;
begin
  Result := Menu('', Items, 0);
end;

function Menu(const Title: string; const Items: TStringArray): Integer;
begin
  Result := Menu(Title, Items, 0);
end;

function Menu(const Title: string; const Items: TStringArray; DefaultIndex: Integer): Integer;
var
  SelectedIndex: Integer;
  Key: Integer;
  Done: Boolean;
  I: Integer;
  MaxWidth: Integer;
  StartX, StartY: Word;
  CurPos: TMVCConsolePoint;
  ConsSize: TMVCConsoleSize;
  Line: string;
  MenuHeight: Integer;
  BoxChars: TBoxChars;
  Hint: string;
begin
  Result := -1;
  if Length(Items) = 0 then Exit;

  Init;
  Hint := 'Use arrows to navigate, Enter to select, ESC to cancel';

  SelectedIndex := EnsureRange(DefaultIndex, 0, High(Items));

  // Calculate max width (must also cover the hint line for proper clearing)
  MaxWidth := Length(Hint);
  if Length(Title) > MaxWidth then
    MaxWidth := Length(Title);
  for I := 0 to High(Items) do
    if Length(Items[I]) + 6 > MaxWidth then
      MaxWidth := Length(Items[I]) + 6;
  Inc(MaxWidth, 4);

  // top border + items + bottom border + hint = N + 3
  // with title: + title + separator = N + 5
  MenuHeight := Length(Items) + 3;
  if Title <> '' then
    Inc(MenuHeight, 2);
  Inc(MenuHeight); // hint line

  ConsSize := GetConsoleSize;
  CurPos := GetCursorPosition;
  StartX := CurPos.X;
  StartY := CurPos.Y;

  if StartY + MenuHeight > ConsSize.Rows then
  begin
    StartY := ConsSize.Rows - MenuHeight - 1;
    GotoXY(StartX, StartY);
  end;

  HideCursor;
  try
    Done := False;
    while not Done do
    begin
      GotoXY(StartX, StartY);
      BoxChars := GetBoxChars(ConsoleTheme.BoxStyle);

      // Top border
      Line := BoxChars.TopLeft + StringOfChar(BoxChars.Horizontal, MaxWidth - 2) + BoxChars.TopRight;
      WriteLine(Line, ConsoleTheme.DrawColor);

      // Title
      if Title <> '' then
      begin
        WriteColoredText(BoxChars.Vertical + ' ', ConsoleTheme.DrawColor);
        WriteColoredText(PadRight(Title, MaxWidth - 4), ConsoleTheme.TextHighlightColor);
        WriteLine(' ' + BoxChars.Vertical, ConsoleTheme.DrawColor);

        Line := BoxChars.LeftJoin + StringOfChar(BoxChars.Horizontal, MaxWidth - 2) + BoxChars.RightJoin;
        WriteLine(Line, ConsoleTheme.DrawColor);
      end;

      // Items
      for I := 0 to High(Items) do
      begin
        WriteColoredText(BoxChars.Vertical + ' ', ConsoleTheme.DrawColor);
        if I = SelectedIndex then
        begin
          SaveColors;
          TextBackground(ConsoleTheme.BackgroundHighlightColor);
          TextColor(ConsoleTheme.TextHighlightColor);
          Write('> ' + PadRight(Items[I], MaxWidth - 6) + ' ');
          RestoreSavedColors;
        end
        else
          WriteColoredText('  ' + PadRight(Items[I], MaxWidth - 6) + ' ', ConsoleTheme.TextColor);
        WriteLine(BoxChars.Vertical, ConsoleTheme.DrawColor);
      end;

      // Bottom border
      Line := BoxChars.BottomLeft + StringOfChar(BoxChars.Horizontal, MaxWidth - 2) + BoxChars.BottomRight;
      WriteLine(Line, ConsoleTheme.DrawColor);

      WriteLine(Hint, DarkGray);
      FlushOutput;

      Key := GetKey;
      case Key of
        KEY_UP:
          begin
            Dec(SelectedIndex);
            if SelectedIndex < 0 then
              SelectedIndex := High(Items);
          end;
        KEY_DOWN:
          begin
            Inc(SelectedIndex);
            if SelectedIndex > High(Items) then
              SelectedIndex := 0;
          end;
        KEY_ENTER:
          begin
            Done := True;
            Result := SelectedIndex;
          end;
        KEY_ESCAPE:
          begin
            Done := True;
            Result := -1;
          end;
      end;
    end;

    // Clear menu area
    for I := 0 to MenuHeight - 1 do
    begin
      if StartY + I < ConsSize.Rows then
      begin
        GotoXY(StartX, StartY + I);
        Write(StringOfChar(' ', Min(MaxWidth, ConsSize.Columns - StartX)));
      end;
    end;
    if StartY < ConsSize.Rows then
      GotoXY(StartX, StartY);

  finally
    ShowCursor;
  end;
end;

// ============================================================================
// HIGH-LEVEL API: PROGRESS
// ============================================================================

type
  TConsoleProgress = class(TInterfacedObject, IProgress)
  private
    FTitle: string;
    FMaxValue: Integer;
    FCurrent: Integer;
    FStartX, FStartY: Integer;
    FWidth: Integer;
    FCompleted: Boolean;
    FSpinnerIndex: Integer;
    FSpinnerChars: string;
    procedure DrawDeterminate;
    procedure DrawIndeterminate;
  public
    constructor Create(const ATitle: string; AMaxValue: Integer; AWidth: Integer = 50);
    destructor Destroy; override;
    procedure Update(Value: Integer);
    procedure Increment(Amount: Integer = 1);
    procedure SetMessage(const Msg: string);
    procedure Complete;
  end;

constructor TConsoleProgress.Create(const ATitle: string; AMaxValue: Integer; AWidth: Integer);
begin
  inherited Create;
  FTitle := ATitle;
  FMaxValue := AMaxValue;
  FCurrent := 0;
  FWidth := AWidth;
  FCompleted := False;
  FSpinnerIndex := 0;
  FSpinnerChars := '|/-\';

  FStartX := GetCursorPosition.X;
  FStartY := GetCursorPosition.Y;

  WriteLine(FTitle, ConsoleTheme.TextColor);
  if FMaxValue > 0 then
    DrawDeterminate
  else
    DrawIndeterminate;
end;

destructor TConsoleProgress.Destroy;
begin
  if not FCompleted then
    Complete;
  inherited;
end;

procedure TConsoleProgress.DrawDeterminate;
var
  Percent, FilledWidth: Integer;
  Bar: string;
begin
  if FMaxValue = 0 then Exit;
  Percent := (FCurrent * 100) div FMaxValue;
  FilledWidth := (FCurrent * FWidth) div FMaxValue;
  Bar := '[' + StringOfChar('=', FilledWidth) + StringOfChar(' ', FWidth - FilledWidth) + ']';
  GotoXY(FStartX, FStartY + 1);
  WriteColoredText(Bar, ConsoleTheme.TextHighlightColor);
  Write(Format(' %d%%', [Percent]));
end;

procedure TConsoleProgress.DrawIndeterminate;
begin
  GotoXY(FStartX, FStartY + 1);
  WriteColoredText('[', ConsoleTheme.DrawColor);
  WriteColoredText(FSpinnerChars[FSpinnerIndex + 1], ConsoleTheme.TextHighlightColor);
  WriteColoredText(']', ConsoleTheme.DrawColor);
  Write(' Processing...');
  FSpinnerIndex := (FSpinnerIndex + 1) mod Length(FSpinnerChars);
end;

procedure TConsoleProgress.Update(Value: Integer);
begin
  if FCompleted then Exit;
  FCurrent := Value;
  if FMaxValue > 0 then
    DrawDeterminate
  else
    DrawIndeterminate;
end;

procedure TConsoleProgress.Increment(Amount: Integer);
begin
  Update(FCurrent + Amount);
end;

procedure TConsoleProgress.SetMessage(const Msg: string);
begin
  GotoXY(FStartX, FStartY);
  Write(Msg.PadRight(FWidth + 10));
  FTitle := Msg;
end;

procedure TConsoleProgress.Complete;
begin
  if FCompleted then Exit;
  FCompleted := True;
  if FMaxValue > 0 then
  begin
    FCurrent := FMaxValue;
    DrawDeterminate;
  end;
  GotoXY(FStartX, FStartY + 2);
  WriteLine('Done!', Green);
end;

function Progress(const Title: string; MaxValue: Integer): IProgress;
begin
  Result := TConsoleProgress.Create(Title, MaxValue);
end;

function Progress(const Title: string): IProgress;
begin
  Result := TConsoleProgress.Create(Title, 0);
end;

// ============================================================================
// HIGH-LEVEL API: CONFIRM & CHOOSE
// ============================================================================

function Confirm(const Question: string): Boolean;
begin
  Result := Confirm(Question, True);
end;

function Confirm(const Question: string; DefaultYes: Boolean): Boolean;
var
  Response: string;
begin
  Write(Question + ' [Y/N]');
  if DefaultYes then
    Write(' (Y): ')
  else
    Write(' (N): ');
  ReadLn(Response);
  Response := Trim(UpperCase(Response));
  if Response = '' then
    Result := DefaultYes
  else
    Result := (Response = 'Y') or (Response = 'YES');
end;

function Choose(const Question: string; const Options: TStringArray): Integer;
var
  I: Integer;
  Response: string;
  Choice: Integer;
begin
  WriteLn(Question);
  for I := 0 to High(Options) do
    WriteLn(Format('  [%d] %s', [I + 1, Options[I]]));
  Write('Your choice: ');
  ReadLn(Response);
  if TryStrToInt(Trim(Response), Choice) then
  begin
    if (Choice >= 1) and (Choice <= Length(Options)) then
      Result := Choice - 1
    else
      Result := -1;
  end
  else
    Result := -1;
end;

// ============================================================================
// HIGH-LEVEL API: SPINNER (non-blocking, thread-based)
// ============================================================================

type
  TSpinnerFrames = TArray<string>;

  TConsoleSpinner = class(TInterfacedObject, ISpinner)
  private
    FFlag: Integer;
    FThread: TThread;
    FFrames: TSpinnerFrames;
    FColor: TConsoleColor;
    FMessage: string;
    FInterval: Integer;
    FSpinnerX, FSpinnerY: Word;
    FMaxDisplayWidth: Integer;
  public
    constructor Create(const AMessage: string; AStyle: TSpinnerStyle; AColor: TConsoleColor);
    destructor Destroy; override;
    procedure Hide;
  end;

function GetSpinnerFrames(AStyle: TSpinnerStyle): TSpinnerFrames;
begin
  case AStyle of
    ssLine:
      Result := TSpinnerFrames.Create('-', '\', '|', '/');
    ssDots:
      Result := TSpinnerFrames.Create(
        #$280B, #$2819, #$2839, #$2838, #$283C,
        #$2834, #$2826, #$2827, #$2807, #$280F);
    ssBounce:
      Result := TSpinnerFrames.Create(
        #$2801, #$2802, #$2804, #$2840,
        #$2880, #$2820, #$2810, #$2808);
    ssGrow:
      Result := TSpinnerFrames.Create(
        #$258F, #$258E, #$258D, #$258C,
        #$258B, #$258A, #$2589, #$2588);
    ssArrow:
      Result := TSpinnerFrames.Create(
        #$2190, #$2196, #$2191, #$2197,
        #$2192, #$2198, #$2193, #$2199);
    ssCircle:
      Result := TSpinnerFrames.Create(#$25D0, #$25D3, #$25D1, #$25D2);
    ssClock:
      Result := TSpinnerFrames.Create(
        #$D83D#$DD50, #$D83D#$DD51, #$D83D#$DD52, #$D83D#$DD53,
        #$D83D#$DD54, #$D83D#$DD55, #$D83D#$DD56, #$D83D#$DD57,
        #$D83D#$DD58, #$D83D#$DD59, #$D83D#$DD5A, #$D83D#$DD5B);
    ssEarth:
      Result := TSpinnerFrames.Create(
        #$D83C#$DF0D, #$D83C#$DF0E, #$D83C#$DF0F);
    ssMoon:
      Result := TSpinnerFrames.Create(
        #$D83C#$DF11, #$D83C#$DF12, #$D83C#$DF13, #$D83C#$DF14,
        #$D83C#$DF15, #$D83C#$DF16, #$D83C#$DF17, #$D83C#$DF18);
    ssWeather:
      Result := TSpinnerFrames.Create(
        #$D83C#$DF24, #$D83C#$DF27, #$26C8,
        #$D83C#$DF29, #$D83C#$DF28, #$D83C#$DF2A);
  else
    Result := TSpinnerFrames.Create('-', '\', '|', '/');
  end;
end;

function GetSpinnerInterval(AStyle: TSpinnerStyle): Integer;
begin
  case AStyle of
    ssClock, ssEarth, ssMoon, ssWeather: Result := 200;
    ssDots, ssBounce: Result := 80;
    ssGrow: Result := 120;
  else
    Result := 100;
  end;
end;

function DisplayWidth(const S: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  I := 1;
  while I <= Length(S) do
  begin
    if (I < Length(S)) and Char.IsHighSurrogate(S, I - 1) then
    begin
      Inc(Result, 2);
      Inc(I, 2);
    end
    else
    begin
      Inc(Result, 1);
      Inc(I);
    end;
  end;
end;

constructor TConsoleSpinner.Create(const AMessage: string; AStyle: TSpinnerStyle; AColor: TConsoleColor);
var
  lFrames: TSpinnerFrames;
  lInterval: Integer;
  lColor: TConsoleColor;
  lSpinnerX, lSpinnerY: Word;
  lMaxWidth: Integer;
  CurPos: TMVCConsolePoint;
  lFrame: string;
begin
  inherited Create;
  FFrames := GetSpinnerFrames(AStyle);
  FColor := AColor;
  FMessage := AMessage;
  FInterval := GetSpinnerInterval(AStyle);
  TInterlocked.Exchange(FFlag, 1);

  if FMessage <> '' then
  begin
    TextColor(FColor);
    Write(FMessage + ' ');
  end;

  HideCursor;

  CurPos := GetCursorPosition;
  FSpinnerX := CurPos.X;
  FSpinnerY := CurPos.Y;

  FMaxDisplayWidth := 0;
  for lFrame in FFrames do
  begin
    lMaxWidth := DisplayWidth(lFrame);
    if lMaxWidth > FMaxDisplayWidth then
      FMaxDisplayWidth := lMaxWidth;
  end;

  lFrames := FFrames;
  lInterval := FInterval;
  lColor := FColor;
  lSpinnerX := FSpinnerX;
  lSpinnerY := FSpinnerY;
  lMaxWidth := FMaxDisplayWidth;

  FThread := TThread.CreateAnonymousThread(
    procedure
    var
      I: Integer;
      Frame: string;
    begin
      I := 0;
      while TInterlocked.CompareExchange(FFlag, 1, 1) = 1 do
      begin
        Frame := lFrames[I mod Length(lFrames)];
        GotoXY(lSpinnerX, lSpinnerY);
        TextColor(lColor);
        Write(Frame + StringOfChar(' ', lMaxWidth - DisplayWidth(Frame)));
        Flush(Output);
        Inc(I);
        Sleep(lInterval);
      end;
    end
  );
  FThread.FreeOnTerminate := False;
  FThread.Start;
end;

destructor TConsoleSpinner.Destroy;
begin
  Hide;
  inherited;
end;

procedure TConsoleSpinner.Hide;
begin
  if TInterlocked.CompareExchange(FFlag, 0, 1) = 0 then
    Exit;
  if Assigned(FThread) then
  begin
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
  GotoXY(FSpinnerX, FSpinnerY);
  Write(StringOfChar(' ', FMaxDisplayWidth));
  GotoXY(FSpinnerX, FSpinnerY);
  ShowCursor;
  Flush(Output);
end;

function Spinner(AStyle: TSpinnerStyle; AColor: TConsoleColor): ISpinner;
begin
  Result := TConsoleSpinner.Create('', AStyle, AColor);
end;

function Spinner(const AMessage: string; AStyle: TSpinnerStyle; AColor: TConsoleColor): ISpinner;
begin
  Result := TConsoleSpinner.Create(AMessage, AStyle, AColor);
end;

// ============================================================================

initialization
  GLock := TObject.Create;
  GSavedCursorX := 0;
  GSavedCursorY := 0;
  GForeGround := Ord(ConsoleTheme.TextColor);
  GBackGround := Ord(ConsoleTheme.BackgroundColor);

finalization
{$IFDEF LINUX}
  RestoreTerminal;
{$ENDIF}
  GLock.Free;

end.
