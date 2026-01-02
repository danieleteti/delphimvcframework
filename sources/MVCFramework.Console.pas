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
// This unit provides cross-platform console functionality including:
// - Colored text output
// - Tables, boxes, progress bars, and menus
// - Interactive menus with keyboard navigation
// - Robust keyboard input handling using ReadConsoleInput on Windows
// - Full support for special keys (arrows, function keys, etc.)
//
// *************************************************************************** }

unit MVCFramework.Console;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.SyncObjs,
  System.Classes
{$IFDEF MSWINDOWS}
  ,WinApi.Windows
{$ENDIF}
{$IFDEF LINUX}
  ,Posix.Unistd,
  Posix.Termios,
  Posix.SysStat,
  Posix.Fcntl
{$ENDIF}
    ;

const
  // Special key codes (256 + VirtualKeyCode to avoid conflicts with ASCII)
  KEY_UP    = 256 + 38;  // VK_UP
  KEY_DOWN  = 256 + 40;  // VK_DOWN
  KEY_LEFT  = 256 + 37;  // VK_LEFT
  KEY_RIGHT = 256 + 39;  // VK_RIGHT
  KEY_ESCAPE = 27;       // Standard ESC
  KEY_ENTER = 13;        // Standard Enter

type
  // https://stackoverflow.com/questions/17125440/c-win32-console-color
  // https://docs.microsoft.com/en-us/dotnet/api/system.consolecolor?view=netcore-3.1
  TConsoleColor = (
    Black = 0, // The color black.
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
    White = 15, // The color white.
    UseDefault = 16 //Use Style Color
    );

  TProgressBarStyle = (pbsSimple, pbsBlocks, pbsArrows, pbsCircles);

  TBoxStyle = (bsSingle, bsDouble, bsRounded, bsThick, bsUseDefault);

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

  // Enhanced console features
  TMVCConsoleProgressBar = class
  private
    FPosition: Integer;
    FMaxValue: Integer;
    FStartX, FStartY: Integer;
    FWidth: Integer;
    FStyle: TProgressBarStyle;
    FTitle: string;
    procedure UpdateDisplay;
  public
    constructor Create(const ATitle: string; AMaxValue: Integer; AWidth: Integer = 50;
                      AStyle: TProgressBarStyle = pbsBlocks);
    procedure SetPosition(AValue: Integer);
    procedure Increment(AValue: Integer = 1);
    procedure Finish;
    property Position: Integer read FPosition;
    property MaxValue: Integer read FMaxValue;
  end;

  TStringArray = array of string;
  TStringMatrix = array of TStringArray;
  TAlignment = (taLeft, taCenter, taRight);
  TListStyle = (lsBullet, lsNumbered, lsDash, lsArrow);
  TConsoleColorArray = array of TConsoleColor;
  TIntegerArray = array of Integer;

  // Menu types
  TMenuItemStyle = record
    Text: string;
    Icon: string;
    Enabled: Boolean;
  end;
  TMenuItemsArray = array of TMenuItemStyle;

  TConsoleColorStyle = record
    TextColor, BackgroundColor, DrawColor, SymbolsColor, BackgroundHighlightColor, TextHighlightColor: TConsoleColor;
    BoxStyle: TBoxStyle;
  end;


var
  //CONSOLE STYLE
  MVCConsoleStyle: TConsoleColorStyle = (
    TextColor : TConsoleColor.Cyan;
    BackgroundColor : TConsoleColor.Black;
    DrawColor : TConsoleColor.White;
    SymbolsColor : TConsoleColor.Gray;
    BackgroundHighlightColor: TConsoleColor.Cyan;
    TextHighlightColor: TConsoleColor.Blue;
    BoxStyle: TBoxStyle.bsRounded;
  );


// Basic console functions
procedure ResetConsole;
procedure TextColor(const color: TConsoleColor);
procedure TextBackground(const color: TConsoleColor);
procedure GotoXY(const X, Y: Word);
function GetConsoleSize: TMVCConsoleSize;
function GetConsoleBufferSize: TMVCConsoleSize;
function GetCursorPosition: TMVCConsolePoint;
procedure ClrScr;
function GetCh: Char;

function GetKey: Integer;  // Returns key code, special keys are 256+VirtualKeyCode
function IsSpecialKey(KeyCode: Integer): Boolean; inline;  // True if key is a special key (>= 256)
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
procedure CenterInScreen(const Text: String);
function KeyPressed: boolean;
procedure EnableUTF8Console;

// Enhanced functions
procedure WriteColoredText(const Text: string; ForeColor: TConsoleColor = UseDefault;
                          BackColor: TConsoleColor = UseDefault);
procedure WriteLineColored(const Text: string; ForeColor: TConsoleColor = UseDefault;
                          BackColor: TConsoleColor = UseDefault);
procedure DrawBox(X, Y, Width, Height: Word; Style: TBoxStyle = bsRounded;
                 const Title: string = '');
procedure DrawHorizontalLine(X, Y, Length: Word; Style: TBoxStyle = bsUseDefault);
procedure DrawVerticalLine(X, Y, Length: Word; Style: TBoxStyle = bsUseDefault);
procedure ClearRegion(X, Y, Width, Height: Word);
procedure SaveCursorPosition;
procedure RestoreCursorPosition;

// Information and utility functions
function ColorName(const Color: TConsoleColor): String;
function IsTerminalCapable: Boolean;
function GetTerminalName: string;
procedure Beep;
procedure FlashScreen;

// Enhanced library functions - ASCII-based for maximum compatibility
procedure WriteSimpleTable(const Headers: TStringArray; const Data: TStringMatrix; const Style: TBoxStyle = bsUseDefault);
procedure ShowSimpleProgressBar(const Title: string; Position, MaxValue: Integer; Width: Integer = 50);
procedure DrawSimpleBox(const Title: string; const Content: TStringArray; Width: Integer = 80; TextColor: TConsoleColor = UseDefault; Style: TBoxStyle = bsUseDefault);
procedure WriteAlignedText(const Text: string; Width: Integer; Alignment: TAlignment = taCenter; TextColor: TConsoleColor = UseDefault);
procedure ShowProgressAnimation(const Title: string; Steps: Integer = 20; DelayMs: Integer = 100);
procedure WriteStatusLine(const Items: TStringArray; const Statuses: TStringArray;
                         const Colors: TConsoleColorArray);
procedure ShowSimpleMenu(const Title: string; const Items: TStringArray; SelectedIndex: Integer = 0);
procedure WriteFormattedList(const Title: string; const Items: TStringArray;
                           ListStyle: TListStyle);

// Interactive menu functions
// These functions use GetKey to handle special keys like arrows
function ShowInteractiveMenu(const Title: string; const Items: TStringArray;
                           DefaultIndex: Integer = 0;
                           const Hint: string = 'Use arrows to navigate, Enter to select, ESC to cancel'): Integer;

function ShowAdvancedMenu(const Title: string; const Items: TMenuItemsArray;
                        DefaultIndex: Integer = 0;
                        HighlightColor: TConsoleColor = DarkCyan;
                        const Hint: string = 'Use arrows to navigate, Enter to select, ESC to cancel'): Integer;

// Helper function for menu items
function CreateMenuItem(const Text: string; const Icon: string = ''; Enabled: Boolean = True): TMenuItemStyle;

// Dashboard and report utilities
procedure ShowSystemDashboard(const Title: string; const ServerStatuses: TStringArray;
                              const ServerColors: TConsoleColorArray;
                              const MetricNames: TStringArray; const MetricValues: TIntegerArray);
procedure WriteReport(const Title: string; const Sections: TStringArray;
                     const SectionContents: TStringMatrix);
procedure ShowLoadingSpinner(const Message: string; Iterations: Integer = 20);
procedure WriteColoredTable(const Headers: TStringArray;
                           const Data: TStringMatrix;
                           HeaderColor: TConsoleColor = UseDefault;
                           DrawColor: TConsoleColor = UseDefault;
                           DataColor: TConsoleColor = UseDefault;
                           BoxStyle: TBoxStyle = bsUseDefault);

// Quick utility functions
procedure WriteHeader(const Text: string; Width: Integer = 80; HeaderColor: TConsoleColor = UseDefault);
procedure WriteSeparator(Width: Integer = 60; CharSymbol: Char = '-');
procedure WriteSuccess(const Message: string);
procedure WriteWarning(const Message: string);
procedure WriteError(const Message: string);
procedure WriteInfo(const Message: string);

// Utility functions
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
  hConsoleInput: THandle;
  GSavedCursorX, GSavedCursorY: Word;

{$IFDEF LINUX}
var
  GOriginalTermios: termios;
  GTerminalSetup: Boolean = False;

const
  // ANSI Color codes for Linux
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
    '97'      // White
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
    '107'     // White
  );

{$ENDIF}

function GetBoxStyleOrDefault(BoxStyle: TBoxStyle): TBoxStyle;
begin
  Result := BoxStyle;
  if Result = bsUseDefault then
  begin
    Result := MVCConsoleStyle.BoxStyle;
  end;
end;

//On Windows Requires UTF8 Console and proper font
function GetBoxChars(Style: TBoxStyle): TBoxChars;
begin
  Style := GetBoxStyleOrDefault(Style);
  case Style of
    bsSingle: begin
      Result.TopLeft := '┌';
      Result.TopRight := '┐';
      Result.BottomLeft := '└';
      Result.BottomRight := '┘';
      Result.Vertical := '│';
      Result.Horizontal := '─';
      Result.LeftJoin := '├';
      Result.RightJoin := '┤';
      Result.TopJoin := '┬';
      Result.BottomJoin := '┴';
      Result.Cross := '┼';
    end;
    bsDouble: begin
      Result.TopLeft := '╔';
      Result.TopRight := '╗';
      Result.BottomLeft := '╚';
      Result.BottomRight := '╝';
      Result.Vertical := '║';
      Result.Horizontal := '═';
      Result.LeftJoin := '╠';
      Result.RightJoin := '╣';
      Result.TopJoin := '╦';
      Result.BottomJoin := '╩';
      Result.Cross := '╬';
    end;
    bsRounded: begin
      Result.TopLeft := '╭';
      Result.TopRight := '╮';
      Result.BottomLeft := '╰';
      Result.BottomRight := '╯';
      Result.Vertical := '│';
      Result.Horizontal := '─';
      Result.LeftJoin := '├';
      Result.RightJoin := '┤';
      Result.TopJoin := '┬';
      Result.BottomJoin := '┴';
      Result.Cross := '┼';
    end;
    bsThick: begin
      Result.TopLeft := '┏';
      Result.TopRight := '┓';
      Result.BottomLeft := '┗';
      Result.BottomRight := '┛';
      Result.Vertical := '┃';
      Result.Horizontal := '━';
      Result.LeftJoin := '┣';
      Result.RightJoin := '┫';
      Result.TopJoin := '┳';
      Result.BottomJoin := '┻';
      Result.Cross := '╋';
    end;
  end;
end;

// ============================================================================
// Utility functions
// ============================================================================


function GetColorOrDefault(Color: TConsoleColor; StyleColorComponent: TStyleColorComponent): TConsoleColor;
begin
  if Color = TConsoleColor.UseDefault then
  begin
    case StyleColorComponent of
      sccText: Result := MVCConsoleStyle.TextColor;
      sccBackground: Result := MVCConsoleStyle.BackgroundColor;
      sccDraw: Result := MVCConsoleStyle.DrawColor;
      sccSymbol: Result := MVCConsoleStyle.SymbolsColor;
      sccHighLightBackground: Result := MVCConsoleStyle.BackgroundHighlightColor;
      sccHighLightText: Result := MVCConsoleStyle.TextHighlightColor;
      else
        raise EMVCConsole.Create('Unknown StyleColorComponent');
    end;
  end
  else
  begin
    Result := Color;
  end;
end;

function PadRight(const S: string; Len: Integer): string;
begin
  Result := S;
  if Length(Result) < Len then
    Result := Result + StringOfChar(' ', Len - Length(Result))
  else if Length(Result) > Len then
    Result := Copy(Result, 1, Len);
end;

procedure WriteTableBottomBorder(ColWidths: array of Integer; const Style: TBoxStyle);
var
  lBoxChars: TBoxChars;
  Line: string;
  I: Integer;
begin
  lBoxChars := GetBoxChars(Style);
  WriteColoredText(lBoxChars.BottomLeft, MVCConsoleStyle.DrawColor);
  Line := '';
  for I := 0 to High(ColWidths) do
  begin
    Line := Line + StringOfChar(lBoxChars.Horizontal, ColWidths[I]);
    if I < High(ColWidths) then
      Line := Line + lBoxChars.BottomJoin
    else
      Line := Line + lBoxChars.BottomRight
  end;
  WriteLineColored(Line, MVCConsoleStyle.DrawColor);
end;

procedure WriteTableTopBorder(ColWidths: array of Integer; const Style: TBoxStyle);
var
  lBoxChars: TBoxChars;
  Line: string;
  I: Integer;
begin
  lBoxChars := GetBoxChars(Style);
  WriteColoredText(lBoxChars.TopLeft, MVCConsoleStyle.DrawColor);
  Line := '';
  for I := 0 to High(ColWidths) do
  begin
    Line := Line + StringOfChar(lBoxChars.Horizontal, ColWidths[I]);
    if I < High(ColWidths) then
      Line := Line + lBoxChars.TopJoin
    else
      Line := Line + lBoxChars.TopRight
  end;
  WriteLineColored(Line, MVCConsoleStyle.DrawColor);
end;


// ============================================================================
// Progress Bar Implementation
// ============================================================================

constructor TMVCConsoleProgressBar.Create(const ATitle: string; AMaxValue: Integer;
                                         AWidth: Integer; AStyle: TProgressBarStyle);
var
  CurPos: TMVCConsolePoint;
begin
  inherited Create;
  FTitle := ATitle;
  FMaxValue := AMaxValue;
  FWidth := AWidth;
  FStyle := AStyle;
  FPosition := 0;

  CurPos := GetCursorPosition;
  FStartX := CurPos.X;
  FStartY := CurPos.Y;

  if FTitle <> '' then
  begin
    WriteLineColored(FTitle, White);
    Inc(FStartY);
  end;

  UpdateDisplay;
end;

procedure TMVCConsoleProgressBar.UpdateDisplay;
var
  Progress: Double;
  FilledChars: Integer;
  I: Integer;
  ProgressChar, EmptyChar: string;
  ProgressLine: string;
begin
  if FMaxValue = 0 then
    Progress := 0
  else
    Progress := FPosition / FMaxValue;
  FilledChars := Round(Progress * FWidth);

  case FStyle of
    pbsSimple: begin
      ProgressChar := '#';
      EmptyChar := '.';
    end;
    pbsBlocks: begin
      ProgressChar := '#';  // Fallback to ASCII
      EmptyChar := '-';
    end;
    pbsArrows: begin
      ProgressChar := '>';
      EmptyChar := '-';
    end;
    pbsCircles: begin
      ProgressChar := 'o';  // Fallback to ASCII
      EmptyChar := '.';
    end;
  end;

  // Build progress line in memory first
  ProgressLine := '[';
  for I := 0 to FWidth - 1 do
  begin
    if I < FilledChars then
      ProgressLine := ProgressLine + ProgressChar
    else
      ProgressLine := ProgressLine + EmptyChar;
  end;
  ProgressLine := ProgressLine + Format('] %3.0f%% (%d/%d)', [Progress * 100, FPosition, FMaxValue]);

  // Clear the line first
  GotoXY(FStartX, FStartY);
  Write(StringOfChar(' ', Length(ProgressLine) + 5));

  // Write the progress line
  GotoXY(FStartX, FStartY);
  Write(ProgressLine);

  // Force flush
{$IFDEF MSWINDOWS}
  Flush(Output);
{$ENDIF}
{$IFDEF LINUX}
  Flush(Output);
{$ENDIF}
end;

procedure TMVCConsoleProgressBar.SetPosition(AValue: Integer);
begin
  FPosition := Min(AValue, FMaxValue);
  UpdateDisplay;
end;

procedure TMVCConsoleProgressBar.Increment(AValue: Integer);
begin
  SetPosition(FPosition + AValue);
end;

procedure TMVCConsoleProgressBar.Finish;
begin
  SetPosition(FMaxValue);
  WriteLn;
end;

// ============================================================================
// Platform-specific implementations
// ============================================================================

// GetKey returns the full key code. For special keys like arrows, it returns
// 256 + VirtualKeyCode to distinguish them from ASCII characters.
// GetCh returns only ASCII characters for backward compatibility.

function ColorName(const color: TConsoleColor): String;
begin
  Result := GetEnumName(TypeInfo(TConsoleColor), Ord(color));
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

function KeyPressed: boolean;
var
  FDSet: fd_set;
  TimeVal: timeval;
begin
  SetupTerminal;
  __FD_ZERO(FDSet);
  __FD_SET(STDIN_FILENO, FDSet);
  TimeVal.tv_sec := 0;
  TimeVal.tv_usec := 0;
  Result := select(STDIN_FILENO + 1, @FDSet, nil, nil, @TimeVal) > 0;
end;

procedure EnableUTF8Console;
begin
  // Linux terminals usually handle UTF-8 by default
  WriteLn(ESC + '[?1049h'); // Enable alternative screen buffer
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
begin
  ReadLn;
end;

procedure UpdateMode;
begin
  Write(ESC + '[' + ANSI_COLORS[TConsoleColor(GForeGround)] + ';' +
        ANSI_BG_COLORS[TConsoleColor(GBackGround)] + 'm');
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
  WinSize: winsize;
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
  Result := GetConsoleSize; // Same as window size on Linux
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
  Write(ESC + '[6n'); // Query cursor position

  Response := '';
  NumberIndex := 0;
  CurrentNumber := '';

  // Read response: ESC[row;colR
  repeat
    Ch := GetCh;
    Response := Response + Ch;
  until (Ch = 'R') or (Length(Response) > 20);

  // Parse response
  for I := 1 to Length(Response) do
  begin
    Ch := Response[I];
    if Ch in ['0'..'9'] then
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
    Result.Y := Numbers[0] - 1; // Convert to 0-based
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
  Write(ESC + '[2J'); // Clear entire screen
  Write(ESC + '[H');  // Move to home position
end;

{$ENDIF}

{$IFDEF MSWINDOWS}

{.$IF not Defined(RIOORBETTER)}
const
  ATTACH_PARENT_PROCESS = DWORD(-1);
function AttachConsole(dwProcessId: DWORD): BOOL; stdcall; external kernel32 name 'AttachConsole';
{.$ENDIF}

procedure EnableUTF8Console;
begin
  SetConsoleOutputCP(CP_UTF8);
end;

procedure WinCheck(const Value: LongBool);
begin
  if not Value then
    raise EMVCConsole.CreateFmt('GetLastError() = %d', [GetLastError]);
end;

procedure KeyInit;
var
  mode: DWORD;
begin
  Reset(Input);
  GInputHandle := TTextRec(Input).Handle;
  hConsoleInput := GInputHandle; // Initialize hConsoleInput

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

function KeyPressed: boolean;
{$IFDEF MSWINDOWS}
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
      // Discard non-keyboard events or key-up events
      ReadConsoleInput(GInputHandle, InputRecord, 1, NumRead);
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  FDSet: fd_set;
  TimeVal: timeval;
begin
  SetupTerminal;
  __FD_ZERO(FDSet);
  __FD_SET(STDIN_FILENO, FDSet);
  TimeVal.tv_sec := 0;
  TimeVal.tv_usec := 0;
  Result := select(STDIN_FILENO + 1, @FDSet, nil, nil, @TimeVal) > 0;
end;
{$ENDIF}

procedure InternalShowCursor(const ShowCursor: Boolean);
var
  info: CONSOLE_CURSOR_INFO;
begin
  Init;
  GetConsoleCursorInfo(GOutHandle, info);
  info.bVisible := ShowCursor;
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
  Key: Integer;
begin
  Key := GetKey;
  if Key < 256 then
    Result := Chr(Key)
  else
    Result := #0;  // Special key, use GetKey for full code
end;

function GetKey: Integer;
{$IFDEF MSWINDOWS}
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
            // Normal ASCII key
            Result := Ord(KeyEvent.AsciiChar);
            Exit;
          end
          else
          begin
            // Special key like arrow keys (no ASCII)
            // Return 256 + VirtualKeyCode
            Result := 256 + KeyEvent.wVirtualKeyCode;
            Exit;
          end;
        end;
      end;
    end;
  until False;
end;
{$ENDIF}
{$IFDEF LINUX}
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

    // Handle Linux escape sequences for arrow keys
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
                'A': Result := KEY_UP;    // Up arrow
                'B': Result := KEY_DOWN;  // Down arrow
                'C': Result := KEY_RIGHT; // Right arrow
                'D': Result := KEY_LEFT;  // Left arrow
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;
{$ENDIF}

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
  begin
    raise EMVCConsole.Create('Invalid Coordinates');
  end;
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
// HIGH LEVEL CROSS-PLATFORM FUNCTIONS
// ============================================================================

procedure CenterInScreen(const Text: String);
var
  Size: TMVCConsoleSize;
begin
  Init;
  Size := GetConsoleSize;
  GotoXY(Size.Columns div 2 - Length(Text) div 2, Size.Rows div 2 - 1);
  Write(Text)
end;

procedure ResetConsole;
begin
  SetDefaultColors;
{$IFDEF LINUX}
  Write(ESC + '[0m'); // Reset all attributes
{$ENDIF}
end;

procedure TextColor(const color: TConsoleColor);
begin
  GForeGround := Ord(color);
  UpdateMode;
end;

procedure TextBackground(const color: TConsoleColor);
begin
  GBackGround := Ord(color) shl 4;
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

// ============================================================================
// ENHANCED FUNCTIONS
// ============================================================================

procedure WriteColoredText(const Text: string; ForeColor: TConsoleColor;
                          BackColor: TConsoleColor);
begin
  Init; // Ensure console is initialized
  SaveColors;
  try
    TextColor(GetColorOrDefault(ForeColor, sccText));
    TextBackground(GetColorOrDefault(BackColor, sccBackground));
    Write(Text);
  finally
    RestoreSavedColors;
  end;
end;

procedure WriteLineColored(const Text: string; ForeColor: TConsoleColor;
                          BackColor: TConsoleColor);
begin
  WriteColoredText(Text, ForeColor, BackColor);
  WriteLn;
end;

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

  // Top border
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

  // Side borders
  for I := 1 to Height - 2 do
  begin
    GotoXY(X, Y + I);
    Write(BoxChars.Vertical);
    GotoXY(X + Width - 1, Y + I);
    Write(BoxChars.Vertical);
  end;

  // Bottom border
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

function IsTerminalCapable: Boolean;
begin
{$IFDEF LINUX}
  Result := isatty(STDOUT_FILENO) = 1;
{$ELSE}
  Result := True; // Assume Windows console is capable
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
  Write(ESC + '[?5h'); // Enable reverse video
  Sleep(100);
  Write(ESC + '[?5l'); // Disable reverse video
{$ELSE}
  // Flash by inverting colors briefly
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
// ENHANCED LIBRARY FUNCTIONS - ASCII Based for Maximum Compatibility
// ============================================================================

procedure WriteSimpleTable(const Headers: TStringArray; const Data: TStringMatrix; const Style: TBoxStyle);
var
  ColWidths: array of Integer;
  I, J: Integer;
  Line, Cell: string;
  lBoxChars: TBoxChars;
begin
  if Length(Headers) = 0 then Exit;

  lBoxChars := GetBoxChars(Style);

  // Calculate column widths
  SetLength(ColWidths, Length(Headers));
  for I := 0 to High(Headers) do
  begin
    ColWidths[I] := Length(Headers[I]);
    for J := 0 to High(Data) do
    begin
      if (I < Length(Data[J])) and (Length(Data[J][I]) > ColWidths[I]) then
        ColWidths[I] := Length(Data[J][I]);
    end;
    Inc(ColWidths[I], 2); // Add padding
  end;

  // Top border
  WriteTableTopBorder(ColWidths, Style);

  // Headers
  WriteColoredText(lBoxChars.Vertical, MVCConsoleStyle.DrawColor);
  Line := '';
  for I := 0 to High(Headers) do
  begin
    Cell := ' ' + PadRight(Headers[I], ColWidths[I] - 2) + ' ';
    WriteColoredText(Cell, MVCConsoleStyle.TextHighlightColor);
    if I < High(ColWidths) then
      WriteColoredText(lBoxChars.Vertical, MVCConsoleStyle.DrawColor)
  end;
  WriteLineColored(lBoxChars.Vertical, MVCConsoleStyle.DrawColor);

  // Header separator
  WriteColoredText(lBoxChars.LeftJoin, MVCConsoleStyle.DrawColor);
  Line := '';
  for I := 0 to High(ColWidths) do
  begin
    Line := StringOfChar(lBoxChars.Horizontal, ColWidths[I]);
    WriteColoredText(Line, MVCConsoleStyle.DrawColor);
    if I < High(ColWidths) then
      WriteColoredText(lBoxChars.Cross, MVCConsoleStyle.DrawColor)
  end;
  WriteLineColored(lBoxChars.RightJoin, MVCConsoleStyle.DrawColor);

  // Data rows
  for I := 0 to High(Data) do
  begin
    WriteColoredText(lBoxChars.Vertical, MVCConsoleStyle.DrawColor);
    Line := '';
    for J := 0 to High(Headers) do
    begin
      if J < Length(Data[I]) then
        Cell := ' ' + PadRight(Data[I][J], ColWidths[J] - 2) + ' '
      else
        Cell := StringOfChar(' ', ColWidths[J]);
      Line := Line + Cell + lBoxChars.Vertical;
    end;
    WriteLineColored(Line);
  end;

  // Bottom border
  WriteTableBottomBorder(ColWidths, Style);
end;

procedure ShowSimpleProgressBar(const Title: string; Position, MaxValue: Integer; Width: Integer);
var
  Progress: Double;
  FilledChars: Integer;
  ProgressLine: string;
  BarContent: string;
begin
  if MaxValue = 0 then Exit;

  Progress := Position / MaxValue;
  FilledChars := Round(Progress * Width);

  // Build the bar content
  BarContent := StringOfChar('=', FilledChars) + StringOfChar(' ', Width - FilledChars);

  // Build complete progress line
  ProgressLine := Format('[%s] %3.0f%% (%d/%d)',
    [BarContent,
     Progress * 100,
     Position,
     MaxValue]);

  if Title <> '' then
  begin
    WriteColoredText(Title + ': ', Gray);
    WriteColoredText(ProgressLine, White);
  end
  else
    WriteColoredText(ProgressLine, White);

  // Force flush to ensure progress bar is displayed
{$IFDEF MSWINDOWS}
  Flush(Output);
{$ENDIF}
{$IFDEF LINUX}
  Flush(Output);
{$ENDIF}
end;

procedure DrawSimpleBox(const Title: string; const Content: TStringArray; Width: Integer; TextColor: TConsoleColor; Style: TBoxStyle);
var
  I: Integer;
  Line: string;
  ContentLine: string;
  lBoxChars: TBoxChars;
begin
  lBoxChars := GetBoxChars(Style);

  // Top border
  Line := lBoxChars.TopLeft + StringOfChar(lBoxChars.Horizontal, Width - 2) + lBoxChars.TopRight;
  WriteLineColored(Line, MVCConsoleStyle.DrawColor);

  // Title (if provided)
  if Title <> '' then
  begin
    ContentLine := ' ' + PadRight(Title, Width - 4) + ' ';
    WriteColoredText(lBoxChars.Vertical, MVCConsoleStyle.DrawColor);
    WriteColoredText(ContentLine, GetColorOrDefault(TextColor, sccHighLightText));
    WriteLineColored(lBoxChars.Vertical, MVCConsoleStyle.DrawColor);

    // Title separator
    Line := lBoxChars.LeftJoin + StringOfChar(lBoxChars.Horizontal, Width - 2) + lBoxChars.RightJoin;
    WriteLineColored(Line, MVCConsoleStyle.DrawColor);
  end;

  // Content lines
  for I := 0 to High(Content) do
  begin
    WriteColoredText(lBoxChars.Vertical, MVCConsoleStyle.DrawColor);
    ContentLine := ' ' + PadRight(Content[I], Width - 4) + ' ';
    WriteColoredText(ContentLine, TextColor);
    WriteLineColored(lBoxChars.Vertical, MVCConsoleStyle.DrawColor);
  end;

  // Bottom border
  Line := lBoxChars.BottomLeft + StringOfChar(lBoxChars.Horizontal, Width - 2) + lBoxChars.BottomRight;
  WriteLineColored(Line, MVCConsoleStyle.DrawColor);
end;

procedure WriteAlignedText(const Text: string; Width: Integer; Alignment: TAlignment; TextColor: TConsoleColor);
var
  PaddingLeft, PaddingRight: Integer;
  AlignedText: string;
begin
  TextColor := GetColorOrDefault(TextColor, sccText);

  if Length(Text) >= Width then
  begin
    WriteLineColored(Text, TextColor);
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

  WriteLineColored(AlignedText, TextColor);
end;

procedure ShowProgressAnimation(const Title: string; Steps: Integer; DelayMs: Integer);
var
  I: Integer;
  Percent: Double;
  ProgressLine: string;
  CurPos: TMVCConsolePoint;
begin
  if Title <> '' then
    WriteLineColored(Title, Yellow);

  // Save cursor position for animation
  CurPos := GetCursorPosition;

  for I := 0 to Steps do
  begin
    Percent := (I / Steps) * 100;
    ProgressLine := Format('Progress: [%s%s] %3.0f%%',
      [StringOfChar('=', I * 40 div Steps),
       StringOfChar(' ', 40 - (I * 40 div Steps)),
       Percent]);

    // Go back to saved position
    GotoXY(CurPos.X, CurPos.Y);

    // Clear the line and write progress
    WriteColoredText(ProgressLine + StringOfChar(' ', 10), White);

    // Force flush
{$IFDEF MSWINDOWS}
    Flush(Output);
{$ENDIF}
{$IFDEF LINUX}
    Flush(Output);
{$ENDIF}

    Sleep(DelayMs);
  end;
  WriteLn;
end;

procedure WriteStatusLine(const Items: TStringArray; const Statuses: TStringArray;
                         const Colors: TConsoleColorArray);
var
  I: Integer;
  MaxLen: Integer;
begin
  MaxLen := Min(Length(Items), Length(Statuses));
  for I := 0 to MaxLen - 1 do
  begin
    WriteColoredText(Items[I] + ': ', Gray);
    if I < Length(Colors) then
      WriteLineColored(Statuses[I], Colors[I])
    else
      WriteLineColored(Statuses[I], White);
  end;
end;

procedure ShowSimpleMenu(const Title: string; const Items: TStringArray; SelectedIndex: Integer);
var
  I: Integer;
  Line: string;
  MaxWidth: Integer;
begin
  // Calculate max width
  MaxWidth := Length(Title);
  for I := 0 to High(Items) do
    if Length(Items[I]) + 4 > MaxWidth then
      MaxWidth := Length(Items[I]) + 4;
  Inc(MaxWidth, 4); // Add border padding

  // Top border
  Line := '+' + StringOfChar('=', MaxWidth - 2) + '+';
  WriteLineColored(Line, MVCConsoleStyle.DrawColor);

  // Title
  WriteColoredText('| ', MVCConsoleStyle.DrawColor);
  WriteColoredText(PadRight(Title, MaxWidth - 4), MVCConsoleStyle.TextColor);
  WriteLineColored(' |', MVCConsoleStyle.DrawColor);

  // Separator
  Line := '+' + StringOfChar('-', MaxWidth - 2) + '+';
  WriteLineColored(Line, MVCConsoleStyle.DrawColor);

  // Menu items
  for I := 0 to High(Items) do
  begin
    WriteColoredText('| ', MVCConsoleStyle.DrawColor);

    if I = SelectedIndex then
    begin
      WriteColoredText('> ', MVCConsoleStyle.TextColor);
      WriteColoredText(PadRight(Items[I], MaxWidth - 6), MVCConsoleStyle.TextColor);
    end
    else
    begin
      WriteColoredText('  ', MVCConsoleStyle.TextColor);
      WriteColoredText(PadRight(Items[I], MaxWidth - 6), MVCConsoleStyle.TextColor);
    end;

    WriteLineColored(' |', MVCConsoleStyle.DrawColor);
  end;

  // Bottom border
  Line := '+' + StringOfChar('=', MaxWidth - 2) + '+';
  WriteLineColored(Line, MVCConsoleStyle.DrawColor);
end;

procedure WriteFormattedList(const Title: string; const Items: TStringArray;
                           ListStyle: TListStyle);
var
  I: Integer;
  Prefix: string;
begin
  if Title <> '' then
  begin
    WriteLineColored(Title, MVCConsoleStyle.TextColor);
    WriteLn;
  end;

  for I := 0 to High(Items) do
  begin
    case ListStyle of
      lsBullet: Prefix := '  • ';
      lsNumbered: Prefix := Format('%3d. ', [I + 1]);
      lsDash: Prefix := '  - ';
      lsArrow: Prefix := '  > ';
    end;

    WriteColoredText(Prefix, MVCConsoleStyle.SymbolsColor);
    WriteLineColored(Items[I], MVCConsoleStyle.TextColor);
  end;
end;

procedure ShowSystemDashboard(const Title: string; const ServerStatuses: TStringArray;
                              const ServerColors: TConsoleColorArray;
                              const MetricNames: TStringArray; const MetricValues: TIntegerArray);
var
  I: Integer;
  Line: string;
  MaxWidth: Integer;
  MaxLen: Integer;
  Progress: Double;
  FilledChars: Integer;
  ProgressBarWidth: Integer;
  ProgressStr: string;
  MetricLine: string;
begin
  MaxWidth := 60;
  ProgressBarWidth := 20;

  // Top border
  Line := '+' + StringOfChar('=', MaxWidth - 2) + '+';
  WriteLineColored(Line, MVCConsoleStyle.DrawColor);

  // Title
  WriteColoredText('| ', MVCConsoleStyle.DrawColor);
  WriteColoredText(PadRight(Title, MaxWidth - 4), MVCConsoleStyle.TextColor);
  WriteLineColored(' |', MVCConsoleStyle.DrawColor);

  // Separator
  Line := '+' + StringOfChar('-', MaxWidth - 2) + '+';
  WriteLineColored(Line, MVCConsoleStyle.DrawColor);

  // Server statuses
  WriteColoredText('| ', MVCConsoleStyle.DrawColor);
  WriteColoredText(PadRight('Server Status:', MaxWidth - 4), MVCConsoleStyle.TextColor);
  WriteLineColored(' |', MVCConsoleStyle.DrawColor);

  MaxLen := Min(Length(ServerStatuses), Length(ServerColors));
  for I := 0 to MaxLen - 1 do
  begin
    WriteColoredText('| ', MVCConsoleStyle.DrawColor);
    WriteColoredText('  ' + ServerStatuses[I], ServerColors[I]);
    WriteColoredText(StringOfChar(' ', MaxWidth - Length(ServerStatuses[I]) - 6), MVCConsoleStyle.TextColor);
    WriteLineColored(' |', MVCConsoleStyle.DrawColor);
  end;

  // Metrics
  if Length(MetricNames) > 0 then
  begin
    Line := '+' + StringOfChar('-', MaxWidth - 2) + '+';
    WriteLineColored(Line, MVCConsoleStyle.DrawColor);

    WriteColoredText('| ', MVCConsoleStyle.DrawColor);
    WriteColoredText(PadRight('Performance Metrics:', MaxWidth - 4), MVCConsoleStyle.TextColor);
    WriteLineColored(' |', MVCConsoleStyle.DrawColor);

    MaxLen := Min(Length(MetricNames), Length(MetricValues));
    for I := 0 to MaxLen - 1 do
    begin
      // Calculate progress
      Progress := MetricValues[I] / 100;
      FilledChars := Round(Progress * ProgressBarWidth);

      // Build progress string
      ProgressStr := '[' + StringOfChar('=', FilledChars) +
                     StringOfChar(' ', ProgressBarWidth - FilledChars) + '] ' +
                     Format('%3d%%', [MetricValues[I]]);

      // Build complete line
      MetricLine := '  ' + MetricNames[I] + ': ' + ProgressStr;

      WriteColoredText('| ', MVCConsoleStyle.DrawColor);
      WriteColoredText(PadRight(MetricLine, MaxWidth - 4), MVCConsoleStyle.TextColor);
      WriteLineColored(' |', MVCConsoleStyle.DrawColor);
    end;
  end;

  // Bottom border
  Line := '+' + StringOfChar('=', MaxWidth - 2) + '+';
  WriteLineColored(Line, MVCConsoleStyle.DrawColor);
end;

procedure WriteReport(const Title: string; const Sections: TStringArray;
                     const SectionContents: TStringMatrix);
var
  I, J: Integer;
  MaxLen: Integer;
begin
  // Report header
  WriteHeader(Title);
  WriteLn;

  // Report sections
  MaxLen := Min(Length(Sections), Length(SectionContents));
  for I := 0 to MaxLen - 1 do
  begin
    WriteLineColored(Sections[I], Yellow);
    WriteSeparator(40);

    for J := 0 to High(SectionContents[I]) do
      WriteLineColored('  ' + SectionContents[I][J], White);

    WriteLn;
  end;

  WriteHeader('End of Report');
end;

procedure ShowLoadingSpinner(const Message: string; Iterations: Integer);
var
  I: Integer;
  SpinChars: array[0..3] of Char;
  CurPos: TMVCConsolePoint;
begin
  SpinChars[0] := '|';
  SpinChars[1] := '/';
  SpinChars[2] := '-';
  SpinChars[3] := '\';

  // Ensure we're using visible colors
  SaveColors;
  try
    TextColor(White);
    TextBackground(Black);

    // Write message
    Write(Message + ' ');

    // Force flush immediately after writing message
{$IFDEF MSWINDOWS}
    Flush(Output);
{$ENDIF}
{$IFDEF LINUX}
    Flush(Output);
{$ENDIF}

    // Save cursor position for spinner
    CurPos := GetCursorPosition;

    TextColor(Yellow);
    for I := 0 to Iterations do
    begin
      // Go back to spinner position
      GotoXY(CurPos.X, CurPos.Y);

      // Write spinner character
      Write(SpinChars[I mod 4]);

      // Force flush for smooth animation
{$IFDEF MSWINDOWS}
      Flush(Output);
{$ENDIF}
{$IFDEF LINUX}
      Flush(Output);
{$ENDIF}

      Sleep(150);
    end;

    // Go back to spinner position and write Done
    GotoXY(CurPos.X, CurPos.Y);
    TextColor(Green);
    Write('Done!');
    WriteLn;
  finally
    RestoreSavedColors;
  end;
end;

procedure WriteColoredTable(const Headers: TStringArray;
                            const Data: TStringMatrix;
                           HeaderColor: TConsoleColor;
                           DrawColor: TConsoleColor;
                           DataColor: TConsoleColor;
                           BoxStyle: TBoxStyle);
var
  ColWidths: array of Integer;
  I, J: Integer;
  Line, Cell: string;
  lBoxChars: TBoxChars;
begin
  if Length(Headers) = 0 then Exit;

  HeaderColor := GetColorOrDefault(HeaderColor, sccHighLightText);
  DrawColor := GetColorOrDefault(DrawColor, sccDraw);
  DataColor := GetColorOrDefault(DataColor, sccText);

  lBoxChars := GetBoxChars(BoxStyle);

  // Calculate column widths
  SetLength(ColWidths, Length(Headers));
  for I := 0 to High(Headers) do
  begin
    ColWidths[I] := Length(Headers[I]);
    for J := 0 to High(Data) do
    begin
      if (I < Length(Data[J])) and (Length(Data[J][I]) > ColWidths[I]) then
        ColWidths[I] := Length(Data[J][I]);
    end;
    Inc(ColWidths[I], 2); // Add padding
  end;

  // Top border
  Line := lBoxChars.TopLeft;
  for I := 0 to High(ColWidths) do
  begin
    Line := Line + StringOfChar(lBoxChars.Horizontal, ColWidths[I]);
    if I < High(ColWidths) then
      Line := Line + lBoxChars.TopJoin
    else
      Line := Line + lBoxChars.TopRight
  end;
  WriteLineColored(Line, DrawColor);

  // Headers with color
  Line := '';
  WriteColoredText(lBoxChars.Vertical, DrawColor);
  for I := 0 to High(Headers) do
  begin
    Cell := ' ' + PadRight(Headers[I], ColWidths[I] - 2) + ' ';
    Line := Cell;
    WriteColoredText(Line, HeaderColor);
    WriteColoredText(lBoxChars.Vertical, DrawColor);
    Line := '';
  end;
  Writeln;

  // Header separator
  Line := lBoxChars.LeftJoin;
  for I := 0 to High(ColWidths) do
  begin
    Line := Line + StringOfChar(lBoxChars.Horizontal, ColWidths[I]);
    if I < High(ColWidths) then
      Line := Line + lBoxChars.Cross
    else
      Line := Line + lBoxChars.RightJoin
  end;
  WriteLineColored(Line, DrawColor);
  // Data rows with color

  for I := 0 to High(Data) do
  begin
    WriteColoredText(lBoxChars.Vertical, DrawColor);
    Line := '';
    for J := 0 to High(Headers) do
    begin
      if J < Length(Data[I]) then
        Cell := ' ' + PadRight(Data[I][J], ColWidths[J] - 2) + ' '
      else
        Cell := StringOfChar(' ', ColWidths[J]);
      WriteColoredText(Cell, DataColor);
      WriteColoredText(lBoxChars.Vertical, DrawColor);
    end;
    Writeln;
  end;

  // Bottom border
  Line := lBoxChars.BottomLeft;
  for I := 0 to High(ColWidths) do
  begin
    Line := Line + StringOfChar(lBoxChars.Horizontal, ColWidths[I]);
    if I < High(ColWidths) then
      Line := Line + lBoxChars.BottomJoin
    else
      Line := Line + lBoxChars.BottomRight
  end;
  WriteLineColored(Line, DrawColor);
end;

// ============================================================================
// QUICK UTILITY FUNCTIONS
// ============================================================================

procedure WriteHeader(const Text: string; Width: Integer; HeaderColor: TConsoleColor);
var
  Line: string;
  PaddingSize: Integer;
  CharSymbol: Char;
begin
  HeaderColor := GetColorOrDefault(HeaderColor, sccHighLightText);
  CharSymbol := GetBoxChars(MVCConsolestyle.BoxStyle).Horizontal;
  Line := StringOfChar(CharSymbol, Width);
  WriteLineColored(Line, MVCConsoleStyle.DrawColor);

  if Text <> '' then
  begin
    PaddingSize := (Width - Length(Text) - 2) div 2;
    Line := StringOfChar(' ', PaddingSize) + ' ' + Text + ' ' +
            StringOfChar(' ', Width - PaddingSize - Length(Text) - 2);
    WriteLineColored(Line, HeaderColor);

    Line := StringOfChar(CharSymbol, Width);
    WriteLineColored(Line, MVCConsoleStyle.DrawColor);
  end;
end;

procedure WriteSeparator(Width: Integer; CharSymbol: Char);
begin
  WriteLineColored(StringOfChar(CharSymbol, Width), Gray);
end;

procedure WriteSuccess(const Message: string);
begin
  WriteColoredText('[SUCCESS] ', Green);
  WriteLineColored(Message, White);
end;

procedure WriteWarning(const Message: string);
begin
  WriteColoredText('[WARNING] ', Yellow);
  WriteLineColored(Message, White);
end;

procedure WriteError(const Message: string);
begin
  WriteColoredText('[ERROR] ', Red);
  WriteLineColored(Message, White);
end;

procedure WriteInfo(const Message: string);
begin
  WriteColoredText('[INFO] ', Cyan);
  WriteLineColored(Message, White);
end;

// ============================================================================
// INTERACTIVE MENU FUNCTIONS
// ============================================================================

function ShowInteractiveMenu(const Title: string; const Items: TStringArray;
                           DefaultIndex: Integer = 0;
                           const Hint: string = 'Use arrows to navigate, Enter to select, ESC to cancel'): Integer;
var
  SelectedIndex: Integer;
  Key: Integer;
  Done: Boolean;
  I: Integer;
  MaxWidth: Integer;
  StartX, StartY: Word;
  CurPos: TMVCConsolePoint;
  ConsoleSize: TMVCConsoleSize;
  Line: string;
  MenuHeight: Integer;
begin
  Result := -1;  // Default to cancelled
  if Length(Items) = 0 then Exit;

  Init; // Ensure console is initialized

  SelectedIndex := DefaultIndex;
  if SelectedIndex < 0 then SelectedIndex := 0;
  if SelectedIndex > High(Items) then SelectedIndex := High(Items);

  // Calculate max width
  MaxWidth := Length(Title);
  for I := 0 to High(Items) do
    if Length(Items[I]) + 6 > MaxWidth then
      MaxWidth := Length(Items[I]) + 6;
  Inc(MaxWidth, 4); // Add border padding

  // Calculate menu height
  MenuHeight := Length(Items) + 5; // Items + borders + title + separator
  if Hint <> '' then Inc(MenuHeight);

  // Get console size and current position
  ConsoleSize := GetConsoleSize;
  CurPos := GetCursorPosition;
  StartX := CurPos.X;
  StartY := CurPos.Y;

  // Ensure menu fits in console
  if StartY + MenuHeight > ConsoleSize.Rows then
  begin
    StartY := ConsoleSize.Rows - MenuHeight - 1;
    //if StartY < 0 then StartY := 0;
    GotoXY(StartX, StartY);
  end;

  // Hide cursor during menu
  HideCursor;
  try
    Done := False;
    while not Done do
    begin
      // Go back to start position
      GotoXY(StartX, StartY);

      // Draw menu
      // Top border
      Line := '+' + StringOfChar('=', MaxWidth - 2) + '+';
      WriteLineColored(Line, Cyan);

      // Title
      WriteColoredText('| ', Cyan);
      WriteColoredText(PadRight(Title, MaxWidth - 4), Yellow);
      WriteLineColored(' |', Cyan);

      // Separator
      Line := '+' + StringOfChar('-', MaxWidth - 2) + '+';
      WriteLineColored(Line, Cyan);

      // Menu items
      for I := 0 to High(Items) do
      begin
        WriteColoredText('| ', Cyan);

        if I = SelectedIndex then
        begin
          // Highlighted item
          SaveColors;
          TextBackground(DarkCyan);
          TextColor(White);
          Write('> ' + PadRight(Items[I], MaxWidth - 6) + ' ');
          RestoreSavedColors;
        end
        else
        begin
          WriteColoredText('  ' + PadRight(Items[I], MaxWidth - 6) + ' ', White);
        end;

        WriteLineColored('|', Cyan);
      end;

      // Bottom border
      Line := '+' + StringOfChar('=', MaxWidth - 2) + '+';
      WriteLineColored(Line, Cyan);

      // Hint
      if Hint <> '' then
        WriteLineColored(Hint, DarkGray);

      // Flush output
{$IFDEF MSWINDOWS}
      Flush(Output);
{$ENDIF}
{$IFDEF LINUX}
      Flush(Output);
{$ENDIF}

      // Read key
      Key := GetKey;

      // Handle keys
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

    // Clear the menu area safely
    for I := 0 to MenuHeight - 1 do
    begin
      if StartY + I < ConsoleSize.Rows then
      begin
        GotoXY(StartX, StartY + I);
        Write(StringOfChar(' ', Min(MaxWidth + 5, ConsoleSize.Columns - StartX)));
      end;
    end;

    if StartY < ConsoleSize.Rows then
      GotoXY(StartX, StartY);

  finally
    ShowCursor;
  end;
end;

function ShowAdvancedMenu(const Title: string; const Items: TMenuItemsArray;
                        DefaultIndex: Integer = 0;
                        HighlightColor: TConsoleColor = DarkCyan;
                        const Hint: string = 'Use arrows to navigate, Enter to select, ESC to cancel'): Integer;
var
  SelectedIndex: Integer;
  Key: Integer;
  Done: Boolean;
  I: Integer;
  MaxWidth: Integer;
  StartX, StartY: Word;
  CurPos: TMVCConsolePoint;
  ConsoleSize: TMVCConsoleSize;
  Line: string;
  ItemText: string;
  MenuHeight: Integer;
begin
  Result := -1;  // Default to cancelled
  if Length(Items) = 0 then Exit;

  HighlightColor := GetColorOrDefault(HighlightColor, sccHighLightBackground);

  Init; // Ensure console is initialized

  SelectedIndex := DefaultIndex;
  if SelectedIndex < 0 then SelectedIndex := 0;
  if SelectedIndex > High(Items) then SelectedIndex := High(Items);

  // Skip to first enabled item if default is disabled
  while (SelectedIndex <= High(Items)) and (not Items[SelectedIndex].Enabled) do
    Inc(SelectedIndex);
  if SelectedIndex > High(Items) then
  begin
    SelectedIndex := 0;
    while (SelectedIndex <= High(Items)) and (not Items[SelectedIndex].Enabled) do
      Inc(SelectedIndex);
  end;

  // Calculate max width
  MaxWidth := Length(Title);
  for I := 0 to High(Items) do
  begin
    ItemText := Items[I].Icon + ' ' + Items[I].Text;
    if Length(ItemText) + 6 > MaxWidth then
      MaxWidth := Length(ItemText) + 6;
  end;
  Inc(MaxWidth, 4); // Add border padding

  // Calculate menu height
  MenuHeight := Length(Items) + 5; // Items + borders + title + separator
  if Hint <> '' then Inc(MenuHeight);

  // Get console size and current position
  ConsoleSize := GetConsoleSize;
  CurPos := GetCursorPosition;
  StartX := CurPos.X;
  StartY := CurPos.Y;

  // Ensure menu fits in console
  if StartY + MenuHeight > ConsoleSize.Rows then
  begin
    StartY := ConsoleSize.Rows - MenuHeight - 1;
    GotoXY(StartX, StartY);
  end;

  // Hide cursor during menu
  HideCursor;
  try
    Done := False;
    while not Done do
    begin
      // Go back to start position
      GotoXY(StartX, StartY);

      // Draw menu
      // Top border
      Line := '+' + StringOfChar('=', MaxWidth - 2) + '+';
      WriteLineColored(Line, Cyan);

      // Title
      WriteColoredText('| ', Cyan);
      WriteColoredText(PadRight(Title, MaxWidth - 4), Yellow);
      WriteLineColored(' |', Cyan);

      // Separator
      Line := '+' + StringOfChar('-', MaxWidth - 2) + '+';
      WriteLineColored(Line, Cyan);

      // Menu items
      for I := 0 to High(Items) do
      begin
        WriteColoredText('| ', Cyan);

        ItemText := Items[I].Icon + ' ' + Items[I].Text;

        if not Items[I].Enabled then
        begin
          // Disabled item
          WriteColoredText('  ' + PadRight(ItemText, MaxWidth - 6) + ' ', DarkGray);
        end
        else if I = SelectedIndex then
        begin
          // Highlighted item
          SaveColors;
          TextBackground(HighlightColor);
          TextColor(White);
          Write('> ' + PadRight(ItemText, MaxWidth - 6) + ' ');
          RestoreSavedColors;
        end
        else
        begin
          WriteColoredText('  ' + PadRight(ItemText, MaxWidth - 6) + ' ', White);
        end;

        WriteLineColored('|', Cyan);
      end;

      // Bottom border
      Line := '+' + StringOfChar('=', MaxWidth - 2) + '+';
      WriteLineColored(Line, Cyan);

      // Hint
      if Hint <> '' then
        WriteLineColored(Hint, DarkGray);

      // Flush output
{$IFDEF MSWINDOWS}
      Flush(Output);
{$ENDIF}
{$IFDEF LINUX}
      Flush(Output);
{$ENDIF}

      // Read key
      Key := GetKey;

      // Handle keys
      case Key of
        KEY_UP:
          begin
            repeat
              Dec(SelectedIndex);
              if SelectedIndex < 0 then
                SelectedIndex := High(Items);
            until Items[SelectedIndex].Enabled;
          end;
        KEY_DOWN:
          begin
            repeat
              Inc(SelectedIndex);
              if SelectedIndex > High(Items) then
                SelectedIndex := 0;
            until Items[SelectedIndex].Enabled;
          end;
        KEY_ENTER:
          begin
            if Items[SelectedIndex].Enabled then
            begin
              Done := True;
              Result := SelectedIndex;
            end;
          end;
        KEY_ESCAPE:
          begin
            Done := True;
            Result := -1;
          end;
      end;
    end;

    // Clear the menu area safely
    for I := 0 to MenuHeight - 1 do
    begin
      if StartY + I < ConsoleSize.Rows then
      begin
        GotoXY(StartX, StartY + I);
        Write(StringOfChar(' ', Min(MaxWidth + 5, ConsoleSize.Columns - StartX)));
      end;
    end;

    if StartY < ConsoleSize.Rows then
      GotoXY(StartX, StartY);

  finally
    ShowCursor;
  end;
end;

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

function IsSpecialKey(KeyCode: Integer): Boolean;
begin
  Result := KeyCode > 255;
end;

function CreateMenuItem(const Text: string; const Icon: string = ''; Enabled: Boolean = True): TMenuItemStyle;
begin
  Result.Text := Text;
  Result.Icon := Icon;
  Result.Enabled := Enabled;
end;

initialization
  GLock := TObject.Create;
  GSavedCursorX := 0;
  GSavedCursorY := 0;

  GForeGround := Ord(MVCConsoleStyle.TextColor);
  GBackGround := Ord(MVCConsoleStyle.BackgroundColor);

finalization
{$IFDEF LINUX}
  RestoreTerminal;
{$ENDIF}
  GLock.Free;

end.
