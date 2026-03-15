// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2026 Daniele Teti
//
// https://github.com/danieleteti/loggerpro
//
// Contributors for this file:
//    Fulgan - https://github.com/Fulgan
//    David Cornelius
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

unit LoggerPro.ConsoleAppender;

interface

uses
  System.Classes,
  System.SysUtils,
  LoggerPro,
  SyncObjs;

type
  /// <summary>
  /// Cross-platform console appender with optional color support.
  /// On Windows: Uses Windows Console API for colors and can create/attach consoles for GUI apps.
  /// On Linux/macOS: Uses ANSI escape codes for colors.
  /// </summary>
  TLoggerProConsoleAppender = class(TLoggerProAppenderBase)
  strict private
    class var FLock: TCriticalSection;
{$IFDEF MSWINDOWS}
    class var FConsoleAllocated: Int64;
{$ENDIF}
    class constructor Create;
    class destructor Destroy;
  protected
    FUTF8Output: Boolean;
{$IFDEF MSWINDOWS}
    FSavedOutputCP: Cardinal;
    fColors: array [TLogType.Debug .. TLogType.Fatal] of Integer;
    fSavedColors: Integer;
{$ELSE}
    fColors: array [TLogType.Debug .. TLogType.Fatal] of string;
{$ENDIF}
    procedure SetColor(const aLogType: TLogType);
    procedure ResetColor;
    procedure SetupColorMappings; virtual;
    /// <summary>
    /// Writes a line of text as UTF-8 bytes directly to stdout, bypassing Writeln.
    /// </summary>
    procedure WriteUTF8Line(const aText: string);
    /// <summary>
    /// Writes text as UTF-8 bytes directly to stdout without appending a line break.
    /// Used for ANSI color escape codes.
    /// </summary>
    procedure WriteUTF8Raw(const aText: string);
  public
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
    /// <summary>
    /// When True, writes UTF-8 bytes directly to stdout instead of using Writeln.
    /// Prevents Unicode mangling on Linux (POSIX locale) and Windows (console code page).
    /// </summary>
    property UTF8Output: Boolean read FUTF8Output write FUTF8Output;
  end;

  TLoggerProConsoleLogFmtAppender = class(TLoggerProConsoleAppender)
  public
    constructor Create(ALogItemRenderer: ILogItemRenderer = nil); override;
    function FormatLog(const ALogItem: TLogItem): string; override;
  end;

  /// <summary>
  /// Simple cross-platform console appender without colors.
  /// Uses plain Writeln, works on all platforms (Windows, Linux, macOS).
  /// </summary>
  TLoggerProSimpleConsoleAppender = class(TLoggerProAppenderBase)
  strict private
    FUTF8Output: Boolean;
{$IFDEF MSWINDOWS}
    FSavedOutputCP: Cardinal;
{$ENDIF}
    /// <summary>
    /// Writes a line of text as UTF-8 bytes directly to stdout, bypassing Writeln.
    /// </summary>
    procedure WriteUTF8Line(const aText: string);
  public
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
    /// <summary>
    /// When True, writes UTF-8 bytes directly to stdout instead of using Writeln.
    /// Prevents Unicode mangling on Linux (POSIX locale) and Windows (console code page).
    /// </summary>
    property UTF8Output: Boolean read FUTF8Output write FUTF8Output;
  end;

  TLoggerProSimpleConsoleLogFmtAppender = class(TLoggerProSimpleConsoleAppender)
  public
    constructor Create(ALogItemRenderer: ILogItemRenderer = nil); override;
    function FormatLog(const ALogItem: TLogItem): string; override;
  end;

{$IFDEF MSWINDOWS}
function AttachConsole(PID: Cardinal): LongBool; stdcall;
{$ENDIF}

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Unistd,
{$ENDIF}
  LoggerPro.Renderers;

{$IFDEF MSWINDOWS}
const
  ATTACH_PARENT_PROCESS = Cardinal(-1);

function AttachConsole; external kernel32 name 'AttachConsole';

const
  { FOREGROUND COLORS - CAN BE COMBINED }
  FOREGROUND_BLUE = 1;
  FOREGROUND_GREEN = 2;
  FOREGROUND_RED = 4;
  FOREGROUND_INTENSITY = 8;
  { BACKGROUND COLORS - CAN BE COMBINED }
  BACKGROUND_BLUE = $10;
  BACKGROUND_GREEN = $20;
  BACKGROUND_RED = $40;
  BACKGROUND_INTENSITY = $80;

function GetCurrentColors: Integer;
var
  info: CONSOLE_SCREEN_BUFFER_INFO;
begin
  Result := -1;
  if GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), info) then
    Result := info.wAttributes;
end;
{$ELSE}
const
  { ANSI escape codes for colors }
  ANSI_RESET = #27'[0m';
  ANSI_GREEN = #27'[32m';           // Debug
  ANSI_WHITE_BRIGHT = #27'[97m';    // Info
  ANSI_YELLOW = #27'[33m';          // Warning (dark yellow)
  ANSI_RED_BRIGHT = #27'[91m';      // Error
  ANSI_MAGENTA_BRIGHT = #27'[95m';  // Fatal
{$ENDIF}

procedure InternalWriteUTF8(const aText: string);
var
  lBytes: TBytes;
{$IFDEF MSWINDOWS}
  lBytesWritten: Cardinal;
{$ENDIF}
begin
  lBytes := TEncoding.UTF8.GetBytes(aText);
  if Length(lBytes) = 0 then
    Exit;
{$IFDEF MSWINDOWS}
  WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), lBytes[0], Length(lBytes), lBytesWritten, nil);
{$ELSE}
  __write(STDOUT_FILENO, @lBytes[0], Length(lBytes));
{$ENDIF}
end;

{ TLoggerProConsoleAppender }

class constructor TLoggerProConsoleAppender.Create;
begin
  FLock := TCriticalSection.Create;
{$IFDEF MSWINDOWS}
  FConsoleAllocated := 0;
{$ENDIF}
end;

class destructor TLoggerProConsoleAppender.Destroy;
begin
  try
    FLock.Enter;
    FreeAndNil(FLock);
  except
    // No exception checking here or the app might blow up with a RTE 217
  end;
end;

procedure TLoggerProConsoleAppender.Setup;
begin
  inherited;
  SetupColorMappings;
{$IFDEF MSWINDOWS}
  if TInterlocked.Read(FConsoleAllocated) < 2 then
  begin
    FLock.Enter;
    try
      if TInterlocked.Increment(FConsoleAllocated) = 1 then
      begin
        // Attempt to attach to the parent console (if there is already a console allocated)
        if not IsConsole then
        begin
          if not AttachConsole(ATTACH_PARENT_PROCESS) then
            AllocConsole; // No console allocated, create a new one
        end;
        fSavedColors := GetCurrentColors;
        TInterlocked.Increment(FConsoleAllocated);
      end;
    finally
      FLock.Leave;
    end;
  end;
  if FUTF8Output then
  begin
    FSavedOutputCP := GetConsoleOutputCP;
    SetConsoleOutputCP(CP_UTF8);
  end;
{$ENDIF}
end;

procedure TLoggerProConsoleAppender.SetupColorMappings;
begin
{$IFDEF MSWINDOWS}
  fColors[TLogType.Debug] := FOREGROUND_GREEN or FOREGROUND_INTENSITY;
  fColors[TLogType.Info] := FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY;
  fColors[TLogType.Warning] := FOREGROUND_RED or FOREGROUND_GREEN;  // Dark yellow/orange
  fColors[TLogType.Error] := FOREGROUND_RED or FOREGROUND_INTENSITY;
  fColors[TLogType.Fatal] := FOREGROUND_RED or FOREGROUND_BLUE or FOREGROUND_INTENSITY;
{$ELSE}
  fColors[TLogType.Debug] := ANSI_GREEN;
  fColors[TLogType.Info] := ANSI_WHITE_BRIGHT;
  fColors[TLogType.Warning] := ANSI_YELLOW;
  fColors[TLogType.Error] := ANSI_RED_BRIGHT;
  fColors[TLogType.Fatal] := ANSI_MAGENTA_BRIGHT;
{$ENDIF}
end;

procedure TLoggerProConsoleAppender.SetColor(const aLogType: TLogType);
begin
{$IFDEF MSWINDOWS}
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), fColors[aLogType]);
{$ELSE}
  if FUTF8Output then
    WriteUTF8Raw(fColors[aLogType])
  else
    Write(fColors[aLogType]);
{$ENDIF}
end;

procedure TLoggerProConsoleAppender.ResetColor;
begin
{$IFDEF MSWINDOWS}
  if fSavedColors > -1 then
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), fSavedColors)
  else
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED);
{$ELSE}
  if FUTF8Output then
    WriteUTF8Raw(ANSI_RESET)
  else
    Write(ANSI_RESET);
{$ENDIF}
end;

procedure TLoggerProConsoleAppender.TearDown;
begin
{$IFDEF MSWINDOWS}
  if fSavedColors > -1 then
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), fSavedColors)
  else
    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED);
  if FUTF8Output then
    SetConsoleOutputCP(FSavedOutputCP);
{$ENDIF}
end;

procedure TLoggerProConsoleAppender.WriteLog(const aLogItem: TLogItem);
var
  lText: string;
begin
  lText := FormatLog(aLogItem);
  FLock.Enter;
  try
    SetColor(aLogItem.LogType);
    if FUTF8Output then
      WriteUTF8Line(lText)
    else
      Writeln(lText);
    ResetColor;
  finally
    FLock.Leave;
  end;
end;

procedure TLoggerProConsoleAppender.WriteUTF8Line(const aText: string);
begin
  InternalWriteUTF8(aText + sLineBreak);
end;

procedure TLoggerProConsoleAppender.WriteUTF8Raw(const aText: string);
begin
  InternalWriteUTF8(aText);
end;

{ TLoggerProConsoleLogFmtAppender }

constructor TLoggerProConsoleLogFmtAppender.Create(ALogItemRenderer: ILogItemRenderer);
begin
  inherited Create(TLogItemRendererLogFmt.Create);
end;

function TLoggerProConsoleLogFmtAppender.FormatLog(const ALogItem: TLogItem): string;
begin
  if Assigned(FOnLogRow) then
    FOnLogRow(ALogItem, Result)
  else
    Result := FLogItemRenderer.RenderLogItem(ALogItem);
end;

{ TLoggerProSimpleConsoleAppender }

procedure TLoggerProSimpleConsoleAppender.Setup;
begin
  inherited;
{$IFDEF MSWINDOWS}
  if FUTF8Output then
  begin
    FSavedOutputCP := GetConsoleOutputCP;
    SetConsoleOutputCP(CP_UTF8);
  end;
{$ENDIF}
end;

procedure TLoggerProSimpleConsoleAppender.TearDown;
begin
{$IFDEF MSWINDOWS}
  if FUTF8Output then
    SetConsoleOutputCP(FSavedOutputCP);
{$ENDIF}
end;

procedure TLoggerProSimpleConsoleAppender.WriteLog(const aLogItem: TLogItem);
begin
  if FUTF8Output then
    WriteUTF8Line(FormatLog(aLogItem))
  else
    Writeln(FormatLog(aLogItem));
end;

procedure TLoggerProSimpleConsoleAppender.WriteUTF8Line(const aText: string);
begin
  InternalWriteUTF8(aText + sLineBreak);
end;

{ TLoggerProSimpleConsoleLogFmtAppender }

constructor TLoggerProSimpleConsoleLogFmtAppender.Create(ALogItemRenderer: ILogItemRenderer);
begin
  inherited Create(TLogItemRendererLogFmt.Create);
end;

function TLoggerProSimpleConsoleLogFmtAppender.FormatLog(const ALogItem: TLogItem): string;
begin
  if Assigned(FOnLogRow) then
    FOnLogRow(ALogItem, Result)
  else
    Result := FLogItemRenderer.RenderLogItem(ALogItem);
end;

end.
