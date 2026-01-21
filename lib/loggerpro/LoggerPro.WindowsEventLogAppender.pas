// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2026 Daniele Teti
//
// https://github.com/danieleteti/loggerpro
//
// Contributors for this file:
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

unit LoggerPro.WindowsEventLogAppender;

interface

{$IF Defined(MSWINDOWS)}

uses
  LoggerPro;

type
  { @abstract(Appender for logging to Windows Event Log)
    Can be used in two modes:
    1. With a TService instance (for Windows Services) - uses TService.LogMessage
    2. With a source name (for any Windows application) - uses ReportEvent API
  }
  TLoggerProWindowsEventLogAppender = class(TLoggerProAppenderBase)
  private
    FService: TObject;  // TService when used in service mode
    FEventSource: THandle;
    FSourceName: string;
    FUseService: Boolean;
  public
    { Creates appender for Windows Service using TService.LogMessage }
    constructor Create(AService: TObject); reintroduce; overload;
    { Creates appender for any Windows application using ReportEvent API }
    constructor Create(const ASourceName: string = ''); reintroduce; overload;
    destructor Destroy; override;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
  end;

{$ENDIF}

implementation

{$IF Defined(MSWINDOWS)}

uses
  Winapi.Windows,
  System.SysUtils,
  Vcl.SvcMgr;

function LogTypeToEventType(aLogType: TLogType): Word;
begin
  case aLogType of
    TLogType.Debug,
    TLogType.Info:
      Result := EVENTLOG_INFORMATION_TYPE;
    TLogType.Warning:
      Result := EVENTLOG_WARNING_TYPE;
    TLogType.Error,
    TLogType.Fatal:
      Result := EVENTLOG_ERROR_TYPE;
  else
    Result := EVENTLOG_INFORMATION_TYPE;
  end;
end;

constructor TLoggerProWindowsEventLogAppender.Create(AService: TObject);
begin
  inherited Create;
  if not (AService is TService) then
    raise ELoggerPro.Create('TLoggerProWindowsEventLogAppender.Create requires a TService instance');
  FService := AService;
  FUseService := True;
  FEventSource := 0;
end;

constructor TLoggerProWindowsEventLogAppender.Create(const ASourceName: string);
begin
  inherited Create;
  FService := nil;
  FUseService := False;
  FEventSource := 0;
  if ASourceName.IsEmpty then
    FSourceName := ChangeFileExt(ExtractFileName(ParamStr(0)), '')
  else
    FSourceName := ASourceName;
end;

destructor TLoggerProWindowsEventLogAppender.Destroy;
begin
  TearDown;
  inherited;
end;

procedure TLoggerProWindowsEventLogAppender.Setup;
begin
  if not FUseService then
  begin
    FEventSource := RegisterEventSource(nil, PChar(FSourceName));
    if FEventSource = 0 then
      RaiseLastOSError;
  end;
end;

procedure TLoggerProWindowsEventLogAppender.TearDown;
begin
  if (not FUseService) and (FEventSource <> 0) then
  begin
    DeregisterEventSource(FEventSource);
    FEventSource := 0;
  end;
end;

procedure TLoggerProWindowsEventLogAppender.WriteLog(const aLogItem: TLogItem);
var
  lEventType: Word;
  lMessage: string;
  lMessagePtr: PChar;
begin
  lEventType := LogTypeToEventType(aLogItem.LogType);
  lMessage := aLogItem.LogMessage;

  if FUseService then
  begin
    // Use TService.LogMessage for Windows Services
    TService(FService).LogMessage(lMessage, lEventType);
  end
  else
  begin
    // Use ReportEvent API for regular applications
    if FEventSource <> 0 then
    begin
      lMessagePtr := PChar(lMessage);
      ReportEvent(
        FEventSource,      // event log handle
        lEventType,        // event type
        0,                 // category
        0,                 // event ID
        nil,               // user SID
        1,                 // number of strings
        0,                 // data size
        @lMessagePtr,      // strings
        nil                // data
      );
    end;
  end;
end;

{$ENDIF}

end.
