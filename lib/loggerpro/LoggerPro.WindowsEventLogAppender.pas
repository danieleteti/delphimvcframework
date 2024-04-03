// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2024 Daniele Teti
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

{$IFNDEF MSWINDOWS}
{$MESSAGE FATAL 'This unit only works with Windows'}
{$ENDIF}

interface

uses
  Vcl.SvcMgr,
  LoggerPro;

type
  { @abstract(This appender is for logging from Windows Services to the Windows Event Log) }
  TLoggerProWindowsEventLogAppender = class(TLoggerProAppenderBase)
  private
    FService: TService;
  public
    constructor Create(AService: TService); reintroduce;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
  end;

implementation

uses
  Winapi.Windows;

constructor TLoggerProWindowsEventLogAppender.Create(AService: TService);
begin
  inherited Create;
  FService := AService;
end;

procedure TLoggerProWindowsEventLogAppender.Setup;
begin
  // do nothing
end;

procedure TLoggerProWindowsEventLogAppender.TearDown;
begin
  // do nothing
end;

procedure TLoggerProWindowsEventLogAppender.WriteLog(const aLogItem: TLogItem);
begin
  case aLogItem.LogType of
    TLogType.Debug,
    TLogType.Info:
      FService.LogMessage(aLogItem.LogMessage, EVENTLOG_INFORMATION_TYPE);
    TLogType.Warning:
      FService.LogMessage(aLogItem.LogMessage, EVENTLOG_WARNING_TYPE);
    TLogType.Error, TLogType.Fatal:
      FService.LogMessage(aLogItem.LogMessage, EVENTLOG_ERROR_TYPE);
  end;
end;

end.
