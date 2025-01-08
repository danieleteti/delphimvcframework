// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2025 Daniele Teti
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

unit LoggerPro.SimpleConsoleAppender;

{$IFNDEF CONSOLE}
{$MESSAGE FATAL 'This unit should only be used in console applications'}
{$ENDIF}

interface

uses
  System.Classes, System.SysUtils,
  LoggerPro;

type
  { @abstract(This appender assumes the application is running from the console and simply uses Writeln
    without any dependency on Windows to send logs to the current console; this allows console logging from Linux)
    To learn how to use this appender, check the sample @code(SimpleConsole_appender.dproj)
  }
  TLoggerProSimpleConsoleAppender = class(TLoggerProAppenderBase)
  public
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
  end;

  TLoggerProSimpleConsoleLogFmtAppender = class(TLoggerProSimpleConsoleAppender)
  public
    constructor Create; override;
  end;

implementation

uses
  LoggerPro.Renderers;

procedure TLoggerProSimpleConsoleAppender.TearDown;
begin
  // do nothing
end;

procedure TLoggerProSimpleConsoleAppender.WriteLog(const aLogItem: TLogItem);
begin
  Writeln(FormatLog(aLogItem));
end;

{ TLoggerProSimpleConsoleLogFmtAppender }

constructor TLoggerProSimpleConsoleLogFmtAppender.Create;
begin
  inherited Create(TLogItemRendererLogFmt.Create);
end;

end.
