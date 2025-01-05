// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2024 Daniele Teti
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

unit LoggerPro.OutputDebugStringAppender;

interface

uses
  LoggerPro, System.Classes;

type
  { @abstract(This appenders sends logs to the @code(OutputDebugString) function on Windows OSes)
    To learn how to use this appender, check the sample @code(outputdebugstring_appender.dproj)
  }
  TLoggerProOutputDebugStringAppender = class(TLoggerProAppenderBase)
  private
    {$IFDEF MSWINDOWS}
    FModuleName: string;
    {$ENDIF}
  public
    constructor Create(aLogItemRenderer: ILogItemRenderer = nil); override;
    procedure Setup; override;
    procedure TearDown; override;
    procedure WriteLog(const aLogItem: TLogItem); override;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils
{$IFDEF MSWINDOWS}
    , Winapi.Windows
    , Winapi.Messages
{$ENDIF}
  ;

{ TLoggerProOutputDebugStringAppender }

constructor TLoggerProOutputDebugStringAppender.Create(aLogItemRenderer: ILogItemRenderer);
begin
  inherited;
end;

procedure TLoggerProOutputDebugStringAppender.Setup;
begin
  inherited;
{$IFDEF MSWINDOWS}
  FModuleName := TPath.GetFileName(GetModuleName(HInstance));
{$ENDIF}
end;

procedure TLoggerProOutputDebugStringAppender.TearDown;
begin
  // do nothing
end;

procedure TLoggerProOutputDebugStringAppender.WriteLog(const aLogItem : TLogItem);
{$IFDEF MSWINDOWS}
var
  lLog: string;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  lLog := '(' + FModuleName + ') ' + FormatLog(aLogItem);
  OutputDebugString(PChar(lLog));
{$ENDIF}
end;

end.
