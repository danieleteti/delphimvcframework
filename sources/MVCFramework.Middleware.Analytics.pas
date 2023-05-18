// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Based on an idea by Nirav Kaku (https://www.facebook.com/nirav.kaku)
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

unit MVCFramework.Middleware.Analytics;

interface

uses
  MVCFramework,
  MVCFramework.Logger,
  System.Classes,
  LoggerPro;

type
  TMVCAnalyticsMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fLogWriter: ILogWriter;
  protected
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnAfterControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string;
      const AActionNAme: string;
      var Handled: Boolean);
     procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);

  public
    constructor Create(const ALogWriter: ILogWriter); virtual;
    property LogWriter: ILogWriter read fLogWriter;
  end;

function GetAnalyticsDefaultLogger: ILogWriter;

implementation

uses
  System.SysUtils, System.DateUtils, LoggerPro.FileAppender, MVCFramework.Commons;

var
  GLogWriter: ILogWriter = nil;

const
  LOG_LEVEL: array [1..5] of TLogType = (TLogType.Info, TLogType.Info, TLogType.Info, TLogType.Warning, TLogType.Error);
  ANALYTICS_TAG = 'analytics';

function GetAnalyticsDefaultLogger: ILogWriter;
var
  lLog: ILogAppender;
begin
  if GLogWriter = nil then
  begin
    TMonitor.Enter(GLock);
    try
      if GLogWriter = nil then // double check locking (https://en.wikipedia.org/wiki/Double-checked_locking)
      begin
        lLog := TLoggerProSimpleFileAppender.Create(10, 5000, AppPath + 'analytics', [], '%s.%2.2d.csv' {'%s.%2.2d.%s.csv'});
        TLoggerProAppenderBase(lLog).OnLogRow := procedure(const LogItem: TLogItem; out LogRow: string)
          begin
            LogRow := Format('%s;%s;%s', [
              FormatDateTime('yyyy-mm-dd hh:nn:ss', LogItem.TimeStamp),
              LogItem.LogTypeAsString,
              LogItem.LogMessage]);
          end;
        GLogWriter := BuildLogWriter([lLog]);
      end;
    finally
      TMonitor.Exit(GLock);
    end;
  end;
  Result := GLogWriter;
end;


{ TMVCAnalyticsMiddleware }

constructor TMVCAnalyticsMiddleware.Create(const ALogWriter: ILogWriter);
begin
  inherited Create;
  fLogWriter := ALogWriter;
end;

procedure TMVCAnalyticsMiddleware.OnAfterControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const Handled: Boolean);
begin
  Context
    .Data
    .Add('fqaction', AControllerQualifiedClassName + '.' + AActionName);
end;

procedure TMVCAnalyticsMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
var
  lWebReq: TMVCWebRequest;
  lWebResp: TMVCWebResponse;
begin
  lWebReq := AContext.Request;
  lWebResp := AContext.Response;
  fLogWriter.Log(LOG_LEVEL[AContext.Response.StatusCode div 100],
    lWebReq.ClientIp + ';' +
    lWebReq.RawWebRequest.Method + ';' +
    lWebReq.RawWebRequest.PathInfo + ';' +
    lWebResp.StatusCode.ToString + ';' +
    AContext.Data.Items['fqaction'] + ';' +
    lWebResp.RawWebResponse.ContentLength.ToString + ';' +
    lWebReq.RawWebRequest.Host,
    ANALYTICS_TAG);
end;

procedure TMVCAnalyticsMiddleware.OnBeforeControllerAction(Context: TWebContext;
  const AControllerQualifiedClassName, AActionNAme: string;
  var Handled: Boolean);
begin
  // do nothing
end;

procedure TMVCAnalyticsMiddleware.OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
begin
  // do nothing
end;

end.
