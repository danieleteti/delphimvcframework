// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
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

unit WebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TMyWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TMyWebModule;

implementation

{$R *.dfm}


uses
  MainControllerU,
  System.IOUtils,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  LoggerPro.FileAppender,
  LoggerPro,
  System.DateUtils,
  MVCFramework.Middleware.Analytics,
  MVCFramework.Middleware.StaticFiles;

var
  GLogWriter: ILogWriter = nil;
  GLock: TObject = nil;

function GetLoggerForAnalytics: ILogWriter;
var
  lLog: ILogAppender;
begin
  if GLogWriter = nil then
  begin
    TMonitor.Enter(GLock);
    try
      if GLogWriter = nil then // double check locking (https://en.wikipedia.org/wiki/Double-checked_locking)
      begin
        lLog := TLoggerProFileAppender.Create(5, 2000, AppPath + 'analytics', [], '%s.%2.2d.%s.csv');
        TLoggerProFileAppender(lLog).OnLogRow := procedure(const LogItem: TLogItem; out LogRow: string)
          begin
            LogRow := Format('%s;%s;%s;%s', [
              FormatDateTime('yyyy-mm-dd hh:nn:ss', LogItem.TimeStamp),
              LogItem.LogTypeAsString,
              LogItem.LogMessage,
              LogItem.LogTag]);
          end;
        GLogWriter := BuildLogWriter([lLog]);
      end;
    finally
      TMonitor.Exit(GLock);
    end;
  end;
  Result := GLogWriter;
end;

procedure TMyWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // session timeout (0 means session cookie)
      Config[TMVCConfigKey.SessionTimeout] := '0';
      // default content-type
      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      // default content charset
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      // unhandled actions are permitted?
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      // default view file extension
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
      // view path
      Config[TMVCConfigKey.ViewPath] := 'templates';
      // Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'false';
    end);
  FMVC.AddController(TMainController);
  FMVC.AddMiddleware(TMVCAnalyticsMiddleware.Create(GetLoggerForAnalytics));
  FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create(
    '/', { StaticFilesPath }
    TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), 'www'), { DocumentRoot }
    'index.html', {IndexDocument - Before it was named fallbackresource}
    False
    ));
end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

initialization

GLock := TObject.Create;

finalization

GLock.Free;

end.
