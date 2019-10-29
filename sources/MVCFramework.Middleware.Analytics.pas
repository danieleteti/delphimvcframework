// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team
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
    FLogWriter: ILogWriter;
  protected
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnAfterControllerAction(Context: TWebContext; const AActionNAme: string; const Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext; const AControllerQualifiedClassName: string; const AActionNAme: string;
      var Handled: Boolean);
  public
    constructor Create(const ALogWriter: ILogWriter); virtual;

  end;

implementation

uses
  System.SysUtils, System.DateUtils;

{ TMVCAnalyticsMiddleware }

constructor TMVCAnalyticsMiddleware.Create(const ALogWriter: ILogWriter);
begin
  inherited Create;
  FLogWriter := ALogWriter;
end;

procedure TMVCAnalyticsMiddleware.OnAfterControllerAction(Context: TWebContext; const AActionNAme: string; const Handled: Boolean);
begin
  // do nothing
end;

procedure TMVCAnalyticsMiddleware.OnBeforeControllerAction(Context: TWebContext; const AControllerQualifiedClassName, AActionNAme: string;
  var Handled: Boolean);
begin
  FLogWriter.Info(Context.Request.ClientIp + ';' + AControllerQualifiedClassName + ';' + AActionNAme + ';' +
    Context.Request.RawWebRequest.Method + ';' + Context.Request.RawWebRequest.PathTranslated + ';' + Context.Request.RawWebRequest.PathInfo
    + ';' + Context.Request.RawWebRequest.Referer + ';' + Context.Request.RawWebRequest.Host, 'analytics');
end;

procedure TMVCAnalyticsMiddleware.OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
begin
  // do nothing
end;

end.
