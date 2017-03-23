// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Middleware.CORS;

{$I dmvcframework.inc}

interface

uses
  System.StrUtils,
  MVCFramework,
  MVCFramework.Commons;

type

  TMVCCORSMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    FAllowedOriginURL: string;
    FAllowsCredentials: string;
  protected
    procedure OnBeforeRouting(
      AContext: TWebContext;
      var AHandled: Boolean
      );

    procedure OnBeforeControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string;
      const AActionName: string;
      var AHandled: Boolean
      );

    procedure OnAfterControllerAction(
      AContext: TWebContext;
      const AActionName: string;
      const AHandled: Boolean
      );
  public
    constructor Create(const AAllowedOriginURL: string = '*'; const AAllowsCredentials: Boolean = True); virtual;
  end;

  TCORSMiddleware = TMVCCORSMiddleware;

implementation

{ TMVCCORSMiddleware }

constructor TMVCCORSMiddleware.Create(const AAllowedOriginURL: string; const AAllowsCredentials: Boolean);
begin
  inherited Create;
  FAllowedOriginURL := AAllowedOriginURL;
  FAllowsCredentials := IfThen(AAllowsCredentials, 'true', 'false');
end;

procedure TMVCCORSMiddleware.OnAfterControllerAction(AContext: TWebContext;
  const AActionName: string; const AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCCORSMiddleware.OnBeforeControllerAction(
  AContext: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCCORSMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
begin
  AContext.Response.RawWebResponse.CustomHeaders.Values['Access-Control-Allow-Origin'] := FAllowedOriginURL;
  AContext.Response.RawWebResponse.CustomHeaders.Values['Access-Control-Allow-Methods'] := 'POST, GET, OPTIONS, PUT, DELETE';
  AContext.Response.RawWebResponse.CustomHeaders.Values['Access-Control-Allow-Headers'] := 'Content-Type, Accept, jwtusername, jwtpassword';
  AContext.Response.RawWebResponse.CustomHeaders.Values['Access-Control-Allow-Credentials'] := FAllowsCredentials;
  if (AContext.Request.HTTPMethod = httpOPTIONS) then
  begin
    AContext.Response.StatusCode := HTTP_STATUS.OK;
    AHandled := True;
  end;
end;

end.
