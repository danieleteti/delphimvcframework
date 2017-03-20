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

interface

uses
  System.StrUtils,
  MVCFramework,
  MVCFramework.Commons;

type

  TCORSMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    FAllowedOriginURL: string;
    FAllowsCredentials: string;
  protected
    { protected declarations }
  public
    constructor Create(const AllowedOriginURL: string = '*'; const AllowsCredentials: Boolean = true); virtual;

    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext; const AActionName: string; const AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string; const AActionName: string; var AHandled: Boolean);
  end;

implementation


{ TCORSMiddleware }

constructor TCORSMiddleware.Create(const AllowedOriginURL: string; const AllowsCredentials: Boolean);
begin
  inherited Create;
  FAllowedOriginURL := AllowedOriginURL;
  FAllowsCredentials := ifthen(AllowsCredentials, 'true', 'false');
end;

procedure TCORSMiddleware.OnAfterControllerAction(
  AContext: TWebContext;
  const AActionName: string;
  const AHandled: Boolean);
begin
  // do nothing
end;

procedure TCORSMiddleware.OnBeforeControllerAction(
  AContext: TWebContext;
  const AControllerQualifiedClassName,
  AActionName: string;
  var AHandled: Boolean);
begin
  // do nothing
end;

procedure TCORSMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
begin
  AContext.Response.RawWebResponse.CustomHeaders.Values['Access-Control-Allow-Origin'] := FAllowedOriginURL;
  AContext.Response.RawWebResponse.CustomHeaders.Values['Access-Control-Allow-Methods'] := 'POST, GET, OPTIONS, PUT, DELETE';
  AContext.Response.RawWebResponse.CustomHeaders.Values['Access-Control-Allow-Headers'] := 'Content-Type, Accept, jwtusername, jwtpassword';
  AContext.Response.RawWebResponse.CustomHeaders.Values['Access-Control-Allow-Credentials'] := FAllowsCredentials;

  if (AContext.Request.HTTPMethod = httpOPTIONS) then
  begin
    AContext.Response.StatusCode := 200;
    AHandled := True;
  end;
end;

end.
