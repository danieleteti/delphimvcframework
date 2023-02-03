// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Middleware.Redirect;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.Commons;

type
  TMVCRedirectMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fRedirectToURL: string;
    fRequestedPathInfos: TArray<String>;
  protected
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string; var Handled: Boolean);
    procedure OnAfterRouting(Context: TWebContext; const AHandled: Boolean);
    procedure OnAfterControllerAction(Context: TWebContext; const AControllerQualifiedClassName: string; const AActionName: string; const AHandled: Boolean);
  public
    constructor Create(
      const RequestedPathInfos: TArray<String>;
      const RedirectToURL: String);
  end;

implementation

uses
  System.SysUtils,
  System.Classes;

constructor TMVCRedirectMiddleware.Create(
  const RequestedPathInfos: TArray<String>;
  const RedirectToURL: String);
begin
  inherited Create;
  fRequestedPathInfos := RequestedPathInfos;
  fRedirectToURL := RedirectToURL;
end;

procedure TMVCRedirectMiddleware.OnAfterControllerAction(
  Context: TWebContext; const AControllerQualifiedClassName: string;
  const AActionName: string; const AHandled: Boolean);
begin
end;

procedure TMVCRedirectMiddleware.OnAfterRouting(Context: TWebContext; const AHandled: Boolean);
begin
end;

procedure TMVCRedirectMiddleware.OnBeforeControllerAction(Context: TWebContext;
  const AControllerQualifiedClassName, AActionNAme: string; var Handled: Boolean);
begin
end;

procedure TMVCRedirectMiddleware.OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
var
  I: Integer;
  lPathInfo: string;
begin
  if not Handled then
  begin
    lPathInfo := Context.Request.PathInfo;
    for I := 0 to Pred(Length(fRequestedPathInfos)) do
    begin
      if lPathInfo = fRequestedPathInfos[I] then
      begin
        Context.Response.RawWebResponse.SendRedirect(fRedirectToURL);
        LogI(Format('Redirected from [%s] to [%s]', [lPathInfo, fRedirectToURL]));
        Handled := True;
      end;
    end;
  end;
end;

end.
