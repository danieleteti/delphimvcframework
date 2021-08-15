// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2021 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Middleware.JWTBlacklist;

{$I dmvcframework.inc}

interface

uses
  System.StrUtils,
  MVCFramework,
  MVCFramework.Commons;

type
  TMVCOnAcceptTokenProc = reference to procedure(AContext: TWebContext; AJWTToken: String;
    var AAccepted: Boolean);
  TMVCOnNewJWTToBlackList = reference to procedure(AContext: TWebContext; AJWTToken: String);

  TMVCJWTBlackListMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fOnAcceptToken: TMVCOnAcceptTokenProc;
    fOnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
    fBlackListRequestURLSegment: string;
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

    procedure OnAfterRouting(
      AContext: TWebContext;
      const AHandled: Boolean
      );
  public
    constructor Create(
      OnAcceptToken: TMVCOnAcceptTokenProc;
      OnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
      BlackListRequestURLSegment: string = '/logout');
  end;

implementation

uses
  MVCFramework.Middleware.JWT, System.SysUtils, System.NetEncoding, MVCFramework.Logger;

constructor TMVCJWTBlackListMiddleware.Create(
  OnAcceptToken: TMVCOnAcceptTokenProc;
  OnNewJWTToBlackList: TMVCOnNewJWTToBlackList;
  BlackListRequestURLSegment: string = '/logout');
begin
  inherited Create;
  fOnAcceptToken := OnAcceptToken;
  fOnNewJWTToBlackList := OnNewJWTToBlackList;
  fBlackListRequestURLSegment := BlackListRequestURLSegment;
  Assert(Assigned(fOnAcceptToken));
  Assert(not fBlackListRequestURLSegment.IsEmpty);
end;

procedure TMVCJWTBlackListMiddleware.OnAfterControllerAction(AContext: TWebContext;
  const AActionName: string; const AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCJWTBlackListMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCJWTBlackListMiddleware.OnBeforeControllerAction(
  AContext: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var AHandled: Boolean);
begin
  // Implement as needed
end;

procedure TMVCJWTBlackListMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
var
  lAuthHeader: string;
  lAuthToken: string;
  lAccepted: Boolean;
begin
  lAuthToken := '';
  lAuthHeader := AContext.Request.Headers[TMVCJWTDefaults.AUTHORIZATION_HEADER];
  if not lAuthHeader.IsEmpty then
  begin
    // retrieve the token from the "authentication Bearer" header
    if lAuthHeader.Substring(0, TMVCJWTDefaults.AUTH_SCHEMA.Length).ToLower = 'bearer' then
    begin
      lAuthToken := lAuthHeader.Remove(0, TMVCJWTDefaults.AUTH_SCHEMA.Length).Trim;
      lAuthToken := Trim(TNetEncoding.URL.Decode(lAuthToken));
    end;
  end;

  if SameText(AContext.Request.PathInfo, fBlackListRequestURLSegment) then
  begin
    // add the token in the blacklist
    if lAuthToken.IsEmpty then
    begin
      raise EMVCException.Create(HTTP_STATUS.BadRequest,
        'JWTToken required - cannot blacklist an unknown token');
    end;
    fOnNewJWTToBlackList(AContext, lAuthToken);
    AContext.Response.StatusCode := HTTP_STATUS.NoContent;
    AHandled := True;
  end
  else
  begin
    // just check if token is blacklisted.
    // if the token is not available, just ignore the check
    // remember, here jwtmiddleware already did its job.
    if lAuthToken.IsEmpty then
    begin
      AHandled := False;
    end
    else
    begin
      lAccepted := True;
      fOnAcceptToken(AContext, lAuthToken, lAccepted);
      if not lAccepted then
      begin
        raise EMVCJWTException.Create(HTTP_STATUS.Forbidden, 'JWT not accepted');
      end;
    end;
  end;
end;

end.
