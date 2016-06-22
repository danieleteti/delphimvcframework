// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2016 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Middleware.JWT;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger,
  MVCFramework.JWT,
  System.Generics.Collections,
  System.DateUtils;

type
  TMVCJwtAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  strict private
    FMVCAuthenticationHandler: IMVCAuthenticationHandler;

    procedure Render(const AErrorCode: UInt16; const AErrorMessage: string; Context: TWebContext;
      const AErrorClassName: string = ''); overload;
  private
    FClaimsToChecks: TJWTCheckableClaims;

  protected
    FSecret: string;
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnAfterControllerAction(Context: TWebContext;
      const AActionName: string; const Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var Handled: Boolean);
  public
    constructor Create(AMVCAuthenticationHandler: IMVCAuthenticationHandler;
      ASecret: string = 'D3lph1MVCFram3w0rk'; ClaimsToCheck: TJWTCheckableClaims = [
      TJWTCheckableClaim.ExpirationTime,
      TJWTCheckableClaim.NotBefore,
      TJWTCheckableClaim.IssuedAt
      ]); virtual;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Session
{$IF CompilerVersion < 27}
    , Data.DBXJSON
{$ELSE}
    , System.JSON, Web.ApacheHTTP
{$ENDIF}
{$IF CompilerVersion >= 21}
    , System.NetEncoding
{$ELSE}
    , Soap.EncdDecd
{$ENDIF};

{ TMVCSalutationMiddleware }

constructor TMVCJwtAuthenticationMiddleware.Create(AMVCAuthenticationHandler
  : IMVCAuthenticationHandler;
  ASecret: string = 'D3lph1MVCFram3w0rk'; ClaimsToCheck: TJWTCheckableClaims = [
  TJWTCheckableClaim.ExpirationTime,
  TJWTCheckableClaim.NotBefore,
  TJWTCheckableClaim.IssuedAt
  ]);
begin
  inherited Create;
  FMVCAuthenticationHandler := AMVCAuthenticationHandler;
  FSecret := ASecret;
  FClaimsToChecks := ClaimsToCheck;
end;

procedure TMVCJwtAuthenticationMiddleware.OnAfterControllerAction
  (Context: TWebContext; const AActionName: string; const Handled: Boolean);
begin
  // do nothing
end;

procedure TMVCJwtAuthenticationMiddleware.OnBeforeControllerAction
  (Context: TWebContext; const AControllerQualifiedClassName,
  AActionName: string; var Handled: Boolean);
var
  lAuthRequired: Boolean;
  lIsAuthorized: Boolean;
  lJWT: TJWT;
  lAuthHeader: string;
  lToken: string;
  lError: String;
begin
  // check if the resource is protected
  FMVCAuthenticationHandler.OnRequest(AControllerQualifiedClassName,
    AActionName, lAuthRequired);
  if not lAuthRequired then
  begin
    Handled := False;
    Exit;
  end;

  // Checking token in subsequent requests
  // ***************************************************
  lJWT := TJWT.Create(FSecret);
  try
    lJWT.RegClaimsToChecks := Self.FClaimsToChecks;
    lAuthHeader := Context.Request.Headers['Authentication'];
    if lAuthHeader.IsEmpty then
    begin
      Render(http_status.Unauthorized, 'Authentication Required', Context);
      Handled := True;
      Exit;
    end;

    if lAuthHeader.StartsWith('bearer', True) then
    begin
      lToken := lAuthHeader.Remove(0, 'bearer'.Length).Trim;
    end;

    if not lJWT.IsValidToken(lToken, lError) then
    begin
      Render(http_status.Unauthorized, 'Invalid Token, ' + lError, Context);
      Handled := True;
    end
    else
    begin
      lJWT.LoadToken(lToken);
      if lJWT.CustomClaims['username'].IsEmpty then
      begin
        Render(http_status.Unauthorized, 'Invalid Token, Authentication Required', Context);
        Handled := True;
      end
      else
      begin
        lIsAuthorized := False;
        FMVCAuthenticationHandler.OnAuthorization(Context.LoggedUser.Roles,
          AControllerQualifiedClassName, AActionName, lIsAuthorized);
        if lIsAuthorized then
        begin
          Handled := False;
        end
        else
        begin
          Render(http_status.Forbidden, 'Authorization Forbidden', Context);
          Handled := True;
        end;
      end;
    end;
  finally
    lJWT.Free;
  end;

end;

procedure TMVCJwtAuthenticationMiddleware.OnBeforeRouting
  (Context: TWebContext; var Handled: Boolean);
var
  lUserName: string;
  lPassword: string;
  lRoles: TList<String>;
  lSessionData: TSessionData;
  lIsValid: Boolean;
  lJWT: TJWT;
begin
  if SameText(Context.Request.PathInfo, '/login') and (Context.Request.HTTPMethod = httpPOST) then
  begin
    lUserName := Context.Request.Headers['jwtusername'];
    lPassword := Context.Request.Headers['jwtpassword'];
    if (lUserName.IsEmpty) or
      (lPassword.IsEmpty) then
    begin
      Render(http_status.Unauthorized, 'Username and password Required', Context);
      Handled := True;
      Exit;
    end;

    // check the authorization for the requested resource
    lRoles := TList<string>.Create;
    try
      lSessionData := TSessionData.Create;
      try
        FMVCAuthenticationHandler.OnAuthentication(lUserName, lPassword,
          lRoles, lIsValid, lSessionData);
        if lIsValid then
        begin
          lJWT := TJWT.Create(FSecret);
          try
            lJWT.Claims.Issuer := 'Delphi MVC Framework';
            lJWT.Claims.ExpirationTime := Now + OneHour; // todo: customize
            lJWT.Claims.NotBefore := Now - OneMinute * 5; // todo: customize
            lJWT.CustomClaims['username'] := lUserName;
            lJWT.CustomClaims['roles'] := String.Join(',', lRoles.ToArray);

            Context.LoggedUser.Roles.AddRange(lRoles);
            Context.LoggedUser.UserName := lUserName;
            Context.LoggedUser.LoggedSince := Now;

            InternalRender(TJSONObject.Create(TJSONPair.Create('token', lJWT.GetToken)),
              TMVCMediaType.APPLICATION_JSON,
              TMVCConstants.DEFAULT_CONTENT_CHARSET, Context);
            Handled := True;
            Exit;
          finally
            lJWT.Free;
          end;
        end
        else
        begin
          Render(http_status.Forbidden, 'Forbidden', Context);
          Handled := True;
        end;
      finally
        lSessionData.Free;
      end;
    finally
      lRoles.Free;
    end;
  end;
end;

procedure TMVCJwtAuthenticationMiddleware.Render(const AErrorCode: UInt16;
  const AErrorMessage: string; Context: TWebContext;
  const AErrorClassName: string);
var
  j: TJSONObject;
  status: string;
begin
  Context.Response.StatusCode := AErrorCode;
  Context.Response.ReasonString := AErrorMessage;
  status := 'error';
  if (AErrorCode div 100) = 2 then
    status := 'ok';
  j := TJSONObject.Create;
  j.AddPair('status', status);
  if AErrorClassName = '' then
    j.AddPair('classname', TJSONNull.Create)
  else
    j.AddPair('classname', AErrorClassName);
  j.AddPair('message', AErrorMessage);

  InternalRender(j, TMVCConstants.DEFAULT_CONTENT_TYPE,
    TMVCConstants.DEFAULT_CONTENT_CHARSET, Context);

end;

end.
