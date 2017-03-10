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

unit MVCFramework.Middleware.JWT;

interface

{$I dmvcframework.inc}


uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger,
  MVCFramework.JWT,
  System.Generics.Collections,
  System.DateUtils, System.SysUtils;

type
  TJWTClaimsSetup = reference to procedure(const JWT: TJWT);

  TMVCJwtAuthenticationMiddleware = class(TInterfacedObject, IMVCMiddleware)
  strict private
    FMVCAuthenticationHandler: IMVCAuthenticationHandler;

    procedure Render(const aErrorCode: UInt16; const aErrorMessage: string; aContext: TWebContext;
      const aErrorClassName: string = ''); overload;
  private
    FClaimsToChecks: TJWTCheckableClaims;
    FSetupJWTClaims: TJWTClaimsSetup;
    FLoginURLSegment: string;

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
      aConfigClaims: TJWTClaimsSetup;
      aSecret: string = 'D3lph1MVCFram3w0rk';
      aLoginURLSegment: string = '/login';
      aClaimsToCheck: TJWTCheckableClaims = [
      TJWTCheckableClaim.ExpirationTime,
      TJWTCheckableClaim.NotBefore,
      TJWTCheckableClaim.IssuedAt
      ]); virtual;
  end;

implementation

uses
  MVCFramework.Session

  {$IFDEF SYSTEMJSON}

    , System.JSON

  {$ELSE}

    , Data.DBXJSON

  {$ENDIF}
  {$IFDEF WEBAPACHEHTTP}

    , Web.ApacheHTTP

  {$ENDIF}
  {$IFDEF SYSTEMNETENCODING}

    , System.NetEncoding

  {$ELSE}

    , Soap.EncdDecd

  {$ENDIF};

{ TMVCSalutationMiddleware }

constructor TMVCJwtAuthenticationMiddleware.Create(AMVCAuthenticationHandler: IMVCAuthenticationHandler;
  aConfigClaims: TJWTClaimsSetup;
  aSecret: string = 'D3lph1MVCFram3w0rk';
  aLoginURLSegment: string = '/login';
  aClaimsToCheck: TJWTCheckableClaims = [
  TJWTCheckableClaim.ExpirationTime,
  TJWTCheckableClaim.NotBefore,
  TJWTCheckableClaim.IssuedAt
  ]);
begin
  inherited Create;
  FMVCAuthenticationHandler := AMVCAuthenticationHandler;
  FSecret := aSecret;
  FLoginURLSegment := aLoginURLSegment;
  FClaimsToChecks := aClaimsToCheck;
  FSetupJWTClaims := aConfigClaims;
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

    // retrieve the token from the "authentication bearer" header
    lToken := '';
    if lAuthHeader.StartsWith('bearer', True) then
    begin
      lToken := lAuthHeader.Remove(0, 'bearer'.Length).Trim;
    end;

    // check the jwt
    if not lJWT.IsValidToken(lToken, lError) then
    begin
      Render(http_status.Unauthorized, lError, Context);
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
        Context.LoggedUser.UserName := lJWT.CustomClaims['username'];
        Context.LoggedUser.Roles.AddRange(lJWT.CustomClaims['roles'].Split([',']));
        Context.LoggedUser.LoggedSince := lJWT.Claims.IssuedAt;
        FMVCAuthenticationHandler.OnAuthorization(Context.LoggedUser.Roles,
          AControllerQualifiedClassName, AActionName, lIsAuthorized);
        if lIsAuthorized then
        begin
          Context.LoggedUser.CustomData := lJWT.CustomClaims.AsCustomData;
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
  lCustomData: TMVCCustomData;
  lIsValid: Boolean;
  lJWT: TJWT;
  lPair: TPair<String, String>;
begin
  if (Context.Request.HTTPMethod = httpPOST) and SameText(Context.Request.PathInfo, FLoginURLSegment) then
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
      lCustomData := TMVCCustomData.Create;
      try
        FMVCAuthenticationHandler.OnAuthentication(lUserName, lPassword,
          lRoles, lIsValid, lCustomData);
        if lIsValid then
        begin
          lJWT := TJWT.Create(FSecret);
          try
            // CustomData becomes custom claims
            for lPair in lCustomData do
            begin
              lJWT.CustomClaims[lPair.Key] := lPair.Value;
            end;

            // let's user config additional claims and custom claims
            FSetupJWTClaims(lJWT);

            // these claims are mandatory and managed by the middleware
            if not lJWT.CustomClaims['username'].IsEmpty then
              raise EMVCJWTException.Create
                ('Custom claim "username" is reserved and cannot be modified in the JWT setup nor in CustomData');
            if not lJWT.CustomClaims['roles'].IsEmpty then
              raise EMVCJWTException.Create
                ('Custom claim "roles" is reserved and cannot be modified in the JWT setup nor in CustomData');

            lJWT.CustomClaims['username'] := lUserName;
            lJWT.CustomClaims['roles'] := String.Join(',', lRoles.ToArray);

            /// / setup the current logged user from the JWT
            // Context.LoggedUser.Roles.AddRange(lRoles);
            // Context.LoggedUser.UserName := lJWT.CustomClaims['username'];
            // Context.LoggedUser.LoggedSince := lJWT.Claims.IssuedAt;
            // Context.LoggedUser.Realm := lJWT.Claims.Subject;
            // Context.LoggedUser.CustomData :=
            /// ////////////////////////////////////////////////

            InternalRender(TJSONObject.Create(TJSONPair.Create('token', lJWT.GetToken)),
              TMVCMediaType.APPLICATION_JSON,
              TMVCConstants.DEFAULT_CONTENT_CHARSET, Context);
            Handled := True;
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
        lCustomData.Free;
      end;
    finally
      lRoles.Free;
    end;
  end;
end;

procedure TMVCJwtAuthenticationMiddleware.Render(const aErrorCode: UInt16;
  const aErrorMessage: string; aContext: TWebContext;
  const aErrorClassName: string = '');
var
  j: TJSONObject;
  status: string;
begin
  aContext.Response.StatusCode := aErrorCode;
  aContext.Response.ReasonString := aErrorMessage;
  status := 'error';
  if (aErrorCode div 100) = 2 then
    status := 'ok';
  j := TJSONObject.Create;
  j.AddPair('status', status);
  if aErrorClassName = '' then
    j.AddPair('classname', TJSONNull.Create)
  else
    j.AddPair('classname', aErrorClassName);
  j.AddPair('message', aErrorMessage);

  InternalRender(j, TMVCConstants.DEFAULT_CONTENT_TYPE,
    TMVCConstants.DEFAULT_CONTENT_CHARSET, aContext);

end;

end.
