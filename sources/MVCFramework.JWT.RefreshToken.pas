// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
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
// ***************************************************************************

unit MVCFramework.JWT.RefreshToken;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.DateUtils,
  System.SyncObjs,
  System.Hash,
  System.Generics.Collections,
  JsonDataObjects,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.JWT,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.JsonDataObjects;

type
  TMVCRefreshTokenStatus = (rtsOK, rtsNotFound, rtsExpired, rtsReuseDetected);

  TMVCRefreshTokenRotation = record
    Status: TMVCRefreshTokenStatus;
    Username: string;
    Roles: string;
    NewRefreshToken: string;
  end;

  TMVCRefreshTokenConfig = record
    Secret: string;
    HMACAlgorithm: string;
    Issuer: string;
    RefreshURLSegment: string;
    AccessTokenTTLSeconds: Integer;
    RefreshTokenTTLSeconds: Integer;
    class function Default(const ASecret: string): TMVCRefreshTokenConfig; static;
  end;

  // Pluggable persistence for refresh tokens. The framework provides the HTTP
  // plumbing, rotation and reuse-detection logic; the store only persists.
  IMVCRefreshTokenStore = interface
    ['{6B2E0E9A-9D3C-4C8E-9C2A-2D7F0E1A4B11}']
    // Creates a brand-new token family and returns the first opaque token.
    function Issue(const AUsername, ARoles: string; const ATTLSeconds: Integer): string;
    // Consumes AOldRefreshToken and mints a new one in the same family.
    // Detects replay of an already-rotated token (reuse) and revokes the family.
    function Rotate(const AOldRefreshToken: string; const ATTLSeconds: Integer): TMVCRefreshTokenRotation;
    // Revokes the whole family the token belongs to (e.g. on logout).
    procedure Revoke(const ARefreshToken: string);
  end;

  // Transport-agnostic domain logic. One implementation, multiple adapters
  // (classic middleware below, HTTPFilter in the .Filters companion unit).
  IMVCRefreshTokenCore = interface
    ['{C1A7F4D2-3B5E-4A1C-8E6D-9F0B2C3D4E5F}']
    // True if the request was the refresh endpoint (it owns and renders it).
    function HandleRefreshRequest(const AContext: TWebContext): Boolean;
    // Issues a fresh access+refresh pair and renders it on AContext.Response.
    procedure IssueTokenPair(const AContext: TWebContext; const AUsername, ARoles: string);
    // Issues a fresh pair without touching the context (caller builds the response).
    procedure NewTokenPair(const AUsername, ARoles: string; out AAccessToken, ARefreshToken: string);
    function BuildAccessToken(const AUsername, ARoles: string): string;
  end;

  TMVCRefreshTokenCore = class(TInterfacedObject, IMVCRefreshTokenCore)
  private
    fConfig: TMVCRefreshTokenConfig;
    fStore: IMVCRefreshTokenStore;
    procedure RenderTokenPair(const AContext: TWebContext; const AAccess, ARefresh: string);
    procedure RenderError(const AContext: TWebContext; const AStatus: Integer; const AMessage: string);
  public
    constructor Create(const AConfig: TMVCRefreshTokenConfig; const AStore: IMVCRefreshTokenStore);
    function HandleRefreshRequest(const AContext: TWebContext): Boolean;
    procedure IssueTokenPair(const AContext: TWebContext; const AUsername, ARoles: string);
    procedure NewTokenPair(const AUsername, ARoles: string; out AAccessToken, ARefreshToken: string);
    function BuildAccessToken(const AUsername, ARoles: string): string;
  end;

  // Classic middleware adapter: intercepts the refresh URL in OnBeforeRouting.
  TMVCJWTRefreshTokenMiddleware = class(TInterfacedObject, IMVCMiddleware)
  private
    fCore: IMVCRefreshTokenCore;
  public
    constructor Create(const AConfig: TMVCRefreshTokenConfig; const AStore: IMVCRefreshTokenStore);
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    procedure OnBeforeControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string; var AHandled: Boolean);
    procedure OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string; const AHandled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  end;

  // Reference in-memory store. Rotation + reuse-detection via token families.
  // Not durable across restarts: use the ActiveRecord store in production.
  TMVCInMemoryRefreshTokenStore = class(TInterfacedObject, IMVCRefreshTokenStore)
  private type
    TEntry = record
      FamilyID: string;
      Username: string;
      Roles: string;
      ExpiresAt: TDateTime;
      Used: Boolean;
    end;
  private
    fLock: TCriticalSection;
    fByHash: TDictionary<string, TEntry>;
    procedure RevokeFamilyLocked(const AFamilyID: string);
  public
    constructor Create;
    destructor Destroy; override;
    function Issue(const AUsername, ARoles: string; const ATTLSeconds: Integer): string;
    function Rotate(const AOldRefreshToken: string; const ATTLSeconds: Integer): TMVCRefreshTokenRotation;
    procedure Revoke(const ARefreshToken: string);
  end;

// SHA-256 of an opaque token. Stores keep the hash, never the token itself.
function MVCRefreshTokenHash(const AToken: string): string;
// A random, opaque (non-JWT, so revocable) refresh token.
function MVCNewOpaqueRefreshToken: string;

implementation

function MVCRefreshTokenHash(const AToken: string): string;
begin
  Result := THashSHA2.GetHashString(AToken, THashSHA2.TSHA2Version.SHA256);
end;

function MVCNewOpaqueRefreshToken: string;
var
  lG1, lG2: TGUID;
begin
  CreateGUID(lG1);
  CreateGUID(lG2);
  Result := GUIDToString(lG1) + GUIDToString(lG2);
  Result := Result.Replace('{', '').Replace('}', '').Replace('-', '');
end;

{ TMVCRefreshTokenConfig }

class function TMVCRefreshTokenConfig.Default(const ASecret: string): TMVCRefreshTokenConfig;
begin
  Result.Secret := ASecret;
  Result.HMACAlgorithm := 'HS512';
  Result.Issuer := 'DMVCFramework';
  Result.RefreshURLSegment := '/auth/refresh';
  Result.AccessTokenTTLSeconds := 15 * 60;
  Result.RefreshTokenTTLSeconds := 7 * 24 * 60 * 60;
end;

{ TMVCRefreshTokenCore }

constructor TMVCRefreshTokenCore.Create(const AConfig: TMVCRefreshTokenConfig;
  const AStore: IMVCRefreshTokenStore);
begin
  inherited Create;
  fConfig := AConfig;
  fStore := AStore;
end;

function TMVCRefreshTokenCore.BuildAccessToken(const AUsername, ARoles: string): string;
var
  lJWT: TJWT;
begin
  lJWT := TJWT.Create(fConfig.Secret, 0, fConfig.HMACAlgorithm);
  try
    lJWT.Claims.Issuer := fConfig.Issuer;
    lJWT.Claims.Subject := AUsername;
    lJWT.Claims.IssuedAt := Now;
    lJWT.Claims.ExpirationTime := Now + (fConfig.AccessTokenTTLSeconds * OneSecond);
    lJWT.CustomClaims['username'] := AUsername;
    lJWT.CustomClaims['roles'] := ARoles;
    Result := lJWT.GetToken;
  finally
    lJWT.Free;
  end;
end;

procedure TMVCRefreshTokenCore.RenderTokenPair(const AContext: TWebContext;
  const AAccess, ARefresh: string);
var
  lJSON: TJSONObject;
begin
  lJSON := TJSONObject.Create;
  try
    lJSON.S['access_token'] := AAccess;
    lJSON.S['refresh_token'] := ARefresh;
    lJSON.S['token_type'] := 'bearer';
    lJSON.I['expires_in'] := fConfig.AccessTokenTTLSeconds;
    AContext.Response.StatusCode := HTTP_STATUS.OK;
    AContext.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
    AContext.Response.Content := lJSON.ToJSON(False);
  finally
    lJSON.Free;
  end;
end;

procedure TMVCRefreshTokenCore.RenderError(const AContext: TWebContext;
  const AStatus: Integer; const AMessage: string);
var
  lJSON: TJSONObject;
begin
  lJSON := TJSONObject.Create;
  try
    lJSON.S['error'] := AMessage;
    AContext.Response.StatusCode := AStatus;
    AContext.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
    AContext.Response.Content := lJSON.ToJSON(False);
  finally
    lJSON.Free;
  end;
end;

procedure TMVCRefreshTokenCore.NewTokenPair(const AUsername, ARoles: string;
  out AAccessToken, ARefreshToken: string);
begin
  ARefreshToken := fStore.Issue(AUsername, ARoles, fConfig.RefreshTokenTTLSeconds);
  AAccessToken := BuildAccessToken(AUsername, ARoles);
end;

procedure TMVCRefreshTokenCore.IssueTokenPair(const AContext: TWebContext;
  const AUsername, ARoles: string);
var
  lAccess, lRefresh: string;
begin
  NewTokenPair(AUsername, ARoles, lAccess, lRefresh);
  RenderTokenPair(AContext, lAccess, lRefresh);
end;

function TMVCRefreshTokenCore.HandleRefreshRequest(const AContext: TWebContext): Boolean;
var
  lBody: TJSONObject;
  lRefresh: string;
  lRot: TMVCRefreshTokenRotation;
begin
  Result := False;
  if not SameText(AContext.Request.PathInfo, fConfig.RefreshURLSegment) then
    Exit;
  Result := True; // from here this URL is ours: always short-circuit

  lRefresh := '';
  lBody := StrToJSONObject(AContext.Request.Body);
  try
    if Assigned(lBody) then
      lRefresh := lBody.S['refresh_token'];
  finally
    lBody.Free;
  end;

  if lRefresh.IsEmpty then
  begin
    RenderError(AContext, HTTP_STATUS.BadRequest, 'Missing refresh_token');
    Exit;
  end;

  lRot := fStore.Rotate(lRefresh, fConfig.RefreshTokenTTLSeconds);
  case lRot.Status of
    rtsOK:
      RenderTokenPair(AContext, BuildAccessToken(lRot.Username, lRot.Roles), lRot.NewRefreshToken);
    rtsReuseDetected:
      RenderError(AContext, HTTP_STATUS.Unauthorized, 'Refresh token reuse detected; session revoked');
  else
    RenderError(AContext, HTTP_STATUS.Unauthorized, 'Invalid or expired refresh token');
  end;
end;

{ TMVCJWTRefreshTokenMiddleware }

constructor TMVCJWTRefreshTokenMiddleware.Create(const AConfig: TMVCRefreshTokenConfig;
  const AStore: IMVCRefreshTokenStore);
begin
  inherited Create;
  fCore := TMVCRefreshTokenCore.Create(AConfig, AStore);
end;

procedure TMVCJWTRefreshTokenMiddleware.OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
begin
  AHandled := fCore.HandleRefreshRequest(AContext);
end;

procedure TMVCJWTRefreshTokenMiddleware.OnBeforeControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName: string; const AActionName: string; var AHandled: Boolean);
begin
  // no-op
end;

procedure TMVCJWTRefreshTokenMiddleware.OnAfterControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName: string; const AActionName: string; const AHandled: Boolean);
begin
  // no-op
end;

procedure TMVCJWTRefreshTokenMiddleware.OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
begin
  // no-op
end;

{ TMVCInMemoryRefreshTokenStore }

constructor TMVCInMemoryRefreshTokenStore.Create;
begin
  inherited Create;
  fLock := TCriticalSection.Create;
  fByHash := TDictionary<string, TEntry>.Create;
end;

destructor TMVCInMemoryRefreshTokenStore.Destroy;
begin
  fByHash.Free;
  fLock.Free;
  inherited;
end;

procedure TMVCInMemoryRefreshTokenStore.RevokeFamilyLocked(const AFamilyID: string);
var
  lPair: TPair<string, TEntry>;
  lKeys: TList<string>;
  lKey: string;
begin
  lKeys := TList<string>.Create;
  try
    for lPair in fByHash do
      if lPair.Value.FamilyID = AFamilyID then
        lKeys.Add(lPair.Key);
    for lKey in lKeys do
      fByHash.Remove(lKey);
  finally
    lKeys.Free;
  end;
end;

function TMVCInMemoryRefreshTokenStore.Issue(const AUsername, ARoles: string;
  const ATTLSeconds: Integer): string;
var
  lEntry: TEntry;
  lGuid: TGUID;
  lToken: string;
begin
  lToken := MVCNewOpaqueRefreshToken;
  CreateGUID(lGuid);
  lEntry.FamilyID := GUIDToString(lGuid);
  lEntry.Username := AUsername;
  lEntry.Roles := ARoles;
  lEntry.ExpiresAt := Now + (ATTLSeconds * OneSecond);
  lEntry.Used := False;
  fLock.Enter;
  try
    fByHash.Add(MVCRefreshTokenHash(lToken), lEntry);
  finally
    fLock.Leave;
  end;
  Result := lToken;
end;

function TMVCInMemoryRefreshTokenStore.Rotate(const AOldRefreshToken: string;
  const ATTLSeconds: Integer): TMVCRefreshTokenRotation;
var
  lHash, lNewToken: string;
  lOld, lNew: TEntry;
begin
  Result.Status := rtsNotFound;
  Result.NewRefreshToken := '';
  lHash := MVCRefreshTokenHash(AOldRefreshToken);
  fLock.Enter;
  try
    if not fByHash.TryGetValue(lHash, lOld) then
      Exit;
    if lOld.Used then
    begin
      // an already-rotated token presented again => stolen. Kill the family.
      RevokeFamilyLocked(lOld.FamilyID);
      Result.Status := rtsReuseDetected;
      Exit;
    end;
    if lOld.ExpiresAt < Now then
    begin
      fByHash.Remove(lHash);
      Result.Status := rtsExpired;
      Exit;
    end;
    lOld.Used := True; // keep, so reuse stays detectable
    fByHash.AddOrSetValue(lHash, lOld);
    lNewToken := MVCNewOpaqueRefreshToken;
    lNew := lOld;
    lNew.Used := False;
    lNew.ExpiresAt := Now + (ATTLSeconds * OneSecond);
    fByHash.Add(MVCRefreshTokenHash(lNewToken), lNew); // same FamilyID
    Result.Status := rtsOK;
    Result.Username := lOld.Username;
    Result.Roles := lOld.Roles;
    Result.NewRefreshToken := lNewToken;
  finally
    fLock.Leave;
  end;
end;

procedure TMVCInMemoryRefreshTokenStore.Revoke(const ARefreshToken: string);
var
  lEntry: TEntry;
begin
  fLock.Enter;
  try
    if fByHash.TryGetValue(MVCRefreshTokenHash(ARefreshToken), lEntry) then
      RevokeFamilyLocked(lEntry.FamilyID);
  finally
    fLock.Leave;
  end;
end;

end.
