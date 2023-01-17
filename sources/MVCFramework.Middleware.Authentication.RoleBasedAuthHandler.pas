// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// Contributor of this file: Janidan - https://github.com/janidan
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

unit MVCFramework.Middleware.Authentication.RoleBasedAuthHandler;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Middleware.Authentication,
  System.Generics.Collections,
  System.Rtti;

type
  MVCRoleEval = (reOR, reAND);

  MVCRequiresRoleAttribute = class(MVCRequiresAuthenticationAttribute)
  public const
    DefaultListSeparator = ';';
  private
    FRole: string;
    FRoleEval: MVCRoleEval;
    FListSep: Char;
  public
    constructor Create(const aRole: string); overload;
    constructor Create(const aRole: string; const aRoleEval: MVCRoleEval); overload;
    constructor Create(const aRole: string; const aRoleEval: MVCRoleEval; const aListSep: Char); overload;
    function GetRoles: TArray<string>;
    property RoleEval: MVCRoleEval read FRoleEval;
  end;

  IMVCRoleBasedAuthenticationHandler = interface(IMVCAuthenticationHandler)
    ['{07ABEF93-DBCC-4C55-BD39-BD1F48490A73}']
    // procedure OnAuthorization(const AContext: TWebContext;
    // const AUserRoles: TList<string>;
    // const AControllerQualifiedClassName: string; const AActionName: string;
    // var AIsAuthorized: Boolean);
  end;

  TRoleBasedAuthHandler = class(TInterfacedObject, IMVCAuthenticationHandler, IMVCRoleBasedAuthenticationHandler)
  private
    FRttiContext: TRttiContext;

    function TryGetAttributes<TTypeOfAttribute: TCustomAttribute>(const aAttributes: TArray<TCustomAttribute>;
      out aListOfAttributes: TArray<TTypeOfAttribute>): Boolean;
    function CheckUserRoles(const AContext: TWebContext; const AUserRoles: TList<string>;
      const aRoleAttributes: TArray<MVCRequiresRoleAttribute>): Boolean;

    function ResolveRole(const AContext: TWebContext; const aRole: string): string;
    function CreateParameterNameList(const aTemplate: string): TList<string>;
  public
    procedure OnRequest(const AContext: TWebContext; const ControllerQualifiedClassName: string;
      const ActionName: string; var AuthenticationRequired: Boolean);

    procedure OnAuthentication(const AContext: TWebContext; const UserName: string; const Password: string;
      UserRoles: TList<string>; var IsValid: Boolean; const SessionData: TDictionary<string, string>); virtual;
      abstract;

    procedure OnAuthorization(const AContext: TWebContext; UserRoles: TList<string>;
      const ControllerQualifiedClassName: string; const ActionName: string; var IsAuthorized: Boolean); overload; virtual;
    constructor Create;
    destructor Destroy; override;
  end;

  TMVCRoleBasedAuthMiddleware = class(TMVCCustomAuthenticationMiddleware, IMVCMiddleware)
  private
    fAuthenticationHandler: IMVCRoleBasedAuthenticationHandler;
    procedure DoRoleBasedBeforeControllerAction(const AContext: TWebContext;
      const aHandler: IMVCRoleBasedAuthenticationHandler; const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean);
  protected
    procedure OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean); override;
  public
    constructor Create(const AAuthenticationHandler: IMVCAuthenticationHandler;
      const ALoginUrl: string = '/system/users/logged'); override;
  end;

implementation

uses
  System.SysUtils,
  System.RegularExpressions;

{ MVCRequiresRoleAttribute }

constructor MVCRequiresRoleAttribute.Create(const aRole: string);
begin
  Self.Create(aRole, MVCRoleEval.reAND, DefaultListSeparator);
end;

constructor MVCRequiresRoleAttribute.Create(const aRole: string; const aRoleEval: MVCRoleEval);
begin
  Self.Create(aRole, aRoleEval, DefaultListSeparator);
end;

constructor MVCRequiresRoleAttribute.Create(const aRole: string; const aRoleEval: MVCRoleEval; const aListSep: Char);
begin
  inherited Create;
  FRole := aRole;
  FRoleEval := aRoleEval;
  FListSep := aListSep;
end;

function MVCRequiresRoleAttribute.GetRoles: TArray<string>;
begin
  Result := FRole.Split([FListSep]);
end;

{ TRoleBasedAuthHandler }

function TRoleBasedAuthHandler.CheckUserRoles(const AContext: TWebContext; const AUserRoles: TList<System.string>;
  const aRoleAttributes: TArray<MVCRequiresRoleAttribute>): Boolean;
var
  vAttribute: MVCRequiresRoleAttribute;
  vSingleRole: string;
begin
  // By default we will say that you are good to go.
  Result := True;
  if (Length(aRoleAttributes) = 0) then
    Exit;

  // All Attributes MUST match -> AND evaluation
  for vAttribute in aRoleAttributes do
  // if not AUserRoles.Contains(ResolveRole(AContext, vAttribute.Role)) then
  // Exit(False);
  begin
    if (vAttribute.RoleEval = MVCRoleEval.reAND) then
    begin
      for vSingleRole in vAttribute.GetRoles do
        if not AUserRoles.Contains(ResolveRole(AContext, vSingleRole)) then
          Exit(False);
    end
    else // OR evaluation
    begin
      // By default we assume we have not found the role.
      Result := False;
      for vSingleRole in vAttribute.GetRoles do
        if AUserRoles.Contains(ResolveRole(AContext, vSingleRole)) then
          Result := True;
      // If one of the roles does not match we exit the check.
      if not Result then
        Exit;
    end;
  end;
end;

constructor TRoleBasedAuthHandler.Create;
begin
  inherited Create;
  FRttiContext := TRttiContext.Create;
end;

function TRoleBasedAuthHandler.CreateParameterNameList(const aTemplate: string): TList<string>;
const
  MatchPattern = '\(\$([A-Za-z0-9\_]+)\)'; // Matches ($<name>) placeholders
var
  S: string;
  Matches: TMatchCollection;
  M: TMatch;
  I: Integer;
begin
  Result := TList<string>.Create;
  try
    Matches := TRegEx.Matches(aTemplate, MatchPattern, [roIgnoreCase, roCompiled, roSingleLine]);
    for M in Matches do
      for I := 0 to M.Groups.Count - 1 do
      begin
        S := M.Groups[I].Value;
        if (Length(S) > 0) and (S[1] <> '(') then
        begin
          Result.Add(S);
          Break;
        end;
      end;
  except
    Result.Free;
    raise;
  end;
end;

destructor TRoleBasedAuthHandler.Destroy;
begin
  FRttiContext.Free;
  inherited;
end;

procedure TRoleBasedAuthHandler.OnAuthorization(const AContext: TWebContext; UserRoles: TList<string>;
  const ControllerQualifiedClassName: string; const ActionName: string; var IsAuthorized: Boolean);
var
  vRttiType: TRttiType;
  vAttributes: TArray<MVCRequiresRoleAttribute>;
  vRttiMethod: TRttiMethod;
begin
  // If there are no restrictions we will allow access to the ressource. (public API)
  IsAuthorized := True;

  // Check all Role requirements on the controller level
  vRttiType := FRttiContext.FindType(ControllerQualifiedClassName);
  if TryGetAttributes<MVCRequiresRoleAttribute>(vRttiType.GetAttributes, vAttributes) then
    if not CheckUserRoles(AContext, UserRoles, vAttributes) then
    begin
      IsAuthorized := False;
      Exit;
    end;

  // At this point the conttoller either has no restrictions or
  // we have successfully cleared these.
  // Verify all roles on the Action.
  vRttiMethod := vRttiType.GetMethod(ActionName);
  if TryGetAttributes<MVCRequiresRoleAttribute>(vRttiMethod.GetAttributes, vAttributes) then
    if not CheckUserRoles(AContext, UserRoles, vAttributes) then
    begin
      IsAuthorized := False;
      Exit;
    end;
end;

procedure TRoleBasedAuthHandler.OnRequest(const AContext: TWebContext; const ControllerQualifiedClassName: string;
  const ActionName: string; var AuthenticationRequired: Boolean);
var
  vRttiType: TRttiType;
  vAttributes: TArray<MVCRequiresAuthenticationAttribute>;
  vRttiMethod: TRttiMethod;
begin
  vRttiType := FRttiContext.FindType(ControllerQualifiedClassName);

  // Check class and Actions if they have role definitions.
  AuthenticationRequired := TryGetAttributes<MVCRequiresAuthenticationAttribute>(vRttiType.GetAttributes, vAttributes);
  if not AuthenticationRequired then
  begin
    vRttiMethod := vRttiType.GetMethod(ActionName);
    AuthenticationRequired := TryGetAttributes<MVCRequiresAuthenticationAttribute>(vRttiMethod.GetAttributes,
      vAttributes);
  end;
end;

function TRoleBasedAuthHandler.ResolveRole(const AContext: TWebContext; const aRole: string): string;
var
  vPlaceholders: TList<string>;
  vPlaceholder: string;
begin
  Result := aRole;
  // In case we don't have a context there is no need to eval for placeholders.
  if not Assigned(AContext) then
    Exit;

  vPlaceholders := CreateParameterNameList(aRole);
  try
    if (vPlaceholders.Count = 0) then
      Exit;

    for vPlaceholder in vPlaceholders do
      Result := Result.Replace(Format('($%s)', [vPlaceholder]), AContext.ParamsTable.Items[vPlaceholder],
        [rfReplaceAll]);
  finally
    vPlaceholders.Free;
  end;
end;

function TRoleBasedAuthHandler.TryGetAttributes<TTypeOfAttribute>(const aAttributes: TArray<TCustomAttribute>;
  out aListOfAttributes: TArray<TTypeOfAttribute>): Boolean;
var
  vAttribute: TCustomAttribute;
  vResultList: TList<TTypeOfAttribute>;
begin
  SetLength(aListOfAttributes, 0);

  if (Length(aAttributes) = 0) then
    Exit(False);

  vResultList := TList<TTypeOfAttribute>.Create;
  try
    for vAttribute in aAttributes do
      if (vAttribute.InheritsFrom(TTypeOfAttribute)) then
        vResultList.Add(vAttribute as TTypeOfAttribute);

    aListOfAttributes := vResultList.ToArray;
    Result := (Length(aListOfAttributes) > 0);
  finally
    vResultList.Free;
  end;
end;

{ TMVCRoleBasedAuthMiddleware }

constructor TMVCRoleBasedAuthMiddleware.Create(const AAuthenticationHandler: IMVCAuthenticationHandler;
  const ALoginUrl: string);
begin
  inherited Create(AAuthenticationHandler, ALoginUrl);
  Supports(AAuthenticationHandler, IMVCRoleBasedAuthenticationHandler, fAuthenticationHandler);
end;

procedure TMVCRoleBasedAuthMiddleware.DoRoleBasedBeforeControllerAction(const AContext: TWebContext;
  const aHandler: IMVCRoleBasedAuthenticationHandler; const AControllerQualifiedClassName: string;
  const AActionName: string; var AHandled: Boolean);
var
  IsValid: Boolean;
  IsAuthorized: Boolean;
  AuthRequired: Boolean;
begin
  // This procedure is a basic copy of the inherited OnBeforeControllerAction procedure.
  // Extention is by enabling the Authorization based on the context the call is being performed.
  aHandler.OnRequest(nil, AControllerQualifiedClassName, AActionName, AuthRequired);
  if not AuthRequired then
  begin
    AHandled := False;
    Exit;
  end;

  AContext.LoggedUser.LoadFromSession(AContext.Session);
  IsValid := AContext.LoggedUser.IsValid;
  if not IsValid then
  begin
    AContext.SessionStop(False);
    SendResponse(AContext, AHandled);
    Exit;
  end;

  IsAuthorized := False;

  // Modification here from:
  // FAuthenticationHandler.OnAuthorization(AContext.LoggedUser.Roles, AControllerQualifiedClassName, AActionName, IsAuthorized);
  // to:
  aHandler.OnAuthorization(AContext, AContext.LoggedUser.Roles, AControllerQualifiedClassName, AActionName,
    IsAuthorized);
  // Modification end

  if IsAuthorized then
  begin
    AHandled := False;
  end
  else
  begin
    if IsValid then
    begin
      SendResponse(AContext, AHandled, HTTP_STATUS.Forbidden)
    end
    else
    begin
      SendResponse(AContext, AHandled, HTTP_STATUS.Unauthorized);
    end;
  end;
end;

procedure TMVCRoleBasedAuthMiddleware.OnBeforeControllerAction(AContext: TWebContext;
  const AControllerQualifiedClassName, AActionName: string; var AHandled: Boolean);
begin
  if Assigned(fAuthenticationHandler) then
  begin
    DoRoleBasedBeforeControllerAction(AContext, fAuthenticationHandler, AControllerQualifiedClassName,
      AActionName, AHandled)
  end
  else
  begin
    inherited OnBeforeControllerAction(AContext, AControllerQualifiedClassName, AActionName, AHandled);
  end;
end;


end.
