// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
// RoleBasedAuthHandler contributed by janidan
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

unit RoleBasedAuthHandler;

interface

uses
  MVCFramework.Commons,
  MVCFramework,
  System.Generics.Collections,
  System.Rtti;

type
  MVCRequiresRoleAttribute = class(MVCBaseAttribute)
  private
    FRole: string;
  public
    constructor Create(const aRole: string);
    property Role: string read FRole;
  end;

  TRoleBasedAuthHandler = class(TInterfacedObject, IMVCAuthenticationHandler)
  private
    FRttiContext: TRttiContext;

    function TryGetAttributes<TTypeOfAttribute: TCustomAttribute>
      (const aAttributes: TArray<TCustomAttribute>;
      out aListOfAttributes: TArray<TTypeOfAttribute>): Boolean;
    function CheckUserRoles(const aUserRoles: TList<string>;
      const aRoleAttributes: TArray<MVCRequiresRoleAttribute>): Boolean;
  public
    procedure OnRequest(const ControllerQualifiedClassName: string;
      const ActionName: string; var AuthenticationRequired: Boolean);

    procedure OnAuthentication(const UserName: string; const Password: string;
      UserRoles: TList<string>; var IsValid: Boolean;
      const SessionData: TDictionary<string, string>); virtual; abstract;

    procedure OnAuthorization(UserRoles: TList<string>;
      const ControllerQualifiedClassName: string; const ActionName: string;
      var IsAuthorized: Boolean);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ MVCRequiresRoleAttribute }

constructor MVCRequiresRoleAttribute.Create(const aRole: string);
begin
  inherited Create;
  FRole := aRole;
end;

{ TRoleBasedAuthHandler }

function TRoleBasedAuthHandler.CheckUserRoles(const aUserRoles
  : TList<System.string>;
  const aRoleAttributes: TArray<MVCRequiresRoleAttribute>): Boolean;
var
  vAttribute: MVCRequiresRoleAttribute;
begin
  // By default we will say that you are good to go.
  Result := True;
  if (Length(aRoleAttributes) = 0) then
    Exit;

  // All Attributes MUST match -> AND evaluation
  for vAttribute in aRoleAttributes do
    if not aUserRoles.Contains(vAttribute.Role) then
      Exit(False);
end;

constructor TRoleBasedAuthHandler.Create;
begin
  inherited Create;
  FRttiContext := TRttiContext.Create;
end;

destructor TRoleBasedAuthHandler.Destroy;
begin
  FRttiContext.Free;
  inherited;
end;

procedure TRoleBasedAuthHandler.OnAuthorization(UserRoles: TList<string>;
  const ControllerQualifiedClassName, ActionName: string;
  var IsAuthorized: Boolean);
var
  vRttiType: TRttiType;
  vAttributes: TArray<MVCRequiresRoleAttribute>;
  vRttiMethod: TRttiMethod;
begin
  // If there are no restrictions we will allow access to the ressource. (public API)
  IsAuthorized := True;

  // Check all Role requirements on the controller level
  vRttiType := FRttiContext.FindType(ControllerQualifiedClassName);
  if TryGetAttributes<MVCRequiresRoleAttribute>(vRttiType.GetAttributes,
    vAttributes) then
    if not CheckUserRoles(UserRoles, vAttributes) then
    begin
      IsAuthorized := False;
      Exit;
    end;

  // At this point the conttoller either has no restrictions or
  // we have successfully cleared these.
  // Verify all roles on the Action.
  vRttiMethod := vRttiType.GetMethod(ActionName);
  if TryGetAttributes<MVCRequiresRoleAttribute>(vRttiMethod.GetAttributes,
    vAttributes) then
    if not CheckUserRoles(UserRoles, vAttributes) then
    begin
      IsAuthorized := False;
      Exit;
    end;
end;

procedure TRoleBasedAuthHandler.OnRequest(const ControllerQualifiedClassName,
  ActionName: string; var AuthenticationRequired: Boolean);
var
  vRttiType: TRttiType;
  vAttributes: TArray<MVCRequiresRoleAttribute>;
  vRttiMethod: TRttiMethod;
begin
  vRttiType := FRttiContext.FindType(ControllerQualifiedClassName);

  // Check class and Actions if they have role definitions.
  AuthenticationRequired := TryGetAttributes<MVCRequiresRoleAttribute>
    (vRttiType.GetAttributes, vAttributes);
  if not AuthenticationRequired then
  begin
    vRttiMethod := vRttiType.GetMethod(ActionName);
    AuthenticationRequired := TryGetAttributes<MVCRequiresRoleAttribute>
      (vRttiMethod.GetAttributes, vAttributes);
  end;
end;

function TRoleBasedAuthHandler.TryGetAttributes<TTypeOfAttribute>
  (const aAttributes: TArray<TCustomAttribute>;
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

end.
