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

unit AuthHandlerU;

interface

uses
  MVCFramework.Commons, MVCFramework, System.Generics.Collections;

type
  TCustomAuth = class(TInterfacedObject, IMVCAuthenticationHandler)
  public
    // called at each request to know if the request requires an authentication
    procedure OnRequest(const AContext: TWebContext; const ControllerQualifiedClassName: string;
      const ActionName: string; var AuthenticationRequired: Boolean);

    // if authentication is required, this method must execute the user authentication
    procedure OnAuthentication(const AContext: TWebContext; const UserName: string; const Password: string;
      UserRoles: TList<System.string>;
      var IsValid: Boolean;
      const SessionData: System.Generics.Collections.TDictionary<System.string, System.string>);

    // if authenticated, this method defines the user roles
    procedure OnAuthorization(const AContext: TWebContext; UserRoles: System.Generics.Collections.TList<System.string>;
      const ControllerQualifiedClassName: string; const ActionName: string;
      var IsAuthorized: Boolean);

  end;

implementation

uses
  System.SysUtils;

{ TCustomAuth }

procedure TCustomAuth.OnAuthentication(const AContext: TWebContext; const UserName: string; const Password: string;
      UserRoles: TList<System.string>;
      var IsValid: Boolean;
      const SessionData: System.Generics.Collections.TDictionary<System.string, System.string>);
begin
  {
    Here you should do the actual query on database or other "users store" to
    check if the user identified by UserName and Password is a valid user.
    You have to fill also the UserRoles list with the roles of the user.
    Moreover additional user properties can be added in the SessionData dictionary
  }

  // We defined 3 statc users here: admin, user1, user2
  IsValid := False;
  if (UserName = 'admin') and (Password = 'adminpass') then
  begin
    IsValid := True;
    UserRoles.Add('admin');
  end
  else if (UserName = 'user1') and (Password = 'user1pass') then
  begin
    IsValid := True;
    UserRoles.Add('role1');
  end
  else if (UserName = 'user2') and (Password = 'user2pass') then
  begin
    IsValid := True;
    UserRoles.Add('role2');
  end;

  // if you dont have "roles" concept in your system, you can also avoid to use them and
  // sets only IsValid := True;
end;

procedure TCustomAuth.OnAuthorization(const AContext: TWebContext; UserRoles: System.Generics.Collections.TList<System.string>;
      const ControllerQualifiedClassName: string; const ActionName: string;
      var IsAuthorized: Boolean);
begin
  if ControllerQualifiedClassName = 'PrivateControllerU.TPrivateController' then
  begin
    IsAuthorized := UserRoles.Contains('admin');
    if not IsAuthorized then
      IsAuthorized := (UserRoles.Contains('role1')) and (ActionName = 'OnlyRole1');
    if not IsAuthorized then
      IsAuthorized := (UserRoles.Contains('role2')) and (ActionName = 'OnlyRole2');
  end
  else
  begin
    // You can always navigate in the public section of API
    IsAuthorized := True;
  end;

end;

procedure TCustomAuth.OnRequest(const AContext: TWebContext; const ControllerQualifiedClassName: string;
      const ActionName: string; var AuthenticationRequired: Boolean);
begin
  {
    This is the the auth schema implemented: All the actions present in the
    'PrivateControllerU.TPrivateController' requires authentication but
    the action 'PublicAction'. In other words, 'PublicAction' can be called also
    without authentication.
  }

  AuthenticationRequired :=
    ControllerQualifiedClassName = 'PrivateControllerU.TPrivateController';
  if AuthenticationRequired then
  begin
    if ActionName = 'PublicAction' then
    begin
      AuthenticationRequired := False;
    end;
  end;

end;

end.
