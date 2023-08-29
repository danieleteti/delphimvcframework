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

unit AuthHandlersU;

interface

uses
  MVCFramework.Commons, System.Generics.Collections, MVCFramework;

type
  TAuthHandlerBase = class abstract(TInterfacedObject, IMVCAuthenticationHandler)
  public
    procedure OnRequest(const AContext: TWebContext; const ControllerQualifiedClassName: string;
      const ActionName: string; var AuthenticationRequired: Boolean); virtual; abstract;
    procedure OnAuthentication(const AContext: TWebContext; const UserName: string; const Password: string;
      UserRoles: System.Generics.Collections.TList<System.string>;
      var IsValid: Boolean; const SessionData: TDictionary<string, string>); virtual;
    procedure OnAuthorization(const AContext: TWebContext;
      UserRoles: System.Generics.Collections.TList<System.string>;
      const ControllerQualifiedClassName: string; const ActionName: string;
      var IsAuthorized: Boolean); virtual;
  end;

  TBasicAuthHandler = class(TAuthHandlerBase)
  public
    procedure OnRequest(const AContext: TWebContext; const ControllerQualifiedClassName: string;
      const ActionName: string; var AuthenticationRequired: Boolean); override;
  end;

  TCustomAuthHandler = class(TAuthHandlerBase)
  public
    procedure OnRequest(const AContext: TWebContext; const ControllerQualifiedClassName: string;
      const ActionName: string; var AuthenticationRequired: Boolean); override;
  end;

implementation

uses
  System.SysUtils;

procedure TAuthHandlerBase.OnAuthentication(
  const AContext: TWebContext;
  const UserName: string; const Password: string;
  UserRoles: System.Generics.Collections.TList<System.string>; var IsValid: Boolean;
  const SessionData: TDictionary<string, string>);
begin
  UserRoles.Clear;
  IsValid := UserName = Password;
  if not IsValid then
    Exit;
  if UserName = 'user1' then
  begin
    IsValid := True;
    UserRoles.Add('role1');
  end;
  if UserName = 'user2' then
  begin
    IsValid := True;
    UserRoles.Add('role2');
  end;
end;

procedure TAuthHandlerBase.OnAuthorization(
  const AContext: TWebContext;
  UserRoles: System.Generics.Collections.TList<System.string>;
  const ControllerQualifiedClassName, ActionName: string;
  var IsAuthorized: Boolean);
begin
  IsAuthorized := False;
  if (ActionName = 'OnlyRole1') or (ActionName = 'OnlyRole1Session') then
    IsAuthorized := UserRoles.Contains('role1');
  if ActionName = 'OnlyRole2' then
    IsAuthorized := UserRoles.Contains('role2');
end;

procedure TBasicAuthHandler.OnRequest(const AContext: TWebContext; const ControllerQualifiedClassName: string;
      const ActionName: string; var AuthenticationRequired: Boolean);
begin
  AuthenticationRequired := ControllerQualifiedClassName.EndsWith
    ('TTestPrivateServerController');
end;

procedure TCustomAuthHandler.OnRequest(const AContext: TWebContext; const ControllerQualifiedClassName: string;
      const ActionName: string; var AuthenticationRequired: Boolean);
begin
  AuthenticationRequired := ControllerQualifiedClassName.EndsWith
    ('TTestPrivateServerControllerCustomAuth');
end;

end.
