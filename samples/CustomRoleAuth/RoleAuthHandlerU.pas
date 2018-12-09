// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// Contributer on this file: Janidan - https://github.com/janidan
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

unit RoleAuthHandlerU;

interface

uses
  MVCFramework.Commons,
  MVCFramework,
  System.Generics.Collections,
  MVCFramework.Middleware.Authentication.RoleBasedAuthHandler;

type
  TCustomRoleAuth = class(TRoleBasedAuthHandler)
  public
    // if authentication is required, this method must execute the user authentication
    procedure OnAuthentication(const AContext: TWebContext;
      const UserName: string;
      const Password: string;
      UserRoles: TList<System.string>;
      var IsValid: Boolean;
      const SessionData: TDictionary<string,string>); override;
  end;

implementation

{ TCustomRoleAuth }

procedure TCustomRoleAuth.OnAuthentication(const AContext: TWebContext; const UserName: string; const Password: string;
      UserRoles: TList<System.string>; var IsValid: Boolean;
      const SessionData: System.Generics.Collections.TDictionary<System.string,
      System.string>);
begin
  {
    Here you should do the actual query on database or other "users store" to
    check if the user identified by UserName and Password is a valid user.
    You have to fill also the UserRoles list with the roles of the user.
    Moreover additional user properties can be added in the SessionData dictionary
  }

  // We defined 3 static users here: admin, user1, user2
  IsValid := False;
  if (UserName = 'admin') and (Password = 'adminpass') then
  begin
    IsValid := True;
    UserRoles.Add('admin');
    UserRoles.Add('role1');
    UserRoles.Add('role2');
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
  end
  else if (UserName = 'user1_2') and (Password = 'user1_2pass') then
  begin
    IsValid := True;
    UserRoles.Add('role1');
    UserRoles.Add('role2');
  end
  else if (UserName = 'user3') and (Password = 'user3pass') then
  begin
    IsValid := True;
    UserRoles.Add('role3');
  end;

  // if you dont have "roles" concept in your system, you can also avoid to use them and
  // sets only IsValid := True;
end;

end.
