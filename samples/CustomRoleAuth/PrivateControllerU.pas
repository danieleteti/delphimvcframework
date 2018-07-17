// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Contributer on this file: Janidan - https://github.com/janidan
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

unit PrivateControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Middleware.Authentication.RoleBasedAuthHandler;

type

  [MVCPath('/private')]
  TPrivateController = class(TMVCController)
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

    [MVCPath('/public/action')]
    [MVCHTTPMethod([httpGET])]
    procedure PublicAction;

    [MVCPath('/authenticatedOnly')]
    [MVCHTTPMethod([httpGET])]
    [MVCRequiresAuthentication]
    procedure AuthenticatedAction;

    [MVCPath('/role1')]
    [MVCHTTPMethod([httpGET])]
    [MVCRequiresRole('role1')]
    procedure OnlyRole1;

    [MVCPath('/role2')]
    [MVCHTTPMethod([httpGET])]
    [MVCRequiresRole('role2')]
    procedure OnlyRole2;

    [MVCPath('/role1and2')]
    [MVCHTTPMethod([httpGET])]
    [MVCRequiresRole('role1')]
    [MVCRequiresRole('role2')]
    // The following is the same as above (redundancy for the example)
    [MVCRequiresRole('role1;role2', MVCRoleEval.reAND)]
    procedure OnlyRole1And2;

    [MVCPath('/role1or2')]
    [MVCHTTPMethod([httpGET])]
    [MVCRequiresRole('role1;role2', MVCRoleEval.reOR)]
    procedure OnlyRole1or2;

    [MVCPath('/role/($role)')]
    [MVCHTTPMethod([httpGET])]
    [MVCRequiresRole('($role)')]
    procedure AccessThisByRole(const role: string);
  end;

implementation

procedure TPrivateController.AccessThisByRole(const role: string);
begin
  Render('OK This resource was accessed by role: ' + role);
end;

procedure TPrivateController.AuthenticatedAction;
begin
  Render('OK from a Authenticated Action');
end;

procedure TPrivateController.Index;
begin
  // use Context property to access to the HTTP request and response
  Render('Hello World');
end;

procedure TPrivateController.OnlyRole1;
begin
  Render('OK from a "role1" action');
end;

procedure TPrivateController.OnlyRole1And2;
begin
  Render('OK from a "role1 and role2" action');
end;

procedure TPrivateController.OnlyRole1or2;
begin
  Render('OK from a "role1 or role2" action');
end;

procedure TPrivateController.OnlyRole2;
begin
  Render('OK from a "role2" action');
end;

procedure TPrivateController.PublicAction;
begin
  Render('OK from a public action (no login required)');
end;

end.
