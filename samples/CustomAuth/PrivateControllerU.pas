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

unit PrivateControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

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

    [MVCPath('/role1')]
    [MVCHTTPMethod([httpGET])]
    procedure OnlyRole1;

    [MVCPath('/role2')]
    [MVCHTTPMethod([httpGET])]
    procedure OnlyRole2;
  end;

implementation

procedure TPrivateController.Index;
begin
  // use Context property to access to the HTTP request and response
  Render('Hello World');

end;

procedure TPrivateController.OnlyRole1;
begin
  Render('OK from a "role1" action');
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
