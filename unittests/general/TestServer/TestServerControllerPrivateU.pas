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

unit TestServerControllerPrivateU;

interface

uses
  MVCFramework;

type

  [MVCPath('/privatecustom')]
  TTestPrivateServerControllerCustomAuth = class(TMVCController)
  public
    [MVCPath('/role1')]
    procedure OnlyRole1;
    [MVCPath('/role2')]
    procedure OnlyRole2;
  end;

implementation


{ TTestPrivateServerControllerCustomAuth }

procedure TTestPrivateServerControllerCustomAuth.OnlyRole1;
begin
  Render('Here''s Action1 from the private controller');
end;

procedure TTestPrivateServerControllerCustomAuth.OnlyRole2;
begin
  Render('Here''s Action2 from the private controller');
end;

end.
