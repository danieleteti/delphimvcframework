// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators with this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
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

unit CustomTypesU;

interface

type
  // custom serialization is by-type so we define a type alias
  // useful to identify all the fields that must be serialized
  // using the custom serializer defined for this type
  TUserRoles = TArray<string>;

  // This is the main object which uses the
  // custom serialized type as property Roles
  TSysUser = class
  private
    FUserName: string;
    FRoles: TUserRoles;
    procedure SetUserName(const Value: string);
    function GetUserRoles: TUserRoles;
  public
    constructor Create(aUserName: string; aRoles: TUserRoles);
    property UserName: string read FUserName write SetUserName;
    // Here we are using the custom-serialized type TUserRoles
    property Roles: TUserRoles read GetUserRoles;
  end;

implementation


{ TSysUser }

constructor TSysUser.Create(aUserName: string; aRoles: TUserRoles);
begin
  inherited Create;
  FUserName := aUserName;
  FRoles := aRoles;
end;

function TSysUser.GetUserRoles: TUserRoles;
begin
  Result := FRoles;
end;

procedure TSysUser.SetUserName(const Value: string);
begin
  FUserName := Value;
end;

end.
