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

unit MVCFramework.Patches;

{$I dmvcframework.inc}

interface

uses
  System.Classes
{$IFDEF SYSTEMJSON}
    , System.JSON
{$ENDIF}
{$IFNDEF SYSTEMJSON} // XE6
    , Data.DBXJSON
{$ENDIF}
    ;

{$IFNDEF TOJSON}


type
  TJSONValueHelper = class helper for TJSONValue
  public
    function GetItem(const Index: Integer): TJSONValue;
    function ToJSON: String;
    function Count: Integer;
    property Items[const Index: Integer]: TJSONValue read GetItem;
  end;
{$ENDIF}

implementation

{ TJSONValueHelper }

{$IFNDEF TOJSON}


function TJSONValueHelper.Count: Integer;
begin
  Result := Size;
end;

function TJSONValueHelper.GetItem(const Index: Integer): TJSONValue;
begin
  Result := Get(Index);
end;

function TJSONValueHelper.ToJSON: String;
begin
  Result := Self.ToString;
end;
{$ENDIF}

end.
