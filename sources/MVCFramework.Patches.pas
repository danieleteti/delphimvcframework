// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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

interface

{$I dmvcframework.inc}


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
    function ToJSON: String;
  end;
{$ENDIF}

implementation

{ TJSONValueHelper }

{$IFNDEF TOJSON}


function TJSONValueHelper.ToJSON: String;
begin
  Result := Self.ToString;
end;
{$ENDIF}

end.
