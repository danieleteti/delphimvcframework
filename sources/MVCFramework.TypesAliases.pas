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

unit MVCFramework.TypesAliases;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils
{$IFDEF SYSTEMJSON}
    , System.JSON
{$ELSE}
    , Data.DBXJSON
{$IFEND}
    ;

type

{$IFDEF SYSTEMJSON}
  TJSONObject = System.JSON.TJSONObject;
  TJSONValue = System.JSON.TJSONValue;
  TJSONPair = System.JSON.TJSONPair;
  TJSONArray = System.JSON.TJSONArray;
  TJSONNumber = System.JSON.TJSONNumber;
  TJSONNull = System.JSON.TJSONNull;
  TJSONString = System.JSON.TJSONString;
  TJSONTrue = System.JSON.TJSONTrue;
  TJSONFalse = System.JSON.TJSONFalse;
{$ELSE}
  TJSONObject = Data.DBXJSON.TJSONObject;
  TJSONValue = Data.DBXJSON.TJSONValue;
  TJSONPair = Data.DBXJSON.TJSONPair;
  TJSONArray = Data.DBXJSON.TJSONArray;
  TJSONNumber = Data.DBXJSON.TJSONNumber;
  TJSONNull = Data.DBXJSON.TJSONNull;
  TJSONString = Data.DBXJSON.TJSONString;
  TJSONTrue = Data.DBXJSON.TJSONTrue;
  TJSONFalse = Data.DBXJSON.TJSONFalse;
{$ENDIF}

implementation

end.
