// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2021 Daniele Teti and the DMVCFramework Team
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
// ***************************************************************************

unit MVCFramework.Utils;

interface

uses
  MVCFramework.Serializer.Commons, JsonDataObjects, MVCFramework.DuckTyping;

function NewJSONSerializer: IMVCJSONSerializer;
function StrToJSONObject(const aString: String; ARaiseExceptionOnError: Boolean = False): TJsonObject;
function StrToJSONArray(const aString: String; ARaiseExceptionOnError: Boolean = False): TJsonArray;
function WrapAsList(const AObject: TObject; AOwnsObject: Boolean = False): IMVCList;

implementation

uses
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.Commons,
  System.SysUtils;

function NewJSONSerializer: IMVCJSONSerializer;
begin
  Result := TMVCJsonDataObjectsSerializer.Create;
end;

function StrToJSONObject(const aString: String; ARaiseExceptionOnError: Boolean = False): TJsonObject;
begin
  Result := MVCFramework.Serializer.JSONDataObjects.StrToJSONObject(aString, ARaiseExceptionOnError);
end;

function StrToJSONArray(const aString: String; ARaiseExceptionOnError: Boolean = False): TJsonArray;
begin
  Result := MVCFramework.Serializer.JSONDataObjects.StrToJSONArray(aString, ARaiseExceptionOnError);
end;

function WrapAsList(const AObject: TObject; AOwnsObject: Boolean = False): IMVCList;
begin
  Result := MVCFramework.DuckTyping.WrapAsList(AObject, AOwnsObject);
end;

end.
