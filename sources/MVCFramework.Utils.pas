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
// ***************************************************************************

unit MVCFramework.Utils;

{$I dmvcframework.inc}

interface

uses
  MVCFramework.Serializer.Commons, JsonDataObjects, MVCFramework.DuckTyping,
  System.Classes;


function NewJSONSerializer: IMVCJSONSerializer;
function StrToJSONObject(const aString: String; ARaiseExceptionOnError: Boolean = False): TJsonObject;
function StrToJSONArray(const aString: String; ARaiseExceptionOnError: Boolean = False): TJsonArray;
function WrapAsList(const AObject: TObject; AOwnsObject: Boolean = False): IMVCList;
function GetMD5HashFromStream(AStream: TStream): string;
function GetMD5HashFromString(const AString: String): string;
function GetSHA1HashFromString(const AString: String): string;
function GetSHA1HashFromStream(AStream: TStream): string;

implementation

uses
{$IF defined(TOKYOORBETTER)}
  System.Hash,
{$ELSE}
  IdHashMessageDigest,
  IdHashSHA,
{$ENDIF}
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.Commons,
  System.SysUtils,
  System.TypInfo;

function GetMD5HashFromStream(AStream: TStream): string;
{$IF not defined(TOKYOORBETTER)}
var
  lMD5Hash: TIdHashMessageDigest5;
{$ENDIF}
begin
{$IF defined(TOKYOORBETTER)}
  Result := THashMD5.GetHashString(AStream);
{$ELSE}
  lMD5Hash := TIdHashMessageDigest5.Create;
  try
    Result := lMD5Hash.HashStreamAsHex(AStream);
  finally
    lMD5Hash.Free;
  end;
{$ENDIF}
end;

function GetSHA1HashFromStream(AStream: TStream): string;
{$IF not defined(TOKYOORBETTER)}
var
  lSHA1Hash: TIdHashSHA1;
{$ENDIF}
begin
{$IF defined(TOKYOORBETTER)}
  Result := THashSHA1.GetHashString(AStream);
{$ELSE}
  lSHA1Hash := TIdHashSHA1.Create;
  try
    Result := lSHA1Hash.HashStreamAsHex(AStream);
  finally
    lSHA1Hash.Free;
  end;
{$ENDIF}
end;

function GetMD5HashFromString(const AString: String): string;
{$IF not defined(TOKYOORBETTER)}
var
  lMD5Hash: TIdHashMessageDigest5;
{$ENDIF}
begin
{$IF defined(TOKYOORBETTER)}
  Result := THashMD5.GetHashString(AString);
{$ELSE}
  lMD5Hash := TIdHashMessageDigest5.Create;
  try
    Result := lMD5Hash.HashStringAsHex(AString);
  finally
    lMD5Hash.Free;
  end;
{$ENDIF}
end;

function GetSHA1HashFromString(const AString: String): string;
{$IF not defined(TOKYOORBETTER)}
var
  lSHA1Hash: TIdHashSHA1;
{$ENDIF}
begin
{$IF defined(TOKYOORBETTER)}
  Result := THashSHA1.GetHashString(AString);
{$ELSE}
  lSHA1Hash := TIdHashSHA1.Create;
  try
    Result := lSHA1Hash.HashStringAsHex(AString);
  finally
    lSHA1Hash.Free;
  end;
{$ENDIF}
end;


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
