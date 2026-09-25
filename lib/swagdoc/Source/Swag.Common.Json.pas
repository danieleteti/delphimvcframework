{******************************************************************************}
{                                                                              }
{  Delphi SwagDoc Library                                                      }
{  Copyright (c) 2018 Marcelo Jaloto                                           }
{  https://github.com/marcelojaloto/SwagDoc                                    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}

unit Swag.Common.Json;

interface

uses
  System.JSON;

type
  /// <summary>
  /// Reads the fields of a JSON object without raising exceptions when a field is absent or has an unexpected type.
  /// </summary>
  TSwagJson = class abstract(TObject)
  public
    /// <summary>
    /// Returns the value as a JSON object, or nil when the value is not an object.
    /// </summary>
    class function AsObject(pJson: TJSONValue): TJSONObject; static;

    /// <summary>
    /// Returns the value as a JSON array, or nil when the value is not an array.
    /// </summary>
    class function AsArray(pJson: TJSONValue): TJSONArray; static;

    /// <summary>
    /// Returns the object of the field, or nil when the field is absent or is not an object.
    /// </summary>
    class function ReadObject(pJson: TJSONObject; const pName: string): TJSONObject; static;

    /// <summary>
    /// Returns the array of the field, or nil when the field is absent or is not an array.
    /// </summary>
    class function ReadArray(pJson: TJSONObject; const pName: string): TJSONArray; static;

    /// <summary>
    /// Returns the text of the field, or an empty string when the field is absent, null, an object or an array.
    /// </summary>
    class function ReadString(pJson: TJSONObject; const pName: string): string; static;

    /// <summary>
    /// Returns True only when the field is the boolean value true.
    /// </summary>
    class function ReadBoolean(pJson: TJSONObject; const pName: string): Boolean; static;

    /// <summary>
    /// Returns a copy of the value of the field, or nil when the field is absent. The caller owns the copy.
    /// </summary>
    class function CloneValue(pJson: TJSONObject; const pName: string): TJSONValue; static;
  end;

implementation

{ TSwagJson }

class function TSwagJson.AsObject(pJson: TJSONValue): TJSONObject;
begin
  if pJson is TJSONObject then
    Result := TJSONObject(pJson)
  else
    Result := nil;
end;

class function TSwagJson.AsArray(pJson: TJSONValue): TJSONArray;
begin
  if pJson is TJSONArray then
    Result := TJSONArray(pJson)
  else
    Result := nil;
end;

class function TSwagJson.ReadObject(pJson: TJSONObject; const pName: string): TJSONObject;
begin
  Result := nil;
  if Assigned(pJson) then
    Result := AsObject(pJson.Values[pName]);
end;

class function TSwagJson.ReadArray(pJson: TJSONObject; const pName: string): TJSONArray;
begin
  Result := nil;
  if Assigned(pJson) then
    Result := AsArray(pJson.Values[pName]);
end;

class function TSwagJson.ReadString(pJson: TJSONObject; const pName: string): string;
var
  vValue: TJSONValue;
begin
  Result := '';
  if not Assigned(pJson) then
    Exit;

  vValue := pJson.Values[pName];
  if Assigned(vValue) and not ((vValue is TJSONNull) or (vValue is TJSONObject) or (vValue is TJSONArray)) then
    Result := vValue.Value;
end;

class function TSwagJson.ReadBoolean(pJson: TJSONObject; const pName: string): Boolean;
begin
  Result := Assigned(pJson) and (pJson.Values[pName] is TJSONBool) and TJSONBool(pJson.Values[pName]).AsBoolean;
end;

class function TSwagJson.CloneValue(pJson: TJSONObject; const pName: string): TJSONValue;
begin
  Result := nil;
  if Assigned(pJson) and Assigned(pJson.Values[pName]) then
    Result := pJson.Values[pName].Clone as TJSONValue;
end;

end.
