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

unit Swag.Doc.Extensions;

interface

uses
  System.SysUtils,
  System.JSON;

type
  ESwagErrorExtensionName = class(Exception);

  /// <summary>
  /// Holds the Specification Extensions of an object. The name of every extension MUST begin with "x-" and the
  /// value can be any JSON value, for example x-logo or x-internal. The extensions are written in Swagger 2.0 and
  /// OpenAPI 3 documents.
  /// </summary>
  TSwagExtensions = class(TObject)
  strict private
    fItems: TJSONObject;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    /// <summary>
    /// Adds or replaces an extension. The extensions take ownership of the value.
    /// An ESwagErrorExtensionName exception is raised when the name does not begin with "x-".
    /// </summary>
    procedure Add(const pName: string; pValue: TJSONValue); overload;

    /// <summary>
    /// Adds or replaces an extension with a string value.
    /// </summary>
    procedure Add(const pName, pValue: string); overload;

    /// <summary>
    /// Removes the extension with the given name, if it exists.
    /// </summary>
    procedure Remove(const pName: string);

    /// <summary>
    /// Removes every extension.
    /// </summary>
    procedure Clear;

    /// <summary>
    /// Returns the number of extensions.
    /// </summary>
    function Count: Integer;

    /// <summary>
    /// Writes a copy of the extensions in the JSON object. Names that already exist in the object are not overwritten.
    /// </summary>
    procedure WriteTo(pJson: TJSONObject);

    /// <summary>
    /// Replaces the current extensions by the fields of the JSON object whose names begin with "x-".
    /// </summary>
    procedure ReadFrom(pJson: TJSONObject);

    /// <summary>
    /// Returns True when the name is a valid extension name, that is, when it begins with "x-".
    /// </summary>
    class function IsExtensionName(const pName: string): Boolean;

    /// <summary>
    /// The extensions as a JSON object, where each pair is an extension.
    /// </summary>
    property Items: TJSONObject read fItems;
  end;

implementation

uses
  System.Generics.Collections;

const
  c_SwagExtensionPrefix = 'x-';

{ TSwagExtensions }

constructor TSwagExtensions.Create;
begin
  inherited Create;
  fItems := TJSONObject.Create;
end;

destructor TSwagExtensions.Destroy;
begin
  FreeAndNil(fItems);
  inherited Destroy;
end;

procedure TSwagExtensions.Add(const pName: string; pValue: TJSONValue);
begin
  if not IsExtensionName(pName) then
  begin
    pValue.Free;
    raise ESwagErrorExtensionName.CreateFmt('The extension name "%s" must begin with "%s".', [pName, c_SwagExtensionPrefix]);
  end;

  fItems.RemovePair(pName).Free;
  fItems.AddPair(pName, pValue);
end;

procedure TSwagExtensions.Add(const pName, pValue: string);
begin
  Add(pName, TJSONString.Create(pValue));
end;

procedure TSwagExtensions.Remove(const pName: string);
begin
  fItems.RemovePair(pName).Free;
end;

procedure TSwagExtensions.Clear;
begin
  while fItems.Count > 0 do
    fItems.RemovePair(fItems.Pairs[0].JsonString.Value).Free;
end;

function TSwagExtensions.Count: Integer;
begin
  Result := fItems.Count;
end;

procedure TSwagExtensions.WriteTo(pJson: TJSONObject);
var
  vIndex: Integer;
  vName: string;
begin
  if not Assigned(pJson) then
    Exit;

  for vIndex := 0 to fItems.Count - 1 do
  begin
    vName := fItems.Pairs[vIndex].JsonString.Value;
    if not Assigned(pJson.Values[vName]) then
      pJson.AddPair(vName, fItems.Pairs[vIndex].JsonValue.Clone as TJSONValue);
  end;
end;

procedure TSwagExtensions.ReadFrom(pJson: TJSONObject);
var
  vIndex: Integer;
  vName: string;
begin
  Clear;
  if not Assigned(pJson) then
    Exit;

  for vIndex := 0 to pJson.Count - 1 do
  begin
    vName := pJson.Pairs[vIndex].JsonString.Value;
    if IsExtensionName(vName) then
      fItems.AddPair(vName, pJson.Pairs[vIndex].JsonValue.Clone as TJSONValue);
  end;
end;

class function TSwagExtensions.IsExtensionName(const pName: string): Boolean;
begin
  Result := pName.StartsWith(c_SwagExtensionPrefix) and (pName.Length > c_SwagExtensionPrefix.Length);
end;

end.
