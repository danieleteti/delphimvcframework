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

unit Swag.Doc.Path.Operation.ResponseHeaders;

interface

uses
  System.SysUtils,
  System.Json;

type
  /// <summary>
  /// Lists the headers that can be sent as part of a response.
  /// </summary>
  TSwagHeaders = class(TObject)
  private
    fName: string;
    fDescription: string;
    fType: string;
  public
    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson : TJSONObject);

    /// <summary>
    /// A header name alias.
    /// </summary>
    property Name: string read fName write fName;

    /// <summary>
    /// A short description of the header.
    /// </summary>
    property Description: string read fDescription write fDescription;

    /// <summary>
    /// Required. The type of the object. The value MUST be one of "string", "number", "integer", "boolean", or "array".
    /// </summary>
    property ValueType: string read fType write fType;
  end;

implementation

{ TSwagHeaders }

function TSwagHeaders.GenerateJsonObject: TJSONObject;
var
  vJsonObject: TJsonObject;
begin
  vJsonObject := TJSONObject.Create;
  if fDescription.Length > 0 then
    vJsonObject.AddPair('description', fDescription);
  if fType.Length > 0 then
    vJsonObject.AddPair('type', fType);
  Result := vJsonObject;
end;

procedure TSwagHeaders.Load(pJson: TJSONObject);
begin
  if Assigned(pJson.Values['description']) then
    fDescription := pJson.Values['description'].Value;
  if Assigned(pJson.Values['type']) then
    fType := pJson.Values['type'].Value;
end;

end.
