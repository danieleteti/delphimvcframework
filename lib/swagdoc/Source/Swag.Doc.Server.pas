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

unit Swag.Doc.Server;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.JSON,
  Swag.Doc.Extensions;

type
  /// <summary>
  /// An object representing a Server Variable for server URL template substitution.
  /// </summary>
  TSwagServerVariable = class(TObject)
  private
    fName: string;
    fDefault: string;
    fDescription: string;
    fEnum: TStringList;
    fExtensions: TSwagExtensions;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson: TJSONObject);

    /// <summary>
    /// The name of the variable, as it appears between braces in the URL of the server.
    /// </summary>
    property Name: string read fName write fName;

    /// <summary>
    /// Required. The default value to use for substitution, which SHALL be sent if an alternate value is not supplied.
    /// If the enum is defined, the value MUST exist in the enum's values.
    /// </summary>
    property Default: string read fDefault write fDefault;

    /// <summary>
    /// An optional description for the server variable. CommonMark syntax MAY be used for rich text representation.
    /// </summary>
    property Description: string read fDescription write fDescription;

    /// <summary>
    /// An enumeration of string values to be used if the substitution options are from a limited set.
    /// </summary>
    property Enum: TStringList read fEnum;

    /// <summary>
    /// The Specification Extensions of the server variable.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

  /// <summary>
  /// An object representing a Server. In OpenAPI 3 the servers replace the host, basePath and schemes
  /// fields of the Swagger 2.0 specification.
  /// </summary>
  TSwagServer = class(TObject)
  private
    fUrl: string;
    fDescription: string;
    fName: string;
    fVariables: TObjectList<TSwagServerVariable>;
    fExtensions: TSwagExtensions;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson: TJSONObject);

    /// <summary>
    /// Adds a variable used by the URL template and returns it, so the enumeration can be filled.
    /// </summary>
    function AddVariable(const pName, pDefault: string; const pDescription: string = ''): TSwagServerVariable;

    /// <summary>
    /// Returns True when the URL of the server is not defined.
    /// </summary>
    function IsEmpty: Boolean;

    /// <summary>
    /// Required. A URL to the target host. This URL supports Server Variables and MAY be relative, to indicate that
    /// the host location is relative to the location where the OpenAPI document is being served.
    /// Variable substitutions will be made when a variable is named in {brackets}.
    /// </summary>
    property Url: string read fUrl write fUrl;

    /// <summary>
    /// An optional string describing the host designated by the URL. CommonMark syntax MAY be used for rich text representation.
    /// </summary>
    property Description: string read fDescription write fDescription;

    /// <summary>
    /// An optional unique string to refer to the host designated by the URL.
    /// </summary>
    property Name: string read fName write fName;

    /// <summary>
    /// A map between a variable name and its value. The value is used for substitution in the server's URL template.
    /// </summary>
    property Variables: TObjectList<TSwagServerVariable> read fVariables;

    /// <summary>
    /// The Specification Extensions of the server.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

implementation

uses
  Swag.Common.Json;

const
  c_SwagServerUrl = 'url';
  c_SwagServerDescription = 'description';
  c_SwagServerName = 'name';
  c_SwagServerVariables = 'variables';
  c_SwagServerVariableEnum = 'enum';
  c_SwagServerVariableDefault = 'default';
  c_SwagServerVariableDescription = 'description';

{ TSwagServerVariable }

constructor TSwagServerVariable.Create;
begin
  inherited Create;
  fEnum := TStringList.Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagServerVariable.Destroy;
begin
  FreeAndNil(fEnum);
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

function TSwagServerVariable.GenerateJsonObject: TJSONObject;
var
  vJsonEnum: TJSONArray;
  vIndex: Integer;
begin
  Result := TJSONObject.Create;
  if fEnum.Count > 0 then
  begin
    vJsonEnum := TJSONArray.Create;
    for vIndex := 0 to fEnum.Count - 1 do
      vJsonEnum.Add(fEnum[vIndex]);
    Result.AddPair(c_SwagServerVariableEnum, vJsonEnum);
  end;
  Result.AddPair(c_SwagServerVariableDefault, fDefault);
  if not fDescription.IsEmpty then
    Result.AddPair(c_SwagServerVariableDescription, fDescription);
  fExtensions.WriteTo(Result);
end;

procedure TSwagServerVariable.Load(pJson: TJSONObject);
var
  vJsonEnum: TJSONArray;
  vIndex: Integer;
begin
  if not Assigned(pJson) then
    Exit;

  vJsonEnum := TSwagJson.ReadArray(pJson, c_SwagServerVariableEnum);
  if Assigned(vJsonEnum) then
    for vIndex := 0 to vJsonEnum.Count - 1 do
      fEnum.Add(vJsonEnum.Items[vIndex].Value);
  fDefault := TSwagJson.ReadString(pJson, c_SwagServerVariableDefault);
  fDescription := TSwagJson.ReadString(pJson, c_SwagServerVariableDescription);
  fExtensions.ReadFrom(pJson);
end;

{ TSwagServer }

constructor TSwagServer.Create;
begin
  inherited Create;
  fVariables := TObjectList<TSwagServerVariable>.Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagServer.Destroy;
begin
  FreeAndNil(fVariables);
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

function TSwagServer.AddVariable(const pName, pDefault: string; const pDescription: string): TSwagServerVariable;
begin
  Result := TSwagServerVariable.Create;
  Result.Name := pName;
  Result.Default := pDefault;
  Result.Description := pDescription;
  fVariables.Add(Result);
end;

function TSwagServer.IsEmpty: Boolean;
begin
  Result := fUrl.IsEmpty;
end;

function TSwagServer.GenerateJsonObject: TJSONObject;
var
  vJsonVariables: TJSONObject;
  vIndex: Integer;
begin
  Result := TJSONObject.Create;
  Result.AddPair(c_SwagServerUrl, fUrl);
  if not fDescription.IsEmpty then
    Result.AddPair(c_SwagServerDescription, fDescription);
  if not fName.IsEmpty then
    Result.AddPair(c_SwagServerName, fName);
  if fVariables.Count > 0 then
  begin
    vJsonVariables := TJSONObject.Create;
    for vIndex := 0 to fVariables.Count - 1 do
      vJsonVariables.AddPair(fVariables[vIndex].Name, fVariables[vIndex].GenerateJsonObject);
    Result.AddPair(c_SwagServerVariables, vJsonVariables);
  end;
  fExtensions.WriteTo(Result);
end;

procedure TSwagServer.Load(pJson: TJSONObject);
var
  vJsonVariables: TJSONObject;
  vIndex: Integer;
begin
  if not Assigned(pJson) then
    Exit;

  fUrl := TSwagJson.ReadString(pJson, c_SwagServerUrl);
  fDescription := TSwagJson.ReadString(pJson, c_SwagServerDescription);
  fName := TSwagJson.ReadString(pJson, c_SwagServerName);
  vJsonVariables := TSwagJson.ReadObject(pJson, c_SwagServerVariables);
  if Assigned(vJsonVariables) then
    for vIndex := 0 to vJsonVariables.Count - 1 do
      if vJsonVariables.Pairs[vIndex].JsonValue is TJSONObject then
        AddVariable(vJsonVariables.Pairs[vIndex].JsonString.Value, '')
          .Load(TJSONObject(vJsonVariables.Pairs[vIndex].JsonValue));
  fExtensions.ReadFrom(pJson);
end;

end.
