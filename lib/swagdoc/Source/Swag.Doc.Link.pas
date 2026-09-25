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

unit Swag.Doc.Link;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.JSON,
  Swag.Doc.Server,
  Swag.Doc.Extensions;

type
  /// <summary>
  /// Represents a possible design-time link for a response. A link describes how values of the response can be
  /// used as parameters or as the request body of another operation, identified by OperationId or OperationRef.
  /// Available in OpenAPI 3 only.
  /// </summary>
  TSwagLink = class(TObject)
  private
    fName: string;
    fRef: string;
    fOperationRef: string;
    fOperationId: string;
    fParameters: TJSONObject;
    fRequestBody: TJSONValue;
    fDescription: string;
    fServer: TSwagServer;
    fExtensions: TSwagExtensions;
    procedure SetRequestBody(const pValue: TJSONValue);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson: TJSONObject);

    /// <summary>
    /// Adds a parameter whose value is a runtime expression, for example $response.body#/id, or a constant string.
    /// The name can be qualified with the parameter location, for example path.id.
    /// </summary>
    procedure AddParameter(const pName, pExpression: string); overload;

    /// <summary>
    /// Adds a parameter with a constant JSON value. The link takes ownership of the value.
    /// </summary>
    procedure AddParameter(const pName: string; pValue: TJSONValue); overload;

    /// <summary>
    /// Generates the map of links, using the Name of each link as the key.
    /// </summary>
    class function GenerateMapJsonObject(pLinks: TObjectList<TSwagLink>): TJSONObject;

    /// <summary>
    /// Loads a map of links into the list, using the key of each item as the Name of the link.
    /// </summary>
    class procedure LoadMap(pJson: TJSONObject; pLinks: TObjectList<TSwagLink>);

    /// <summary>
    /// A short name for the link, used as its key in the map that contains it.
    /// </summary>
    property Name: string read fName write fName;

    /// <summary>
    /// A reference to a reusable link, for example #/components/links/GetEmployeeById. When defined, only the
    /// reference and the Description are written.
    /// </summary>
    property Ref: string read fRef write fRef;

    /// <summary>
    /// A URI reference to an OAS operation. This field is mutually exclusive of the OperationId field.
    /// </summary>
    property OperationRef: string read fOperationRef write fOperationRef;

    /// <summary>
    /// The name of an existing, resolvable OAS operation, as defined with a unique operationId.
    /// This field is mutually exclusive of the OperationRef field.
    /// </summary>
    property OperationId: string read fOperationId write fOperationId;

    /// <summary>
    /// A map representing parameters to pass to the linked operation. Each value is a constant or a runtime expression.
    /// </summary>
    property Parameters: TJSONObject read fParameters;

    /// <summary>
    /// A literal value or runtime expression to use as a request body when calling the target operation.
    /// The link takes ownership of the assigned value.
    /// </summary>
    property RequestBody: TJSONValue read fRequestBody write SetRequestBody;

    /// <summary>
    /// A description of the link. CommonMark syntax MAY be used for rich text representation.
    /// </summary>
    property Description: string read fDescription write fDescription;

    /// <summary>
    /// A server object to be used by the target operation. It is written only when its Url is defined.
    /// </summary>
    property Server: TSwagServer read fServer;

    /// <summary>
    /// The Specification Extensions of the link.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

implementation

uses
  Swag.Common.Json;

const
  c_SwagLinkRef = '$ref';
  c_SwagLinkOperationRef = 'operationRef';
  c_SwagLinkOperationId = 'operationId';
  c_SwagLinkParameters = 'parameters';
  c_SwagLinkRequestBody = 'requestBody';
  c_SwagLinkDescription = 'description';
  c_SwagLinkServer = 'server';

{ TSwagLink }

constructor TSwagLink.Create;
begin
  inherited Create;
  fParameters := TJSONObject.Create;
  fServer := TSwagServer.Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagLink.Destroy;
begin
  FreeAndNil(fParameters);
  FreeAndNil(fRequestBody);
  FreeAndNil(fServer);
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

procedure TSwagLink.SetRequestBody(const pValue: TJSONValue);
begin
  if fRequestBody = pValue then
    Exit;
  fRequestBody.Free;
  fRequestBody := pValue;
end;

procedure TSwagLink.AddParameter(const pName, pExpression: string);
begin
  AddParameter(pName, TJSONString.Create(pExpression));
end;

procedure TSwagLink.AddParameter(const pName: string; pValue: TJSONValue);
begin
  fParameters.RemovePair(pName).Free;
  fParameters.AddPair(pName, pValue);
end;

function TSwagLink.GenerateJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  if not fRef.IsEmpty then
  begin
    Result.AddPair(c_SwagLinkRef, fRef);
    if not fDescription.IsEmpty then
      Result.AddPair(c_SwagLinkDescription, fDescription);
    Exit;
  end;

  if not fOperationRef.IsEmpty then
    Result.AddPair(c_SwagLinkOperationRef, fOperationRef);
  if not fOperationId.IsEmpty then
    Result.AddPair(c_SwagLinkOperationId, fOperationId);
  if fParameters.Count > 0 then
    Result.AddPair(c_SwagLinkParameters, fParameters.Clone as TJSONObject);
  if Assigned(fRequestBody) then
    Result.AddPair(c_SwagLinkRequestBody, fRequestBody.Clone as TJSONValue);
  if not fDescription.IsEmpty then
    Result.AddPair(c_SwagLinkDescription, fDescription);
  if not fServer.IsEmpty then
    Result.AddPair(c_SwagLinkServer, fServer.GenerateJsonObject);
  fExtensions.WriteTo(Result);
end;

procedure TSwagLink.Load(pJson: TJSONObject);
var
  vJsonParameters: TJSONObject;
begin
  if not Assigned(pJson) then
    Exit;

  fRef := TSwagJson.ReadString(pJson, c_SwagLinkRef);
  fDescription := TSwagJson.ReadString(pJson, c_SwagLinkDescription);
  if not fRef.IsEmpty then
    Exit;

  fOperationRef := TSwagJson.ReadString(pJson, c_SwagLinkOperationRef);
  fOperationId := TSwagJson.ReadString(pJson, c_SwagLinkOperationId);
  vJsonParameters := TSwagJson.ReadObject(pJson, c_SwagLinkParameters);
  if Assigned(vJsonParameters) then
  begin
    fParameters.Free;
    fParameters := vJsonParameters.Clone as TJSONObject;
  end;
  SetRequestBody(TSwagJson.CloneValue(pJson, c_SwagLinkRequestBody));
  fServer.Load(TSwagJson.ReadObject(pJson, c_SwagLinkServer));
  fExtensions.ReadFrom(pJson);
end;

class function TSwagLink.GenerateMapJsonObject(pLinks: TObjectList<TSwagLink>): TJSONObject;
var
  vLink: TSwagLink;
begin
  Result := TJSONObject.Create;
  for vLink in pLinks do
    Result.AddPair(vLink.Name, vLink.GenerateJsonObject);
end;

class procedure TSwagLink.LoadMap(pJson: TJSONObject; pLinks: TObjectList<TSwagLink>);
var
  vIndex: Integer;
  vLink: TSwagLink;
begin
  if not Assigned(pJson) then
    Exit;

  for vIndex := 0 to pJson.Count - 1 do
  begin
    if not (pJson.Pairs[vIndex].JsonValue is TJSONObject) then
      Continue;

    vLink := TSwagLink.Create;
    vLink.Name := pJson.Pairs[vIndex].JsonString.Value;
    vLink.Load(TJSONObject(pJson.Pairs[vIndex].JsonValue));
    pLinks.Add(vLink);
  end;
end;

end.
