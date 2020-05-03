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

unit Swag.Doc.Path;

interface

uses  
  System.Classes,
  System.Generics.Collections,
  System.RegularExpressions,
  System.JSON,
  Swag.Common.Types,
  Swag.Doc.Path.Operation.RequestParameter,
  Swag.Doc.Path.Operation.Response,
  Swag.Doc.Path.Operation;

type
  /// <summary>
  /// Holds the relative paths to the individual endpoints.
  /// The path is appended to the basePath in order to construct the full URL.
  /// The Paths may be empty, due to ACL constraints.
  /// </summary>
  TSwagPath = class(TObject)
  private
    fOperations: TObjectList<TSwagPathOperation>;
    fUri: string;
    fParameters: TObjectList<TSwagRequestParameter>;
    procedure LoadResponse(pOperation: TSwagPathOperation; pJsonResponse: TJSONObject);
    procedure LoadOperationScopedParameters(pOperation: TSwagPathOperation; pJsonRequestParams: TJSONArray);
    procedure LoadPathScopedParameters(pJsonRequestParams: TJSONArray);
    procedure LoadTags(pOperation: TSwagPathOperation; pJsonTags: TJSONArray);
    procedure LoadProduces(pOperation: TSwagPathOperation; pJsonProduces: TJSONArray);
    procedure LoadConsumes(pOperation: TSwagPathOperation; pJsonConsumes: TJSONArray);
    function GenerateParametersJsonObject: TJSONArray;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson: TJSONObject);

    /// <summary>
    /// A relative path to an individual endpoint. The field name MUST begin with a slash.
    /// The path is appended to the basePath in order to construct the full URL. Path templating is allowed.
    /// </summary>
    property Uri: string read fUri write fUri;

    /// <summary>
    /// Describes a single API operation on a path.
    /// </summary>
    property Operations: TObjectList<TSwagPathOperation> read fOperations;

    property Parameters: TObjectList<TSwagRequestParameter> read fParameters;
  end;

implementation

uses
  System.SysUtils,
  Swag.Common.Types.Helpers;

{ TSwagPath }

constructor TSwagPath.Create;
begin
  inherited Create;
  fOperations := TObjectList<TSwagPathOperation>.Create;
  fParameters := TObjectList<TSwagRequestParameter>.Create;
end;

destructor TSwagPath.Destroy;
begin
  FreeAndNil(fOperations);
  FreeAndNil(fParameters);
  inherited Destroy;
end;

function TSwagPath.GenerateParametersJsonObject: TJSONArray;
var
  vIndex: Integer;
begin
  Result := TJSONArray.Create;
  for vIndex := 0 to fParameters.Count - 1 do
  begin
    Result.Add(fParameters[vIndex].GenerateJsonObject);
  end;
end;

function TSwagPath.GenerateJsonObject: TJSONObject;
var
  vIndex: integer;
begin
  Result := TJsonObject.Create;
  if fParameters.Count > 0 then
    Result.AddPair('parameters', GenerateParametersJsonObject);
  for vIndex := 0 to fOperations.Count -1 do
    Result.AddPair(fOperations.Items[vIndex].OperationToString, fOperations.Items[vIndex].GenerateJsonObject);
end;

procedure TSwagPath.Load(pJson: TJSONObject);
var
  vIndex: Integer;
  vOperation: TSwagPathOperation;
  vOperationJson: TJSONObject;
  vOperationExternalDocs: TJSONObject;
  vOperationName: string;
begin
  if not Assigned(pJson) then
    Exit;

  for vIndex := 0 to pJson.Count - 1 do
  begin
    vOperation := TSwagPathOperation.Create;
    vOperationName := pJson.Pairs[vIndex].JsonString.Value;
    if vOperationName = 'parameters' then
    begin
      LoadPathScopedParameters(pJson.Pairs[vIndex].JsonValue as TJSONArray);
      continue;
    end;
    if TRegEx.IsMatch(vOperationName, '(^x-)') then
    begin
      // This is an extension value - ignore
      continue;
    end;
    if not (pJson.Pairs[vIndex].JsonValue is TJSONObject) then
    begin
      // This shouldn't happen - although it may be valid in openapi documents
      continue;
    end;

    vOperationJson := pJson.Pairs[vIndex].JsonValue as TJSONObject;
    if Assigned(vOperationJson.Values['description']) then
      vOperation.Description := vOperationJson.Values['description'].Value;
    if Assigned(vOperationJson.Values['summary']) then
      vOperation.Summary := vOperationJson.Values['summary'].Value;

    vOperationExternalDocs := vOperationJson.Values['externalDocs'] as TJSONObject;
    if Assigned(vOperationExternalDocs) then
    begin
      if Assigned(vOperationExternalDocs.Values['url']) then
        vOperation.ExternalDocs.Url := vOperationExternalDocs.Values['url'].Value;
      if Assigned(vOperationExternalDocs.Values['description'])then
        vOperation.ExternalDocs.Description := vOperationExternalDocs.Values['description'].Value;
    end;

    vOperation.Operation.ToType(pJson.Pairs[vIndex].JsonString.Value);

    if Assigned(vOperationJson.Values['operationId']) then
      vOperation.OperationId := vOperationJson.Values['operationId'].Value;

    if Assigned(vOperationJson.Values['deprecated']) then
      vOperation.Deprecated := (vOperationJson.Values['deprecated'] as TJSONBool).AsBoolean;

    LoadTags(vOperation, vOperationJson.Values['tags'] as TJSONArray);
    LoadProduces(vOperation, vOperationJson.Values['produces'] as TJSONArray);
    LoadConsumes(vOperation, vOperationJson.Values['consumes'] as TJSONArray);
    LoadOperationScopedParameters(vOperation, vOperationJson.Values['parameters'] as TJSONArray);
    LoadResponse(vOperation, vOperationJson.Values['responses'] as TJSONObject);

    fOperations.Add(vOperation);
  end;
end;

procedure TSwagPath.LoadTags(pOperation: TSwagPathOperation; pJsonTags: TJSONArray);
var
  vIndex: Integer;
  vTag: string;
begin
  if not Assigned(pJsonTags) then
    Exit;

  for vIndex := 0 to pJsonTags.Count - 1 do
  begin
    vTag := pJsonTags.Items[vIndex].Value;
    pOperation.Tags.Add(vTag);
  end;
end;

procedure TSwagPath.LoadPathScopedParameters(pJsonRequestParams: TJSONArray);
var
  vIndex: Integer;
  vRequestParam: TSwagRequestParameter;
begin
  if not Assigned(pJsonRequestParams) then
    Exit;

  for vIndex := 0 to pJsonRequestParams.Count - 1 do
  begin
    vRequestParam := TSwagRequestParameter.Create;
    vRequestParam.Load(pJsonRequestParams.Items[vIndex] as TJSONObject);
    Parameters.Add(vRequestParam);
  end;
end;

procedure TSwagPath.LoadOperationScopedParameters(pOperation: TSwagPathOperation; pJsonRequestParams: TJSONArray);
var
  vIndex: Integer;
  vRequestParam: TSwagRequestParameter;
begin
  if not Assigned(pJsonRequestParams) then
    Exit;

  for vIndex := 0 to pJsonRequestParams.Count - 1 do
  begin
    vRequestParam := TSwagRequestParameter.Create;
    vRequestParam.Load(pJsonRequestParams.Items[vIndex] as TJSONObject);
    pOperation.Parameters.Add(vRequestParam);
  end;
end;

procedure TSwagPath.LoadProduces(pOperation: TSwagPathOperation; pJsonProduces: TJSONArray);
var
  vIndex: Integer;
  vProduces: string;
begin
  if not Assigned(pJsonProduces) then
    Exit;

  for vIndex := 0 to pJsonProduces.Count - 1 do
  begin
    vProduces := pJsonProduces.Items[vIndex].Value;
    pOperation.Produces.Add(vProduces);
  end;
end;

procedure TSwagPath.LoadConsumes(pOperation: TSwagPathOperation; pJsonConsumes: TJSONArray);
var
  vIndex: Integer;
  vConsumes: string;
begin
  if not Assigned(pJsonConsumes) then
    Exit;

  for vIndex := 0 to pJsonConsumes.Count - 1 do
  begin
    vConsumes := pJsonConsumes.Items[vIndex].Value;
    pOperation.Consumes.Add(vConsumes);
  end;
end;

procedure TSwagPath.LoadResponse(pOperation: TSwagPathOperation; pJsonResponse: TJSONObject);
var
  vIndex: Integer;
  vResponse: TSwagResponse;
begin
  if not Assigned(pJsonResponse) then
    Exit;

  for vIndex := 0 to pJsonResponse.Count - 1 do
  begin
    vResponse := TSwagResponse.Create;
    vResponse.StatusCode := pJsonResponse.Pairs[vIndex].JsonString.Value;
    vResponse.Load(pJsonResponse.Pairs[vIndex].JsonValue as TJSONObject);
    pOperation.Responses.Add(vResponse.StatusCode, vResponse);
  end;
end;

end.
