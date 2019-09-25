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
    procedure LoadResponse(pOperation: TSwagPathOperation; pJsonResponse: TJSONObject);
    procedure LoadParameters(pOperation: TSwagPathOperation; pJsonRequestParams: TJSONArray);
    procedure LoadTags(pOperation: TSwagPathOperation; pJsonTags: TJSONArray);
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
end;

destructor TSwagPath.Destroy;
begin
  FreeAndNil(fOperations);
  inherited Destroy;
end;

function TSwagPath.GenerateJsonObject: TJSONObject;
var
  vIndex: integer;
begin
  Result := TJsonObject.Create;
  for vIndex := 0 to fOperations.Count -1 do
    Result.AddPair(fOperations.Items[vIndex].OperationToString, fOperations.Items[vIndex].GenerateJsonObject);
end;

procedure TSwagPath.Load(pJson: TJSONObject);
var
  vIndex: Integer;
  vOperation: TSwagPathOperation;
  vOperationJson: TJSONObject;
begin
  if not Assigned(pJson) then
    Exit;

  for vIndex := 0 to pJson.Count - 1 do
  begin
    vOperation := TSwagPathOperation.Create;
    vOperationJson := pJson.Pairs[vIndex].JsonValue as TJSONObject;
    vOperation.Description := vOperationJson.Values['description'].Value;
    vOperation.Operation.ToType(pJson.Pairs[vIndex].JsonString.Value);

    if Assigned(vOperationJson.Values['operationId']) then
      vOperation.OperationId := vOperationJson.Values['operationId'].Value;

    if Assigned(vOperationJson.Values['deprecated']) then
      vOperation.Deprecated := (vOperationJson.Values['deprecated'] as TJSONBool).AsBoolean;

    LoadTags(vOperation, vOperationJson.Values['tags'] as TJSONArray);
    LoadParameters(vOperation, vOperationJson.Values['parameters'] as TJSONArray);
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

procedure TSwagPath.LoadParameters(pOperation: TSwagPathOperation; pJsonRequestParams: TJSONArray);
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
