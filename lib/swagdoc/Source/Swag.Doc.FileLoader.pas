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


unit Swag.Doc.FileLoader;

interface

uses
  Swag.Doc;

type
  /// <summary>
  /// Loads a swagger.json or an openapi.json file into a TSwagDoc object model.
  /// The version of the specification is detected by the root field of the document and the SpecVersion
  /// property of the target document is updated accordingly.
  /// </summary>
  TSwagFileLoader = class(TObject)
  strict private
    fSwagDoc: TSwagDoc;
  public
    constructor Create(pSwagDocTarget: TSwagDoc);
    procedure Load(const pFilename: string);
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  System.JSON,
  System.IOUtils,
  System.Classes,
  Swag.Common.Types,
  Swag.Common.Types.Helpers,
  Swag.Doc.Path.Operation,
  Swag.Doc.Path.Operation.Response,
  Swag.Doc.Tags,
  Swag.Doc.SecurityDefinition,
  Swag.Doc.SecurityDefinitionBasic,
  Swag.Doc.SecurityDefinitionApiKey,
  Swag.Doc.SecurityDefinitionOAuth2,
  Swag.Doc.SecurityDefinitionHttp,
  Swag.Doc.SecurityDefinitionOpenIdConnect,
  Swag.Doc.SecurityDefinitionMutualTls,
  Swag.Doc.SecurityRequirement,
  Swag.Doc.Definition,
  Swag.Doc.Path.Operation.RequestParameter,
  Swag.Doc.OpenApi.Loader;


{ TSwagFileLoader }

constructor TSwagFileLoader.Create(pSwagDocTarget: TSwagDoc);
begin
  inherited Create;
  fSwagDoc := pSwagDocTarget;
end;

procedure TSwagFileLoader.Load(const pFilename: string);
var
  vSwaggerJson: TJSONValue;
  vJsonObj: TJSONObject;
  vPath: TSwagPath;
  vJsonTagsArray: TJSONArray;
  vTag: TSwagTag;
  vJsonSchemesArray: TJSONArray;
  vJsonProduces: TJSONArray;
  vJsonConsumes: TJSONArray;
  vJsonDefinitions: TJSONObject;
  vDefinition: TSwagDefinition;
  vJsonParameters: TJSONObject;
  vParameter: TSwagRequestParameter;
  vJsonResponses: TJSONObject;
  vResponse: TSwagResponse;
  vJsonSecurity: TJSONArray;
  vJsonSecurityDefinitions: TJSONObject;
  vJsonSecurityDefinitionItem: TJSONObject;
  vSecurityDefinitionClass: TPersistentClass;
  vSecurityDefinitionType: TSwagSecurityDefinitionType;
  vSecurityDefinition: TSwagSecurityDefinition;
  vJsonExternalDocs: TJSONObject;
  vOpenApiLoader: TSwagOpenApiLoader;
  vIndex: Integer;
begin
  if not FileExists(pFilename) then
    raise ESwagErrorLoadSwaggerJsonFile.Create('File doesn''t exist ['+pFilename+']');

  vSwaggerJson := TJSONObject.ParseJSONValue(TFile.ReadAllText(pFilename)) as TJSONObject;
  try
    if not Assigned(vSwaggerJson) then
      raise ESwagErrorLoadSwaggerJsonFile.Create('File could not be loaded ['+pFilename+']');

    if Assigned((vSwaggerJson as TJSONObject).Values[c_OpenApi]) then
    begin
      fSwagDoc.SpecVersion := svOpenApi3;
      vOpenApiLoader := TSwagOpenApiLoader.Create(fSwagDoc);
      try
        vOpenApiLoader.Load(vSwaggerJson as TJSONObject);
      finally
        vOpenApiLoader.Free;
      end;
      Exit;
    end;

    fSwagDoc.SpecVersion := svSwagger2;

    fSwagDoc.Info.Load((vSwaggerJson as TJSONObject).Values[c_SwagInfo] as TJSONObject);

    vJsonTagsArray := (vSwaggerJson as TJSONObject).Values[c_SwagTags] as TJSONArray;
    if Assigned(vJsonTagsArray) then
      for vIndex := 0 to vJsonTagsArray.Count - 1 do
      begin
        vTag := TSwagTag.Create;
        vTag.Load(vJsonTagsArray.Items[vIndex] as TJSONObject);
        fSwagDoc.Tags.Add(vTag);
      end;

    vJsonObj := (vSwaggerJson as TJSONObject).Values[c_SwagPaths] as TJSONObject;
    vJsonSchemesArray := (vSwaggerJson as TJSONObject).Values[c_SwagSchemes] as TJSONArray;
    if Assigned(vJsonSchemesArray) then
      for vIndex := 0 to vJsonSchemesArray.Count - 1 do
      begin
        fSwagDoc.Schemes.Add(vJsonSchemesArray.Items[vIndex].Value);
      end;

    if Assigned((vSwaggerJson as TJSONObject).Values[c_SwagHost]) then
      fSwagDoc.Host := (vSwaggerJson as TJSONObject).Values[c_SwagHost].Value
    else
      fSwagDoc.Host := EmptyStr;

    if Assigned((vSwaggerJson as TJSONObject).Values[c_SwagBasePath]) then
      fSwagDoc.BasePath := (vSwaggerJson as TJSONObject).Values[c_SwagBasePath].Value
    else
      fSwagDoc.BasePath := EmptyStr;

    if Assigned(vJsonObj) then
      for vIndex := 0 to vJsonObj.Count - 1 do
      begin
        vPath := TSwagPath.Create;
        vPath.Uri := vJsonObj.Pairs[vIndex].JSONString.Value;
        vPath.Load((vJsonObj.Pairs[vIndex].JsonValue) as TJSONObject);
        fSwagDoc.Paths.Add(vPath);
      end;

    vJsonProduces := (vSwaggerJson as TJSONObject).Values[c_SwagProduces] as TJSONArray;
    if Assigned(vJsonProduces) then
      for vIndex := 0 to vJsonProduces.Count - 1 do
      begin
        fSwagDoc.Produces.Add(vJsonProduces.Items[vIndex].Value);
      end;

    vJsonConsumes := (vSwaggerJson as TJSONObject).Values[c_SwagConsumes] as TJSONArray;
    if Assigned(vJsonConsumes) then
      for vIndex := 0 to vJsonConsumes.count - 1 do
      begin
        fSwagDoc.Consumes.Add(vJsonConsumes.Items[vIndex].Value);
      end;

    vJsonDefinitions := (vSwaggerJson as TJSONObject).Values[c_SwagDefinitions] as TJSONObject;
    if Assigned(vJsonDefinitions) then
      for vIndex := 0 to vJsonDefinitions.Count - 1 do
      begin
        vDefinition := TSwagDefinition.Create;
        vDefinition.Name := (vJsonDefinitions.Pairs[vIndex] as TJSONPair).JsonString.Value;
        vDefinition.JsonSchema := ((vJsonDefinitions.Pairs[vIndex] as TJSONPair).JsonValue.Clone as TJSONObject);
        fSwagDoc.Definitions.Add(vDefinition);
      end;

    vJsonParameters := (vSwaggerJson as TJSONObject).Values[c_SwagParameters] as TJSONObject;
    if Assigned(vJsonParameters) then
      for vIndex := 0 to vJsonParameters.Count - 1 do
      begin
        vParameter := TSwagRequestParameter.Create;
        vParameter.Name := (vJsonParameters.Pairs[vIndex] as TJSONPair).JsonString.Value;
        vParameter.Load((vJsonParameters.Pairs[vIndex] as TJSONPair).JsonValue as TJSONObject);
        fSwagDoc.Parameters.Add(vParameter);
      end;

    if (vSwaggerJson as TJSONObject).Values[c_SwagResponses] is TJSONObject then
    begin
      vJsonResponses := TJSONObject((vSwaggerJson as TJSONObject).Values[c_SwagResponses]);
      for vIndex := 0 to vJsonResponses.Count - 1 do
        if vJsonResponses.Pairs[vIndex].JsonValue is TJSONObject then
        begin
          vResponse := TSwagResponse.Create;
          vResponse.Name := vJsonResponses.Pairs[vIndex].JsonString.Value;
          vResponse.Load(TJSONObject(vJsonResponses.Pairs[vIndex].JsonValue));
          fSwagDoc.Responses.Add(vResponse);
        end;
    end;

    vJsonSecurityDefinitions := (vSwaggerJson as TJSONObject).Values[c_SwagSecurityDefinitions] as TJSONObject;
    if Assigned(vJsonSecurityDefinitions) then
      for vIndex := 0 to vJsonSecurityDefinitions.Count - 1 do
      begin
        vJsonSecurityDefinitionItem := vJsonSecurityDefinitions.Pairs[vIndex].JsonValue as TJSONObject;
        if not Assigned(vJsonSecurityDefinitionItem) or
           not Assigned(vJsonSecurityDefinitionItem.Values[c_SwagSecurityDefinitionsType]) or
           vJsonSecurityDefinitionItem.Values[c_SwagSecurityDefinitionsType].Value.Trim.IsEmpty then
          Continue;

        vSecurityDefinitionType.ToType(vJsonSecurityDefinitionItem.Values[c_SwagSecurityDefinitionsType].Value);
        vSecurityDefinitionClass := TSwagGetClassSecurityDefinition.Find(vSecurityDefinitionType);
        if Assigned(vSecurityDefinitionClass) then
        begin
          vSecurityDefinition := TSwagSecurityDefinitionClass(vSecurityDefinitionClass).Create;
          vSecurityDefinition.SchemeName := (vJsonSecurityDefinitions.Pairs[vIndex] as TJSONPair).JsonString.Value;
          vSecurityDefinition.Extensions.ReadFrom(vJsonSecurityDefinitionItem);
          vSecurityDefinition.Load((vJsonSecurityDefinitions.Pairs[vIndex] as TJSONPair).JsonValue as TJSONObject);
          fSwagDoc.SecurityDefinitions.Add(vSecurityDefinition);
        end;
      end;

    if (vSwaggerJson as TJSONObject).Values[c_SwagSecurity] is TJSONArray then
    begin
      vJsonSecurity := TJSONArray((vSwaggerJson as TJSONObject).Values[c_SwagSecurity]);
      if vJsonSecurity.Count = 0 then
        fSwagDoc.DisableSecurity := True
      else
        TSwagSecurityRequirement.LoadArray(vJsonSecurity, fSwagDoc.SecurityRequirements);
    end;

    vJsonExternalDocs := (vSwaggerJson as TJSONObject).Values[c_SwagExternalDocs] as TJSONObject;
    if Assigned(vJsonExternalDocs) then
      fSwagDoc.ExternalDocs.Load(vJsonExternalDocs);

    fSwagDoc.Extensions.ReadFrom(vSwaggerJson as TJSONObject);
  finally
    vSwaggerJson.Free;
  end;
end;

end.
