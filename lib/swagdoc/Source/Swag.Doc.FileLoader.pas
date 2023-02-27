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
  Swag.Doc.Path,
  Swag.Doc.Tags,
  Swag.Doc.SecurityDefinition,
  Swag.Doc.Definition,
  Swag.Doc.Path.Operation.RequestParameter;


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
  vJsonSecurityDefinitions: TJSONObject;
  vJsonSecurityDefinitionItem: TJSONObject;
  vSecurityDefinitionClass: TPersistentClass;
  vSecurityDefinitionType: TSwagSecurityDefinitionType;
  vSecurityDefinition: TSwagSecurityDefinition;
  vJsonExternalDocs: TJSONObject;
  vIndex: Integer;
begin
  if not FileExists(pFilename) then
    raise ESwagErrorLoadSwaggerJsonFile.Create('File doesn''t exist ['+pFilename+']');

  vSwaggerJson := TJSONObject.ParseJSONValue(TFile.ReadAllText(pFilename)) as TJSONObject;
  try
    if not Assigned(vSwaggerJson) then
      raise ESwagErrorLoadSwaggerJsonFile.Create('File could not be loaded ['+pFilename+']');

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

    { TODO : test... }
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
          vSecurityDefinition := TSwagSecurityDefinition(vSecurityDefinitionClass).Create;
          vSecurityDefinition.SchemeName := (vJsonSecurityDefinitions.Pairs[vIndex] as TJSONPair).JsonString.Value;
          vSecurityDefinition.Load((vJsonSecurityDefinitions.Pairs[vIndex] as TJSONPair).JsonValue as TJSONObject);
          fSwagDoc.SecurityDefinitions.Add(vSecurityDefinition);
        end;
      end;

    vJsonExternalDocs := (vSwaggerJson as TJSONObject).Values[c_SwagExternalDocs] as TJSONObject;
    if Assigned(vJsonExternalDocs) then
    begin
      if Assigned(vJsonExternalDocs.Values[c_SwagExternalDocsUrl]) then
        fSwagDoc.ExternalDocs.Url := vJsonExternalDocs.Values[c_SwagExternalDocsUrl].Value;
      if Assigned(vJsonExternalDocs.Values[c_SwagExternalDocsDescription]) then
        fSwagDoc.ExternalDocs.Description := vJsonExternalDocs.Values[c_SwagExternalDocsDescription].Value;
    end;
  finally
    vSwaggerJson.Free;
  end;
end;

end.
