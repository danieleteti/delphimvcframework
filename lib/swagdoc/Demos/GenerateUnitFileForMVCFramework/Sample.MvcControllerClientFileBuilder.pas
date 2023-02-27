{******************************************************************************}
{                                                                              }
{  Delphi SwagDoc Library                                                      }
{  Copyright (c) 2018 Marcelo Jaloto                                           }
{  https://github.com/marcelojaloto/SwagDoc                                    }
{                                                                              }
{  Sample author: geoffsmith82 - 2019                                          }
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

unit Sample.MvcControllerClientFileBuilder;

interface

uses
  System.Classes,
  System.Json,
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  Swag.Doc,
  Swag.Common.Types,
  Swag.Doc.Path.Operation,
  Swag.Doc.Path.Operation.Response,
  Swag.Doc.Path.Operation.RequestParameter,
  Sample.DelphiUnit.Generate;

type
  TSwagDocToDelphiRESTClientBuilder = class(TObject)
  strict private
    fSwagDoc: TSwagDoc;

    function CapitalizeFirstLetter(const pTypeName: string): string;
    function RewriteUriToSwaggerWay(const pUri: string): string;
    function OperationIdToFunctionName(pOperation: TSwagPathOperation): string;
    function GenerateUnitText(pDelphiUnit: TDelphiUnit): string;
    function ConvertSwaggerTypeToDelphiType(pSwaggerType: TSwagRequestParameter): TUnitTypeDefinition;
    function ConvertRefToType(const pRef: string): string;
    function ConvertRefToVarName(const pRef: string): string;

    procedure ChildType(pDelphiUnit: TDelphiUnit; pJson: TJSONPair);
    procedure HandleArray(pField: TUnitFieldDefinition; pJson: TJSONPair);
    procedure ConvertSwaggerDefinitionsToTypeDefinitions(pDelphiUnit: TDelphiUnit);
  public
    constructor Create(pSwagDoc: TSwagDoc); reintroduce;
    function Generate: string;
  end;

implementation

uses
  Winapi.Windows,
  System.IOUtils,
  System.TypInfo,
  Json.Common.Helpers;

{ TSwagDocToDelphiMVCFrameworkBuilder }

constructor TSwagDocToDelphiRESTClientBuilder.Create(pSwagDoc: TSwagDoc);
begin
  inherited Create;
  fSwagDoc := pSwagDoc;
end;

function TSwagDocToDelphiRESTClientBuilder.OperationIdToFunctionName(pOperation: TSwagPathOperation): string;
begin
  Result := pOperation.OperationId.Replace('{','').Replace('}','').Replace('-','');
  if not CharInSet(Result[1], ['a'..'z','A'..'Z']) then
    Result := 'F' + Result;

  Result := CapitalizeFirstLetter(Result);
end;

function TSwagDocToDelphiRESTClientBuilder.RewriteUriToSwaggerWay(const pUri: string): string;
begin
  Result := pUri.Replace('{','($').Replace('}',')');
end;

function TSwagDocToDelphiRESTClientBuilder.CapitalizeFirstLetter(const pTypeName: string): string;
begin
  if pTypeName.Length > 2 then
    Result := Copy(pTypeName, 1, 1).ToUpper + Copy(pTypeName, 2, pTypeName.Length - 1)
  else
    Result := pTypeName;
end;

function TSwagDocToDelphiRESTClientBuilder.ConvertRefToType(const pRef: string): string;
begin
  Result := Copy(pRef, pRef.LastIndexOf('/') + 2);
  Result := Copy(Result,1,1).ToUpper + Copy(Result,2);
  if Result.ToLower <> 'string' then
    Result := 'T' + Result;
end;

function TSwagDocToDelphiRESTClientBuilder.ConvertRefToVarName(const pRef: string): string;
begin
  Result := Copy(pRef, pRef.LastIndexOf('/') + 2);
end;

function TSwagDocToDelphiRESTClientBuilder.Generate: string;
var
  vPathIndex: Integer;
  vOperationIndex: Integer;
  vParameterIndex: Integer;
  vDelphiUnit: TDelphiUnit;
  vMVCControllerClient: TUnitTypeDefinition;
  vMethod: TUnitMethod;
  vResponse: TPair<string, TSwagResponse>;
  vSchemaObj: TJsonObject;
  vResultParam: TUnitParameter;
  vField: TUnitFieldDefinition;
  vRef: String;
begin
  vDelphiUnit := TDelphiUnit.Create;
  try
    vDelphiUnit.UnitFile := 'UnitFilenameMvcControllerClient';
    vDelphiUnit.AddInterfaceUnit('IPPeerClient');
    vDelphiUnit.AddInterfaceUnit('REST.Client');
    vDelphiUnit.AddInterfaceUnit('REST.Authenticator.OAuth');
    vDelphiUnit.AddInterfaceUnit('REST.Types');
    vDelphiUnit.AddInterfaceUnit('MVCFramework');
    vDelphiUnit.AddInterfaceUnit('MVCFramework.Commons');
    vDelphiUnit.AddImplementationUnit('Swag.Doc');

    ConvertSwaggerDefinitionsToTypeDefinitions(vDelphiUnit);

    vMVCControllerClient := TUnitTypeDefinition.Create;
    vMVCControllerClient.TypeName := 'TMyMVCControllerClient';
    vMVCControllerClient.TypeInherited := 'TObject';
    vMVCControllerClient.AddAttribute('  [MVCPath(''' + RewriteUriToSwaggerWay(fSwagDoc.BasePath) + ''')]');

    vField := TUnitFieldDefinition.Create;
    vField.FieldName := 'RESTClient';
    vField.FieldType := 'TRESTClient';
    vMVCControllerClient.Fields.Add(vField);

    vField := TUnitFieldDefinition.Create;
    vField.FieldName := 'RESTRequest';
    vField.FieldType := 'TRESTRequest';
    vMVCControllerClient.Fields.Add(vField);

    vField := TUnitFieldDefinition.Create;
    vField.FieldName := 'RESTResponse';
    vField.FieldType := 'TRESTResponse';
    vMVCControllerClient.Fields.Add(vField);

    vDelphiUnit.AddType(vMVCControllerClient);
    ConvertSwaggerDefinitionsToTypeDefinitions(vDelphiUnit);

    for vPathIndex := 0 to fSwagDoc.Paths.Count - 1 do
    begin
      for vOperationIndex := 0 to fSwagDoc.Paths[vPathIndex].Operations.Count - 1 do
      begin
        vMethod := TUnitMethod.Create;
        if fSwagDoc.Paths[vPathIndex].Operations[vOperationIndex].Description.Trim.Length > 0 then
          vMethod.AddAttribute('    [MVCDoc(' + QuotedStr(fSwagDoc.Paths[vPathIndex].Operations[vOperationIndex].Description) + ')]');
        vMethod.AddAttribute('    [MVCPath(''' + fSwagDoc.Paths[vPathIndex].Uri + ''')]');
        vMethod.AddAttribute('    [MVCHTTPMethod([http' + fSwagDoc.Paths[vPathIndex].Operations[vOperationIndex].OperationToString + '])]');
        vMethod.Name := OperationIdToFunctionName(fSwagDoc.Paths[vPathIndex].Operations[vOperationIndex]);

        for vParameterIndex := 0 to fSwagDoc.Paths[vPathIndex].Operations[vOperationIndex].Parameters.Count - 1 do
        begin
          vResultParam := TUnitParameter.Create;
          vResultParam.ParamName := CapitalizeFirstLetter(fSwagDoc.Paths[vPathIndex].Operations[vOperationIndex].Parameters[vParameterIndex].Name);
          vResultParam.ParamType := ConvertSwaggerTypeToDelphiType(fSwagDoc.Paths[vPathIndex].Operations[vOperationIndex].Parameters[vParameterIndex]);
          vMethod.AddParameter(vResultParam);
        end;

        for vResponse in fSwagDoc.Paths[vPathIndex].Operations[vOperationIndex].Responses do
        begin
          vSchemaObj := vResponse.Value.Schema.JsonSchema;
          if vSchemaObj = nil then
            continue;
          if vSchemaObj.TryGetValue('$ref', vRef) then
          begin
            vMethod.AddAttribute('    [MVCResponse(' + vResponse.Key + ', ' +
                                                   QuotedStr(vResponse.Value.Description) + ', ' + ConvertRefToType(vRef) + ')]');
            vResultParam := TUnitParameter.Create;
            vResultParam.ParamName := ConvertRefToVarName(vRef);
            vResultParam.ParamType := TUnitTypeDefinition.Create;
            vResultParam.ParamType.TypeName := ConvertRefToType(vRef);
            vMethod.AddLocalVariable(vResultParam);
            vMethod.Content.Add('  ' + ConvertRefToVarName(vRef) + ' := ' + ConvertRefToType(vRef) + '.Create;');
//            method.Content.Add('  Render(' + response.Key + ', ' + ConvertRefToVarName(ref) + ');');
          end
          else
          begin
            if not vSchemaObj.TryGetValue('properties', vSchemaObj) then
              continue;
            if not vSchemaObj.TryGetValue('employees', vSchemaObj) then
              continue;
            if not vSchemaObj.TryGetValue('items', vSchemaObj) then
              continue;
            if vSchemaObj.TryGetValue('$ref', vRef) then
            begin
              vMethod.AddAttribute('    [MVCResponseList(' + vResponse.Key + ', ' +
                                                     QuotedStr(vResponse.Value.Description) + ', ' + ConvertRefToType(vRef) + ')]');
              vResultParam := TUnitParameter.Create;
              vResultParam.ParamName := ConvertRefToVarName(vRef);
              vResultParam.ParamType := TUnitTypeDefinition.Create;
              vResultParam.ParamType.TypeName := 'TObjectList<' + ConvertRefToType(vRef) + '>';
              vMethod.AddLocalVariable(vResultParam);
              vDelphiUnit.AddInterfaceUnit('Generics.Collections');
              vMethod.Content.Add('  ' + ConvertRefToVarName(vRef) + ' := TObjectList<' + ConvertRefToType(vRef) + '>.Create;');
            end;
          end;
        end;

        vMVCControllerClient.Methods.Add(vMethod);
      end;
    end;

    vDelphiUnit.SortTypeDefinitions;

    Result := GenerateUnitText(vDelphiUnit);
  finally
    vDelphiUnit.Free;
  end;
end;

procedure TSwagDocToDelphiRESTClientBuilder.HandleArray(pField : TUnitFieldDefinition; pJson: TJSONPair);
var
  vJsonObj: TJSONObject;
  vJsonVal: TJSONValue;
  vType: string;
begin
  if Assigned(((pJson.JsonValue as TJSONObject).Values['items'] as TJSONObject).Values['type']) then
  begin
    vType := ((pJson.JsonValue as TJSONObject).Values['items'] as TJSONObject).Values['type'].Value;
    if vType.ToLower <> 'string' then
      vType := 'T' + vType;
    pField.FieldType := 'array of ' + vType;
  end
  else
  begin
    OutputDebugString(PChar(pJson.ToJSON));
    vJsonVal := (pJson.JsonValue as TJSONObject).Values['items'] as TJSONObject;
    OutputDebugString(PChar(vJsonVal.ToJSON));
    vJsonObj := vJsonVal as TJSONObject;
    vJsonVal := vJsonObj.Values['$ref'];
    OutputDebugString(PChar(vJsonVal.Value));
    pField.FieldType := 'array of ' + ConvertRefToType(vJsonVal.value);
  end;
end;


procedure TSwagDocToDelphiRESTClientBuilder.ChildType(pDelphiUnit : TDelphiUnit; pJson: TJSONPair);
var
  vTypeInfo: TUnitTypeDefinition;
  vJsonProps: TJSONObject;
  vFieldInfo: TUnitFieldDefinition;
  vTypeObj: TJSONObject;
  vJsonPropIndex: Integer;
  vValue : string;
begin
  OutputDebugString(PChar('Child: ' + pJson.ToJSON));
  vTypeInfo := TUnitTypeDefinition.Create;
  vTypeInfo.TypeName := 'T' + CapitalizeFirstLetter(pJson.JSONString.Value);

  vJsonProps := (pJson.JSONValue as TJSONObject).Values['properties'] as TJSONObject;
  for vJsonPropIndex := 0 to vJsonProps.Count - 1 do
  begin
    OutputDebugString(PChar(vJsonProps.Pairs[vJsonPropIndex].ToJSON));
    vFieldInfo := TUnitFieldDefinition.Create;
    vFieldInfo.FieldName := vJsonProps.Pairs[vJsonPropIndex].JsonString.Value;
    vTypeObj := vJsonProps.Pairs[vJsonPropIndex].JsonValue as TJSONObject;
    vFieldInfo.FieldType := vTypeObj.Values['type'].Value;
    if vFieldInfo.FieldType = 'number' then
      vFieldInfo.FieldType := 'Double'
    else if vFieldInfo.FieldType = 'object' then
    begin
      vFieldInfo.FieldType := 'T' + CapitalizeFirstLetter(vJsonProps.Pairs[vJsonPropIndex].JsonString.Value);
      ChildType(pDelphiUnit, vJsonProps.Pairs[vJsonPropIndex]);
    end;
    if vTypeObj.TryGetValue('description', vValue) then
      vFieldInfo.AddAttribute('[MVCDoc(' + QuotedStr(vValue) + ')]');

    if vTypeObj.TryGetValue('format', vValue) then
    begin
      if (vFieldInfo.FieldType.ToLower = 'integer') and (vValue.ToLower = 'int64') then
        vFieldInfo.FieldType := 'Int64';
      vFieldInfo.AddAttribute('[MVCFormat(' + QuotedStr(vValue) + ')]');
    end;
    if vTypeObj.TryGetValue('maxLength', vValue) then
      vFieldInfo.AddAttribute('[MVCMaxLength(' + vValue + ')]');
    vTypeInfo.Fields.Add(vFieldInfo);
  end;
  pDelphiUnit.AddType(vTypeInfo);
end;

procedure TSwagDocToDelphiRESTClientBuilder.ConvertSwaggerDefinitionsToTypeDefinitions(pDelphiUnit: TDelphiUnit);
var
  vTypeInfo: TUnitTypeDefinition;
  vJsonProps: TJSONObject;
  vFieldInfo: TUnitFieldDefinition;
  vTypeObj: TJSONObject;
  DefinitionIndex: Integer;
  vJsonPropIndex: Integer;
  vValue : string;
begin
  for DefinitionIndex := 0 to fSwagDoc.Definitions.Count - 1 do
  begin
    vTypeInfo := TUnitTypeDefinition.Create;
    vTypeInfo.TypeName := 'T' + CapitalizeFirstLetter(fSwagDoc.Definitions[DefinitionIndex].Name);
    vJsonProps := fSwagDoc.Definitions[DefinitionIndex].JsonSchema.Values['properties'] as TJSONObject;
    for vJsonPropIndex := 0 to vJsonProps.Count - 1 do
    begin
      OutputDebugString(PChar(vJsonProps.Pairs[vJsonPropIndex].ToJSON));
      vFieldInfo := TUnitFieldDefinition.Create;
      vFieldInfo.FieldName := vJsonProps.Pairs[vJsonPropIndex].JsonString.Value;
      vTypeObj := vJsonProps.Pairs[vJsonPropIndex].JsonValue as TJSONObject;
      if Assigned(vTypeObj.Values['type']) then
        vFieldInfo.FieldType := vTypeObj.Values['type'].Value
      else
        vFieldInfo.FieldType := ConvertRefToType(vTypeObj.Values['$ref'].Value);

      if vFieldInfo.FieldType = 'number' then
        vFieldInfo.FieldType := 'Double'
      else if vFieldInfo.FieldType = 'object' then
      begin
        vFieldInfo.FieldType := 'T' + CapitalizeFirstLetter(vJsonProps.Pairs[vJsonPropIndex].JsonString.Value);
        ChildType(pDelphiUnit, vJsonProps.Pairs[vJsonPropIndex]);
      end
      else if vFieldInfo.FieldType = 'array' then
      begin
        HandleArray(vFieldInfo, vJsonProps.Pairs[vJsonPropIndex]);
      end;
      if vTypeObj.TryGetValue('description', vValue) then
      begin
        if vValue.Trim.Length > 0 then
          vFieldInfo.AddAttribute('[MVCDoc(' + QuotedStr(vValue) + ')]');
      end;
      if vTypeObj.TryGetValue('format', vValue) then
      begin
        if (vFieldInfo.FieldType.ToLower = 'integer') and (vValue.ToLower = 'int64') then
          vFieldInfo.FieldType := 'Int64';
        vFieldInfo.AddAttribute('[MVCFormat(' + QuotedStr(vValue) + ')]');
      end;
      if vTypeObj.TryGetValue('maxLength', vValue) then
        vFieldInfo.AddAttribute('[MVCMaxLength(' + vValue + ')]');
      if vTypeObj.TryGetValue('minimum', vValue) then
        vFieldInfo.AddAttribute('[MVCMinimum(' + vValue + ')]');
      if vTypeObj.TryGetValue('maximum', vValue) then
        vFieldInfo.AddAttribute('[MVCMaximum(' + vValue + ')]');
      vTypeInfo.Fields.Add(vFieldInfo);
    end;
    pDelphiUnit.AddType(vTypeInfo);
  end;
end;

function TSwagDocToDelphiRESTClientBuilder.ConvertSwaggerTypeToDelphiType(pSwaggerType: TSwagRequestParameter): TUnitTypeDefinition;
var
  vSwaggerType: TSwagTypeParameter;
  vJson: TJSONObject;
begin
  Result := TUnitTypeDefinition.Create;
  vSwaggerType := pSwaggerType.TypeParameter;
  case vSwaggerType of
    stpNotDefined:
    begin
      if Assigned(pSwaggerType.Schema.JsonSchema.Values['$ref']) then
        Result.TypeName := ConvertRefToType(pSwaggerType.Schema.JsonSchema.Values['$ref'].Value)
      else
      begin
        Result.TypeName := pSwaggerType.Schema.JsonSchema.Values['type'].Value;
        if Result.TypeName = 'array' then
        begin
          if Assigned(pSwaggerType.Schema.JsonSchema.Values['items']) then
            if Assigned((pSwaggerType.Schema.JsonSchema.Values['items'] as TJSONObject).Values['$ref']) then
              Result.TypeName := 'array of ' + ConvertRefToType((pSwaggerType.Schema.JsonSchema.Values['items'] as TJSONObject).Values['$ref'].Value);
        end;
      end;
    end;
    stpString: Result.TypeName := 'String';
    stpNumber: Result.TypeName := 'Double';
    stpInteger: Result.TypeName := 'Integer';
    stpBoolean: Result.TypeName := 'Boolean';
    stpArray:
    begin
      vJson := pSwaggerType.Schema.JsonSchema;
      if Assigned(vJson) then
      begin
        OutputDebugString(PChar('TYPE: ' + vJson.ToJson));
        Result.TypeName := 'array of ' + pSwaggerType.Schema.JsonSchema.Values['type'].Value;
      end
      else
      begin
        if Assigned(pSwaggerType.Items.Values['type']) then
        begin
          Result.TypeName := 'array of ' + pSwaggerType.Items.Values['type'].Value;
        end
        else
          Result.TypeName := 'array of ';
      end;
    end;
    stpFile: Result.TypeName := 'err File';
  end;
end;

function TSwagDocToDelphiRESTClientBuilder.GenerateUnitText(pDelphiUnit: TDelphiUnit): string;
begin
  pDelphiUnit.Title := fSwagDoc.Info.Title;
  pDelphiUnit.Description := fSwagDoc.Info.Description;
  pDelphiUnit.License := fSwagDoc.Info.License.Name;
  Result := pDelphiUnit.Generate;
end;

end.
