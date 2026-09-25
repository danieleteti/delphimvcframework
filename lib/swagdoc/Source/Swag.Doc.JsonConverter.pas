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

unit Swag.Doc.JsonConverter;

interface

uses
  System.JSON,
  Swag.Common.Types;

type
  /// <summary>
  /// Rewrites a JSON document, or any part of it, to the conventions of the target specification family.
  /// The reference prefixes and the schema keywords that differ between Swagger 2.0 and OpenAPI 3 are converted
  /// without any knowledge of the object model, so the schemas written for one family are accepted by the other one:
  /// * the nullable keyword and the x-nullable extension become a type that also accepts null in OpenAPI 3,
  ///   as required by JSON Schema 2020-12, and the null type becomes the x-nullable extension in Swagger 2.0,
  ///   where a nullable reference is wrapped into allOf because the siblings of a reference are ignored;
  /// * the boolean exclusiveMinimum and exclusiveMaximum keywords become numeric limits in OpenAPI 3;
  /// * the file type becomes a binary string in OpenAPI 3;
  /// * the examples array of a schema becomes the example keyword in Swagger 2.0.
  /// Example values, default values, enumerations and constants are data and are never rewritten.
  /// Documents written for OpenAPI 3.0 are converted to the current OpenAPI 3 release by the same rules.
  /// </summary>
  TSwagJsonConverter = class(TObject)
  strict private
    class function IsNameMap(const pKey: string): Boolean;
    class function IsInstanceData(const pKey: string): Boolean;
    class function ArrayContainsNull(pJson: TJSONArray): Boolean;
    class function ArrayContainsString(pJson: TJSONArray; const pValue: string): Boolean;
    class procedure ReplacePair(pJson: TJSONObject; const pName: string; pValue: TJSONValue);
    class procedure ReplaceRefPrefix(pJson: TJSONObject; const pOldPrefix, pNewPrefix: string);
    class procedure RenameBooleanPair(pJson: TJSONObject; const pOldName, pNewName: string);
    class procedure ReplaceFileType(pJson: TJSONObject);
    class procedure ConvertNullableToTypeArray(pJson: TJSONObject);
    class procedure ConvertTypeArrayToNullable(pJson: TJSONObject; const pWriteNullableExtension: Boolean);
    class procedure ConvertNullableAnyOfToNullable(pJson: TJSONObject);
    class procedure WrapNullableRefIntoAllOf(pJson: TJSONObject);
    class procedure ConvertExclusiveLimitToNumber(pJson: TJSONObject; const pLimitName, pExclusiveName: string);
    class procedure ConvertExclusiveLimitToBoolean(pJson: TJSONObject; const pLimitName, pExclusiveName: string);
    class procedure ConvertExamplesToExample(pJson: TJSONObject);
    class procedure ConvertObjectToSwagger2(pJson: TJSONObject; const pWriteNullableExtension: Boolean);
    class procedure ConvertObjectToOpenApi3(pJson: TJSONObject);
    class procedure ConvertValue(pJson: TJSONValue; const pVersion: TSwagVersion; const pIsNameMap: Boolean;
      const pWriteNullableExtension: Boolean);
  public
    /// <summary>
    /// Converts, in place, the references and the schema keywords of the given JSON value to the target family.
    /// A nullable schema loses its nullability in a Swagger 2.0 document, which has no keyword for it.
    /// </summary>
    class procedure Convert(pJson: TJSONValue; const pVersion: TSwagVersion); overload;

    /// <summary>
    /// Converts, in place, the references and the schema keywords of the given JSON value to the target family.
    /// When pWriteNullableExtension is True, a nullable schema is written with the x-nullable extension in a
    /// Swagger 2.0 document. The x-nullable extension read from a Swagger 2.0 document is always kept.
    /// </summary>
    class procedure Convert(pJson: TJSONValue; const pVersion: TSwagVersion;
      const pWriteNullableExtension: Boolean); overload;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections;

const
  c_JsonRef = '$ref';
  c_JsonType = 'type';
  c_JsonFormat = 'format';
  c_JsonEnum = 'enum';
  c_JsonAnyOf = 'anyOf';
  c_JsonAllOf = 'allOf';
  c_JsonMinimum = 'minimum';
  c_JsonMaximum = 'maximum';
  c_JsonExclusiveMinimum = 'exclusiveMinimum';
  c_JsonExclusiveMaximum = 'exclusiveMaximum';
  c_JsonExample = 'example';
  c_JsonExamples = 'examples';
  c_JsonNullable = 'nullable';
  c_JsonNullableExtension = 'x-nullable';
  c_JsonExtensionPrefix = 'x-';
  c_JsonTypeFile = 'file';
  c_JsonTypeString = 'string';
  c_JsonTypeNull = 'null';
  c_JsonFormatBinary = 'binary';
  c_Swagger2RefPrefixes: array[0..2] of string = ('#/definitions/', '#/parameters/', '#/responses/');
  c_OpenApi3RefPrefixes: array[0..2] of string =
    ('#/components/schemas/', '#/components/parameters/', '#/components/responses/');
  c_OpenApi3RequestBodiesRefPrefix = '#/components/requestBodies/';
  c_Swagger2RequestBodiesRefPrefix = '#/parameters/';
  c_JsonNameMaps: array[0..22] of string = ('properties', 'patternProperties', 'dependentSchemas', '$defs',
    'definitions', 'schemas', 'responses', 'parameters', 'requestBodies', 'headers', 'securitySchemes',
    'securityDefinitions', 'paths', 'webhooks', 'callbacks', 'links', 'pathItems', 'mediaTypes', 'content',
    'encoding', 'variables', 'mapping', 'scopes');
  c_JsonInstanceData: array[0..4] of string = ('example', 'examples', 'default', 'enum', 'const');

{ TSwagJsonConverter }

class procedure TSwagJsonConverter.Convert(pJson: TJSONValue; const pVersion: TSwagVersion);
begin
  Convert(pJson, pVersion, False);
end;

class procedure TSwagJsonConverter.Convert(pJson: TJSONValue; const pVersion: TSwagVersion;
  const pWriteNullableExtension: Boolean);
begin
  ConvertValue(pJson, pVersion, False, pWriteNullableExtension);
end;

class procedure TSwagJsonConverter.ConvertValue(pJson: TJSONValue; const pVersion: TSwagVersion;
  const pIsNameMap: Boolean; const pWriteNullableExtension: Boolean);
var
  vObject: TJSONObject;
  vIndex: Integer;
  vKey: string;
begin
  if pJson is TJSONArray then
  begin
    for vIndex := 0 to TJSONArray(pJson).Count - 1 do
      ConvertValue(TJSONArray(pJson).Items[vIndex], pVersion, False, pWriteNullableExtension);
    Exit;
  end;

  if not (pJson is TJSONObject) then
    Exit;

  vObject := TJSONObject(pJson);
  if not pIsNameMap then
    case pVersion of
      svSwagger2: ConvertObjectToSwagger2(vObject, pWriteNullableExtension);
      svOpenApi3: ConvertObjectToOpenApi3(vObject);
    end;

  for vIndex := 0 to vObject.Count - 1 do
  begin
    vKey := vObject.Pairs[vIndex].JsonString.Value;
    if pIsNameMap then
      ConvertValue(vObject.Pairs[vIndex].JsonValue, pVersion, False, pWriteNullableExtension)
    else if not (IsInstanceData(vKey) or vKey.StartsWith(c_JsonExtensionPrefix)) then
      ConvertValue(vObject.Pairs[vIndex].JsonValue, pVersion, IsNameMap(vKey), pWriteNullableExtension);
  end;
end;

class procedure TSwagJsonConverter.ConvertObjectToSwagger2(pJson: TJSONObject;
  const pWriteNullableExtension: Boolean);
var
  vIndex: Integer;
begin
  ConvertNullableAnyOfToNullable(pJson);
  for vIndex := Low(c_OpenApi3RefPrefixes) to High(c_OpenApi3RefPrefixes) do
    ReplaceRefPrefix(pJson, c_OpenApi3RefPrefixes[vIndex], c_Swagger2RefPrefixes[vIndex]);
  ReplaceRefPrefix(pJson, c_OpenApi3RequestBodiesRefPrefix, c_Swagger2RequestBodiesRefPrefix);
  if pWriteNullableExtension then
    RenameBooleanPair(pJson, c_JsonNullable, c_JsonNullableExtension)
  else
    pJson.RemovePair(c_JsonNullable).Free;
  ConvertTypeArrayToNullable(pJson, pWriteNullableExtension);
  WrapNullableRefIntoAllOf(pJson);
  ConvertExclusiveLimitToBoolean(pJson, c_JsonMinimum, c_JsonExclusiveMinimum);
  ConvertExclusiveLimitToBoolean(pJson, c_JsonMaximum, c_JsonExclusiveMaximum);
  ConvertExamplesToExample(pJson);
end;

class procedure TSwagJsonConverter.ConvertObjectToOpenApi3(pJson: TJSONObject);
var
  vIndex: Integer;
begin
  for vIndex := Low(c_Swagger2RefPrefixes) to High(c_Swagger2RefPrefixes) do
    ReplaceRefPrefix(pJson, c_Swagger2RefPrefixes[vIndex], c_OpenApi3RefPrefixes[vIndex]);
  RenameBooleanPair(pJson, c_JsonNullableExtension, c_JsonNullable);
  ReplaceFileType(pJson);
  ConvertExclusiveLimitToNumber(pJson, c_JsonMinimum, c_JsonExclusiveMinimum);
  ConvertExclusiveLimitToNumber(pJson, c_JsonMaximum, c_JsonExclusiveMaximum);
  ConvertNullableToTypeArray(pJson);
end;

class function TSwagJsonConverter.IsNameMap(const pKey: string): Boolean;
var
  vIndex: Integer;
begin
  Result := False;
  for vIndex := Low(c_JsonNameMaps) to High(c_JsonNameMaps) do
    if pKey = c_JsonNameMaps[vIndex] then
      Exit(True);
end;

class function TSwagJsonConverter.IsInstanceData(const pKey: string): Boolean;
var
  vIndex: Integer;
begin
  Result := False;
  for vIndex := Low(c_JsonInstanceData) to High(c_JsonInstanceData) do
    if pKey = c_JsonInstanceData[vIndex] then
      Exit(True);
end;

class function TSwagJsonConverter.ArrayContainsNull(pJson: TJSONArray): Boolean;
var
  vIndex: Integer;
begin
  Result := False;
  for vIndex := 0 to pJson.Count - 1 do
    if pJson.Items[vIndex] is TJSONNull then
      Exit(True);
end;

class function TSwagJsonConverter.ArrayContainsString(pJson: TJSONArray; const pValue: string): Boolean;
var
  vIndex: Integer;
begin
  Result := False;
  for vIndex := 0 to pJson.Count - 1 do
    if (pJson.Items[vIndex] is TJSONString) and (pJson.Items[vIndex].Value = pValue) then
      Exit(True);
end;

class procedure TSwagJsonConverter.ReplacePair(pJson: TJSONObject; const pName: string; pValue: TJSONValue);
begin
  pJson.RemovePair(pName).Free;
  pJson.AddPair(pName, pValue);
end;

class procedure TSwagJsonConverter.ReplaceRefPrefix(pJson: TJSONObject; const pOldPrefix, pNewPrefix: string);
var
  vRef: string;
begin
  if not (pJson.Values[c_JsonRef] is TJSONString) then
    Exit;

  vRef := pJson.Values[c_JsonRef].Value;
  if not vRef.StartsWith(pOldPrefix) then
    Exit;

  ReplacePair(pJson, c_JsonRef, TJSONString.Create(pNewPrefix + vRef.Substring(pOldPrefix.Length)));
end;

class procedure TSwagJsonConverter.RenameBooleanPair(pJson: TJSONObject; const pOldName, pNewName: string);
var
  vValue: Boolean;
begin
  if not (pJson.Values[pOldName] is TJSONBool) then
    Exit;

  vValue := TJSONBool(pJson.Values[pOldName]).AsBoolean;
  pJson.RemovePair(pOldName).Free;
  ReplacePair(pJson, pNewName, TJSONBool.Create(vValue));
end;

class procedure TSwagJsonConverter.ReplaceFileType(pJson: TJSONObject);
begin
  if not (pJson.Values[c_JsonType] is TJSONString) then
    Exit;
  if pJson.Values[c_JsonType].Value <> c_JsonTypeFile then
    Exit;

  ReplacePair(pJson, c_JsonType, TJSONString.Create(c_JsonTypeString));
  if not Assigned(pJson.Values[c_JsonFormat]) then
    pJson.AddPair(c_JsonFormat, c_JsonFormatBinary);
end;

class procedure TSwagJsonConverter.ConvertNullableToTypeArray(pJson: TJSONObject);
var
  vNullable: Boolean;
  vTypes: TJSONArray;
  vRefSchema: TJSONObject;
  vNullSchema: TJSONObject;
  vAnyOf: TJSONArray;
begin
  if not (pJson.Values[c_JsonNullable] is TJSONBool) then
    Exit;

  vNullable := TJSONBool(pJson.Values[c_JsonNullable]).AsBoolean;
  pJson.RemovePair(c_JsonNullable).Free;
  if not vNullable then
    Exit;

  if pJson.Values[c_JsonType] is TJSONString then
  begin
    vTypes := TJSONArray.Create;
    vTypes.Add(pJson.Values[c_JsonType].Value);
    vTypes.Add(c_JsonTypeNull);
    ReplacePair(pJson, c_JsonType, vTypes);
  end
  else if pJson.Values[c_JsonType] is TJSONArray then
  begin
    vTypes := TJSONArray(pJson.Values[c_JsonType]);
    if not ArrayContainsString(vTypes, c_JsonTypeNull) then
      vTypes.Add(c_JsonTypeNull);
  end
  else if (pJson.Values[c_JsonRef] is TJSONString) and not Assigned(pJson.Values[c_JsonAnyOf]) then
  begin
    vRefSchema := TJSONObject.Create;
    vRefSchema.AddPair(c_JsonRef, pJson.Values[c_JsonRef].Value);
    vNullSchema := TJSONObject.Create;
    vNullSchema.AddPair(c_JsonType, c_JsonTypeNull);
    vAnyOf := TJSONArray.Create;
    vAnyOf.AddElement(vRefSchema);
    vAnyOf.AddElement(vNullSchema);
    pJson.RemovePair(c_JsonRef).Free;
    pJson.AddPair(c_JsonAnyOf, vAnyOf);
  end
  else if (pJson.Values[c_JsonAllOf] is TJSONArray) and (TJSONArray(pJson.Values[c_JsonAllOf]).Count = 1) and
    (TJSONArray(pJson.Values[c_JsonAllOf]).Items[0] is TJSONObject) and
    (TJSONObject(TJSONArray(pJson.Values[c_JsonAllOf]).Items[0]).Count = 1) and
    (TJSONObject(TJSONArray(pJson.Values[c_JsonAllOf]).Items[0]).Values[c_JsonRef] is TJSONString) and
    not Assigned(pJson.Values[c_JsonAnyOf]) then
  begin
    vRefSchema := TJSONObject.Create;
    vRefSchema.AddPair(c_JsonRef, TJSONObject(TJSONArray(pJson.Values[c_JsonAllOf]).Items[0]).Values[c_JsonRef].Value);
    vNullSchema := TJSONObject.Create;
    vNullSchema.AddPair(c_JsonType, c_JsonTypeNull);
    vAnyOf := TJSONArray.Create;
    vAnyOf.AddElement(vRefSchema);
    vAnyOf.AddElement(vNullSchema);
    pJson.RemovePair(c_JsonAllOf).Free;
    pJson.AddPair(c_JsonAnyOf, vAnyOf);
  end;

  if (pJson.Values[c_JsonEnum] is TJSONArray) and not ArrayContainsNull(TJSONArray(pJson.Values[c_JsonEnum])) then
    TJSONArray(pJson.Values[c_JsonEnum]).AddElement(TJSONNull.Create);
end;

class procedure TSwagJsonConverter.ConvertTypeArrayToNullable(pJson: TJSONObject;
  const pWriteNullableExtension: Boolean);
var
  vTypes: TJSONArray;
  vEnum: TJSONArray;
  vIndex: Integer;
  vType: string;
  vFirstType: string;
  vHasNull: Boolean;
begin
  if not (pJson.Values[c_JsonType] is TJSONArray) then
    Exit;

  vTypes := TJSONArray(pJson.Values[c_JsonType]);
  vHasNull := False;
  vFirstType := EmptyStr;
  for vIndex := 0 to vTypes.Count - 1 do
  begin
    vType := vTypes.Items[vIndex].Value;
    if vType = c_JsonTypeNull then
      vHasNull := True
    else if vFirstType.IsEmpty then
      vFirstType := vType;
  end;

  if vFirstType.IsEmpty then
    pJson.RemovePair(c_JsonType).Free
  else
    ReplacePair(pJson, c_JsonType, TJSONString.Create(vFirstType));

  if not vHasNull then
    Exit;

  if pWriteNullableExtension then
    ReplacePair(pJson, c_JsonNullableExtension, TJSONBool.Create(True));
  if pJson.Values[c_JsonEnum] is TJSONArray then
  begin
    vEnum := TJSONArray(pJson.Values[c_JsonEnum]);
    for vIndex := vEnum.Count - 1 downto 0 do
      if vEnum.Items[vIndex] is TJSONNull then
        vEnum.Remove(vIndex).Free;
  end;
end;

class procedure TSwagJsonConverter.ConvertNullableAnyOfToNullable(pJson: TJSONObject);
var
  vAnyOf: TJSONArray;
  vItem: TJSONValue;
  vOther: TJSONValue;
  vIndex: Integer;
  vNullCount: Integer;
  vRef: string;
begin
  if not (pJson.Values[c_JsonAnyOf] is TJSONArray) then
    Exit;

  vAnyOf := TJSONArray(pJson.Values[c_JsonAnyOf]);
  if vAnyOf.Count <> 2 then
    Exit;

  vOther := nil;
  vNullCount := 0;
  for vIndex := 0 to vAnyOf.Count - 1 do
  begin
    vItem := vAnyOf.Items[vIndex];
    if (vItem is TJSONObject) and (TJSONObject(vItem).Count = 1) and
      (TJSONObject(vItem).Values[c_JsonType] is TJSONString) and
      (TJSONObject(vItem).Values[c_JsonType].Value = c_JsonTypeNull) then
      Inc(vNullCount)
    else
      vOther := vItem;
  end;

  if (vNullCount <> 1) or not (vOther is TJSONObject) or (TJSONObject(vOther).Count <> 1) or
    not (TJSONObject(vOther).Values[c_JsonRef] is TJSONString) then
    Exit;

  vRef := TJSONObject(vOther).Values[c_JsonRef].Value;
  pJson.RemovePair(c_JsonAnyOf).Free;
  ReplacePair(pJson, c_JsonRef, TJSONString.Create(vRef));
  ReplacePair(pJson, c_JsonNullableExtension, TJSONBool.Create(True));
end;

class procedure TSwagJsonConverter.WrapNullableRefIntoAllOf(pJson: TJSONObject);
var
  vRef: string;
  vRefSchema: TJSONObject;
  vAllOf: TJSONArray;
begin
  if not (pJson.Values[c_JsonRef] is TJSONString) then
    Exit;
  if not (pJson.Values[c_JsonNullableExtension] is TJSONBool) then
    Exit;
  if Assigned(pJson.Values[c_JsonAllOf]) then
    Exit;

  vRef := pJson.Values[c_JsonRef].Value;
  pJson.RemovePair(c_JsonRef).Free;
  vRefSchema := TJSONObject.Create;
  vRefSchema.AddPair(c_JsonRef, vRef);
  vAllOf := TJSONArray.Create;
  vAllOf.AddElement(vRefSchema);
  pJson.AddPair(c_JsonAllOf, vAllOf);
end;

class procedure TSwagJsonConverter.ConvertExclusiveLimitToNumber(pJson: TJSONObject;
  const pLimitName, pExclusiveName: string);
var
  vExclusive: Boolean;
  vLimit: TJSONPair;
begin
  if not (pJson.Values[pExclusiveName] is TJSONBool) then
    Exit;

  vExclusive := TJSONBool(pJson.Values[pExclusiveName]).AsBoolean;
  pJson.RemovePair(pExclusiveName).Free;
  if (not vExclusive) or not (pJson.Values[pLimitName] is TJSONNumber) then
    Exit;

  vLimit := pJson.RemovePair(pLimitName);
  try
    pJson.AddPair(pExclusiveName, vLimit.JsonValue.Clone as TJSONValue);
  finally
    vLimit.Free;
  end;
end;

class procedure TSwagJsonConverter.ConvertExclusiveLimitToBoolean(pJson: TJSONObject;
  const pLimitName, pExclusiveName: string);
var
  vLimit: TJSONPair;
begin
  if not (pJson.Values[pExclusiveName] is TJSONNumber) then
    Exit;

  vLimit := pJson.RemovePair(pExclusiveName);
  try
    if not Assigned(pJson.Values[pLimitName]) then
    begin
      pJson.AddPair(pLimitName, vLimit.JsonValue.Clone as TJSONValue);
      pJson.AddPair(pExclusiveName, TJSONBool.Create(True));
    end;
  finally
    vLimit.Free;
  end;
end;

class procedure TSwagJsonConverter.ConvertExamplesToExample(pJson: TJSONObject);
var
  vExamples: TJSONPair;
  vValues: TJSONArray;
begin
  if not (pJson.Values[c_JsonExamples] is TJSONArray) then
    Exit;

  vExamples := pJson.RemovePair(c_JsonExamples);
  try
    vValues := TJSONArray(vExamples.JsonValue);
    if (vValues.Count > 0) and not Assigned(pJson.Values[c_JsonExample]) then
      pJson.AddPair(c_JsonExample, vValues.Items[0].Clone as TJSONValue);
  finally
    vExamples.Free;
  end;
end;

end.
