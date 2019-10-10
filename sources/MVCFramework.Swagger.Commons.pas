// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file:
//    João Antônio Duarte (https://github.com/joaoduarte19)
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// *************************************************************************** }

unit MVCFramework.Swagger.Commons;

interface

uses
  Json.Schema,
  System.Rtti,
  Swag.Common.Types,
  MVCFramework.Commons,
  Swag.Doc.Path.Operation.RequestParameter,
  Swag.Doc.Path.Operation,
  Swag.Doc.Path,
  System.JSON,
  Json.Schema.Field;

type
  TMVCSwagParamLocation = (plNotDefined, plQuery, plHeader, plPath, plFormData, plBody);
  TMVCSwagParamType = (ptNotDefined, ptString, ptNumber, ptInteger, ptBoolean, ptArray, ptFile);
  TMVCSwagSchemaType = (stUnknown, stInteger, stInt64, stNumber, stDateTime, stDate, stTime,
    stEnumeration, stBoolean, stObject, stArray, stString, stChar, stGuid);

  /// <summary>
  /// Swagger info object
  /// </summary>
  TMVCSwaggerInfo = record
    Title: string;
    Version: string;
    TermsOfService: string;
    Description: string;
    ContactName: string;
    ContactEmail: string;
    ContactUrl: string;
    LicenseName: string;
    LicenseUrl: string;
  end;

  /// <summary>
  /// Specify swagger path summary. See <see href="https://swagger.io/docs/specification/2-0/paths-and-operations/">
  /// Swagger path and operations</see>
  /// </summary>
  MVCSwagSummaryAttribute = class(TCustomAttribute)
  private
    FTags: string;
    FDeprecated: Boolean;
    FDescription: string;
    FPathId: string;
  public
    constructor Create(const ATags, ADescription: string; const APathId: string = ''; ADeprecated: Boolean = False);
    function GetTags: TArray<string>;
    property Tags: string read FTags;
    property Description: string read FDescription;
    property PathId: string read FPathId;
    property Deprecated: Boolean read FDeprecated;
  end;

  /// <summary>
  /// Specify swagger path responses.
  /// </summary>
  MVCSwagResponsesAttribute = class(TCustomAttribute)
  private
    FStatusCode: Integer;
    FDescription: string;
    FJsonSchema: string;
    FJsonSchemaClass: TClass;
  public
    constructor Create(
      const AStatusCode: Integer;
      const ADescription: string;
      const AJsonSchema: string = ''
      ); overload;
    constructor Create(
      const AStatusCode: Integer;
      const ADescription: string;
      const AJsonSchemaClass: TClass
      ); overload;

    property StatusCode: Integer read FStatusCode;
    property Description: string read FDescription;
    property JsonSchema: string read FJsonSchema;
    property JsonSchemaClass: TClass read FJsonSchemaClass;
  end;

  /// <summary>
  /// Specify swagger path params.
  /// </summary>
  MVCSwagParamAttribute = class(TCustomAttribute)
  private
    FParamLocation: TMVCSwagParamLocation;
    FParamName: string;
    FParamDescription: string;
    FParamType: TMVCSwagParamType;
    FRequired: Boolean;
    FJsonSchema: string;
    FJsonSchemaClass: TClass;
  public
    constructor Create(
      const AParamLocation: TMVCSwagParamLocation;
      const AParamName: string;
      const AParamDescription: string;
      const AParamType: TMVCSwagParamType;
      const ARequired: Boolean = True;
      const AJsonSchema: string = ''); overload;
    constructor Create(
      const AParamLocation: TMVCSwagParamLocation;
      const AParamName: string;
      const AParamDescription: string;
      const AJsonSchemaClass: TClass;
      const AParamType: TMVCSwagParamType = ptNotDefined;
      const ARequired: Boolean = True); overload;

    property ParamLocation: TMVCSwagParamLocation read FParamLocation;
    property ParamName: string read FParamName;
    property ParamDescription: string read FParamDescription;
    property ParamType: TMVCSwagParamType read FParamType;
    property Required: Boolean read FRequired;
    property JsonSchema: string read FJsonSchema;
    property JsonSchemaClass: TClass read FJsonSchemaClass;
  end;

  /// <summary>
  /// Specifies the field definition in the json schema.
  /// Use this attribute on a class property that will be mapped to a json schema
  /// </summary>
  MVCSwagJsonSchemaFieldAttribute = class(TCustomAttribute)
  private
    FSchemaFieldType: TMVCSwagSchemaType;
    FFieldName: string;
    FDescription: string;
    FRequired: Boolean;
    FNullable: Boolean;
  public
    constructor Create(
      const ASchemaFieldType: TMVCSwagSchemaType;
      const AFieldName: string;
      const ADescription: string;
      const ARequired: Boolean;
      const ANullable: Boolean); overload;
    constructor Create(
      const AFieldName: string;
      const ADescription: string;
      const ARequired: Boolean;
      const ANullable: Boolean); overload;

    property SchemaFieldType: TMVCSwagSchemaType read FSchemaFieldType;
    property FieldName: string read FFieldName;
    property Description: string read FDescription;
    property Required: Boolean read FRequired;
    property Nullable: Boolean read FNullable;
  end;

  /// <summary>
  /// SwaggerDoc Methods
  /// </summary>
  TMVCSwagger = class sealed
  private
    class var FRttiContext: TRttiContext;
    class function GetMVCSwagParamsFromMethod(const AMethod: TRttiMethod): TArray<MVCSwagParamAttribute>;
    class function MVCParamLocationToSwagRequestParamInLocation(const AMVCSwagParamLocation: TMVCSwagParamLocation):
      TSwagRequestParameterInLocation;
    class function MVCParamTypeToSwagTypeParameter(const AMVSwagParamType: TMVCSwagParamType): TSwagTypeParameter;
    class function ExtractJsonSchemaFromClass(const AClass: TClass): TJSONObject;
    class function GetJsonFieldClass(const ASchemaFieldType: TMVCSwagSchemaType): TJsonFieldClass;
    class function TypeKindToMVCSwagSchemaType(APropType: TRttiType): TMVCSwagSchemaType; static;
  public
    class constructor Create;
    class destructor Destroy;
    class function MVCHttpMethodToSwagPathOperation(const AMVCHTTPMethod: TMVCHTTPMethodType): TSwagPathTypeOperation;
    class function MVCPathToSwagPath(const AResourcePath: string): string;
    class function GetParamsFromMethod(const AResourcePath: string; const AMethod: TRttiMethod):
      TArray<TSwagRequestParameter>;
    class function RttiTypeToSwagType(const ARttiType: TRttiType): TSwagTypeParameter;
    class procedure FillOperationSummary(const ASwagPathOperation: TSwagPathOperation; const AMethod: TRttiMethod);
    class function MethodRequiresAuthentication(const AMethod: TRttiMethod; const AType: TRttiType): Boolean;
    class function GetJWTAuthenticationPath(const AJWTUrlSegment: string): TSwagPath;
  end;

const
  SECURITY_BEARER_NAME = 'bearer';
  SECURITY_BASIC_NAME = 'basic';
  JWT_JSON_SCHEMA =
    '{' + sLineBreak +
    '	 "type": "object",' + sLineBreak +
    '	 "properties": {' + sLineBreak +
    '		 "token": {' + sLineBreak +
    '			 "type": "string",' + sLineBreak +
    '			 "description": "JWT Token"' + sLineBreak +
    '		 }' + sLineBreak +
    '	 }' + sLineBreak +
    '}';
  JWT_DEFAULT_DESCRIPTION = 'For accessing the API a valid JWT token must be passed in all the queries ' +
    'in the ''Authorization'' header.' + sLineBreak + sLineBreak +
    'A valid JWT token is generated by the API and retourned as answer of a call ' + 'to the route defined '+
    'in the JWT middleware giving a valid username and password.' + sLineBreak + sLineBreak +
    'The following syntax must be used in the ''Authorization'' header :' + sLineBreak + sLineBreak +
    '    Bearer xxxxxx.yyyyyyy.zzzzzz' + sLineBreak;

implementation

uses
  System.RegularExpressions,
  System.SysUtils,
  MVCFramework,
  Swag.Doc.Path.Operation.Response,
  System.Classes,
  System.TypInfo,
  MVCFramework.Serializer.Commons,
  MVCFramework.Middleware.Authentication.RoleBasedAuthHandler,
  Json.Schema.Field.Numbers,
  Json.Schema.Field.Strings,
  Json.Schema.Field.Arrays,
  Json.Schema.Field.DateTimes,
  Json.Schema.Field.Enums,
  Json.Schema.Field.Booleans;

{ TSwaggerUtils }

class constructor TMVCSwagger.Create;
begin
  FRttiContext := TRttiContext.Create;
end;

class function TMVCSwagger.RttiTypeToSwagType(const ARttiType: TRttiType): TSwagTypeParameter;
begin
  case ARttiType.TypeKind of
    tkInteger, tkInt64:
      Result := stpInteger;
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
      Result := stpString;
    tkFloat:
      if (ARttiType.Handle = TypeInfo(TDateTime)) or
        (ARttiType.Handle = TypeInfo(TDate)) or
        (ARttiType.Handle = TypeInfo(TTime)) then
        Result := stpString
      else
        Result := stpNumber;
    tkEnumeration:
      if ARttiType.Handle = TypeInfo(Boolean) then
        Result := stpBoolean
      else
        Result := stpArray;
  else
    Result := stpNotDefined;
  end;
end;

class destructor TMVCSwagger.Destroy;
begin
  FRttiContext.Free;
end;

type
  TFieldSchemaDefinition = record
    SchemaFieldType: TMVCSwagSchemaType;
    FieldName: string;
    Description: string;
    Required: Boolean;
    Nullable: Boolean;
    class function Create: TFieldSchemaDefinition; static; inline;
  end;

class function TMVCSwagger.ExtractJsonSchemaFromClass(const AClass: TClass): TJSONObject;
var
  LObjType: TRttiType;
  LProp: TRttiProperty;
  LJsonSchema: TJsonSchema;
  LAttr: TCustomAttribute;
  LJSFieldAttr: MVCSwagJsonSchemaFieldAttribute;
  LFieldSchemaDef: TFieldSchemaDefinition;
  LJsonField: TJsonField;
  LSkipProp: Boolean;
  LJsonFieldClass: TJsonFieldClass;
begin
  LObjType := FRttiContext.GetType(AClass.ClassInfo);
  LJsonSchema := TJsonSchema.Create;
  try
    LFieldSchemaDef := TFieldSchemaDefinition.Create;

    for LProp in LObjType.GetProperties do
    begin
      LSkipProp := False;
      LFieldSchemaDef := TFieldSchemaDefinition.Create;

      for LAttr in LProp.GetAttributes do
      begin
        if LAttr is MVCDoNotSerializeAttribute then
        begin
          LSkipProp := True;
          Break;
        end;

        if LAttr is MVCSwagJsonSchemaFieldAttribute then
        begin
          LJSFieldAttr := MVCSwagJsonSchemaFieldAttribute(LAttr);
          LFieldSchemaDef.SchemaFieldType := LJSFieldAttr.SchemaFieldType;
          LFieldSchemaDef.FieldName := LJSFieldAttr.FieldName;
          LFieldSchemaDef.Description := LJSFieldAttr.Description;
          LFieldSchemaDef.Required := LJSFieldAttr.Required;
          LFieldSchemaDef.Nullable := LJSFieldAttr.Nullable;
          Break;
        end;
      end;

      if LSkipProp then
        Continue;

      if LFieldSchemaDef.SchemaFieldType = stUnknown then
      begin
        LFieldSchemaDef.SchemaFieldType := TypeKindToMVCSwagSchemaType(LProp.PropertyType);
        LFieldSchemaDef.FieldName := TMVCSerializerHelper.GetKeyName(LProp, LObjType);
      end;

      LJsonFieldClass := GetJsonFieldClass(LFieldSchemaDef.SchemaFieldType);
      if not Assigned(LJsonFieldClass) then
        Continue;

      LJsonField := LJsonFieldClass.Create;

      if not Assigned(LJsonField) then
        Continue;

      LJsonField.Name := LFieldSchemaDef.FieldName;
      LJsonField.Required := LFieldSchemaDef.Required;
      LJsonField.Nullable := LFieldSchemaDef.Nullable;
      if not LFieldSchemaDef.Description.IsEmpty then
        TJsonFieldInteger(LJsonField).Description := LFieldSchemaDef.Description;

      LJsonSchema.Root.AddField(LJsonField);
    end;
    Result := LJsonSchema.ToJson;
  finally
    LJsonSchema.Free;
  end;
end;

class procedure TMVCSwagger.FillOperationSummary(const ASwagPathOperation: TSwagPathOperation;
  const AMethod: TRttiMethod);
var
  LAttr: TCustomAttribute;
  LSwagResponse: TSwagResponse;
  LSwagResponsesAttr: MVCSwagResponsesAttribute;
begin
  for LAttr in AMethod.GetAttributes do
  begin
    if LAttr is MVCSwagSummaryAttribute then
    begin
      ASwagPathOperation.Tags.AddRange(MVCSwagSummaryAttribute(LAttr).GetTags);
      ASwagPathOperation.Description := MVCSwagSummaryAttribute(LAttr).Description;
      ASwagPathOperation.OperationId := MVCSwagSummaryAttribute(LAttr).PathId;
      ASwagPathOperation.Deprecated := MVCSwagSummaryAttribute(LAttr).Deprecated;
    end;
    if LAttr is MVCConsumesAttribute then
    begin
      ASwagPathOperation.Consumes.Add(MVCConsumesAttribute(LAttr).Value)
    end;
    if LAttr is MVCProducesAttribute then
    begin
      ASwagPathOperation.Produces.Add(MVCProducesAttribute(LAttr).Value)
    end;
    if LAttr is MVCSwagResponsesAttribute then
    begin
      LSwagResponsesAttr := MVCSwagResponsesAttribute(LAttr);

      LSwagResponse := TSwagResponse.Create;
      LSwagResponse.StatusCode := LSwagResponsesAttr.StatusCode.ToString;
      LSwagResponse.Description := LSwagResponsesAttr.Description;
      if not LSwagResponsesAttr.JsonSchema.IsEmpty then
        LSwagResponse.Schema.JsonSchema := TJSONObject.ParseJSONValue(LSwagResponsesAttr.JsonSchema) as TJSONObject
      else if Assigned(LSwagResponsesAttr.JsonSchemaClass) then
        LSwagResponse.Schema.JsonSchema := ExtractJsonSchemaFromClass(LSwagResponsesAttr.JsonSchemaClass);

      ASwagPathOperation.Responses.Add(LSwagResponse.StatusCode, LSwagResponse);
    end;
  end;

  if ASwagPathOperation.Tags.Count = 0 then
    ASwagPathOperation.Tags.Add(AMethod.Parent.QualifiedName);

  if ASwagPathOperation.Produces.Count <= 0 then
    ASwagPathOperation.Produces.Add(TMVCMediaType.APPLICATION_JSON);

  if ASwagPathOperation.Responses.Count <= 0 then
  begin
    LSwagResponse := TSwagResponse.Create;
    LSwagResponse.StatusCode := HTTP_STATUS.OK.ToString;
    LSwagResponse.Description := 'Ok';
    ASwagPathOperation.Responses.Add(LSwagResponse.StatusCode, LSwagResponse);

    LSwagResponse := TSwagResponse.Create;
    LSwagResponse.StatusCode := HTTP_STATUS.InternalServerError.ToString;
    LSwagResponse.Description := 'Internal server error';
    ASwagPathOperation.Responses.Add(LSwagResponse.StatusCode, LSwagResponse);
  end;

end;

class function TMVCSwagger.GetJsonFieldClass(const ASchemaFieldType: TMVCSwagSchemaType): TJsonFieldClass;
begin
  case ASchemaFieldType of
    stInteger:
      Result := TJsonFieldInteger;
    stInt64:
      Result := TJsonFieldInt64;
    stNumber:
      Result := TJsonFieldNumber;
    stDateTime:
      Result := TJsonFieldDateTime;
    stDate:
      Result := TJsonFieldDate;
    stTime:
      Result := TJsonFieldTime;
    stEnumeration:
      Result := TJsonFieldEnum;
    stBoolean:
      Result := TJsonFieldBoolean;
    stArray:
      Result := TJsonFieldArray;
    stString, stChar:
      Result := TJsonFieldString;
    stGuid:
      Result := TJsonFieldGuid;
  else
    Result := nil;
  end;
end;

class function TMVCSwagger.TypeKindToMVCSwagSchemaType(APropType: TRttiType): TMVCSwagSchemaType;
begin
  Result := stUnknown;

  if APropType.TypeKind = tkUnknown then
    Exit;

  case APropType.TypeKind of
    tkClass:
      begin
        if (APropType.Handle = TypeInfo(TStream)) or
          (APropType.Handle = TypeInfo(TMemoryStream)) or
          (APropType.Handle = TypeInfo(TStringStream)) then
          Result := stString
        else
          Result := stObject;
      end;
    tkArray:
      Result := stArray;
    tkString, tkUString, tkChar:
      Result := stString;
    tkRecord:
      begin
        if APropType.Handle = TypeInfo(TGUID) then
          Result := stGuid
      end;
    tkInteger:
      Result := stInteger;
    tkInt64:
      Result := stInt64;
    tkEnumeration:
      begin
        if APropType.Handle = TypeInfo(Boolean) then
          Result := stBoolean
        else
          Result := stEnumeration;
      end;
    tkFloat:
      begin
        if APropType.Handle = TypeInfo(TDateTime) then
          Result := stDateTime
        else if APropType.Handle = TypeInfo(TDate) then
          Result := stDate
        else if APropType.Handle = TypeInfo(TTime) then
          Result := stTime
        else
          Result := stNumber;
      end;
  end;
end;

class function TMVCSwagger.GetJWTAuthenticationPath(const AJWTUrlSegment: string): TSwagPath;
var
  LSwagPathOp: TSwagPathOperation;
  LSwagResponse: TSwagResponse;
begin
  LSwagPathOp := TSwagPathOperation.Create;
  LSwagPathOp.Tags.Add('JWT Authentication');
  LSwagPathOp.Operation := ohvPost;
  LSwagPathOp.Security.Add(SECURITY_BASIC_NAME);
  LSwagPathOp.Description := 'Create JSON Web Token';
  LSwagPathOp.Produces.Add(TMVCMediaType.APPLICATION_JSON);

  LSwagResponse := TSwagResponse.Create;
  LSwagResponse.StatusCode := HTTP_STATUS.Unauthorized.ToString;
  LSwagResponse.Description := 'Invalid authorization type';
  LSwagPathOp.Responses.Add(LSwagResponse.StatusCode, LSwagResponse);

  LSwagResponse := TSwagResponse.Create;
  LSwagResponse.StatusCode := HTTP_STATUS.Forbidden.ToString;
  LSwagResponse.Description := 'Forbidden';
  LSwagPathOp.Responses.Add(LSwagResponse.StatusCode, LSwagResponse);

  LSwagResponse := TSwagResponse.Create;
  LSwagResponse.StatusCode := HTTP_STATUS.InternalServerError.ToString;
  LSwagResponse.Description := 'Internal server error';
  LSwagPathOp.Responses.Add(LSwagResponse.StatusCode, LSwagResponse);

  LSwagResponse := TSwagResponse.Create;
  LSwagResponse.StatusCode := HTTP_STATUS.OK.ToString;
  LSwagResponse.Description := 'OK';
  LSwagResponse.Schema.JsonSchema := TJSONObject.ParseJSONValue(JWT_JSON_SCHEMA) as TJSONObject;
  LSwagPathOp.Responses.Add(LSwagResponse.StatusCode, LSwagResponse);

  Result := TSwagPath.Create;
  Result.Uri := AJwtUrlSegment;
  Result.Operations.Add(LSwagPathOp);
end;

class function TMVCSwagger.GetMVCSwagParamsFromMethod(const AMethod: TRttiMethod): TArray<MVCSwagParamAttribute>;
var
  LAttr: TCustomAttribute;
begin
  SetLength(Result, 0);
  for LAttr in AMethod.GetAttributes do
  begin
    if LAttr is MVCSwagParamAttribute then
    begin
      Insert([MVCSwagParamAttribute(LAttr)], Result, High(Result));
    end;
  end;
end;

class function TMVCSwagger.GetParamsFromMethod(const AResourcePath: string; const AMethod: TRttiMethod):
  TArray<TSwagRequestParameter>;

  function TryGetMVCPathParamByName(const AParams: TArray<MVCSwagParamAttribute>;
    const
      AParamName:
      string;
    out AMVCParam: MVCSwagParamAttribute;
    out AIndex: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    AMVCParam := nil;
    AIndex := - 1;
    for I := Low(AParams) to High(AParams) do
      if SameText(AParams[I].ParamName, AParamName) and (AParams[I].ParamLocation = plPath) then
      begin
        AMVCParam := AParams[I];
        AIndex := I;
        Exit(True);
      end;
  end;

var
  LMatches: TMatchCollection;
  LMatch: TMatch;
  LParamName: string;
  LMethodParam: TRttiParameter;
  LSwagParam: TSwagRequestParameter;
  LMVCSwagParams: TArray<MVCSwagParamAttribute>;
  LMVCParam: MVCSwagParamAttribute;
  LIndex: Integer;
  I: Integer;
begin
  LMVCSwagParams := GetMVCSwagParamsFromMethod(AMethod);

  SetLength(Result, 0);

  // Path parameters
  LMatches := TRegEx.Matches(AResourcePath, '({)([\w_]+)(})', [roIgnoreCase, roMultiLine]);
  for LMatch in LMatches do
  begin
    LParamName := LMatch.Groups[2].Value;
    for LMethodParam in AMethod.GetParameters do
    begin
      if SameText(LMethodParam.Name, LParamName) then
      begin
        LSwagParam := TSwagRequestParameter.Create;

        if TryGetMVCPathParamByName(LMVCSwagParams, LParamName, LMVCParam, LIndex) then
        begin
          LSwagParam.Name := LParamName;
          LSwagParam.InLocation := MVCParamLocationToSwagRequestParamInLocation(LMVCParam.ParamLocation);
          LSwagParam.Required := LMVCParam.Required;
          LSwagParam.TypeParameter := MVCParamTypeToSwagTypeParameter(LMVCParam.ParamType);
          LSwagParam.Description := LMVCParam.ParamDescription;
          if not LMVCParam.JsonSchema.IsEmpty then
            LSwagParam.Schema.JsonSchema := TJSONObject.ParseJSONValue(LMVCParam.JsonSchema) as TJSONObject
          else if Assigned(LMVCParam.JsonSchemaClass) then
            LSwagParam.Schema.JsonSchema := ExtractJsonSchemaFromClass(LMVCParam.JsonSchemaClass);
          Delete(LMVCSwagParams, LIndex, 1);
        end
        else
        begin
          LSwagParam.Name := LParamName;
          LSwagParam.InLocation := rpiPath;
          LSwagParam.Required := True;
          LSwagParam.TypeParameter := RttiTypeToSwagType(LMethodParam.ParamType);
        end;
        Insert([LSwagParam], Result, High(Result));
      end;
    end;
  end;

  // Other parameters
  for I := Low(LMVCSwagParams) to High(LMVCSwagParams) do
  begin
    LSwagParam := TSwagRequestParameter.Create;
    LSwagParam.Name := LMVCSwagParams[I].ParamName;
    LSwagParam.InLocation := MVCParamLocationToSwagRequestParamInLocation(LMVCSwagParams[I].ParamLocation);
    LSwagParam.Required := LMVCSwagParams[I].Required;
    LSwagParam.TypeParameter := MVCParamTypeToSwagTypeParameter(LMVCSwagParams[I].ParamType);
    LSwagParam.Description := LMVCSwagParams[I].ParamDescription;
    if not LMVCSwagParams[I].JsonSchema.IsEmpty then
      LSwagParam.Schema.JsonSchema := TJSONObject.ParseJSONValue(LMVCSwagParams[I].JsonSchema) as TJSONObject
    else if Assigned(LMVCSwagParams[I].JsonSchemaClass) then
      LSwagParam.Schema.JsonSchema := ExtractJsonSchemaFromClass(LMVCSwagParams[I].JsonSchemaClass);

    Insert([LSwagParam], Result, High(Result));
  end;

end;

class function TMVCSwagger.MethodRequiresAuthentication(const AMethod: TRttiMethod; const AType: TRttiType): Boolean;
var
  LAttr: TCustomAttribute;
begin
  Result := False;

  for LAttr in AMethod.GetAttributes do
    if LAttr is MVCRequiresAuthenticationAttribute then
      Exit(True);

  for LAttr in AType.GetAttributes do
    if LAttr is MVCRequiresAuthenticationAttribute then
      Exit(True);
end;

class function TMVCSwagger.MVCHttpMethodToSwagPathOperation(const AMVCHTTPMethod: TMVCHTTPMethodType):
  TSwagPathTypeOperation;
begin
  case AMVCHTTPMethod of
    httpGET:
      Result := ohvGet;
    httpPOST:
      Result := ohvPost;
    httpPUT:
      Result := ohvPut;
    httpDELETE:
      Result := ohvDelete;
    httpHEAD:
      Result := ohvHead;
    httpOPTIONS:
      Result := ohvOptions;
    httpPATCH:
      Result := ohvPatch;
  else
    Result := ohvNotDefined;
  end;
end;

class function TMVCSwagger.MVCParamLocationToSwagRequestParamInLocation(const AMVCSwagParamLocation: TMVCSwagParamLocation)
  : TSwagRequestParameterInLocation;
begin
  case AMVCSwagParamLocation of
    plQuery:
      Result := rpiQuery;
    plHeader:
      Result := rpiHeader;
    plPath:
      Result := rpiPath;
    plFormData:
      Result := rpiFormData;
    plBody:
      Result := rpiBody;
  else
    Result := rpiNotDefined;
  end;
end;

class function TMVCSwagger.MVCParamTypeToSwagTypeParameter(const AMVSwagParamType: TMVCSwagParamType): TSwagTypeParameter;
begin
  case AMVSwagParamType of
    ptString:
      Result := stpString;
    ptNumber:
      Result := stpNumber;
    ptInteger:
      Result := stpInteger;
    ptBoolean:
      Result := stpBoolean;
    ptArray:
      Result := stpArray;
    ptFile:
      Result := stpFile;
  else
    Result := stpNotDefined;
  end;
end;

class function TMVCSwagger.MVCPathToSwagPath(const AResourcePath: string): string;
begin
  Result := TRegEx.Replace(AResourcePath, '(\([($])([\w_]+)([)])', '{\2}', [roIgnoreCase, roMultiLine]);
end;

{ MVCSwagSummary }

constructor MVCSwagSummaryAttribute.Create(const ATags, ADescription: string; const APathId: string;
  ADeprecated: Boolean);
begin
  FTags := ATags;
  FDescription := ADescription;
  FPathId := APathId;
  FDeprecated := ADeprecated;
end;

function MVCSwagSummaryAttribute.GetTags: TArray<string>;
begin
  Result := FTags.Split([',']);
end;

{ MVCSwagResponsesAttribute }

constructor MVCSwagResponsesAttribute.Create(const AStatusCode: Integer; const ADescription: string;
  const AJsonSchema: string);
begin
  FStatusCode := AStatusCode;
  FDescription := ADescription;
  FJsonSchema := AJsonSchema;
  FJsonSchemaClass := nil;
end;

constructor MVCSwagResponsesAttribute.Create(const AStatusCode: Integer; const ADescription: string;
  const AJsonSchemaClass: TClass);
begin
  Create(AStatusCode, ADescription, '');
  FJsonSchemaClass := AJsonSchemaClass;
end;

{ MVCSwagParamAttribute }

constructor MVCSwagParamAttribute.Create(const AParamLocation: TMVCSwagParamLocation; const AParamName,
  AParamDescription: string; const AParamType: TMVCSwagParamType; const ARequired: Boolean; const AJsonSchema: string);
begin
  FParamLocation := AParamLocation;
  FParamName := AParamName;
  FParamDescription := AParamDescription;
  FParamType := AParamType;
  FRequired := ARequired;
  FJsonSchema := AJsonSchema;
  FJsonSchemaClass := nil;
end;

constructor MVCSwagParamAttribute.Create(const AParamLocation: TMVCSwagParamLocation; const AParamName,
  AParamDescription: string; const AJsonSchemaClass: TClass; const AParamType: TMVCSwagParamType;
  const ARequired: Boolean);
begin
  Create(AParamLocation, AParamName, AParamDescription, AParamType, ARequired, '');
  FJsonSchemaClass := AJsonSchemaClass;
end;

{ MVCSwagJsonSchemaFieldAttribute }

constructor MVCSwagJsonSchemaFieldAttribute.Create(const ASchemaFieldType: TMVCSwagSchemaType;
  const AFieldName, ADescription: string; const ARequired, ANullable: Boolean);
begin
  FSchemaFieldType := ASchemaFieldType;
  FFieldName := AFieldName;
  FDescription := ADescription;
  FRequired := ARequired;
  FNullable := ANullable;
end;

constructor MVCSwagJsonSchemaFieldAttribute.Create(const AFieldName, ADescription: string; const ARequired,
  ANullable: Boolean);
begin
  Create(stUnknown, AFieldName, ADescription, ARequired, ANullable);
end;

{ TFieldSchemaDefinition }

class function TFieldSchemaDefinition.Create: TFieldSchemaDefinition;
begin
  Result.SchemaFieldType := stUnknown;
  Result.FieldName := '';
  Result.Description := '';
  Result.Required := False;
  Result.Nullable := False;
end;

end.
