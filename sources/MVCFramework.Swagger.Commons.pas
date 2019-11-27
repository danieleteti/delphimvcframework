// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file:
// João Antônio Duarte (https://github.com/joaoduarte19)
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
  System.SysUtils,
  MVCFramework.Commons,
  Swag.Doc.Path.Operation.RequestParameter,
  Swag.Doc.Path.Operation,
  Swag.Doc.Path,
  System.Json,
  Json.Schema.Field,
  Json.Schema.Field.Objects,
  Swag.Doc.Definition;

type
  TMVCSwagParamLocation = (plNotDefined, plQuery, plHeader, plPath, plFormData, plBody);
  TMVCSwagParamType = (ptNotDefined, ptString, ptNumber, ptInteger, ptBoolean, ptArray, ptFile);
  TMVCSwagSchemaType = (stUnknown, stInteger, stInt64, stNumber, stDateTime, stDate, stTime, stEnumeration, stBoolean,
    stObject, stArray, stString, stChar, stGuid);
  TMVCSwagAuthenticationType = (atBasic, atJsonWebToken);

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
    fTags: string;
    fDeprecated: Boolean;
    fDescription: string;
    fOperationID: string;
  public
    constructor Create(const ATags, ADescription: string; const AOperationId: string = '';
      ADeprecated: Boolean = False);
    function GetTags: TArray<string>;
    property Tags: string read fTags;
    property Description: string read fDescription;
    property OperationID: string read fOperationID;
    property Deprecated: Boolean read fDeprecated;
  end;

  /// <summary>
  /// Specifies your controller authentication type
  /// </summary>
  MVCSwagAuthenticationAttribute = class(TCustomAttribute)
  private
    FAuthenticationType: TMVCSwagAuthenticationType;
  public
    constructor Create(const AAuthenticationType: TMVCSwagAuthenticationType = atJsonWebToken);
    property AuthenticationType: TMVCSwagAuthenticationType read FAuthenticationType;
  end;

  /// <summary>
  /// Specify swagger path responses.
  /// </summary>
  MVCSwagResponsesAttribute = class(TCustomAttribute)
  private
    fStatusCode: Integer;
    fDescription: string;
    fJsonSchema: string;
    fJsonSchemaClass: TClass;
    fIsArray: Boolean;
  public
    constructor Create(const AStatusCode: Integer; const ADescription: string; const AJsonSchema: string = '');
      overload;

    constructor Create(const AStatusCode: Integer; const ADescription: string; const AJsonSchemaClass: TClass;
      const AIsArray: Boolean = False); overload;

    property StatusCode: Integer read fStatusCode;
    property Description: string read fDescription;
    property JsonSchema: string read fJsonSchema;
    property JsonSchemaClass: TClass read fJsonSchemaClass;
    property IsArray: Boolean read fIsArray;
  end;

  /// <summary>
  /// Specify swagger path params.
  /// </summary>
  MVCSwagParamAttribute = class(TCustomAttribute)
  private
    fParamLocation: TMVCSwagParamLocation;
    fParamName: string;
    fParamDescription: string;
    fParamType: TMVCSwagParamType;
    fRequired: Boolean;
    fJsonSchema: string;
    fJsonSchemaClass: TClass;
  public
    constructor Create(const AParamLocation: TMVCSwagParamLocation; const AParamName: string;
      const AParamDescription: string; const AParamType: TMVCSwagParamType; const ARequired: Boolean = True;
      const AJsonSchema: string = ''); overload;
    constructor Create(const AParamLocation: TMVCSwagParamLocation; const AParamName: string;
      const AParamDescription: string; const AJsonSchemaClass: TClass;
      const AParamType: TMVCSwagParamType = ptNotDefined; const ARequired: Boolean = True); overload;

    property ParamLocation: TMVCSwagParamLocation read fParamLocation;
    property ParamName: string read fParamName;
    property ParamDescription: string read fParamDescription;
    property ParamType: TMVCSwagParamType read fParamType;
    property Required: Boolean read fRequired;
    property JsonSchema: string read fJsonSchema;
    property JsonSchemaClass: TClass read fJsonSchemaClass;
  end;

  /// <summary>
  /// Specifies the field definition in the json schema.
  /// Use this attribute on a class property that will be mapped to a json schema
  /// </summary>
  MVCSwagJSONSchemaFieldAttribute = class(TCustomAttribute)
  private
    FSchemaFieldType: TMVCSwagSchemaType;
    FFieldName: string;
    fDescription: string;
    fRequired: Boolean;
    FNullable: Boolean;
  public
    constructor Create(const ASchemaFieldType: TMVCSwagSchemaType; const AFieldName: string; const ADescription: string;
      const ARequired: Boolean = True; const ANullable: Boolean = False); overload;
    constructor Create(const AFieldName: string; const ADescription: string; const ARequired: Boolean = True;
      const ANullable: Boolean = False); overload;

    property SchemaFieldType: TMVCSwagSchemaType read FSchemaFieldType;
    property FieldName: string read FFieldName;
    property Description: string read fDescription;
    property Required: Boolean read fRequired;
    property Nullable: Boolean read FNullable;
  end;

  /// <summary>
  /// Use this attribute in the class or method to ignore the path in creating swagger documentation.
  /// </summary>
  MVCSwagIgnorePathAttribute = class(TCustomAttribute);

  /// <summary>
  /// SwaggerDoc Methods
  /// </summary>
  TMVCSwagger = class sealed
  private
    class var FRttiContext: TRttiContext;
    class function GetMVCSwagParamsFromMethod(const AMethod: TRttiMethod): TArray<MVCSwagParamAttribute>;
    class function MVCParamLocationToSwagRequestParamInLocation(const AMVCSwagParamLocation: TMVCSwagParamLocation)
      : TSwagRequestParameterInLocation;
    class function MVCParamTypeToSwagTypeParameter(const AMVSwagParamType: TMVCSwagParamType): TSwagTypeParameter;
    class procedure ExtractJsonSchemaFromClass(const AJsonFieldRoot: TJsonFieldObject; const AClass: TClass); overload;
    class function ExtractJsonSchemaFromClass(const AClass: TClass; const AIsArray: Boolean = False)
      : TJSONObject; overload;
    class function GetJsonFieldClass(const ASchemaFieldType: TMVCSwagSchemaType): TJsonFieldClass;
    class function TypeKindToMVCSwagSchemaType(APropType: TRttiType): TMVCSwagSchemaType; static;
    class function TypeIsEnumerable(const ARttiType: TRttiType): Boolean; static;
  public
    class constructor Create;
    class destructor Destroy;
    class function MVCHttpMethodToSwagPathOperation(const AMVCHTTPMethod: TMVCHTTPMethodType): TSwagPathTypeOperation;
    class function MVCPathToSwagPath(const AResourcePath: string): string;
    class function GetParamsFromMethod(const AResourcePath: string; const AMethod: TRttiMethod)
      : TArray<TSwagRequestParameter>;
    class function RttiTypeToSwagType(const ARttiType: TRttiType): TSwagTypeParameter;
    class procedure FillOperationSummary(const ASwagPathOperation: TSwagPathOperation; const AMethod: TRttiMethod);
    class function MethodRequiresAuthentication(const AMethod: TRttiMethod; const AType: TRttiType;
      out AAuthenticationTypeName: string): Boolean;
    class function GetJWTAuthenticationPath(const AJWTUrlSegment: string; AUserNameHeaderName, APasswordHeaderName: string): TSwagPath;
  end;

const
  SECURITY_BEARER_NAME = 'bearer';
  SECURITY_BASIC_NAME = 'basic';
  JWT_JSON_SCHEMA = '{' + sLineBreak + '	 "type": "object",' + sLineBreak + '	 "properties": {' + sLineBreak +
    '		 "token": {' + sLineBreak + '			 "type": "string",' + sLineBreak + '			 "description": "JWT Token"' +
    sLineBreak + '		 }' + sLineBreak + '	 }' + sLineBreak + '}';
  JWT_DEFAULT_DESCRIPTION = 'For accessing the API a valid JWT token must be passed in all the queries ' +
    'in the ''Authorization'' header.' + sLineBreak + sLineBreak +
    'A valid JWT token is generated by the API and retourned as answer of a call ' + 'to the route defined ' +
    'in the JWT middleware giving a valid username and password.' + sLineBreak + sLineBreak +
    'The following syntax must be used in the ''Authorization'' header :' + sLineBreak + sLineBreak +
    '    Bearer xxxxxx.yyyyyyy.zzzzzz' + sLineBreak;

implementation

uses
  System.Classes,
  System.RegularExpressions,
  MVCFramework,
  MVCFramework.Serializer.Abstract,
  MVCFramework.Serializer.Commons,
  Swag.Doc.Path.Operation.Response,
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
      if (ARttiType.Handle = TypeInfo(TDateTime)) or (ARttiType.Handle = TypeInfo(TDate)) or
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

class function TMVCSwagger.TypeIsEnumerable(const ARttiType: TRttiType): Boolean;
begin
  Result := ARttiType.GetMethod('GetEnumerator') <> nil;
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

  THackMVCAbstractSerializer = class(TMVCAbstractSerializer);

class procedure TMVCSwagger.ExtractJsonSchemaFromClass(const AJsonFieldRoot: TJsonFieldObject; const AClass: TClass);
var
  lFieldSchemaDef: TFieldSchemaDefinition;
  lObjType: TRttiType;
  lProp: TRttiProperty;
  lSkipProp: Boolean;
  lAttr: TCustomAttribute;
  lJSFieldAttr: MVCSwagJSONSchemaFieldAttribute;
  lJsonFieldClass: TJsonFieldClass;
  lJsonField: TJsonField;
  lJsonFieldObject: TJsonFieldObject;
  lAbstractSer: THackMVCAbstractSerializer;
  lClass: TClass;
begin
  lObjType := FRttiContext.GetType(AClass);
  for lProp in lObjType.GetProperties do
  begin
    lSkipProp := False;
    lFieldSchemaDef := TFieldSchemaDefinition.Create;

    for lAttr in lProp.GetAttributes do
    begin
      if lAttr is MVCDoNotSerializeAttribute then
      begin
        lSkipProp := True;
        Break;
      end;

      if lAttr is MVCSwagJSONSchemaFieldAttribute then
      begin
        lJSFieldAttr := MVCSwagJSONSchemaFieldAttribute(lAttr);
        lFieldSchemaDef.SchemaFieldType := lJSFieldAttr.SchemaFieldType;
        lFieldSchemaDef.FieldName := lJSFieldAttr.FieldName;
        lFieldSchemaDef.Description := lJSFieldAttr.Description;
        lFieldSchemaDef.Required := lJSFieldAttr.Required;
        lFieldSchemaDef.Nullable := lJSFieldAttr.Nullable;
        Break;
      end;
    end;

    if lSkipProp then
      Continue;

    if lFieldSchemaDef.SchemaFieldType = stUnknown then
    begin
      lFieldSchemaDef.SchemaFieldType := TypeKindToMVCSwagSchemaType(lProp.PropertyType);
      lFieldSchemaDef.FieldName := TMVCSerializerHelper.GetKeyName(lProp, lObjType);
    end;

    lJsonFieldClass := GetJsonFieldClass(lFieldSchemaDef.SchemaFieldType);
    if not Assigned(lJsonFieldClass) then
      Continue;

    lJsonField := lJsonFieldClass.Create;

    if (lJsonField is TJsonFieldObject) and (not TypeIsEnumerable(lProp.PropertyType)) then
    begin
      ExtractJsonSchemaFromClass((lJsonField as TJsonFieldObject), lProp.PropertyType.AsInstance.MetaClassType);
    end;

    if (lJsonField is TJsonFieldArray) and TypeIsEnumerable(lProp.PropertyType) then
    begin
      lJsonFieldObject := TJsonFieldObject.Create;

      lAbstractSer := THackMVCAbstractSerializer.Create;
      try
        lClass := lAbstractSer.GetObjectTypeOfGenericList(lProp.PropertyType.Handle);
        ExtractJsonSchemaFromClass(lJsonFieldObject, lClass);
        (lJsonField as TJsonFieldArray).ItemFieldType := lJsonFieldObject;
      finally
        lAbstractSer.Free;
      end;
    end;

    lJsonField.Name := lFieldSchemaDef.FieldName;
    lJsonField.Required := lFieldSchemaDef.Required;
    lJsonField.Nullable := lFieldSchemaDef.Nullable;
    if not lFieldSchemaDef.Description.IsEmpty then
      TJsonFieldInteger(lJsonField).Description := lFieldSchemaDef.Description;

    AJsonFieldRoot.AddField(lJsonField);
  end;
end;

class function TMVCSwagger.ExtractJsonSchemaFromClass(const AClass: TClass; const AIsArray: Boolean): TJSONObject;
var
  lJsonSchema: TJsonField;
  lJsonRoot: TJsonFieldObject;
begin
  if AIsArray then
  begin
    lJsonSchema := TJsonFieldArray.Create
  end
  else
  begin
    lJsonSchema := TJsonFieldObject.Create;
  end;
  try
    if AIsArray then
    begin
      lJsonRoot := TJsonFieldObject.Create;
      TJsonFieldArray(lJsonSchema).ItemFieldType := lJsonRoot;
      TJsonFieldArray(lJsonSchema).Name := 'items';
    end
    else
    begin
      lJsonRoot := lJsonSchema as TJsonFieldObject;
    end;

    ExtractJsonSchemaFromClass(lJsonRoot, AClass);
    Result := lJsonSchema.ToJsonSchema;
  finally
    lJsonSchema.Free;
  end;
end;

class procedure TMVCSwagger.FillOperationSummary(const ASwagPathOperation: TSwagPathOperation;
  const AMethod: TRttiMethod);
var
  lAttr: TCustomAttribute;
  lSwagResponse: TSwagResponse;
  lSwagResponsesAttr: MVCSwagResponsesAttribute;
begin
  for lAttr in AMethod.GetAttributes do
  begin
    if lAttr is MVCSwagSummaryAttribute then
    begin
      ASwagPathOperation.Tags.AddRange(MVCSwagSummaryAttribute(lAttr).GetTags);
      ASwagPathOperation.Description := MVCSwagSummaryAttribute(lAttr).Description;
      ASwagPathOperation.OperationID := MVCSwagSummaryAttribute(lAttr).OperationID;
      // dt [2019-10-31] Ensure OperationID is always defined
      if ASwagPathOperation.OperationID.IsEmpty then
      begin
        ASwagPathOperation.OperationID := AMethod.Name;
      end;
      // dt [2019-10-31]
      ASwagPathOperation.Deprecated := MVCSwagSummaryAttribute(lAttr).Deprecated;
    end;
    if lAttr is MVCConsumesAttribute then
    begin
      ASwagPathOperation.Consumes.Add(MVCConsumesAttribute(lAttr).Value)
    end;
    if lAttr is MVCProducesAttribute then
    begin
      ASwagPathOperation.Produces.Add(MVCProducesAttribute(lAttr).Value)
    end;
    if lAttr is MVCSwagResponsesAttribute then
    begin
      lSwagResponsesAttr := MVCSwagResponsesAttribute(lAttr);

      lSwagResponse := TSwagResponse.Create;
      lSwagResponse.StatusCode := lSwagResponsesAttr.StatusCode.ToString;
      lSwagResponse.Description := lSwagResponsesAttr.Description;
      if not lSwagResponsesAttr.JsonSchema.IsEmpty then
      begin
        lSwagResponse.Schema.JsonSchema := TJSONObject.ParseJSONValue(lSwagResponsesAttr.JsonSchema) as TJSONObject
      end
      else if Assigned(lSwagResponsesAttr.JsonSchemaClass) then
      begin
        lSwagResponse.Schema.JsonSchema := ExtractJsonSchemaFromClass(lSwagResponsesAttr.JsonSchemaClass,
          lSwagResponsesAttr.IsArray);
      end;
      ASwagPathOperation.Responses.Add(lSwagResponse.StatusCode, lSwagResponse);
    end;
  end;

  if ASwagPathOperation.Tags.Count = 0 then
    ASwagPathOperation.Tags.Add(AMethod.Parent.QualifiedName);

  if ASwagPathOperation.Produces.Count <= 0 then
    ASwagPathOperation.Produces.Add(TMVCMediaType.APPLICATION_JSON);

  if ASwagPathOperation.Responses.Count <= 0 then
  begin
    lSwagResponse := TSwagResponse.Create;
    lSwagResponse.StatusCode := HTTP_STATUS.OK.ToString;
    lSwagResponse.Description := 'Ok';
    ASwagPathOperation.Responses.Add(lSwagResponse.StatusCode, lSwagResponse);

    lSwagResponse := TSwagResponse.Create;
    lSwagResponse.StatusCode := HTTP_STATUS.InternalServerError.ToString;
    lSwagResponse.Description := 'Internal server error';
    ASwagPathOperation.Responses.Add(lSwagResponse.StatusCode, lSwagResponse);
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
    stObject:
      Result := TJsonFieldObject;
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
        if (APropType.Handle = TypeInfo(TStream)) or (APropType.Handle = TypeInfo(TMemoryStream)) or
          (APropType.Handle = TypeInfo(TStringStream)) then
          Result := stString
        else if TypeIsEnumerable(APropType) then
          Result := stArray
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

class function TMVCSwagger.GetJWTAuthenticationPath(const AJWTUrlSegment: string; AUserNameHeaderName, APasswordHeaderName: string): TSwagPath;
var
  lSwagPathOp: TSwagPathOperation;
  lSwagResponse: TSwagResponse;
  lSwagParam: TSwagRequestParameter;
begin
  lSwagPathOp := TSwagPathOperation.Create;
  lSwagPathOp.Tags.Add('JWT Authentication');
  lSwagPathOp.Operation := ohvPost;
  lSwagPathOp.Security.Add(SECURITY_BASIC_NAME);
  lSwagPathOp.Description := 'Create JSON Web Token';
  lSwagPathOp.Produces.Add(TMVCMediaType.APPLICATION_JSON);
  lSwagParam := TSwagRequestParameter.Create;
  lSwagParam.Name := AUserNameHeaderName;
  lSwagParam.TypeParameter := stpString;
  lSwagParam.Required := true;
  lSwagParam.InLocation := rpiHeader;
  lSwagPathOp.Parameters.Add(lSwagParam);

  lSwagParam := TSwagRequestParameter.Create;
  lSwagParam.Name := APasswordHeaderName;
  lSwagParam.TypeParameter := stpString;
  lSwagParam.Required := true;
  lSwagParam.InLocation := rpiHeader;
  lSwagPathOp.Parameters.Add(lSwagParam);

  lSwagResponse := TSwagResponse.Create;
  lSwagResponse.StatusCode := HTTP_STATUS.Unauthorized.ToString;
  lSwagResponse.Description := 'Invalid authorization type';
  lSwagPathOp.Responses.Add(lSwagResponse.StatusCode, lSwagResponse);

  lSwagResponse := TSwagResponse.Create;
  lSwagResponse.StatusCode := HTTP_STATUS.Forbidden.ToString;
  lSwagResponse.Description := 'Forbidden';
  lSwagPathOp.Responses.Add(lSwagResponse.StatusCode, lSwagResponse);

  lSwagResponse := TSwagResponse.Create;
  lSwagResponse.StatusCode := HTTP_STATUS.InternalServerError.ToString;
  lSwagResponse.Description := 'Internal server error';
  lSwagPathOp.Responses.Add(lSwagResponse.StatusCode, lSwagResponse);

  lSwagResponse := TSwagResponse.Create;
  lSwagResponse.StatusCode := HTTP_STATUS.OK.ToString;
  lSwagResponse.Description := 'OK';
  lSwagResponse.Schema.JsonSchema := TJSONObject.ParseJSONValue(JWT_JSON_SCHEMA) as TJSONObject;
  lSwagPathOp.Responses.Add(lSwagResponse.StatusCode, lSwagResponse);

  Result := TSwagPath.Create;
  Result.Uri := AJWTUrlSegment;
  Result.Operations.Add(lSwagPathOp);
end;

class function TMVCSwagger.GetMVCSwagParamsFromMethod(const AMethod: TRttiMethod): TArray<MVCSwagParamAttribute>;
var
  lAttr: TCustomAttribute;
begin
  SetLength(Result, 0);
  for lAttr in AMethod.GetAttributes do
  begin
    if lAttr is MVCSwagParamAttribute then
    begin
      Insert([MVCSwagParamAttribute(lAttr)], Result, High(Result));
    end;
  end;
end;

class function TMVCSwagger.GetParamsFromMethod(const AResourcePath: string; const AMethod: TRttiMethod)
  : TArray<TSwagRequestParameter>;

  function TryGetMVCPathParamByName(const AParams: TArray<MVCSwagParamAttribute>; const AParamName: string;
    out AMVCParam: MVCSwagParamAttribute; out AIndex: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    AMVCParam := nil;
    AIndex := -1;
    for I := Low(AParams) to High(AParams) do
      if SameText(AParams[I].ParamName, AParamName) and (AParams[I].ParamLocation = plPath) then
      begin
        AMVCParam := AParams[I];
        AIndex := I;
        Exit(True);
      end;
  end;

var
  lMatches: TMatchCollection;
  lMatch: TMatch;
  lParamName: string;
  lMethodParam: TRttiParameter;
  lSwagParam: TSwagRequestParameter;
  lMVCSwagParams: TArray<MVCSwagParamAttribute>;
  lMVCParam: MVCSwagParamAttribute;
  lIndex: Integer;
  I: Integer;
begin
  lMVCSwagParams := GetMVCSwagParamsFromMethod(AMethod);

  SetLength(Result, 0);

  // Path parameters
  lMatches := TRegEx.Matches(AResourcePath, '({)([\w_]+)(})', [roIgnoreCase, roMultiLine]);
  for lMatch in lMatches do
  begin
    lParamName := lMatch.Groups[2].Value;
    for lMethodParam in AMethod.GetParameters do
    begin
      if SameText(lMethodParam.Name, lParamName) then
      begin
        lSwagParam := TSwagRequestParameter.Create;

        if TryGetMVCPathParamByName(lMVCSwagParams, lParamName, lMVCParam, lIndex) then
        begin
          lSwagParam.Name := lParamName;
          lSwagParam.InLocation := MVCParamLocationToSwagRequestParamInLocation(lMVCParam.ParamLocation);
          lSwagParam.Required := lMVCParam.Required;
          lSwagParam.TypeParameter := MVCParamTypeToSwagTypeParameter(lMVCParam.ParamType);
          lSwagParam.Description := lMVCParam.ParamDescription;
          if not lMVCParam.JsonSchema.IsEmpty then
          begin
            lSwagParam.Schema.JsonSchema := TJSONObject.ParseJSONValue(lMVCParam.JsonSchema) as TJSONObject
          end
          else if Assigned(lMVCParam.JsonSchemaClass) then
          begin
            lSwagParam.Schema.JsonSchema := ExtractJsonSchemaFromClass(lMVCParam.JsonSchemaClass,
              lMVCParam.ParamType = ptArray);
          end;
          Delete(lMVCSwagParams, lIndex, 1);
        end
        else
        begin
          lSwagParam.Name := lParamName;
          lSwagParam.InLocation := rpiPath;
          lSwagParam.Required := True;
          lSwagParam.TypeParameter := RttiTypeToSwagType(lMethodParam.ParamType);
        end;
        Insert([lSwagParam], Result, High(Result));
      end;
    end;
  end;

  // Other parameters
  for I := Low(lMVCSwagParams) to High(lMVCSwagParams) do
  begin
    lSwagParam := TSwagRequestParameter.Create;
    lSwagParam.Name := lMVCSwagParams[I].ParamName;
    lSwagParam.InLocation := MVCParamLocationToSwagRequestParamInLocation(lMVCSwagParams[I].ParamLocation);
    lSwagParam.Required := lMVCSwagParams[I].Required;
    lSwagParam.TypeParameter := MVCParamTypeToSwagTypeParameter(lMVCSwagParams[I].ParamType);
    lSwagParam.Description := lMVCSwagParams[I].ParamDescription;
    if not lMVCSwagParams[I].JsonSchema.IsEmpty then
    begin
      lSwagParam.Schema.JsonSchema := TJSONObject.ParseJSONValue(lMVCSwagParams[I].JsonSchema) as TJSONObject
    end
    else if Assigned(lMVCSwagParams[I].JsonSchemaClass) then
    begin
      lSwagParam.Schema.JsonSchema := ExtractJsonSchemaFromClass(lMVCSwagParams[I].JsonSchemaClass,
        lMVCSwagParams[I].ParamType = ptArray);
    end;

    Insert([lSwagParam], Result, High(Result));
  end;

end;

class function TMVCSwagger.MethodRequiresAuthentication(const AMethod: TRttiMethod; const AType: TRttiType;
  out AAuthenticationTypeName: string): Boolean;
var
  lAttr: TCustomAttribute;
begin
  Result := False;
  AAuthenticationTypeName := '';

  for lAttr in AMethod.GetAttributes do
    if lAttr is MVCRequiresAuthenticationAttribute then
    begin
      AAuthenticationTypeName := SECURITY_BEARER_NAME;
      Exit(True);
    end
    else if lAttr is MVCSwagAuthenticationAttribute then
    begin
      case MVCSwagAuthenticationAttribute(lAttr).AuthenticationType of
        atBasic:
          AAuthenticationTypeName := SECURITY_BASIC_NAME;
        atJsonWebToken:
          AAuthenticationTypeName := SECURITY_BEARER_NAME;
      end;
      Exit(True);
    end;

  for lAttr in AType.GetAttributes do
    if lAttr is MVCRequiresAuthenticationAttribute then
    begin
      AAuthenticationTypeName := SECURITY_BEARER_NAME;
      Exit(True);
    end
    else if lAttr is MVCSwagAuthenticationAttribute then
    begin
      case MVCSwagAuthenticationAttribute(lAttr).AuthenticationType of
        atBasic:
          AAuthenticationTypeName := SECURITY_BASIC_NAME;
        atJsonWebToken:
          AAuthenticationTypeName := SECURITY_BEARER_NAME;
      end;
      Exit(True);
    end;
end;

class function TMVCSwagger.MVCHttpMethodToSwagPathOperation(const AMVCHTTPMethod: TMVCHTTPMethodType)
  : TSwagPathTypeOperation;
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

class function TMVCSwagger.MVCParamLocationToSwagRequestParamInLocation(const AMVCSwagParamLocation
  : TMVCSwagParamLocation): TSwagRequestParameterInLocation;
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

class function TMVCSwagger.MVCParamTypeToSwagTypeParameter(const AMVSwagParamType: TMVCSwagParamType)
  : TSwagTypeParameter;
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

constructor MVCSwagSummaryAttribute.Create(const ATags, ADescription: string; const AOperationId: string;
  ADeprecated: Boolean);
begin
  fTags := ATags;
  fDescription := ADescription;
  fOperationID := AOperationId;
  fDeprecated := ADeprecated;
end;

function MVCSwagSummaryAttribute.GetTags: TArray<string>;
begin
  Result := fTags.Split([',']);
end;

{ MVCSwagResponsesAttribute }

constructor MVCSwagResponsesAttribute.Create(const AStatusCode: Integer; const ADescription: string;
  const AJsonSchema: string);
begin
  fStatusCode := AStatusCode;
  fDescription := ADescription;
  fJsonSchema := AJsonSchema;
  fJsonSchemaClass := nil;
end;

constructor MVCSwagResponsesAttribute.Create(const AStatusCode: Integer; const ADescription: string;
  const AJsonSchemaClass: TClass; const AIsArray: Boolean);
begin
  Create(AStatusCode, ADescription, '');
  fJsonSchemaClass := AJsonSchemaClass;
  fIsArray := AIsArray;
end;

{ MVCSwagParamAttribute }

constructor MVCSwagParamAttribute.Create(const AParamLocation: TMVCSwagParamLocation;
  const AParamName, AParamDescription: string; const AParamType: TMVCSwagParamType; const ARequired: Boolean;
  const AJsonSchema: string);
begin
  fParamLocation := AParamLocation;
  fParamName := AParamName;
  fParamDescription := AParamDescription;
  fParamType := AParamType;
  fRequired := ARequired;
  fJsonSchema := AJsonSchema;
  fJsonSchemaClass := nil;
end;

constructor MVCSwagParamAttribute.Create(const AParamLocation: TMVCSwagParamLocation;
  const AParamName, AParamDescription: string; const AJsonSchemaClass: TClass; const AParamType: TMVCSwagParamType;
  const ARequired: Boolean);
begin
  Create(AParamLocation, AParamName, AParamDescription, AParamType, ARequired, '');
  fJsonSchemaClass := AJsonSchemaClass;
end;

{ MVCSwagJSONSchemaFieldAttribute }

constructor MVCSwagJSONSchemaFieldAttribute.Create(const AFieldName, ADescription: string;
  const ARequired, ANullable: Boolean);
begin
  Create(stUnknown, AFieldName, ADescription, ARequired, ANullable);
end;

constructor MVCSwagJSONSchemaFieldAttribute.Create(const ASchemaFieldType: TMVCSwagSchemaType;
  const AFieldName, ADescription: string; const ARequired, ANullable: Boolean);
begin
  FSchemaFieldType := ASchemaFieldType;
  FFieldName := AFieldName;
  fDescription := ADescription;
  fRequired := ARequired;
  FNullable := ANullable;
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

{ MVCSwagAuthenticationAttribute }

constructor MVCSwagAuthenticationAttribute.Create(const AAuthenticationType: TMVCSwagAuthenticationType);
begin
  FAuthenticationType := AAuthenticationType;
end;

end.
