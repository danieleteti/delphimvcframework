// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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
  Swag.Doc.Definition,
  System.Generics.Collections, 
  System.Generics.Defaults;

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
    constructor Create(const aTags, aDescription: string; const aOperationId: string = '';
      aDeprecated: Boolean = False);
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
    fAuthenticationType: TMVCSwagAuthenticationType;
  public
    constructor Create(const aAuthenticationType: TMVCSwagAuthenticationType = atJsonWebToken);
    property AuthenticationType: TMVCSwagAuthenticationType read fAuthenticationType;
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
    fRecordType: TRttiType;
  public
    constructor Create(const aStatusCode: Integer; const aDescription: string; const aJsonSchema: string = '');
      overload;

    constructor Create(const aStatusCode: Integer; const aDescription: string; const aJsonSchemaClass: TClass;
      const aIsArray: Boolean = False); overload;

    constructor Create(const aFullyQualifiedRecordName: String; const aStatusCode: Integer; const aDescription: string;
      const aIsArray: Boolean = false; const aJsonSchema: String = ''); overload;

    property StatusCode: Integer read fStatusCode;
    property Description: string read fDescription;
    property JsonSchema: string read fJsonSchema;
    property JsonSchemaClass: TClass read fJsonSchemaClass;
    property IsArray: Boolean read fIsArray;
    property RecordType: TRttiType read fRecordType;
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
    fDefaultValue: string;
    fEnumValues: string;
    fRecordType: TRttiType;
    function GetEnumValues: TArray<string>;
  public
    constructor Create(const aParamLocation: TMVCSwagParamLocation; const aParamName: string;
      const aParamDescription: string; const aParamType: TMVCSwagParamType; const aRequired: Boolean = True;
      const aDefaultValue: string = ''; const aEnumValues: string = ''; const aJsonSchema: string = ''); overload;
    constructor Create(const aParamLocation: TMVCSwagParamLocation; const aParamName: string;
      const aParamDescription: string; const aJsonSchemaClass: TClass;
      const aParamType: TMVCSwagParamType = ptNotDefined; const aRequired: Boolean = True;
      const aDefaultValue: string = ''; const aEnumValues: string = ''); overload;

    constructor Create(const aParamLocation: TMVCSwagParamLocation; const aParamName: string;
      const aParamDescription: string; const aFullyQualifiedRecordName: string;
      const aParamType: TMVCSwagParamType = ptNotDefined; const aRequired: Boolean = True;
      const aDefaultValue: string = ''; const aEnumValues: string = ''); overload;

    property ParamLocation: TMVCSwagParamLocation read fParamLocation;
    property ParamName: string read fParamName;
    property ParamDescription: string read fParamDescription;
    property ParamType: TMVCSwagParamType read fParamType;
    property Required: Boolean read fRequired;
    property DefaultValue: string read fDefaultValue;
    property EnumValues: TArray<string> read GetEnumValues;
    property JsonSchema: string read fJsonSchema;
    property JsonSchemaClass: TClass read fJsonSchemaClass;
    property RecordType: TRttiType read fRecordType;
  end;

  /// <summary>
  /// Specifies the field definition in the json schema.
  /// Use this attribute on a class property that will be mapped to a json schema
  /// </summary>
  MVCSwagJSONSchemaFieldAttribute = class(TCustomAttribute)
  private
    fSchemaFieldType: TMVCSwagSchemaType;
    fFieldName: string;
    fDescription: string;
    fRequired: Boolean;
    fNullable: Boolean;
    fMinLength: Integer;
    fMaxLength: Integer;
  public
    constructor Create(const aSchemaFieldType: TMVCSwagSchemaType; const aFieldName: string; const aDescription: string;
      const aRequired: Boolean = True; const aNullable: Boolean = False; const aMinLength: Integer = 0;
      const aMaxLength: Integer = 0); overload;
    constructor Create(const aFieldName: string; const aDescription: string; const aRequired: Boolean = True;
      const aNullable: Boolean = False; const aMinLength: Integer = 0; const aMaxLength: Integer = 0); overload;

    property SchemaFieldType: TMVCSwagSchemaType read fSchemaFieldType;
    property FieldName: string read fFieldName;
    property Description: string read fDescription;
    property Required: Boolean read fRequired;
    property Nullable: Boolean read fNullable;
    property MinLength: Integer read fMinLength;
    property MaxLength: Integer read fMaxLength;
  end;

  /// <summary>
  /// Use this attribute in controller to define a default model for all
  /// the actions which refer to "SWAGUseDefulatControllerModel" as Model class.
  /// It is useful when, using a base controller, you need to override the model
  /// used by child controller actions
  /// </summary>
  MVCSWAGDefaultModel = class(TCustomAttribute)
  private
    fJsonSchemaClass: TClass;
    fSingularModelName: string;
    fPluralModelName: string;
  public
    constructor Create(const aJsonSchemaClass: TClass; const aSingularModelName: String; const aPluralModelName: String);
    property JsonSchemaClass: TClass read fJsonSchemaClass;
    property SingularModelName: string read fSingularModelName;
    property PluralModelName: string read fPluralModelName;
  end;

  /// <summary>
  /// Use this attribute in controller to define a default Summary Tag for all
  /// the actions which refer to "USE_DEFAULT_SUMMARY_TAG" as tag.
  /// It is useful when, using a base controller, you need to override the tag
  /// used by child controller actions
  /// </summary>
  MVCSWAGDefaultSummaryTags = class(TCustomAttribute)
  private
    fDefaultTags: String;
  public
    constructor Create(const aDefaultTags: String);
    function GetTags: TArray<string>;
    property DefaultTags: String read fDefaultTags;
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
    class var fRttiContext: TRttiContext;
    class function GetMVCSwagParamsFromMethod(const aMethod: TRttiMethod): TArray<MVCSwagParamAttribute>;
    class function MVCParamLocationToSwagRequestParamInLocation(const aMVCSwagParamLocation: TMVCSwagParamLocation)
      : TSwagRequestParameterInLocation;
    class function MVCParamTypeToSwagTypeParameter(const aMVSwagParamType: TMVCSwagParamType): TSwagTypeParameter;
    class procedure ExtractJsonSchemaFromClass(const aJsonFieldRoot: TJsonFieldObject; const aClass: TClass); overload;
    class function ExtractJsonSchemaFromClass(const aClass: TClass; const aIsArray: Boolean = False)
      : TJSONObject; overload;
    class procedure ExtractJsonSchemaFromRecord(const aRttiType: TRttiType; const aJsonFieldRoot: TJsonFieldObject); overload;
    class function ExtractJsonSchemaFromRecord(const aTypeName: String; const aIsArray: Boolean = False): TJSONObject; overload;
    class function GetJsonFieldClass(const aSchemaFieldType: TMVCSwagSchemaType): TJsonFieldClass;
    class function TypeKindToMVCSwagSchemaType(aPropType: TRttiType): TMVCSwagSchemaType; static;
    class function TypeIsEnumerable(const aRttiType: TRttiType): Boolean; static;
    class function ApplyModelName(const Value: String; const Singular: String; const Plural: String): String;
    class procedure AddRequestModelDefinition(
      const aSwagReqParam:  TSwagRequestParameter;
      const aParamSchemaClass: TClass;
      const aSwagDefinitions: TObjectList<TSwagDefinition>;
      const aComparer: IComparer<TSwagDefinition>;
      const aMVCSwagParamType: TMVCSwagParamType;
      const aRecordType: TRttiType);
  public
    class constructor Create;
    class destructor Destroy;
    class function MVCHttpMethodToSwagPathOperation(const aMVCHTTPMethod: TMVCHTTPMethodType): TSwagPathTypeOperation;
    class function MVCPathToSwagPath(const aResourcePath: string): string;
    class function GetParamsFromMethod(const aResourcePath: string; const aMethod: TRttiMethod;
      const aSwagDefinitions: TObjectList<TSwagDefinition>;
      const aControllerDefaultModelClass: TClass;
      const aControllerDefaultModelSingularName: String;
      const aControllerDefaultModelPluralName: String
      ): TArray<TSwagRequestParameter>;
    class function RttiTypeToSwagType(const aRttiType: TRttiType): TSwagTypeParameter;
    class procedure FillOperationSummary(
      const aSwagPathOperation: TSwagPathOperation;
      const aMethod: TRttiMethod; const aSwagDefinitions: TObjectList<TSwagDefinition>;
      const aHTTPMethod: TMVCHTTPMethodType;
      const aControllerDefaultModel: TClass;
      const aControllerDefaultModelSingularName: String;
      const aControllerDefaultModelPluralName: String;
      const aControllerDefaultSummaryTags: TArray<String>);
    class function MethodRequiresAuthentication(const aMethod: TRttiMethod; const aType: TRttiType;
      out aAuthenticationTypeName: string): Boolean;
    class function GetJWTAuthenticationPath(const aJWTUrlSegment: string;
      aUserNameHeaderName, aPasswordHeaderName: string): TSwagPath;
  end;

  SWAGUseDefaultControllerModel = class sealed

  end;


  EMVCSWAGError = class(EMVCException)
  end;

  TArrayHelper = class
  public
    class procedure QuickSort<T>(var Values: array of T; const Comparer: IComparer<T>;  L, R: Integer); static;
  end;

  TSwaggerConst = record
    const USE_DEFAULT_SUMMARY_TAGS = '{default}';
    const SINGULAR_MODEL_NAME = '{singularmodel}';
    const PLURAL_MODEL_NAME = '{pluralmodel}';
  end;

const
  JWT_AUTHENTICATION_TAG = 'JWT Authentication';
  SECURITY_BEARER_NAME = 'bearer';
  SECURITY_BASIC_NAME = 'basic';
  JWT_JSON_SCHEMA = '{' + sLineBreak +
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
    'A valid JWT token is generated by the API and returned as answer of a call ' + 'to the route defined ' +
    'in the JWT middleware giving a valid username and password.' + sLineBreak + sLineBreak +
    'The following syntax must be used in the ''Authorization'' header :' + sLineBreak + sLineBreak +
    '    Bearer xxxxxx.yyyyyyy.zzzzzz' + sLineBreak;

implementation

uses
  System.Classes,
  System.RegularExpressions,
  System.TypInfo,
  MVCFramework,
  MVCFramework.Nullables,
  MVCFramework.Serializer.Abstract,
  MVCFramework.Serializer.Commons,
  Swag.Doc.Path.Operation.Response,
  Json.Schema.Field.Numbers,
  Json.Schema.Field.Strings,
  Json.Schema.Field.Arrays,
  Json.Schema.Field.DateTimes,
  Json.Schema.Field.Enums,
  Json.Schema.Field.Booleans;

function GetRecordType(const aQualifiedName: String): TRttiType;
var
  lContext: TRttiContext;
begin
  lContext := TRttiContext.Create;
  try
    result := lContext.FindType(aQualifiedName);
  finally
    lContext.Free;
  end;
end;

{ TSwaggerUtils }

class procedure TMVCSwagger.AddRequestModelDefinition(
  const aSwagReqParam: TSwagRequestParameter; const aParamSchemaClass: TClass;
  const aSwagDefinitions: TObjectList<TSwagDefinition>;
  const aComparer: IComparer<TSwagDefinition>;
  const aMVCSwagParamType: TMVCSwagParamType;
  const aRecordType: TRttiType);
var
  lClassName: String;
  lSwagDef: TSwagDefinition;
  lSwagDefinition: TSwagDefinition;
  lIndex: Integer;
begin
  if Assigned(aRecordType) then
    lClassName := aRecordType.Name
  else
    lClassName := aParamSchemaClass.ClassName;
  if lClassName.ToUpper.StartsWith('T') then
    lClassName := lClassName.Remove(0, 1);

  aSwagDefinitions.Sort(aComparer);
  lSwagDef := TSwagDefinition.Create;
  try
    lSwagDef.Name := lClassName;
    if not aSwagDefinitions.BinarySearch(lSwagDef, lIndex, aComparer) then
    begin
      lSwagDefinition := TSwagDefinition.Create;
      lSwagDefinition.Name := lClassName;
      if Assigned(aRecordType) then
        lSwagDefinition.JsonSchema := ExtractJsonSchemaFromRecord(
          aRecordType.QualifiedName,
          aMVCSwagParamType = ptArray)
      else
        lSwagDefinition.JsonSchema := ExtractJsonSchemaFromClass(
          aParamSchemaClass,
          aMVCSwagParamType = ptArray);
      aSwagDefinitions.Add(lSwagDefinition);
    end;
  finally
    lSwagDef.Free;
  end;
  aSwagReqParam.Schema.Name := lClassName;
end;

class function TMVCSwagger.ApplyModelName(const Value, Singular,
  Plural: String): String;
begin
  Result := StringReplace(Value, TSwaggerConst.SINGULAR_MODEL_NAME, Singular, [rfReplaceAll]);
  Result := StringReplace(Result, TSwaggerConst.PLURAL_MODEL_NAME, Plural, [rfReplaceAll]);
end;

class constructor TMVCSwagger.Create;
begin
  fRttiContext := TRttiContext.Create;
end;

class function TMVCSwagger.RttiTypeToSwagType(const aRttiType: TRttiType): TSwagTypeParameter;
begin
  case aRttiType.TypeKind of
    tkInteger, tkInt64:
      Result := stpInteger;
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString:
      Result := stpString;
    tkFloat:
      if (aRttiType.Handle = TypeInfo(TDateTime)) or (aRttiType.Handle = TypeInfo(TDate)) or
        (aRttiType.Handle = TypeInfo(TTime)) then
        Result := stpString
      else
        Result := stpNumber;
    tkEnumeration:
      if aRttiType.Handle = TypeInfo(Boolean) then
        Result := stpBoolean
      else
        Result := stpArray;
    tkRecord:
      if aRttiType.Handle = TypeInfo(TGUID) then
        Result := stpString
      else
        Result := stpNotDefined;
  else
    Result := stpNotDefined;
  end;
end;

class destructor TMVCSwagger.Destroy;
begin
  fRttiContext.Free;
end;

class function TMVCSwagger.TypeIsEnumerable(const aRttiType: TRttiType): Boolean;
begin
  Result := aRttiType.GetMethod('GetEnumerator') <> nil;
end;

type
  TFieldSchemaDefinition = record
    SchemaFieldType: TMVCSwagSchemaType;
    FieldName: string;
    Description: string;
    Required: Boolean;
    Nullable: Boolean;
    MinLength: Integer;
    MaxLength: Integer;
    class function Create: TFieldSchemaDefinition; static; inline;
  end;

class procedure TMVCSwagger.ExtractJsonSchemaFromClass(const aJsonFieldRoot: TJsonFieldObject; const aClass: TClass);
var
  lFieldSchemaDef: TFieldSchemaDefinition;
  lObjType: TRttiType;
  lProp: TRttiProperty;
  lSkipProp: Boolean;
  lAttr: TCustomAttribute;
  lEnumAttr: MVCEnumSerializationAttribute;
  lJSFieldAttr: MVCSwagJSONSchemaFieldAttribute;
  lJsonFieldClass: TJsonFieldClass;
  lJsonField: TJsonField;
  lJsonFieldType: TRttiType;
  lJsonFieldObject: TJsonFieldObject;
  lAbstractSer: TMVCAbstractSerializer;
  lClass: TClass;
  lEnumSerType: TMVCEnumSerializationType;
  lEnumMappedValues: TList<string>;
  I: Integer;
  lInheritsFromTInterfacedOb: Boolean;
begin
  lObjType := fRttiContext.GetType(aClass);

  lInheritsFromTInterfacedOb := aClass.InheritsFrom(TInterfacedObject);
  for lProp in lObjType.GetProperties do
  begin
    lSkipProp := False;
    lFieldSchemaDef := TFieldSchemaDefinition.Create;

    if lInheritsFromTInterfacedOb and SameText(lProp.Name, 'RefCount') then
      Continue;

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
        lFieldSchemaDef.MinLength := lJSFieldAttr.MinLength;
        lFieldSchemaDef.MaxLength := lJSFieldAttr.MaxLength;
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
      lAbstractSer := TMVCAbstractSerializer.Create;
      try
        if lAbstractSer.GetObjectTypeOfGenericList(lProp.PropertyType.Handle, lJsonFieldType) then
        begin
          if lJsonFieldType.IsInstance then
          begin
            lClass := lJsonFieldType.AsInstance.MetaclassType;
            lJsonFieldObject := TJsonFieldObject.Create;
            ExtractJsonSchemaFromClass(lJsonFieldObject, lClass);
            TJsonFieldArray(lJsonField).ItemFieldType := lJsonFieldObject;
          end
          else
          begin
            lJsonFieldClass := GetJsonFieldClass(TypeKindToMVCSwagSchemaType(lJsonFieldType));
            if Assigned(lJsonFieldClass) then
              TJsonFieldArray(lJsonField).ItemFieldType := lJsonFieldClass.Create;
          end;
        end;
      finally
        lAbstractSer.Free;
      end;
    end
    else if (lJsonField is TJsonFieldArray) and (lProp.PropertyType is TRttiDynamicArrayType) then
    begin
      lJsonFieldClass := GetJsonFieldClass(TypeKindToMVCSwagSchemaType(
        TRttiDynamicArrayType(lProp.PropertyType).ElementType));
      if Assigned(lJsonFieldClass) then
        TJsonFieldArray(lJsonField).ItemFieldType := lJsonFieldClass.Create;
    end
    else if lJsonField is TJsonFieldEnum then /// Extract enumerator information
    begin
      lEnumSerType := estEnumName;
      lEnumMappedValues := nil;
      if TMVCSerializerHelper.HasAttribute<MVCEnumSerializationAttribute>(lProp, lEnumAttr) then
      begin
        lEnumSerType := lEnumAttr.SerializationType;
        lEnumMappedValues := lEnumAttr.MappedValues;
      end;
      case lEnumSerType of
        estEnumName, estEnumOrd :
          begin
            if lEnumSerType = estEnumName then
              TJsonFieldEnum(lJsonField).EnumType := etString
            else
              TJsonFieldEnum(lJsonField).EnumType := etNumber;

            for I := lProp.PropertyType.AsOrdinal.MinValue to lProp.PropertyType.AsOrdinal.MaxValue do
            begin
              TJsonFieldEnum(lJsonField).AddItem(I, GetEnumName(lProp.PropertyType.Handle, I));
            end;
          end;
        estEnumMappedValues:
          begin
            TJsonFieldEnum(lJsonField).EnumType := etString;
            TJsonFieldEnum(lJsonField).AddItems(lEnumMappedValues.ToArray);
          end;
      end;
    end
    else if lJsonField is TJsonFieldString then
    begin
      TJsonFieldString(lJsonField).MinLength := lFieldSchemaDef.MinLength;
      TJsonFieldString(lJsonField).MaxLength := lFieldSchemaDef.MaxLength;
    end;

    lJsonField.Name := lFieldSchemaDef.FieldName;
    lJsonField.Required := lFieldSchemaDef.Required;
    lJsonField.Nullable := lFieldSchemaDef.Nullable;
    if not lFieldSchemaDef.Description.IsEmpty then
      TJsonFieldInteger(lJsonField).Description := lFieldSchemaDef.Description;

    aJsonFieldRoot.AddField(lJsonField);
  end;
end;

class function TMVCSwagger.ExtractJsonSchemaFromClass(const aClass: TClass; const aIsArray: Boolean): TJSONObject;
var
  lJsonSchema: TJsonField;
  lJsonRoot: TJsonFieldObject;
begin
  if aIsArray then
  begin
    lJsonSchema := TJsonFieldArray.Create
  end
  else
  begin
    lJsonSchema := TJsonFieldObject.Create;
  end;
  try
    if aIsArray then
    begin
      lJsonRoot := TJsonFieldObject.Create;
      TJsonFieldArray(lJsonSchema).ItemFieldType := lJsonRoot;
      TJsonFieldArray(lJsonSchema).Name := 'items';
    end
    else
    begin
      lJsonRoot := lJsonSchema as TJsonFieldObject;
    end;

    ExtractJsonSchemaFromClass(lJsonRoot, aClass);
    Result := lJsonSchema.ToJsonSchema;
  finally
    lJsonSchema.Free;
  end;
end;

class function TMVCSwagger.ExtractJsonSchemaFromRecord(const aTypeName: String; const aIsArray: Boolean): TJSONObject;
var
  lJsonSchema: TJsonField;
  lJsonRoot: TJsonFieldObject;
  lContext: TRttiContext;
  lTypeInfo: TRttiType;
begin
  if aIsArray then
  begin
    lJsonSchema := TJsonFieldArray.Create
  end
  else
  begin
    lJsonSchema := TJsonFieldObject.Create;
  end;
  try
    if aIsArray then
    begin
      lJsonRoot := TJsonFieldObject.Create;
      TJsonFieldArray(lJsonSchema).ItemFieldType := lJsonRoot;
      TJsonFieldArray(lJsonSchema).Name := 'items';
    end
    else
    begin
      lJsonRoot := lJsonSchema as TJsonFieldObject;
    end;
    lContext := TRttiContext.Create;
    try
      lTypeInfo := lContext.FindType(aTypeName);
    finally
      lContext.Free;
    end;
    if lTypeInfo = nil then
      raise EMVCSWAGError.Create(HTTP_STATUS.InternalServerError,
            'SWAGGER Definition Error: Type "'+aTypeName+'" not found. Is the unit name included to create a fully '+
            'qualified name for "'+aTypeName+'"?');
    ExtractJsonSchemaFromRecord(lTypeInfo, lJsonRoot);
    Result := lJsonSchema.ToJsonSchema;
  finally
    lJsonSchema.Free;
  end;
end;

class procedure TMVCSwagger.ExtractJsonSchemaFromRecord(const aRttiType: TRttiType; const aJsonFieldRoot: TJsonFieldObject);
var
  lFieldSchemaDef: TFieldSchemaDefinition;
  lField: TRttiField;
  lSkipProp: Boolean;
  lAttr: TCustomAttribute;
  lEnumAttr: MVCEnumSerializationAttribute;
  lJSFieldAttr: MVCSwagJSONSchemaFieldAttribute;
  lJsonFieldClass: TJsonFieldClass;
  lJsonField: TJsonField;
  lJsonFieldType: TRttiType;
  lJsonFieldObject: TJsonFieldObject;
  lAbstractSer: TMVCAbstractSerializer;
  lClassType: TRttiType;
  lEnumSerType: TMVCEnumSerializationType;
  lEnumMappedValues: TList<string>;
  I: Integer;
begin
  for lField in aRttiType.GetFields do
  begin
    lSkipProp := False;
    lFieldSchemaDef := TFieldSchemaDefinition.Create;

    for lAttr in lField.GetAttributes do
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
        lFieldSchemaDef.MinLength := lJSFieldAttr.MinLength;
        lFieldSchemaDef.MaxLength := lJSFieldAttr.MaxLength;
        Break;
      end;
    end;

    if lSkipProp then
      Continue;

    if lFieldSchemaDef.SchemaFieldType = stUnknown then
    begin
      lFieldSchemaDef.SchemaFieldType := TypeKindToMVCSwagSchemaType(lField.FieldType);
      lFieldSchemaDef.FieldName := TMVCSerializerHelper.GetKeyName(lField, lField.FieldType);
    end;

    lJsonFieldClass := GetJsonFieldClass(lFieldSchemaDef.SchemaFieldType);
    if not Assigned(lJsonFieldClass) then
      Continue;

    lJsonField := lJsonFieldClass.Create;

    if (lJsonField is TJsonFieldObject) and (not TypeIsEnumerable(lField.FieldType)) then
    begin
      ExtractJsonSchemaFromRecord(lField.FieldType, (lJsonField as TJsonFieldObject));
    end;

    if (lJsonField is TJsonFieldArray) and TypeIsEnumerable(lField.FieldType) then
    begin
      lAbstractSer := TMVCAbstractSerializer.Create;
      try
        if lAbstractSer.GetObjectTypeOfGenericList(lField.FieldType.Handle, lJsonFieldType) then
        begin
          if lJsonFieldType.IsInstance then
          begin
            lClassType := lJsonFieldType;
            lJsonFieldObject := TJsonFieldObject.Create;
            ExtractJsonSchemaFromClass(lJsonFieldObject, lClassType.ClassType);
            TJsonFieldArray(lJsonField).ItemFieldType := lJsonFieldObject;
          end
          else
          begin
            lJsonFieldClass := GetJsonFieldClass(TypeKindToMVCSwagSchemaType(lJsonFieldType));
            if Assigned(lJsonFieldClass) then
              TJsonFieldArray(lJsonField).ItemFieldType := lJsonFieldClass.Create;
          end;
        end;
      finally
        lAbstractSer.Free;
      end;
    end
    else if (lJsonField is TJsonFieldArray) and (lField.FieldType is TRttiDynamicArrayType) then
    begin
      lJsonFieldClass := GetJsonFieldClass(TypeKindToMVCSwagSchemaType(
        TRttiDynamicArrayType(lField.FieldType).ElementType));
      if Assigned(lJsonFieldClass) then
        TJsonFieldArray(lJsonField).ItemFieldType := lJsonFieldClass.Create;
    end
    else if lJsonField is TJsonFieldEnum then /// Extract enumerator information
    begin
      lEnumSerType := estEnumName;
      lEnumMappedValues := nil;
      if TMVCSerializerHelper.HasAttribute<MVCEnumSerializationAttribute>(lField, lEnumAttr) then
      begin
        lEnumSerType := lEnumAttr.SerializationType;
        lEnumMappedValues := lEnumAttr.MappedValues;
      end;
      case lEnumSerType of
        estEnumName, estEnumOrd :
          begin
            if lEnumSerType = estEnumName then
              TJsonFieldEnum(lJsonField).EnumType := etString
            else
              TJsonFieldEnum(lJsonField).EnumType := etNumber;

            for I := lField.FieldType.AsOrdinal.MinValue to lField.FieldType.AsOrdinal.MaxValue do
            begin
              TJsonFieldEnum(lJsonField).AddItem(I, GetEnumName(lField.FieldType.Handle, I));
            end;
          end;
        estEnumMappedValues:
          begin
            TJsonFieldEnum(lJsonField).EnumType := etString;
            TJsonFieldEnum(lJsonField).AddItems(lEnumMappedValues.ToArray);
          end;
      end;
    end
    else if lJsonField is TJsonFieldString then
    begin
      TJsonFieldString(lJsonField).MinLength := lFieldSchemaDef.MinLength;
      TJsonFieldString(lJsonField).MaxLength := lFieldSchemaDef.MaxLength;
    end;

    lJsonField.Name := lFieldSchemaDef.FieldName;
    lJsonField.Required := lFieldSchemaDef.Required;
    lJsonField.Nullable := lFieldSchemaDef.Nullable;
    if not lFieldSchemaDef.Description.IsEmpty then
    begin
      TJsonFieldInteger(lJsonField).Description := lFieldSchemaDef.Description;
    end;

    aJsonFieldRoot.AddField(lJsonField);
  end;
end;

class procedure TMVCSwagger.FillOperationSummary(
  const aSwagPathOperation: TSwagPathOperation;
  const aMethod: TRttiMethod; const aSwagDefinitions: TObjectList<TSwagDefinition>;
  const aHTTPMethod: TMVCHTTPMethodType;
  const aControllerDefaultModel: TClass;
  const aControllerDefaultModelSingularName: String;
  const aControllerDefaultModelPluralName: String;
  const aControllerDefaultSummaryTags: TArray<String>);
var
  lAttr: TCustomAttribute;
  lSwagResponse: TSwagResponse;
  lSwagResponsesAttr: MVCSwagResponsesAttribute;
  lComparer: IComparer<TSwagDefinition>;
  lSwagDefinition: TSwagDefinition;
  lSwagDef: TSwagDefinition;
  lClassName: string;
  lIndex: Integer;
  lJsonSchema: TJsonFieldArray;
  lModelClass: TClass;
begin
  for lAttr in aMethod.GetAttributes do
  begin
    if lAttr is MVCSwagSummaryAttribute then
    begin
      if MVCSwagSummaryAttribute(lAttr).fTags = TSwaggerConst.USE_DEFAULT_SUMMARY_TAGS then
      begin
        if Length(aControllerDefaultSummaryTags) = 0 then
        begin
          raise EMVCSWAGError.Create(HTTP_STATUS.InternalServerError,
            Format('SWAGGER Definition Error: Action "%s" uses "USE_DEFAULT_SUMMARY_TAG" but its controller "%s" doesn''t define a "MVCSWAGDefaultSummaryTags" attribute',
              [aMethod.ToString, aMethod.Parent.ToString]));
        end;

        aSwagPathOperation.Tags.AddRange(aControllerDefaultSummaryTags);
      end
      else
      begin
        aSwagPathOperation.Tags.AddRange(MVCSwagSummaryAttribute(lAttr).GetTags);
      end;
      aSwagPathOperation.Description := ApplyModelName(
          MVCSwagSummaryAttribute(lAttr).Description,
          aControllerDefaultModelSingularName,
          aControllerDefaultModelPluralName);
      aSwagPathOperation.OperationID :=
          //GetEnumName(TypeInfo(TMVCHTTPMethodType), Ord(aHTTPMethod)).Substring(4) + '.' +
          ApplyModelName(
            MVCSwagSummaryAttribute(lAttr).OperationID,
            aControllerDefaultModelSingularName,
            aControllerDefaultModelPluralName);
      if MVCSwagSummaryAttribute(lAttr).OperationID.IsEmpty then
      begin
        aSwagPathOperation.OperationID :=
          //GetEnumName(TypeInfo(TMVCHTTPMethodType), Ord(aHTTPMethod)).Substring(4) + '.' +
          aMethod.Parent.QualifiedName + '_' + aMethod.Name;
      end;
      // dt [2019-10-31]
      aSwagPathOperation.Deprecated := MVCSwagSummaryAttribute(lAttr).Deprecated;
    end;
    if lAttr is MVCConsumesAttribute then
    begin
      aSwagPathOperation.Consumes.Add(MVCConsumesAttribute(lAttr).Value)
    end;
    if lAttr is MVCProducesAttribute then
    begin
      aSwagPathOperation.Produces.Add(MVCProducesAttribute(lAttr).Value)
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
      else if ((Assigned(lSwagResponsesAttr.JsonSchemaClass)) or (Assigned(lSwagResponsesAttr.RecordType))) then
      begin
        lComparer := TDelegatedComparer<TSwagDefinition>.Create(
          function(const Left, Right: TSwagDefinition): Integer
          begin
            Result := CompareText(Left.Name, Right.Name);
          end);

        if lSwagResponsesAttr.JsonSchemaClass = SWAGUseDefaultControllerModel then
        begin
          if not Assigned(aControllerDefaultModel) then
          begin
            raise EMVCSWAGError.Create(HTTP_STATUS.InternalServerError,
              Format('SWAGGER Definition Error: Action "%s" uses "SWAGUseDefaultControllerModel" but its controller "%s" doesn''t define a "MVCSWAGDefaultModel" attribute',
                [aMethod.ToString, aMethod.Parent.ToString]));
          end;
          lModelClass := aControllerDefaultModel;
        end
        else
        begin
          lModelClass := lSwagResponsesAttr.JsonSchemaClass;
        end;

        if Assigned(lSwagResponsesAttr.RecordType) then
          lClassName := lSwagResponsesAttr.RecordType.Name
        else
          lClassName := lModelClass.ClassName;
        if lClassName.ToUpper.StartsWith('T') then
          lClassName := lClassName.Remove(0, 1);
        aSwagDefinitions.Sort(lComparer);
        lSwagDef := TSwagDefinition.Create;
        try
          lSwagDef.Name := lClassName;
          if not aSwagDefinitions.BinarySearch(lSwagDef, lIndex, lComparer) then
          begin
            lSwagDefinition := TSwagDefinition.Create;
            lSwagDefinition.Name := lClassName;
            if Assigned(lSwagResponsesAttr.RecordType) then
              lSwagDefinition.JsonSchema := ExtractJsonSchemaFromRecord(lSwagResponsesAttr.RecordType.QualifiedName, False)
            else
              lSwagDefinition.JsonSchema := ExtractJsonSchemaFromClass(lModelClass, False);
            aSwagDefinitions.Add(lSwagDefinition);
          end;
        finally
          lSwagDef.Free;
        end;
        if lSwagResponsesAttr.IsArray then
        begin
          lJsonSchema := TJsonFieldArray.Create;
          try
            lJsonSchema.Name := 'items';
            lJsonSchema.ItemFieldType := TJsonFieldObject.Create;
            TJsonFieldObject(lJsonSchema.ItemFieldType).Ref := lClassName;
            lSwagResponse.Schema.JsonSchema := lJsonSchema.ToJsonSchema;
          finally
            lJsonSchema.Free;
          end;
        end
        else
        begin
          lSwagResponse.Schema.Name := lClassName;
        end;
      end;
      aSwagPathOperation.Responses.Add(lSwagResponse.StatusCode, lSwagResponse);
    end;
  end;

  if aSwagPathOperation.Tags.Count = 0 then
    aSwagPathOperation.Tags.Add(aMethod.Parent.QualifiedName);

  if aSwagPathOperation.Produces.Count <= 0 then
    aSwagPathOperation.Produces.Add(TMVCMediaType.APPLICATION_JSON);

  if aSwagPathOperation.Responses.Count <= 0 then
  begin {add default responses}
    lSwagResponse := TSwagResponse.Create;
    lSwagResponse.StatusCode := IntToStr(HTTP_STATUS.OK);
    lSwagResponse.Description := 'OK';
    aSwagPathOperation.Responses.Add(lSwagResponse.StatusCode, lSwagResponse);

    lSwagResponse := TSwagResponse.Create;
    lSwagResponse.StatusCode := IntToStr(HTTP_STATUS.InternalServerError);
    lSwagResponse.Description := 'Internal server error';
    aSwagPathOperation.Responses.Add(lSwagResponse.StatusCode, lSwagResponse);
  end;
end;

class function TMVCSwagger.GetJsonFieldClass(const aSchemaFieldType: TMVCSwagSchemaType): TJsonFieldClass;
begin
  case aSchemaFieldType of
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

class function TMVCSwagger.TypeKindToMVCSwagSchemaType(aPropType: TRttiType): TMVCSwagSchemaType;
begin
  Result := stUnknown;

  if aPropType.TypeKind = tkUnknown then
    Exit;

  case aPropType.TypeKind of
    tkClass:
      begin
        if (aPropType.Handle = TypeInfo(TStream)) or (aPropType.Handle = TypeInfo(TMemoryStream)) or
          (aPropType.Handle = TypeInfo(TStringStream)) then
          Result := stString
        else if aPropType.Handle = TypeInfo(TMVCStringDictionary) then
          Result := stObject
        else if TypeIsEnumerable(aPropType) then
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
        if aPropType.Handle = TypeInfo(TGUID) then
          Result := stGuid
        else if aPropType.Handle = TypeInfo(NullableString) then
          Result := stString
        else if aPropType.Handle = TypeInfo(NullableCurrency) then
          Result := stNumber
        else if aPropType.Handle = TypeInfo(NullableBoolean) then
          Result := stBoolean
        else if aPropType.Handle = TypeInfo(NullableTDate) then
          Result := stDate
        else if aPropType.Handle = TypeInfo(NullableTTime) then
          Result := stTime
        else if aPropType.Handle = TypeInfo(NullableTDateTime) then
          Result := stDateTime
        else if aPropType.Handle = TypeInfo(NullableSingle) then
          Result := stNumber
        else if aPropType.Handle = TypeInfo(NullableDouble) then
          Result := stNumber
        else if aPropType.Handle = TypeInfo(NullableExtended) then
          Result := stNumber
        else if aPropType.Handle = TypeInfo(NullableInt16) then
          Result := stInteger
        else if aPropType.Handle = TypeInfo(NullableUInt16) then
          Result := stInteger
        else if aPropType.Handle = TypeInfo(NullableInt32) then
          Result := stInteger
        else if aPropType.Handle = TypeInfo(NullableUInt32) then
          Result := stInteger
        else if aPropType.Handle = TypeInfo(NullableInt64) then
          Result := stInt64
        else if aPropType.Handle = TypeInfo(NullableUInt64) then
          Result := stInt64
        else if aPropType.IsRecord then
          Result := stObject;
      end;
    tkInteger:
      Result := stInteger;
    tkInt64:
      Result := stInt64;
    tkEnumeration:
      begin
        if aPropType.Handle = TypeInfo(Boolean) then
          Result := stBoolean
        else
          Result := stEnumeration;
      end;
    tkFloat:
      begin
        if aPropType.Handle = TypeInfo(TDateTime) then
          Result := stDateTime
        else if aPropType.Handle = TypeInfo(TDate) then
          Result := stDate
        else if aPropType.Handle = TypeInfo(TTime) then
          Result := stTime
        else
          Result := stNumber;
      end;
  end;
end;

class function TMVCSwagger.GetJWTAuthenticationPath(const aJWTUrlSegment: string;
  aUserNameHeaderName, aPasswordHeaderName: string): TSwagPath;
var
  lSwagPathOp: TSwagPathOperation;
  lSwagResponse: TSwagResponse;
  lSwagParam: TSwagRequestParameter;
begin
  lSwagPathOp := TSwagPathOperation.Create;
  lSwagPathOp.Tags.Add(JWT_AUTHENTICATION_TAG);
  lSwagPathOp.Operation := ohvPost;
  lSwagPathOp.Security.Add(SECURITY_BASIC_NAME);
  lSwagPathOp.Description := 'Create JSON Web Token';
  lSwagPathOp.Produces.Add(TMVCMediaType.APPLICATION_JSON);
  lSwagParam := TSwagRequestParameter.Create;
  lSwagParam.Name := aUserNameHeaderName;
  lSwagParam.TypeParameter := stpString;
  lSwagParam.Required := False;
  lSwagParam.InLocation := rpiHeader;
  lSwagPathOp.Parameters.Add(lSwagParam);

  lSwagParam := TSwagRequestParameter.Create;
  lSwagParam.Name := aPasswordHeaderName;
  lSwagParam.TypeParameter := stpString;
  lSwagParam.Required := False;
  lSwagParam.InLocation := rpiHeader;
  lSwagPathOp.Parameters.Add(lSwagParam);

  lSwagResponse := TSwagResponse.Create;
  lSwagResponse.StatusCode := IntToStr(HTTP_STATUS.Unauthorized);
  lSwagResponse.Description := 'Invalid authorization type';
  lSwagPathOp.Responses.Add(lSwagResponse.StatusCode, lSwagResponse);

  lSwagResponse := TSwagResponse.Create;
  lSwagResponse.StatusCode := IntToStr(HTTP_STATUS.Forbidden);
  lSwagResponse.Description := 'Forbidden';
  lSwagPathOp.Responses.Add(lSwagResponse.StatusCode, lSwagResponse);

  lSwagResponse := TSwagResponse.Create;
  lSwagResponse.StatusCode := IntToStr(HTTP_STATUS.InternalServerError);
  lSwagResponse.Description := 'Internal server error';
  lSwagPathOp.Responses.Add(lSwagResponse.StatusCode, lSwagResponse);

  lSwagResponse := TSwagResponse.Create;
  lSwagResponse.StatusCode := IntToStr(HTTP_STATUS.OK);
  lSwagResponse.Description := 'OK';
  lSwagResponse.Schema.JsonSchema := TJSONObject.ParseJSONValue(JWT_JSON_SCHEMA) as TJSONObject;
  lSwagPathOp.Responses.Add(lSwagResponse.StatusCode, lSwagResponse);

  Result := TSwagPath.Create;
  Result.Uri := aJWTUrlSegment;
  Result.Operations.Add(lSwagPathOp);
end;

class function TMVCSwagger.GetMVCSwagParamsFromMethod(const aMethod: TRttiMethod): TArray<MVCSwagParamAttribute>;
var
  lAttr: TCustomAttribute;
begin
  SetLength(Result, 0);
  for lAttr in aMethod.GetAttributes do
  begin
    if lAttr is MVCSwagParamAttribute then
    begin
      Insert([MVCSwagParamAttribute(lAttr)], Result, High(Result));
    end;
  end;
end;

class function TMVCSwagger.GetParamsFromMethod(const aResourcePath: string; const aMethod: TRttiMethod;
  const aSwagDefinitions: TObjectList<TSwagDefinition>;
  const aControllerDefaultModelClass: TClass;
  const aControllerDefaultModelSingularName: String;
  const aControllerDefaultModelPluralName: String): TArray<TSwagRequestParameter>;

  function TryGetMVCPathParamByName(const AParams: TArray<MVCSwagParamAttribute>; const aParamName: string;
    out AMVCParam: MVCSwagParamAttribute; out AIndex: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    AMVCParam := nil;
    AIndex := - 1;
    for I := Low(AParams) to High(AParams) do
      if SameText(AParams[I].ParamName, aParamName) and (AParams[I].ParamLocation = plPath) then
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
  lSwagReqParam: TSwagRequestParameter;
  lMVCSwagParams: TArray<MVCSwagParamAttribute>;
  lMVCParam: MVCSwagParamAttribute;
  lIndex: Integer;
  I: Integer;
  lComparer: IComparer<TSwagDefinition>;
  lParamSchemaClass: TClass;
begin
  lComparer := TDelegatedComparer<TSwagDefinition>.Create(
    function(const Left, Right: TSwagDefinition): Integer
    begin
      Result := CompareText(Left.Name, Right.Name);
    end);

  lMVCSwagParams := GetMVCSwagParamsFromMethod(aMethod);
  if aMethod.Name.Contains('Create') then
  begin
    SetLength(Result, 0); //just for breakpoint
  end;
  SetLength(Result, 0);

  // Path parameters
  lMatches := TRegEx.Matches(aResourcePath, '({)([\w_]+)(})', [roIgnoreCase, roMultiLine]);
  for lMatch in lMatches do
  begin
    lParamName := lMatch.Groups[2].Value;
    for lMethodParam in aMethod.GetParameters do
    begin
      if SameText(lMethodParam.Name, lParamName) then
      begin
        lSwagReqParam := TSwagRequestParameter.Create;

        if TryGetMVCPathParamByName(lMVCSwagParams, lParamName, lMVCParam, lIndex) then
        begin
          lSwagReqParam.Name := {lParamName} ApplyModelName(lParamName, aControllerDefaultModelSingularName, aControllerDefaultModelPluralName);
          lSwagReqParam.InLocation := MVCParamLocationToSwagRequestParamInLocation(lMVCParam.ParamLocation);
          lSwagReqParam.Required := lMVCParam.Required;
          lSwagReqParam.Default := lMVCParam.DefaultValue;
          lSwagReqParam.Enum.Text := string.Join(sLineBreak, lMVCParam.EnumValues);
          lSwagReqParam.TypeParameter := MVCParamTypeToSwagTypeParameter(lMVCParam.ParamType);
          lSwagReqParam.Description := ApplyModelName(lMVCParam.ParamDescription, aControllerDefaultModelSingularName, aControllerDefaultModelPluralName);
          if not lMVCParam.JsonSchema.IsEmpty then
          begin
            lSwagReqParam.Schema.JsonSchema := TJSONObject.ParseJSONValue(lMVCParam.JsonSchema) as TJSONObject
          end
          else if Assigned(lMVCParam.JsonSchemaClass) then
          begin
            AddRequestModelDefinition(
              lSwagReqParam,
              lMVCParam.JsonSchemaClass,
              aSwagDefinitions,
              lComparer,
              lMVCParam.ParamType,
              lMVCParam.RecordType);
          end;

          Delete(lMVCSwagParams, lIndex, 1);
        end
        else
        begin
          lSwagReqParam.Name := {lParamName} ApplyModelName(lParamName, aControllerDefaultModelSingularName, aControllerDefaultModelPluralName);
          lSwagReqParam.InLocation := rpiPath;
          lSwagReqParam.Required := True;
          lSwagReqParam.TypeParameter := RttiTypeToSwagType(lMethodParam.ParamType);
        end;
        Insert([lSwagReqParam], Result, High(Result));
      end;
    end;
  end;

  // Other parameters
  for I := Low(lMVCSwagParams) to High(lMVCSwagParams) do
  begin
    lSwagReqParam := TSwagRequestParameter.Create;
    lSwagReqParam.Name := {lMVCSwagParams[I].ParamName} ApplyModelName(lMVCSwagParams[I].ParamName, aControllerDefaultModelSingularName, aControllerDefaultModelPluralName);
    lSwagReqParam.InLocation := MVCParamLocationToSwagRequestParamInLocation(lMVCSwagParams[I].ParamLocation);
    lSwagReqParam.Required := lMVCSwagParams[I].Required;
    lSwagReqParam.Default := lMVCSwagParams[I].DefaultValue;
    lSwagReqParam.Enum.Text := string.Join(sLineBreak, lMVCSwagParams[I].EnumValues);
    lSwagReqParam.TypeParameter := MVCParamTypeToSwagTypeParameter(lMVCSwagParams[I].ParamType);
    lSwagReqParam.Description := ApplyModelName(lMVCSwagParams[I].ParamDescription, aControllerDefaultModelSingularName, aControllerDefaultModelPluralName);
    if not lMVCSwagParams[I].JsonSchema.IsEmpty then
    begin
      lSwagReqParam.Schema.JsonSchema := TJSONObject.ParseJSONValue(lMVCSwagParams[I].JsonSchema) as TJSONObject
    end
    else if Assigned(lMVCSwagParams[I].JsonSchemaClass) or Assigned(lMVCSwagParams[I].RecordType) then
    begin
      if lMVCSwagParams[I].JsonSchemaClass = SWAGUseDefaultControllerModel then
      begin
        if not Assigned(aControllerDefaultModelClass) then
        begin
          raise EMVCSWAGError.Create(HTTP_STATUS.InternalServerError,
            Format('SWAGGER Definition Error: Action "%s" uses "SWAGUseDefaultControllerModel" but its controller "%s" doesn''t define a "MVCSWAGDefaultModel" attribute',
              [aMethod.ToString, aMethod.Parent.ToString]));
        end;
        lParamSchemaClass := aControllerDefaultModelClass;
//        lSwagParam.Schema.JsonSchema := ExtractJsonSchemaFromClass(aControllerDefaultModelClass,
//          lMVCSwagParams[I].ParamType = ptArray);
      end
      else
      begin
        lParamSchemaClass := lMVCSwagParams[I].JsonSchemaClass;
//        lSwagParam.Schema.JsonSchema := ExtractJsonSchemaFromClass(lMVCSwagParams[I].JsonSchemaClass,
//          lMVCSwagParams[I].ParamType = ptArray);
      end;
      AddRequestModelDefinition(
        lSwagReqParam,
        lParamSchemaClass,
        aSwagDefinitions,
        lComparer,
        lMVCSwagParams[I].ParamType,
        lMVCSwagParams[i].RecordType);
    end;
    Insert([lSwagReqParam], Result, High(Result));
  end;
end;

class function TMVCSwagger.MethodRequiresAuthentication(const aMethod: TRttiMethod; const aType: TRttiType;
  out aAuthenticationTypeName: string): Boolean;
var
  lAttr: TCustomAttribute;
begin
  Result := False;
  aAuthenticationTypeName := '';

  for lAttr in aMethod.GetAttributes do
    if lAttr is MVCRequiresAuthenticationAttribute then
    begin
      aAuthenticationTypeName := SECURITY_BEARER_NAME;
      Exit(True);
    end
    else if lAttr is MVCSwagAuthenticationAttribute then
    begin
      case MVCSwagAuthenticationAttribute(lAttr).AuthenticationType of
        atBasic:
          aAuthenticationTypeName := SECURITY_BASIC_NAME;
        atJsonWebToken:
          aAuthenticationTypeName := SECURITY_BEARER_NAME;
      end;
      Exit(True);
    end;

  for lAttr in aType.GetAttributes do
    if lAttr is MVCRequiresAuthenticationAttribute then
    begin
      aAuthenticationTypeName := SECURITY_BEARER_NAME;
      Exit(True);
    end
    else if lAttr is MVCSwagAuthenticationAttribute then
    begin
      case MVCSwagAuthenticationAttribute(lAttr).AuthenticationType of
        atBasic:
          aAuthenticationTypeName := SECURITY_BASIC_NAME;
        atJsonWebToken:
          aAuthenticationTypeName := SECURITY_BEARER_NAME;
      end;
      Exit(True);
    end;
end;

class function TMVCSwagger.MVCHttpMethodToSwagPathOperation(const aMVCHTTPMethod: TMVCHTTPMethodType)
  : TSwagPathTypeOperation;
begin
  case aMVCHTTPMethod of
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

class function TMVCSwagger.MVCParamLocationToSwagRequestParamInLocation(const aMVCSwagParamLocation
  : TMVCSwagParamLocation): TSwagRequestParameterInLocation;
begin
  case aMVCSwagParamLocation of
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

class function TMVCSwagger.MVCParamTypeToSwagTypeParameter(const aMVSwagParamType: TMVCSwagParamType)
  : TSwagTypeParameter;
begin
  case aMVSwagParamType of
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

class function TMVCSwagger.MVCPathToSwagPath(const aResourcePath: string): string;
begin
  Result := TRegEx.Replace(aResourcePath, '(\([($])([\w_]+)([)])', '{\2}', [roIgnoreCase, roMultiLine]);
end;

{ MVCSwagSummary }

constructor MVCSwagSummaryAttribute.Create(const aTags, aDescription: string; const aOperationId: string;
  aDeprecated: Boolean);
begin
  fTags := aTags;
  fDescription := aDescription;
  fOperationID := aOperationId;
  fDeprecated := aDeprecated;
end;

function MVCSwagSummaryAttribute.GetTags: TArray<string>;
begin
  Result := fTags.Split([',']);
end;

{ MVCSwagResponsesAttribute }

constructor MVCSwagResponsesAttribute.Create(const aStatusCode: Integer; const aDescription: string;
  const aJsonSchema: string);
begin
  fStatusCode := aStatusCode;
  fDescription := aDescription;
  fJsonSchema := aJsonSchema;
  fJsonSchemaClass := nil;
  fIsArray := False;
end;

constructor MVCSwagResponsesAttribute.Create(const aStatusCode: Integer; const aDescription: string;
  const aJsonSchemaClass: TClass; const aIsArray: Boolean);
begin
  Create(aStatusCode, aDescription, '');
  fJsonSchemaClass := aJsonSchemaClass;
  fIsArray := aIsArray;
end;

constructor MVCSwagResponsesAttribute.Create(const aFullyQualifiedRecordName: String; const aStatusCode: Integer;
  const aDescription: string; const aIsArray: Boolean; const aJsonSchema: String);
begin
  Create(aStatusCode, aDescription, aJsonSchema);
  fRecordType := GetRecordType(aFullyQualifiedRecordName);
  fIsArray := aIsArray;
end;

{ MVCSwagParamAttribute }

constructor MVCSwagParamAttribute.Create(const aParamLocation: TMVCSwagParamLocation;
  const aParamName, aParamDescription: string; const aParamType: TMVCSwagParamType; const aRequired: Boolean;
  const aDefaultValue, aEnumValues, aJsonSchema: string);
begin
  fParamLocation := aParamLocation;
  fParamName := aParamName;
  fParamDescription := aParamDescription;
  fParamType := aParamType;
  fRequired := aRequired;
  fDefaultValue := aDefaultValue;
  fEnumValues := aEnumValues;
  fJsonSchema := aJsonSchema;
  fJsonSchemaClass := nil;
  fRecordType := nil;
end;

constructor MVCSwagParamAttribute.Create(const aParamLocation: TMVCSwagParamLocation;
  const aParamName, aParamDescription: string; const aJsonSchemaClass: TClass; const aParamType: TMVCSwagParamType;
  const aRequired: Boolean; const aDefaultValue, aEnumValues: string);
begin
  Create(aParamLocation, aParamName, aParamDescription, aParamType, aRequired, aDefaultValue, aEnumValues, '');
  fJsonSchemaClass := aJsonSchemaClass;
end;

constructor MVCSwagParamAttribute.Create(const aParamLocation: TMVCSwagParamLocation; const aParamName,
  aParamDescription, aFullyQualifiedRecordName: string; const aParamType: TMVCSwagParamType; const aRequired: Boolean;
  const aDefaultValue, aEnumValues: string);
begin
  Create(aParamLocation, aParamName, aParamDescription, aParamType, aRequired, aDefaultValue, aEnumValues, '');
  fRecordType := GetRecordType(aFullyQualifiedRecordName);
end;

function MVCSwagParamAttribute.GetEnumValues: TArray<string>;
begin
  Result := fEnumValues.Split([',', ';']);
end;

{ MVCSwagJSONSchemaFieldAttribute }

constructor MVCSwagJSONSchemaFieldAttribute.Create(const aFieldName, aDescription: string;
const aRequired, aNullable: Boolean; const aMinLength, aMaxLength: Integer);
begin
  Create(stUnknown, aFieldName, aDescription, aRequired, aNullable, aMinLength, aMaxLength);
end;

constructor MVCSwagJSONSchemaFieldAttribute.Create(const aSchemaFieldType: TMVCSwagSchemaType;
  const aFieldName, aDescription: string; const aRequired, aNullable: Boolean; const aMinLength, aMaxLength: Integer);
begin
  fSchemaFieldType := aSchemaFieldType;
  fFieldName := aFieldName;
  fDescription := aDescription;
  fRequired := aRequired;
  fNullable := aNullable;
  fMinLength := aMinLength;
  fMaxLength := aMaxLength;
end;

{ TFieldSchemaDefinition }

class function TFieldSchemaDefinition.Create: TFieldSchemaDefinition;
begin
  Result.SchemaFieldType := stUnknown;
  Result.FieldName := '';
  Result.Description := '';
  Result.Required := False;
  Result.Nullable := False;
  Result.MinLength := 0;
  Result.MaxLength := 0;
end;

{ MVCSwagAuthenticationAttribute }

constructor MVCSwagAuthenticationAttribute.Create(const aAuthenticationType: TMVCSwagAuthenticationType);
begin
  fAuthenticationType := aAuthenticationType;
end;

{ TArrayHelper }

class procedure TArrayHelper.QuickSort<T>(var Values: array of T; const Comparer: IComparer<T>; L, R: Integer);
var
  I, J: Integer;
  pivot, temp: T;
begin
  if L < R then
  begin
    repeat
      if (R - L) = 1 then
      begin
        if Comparer.Compare(Values[L], Values[R]) > 0 then
        begin
          temp := Values[L];
          Values[L] := Values[R];
          Values[R] := temp;
        end;
        break;
      end;
      I := L;
      J := R;
      pivot := Values[L + (R - L) shr 1];
      repeat
        while Comparer.Compare(Values[I], pivot) < 0 do
          Inc(I);
        while Comparer.Compare(Values[J], pivot) > 0 do
          Dec(J);
        if I <= J then
        begin
          if I <> J then
          begin
            temp := Values[I];
            Values[I] := Values[J];
            Values[J] := temp;
          end;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if (J - L) > (R - I) then
      begin
        if I < R then
          QuickSort<T>(Values, Comparer, I, R);
        R := J;
      end
      else
      begin
        if L < J then
          QuickSort<T>(Values, Comparer, L, J);
        L := I;
      end;
    until L >= R;
  end;
end;

{ MVCSWAGDefaultModel }

constructor MVCSWAGDefaultModel.Create(const aJsonSchemaClass: TClass; const aSingularModelName: String; const aPluralModelName: String);
begin
  inherited Create;
  fJsonSchemaClass := aJsonSchemaClass;
  fSingularModelName := aSingularModelName;
  fPluralModelName := aPluralModelName;
end;

{ MVCSWAGDefaultSummaryTags }

constructor MVCSWAGDefaultSummaryTags.Create(const aDefaultTags: String);
begin
  inherited Create;
  fDefaultTags := aDefaultTags;
end;

function MVCSWAGDefaultSummaryTags.GetTags: TArray<string>;
begin
  Result := fDefaultTags.Split([',']);
end;


end.
