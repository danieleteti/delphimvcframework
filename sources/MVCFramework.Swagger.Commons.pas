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
  System.JSON;

type
  TMVCSwagParamLocation = (plNotDefined, plQuery, plHeader, plPath, plFormData, plBody);
  TMVCSwagParamType = (ptNotDefined, ptString, ptNumber, ptInteger, ptBoolean, ptArray, ptFile);

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

  TMVCSwagger = class sealed
  private
    class var FRttiContext: TRttiContext;
    class function GetMVCSwagParamsFromMethod(const AMethod: TRttiMethod): TArray<MVCSwagParamAttribute>;
    class function MVCParamLocationToSwagRequestParamInLocation(
      const AMVCSwagParamLocation: TMVCSwagParamLocation): TSwagRequestParameterInLocation;
    class function MVCParamTypeToSwagTypeParameter(const AMVSwagParamType: TMVCSwagParamType): TSwagTypeParameter;
    class function ExtractJsonSchemaFromClass(const AClass: TClass): TJSONObject;
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

implementation

uses
  System.RegularExpressions,
  System.SysUtils,
  MVCFramework,
  Swag.Doc.Path.Operation.Response,
  System.Classes,
  MVCFramework.Serializer.Commons,
  MVCFramework.Middleware.Authentication.RoleBasedAuthHandler;

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

class function TMVCSwagger.ExtractJsonSchemaFromClass(const AClass: TClass): TJSONObject;
var
  LObjType: TRttiType;
  LProp: TRttiProperty;
  LJsonSchema: TJsonSchema;
  LPropName: string;
begin
  LObjType := FRttiContext.GetType(AClass);
  LJsonSchema := TJsonSchema.Create;
  try
    for LProp in LObjType.GetProperties do
    begin
      LPropName := TMVCSerializerHelper.GetKeyName(LProp, LObjType);
      case LProp.PropertyType.TypeKind of
        tkInteger, tkInt64:
          LJsonSchema.AddField<Integer>(LPropName);
        tkChar, tkString, tkWChar, tkLString, tkWString, tkUString, tkVariant:
          LJsonSchema.AddField<string>(LPropName);
        tkEnumeration:
          if (LProp.Handle = TypeInfo(Boolean)) then
            LJsonSchema.AddField<string>(LPropName);
        tkFloat:
          if (LProp.Handle = TypeInfo(TDateTime)) or
            (LProp.Handle = TypeInfo(TDate)) or
            (LProp.Handle = TypeInfo(TTime)) then
            LJsonSchema.AddField<string>(LPropName)
          else
            LJsonSchema.AddField<Currency>(LPropName);
        tkClass:
          if (LProp.Handle = TypeInfo(TStream)) or
            (LProp.Handle = TypeInfo(TStringStream)) or
            (LProp.Handle = TypeInfo(TMemoryStream)) then
            LJsonSchema.AddField<string>(LPropName);
        tkRecord:
          if LProp.Handle = TypeInfo(TGUID) then
            LJsonSchema.AddField<string>(LPropName);
      end;
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

class function TMVCSwagger.GetParamsFromMethod(const AResourcePath: string;
  const AMethod: TRttiMethod): TArray<TSwagRequestParameter>;

  function TryGetMVCPathParamByName(const AParams: TArray<MVCSwagParamAttribute>;
    const AParamName: string; out AMVCParam: MVCSwagParamAttribute; out AIndex: Integer): Boolean;
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

class function TMVCSwagger.MVCHttpMethodToSwagPathOperation(
  const AMVCHTTPMethod: TMVCHTTPMethodType): TSwagPathTypeOperation;
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

class function TMVCSwagger.MVCParamLocationToSwagRequestParamInLocation(
  const AMVCSwagParamLocation: TMVCSwagParamLocation): TSwagRequestParameterInLocation;
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

class function TMVCSwagger.MVCParamTypeToSwagTypeParameter(
  const AMVSwagParamType: TMVCSwagParamType): TSwagTypeParameter;
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

end.
