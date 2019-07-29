unit MVCFramework.Swagger.Commons;

interface

uses
  Json.Schema,
  System.Rtti,
  Swag.Common.Types,
  MVCFramework.Commons,
  Swag.Doc.Path.Operation.RequestParameter,
  Swag.Doc.Path.Operation;

type
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

  MVCSwagResponsesAttribute = class(TCustomAttribute)
  private
    FStatusCode: Integer;
    FDescription: string;
    FJsonSchema: string;
  public
    constructor Create(const AStatusCode: Integer; const ADescription: string; const AJsonSchema: string = '');
    property StatusCode: Integer read FStatusCode;
    property Description: string read FDescription;
    property JsonSchema: string read FJsonSchema;
  end;

  TMVCSwagParamLocation = (plNotDefined, plQuery, plHeader, plPath, plFormData, plBody);
  TMVCSwagParamType = (ptNotDefined, ptString, ptNumber, ptInteger, ptBoolean, ptArray, ptFile);

  MVCSwagParamAttribute = class(TCustomAttribute)
  private
    FParamLocation: TMVCSwagParamLocation;
    FParamName: string;
    FParamDescription: string;
    FParamType: TMVCSwagParamType;
    FRequired: Boolean;
  public
    constructor Create(const AParamLocation: TMVCSwagParamLocation; const AParamName, AParamDescription: string;
      const AParamType: TMVCSwagParamType; const ARequired: Boolean = True);
    property ParamLocation: TMVCSwagParamLocation read FParamLocation;
    property ParamName: string read FParamName;
    property ParamDescription: string read FParamDescription;
    property ParamType: TMVCSwagParamType read FParamType;
    property Required: Boolean read FRequired;
  end;


  TMVCSwagger = class sealed
  private
    class var FRttiContext: TRttiContext;
    class function GetMVCSwagParamsFromMethod(const AMethod: TRttiMethod): TArray<MVCSwagParamAttribute>;
    class function MVCParamLocationToSwagRequestParamInLocation(
      const AMVCSwagParamLocation: TMVCSwagParamLocation):TSwagRequestParameterInLocation;
    class function MVCParamTypeToSwagTypeParameter(const AMVSwagParamType: TMVCSwagParamType):TSwagTypeParameter;
  public
    class constructor Create;
    class destructor Destroy;
    class function MVCHttpMethodToSwagPathOperation(const AMVCHTTPMethod: TMVCHTTPMethodType): TSwagPathTypeOperation;
    class function MVCPathToSwagPath(const AResourcePath: string): string;
    class function GetParamsFromMethod(const AResourcePath: string;
      const AMethod: TRttiMethod): TArray<TSwagRequestParameter>;
    class function RttiTypeToSwagType(const ARttiType: TRttiType): TSwagTypeParameter;
    class procedure FillOperationSummary(const ASwagPathOperation: TSwagPathOperation; const AMethod: TRttiMethod);
  end;


implementation

uses
  System.RegularExpressions,
  System.SysUtils,
  MVCFramework,
  Swag.Doc.Path.Operation.Response;

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

class procedure TMVCSwagger.FillOperationSummary(const ASwagPathOperation: TSwagPathOperation;
  const AMethod: TRttiMethod);
var
  LAttr: TCustomAttribute;
  LSwagResponse: TSwagResponse;
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
      LSwagResponse := TSwagResponse.Create;
      LSwagResponse.StatusCode := MVCSwagResponsesAttribute(LAttr).StatusCode.ToString;
      LSwagResponse.Description := MVCSwagResponsesAttribute(LAttr).Description;
      if not MVCSwagResponsesAttribute(LAttr).JsonSchema.IsEmpty then
        LSwagResponse.Schema.JsonSchema.ParseJSONValue(TEncoding.UTF8.GetBytes(MVCSwagResponsesAttribute(LAttr).JsonSchema), 0);
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
    LSwagResponse.StatusCode := '200';
    LSwagResponse.Description := 'Ok';
    ASwagPathOperation.Responses.Add(LSwagResponse.StatusCode, LSwagResponse);

    LSwagResponse := TSwagResponse.Create;
    LSwagResponse.StatusCode := '500';
    LSwagResponse.Description := 'Internal server error';
    ASwagPathOperation.Responses.Add(LSwagResponse.StatusCode, LSwagResponse);
  end;

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

  //Other parameters
  for I := Low(LMVCSwagParams) to High(LMVCSwagParams) do
  begin
    LSwagParam := TSwagRequestParameter.Create;
    LSwagParam.Name := LMVCSwagParams[I].ParamName;
    LSwagParam.InLocation := MVCParamLocationToSwagRequestParamInLocation(LMVCSwagParams[I].ParamLocation);
    LSwagParam.Required := LMVCSwagParams[I].Required;
    LSwagParam.TypeParameter := MVCParamTypeToSwagTypeParameter(LMVCSwagParams[I].ParamType);
    Insert([LSwagParam], Result, High(Result));
  end;

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
end;

{ MVCSwagParamAttribute }

constructor MVCSwagParamAttribute.Create(const AParamLocation: TMVCSwagParamLocation; const AParamName,
  AParamDescription: string; const AParamType: TMVCSwagParamType; const ARequired: Boolean);
begin
  FParamLocation := AParamLocation;
  FParamName := AParamName;
  FParamDescription := AParamDescription;
  FParamType := AParamType;
  FRequired := ARequired;
end;

end.
