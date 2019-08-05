unit MVCFramework.Controllers.Swagger;

interface

uses
    Classes
  , MVCFramework
  , MVCFramework.Commons
  , System.Generics.Collections
  , Swag.Doc
  , Swag.Doc.Path
  , Swag.Doc.Path.Operation
  , Swag.Doc.Definition
  , Swag.Common.Types
  , Swag.Doc.Path.Operation.Response
  , Swag.Doc.Path.Operation.RequestParameter
  , Json.Schema
  , Json.Schema.Field
  , Json.Schema.Field.Strings
  , Json.Schema.Field.Arrays
  , Json.Schema.Field.Objects
  , Json.Schema.Field.Numbers
  ;

type
  TMVCStatusResponses = class
  strict private
    FStatusCode : Integer;
    FStatusDescription : String;
    FReturnType : TClass;
    FKind: TTypeKind;
  public
    property StatusCode: Integer read FStatusCode write FStatusCode;
    property StatusDescription : string read FStatusDescription write FStatusDescription;
    property ReturnType: TClass read FReturnType write FReturnType;
    property Kind: TTypeKind read FKind write FKind;
    function GetReturnTypeName:string;
    constructor Create(AAttribute: MVCResponseAttribute); overload;
    constructor Create(AAttribute: MVCResponseListAttribute); overload;
  end;

  TMVCEndPoint = class
  strict private
    FDoc : string;
    FMethod : TMVCHTTPMethods;
    FOperationId: string;
    FProduces : string;
    FConsumes : string;
    FPath : string;
  public
    Statuses : TObjectList<TMVCStatusResponses>;
    Params : TStringList;
    property Documentation : string read FDoc write FDoc;
    property Method : TMVCHTTPMethods read FMethod write FMethod;
    property OperationId : string read FOperationId write FOperationId;
    property Produces : string read FProduces write FProduces;
    property Consumes : string read FConsumes write FConsumes;
    property Path : string read FPath write FPath;
  public
    constructor Create;
    destructor Destroy; override;
  end;

type

  [MVCPath('/')]
  TMVCSwaggerController = class(TMVCController)
  private
    fEndpoints : TObjectList<TMVCEndPoint>;
    fSwagDoc: TSwagDoc;
    fDefinitions : TList<TClass>;
    procedure ConvertFieldAttributesToSwagger(AField: TJsonField; AAttributes: TArray<TCustomAttribute>);
  private
    procedure ProcessControllerMethods(aClass: TClass);
    procedure ProcessObject(schema: TJSONSchema; aClass:TClass);
    procedure ProcessObjectForDefinition(aClass:TClass);
  public
    class function ProcessAndRewriteURL(params: TStringList; const rootPath:string; const path:string): string;
    class function MVCMethodToSwaggerOperation(inMethod:TMVCHTTPMethodType): TSwagPathTypeOperation;
    [MVCDoc('This is some documentation')]
    [MVCPath('/swagger')]
    [MVCHTTPMethod([httpGET])]
    procedure Swagger;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
    RTTI
  , windows
  , System.SysUtils
  , System.RegularExpressions
  , System.RegularExpressionsCore
  ;

{ TMVCSwaggerController }

constructor TMVCSwaggerController.Create;
begin
  inherited;
  fEndpoints := TObjectList<TMVCEndPoint>.Create;
  fDefinitions := TList<TClass>.Create;
  fSwagDoc := TSwagDoc.Create;
end;

destructor TMVCSwaggerController.Destroy;
begin
  FreeAndNil(fEndpoints);
  FreeAndNil(fSwagDoc);
  FreeAndNil(fDefinitions);
  inherited;
end;

procedure TMVCSwaggerController.ConvertFieldAttributesToSwagger(AField: TJsonField; AAttributes: TArray<TCustomAttribute>);
var
  LAttribute: TCustomAttribute;
begin
  for LAttribute in AAttributes do
  begin
    if LAttribute is MVCDocAttribute then
    begin
      AField.Description := MVCDocAttribute(LAttribute).Value;
    end
    else if LAttribute is MVCPatternAttribute then
    begin
      (AField as TJSONFieldString).Pattern := MVCPatternAttribute(LAttribute).Value;
    end
    else if LAttribute is MVCMaxLengthAttribute then
    begin
      (AField as TJSONFieldString).MaxLength := MVCMaxLengthAttribute(LAttribute).Value;
    end
    else if LAttribute is MVCMinimumAttribute then
    begin
      if AField is TJsonFieldInt64 then
        (AField as TJsonFieldInt64).MinValue := MVCMinimumAttribute(LAttribute).Value
      else if AField is TJsonFieldInteger then
        (AField as TJsonFieldInteger).MinValue := MVCMinimumAttribute(LAttribute).Value
      else if AField is TJsonFieldNumber then
        (AField as TJsonFieldNumber).MinValue := MVCMinimumAttribute(LAttribute).Value
      else
        raise Exception.Create('Minimum not valid on ' + AField.ClassName);
    end
    else if LAttribute is MVCMaximumAttribute then
    begin
      if AField is TJsonFieldInt64 then
        (AField as TJsonFieldInt64).MaxValue := MVCMaximumAttribute(LAttribute).Value
      else if AField is TJsonFieldInteger then
        (AField as TJsonFieldInteger).MaxValue := MVCMaximumAttribute(LAttribute).Value
      else if AField is TJsonFieldNumber then
        (AField as TJsonFieldNumber).MaxValue := MVCMaximumAttribute(LAttribute).Value
      else
        raise Exception.Create('Maximum not valid on ' + AField.ClassName);
    end
    else if LAttribute is MVCFormatAttribute then
    begin
      if AField is TJsonFieldString then
        (AField as TJsonFieldString).Format := MVCFormatAttribute(LAttribute).Value
      else if AField is TJsonFieldString then
        (AField as TJsonFieldInteger).Format := MVCFormatAttribute(LAttribute).Value
      else if AField is TJsonFieldInt64 then
        (AField as TJsonFieldInt64).Format := MVCFormatAttribute(LAttribute).Value
      else if AField is TJsonFieldNumber then
        (AField as TJsonFieldNumber).Format := MVCFormatAttribute(LAttribute).Value
      else
        raise Exception.Create('Format not valid on ' + AField.ClassName);
    end;
  end;
end;

class function TMVCSwaggerController.MVCMethodToSwaggerOperation(inMethod:TMVCHTTPMethodType): TSwagPathTypeOperation;
begin
  case inMethod of
    httpGET: Result := TSwagPathTypeOperation.ohvGet;
    httpPOST: Result := TSwagPathTypeOperation.ohvPost;
    httpPUT: Result := TSwagPathTypeOperation.ohvPut;
    httpDELETE: Result := TSwagPathTypeOperation.ohvDelete;
    httpHEAD: Result := TSwagPathTypeOperation.ohvHead;
    httpOPTIONS: Result := TSwagPathTypeOperation.ohvOptions;
    httpPATCH: Result := TSwagPathTypeOperation.ohvPatch;
    httpTRACE: Result := TSwagPathTypeOperation.ohvTrace;
  else
    Result := TSwagPathTypeOperation.ohvNotDefined;
  end;
end;

class function TMVCSwaggerController.ProcessAndRewriteURL(params: TStringList; const rootPath:string; const path:string):string;
var
  LRegEx : TRegEx;
  LMatches: TMatchCollection;
  i: Integer;
begin
  if (rootPath = '/') and path.StartsWith('/') then
    Result := path
  else
    Result := rootPath + Path;

  LRegEx := TRegEx.Create('\(\$\w+\)');
  LMatches := LRegEx.Matches(Result);
  for i := LMatches.Count - 1 downto 0 do
  begin
    params.Add(Copy(LMatches.Item[i].Value,3,LMatches.Item[i].Value.length-3));
    Result[LMatches.Item[i].Index + LMatches.Item[i].Length - 1] := '}';
    Result[LMatches.Item[i].Index] := '{';
    Delete(Result, LMatches.Item[i].Index + 1,1);
  end;
end;


procedure TMVCSwaggerController.ProcessControllerMethods(aClass: TClass);
var
  LRttiContext: TRttiContext;
  LRttiType: TRttiType;
  LAttribute: TCustomAttribute;
  LMethods : TArray<TRttiMethod>;
  LMethod: TRttiMethod;
  LRootPath : string;
  LEndpoint : TMVCEndPoint;
  LStatus : TMVCStatusResponses;
begin
  try
    LRttiContext := TRttiContext.Create;
    LRttiType := LRttiContext.GetType(aClass);
    for LAttribute in LRttiType.GetAttributes do
    begin
      if LAttribute is MVCPathAttribute then
      begin
        LRootPath := MVCPathAttribute(LAttribute).Path;
      end
    end;

    LMethods := LRttiType.GetMethods;
    for LMethod in LMethods do
    begin
      LEndpoint := TMVCEndPoint.Create;
      for LAttribute in LMethod.GetAttributes do
      begin
        if LAttribute is MVCPathAttribute then
        begin
          LEndpoint.Path := TMVCSwaggerController.ProcessAndRewriteURL(LEndpoint.params, LRootPath, MVCPathAttribute(LAttribute).Path);
        end
        else if LAttribute is MVCHTTPMethodAttribute then
        begin
          LEndpoint.Method := MVCHTTPMethodAttribute(LAttribute).MVCHTTPMethods;
        end
        else if LAttribute is MVCDocAttribute then
        begin
          LEndpoint.Documentation := MVCDocAttribute(LAttribute).Value;
        end
        else if LAttribute is MVCProducesAttribute then
        begin
          LEndpoint.Produces := MVCProducesAttribute(LAttribute).Value;
        end
        else if LAttribute is MVCConsumesAttribute then
        begin
          LEndpoint.Consumes := MVCConsumesAttribute(LAttribute).Value;
        end
        else if LAttribute is MVCResponseAttribute then
        begin
          LStatus := TMVCStatusResponses.Create(MVCResponseAttribute(LAttribute));
          LEndpoint.statuses.Add(LStatus);
        end
        else if LAttribute is MVCResponseListAttribute then
        begin
          LStatus := TMVCStatusResponses.Create(MVCResponseListAttribute(LAttribute));
          LEndpoint.statuses.Add(LStatus);
        end
      end;
      LEndpoint.OperationId := LMethod.Name;
      fEndpoints.Add(LEndpoint);
    end;
  finally
    LRttiContext.Free;
  end;
end;

procedure TMVCSwaggerController.ProcessObjectForDefinition(aClass:TClass);
var
  LRttiContext: TRttiContext;
  LRttiType: TRttiType;
  LField : TJsonField;
  LChildschema : TJsonSchema;
  LDefinition : TSwagDefinition;
  LSchema : TJsonSchema;
  LIndexedProperty: TRttiIndexedProperty;
  LProperty : TRttiProperty;
  LChildObject : TJSONFieldObject;
  LTypeKind : TTypeKind;
  LPropertyName : string;
  LAttributes : TArray<TCustomAttribute>;
begin
  LSchema := TJsonSchema.Create;
  LRttiContext := TRttiContext.Create;
  try
    LRttiType := LRttiContext.GetType(aClass);
    for LIndexedProperty in LRttiType.GetIndexedProperties do
    begin
      LTypeKind := LIndexedProperty.PropertyType.TypeKind;
      LPropertyName := LIndexedProperty.Name;
      case LTypeKind of
        tkClassRef, tkPointer, tkProcedure, tkMRecord, tkInterface,
        tkEnumeration, tkMethod, tkVariant, tkSet, tkRecord: ;
        tkWChar, tkLString, tkWString, tkString, tkUString, tkChar:
          LField := LSchema.AddField<String>(LPropertyName);
        tkUnknown:
          LField := LSchema.AddField<String>(LPropertyName);
        tkInteger:
          LField := LSchema.AddField<Integer>(LPropertyName);
        tkInt64:
          LField := LSchema.AddField<Int64>(LPropertyName);
        tkFloat:
          LField := LSchema.AddField<Double>(LPropertyName);
        tkClass:
        begin
          LField := LSchema.AddField<TJsonFieldArray>(LPropertyName);
          LChildObject := TJsonFieldObject.Create;
          LChildObject.Ref := '#/definitions/' + TRttiInstanceType(LIndexedProperty.PropertyType).MetaclassType.ClassName;
          OutputDebugString(PChar(LIndexedProperty.PropertyType.Name));

          (LField as TJsonFieldArray).ItemFieldType := LChildObject;
          OutputDebugString(PChar(LIndexedProperty.PropertyType.Name));
        end;
        tkArray:
          OutputDebugString(PChar(LIndexedProperty.PropertyType.Name));
        tkDynArray:
          OutputDebugString(PChar(LIndexedProperty.PropertyType.Name));
      end;
      LAttributes := LIndexedProperty.GetAttributes;
      ConvertFieldAttributesToSwagger(LField, LAttributes);
    end;

    for LProperty in LRttiType.GetProperties do
    begin
      LPropertyName := LProperty.Name;
      if (LProperty.PropertyType.TypeKind = tkInteger) then
      begin
        LField := LSchema.AddField<Integer>(LPropertyName);
      end
      else if (LProperty.PropertyType.TypeKind = tkInt64) then
      begin
        LField := LSchema.AddField<Int64>(LPropertyName);
      end
      else if (LProperty.PropertyType.TypeKind = tkFloat) then
      begin
        LField := LSchema.AddField<Double>(LPropertyName);
      end
      else if (LProperty.PropertyType.TypeKind = tkWString) then
      begin
        LField := LSchema.AddField<WideString>(LPropertyName);
      end
      else if (LProperty.PropertyType.TypeKind = tkClass) then
      begin
        if LProperty.PropertyType.Name='TRttiInstanceType' then continue;
        if LProperty.PropertyType.Name.StartsWith('TRtti') then continue;
        LChildschema := TJsonSchema.Create;
        LChildschema.Ref := '#/definitions/' + TRttiInstanceType(LProperty.PropertyType).MetaclassType.ClassName;
        OutputDebugString(PChar(LProperty.PropertyType.Name));
        LField := LSchema.AddField(LChildschema);
        LField.Name := LPropertyName;
      end
      else
      begin
        LField := LSchema.AddField<string>(LPropertyName);
      end;

      LAttributes := LProperty.GetAttributes;
      ConvertFieldAttributesToSwagger(LField, LAttributes);
    end;
    LDefinition := TSwagDefinition.Create;
    LDefinition.SetJsonSchema(aClass.ClassName, LSchema);
    fSwagDoc.Definitions.Add(LDefinition);
  finally
    LRttiContext.Free;
  end;
end;



procedure TMVCSwaggerController.ProcessObject(schema: TJSONSchema; aClass:TClass);
var
  LRttiContext: TRttiContext;
  LRttiType: TRttiType;
  LProperty : TRttiProperty;
  LAttribute : TCustomAttribute;
  LField : TJsonField;
  LChildschema : TJsonSchema;
begin
  LRttiContext := TRttiContext.Create;
  LField := nil;
  try
    LRttiType := LRttiContext.GetType(aClass);
    for LProperty in LRttiType.GetProperties do
    begin
      if (LProperty.PropertyType.TypeKind = tkInteger) then
      begin
        LField := schema.AddField<Integer>(LProperty.Name);
      end
      else if (LProperty.PropertyType.TypeKind = tkInt64) then
      begin
        LField := schema.AddField<Int64>(LProperty.Name);
      end
      else if (LProperty.PropertyType.TypeKind = tkFloat) then
      begin
        LField := schema.AddField<Double>(LProperty.Name);
      end
      else if (LProperty.PropertyType.TypeKind = tkArray) then
      begin
        OutputDebugString(PChar(LProperty.PropertyType.Name));
      end
      else if (LProperty.PropertyType.TypeKind = tkDynArray) then
      begin
        OutputDebugString(PChar(LProperty.PropertyType.Name));
      end
      else if (LProperty.PropertyType.TypeKind = tkClass) then
      begin
        if LProperty.PropertyType.Name='TRttiInstanceType' then
          continue;
        if LProperty.PropertyType.Name.StartsWith('TRtti') then
          continue;
        LChildschema := TJsonSchema.Create;
        OutputDebugString(PChar(LProperty.PropertyType.Name));
        ProcessObject(LChildschema, TRttiInstanceType(LProperty.PropertyType).MetaclassType);
        fDefinitions.Add(TRttiInstanceType(LProperty.PropertyType).MetaclassType);
        LField := schema.AddField(LChildschema);
        LField.Name := LProperty.Name;
      end
      else if (LProperty.PropertyType.TypeKind = tkWString) then
      begin
        LField := schema.AddField<WideString>(LProperty.Name);
      end
      else
      begin
        LField := schema.AddField<string>(LProperty.Name);
      end;
      for LAttribute in LProperty.GetAttributes do
      begin
        if LAttribute is MVCDocAttribute then
        begin
          LField.Description := MVCDocAttribute(LAttribute).Value;
        end
        else if LAttribute is MVCPatternAttribute then
        begin
          // Property doesnt currently exist on TJSONField
          if LField is TJsonFieldString then
            (LField as TJsonFieldString).Pattern := MVCPatternAttribute(LAttribute).Value;
        end;
      end;
    end;
  finally
    LRttiContext.Free;
  end;
end;

procedure TMVCSwaggerController.Swagger;
var
  i, p, j: Integer;
  k: Integer;
  LPath : TSwagPath;
  LPathOperation : TSwagPathOperation;
  LSwagResponse : TSwagResponse;
  LHttpMethod : TMVCHTTPMethodType;
  LParam : TSwagRequestParameter;
  LSchema : TJsonSchema;
  LController : TMVCControllerDelegate;
  LDefinition : TClass;
begin
  for LController in Engine.Controllers do
  begin
    ProcessControllerMethods(LController.Clazz);
  end;

  Context.Response.ContentType := 'application/json';

  for i := 0 to fEndpoints.Count - 1 do
  begin
    LPath := TSwagPath.Create;
    LPath.Uri := fEndpoints[i].path;
    for LHttpMethod in fEndpoints[i].method do
    begin
      LPathOperation := TSwagPathOperation.Create;
      LPathOperation.Operation := TMVCSwaggerController.MVCMethodToSwaggerOperation(LHttpMethod);
      LPathOperation.Description := fEndpoints[i].Documentation;
      
      for p := fEndpoints[i].params.Count - 1 downto 0 do
      begin
        LParam := TSwagRequestParameter.Create;
        LParam.Name := fEndpoints[i].params[p];
        LParam.InLocation := rpiPath;
        LPathOperation.Parameters.Add(LParam);
      end;
      
      if fEndpoints[i].Produces.length > 0 then
        LPathOperation.Produces.Add(fEndpoints[i].produces);

      if fEndpoints[i].Consumes.length > 0 then
        LPathOperation.Consumes.Add(fEndpoints[i].consumes);

      for j := 0 to fEndpoints[i].statuses.Count - 1 do
      begin
        LSwagResponse := TSwagResponse.Create;
        LSwagResponse.StatusCode := fEndpoints[i].Statuses[j].statusCode.ToString;
        LSwagResponse.Description := fEndpoints[i].Statuses[j].statusDescription;
        LSchema := TJsonSchema.Create;
        if Assigned(fEndpoints[i].Statuses[j].ReturnType) then
        begin
          ProcessObjectForDefinition(fEndpoints[i].Statuses[j].ReturnType);
          ProcessObject(LSchema, fEndpoints[i].Statuses[j].ReturnType);
          LSwagResponse.Schema.SetJsonSchema(fEndpoints[i].Statuses[j].ReturnType.ClassName, LSchema);
        end;
        LPathOperation.Responses.Add(fEndpoints[i].Statuses[j].StatusCode.ToString, LSwagResponse);
      end;
      OutputDebugString(PChar(TRttiEnumerationType.GetName(LPathOperation.Operation) + ' ' + LPath.Uri ));
      LPathOperation.OperationId := fEndpoints[i].OperationId;
      LPath.Operations.Add(LPathOperation);
    end;
    fSwagDoc.Paths.Add(LPath);
  end;

  for LDefinition in fDefinitions do
  begin
    ProcessObjectForDefinition(LDefinition);
  end;

  fSwagDoc.GenerateSwaggerJson;
  Context.Response.Content := fSwagDoc.SwaggerJson.ToJSON;
end;

{ TMVCEndPoint }

constructor TMVCEndPoint.Create;
begin
  statuses := TObjectList<TMVCStatusResponses>.Create;
  params := TStringList.Create;
end;

destructor TMVCEndPoint.Destroy;
begin
  FreeAndNil(statuses);
  FreeAndNil(params);
  inherited;
end;

{ TStatusResponses }

constructor TMVCStatusResponses.Create(AAttribute: MVCResponseAttribute);
begin
  StatusCode := AAttribute.StatusCode;
  StatusDescription := AAttribute.Description;
  ReturnType := AAttribute.ResponseClass;
  Kind := tkClass;
end;

constructor TMVCStatusResponses.Create(AAttribute: MVCResponseListAttribute);
begin
  StatusCode := AAttribute.StatusCode;
  StatusDescription := AAttribute.Description;
  ReturnType := AAttribute.ResponseClass;
  Kind := tkArray;
end;

// Possibly use this method to strip the Default 'T' off the start of an object name
function TMVCStatusResponses.GetReturnTypeName: string;
begin
  Result := ReturnType.ClassName;
end;

end.
