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
  ;

type
  TMVCStatusResponses = class
  strict private
    FStatusCode : Integer;
    FStatusDescription : String;
    FReturnType : TClass;
  public
    property StatusCode: Integer read FStatusCode write FStatusCode;
    property StatusDescription : string read FStatusDescription write FStatusDescription;
    property ReturnType: TClass read FReturnType write FReturnType;
    function GetReturnTypeName:string;
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
    statuses : TObjectList<TMVCStatusResponses>;
    params : TStringList;
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
  private
    procedure ProcessControllerMethods(aClass: TClass);
    procedure processObject(schema: TJSONSchema; aClass:TClass);
    procedure processObjectForDefinition(aClass:TClass);
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

class function TMVCSwaggerController.MVCMethodToSwaggerOperation(inMethod:TMVCHTTPMethodType): TSwagPathTypeOperation;
begin
  if inMethod = httpGET then
    Result := TSwagPathTypeOperation.ohvGet
  else if inMethod = httpPOST then
    Result := TSwagPathTypeOperation.ohvPost
  else if inMethod = httpPUT then
    Result := TSwagPathTypeOperation.ohvPut
  else if inMethod = httpDELETE then
    Result := TSwagPathTypeOperation.ohvDelete
  else if inMethod = httpPATCH then
    Result := TSwagPathTypeOperation.ohvPatch
  else if inMethod = httpOPTIONS then
    Result := TSwagPathTypeOperation.ohvOptions
  else if inMethod = httpHead then
    Result := TSwagPathTypeOperation.ohvHead
  else if inMethod = httpTRACE then
    Result := TSwagPathTypeOperation.ohvTRACE
  else
    Result := TSwagPathTypeOperation.ohvNotDefined;
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
  LDocumentation : string;
  LHttpMethod : TMVCHTTPMethods;
  LPath : string;
  LEndpoint : TMVCEndPoint;
  LStatus : TMVCStatusResponses;
  LProduces : string;
  LConsumes : string;
  LOperationId : string;
  LParams : TStringList;
begin
  LParams := nil;
  try
    LRttiContext := TRttiContext.Create;
    LParams := TStringList.Create;
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
      LDocumentation := '';
      LHttpMethod := [];
      LPath := '';
      LEndpoint := TMVCEndPoint.Create;
      LStatus := nil;
      LProduces := '';
      LConsumes := '';
      LOperationId := '';
      LParams.Clear;
      for LAttribute in LMethod.GetAttributes do
      begin
        if LAttribute is MVCPathAttribute then
        begin
          LPath := TMVCSwaggerController.ProcessAndRewriteURL(LEndpoint.params, LRootPath, MVCPathAttribute(LAttribute).Path);
        end
        else if LAttribute is MVCHTTPMethodAttribute then
        begin
          LHttpMethod := MVCHTTPMethodAttribute(LAttribute).MVCHTTPMethods;
        end
        else if LAttribute is MVCDocAttribute then
        begin
          LDocumentation := MVCDocAttribute(LAttribute).Value;
        end
        else if LAttribute is MVCProducesAttribute then
        begin
          LProduces := MVCProducesAttribute(LAttribute).Value;
        end
        else if LAttribute is MVCConsumesAttribute then
        begin
          LConsumes := MVCConsumesAttribute(LAttribute).Value;
        end
        else if LAttribute is MVCResponseAttribute then
        begin
          LStatus := TMVCStatusResponses.Create;
          LStatus.statusCode := MVCResponseAttribute(LAttribute).StatusCode;
          LStatus.statusDescription := MVCResponseAttribute(LAttribute).Description;
          LStatus.ReturnType := MVCResponseAttribute(LAttribute).ResponseClass;
          LEndpoint.statuses.Add(LStatus);
        end
        else if LAttribute is MVCResponseListAttribute then
        begin
          LStatus := TMVCStatusResponses.Create;
          LStatus.statusCode := MVCResponseListAttribute(LAttribute).StatusCode;
          LStatus.statusDescription := MVCResponseListAttribute(LAttribute).Description;
          LStatus.ReturnType := MVCResponseListAttribute(LAttribute).ResponseClass;
          LEndpoint.statuses.Add(LStatus);
        end
      end;

      LEndpoint.OperationId := LMethod.Name;
      LEndpoint.Documentation := LDocumentation;
      LEndpoint.Method := LHttpMethod;
      LEndpoint.Path := LPath;
      LEndpoint.Produces := LProduces;
      LEndpoint.Consumes := LConsumes;

      fEndpoints.Add(LEndpoint);
    end;
  finally
    FreeAndNil(LParams);
    LRttiContext.Free;
  end;
end;

procedure TMVCSwaggerController.processObjectForDefinition(aClass:TClass);
var
  LRttiContext: TRttiContext;
  LRttiType: TRttiType;
  LProperty : TRttiProperty;
  LAttribute : TCustomAttribute;
  LField : TJsonField;
  LChildschema : TJsonSchema;
  LDefinition : TSwagDefinition;
  LSchema : TJsonSchema;
  LIndexedProperty: TRttiIndexedProperty;
  LJsonArray : TJsonFieldArray;
  LChildObject : TJSONFieldObject;
begin
  LSchema := TJsonSchema.Create;
  LRttiContext := TRttiContext.Create;
  try
    LRttiType := LRttiContext.GetType(aClass);
    for LIndexedProperty in LRttiType.GetIndexedProperties do
    begin
      if (LIndexedProperty.PropertyType.TypeKind = tkArray) then
      begin
        OutputDebugString(PChar(LIndexedProperty.PropertyType.Name));
      end
      else if (LIndexedProperty.PropertyType.TypeKind = tkDynArray) then
      begin
        OutputDebugString(PChar(LIndexedProperty.PropertyType.Name));
      end
      else if (LIndexedProperty.PropertyType.TypeKind = tkClass) then
      begin
        LField := LSchema.AddField<TJsonFieldArray>(LIndexedProperty.Name,'');
        LChildObject := TJsonFieldObject.Create;
        LChildObject.Ref := '#/definitions/' + TRttiInstanceType(LIndexedProperty.PropertyType).MetaclassType.ClassName;
        OutputDebugString(PChar(LIndexedProperty.PropertyType.Name));

        (LField as TJsonFieldArray).ItemFieldType := LChildObject;
        OutputDebugString(PChar(LIndexedProperty.PropertyType.Name));
      end
    end;

    for LProperty in LRttiType.GetProperties do
    begin
      if (LProperty.PropertyType.TypeKind = tkInteger) then
      begin
        LField := LSchema.AddField<Integer>(LProperty.Name,'');
      end
      else if (LProperty.PropertyType.TypeKind = tkInt64) then
      begin
        LField := LSchema.AddField<Int64>(LProperty.Name,'');
      end
      else if (LProperty.PropertyType.TypeKind = tkFloat) then
      begin
        LField := LSchema.AddField<Double>(LProperty.Name,'');
      end
      else if (LProperty.PropertyType.TypeKind = tkClass) then
      begin
        if LProperty.PropertyType.Name='TRttiInstanceType' then continue;
        if LProperty.PropertyType.Name.StartsWith('TRtti') then continue;
        LChildschema := TJsonSchema.Create;
        LChildschema.Ref := '#/definitions/' + TRttiInstanceType(LProperty.PropertyType).MetaclassType.ClassName;
        OutputDebugString(PChar(LProperty.PropertyType.Name));
        LField := LSchema.AddField(LChildschema);
        LField.Name := LProperty.Name;
      end
      else if (LProperty.PropertyType.TypeKind = tkWString) then
      begin
        LField := LSchema.AddField<WideString>(LProperty.Name,'');
      end
      else
      begin
        LField := LSchema.AddField<string>(LProperty.Name,'');
      end;
      for LAttribute in LProperty.GetAttributes do
      begin
        if LAttribute is MVCDocAttribute then
        begin
          LField.Description := MVCDocAttribute(LAttribute).Value;
        end
        else if LAttribute is MVCPatternAttribute then
        begin
          (LField as TJSONFieldString).Pattern := MVCPatternAttribute(LAttribute).Value;
        end;

      end;
    end;
    LDefinition := TSwagDefinition.Create;
    LDefinition.SetJsonSchema(aClass.ClassName, LSchema);
    fSwagDoc.Definitions.Add(LDefinition);

  finally
    LRttiContext.Free;
  end;
end;



procedure TMVCSwaggerController.processObject(schema: TJSONSchema; aClass:TClass);
var
  LRttiContext: TRttiContext;
  LRttiType: TRttiType;
  LProperty : TRttiProperty;
  LAttribute : TCustomAttribute;
  LField : TJsonField;
  LChildschema : TJsonSchema;
  LDefinition : TSwagDefinition;
begin
  LRttiContext := TRttiContext.Create;
  try
    LRttiType := LRttiContext.GetType(aClass);
    for LProperty in LRttiType.GetProperties do
    begin
      if (LProperty.PropertyType.TypeKind = tkInteger) then
      begin
        LField := schema.AddField<Integer>(LProperty.Name,'');
      end
      else if (LProperty.PropertyType.TypeKind = tkInt64) then
      begin
        LField := schema.AddField<Int64>(LProperty.Name,'');
      end
      else if (LProperty.PropertyType.TypeKind = tkFloat) then
      begin
        LField := schema.AddField<Double>(LProperty.Name,'');
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
        processObject(LChildschema, TRttiInstanceType(LProperty.PropertyType).MetaclassType);
        fDefinitions.Add(TRttiInstanceType(LProperty.PropertyType).MetaclassType);
        LField := schema.AddField(LChildschema);
        LField.Name := LProperty.Name;
      end
      else if (LProperty.PropertyType.TypeKind = tkWString) then
      begin
        LField := schema.AddField<WideString>(LProperty.Name,'');
      end
      else
      begin
        LField := schema.AddField<string>(LProperty.Name,'');
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
  iControllers : Integer;
  i, p, j: Integer;
  k: Integer;
  LPath : TSwagPath;
  LPathOperation : TSwagPathOperation;
  LSwagResponse : TSwagResponse;
  LHttpMethod : TMVCHTTPMethodType;
  LParam : TSwagRequestParameter;
  LSchema : TJsonSchema;
begin
  for iControllers := 0 to Engine.Controllers.Count - 1 do
  begin
    ProcessControllerMethods(Engine.Controllers[iControllers].Clazz);
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
      
      if fEndpoints[i].produces.length > 0 then
        LPathOperation.Produces.Add(fEndpoints[i].produces);

      if fEndpoints[i].consumes.length > 0 then
        LPathOperation.Consumes.Add(fEndpoints[i].consumes);

      for j := 0 to fEndpoints[i].statuses.Count - 1 do
      begin
        LSwagResponse := TSwagResponse.Create;
        LSwagResponse.StatusCode := fEndpoints[i].statuses[j].statusCode.ToString;
        LSwagResponse.Description := fEndpoints[i].statuses[j].statusDescription;
        LSchema := TJsonSchema.Create;
        if Assigned(fEndpoints[i].statuses[j].ReturnType) then
        begin
          processObjectForDefinition(fEndpoints[i].statuses[j].ReturnType);
          processObject(LSchema, fEndpoints[i].statuses[j].ReturnType);
          LSwagResponse.Schema.SetJsonSchema(fEndpoints[i].statuses[j].ReturnType.ClassName, LSchema);
        end;
        LPathOperation.Responses.Add(fEndpoints[i].statuses[j].statusCode.ToString, LSwagResponse);
      end;
      OutputDebugString(PChar(TRttiEnumerationType.GetName(LPathOperation.Operation) + ' ' + LPath.Uri ));
      LPathOperation.OperationId := fEndpoints[i].operationId;
      LPath.Operations.Add(LPathOperation);
      LPathOperation := nil;
    end;
    fSwagDoc.Paths.Add(LPath);
  end;

  for k := 0 to fDefinitions.Count - 1 do
  begin
    processObjectForDefinition(fDefinitions[k]);
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

// Possibly use this method to strip the Default 'T' off the start of an object name
function TMVCStatusResponses.GetReturnTypeName: string;
begin
  Result := ReturnType.ClassName;
end;

end.
