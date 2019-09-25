unit Sample.SwagDoc.DelphiMVCFramework;

interface

uses
  classes,
  system.json,
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  Swag.Doc,
  Swag.Common.Types,
  Swag.Doc.Path.Operation,
  Swag.Doc.Path.Operation.Response,
  Swag.Doc.Path.Operation.RequestParameter,
  DelphiUnit
  ;

type
  TSwagDocToDelphiMVCFrameworkBuilder = class(TObject)
  private
    FSwagDoc : TSwagDoc;
    function CapitalizeFirstLetter(const typeName: string): string;
    function RewriteUriToSwaggerWay(const uri:string): string;
    function OperationIdToFunctionName(inOperation: TSwagPathOperation): string;
    function GenerateUnitText(delphiUnit: TDelphiUnit): string;
    function ConvertSwaggerTypeToDelphiType(inSwaggerType: TSwagRequestParameter): TUnitTypeDefinition;
    function ConvertRefToType(const inRef:String): string;
    function ConvertRefToVarName(const inRef:String): string;
    procedure ChildType(DelphiUnit : TDelphiUnit; json: TJSONPair);
    procedure HandleArray(inField: TUnitFieldDefinition; json: TJSONPair);
    procedure ConvertSwaggerDefinitionsToTypeDefinitions(delphiUnit: TDelphiUnit);
    procedure ConvertSwaggerRequestParametersToDelphi(AMethod: TUnitMethod; AParameters: TObjectList<TSwagRequestParameter>);
    procedure ConvertSwaggerResponsesToDelphiMethods(ADelphiUnit: TDelphiUnit; AMethod: TUnitMethod; AOperation: TSwagPathOperation);
    function SwaggerTypeAsString(ASwaggerType: TSwagTypeParameter): string;
    procedure CreatePathParam(ASwagParam: TSwagRequestParameter; AParam: TUnitParameter);
    function HandleFormatOnParameter(const inParamType:string; param: TSwagRequestParameter): string;
    procedure CreateNonPathParam(ASwagParam: TSwagRequestParameter; AMethod : TUnitMethod);
    function InLocationAsString(ASwaggerType: TSwagRequestParameterInLocation): string;
  public
    constructor Create(SwagDoc: TSwagDoc);
    function Generate: string;
  end;

implementation

uses
  Json.Common.Helpers
  , Winapi.Windows
  , System.IOUtils
  , MVCFramework.Commons
  , TypInfo
  ;

{ TSwagDocToDelphiMVCFrameworkBuilder }

function TSwagDocToDelphiMVCFrameworkBuilder.OperationIdToFunctionName(inOperation: TSwagPathOperation):string;
begin
  Result := inOperation.OperationId.Replace('{','').Replace('}','').Replace('-','');
  if not CharInSet(Result[1], ['a'..'z','A'..'Z']) then
    Result := 'F' + Result;

  Result := CapitalizeFirstLetter(Result);
end;

function TSwagDocToDelphiMVCFrameworkBuilder.RewriteUriToSwaggerWay(const uri:string):string;
begin
  Result := uri.Replace('{','($').Replace('}',')');
end;

function TSwagDocToDelphiMVCFrameworkBuilder.CapitalizeFirstLetter(const typeName: string): string;
begin
  if typeName.Length > 2 then
    Result := Copy(typeName, 1, 1).ToUpper + Copy(typeName, 2, typeName.Length - 1)
  else
    Result := typeName;
end;

constructor TSwagDocToDelphiMVCFrameworkBuilder.Create(SwagDoc: TSwagDoc);
begin
  FSwagDoc := SwagDoc;
end;

function TSwagDocToDelphiMVCFrameworkBuilder.ConvertRefToType(const inRef:String):string;
begin
  Result := Copy(inRef, inRef.LastIndexOf('/') + 2);
  Result := Copy(Result,1,1).ToUpper + Copy(Result,2);
  Result := 'T' + Result;
end;

function TSwagDocToDelphiMVCFrameworkBuilder.ConvertRefToVarName(const inRef:String):string;
begin
  Result := Copy(inRef, inRef.LastIndexOf('/') + 2);
end;

function TSwagDocToDelphiMVCFrameworkBuilder.Generate: string;
var
  i: Integer;
  j: Integer;
  LDelphiUnit : TDelphiUnit;
  LMVCController : TUnitTypeDefinition;
  LMethod : TUnitMethod;
begin
  LDelphiUnit := nil;
  try
    LDelphiUnit := TDelphiUnit.Create;
    LDelphiUnit.UnitFile := 'mvccontroller';
    LDelphiUnit.AddInterfaceUnit('MVCFramework');
    LDelphiUnit.AddInterfaceUnit('MVCFramework.Commons');
    LDelphiUnit.AddInterfaceUnit('MVCFramework.Logger');
    LDelphiUnit.AddInterfaceUnit('MVCFramework.JWT');
    LDelphiUnit.AddInterfaceUnit('Generics.Collections');
    LDelphiUnit.AddInterfaceUnit('Swag.Common.Types');

    LDelphiUnit.AddImplementationUnit('Swag.Doc');

    ConvertSwaggerDefinitionsToTypeDefinitions(LDelphiUnit);

    LMVCController := TUnitTypeDefinition.Create;
    LMVCController.TypeName := 'TMyMVCController';
    LMVCController.TypeInherited := 'TMVCController';
    LMVCController.AddAttribute('  [MVCPath(''' + fSwagDoc.BasePath + ''')]');

    LDelphiUnit.AddType(LMVCController);

    for i := 0 to fSwagDoc.Paths.Count - 1 do
    begin
      for j := 0 to fSwagDoc.Paths[i].Operations.Count - 1 do
      begin
        LMethod := TUnitMethod.Create;
        if fSwagDoc.Paths[i].Operations[j].Description.Trim.Length > 0 then
          LMethod.AddAttribute('    [MVCDoc(' + QuotedStr(fSwagDoc.Paths[i].Operations[j].Description) + ')]');
        LMethod.AddAttribute('    [MVCPath(''' + RewriteUriToSwaggerWay(fSwagDoc.Paths[i].Uri) + ''')]');
        LMethod.AddAttribute('    [MVCHTTPMethod([http' + fSwagDoc.Paths[i].Operations[j].OperationToString.ToUpper + '])]');
        LMethod.Name := OperationIdToFunctionName(fSwagDoc.Paths[i].Operations[j]);

        ConvertSwaggerRequestParametersToDelphi(LMethod, FSwagDoc.Paths[i].Operations[j].Parameters);
        ConvertSwaggerResponsesToDelphiMethods(LDelphiUnit, LMethod, FSwagDoc.Paths[i].Operations[j]);
        LMVCController.FMethods.Add(LMethod);
      end;
    end;

    LDelphiUnit.SortTypeDefinitions;

    Result := GenerateUnitText(LDelphiUnit);
  finally
    LDelphiUnit.Free;
  end;
end;

procedure TSwagDocToDelphiMVCFrameworkBuilder.CreateNonPathParam(ASwagParam: TSwagRequestParameter; AMethod : TUnitMethod);
var
  param1 : string;
  param2 : string;
  paramType : string;
  param4 : string;
  param5 : string;
  params : string;

begin
  param1 := ASwagParam.Name;
  param2 := InLocationAsString(ASwagParam.InLocation);
  paramType := SwaggerTypeAsString(ASwagParam.TypeParameter);
  param4 := ASwagParam.Pattern;
  param5 := ASwagParam.Format;

  if ASwagParam.TypeParameter = stpNotDefined then
  begin
    if ASwagParam.Schema.JsonSchema.Values['$ref']<>nil then
      paramType := ConvertRefToType(ASwagParam.Schema.JsonSchema.Values['$ref'].Value);
  end;


  if param1.Length = 0 then
    raise Exception.Create('Parameter name not specified');

  if ASwagParam.InLocation = rpiNotDefined then
    raise Exception.Create('Parameter location not specified');

  if paramType.Length = 0 then
    raise Exception.Create('Parameter type not specified');

  params := param1.QuotedString + ', ' + param2 + ', '+ paramType;
  if param5.Length > 0 then
    params := params + ', ' + param4.QuotedString + ', ' + param5.QuotedString
  else if param4.Length > 0 then
  params := params + ', ' + param4.QuotedString;

  AMethod.AddAttribute('[MVCParam(' + params + ')]');
end;


procedure TSwagDocToDelphiMVCFrameworkBuilder.CreatePathParam(ASwagParam: TSwagRequestParameter; AParam: TUnitParameter);
var
  param1 : string;
  param2 : string;
  param3 : string;
  params : string;
begin
  param1 := SwaggerTypeAsString(ASwagParam.TypeParameter);
  param2 := ASwagParam.Pattern;
  param3 := ASwagParam.Format;

  params := param1;
  if param3.Length > 0 then
    params := params + ', ' + param2.QuotedString + ', ' + param3.QuotedString
  else if param2.Length > 0 then
    params := params + ', ' + param2.QuotedString;

  if ASwagParam.Description.Trim <> '' then
  begin
  AParam.AddAttribute('[MVCDoc(' + ASwagParam.Description.QuotedString + ')]');
  end;

  AParam.AddAttribute('[MVCPathParam(' + params + ')]');
end;

function ReturnStatusCode(inStatusCode: string):string;
begin
  inStatusCode := inStatusCode.ToLower;
  if (inStatusCode = 'default') or (inStatusCode = '200') then
    Result := 'HTTP_STATUS.OK'
  else if inStatusCode = '400' then
    Result := 'HTTP_STATUS.BadRequest'
  else if inStatusCode = '404' then
    Result := 'HTTP_STATUS.NotFound'
  else if inStatusCode = '405' then
    Result := 'HTTP_STATUS.MethodNotAllowed'
  else
    Result := inStatusCode;
end;

procedure TSwagDocToDelphiMVCFrameworkBuilder.ConvertSwaggerResponsesToDelphiMethods(ADelphiUnit: TDelphiUnit; AMethod: TUnitMethod; AOperation: TSwagPathOperation);
var
  LResponse: System.Generics.Collections.TPair<string, TSwagResponse>;
  LSchemaObj: TJSONObject;
  LRef: string;
  LResultParam: TUnitParameter;
begin
  for LResponse in AOperation.Responses do
  begin
    LSchemaObj := LResponse.Value.Schema.JsonSchema;
    if LSchemaObj = nil then  // No Return Info to Http Method
    begin
      AMethod.Content.Add('  // ' + LResponse.Key + ' ' + LResponse.Value.Description);
      AMethod.AddAttribute('    [MVCResponse(' + ReturnStatusCode(LResponse.Key) + ', ' + QuotedStr(LResponse.Value.Description) + ')]');
      continue;
    end
    else if LSchemaObj.TryGetValue('$ref', LRef) then
    begin
      AMethod.AddAttribute('    [MVCResponse(' + ReturnStatusCode(LResponse.Key) + ', ' + QuotedStr(LResponse.Value.Description) + ', ' + ConvertRefToType(LRef) + ')]');
      LResultParam := TUnitParameter.Create;
      LResultParam.ParamName := ConvertRefToVarName(LRef);
      LResultParam.ParamType := TUnitTypeDefinition.Create;
      LResultParam.ParamType.TypeName := ConvertRefToType(LRef);
      AMethod.AddLocalVariable(LResultParam);
      AMethod.Content.Add('  ' + ConvertRefToVarName(LRef) + ' := ' + ConvertRefToType(LRef) + '.Create;');
      AMethod.Content.Add('');
      AMethod.Content.Add('  {TODO: Implement filling ' + ConvertRefToVarName(LRef) + ' }');
      AMethod.Content.Add('  Render(' + ReturnStatusCode(LResponse.Key) + ', ' + ConvertRefToVarName(LRef) + ');');
    end
    else
    begin
      if not LSchemaObj.TryGetValue('items', LSchemaObj) then
        continue;
      if LSchemaObj.TryGetValue('$ref', LRef) then
      begin
        ADelphiUnit.AddInterfaceUnit('Generics.Collections');
        AMethod.AddAttribute('    [MVCResponseList(' + ReturnStatusCode(LResponse.Key) + ', ' + QuotedStr(LResponse.Value.Description) + ', ' + ConvertRefToType(LRef) + ')]');
        LResultParam := TUnitParameter.Create;
        LResultParam.ParamName := ConvertRefToVarName(LRef);
        LResultParam.ParamType := TUnitTypeDefinition.Create;
        LResultParam.ParamType.TypeName := 'TObjectList<' + ConvertRefToType(LRef) + '>';
        AMethod.AddLocalVariable(LResultParam);
        AMethod.Content.Add('  ' + ConvertRefToVarName(LRef) + ' := Context.Request.BodyAsListOf<' + ConvertRefToType(LRef) + '>;');
        AMethod.Content.Add('');
        AMethod.Content.Add('  {TODO: Implement filling ' + ConvertRefToVarName(LRef) + ' }');
        AMethod.Content.Add('');
        AMethod.Content.Add('  Render(' + ReturnStatusCode(LResponse.Key) + ', ' + ConvertRefToVarName(LRef) + ');');
      end
      else
      begin
        AMethod.AddAttribute('    [MVCResponse(' + LResponse.Key + ', ' + QuotedStr(LResponse.Value.Description) + ')]');
      end;
    end;
  end;
end;


function TSwagDocToDelphiMVCFrameworkBuilder.HandleFormatOnParameter(const inParamType:string; param: TSwagRequestParameter): string;
begin
  if param.Format.ToLower = 'int64' then
  begin
    Result := 'Int64';
    if inParamType.ToLower <> 'integer' then
       raise Exception.Create('Parameter Type and Format do not match');
  end
  else
  begin
    Result := inParamType;
  end;
end;

procedure TSwagDocToDelphiMVCFrameworkBuilder.ConvertSwaggerRequestParametersToDelphi(AMethod: TUnitMethod; AParameters: TObjectList<TSwagRequestParameter>);
var
  LType: string;
  LParam: TUnitParameter;
  LParamType: TUnitTypeDefinition;
  LSwagParam : TSwagRequestParameter;
  LResultParam : TUnitParameter;
begin
  for LSwagParam in AParameters do
  begin
    if LSwagParam.InLocation = rpiBody then
    begin
      LResultParam := TUnitParameter.Create;
      LResultParam.ParamName := 'param' + CapitalizeFirstLetter(LSwagParam.Name);
      LResultParam.ParamType := ConvertSwaggerTypeToDelphiType(LSwagParam);

      CreateNonPathParam(LSwagParam, AMethod);

      AMethod.AddLocalVariable(LResultParam);
      if LResultParam.ParamType.TypeName.StartsWith('array of') then
      begin
        LType := Trim(Copy(LResultParam.ParamType.TypeName, 9));
        LResultParam.ParamType.TypeName := 'TObjectList<' + LType + '>';
        AMethod.Content.Add('  param' + CapitalizeFirstLetter(LSwagParam.Name) + ' := Context.Request.BodyAsListOf<' + LType + '>;');
      end
      else
        AMethod.Content.Add('  param' + CapitalizeFirstLetter(LSwagParam.Name) + ' := Context.Request.BodyAs<' + LResultParam.ParamType.TypeName + '>;');
    end
    else if LSwagParam.InLocation <> rpiPath then
    begin
      LResultParam := TUnitParameter.Create;
      LResultParam.ParamName := 'param' + CapitalizeFirstLetter(LSwagParam.Name);
      LResultParam.ParamType := TUnitTypeDefinition.Create;
      CreateNonPathParam(LSwagParam, AMethod);
      LResultParam.ParamType.TypeName := 'String';
      AMethod.AddLocalVariable(LResultParam);
      AMethod.Content.Add('  param' + CapitalizeFirstLetter(LSwagParam.Name) + ' := Context.Request.Params[' + QuotedStr(LSwagParam.Name) + '];');
    end
    else
    begin
      LParam := TUnitParameter.Create;
      LParam.ParamName := LSwagParam.Name;
      CreatePathParam(LSwagParam, LParam);
      LParamType := ConvertSwaggerTypeToDelphiType(LSwagParam);
      LParamType.TypeName := HandleFormatOnParameter(LParamType.TypeName, LSwagParam);
      LParam.ParamType := LParamType;
      AMethod.AddParameter(LParam);
    end;
  end;
end;

procedure TSwagDocToDelphiMVCFrameworkBuilder.HandleArray(inField : TUnitFieldDefinition; json: TJSONPair);
var
  jsonObj : TJSONObject;
  jsonVal : TJSONValue;
  LType : String;
begin
  if Assigned(((json.JsonValue as TJSONObject).Values['items'] as TJSONObject).Values['type']) then
  begin
    LType := ((json.JsonValue as TJSONObject).Values['items'] as TJSONObject).Values['type'].Value;
    if LType.ToLower <> 'string' then
      LType := 'T' + LType;
    inField.FieldType := 'array of ' + LType;
  end
  else
  begin
    OutputDebugString(PChar(json.ToJSON));
    jsonVal := (json.JsonValue as TJSONObject).Values['items'] as TJSONObject;
    OutputDebugString(PChar(jsonVal.ToJSON));
    jsonObj := jsonVal as TJSONObject;
    jsonVal := jsonObj.Values['$ref'];
    OutputDebugString(PChar(jsonVal.Value));
    inField.FieldType := 'array of ' + ConvertRefToType(jsonVal.value);
  end;
end;


procedure TSwagDocToDelphiMVCFrameworkBuilder.ChildType(DelphiUnit : TDelphiUnit; json: TJSONPair);
var
  LTypeInfo: TUnitTypeDefinition;
  LJsonProps: TJSONObject;
  LFieldInfo: TUnitFieldDefinition;
  LTypeObj: TJSONObject;
  j: Integer;
  LValue : string;
begin
  OutputDebugString(PChar('Child: ' + json.ToJSON));
  LTypeInfo := TUnitTypeDefinition.Create;
  LTypeInfo.TypeName := 'T' + CapitalizeFirstLetter(json.JSONString.Value);

  LJsonProps := (json.JSONValue as TJSONObject).Values['properties'] as TJSONObject;
  for j := 0 to LJsonProps.Count - 1 do
  begin
    OutputDebugString(PChar(LJsonProps.Pairs[j].ToJSON));
    LFieldInfo := TUnitFieldDefinition.Create;
    LFieldInfo.FieldName := LJsonProps.Pairs[j].JsonString.Value;
    LTypeObj := LJsonProps.Pairs[j].JsonValue as TJSONObject;
    LFieldInfo.FieldType := LTypeObj.Values['type'].Value;
    if LFieldInfo.FieldType = 'number' then
      LFieldInfo.FieldType := 'Double'
    else if LFieldInfo.FieldType = 'object' then
    begin
      LFieldInfo.FieldType := 'T' + CapitalizeFirstLetter(LJsonProps.Pairs[j].JsonString.Value);
      ChildType(DelphiUnit, LJsonProps.Pairs[j]);
    end;
    if LTypeObj.TryGetValue('description', LValue) then
      LFieldInfo.AddAttribute('[MVCDoc(' + QuotedStr(LValue) + ')]');

    if LTypeObj.TryGetValue('format', LValue) then
    begin
      if (LFieldInfo.FieldType.ToLower = 'integer') and (LValue.ToLower = 'int64') then
        LFieldInfo.FieldType := 'Int64';
      LFieldInfo.AddAttribute('[MVCFormat(' + QuotedStr(LValue) + ')]');
    end;
    if LTypeObj.TryGetValue('maxLength', LValue) then
      LFieldInfo.AddAttribute('[MVCMaxLength(' + LValue + ')]');
    LTypeInfo.Fields.Add(LFieldInfo);
  end;
  delphiUnit.AddType(LTypeInfo);
end;

procedure TSwagDocToDelphiMVCFrameworkBuilder.ConvertSwaggerDefinitionsToTypeDefinitions(delphiUnit: TDelphiUnit);
var
  LTypeInfo: TUnitTypeDefinition;
  LJsonProps: TJSONObject;
  LFieldInfo: TUnitFieldDefinition;
  LTypeObj: TJSONObject;
  i: Integer;
  j: Integer;
  LValue : string;
begin
  for i := 0 to fSwagDoc.Definitions.Count - 1 do
  begin
    LTypeInfo := TUnitTypeDefinition.Create;
    LTypeInfo.TypeName := 'T' + CapitalizeFirstLetter(fSwagDoc.Definitions[i].Name);
    LJsonProps := fSwagDoc.Definitions[i].JsonSchema.Values['properties'] as TJSONObject;
    for j := 0 to LJsonProps.Count - 1 do
    begin
      OutputDebugString(PChar(LJsonProps.Pairs[j].ToJSON));
      LFieldInfo := TUnitFieldDefinition.Create;
      LFieldInfo.FieldName := LJsonProps.Pairs[j].JsonString.Value;
      LTypeObj := LJsonProps.Pairs[j].JsonValue as TJSONObject;
      if Assigned(LTypeObj.Values['type']) then
        LFieldInfo.FieldType := LTypeObj.Values['type'].Value
      else
        LFieldInfo.FieldType := ConvertRefToType(LTypeObj.Values['$ref'].Value);

      if LFieldInfo.FieldType = 'number' then
        LFieldInfo.FieldType := 'Double'
      else if LFieldInfo.FieldType = 'object' then
      begin
        LFieldInfo.FieldType := 'T' + CapitalizeFirstLetter(LJsonProps.Pairs[j].JsonString.Value);
        ChildType(DelphiUnit, LJsonProps.Pairs[j]);
      end
      else if LFieldInfo.FieldType = 'array' then
      begin
        HandleArray(LFieldInfo, LJsonProps.Pairs[j]);
      end;
      if LTypeObj.TryGetValue('description', LValue) then
      begin
        if LValue.Trim.Length > 0 then
          LFieldInfo.AddAttribute('[MVCDoc(' + QuotedStr(LValue) + ')]');
      end;
      if LTypeObj.TryGetValue('format', LValue) then
      begin
        if (LFieldInfo.FieldType.ToLower = 'integer') and (LValue.ToLower = 'int64') then
          LFieldInfo.FieldType := 'Int64';
        LFieldInfo.AddAttribute('[MVCFormat(' + QuotedStr(LValue) + ')]');
      end;
      if LTypeObj.TryGetValue('maxLength', LValue) then
        LFieldInfo.AddAttribute('[MVCMaxLength(' + LValue + ')]');
      if LTypeObj.TryGetValue('minimum', LValue) then
        LFieldInfo.AddAttribute('[MVCMinimum(' + LValue + ')]');
      if LTypeObj.TryGetValue('maximum', LValue) then
        LFieldInfo.AddAttribute('[MVCMaximum(' + LValue + ')]');
      LTypeInfo.Fields.Add(LFieldInfo);
    end;
    delphiUnit.AddType(LTypeInfo);
  end;
end;


function TSwagDocToDelphiMVCFrameworkBuilder.SwaggerTypeAsString(ASwaggerType: TSwagTypeParameter):string;
begin
  Result := TypInfo.GetEnumName(System.TypeInfo(TSwagTypeParameter), Integer(ASwaggerType));
end;

function TSwagDocToDelphiMVCFrameworkBuilder.InLocationAsString(ASwaggerType: TSwagRequestParameterInLocation):string;
begin
  Result := TypInfo.GetEnumName(System.TypeInfo(TSwagRequestParameterInLocation), Integer(ASwaggerType));
end;


function TSwagDocToDelphiMVCFrameworkBuilder.ConvertSwaggerTypeToDelphiType(inSwaggerType: TSwagRequestParameter): TUnitTypeDefinition;
var
  LSwaggerType : TSwagTypeParameter;
  json : TJSONObject;
begin
  Result := TUnitTypeDefinition.Create;
  LSwaggerType := inSwaggerType.TypeParameter;
  case LSwaggerType of
    stpNotDefined:
    begin
      if Assigned(inSwaggerType.Schema.JsonSchema.Values['$ref']) then
        Result.TypeName := ConvertRefToType(inSwaggerType.Schema.JsonSchema.Values['$ref'].Value)
      else
      begin
        Result.TypeName := inSwaggerType.Schema.JsonSchema.Values['type'].Value;
        if Result.TypeName = 'array' then
        begin
          if Assigned(inSwaggerType.Schema.JsonSchema.Values['items']) then
            if Assigned((inSwaggerType.Schema.JsonSchema.Values['items'] as TJSONObject).Values['$ref']) then
              Result.TypeName := 'array of ' + ConvertRefToType((inSwaggerType.Schema.JsonSchema.Values['items'] as TJSONObject).Values['$ref'].Value);
        end;
      end;
    end;
    stpString: Result.TypeName := 'String';
    stpNumber: Result.TypeName := 'Double';
    stpInteger: Result.TypeName := 'Integer';
    stpBoolean: Result.TypeName := 'Boolean';
    stpArray:
    begin
      json := inSwaggerType.Schema.JsonSchema;
      if Assigned(json) then
      begin
        OutputDebugString(PChar('TYPE: ' + json.ToJson));
        Result.TypeName := 'array of ' + inSwaggerType.Schema.JsonSchema.Values['type'].Value;
      end
      else
      begin
        if Assigned(inSwaggerType.Items.Values['type']) then
        begin
          Result.TypeName := 'array of ' + inSwaggerType.Items.Values['type'].Value;
        end
        else
          Result.TypeName := 'array of ';
      end;
    end;
    stpFile: Result.TypeName := 'err File';
  end;
end;

function TSwagDocToDelphiMVCFrameworkBuilder.GenerateUnitText(delphiUnit: TDelphiUnit): string;
begin
  delphiUnit.Title := fSwagDoc.Info.Title;
  delphiUnit.Description := FSwagDoc.Info.Description;
  delphiUnit.License := FSwagDoc.Info.License.Name;
  Result := delphiUnit.Generate;
end;

end.
