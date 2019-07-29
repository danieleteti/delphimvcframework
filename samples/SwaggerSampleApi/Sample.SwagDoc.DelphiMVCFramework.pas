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
  DelphiUnit
  ;

type


  TSwagDocToDelphiMVCFrameworkBuilder = class(TObject)
  private
    FSwagDoc : TSwagDoc;
    function TidyUpTypeName(const typeName: string): string;
    function TidyUpURI(const uri:string): string;
    function OperationIdToFunctionName(inOperation: TSwagPathOperation): string;
    procedure SortTypeDefinitions(delphiUnit: TDelphiUnit);
    function GenerateUnitText(delphiUnit: TDelphiUnit): string;
    procedure ConvertSwaggerDefinitionsToTypeDefinitions(delphiUnit: TDelphiUnit);
    function ConvertRefToType(inRef: String): string;
    function ConvertRefToVarName(inRef: String): string;
    procedure ChildType(DelphiUnit : TDelphiUnit; json: TJSONPair);
  public
    constructor Create(SwagDoc: TSwagDoc);
    function Generate: string;
  end;

implementation

uses
  Json.Common.Helpers
  , Winapi.Windows
  ;

{ TSwagDocToDelphiMVCFrameworkBuilder }

function TypeParam(param : TSwagTypeParameter): string;
begin
  case param of
    stpInteger: begin
      Result := 'Integer';
      end
    else
      Result := 'string';
    end;
end;

function TSwagDocToDelphiMVCFrameworkBuilder.OperationIdToFunctionName(inOperation: TSwagPathOperation):string;
begin
  Result := inOperation.OperationId.Replace('{','').Replace('}','').Replace('-','');
  if not CharInSet(Result[1], ['a'..'z','A'..'Z']) then
    Result := 'F' + Result;
end;

function TSwagDocToDelphiMVCFrameworkBuilder.TidyUpURI(const uri:string):string;
begin
  Result := uri.Replace('{','($').Replace('}',')');
end;

function TSwagDocToDelphiMVCFrameworkBuilder.TidyUpTypeName(const typeName: string): string;
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

function TSwagDocToDelphiMVCFrameworkBuilder.ConvertRefToType(inRef:String):string;
begin
  Result := Copy(inRef, inRef.LastIndexOf('/') + 2);
  Result := Copy(Result,1,1).ToUpper + Copy(Result,2);
  Result := 'T' + Result;
end;

function TSwagDocToDelphiMVCFrameworkBuilder.ConvertRefToVarName(inRef:String):string;
begin
  Result := Copy(inRef, inRef.LastIndexOf('/') + 2);
end;

function TSwagDocToDelphiMVCFrameworkBuilder.Generate: string;
var
  i: Integer;
  j: Integer;
  k: Integer;
  LDelphiUnit : TDelphiUnit;
  LMVCController : TUnitTypeDefinition;
  LMethod : TUnitMethod;
  LParam : TUnitParameter;
  LParamType : TUnitTypeDefinition;
  LResponse : TPair<string, TSwagResponse>;
  LSchemaObj : TJsonObject;
  LResultParam : TUnitParameter;
  LRef : String;
begin
  LDelphiUnit := nil;
  try
    LDelphiUnit := TDelphiUnit.Create;
    LDelphiUnit.UnitFile := 'mvccontroller';
    LDelphiUnit.AddInterfaceUnit('MVCFramework');
    LDelphiUnit.AddInterfaceUnit('MVCFramework.Commons');
    LDelphiUnit.AddInterfaceUnit('MVCFramework.Logger');
    LDelphiUnit.AddInterfaceUnit('MVCFramework.JWT');
    LDelphiUnit.AddImplementationUnit('Swag.Doc');

    LMVCController := TUnitTypeDefinition.Create;
    LMVCController.TypeName := 'TMyMVCController';
    LMVCController.TypeInherited := 'TMVCController';
    LMVCController.AddAttribute('  [MVCPath(''' + fSwagDoc.BasePath + ''')]');

    LDelphiUnit.AddType(LMVCController);
    ConvertSwaggerDefinitionsToTypeDefinitions(LDelphiUnit);

    for i := 0 to fSwagDoc.Paths.Count - 1 do
    begin
      for j := 0 to fSwagDoc.Paths[i].Operations.Count - 1 do
      begin
        LMethod := TUnitMethod.Create;
        LMethod.AddAttribute('    [MVCDoc(' + QuotedStr(fSwagDoc.Paths[i].Operations[j].Description) + ')]');
        LMethod.AddAttribute('    [MVCPath(''' + TidyUpURI(fSwagDoc.Paths[i].Uri) + ''')]');
        LMethod.AddAttribute('    [MVCHTTPMethod([http' + fSwagDoc.Paths[i].Operations[j].OperationToString + '])]');
        LMethod.Name := OperationIdToFunctionName(fSwagDoc.Paths[i].Operations[j]);


        for LResponse in FSwagDoc.Paths[i].Operations[j].Responses do
        begin
//        MVCResponse(200, 'success', TEmployee)
          LSchemaObj := LResponse.Value.Schema.JsonSchema;
          if LSchemaObj = nil then
            continue;
          if LSchemaObj.TryGetValue('$ref', LRef) then
          begin
            LMethod.AddAttribute('    [MVCResponse(' + LResponse.Key + ', ' +
                                                   QuotedStr(LResponse.Value.Description) + ', ' + ConvertRefToType(LRef) + ')]');
            LResultParam := TUnitParameter.Create;
            LResultParam.ParamName := ConvertRefToVarName(LRef);
            LResultParam.ParamType := TUnitTypeDefinition.Create;
            LResultParam.ParamType.TypeName := ConvertRefToType(LRef);
            LMethod.AddLocalVariable(LResultParam);
            LMethod.Content.Add('  ' + ConvertRefToVarName(LRef) + ' := ' + ConvertRefToType(LRef) + '.Create;');
              for k := 0 to FSwagDoc.Paths[i].Operations[j].Parameters.Count - 1 do
              begin
                if fSwagDoc.Paths[i].Operations[j].Parameters[k].InLocation <> rpiPath then
                begin
                  LResultParam := TUnitParameter.Create;
                  LResultParam.ParamName := 'param' + TidyUpTypeName(fSwagDoc.Paths[i].Operations[j].Parameters[k].Name);
                  LResultParam.ParamType := TUnitTypeDefinition.Create;
                  LResultParam.ParamType.TypeName := 'String';
                  LMethod.AddLocalVariable(LResultParam);
                  LMethod.Content.Add('  param' + TidyUpTypeName(fSwagDoc.Paths[i].Operations[j].Parameters[k].Name) + ' := Context.Request.Params[' + QuotedStr(fSwagDoc.Paths[i].Operations[j].Parameters[k].Name) + '];');
                end;
              end;
//            method.Content.Add('  Render(' + response.Key + ', ' + ConvertRefToVarName(ref) + ');');
          end
          else
          begin
            if not LSchemaObj.TryGetValue('properties', LSchemaObj) then
              continue;
            if not LSchemaObj.TryGetValue('employees', LSchemaObj) then
              continue;
            if not LSchemaObj.TryGetValue('items', LSchemaObj) then
              continue;
            if LSchemaObj.TryGetValue('$ref', LRef) then
            begin
              LMethod.AddAttribute('    [MVCResponseList(' + LResponse.Key + ', ' +
                                                     QuotedStr(LResponse.Value.Description) + ', ' + ConvertRefToType(LRef) + ')]');
              LResultParam := TUnitParameter.Create;
              LResultParam.ParamName := ConvertRefToVarName(LRef);
              LResultParam.ParamType := TUnitTypeDefinition.Create;
              LResultParam.ParamType.TypeName := 'TObjectList<' + ConvertRefToType(LRef) + '>';
              LMethod.AddLocalVariable(LResultParam);
              LDelphiUnit.AddInterfaceUnit('Generics.Collections');
              LMethod.Content.Add('  ' + ConvertRefToVarName(LRef) + ' := TObjectList<' + ConvertRefToType(LRef) + '>.Create;');

              for k := 0 to FSwagDoc.Paths[i].Operations[j].Parameters.Count - 1 do
              begin
                if fSwagDoc.Paths[i].Operations[j].Parameters[k].InLocation <> rpiPath then
                begin
                  LResultParam := TUnitParameter.Create;
                  LResultParam.ParamName := 'param' + fSwagDoc.Paths[i].Operations[j].Parameters[k].Name;
                  LResultParam.ParamType := TUnitTypeDefinition.Create;
                  LResultParam.ParamType.TypeName := 'String';
                  LMethod.AddLocalVariable(LResultParam);
                  LMethod.Content.Add('  ' + fSwagDoc.Paths[i].Operations[j].Parameters[k].Name + ' := Context.Request.Params[' + QuotedStr(fSwagDoc.Paths[i].Operations[j].Parameters[k].Name) + '];');
                end;
              end;



//              method.Content.Add('  Render(' + response.Key + ', ' + ConvertRefToVarName(ref) + ');');
            end
            else
            begin
              for k := 0 to FSwagDoc.Paths[i].Operations[j].Parameters.Count - 1 do
              begin
                if fSwagDoc.Paths[i].Operations[j].Parameters[k].InLocation <> rpiPath then
                begin
                  LResultParam := TUnitParameter.Create;
                  LResultParam.ParamName := 'param' + fSwagDoc.Paths[i].Operations[j].Parameters[k].Name;
                  LResultParam.ParamType := TUnitTypeDefinition.Create;
                  LResultParam.ParamType.TypeName := 'String';
                  LMethod.AddLocalVariable(LResultParam);
                  LMethod.Content.Add('  ' + fSwagDoc.Paths[i].Operations[j].Parameters[k].Name + ' := Context.Request.Params[' + QuotedStr(fSwagDoc.Paths[i].Operations[j].Parameters[k].Name) + '];');
                end;
              end;
              LMethod.AddAttribute('    [MVCResponse(' + LResponse.Key + ', ' +
                                                   QuotedStr(LResponse.Value.Description) + ')]');
            end;
          end;
        end;


        for k := 0 to fSwagDoc.Paths[i].Operations[j].Parameters.Count - 1 do
        begin
          if fSwagDoc.Paths[i].Operations[j].Parameters[k].InLocation = rpiPath then
          begin
            LParam := TUnitParameter.Create;
            LParam.ParamName := fSwagDoc.Paths[i].Operations[j].Parameters[k].Name;
            LParamType := TUnitTypeDefinition.Create;
            LParamType.TypeName := TypeParam(fSwagDoc.Paths[i].Operations[j].Parameters[k].TypeParameter);
            LParam.ParamType := LParamType;
            LMethod.AddParameter(LParam);
          end;
        end;
        LMVCController.FMethods.Add(LMethod);
      end;
    end;

    SortTypeDefinitions(LDelphiUnit);

    Result := GenerateUnitText(LDelphiUnit);
  finally
    fSwagDoc.Free;
    LDelphiUnit.Free;
  end;
end;


procedure TSwagDocToDelphiMVCFrameworkBuilder.ChildType(DelphiUnit : TDelphiUnit; json: TJSONPair);
var
  LTypeInfo: TUnitTypeDefinition;
  LJsonProps: TJSONObject;
  LFieldInfo: TUnitFieldDefinition;
  LTypeObj: TJSONObject;
  i: Integer;
  j: Integer;
  LValue : string;
  LSchemaObj : TJSONObject;
begin
  OutputDebugString(PChar('Child: ' + json.ToJSON));
  LTypeInfo := TUnitTypeDefinition.Create;
  LTypeInfo.TypeName := 'T' + TidyUpTypeName(json.JSONString.Value);

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
      LFieldInfo.FieldType := 'T' + TidyUpTypeName(LJsonProps.Pairs[j].JsonString.Value);
      ChildType(DelphiUnit, LJsonProps.Pairs[j]);
    end;
    if LTypeObj.TryGetValue('description', LValue) then
      LFieldInfo.AddAttribute('[MVCDoc(' + QuotedStr(LValue) + ')]');
    if LTypeObj.TryGetValue('format', LValue) then
      LFieldInfo.AddAttribute('[MVCFormat(' + QuotedStr(LValue) + ')]');
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
  LSchemaObj : TJSONObject;
begin
  for i := 0 to fSwagDoc.Definitions.Count - 1 do
  begin
    LTypeInfo := TUnitTypeDefinition.Create;
    LTypeInfo.TypeName := 'T' + TidyUpTypeName(fSwagDoc.Definitions[i].Name);
    LJsonProps := fSwagDoc.Definitions[i].JsonSchema.Values['properties'] as TJSONObject;
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
        LFieldInfo.FieldType := 'T' + TidyUpTypeName(LJsonProps.Pairs[j].JsonString.Value);
        ChildType(DelphiUnit, LJsonProps.Pairs[j]);
      end;
      if LTypeObj.TryGetValue('description', LValue) then
        LFieldInfo.AddAttribute('[MVCDoc(' + QuotedStr(LValue) + ')]');
      if LTypeObj.TryGetValue('format', LValue) then
        LFieldInfo.AddAttribute('[MVCFormat(' + QuotedStr(LValue) + ')]');
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

function TSwagDocToDelphiMVCFrameworkBuilder.GenerateUnitText(delphiUnit: TDelphiUnit): string;
var
  i: Integer;
  j: Integer;
  LMethod: TUnitMethod;
  LMvcFile: TStringList;
  LTypeDefinition : string;
begin
  LMvcFile := TStringList.Create;
  try
    LMvcFile.Add(delphiUnit.GenerateInterfaceSectionStart);
    LMvcFile.Add(delphiUnit.GenerateInterfaceUses);
    LMvcFile.Add('(*');
    LMvcFile.Add('Title: ' + fSwagDoc.Info.Title);
    LMvcFile.Add('Description: ' + fSwagDoc.Info.Description);
    LMvcFile.Add('License: ' + fSwagDoc.Info.License.Name);
    LMvcFile.Add('*)');
    LMvcFile.Add('');
    LMvcFile.Add('type');

    SortTypeDefinitions(delphiUnit);

    for i := 0 to delphiUnit.TypeDefinitions.Count - 1 do
    begin
      LMvcFile.Add(delphiUnit.TypeDefinitions[i].GenerateInterface);
    end;
    LMvcFile.Add(delphiUnit.GenerateImplementationSectionStart);
    LMvcFile.Add(delphiUnit.GenerateImplementationUses);
    LMvcFile.Add('');
    for j := 0 to delphiUnit.TypeDefinitions.Count - 1 do
    begin
      for LMethod in delphiUnit.TypeDefinitions[j].GetMethods do
      begin
        LMvcFile.Add(LMethod.GenerateImplementation(delphiUnit.TypeDefinitions[j]));
      end;
    end;
    LMvcFile.Add('end.');
    Result := LMvcFile.Text;
    LMvcFile.SaveToFile('C:\Programming\SwagDoc\Demos\SampleApi\mvccontroller.pas');
  finally
    FreeAndNil(LMvcFile);
  end;
end;


procedure TSwagDocToDelphiMVCFrameworkBuilder.SortTypeDefinitions(delphiUnit: TDelphiUnit);
begin
  { TODO : Make this much more advanced to handle dependency ordering of declarations }

  delphiUnit.TypeDefinitions.Sort(TComparer<TUnitTypeDefinition>.Construct(function (const L, R: TUnitTypeDefinition): integer
  begin
    if L.TypeInherited = 'TMyMVCController' then
      Result := -1
    else if R.TypeInherited = 'TMyMVCController' then
      Result := 1
    else if L.TypeName = R.TypeName then
      Result := 0
    else if L.TypeName < R.TypeName then
      Result := -1
    else
      Result := 1;
  end));
end;

end.
