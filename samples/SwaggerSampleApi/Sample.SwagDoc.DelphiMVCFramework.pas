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
  delphiUnit : TDelphiUnit;
  MVCController : TUnitTypeDefinition;
  method : TUnitMethod;
  param : TUnitParameter;
  paramType : TUnitTypeDefinition;
  response : TPair<string,TSwagResponse>;
  schemaObj : TJsonObject;
  resultParam : TUnitParameter;
  ref : String;
begin
  delphiUnit := nil;
  try
    delphiUnit := TDelphiUnit.Create;
    delphiUnit.UnitFile := 'mvccontroller';
    delphiUnit.AddInterfaceUnit('MVCFramework');
    delphiUnit.AddInterfaceUnit('MVCFramework.Commons');
    delphiUnit.AddInterfaceUnit('MVCFramework.Logger');
    delphiUnit.AddInterfaceUnit('MVCFramework.JWT');
    delphiUnit.AddImplementationUnit('Swag.Doc');

    MVCController := TUnitTypeDefinition.Create;
    MVCController.TypeName := 'TMyMVCController';
    MVCController.TypeInherited := 'TMVCController';
    MVCController.AddAttribute('  [MVCPath(''' + fSwagDoc.BasePath + ''')]');

    delphiUnit.AddType(MVCController);
    ConvertSwaggerDefinitionsToTypeDefinitions(delphiUnit);

    for i := 0 to fSwagDoc.Paths.Count - 1 do
    begin
      for j := 0 to fSwagDoc.Paths[i].Operations.Count - 1 do
      begin
        method := TUnitMethod.Create;
        method.AddAttribute('    [MVCDoc(' + QuotedStr(fSwagDoc.Paths[i].Operations[j].Description) + ')]');
        method.AddAttribute('    [MVCPath(''' + TidyUpURI(fSwagDoc.Paths[i].Uri) + ''')]');
        method.AddAttribute('    [MVCHTTPMethod([http' + fSwagDoc.Paths[i].Operations[j].OperationToString + '])]');
        method.Name := OperationIdToFunctionName(fSwagDoc.Paths[i].Operations[j]);


        for response in FSwagDoc.Paths[i].Operations[j].Responses do
        begin
//        MVCResponse(200, 'success', TEmployee)
          schemaObj := response.Value.Schema.JsonSchema;
          if schemaObj = nil then
            continue;
          if schemaObj.TryGetValue('$ref', ref) then
          begin
            method.AddAttribute('    [MVCResponse(' + response.Key + ', ' +
                                                   QuotedStr(response.Value.Description) + ', ' + ConvertRefToType(ref) + ')]');
            resultParam := TUnitParameter.Create;
            resultParam.ParamName := ConvertRefToVarName(ref);
            resultParam.ParamType := TUnitTypeDefinition.Create;
            resultParam.ParamType.TypeName := ConvertRefToType(ref);
            method.AddLocalVariable(resultParam);
            method.Content.Add('  ' + ConvertRefToVarName(ref) + ' := ' + ConvertRefToType(ref) + '.Create;');
              for k := 0 to FSwagDoc.Paths[i].Operations[j].Parameters.Count - 1 do
              begin
                if fSwagDoc.Paths[i].Operations[j].Parameters[k].InLocation <> rpiPath then
                begin
                  resultParam := TUnitParameter.Create;
                  resultParam.ParamName := 'param' + TidyUpTypeName(fSwagDoc.Paths[i].Operations[j].Parameters[k].Name);
                  resultParam.ParamType := TUnitTypeDefinition.Create;
                  resultParam.ParamType.TypeName := 'String';
                  method.AddLocalVariable(resultParam);
                  method.Content.Add('  param' + TidyUpTypeName(fSwagDoc.Paths[i].Operations[j].Parameters[k].Name) + ' := Context.Request.Params[' + QuotedStr(fSwagDoc.Paths[i].Operations[j].Parameters[k].Name) + '];');
                end;
              end;
//            method.Content.Add('  Render(' + response.Key + ', ' + ConvertRefToVarName(ref) + ');');
          end
          else
          begin
            if not schemaObj.TryGetValue('properties', schemaObj) then
              continue;
            if not schemaObj.TryGetValue('employees', schemaObj) then
              continue;
            if not schemaObj.TryGetValue('items', schemaObj) then
              continue;
            if schemaObj.TryGetValue('$ref', ref) then
            begin
              method.AddAttribute('    [MVCResponseList(' + response.Key + ', ' +
                                                     QuotedStr(response.Value.Description) + ', ' + ConvertRefToType(ref) + ')]');
              resultParam := TUnitParameter.Create;
              resultParam.ParamName := ConvertRefToVarName(ref);
              resultParam.ParamType := TUnitTypeDefinition.Create;
              resultParam.ParamType.TypeName := 'TObjectList<' + ConvertRefToType(ref) + '>';
              method.AddLocalVariable(resultParam);
              delphiUnit.AddInterfaceUnit('Generics.Collections');
              method.Content.Add('  ' + ConvertRefToVarName(ref) + ' := TObjectList<' + ConvertRefToType(ref) + '>.Create;');

              for k := 0 to FSwagDoc.Paths[i].Operations[j].Parameters.Count - 1 do
              begin
                if fSwagDoc.Paths[i].Operations[j].Parameters[k].InLocation <> rpiPath then
                begin
                  resultParam := TUnitParameter.Create;
                  resultParam.ParamName := 'param' + fSwagDoc.Paths[i].Operations[j].Parameters[k].Name;
                  resultParam.ParamType := TUnitTypeDefinition.Create;
                  resultParam.ParamType.TypeName := 'String';
                  method.AddLocalVariable(resultParam);
                  method.Content.Add('  ' + fSwagDoc.Paths[i].Operations[j].Parameters[k].Name + ' := Context.Request.Params[' + QuotedStr(fSwagDoc.Paths[i].Operations[j].Parameters[k].Name) + '];');
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
                  resultParam := TUnitParameter.Create;
                  resultParam.ParamName := 'param' + fSwagDoc.Paths[i].Operations[j].Parameters[k].Name;
                  resultParam.ParamType := TUnitTypeDefinition.Create;
                  resultParam.ParamType.TypeName := 'String';
                  method.AddLocalVariable(resultParam);
                  method.Content.Add('  ' + fSwagDoc.Paths[i].Operations[j].Parameters[k].Name + ' := Context.Request.Params[' + QuotedStr(fSwagDoc.Paths[i].Operations[j].Parameters[k].Name) + '];');
                end;
              end;
              method.AddAttribute('    [MVCResponse(' + response.Key + ', ' +
                                                   QuotedStr(response.Value.Description) + ')]');
            end;
          end;
        end;


        for k := 0 to fSwagDoc.Paths[i].Operations[j].Parameters.Count - 1 do
        begin
          if fSwagDoc.Paths[i].Operations[j].Parameters[k].InLocation = rpiPath then
          begin
            param := TUnitParameter.Create;
            param.ParamName := fSwagDoc.Paths[i].Operations[j].Parameters[k].Name;
            paramType := TUnitTypeDefinition.Create;
            paramType.TypeName := TypeParam(fSwagDoc.Paths[i].Operations[j].Parameters[k].TypeParameter);
            param.ParamType := paramType;
            method.AddParameter(param);
          end;
        end;
        MVCController.FMethods.Add(method);
      end;
    end;

    SortTypeDefinitions(delphiUnit);

    Result := GenerateUnitText(delphiUnit);
  finally
    fSwagDoc.Free;
    delphiUnit.Free;
  end;
end;


procedure TSwagDocToDelphiMVCFrameworkBuilder.ChildType(DelphiUnit : TDelphiUnit; json: TJSONPair);
var
  TypeInfo: TUnitTypeDefinition;
  jsonProps: TJSONObject;
  FieldInfo: TUnitFieldDefinition;
  typeObj: TJSONObject;
  i: Integer;
  j: Integer;
  value : string;
  schemaObj : TJSONObject;
begin
  OutputDebugString(PChar('Child: ' + json.ToJSON));
  TypeInfo := TUnitTypeDefinition.Create;
  TypeInfo.TypeName := 'T' + TidyUpTypeName(json.JSONString.Value);

  jsonProps := (json.JSONValue as TJSONObject).Values['properties'] as TJSONObject;
  for j := 0 to jsonProps.Count - 1 do
  begin
    OutputDebugString(PChar(jsonProps.Pairs[j].ToJSON));
    FieldInfo := TUnitFieldDefinition.Create;
    FieldInfo.FieldName := jsonProps.Pairs[j].JsonString.Value;
    typeObj := jsonProps.Pairs[j].JsonValue as TJSONObject;
    FieldInfo.FieldType := typeObj.Values['type'].Value;
    if FieldInfo.FieldType = 'number' then
      FieldInfo.FieldType := 'Double'
    else if FieldInfo.FieldType = 'object' then
    begin
      FieldInfo.FieldType := 'T' + TidyUpTypeName(jsonProps.Pairs[j].JsonString.Value);
      ChildType(DelphiUnit, jsonProps.Pairs[j]);
    end;
    if typeObj.TryGetValue('description', value) then
      FieldInfo.AddAttribute('[MVCDoc(' + QuotedStr(value) + ')]');
    if typeObj.TryGetValue('format', value) then
      FieldInfo.AddAttribute('[MVCFormat(' + QuotedStr(value) + ')]');
    if typeObj.TryGetValue('maxLength', value) then
      FieldInfo.AddAttribute('[MVCMaxLength(' + value + ')]');
    TypeInfo.Fields.Add(FieldInfo);
  end;
  delphiUnit.AddType(TypeInfo);
end;

procedure TSwagDocToDelphiMVCFrameworkBuilder.ConvertSwaggerDefinitionsToTypeDefinitions(delphiUnit: TDelphiUnit);
var
  TypeInfo: TUnitTypeDefinition;
  jsonProps: TJSONObject;
  FieldInfo: TUnitFieldDefinition;
  typeObj: TJSONObject;
  i: Integer;
  j: Integer;
  value : string;
  schemaObj : TJSONObject;
begin
  for i := 0 to fSwagDoc.Definitions.Count - 1 do
  begin
    TypeInfo := TUnitTypeDefinition.Create;
    TypeInfo.TypeName := 'T' + TidyUpTypeName(fSwagDoc.Definitions[i].Name);
    jsonProps := fSwagDoc.Definitions[i].JsonSchema.Values['properties'] as TJSONObject;
    for j := 0 to jsonProps.Count - 1 do
    begin
      OutputDebugString(PChar(jsonProps.Pairs[j].ToJSON));
      FieldInfo := TUnitFieldDefinition.Create;
      FieldInfo.FieldName := jsonProps.Pairs[j].JsonString.Value;
      typeObj := jsonProps.Pairs[j].JsonValue as TJSONObject;
      FieldInfo.FieldType := typeObj.Values['type'].Value;
      if FieldInfo.FieldType = 'number' then
        FieldInfo.FieldType := 'Double'
      else if FieldInfo.FieldType = 'object' then
      begin
        FieldInfo.FieldType := 'T' + TidyUpTypeName(jsonProps.Pairs[j].JsonString.Value);
        ChildType(DelphiUnit, jsonProps.Pairs[j]);
      end;
      if typeObj.TryGetValue('description', value) then
        FieldInfo.AddAttribute('[MVCDoc(' + QuotedStr(value) + ')]');
      if typeObj.TryGetValue('format', value) then
        FieldInfo.AddAttribute('[MVCFormat(' + QuotedStr(value) + ')]');
      if typeObj.TryGetValue('maxLength', value) then
        FieldInfo.AddAttribute('[MVCMaxLength(' + value + ')]');
      if typeObj.TryGetValue('minimum', value) then
        FieldInfo.AddAttribute('[MVCMinimum(' + value + ')]');
      if typeObj.TryGetValue('maximum', value) then
        FieldInfo.AddAttribute('[MVCMaximum(' + value + ')]');
      TypeInfo.Fields.Add(FieldInfo);
    end;
    delphiUnit.AddType(TypeInfo);
  end;
end;

function TSwagDocToDelphiMVCFrameworkBuilder.GenerateUnitText(delphiUnit: TDelphiUnit): string;
var
  i: Integer;
  j: Integer;
  method: TUnitMethod;
  mvcFile: TStringList;
  typedefinition : string;
begin
  mvcFile := TStringList.Create;
  try
    mvcFile.Add(delphiUnit.GenerateInterfaceSectionStart);
    mvcFile.Add(delphiUnit.GenerateInterfaceUses);
    mvcFile.Add('(*');
    mvcFile.Add('Title: ' + fSwagDoc.Info.Title);
    mvcFile.Add('Description: ' + fSwagDoc.Info.Description);
    mvcFile.Add('License: ' + fSwagDoc.Info.License.Name);
    mvcFile.Add('*)');
    mvcFile.Add('');
    mvcFile.Add('type');

    SortTypeDefinitions(delphiUnit);

    for i := 0 to delphiUnit.TypeDefinitions.Count - 1 do
    begin
      mvcFile.Add(delphiUnit.TypeDefinitions[i].GenerateInterface);
    end;
    mvcFile.Add(delphiUnit.GenerateImplementationSectionStart);
    mvcFile.Add(delphiUnit.GenerateImplementationUses);
    mvcFile.Add('');
    for j := 0 to delphiUnit.TypeDefinitions.Count - 1 do
    begin
      for method in delphiUnit.TypeDefinitions[j].GetMethods do
      begin
        mvcFile.Add(method.GenerateImplementation(delphiUnit.TypeDefinitions[j]));
      end;
    end;
    mvcFile.Add('end.');
    Result := mvcFile.Text;
    mvcFile.SaveToFile('C:\Programming\SwagDoc\Demos\SampleApi\mvccontroller.pas');
  finally
    FreeAndNil(mvcFile);
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
