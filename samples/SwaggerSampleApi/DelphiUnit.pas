unit DelphiUnit;

interface

uses
  classes,
  system.json,
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections,
  System.Generics.Defaults
  ;

type
  TUnitTypeDefinition = class;

  TUnitFieldDefinition = class
  private
    FFieldName: string;
    FFieldType: string;
    FAttributes: TStringList;
    FDescription: string;
  public
    property FieldName: string read FFieldName write FFieldName;
    property FieldType: string read FFieldType write FFieldType;
    property Description: string read FDescription write FDescription;
    procedure AddAttribute(const inAttribute: string);
    function GenerateInterface: string;
    constructor Create;
    destructor Destroy; override;
  end;

  TUnitParameter = class
  private
    FFlags: TParamFlags;
    FType: TUnitTypeDefinition;
    FParamName: string;
    FAttributes: TStringList;
  public
    property Attributes: TStringList read FAttributes write FAttributes;
    property ParamName: string read FParamName write FParamName;
    property Flags: TParamFlags read FFlags write FFlags;
    property ParamType: TUnitTypeDefinition read FType write FType;
    procedure AddAttribute(const inAttribute: string);
    constructor Create;
    destructor Destroy; override;

  end;

  TUnitMethod = class
  private
    FAttributes: TStringList;
    FMethodKind: TMethodKind;
    FVisibility: TMemberVisibility;
    FName: string;
    FIsStatic: Boolean;
    FIsClassMethod: Boolean;
    FReturnType: TUnitTypeDefinition;
    FParams: TObjectList<TUnitParameter>;
    FVars: TObjectList<TUnitParameter>;
    FContent: TStringList;
    function MethodKindToDelphiString(var LHasReturn: Boolean): string;
    procedure ParametersToDelphiString(var AParamString: string; AIncludeAttributes: Boolean);
    procedure MethodLocalVarsToDelphiString(LFuncSL: TStringList);
    function GetIsConstructor: Boolean;
    function GetIsDestructor: Boolean;
  public
    property Content: TStringList read FContent write FContent;
    property MethodKind: TMethodKind read FMethodKind write FMethodKind;
    property Visibility: TMemberVisibility read FVisibility write FVisibility;
    property Name: string read FName write FName;
    property IsConstructor: Boolean read GetIsConstructor;
    property IsDestructor: Boolean read GetIsDestructor;
    property IsClassMethod: Boolean read FIsClassMethod write FIsClassMethod;
    // Static: No 'Self' parameter
    property IsStatic: Boolean read FIsStatic write FIsStatic;
    property ReturnType: TUnitTypeDefinition read FReturnType write FReturnType;
    function GetParameters: TArray<TUnitParameter>;
    procedure AddParameter(param: TUnitParameter);
    procedure AddLocalVariable(inVar: TUnitParameter);
    procedure AddAttribute(const inAttribute: string);
    function GenerateInterface: string;
    function GenerateImplementation(inOnType: TUnitTypeDefinition): string;
    constructor Create;
    destructor Destroy; override;
  end;

  TUnitTypeDefinition = class
  private
    FTypeName: string;
    FTypeInheritedFrom: string;
    FAttributes: TStringList;
    FTypeKind: TTypeKind;
    FForwardDeclare: Boolean;
    FGuid : TGUID;
  public
    Fields: TObjectList<TUnitFieldDefinition>;
    FMethods: TObjectList<TUnitMethod>;
    property Guid: TGUID read FGuid write FGuid;
    property TypeName: string read FTypeName write FTypeName;
    property TypeKind: TTypeKind read FTypeKind write FTypeKind;
    property TypeInherited: string read FTypeInheritedFrom write FTypeInheritedFrom;
    property ForwardDeclare: Boolean read FForwardDeclare write FForwardDeclare;
    function GetMethods(): TArray<TUnitMethod>;
    procedure AddAttribute(const inAttribute: string);
    function GenerateInterface: string;
    function GenerateForwardInterface: string;
    constructor Create;
    destructor Destroy; override;
  end;

  TDelphiUnit = class
  private
    FInterfaceUses: TStringList;
    FImplementationUses: TStringList;
    FInterfaceConstant: TStringList;
    FImplementationConstant: TStringList;
    FUnitName: string;
    FTitle: String;
    FDescription: string;
    FLicense: string;
  public
    TypeDefinitions: TObjectList<TUnitTypeDefinition>;
    function GenerateInterfaceSectionStart: string; virtual;
    function GenerateInterfaceUses: string; virtual;
    function GenerateImplementationSectionStart: string; virtual;
    function GenerateImplementationUses: string; virtual;
    function GenerateImplementationConstants: string; virtual;
    function CreateGUID: TGuid;
  public
    property UnitFile: string read FUnitName write FUnitName;
    property Title: String read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property License: string read FLicense write FLicense;
    procedure AddInterfaceUnit(const inFilename: string); virtual;
    procedure AddInterfaceConstant(const inName:string; const inValue:string);
    procedure AddImplementationUnit(const inFilename: string); virtual;
    procedure AddImplementationConstant(const inName:string; const inValue:string);
    procedure AddType(inTypeInfo: TUnitTypeDefinition);
    procedure SortTypeDefinitions;
    function Generate: string;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

function DelphiVarName(const inVarname: string):string;
begin
  Result := inVarname;
  if Result.ToLower = 'type' then
    Result := '&' + Result
  else if Result.ToLower = 'file' then
    Result := '&' + Result;
end;


{ TDelphiUnit }

procedure TDelphiUnit.AddImplementationConstant(const inName, inValue: string);
begin
  FImplementationConstant.AddPair(inName, inValue);
end;

procedure TDelphiUnit.AddImplementationUnit(const inFilename: string);
var
  IntIndex : Integer;
begin
  IntIndex := FInterfaceUses.IndexOf(inFilename);
  if IntIndex < 0 then
  begin
    if FImplementationUses.IndexOf(inFilename) < 0 then
      FImplementationUses.Add(inFilename);
  end;
end;

procedure TDelphiUnit.AddInterfaceConstant(const inName, inValue: string);
begin
  FInterfaceConstant.AddPair(inName, inValue);
end;

procedure TDelphiUnit.AddInterfaceUnit(const inFilename: string);
var
  ImpIndex : Integer;
begin
  ImpIndex := FImplementationUses.IndexOf(inFilename);
  if ImpIndex >= 0 then
    FImplementationUses.Delete(ImpIndex);

  if FInterfaceUses.IndexOf(inFilename) < 0 then
    FInterfaceUses.Add(inFilename);
end;

procedure TDelphiUnit.AddType(inTypeInfo: TUnitTypeDefinition);
begin
  TypeDefinitions.Add(inTypeInfo);
end;

constructor TDelphiUnit.Create;
begin
  FInterfaceUses := TStringList.Create;
  FInterfaceConstant := TStringList.Create;
  FImplementationConstant := TStringList.Create;
  FImplementationUses := TStringList.Create;
  TypeDefinitions := TObjectList<TUnitTypeDefinition>.Create;
end;

destructor TDelphiUnit.Destroy;
begin
  FreeAndNil(FInterfaceUses);
  FreeAndNil(FImplementationUses);
  FreeAndNil(FInterfaceConstant);
  FreeAndNil(FImplementationConstant);
  FreeAndNil(TypeDefinitions);
  inherited;
end;

function TDelphiUnit.GenerateImplementationConstants: string;
var
  SL : TStringList;
  i : Integer;
begin
  SL := TStringList.Create;
  try
    if FImplementationConstant.Count > 0 then
    begin
      SL.Add('const');
      for i := 0 to FImplementationConstant.Count - 1 do
      begin
        SL.Add('  ' + FImplementationConstant.Names[i] + ' = ' + FImplementationConstant.ValueFromIndex[i] + ';');
      end;
    end;
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

function TDelphiUnit.GenerateImplementationSectionStart: string;
var
  LImplementationSection: TStringList;
begin
  LImplementationSection := TStringList.Create;
  try
    LImplementationSection.Add('');
    LImplementationSection.Add('implementation');
    LImplementationSection.Add('');
    Result := LImplementationSection.Text;
  finally
    FreeAndNil(LImplementationSection);
  end;
end;

function TDelphiUnit.GenerateImplementationUses: string;
var
  LUsesSL: TStringList;
  i: Integer;
begin
  LUsesSL := TStringList.Create;
  try
    if FImplementationUses.Count > 0 then
    begin
      LUsesSL.Add('uses');
      for i := 0 to FImplementationUses.Count - 1 do
      begin
        if i = 0 then
          LUsesSL.Add('    ' + FImplementationUses[i])
        else
          LUsesSL.Add('  , ' + FImplementationUses[i]);
      end;
      LUsesSL.Add('  ;');
    end;
    LUsesSL.Add('');
    Result := LUsesSL.Text;
  finally
    FreeAndNil(LUsesSL);
  end;
end;

function TDelphiUnit.GenerateInterfaceSectionStart: string;
var
  LInterfaceSection: TStringList;
begin
  LInterfaceSection := TStringList.Create;
  try
    LInterfaceSection.Add('unit ' + UnitFile + ';');
    LInterfaceSection.Add('');
    LInterfaceSection.Add('interface');
    LInterfaceSection.Add('');
    Result := LInterfaceSection.Text;
  finally
    FreeAndNil(LInterfaceSection);
  end;
end;

function TDelphiUnit.GenerateInterfaceUses: string;
var
  LUsesSL: TStringList;
  i: Integer;
begin
  LUsesSL := TStringList.Create;
  try
    if FInterfaceUses.Count > 0 then
    begin
      LUsesSL.Add('uses');
      for i := 0 to FInterfaceUses.Count - 1 do
      begin
        if i = 0 then
          LUsesSL.Add('    ' + FInterfaceUses[i])
        else
          LUsesSL.Add('  , ' + FInterfaceUses[i]);
      end;
      LUsesSL.Add('  ;');
    end;
    LUsesSL.Add('');
    Result := LUsesSL.Text;
  finally
    FreeAndNil(LUsesSL);
  end;
end;

function TDelphiUnit.Generate:string;
var
  i: Integer;
  j: Integer;
  LMethod: TUnitMethod;
  LMvcFile: TStringList;
  LForwardAlreadyDeclared : Boolean;
begin
  LForwardAlreadyDeclared := False;
  LMvcFile := TStringList.Create;
  try
    LMvcFile.Add(GenerateInterfaceSectionStart);
    LMvcFile.Add(GenerateInterfaceUses);
    LMvcFile.Add('(*');
    LMvcFile.Add('Title: ' + Title);
    LMvcFile.Add('Description: ' + Description);
    LMvcFile.Add('License: ' + License);
    LMvcFile.Add('*)');
    LMvcFile.Add('');
    LMvcFile.Add('type');

    SortTypeDefinitions;

    if FInterfaceConstant.Count > 0 then
    begin
      LMvcFile.Add('const');
      for i := 0 to FInterfaceConstant.Count - 1 do
      begin
        LMvcFile.Add('  ' + FInterfaceConstant.Names[i] + ' = ' + FInterfaceConstant.ValueFromIndex[i] + ';');
      end;
    end;

    for i := 0 to TypeDefinitions.Count - 1 do
    begin
      if TypeDefinitions[i].ForwardDeclare then
      begin
        if not LForwardAlreadyDeclared then
          LMvcFile.Add('  // Forward Declarations');
        LMvcFile.Add(TypeDefinitions[i].GenerateForwardInterface);
        LForwardAlreadyDeclared := True;
      end;
    end;

    for i := 0 to TypeDefinitions.Count - 1 do
    begin
      LMvcFile.Add(TypeDefinitions[i].GenerateInterface);
    end;

    LMvcFile.Add(GenerateImplementationSectionStart);
    LMvcFile.Add(GenerateImplementationUses);
    LMvcFile.Add('');
    GenerateImplementationConstants;
    for j := 0 to TypeDefinitions.Count - 1 do
    begin
      for LMethod in TypeDefinitions[j].GetMethods do
      begin
        LMvcFile.Add(LMethod.GenerateImplementation(TypeDefinitions[j]));
      end;
    end;
    LMvcFile.Add('end.');
    Result := LMvcFile.Text;
  finally
    FreeAndNil(LMvcFile);
  end;
end;

function TDelphiUnit.CreateGUID:TGuid;
var
  guid : TGUID;
begin
  System.SysUtils.CreateGuid(guid);
  Result := guid;
end;

procedure TDelphiUnit.SortTypeDefinitions;
begin
  { TODO : Make this much more advanced to handle dependency ordering of declarations }

  TypeDefinitions.Sort(TComparer<TUnitTypeDefinition>.Construct(function (const L, R: TUnitTypeDefinition): integer
  begin
    if L.TypeName = 'TMyMVCController' then
      Result := 1
    else if R.TypeName = 'TMyMVCController' then
      Result := -1
    else
      Result := CompareText(L.TypeName, R.TypeName);
  end));
end;

{ TTypeDefinition }

procedure TUnitTypeDefinition.AddAttribute(const inAttribute: string);
begin
  FAttributes.Add(inAttribute);
end;

constructor TUnitTypeDefinition.Create;
begin
  FAttributes := TStringList.Create;
  Fields := TObjectList<TUnitFieldDefinition>.Create;
  FMethods := TObjectList<TUnitMethod>.Create;
  FTypeKind := tkClass;
  FForwardDeclare := False;
end;

destructor TUnitTypeDefinition.Destroy;
begin
  FreeAndNil(FAttributes);
  FreeAndNil(Fields);
  FreeAndNil(FMethods);
  inherited;
end;

function TUnitTypeDefinition.GenerateForwardInterface: string;
begin
  if FTypeKind = tkClass then
    Result := '  ' + TypeName + ' : class;'
  else if FTypeKind = tkInterface then
    Result := '  ' + TypeName + ' : interface;'
  else
    Result := '  ' + TypeName + 'xxxx';
end;

function TUnitTypeDefinition.GenerateInterface: string;
var
  LInterfaceSL: TStringList;
  i: Integer;
  j: Integer;
begin
  LInterfaceSL := TStringList.Create;
  try
    for i := 0 to FAttributes.Count - 1 do
    begin
      LInterfaceSL.Add(FAttributes[i]);
    end;
    if FTypeKind = tkClass then
    begin
      if TypeInherited.Length > 0 then
        LInterfaceSL.Add('  ' + TypeName + ' = class(' + TypeInherited + ')')
      else
        LInterfaceSL.Add('  ' + TypeName + ' = class');
    end
    else if FTypeKind = tkInterface then
    begin
      if TypeInherited.Length > 0 then
      begin
        LInterfaceSL.Add('  ' + TypeName + ' = interface(' + TypeInherited + ')');
        LInterfaceSL.Add('    [' + GUIDToString(FGuid).QuotedString + ']');
      end
      else
      begin
        LInterfaceSL.Add('  ' + TypeName + ' = interface');
        LInterfaceSL.Add('    [' + GUIDToString(FGuid).QuotedString + ']');
      end;
    end;

    for j := 0 to Fields.Count - 1 do
    begin
      LInterfaceSL.Add(Fields[j].GenerateInterface);
    end;

    for j := 0 to FMethods.Count - 1 do
    begin
      LInterfaceSL.Add(TrimRight(FMethods[j].GenerateInterface));
      LInterfaceSL.Add('');
    end;

    LInterfaceSL.Add('  end;');

    Result := LInterfaceSL.Text;
  finally
    FreeAndNil(LInterfaceSL);
  end;
end;

function TUnitTypeDefinition.GetMethods: TArray<TUnitMethod>;
var
  i: Integer;
begin
  SetLength(Result, FMethods.Count);
  for i := 0 to FMethods.Count - 1 do
  begin
    Result[i] := FMethods[i];
  end;
end;

{ TFieldDefinition }

procedure TUnitFieldDefinition.AddAttribute(const inAttribute: string);
begin
  FAttributes.Add(inAttribute);
end;

constructor TUnitFieldDefinition.Create;
begin
  FAttributes := TStringList.Create;
end;

destructor TUnitFieldDefinition.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited;
end;

function TUnitFieldDefinition.GenerateInterface: string;
var
  i : Integer;
  SL : TStringList;
  LType : string;
begin
  SL := TStringList.Create;
  try
    LType := FFieldType;
    for i := 0 to FAttributes.Count - 1 do
    begin
      SL.Add('    ' + FAttributes[i]);
    end;

    if Description.Length > 0 then
      SL.Add('    [MVCDoc(' + QuotedStr(Description) + ')]');

    SL.Add('    ' + DelphiVarName(FFieldName) + ' : ' + LType + ';');
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

{ TUnitMethod }

procedure TUnitMethod.AddAttribute(const inAttribute: string);
begin
  FAttributes.Add(inAttribute);
end;

procedure TUnitMethod.AddLocalVariable(inVar: TUnitParameter);
begin
  FVars.Add(inVar);
end;

procedure TUnitMethod.AddParameter(param: TUnitParameter);
begin
  FParams.Add(param);
end;

constructor TUnitMethod.Create;
begin
  FParams := TObjectList<TUnitParameter>.Create;
  FAttributes := TStringList.Create;
  FVars := TObjectList<TUnitParameter>.Create;
  FContent := TStringList.Create;
end;

destructor TUnitMethod.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FAttributes);
  FreeAndNil(FVars);
  FreeAndNil(FContent);
  inherited;
end;

procedure TUnitMethod.MethodLocalVarsToDelphiString(LFuncSL: TStringList);
var
  i: Integer;
begin
  if FVars.Count > 0 then
  begin
    LFuncSL.Add('var');
    for i := 0 to FVars.Count - 1 do
    begin
      LFuncSL.Add('  ' + FVars[i].ParamName + ' : ' + FVars[i].ParamType.TypeName + ';');
    end;
  end;
end;

procedure TUnitMethod.ParametersToDelphiString(var AParamString: string; AIncludeAttributes: Boolean);
var
  LParam: TUnitParameter;
  LParamFlagString: string;
  LParamName: string;
  LParamAttributeString : string;
  I: Integer;
begin
  AParamString := '(';
  for LParam in GetParameters do
  begin
    LParamFlagString := '';
    if pfConst in LParam.Flags then
      LParamFlagString := 'const'
    else if pfVar in LParam.Flags then
      LParamFlagString := 'var'
    else if pfOut in LParam.Flags then
      LParamFlagString := 'out'
    else if pfArray in LParam.Flags then
      LParamFlagString := 'array of';
    if LParamFlagString.Length > 0 then
      LParamFlagString := LParamFlagString + ' ';

    if AIncludeAttributes then
    begin
      for I := 0 to LParam.Attributes.Count - 1 do
      begin
        LParamAttributeString := LParamAttributeString + ' ' + LParam.Attributes[i];
      end;

      LParamAttributeString := Trim(LParamAttributeString) + ' ';
    end;


    LParamName := DelphiVarName(LParam.ParamName);
    AParamString := AParamString + LParamAttributeString + LParamFlagString + LParamName + ': ' + LParam.FType.FTypeName + '; ';
  end;
  if AParamString.EndsWith('; ') then
    AParamString := AParamString.Remove(AParamString.Length - 2);
  AParamString := AParamString + ')';
  if AParamString = '()' then
    AParamString := '';
end;

function TUnitMethod.MethodKindToDelphiString(var LHasReturn: Boolean): string;
begin
  case MethodKind of
    mkProcedure:
      Result := 'procedure';
    mkFunction:
      begin
        Result := 'function';
        LHasReturn := True;
      end;
    mkDestructor:
      Result := 'destructor';
    mkConstructor:
      Result := 'constructor';
    mkClassFunction:
      begin
        Result := 'class function';
        LHasReturn := True;
      end;
    mkClassProcedure:
      Result := 'class procedure';
    mkClassConstructor:
      Result := 'class constructor';
    mkClassDestructor:
      Result := 'class destructor';
  else
    Result := 'unknown';
  end;
end;

function TUnitMethod.GenerateImplementation(inOnType: TUnitTypeDefinition): string;
var
  LProcTypeString: string;
  LHasReturn: Boolean;
  LParamString: string;
  LClassNameProcIn: string;
  LFuncSL: TStringList;
begin
  LHasReturn := False;
  LClassNameProcIn := '';
  LProcTypeString := MethodKindToDelphiString(LHasReturn);

  if Assigned(inOnType) then
    LClassNameProcIn := inOnType.TypeName + '.';
  ParametersToDelphiString(LParamString, False);

  if LHasReturn then
    Result := LProcTypeString + ' ' + LClassNameProcIn + FName + LParamString + ': ' + ReturnType.FTypeName + ';'
  else
    Result := LProcTypeString + ' ' + LClassNameProcIn + FName + LParamString + ';';

  LFuncSL := TStringList.Create;
  try
    LFuncSL.Text := Result;

    MethodLocalVarsToDelphiString(LFuncSL);

    LFuncSL.Add('begin');
    LFuncSL.Add(Content.Text);
    LFuncSL.Add('end;');

    Result := LFuncSL.Text;
  finally
    FreeAndNil(LFuncSL);
  end;
end;

function TUnitMethod.GenerateInterface: string;
var
  LProcTypeString: string;
  LHasReturn: Boolean;
  LParamString: string;
  LAttributeString: string;
begin
  LHasReturn := False;

  LProcTypeString := MethodKindToDelphiString(LHasReturn);

  ParametersToDelphiString(LParamString, True);

  if LHasReturn then
    Result := '    ' + LProcTypeString + ' ' + FName + LParamString + ': ' + ReturnType.FTypeName + ';'
  else
    Result := '    ' + LProcTypeString + ' ' + FName + LParamString + ';';

  LAttributeString := FAttributes.Text;
  Result := LAttributeString + Result;
end;

function TUnitMethod.GetIsConstructor: Boolean;
begin
  Result := MethodKind = mkConstructor;
end;

function TUnitMethod.GetIsDestructor: Boolean;
begin
  Result := MethodKind = mkDestructor;
end;

function TUnitMethod.GetParameters: TArray<TUnitParameter>;
var
  i: Integer;
begin
  setLength(Result, FParams.Count);
  for i := 0 to FParams.Count - 1 do
  begin
    Result[i] := FParams[i];
  end;
end;

{ TUnitParameter }

procedure TUnitParameter.AddAttribute(const inAttribute: string);
begin
  FAttributes.Add(inAttribute);
end;

constructor TUnitParameter.Create;
begin
  FAttributes := TStringList.Create;
end;

destructor TUnitParameter.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited;
end;

end.

