unit DelphiUnit;

interface

uses
  classes, system.json, System.SysUtils, System.Rtti, System.TypInfo,
  System.Generics.Collections;

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
  public
    property ParamName: string read FParamName write FParamName;
    property Flags: TParamFlags read FFlags write FFlags;
    property ParamType: TUnitTypeDefinition read FType write FType;
  end;

  TUnitMethod = class
  private
    FAttributes: TStringList;
    FMethodKind: TMethodKind;
    FVisibility: TMemberVisibility;
    FName: string;
    FIsStatic: Boolean;
    FIsClassMethod: Boolean;
    FIsConstructor: Boolean;
    FIsDestructor: Boolean;
    FReturnType: TUnitTypeDefinition;
    FParams: TObjectList<TUnitParameter>;
    FVars: TObjectList<TUnitParameter>;
    FContent: TStringList;
  public
    property Content: TStringList read FContent write FContent;
    property MethodKind: TMethodKind read FMethodKind write FMethodKind;
    property Visibility: TMemberVisibility read FVisibility write FVisibility;
    property Name: string read FName write FName;
    property IsConstructor: Boolean read FIsConstructor write FIsConstructor;
    property IsDestructor: Boolean read FIsDestructor write FIsDestructor;
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
  public
    Fields: TObjectList<TUnitFieldDefinition>;
    FMethods: TObjectList<TUnitMethod>;
    property TypeName: string read FTypeName write FTypeName;
    property TypeInherited: string read FTypeInheritedFrom write FTypeInheritedFrom;
    function GetMethods(): TArray<TUnitMethod>;
    procedure AddAttribute(const inAttribute: string);
    function GenerateInterface: string;
    constructor Create;
    destructor Destroy; override;
  end;

  TDelphiUnit = class
  private
    FInterfaceUses: TStringList;
    FImplementationUses: TStringList;
    FUnitName: string;
  public
    TypeDefinitions: TObjectList<TUnitTypeDefinition>;
    function GenerateInterfaceSectionStart: string; virtual;
    function GenerateInterfaceUses: string; virtual;
    function GenerateImplementationSectionStart: string; virtual;
    function GenerateImplementationUses: string; virtual;
  public
    property UnitFile: string read FUnitName write FUnitName;
    procedure AddInterfaceUnit(const inFilename: string); virtual;
    procedure AddImplementationUnit(const inFilename: string); virtual;
    procedure AddType(inTypeInfo: TUnitTypeDefinition);
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TDelphiUnit }

procedure TDelphiUnit.AddImplementationUnit(const inFilename: string);
var
  IntIndex : Integer;
begin
  IntIndex := FInterfaceUses.IndexOf(inFilename);
  if IntIndex < 0 then
    FImplementationUses.Add(inFilename);
end;

procedure TDelphiUnit.AddInterfaceUnit(const inFilename: string);
var
  ImpIndex : Integer;
begin
  ImpIndex := FImplementationUses.IndexOf(inFilename);
  if ImpIndex >= 0 then
    FImplementationUses.Delete(ImpIndex);

  FInterfaceUses.Add(inFilename);
end;

procedure TDelphiUnit.AddType(inTypeInfo: TUnitTypeDefinition);
begin
  TypeDefinitions.Add(inTypeInfo);
end;

constructor TDelphiUnit.Create;
begin
  FInterfaceUses := TStringList.Create;
  FInterfaceUses.Duplicates := dupIgnore;
  FImplementationUses := TStringList.Create;
  FImplementationUses.Duplicates := dupIgnore;
  TypeDefinitions := TObjectList<TUnitTypeDefinition>.Create;
end;

destructor TDelphiUnit.Destroy;
begin
  FreeAndNil(FInterfaceUses);
  FreeAndNil(FImplementationUses);
  FreeAndNil(TypeDefinitions);
  inherited;
end;

function TDelphiUnit.GenerateImplementationSectionStart: string;
var
  implementationSection: TStringList;
begin
  implementationSection := TStringList.Create;
  try
    implementationSection.Add('');
    implementationSection.Add('implementation');
    implementationSection.Add('');
    Result := implementationSection.Text;
  finally
    FreeAndNil(implementationSection);
  end;
end;

function TDelphiUnit.GenerateImplementationUses: string;
var
  usesSL: TStringList;
  i: Integer;
begin
  usesSL := TStringList.Create;
  try
    if FImplementationUses.Count > 0 then
    begin
      usesSL.Add('uses');
      for i := 0 to FImplementationUses.Count - 1 do
      begin
        if i = 0 then
          usesSL.Add('    ' + FImplementationUses[i])
        else
          usesSL.Add('  , ' + FImplementationUses[i]);
      end;
      usesSL.Add('  ;');
    end;
    usesSL.Add('');
    Result := usesSL.Text;
  finally
    FreeAndNil(usesSL);
  end;
end;

function TDelphiUnit.GenerateInterfaceSectionStart: string;
var
  interfaceSection: TStringList;
begin
  interfaceSection := TStringList.Create;
  try
    interfaceSection.Add('unit ' + UnitFile + ';');
    interfaceSection.Add('');
    interfaceSection.Add('interface');
    interfaceSection.Add('');
    Result := interfaceSection.Text;
  finally
    FreeAndNil(interfaceSection);
  end;
end;

function TDelphiUnit.GenerateInterfaceUses: string;
var
  usesSL: TStringList;
  i: Integer;
begin
  usesSL := TStringList.Create;
  try
    if FInterfaceUses.Count > 0 then
    begin
      usesSL.Add('uses');
      for i := 0 to FInterfaceUses.Count - 1 do
      begin
        if i = 0 then
          usesSL.Add('    ' + FInterfaceUses[i])
        else
          usesSL.Add('  , ' + FInterfaceUses[i]);
      end;
      usesSL.Add('  ;');
    end;
    usesSL.Add('');
    Result := usesSL.Text;
  finally
    FreeAndNil(usesSL);
  end;
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
end;

destructor TUnitTypeDefinition.Destroy;
begin
  FreeAndNil(FAttributes);
  FreeAndNil(Fields);
  FreeAndNil(FMethods);
  inherited;
end;

function TUnitTypeDefinition.GenerateInterface: string;
var
  interfaceSL: TStringList;
  i: Integer;
  j: Integer;
begin
  interfaceSL := TStringList.Create;
  try
    for i := 0 to FAttributes.Count - 1 do
    begin
      interfaceSL.Add(FAttributes[i]);
    end;
    if TypeInherited.Length > 0 then
      interfaceSL.Add('  ' + TypeName + ' = class(' + TypeInherited + ')')
    else
      interfaceSL.Add('  ' + TypeName + ' = class');

    for j := 0 to Fields.Count - 1 do
    begin
      interfaceSL.Add(Fields[j].GenerateInterface);
    end;

    for j := 0 to FMethods.Count - 1 do
    begin
      interfaceSL.Add(TrimRight(FMethods[j].GenerateInterface));
      interfaceSL.Add('');
    end;

    interfaceSL.Add('  end;');

    Result := interfaceSL.Text;
  finally
    FreeAndNil(interfaceSL);
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
  i: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    for i := 0 to FAttributes.Count - 1 do
    begin
      SL.Add('    ' + FAttributes[i]);
    end;
    if Description.Length > 0 then
      SL.Add('    [MVCDoc(' + QuotedStr(Description) + ')]');
    SL.Add('    ' + FFieldName + ' : ' + FFieldType + ';');

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

function TUnitMethod.GenerateImplementation(inOnType: TUnitTypeDefinition): string;
var
  procTypeString: string;
  hasReturn: Boolean;
  param: TUnitParameter;
  paramFlagString: string;
  paramString: string;
  classNameProcIn: string;
  i: Integer;
  funcSL: TStringList;
begin
  hasReturn := False;
  classNameProcIn := '';
  case MethodKind of
    mkProcedure:
      procTypeString := 'procedure';
    mkFunction:
      begin
        procTypeString := 'function';
        hasReturn := True;
      end;
    mkDestructor:
      procTypeString := 'destructor';
    mkConstructor:
      procTypeString := 'constructor';
    mkClassFunction:
      begin
        procTypeString := 'class function';
        hasReturn := True;
      end;
    mkClassProcedure:
      procTypeString := 'class procedure';
    mkClassConstructor:
      procTypeString := 'class constructor';
    mkClassDestructor:
      procTypeString := 'class destructor';
  else
    procTypeString := 'unknown'; // invalid ... will cause a compile error
  end;

  if Assigned(inOnType) then
    classNameProcIn := inOnType.TypeName + '.';

  paramString := '(';
  for param in GetParameters do
  begin
    paramFlagString := '';
    if pfConst in param.Flags then
      paramFlagString := 'const'
    else if pfVar in param.Flags then
      paramFlagString := 'var';

    if paramFlagString.Length > 0 then
      paramFlagString := paramFlagString + ' ';

    paramString := paramString + paramFlagString + param.ParamName + ': ' + param.FType.FTypeName + ';';
  end;

  if paramString.EndsWith(';') then
    paramString := paramString.Remove(paramString.Length - 1);

  paramString := paramString + ')';

  if paramString = '()' then
    paramString := '';

  if hasReturn then
    Result := procTypeString + ' ' + classNameProcIn + FName + paramString + ': ' + ReturnType.FTypeName + ';'
  else
    Result := procTypeString + ' ' + classNameProcIn + FName + paramString + ';';

  funcSL := TStringList.Create;
  try
    funcSL.Text := Result;
    if FVars.Count > 0 then
    begin
      funcSL.Add('var');
      for i := 0 to FVars.Count - 1 do
      begin
        funcSL.Add('  ' + FVars[i].ParamName + ' : ' + FVars[i].ParamType.TypeName + ';');
      end;
    end;
    funcSL.Add('begin');
    funcSL.Add(Content.Text);
    funcSL.Add('end;');

    Result := funcSL.Text;
  finally
    FreeAndNil(funcSL);
  end;
end;

function TUnitMethod.GenerateInterface: string;
var
  procTypeString: string;
  hasReturn: Boolean;
  param: TUnitParameter;
  paramFlagString: string;
  paramString: string;
  attributeString: string;
begin
  hasReturn := False;
  case MethodKind of
    mkProcedure:
      procTypeString := 'procedure';
    mkFunction:
      begin
        procTypeString := 'function';
        hasReturn := True;
      end;
    mkDestructor:
      procTypeString := 'destructor';
    mkConstructor:
      procTypeString := 'constructor';
    mkClassFunction:
      begin
        procTypeString := 'class function';
        hasReturn := True;
      end;
    mkClassProcedure:
      procTypeString := 'class procedure';
    mkClassConstructor:
      procTypeString := 'class constructor';
    mkClassDestructor:
      procTypeString := 'class destructor';
  else
    procTypeString := 'unknown'; // invalid ... will cause a compile error
  end;

  paramString := '(';
  for param in GetParameters do
  begin
    paramFlagString := '';
    if pfConst in param.Flags then
      paramFlagString := 'const'
    else if pfVar in param.Flags then
      paramFlagString := 'var';

    if paramFlagString.Length > 0 then
      paramFlagString := paramFlagString + ' ';
    paramString := paramString + paramFlagString + param.ParamName + ': ' + param.FType.FTypeName + ';';
  end;

  if paramString.EndsWith(';') then
    paramString := paramString.Remove(paramString.Length - 1);

  paramString := paramString + ')';

  if paramString = '()' then
    paramString := '';

  if hasReturn then
    Result := '    ' + procTypeString + ' ' + FName + paramString + ': ' + ReturnType.FTypeName + ';'
  else
    Result := '    ' + procTypeString + ' ' + FName + paramString + ';';

  attributeString := FAttributes.Text;
  Result := attributeString + Result;
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

end.

