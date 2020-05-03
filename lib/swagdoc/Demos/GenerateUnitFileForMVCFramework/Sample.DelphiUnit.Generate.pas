{******************************************************************************}
{                                                                              }
{  Delphi SwagDoc Library                                                      }
{  Copyright (c) 2018 Marcelo Jaloto                                           }
{  https://github.com/marcelojaloto/SwagDoc                                    }
{                                                                              }
{  Sample author: geoffsmith82 - 2019                                          }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}

unit Sample.DelphiUnit.Generate;

interface

uses
  System.Classes,
  System.Json,
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  TUnitTypeDefinition = class;

  TUnitFieldDefinition = class
  strict private
    fFieldName: string;
    fFieldType: string;
    fAttributes: TStringList;
    fDescription: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddAttribute(const pAttribute: string);

    function GenerateInterface: string;

    property FieldName: string read fFieldName write fFieldName;
    property FieldType: string read fFieldType write fFieldType;
    property Description: string read fDescription write fDescription;
  end;

  TUnitParameter = class
  strict private
    fFlags: TParamFlags;
    fType: TUnitTypeDefinition;
    fParamName: string;
    fAttributes: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddAttribute(const pAttribute: string);

    property Attributes: TStringList read fAttributes write fAttributes;
    property ParamName: string read fParamName write fParamName;
    property Flags: TParamFlags read fFlags write fFlags;
    property ParamType: TUnitTypeDefinition read fType write fType;
  end;

  TUnitMethod = class
  strict private
    fAttributes: TStringList;
    fMethodKind: TMethodKind;
    fVisibility: TMemberVisibility;
    fName: string;
    fIsStatic: Boolean;
    fIsClassMethod: Boolean;
    fReturnType: TUnitTypeDefinition;
    fParams: TObjectList<TUnitParameter>;
    fVars: TObjectList<TUnitParameter>;
    fContent: TStringList;

    procedure ParametersToDelphiString(var pParamString: string; pIncludeAttributes: Boolean);
    procedure MethodLocalVarsToDelphiString(pFuncSL: TStringList);

    function MethodKindToDelphiString(var pHasReturn: Boolean): string;
    function GetIsConstructor: Boolean;
    function GetIsDestructor: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddParameter(pParam: TUnitParameter);
    procedure AddLocalVariable(pVar: TUnitParameter);
    procedure AddAttribute(const pAttribute: string);

    function GetParameters: TArray<TUnitParameter>;
    function GenerateInterface: string;
    function GenerateImplementation(pOnType: TUnitTypeDefinition): string;

    property Content: TStringList read fContent write fContent;
    property MethodKind: TMethodKind read fMethodKind write fMethodKind;
    property Visibility: TMemberVisibility read fVisibility write fVisibility;
    property Name: string read fName write fName;
    property IsConstructor: Boolean read GetIsConstructor;
    property IsDestructor: Boolean read GetIsDestructor;
    property IsClassMethod: Boolean read fIsClassMethod write fIsClassMethod;
    // Static: No 'Self' parameter
    property IsStatic: Boolean read fIsStatic write fIsStatic;
    property ReturnType: TUnitTypeDefinition read fReturnType write fReturnType;
  end;

  TUnitTypeDefinition = class
  strict private
    fTypeName: string;
    fTypeInheritedFrom: string;
    fAttributes: TStringList;
    fTypeKind: TTypeKind;
    fForwardDeclare: Boolean;
    fGuid : TGUID;
    fFields: TObjectList<TUnitFieldDefinition>;
    fMethods: TObjectList<TUnitMethod>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddAttribute(const pAttribute: string);

    function GetMethods: TArray<TUnitMethod>;
    function GenerateInterface: string;
    function GenerateForwardInterface: string;

    property Guid: TGUID read fGuid write fGuid;
    property TypeName: string read fTypeName write fTypeName;
    property TypeKind: TTypeKind read fTypeKind write fTypeKind;
    property TypeInherited: string read fTypeInheritedFrom write fTypeInheritedFrom;
    property ForwardDeclare: Boolean read fForwardDeclare write fForwardDeclare;
    property Fields: TObjectList<TUnitFieldDefinition> read fFields;
    property Methods: TObjectList<TUnitMethod> read fMethods;
  end;

  TDelphiUnit = class
  strict private
    fInterfaceUses: TStringList;
    fImplementationUses: TStringList;
    fInterfaceConstant: TStringList;
    fInterfaceVar: TStringList;
    fImplementationConstant: TStringList;
    fUnitName: string;
    fTitle: string;
    fDescription: string;
    fLicense: string;
    fTypeDefinitions: TObjectList<TUnitTypeDefinition>;
    fUnitHasResourceFile: Boolean;
  private
    function GenerateInterfaceVar: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Generate: string;
    function GenerateInterfaceSectionStart: string; virtual;
    function GenerateInterfaceUses: string; virtual;
    function GenerateImplementationSectionStart: string; virtual;
    function GenerateImplementationUses: string; virtual;
    function GenerateImplementationConstants: string; virtual;
    function CreateGUID: TGuid;

    procedure AddInterfaceUnit(const pFilename: string); virtual;
    procedure AddInterfaceConstant(const pName: string; const pValue: string);
    procedure AddInterfaceVar(const pName:string; pTypeInfo: TUnitTypeDefinition);
    procedure AddImplementationUnit(const pFilename: string); virtual;
    procedure AddImplementationConstant(const pName: string; const pValue: string);
    procedure AddType(pTypeInfo: TUnitTypeDefinition);
    procedure SortTypeDefinitions;

    property UnitFile: string read fUnitName write fUnitName;
    property UnitHasResourceFile: Boolean read fUnitHasResourceFile write fUnitHasResourceFile;
    property Title: string read fTitle write fTitle;
    property Description: string read fDescription write fDescription;
    property License: string read fLicense write fLicense;
  end;

implementation

function DelphiVarName(const pVarName: string):string;
begin
  Result := pVarName;
  if Result.ToLower = 'type' then
    Result := '&' + Result
  else if Result.ToLower = 'file' then
    Result := '&' + Result;
end;

{ TDelphiUnit }

procedure TDelphiUnit.AddImplementationConstant(const pName, pValue: string);
begin
  fImplementationConstant.AddPair(pName, pValue);
end;

procedure TDelphiUnit.AddImplementationUnit(const pFilename: string);
var
  vInterfaceIndex : Integer;
begin
  vInterfaceIndex := fInterfaceUses.IndexOf(pFilename);
  if vInterfaceIndex < 0 then
  begin
    if fImplementationUses.IndexOf(pFilename) < 0 then
      fImplementationUses.Add(pFilename);
  end;
end;

procedure TDelphiUnit.AddInterfaceVar(const pName: string; pTypeInfo: TUnitTypeDefinition);
begin
  fInterfaceVar.AddPair(pName, pTypeInfo.TypeName);
end;

procedure TDelphiUnit.AddInterfaceConstant(const pName, pValue: string);
begin
  fInterfaceConstant.AddPair(pName, pValue);
end;

procedure TDelphiUnit.AddInterfaceUnit(const pFilename: string);
var
  vImpIndex : Integer;
begin
  vImpIndex := fImplementationUses.IndexOf(pFilename);
  if vImpIndex >= 0 then
    fImplementationUses.Delete(vImpIndex);

  if fInterfaceUses.IndexOf(pFilename) < 0 then
    fInterfaceUses.Add(pFilename);
end;

procedure TDelphiUnit.AddType(pTypeInfo: TUnitTypeDefinition);
begin
  fTypeDefinitions.Add(pTypeInfo);
end;

constructor TDelphiUnit.Create;
begin
  fInterfaceUses := TStringList.Create;
  fInterfaceConstant := TStringList.Create;
  fInterfaceVar := TStringList.Create;
  fImplementationConstant := TStringList.Create;
  fImplementationUses := TStringList.Create;
  fTypeDefinitions := TObjectList<TUnitTypeDefinition>.Create;
end;

destructor TDelphiUnit.Destroy;
begin
  FreeAndNil(fInterfaceUses);
  FreeAndNil(fImplementationUses);
  FreeAndNil(fInterfaceConstant);
  FreeAndNil(fInterfaceVar);
  FreeAndNil(fImplementationConstant);
  FreeAndNil(fTypeDefinitions);
  inherited Destroy;
end;

function TDelphiUnit.GenerateImplementationConstants: string;
var
  vConstList : TStringList;
  vImpIndex : Integer;
begin
  vConstList := TStringList.Create;
  try
    if fImplementationConstant.Count > 0 then
    begin
      vConstList.Add('const');
      for vImpIndex := 0 to fImplementationConstant.Count - 1 do
      begin
        vConstList.Add('  ' + fImplementationConstant.Names[vImpIndex] + ' = ' + fImplementationConstant.ValueFromIndex[vImpIndex] + ';');
      end;
    end;
    Result := vConstList.Text;
  finally
    FreeAndNil(vConstList);
  end;
end;

function TDelphiUnit.GenerateInterfaceVar: string;
var
  vVarList: TStringList;
  vImpIndex: Integer;
begin
  vVarList := TStringList.Create;
  try
    if fInterfaceVar.Count > 0 then
    begin
      vVarList.Add('var');
      for vImpIndex := 0 to fInterfaceVar.Count - 1 do
      begin
        vVarList.Add('  ' + fInterfaceVar.Names[vImpIndex] + ' : ' + fInterfaceVar.ValueFromIndex[vImpIndex] + ';');
      end;
    end;
    Result := vVarList.Text;
  finally
    FreeAndNil(vVarList);
  end;
end;

function TDelphiUnit.GenerateImplementationSectionStart: string;
var
  vImplementationSection: TStringList;
begin
  vImplementationSection := TStringList.Create;
  try
    vImplementationSection.Add('');
    vImplementationSection.Add('implementation');
    vImplementationSection.Add('');
    Result := vImplementationSection.Text;
  finally
    FreeAndNil(vImplementationSection);
  end;
end;

function TDelphiUnit.GenerateImplementationUses: string;
var
  vUsesList: TStringList;
  vImplIndex: Integer;
begin
  vUsesList := TStringList.Create;
  try
    if fUnitHasResourceFile then
      vUsesList.Add('{$R *.dfm}');
    vUsesList.Add('');
    if fImplementationUses.Count > 0 then
    begin
      vUsesList.Add('uses');
      for vImplIndex := 0 to fImplementationUses.Count - 1 do
      begin
        if vImplIndex = 0 then
          vUsesList.Add('    ' + fImplementationUses[vImplIndex])
        else
          vUsesList.Add('  , ' + fImplementationUses[vImplIndex]);
      end;
      vUsesList.Add('  ;');
    end;
    vUsesList.Add('');
    Result := vUsesList.Text;
  finally
    FreeAndNil(vUsesList);
  end;
end;

function TDelphiUnit.GenerateInterfaceSectionStart: string;
var
  vInterfaceSectionList: TStringList;
begin
  vInterfaceSectionList := TStringList.Create;
  try
    vInterfaceSectionList.Add('unit ' + UnitFile + ';');
    vInterfaceSectionList.Add('');
    vInterfaceSectionList.Add('interface');
    vInterfaceSectionList.Add('');
    Result := vInterfaceSectionList.Text;
  finally
    FreeAndNil(vInterfaceSectionList);
  end;
end;

function TDelphiUnit.GenerateInterfaceUses: string;
var
  vUsesList: TStringList;
  vUseIndex: Integer;
begin
  vUsesList := TStringList.Create;
  try
    if fInterfaceUses.Count > 0 then
    begin
      vUsesList.Add('uses');
      for vUseIndex := 0 to fInterfaceUses.Count - 1 do
      begin
        if vUseIndex = 0 then
          vUsesList.Add('    ' + fInterfaceUses[vUseIndex])
        else
          vUsesList.Add('  , ' + fInterfaceUses[vUseIndex]);
      end;
      vUsesList.Add('  ;');
    end;
    vUsesList.Add('');
    Result := vUsesList.Text;
  finally
    FreeAndNil(vUsesList);
  end;
end;

function TDelphiUnit.Generate: string;
var
  vIndex: Integer;
  vMethod: TUnitMethod;
  vUnitFileList: TStringList;
  vForwardAlreadyDeclared: Boolean;
begin
  vForwardAlreadyDeclared := False;
  vUnitFileList := TStringList.Create;
  try
    vUnitFileList.Add(GenerateInterfaceSectionStart);
    vUnitFileList.Add(GenerateInterfaceUses);
    vUnitFileList.Add('(*');
    vUnitFileList.Add('Title: ' + Title);
    vUnitFileList.Add('Description: ' + Description);
    vUnitFileList.Add('License: ' + License);
    vUnitFileList.Add('*)');
    vUnitFileList.Add('');
    vUnitFileList.Add('type');

    SortTypeDefinitions;

    if fInterfaceConstant.Count > 0 then
    begin
      vUnitFileList.Add('const');
      for vIndex := 0 to fInterfaceConstant.Count - 1 do
      begin
        vUnitFileList.Add('  ' + fInterfaceConstant.Names[vIndex] + ' = ' + fInterfaceConstant.ValueFromIndex[vIndex] + ';');
      end;
    end;

    for vIndex := 0 to fTypeDefinitions.Count - 1 do
    begin
      if fTypeDefinitions[vIndex].ForwardDeclare then
      begin
        if not vForwardAlreadyDeclared then
          vUnitFileList.Add('  // Forward Declarations');
        vUnitFileList.Add(fTypeDefinitions[vIndex].GenerateForwardInterface);
        vForwardAlreadyDeclared := True;
      end;
    end;

    for vIndex := 0 to fTypeDefinitions.Count - 1 do
    begin
      vUnitFileList.Add(fTypeDefinitions[vIndex].GenerateInterface);
    end;

    vUnitFileList.Add(GenerateInterfaceVar);

    vUnitFileList.Add(GenerateImplementationSectionStart);
    vUnitFileList.Add(GenerateImplementationUses);
    vUnitFileList.Add('');
    GenerateImplementationConstants;
    for vIndex := 0 to fTypeDefinitions.Count - 1 do
    begin
      for vMethod in fTypeDefinitions[vIndex].GetMethods do
      begin
        vUnitFileList.Add(vMethod.GenerateImplementation(fTypeDefinitions[vIndex]));
      end;
    end;
    vUnitFileList.Add('end.');
    Result := vUnitFileList.Text;
  finally
    FreeAndNil(vUnitFileList);
  end;
end;

function TDelphiUnit.CreateGUID: TGuid;
var
  vGuid: TGUID;
begin
  System.SysUtils.CreateGuid(vGuid);
  Result := vGuid;
end;

procedure TDelphiUnit.SortTypeDefinitions;
begin
  { TODO : Make this much more advanced to handle dependency ordering of declarations }

  fTypeDefinitions.Sort(TComparer<TUnitTypeDefinition>.Construct(
    function(const vTypeA, vTypeB: TUnitTypeDefinition): Integer
    begin
      if vTypeA.TypeName = 'TMyMVCController' then
        Result := 1
      else if vTypeB.TypeName = 'TMyMVCController' then
        Result := -1
      else
        Result := CompareText(vTypeA.TypeName, vTypeB.TypeName);
    end));
end;

{ TTypeDefinition }

constructor TUnitTypeDefinition.Create;
begin
  fAttributes := TStringList.Create;
  fFields := TObjectList<TUnitFieldDefinition>.Create;
  fMethods := TObjectList<TUnitMethod>.Create;
  fTypeKind := tkClass;
  fForwardDeclare := False;
end;

destructor TUnitTypeDefinition.Destroy;
begin
  FreeAndNil(fAttributes);
  FreeAndNil(fFields);
  FreeAndNil(fMethods);
  inherited Destroy;
end;

procedure TUnitTypeDefinition.AddAttribute(const pAttribute: string);
begin
  fAttributes.Add(pAttribute);
end;

function TUnitTypeDefinition.GenerateForwardInterface: string;
begin
  if fTypeKind = tkClass then
    Result := '  ' + TypeName + ' : class;'
  else if fTypeKind = tkInterface then
    Result := '  ' + TypeName + ' : interface;'
  else
    Result := '  ' + TypeName + 'xxxx';
end;

function TUnitTypeDefinition.GenerateInterface: string;
var
  vInterfaceList: TStringList;
  vAttributeIndex: Integer;
  vFieldIndex: Integer;
begin
  vInterfaceList := TStringList.Create;
  try
    for vAttributeIndex := 0 to fAttributes.Count - 1 do
    begin
      vInterfaceList.Add(fAttributes[vAttributeIndex]);
    end;
    if fTypeKind = tkClass then
    begin
      if TypeInherited.Length > 0 then
        vInterfaceList.Add('  ' + TypeName + ' = class(' + TypeInherited + ')')
      else
        vInterfaceList.Add('  ' + TypeName + ' = class');
    end
    else if fTypeKind = tkInterface then
    begin
      if TypeInherited.Length > 0 then
      begin
        vInterfaceList.Add('  ' + TypeName + ' = interface(' + TypeInherited + ')');
        vInterfaceList.Add('    [' + GUIDToString(fGuid).QuotedString + ']');
      end
      else
      begin
        vInterfaceList.Add('  ' + TypeName + ' = interface');
        vInterfaceList.Add('    [' + GUIDToString(fGuid).QuotedString + ']');
      end;
    end;

    for vFieldIndex := 0 to fFields.Count - 1 do
    begin
      vInterfaceList.Add(fFields[vFieldIndex].GenerateInterface);
    end;

    for vFieldIndex := 0 to fMethods.Count - 1 do
    begin
      vInterfaceList.Add(TrimRight(fMethods[vFieldIndex].GenerateInterface));
      vInterfaceList.Add('');
    end;

    vInterfaceList.Add('  end;');

    Result := vInterfaceList.Text;
  finally
    FreeAndNil(vInterfaceList);
  end;
end;

function TUnitTypeDefinition.GetMethods: TArray<TUnitMethod>;
var
  vMethodIndex: Integer;
begin
  SetLength(Result, fMethods.Count);
  for vMethodIndex := 0 to fMethods.Count - 1 do
  begin
    Result[vMethodIndex] := fMethods[vMethodIndex];
  end;
end;

{ TFieldDefinition }

constructor TUnitFieldDefinition.Create;
begin
  fAttributes := TStringList.Create;
end;

destructor TUnitFieldDefinition.Destroy;
begin
  FreeAndNil(fAttributes);
  inherited Destroy;
end;

procedure TUnitFieldDefinition.AddAttribute(const pAttribute: string);
begin
  fAttributes.Add(pAttribute);
end;

function TUnitFieldDefinition.GenerateInterface: string;
var
  vAttributeIndex: Integer;
  vInterfaceList: TStringList;
  vType: string;
begin
  vInterfaceList := TStringList.Create;
  try
    vType := fFieldType;
    for vAttributeIndex := 0 to fAttributes.Count - 1 do
    begin
      vInterfaceList.Add('    ' + fAttributes[vAttributeIndex]);
    end;

    if Description.Length > 0 then
      vInterfaceList.Add('    [MVCDoc(' + QuotedStr(Description) + ')]');

    vInterfaceList.Add('    ' + DelphiVarName(fFieldName + ' : ' + vType + ';'));
    Result := vInterfaceList.Text;
  finally
    FreeAndNil(vInterfaceList);
  end;
end;

{ TUnitMethod }

constructor TUnitMethod.Create;
begin
  fParams := TObjectList<TUnitParameter>.Create;
  fAttributes := TStringList.Create;
  fVars := TObjectList<TUnitParameter>.Create;
  fContent := TStringList.Create;
end;

destructor TUnitMethod.Destroy;
begin
  FreeAndNil(fParams);
  FreeAndNil(fAttributes);
  FreeAndNil(fVars);
  FreeAndNil(fContent);
  inherited Destroy;
end;

procedure TUnitMethod.AddAttribute(const pAttribute: string);
begin
  fAttributes.Add(pAttribute);
end;

procedure TUnitMethod.AddLocalVariable(pVar: TUnitParameter);
begin
  fVars.Add(pVar);
end;

procedure TUnitMethod.AddParameter(pParam: TUnitParameter);
begin
  fParams.Add(pParam);
end;

procedure TUnitMethod.MethodLocalVarsToDelphiString(pFuncSL: TStringList);
var
  vVarIndex: Integer;
begin
  if fVars.Count <= 0 then
    Exit;

  pFuncSL.Add('var');
  for vVarIndex := 0 to fVars.Count - 1 do
  begin
    pFuncSL.Add('  ' + fVars[vVarIndex].ParamName + ' : ' + fVars[vVarIndex].ParamType.TypeName + ';');
  end;
end;

procedure TUnitMethod.ParametersToDelphiString(var pParamString: string; pIncludeAttributes: Boolean);
var
  vParam: TUnitParameter;
  vParamFlagString: string;
  vParamName: string;
  vParamAttributeString : string;
  vAttributeIndex: Integer;
begin
  pParamString := '(';
  for vParam in GetParameters do
  begin
    vParamFlagString := '';
    if pfConst in vParam.Flags then
      vParamFlagString := 'const'
    else if pfVar in vParam.Flags then
      vParamFlagString := 'var'
    else if pfOut in vParam.Flags then
      vParamFlagString := 'out'
    else if pfArray in vParam.Flags then
      vParamFlagString := 'array of';
    if vParamFlagString.Length > 0 then
      vParamFlagString := vParamFlagString + ' ';

    if pIncludeAttributes then
    begin
      for vAttributeIndex := 0 to vParam.Attributes.Count - 1 do
      begin
        vParamAttributeString := vParamAttributeString + ' ' + vParam.Attributes[vAttributeIndex];
      end;

      vParamAttributeString := Trim(vParamAttributeString) + ' ';
    end;

    vParamName := DelphiVarName(vParam.ParamName);
    pParamString := pParamString + vParamAttributeString + vParamFlagString + vParamName + ': ' + vParam.ParamType.TypeName + '; ';
  end;
  if pParamString.EndsWith('; ') then
    pParamString := pParamString.Remove(pParamString.Length - 2);
  pParamString := pParamString + ')';
  if pParamString = '()' then
    pParamString := '';
end;

function TUnitMethod.MethodKindToDelphiString(var pHasReturn: Boolean): string;
begin
  case MethodKind of
    mkProcedure:
      Result := 'procedure';
    mkFunction:
      begin
        Result := 'function';
        pHasReturn := True;
      end;
    mkDestructor:
      Result := 'destructor';
    mkConstructor:
      Result := 'constructor';
    mkClassFunction:
      begin
        Result := 'class function';
        pHasReturn := True;
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

function TUnitMethod.GenerateImplementation(pOnType: TUnitTypeDefinition): string;
var
  vProcTypeString: string;
  vHasReturn: Boolean;
  vParamString: string;
  vClassNameProcIn: string;
  vFunctionList: TStringList;
begin
  vHasReturn := False;
  vClassNameProcIn := '';
  vProcTypeString := MethodKindToDelphiString(vHasReturn);

  if Assigned(pOnType) then
    vClassNameProcIn := pOnType.TypeName + '.';
  ParametersToDelphiString(vParamString, False);

  if vHasReturn then
    Result := vProcTypeString + ' ' + vClassNameProcIn + fName + vParamString + ': ' + ReturnType.TypeName + ';'
  else
    Result := vProcTypeString + ' ' + vClassNameProcIn + fName + vParamString + ';';

  vFunctionList := TStringList.Create;
  try
    vFunctionList.Text := Result;

    MethodLocalVarsToDelphiString(vFunctionList);

    vFunctionList.Add('begin');
    vFunctionList.Add(Content.Text);
    vFunctionList.Add('end;');

    Result := vFunctionList.Text;
  finally
    FreeAndNil(vFunctionList);
  end;
end;

function TUnitMethod.GenerateInterface: string;
var
  vProcTypeString: string;
  vHasReturn: Boolean;
  vParamString: string;
  vAttributeString: string;
begin
  vHasReturn := False;
  vProcTypeString := MethodKindToDelphiString(vHasReturn);

  ParametersToDelphiString(vParamString, True);

  if vHasReturn then
    Result := '    ' + vProcTypeString + ' ' + fName + vParamString + ': ' + ReturnType.TypeName + ';'
  else
    Result := '    ' + vProcTypeString + ' ' + fName + vParamString + ';';

  vAttributeString := fAttributes.Text;
  Result := vAttributeString + Result;
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
  vParam: Integer;
begin
  SetLength(Result, fParams.Count);
  for vParam := 0 to fParams.Count - 1 do
  begin
    Result[vParam] := fParams[vParam];
  end;
end;

{ TUnitParameter }

constructor TUnitParameter.Create;
begin
  fAttributes := TStringList.Create;
end;

destructor TUnitParameter.Destroy;
begin
  FreeAndNil(fAttributes);
  FreeAndNil(fType);
  inherited Destroy;
end;

procedure TUnitParameter.AddAttribute(const pAttribute: string);
begin
  fAttributes.Add(pAttribute);
end;

end.

