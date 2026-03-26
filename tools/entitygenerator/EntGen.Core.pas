// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

unit EntGen.Core;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  FireDAC.Stan.Intf, FireDAC.Comp.Client, FireDAC.Phys.Intf;

type
  TEntGenNameCase = (ncLowerCase, ncUpperCase, ncCamelCase, ncPascalCase, ncSnakeCase, ncAsIs);

  TEntGenFieldNameFormat = (fnAsIs, fnPascalCase);

  TEntGenTableClassMap = TDictionary<string, string>; // TABLE_NAME -> CLASS_NAME

  TEntGenConfig = record
    Schema: string;
    NameCase: TEntGenNameCase;
    FieldNameFormat: TEntGenFieldNameFormat;
    GenerateMapping: Boolean;
    ClassAsAbstract: Boolean;
    Tables: TArray<string>;       // empty = all
    ExcludeTables: TArray<string>;
    TableClassMap: TEntGenTableClassMap; // optional, nil = auto naming
  end;

  TEntGenLogProc = reference to procedure(const AMessage: string);

  TMVCEntityGenerator = class
  private
    const INDENT = '  ';
  private
    fIntfBuff: TStringBuilder;
    fImplBuff: TStringBuilder;
    fInitBuff: TStringBuilder;
    fConnection: TFDConnection;
    fConfig: TEntGenConfig;
    fOnLog: TEntGenLogProc;
    procedure Log(const AMsg: string); overload;
    procedure Log(const AFmt: string; const AArgs: array of const); overload;

    { Naming }
    class function GetDelphiClassName(const ATableName: string): string;
    class function GetFieldVarName(const AFieldName: string): string;
    class function IsReservedKeyword(const AValue: string): Boolean;
    class function GetUniqueFieldNames(AMetaDS: TFDMetaInfoQuery;
      AFormatAsPascalCase: Boolean): TArray<string>;
    class function GetDelphiType(const AFireDACType: TFDDataType;
      const AColumnAttribs: TFDDataAttributes;
      const AForceNullable: Boolean = False): string;
    class function GetColumnAttributes(AMetaDS: TFDMetaInfoQuery): TFDDataAttributes;
    class function NameCaseToStr(ANameCase: TEntGenNameCase): string;
    class function IsAutoIncField(const AColumnAttribs: TFDDataAttributes;
      const AColumnTypeName: string): Boolean;

    { Emit methods }
    procedure EmitLicenseHeader;
    procedure EmitUnitHeader(const AUnitName: string);
    procedure EmitUnitFooter;
    procedure EmitClassHeader(const ATableName, AClassName: string;
      ANameCase: TEntGenNameCase; AIsAbstract: Boolean);
    procedure EmitClassFooter;
    procedure EmitField(const ADatabaseFieldName, AUniqueFieldName: string;
      AFieldDataType: TFDDataType; const AColumnAttribs: TFDDataAttributes;
      AIsPK: Boolean; const AColumnTypeName: string);
    procedure EmitProperty(const AFieldName: string;
      const AColumnAttribs: TFDDataAttributes;
      AFieldDataType: TFDDataType; AIsPK: Boolean;
      const AColumnTypeName: string);

    { Table filtering }
    class function MatchesPattern(const ATableName, APattern: string): Boolean;
    class function MatchesAnyPattern(const ATableName: string;
      const APatterns: TArray<string>): Boolean;
    function ShouldGenerateTable(const ATableName: string): Boolean;
    procedure GenerateEntity(const ATableName: string);
  public
    constructor Create(AConnection: TFDConnection);
    destructor Destroy; override;
    function Generate(const AConfig: TEntGenConfig): string;
    procedure GenerateToFile(const AConfig: TEntGenConfig; const AOutputFile: string);
    property OnLog: TEntGenLogProc read fOnLog write fOnLog;
  end;

implementation

uses
  System.IOUtils, System.TypInfo, System.DateUtils,
  System.RegularExpressions, System.Masks,
  FireDAC.Stan.Option, FireDAC.DatS,
  MVCFramework.Commons;

const
  PASCAL_KEYWORDS =
    ';and;array;as;asm;begin;break;case;class;const;constref;constructor;' +
    'continue;destructor;dispose;div;do;downto;else;end;except;exit;exports;' +
    'false;file;finalization;finally;for;function;goto;if;implementation;in;' +
    'inherited;initialization;inline;interface;is;label;library;mod;new;nil;' +
    'not;object;of;on;operator;or;out;packed;procedure;program;property;raise;' +
    'record;reference;repeat;self;set;shl;shr;string;then;threadvar;to;true;' +
    'try;type;unit;until;uses;var;while;with;xor;';

  META_F_COLUMN_NAME = 'COLUMN_NAME';
  META_F_COLUMN_DATATYPE = 'COLUMN_DATATYPE';
  META_F_COLUMN_TYPENAME = 'COLUMN_TYPENAME';
  META_F_COLUMN_ATTRIBUTES = 'COLUMN_ATTRIBUTES';

{ TMVCEntityGenerator }

constructor TMVCEntityGenerator.Create(AConnection: TFDConnection);
begin
  inherited Create;
  fConnection := AConnection;
  fIntfBuff := TStringBuilder.Create;
  fImplBuff := TStringBuilder.Create;
  fInitBuff := TStringBuilder.Create;
end;

destructor TMVCEntityGenerator.Destroy;
begin
  fIntfBuff.Free;
  fImplBuff.Free;
  fInitBuff.Free;
  inherited;
end;

procedure TMVCEntityGenerator.Log(const AMsg: string);
begin
  if Assigned(fOnLog) then
    fOnLog(AMsg);
end;

procedure TMVCEntityGenerator.Log(const AFmt: string; const AArgs: array of const);
begin
  Log(Format(AFmt, AArgs));
end;

{ --- Naming helpers --- }

class function TMVCEntityGenerator.IsReservedKeyword(const AValue: string): Boolean;
begin
  Result := PASCAL_KEYWORDS.Contains(';' + AValue.ToLower + ';');
end;

class function TMVCEntityGenerator.GetDelphiClassName(const ATableName: string): string;
var
  lTableName: string;
  lNextLetter: Integer;
  lNextLetterChar: string;
begin
  lTableName := ATableName.ToLower.DeQuotedString('"').Replace(' ', '_', [rfReplaceAll]);
  lTableName := lTableName.DeQuotedString('"').Replace('.', '__', [rfReplaceAll]);
  Result := 'T' + lTableName.Substring(0, 1).ToUpper + lTableName.Substring(1).ToLower;
  while Result.IndexOf('_') > -1 do
  begin
    lNextLetter := Result.IndexOf('_') + 1;
    lNextLetterChar := UpperCase(Result.Chars[lNextLetter]);
    Result := Result.Remove(lNextLetter, 1);
    Result := Result.Insert(Result.IndexOf('_') + 1, lNextLetterChar);
    Result := Result.Remove(Result.IndexOf('_'), 1);
  end;
end;

class function TMVCEntityGenerator.GetFieldVarName(const AFieldName: string): string;
begin
  if AFieldName.Length <= 2 then
    Exit('f' + AFieldName.ToUpper);
  Result := 'f' + AFieldName;
end;

class function TMVCEntityGenerator.GetColumnAttributes(AMetaDS: TFDMetaInfoQuery): TFDDataAttributes;
var
  I: Integer;
begin
  I := AMetaDS.FieldByName(META_F_COLUMN_ATTRIBUTES).AsInteger;
  Result := TFDDataAttributes(Pointer(@I)^);
end;

class function TMVCEntityGenerator.GetDelphiType(
  const AFireDACType: TFDDataType;
  const AColumnAttribs: TFDDataAttributes;
  const AForceNullable: Boolean): string;
begin
  case AFireDACType of
    dtWideString, dtWideMemo:
      Result := 'String';
    dtAnsiString, dtMemo:
      Result := 'String';
    dtByte:
      Result := 'Byte';
    dtInt16:
      Result := 'Int16';
    dtUInt16:
      Result := 'UInt16';
    dtInt32:
      Result := 'Int32';
    dtUInt32:
      Result := 'UInt32';
    dtInt64:
      Result := 'Int64';
    dtUInt64:
      Result := 'UInt64';
    dtBoolean:
      Result := 'Boolean';
    dtDouble, dtExtended:
      Result := 'Double';
    dtSingle:
      Result := 'Single';
    dtCurrency, dtBCD, dtFmtBCD:
      Result := 'Currency';
    dtDate:
      Result := 'TDate';
    dtTime:
      Result := 'TTime';
    dtDateTime:
      Result := 'TDateTime';
    dtTimeIntervalFull:
      Result := 'TDateTime {dtTimeIntervalFull}';
    dtDateTimeStamp:
      Result := 'TDateTime {dtDateTimeStamp}';
    dtDateTimeStampOff:
      Result := 'TDateTime {dtDateTimeStampOff}';
    dtBlob:
      Result := 'TStream';
    dtXML:
      Result := 'String {XML}';
    dtGuid:
      Result := 'TGuid';
  else
    Result := '<UNSUPPORTED TYPE: ' + GetEnumName(TypeInfo(TFDDataType), Ord(AFireDACType)) + '>';
  end;

  if AForceNullable or ((Result <> 'TStream') and (caAllowNull in AColumnAttribs)) then
    Result := 'Nullable' + Result;
end;

class function TMVCEntityGenerator.GetUniqueFieldNames(
  AMetaDS: TFDMetaInfoQuery;
  AFormatAsPascalCase: Boolean): TArray<string>;
var
  I: Integer;
  lList: TStringList;
  lF, lFTemp, lFieldName: string;
  lCount: Integer;
begin
  AMetaDS.First;
  SetLength(Result, AMetaDS.RecordCount);
  lList := TStringList.Create;
  try
    lList.Sorted := True;
    I := 0;
    while not AMetaDS.Eof do
    begin
      lFieldName := AMetaDS.FieldByName(META_F_COLUMN_NAME).AsString;
      lCount := 0;
      if AFormatAsPascalCase then
        lF := CamelCase(lFieldName, True)
      else
        lF := lFieldName;

      if lList.IndexOf(lF) > -1 then
        lF := lFieldName;

      lFTemp := lF;
      if IsReservedKeyword(lFTemp) then
        lFTemp := '_' + lFTemp;

      while lList.IndexOf(lFTemp) > -1 do
      begin
        Inc(lCount);
        lFTemp := lF + '__' + IntToStr(lCount);
      end;
      lF := lFTemp;
      lList.Add(lF);
      Result[I] := lF;
      Inc(I);
      AMetaDS.Next;
    end;
  finally
    lList.Free;
  end;
end;

class function TMVCEntityGenerator.NameCaseToStr(ANameCase: TEntGenNameCase): string;
begin
  case ANameCase of
    ncLowerCase: Result := 'LowerCase';
    ncUpperCase: Result := 'UpperCase';
    ncCamelCase: Result := 'CamelCase';
    ncPascalCase: Result := 'PascalCase';
    ncSnakeCase: Result := 'SnakeCase';
    ncAsIs: Result := 'AsIs';
  else
    Result := 'LowerCase';
  end;
end;

{ --- Emit methods --- }

procedure TMVCEntityGenerator.EmitLicenseHeader;
begin
  fIntfBuff.AppendLine('// ***************************************************************************');
  fIntfBuff.AppendLine('//');
  fIntfBuff.AppendLine('// Delphi MVC Framework');
  fIntfBuff.AppendLine('//');
  fIntfBuff.AppendLine('// Copyright (c) 2010-' + YearOf(Date).ToString + ' Daniele Teti and the DMVCFramework Team');
  fIntfBuff.AppendLine('//');
  fIntfBuff.AppendLine('// https://github.com/danieleteti/delphimvcframework');
  fIntfBuff.AppendLine('//');
  fIntfBuff.AppendLine('// ***************************************************************************');
  fIntfBuff.AppendLine('//');
  fIntfBuff.AppendLine('// Licensed under the Apache License, Version 2.0 (the "License");');
  fIntfBuff.AppendLine('// you may not use this file except in compliance with the License.');
  fIntfBuff.AppendLine('// You may obtain a copy of the License at');
  fIntfBuff.AppendLine('//');
  fIntfBuff.AppendLine('// http://www.apache.org/licenses/LICENSE-2.0');
  fIntfBuff.AppendLine('//');
  fIntfBuff.AppendLine('// Unless required by applicable law or agreed to in writing, software');
  fIntfBuff.AppendLine('// distributed under the License is distributed on an "AS IS" BASIS,');
  fIntfBuff.AppendLine('// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.');
  fIntfBuff.AppendLine('// See the License for the specific language governing permissions and');
  fIntfBuff.AppendLine('// limitations under the License.');
  fIntfBuff.AppendLine('//');
  fIntfBuff.AppendLine('// ***************************************************************************');
  fIntfBuff.AppendLine('');
end;

procedure TMVCEntityGenerator.EmitUnitHeader(const AUnitName: string);
begin
  fIntfBuff.AppendLine('unit ' + AUnitName + ';');
  fIntfBuff.AppendLine('');
  fIntfBuff.AppendLine('interface');
  fIntfBuff.AppendLine('');
  fIntfBuff.AppendLine('uses');
  fIntfBuff.AppendLine('  MVCFramework.Serializer.Commons,');
  fIntfBuff.AppendLine('  MVCFramework.Nullables,');
  fIntfBuff.AppendLine('  MVCFramework.ActiveRecord,');
  fIntfBuff.AppendLine('  System.Classes;');
  fIntfBuff.AppendLine('');
  fIntfBuff.AppendLine('type');
  fIntfBuff.AppendLine('');

  fImplBuff.AppendLine('implementation');
  fImplBuff.AppendLine('');

  if fConfig.GenerateMapping then
  begin
    fInitBuff.AppendLine('initialization');
    fInitBuff.AppendLine('');
  end;
end;

procedure TMVCEntityGenerator.EmitUnitFooter;
begin
  if fConfig.GenerateMapping then
    fInitBuff.AppendLine('');
  fInitBuff.Append('end.');
end;

procedure TMVCEntityGenerator.EmitClassHeader(const ATableName, AClassName: string;
  ANameCase: TEntGenNameCase; AIsAbstract: Boolean);
var
  lAbstract: string;
begin
  fIntfBuff.AppendLine(INDENT + '[MVCNameCase(nc' + NameCaseToStr(ANameCase) + ')]');
  fIntfBuff.AppendLine(INDENT + Format('[MVCTable(''%s'')]', [ATableName]));
  lAbstract := '';
  if AIsAbstract then
    lAbstract := ' abstract';
  fIntfBuff.AppendLine(INDENT + AClassName + ' = class' + lAbstract + '(TMVCActiveRecord)');

  if fConfig.GenerateMapping then
    fInitBuff.AppendLine(Format('ActiveRecordMappingRegistry.AddEntity(''%s'', %s);',
      [ATableName.ToLower, AClassName]));
end;

procedure TMVCEntityGenerator.EmitClassFooter;
begin
  fIntfBuff.AppendLine(INDENT + 'end;');
  fIntfBuff.AppendLine('');
end;

class function TMVCEntityGenerator.IsAutoIncField(
  const AColumnAttribs: TFDDataAttributes;
  const AColumnTypeName: string): Boolean;
begin
  Result := (caAutoInc in AColumnAttribs) or
    AColumnTypeName.ToLower.Contains('identity') or
    AColumnTypeName.ToLower.Contains('serial');
end;

procedure TMVCEntityGenerator.EmitField(
  const ADatabaseFieldName, AUniqueFieldName: string;
  AFieldDataType: TFDDataType;
  const AColumnAttribs: TFDDataAttributes;
  AIsPK: Boolean;
  const AColumnTypeName: string);
var
  lRTTIAttrib, lField: string;
  lColType: string;
  lIsAutoInc: Boolean;
begin
  lIsAutoInc := IsAutoIncField(AColumnAttribs, AColumnTypeName);
  if AIsPK then
  begin
    if lIsAutoInc then
      lRTTIAttrib := Format('[MVCTableField(''%s'', [foPrimaryKey, foAutoGenerated])]', [ADatabaseFieldName])
    else
      lRTTIAttrib := Format('[MVCTableField(''%s'', [foPrimaryKey])]', [ADatabaseFieldName]);
  end
  else
  begin
    lColType := AColumnTypeName.ToLower;
    if lColType.Contains('json') or lColType.Contains('xml') then
      lRTTIAttrib := Format('[MVCTableField(''%s'', [], ''%s'')]', [ADatabaseFieldName, lColType])
    else
      lRTTIAttrib := Format('[MVCTableField(''%s'')]', [ADatabaseFieldName]);
  end;

  if AIsPK and lIsAutoInc then
    lField := GetFieldVarName(AUniqueFieldName) + ': ' + GetDelphiType(AFieldDataType, AColumnAttribs, True) + ';'
  else
    lField := GetFieldVarName(AUniqueFieldName) + ': ' + GetDelphiType(AFieldDataType, AColumnAttribs) + ';';

  if GetDelphiType(AFieldDataType, AColumnAttribs).ToUpper.Contains('UNSUPPORTED TYPE') then
  begin
    lRTTIAttrib := '//' + lRTTIAttrib;
    lField := '//' + lField;
  end
  else
  begin
    lRTTIAttrib := '  ' + lRTTIAttrib;
    lField := '  ' + lField;
  end;
  fIntfBuff.AppendLine(INDENT + lRTTIAttrib);
  fIntfBuff.Append(INDENT + lField + sLineBreak);
end;

procedure TMVCEntityGenerator.EmitProperty(
  const AFieldName: string;
  const AColumnAttribs: TFDDataAttributes;
  AFieldDataType: TFDDataType;
  AIsPK: Boolean;
  const AColumnTypeName: string);
var
  lProp: string;
  lFieldVar: string;
begin
  lFieldVar := GetFieldVarName(AFieldName);
  if AIsPK then
    lProp := 'property ' + lFieldVar.Substring(1) + ': ' +
      GetDelphiType(AFieldDataType, AColumnAttribs,
        (caAllowNull in AColumnAttribs) or IsAutoIncField(AColumnAttribs, AColumnTypeName)) +
      ' read ' + lFieldVar + ' write ' + lFieldVar + ';'
  else
    lProp := 'property ' + lFieldVar.Substring(1) + ': ' +
      GetDelphiType(AFieldDataType, AColumnAttribs) +
      ' read ' + lFieldVar + ' write ' + lFieldVar + ';';

  if GetDelphiType(AFieldDataType, AColumnAttribs).ToUpper.Contains('UNSUPPORTED TYPE') then
    lProp := '  //' + lProp
  else
    lProp := '  ' + lProp;

  fIntfBuff.AppendLine(INDENT + lProp);
end;

{ --- Table filtering --- }

class function TMVCEntityGenerator.MatchesPattern(const ATableName, APattern: string): Boolean;
var
  lPattern: string;
begin
  lPattern := APattern.Trim;
  if lPattern.IsEmpty then
    Exit(False);

  { Regex: pattern enclosed in / / }
  if lPattern.StartsWith('/') and lPattern.EndsWith('/') and (lPattern.Length > 2) then
    Exit(TRegEx.IsMatch(ATableName, lPattern.Substring(1, lPattern.Length - 2), [roIgnoreCase]));

  { Wildcard: contains * or ? -> use TMask }
  if lPattern.Contains('*') or lPattern.Contains('?') then
    Exit(MatchesMask(ATableName, lPattern));

  { Exact match (case insensitive) }
  Result := SameText(lPattern, ATableName);
end;

class function TMVCEntityGenerator.MatchesAnyPattern(const ATableName: string;
  const APatterns: TArray<string>): Boolean;
var
  S: string;
begin
  for S in APatterns do
    if MatchesPattern(ATableName, S) then
      Exit(True);
  Result := False;
end;

function TMVCEntityGenerator.ShouldGenerateTable(const ATableName: string): Boolean;
begin
  { If explicit table list specified, table must match at least one pattern }
  if Length(fConfig.Tables) > 0 then
  begin
    if not MatchesAnyPattern(ATableName, fConfig.Tables) then
      Exit(False);
  end;

  { Check exclude list }
  if Length(fConfig.ExcludeTables) > 0 then
  begin
    if MatchesAnyPattern(ATableName, fConfig.ExcludeTables) then
      Exit(False);
  end;

  Result := True;
end;

{ --- Entity generation for a single table --- }

procedure TMVCEntityGenerator.GenerateEntity(const ATableName: string);
var
  lClassName: string;
  lKeyFields: TStringList;
  lQryMeta: TFDMetaInfoQuery;
  lUniqueFieldNames: TArray<string>;
  lFieldNamesToInit: TArray<string>;
  lTypesName: TArray<string>;
  lFieldDataType: TFDDataType;
  lColAttrib: TFDDataAttributes;
  lFieldName, lColumnTypeName: string;
  I, F: Integer;
begin
  if Assigned(fConfig.TableClassMap) and fConfig.TableClassMap.TryGetValue(ATableName, lClassName) then
    { Use explicit mapping }
  else
    lClassName := GetDelphiClassName(ATableName);
  if fConfig.ClassAsAbstract then
    lClassName := lClassName.Chars[0] + 'Custom' + lClassName.Substring(1);

  Log('  Generating [%s] for table [%s]', [lClassName, ATableName]);

  EmitClassHeader(ATableName, lClassName, fConfig.NameCase, fConfig.ClassAsAbstract);

  lKeyFields := TStringList.Create;
  lQryMeta := TFDMetaInfoQuery.Create(nil);
  try
    lQryMeta.Connection := fConnection;
    fConnection.GetKeyFieldNames(
      fConnection.Params.Database,
      fConfig.Schema,
      ATableName, '', lKeyFields);
    { Fallback: if GetKeyFieldNames returns nothing, query PK metadata directly }
    if lKeyFields.Count = 0 then
    begin
      fConnection.GetKeyFieldNames('', fConfig.Schema, ATableName, '', lKeyFields);
    end;
    if lKeyFields.Count = 0 then
    begin
      var lPKQuery := TFDMetaInfoQuery.Create(nil);
      try
        lPKQuery.Connection := fConnection;
        lPKQuery.MetaInfoKind := mkPrimaryKeyFields;
        lPKQuery.ObjectName := ATableName;
        lPKQuery.Open;
        while not lPKQuery.Eof do
        begin
          lKeyFields.Add(lPKQuery.FieldByName('COLUMN_NAME').AsString);
          lPKQuery.Next;
        end;
      finally
        lPKQuery.Free;
      end;
    end;
    lQryMeta.MetaInfoKind := mkTableFields;
    lQryMeta.ObjectName := ATableName;
    lQryMeta.SchemaName := fConfig.Schema;
    lQryMeta.CatalogName := fConnection.Params.Database;
    lQryMeta.Open;
    lQryMeta.FetchAll;
    lQryMeta.First;

    lFieldNamesToInit := [];
    lTypesName := [];
    lUniqueFieldNames := GetUniqueFieldNames(lQryMeta, fConfig.FieldNameFormat = fnPascalCase);

    { Private fields }
    fIntfBuff.AppendLine(INDENT + 'private');
    I := 0;
    lQryMeta.First;
    while not lQryMeta.Eof do
    begin
      lColAttrib := GetColumnAttributes(lQryMeta);
      lFieldDataType := TFDDataType(lQryMeta.FieldByName(META_F_COLUMN_DATATYPE).AsInteger);
      lFieldName := lQryMeta.FieldByName(META_F_COLUMN_NAME).AsString;
      lColumnTypeName := lQryMeta.FieldByName(META_F_COLUMN_TYPENAME).AsString;

      EmitField(
        lFieldName,
        lUniqueFieldNames[I],
        lFieldDataType,
        lColAttrib,
        lKeyFields.IndexOf(lFieldName) > -1,
        lColumnTypeName);

      if GetDelphiType(lFieldDataType, lColAttrib) = 'TStream' then
      begin
        lFieldNamesToInit := lFieldNamesToInit + [GetFieldVarName(lUniqueFieldNames[I])];
        lTypesName := lTypesName + ['TMemoryStream'];
      end;
      Inc(I);
      lQryMeta.Next;
    end;

    { Constructor / Destructor - only if there are fields to initialize }
    fIntfBuff.AppendLine(INDENT + 'public');

    if Length(lFieldNamesToInit) > 0 then
    begin
      fIntfBuff.AppendLine(INDENT + '  constructor Create; override;');
      fImplBuff.AppendLine('constructor ' + lClassName + '.Create;');
      fImplBuff.AppendLine('begin');
      fImplBuff.AppendLine('  inherited Create;');
      for F := Low(lFieldNamesToInit) to High(lFieldNamesToInit) do
        fImplBuff.AppendLine('  ' + lFieldNamesToInit[F] + ' := ' + lTypesName[F] + '.Create;');
      fImplBuff.AppendLine('end;');
      fImplBuff.AppendLine('');

      fIntfBuff.AppendLine(INDENT + '  destructor Destroy; override;');
      fImplBuff.AppendLine('destructor ' + lClassName + '.Destroy;');
      fImplBuff.AppendLine('begin');
      for F := Low(lFieldNamesToInit) to High(lFieldNamesToInit) do
        fImplBuff.AppendLine('  ' + lFieldNamesToInit[F] + '.Free;');
      fImplBuff.AppendLine('  inherited;');
      fImplBuff.AppendLine('end;');
      fImplBuff.AppendLine('');
    end;

    { Public properties }
    I := 0;
    lQryMeta.First;
    while not lQryMeta.Eof do
    begin
      lFieldDataType := TFDDataType(lQryMeta.FieldByName(META_F_COLUMN_DATATYPE).AsInteger);
      lColAttrib := GetColumnAttributes(lQryMeta);
      EmitProperty(
        lUniqueFieldNames[I],
        lColAttrib,
        lFieldDataType,
        lKeyFields.IndexOf(lQryMeta.FieldByName(META_F_COLUMN_NAME).AsString) > -1,
        lQryMeta.FieldByName(META_F_COLUMN_TYPENAME).AsString);
      Inc(I);
      lQryMeta.Next;
    end;

    EmitClassFooter;
  finally
    lQryMeta.Free;
    lKeyFields.Free;
  end;
end;

{ --- Main generation --- }

function TMVCEntityGenerator.Generate(const AConfig: TEntGenConfig): string;
var
  lTables: TStringList;
  lTable: string;
  lGeneratedCount: Integer;
begin
  fConfig := AConfig;
  fIntfBuff.Clear;
  fImplBuff.Clear;
  fInitBuff.Clear;

  lTables := TStringList.Create;
  try
    if not fConnection.Connected then
      fConnection.Connected := True;

    fConnection.GetTableNames(
      fConnection.Params.Database,
      AConfig.Schema, '', lTables);

    Log('Found %d tables in database', [lTables.Count]);

    EmitLicenseHeader;
    { Unit name placeholder - caller should set the real name via GenerateToFile }
    EmitUnitHeader('EntitiesU');

    lGeneratedCount := 0;
    for lTable in lTables do
    begin
      if not ShouldGenerateTable(lTable) then
      begin
        Log('  Skipping table [%s]', [lTable]);
        Continue;
      end;
      GenerateEntity(lTable);
      Inc(lGeneratedCount);
    end;
    EmitUnitFooter;

    Log('Generated %d entities', [lGeneratedCount]);
    Result := fIntfBuff.ToString + fImplBuff.ToString + fInitBuff.ToString;
  finally
    lTables.Free;
  end;
end;

procedure TMVCEntityGenerator.GenerateToFile(const AConfig: TEntGenConfig;
  const AOutputFile: string);
var
  lTables: TStringList;
  lTable: string;
  lGeneratedCount: Integer;
  lUnitName: string;
begin
  fConfig := AConfig;
  fIntfBuff.Clear;
  fImplBuff.Clear;
  fInitBuff.Clear;

  lUnitName := TPath.GetFileNameWithoutExtension(AOutputFile);

  lTables := TStringList.Create;
  try
    if not fConnection.Connected then
      fConnection.Connected := True;

    fConnection.GetTableNames(
      fConnection.Params.Database,
      AConfig.Schema, '', lTables);

    Log('Found %d tables in database', [lTables.Count]);

    EmitLicenseHeader;
    EmitUnitHeader(lUnitName);

    lGeneratedCount := 0;
    for lTable in lTables do
    begin
      if not ShouldGenerateTable(lTable) then
      begin
        Log('  Skipping table [%s]', [lTable]);
        Continue;
      end;
      GenerateEntity(lTable);
      Inc(lGeneratedCount);
    end;
    EmitUnitFooter;

    TFile.WriteAllText(AOutputFile,
      fIntfBuff.ToString + fImplBuff.ToString + fInitBuff.ToString,
      TEncoding.UTF8);

    Log('Generated %d entities to %s', [lGeneratedCount, AOutputFile]);
  finally
    lTables.Free;
  end;
end;

end.
