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
  FireDAC.Stan.Intf, FireDAC.Comp.Client, FireDAC.Phys.Intf,
  LoggerPro;

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
    AutoRequired: Boolean;        // emit [MVCRequired] on NOT NULL non-PK columns
    AutoMaxLength: Boolean;       // emit [MVCMaxLength(N)] on bounded string columns
    // Lists of "table.column" pairs that drive the field-options the
    // generator emits. A column may appear in both lists; the resulting
    // attribute will be [foReadOnly, foRefresh].
    //
    //   ReadOnlyColumns -> add foReadOnly. Use for columns the user must
    //                      not write (computed / GENERATED ALWAYS,
    //                      ROWVERSION, audit-by-trigger, …). Also skips
    //                      [MVCRequired] / [MVCMaxLength] on the field
    //                      (the user can't write it, validation makes no
    //                      sense).
    //
    //   RefreshColumns  -> add foRefresh. Use for columns whose value
    //                      the user wants to read back from the DB after
    //                      INSERT / UPDATE (e.g. a BEFORE trigger
    //                      normalises the input).
    //
    // Match is case-insensitive. There is intentionally no auto-detect:
    // FireDAC's metadata flags (caCalculated / caReadOnly) are
    // inconsistently set across drivers — explicit user input is the
    // only reliable source.
    ReadOnlyColumns: TArray<string>;
    RefreshColumns: TArray<string>;
    // Auto-detect audit columns by name. When AutoAudit is on, columns
    // matching the configured names get the corresponding [MVCAudit*]
    // attribute and are excluded from [MVCRequired] (the framework
    // populates the value in OnBeforeInsert / OnBeforeUpdate).
    //
    // The 4 *Name* fields are configurable so users with non-standard
    // schema conventions can redirect (e.g. AUDIT_CREATED_AT_NAME=row_created).
    // Empty defaults from .env / CLI are filled with the canonical names
    // by the CLI bootstrap.
    AutoAudit: Boolean;
    AuditCreatedAtName: string;
    AuditUpdatedAtName: string;
    AuditCreatedByName: string;
    AuditUpdatedByName: string;
    // Auto-detect soft-delete columns by name. Two modes:
    //   * timestamp mode -> column type must be NullableTDateTime (plain
    //     TDateTime defaults to 1899-12-30 which never matches the
    //     framework's `WHERE deleted_at IS NULL` auto-filter)
    //   * flag mode      -> column type must be Boolean
    // Emitted attribute is [MVCSoftDeleted] in both cases. If a table has
    // BOTH a timestamp-mode and a flag-mode column, the timestamp wins
    // (it's the more expressive form) and a warning is logged.
    AutoSoftDelete: Boolean;
    SoftDeleteTimestampNames: TArray<string>;
    SoftDeleteFlagNames: TArray<string>;
  end;

  TEntGenResult = record
    GeneratedCount: Integer;
    SkippedCount: Integer;
    DiscoveredCount: Integer;
    OutputSource: string;
    OutputFile: string;
  end;

  TMVCEntityGenerator = class
  private
    const INDENT = '  ';
    const LOG_TAG = 'entgen';
  private
    fIntfBuff: TStringBuilder;
    fImplBuff: TStringBuilder;
    fInitBuff: TStringBuilder;
    fConnection: TFDConnection;
    fConfig: TEntGenConfig;
    fLogger: ILogWriter;
    fLastResult: TEntGenResult;
    // Per-table state, set by GenerateEntity before iterating fields:
    // the column name of the chosen soft-delete column, or empty if none.
    fCurrentSoftDeleteFieldName: string;
    procedure LogInfo(const AFmt: string; const AArgs: array of const); overload;
    procedure LogInfo(const AMsg: string); overload;
    procedure LogDebug(const AFmt: string; const AArgs: array of const);
    procedure LogWarn(const AFmt: string; const AArgs: array of const);

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
    procedure EmitField(const ATableName, ADatabaseFieldName, AUniqueFieldName: string;
      AFieldDataType: TFDDataType; const AColumnAttribs: TFDDataAttributes;
      AIsPK: Boolean; const AColumnTypeName: string;
      AColumnLength: Integer);
    function IsColumnIn(const AList: TArray<string>;
      const ATableName, AColumnName: string): Boolean;
    // Pre-scan a table's metadata to pick the soft-delete column (if any).
    // Timestamp wins over flag. Returns empty string if no valid match.
    function ChooseSoftDeleteField(const ATableName: string;
      AMetaQry: TFDMetaInfoQuery): string;
    procedure EmitProperty(const AFieldName: string;
      const AColumnAttribs: TFDDataAttributes;
      AFieldDataType: TFDDataType; AIsPK: Boolean;
      const AColumnTypeName: string);

    { Table filtering }
    class function MatchesPattern(const ATableName, APattern: string): Boolean;
    class function MatchesAnyPattern(const ATableName: string;
      const APatterns: TArray<string>): Boolean;
    function ShouldGenerateTable(const ATableName: string): Boolean;
    function GetMetaCatalogName: string;
    procedure GenerateEntity(const ATableName: string);
    procedure RunGeneration(const AConfig: TEntGenConfig; const AUnitName: string);
  public
    constructor Create(AConnection: TFDConnection);
    destructor Destroy; override;
    function Generate(const AConfig: TEntGenConfig): string;
    procedure GenerateToFile(const AConfig: TEntGenConfig; const AOutputFile: string);
    property Logger: ILogWriter read fLogger write fLogger;
    property LastResult: TEntGenResult read fLastResult;
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
  META_F_COLUMN_LENGTH = 'COLUMN_LENGTH';

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

procedure TMVCEntityGenerator.LogInfo(const AMsg: string);
begin
  if Assigned(fLogger) then
    fLogger.Info(AMsg, LOG_TAG);
end;

procedure TMVCEntityGenerator.LogInfo(const AFmt: string; const AArgs: array of const);
begin
  LogInfo(Format(AFmt, AArgs));
end;

procedure TMVCEntityGenerator.LogDebug(const AFmt: string; const AArgs: array of const);
begin
  if Assigned(fLogger) then
    fLogger.Debug(Format(AFmt, AArgs), LOG_TAG);
end;

procedure TMVCEntityGenerator.LogWarn(const AFmt: string; const AArgs: array of const);
begin
  if Assigned(fLogger) then
    fLogger.Warn(Format(AFmt, AArgs), LOG_TAG);
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
  // Validators unit is needed when AutoRequired / AutoMaxLength emit
  // [MVCRequired] / [MVCMaxLength(...)] attributes on generated fields.
  if fConfig.AutoRequired or fConfig.AutoMaxLength then
    fIntfBuff.AppendLine('  MVCFramework.Validators,');
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

function TMVCEntityGenerator.IsColumnIn(const AList: TArray<string>;
  const ATableName, AColumnName: string): Boolean;
var
  lTarget: string;
  lEntry: string;
begin
  // Case-insensitive comparison on "table.column" pairs.
  Result := False;
  if Length(AList) = 0 then
    Exit;
  lTarget := ATableName + '.' + AColumnName;
  for lEntry in AList do
    if SameText(lEntry.Trim, lTarget) then
      Exit(True);
end;

function TMVCEntityGenerator.ChooseSoftDeleteField(const ATableName: string;
  AMetaQry: TFDMetaInfoQuery): string;

  function NameMatches(const AColName: string;
    const ANameList: TArray<string>): Boolean;
  var
    lEntry: string;
  begin
    Result := False;
    for lEntry in ANameList do
      if SameText(lEntry.Trim, AColName) then
        Exit(True);
  end;

var
  lTimestampMatch, lFlagMatch: string;
  lColName, lDelphiType: string;
  lFieldDataType: TFDDataType;
  lColAttrib: TFDDataAttributes;
begin
  Result := '';
  if not fConfig.AutoSoftDelete then
    Exit;
  if (Length(fConfig.SoftDeleteTimestampNames) = 0) and
     (Length(fConfig.SoftDeleteFlagNames) = 0) then
    Exit;

  // First pass: find the FIRST timestamp-mode and the FIRST flag-mode
  // matches that also pass type validation. Reset cursor so we don't
  // disturb the caller's iteration.
  lTimestampMatch := '';
  lFlagMatch := '';
  AMetaQry.First;
  while not AMetaQry.Eof do
  begin
    lColName := AMetaQry.FieldByName(META_F_COLUMN_NAME).AsString;
    lFieldDataType := TFDDataType(AMetaQry.FieldByName(META_F_COLUMN_DATATYPE).AsInteger);
    lColAttrib := GetColumnAttributes(AMetaQry);
    lDelphiType := GetDelphiType(lFieldDataType, lColAttrib);

    if (lTimestampMatch = '') and NameMatches(lColName, fConfig.SoftDeleteTimestampNames) then
    begin
      // Timestamp mode: REQUIRES NullableTDateTime. Plain TDateTime
      // defaults to 1899-12-30 which never matches the auto-filter
      // `WHERE deleted_at IS NULL`, breaking the feature silently.
      if lDelphiType.StartsWith('NullableTDateTime') then
        lTimestampMatch := lColName
      else
        LogWarn(
          'Column [%s.%s] matches a SOFT_DELETE_TIMESTAMP_NAME but its ' +
          'type is [%s] (must be NullableTDateTime - the column needs ' +
          'to be NULL-able). Skipping [MVCSoftDeleted].',
          [ATableName, lColName, lDelphiType]);
    end;
    if (lFlagMatch = '') and NameMatches(lColName, fConfig.SoftDeleteFlagNames) then
    begin
      if lDelphiType = 'Boolean' then
        lFlagMatch := lColName
      else
        LogWarn(
          'Column [%s.%s] matches a SOFT_DELETE_FLAG_NAME but its type ' +
          'is [%s] (must be Boolean). Skipping [MVCSoftDeleted].',
          [ATableName, lColName, lDelphiType]);
    end;
    AMetaQry.Next;
  end;
  AMetaQry.First;

  // Conflict resolution: timestamp wins.
  if (lTimestampMatch <> '') and (lFlagMatch <> '') then
  begin
    LogWarn(
      'Table [%s] has both a timestamp-mode column [%s] and a flag-mode ' +
      'column [%s] candidate for [MVCSoftDeleted]. Timestamp wins ' +
      '(it''s the more expressive form: also captures *when* the row ' +
      'was deleted). The flag column is left as a regular Boolean.',
      [ATableName, lTimestampMatch, lFlagMatch]);
    Result := lTimestampMatch;
  end
  else if lTimestampMatch <> '' then
    Result := lTimestampMatch
  else
    Result := lFlagMatch;
end;

procedure TMVCEntityGenerator.EmitField(
  const ATableName, ADatabaseFieldName, AUniqueFieldName: string;
  AFieldDataType: TFDDataType;
  const AColumnAttribs: TFDDataAttributes;
  AIsPK: Boolean;
  const AColumnTypeName: string;
  AColumnLength: Integer);
var
  lRTTIAttrib, lField: string;
  lColType: string;
  lIsAutoInc: Boolean;
  lIsReadOnly: Boolean;
  lIsRefresh: Boolean;
  lIsString: Boolean;
  lIsBoundedString: Boolean;
  lIsUnsupported: Boolean;
  lValidationLines: TArray<string>;
  lLine: string;
  lOptions: TArray<string>;
  lOptionsStr: string;
  lTypeHint: string;
  lAuditAttrib: string;
  lDelphiType: string;
  lExpectedDateTime: Boolean;
  lExpectedString: Boolean;
  lIsSoftDelete: Boolean;
begin
  lIsAutoInc := IsAutoIncField(AColumnAttribs, AColumnTypeName);
  // foReadOnly / foRefresh are user-declared via the READONLY_COLUMNS /
  // REFRESH_COLUMNS lists. FireDAC's metadata flags (caCalculated /
  // caReadOnly) are unreliable across drivers — PG, in particular,
  // leaves them unset on GENERATED ALWAYS columns — so we never auto-detect.
  lIsReadOnly := IsColumnIn(fConfig.ReadOnlyColumns, ATableName, ADatabaseFieldName);
  lIsRefresh  := IsColumnIn(fConfig.RefreshColumns,  ATableName, ADatabaseFieldName);

  // Build the field-options set lexically.
  lOptions := [];
  if AIsPK then
    lOptions := lOptions + ['foPrimaryKey'];
  if AIsPK and lIsAutoInc then
    lOptions := lOptions + ['foAutoGenerated'];
  if lIsReadOnly then
    lOptions := lOptions + ['foReadOnly'];
  if lIsRefresh then
    lOptions := lOptions + ['foRefresh'];

  // JSON / XML columns get the third "type hint" argument of MVCTableField.
  lColType := AColumnTypeName.ToLower;
  if (not AIsPK) and (lColType.Contains('json') or lColType.Contains('xml')) then
    lTypeHint := lColType
  else
    lTypeHint := '';

  // Compose the [MVCTableField(...)] string, picking the shortest valid
  // overload (omit empty options; omit type hint when not needed).
  if Length(lOptions) > 0 then
  begin
    lOptionsStr := '[' + String.Join(', ', lOptions) + ']';
    if lTypeHint <> '' then
      lRTTIAttrib := Format('[MVCTableField(''%s'', %s, ''%s'')]',
        [ADatabaseFieldName, lOptionsStr, lTypeHint])
    else
      lRTTIAttrib := Format('[MVCTableField(''%s'', %s)]',
        [ADatabaseFieldName, lOptionsStr]);
  end
  else if lTypeHint <> '' then
    lRTTIAttrib := Format('[MVCTableField(''%s'', [], ''%s'')]',
      [ADatabaseFieldName, lTypeHint])
  else
    lRTTIAttrib := Format('[MVCTableField(''%s'')]', [ADatabaseFieldName]);

  if AIsPK and lIsAutoInc then
    lField := GetFieldVarName(AUniqueFieldName) + ': ' + GetDelphiType(AFieldDataType, AColumnAttribs, True) + ';'
  else
    lField := GetFieldVarName(AUniqueFieldName) + ': ' + GetDelphiType(AFieldDataType, AColumnAttribs) + ';';

  // Audit-column auto-detection (Tier 2 — convention by NAME, configurable).
  // FireDAC's metadata flags can't tell us whether a column is meant for
  // audit use, but the column name is user-typed and reliable. Match
  // case-insensitively against the four configurable names; the framework
  // populates the field in OnBeforeInsert / OnBeforeUpdate, so the user
  // never writes the value (we'll skip [MVCRequired] on it below).
  //
  // Type validation: [MVCAuditCreatedAt] / [MVCAuditUpdatedAt] need
  // TDateTime / NullableTDateTime. [MVCAuditCreatedBy] / [MVCAuditUpdatedBy]
  // need string / NullableString. On mismatch we skip the audit attribute
  // and warn — the user probably named a non-audit column with an
  // audit-shaped name, we'd rather emit nothing than wrong attributes.
  lAuditAttrib := '';
  if fConfig.AutoAudit then
  begin
    lDelphiType := GetDelphiType(AFieldDataType, AColumnAttribs);
    // GetDelphiType may decorate the base name with a {dtXxx} comment
    // (e.g. PG TIMESTAMP -> 'TDateTime {dtDateTimeStamp}'). Match by
    // prefix so the comment doesn't break the comparison.
    // Note: order matters - check NullableXxx first since 'TDateTime'
    // is a prefix of 'NullableTDateTime' only after stripping 'Nullable'.
    lExpectedDateTime :=
      lDelphiType.StartsWith('NullableTDateTime') or
      lDelphiType.StartsWith('NullableTDate') or
      lDelphiType.StartsWith('TDateTime');
    lExpectedString :=
      (lDelphiType = 'NullableString') or (lDelphiType = 'String');

    if SameText(ADatabaseFieldName, fConfig.AuditCreatedAtName) then
    begin
      if lExpectedDateTime then
        lAuditAttrib := '[MVCAuditCreatedAt]'
      else
        LogWarn('Column [%s] matches AuditCreatedAtName but its type [%s] ' +
          'is not (Nullable)TDateTime - skipping [MVCAuditCreatedAt]',
          [ADatabaseFieldName, lDelphiType]);
    end
    else if SameText(ADatabaseFieldName, fConfig.AuditUpdatedAtName) then
    begin
      if lExpectedDateTime then
        lAuditAttrib := '[MVCAuditUpdatedAt]'
      else
        LogWarn('Column [%s] matches AuditUpdatedAtName but its type [%s] ' +
          'is not (Nullable)TDateTime - skipping [MVCAuditUpdatedAt]',
          [ADatabaseFieldName, lDelphiType]);
    end
    else if SameText(ADatabaseFieldName, fConfig.AuditCreatedByName) then
    begin
      if lExpectedString then
        lAuditAttrib := '[MVCAuditCreatedBy]'
      else
        LogWarn('Column [%s] matches AuditCreatedByName but its type [%s] ' +
          'is not (Nullable)String - skipping [MVCAuditCreatedBy]',
          [ADatabaseFieldName, lDelphiType]);
    end
    else if SameText(ADatabaseFieldName, fConfig.AuditUpdatedByName) then
    begin
      if lExpectedString then
        lAuditAttrib := '[MVCAuditUpdatedBy]'
      else
        LogWarn('Column [%s] matches AuditUpdatedByName but its type [%s] ' +
          'is not (Nullable)String - skipping [MVCAuditUpdatedBy]',
          [ADatabaseFieldName, lDelphiType]);
    end;
  end;

  // Soft-delete column for THIS table was already chosen at GenerateEntity
  // time (timestamp wins over flag if both are present). All we need to do
  // here is emit [MVCSoftDeleted] when the column matches.
  lIsSoftDelete := (fCurrentSoftDeleteFieldName <> '') and
    SameText(ADatabaseFieldName, fCurrentSoftDeleteFieldName);

  // Auto-validation attributes (Tier 1 — derived from FireDAC schema metadata).
  lValidationLines := [];

  // Stash the audit / soft-delete attribute first so they appear closest
  // to the [MVCTableField] line in the emitted source.
  if lAuditAttrib <> '' then
    lValidationLines := lValidationLines + [lAuditAttrib];
  if lIsSoftDelete then
    lValidationLines := lValidationLines + ['[MVCSoftDeleted]'];

  // [MVCRequired] — emit when the column is NOT NULL and the framework is not
  // already going to assign the value: skip for autogen PK, read-only columns,
  // and audit columns (the framework populates them in OnBeforeInsert /
  // OnBeforeUpdate). foRefresh alone does NOT skip [MVCRequired] — the user
  // still writes the value, the DB only re-reads it.
  if fConfig.AutoRequired
     and (not (caAllowNull in AColumnAttribs))
     and (not (AIsPK and lIsAutoInc))
     and (not lIsReadOnly)
     and (lAuditAttrib = '')
     and (not lIsSoftDelete) then
  begin
    lValidationLines := lValidationLines + ['[MVCRequired]'];
  end;

  // [MVCMaxLength(N)] — bounded string types only. Skip dtMemo / dtWideMemo
  // (TEXT / CLOB are unbounded), skip non-string types entirely, and skip
  // read-only columns (the user can't write them, the validator wouldn't fire).
  // AColumnLength is reported by FireDAC in characters for char types; 0 means
  // "unknown" or "unbounded" — be conservative and skip.
  lIsString := (AFieldDataType = dtWideString) or (AFieldDataType = dtAnsiString);
  lIsBoundedString := lIsString and (AColumnLength > 0);
  if fConfig.AutoMaxLength
     and lIsBoundedString
     and (not lIsReadOnly)
     and (lAuditAttrib = '')
     and (not lIsSoftDelete) then
  begin
    lValidationLines := lValidationLines + [Format('[MVCMaxLength(%d)]', [AColumnLength])];
  end;

  lIsUnsupported := GetDelphiType(AFieldDataType, AColumnAttribs).ToUpper.Contains('UNSUPPORTED TYPE');
  if lIsUnsupported then
  begin
    LogWarn('Unsupported column type for field [%s] (%s) - emitting as comment',
      [ADatabaseFieldName, AColumnTypeName]);
    lRTTIAttrib := '//' + lRTTIAttrib;
    lField := '//' + lField;
  end
  else
  begin
    lRTTIAttrib := '  ' + lRTTIAttrib;
    lField := '  ' + lField;
  end;
  fIntfBuff.AppendLine(INDENT + lRTTIAttrib);
  // Validation attributes go BETWEEN the [MVCTableField] and the field
  // declaration so RTTI keeps them grouped on the same field.
  for lLine in lValidationLines do
  begin
    if lIsUnsupported then
      fIntfBuff.AppendLine(INDENT + '  //' + lLine)
    else
      fIntfBuff.AppendLine(INDENT + '  ' + lLine);
  end;
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

{ --- Metadata helpers --- }

function TMVCEntityGenerator.GetMetaCatalogName: string;
var
  lDriver: string;
begin
  { For drivers whose DATABASE param is a file path (SQLite, Firebird/IB
    embedded, MSAccess) the path contains characters like ":" and "\" that
    FireDAC would try to use inside its metadata queries - with SQLite the ":"
    is parsed as a bind-parameter prefix and the query fails. Pass an empty
    catalog in that case; FireDAC falls back to the connection's default
    catalog, which is what we want. }
  lDriver := fConnection.Params.Values['DriverID'].ToUpper;
  if (lDriver = 'SQLITE') or (lDriver = 'FB') or (lDriver = 'IB') or
     (lDriver = 'MSACC') or (lDriver = 'ADS') then
    Result := ''
  else
    Result := fConnection.Params.Database;
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

  LogInfo('Generating [%s] for table [%s]', [lClassName, ATableName]);

  EmitClassHeader(ATableName, lClassName, fConfig.NameCase, fConfig.ClassAsAbstract);

  lKeyFields := TStringList.Create;
  lQryMeta := TFDMetaInfoQuery.Create(nil);
  try
    lQryMeta.Connection := fConnection;
    fConnection.GetKeyFieldNames(
      GetMetaCatalogName,
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
    lQryMeta.CatalogName := GetMetaCatalogName;
    lQryMeta.Open;
    lQryMeta.FetchAll;
    lQryMeta.First;

    // Pick the soft-delete column for THIS table (if any). The decision
    // has to happen before we start emitting fields because conflict
    // resolution (timestamp wins over flag) is a table-wide property.
    fCurrentSoftDeleteFieldName := ChooseSoftDeleteField(ATableName, lQryMeta);

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
        ATableName,
        lFieldName,
        lUniqueFieldNames[I],
        lFieldDataType,
        lColAttrib,
        lKeyFields.IndexOf(lFieldName) > -1,
        lColumnTypeName,
        lQryMeta.FieldByName(META_F_COLUMN_LENGTH).AsInteger);

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

procedure TMVCEntityGenerator.RunGeneration(const AConfig: TEntGenConfig; const AUnitName: string);
var
  lTables: TStringList;
  lTable: string;
  lGeneratedCount, lSkippedCount: Integer;
begin
  fConfig := AConfig;
  fIntfBuff.Clear;
  fImplBuff.Clear;
  fInitBuff.Clear;
  fLastResult := Default(TEntGenResult);

  lTables := TStringList.Create;
  try
    if not fConnection.Connected then
      fConnection.Connected := True;

    fConnection.GetTableNames(
      GetMetaCatalogName,
      AConfig.Schema, '', lTables);

    LogInfo('Found %d tables in database', [lTables.Count]);

    EmitLicenseHeader;
    EmitUnitHeader(AUnitName);

    lGeneratedCount := 0;
    lSkippedCount := 0;
    for lTable in lTables do
    begin
      if not ShouldGenerateTable(lTable) then
      begin
        LogDebug('Skipping table [%s]', [lTable]);
        Inc(lSkippedCount);
        Continue;
      end;
      GenerateEntity(lTable);
      Inc(lGeneratedCount);
    end;
    EmitUnitFooter;

    fLastResult.GeneratedCount := lGeneratedCount;
    fLastResult.SkippedCount := lSkippedCount;
    fLastResult.DiscoveredCount := lTables.Count;
  finally
    lTables.Free;
  end;
end;

function TMVCEntityGenerator.Generate(const AConfig: TEntGenConfig): string;
begin
  { Unit name placeholder - caller should set the real name via GenerateToFile }
  RunGeneration(AConfig, 'EntitiesU');
  Result := fIntfBuff.ToString + fImplBuff.ToString + fInitBuff.ToString;
  fLastResult.OutputSource := Result;
  LogInfo('Generated %d entities', [fLastResult.GeneratedCount]);
end;

procedure TMVCEntityGenerator.GenerateToFile(const AConfig: TEntGenConfig;
  const AOutputFile: string);
var
  lSource: string;
begin
  RunGeneration(AConfig, TPath.GetFileNameWithoutExtension(AOutputFile));
  lSource := fIntfBuff.ToString + fImplBuff.ToString + fInitBuff.ToString;
  TFile.WriteAllText(AOutputFile, lSource, TEncoding.UTF8);
  fLastResult.OutputSource := lSource;
  fLastResult.OutputFile := AOutputFile;
  LogInfo('Generated %d entities to %s', [fLastResult.GeneratedCount, AOutputFile]);
end;

end.
