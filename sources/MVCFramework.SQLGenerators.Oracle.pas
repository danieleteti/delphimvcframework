// *************************************************************************** }
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

unit MVCFramework.SQLGenerators.Oracle;

// Oracle 12c+ SQL Generator
// Uses RETURNING clause for auto-generated primary keys (like Firebird/PostgreSQL)
// Supports sequences via DUAL
// No native UUID type (HasNativeUUID = False, inherited default)

interface

uses
  System.Generics.Collections,
  MVCFramework.RQL.Parser,
  MVCFramework.ActiveRecord,
  MVCFramework.Commons;

type
  TMVCSQLGeneratorOracle = class(TMVCSQLGenerator)
  protected
    function GetCompilerClass: TRQLCompilerClass; override;
    /// <summary>Builds Oracle-specific RETURNING...INTO clause.
    /// Returns empty string when neither auto-generated PK nor foRefresh
    /// fields are present. OutParamNames receives the ":param_out" names
    /// corresponding to each returned column.</summary>
    function BuildOracleReturningClause(const TableMap: TMVCTableMap;
      out OutParamNames: TArray<string>): string;
  public
    function CreateInsertSQL(
      const TableMap: TMVCTableMap;
      const ARInstance: TMVCActiveRecord): string; override;
    function CreateUpdateSQL(
      const TableMap: TMVCTableMap;
      const ARInstance: TMVCActiveRecord): string; overload; override;
    function CreateUpdateSQL(const TableMap: TMVCTableMap;
      const ARInstance: TMVCActiveRecord;
      const AChangedFields: TArray<string>): string; overload; override;
    function GetSequenceValueSQL(const PKFieldName: string;
      const SequenceName: string;
      const Step: Integer = 1): string; override;
    function UsesReturningIntoParams: Boolean; override;
  end;

implementation

uses
  System.Rtti,
  System.SysUtils,
  System.Classes,
  MVCFramework.RQL.AST2Oracle;

function TMVCSQLGeneratorOracle.BuildOracleReturningClause(
  const TableMap: TMVCTableMap; out OutParamNames: TArray<string>): string;
var
  lCols: TStringList;
  lParams: TStringList;
  lFieldInfo: TFieldInfo;
  lColsPart: string;
  lParamsPart: string;
  i: Integer;
begin
  lCols := TStringList.Create;
  lParams := TStringList.Create;
  try
    if TableMap.fAutoGenPKIndex >= 0 then
    begin
      lCols.Add(AutoGenPKFieldName(TableMap));
      lParams.Add(':' + GetParamNameForSQL(TableMap.fPrimaryKeys[TableMap.fAutoGenPKIndex].FieldName) + '_out');
    end;
    for lFieldInfo in TableMap.RefreshFields do
    begin
      lCols.Add(GetFieldNameForSQL(lFieldInfo.FieldName));
      lParams.Add(':' + GetParamNameForSQL(lFieldInfo.FieldName) + '_out');
    end;
    if lCols.Count = 0 then
    begin
      Result := '';
      SetLength(OutParamNames, 0);
      Exit;
    end;
    lColsPart := '';
    for i := 0 to lCols.Count - 1 do
    begin
      if i > 0 then
        lColsPart := lColsPart + ', ';
      lColsPart := lColsPart + lCols[i];
    end;
    lParamsPart := '';
    for i := 0 to lParams.Count - 1 do
    begin
      if i > 0 then
        lParamsPart := lParamsPart + ', ';
      lParamsPart := lParamsPart + lParams[i];
    end;
    Result := ' RETURNING ' + lColsPart + ' INTO ' + lParamsPart;
    SetLength(OutParamNames, lParams.Count);
    for i := 0 to lParams.Count - 1 do
      OutParamNames[i] := lParams[i];
  finally
    lCols.Free;
    lParams.Free;
  end;
end;

function TMVCSQLGeneratorOracle.CreateInsertSQL(
  const TableMap: TMVCTableMap;
  const ARInstance: TMVCActiveRecord): string;
var
  lKeyValue: TPair<TRttiField, TFieldInfo>;
  lSB: TStringBuilder;
  lFieldName: String;
  lOutParamNames: TArray<string>;
  lReturningClause: string;
begin
  lSB := TStringBuilder.Create;
  try
    lSB.Append('INSERT INTO ' + GetTableNameForSQL(TableMap.fTableName) + ' (');
    // All PK columns participating in the INSERT. Single-PK: identical output.
    lSB.Append(PKInsertColumns(TableMap));

    for lFieldName in fPartitionInfo.FieldNames do
      lSB.Append(GetFieldNameForSQL(lFieldName) + ',');

    for lKeyValue in TableMap.fMap do
    begin
      if lKeyValue.Value.Insertable then
        lSB.Append(GetFieldNameForSQL(lKeyValue.Value.FieldName) + ',');
    end;

    lSB.Remove(lSB.Length - 1, 1);
    lSB.Append(') values (');

    lSB.Append(PKInsertParams(TableMap));

    for lFieldName in fPartitionInfo.FieldNames do
      lSB.Append(':' + GetParamNameForSQL(lFieldName) + ',');

    for lKeyValue in TableMap.fMap do
    begin
      if lKeyValue.Value.IsVersion then
        lSB.Append(OBJECT_VERSION_STARTING_VALUE + ',')
      else if lKeyValue.Value.Insertable then
        lSB.Append(':' + GetParamNameForSQL(lKeyValue.Value.FieldName) + ',');
    end;

    lSB.Remove(lSB.Length - 1, 1);
    lSB.Append(')');

    // Oracle 12c+ supports RETURNING...INTO clause for auto-generated keys and foRefresh fields
    lReturningClause := BuildOracleReturningClause(TableMap, lOutParamNames);
    if lReturningClause <> '' then
      lSB.Append(lReturningClause);

    Result := lSB.ToString;
  finally
    lSB.Free;
  end;
end;

function TMVCSQLGeneratorOracle.CreateUpdateSQL(const TableMap: TMVCTableMap;
  const ARInstance: TMVCActiveRecord): string;
var
  lFieldInfo: TFieldInfo;
  lColsPart: string;
  lParamsPart: string;
  lFirst: Boolean;
begin
  Result := inherited CreateUpdateSQL(TableMap, ARInstance);
  lColsPart := '';
  lParamsPart := '';
  lFirst := True;
  for lFieldInfo in TableMap.RefreshFields do
  begin
    if lFirst then
    begin
      lColsPart := GetFieldNameForSQL(lFieldInfo.FieldName);
      lParamsPart := ':' + GetParamNameForSQL(lFieldInfo.FieldName) + '_out';
      lFirst := False;
    end
    else
    begin
      lColsPart := lColsPart + ', ' + GetFieldNameForSQL(lFieldInfo.FieldName);
      lParamsPart := lParamsPart + ', :' + GetParamNameForSQL(lFieldInfo.FieldName) + '_out';
    end;
  end;
  if lColsPart <> '' then
    Result := Result + ' RETURNING ' + lColsPart + ' INTO ' + lParamsPart;
end;

function TMVCSQLGeneratorOracle.CreateUpdateSQL(const TableMap: TMVCTableMap;
  const ARInstance: TMVCActiveRecord;
  const AChangedFields: TArray<string>): string;
var
  lFieldInfo: TFieldInfo;
  lColsPart: string;
  lParamsPart: string;
  lFirst: Boolean;
begin
  Result := inherited CreateUpdateSQL(TableMap, ARInstance, AChangedFields);
  lColsPart := '';
  lParamsPart := '';
  lFirst := True;
  for lFieldInfo in TableMap.RefreshFields do
  begin
    if lFirst then
    begin
      lColsPart := GetFieldNameForSQL(lFieldInfo.FieldName);
      lParamsPart := ':' + GetParamNameForSQL(lFieldInfo.FieldName) + '_out';
      lFirst := False;
    end
    else
    begin
      lColsPart := lColsPart + ', ' + GetFieldNameForSQL(lFieldInfo.FieldName);
      lParamsPart := lParamsPart + ', :' + GetParamNameForSQL(lFieldInfo.FieldName) + '_out';
    end;
  end;
  if lColsPart <> '' then
    Result := Result + ' RETURNING ' + lColsPart + ' INTO ' + lParamsPart;
end;

function TMVCSQLGeneratorOracle.GetCompilerClass: TRQLCompilerClass;
begin
  Result := TRQLOracleCompiler;
end;

function TMVCSQLGeneratorOracle.GetSequenceValueSQL(
  const PKFieldName: string; const SequenceName: string;
  const Step: Integer): string;
begin
  // Oracle uses sequences via DUAL pseudo-table
  Result := Format('SELECT %s.NEXTVAL AS %s FROM DUAL',
    [GetFieldNameForSQL(SequenceName), GetFieldNameForSQL(PKFieldName)]);
end;

function TMVCSQLGeneratorOracle.UsesReturningIntoParams: Boolean;
begin
  // BuildOracleReturningClause emits "RETURNING col INTO :col_out"
  Result := True;
end;

initialization

TMVCSQLGeneratorRegistry.Instance.RegisterSQLGenerator('oracle', TMVCSQLGeneratorOracle);

finalization

TMVCSQLGeneratorRegistry.Instance.UnRegisterSQLGenerator('oracle');

end.
