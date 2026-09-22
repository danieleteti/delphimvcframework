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

unit MVCFramework.SQLGenerators.MSSQL;

interface

uses
  {
    //do not include these 2 unit in this unit
    //because it will not compiles in Delphi Pro
  FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.MSSQL,
  }
  System.Generics.Collections,
  MVCFramework.RQL.Parser,
  MVCFramework.ActiveRecord,
  MVCFramework.Commons;

type
  TMVCSQLGeneratorMSSQL = class(TMVCSQLGenerator)
  protected
    function GetCompilerClass: TRQLCompilerClass; override;
    /// <summary>SQL Server has no BEFORE triggers, and OUTPUT reports the row
    /// as it was before the AFTER triggers ran. It also rejects "OUTPUT" without
    /// INTO on a table with enabled triggers. So whatever foRefresh promises
    /// (DB defaults, computed columns, values written by a trigger) is read
    /// back by selecting the row again by key, in the same batch.
    /// Diagnosis of the trigger failure: Flavio Basile.</summary>
    function RefreshColumnList(const TableMap: TMVCTableMap;
      const AIncludeAutoGenPK: Boolean; const APrefix: string = ''): string;
    function AppendUpdateRefresh(const TableMap: TMVCTableMap; const ASQL: string): string;
    function BuildSoftDeleteWhereSuffix(const TableMap: TMVCTableMap): string; override;
    function BuildSoftDeleteSetDeleted(const TableMap: TMVCTableMap): string; override;
    function BuildSoftDeleteSetRestored(const TableMap: TMVCTableMap): string; override;
  public
    /// <summary>The driver reports the LAST row count of the batch, which is
    /// a trigger's when the trigger does not SET NOCOUNT ON: a stale-version
    /// UPDATE that changed nothing then looks successful. @@ROWCOUNT right
    /// after the statement is the statement's own, triggers or not.</summary>
    function GetRowsAffectedSQL: string; override;
    function CreateInsertSQL(
      const TableMap: TMVCTableMap;
      const ARInstance: TMVCActiveRecord): string; override;
    function CreateUpdateSQL(
      const TableMap: TMVCTableMap;
      const ARInstance: TMVCActiveRecord): string; overload; override;
    function CreateUpdateSQL(const TableMap: TMVCTableMap;
      const ARInstance: TMVCActiveRecord;
      const AChangedFields: TArray<string>): string; overload; override;
  end;

implementation

uses
  System.Rtti,
  System.SysUtils,
  Data.DB,
  MVCFramework.RQL.AST2MSSQL;

function TMVCSQLGeneratorMSSQL.RefreshColumnList(const TableMap: TMVCTableMap;
  const AIncludeAutoGenPK: Boolean; const APrefix: string): string;
var
  lFieldInfo: TFieldInfo;
begin
  Result := '';
  if AIncludeAutoGenPK and (TableMap.fAutoGenPKIndex >= 0) then
    Result := APrefix + AutoGenPKFieldName(TableMap);
  for lFieldInfo in TableMap.RefreshFields do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + APrefix + GetFieldNameForSQL(lFieldInfo.FieldName);
  end;
end;

function TMVCSQLGeneratorMSSQL.AppendUpdateRefresh(const TableMap: TMVCTableMap;
  const ASQL: string): string;
begin
  Result := ASQL;
  if TableMap.RefreshFields.Count = 0 then
    Exit;
  // The UPDATE's own row count is captured before anything else can change
  // it: a missed row (not found, stale version) must bring back no row.
  Result := Result + ';' + sLineBreak +
    'DECLARE @dmvc_rows INT = @@ROWCOUNT;' + sLineBreak +
    'SELECT ' + RefreshColumnList(TableMap, False) +
    ' FROM ' + GetTableNameForSQL(TableMap.fTableName) +
    ' WHERE ' + BuildPKWhereConjunction(TableMap, ' = :') + ' AND @dmvc_rows > 0;';
end;

function TMVCSQLGeneratorMSSQL.CreateInsertSQL(
  const TableMap: TMVCTableMap;
  const ARInstance: TMVCActiveRecord): string;
var
  lKeyValue: TPair<TRttiField, TFieldInfo>;
  lSB: TStringBuilder;
  lFieldName: String;
  lPK: TMVCPKInfo;
  lPKName, lTableName, lKeyType: string;
  lValuesPos, I: Integer;
  lAllPKsInserted: Boolean;
begin
  lTableName := GetTableNameForSQL(TableMap.fTableName);
  lSB := TStringBuilder.Create;
  try
    lSB.Append('INSERT INTO ' + lTableName + '(');
    lSB.Append(PKInsertColumns(TableMap));

    {partition}
    for lFieldName in fPartitionInfo.FieldNames do
    begin
      lSB.Append(GetFieldNameForSQL(lFieldName) + ',');
    end;
    {end-partition}

    for lKeyValue in TableMap.fMap do
    begin
      if lKeyValue.Value.Insertable then
      begin
        lSB.Append(GetFieldNameForSQL(lKeyValue.Value.FieldName) + ',');
      end;
    end;
    lSB.Remove(lSB.Length - 1, 1);
    lSB.Append(') values (');
    lSB.Append(PKInsertParams(TableMap));

    {partition}
    for lFieldName in fPartitionInfo.FieldNames do
    begin
      lSB.Append(':' + GetParamNameForSQL(lFieldName) + ',');
    end;
    {end-partition}

    for lKeyValue in TableMap.fMap do
    begin
      if lKeyValue.Value.IsVersion then
      begin
        lSB.Append(OBJECT_VERSION_STARTING_VALUE + ',');
      end else if lKeyValue.Value.Insertable then
      begin
        lSB.Append(':' + GetParamNameForSQL(lKeyValue.Value.FieldName) + ',');
      end;
    end;
    lSB.Remove(lSB.Length - 1, 1);
    lSB.Append(')');
    Result := lSB.ToString;
  finally
    lSB.Free;
  end;

  lValuesPos := Pos(' VALUES (', UpperCase(Result));
  if TableMap.fAutoGenPKIndex >= 0 then
  begin
    lPK := TableMap.fPrimaryKeys[TableMap.fAutoGenPKIndex];
    lPKName := GetFieldNameForSQL(lPK.FieldName);
    if lPK.FieldType in [ftInteger, ftLargeInt] then
    begin
      // IDENTITY. SCOPE_IDENTITY is confined to this scope, so a trigger's own
      // inserts cannot shadow it. NULL means the column is not an IDENTITY:
      // fail instead of leaving the in-memory key at 0.
      Result := Result + ';' + sLineBreak +
        'IF SCOPE_IDENTITY() IS NULL THROW 50000, ''DMVCFramework - ' +
        'SCOPE_IDENTITY() is NULL after INSERT into ' + TableMap.fTableName.Replace('''', '''''') +
        ', an auto-generated integer key must be an IDENTITY column'', 1;' + sLineBreak +
        'SELECT ' + RefreshColumnList(TableMap, True) + ' FROM ' + lTableName +
        ' WHERE ' + lPKName + ' = SCOPE_IDENTITY();';
    end
    else
    begin
      // A GUID or string key filled by a DEFAULT: only OUTPUT can hand it
      // back, and INTO a table variable keeps it legal on a table with triggers.
      if lPK.FieldType = ftGuid then
        lKeyType := 'UNIQUEIDENTIFIER'
      else
        lKeyType := 'NVARCHAR(4000)';
      Insert(' OUTPUT inserted.' + lPKName + ' INTO @dmvc_key', Result, lValuesPos);
      Result := 'DECLARE @dmvc_key TABLE (k ' + lKeyType + ');' + sLineBreak +
        Result + ';' + sLineBreak +
        'SELECT ' + RefreshColumnList(TableMap, True) + ' FROM ' + lTableName +
        ' WHERE ' + lPKName + ' = (SELECT k FROM @dmvc_key);';
    end;
  end
  else if TableMap.RefreshFields.Count > 0 then
  begin
    lAllPKsInserted := TableMap.HasPK;
    for I := 0 to High(TableMap.fPrimaryKeys) do
      lAllPKsInserted := lAllPKsInserted and TableMap.fPrimaryKeys[I].InInsert;
    if lAllPKsInserted then
      // The key values are the INSERT's own parameters.
      Result := Result + ';' + sLineBreak +
        'SELECT ' + RefreshColumnList(TableMap, False) + ' FROM ' + lTableName +
        ' WHERE ' + BuildPKWhereConjunction(TableMap, ' = :') + ';'
    else
      // No key to find the row again: OUTPUT is the only way left, and SQL
      // Server accepts it only on a table without triggers.
      Insert(' OUTPUT ' + RefreshColumnList(TableMap, False, 'inserted.'), Result, lValuesPos);
  end;
end;

function TMVCSQLGeneratorMSSQL.CreateUpdateSQL(
  const TableMap: TMVCTableMap;
  const ARInstance: TMVCActiveRecord): string;
begin
  Result := AppendUpdateRefresh(TableMap, inherited CreateUpdateSQL(TableMap, ARInstance));
end;

function TMVCSQLGeneratorMSSQL.CreateUpdateSQL(
  const TableMap: TMVCTableMap;
  const ARInstance: TMVCActiveRecord;
  const AChangedFields: TArray<string>): string;
begin
  Result := AppendUpdateRefresh(TableMap,
    inherited CreateUpdateSQL(TableMap, ARInstance, AChangedFields));
end;


function TMVCSQLGeneratorMSSQL.BuildSoftDeleteWhereSuffix(const TableMap: TMVCTableMap): string;
begin
  Result := inherited BuildSoftDeleteWhereSuffix(TableMap);
  // MSSQL has no FALSE keyword in default contexts; use 0 for BIT comparison.
  Result := StringReplace(Result, ' = FALSE', ' = 0', [rfIgnoreCase]);
end;

function TMVCSQLGeneratorMSSQL.BuildSoftDeleteSetDeleted(const TableMap: TMVCTableMap): string;
begin
  Result := inherited BuildSoftDeleteSetDeleted(TableMap);
  // MSSQL BIT columns: TRUE -> 1
  Result := StringReplace(Result, ' = TRUE', ' = 1', [rfIgnoreCase]);
end;

function TMVCSQLGeneratorMSSQL.BuildSoftDeleteSetRestored(const TableMap: TMVCTableMap): string;
begin
  Result := inherited BuildSoftDeleteSetRestored(TableMap);
  // MSSQL BIT columns: FALSE -> 0
  Result := StringReplace(Result, ' = FALSE', ' = 0', [rfIgnoreCase]);
end;

function TMVCSQLGeneratorMSSQL.GetRowsAffectedSQL: string;
begin
  Result := 'SELECT CAST(@@ROWCOUNT AS BIGINT) AS dmvc_rows_affected';
end;

function TMVCSQLGeneratorMSSQL.GetCompilerClass: TRQLCompilerClass;
begin
  Result := TRQLMSSQLCompiler;
end;

initialization

TMVCSQLGeneratorRegistry.Instance.RegisterSQLGenerator('mssql', TMVCSQLGeneratorMSSQL);

finalization

TMVCSQLGeneratorRegistry.Instance.UnRegisterSQLGenerator('mssql');

end.
