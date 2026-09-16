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

unit MVCFramework.SQLGenerators.MySQL;

interface

uses
  FireDAC.Phys.MySQLDef,
  FireDAC.Phys.MySQL,
  System.Rtti,
  System.Generics.Collections,
  MVCFramework.RQL.Parser,
  MVCFramework.ActiveRecord,
  MVCFramework.Commons;

type
  TMVCSQLGeneratorMySQL = class(TMVCSQLGenerator)
  protected
    function GetCompilerClass: TRQLCompilerClass; override;
  public
    function CreateInsertSQL(
      const TableMap: TMVCTableMap;
      const ARInstance: TMVCActiveRecord
      ): string; override;
    function HandlesRefreshNatively: Boolean; override;
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.RQL.AST2MySQL;

function TMVCSQLGeneratorMySQL.CreateInsertSQL(
  const TableMap: TMVCTableMap;
  const ARInstance: TMVCActiveRecord
  ): string;
var
  lKeyValue: TPair<TRttiField, TFieldInfo>;
  lSB: TStringBuilder;
  lFieldName: String;
begin
  lSB := TStringBuilder.Create;
  try
    lSB.Append('INSERT INTO ' + GetTableNameForSQL(TableMap.fTableName) + '(');
    // All PK columns that participate in the INSERT. Single-PK: identical output.
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

    if TableMap.fAutoGenPKIndex >= 0 then
    begin
      { The alias has to be quoted like any other identifier: written raw, a key
        column named "id with spaces" produced "... as id with spaces" and the
        server rejected the whole INSERT. Any name needing quotes - spaces, or a
        reserved word - did the same. }
      lSB.Append(';SELECT LAST_INSERT_ID() as ' +
        GetFieldNameForSQL(TableMap.fPrimaryKeys[TableMap.fAutoGenPKIndex].FieldName));
    end;
    Result := lSB.ToString;
  finally
    lSB.Free;
  end;
end;

function TMVCSQLGeneratorMySQL.HandlesRefreshNatively: Boolean;
begin
  Result := False;
end;

function TMVCSQLGeneratorMySQL.GetCompilerClass: TRQLCompilerClass;
begin
  Result := TRQLMySQLCompiler;
end;

initialization

TMVCSQLGeneratorRegistry.Instance.RegisterSQLGenerator('mysql', TMVCSQLGeneratorMySQL);

finalization

TMVCSQLGeneratorRegistry.Instance.UnRegisterSQLGenerator('mysql');

end.
