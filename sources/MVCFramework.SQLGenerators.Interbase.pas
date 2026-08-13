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

unit MVCFramework.SQLGenerators.Interbase;

interface

uses
  System.Rtti,
  System.Generics.Collections,
  FireDAC.Phys.IB,
  FireDAC.Phys.IBDef,
  MVCFramework.ActiveRecord,
  MVCFramework.Commons,
  MVCFramework.SQLGenerators.Firebird,
  MVCFramework.RQL.Parser;

type
  TMVCSQLGeneratorInterbase = class(TMVCSQLGeneratorFirebird)
  public
    function CreateInsertSQL(
      const TableMap: TMVCTableMap;
      const ARInstance: TMVCActiveRecord): string; override;
    function HasReturning: Boolean; override;
    function HandlesRefreshNatively: Boolean; override;
  end;

implementation

uses
  System.SysUtils;

{ TMVCSQLGeneratorInterbase }

function TMVCSQLGeneratorInterbase.CreateInsertSQL(
  const TableMap: TMVCTableMap;
  const ARInstance: TMVCActiveRecord): string;
var
  lKeyValue: TPair<TRttiField, TFieldInfo>;
  lSB: TStringBuilder;
  lFieldName: String;
  lPKIdx: Integer;
begin
  lSB := TStringBuilder.Create;
  try
    lSB.Append('INSERT INTO ' + GetTableNameForSQL(TableMap.fTableName) + '(');
    // Interbase has no RETURNING: an auto-generated PK value is pre-fetched via a
    // generator (FillPrimaryKey) BEFORE the insert, so every PK column that is not
    // read-only is written as a value. Single-PK: identical to the old emission.
    for lPKIdx := 0 to High(TableMap.fPrimaryKeys) do
      if not (foReadOnly in TableMap.fPrimaryKeys[lPKIdx].Options) then
        lSB.Append(GetFieldNameForSQL(TableMap.fPrimaryKeys[lPKIdx].FieldName) + ',');

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
    for lPKIdx := 0 to High(TableMap.fPrimaryKeys) do
      if not (foReadOnly in TableMap.fPrimaryKeys[lPKIdx].Options) then
        lSB.Append(':' + GetParamNameForSQL(TableMap.fPrimaryKeys[lPKIdx].FieldName) + ',');

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
end;

function TMVCSQLGeneratorInterbase.HasReturning: Boolean;
begin
  Result := False;
end;

function TMVCSQLGeneratorInterbase.HandlesRefreshNatively: Boolean;
begin
  { Inherited from the Firebird generator this answered True, which claims the
    INSERT surfaces foRefresh columns through a RETURNING clause. Interbase has
    no RETURNING and CreateInsertSQL above emits none, so the framework opened a
    statement that returns no result set and the insert died with "Cannot open /
    define command, which does not return result sets". Those columns are read
    back by the separate SELECT in RefreshFromDB instead. }
  Result := False;
end;

initialization

TMVCSQLGeneratorRegistry.Instance.RegisterSQLGenerator('interbase',
  TMVCSQLGeneratorInterbase);

finalization

TMVCSQLGeneratorRegistry.Instance.UnRegisterSQLGenerator('interbase');

end.
