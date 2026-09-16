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

unit MVCFramework.RQL.AST2Oracle;

// Oracle 12c+ RQL Compiler
// Uses standard SQL:2008 OFFSET/FETCH syntax (same as MSSQL)
// Boolean literals mapped to 1/0 (Oracle has no native BOOLEAN column type)
// Field/table quoting uses double quotes

interface

uses
  MVCFramework.RQL.Parser,
  MVCFramework.Commons;

type
  TRQLOracleCompiler = class(TRQLCompiler)
  private
    function RQLFilterToSQL(const aRQLFIlter: TRQLFilter): string;
    function RQLSortToSQL(const aRQLSort: TRQLSort): string;
    function RQLLimitToSQL(const aRQLLimit: TRQLLimit): string;
    function RQLWhereToSQL(const aRQLWhere: TRQLWhere): string;
    function RQLLogicOperatorToSQL(const aRQLFIlter: TRQLLogicOperator): string;
  protected
    function RQLCustom2SQL(const aRQLCustom: TRQLCustom): string; override;
  end;

implementation

uses
  System.SysUtils;

function TRQLOracleCompiler.RQLCustom2SQL(
  const aRQLCustom: TRQLCustom): string;
begin
  if aRQLCustom is TRQLFilter then
    Result := RQLFilterToSQL(TRQLFilter(aRQLCustom))
  else if aRQLCustom is TRQLLogicOperator then
    Result := RQLLogicOperatorToSQL(TRQLLogicOperator(aRQLCustom))
  else if aRQLCustom is TRQLSort then
    Result := RQLSortToSQL(TRQLSort(aRQLCustom))
  else if aRQLCustom is TRQLLimit then
    Result := RQLLimitToSQL(TRQLLimit(aRQLCustom))
  else if aRQLCustom is TRQLWhere then
    Result := RQLWhereToSQL(TRQLWhere(aRQLCustom))
  else
    raise ERQLException.CreateFmt('Unknown token in compiler: %s', [aRQLCustom.ClassName]);
end;

function TRQLOracleCompiler.RQLFilterToSQL(const aRQLFIlter: TRQLFilter): string;
var
  lValue, lDBFieldName: string;
begin
  if (aRQLFIlter.RightValueType = vtString) and not(aRQLFIlter.Token in [tkContains, tkStarts]) then
    lValue := aRQLFIlter.OpRight.QuotedString('''')
  else if aRQLFIlter.RightValueType = vtBoolean then
  begin
    // Oracle has no native BOOLEAN column type - use 1/0
    if SameText(aRQLFIlter.OpRight, 'true') then
      lValue := '1'
    else
      lValue := '0';
  end
  else
    lValue := aRQLFIlter.OpRight;

  lDBFieldName := GetDatabaseFieldName(aRQLFIlter.OpLeft, True);

  case aRQLFIlter.Token of
    tkEq:
      begin
        if aRQLFIlter.RightValueType = vtNull then
          Result := Format('(%s IS NULL)', [lDBFieldName])
        else
          Result := Format('(%s = %s)', [lDBFieldName, lValue]);
      end;
    tkLt:
      Result := Format('(%s < %s)', [lDBFieldName, lValue]);
    tkLe:
      Result := Format('(%s <= %s)', [lDBFieldName, lValue]);
    tkGt:
      Result := Format('(%s > %s)', [lDBFieldName, lValue]);
    tkGe:
      Result := Format('(%s >= %s)', [lDBFieldName, lValue]);
    tkNe:
      begin
        if aRQLFIlter.RightValueType = vtNull then
          Result := Format('(%s IS NOT NULL)', [lDBFieldName])
        else
          Result := Format('(%s != %s)', [lDBFieldName, lValue]);
      end;
    tkContains:
      begin
        lValue := Format('%%%s%%', [lValue]).QuotedString('''');
        Result := Format('(LOWER(%s) LIKE %s)', [lDBFieldName, lValue.ToLower]);
      end;
    tkStarts:
      begin
        lValue := Format('%s%%', [lValue]).QuotedString('''');
        Result := Format('(LOWER(%s) LIKE %s)', [lDBFieldName, lValue.ToLower]);
      end;
    tkIn:
      begin
        case aRQLFIlter.RightValueType of
          vtNumericArray:
            Result := Format('(%s IN (%s))', [
              lDBFieldName, string.Join(',', aRQLFIlter.OpRightArray)]);
          vtStringArray:
            Result := Format('(%s IN (%s))', [
              lDBFieldName, string.Join(',', QuoteStringArray(aRQLFIlter.OpRightArray))]);
        else
          raise ERQLException.Create('Invalid RightValueType for tkIn');
        end;
      end;
    tkOut:
      begin
        case aRQLFIlter.RightValueType of
          vtNumericArray:
            Result := Format('(%s NOT IN (%s))', [
              lDBFieldName, string.Join(',', aRQLFIlter.OpRightArray)]);
          vtStringArray:
            Result := Format('(%s NOT IN (%s))', [
              lDBFieldName, string.Join(',', QuoteStringArray(aRQLFIlter.OpRightArray))]);
        else
          raise ERQLException.Create('Invalid RightValueType for tkOut');
        end;
      end;
  end;
end;

function TRQLOracleCompiler.RQLLimitToSQL(const aRQLLimit: TRQLLimit): string;
begin
  // Oracle 12c+ supports standard SQL:2008 OFFSET/FETCH syntax
  Result := Format(' /*limit*/ OFFSET %d ROWS FETCH NEXT %d ROWS ONLY', [aRQLLimit.Start, aRQLLimit.Count]);
end;

function TRQLOracleCompiler.RQLLogicOperatorToSQL(const aRQLFIlter: TRQLLogicOperator): string;
var
  lJoin: string;
  lRQLCustom: TRQLCustom;
  lFirst: Boolean;
begin
  case aRQLFIlter.Token of
    tkAnd: lJoin := ' and ';
    tkOr:  lJoin := ' or ';
  else
    raise ERQLException.Create('Invalid token in RQLLogicOperator');
  end;

  Result := '';
  lFirst := True;
  for lRQLCustom in aRQLFIlter.FilterAST do
  begin
    if not lFirst then
      Result := Result + lJoin;
    lFirst := False;
    Result := Result + RQLCustom2SQL(lRQLCustom);
  end;
  Result := '(' + Result + ')';
end;

function TRQLOracleCompiler.RQLSortToSQL(const aRQLSort: TRQLSort): string;
var
  I: Integer;
begin
  Result := ' /*sort*/ ORDER BY';
  for I := 0 to aRQLSort.Fields.Count - 1 do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + ' ' + GetDatabaseFieldName(aRQLSort.Fields[I], True);
    if aRQLSort.Signs[I] = '+' then
      Result := Result + ' ASC'
    else
      Result := Result + ' DESC';
  end;
end;

function TRQLOracleCompiler.RQLWhereToSQL(const aRQLWhere: TRQLWhere): string;
begin
  Result := ' WHERE ';
end;

initialization

TRQLCompilerRegistry.Instance.RegisterCompiler('oracle', TRQLOracleCompiler);

finalization

TRQLCompilerRegistry.Instance.UnRegisterCompiler('oracle');

end.
