// *************************************************************************** }
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.RQL.AST2FirebirdSQL;

interface

uses
  MVCFramework.RQL.Parser;

const
  RQLFirebirdReservedWords: TArray<string> = [
    'ADD', 'ADMIN', 'ALL', 'ALTER', 'AND', 'ANY', 'AS', 'AT', 'AVG',
    'BEGIN', 'BETWEEN', 'BIGINT', 'BIT_LENGTH', 'BLOB', 'BOTH', 'BY',
    'CASE', 'CAST', 'CHAR', 'CHAR_LENGTH', 'CHARACTER', 'CHARACTER_LENGTH',
    'CHECK', 'CLOSE', 'COLLATE', 'COLUMN', 'COMMIT', 'CONNECT', 'CONSTRAINT',
    'COUNT', 'CREATE', 'CROSS', 'CURRENT', 'CURRENT_CONNECTION',
    'CURRENT_DATE', 'CURRENT_ROLE', 'CURRENT_TIME', 'CURRENT_TIMESTAMP',
    'CURRENT_TRANSACTION', 'CURRENT_USER', 'CURSOR',
    'DATE', 'DAY', 'DEC', 'DECIMAL', 'DECLARE', 'DEFAULT', 'DELETE',
    'DISCONNECT', 'DISTINCT', 'DOUBLE', 'DROP',
    'ELSE', 'END', 'ESCAPE', 'EXECUTE', 'EXISTS', 'EXTERNAL', 'EXTRACT',
    'FETCH', 'FILTER', 'FLOAT', 'FOR', 'FOREIGN', 'FROM', 'FULL', 'FUNCTION',
    'GDSCODE', 'GLOBAL', 'GRANT', 'GROUP',
    'HAVING', 'HOUR',
    'IN', 'INDEX', 'INNER', 'INSENSITIVE', 'INSERT', 'INT', 'INTEGER', 'INTO', 'IS',
    'JOIN',
    'LEADING', 'LEFT', 'LIKE', 'LONG', 'LOWER',
    'MAX', 'MAXIMUM_SEGMENT', 'MERGE', 'MIN', 'MINUTE', 'MONTH',
    'NATIONAL', 'NATURAL', 'NCHAR', 'NO', 'NOT', 'NULL', 'NUMERIC',
    'OCTET_LENGTH', 'OF', 'ON', 'ONLY', 'OPEN', 'OR', 'ORDER', 'OUTER',
    'PARAMETER', 'PLAN', 'POSITION', 'POST_EVENT', 'PRECISION', 'PRIMARY', 'PROCEDURE',
    'RDB$DB_KEY', 'REAL', 'RECORD_VERSION', 'RECREATE', 'RECURSIVE', 'REFERENCES', 'RELEASE',
    'RETURNING_VALUES', 'RETURNS', 'REVOKE', 'RIGHT', 'ROLLBACK', 'ROW_COUNT', 'ROWS',
    'SAVEPOINT', 'SECOND', 'SELECT', 'SENSITIVE', 'SET', 'SIMILAR', 'SMALLINT', 'SOME',
    'SQLCODE', 'SQLSTATE', 'START', 'SUM',
    'TABLE', 'THEN', 'TIME', 'TIMESTAMP', 'TO', 'TRAILING', 'TRIGGER', 'TRIM',
    'UNION', 'UNIQUE', 'UPDATE', 'UPPER', 'USER', 'USING',
    'VALUE', 'VALUES', 'VARCHAR', 'VARIABLE', 'VARYING', 'VIEW',
    'WHEN', 'WHERE', 'WHILE', 'WITH',
    'YEAR'];

type
  TRQLFirebirdCompiler = class(TRQLCompiler)
  protected
    function GetLiteralBoolean(const Value: Boolean): String; virtual;
    function RQLFilterToSQL(const aRQLFIlter: TRQLFilter): string; virtual;
    function RQLSortToSQL(const aRQLSort: TRQLSort): string; virtual;
    function RQLLimitToSQL(const aRQLLimit: TRQLLimit): string; virtual;
    function RQLWhereToSQL(const aRQLWhere: TRQLWhere): string; virtual;
    function RQLLogicOperatorToSQL(const aRQLFIlter: TRQLLogicOperator): string; virtual;
    function RQLCustom2SQL(const aRQLCustom: TRQLCustom): string; override;
  public
    function GetFieldNameForSQL(const FieldName: string): string; override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  MVCFramework.ActiveRecord;

{ TRQLFirebirdCompiler }

function TRQLFirebirdCompiler.GetFieldNameForSQL(const FieldName: string): string;
begin
  if MatchStr(FieldName.ToUpper, RQLFirebirdReservedWords) then
    Result := FieldName.QuotedString('"')
  else
    Result := inherited;
end;

function TRQLFirebirdCompiler.GetLiteralBoolean(const Value: Boolean): String;
begin
  if Value then
  begin
    Exit('true');
  end;
  Exit('false');
end;

function TRQLFirebirdCompiler.RQLCustom2SQL(
  const aRQLCustom: TRQLCustom): string;
begin
  if aRQLCustom is TRQLFilter then
  begin
    Result := RQLFilterToSQL(TRQLFilter(aRQLCustom));
  end
  else if aRQLCustom is TRQLLogicOperator then
  begin
    Result := RQLLogicOperatorToSQL(TRQLLogicOperator(aRQLCustom));
  end
  else if aRQLCustom is TRQLSort then
  begin
    Result := RQLSortToSQL(TRQLSort(aRQLCustom));
  end
  else if aRQLCustom is TRQLLimit then
  begin
    Result := RQLLimitToSQL(TRQLLimit(aRQLCustom));
  end
  else if aRQLCustom is TRQLWhere then
  begin
    Result := RQLWhereToSQL(TRQLWhere(aRQLCustom));
  end
  else
    raise ERQLException.CreateFmt('Unknown token in compiler: %s', [aRQLCustom.ClassName]);
end;

function TRQLFirebirdCompiler.RQLFilterToSQL(const aRQLFIlter: TRQLFilter): string;
var
  lValue, lDBFieldName: string;
begin
  if aRQLFIlter.RightValueType = vtString then
    lValue := aRQLFIlter.OpRight.QuotedString('''')
  else if aRQLFIlter.RightValueType = vtBoolean then
  begin
    if SameText(aRQLFIlter.OpRight, 'true') then
      lValue := GetLiteralBoolean(true)
    else
      lValue := GetLiteralBoolean(false);
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
      begin
        Result := Format('(%s < %s)', [lDBFieldName, lValue]);
      end;
    tkLe:
      begin
        Result := Format('(%s <= %s)', [lDBFieldName, lValue]);
      end;
    tkGt:
      begin
        Result := Format('(%s > %s)', [lDBFieldName, lValue]);
      end;
    tkGe:
      begin
        Result := Format('(%s >= %s)', [lDBFieldName, lValue]);
      end;
    tkNe:
      begin
        if aRQLFIlter.RightValueType = vtNull then
          Result := Format('(%s IS NOT NULL)', [lDBFieldName])
        else
          Result := Format('(%s != %s)', [lDBFieldName, lValue]);
      end;
    tkContains:
      begin
        Result := Format('(%s containing %s)', [lDBFieldName, lValue.ToLower])
      end;
    tkStarts:
      begin
        Result := Format('(%s starting with %s)', [lDBFieldName, lValue.ToLower])
      end;
    tkIn:
      begin
        case aRQLFIlter.RightValueType of
          vtIntegerArray: // if array is empty, RightValueType is always vtIntegerArray
            begin
              Result := Format('(%s IN (%s))', [
                lDBFieldName, string.Join(',', aRQLFIlter.OpRightArray)
                ]);
            end;
          vtStringArray:
            begin
              Result := Format('(%s IN (%s))', [
                lDBFieldName, string.Join(',', QuoteStringArray(aRQLFIlter.OpRightArray))
                ]);
            end;
        else
          raise ERQLException.Create('Invalid RightValueType for tkIn');
        end;
      end;
    tkOut:
      begin
        case aRQLFIlter.RightValueType of
          vtIntegerArray:
            begin
              Result := Format('(%s NOT IN (%s))', [
                lDBFieldName, string.Join(',', aRQLFIlter.OpRightArray)
                ]);
            end;
          vtStringArray:
            begin
              Result := Format('(%s NOT IN (%s))', [
                lDBFieldName, string.Join(',', QuoteStringArray(aRQLFIlter.OpRightArray))
                ]);
            end;
        else
          raise ERQLException.Create('Invalid RightValueType for tkOut');
        end;
      end;
  end;
end;

function TRQLFirebirdCompiler.RQLLimitToSQL(const aRQLLimit: TRQLLimit): string;
begin
  // firebird ROWS requires Start > 0. Limit function is 0 based, so we have to add 1 to start.
  Result := Format(' /*limit*/  ROWS %d to %d', [aRQLLimit.Start + 1, aRQLLimit.Start + aRQLLimit.Count]);
end;

function TRQLFirebirdCompiler.RQLLogicOperatorToSQL(const aRQLFIlter: TRQLLogicOperator): string;
var
  lJoin: string;
  lRQLCustom: TRQLCustom;
  lFirst: Boolean;
begin
  case aRQLFIlter.Token of
    tkAnd:
      begin
        lJoin := ' and ';
      end;
    tkOr:
      begin
        lJoin := ' or ';
      end;
  else
    raise ERQLException.Create('Invalid token in RQLLogicOperator');
  end;

  Result := '';
  lFirst := True;
  for lRQLCustom in aRQLFIlter.FilterAST do
  begin
    if not lFirst then
    begin
      Result := Result + lJoin;
    end;
    lFirst := False;
    Result := Result + RQLCustom2SQL(lRQLCustom);
  end;
  Result := '(' + Result + ')';
end;

function TRQLFirebirdCompiler.RQLSortToSQL(const aRQLSort: TRQLSort): string;
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

function TRQLFirebirdCompiler.RQLWhereToSQL(const aRQLWhere: TRQLWhere): string;
begin
  Result := ' where ';
end;

initialization

TRQLCompilerRegistry.Instance.RegisterCompiler('firebird', TRQLFirebirdCompiler);

finalization

TRQLCompilerRegistry.Instance.UnRegisterCompiler('firebird');

end.
