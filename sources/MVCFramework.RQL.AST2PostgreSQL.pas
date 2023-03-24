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

unit MVCFramework.RQL.AST2PostgreSQL;

interface

uses
  MVCFramework.RQL.Parser;

type
  TRQLPostgreSQLCompiler = class(TRQLCompiler)
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
  System.SysUtils,
  FireDAC.Phys.PG;

{ TRQLPostgreSQLCompiler }

function TRQLPostgreSQLCompiler.RQLCustom2SQL(
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

function TRQLPostgreSQLCompiler.RQLFilterToSQL(const aRQLFIlter: TRQLFilter): string;
var
  lValue, lDBFieldName: string;
begin
  if (aRQLFIlter.RightValueType = vtString) and not(aRQLFIlter.Token in [tkContains, tkStarts]) then
    lValue := aRQLFIlter.OpRight.QuotedString('''')
  else
    lValue := aRQLFIlter.OpRight;

  lDBFieldName := GetDatabaseFieldName(aRQLFIlter.OpLeft, True);

  case aRQLFIlter.Token of
    tkEq:
      begin
        if aRQLFIlter.RightValueType = vtNull then
          Result := Format('(%s IS NULL)', [GetFieldNameForSQL(lDBFieldName)])
        else
          Result := Format('(%s = %s)', [GetFieldNameForSQL(lDBFieldName), lValue]);
      end;
    tkLt:
      begin
        Result := Format('(%s < %s)', [GetFieldNameForSQL(lDBFieldName), lValue]);
      end;
    tkLe:
      begin
        Result := Format('(%s <= %s)', [GetFieldNameForSQL(lDBFieldName), lValue]);
      end;
    tkGt:
      begin
        Result := Format('(%s > %s)', [GetFieldNameForSQL(lDBFieldName), lValue]);
      end;
    tkGe:
      begin
        Result := Format('(%s >= %s)', [GetFieldNameForSQL(lDBFieldName), lValue]);
      end;
    tkNe:
      begin
        if aRQLFIlter.RightValueType = vtNull then
          Result := Format('(%s IS NOT NULL)', [GetFieldNameForSQL(lDBFieldName)])
        else
          Result := Format('(%s != %s)', [GetFieldNameForSQL(lDBFieldName), lValue]);
      end;
    tkContains:
      begin
        lValue := Format('%%%s%%', [lValue]).QuotedString('''');
        Result := Format('(%s ILIKE %s)', [GetFieldNameForSQL(lDBFieldName), lValue.ToLower])
      end;
    tkStarts:
      begin
        lValue := Format('%s%%', [lValue]).QuotedString('''');
        Result := Format('(%s ILIKE %s)', [GetFieldNameForSQL(lDBFieldName), lValue.ToLower])
      end;
    tkIn:
      begin
        case aRQLFIlter.RightValueType of
          vtIntegerArray: // if array is empty, RightValueType is always vtIntegerArray
            begin
              Result := Format('(%s IN (%s))', [
                GetFieldNameForSQL(lDBFieldName), string.Join(',', aRQLFIlter.OpRightArray)
                ]);
            end;
          vtStringArray:
            begin
              Result := Format('(%s IN (%s))', [
                GetFieldNameForSQL(lDBFieldName), string.Join(',', QuoteStringArray(aRQLFIlter.OpRightArray))
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
                GetFieldNameForSQL(lDBFieldName), string.Join(',', aRQLFIlter.OpRightArray)
                ]);
            end;
          vtStringArray:
            begin
              Result := Format('(%s NOT IN (%s))', [
                GetFieldNameForSQL(lDBFieldName), string.Join(',', QuoteStringArray(aRQLFIlter.OpRightArray))
                ]);
            end;
        else
          raise ERQLException.Create('Invalid RightValueType for tkOut');
        end;
      end;
  end;
end;

function TRQLPostgreSQLCompiler.RQLLimitToSQL(const aRQLLimit: TRQLLimit): string;
begin
  if aRQLLimit.Start = 0 then
  begin
    Result := Format(' /*limit*/ LIMIT %d', [aRQLLimit.Count]);
  end
  else
  begin
    Result := Format(' /*limit*/ LIMIT %d OFFSET %d', [aRQLLimit.Count, aRQLLimit.Start]);
  end;
end;

function TRQLPostgreSQLCompiler.RQLLogicOperatorToSQL(const aRQLFIlter: TRQLLogicOperator): string;
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

function TRQLPostgreSQLCompiler.RQLSortToSQL(const aRQLSort: TRQLSort): string;
var
  I: Integer;
begin
  Result := ' /*sort*/ ORDER BY';
  for I := 0 to aRQLSort.Fields.Count - 1 do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + ' ' + GetFieldNameForSQL(GetDatabaseFieldName(aRQLSort.Fields[I], True));
    if aRQLSort.Signs[I] = '+' then
      Result := Result + ' ASC'
    else
      Result := Result + ' DESC';
  end;
end;

function TRQLPostgreSQLCompiler.RQLWhereToSQL(const aRQLWhere: TRQLWhere): string;
begin
  Result := ' WHERE ';
end;

initialization

TRQLCompilerRegistry.Instance.RegisterCompiler('postgresql', TRQLPostgreSQLCompiler);

finalization

TRQLCompilerRegistry.Instance.UnRegisterCompiler('postgresql');

end.
