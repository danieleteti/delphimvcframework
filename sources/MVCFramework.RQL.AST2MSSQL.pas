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

unit MVCFramework.RQL.AST2MSSQL;

interface

uses
  MVCFramework.RQL.Parser,
  MVCFramework.Commons;

type
  TRQLMSSQLCompiler = class(TRQLCompiler)
  private
    function RQLFilterToSQL(const aRQLFIlter: TRQLFilter): string;
    function RQLSortToSQL(const aRQLSort: TRQLSort): string;
    function RQLLimitToSQL(const aRQLLimit: TRQLLimit): string;
    function RQLWhereToSQL(const aRQLWhere: TRQLWhere): string;
    function RQLLogicOperatorToSQL(const aRQLFIlter: TRQLLogicOperator): string;
  protected
    procedure AdjustAST(const aRQLAST: TRQLAbstractSyntaxTree); override;
    function RQLCustom2SQL(const aRQLCustom: TRQLCustom): string; override;
  end;

implementation

uses
  System.SysUtils;

{ TRQLMSSQLCompiler }

procedure TRQLMSSQLCompiler.AdjustAST(const aRQLAST: TRQLAbstractSyntaxTree);
var
  lLimit, lTmp: TRQLCustom;
  lSort: TRQLSort;
begin
  inherited;
  if aRQLAST.TreeContainsToken(tkLimit, lLimit) then
  begin
    if TRQLLimit(lLimit).Count = 0 then
    begin
      raise ERQLException.Create('MSSQL Server do not support "FETCH NEXT 0"');
    end;
    if not aRQLAST.TreeContainsToken(tkSort, lTmp) then
    begin
      if aRQLAST.Last is TRQLLimit then
      begin
        lSort := TRQLSort.Create;
        aRQLAST.Insert(aRQLAST.Count-1, lSort);
        lSort.Add('+', GetPKFieldName);
      end
      else
      begin
        raise ERQLException.Create('Invalid position for RQLLimit');
      end;
    end;
  end;
end;

function TRQLMSSQLCompiler.RQLCustom2SQL(
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

function TRQLMSSQLCompiler.RQLFilterToSQL(const aRQLFIlter: TRQLFilter): string;
var
  lValue, lDBFieldName: string;
begin
  if (aRQLFIlter.RightValueType = vtString) and not(aRQLFIlter.Token in [tkContains, tkStarts]) then
    lValue := aRQLFIlter.OpRight.QuotedString('''')
  else if aRQLFIlter.RightValueType = vtBoolean then
  begin
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
        lValue := Format('%%%s%%', [lValue]).QuotedString('''');
        Result := Format('(LOWER(%s) LIKE %s)', [lDBFieldName, lValue.ToLower])
      end;
    tkStarts:
      begin
        lValue := Format('%s%%', [lValue]).QuotedString('''');
        Result := Format('(LOWER(%s) LIKE %s)', [lDBFieldName, lValue.ToLower])
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
          vtIntegerArray: // if array is empty, RightValueType is always vtIntegerArray
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

function TRQLMSSQLCompiler.RQLLimitToSQL(const aRQLLimit: TRQLLimit): string;
begin
  Result := Format(' /*limit*/ OFFSET %d ROWS FETCH NEXT %d ROWS ONLY', [aRQLLimit.Start, aRQLLimit.Count]);
end;

function TRQLMSSQLCompiler.RQLLogicOperatorToSQL(const aRQLFIlter: TRQLLogicOperator): string;
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

function TRQLMSSQLCompiler.RQLSortToSQL(const aRQLSort: TRQLSort): string;
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

function TRQLMSSQLCompiler.RQLWhereToSQL(const aRQLWhere: TRQLWhere): string;
begin
  Result := ' WHERE ';
end;

initialization

TRQLCompilerRegistry.Instance.RegisterCompiler('mssql', TRQLMSSQLCompiler);

finalization

TRQLCompilerRegistry.Instance.UnRegisterCompiler('mssql');

end.
