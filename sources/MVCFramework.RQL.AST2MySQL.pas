// *************************************************************************** }
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2019 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.RQL.AST2MySQL;

interface

uses
  MVCFramework.RQL.Parser;

type
  TRQLMySQLCompiler = class(TRQLCompiler)
  private
    function RQLFilterToSQL(const aRQLFIlter: TRQLFilter): string;
    function RQLSortToSQL(const aRQLSort: TRQLSort): string;
    function RQLLimitToSQL(const aRQLLimit: TRQLLimit): string;
    function RQLWhereToSQL(const aRQLWhere: TRQLWhere): string;
    function RQLLogicOperatorToSQL(const aRQLFIlter: TRQLLogicOperator): string;
    function RQLCustom2SQL(const aRQLCustom: TRQLCustom): string;
  public
    procedure AST2SQL(const aRQLAST: TRQLAbstractSyntaxTree; out aSQL: string); override;
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.RQL.AST2FirebirdSQL;

{ TRQLMySQLCompiler }

function TRQLMySQLCompiler.RQLCustom2SQL(
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

function TRQLMySQLCompiler.RQLFilterToSQL(const aRQLFIlter: TRQLFilter): string;
var
  lValue, lDBFieldName: string;
begin
  if aRQLFIlter.RightIsString then
    lValue := aRQLFIlter.OpRight.QuotedString('''')
  else
    lValue := aRQLFIlter.OpRight;

  lDBFieldName := GetDatabaseFieldName(aRQLFIlter.OpLeft);

  case aRQLFIlter.Token of
    tkEq:
      begin
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
        Result := Format('(%s != %s)', [lDBFieldName, lValue]);
      end;
    tkContains:
      begin
        Result := Format('(LOWER(%s) LIKE ''%%%s%%'')', [lDBFieldName, lValue.DeQuotedString.ToLower ])
      end;
  end;
end;

function TRQLMySQLCompiler.RQLLimitToSQL(const aRQLLimit: TRQLLimit): string;
begin
  Result := Format(' /*limit*/ LIMIT %d, %d', [aRQLLimit.Start, aRQLLimit.Count]);
end;

function TRQLMySQLCompiler.RQLLogicOperatorToSQL(const aRQLFIlter: TRQLLogicOperator): string;
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

function TRQLMySQLCompiler.RQLSortToSQL(const aRQLSort: TRQLSort): string;
var
  I: Integer;
begin
  Result := ' /*sort*/ ORDER BY';
  for I := 0 to aRQLSort.Fields.Count - 1 do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + ' ' + GetDatabaseFieldName(aRQLSort.Fields[I]);
    if aRQLSort.Signs[I] = '+' then
      Result := Result + ' ASC'
    else
      Result := Result + ' DESC';
  end;
end;

function TRQLMySQLCompiler.RQLWhereToSQL(const aRQLWhere: TRQLWhere): string;
begin
  Result := ' where ';
end;

procedure TRQLMySQLCompiler.AST2SQL(const aRQLAST: TRQLAbstractSyntaxTree;
  out aSQL: string);
var
  lBuff: TStringBuilder;
  lItem: TRQLCustom;
begin
  inherited;

  {
    Here you can rearrange tokens in the list, for example:
    For firebird and mysql syntax you have: filters, sort, limit (default)
    For MSSQL syntax you need to rearrange in: limit, filters, sort
  }

  lBuff := TStringBuilder.Create;
  try
    for lItem in aRQLAST do
    begin
      lBuff.Append(RQLCustom2SQL(lItem));
    end;
    aSQL := lBuff.ToString;
  finally
    lBuff.Free;
  end;
end;

initialization

TRQLCompilerRegistry.Instance.RegisterCompiler('mysql', TRQLMySQLCompiler);

finalization

TRQLCompilerRegistry.Instance.UnRegisterCompiler('mysql');

end.
