// *************************************************************************** }
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2018 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.RQL2SQL;

interface

uses
  System.Generics.Collections,
  System.SysUtils;

{
  http://www.persvr.org/rql/
  https://github.com/persvr/rql

  http://dundalek.com/rql
  https://www.sitepen.com/blog/2010/11/02/resource-query-language-a-query-language-for-the-web-nosql/

  Here is a definition of the common operators (individual stores may have support for more less operators):

  eq(<property>,<value>) - Filters for objects where the specified property's value is equal to the provided value
  lt(<property>,<value>) - Filters for objects where the specified property's value is less than the provided value
  le(<property>,<value>) - Filters for objects where the specified property's value is less than or equal to the provided value
  gt(<property>,<value>) - Filters for objects where the specified property's value is greater than the provided value
  ge(<property>,<value>) - Filters for objects where the specified property's value is greater than or equal to the provided value
  ne(<property>,<value>) - Filters for objects where the specified property's value is not equal to the provided value
  and(<query>,<query>,...) - Applies all the given queries
  or(<query>,<query>,...) - The union of the given queries


  sort(<+|-><property) - Sorts by the given property in order specified by the prefix (+ for ascending, - for descending)
  select(<property>,<property>,...) - Trims each object down to the set of properties defined in the arguments
  values(<property>) - Returns an array of the given property value for each object
  aggregate(<property|function>,...) - Aggregates the array, grouping by objects that are distinct for the provided properties, and then reduces the remaining other property values using the provided functions
  distinct() - Returns a result set with duplicates removed
  in(<property>,<array-of-values>) - Filters for objects where the specified property's value is in the provided array
  out(<property>,<array-of-values>) - Filters for objects where the specified property's value is not in the provided array
  limit(count,start,maxCount) - Returns the given range of objects from the result set

  //////NOT AVAILABLES
  contains(<property>,<value | expression>) - Filters for objects where the specified property's value is an array and the array contains any value that equals the provided value or satisfies the provided expression.
  excludes(<property>,<value | expression>) - Filters for objects where the specified property's value is an array and the array does not contain any of value that equals the provided value or satisfies the provided expression.
  rel(<relation name?>,<query>) - Applies the provided query against the linked data of the provided relation name.
  sum(<property?>) - Finds the sum of every value in the array or if the property argument is provided, returns the sum of the value of property for every object in the array
  mean(<property?>) - Finds the mean of every value in the array or if the property argument is provided, returns the mean of the value of property for every object in the array
  max(<property?>) - Finds the maximum of every value in the array or if the property argument is provided, returns the maximum of the value of property for every object in the array
  min(<property?>) - Finds the minimum of every value in the array or if the property argument is provided, returns the minimum of the value of property for every object in the array
  recurse(<property?>) - Recursively searches, looking in children of the object as objects in arrays in the given property value
  first() - Returns the first record of the query's result set
  one() - Returns the first and only record of the query's result set, or produces an error if the query's result set has more or less than one record in it.
  count() - Returns the count of the number of records in the query's result set
}

type
  TRQLToken = (tkEq, tkLt, tkLe, tkGt, tkGe, tkNe, tkAnd, tkOr, tkSort, { RQL } tkAmpersand, tkEOF, tkOpenPar, tkClosedPar, tkComma,
    tkPlus, tkMinus, tkDblQuote, tkQuote, tkSpace, tkUnknown);

  TRQLCustom = class abstract
  public
    function ToSQL: string; virtual; abstract;
    constructor Create; virtual;
  end;

  TRQLCustomOperator = class abstract(TRQLCustom)

  end;

  TRQLWhere = class(TRQLCustom)
    function ToSQL: string; override;
  end;

  TRQLFilter = class(TRQLCustomOperator)
  public
    Token: TRQLToken;
    OpLeft: string;
    OpRight: string;
    RightIsString: Boolean;
    function ToSQL: string; override;
  end;

  TRQLLogicOperator = class(TRQLCustom)
  private
    fRQLFilter: TObjectList<TRQLCustom>;
    fToken: TRQLToken;
  public
    constructor Create(const Token: TRQLToken); reintroduce;
    destructor Destroy; override;
    function ToSQL: string; override;
    property FilterAST: TObjectList<TRQLCustom> read fRQLFilter;
    procedure AddRQLCustom(const aRQLCustom: TRQLCustom);
  end;

  TRQLSort = class(TRQLCustom)
  private
    fFields: TList<string>;
    fSigns: TList<string>;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ToSQL: string; override;
    procedure Add(const Sign, FieldName: string);

  const
    SIGNS: array [tkPlus .. tkMinus] of string = ('+', '-');
  end;

  ERQLException = class(Exception)

  end;

  TRQL2SQL = class
  private
    fCurIdx: Integer;
    fInput: string;
    fAST: TObjectList<TRQLCustom>;
    fSavedPos: Integer;
    fInputLength: Integer;
  protected
    /// /// RQL Sections
    function ParseFilters: Boolean;
    procedure ParseSort;
    /// ///RQL functions
    procedure ParseBinOperator(const aToken: TRQLToken; const aAST: TObjectList<TRQLCustom>);
    procedure ParseLogicOperator(const aToken: TRQLToken; const aAST: TObjectList<TRQLCustom>);
    /// //Parser utils
    function MatchFieldName(out lFieldName: string): Boolean;
    function MatchFieldStringValue(out lFieldValue: string): Boolean;
    function MatchFieldNumericValue(out lFieldValue: string): Boolean;
    function MatchSymbol(const Symbol: Char): Boolean;
    procedure SaveCurPos;
    procedure BackToLastPos;
    function C(const LookAhead: UInt8 = 0): Char;
    function GetToken: TRQLToken;
    procedure Skip(const Count: UInt8);
    procedure Error(const Message: string);
    function IsLetter(const aChar: Char): Boolean;
    function IsDigit(const aChar: Char): Boolean;
    procedure EatWhiteSpaces;
    procedure CheckEOF(const Token: TRQLToken);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(const RQL: string; out SQL: string);
  end;

implementation


uses
  System.Character;

{ TRQL2SQL }

procedure TRQL2SQL.BackToLastPos;
begin
  fCurIdx := fSavedPos;
end;

function TRQL2SQL.C(const LookAhead: UInt8): Char;
begin
  if fCurIdx + LookAhead >= fInputLength then
    Exit(#0);
  Result := fInput.Chars[fCurIdx + LookAhead];
end;

procedure TRQL2SQL.CheckEOF(const Token: TRQLToken);
begin
  if Token = tkEOF then
    Error('Unexpected end of expression');
end;

constructor TRQL2SQL.Create;
begin
  inherited;
  fAST := TObjectList<TRQLCustom>.Create(True);
end;

destructor TRQL2SQL.Destroy;
begin
  fAST.Free;
  inherited;
end;

procedure TRQL2SQL.EatWhiteSpaces;
var
  lToken: TRQLToken;
begin
  while True do
  begin
    SaveCurPos;
    lToken := GetToken;
    if lToken <> tkSpace then
    begin
      BackToLastPos;
      Break;
    end
    else
    begin
      Skip(1);
    end;
  end;
end;

procedure TRQL2SQL.Error(const Message: string);
begin
  raise ERQLException.CreateFmt('[Error] %s (column %d - found %s)', [message, fCurIdx, C(0)]);
end;

procedure TRQL2SQL.Execute(const RQL: string; out SQL: string);
var
  lBuff: TStringBuilder;
  lItem: TRQLCustom;
begin
  fAST.Clear;
  fCurIdx := 0;
  fInput := RQL.Trim;
  fInputLength := Length(RQL);
  if fInputLength = 0 then
  begin
    SQL := '';
    Exit;
  end;
  if ParseFilters then
  begin
    fAST.Insert(0, TRQLWhere.Create);
    if GetToken = tkComma then
    begin
      EatWhiteSpaces;
      ParseSort;
    end;
  end
  else
  begin
    EatWhiteSpaces;
    ParseSort;
  end;
  EatWhiteSpaces;
  if GetToken <> tkEOF then
    Error('Expected EOF');
  lBuff := TStringBuilder.Create;
  try
    for lItem in fAST do
    begin
      lBuff.Append(lItem.ToSQL);
    end;
    SQL := lBuff.ToString;
  finally
    lBuff.Free;
  end;
end;

function TRQL2SQL.GetToken:
  TRQLToken;
var
  lChar: Char;
begin
  lChar := C(0);
  if (lChar = #0) then
  begin
    Exit(tkEOF);
  end;
  if (lChar = ',') then
  begin
    Skip(1);
    Exit(tkComma);
  end;
  if (lChar = '+') then
  begin
    Skip(1);
    Exit(tkPlus);
  end;
  if (lChar = '"') then
  begin
    Skip(1);
    Exit(tkDblQuote);
  end;
  if (lChar = '''') then
  begin
    Skip(1);
    Exit(tkQuote);
  end;
  if (lChar = '-') then
  begin
    Skip(1);
    Exit(tkMinus);
  end;
  if (lChar = '&') then
  begin
    Skip(1);
    Exit(tkAmpersand);
  end;
  if (lChar = '(') then
  begin
    Skip(1);
    Exit(tkOpenPar);
  end;
  if (lChar = ')') then
  begin
    Skip(1);
    Exit(tkClosedPar);
  end;
  if (lChar = 'e') and (C(1) = 'q') then
  begin
    Skip(2);
    Exit(tkEq);
  end;
  if (lChar = 'l') and (C(1) = 't') then
  begin
    Skip(2);
    Exit(tkLt);
  end;
  if (lChar = 'l') and (C(1) = 'e') then
  begin
    Skip(2);
    Exit(tkLe);
  end;
  if (lChar = 'g') and (C(1) = 't') then
  begin
    Skip(2);
    Exit(tkGt);
  end;
  if (lChar = 'g') and (C(1) = 'e') then
  begin
    Skip(2);
    Exit(tkGe);
  end;
  if (lChar = 'n') and (C(1) = 'e') then
  begin
    Skip(2);
    Exit(tkNe);
  end;
  if (lChar = 'a') and (C(1) = 'n') and (C(2) = 'd') then
  begin
    Skip(3);
    Exit(tkAnd);
  end;
  if (lChar = 'o') and (C(1) = 'r') then
  begin
    Skip(2);
    Exit(tkOr);
  end;
  if (lChar = 's') and (C(1) = 'o') and (C(2) = 'r') and (C(3) = 't') then
  begin
    Skip(4);
    Exit(tkSort);
  end;
  if (lChar = ' ') then
  begin
    Exit(tkSpace);
  end;

  Exit(tkUnknown);
end;

function TRQL2SQL.IsDigit(const aChar: Char): Boolean;
begin
  Result := (aChar >= '0') and (aChar <= '9');
end;

function TRQL2SQL.IsLetter(const aChar: Char): Boolean;
begin
  Result := ((aChar >= 'a') and (aChar <= 'z')) or ((aChar >= 'A') and (aChar <= 'Z'));
end;

{ eq(<property>,<value>) }
procedure TRQL2SQL.ParseBinOperator(const aToken: TRQLToken; const aAST: TObjectList<TRQLCustom>);
var
  lFieldName, lFieldValue: string;
  lBinOp: TRQLFilter;
  lValueIsString: Boolean;
  lToken: TRQLToken;
begin
  EatWhiteSpaces;
  if GetToken <> tkOpenPar then
    Error('Expected "("');
  EatWhiteSpaces;
  if not MatchFieldName(lFieldName) then
    Error('Expected field');
  EatWhiteSpaces;
  if GetToken <> tkComma then
    Error('Expected comma');
  EatWhiteSpaces;

  SaveCurPos;
  lToken := GetToken;
  if lToken = tkDblQuote then
  begin
    if not MatchFieldStringValue(lFieldValue) then
      Error('Expected string value');
    if not MatchSymbol('"') then
      Error('Unclosed string');
    lValueIsString := True;
  end
  else
  begin
    BackToLastPos;
    if not MatchFieldNumericValue(lFieldValue) then
      Error('Expected numeric value');
    lValueIsString := False;
  end;
  EatWhiteSpaces;
  if GetToken <> tkClosedPar then
    Error('Expected ")"');
  lBinOp := TRQLFilter.Create;
  aAST.Add(lBinOp);
  lBinOp.Token := aToken;
  lBinOp.OpLeft := lFieldName;
  lBinOp.RightIsString := lValueIsString;
  lBinOp.OpRight := lFieldValue;
end;

function TRQL2SQL.ParseFilters: Boolean;
var
  lTk: TRQLToken;
begin
  EatWhiteSpaces;
  SaveCurPos;
  Result := True;
  lTk := GetToken;
  case lTk of
    tkEq, tkLt, tkLe, tkGt, tkGe, tkNe:
      begin
        ParseBinOperator(lTk, fAST);
      end;
    tkAnd, tkOr:
      begin
        ParseLogicOperator(lTk, fAST);
      end;
  else
    begin
      Result := False;
      BackToLastPos;
    end;
  end;
end;

procedure TRQL2SQL.ParseLogicOperator(const aToken: TRQLToken;
  const aAST: TObjectList<TRQLCustom>);
var
  lToken:
    TRQLToken;
  lLogicOp:
    TRQLLogicOperator;
begin
  EatWhiteSpaces;
  lToken := GetToken;
  if lToken <> tkOpenPar then
    Error('Expected "("');
  EatWhiteSpaces;
  lLogicOp := TRQLLogicOperator.Create(aToken);
  aAST.Add(lLogicOp);
  while True do
  begin
    EatWhiteSpaces;
    lToken := GetToken;
    case lToken of
      tkEq, tkLt, tkLe, tkGt, tkGe, tkNe:
        begin
          ParseBinOperator(lToken, lLogicOp.FilterAST);
        end;
      tkAnd, tkOr:
        begin
          ParseLogicOperator(lToken, lLogicOp.FilterAST);
        end;
      tkComma:
        begin
          // do nothing
        end;
      tkClosedPar:
        begin
          Break;
        end;
    else
      Error('Expected ")" or <Filter>');
    end;
  end;
end;

procedure TRQL2SQL.ParseSort;
var
  lToken: TRQLToken;
  lFieldName: string;
  lSort: TRQLSort;
begin
  if GetToken <> tkSort then
    Error('Expected "sort"');
  if GetToken <> tkOpenPar then
    Error('Expected "("');
  lSort := TRQLSort.Create;
  fAST.Add(lSort);

  while True do
  begin
    lToken := GetToken;
    if not(lToken in [tkPlus, tkMinus]) then
      Error('Expected "+" or "-"');
    if not MatchFieldName(lFieldName) then
      Error('Expected field name');
    lSort.Add(TRQLSort.SIGNS[lToken], lFieldName);
    SaveCurPos;
    if GetToken <> tkComma then
    begin
      BackToLastPos;
      Break;
    end;
  end;
  if GetToken <> tkClosedPar then
    Error('Expected ")"');
end;

procedure TRQL2SQL.SaveCurPos;
begin
  fSavedPos := fCurIdx;
end;

procedure TRQL2SQL.Skip(const Count: UInt8);
begin
  Inc(fCurIdx, Count);
end;

function TRQL2SQL.MatchFieldName(out lFieldName: string): Boolean;
var
  lChar: Char;
begin
  Result := True;
  lChar := C(0);
  if IsLetter(lChar) then
  begin
    lFieldName := lChar;
    while True do
    begin
      Skip(1);
      lChar := C(0);
      if IsLetter(lChar) or IsDigit(lChar) or (CharInSet(lChar, ['_'])) then
      begin
        lFieldName := lFieldName + lChar;
      end
      else
        Break;
    end;
  end
  else
    Exit(False);
end;

function TRQL2SQL.MatchFieldNumericValue(out lFieldValue: string): Boolean;
var
  lChar: Char;
begin
  Result := True;
  lChar := C(0);
  if IsDigit(lChar) then
  begin
    lFieldValue := lChar;
    while True do
    begin
      Skip(1);
      lChar := C(0);
      if IsDigit(lChar) then
      begin
        lFieldValue := lFieldValue + lChar;
      end
      else
        Break;
    end;
  end
  else
    Exit(False);
end;

function TRQL2SQL.MatchFieldStringValue(out lFieldValue: string): Boolean;
var
  lChar: Char;
begin
  Result := True;
  while True do
  begin
    lChar := C(0);
    // escape chars
    if lChar = '\' then
    begin
      if C(1) = '"' then
      begin
        lFieldValue := lFieldValue + '"';
        Skip(2);
        Continue;
      end;
    end;

    if lChar <> '"' then
    begin
      lFieldValue := lFieldValue + lChar;
    end
    else
      Break;
    Skip(1);
  end;
end;

function TRQL2SQL.MatchSymbol(const Symbol: Char): Boolean;
begin
  Result := C(0) = Symbol;
  if Result then
    Skip(1);
end;

{ TRQLFilter }

function TRQLFilter.ToSQL: string;
var
  lValue: string;
begin
  if RightIsString then
    lValue := OpRight.QuotedString('''')
  else
    lValue := OpRight;

  case Token of
    tkEq:
      begin
        Result := Format('(%s = %s)', [OpLeft, lValue]);
      end;
    tkLt:
      begin
        Result := Format('(%s < %s)', [OpLeft, lValue]);
      end;
    tkLe:
      begin
        Result := Format('(%s <= %s)', [OpLeft, lValue]);
      end;
    tkGt:
      begin
        Result := Format('(%s > %s)', [OpLeft, lValue]);
      end;
    tkGe:
      begin
        Result := Format('(%s >= %s)', [OpLeft, lValue]);
      end;
    tkNe:
      begin
        Result := Format('(%s != %s)', [OpLeft, lValue]);
      end;
  end;
end;

{ TRQLCustom }

constructor TRQLCustom.Create;
begin
  inherited;
end;

{ TRQLLogicOperator }

procedure TRQLLogicOperator.AddRQLCustom(const aRQLCustom: TRQLCustom);
begin
  fRQLFilter.Add(aRQLCustom);
end;

constructor TRQLLogicOperator.Create(const Token: TRQLToken);
begin
  inherited Create;
  fToken := Token;
  fRQLFilter := TObjectList<TRQLCustom>.Create(True);
end;

destructor TRQLLogicOperator.Destroy;
begin
  fRQLFilter.Free;
  inherited;
end;

function TRQLLogicOperator.ToSQL: string;
var
  lJoin: string;
  lRQLCustom: TRQLCustom;
  lFirst: Boolean;
begin
  case fToken of
    tkAnd:
      begin
        lJoin := ' and ';
      end;
    tkOr:
      begin
        lJoin := ' or ';
      end;
  end;

  Result := '';
  lFirst := True;
  for lRQLCustom in fRQLFilter do
  begin
    if not lFirst then
    begin
      Result := Result + lJoin;
    end;
    lFirst := False;
    Result := Result + lRQLCustom.ToSQL;
  end;
  Result := '(' + Result + ')';
end;

{ TRQLSort }

procedure TRQLSort.Add(const Sign, FieldName: string);
begin
  if (Sign <> '+') and (Sign <> '-') then
    raise Exception.Create('Invalid Sign: ' + Sign);

  fFields.Add(FieldName);
  fSigns.Add(Sign);
end;

constructor TRQLSort.Create;
begin
  inherited;
  fFields := TList<string>.Create;
  fSigns := TList<string>.Create;
end;

destructor TRQLSort.Destroy;
begin
  fFields.Free;
  fSigns.Free;
  inherited;
end;

function TRQLSort.ToSQL: string;
var
  lFieldName: string;
  I: Integer;
begin
  Result := ' ORDER BY';
  for I := 0 to fFields.Count - 1 do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + ' ' + fFields[I];
    if fSigns[I] = '+' then
      Result := Result + ' ASC'
    else
      Result := Result + ' DESC';
  end;
end;

{ TRQLWhere }

function TRQLWhere.ToSQL: string;
begin
  Result := ' where ';
end;

end.
