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

unit MVCFramework.RQL.Parser;

interface

uses
  System.Generics.Collections,
  System.Math,
  System.SysUtils,
  MVCFramework.Commons;

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
  limit(count,start,maxCount) - Returns the given range of objects from the result set
  contains(<property>,<value | expression>) - Filters for objects where the specified property's value is an array and the array contains any value that equals the provided value or satisfies the provided expression.
  in(<property>,<array-of-values>) - Filters for objects where the specified property's value is in the provided array
  out(<property>,<array-of-values>) - Filters for objects where the specified property's value is not in the provided array

  //////NOT AVAILABLES
  select(<property>,<property>,...) - Trims each object down to the set of properties defined in the arguments
  values(<property>) - Returns an array of the given property value for each object
  aggregate(<property|function>,...) - Aggregates the array, grouping by objects that are distinct for the provided properties, and then reduces the remaining other property values using the provided functions
  distinct() - Returns a result set with duplicates removed
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
  TRQLToken = (tkEq, tkLt, tkLe, tkGt, tkGe, tkNe, tkAnd, tkOr, tkSort, tkLimit, { RQL } tkAmpersand, tkEOF,
    tkOpenPar, tkClosedPar, tkOpenBracket, tkCloseBracket, tkComma, tkSemicolon, tkPlus, tkMinus, tkDblQuote,
    tkQuote, tkSpace, tkContains, tkIn, tkOut, tkUnknown, tkStarts);

  TRQLValueType = (vtInteger, vtString, vtBoolean, vtNull, vtIntegerArray, vtStringArray);

  TRQLCustom = class;

  TRQLAbstractSyntaxTree = class(TObjectList<TRQLCustom>)
  public
    constructor Create;
    function TreeContainsToken(const aToken: TRQLToken; out aFoundItem: TRQLCustom): Boolean;
  end;

  TRQLCompiler = class abstract
  private
    fMapping: TMVCFieldsMapping;
  protected
    function GetDatabaseFieldName(const RQLPropertyName: string; const UsePropertyNameIfAttributeDoesntExists: Boolean = False): string;
    function QuoteStringArray(const aStringArray: TArray<string>): TArray<string>;
    function RQLCustom2SQL(const aRQLCustom: TRQLCustom): string; virtual; abstract;
    procedure AdjustAST(const aRQLAST: TRQLAbstractSyntaxTree); virtual;
  public
    constructor Create(const Mapping: TMVCFieldsMapping); virtual;
    procedure AST2SQL(const aRQLAST: TRQLAbstractSyntaxTree; out aSQL: string); virtual;
    // Overwritten by descendant if the SQL syntaxt requires more than the simple table name
    // or if the table name contains spaces.
    function GetTableNameForSQL(const TableName: string): string; virtual;
    // Overwritten by descendant if the SQL syntaxt requires more than the simple field name
    // or if the field name contains spaces.
    function GetFieldNameForSQL(const FieldName: string): string; virtual;
    function GetParamNameForSQL(const FieldName: string): string; virtual;
    function GetPKFieldName: String;
  end;

  TRQLCompilerClass = class of TRQLCompiler;

  TRQLCustom = class abstract
  public
    Token: TRQLToken;
    constructor Create; virtual;
  end;

  TRQLCustomOperator = class abstract(TRQLCustom)

  end;

  TRQLWhere = class(TRQLCustom)
  end;

  /// <summary>
  /// "limit" function. "Start" is 0 based.
  /// </summary>
  TRQLLimit = class(TRQLCustom)
  public
    Start: Int64;
    Count: Int64;
  end;

  TRQLFilter = class(TRQLCustomOperator)
  public
    OpLeft: string;
    OpRight: string;
    OpRightArray: TArray<string>;
    RightValueType: TRQLValueType;
  end;

  TRQLLogicOperator = class(TRQLCustom)
  private
    fRQLFilter: TObjectList<TRQLCustom>;
  public
    constructor Create(const Token: TRQLToken); reintroduce;
    destructor Destroy; override;
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
    procedure Add(const Sign, FieldName: string);
    property Fields: TList<string> read fFields;
    property Signs: TList<string> read fSigns;

  const
    SIGNS_DESCR: array [tkPlus .. tkMinus] of string = ('+', '-');
  end;

  ERQLException = class(Exception)

  end;

  ERQLCompilerNotFound = class(ERQLException)

  end;

  TRQL2SQL = class
  private
    fCurIdx: Integer;
    fInput: string;
    fAST: TRQLAbstractSyntaxTree;
    fSavedPos: Integer;
    fInputLength: Integer;
    fCurr: Char;
    fCurrToken: TRQLToken;
  protected
    /// /// RQL Sections
    function ParseFilters: Boolean;
    function ParseSort: Boolean;
    function ParseLimit(const MaxRecordCount: Integer): Boolean;
    /// ///RQL functions
    procedure ParseBinOperator(const aToken: TRQLToken; const aAST: TObjectList<TRQLCustom>);
    procedure ParseLogicOperator(const aToken: TRQLToken; const aAST: TObjectList<TRQLCustom>);
    procedure ParseSortLimit(const Required: Boolean; const MaxRecordCount: Integer);
    /// //Parser utils
    function MatchFieldName(out lFieldName: string): Boolean;
    function MatchFieldStringValue(out lFieldValue: string): Boolean;
    function MatchFieldNumericValue(out lFieldValue: string): Boolean;
    function MatchFieldArrayValue(out lFieldValue: string): Boolean;
    function MatchFieldBooleanValue(out lFieldValue: string): Boolean;
    function MatchFieldNullValue(out lFieldValue: string): Boolean;
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
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute(
      const RQL: string;
      out SQL: string;
      const RQLCompiler: TRQLCompiler;
      const SetArtificialLimit: Boolean = true;
      const UseFilterOnly: Boolean = False;
      const MaxRecordCount: Int32 = TMVCConstants.MAX_RECORD_COUNT);
  end;

  TRQLCompilerRegistry = class sealed
  private
    class var sInstance: TRQLCompilerRegistry;

  class var
    _Lock: TObject;
    fCompilers: TDictionary<string, TRQLCompilerClass>;
  protected
    constructor Create;
  public
    destructor Destroy; override;
    class function Instance: TRQLCompilerRegistry;
    class destructor Destroy;
    class constructor Create;
    procedure RegisterCompiler(
      const aBackend: string;
      const aRQLBackendClass: TRQLCompilerClass);
    procedure UnRegisterCompiler(const aBackend: string);
    function GetCompiler(const aBackend: string): TRQLCompilerClass;
    function RegisteredCompilers: TArray<string>;
  end;

implementation


uses
  System.Character,
  System.StrUtils;

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
  fCurr := fInput.Chars[fCurIdx];
end;

procedure TRQL2SQL.CheckEOF(const Token: TRQLToken);
begin
  if Token = tkEOF then
    Error('Unexpected end of expression');
end;

constructor TRQL2SQL.Create;
begin
  inherited Create;
  fAST := TRQLAbstractSyntaxTree.Create;
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
  while true do
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
var
  I: Integer;
  lMsg: string;
begin
  lMsg := '';
  for I := 0 to 4 do
  begin
    lMsg := lMsg + IfThen(C(I) = #0, '', C(I));
  end;
  if lMsg.Trim.IsEmpty then
    lMsg := '<EOF>';
  raise ERQLException.CreateFmt('[Error] %s (column %d - found %s)', [message, fCurIdx, lMsg]);
end;

procedure TRQL2SQL.Execute(
  const RQL: string;
  out SQL: string;
  const RQLCompiler: TRQLCompiler;
  const SetArtificialLimit: Boolean;
  const UseFilterOnly: Boolean;
  const MaxRecordCount: Int32);
var
  lLimit: TRQLLimit;
  lRQLItem: TRQLCustom;
begin
  fAST.Clear;
  fCurIdx := 0;
  fCurrToken := tkUnknown;
  fInput := RQL.Trim;
  fInputLength := Length(RQL);
  {
    filters&sort&limit
    filters&sort
    filters&limit
    &sort&limit
    sort
    limit
  }
  EatWhiteSpaces;
  if ParseFilters then
  begin
    fAST.Insert(0, TRQLWhere.Create);
    if GetToken = tkSemicolon then
    begin
      ParseSortLimit(true, MaxRecordCount);
    end;
  end
  else
  begin
    ParseSortLimit(False, MaxRecordCount);
  end;
  EatWhiteSpaces;
  if GetToken <> tkEOF then
    Error('Expected EOF');

  // add artificial limit
  if SetArtificialLimit and (MaxRecordCount > -1) and (not fAST.TreeContainsToken(tkLimit, lRQLItem)) then
  begin
    lLimit := TRQLLimit.Create;
    fAST.Add(lLimit);
    lLimit.Token := tkLimit;
    lLimit.Start := 0;
    lLimit.Count := MaxRecordCount;
  end;


//  if fAST.TreeContainsToken(tkLimit, lRQLItem) then
//  begin
//    if (TRQLLimit(lRQLItem).Count > 0) and (not fAST.TreeContainsToken(tkSort, lRQLItem)) then
//    begin
//      lSort := TRQLSort.Create;
//      lSort.Add('+', '1');
//      fAST.Insert(fAST.Count-1, lSort);
//    end
//    else
//    begin
//      fAST.Remove(lRQLItem);
//      fAST.TreeContainsToken(tk)
//      fAST.Add(lAlwaysFalse);
//      lAlwaysFalse.OpLeft := '1';
//      lAlwaysFalse.OpRight := '2';
//      lAlwaysFalse.RightValueType := vtInteger;
//      lAlwaysFalse.Token := tkEq;
//    end;
//  end;



  if UseFilterOnly then
  {If we need only the filter part, remove sort and limit tokens}
  begin
    if fAST.TreeContainsToken(tkSort, lRQLItem) then
    begin
      fAST.Remove(lRQLItem);
    end;
    if fAST.TreeContainsToken(tkLimit, lRQLItem) then
    begin
      fAST.Remove(lRQLItem);
    end;
  end;

  // Emit code from AST using backend
  RQLCompiler.AST2SQL(fAST, SQL);
end;

function TRQL2SQL.GetToken: TRQLToken;
var
  lChar: Char;
begin
  lChar := C(0);
  if (lChar = #0) then
  begin
    fCurrToken := tkEOF;
    Exit(fCurrToken);
  end;
  if (lChar = ',') then
  begin
    Skip(1);
    fCurrToken := tkComma;
    Exit(fCurrToken);
  end;
  if (lChar = ';') then
  begin
    Skip(1);
    fCurrToken := tkSemicolon;
    Exit(fCurrToken);
  end;
  if (lChar = '+') then
  begin
    Skip(1);
    fCurrToken := tkPlus;
    Exit(fCurrToken);
  end;
  if (lChar = '"') then
  begin
    Skip(1);
    fCurrToken := tkDblQuote;
    Exit(fCurrToken);
  end;
  if (lChar = '''') then
  begin
    Skip(1);
    fCurrToken := tkQuote;
    Exit(fCurrToken);
  end;
  if (lChar = '-') then
  begin
    Skip(1);
    fCurrToken := tkMinus;
    Exit(fCurrToken);
  end;
  if (lChar = '&') then
  begin
    Skip(1);
    fCurrToken := tkAmpersand;
    Exit(fCurrToken);
  end;
  if (lChar = '(') then
  begin
    Skip(1);
    fCurrToken := tkOpenPar;
    Exit(fCurrToken);
  end;
  if (lChar = ')') then
  begin
    Skip(1);
    fCurrToken := tkClosedPar;
    Exit(fCurrToken);
  end;
  if (lChar = '[') then
  begin
    Skip(1);
    fCurrToken := tkOpenBracket;
    Exit(fCurrToken);
  end;
  if (lChar = ']') then
  begin
    Skip(1);
    fCurrToken := tkCloseBracket;
    Exit(fCurrToken);
  end;
  if (lChar = 'e') and (C(1) = 'q') then
  begin
    Skip(2);
    fCurrToken := tkEq;
    Exit(fCurrToken);
  end;
  if (lChar = 'l') and (C(1) = 't') then
  begin
    Skip(2);
    fCurrToken := tkLt;
    Exit(fCurrToken);
  end;
  if (lChar = 'l') and (C(1) = 'e') then
  begin
    Skip(2);
    fCurrToken := tkLe;
    Exit(fCurrToken);
  end;
  if (lChar = 'g') and (C(1) = 't') then
  begin
    Skip(2);
    fCurrToken := tkGt;
    Exit(fCurrToken);
  end;
  if (lChar = 'g') and (C(1) = 'e') then
  begin
    Skip(2);
    fCurrToken := tkGe;
    Exit(fCurrToken);
  end;
  if (lChar = 'n') and (C(1) = 'e') then
  begin
    Skip(2);
    fCurrToken := tkNe;
    Exit(fCurrToken);
  end;
  if (lChar = 'a') and (C(1) = 'n') and (C(2) = 'd') then
  begin
    Skip(3);
    fCurrToken := tkAnd;
    Exit(fCurrToken);
  end;
  if (lChar = 'o') and (C(1) = 'r') then
  begin
    Skip(2);
    fCurrToken := tkOr;
    Exit(fCurrToken);
  end;
  if (lChar = 's') and (C(1) = 'o') and (C(2) = 'r') and (C(3) = 't') then
  begin
    Skip(4);
    fCurrToken := tkSort;
    Exit(fCurrToken);
  end;
  if (lChar = 'l') and (C(1) = 'i') and (C(2) = 'm') and (C(3) = 'i') and (C(4) = 't') then
  begin
    Skip(5);
    fCurrToken := tkLimit;
    Exit(fCurrToken);
  end;
  if (lChar = 'c') and (C(1) = 'o') and (C(2) = 'n') and (C(3) = 't') and (C(4) = 'a') and (C(5) = 'i') and
    (C(6) = 'n') and (C(7) = 's') then
  begin
    Skip(8);
    fCurrToken := tkContains;
    Exit(fCurrToken);
  end;
  if (lChar = 's') and (C(1) = 't') and (C(2) = 'a') and (C(3) = 'r') and (C(4) = 't') and (C(5) = 's') then
  begin
    Skip(6);
    fCurrToken := tkStarts;
    Exit(fCurrToken);
  end;
  if (lChar = 'i') and (C(1) = 'n') then
  begin
    Skip(2);
    fCurrToken := tkIn;
    Exit(fCurrToken);
  end;
  if (lChar = 'o') and (C(1) = 'u') and (C(2) = 't') then
  begin
    Skip(3);
    fCurrToken := tkOut;
    Exit(fCurrToken);
  end;
  if (lChar = ' ') then
  begin
    fCurrToken := tkSpace;
    Exit(fCurrToken);
  end;

  fCurrToken := tkUnknown;
  Exit(fCurrToken);
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
  lValueType: TRQLValueType;
  lToken: TRQLToken;
  lList: TList<string>;
  lArrayValue: TArray<string>;
begin
  lValueType := TRQLValueType.vtInteger; // default
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
    lValueType := vtString;
  end
  else if (aToken in [tkIn, tkOut]) and (lToken = tkOpenBracket) then
  begin
    lList := TList<string>.Create;
    try
      // if not MatchFieldArrayValue(lFieldValue) then
      // Error('Expected array value');
      EatWhiteSpaces;
      if C(0) = '"' then
      begin
        Skip(1);
        lValueType := vtStringArray;
        while MatchFieldStringValue(lFieldValue) do
        begin
          lList.Add(lFieldValue);
          if not MatchSymbol('"') then
            Error('Expected ''"''');
          EatWhiteSpaces;
          if not MatchSymbol(',') then
            Break;
          EatWhiteSpaces;
          if GetToken <> tkDblQuote then
            Error('Expected ["]');
        end;
      end
      else
      begin
        lValueType := vtIntegerArray;
        while MatchFieldNumericValue(lFieldValue) do
        begin
          lList.Add(lFieldValue);
          EatWhiteSpaces;
          if not MatchSymbol(',') then
            Break;
          EatWhiteSpaces;
        end;
      end;
      if not MatchSymbol(']') then
        Error('Unclosed bracket');
      lArrayValue := lList.ToArray;
    finally
      lList.Free;
    end;
  end
  else
  begin
    BackToLastPos;

    if MatchFieldBooleanValue(lFieldValue) then
      lValueType := vtBoolean
    else if MatchFieldNullValue(lFieldValue) then
      lValueType := vtNull
    else if MatchFieldNumericValue(lFieldValue) then
      lValueType := vtInteger
    else
      Error('Expected numeric, boolean or null value');
  end;
  EatWhiteSpaces;
  if GetToken <> tkClosedPar then
    Error('Expected ")"');
  lBinOp := TRQLFilter.Create;
  aAST.Add(lBinOp);
  lBinOp.Token := aToken;
  lBinOp.OpLeft := lFieldName;
  lBinOp.RightValueType := lValueType;
  if lBinOp.RightValueType in [vtIntegerArray, vtStringArray] then
    lBinOp.OpRightArray := lArrayValue
  else
    lBinOp.OpRight := lFieldValue;
end;

function TRQL2SQL.ParseFilters: Boolean;
var
  lTk: TRQLToken;
begin
  EatWhiteSpaces;
  SaveCurPos;
  Result := true;
  lTk := GetToken;
  case lTk of
    tkEq, tkLt, tkLe, tkGt, tkGe, tkNe, tkContains, tkStarts, tkIn, tkOut:
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

function TRQL2SQL.ParseLimit(const MaxRecordCount: Integer): Boolean;
var
  lStart: string;
  lCount: string;
  lRQLLimit: TRQLLimit;
begin
  SaveCurPos;
  if GetToken <> tkLimit then
  begin
    BackToLastPos;
    Exit(False);
  end;
  if GetToken <> tkOpenPar then
    Error('Expected "("');

  if not MatchFieldNumericValue(lStart) then
    Error('Expected number');

  if GetToken <> tkComma then
    Error('Expected comma');

  if not MatchFieldNumericValue(lCount) then
    Error('Expected number');

  if GetToken <> tkClosedPar then
    Error('Expected ")"');

  lRQLLimit := TRQLLimit.Create;
  fAST.Add(lRQLLimit);
  lRQLLimit.Token := tkLimit;
  lRQLLimit.Start := StrToInt64(lStart); // XE7 compat
  if MaxRecordCount > -1 then
  begin
    lRQLLimit.Count := Min(StrToInt64(lCount), MaxRecordCount);
  end
  else
  begin
    lRQLLimit.Count := StrToInt64(lCount);
  end;
  Result := true;
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
  while true do
  begin
    EatWhiteSpaces;
    lToken := GetToken;
    case lToken of
      tkEq, tkLt, tkLe, tkGt, tkGe, tkNe, tkContains, tkStarts, tkIn, tkOut:
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

function TRQL2SQL.ParseSort: Boolean;
var
  lToken: TRQLToken;
  lFieldName: string;
  lSort: TRQLSort;
begin
  Result := true;
  SaveCurPos;
  if GetToken <> tkSort then
  begin
    BackToLastPos;
    Exit(False);
  end;

  if GetToken <> tkOpenPar then
    Error('Expected "("');
  lSort := TRQLSort.Create;
  fAST.Add(lSort);
  lSort.Token := tkSort;

  while true do
  begin
    EatWhiteSpaces;
    lToken := GetToken;
    if not(lToken in [tkPlus, tkMinus]) then
      Error('Expected "+" or "-"');
    if not MatchFieldName(lFieldName) then
      Error('Expected field name');
    lSort.Add(TRQLSort.SIGNS_DESCR[lToken], lFieldName);
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

procedure TRQL2SQL.ParseSortLimit(const Required: Boolean; const MaxRecordCount: Integer);
var
  lFoundSort: Boolean;
  lFoundLimit: Boolean;
begin
  EatWhiteSpaces;
  lFoundSort := ParseSort;
  EatWhiteSpaces;
  SaveCurPos;
  if GetToken <> tkSemicolon then
  begin
    BackToLastPos;
  end;
  lFoundLimit := ParseLimit(MaxRecordCount);
  if Required and (not(lFoundSort or lFoundLimit)) then
    Error('Expected "sort" and/or "limit"');
end;

procedure TRQL2SQL.SaveCurPos;
begin
  fSavedPos := fCurIdx;
end;

procedure TRQL2SQL.Skip(const Count: UInt8);
begin
  Inc(fCurIdx, Count);
end;

function TRQL2SQL.MatchFieldArrayValue(out lFieldValue: string): Boolean;
var
  lStrFieldValue: string;
  lNumFieldValue: string;
  lIntValue: string;
begin
  EatWhiteSpaces;
  if GetToken = tkDblQuote then
  begin
    while MatchFieldStringValue(lStrFieldValue) do
    begin
      MatchSymbol('"');
      EatWhiteSpaces;
      if GetToken = tkComma then
      begin
        EatWhiteSpaces;
        MatchSymbol('"');
        Continue;
      end
      else
      begin
        Skip(1);
        if GetToken <> tkDblQuote then
        begin
          Exit(true);
        end;
      end;
    end;
  end
  else
  begin
    MatchFieldNumericValue(lIntValue);
  end;

  while MatchFieldNumericValue(lNumFieldValue) do
  begin
    EatWhiteSpaces;
    if MatchSymbol(',') then
    begin
      Continue;
    end
    else
    begin
      Exit(true);
    end;
  end;
  Result := False;
end;

function TRQL2SQL.MatchFieldBooleanValue(out lFieldValue: string): Boolean;
var
  lChar: Char;
begin
  lFieldValue := '';
  lChar := C(0).ToLower;

  if (lChar = 't') and (C(1).ToLower = 'r') and (C(2).ToLower = 'u') and (C(3).ToLower = 'e') then
  begin
    Skip(4);
    Result := true;
    lFieldValue := 'true';
  end
  else if (lChar = 'f') and (C(1).ToLower = 'a') and (C(2).ToLower = 'l') and (C(3).ToLower = 's') and
    (C(4).ToLower = 'e') then
  begin
    Skip(5);
    Result := true;
    lFieldValue := 'false';
  end
  else
    Exit(False)
end;

function TRQL2SQL.MatchFieldName(out lFieldName: string): Boolean;
var
  lChar: Char;
begin
  Result := true;
  lChar := C(0);
  if IsLetter(lChar) then
  begin
    lFieldName := lChar;
    while true do
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

function TRQL2SQL.MatchFieldNullValue(out lFieldValue: string): Boolean;
var
  lChar: Char;
begin
  lFieldValue := '';
  lChar := C(0).ToLower;

  if (lChar = 'n') and (C(1).ToLower = 'u') and (C(2).ToLower = 'l') and (C(3).ToLower = 'l') then
  begin
    Skip(4);
    Result := true;
    lFieldValue := 'NULL';
  end
  else
    Exit(False)
end;

function TRQL2SQL.MatchFieldNumericValue(out lFieldValue: string): Boolean;
var
  lChar: Char;
begin
  Result := true;
  lFieldValue := '';
  lChar := C(0);

  if CharInSet(lChar, ['+', '-']) then
  begin
    lFieldValue := lChar;
    Skip(1);
    lChar := C(0);
  end;

  if IsDigit(lChar) then
  begin
    lFieldValue := lFieldValue + lChar;
    while true do
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
  Result := true;
  while true do
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

    SaveCurPos;
    CheckEOF(GetToken);
    BackToLastPos;

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

{ TRQLCustom }

constructor TRQLCustom.Create;
begin
  inherited;
  Token := tkUnknown;
end;

{ TRQLLogicOperator }

procedure TRQLLogicOperator.AddRQLCustom(const aRQLCustom: TRQLCustom);
begin
  fRQLFilter.Add(aRQLCustom);
end;

constructor TRQLLogicOperator.Create(const Token: TRQLToken);
begin
  inherited Create;
  Self.Token := Token;
  fRQLFilter := TObjectList<TRQLCustom>.Create(true);
end;

destructor TRQLLogicOperator.Destroy;
begin
  fRQLFilter.Free;
  inherited;
end;

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

constructor TRQLCompilerRegistry.Create;
begin
  inherited;
  fCompilers := TDictionary<string, TRQLCompilerClass>.Create;
end;

destructor TRQLCompilerRegistry.Destroy;
begin
  fCompilers.Free;
  inherited;
end;

class constructor TRQLCompilerRegistry.Create;
begin
  _Lock := TObject.Create;
end;

class destructor TRQLCompilerRegistry.Destroy;
begin
  _Lock.Free;
  sInstance.Free;
end;

function TRQLCompilerRegistry.GetCompiler(const aBackend: string): TRQLCompilerClass;
begin
  if not fCompilers.TryGetValue(aBackend, Result) then
  begin
    raise ERQLCompilerNotFound.Create('RQL Compiler not found [HINT] Include unit "MVCFramework.RQL.AST2<yourdbengine>.pas"');
  end;
end;

class
  function TRQLCompilerRegistry.Instance: TRQLCompilerRegistry;
begin
  if not Assigned(sInstance) then
  begin
    TMonitor.Enter(_Lock);
    try
      if not Assigned(sInstance) then
      begin
        sInstance := TRQLCompilerRegistry.Create;
      end;
    finally
      TMonitor.Exit(_Lock);
    end;
  end;
  Result := sInstance;
end;

procedure TRQLCompilerRegistry.RegisterCompiler(const aBackend: string; const aRQLBackendClass: TRQLCompilerClass);
begin
  fCompilers.AddOrSetValue(aBackend, aRQLBackendClass);
end;

function TRQLCompilerRegistry.RegisteredCompilers: TArray<string>;
begin
  Result := fCompilers.Keys.ToArray;
end;

procedure TRQLCompilerRegistry.UnRegisterCompiler(const aBackend: string);
begin
  fCompilers.Remove(aBackend);
end;

{ TRQLCompiler }

procedure TRQLCompiler.AdjustAST(const aRQLAST: TRQLAbstractSyntaxTree);
begin
  //do nothing
end;

procedure TRQLCompiler.AST2SQL(const aRQLAST: TRQLAbstractSyntaxTree;
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

  AdjustAST(aRQLAST);
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

constructor TRQLCompiler.Create(const Mapping: TMVCFieldsMapping);
begin
  inherited Create;
  fMapping := Mapping;
end;

function TRQLCompiler.GetDatabaseFieldName(
  const RQLPropertyName: string; const UsePropertyNameIfAttributeDoesntExists: Boolean): string;
var
  lField: TMVCFieldMap;
  lRQLProperty: string;
begin

  if Length(fMapping) = 0 then
  begin
    { If there isn't a mapping, then just pass the RQLProperty as DataBaseFieldName }
    Result := RQLPropertyName;
    Exit;
  end;

  lRQLProperty := RQLPropertyName.ToLower;
  for lField in fMapping do
  begin
    if lField.InstanceFieldName = lRQLProperty then
      Exit(GetFieldNameForSQL(lField.DatabaseFieldName));
  end;

  //if no propert foundwith this name, let's look in the aliases
  for lField in fMapping do
  begin
    if SameText(lField.Alias, lRQLProperty) then
      Exit(GetFieldNameForSQL(lField.DatabaseFieldName));
  end;


  { TODO -oDanieleT -cGeneral : Here we should consider also MVCNameAs attribute to find the name }
  if UsePropertyNameIfAttributeDoesntExists then
    Exit(GetFieldNameForSQL(RQLPropertyName))
  else
    raise ERQLException.CreateFmt('Property %s does not exist or is transient and cannot be used in RQL',
      [RQLPropertyName]);
end;

function TRQLCompiler.GetFieldNameForSQL(const FieldName: string): string;
begin
  if FieldName.Contains(' ') and (FieldName.Chars[0] <> '"') then
  begin
    Result := FieldName.QuotedString('"');
  end
  else
  begin
    Result := FieldName;
  end;
end;

function TRQLCompiler.GetParamNameForSQL(const FieldName: string): string;
begin
  Result := FieldName.Replace(' ', '_', [rfReplaceAll]);
end;

function TRQLCompiler.GetPKFieldName: String;
begin
  Result := fMapping[0].InstanceFieldName;
end;

function TRQLCompiler.GetTableNameForSQL(const TableName: string): string;
begin
  if TableName.Contains(' ') then
  begin
    Result := TableName.QuotedString('"');
  end
  else
  begin
    Result := TableName;
  end;
end;

function TRQLCompiler.QuoteStringArray(const aStringArray: TArray<string>): TArray<string>;
var
  lValue: string;
begin
  SetLength(Result, 0);
  for lValue in aStringArray do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[high(Result)] := lValue.QuotedString('''');
  end;
end;

{ TRQLAbstractSyntaxTree }

constructor TRQLAbstractSyntaxTree.Create;
begin
  inherited Create(true);
end;

function TRQLAbstractSyntaxTree.TreeContainsToken(
  const aToken: TRQLToken; out aFoundItem: TRQLCustom): Boolean;
var
  lItem: TRQLCustom;
begin
  Result := False;
  for lItem in Self do
  begin
    if lItem.Token = aToken then
    begin
      aFoundItem := lItem;
      Exit(true);
    end;
  end;
end;

end.
