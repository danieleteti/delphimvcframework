// ***************************************************************************
//
// Delphi Expression Evaluator
//
// Copyright (c) 2024-2025 Daniele Teti
//
// https://github.com/danieleteti/delphi-expressions-evaluator
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
// *************************************************************************** }

unit ExprEvaluator;

interface

{$I dmvcframework.inc}

uses
  System.SysUtils, System.Variants, System.Math, System.Generics.Collections,
  System.Masks;

type
  TFuncHandler = reference to function(const Args: array of Variant): Variant;
  TExternalVariableResolver = reference to function(const VarName: string; out Value: Variant): Boolean;

  /// <summary>
  /// Interface for expression evaluation without manual memory management
  /// </summary>
  IExprEvaluator = interface
    ['{A5B9C8D7-E6F4-4A2B-9C1D-8E7F6A5B4C3D}']
    /// <summary>
    /// Register a custom function
    /// </summary>
    procedure RegisterFunction(const Name: string; Handler: TFuncHandler);

    /// <summary>
    /// Evaluate a mathematical/logical expression
    /// </summary>
    function Evaluate(const Expr: string): Variant;

    /// <summary>
    /// Set a variable value
    /// </summary>
    procedure SetVar(const Name: string; Value: Variant);

    /// <summary>
    /// Get a variable value
    /// </summary>
    function GetVar(const Name: string): Variant;

    /// <summary>
    /// Convert a Variant to string using consistent format settings (dot as decimal separator)
    /// </summary>
    function VariantToString(const Value: Variant): string;

    /// <summary>
    /// Set a callback to resolve variables from external sources (e.g., template engines).
    /// The resolver is called when a variable is not found in the internal dictionary.
    /// </summary>
    procedure SetOnResolveExternalVariable(const Resolver: TExternalVariableResolver);
  end;

  TExprEvaluator = class(TInterfacedObject, IExprEvaluator)
  private
    FFunctions: TDictionary<string, TFuncHandler>;
    FVariables: TDictionary<string, Variant>;
    FInput: string;
    FPos: Integer;
    FFormatSettings: TFormatSettings;
    FSkipEvaluation: Boolean;  // When true, parse but don't execute functions
    FOnResolveExternalVariable: TExternalVariableResolver;

    function ParseIfExpression: Variant;
    function ParseLogical: Variant;
    function ParseRelational: Variant;
    function ParseAdditive: Variant;
    function ParseMultiplicative: Variant;
    function ParseFactor: Variant;
    function ParsePrimary: Variant;
    function ParseString: string;
    function CurrentChar: Char;
    procedure NextChar;
    procedure SkipWhitespace;
    function IsDigit(c: Char): Boolean;
    function IsAlpha(c: Char): Boolean;
    function ParseNumber: string;
    function ParseIdentifier: string;
    function CallFunction(const FuncName: string; Args: TArray<Variant>): Variant;
    function GetVariable(const VarName: string): Variant;
    procedure SetVariable(const VarName: string; Value: Variant);
    function ParseAssignment: Variant;
    function IsKeywordAtPosition(const Keyword: string): Boolean;
    function ConsumeKeyword(const Keyword: string): Boolean;
    // Helper methods for argument validation
    procedure ValidateArgCount(const FuncName: string; Expected, Actual: Integer);
    procedure ValidateNumeric(const FuncName: string; const Arg: Variant);
    // Helper methods for QuickSort
    procedure QuickSortNumeric(var Arr: TArray<Double>; Low, High: Integer);
    procedure QuickSortString(var Arr: TArray<string>; Low, High: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterFunction(const Name: string; Handler: TFuncHandler);
    function Evaluate(const Expr: string): Variant;
    procedure SetVar(const Name: string; Value: Variant);
    function GetVar(const Name: string): Variant;
    function VariantToString(const Value: Variant): string;
    procedure SetOnResolveExternalVariable(const Resolver: TExternalVariableResolver);
  end;

/// <summary>
/// Create a new expression evaluator instance
/// </summary>
function CreateExprEvaluator: IExprEvaluator;

implementation

function CreateExprEvaluator: IExprEvaluator;
begin
  Result := TExprEvaluator.Create;
end;

{ TExprEvaluator }

constructor TExprEvaluator.Create;
begin
  inherited;
  FFunctions := TDictionary<string, TFuncHandler>.Create;
  FVariables := TDictionary<string, Variant>.Create;
  FSkipEvaluation := False;
  FOnResolveExternalVariable := nil;

  // Create local FormatSettings with dot as decimal separator
  FFormatSettings := TFormatSettings.Create('en-US');
  FFormatSettings.DecimalSeparator := '.';

  // Register standard functions
  RegisterFunction('sqrt', function(const Args: array of Variant): Variant
    begin
      if Length(Args) <> 1 then
        raise Exception.Create('sqrt requires 1 argument');
      if not VarIsNumeric(Args[0]) then
        raise Exception.Create('sqrt requires a numeric argument');
      Result := Sqrt(Args[0]);
    end);

  RegisterFunction('logn', function(const Args: array of Variant): Variant
    begin
      if Length(Args) <> 1 then
        raise Exception.Create('logn requires 1 argument');
      if not VarIsNumeric(Args[0]) then
        raise Exception.Create('logn requires a numeric argument');
      Result := Ln(Args[0]);
    end);

  RegisterFunction('log', function(const Args: array of Variant): Variant
    begin
      if Length(Args) <> 1 then
        raise Exception.Create('log requires 1 argument');
      if not VarIsNumeric(Args[0]) then
        raise Exception.Create('log requires a numeric argument');
      Result := System.Math.Log10(Args[0]);
    end);

  RegisterFunction('round', function(const Args: array of Variant): Variant
    begin
      if Length(Args) <> 2 then
        raise Exception.Create('Round requires 2 arguments');
      if not VarIsNumeric(Args[0]) then
        raise Exception.Create('Round requires first argument to be numeric');
      if not VarIsNumeric(Args[1]) then
        raise Exception.Create('Round requires second argument to be numeric');
      Result := System.Math.RoundTo(Args[0], Args[1]);
    end);

  RegisterFunction('abs', function(const Args: array of Variant): Variant
    begin
      ValidateArgCount('abs', 1, Length(Args));
      ValidateNumeric('abs', Args[0]);
      Result := Abs(Args[0]);
    end);

  RegisterFunction('floor', function(const Args: array of Variant): Variant
    begin
      ValidateArgCount('floor', 1, Length(Args));
      ValidateNumeric('floor', Args[0]);
      Result := Floor(Args[0]);
    end);

  RegisterFunction('ceil', function(const Args: array of Variant): Variant
    begin
      ValidateArgCount('ceil', 1, Length(Args));
      ValidateNumeric('ceil', Args[0]);
      Result := Ceil(Args[0]);
    end);

  RegisterFunction('power', function(const Args: array of Variant): Variant
    begin
      ValidateArgCount('power', 2, Length(Args));
      ValidateNumeric('power', Args[0]);
      ValidateNumeric('power', Args[1]);
      Result := Power(Args[0], Args[1]);
    end);

  RegisterFunction('contains', function(const Args: array of Variant): Variant
    begin
      if Length(Args) <> 2 then
        raise Exception.Create('Contains requires 2 arguments');
      if VarIsNull(Args[0]) or VarIsNull(Args[1]) then
        raise Exception.Create('Contains requires non-null arguments');
      {$IF Defined(FLORENCEORBETTER)}
      Result := String(Args[1]).Contains(String(Args[0]), True);
      {$ELSE}
      Result := String(Args[1]).ToLower.Contains(String(Args[0]).ToLower);
      {$ENDIF}
    end);

  RegisterFunction('ToString', function(const Args: array of Variant): Variant
    begin
      if Length(Args) <> 1 then
        raise Exception.Create('ToString requires 1 argument');
      Result := VarToStr(Args[0]);
    end);

  // Add ToInteger function
  RegisterFunction('ToInteger', function(const Args: array of Variant): Variant
    begin
      if Length(Args) <> 1 then
        raise Exception.Create('ToInteger requires 1 argument');
      Result := StrToInt(VarToStr(Args[0]));
    end);

  // Add ToFloat function
  RegisterFunction('ToFloat', function(const Args: array of Variant): Variant
    begin
      if Length(Args) <> 1 then
        raise Exception.Create('ToFloat requires 1 argument');
      Result := StrToFloat(VarToStr(Args[0]), FFormatSettings);
    end);

  // Add Min function (2 or more numeric arguments)
  RegisterFunction('Min', function(const Args: array of Variant): Variant
    var
      I: Integer;
    begin
      if Length(Args) < 2 then
        raise Exception.Create('Min requires at least 2 arguments');

      // Validate all arguments are numeric
      for I := 0 to High(Args) do
      begin
        if not (VarIsNumeric(Args[I])) then
          raise Exception.Create('Min requires all arguments to be numeric');
      end;

      Result := Args[0];
      for I := 1 to High(Args) do
      begin
        if Args[I] < Result then
          Result := Args[I];
      end;
    end);

  // Add Max function (2 or more numeric arguments)
  RegisterFunction('Max', function(const Args: array of Variant): Variant
    var
      I: Integer;
    begin
      if Length(Args) < 2 then
        raise Exception.Create('Max requires at least 2 arguments');

      // Validate all arguments are numeric
      for I := 0 to High(Args) do
      begin
        if not (VarIsNumeric(Args[I])) then
          raise Exception.Create('Max requires all arguments to be numeric');
      end;

      Result := Args[0];
      for I := 1 to High(Args) do
      begin
        if Args[I] > Result then
          Result := Args[I];
      end;
    end);

  // Add Sort function (2 or more homogeneous arguments)
  RegisterFunction('Sort', function(const Args: array of Variant): Variant
    var
      I: Integer;
      IsNumeric: Boolean;
      NumericValues: TArray<Double>;
      StringValues: TArray<string>;
      ResultStr: string;
      LocalFS: TFormatSettings;
    begin
      if Length(Args) < 2 then
        raise Exception.Create('Sort requires at least 2 arguments');

      // Use local format settings with dot as decimal separator
      LocalFS := FFormatSettings;

      // Determine if arguments are numeric or string by checking first argument
      IsNumeric := VarIsNumeric(Args[0]);

      // Validate homogeneous types and prepare arrays
      if IsNumeric then
      begin
        SetLength(NumericValues, Length(Args));
        for I := 0 to High(Args) do
        begin
          if not VarIsNumeric(Args[I]) then
            raise Exception.Create('Sort requires all arguments to be of the same type (all numeric or all strings)');
          NumericValues[I] := Args[I];
        end;

        // QuickSort for numeric values
        QuickSortNumeric(NumericValues, 0, High(NumericValues));

        // Build result string using dot as decimal separator
        ResultStr := '';
        for I := 0 to High(NumericValues) do
        begin
          if I > 0 then
            ResultStr := ResultStr + ',';
          // Use FloatToStrF with LocalFS to ensure consistent decimal separator
          ResultStr := ResultStr + FloatToStrF(NumericValues[I], ffGeneral, 15, 0, LocalFS);
        end;
      end
      else
      begin
        SetLength(StringValues, Length(Args));
        for I := 0 to High(Args) do
        begin
          if VarIsNumeric(Args[I]) then
            raise Exception.Create('Sort requires all arguments to be of the same type (all numeric or all strings)');
          StringValues[I] := VarToStr(Args[I]);
        end;

        // QuickSort for string values
        QuickSortString(StringValues, 0, High(StringValues));

        // Build result string
        ResultStr := '';
        for I := 0 to High(StringValues) do
        begin
          if I > 0 then
            ResultStr := ResultStr + ',';
          ResultStr := ResultStr + StringValues[I];
        end;
      end;

      Result := ResultStr;
    end);

  // String manipulation functions
  RegisterFunction('Length', function(const Args: array of Variant): Variant
    begin
      if Length(Args) <> 1 then
        raise Exception.Create('Length requires 1 argument');
      Result := Length(VarToStr(Args[0]));
    end);

  RegisterFunction('Upper', function(const Args: array of Variant): Variant
    begin
      if Length(Args) <> 1 then
        raise Exception.Create('Upper requires 1 argument');
      Result := UpperCase(VarToStr(Args[0]));
    end);

  RegisterFunction('Lower', function(const Args: array of Variant): Variant
    begin
      if Length(Args) <> 1 then
        raise Exception.Create('Lower requires 1 argument');
      Result := LowerCase(VarToStr(Args[0]));
    end);

  RegisterFunction('Trim', function(const Args: array of Variant): Variant
    begin
      if Length(Args) <> 1 then
        raise Exception.Create('Trim requires 1 argument');
      Result := Trim(VarToStr(Args[0]));
    end);

  RegisterFunction('Left', function(const Args: array of Variant): Variant
    var
      Str: string;
      Count: Integer;
    begin
      if Length(Args) <> 2 then
        raise Exception.Create('Left requires 2 arguments (string, count)');
      Str := VarToStr(Args[0]);
      Count := Args[1];
      if Count < 0 then
        raise Exception.Create('Left count must be non-negative');
      if Count > Length(Str) then
        Count := Length(Str);
      Result := Copy(Str, 1, Count);
    end);

  RegisterFunction('Right', function(const Args: array of Variant): Variant
    var
      Str: string;
      Count: Integer;
    begin
      if Length(Args) <> 2 then
        raise Exception.Create('Right requires 2 arguments (string, count)');
      Str := VarToStr(Args[0]);
      Count := Args[1];
      if Count < 0 then
        raise Exception.Create('Right count must be non-negative');
      if Count > Length(Str) then
        Count := Length(Str);
      Result := Copy(Str, Length(Str) - Count + 1, Count);
    end);

  RegisterFunction('Substr', function(const Args: array of Variant): Variant
    var
      Str: string;
      Start, Len: Integer;
    begin
      if (Length(Args) < 2) or (Length(Args) > 3) then
        raise Exception.Create('Substr requires 2 or 3 arguments (string, start [, length])');
      Str := VarToStr(Args[0]);
      Start := Args[1];
      if Start < 1 then
        raise Exception.Create('Substr start position must be >= 1');
      if Length(Args) = 3 then
      begin
        Len := Args[2];
        if Len < 0 then
          raise Exception.Create('Substr length must be non-negative');
        Result := Copy(Str, Start, Len);
      end
      else
        Result := Copy(Str, Start, MaxInt);
    end);

  RegisterFunction('IndexOf', function(const Args: array of Variant): Variant
    var
      Str, SubStr: string;
    begin
      if Length(Args) <> 2 then
        raise Exception.Create('IndexOf requires 2 arguments (string, substring)');
      Str := VarToStr(Args[0]);
      SubStr := VarToStr(Args[1]);
      Result := Pos(SubStr, Str);
    end);

  RegisterFunction('Replace', function(const Args: array of Variant): Variant
    var
      Str, OldStr, NewStr: string;
    begin
      if Length(Args) <> 3 then
        raise Exception.Create('Replace requires 3 arguments (string, old, new)');
      Str := VarToStr(Args[0]);
      OldStr := VarToStr(Args[1]);
      NewStr := VarToStr(Args[2]);
      Result := StringReplace(Str, OldStr, NewStr, [rfReplaceAll]);
    end);

  // Date/Time functions
  RegisterFunction('Now', function(const Args: array of Variant): Variant
    begin
      if Length(Args) <> 0 then
        raise Exception.Create('Now requires no arguments');
      Result := Now;
    end);

  RegisterFunction('Today', function(const Args: array of Variant): Variant
    begin
      if Length(Args) <> 0 then
        raise Exception.Create('Today requires no arguments');
      Result := Date;
    end);

  RegisterFunction('Year', function(const Args: array of Variant): Variant
    var
      DateValue: TDateTime;
      Y, M, D: Word;
    begin
      if Length(Args) <> 1 then
        raise Exception.Create('Year requires 1 argument (date)');
      try
        DateValue := VarToDateTime(Args[0]);
        DecodeDate(DateValue, Y, M, D);
        Result := Y;
      except
        raise Exception.Create('Year requires a valid date argument');
      end;
    end);

  RegisterFunction('Month', function(const Args: array of Variant): Variant
    var
      DateValue: TDateTime;
      Y, M, D: Word;
    begin
      if Length(Args) <> 1 then
        raise Exception.Create('Month requires 1 argument (date)');
      try
        DateValue := VarToDateTime(Args[0]);
        DecodeDate(DateValue, Y, M, D);
        Result := M;
      except
        raise Exception.Create('Month requires a valid date argument');
      end;
    end);

  RegisterFunction('Day', function(const Args: array of Variant): Variant
    var
      DateValue: TDateTime;
      Y, M, D: Word;
    begin
      if Length(Args) <> 1 then
        raise Exception.Create('Day requires 1 argument (date)');
      try
        DateValue := VarToDateTime(Args[0]);
        DecodeDate(DateValue, Y, M, D);
        Result := D;
      except
        raise Exception.Create('Day requires a valid date argument');
      end;
    end);

  RegisterFunction('FormatDate', function(const Args: array of Variant): Variant
    var
      DateValue: TDateTime;
      Format: string;
    begin
      if Length(Args) <> 2 then
        raise Exception.Create('FormatDate requires 2 arguments (date, format)');
      try
        DateValue := VarToDateTime(Args[0]);
        Format := VarToStr(Args[1]);
        Result := FormatDateTime(Format, DateValue);
      except
        raise Exception.Create('FormatDate requires a valid date and format string');
      end;
    end);

  RegisterFunction('ParseDate', function(const Args: array of Variant): Variant
    var
      DateStr: string;
      Year, Month, Day: Word;
      YearStr, MonthStr, DayStr: string;
    begin
      if Length(Args) <> 1 then
        raise Exception.Create('ParseDate requires 1 argument (date string in YYYY-MM-DD format)');

      DateStr := VarToStr(Args[0]);

      // Validate format: must be exactly YYYY-MM-DD (10 characters)
      if Length(DateStr) <> 10 then
        raise Exception.Create('ParseDate requires date string in YYYY-MM-DD format');

      if (DateStr[5] <> '-') or (DateStr[8] <> '-') then
        raise Exception.Create('ParseDate requires date string in YYYY-MM-DD format');

      try
        YearStr := Copy(DateStr, 1, 4);
        MonthStr := Copy(DateStr, 6, 2);
        DayStr := Copy(DateStr, 9, 2);

        Year := StrToInt(YearStr);
        Month := StrToInt(MonthStr);
        Day := StrToInt(DayStr);

        // Validate ranges
        if (Year < 1) or (Month < 1) or (Month > 12) or (Day < 1) or (Day > 31) then
          raise Exception.Create('ParseDate: Invalid date values');

        Result := EncodeDate(Year, Month, Day);
      except
        on E: Exception do
          if Pos('ParseDate', E.Message) > 0 then
            raise
          else
            raise Exception.Create('ParseDate requires date string in YYYY-MM-DD format');
      end;
    end);

  RegisterFunction('DateAdd', function(const Args: array of Variant): Variant
    var
      DateValue: TDateTime;
      Days: Integer;
    begin
      if Length(Args) <> 2 then
        raise Exception.Create('DateAdd requires 2 arguments (date, days)');
      try
        DateValue := VarToDateTime(Args[0]);
        Days := Args[1];
        Result := DateValue + Days;
      except
        raise Exception.Create('DateAdd requires a valid date and number of days');
      end;
    end);

  RegisterFunction('DateDiff', function(const Args: array of Variant): Variant
    var
      Date1, Date2: TDateTime;
    begin
      if Length(Args) <> 2 then
        raise Exception.Create('DateDiff requires 2 arguments (date1, date2)');
      try
        Date1 := VarToDateTime(Args[0]);
        Date2 := VarToDateTime(Args[1]);
        Result := Trunc(Date2 - Date1);
      except
        raise Exception.Create('DateDiff requires two valid dates');
      end;
    end);
end;

destructor TExprEvaluator.Destroy;
begin
  FFunctions.Free;
  FVariables.Free;
  inherited;
end;

procedure TExprEvaluator.SetOnResolveExternalVariable(const Resolver: TExternalVariableResolver);
begin
  FOnResolveExternalVariable := Resolver;
end;

procedure TExprEvaluator.ValidateArgCount(const FuncName: string; Expected, Actual: Integer);
begin
  if Expected <> Actual then
    raise Exception.CreateFmt('%s requires %d argument(s), got %d', [FuncName, Expected, Actual]);
end;

procedure TExprEvaluator.ValidateNumeric(const FuncName: string; const Arg: Variant);
begin
  if not VarIsNumeric(Arg) then
    raise Exception.CreateFmt('%s requires numeric argument', [FuncName]);
end;

procedure TExprEvaluator.QuickSortNumeric(var Arr: TArray<Double>; Low, High: Integer);
var
  I, J: Integer;
  Pivot, Temp: Double;
begin
  if Low < High then
  begin
    Pivot := Arr[(Low + High) div 2];
    I := Low;
    J := High;
    repeat
      while Arr[I] < Pivot do Inc(I);
      while Arr[J] > Pivot do Dec(J);
      if I <= J then
      begin
        Temp := Arr[I];
        Arr[I] := Arr[J];
        Arr[J] := Temp;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    QuickSortNumeric(Arr, Low, J);
    QuickSortNumeric(Arr, I, High);
  end;
end;

procedure TExprEvaluator.QuickSortString(var Arr: TArray<string>; Low, High: Integer);
var
  I, J: Integer;
  Pivot, Temp: string;
begin
  if Low < High then
  begin
    Pivot := Arr[(Low + High) div 2];
    I := Low;
    J := High;
    repeat
      while Arr[I] < Pivot do Inc(I);
      while Arr[J] > Pivot do Dec(J);
      if I <= J then
      begin
        Temp := Arr[I];
        Arr[I] := Arr[J];
        Arr[J] := Temp;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    QuickSortString(Arr, Low, J);
    QuickSortString(Arr, I, High);
  end;
end;

procedure TExprEvaluator.RegisterFunction(const Name: string; Handler: TFuncHandler);
begin
  FFunctions.AddOrSetValue(UpperCase(Name), Handler);
end;

function TExprEvaluator.Evaluate(const Expr: string): Variant;
var
  Exprs: TArray<string>;
  I: Integer;
  OldInput: string;
  OldPos: Integer;
begin
  // Save current state
  OldInput := FInput;
  OldPos := FPos;

  // Split by semicolon to support multiple expressions
  Exprs := Expr.Split([';']);
  Result := Unassigned;
  for I := 0 to High(Exprs) do
  begin
    FInput := Trim(Exprs[I]);
    FPos := 1;
    Result := ParseAssignment;
  end;

  // Restore original state
  FInput := OldInput;
  FPos := OldPos;
end;

function TExprEvaluator.ParseAssignment: Variant;
var
  Id: string;
  SavePos: Integer;
begin
  SkipWhitespace;
  if IsAlpha(CurrentChar) then
  begin
    SavePos := FPos;
    Id := ParseIdentifier;
    SkipWhitespace;
    if Copy(FInput, FPos, 2) = ':=' then
    begin
      Inc(FPos, 2); // skip :=
      SkipWhitespace;
      Result := ParseIfExpression;
      SetVariable(Id, Result);
    end
    else
    begin
      FPos := SavePos; // backtrack to saved position
      Result := ParseIfExpression;
    end;
  end
  else
    Result := ParseIfExpression;
end;

function TExprEvaluator.ParseIfExpression: Variant;
var
  Condition, ThenValue, ElseValue: Variant;
  IfWord: string;
  SavePos: Integer;
begin
  SkipWhitespace;
  if FPos > Length(FInput) then
  begin
    Result := ParseLogical;
    Exit;
  end;

  // Check if current character can start an identifier
  if not IsAlpha(CurrentChar) then
  begin
    Result := ParseLogical;
    Exit;
  end;

  SavePos := FPos;
  IfWord := ParseIdentifier;
  if IfWord <> 'IF' then
  begin
    FPos := SavePos; // backtrack to saved position
    Result := ParseLogical;
    Exit;
  end;

  Condition := ParseLogical;

  SkipWhitespace;
  if not ConsumeKeyword('THEN') then
    raise Exception.Create('Expected THEN after IF condition');

  ThenValue := ParseIfExpression;

  SkipWhitespace;
  if not ConsumeKeyword('ELSE') then
    raise Exception.Create('Expected ELSE after THEN');

  ElseValue := ParseIfExpression;

  if Condition then
    Result := ThenValue
  else
    Result := ElseValue;
end;

function TExprEvaluator.CurrentChar: Char;
begin
  if FPos <= Length(FInput) then
    Result := FInput[FPos]
  else
    Result := #0;
end;

procedure TExprEvaluator.NextChar;
begin
  Inc(FPos);
end;

procedure TExprEvaluator.SkipWhitespace;
var
  CommentEnd: Integer;
begin
  while FPos <= Length(FInput) do
  begin
    // Skip regular whitespace
    if CharInSet(FInput[FPos], [' ', #9, #10, #13]) then
    begin
      Inc(FPos);
      Continue;
    end;

    // Handle // style comments (single line)
    if (FPos < Length(FInput)) and (FInput[FPos] = '/') and (FInput[FPos + 1] = '/') then
    begin
      // Skip to end of line or end of input
      Inc(FPos, 2); // Skip the //
      while (FPos <= Length(FInput)) and not CharInSet(FInput[FPos], [#10, #13]) do
        Inc(FPos);
      Continue;
    end;

    // Handle { } style comments (can span multiple lines)
    if FInput[FPos] = '{' then
    begin
      Inc(FPos); // Skip the opening {
      // Find the closing }
      CommentEnd := FPos;
      while (CommentEnd <= Length(FInput)) and (FInput[CommentEnd] <> '}') do
        Inc(CommentEnd);

      if CommentEnd <= Length(FInput) then
        FPos := CommentEnd + 1 // Skip past the closing }
      else
        raise Exception.Create('Unterminated comment - missing closing }');
      Continue;
    end;

    // No more whitespace or comments to skip
    Break;
  end;
end;

function TExprEvaluator.IsDigit(c: Char): Boolean;
begin
  Result := CharInSet(c, ['0'..'9']);
end;

function TExprEvaluator.IsAlpha(c: Char): Boolean;
begin
  Result := CharInSet(c, ['a'..'z', 'A'..'Z', '_']);
end;

function TExprEvaluator.ParseNumber: string;
var
  Start: Integer;
begin
  Start := FPos;
  while (FPos <= Length(FInput)) and (IsDigit(CurrentChar) or (CurrentChar = '.')) do
    NextChar;
  Result := Copy(FInput, Start, FPos - Start);
end;

function TExprEvaluator.ParseIdentifier: string;
var
  Start: Integer;
begin
  Start := FPos;
  // Include dots to support member access (e.g., f.Index, obj.property.subprop)
  while (FPos <= Length(FInput)) and (IsAlpha(CurrentChar) or IsDigit(CurrentChar) or (CurrentChar = '.')) do
    NextChar;
  Result := UpperCase(Copy(FInput, Start, FPos - Start));
end;

function TExprEvaluator.ParseString: string;
var
  Start: Integer;
begin
  NextChar; // skip opening "
  Start := FPos;
  while (FPos <= Length(FInput)) and (CurrentChar <> '"') do
    NextChar;
  if CurrentChar <> '"' then
    raise Exception.Create('Unterminated string');
  Result := Copy(FInput, Start, FPos - Start);
  NextChar; // skip closing "
end;

function TExprEvaluator.ParsePrimary: Variant;
var
  NumStr: string;
  Id: string;
  Args: TArray<Variant>;
begin
  SkipWhitespace;

  if FPos > Length(FInput) then
  begin
    Result := Unassigned;
    Exit;
  end;

  if CurrentChar = '(' then
  begin
    NextChar; // skip '('
    Result := ParseIfExpression;
    SkipWhitespace;
    if CurrentChar <> ')' then
      raise Exception.Create('Unclosed parenthesis');
    NextChar; // skip ')'
  end
  else if CurrentChar = '"' then
  begin
    Result := ParseString;
  end
  else if IsDigit(CurrentChar) then
  begin
    NumStr := ParseNumber;
    Result := StrToFloat(NumStr, FFormatSettings);
  end
  else if CurrentChar = '-' then
  begin
    NextChar; // skip '-'
    SkipWhitespace;
    if IsDigit(CurrentChar) then
    begin
      NumStr := ParseNumber;
      if not FSkipEvaluation then
        Result := -StrToFloat(NumStr, FFormatSettings)
      else
        Result := 0;
    end
    else
    begin
      // This is a unary minus on an expression, parse the expression and negate it
      if not FSkipEvaluation then
        Result := -ParsePrimary
      else
      begin
        ParsePrimary;  // Parse but discard
        Result := 0;
      end;
    end;
  end
  else if IsAlpha(CurrentChar) and ConsumeKeyword('NOT') then
  begin
    SkipWhitespace;
    if not FSkipEvaluation then
      Result := not ParsePrimary
    else
    begin
      ParsePrimary;  // Parse but discard
      Result := False;
    end;
  end
  else if IsAlpha(CurrentChar) then
  begin
    Id := ParseIdentifier;
    SkipWhitespace;
    if CurrentChar = '(' then
    begin
      NextChar; // skip '('
      SetLength(Args, 0);
      if CurrentChar <> ')' then
      begin
        repeat
          SetLength(Args, Length(Args) + 1);
          Args[High(Args)] := ParseIfExpression;
          SkipWhitespace;
          if CurrentChar = ',' then
          begin
            NextChar;
            SkipWhitespace;
          end
          else
            Break;
        until False;
      end;
      if CurrentChar <> ')' then
        raise Exception.Create('Unclosed parenthesis in function call');
      NextChar; // skip ')'
      Result := CallFunction(Id, Args);
    end
    else
    begin
      Result := GetVariable(Id);
    end;
  end
  else
    raise Exception.Create('Unexpected character: ' + QuotedStr(CurrentChar));
end;

function TExprEvaluator.ParseFactor: Variant;
var
  Left: Variant;
  Right: Variant;
begin
  Left := ParsePrimary;
  // Allow Null values only in skip mode (for short-circuit evaluation)
  if (VarIsEmpty(Left) or VarIsNull(Left)) and not FSkipEvaluation then
    raise Exception.Create('Expected expression');

  SkipWhitespace;
  while CurrentChar = '^' do
  begin
    NextChar; // skip ^
    Right := ParsePrimary;
    if (VarIsEmpty(Right) or VarIsNull(Right)) and not FSkipEvaluation then
      raise Exception.Create('Expected expression after ^');

    if not FSkipEvaluation then
      Left := System.Math.Power(Left, Right);
    SkipWhitespace;
  end;
  Result := Left;
end;

function TExprEvaluator.ParseMultiplicative: Variant;
var
  Left: Variant;
  Op: Char;
  Right: Variant;
begin
  Left := ParseFactor;
  SkipWhitespace;

  while CharInSet(CurrentChar, ['*', '/', 'm', 'M', 'd', 'D']) do
  begin
    if CharInSet(CurrentChar, ['m', 'M']) then
    begin
      if ConsumeKeyword('MOD') then
      begin
        SkipWhitespace;
        Right := ParseFactor;
        if not FSkipEvaluation then
          Left := Trunc(Left) mod Trunc(Right);
      end
      else
        Break;
    end
    else if CharInSet(CurrentChar, ['d', 'D']) then
    begin
      if ConsumeKeyword('DIV') then
      begin
        SkipWhitespace;
        Right := ParseFactor;
        if not FSkipEvaluation then
          Left := Trunc(Left) div Trunc(Right);
      end
      else
        Break;
    end
    else
    begin
      Op := CurrentChar;
      NextChar;
      Right := ParseFactor;

      if not FSkipEvaluation then
      begin
        case Op of
          '*': Left := Left * Right;
          '/':
            begin
              if Right = 0 then
                raise Exception.Create('Division by zero');
              Left := Left / Right;
            end;
        end;
      end;
    end;
    SkipWhitespace;
  end;

  Result := Left;
end;

function TExprEvaluator.ParseAdditive: Variant;
var
  Left: Variant;
  Op: Char;
  Right: Variant;
begin
  Left := ParseMultiplicative;
  SkipWhitespace;

  while CharInSet(CurrentChar, ['+', '-']) do
  begin
    Op := CurrentChar;
    NextChar;
    Right := ParseMultiplicative;

    if not FSkipEvaluation then
    begin
      case Op of
        '+':
          begin
            if VarIsStr(Left) or VarIsStr(Right) then
              Left := VarToStr(Left) + VarToStr(Right)
            else
              Left := Left + Right;
          end;
        '-': Left := Left - Right;
      end;
    end;
    SkipWhitespace;
  end;

  Result := Left;
end;

function TExprEvaluator.ParseRelational: Variant;
var
  Left: Variant;
  Right: Variant;
begin
  Left := ParseAdditive;
  SkipWhitespace;

  // Handle comparison operators and LIKE
  if CharInSet(CurrentChar, ['<', '>', '=']) then
  begin
    if CurrentChar = '=' then
    begin
      NextChar;
      Right := ParseAdditive;
      if not FSkipEvaluation then
        Result := Left = Right
      else
        Result := False;
    end
    else if CurrentChar = '<' then
    begin
      NextChar;
      if CurrentChar = '>' then
      begin
        NextChar;
        Right := ParseAdditive;
        if not FSkipEvaluation then
          Result := Left <> Right
        else
          Result := False;
      end
      else if CurrentChar = '=' then
      begin
        NextChar;
        Right := ParseAdditive;
        if not FSkipEvaluation then
          Result := Left <= Right
        else
          Result := False;
      end
      else
      begin
        Right := ParseAdditive;
        if not FSkipEvaluation then
          Result := Left < Right
        else
          Result := False;
      end;
    end
    else if CurrentChar = '>' then
    begin
      NextChar;
      if CurrentChar = '=' then
      begin
        NextChar;
        Right := ParseAdditive;
        if not FSkipEvaluation then
          Result := Left >= Right
        else
          Result := False;
      end
      else
      begin
        Right := ParseAdditive;
        if not FSkipEvaluation then
          Result := Left > Right
        else
          Result := False;
      end;
    end;
  end
  else if ConsumeKeyword('LIKE') then
  begin
    // LIKE operator for pattern matching
    SkipWhitespace;
    Right := ParseAdditive;
    if not FSkipEvaluation then
      Result := MatchesMask(VarToStr(Left), VarToStr(Right))
    else
      Result := False;
  end
  else
    Result := Left;
end;

function TExprEvaluator.ParseLogical: Variant;
var
  Left: Variant;
  Right: Variant;
  LeftBool: Boolean;
  OldSkip: Boolean;
begin
  Left := ParseRelational;
  SkipWhitespace;

  // Handle logical operators with short-circuit evaluation
  while True do
  begin
    if ConsumeKeyword('AND') then
    begin
      SkipWhitespace;
      // Short-circuit: if Left is False, skip Right evaluation
      if VarIsType(Left, varBoolean) and not FSkipEvaluation then
      begin
        LeftBool := Left;
        if not LeftBool then
        begin
          // Enable skip mode to parse but not execute
          OldSkip := FSkipEvaluation;
          FSkipEvaluation := True;
          try
            ParseRelational;  // Parse to consume tokens, but don't execute
          finally
            FSkipEvaluation := OldSkip;
          end;
          Left := False;  // Short-circuit result
        end
        else
        begin
          Right := ParseRelational;
          Left := LeftBool and Boolean(Right);
        end;
      end
      else
        Left := Left and ParseRelational;
      SkipWhitespace;
    end
    else if ConsumeKeyword('OR') then
    begin
      SkipWhitespace;
      // Short-circuit: if Left is True, skip Right evaluation
      if VarIsType(Left, varBoolean) and not FSkipEvaluation then
      begin
        LeftBool := Left;
        if LeftBool then
        begin
          // Enable skip mode to parse but not execute
          OldSkip := FSkipEvaluation;
          FSkipEvaluation := True;
          try
            ParseRelational;  // Parse to consume tokens, but don't execute
          finally
            FSkipEvaluation := OldSkip;
          end;
          Left := True;  // Short-circuit result
        end
        else
        begin
          Right := ParseRelational;
          Left := LeftBool or Boolean(Right);
        end;
      end
      else
        Left := Left or ParseRelational;
      SkipWhitespace;
    end
    else if ConsumeKeyword('XOR') then
    begin
      SkipWhitespace;
      // XOR cannot be short-circuited - both sides must be evaluated
      Left := Left xor ParseRelational;
      SkipWhitespace;
    end
    else
      Break;
  end;

  Result := Left;
end;


function TExprEvaluator.CallFunction(const FuncName: string; Args: TArray<Variant>): Variant;
var
  Handler: TFuncHandler;
begin
  // In skip mode, return a dummy value without calling the function
  if FSkipEvaluation then
  begin
    Result := Null;
    Exit;
  end;

  if not FFunctions.TryGetValue(FuncName, Handler) then
    raise Exception.Create('Unknown function: ' + FuncName);
  Result := Handler(Args);
end;

function TExprEvaluator.GetVariable(const VarName: string): Variant;
var
  UpperVarName: string;
begin
  UpperVarName := UpperCase(VarName);

  // Handle boolean constants
  if UpperVarName = 'TRUE' then
    Result := True
  else if UpperVarName = 'FALSE' then
    Result := False
  else if FVariables.TryGetValue(UpperVarName, Result) then
    Exit
  else if Assigned(FOnResolveExternalVariable) then
  begin
    if not FOnResolveExternalVariable(VarName, Result) then
      raise Exception.Create('Unknown variable: ' + VarName);
  end
  else
    raise Exception.Create('Unknown variable: ' + VarName);
end;

procedure TExprEvaluator.SetVariable(const VarName: string; Value: Variant);
begin
  FVariables.AddOrSetValue(UpperCase(VarName), Value);
end;

procedure TExprEvaluator.SetVar(const Name: string; Value: Variant);
begin
  SetVariable(Name, Value);
end;

function TExprEvaluator.GetVar(const Name: string): Variant;
begin
  Result := GetVariable(Name);
end;

function TExprEvaluator.VariantToString(const Value: Variant): string;
begin
  // Convert Variant to string using FFormatSettings to ensure dot as decimal separator
  case VarType(Value) of
    varSmallint, varInteger, varByte, varShortInt, varWord, varLongWord, varInt64, varUInt64:
      Result := IntToStr(Value);
    varSingle, varDouble, varCurrency:
      Result := FloatToStr(Double(Value), FFormatSettings);
    varBoolean:
      if Value then
        Result := 'True'
      else
        Result := 'False';
    varString, varUString, varOleStr:
      Result := VarToStr(Value);
  else
    // For other types, use standard VarToStr
    Result := VarToStr(Value);
  end;
end;

function TExprEvaluator.IsKeywordAtPosition(const Keyword: string): Boolean;
var
  KeywordLen: Integer;
  NextCharPos: Integer;
begin
  KeywordLen := Length(Keyword);

  // Check if we have enough characters left
  if FPos + KeywordLen - 1 > Length(FInput) then
  begin
    Result := False;
    Exit;
  end;

  // Check if the keyword matches (case insensitive)
  if UpperCase(Copy(FInput, FPos, KeywordLen)) <> UpperCase(Keyword) then
  begin
    Result := False;
    Exit;
  end;

  // Check word boundary - next character should be a delimiter or end of input
  NextCharPos := FPos + KeywordLen;
  Result := (NextCharPos > Length(FInput)) or
            CharInSet(FInput[NextCharPos], [' ', #9, #10, #13, ')', ';', ',']);
end;

function TExprEvaluator.ConsumeKeyword(const Keyword: string): Boolean;
begin
  Result := IsKeywordAtPosition(Keyword);
  if Result then
    Inc(FPos, Length(Keyword));
end;

end.