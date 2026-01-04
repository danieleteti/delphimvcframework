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

unit MVCFramework.DotEnv.Parser;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.Variants,
  ExprEvaluator;

type
  {$SCOPEDENUMS ON}
  TMVCDotEnvParserState = (FileThenEnv, EnvThenFile, OnlyFile, OnlyEnv);

  TMVCDotEnvDictionary = class(TDictionary<String, String>)
  public
    constructor Create; virtual;
  end;

  EMVCDotEnvParser = class(Exception)

  end;

  TLineBreakStyle = (MSWindows, Linux { MacOS too });
  TStringQuotedStyle = (SingleQuoted, DoublyQuoted, UnQuoted);

  {
    For Windows, it is CRLF
    For UNIX, it is LF
    For MAC (up through version 9) it was CR
    For MAC OS X, it is LF

    https://en.wikipedia.org/wiki/Newline
  }

  {
    https://pypi.org/project/python-dotenv/
  }

  TMVCDotEnvParser = class
  private
    fCode: string;
    fCurrChar: Char;
    fIndex: Integer;
    fCurLine: Integer;
    fSavedIndex: Integer;
    fCodeLength: Integer;
    fCurrentLineBreak: string;
    fCurrentLineBreakLength: Integer;
    fExprEvaluator: TExprEvaluator;
    fEnvDict: TMVCDotEnvDictionary;
    function IsPartOfLineBreak(const aChar: Char): Boolean;
    function MatchIdentifier(out Value: String): Boolean;
    function MatchKey(out Token: String): Boolean;
    function MatchValue(out Token: String): Boolean;
    function MatchSymbol(const Symbol: Char): Boolean;
    procedure Check(Value: Boolean; Error: String = '');
    function MatchString(out Value: String): Boolean;
    function EatLineBreaks: Boolean;
    function EatSpaces: Boolean;
    procedure EatUpToLineBreak;
    function NextChar: Char;
    function PeekNextChar: Char;
    function DetectLineBreakStyle(Code: String): TLineBreakStyle;
    procedure MatchInLineComment;
    function EvaluateExpression(const Expr: string): string;
    function MatchExpression(out Value: string): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Parse(const EnvDictionay: TMVCDotEnvDictionary; const DotEnvCode: String);
  end;

implementation

uses
  System.IOUtils,
  System.TypInfo,
  System.Classes;

const
  LINE_BREAKS: array[TLineBreakStyle.MSWindows..TLineBreakStyle.Linux] of AnsiString = (#13#10, #10);

{ TMVCDotEnvParser }

procedure TMVCDotEnvParser.Check(Value: Boolean; Error: String);
var
  I: Integer;
  lNear: String;
  lSavedCurrChar: Char;
  lSavedCurrLine: Integer;
begin
  if not Value then
  begin
    lSavedCurrChar := fCurrChar;
    lSavedCurrLine := fCurLine;
    lNear := fCurrChar;
    for I := 1 to 20 do
    begin
      NextChar;
      if IsPartOfLineBreak(fCurrChar) then
      begin
        Break;
      end;
      if fCurrChar <> #0 then
      begin
        lNear := lNear + fCurrChar;
      end;
    end;

    if lSavedCurrChar <> #0 then
      raise EMVCDotEnvParser
          .CreateFmt('Error: %s - got "%s" at line: %d near "%s"', [Error, lSavedCurrChar, lSavedCurrLine, lNear])
    else
      raise EMVCDotEnvParser.CreateFmt('Error: %s at line: %d near "%s"', [Error, lSavedCurrLine, lNear])
  end;
end;

constructor TMVCDotEnvParser.Create;
begin
  inherited;
  fExprEvaluator := TExprEvaluator.Create;
end;

destructor TMVCDotEnvParser.Destroy;
begin
  fExprEvaluator.Free;
  inherited;
end;

function TMVCDotEnvParser.DetectLineBreakStyle(Code: String): TLineBreakStyle;
begin
  if Code.Contains(String(LINE_BREAKS[TLineBreakStyle.MSWindows])) then
    Exit(TLineBreakStyle.MSWindows);
  if Code.Contains(String(LINE_BREAKS[TLineBreakStyle.Linux])) then
    Exit(TLineBreakStyle.Linux);
  Result := TLineBreakStyle.MSWindows; // just one line or empty file
end;

function TMVCDotEnvParser.EatLineBreaks: Boolean;
begin
  Result := False;
  while CharInSet(fCode.Chars[fIndex], [#13, #10]) do
  begin
    Result := True;
    fCurrChar := NextChar;
    if fCurrChar = Char(fCurrentLineBreak[1]) then
    begin
      Inc(fCurLine);
      fSavedIndex := fIndex;
    end;
  end;
end;

function TMVCDotEnvParser.EatSpaces: Boolean;
begin
  Result := False;
  while CharInSet(fCode.Chars[fIndex], [#32, #9]) do
  begin
    Result := True;
    NextChar;
  end;
end;

procedure TMVCDotEnvParser.EatUpToLineBreak;
begin
  while not CharInSet(fCode.Chars[fIndex], [#13, #10, #0]) do
  begin
    NextChar;
  end;
end;

function TMVCDotEnvParser.IsPartOfLineBreak(const aChar: Char): Boolean;
begin
  Result := (fCurrentLineBreakLength = 1) and (fCurrChar = fCurrentLineBreak[1]);
  Result :=
      Result
          or ((fCurrentLineBreakLength = 2)
              and ((fCurrChar = fCurrentLineBreak[1]) or (fCurrChar = fCurrentLineBreak[2])));
end;

procedure TMVCDotEnvParser.MatchInLineComment;
begin
  EatSpaces;
  if MatchSymbol('#') then
  begin
    EatUpToLineBreak;
  end;
end;

procedure TMVCDotEnvParser.Parse(const EnvDictionay: TMVCDotEnvDictionary; const DotEnvCode: String);
var
  lKey: string;
  lValue: string;
  lLineBreakStyle: TLineBreakStyle;
begin
  fCode := DotEnvCode;
  fCodeLength := Length(fCode);
  fEnvDict := EnvDictionay;
  lLineBreakStyle := DetectLineBreakStyle(fCode);
  fCurrentLineBreak := String(LINE_BREAKS[lLineBreakStyle]);
  fCurrentLineBreakLength := Length(fCurrentLineBreak);
  fIndex := -1;
  fCurLine := 1;
  fSavedIndex := 0;
  if fCodeLength = 0 then { empty .env file }
  begin
    Exit;
  end;
  NextChar;
  while fIndex < Length(DotEnvCode) do
  begin
    EatSpaces;
    if EatLineBreaks then
    begin
      Continue;
    end;
    EatSpaces;
    if MatchKey(lKey) then
    begin
      EatSpaces;
      Check(MatchSymbol('='), 'Expected "="');
      EatSpaces;
      MatchValue(lValue);
      EnvDictionay.AddOrSetValue(lKey, lValue);
      EatSpaces;
      MatchInLineComment;
      Inc(fCurLine);
    end
    else if fCurrChar = #0 then
    begin
      Break;
    end
    else if CharInSet(fCurrChar, [';', '#']) then
    begin
      EatUpToLineBreak;
      EatLineBreaks;
      Inc(fCurLine);
    end
    else
    begin
      Check(False, Format('Unexpected char "%s" at line %d', [fCurrChar, fCurLine]));
    end;
  end;
end;

function TMVCDotEnvParser.PeekNextChar: Char;
begin
  if fIndex >= (fCodeLength - 1) then
  begin
    Result := #0;
  end
  else
  begin
    Result := fCode.Chars[fIndex + 1];
  end;
end;

function TMVCDotEnvParser.MatchKey(out Token: String): Boolean;
var
  lTmp: String;
begin
  lTmp := '';
  if MatchSymbol('''') then
  begin
    Check(MatchIdentifier(Token));
    Check(MatchSymbol(''''));
    Result := True;
  end
  else
  begin
    Result := MatchIdentifier(Token);
  end;
end;

function TMVCDotEnvParser.MatchSymbol(const Symbol: Char): Boolean;
begin
  Result := fCode.Chars[fIndex] = Symbol;
  if Result then
  begin
    NextChar;
  end;
end;

function TMVCDotEnvParser.MatchIdentifier(out Value: String): Boolean;
const
  FirstCharSet = ['a'..'z', 'A'..'Z', '_', '.'];
  CharSet = ['0'..'9'] + FirstCharSet;
begin
  Value := '';
  if CharInSet(fCode.Chars[fIndex], FirstCharSet) then
  begin
    Value := fCode.Chars[fIndex];
    NextChar;
  end
  else
  begin
    Exit(False);
  end;

  while CharInSet(fCode.Chars[fIndex], CharSet) do
  begin
    Value := Value + fCode.Chars[fIndex];
    NextChar;
  end;
  Result := not Value.IsEmpty;
end;

function TMVCDotEnvParser.MatchString(out Value: String): Boolean;
  procedure MatchUpToCharacterSingleLine(out Value: String; const Delimiter1: Char);
  begin
    while (fIndex < fCodeLength)
        and (fCode.Chars[fIndex] <> Delimiter1)
        and (not CharInSet(fCode.Chars[fIndex], [#13, #10])) do
    begin
      Check(fCode.Chars[fIndex] <> #0, 'Unexpected end of file');
      Value := Value + fCode.Chars[fIndex];
      NextChar;
    end;
  end;
  procedure MatchUpToCharacterMultiLine(out Value: String; const Delimiter1: Char);
  begin
    while (fCurrChar <> Delimiter1) do
    begin
      Check(fCurrChar <> #0, 'Unexpected end of file');
      Value := Value + fCurrChar;

      if Value.EndsWith(fCurrentLineBreak) then
      begin
        Inc(fCurLine);
      end;

      NextChar;
      if fCurrChar = '\' then
      begin
        if PeekNextChar = Delimiter1 then
        begin
          Value := Value + Delimiter1;
          NextChar;
          NextChar;
        end;
      end;
    end;
  end;

begin
  Value := '';
  EatSpaces;
  if MatchSymbol('"') then
  begin
    MatchUpToCharacterMultiLine(Value, '"');
    Check(MatchSymbol('"'), 'Expected ''"''');
    EatSpaces;
    MatchInLineComment;
  end
  else if MatchSymbol('''') then
  begin
    MatchUpToCharacterMultiLine(Value, '''');
    Check(MatchSymbol(''''), 'Expected ''''');
    EatSpaces;
    MatchInLineComment;
  end
  else
  begin
    MatchUpToCharacterSingleLine(Value, '#');
    Value := Value.Trim;
  end;
  Result := not Value.IsEmpty;
end;

function TMVCDotEnvParser.MatchValue(out Token: String): Boolean;
begin
  // First try to match an expression
  if MatchExpression(Token) then
  begin
    Result := True;
  end
  else
  begin
    // If not an expression, use standard string matching
    Result := MatchString(Token);
  end;
end;

function TMVCDotEnvParser.NextChar: Char;
begin
  if fIndex >= (fCodeLength - 1) then
  begin
    fIndex := fCodeLength;
    fCurrChar := #0;
  end
  else
  begin
    Inc(fIndex);
    fCurrChar := fCode.Chars[fIndex];
  end;
  Result := fCurrChar;
end;

function TMVCDotEnvParser.EvaluateExpression(const Expr: string): string;
var
  lResult: Variant;
  lKey: string;
begin
  // Set all current environment variables as expression variables
  if Assigned(fEnvDict) then
  begin
    for lKey in fEnvDict.Keys do
    begin
      fExprEvaluator.SetVar(lKey, fEnvDict[lKey]);
    end;
  end;

  try
    lResult := fExprEvaluator.Evaluate(Expr);
    // Use VariantToString to ensure consistent formatting (dot as decimal separator)
    Result := fExprEvaluator.VariantToString(lResult);
  except
    on E: Exception do
    begin
      raise EMVCDotEnvParser
          .CreateFmt('Expression evaluation error in "%s": %s at line %d', [Expr, E.Message, fCurLine]);
    end;
  end;
end;

function TMVCDotEnvParser.MatchExpression(out Value: string): Boolean;
var
  lExpr: string;
  lStartPos: Integer;
begin
  Value := '';

  // Check for $[ syntax
  if not MatchSymbol('$') then
    Exit(False);

  if not MatchSymbol('[') then
  begin
    // Backtrack - this might be a regular ${} placeholder
    Dec(fIndex);
    fCurrChar := fCode.Chars[fIndex];
    Exit(False);
  end;

  lStartPos := fIndex;
  lExpr := '';

  // Find closing bracket
  while (fCurrChar <> ']') and (fCurrChar <> #0) do
  begin
    lExpr := lExpr + fCurrChar;
    NextChar;
  end;

  if fCurrChar <> ']' then
  begin
    fIndex := lStartPos - 2; // Backtrack past both $ and [
    NextChar;
    Exit(False);
  end;

  // Consume closing bracket
  NextChar;

  if lExpr.Trim.IsEmpty then
  begin
    raise EMVCDotEnvParser.CreateFmt('Empty expression at line %d', [fCurLine]);
  end;

  try
    Value := EvaluateExpression(lExpr.Trim);
    Result := True;
  except
    on E: Exception do
    begin
      // Re-raise with more context
      raise EMVCDotEnvParser.CreateFmt('Expression error at line %d: %s', [fCurLine, E.Message]);
    end;
  end;
end;

{ TMVCDotEnvDictionary }

constructor TMVCDotEnvDictionary.Create;
begin
  inherited Create;
end;

end.
