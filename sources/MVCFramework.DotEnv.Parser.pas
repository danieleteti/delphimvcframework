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

unit MVCFramework.DotEnv.Parser;

interface

uses System.Generics.Collections, System.SysUtils;

type
{$SCOPEDENUMS ON}
  TMVCDotEnvParserState = (FileThenEnv, EnvThenFile, OnlyFile, OnlyEnv);

  TMVCDotEnvDictionary = class(TDictionary<String, String>)
  public
    constructor Create; virtual;
  end;

  EMVCDotEnvParser = class(Exception)

  end;

  TLineBreakStyle = (MSWindows, Linux { MacOS too } );
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
    fLineBreakStyle: TLineBreakStyle;
    fLineBreaksStyle: TLineBreakStyle;
    fSavedIndex: Integer;
    fCodeLength: Integer;
    function MatchIdentifier(out Value: String): Boolean;
    function MatchKey(out Token: String): Boolean;
    function MatchValue(out Token: String): Boolean;
    function MatchSymbol(const Symbol: Char): Boolean;
    procedure Check(Value: Boolean; Error: String = '');
    function MatchString(out Value: String): Boolean;
    procedure EatLineBreaks;
    procedure EatUpToLineBreak;
    function NextChar: Char;
    procedure EatSpaces;
    function DetectLineBreakStyle(Code: String): TLineBreakStyle;
    procedure MatchInLineComment;
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
  LINE_BREAKS: array [TLineBreakStyle.MSWindows .. TLineBreakStyle.Linux] of AnsiString = (#13#10, #10);

  { TMVCDotEnvParser }

procedure TMVCDotEnvParser.Check(Value: Boolean; Error: String);
begin
  if not Value then
  begin
    raise EMVCDotEnvParser.CreateFmt('Error: %s - got "%s" at line: %d - index: %d',
      [Error, fCurrChar, fCurLine, fIndex - fSavedIndex]);
  end;
end;

constructor TMVCDotEnvParser.Create;
begin
  inherited;
end;

destructor TMVCDotEnvParser.Destroy;
begin
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

procedure TMVCDotEnvParser.EatLineBreaks;
begin
  while CharInSet(fCode.Chars[fIndex], [#13, #10]) do
  begin
    fCurrChar := NextChar;
    if (fCurrChar = String(LINE_BREAKS[fLineBreakStyle])[1]) then
    begin
      Inc(fCurLine);
      fSavedIndex := fIndex;
    end;
  end;
end;

procedure TMVCDotEnvParser.EatSpaces;
begin
  while CharInSet(fCode.Chars[fIndex], [#32, #9]) do
  begin
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
begin
  fCode := DotEnvCode;
  fCodeLength := Length(fCode);
  fLineBreaksStyle := DetectLineBreakStyle(fCode);
  fIndex := -1;
  fCurLine := 0;
  fSavedIndex := 0;
  if fCodeLength = 0 then { empty .env file }
  begin
    Exit;
  end;
  NextChar;
  while fIndex < Length(DotEnvCode) do
  begin
    EatLineBreaks;
    EatSpaces;
    if MatchKey(lKey) then
    begin
      EatSpaces;
      Check(MatchSymbol('='), 'Expected "="');
      EatSpaces;
      Check(MatchValue(lValue), 'Expected "Value"');
      EnvDictionay.AddOrSetValue(lKey, lValue);
      EatSpaces;
      MatchInLineComment;
    end
    else if fCurrChar = #0 then
    begin
      Break;
    end
    else if CharInSet(fCurrChar, [';', '#']) then
    begin
      EatUpToLineBreak;
      EatLineBreaks;
    end
    else
    begin
      raise EMVCDotEnvParser.CreateFmt('Unexpected char "%s" at %d', [fCurrChar, fIndex - fSavedIndex]);
    end;
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
begin
  Value := '';
  while CharInSet(fCode.Chars[fIndex], ['0' .. '9', 'a' .. 'z', 'A' .. 'Z', '_', '.', ':', '$', '%']) do
  begin
    Value := Value + fCode.Chars[fIndex];
    NextChar;
  end;
  Result := not Value.IsEmpty;
end;

function TMVCDotEnvParser.MatchString(out Value: String): Boolean;
  procedure MatchUpToCharacterSingleLine(out Value: String; const Delimiter1: Char);
  begin
    while (fIndex < fCodeLength) and (fCode.Chars[fIndex] <> Delimiter1) and
      (not CharInSet(fCode.Chars[fIndex], [#13, #10])) do
    begin
      Check(fCode.Chars[fIndex] <> #0, 'Unexpected end of file');
      Value := Value + fCode.Chars[fIndex];
      NextChar;
    end;
  end;
  procedure MatchUpToCharacterMultiLine(out Value: String; const Delimiter1: Char);
  begin
    while (fIndex < fCodeLength) and (fCode.Chars[fIndex] <> Delimiter1) do
    begin
      Check(fCode.Chars[fIndex] <> #0, 'Unexpected end of file');
      Value := Value + fCode.Chars[fIndex];
      NextChar;
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
  Result := MatchString(Token);
end;

function TMVCDotEnvParser.NextChar: Char;
begin
  if fIndex >= (fCodeLength - 1) then
  begin
    fIndex := fCodeLength;
    Exit(#0);
  end;
  Inc(fIndex);
  Result := fCode.Chars[fIndex];
  fCurrChar := Result;
end;

{ TMVCDotEnvDictionary }

constructor TMVCDotEnvDictionary.Create;
begin
  inherited Create;
end;

end.
