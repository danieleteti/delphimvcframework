// ***************************************************************************
//
// Copyright (c) 2016-2024 Daniele Teti
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

unit MVCFramework.TemplatePro;

interface

uses
  System.Generics.Collections,
  MVCFramework.Rtti.Utils,
  MVCFramework.DuckTyping,
  Classes,
  SysUtils,
  Data.DB,
  System.RTTI;

type
  ETProException = class(Exception)

  end;

  EParserException = class(ETProException)

  end;

  ERenderException = class(ETProException)

  end;



  ITProVariableAdapter = interface
    ['{9A0E5797-A8D2-413F-A8B0-5D6E67DD1701}']
    function CurrentIndex: Int64;
    procedure Reset;
    function GetMemberValue(const aMemberName: string): string;
    function Next: Boolean;
    function Eof: Boolean;
  end;

  TTokenType = (ttContent, ttLoop, ttEndLoop, ttIfThen, ttEndIf, ttStartTag, ttEndTag, ttValue, ttReset, ttField, ttLineBreak, ttEOF);

  const
    TOKEN_TYPE_DESCR: array [Low(TTokenType)..High(TTokenType)] of string =
      ('ttContent', 'ttLoop', 'ttEndLoop', 'ttIfThen', 'ttEndIf', 'ttStartTag', 'ttEndTag', 'ttValue', 'ttReset', 'ttField', 'ttLineBreak', 'ttEOF');
  type
    TToken = packed record
      TokenType: TTokenType;
      Value: String;
      Ref: Integer;
      class function Create(TokType: TTokenType; Value: String; Ref: Integer = -1): TToken; static;
      function TokenTypeAsString: String;
    end;

  TTokenWalkProc = reference to procedure(const Index: Integer; const Token: TToken);

  TTemplateFunction = reference to function(aParameters: TArray<string>; const aValue: string): string;

  TTProVariablesInfo = (viSimpleType, viObject, viDataSet, viListOfObject);
  TTProVariablesInfos = set of TTProVariablesInfo;


  TVarInfo = class
    VarValue: TValue;
    VarOption: TTProVariablesInfos;
    VarIterator: Int64;
    constructor Create(const VarValue: TValue; const VarOption: TTProVariablesInfos; const VarIterator: Int64);
  end;

  TTProVariables = class(TObjectDictionary<string, TVarInfo>)
  public
    constructor Create;
  end;

  TTemplateProCompiledTemplate = class
  private
    fTokens: TList<TToken>;
    fVariables: TTProVariables;
    constructor Create(Tokens: TList<TToken>);
    procedure Error(const aMessage: String);
    function InternalRender: String;
    function GetVarAsString(const aName: string): string;
    function GetVarAsTValue(const aName: string): TValue;
    function EvaluateIfExpression(const aIdentifier: string): Boolean;
    function GetVariables: TTProVariables;
  public
    function Render: String;
    procedure ForEachToken(const TokenProc: TTokenWalkProc);
    procedure ClearData;
    procedure SetData(const Name: String; Value: TValue); overload;
  end;

  TTemplateProEngine = class
  strict private
    fOutput: string;
    function MatchStartTag: Boolean;
    function MatchEndTag: Boolean;
    function MatchVariable(var aIdentifier: string): Boolean;
    function MatchReset(var aDataSet: string): Boolean;
//    function MatchField(var aDataSet: string; var aFieldName: string): Boolean;
    function MatchSymbol(const aSymbol: string): Boolean;
  private
    fInputString: string;
    fCharIndex: Int64;
    fCurrentLine: Integer;
    fEncoding: TEncoding;
    fTemplateFunctions: TDictionary<string, TTemplateFunction>;
    procedure Error(const aMessage: string);
    function Step: Char;
    function CurrentChar: Char;
    function ExecuteFunction(aFunctionName: string; aParameters: TArray<string>; aValue: string): string;

    function ExecuteFieldFunction(aFunctionName: string; aParameters: TArray<string>; aValue: TValue): string;

    procedure CheckParNumber(const aHowManyPars: Integer; const aParameters: TArray<string>); overload;
    procedure CheckParNumber(const aMinParNumber, aMaxParNumber: Integer; const aParameters: TArray<string>); overload;
  public
    function Compile(const aTemplateString: string): TTemplateProCompiledTemplate; overload;
    constructor Create(aEncoding: TEncoding = nil);
    destructor Destroy; override;
    procedure AddTemplateFunction(const FunctionName: string; const FunctionImpl: TTemplateFunction);
  end;

function HTMLEntitiesEncode(s: string): string;

implementation

uses
  System.StrUtils;

const
  IdenfierAllowedFirstChars = ['a' .. 'z', 'A' .. 'Z', '_'];
  IdenfierAllowedChars = IdenfierAllowedFirstChars + ['0' .. '9'];
  ValueAllowedChars = IdenfierAllowedChars + [' ', '-', '+', '*', '.', '@', '/', '\']; // maybe a lot others
  START_TAG_1 = '{{';
  END_TAG_1 = '}}';

  { TParser }

procedure TTemplateProEngine.AddTemplateFunction(const FunctionName: string; const FunctionImpl: TTemplateFunction);
begin
  fTemplateFunctions.Add(FunctionName.ToLower, FunctionImpl);
end;

procedure TTemplateProEngine.CheckParNumber(const aMinParNumber, aMaxParNumber: Integer;
  const aParameters: TArray<string>);
var
  lParNumber: Integer;
begin
  lParNumber := Length(aParameters);
  if (lParNumber < aMinParNumber) or (lParNumber > aMaxParNumber) then
  begin
    if aMinParNumber = aMaxParNumber then
      Error(Format('Expected %d parameters, got %d', [aMinParNumber, lParNumber]))
    else
      Error(Format('Expected from %d to %d parameters, got %d', [aMinParNumber, aMaxParNumber, lParNumber]));
  end;
end;


constructor TTemplateProEngine.Create(aEncoding: TEncoding = nil);
begin
  inherited Create;
  if aEncoding = nil then
    fEncoding := TEncoding.UTF8 { default encoding }
  else
    fEncoding := aEncoding;
  fOutput := '';
  fTemplateFunctions := TDictionary<string, TTemplateFunction>.Create;
end;

function TTemplateProEngine.CurrentChar: Char;
begin
  Result := fInputString.Chars[fCharIndex];
end;

destructor TTemplateProEngine.Destroy;
begin
  fTemplateFunctions.Free;
  inherited;
end;

//function TTemplateProEngine.SetDataSourceByName(const aName: string): Boolean;
//var
//  ds: TPair<string, ITProVariableAdapter>;
//begin
//  { TODO -oDanieleT -cGeneral : Refactor this method to use GetDataSourceByName }
//  Result := False;
//  for ds in fDataSources do
//  begin
//    if SameText(ds.Key, aName) then
//    begin
//      fCurrentDataSource := ds.Value;
//      Result := True;
//      Break;
//    end;
//  end;
//end;

//function TTemplateProEngine.GetDataSourceByName(const aName: string; out aDataSource: ITProVariableAdapter): Boolean;
//var
//  ds: TPair<string, ITProVariableAdapter>;
//begin
//  Result := False;
//  for ds in fDataSources do
//  begin
//    if SameText(ds.Key, aName) then
//    begin
//      aDataSource := ds.Value;
//      Result := True;
//      Break;
//    end;
//  end;
//end;

function TTemplateProEngine.MatchEndTag: Boolean;
begin
  Result := MatchSymbol(END_TAG_1);
end;

//function TTemplateProEngine.MatchField(var aDataSet: string; var aFieldName: string): Boolean;
//begin
//  Result := False;
//  aFieldName := '';
//  if not MatchSymbol(':') then
//    Exit;
//  if not MatchVariable(aDataSet) then
//    Error('Expected dataset name');
//  if not MatchSymbol('.') then
//    Error('Expected "."');
//  if not MatchVariable(aFieldName) then
//    Error('Expected field name');
//  Result := True;
//end;

function TTemplateProEngine.MatchVariable(var aIdentifier: string): Boolean;
var
  lTmp: String;
begin
  lTmp := '';
  Result := False;
  if CharInSet(fInputString.Chars[fCharIndex], IdenfierAllowedFirstChars) then
  begin
    while CharInSet(fInputString.Chars[fCharIndex], IdenfierAllowedChars) do
    begin
      lTmp := lTmp + fInputString.Chars[fCharIndex];
      Inc(fCharIndex);
    end;
    Result := True;
    aIdentifier := lTmp;
  end;
  if Result then
  begin
    while MatchSymbol('.') do
    begin
      lTmp := '';
      if not MatchVariable(lTmp) then
      begin
        Error('Expected identifier after ' + aIdentifier);
      end;
      aIdentifier := aIdentifier + '.' + lTmp;
    end;
  end;
end;

function TTemplateProEngine.MatchReset(var aDataSet: string): Boolean;
begin
  if not MatchSymbol('reset') then
    Exit(False);
  Result := MatchSymbol('(') and MatchVariable(aDataSet) and MatchSymbol(')');
end;

function TTemplateProEngine.MatchStartTag: Boolean;
begin
  Result := MatchSymbol(START_TAG_1);
end;

function TTemplateProEngine.MatchSymbol(const aSymbol: string): Boolean;
var
  lSymbolIndex: Integer;
  lSavedCharIndex: Int64;
begin
  if aSymbol.IsEmpty then
    Exit(True);
  lSavedCharIndex := fCharIndex;
  lSymbolIndex := 0;
  while (fInputString.Chars[fCharIndex] = aSymbol.Chars[lSymbolIndex]) and (lSymbolIndex < Length(aSymbol)) do
  begin
    Inc(fCharIndex);
    Inc(lSymbolIndex);
  end;
  Result := (lSymbolIndex > 0) and (lSymbolIndex = Length(aSymbol));
  if not Result then
    fCharIndex := lSavedCharIndex;
end;

function TTemplateProEngine.Step: Char;
begin
  Inc(fCharIndex);
  Result := CurrentChar;
end;

function TTemplateProEngine.Compile(const aTemplateString: string): TTemplateProCompiledTemplate;
var
  lSectionStack: array [0..49] of Integer; //max 50 nested loops
  lCurrentSectionIndex: Integer;

  lIfStatementStack: array [0..49] of Integer; //max 50 nested ifs
  lCurrentIfIndex: Integer;
  lLastToken: TTokenType;
  lChar: Char;
  lVarName: string;
  lFuncName: string;
  lIdentifier: string;
  lDataSet: string;
  lFieldName: string;
  //lFuncParams: TArray<string>;
  lStartVerbatim: UInt64;
  lEndVerbatim: UInt64;
  lTokens: TList<TToken>;
  lIndexOfLatestIfStatement: UInt64;
  function GetFunctionParameters: TArray<string>;
  var
    lFuncPar: string;
  begin
    Result := [];
    while MatchSymbol(':') do
    begin
      lFuncPar := '';
      if not MatchVariable(lFuncPar) then
        Error('Expected function parameter');
      Result := Result + [lFuncPar];
    end;
  end;
begin
  fCharIndex := -1;
  fCurrentLine := 1;
  lCurrentIfIndex := -1;
  lCurrentSectionIndex := -1;
  fInputString := aTemplateString;
  lTokens := TList<TToken>.Create;
  try
    lStartVerbatim := 0;
    lChar := Step;
    while fCharIndex <= aTemplateString.Length do
    begin
      lChar := CurrentChar;
      if lChar = #0 then //eof
      begin
        lEndVerbatim := fCharIndex;
        if lEndVerbatim - lStartVerbatim > 0 then
        begin
          lLastToken := ttContent;
          lTokens.Add(TToken.Create(ttContent, aTemplateString.Substring(lStartVerbatim, lEndVerbatim - lStartVerbatim)));
        end;
        lTokens.Add(TToken.Create(ttEOF, ''));
        Break;
      end;

      if MatchSymbol(sLineBreak) then         {linebreak}
      begin
        lEndVerbatim := fCharIndex - Length(sLineBreak);
        if lEndVerbatim - lStartVerbatim > 0 then
        begin
          lTokens.Add(TToken.Create(ttContent, aTemplateString.Substring(lStartVerbatim, lEndVerbatim - lStartVerbatim)));
        end;
        lStartVerbatim := fCharIndex;
        lEndVerbatim := lStartVerbatim;
        lLastToken := ttLineBreak;
        lTokens.Add(TToken.Create(ttLineBreak, ''));
        lChar := CurrentChar;
        Inc(fCurrentLine);
      end else if MatchStartTag then         {starttag}
      begin
        lChar := CurrentChar;
        lEndVerbatim := fCharIndex - Length(START_TAG_1);

        if lEndVerbatim - lStartVerbatim > 0 then
        begin
          lLastToken := ttContent;
          lTokens.Add(TToken.Create(ttContent, aTemplateString.Substring(lStartVerbatim, lEndVerbatim - lStartVerbatim)));
        end;

        if MatchSymbol('loop') then {loop}
        begin
          lChar := CurrentChar;
          if not MatchSymbol('(') then
            Error('Expected "("');
          if not MatchVariable(lIdentifier) then
            Error('Expected identifier after "loop("');
          if not MatchSymbol(')') then
            Error('Expected ")" after "' + lIdentifier + '"');
          if not MatchEndTag then
            Error('Expected closing tag for "loop(' + lIdentifier + ')"');
          // create another element in the sections stack
          Inc(lCurrentSectionIndex);
          lSectionStack[lCurrentSectionIndex] := lTokens.Count;
          lLastToken := ttLoop;
          lTokens.Add(TToken.Create(lLastToken, lIdentifier));
          lStartVerbatim := fCharIndex;
          lEndVerbatim := lStartVerbatim;
        end else if MatchSymbol('endloop') then //endloop
        begin
          lChar := CurrentChar;
          if not MatchEndTag then
            Error('Expected closing tag');
          if lCurrentSectionIndex = -1 then
          begin
            Error('endloop without loop');
          end;
          lLastToken := ttEndLoop;
          lTokens.Add(TToken.Create(ttEndLoop, '', lSectionStack[lCurrentSectionIndex]));
          Dec(lCurrentSectionIndex);
          lStartVerbatim := fCharIndex;
          lEndVerbatim := lStartVerbatim;
        end else if MatchSymbol('endif') then
        begin
          if lCurrentIfIndex = -1 then
          begin
            Error('"endif" without "if"');
          end;
          if not MatchEndTag then
          begin
            Error('Expected closing tag for "endif"');
          end;
          lLastToken := ttEndIf;
          lTokens.Add(TToken.Create(ttEndIf, ''));

          // jumps handling...
          lIndexOfLatestIfStatement := lIfStatementStack[lCurrentIfIndex];
          lTokens[lIndexOfLatestIfStatement] :=
            TToken.Create(ttIfThen, lTokens[lIndexOfLatestIfStatement].Value, lTokens.Count - 1);

          Dec(lCurrentIfIndex);
          lStartVerbatim := fCharIndex;
          lEndVerbatim := lStartVerbatim;
        end else if MatchSymbol('if') then
        begin
          if not MatchSymbol('(') then
            Error('Expected "("');
          if not MatchVariable(lIdentifier) then
            Error('Expected identifier after "if("');
          if not MatchSymbol(')') then
            Error('Expected ")" after "' + lIdentifier + '"');
          if not MatchEndTag then
            Error('Expected closing tag for "if(' + lIdentifier + ')"');

          lLastToken := ttIfThen;
          lTokens.Add(TToken.Create(ttIfThen, lIdentifier));
          Inc(lCurrentIfIndex);
          lIfStatementStack[lCurrentIfIndex] := lTokens.Count - 1;
          lStartVerbatim := fCharIndex;
          lEndVerbatim := lStartVerbatim;
        end else if MatchReset(lDataSet) then  {reset}
        begin
          if not MatchEndTag then
            Error('Expected closing tag');
          lLastToken := ttReset;
          lTokens.Add(TToken.Create(ttReset, lDataSet));
          lStartVerbatim := fCharIndex;
          lEndVerbatim := lStartVerbatim;
          lChar := Step;
        end else if MatchVariable(lVarName) then {variable}
        begin
          lChar := CurrentChar;
          if lVarName.IsEmpty then
            Error('Invalid variable name');
          lFuncName := '';
          {
          if MatchSymbol('|') then
          begin
            if not MatchIdentifier(lFuncName) then
              Error('Invalid function name');
            lFuncParams := GetFunctionParameters;
            if not MatchEndTag then
              Error('Expected end tag');
            AppendOutput(ExecuteFunction(lFuncName, lFuncParams, GetVarAsString(lVarName)));
          end
          else
          begin
            if not MatchEndTag then
              Error('Expected end tag');
            AppendOutput(GetVarAsString(lVarName));
          end;
          }
          if not MatchEndTag then
          begin
            Error('Expected end tag "' + END_TAG_1 + '"');
          end;
          lChar := CurrentChar;
          lStartVerbatim := fCharIndex;
          lEndVerbatim := lStartVerbatim;
          lLastToken := ttValue;
          lTokens.Add(TToken.Create(ttValue, lVarName));
        end else if MatchSymbol('#') then
        begin
          while not MatchEndTag do
          begin
            lChar := Step;
          end;
          lChar := CurrentChar;
          lStartVerbatim := fCharIndex;
          lEndVerbatim := lStartVerbatim;
        end
        else
        begin
          Error('Expected command, got "' + CurrentChar + '"');
        end;
      end
      else
      begin
        // output verbatim
        Inc(lEndVerbatim);
        lChar := Step;
      end;
    end;
    Result := TTemplateProCompiledTemplate.Create(lTokens);
  except
    on E: Exception do
    begin
      lTokens.Free;
      raise;
    end;
  end;
end;

function CapitalizeString(const s: string; const CapitalizeFirst: Boolean): string;
const
  ALLOWEDCHARS = ['a' .. 'z', '_'];
var
  index: Integer;
  bCapitalizeNext: Boolean;
begin
  bCapitalizeNext := CapitalizeFirst;
  Result := lowercase(s);
  if Result <> EmptyStr then
  begin
    for index := 1 to Length(Result) do
    begin
      if bCapitalizeNext then
      begin
        Result[index] := UpCase(Result[index]);
        bCapitalizeNext := False;
      end
      else if not CharInSet(Result[index], ALLOWEDCHARS) then
      begin
        bCapitalizeNext := True;
      end;
    end; // for
  end; // if
end;

procedure TTemplateProEngine.Error(const aMessage: string);
begin
  raise EParserException.CreateFmt('%s - at line %d', [aMessage, fCurrentLine]);
end;

procedure TTemplateProEngine.CheckParNumber(const aHowManyPars: Integer; const aParameters: TArray<string>);
begin
  CheckParNumber(aHowManyPars, aHowManyPars, aParameters);
end;

function TTemplateProEngine.ExecuteFunction(aFunctionName: string; aParameters: TArray<string>;
  aValue: string): string;
var
  lFunc: TTemplateFunction;
begin
  aFunctionName := lowercase(aFunctionName);
  if aFunctionName = 'tohtml' then
  begin
    Exit(HTMLEntitiesEncode(aValue));
  end;
  if aFunctionName = 'uppercase' then
  begin
    Exit(UpperCase(aValue));
  end;
  if aFunctionName = 'lowercase' then
  begin
    Exit(lowercase(aValue));
  end;
  if aFunctionName = 'capitalize' then
  begin
    Exit(CapitalizeString(aValue, True));
  end;
  if aFunctionName = 'rpad' then
  begin
    CheckParNumber(1, 2, aParameters);
    if Length(aParameters) = 1 then
      Exit(aValue.PadRight(aParameters[0].ToInteger))
    else
      Exit(aValue.PadRight(aParameters[0].ToInteger, aParameters[1].Chars[0]));
  end;
  if aFunctionName = 'lpad' then
  begin
    if Length(aParameters) = 1 then
      Exit(aValue.PadLeft(aParameters[0].ToInteger))
    else
      Exit(aValue.PadLeft(aParameters[0].ToInteger, aParameters[1].Chars[0]));
  end;

  if not fTemplateFunctions.TryGetValue(aFunctionName, lFunc) then
  begin
    raise EParserException.CreateFmt('Unknown function [%s]', [aFunctionName]);
  end;
  Result := lFunc(aParameters, aValue);
end;

function TTemplateProEngine.ExecuteFieldFunction(aFunctionName: string; aParameters: TArray<string>;
  aValue: TValue): string;
var
  lDateValue: TDate;
  lDateTimeValue: TDateTime;
  lStrValue: string;
begin
  aFunctionName := lowercase(aFunctionName);
  if aFunctionName = 'tohtml' then
  begin
    Exit(HTMLEntitiesEncode(aValue.AsString));
  end;
  if aFunctionName = 'uppercase' then
  begin
    Exit(UpperCase(aValue.AsString));
  end;
  if aFunctionName = 'lowercase' then
  begin
    Exit(lowercase(aValue.AsString));
  end;
  if aFunctionName = 'capitalize' then
  begin
    Exit(CapitalizeString(aValue.AsString, True));
  end;
  if aFunctionName = 'rpad' then
  begin
    if aValue.IsType<Integer> then
      lStrValue := aValue.AsInteger.ToString
    else if aValue.IsType<string> then
      lStrValue := aValue.AsString
    else
      Error(Format('Invalid parameter/s for function: %s', [aFunctionName]));

    CheckParNumber(1, 2, aParameters);
    if Length(aParameters) = 1 then
    begin
      Exit(lStrValue.PadRight(aParameters[0].ToInteger));
    end
    else
    begin
      Exit(lStrValue.PadRight(aParameters[0].ToInteger, aParameters[1].Chars[0]));
    end;
  end;
  if aFunctionName = 'lpad' then
  begin
    if aValue.IsType<Integer> then
      lStrValue := aValue.AsInteger.ToString
    else if aValue.IsType<string> then
      lStrValue := aValue.AsString
    else
      Error(Format('Invalid parameter/s for function: ', [aFunctionName]));

    CheckParNumber(1, 2, aParameters);
    if Length(aParameters) = 1 then
    begin
      Exit(lStrValue.PadLeft(aParameters[0].ToInteger));
    end
    else
    begin
      Exit(lStrValue.PadLeft(aParameters[0].ToInteger, aParameters[1].Chars[0]));
    end;
  end;

  if aFunctionName = 'datetostr' then
  begin
    if not aValue.TryAsType<TDate>(lDateValue) then
      Error('Invalid Date');
    Exit(DateToStr(lDateValue));
  end;
  if aFunctionName = 'datetimetostr' then
  begin
    if not aValue.TryAsType<TDateTime>(lDateTimeValue) then
      Error('Invalid DateTime');
    Exit(DateTimeToStr(lDateTimeValue));
  end;
  if aFunctionName = 'formatdatetime' then
  begin
    CheckParNumber(1, aParameters);
    if not aValue.TryAsType<TDateTime>(lDateTimeValue) then
      Error('Invalid DateTime');
    Exit(FormatDateTime(aParameters[0], lDateTimeValue));
  end;

  Error(Format('Unknown function [%s]', [aFunctionName]));
end;

function HTMLEntitiesEncode(s: string): string;
  procedure repl(var s: string; r: string; posi: Integer);
  begin
    delete(s, posi, 1);
    insert(r, s, posi);
  end;

var
  I: Integer;
  r: string;
begin
  I := 0;
  while I < Length(s) do
  begin
    r := '';
    case ord(s[I]) of
      160:
        r := 'nbsp';
      161:
        r := 'excl';
      162:
        r := 'cent';
      163:
        r := 'ound';
      164:
        r := 'curren';
      165:
        r := 'yen';
      166:
        r := 'brvbar';
      167:
        r := 'sect';
      168:
        r := 'uml';
      169:
        r := 'copy';
      170:
        r := 'ordf';
      171:
        r := 'laquo';
      172:
        r := 'not';
      173:
        r := 'shy';
      174:
        r := 'reg';
      175:
        r := 'macr';
      176:
        r := 'deg';
      177:
        r := 'plusmn';
      178:
        r := 'sup2';
      179:
        r := 'sup3';
      180:
        r := 'acute';
      181:
        r := 'micro';
      182:
        r := 'para';
      183:
        r := 'middot';
      184:
        r := 'cedil';
      185:
        r := 'sup1';
      186:
        r := 'ordm';
      187:
        r := 'raquo';
      188:
        r := 'frac14';
      189:
        r := 'frac12';
      190:
        r := 'frac34';
      191:
        r := 'iquest';
      192:
        r := 'Agrave';
      193:
        r := 'Aacute';
      194:
        r := 'Acirc';
      195:
        r := 'Atilde';
      196:
        r := 'Auml';
      197:
        r := 'Aring';
      198:
        r := 'AElig';
      199:
        r := 'Ccedil';
      200:
        r := 'Egrave';
      201:
        r := 'Eacute';
      202:
        r := 'Ecirc';
      203:
        r := 'Euml';
      204:
        r := 'Igrave';
      205:
        r := 'Iacute';
      206:
        r := 'Icirc';
      207:
        r := 'Iuml';
      208:
        r := 'ETH';
      209:
        r := 'Ntilde';
      210:
        r := 'Ograve';
      211:
        r := 'Oacute';
      212:
        r := 'Ocirc';
      213:
        r := 'Otilde';
      214:
        r := 'Ouml';
      215:
        r := 'times';
      216:
        r := 'Oslash';
      217:
        r := 'Ugrave';
      218:
        r := 'Uacute';
      219:
        r := 'Ucirc';
      220:
        r := 'Uuml';
      221:
        r := 'Yacute';
      222:
        r := 'THORN';
      223:
        r := 'szlig';
      224:
        r := 'agrave';
      225:
        r := 'aacute';
      226:
        r := 'acirc';
      227:
        r := 'atilde';
      228:
        r := 'auml';
      229:
        r := 'aring';
      230:
        r := 'aelig';
      231:
        r := 'ccedil';
      232:
        r := 'egrave';
      233:
        r := 'eacute';
      234:
        r := 'ecirc';
      235:
        r := 'euml';
      236:
        r := 'igrave';
      237:
        r := 'iacute';
      238:
        r := 'icirc';
      239:
        r := 'iuml';
      240:
        r := 'eth';
      241:
        r := 'ntilde';
      242:
        r := 'ograve';
      243:
        r := 'oacute';
      244:
        r := 'ocirc';
      245:
        r := 'otilde';
      246:
        r := 'ouml';
      247:
        r := 'divide';
      248:
        r := 'oslash';
      249:
        r := 'ugrave';
      250:
        r := 'uacute';
      251:
        r := 'ucirc';
      252:
        r := 'uuml';
      253:
        r := 'yacute';
      254:
        r := 'thorn';
      255:
        r := 'yuml';
    end;
    if r <> '' then
    begin
      repl(s, '&' + r + ';', I);
    end;
    Inc(I)
  end;
  Result := s;
end;

{ TToken }

class function TToken.Create(TokType: TTokenType; Value: String; Ref: Integer): TToken;
begin
  Result.TokenType:= TokType;
  Result.Value := Value;
  Result.Ref := Ref;
end;

function TToken.TokenTypeAsString: String;
begin
  Result := TOKEN_TYPE_DESCR[self.TokenType];
end;

{ TTemplateProCompiledTemplate }

constructor TTemplateProCompiledTemplate.Create(Tokens: TList<TToken>);
begin
  inherited Create;
  fTokens := Tokens;
end;

procedure TTemplateProCompiledTemplate.Error(const aMessage: String);
begin
  raise ERenderException.Create(aMessage);
end;

procedure TTemplateProCompiledTemplate.ForEachToken(
  const TokenProc: TTokenWalkProc);
var
  I: Integer;
begin
  for I := 0 to fTokens.Count - 1 do
  begin
    TokenProc(I, fTokens[I]);
  end;
end;


function TTemplateProCompiledTemplate.InternalRender: String;
var
  lIdx: UInt64;
  lBuff: TStringBuilder;
  lDataSource: ITProVariableAdapter;
  lSectionStack: array[0..49] of String;
  lLoopStmIndex: Integer;
  lDataSourceName: string;
  lPieces: TArray<String>;
  lFieldName: string;
  lLastTag: TTokenType;
  lVariable: TVarInfo;
  lWrapped: IMVCList;
begin
  lLastTag := ttEOF;
  lBuff := TStringBuilder.Create;
  try
    lIdx := 0;
    while fTokens[lIdx].TokenType <> ttEOF do
    begin
      Writeln(Format('%4d: %s', [lIdx, fTokens[lIdx].TokenTypeAsString]));
      //Readln;
      case fTokens[lIdx].TokenType of
        ttContent: begin
          lBuff.Append(HTMLEntitiesEncode(fTokens[lIdx].Value));
        end;
        ttLoop: begin
          if GetVariables.TryGetValue(fTokens[lIdx].Value, lVariable) then
          begin
            if viDataSet in lVariable.VarOption then
            begin
              if TDataset(lVariable.VarValue.AsObject).Eof then
              begin
                lIdx := fTokens[lIdx].Ref; //skip to endif
                Continue;
              end
            end else if viObject in lVariable.VarOption then
            begin
              Error(Format('Cannot iterate over a not iterable object [%s]', [fTokens[lIdx].Value]));
            end else if viListOfObject in lVariable.VarOption then
            begin
              lWrapped := TDuckTypedList.Wrap(lVariable.VarValue.AsObject);
              if lVariable.VarIterator = lWrapped.Count - 1 then
              begin
                lIdx := fTokens[lIdx].Ref; //skip to endif
                Continue;
              end
              else
              begin
                lVariable.VarIterator := lVariable.VarIterator + 1;
              end;
            end;
          end
          else
          begin
            Error(Format('Unknown variable in loop statement [%s]', [fTokens[lIdx].Value]));
          end;
        end;
        ttEndLoop: begin
          lLoopStmIndex := fTokens[lIdx].Ref;
          lDataSourceName := fTokens[lLoopStmIndex].Value;
          if GetVariables.TryGetValue(lDataSourceName, lVariable) then
          begin
            if viDataSet in lVariable.VarOption then
            begin
              TDataset(lVariable.VarValue.AsObject).Next;
              if not TDataset(lVariable.VarValue.AsObject).Eof then
              begin
                lIdx := fTokens[lIdx].Ref; //goto loop
                Continue;
              end;
            end else if viListOfObject in lVariable.VarOption then
            begin
              lWrapped := TDuckTypedList.Wrap(lVariable.VarValue.AsObject);
              if lVariable.VarIterator < lWrapped.Count - 1 then
              begin
                lIdx := fTokens[lIdx].Ref; //skip to endif
                Continue;
              end;
            end;
          end
        end;
        ttIfThen: begin
          if not EvaluateIfExpression(fTokens[lIdx].Value) then
          begin
            lIdx := fTokens[lIdx].Ref; //jump to "endif"
            Continue;
          end;
        end;
        ttEndIf: begin end;
        ttStartTag: begin end;
        ttEndTag : begin end;
        ttValue: begin
          lBuff.Append(HTMLEntitiesEncode(GetVarAsString(fTokens[lIdx].Value)));
        end;
        ttReset: begin end;
        ttField: begin end;
        ttLineBreak: begin
          if not (lLastTag in [ttLoop, ttEndLoop, ttIfThen, ttReset]) then
          begin
            lBuff.AppendLine;
          end;
        end;
        else
        begin
          Error('Invalid token: ' + fTokens[lIdx].TokenTypeAsString);
        end;
      end;

      lLastTag := fTokens[lIdx].TokenType;
      Inc(lIdx);
    end;
    Result := lBuff.ToString;
  finally
    lBuff.Free;
  end;
end;

function TTemplateProCompiledTemplate.Render: String;
begin
  Result := InternalRender();
end;

function TTemplateProCompiledTemplate.GetVarAsString(const aName: string): string;
var
  lValue: TValue;
begin
  lValue := GetVarAsTValue(aName);
  if lValue.IsObject and (lValue.AsObject is TField) then
  begin
    Result := TField(lValue.AsObject).AsString;
  end
  else
  begin
    Result := lValue.ToString;
  end;
end;

function TTemplateProCompiledTemplate.GetVarAsTValue(
  const aName: string): TValue;
var
  lVariable: TVarInfo;
  lPieces: TArray<String>;
  lDS: TDataSet;
begin
  lPieces := aName.Split(['.']);
  Result := '';
  if GetVariables.TryGetValue(lPieces[0], lVariable) then
  begin
    if viDataSet in lVariable.VarOption then
    begin
      lDS := TDataSet(lVariable.VarValue.AsObject);
      Result := lDS.FieldByName(lPieces[1]);
    end
    else if viListOfObject in lVariable.VarOption then
    begin
      Result := TRttiUtils.GetProperty(WrapAsList(lVariable.VarValue.AsObject).GetItem(lVariable.VarIterator), lPieces[1]);
    end
    else if viObject in lVariable.VarOption then
    begin
      Result := TRttiUtils.GetProperty(lVariable.VarValue.AsObject, lPieces[1]);
    end
    else if viSimpleType in lVariable.VarOption then
    begin
      if lVariable.VarValue.IsEmpty then
        Result := TValue.Empty
      else
        Result := lVariable.VarValue;
    end;
  end;
end;

function TTemplateProCompiledTemplate.GetVariables: TTProVariables;
begin
  if not Assigned(fVariables) then
  begin
    fVariables := TTProVariables.Create;
  end;
  Result := fVariables;
end;

function TTemplateProCompiledTemplate.EvaluateIfExpression(const aIdentifier: string): Boolean;
var
  lVarValue: String;
begin
  lVarValue := GetVarAsString(aIdentifier);
  if SameText(lVarValue, 'false') or (lVarValue = '0') or lVarValue.IsEmpty then
  begin
    Exit(False);
  end
  else
  begin
    Exit(True);
  end;
end;


//procedure TTemplateProCompiledTemplate.SetData(const Name: String;
//  Value: TValue);
//begin
////  GetVariables.Add(Name, Value);
//end;

//procedure TTemplateProCompiledTemplate.SetCollectionData<T>(const Name: String; Value: TObjectList<T>);
//begin
//  GetVariables.Add(Name, TTPObjectListAdapter<T>.Create(Value));
//end;

procedure TTemplateProCompiledTemplate.SetData(const Name: String;
  Value: TValue);
var
  lMVCList: IMVCList;
begin
  case Value.Kind of
    tkClass:
    begin
      if Value.AsObject is TDataSet then
      begin
        GetVariables.Add(Name, TVarInfo.Create(Value.AsObject, [viDataSet], -1));
      end
      else
      begin
        if TDuckTypedList.CanBeWrappedAsList(Value.AsObject, lMVCList) then
        begin
          GetVariables.Add(Name, TVarInfo.Create(TDuckTypedList(Value.AsObject), [viListOfObject], -1));
        end
        else
        begin
          GetVariables.Add(Name, TVarInfo.Create(Value.AsObject, [viObject], -1));
        end;
      end;
    end;
    tkInteger, tkString, tkUString, tkFloat, tkEnumeration : GetVariables.Add(Name, TVarInfo.Create(Value, [viSimpleType], -1));
    else
      raise ETProException.Create('Invalid type for variable ' + Name);
  end;

end;


procedure TTemplateProCompiledTemplate.ClearData;
begin
  GetVariables.Clear;
end;

{ TVarInfo }

constructor TVarInfo.Create(const VarValue: TValue;
  const VarOption: TTProVariablesInfos; const VarIterator: Int64);
begin
  Self.VarValue := VarValue;
  Self.VarOption := VarOption;
  Self.VarIterator := VarIterator;
end;

{ TTProVariables }

constructor TTProVariables.Create;
begin
  inherited Create([doOwnsValues]);
end;

end.
