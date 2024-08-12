// ***************************************************************************
//
// Copyright (c) 2016-2024 Daniele Teti
//
// https://github.com/danieleteti/templatepro
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

unit TemplatePro;

interface

uses
  System.Generics.Collections,
  System.Classes,
  System.SysUtils,
  Data.DB,
  System.DateUtils,
  System.RTTI;

type
  ETProException = class(Exception)

  end;

  ETProParserException = class(ETProException)

  end;

  ETProRenderException = class(ETProException)

  end;

  ETProDuckTypingException = class(ETProException)

  end;

  TIfThenElseIndex = record
    IfIndex, ElseIndex: Int64;
  end;

  TTokenType = (
    ttContent, ttInclude, ttLoop, ttEndLoop, ttIfThen, ttElse, ttEndIf, ttStartTag,
    ttLiteralString, ttEndTag, ttValue, ttFilterName, ttFilterParameter, ttReset, ttLineBreak, ttEOF);
  const
    TOKEN_TYPE_DESCR: array [Low(TTokenType)..High(TTokenType)] of string =
      ('ttContent', 'ttInclude', 'ttLoop', 'ttEndLoop', 'ttIfThen', 'ttElse', 'ttEndIf', 'ttStartTag',
       'ttLiteralString', 'ttEndTag', 'ttValue', 'ttFilterName', 'ttFilterParameter', 'ttReset', 'ttLineBreak', 'ttEOF');
  type
    TToken = packed record
      TokenType: TTokenType;
      Value: String;
      Ref1, Ref2: Integer;
      class function Create(TokType: TTokenType; Value: String; Ref1: Integer = -1; Ref2: Integer = -1): TToken; static;
      function TokenTypeAsString: String;
      function ToString: String;
    end;

  TTokenWalkProc = reference to procedure(const Index: Integer; const Token: TToken);

  TTProTemplateFunction = function(const aValue: TValue; const aParameters: TArray<string>): string;

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

  ITProCompiledTemplate = interface
    ['{0BE04DE7-6930-456B-86EE-BFD407BA6C46}']
    function Render: String;
    procedure ForEachToken(const TokenProc: TTokenWalkProc);
    procedure ClearData;
    procedure SetData(const Name: String; Value: TValue); overload;
    procedure AddTemplateFunction(const FunctionName: string; const FunctionImpl: TTProTemplateFunction);
    procedure DumpToFile(const FileName: String);
  end;

  TTProCompiledTemplate = class(TInterfacedObject, ITProCompiledTemplate)
  private
    fTokens: TList<TToken>;
    fVariables: TTProVariables;
    fTemplateFunctions: TDictionary<string, TTProTemplateFunction>;
    constructor Create(Tokens: TList<TToken>);
    procedure Error(const aMessage: String);
    function IsTruthy(const Value: TValue): Boolean;
    function GetVarAsString(const aName: string): string;
    function GetVarAsTValue(const aName: string): TValue;
    function EvaluateIfExpression(aIdentifier: string): Boolean;
    function GetVariables: TTProVariables;
    function ExecuteFilter(aFunctionName: string; aParameters: TArray<string>; aValue: TValue): string;
    procedure CheckParNumber(const aHowManyPars: Integer; const aParameters: TArray<string>); overload;
    procedure CheckParNumber(const aMinParNumber, aMaxParNumber: Integer; const aParameters: TArray<string>); overload;
    function GetPseudoVariable(const Variable: TVarInfo; const PseudoVarName: String): TValue;
  public
    destructor Destroy; override;
    function Render: String;
    procedure ForEachToken(const TokenProc: TTokenWalkProc);
    procedure ClearData;
    procedure SetData(const Name: String; Value: TValue); overload;
    procedure AddTemplateFunction(const FunctionName: string; const FunctionImpl: TTProTemplateFunction);
    procedure DumpToFile(const FileName: String);
  end;

  TTProCompiler = class
  strict private
    function MatchStartTag: Boolean;
    function MatchEndTag: Boolean;
    function MatchVariable(var aIdentifier: string): Boolean;
    function MatchFilterParamValue(var aParamValue: string): Boolean;
    function MatchReset(var aDataSet: string): Boolean;
    function MatchSymbol(const aSymbol: string): Boolean;
    function MatchString(out aStringValue: string): Boolean;
    procedure InternalMatchFilter(lIdentifier: String; var lStartVerbatim: UInt64; const CurrToken: TTokenType; aTokens: TList<TToken>; const lRef2: Integer);
    function GetFunctionParameters: TArray<String>;
  private
    fInputString: string;
    fCharIndex: Int64;
    fCurrentLine: Integer;
    fEncoding: TEncoding;
    fCurrentFileName: String;
    procedure Error(const aMessage: string);
    function Step: Char;
    function CurrentChar: Char;
    procedure InternalCompileIncludedTemplate(const aTemplate: string; const aTokens: TList<TToken>; const aFileNameRefPath: String);
    procedure Compile(const aTemplate: string; const aTokens: TList<TToken>; const aFileNameRefPath: String); overload;
  public
    function Compile(const aTemplate: string; const aFileNameRefPath: String = ''): ITProCompiledTemplate; overload;
    constructor Create(aEncoding: TEncoding = nil);
  end;

  ITProWrappedList = interface
    ['{C1963FBF-1E42-4E2A-A17A-27F3945F13ED}']
    function GetItem(const AIndex: Integer): TObject;
    procedure Add(const AObject: TObject);
    function Count: Integer;
    procedure Clear;
    function IsWrappedList: Boolean; overload;
    function ItemIsObject(const AIndex: Integer; out AValue: TValue): Boolean;
  end;

function HTMLEncode(s: string): string;
function HTMLSpecialCharsEncode(s: string): string;


implementation

uses
  System.StrUtils, System.IOUtils, System.NetEncoding, System.Math;

const
  IdenfierAllowedFirstChars = ['a' .. 'z', 'A' .. 'Z', '_', '@'];
  IdenfierAllowedChars = ['a' .. 'z', 'A' .. 'Z', '_', '0' .. '9'];
  ValueAllowedChars = IdenfierAllowedChars + [' ', '-', '+', '*', '.', '@', '/', '\']; // maybe a lot others
  START_TAG = '{{';
  END_TAG = '}}';

type
  TTProRTTIUtils = class sealed
  public
    class function GetProperty(AObject: TObject; const APropertyName: string): TValue;
  end;

  TTProDuckTypedList = class(TInterfacedObject, ITProWrappedList)
  private
    FObjectAsDuck: TObject;
    FObjType: TRttiType;
    FAddMethod: TRttiMethod;
    FClearMethod: TRttiMethod;
    FCountProperty: TRttiProperty;
    FGetItemMethod: TRttiMethod;
    FGetCountMethod: TRttiMethod;
  protected
    procedure Add(const AObject: TObject);
    procedure Clear;
    function ItemIsObject(const AIndex: Integer; out AValue: TValue): Boolean;
  public
    constructor Create(const AObjectAsDuck: TObject); overload;
    constructor Create(const AInterfaceAsDuck: IInterface); overload;

    function IsWrappedList: Boolean; overload;
    function Count: Integer;
    procedure GetItemAsTValue(const AIndex: Integer; out AValue: TValue);
    function GetItem(const AIndex: Integer): TObject;
    class function CanBeWrappedAsList(const AObjectAsDuck: TObject): Boolean; overload; static;
    class function CanBeWrappedAsList(const AObjectAsDuck: TObject; out AMVCList: ITProWrappedList): Boolean; overload; static;
    class function CanBeWrappedAsList(const AInterfaceAsDuck: IInterface): Boolean; overload; static;
    class function Wrap(const AObjectAsDuck: TObject): ITProWrappedList; static;
  end;

var
  GlContext: TRttiContext;

function WrapAsList(const AObject: TObject): ITProWrappedList;
begin
  Result := TTProDuckTypedList.Wrap(AObject);
end;


{ TParser }

procedure TTProCompiledTemplate.AddTemplateFunction(const FunctionName: string; const FunctionImpl: TTProTemplateFunction);
begin
  fTemplateFunctions.Add(FunctionName.ToLower, FunctionImpl);
end;

function TTProCompiledTemplate.GetPseudoVariable(const Variable: TVarInfo; const PseudoVarName: String): TValue;
begin
  if PseudoVarName = '@@index' then
  begin
    Result := Variable.VarIterator + 1;
  end
  else if PseudoVarName = '@@odd' then
  begin
    Result := (Variable.VarIterator + 1) mod 2 > 0;
  end
  else if PseudoVarName = '@@even' then
  begin
    Result := (Variable.VarIterator + 1) mod 2 = 0;
  end
  else
  begin
    Result := TValue.Empty;
  end;
end;

procedure TTProCompiledTemplate.CheckParNumber(const aMinParNumber, aMaxParNumber: Integer;
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

procedure TTProCompiler.InternalCompileIncludedTemplate(const aTemplate: string;
  const aTokens: TList<TToken>; const aFileNameRefPath: String);
var
  lCompiler: TTProCompiler;
begin
  lCompiler := TTProCompiler.Create(fEncoding);
  try
    lCompiler.Compile(aTemplate, aTokens, aFileNameRefPath);
    Assert(aTokens[aTokens.Count - 1].TokenType = ttEOF);
    aTokens.Delete(aTokens.Count - 1); // remove the EOF
  finally
    lCompiler.Free;
  end;
end;

procedure TTProCompiler.InternalMatchFilter(lIdentifier: String; var lStartVerbatim: UInt64; const CurrToken: TTokenType; aTokens: TList<TToken>; const lRef2: Integer);
var
  lFuncName: string;
  lFuncParamsCount: Integer;
  lFuncParams: TArray<String>;
  I: Integer;
begin
  lFuncName := '';
  lFuncParamsCount := -1; {-1 means "no filter applied to value"}
  if MatchSymbol('|') then
  begin
    if not MatchVariable(lFuncName) then
      Error('Invalid function name applied to variable or literal string "' + lIdentifier + '"');
    lFuncParams := GetFunctionParameters;
    lFuncParamsCount := Length(lFuncParams);
  end;

  if not MatchEndTag then
  begin
    Error('Expected end tag "' + END_TAG + '"');
  end;
  lStartVerbatim := fCharIndex;
  aTokens.Add(TToken.Create(CurrToken, lIdentifier, lFuncParamsCount, lRef2));

  //add function with params
  if not lFuncName.IsEmpty then
  begin
    aTokens.Add(TToken.Create(ttFilterName, lFuncName, lFuncParamsCount));
    if lFuncParamsCount > 0 then
    begin
      for I := 0 to lFuncParamsCount -1 do
      begin
        aTokens.Add(TToken.Create(ttFilterParameter, lFuncParams[I]));
      end;
    end;
  end;

end;

constructor TTProCompiler.Create(aEncoding: TEncoding = nil);
begin
  inherited Create;
  if aEncoding = nil then
    fEncoding := TEncoding.UTF8 { default encoding }
  else
    fEncoding := aEncoding;
end;

function TTProCompiler.CurrentChar: Char;
begin
  Result := fInputString.Chars[fCharIndex];
end;

function TTProCompiler.MatchEndTag: Boolean;
begin
  Result := MatchSymbol(END_TAG);
end;

function TTProCompiler.MatchVariable(var aIdentifier: string): Boolean;
var
  lTmp: String;
begin
  lTmp := '';
  Result := False;
  if CharInSet(fInputString.Chars[fCharIndex], IdenfierAllowedFirstChars) then
  begin
    lTmp := fInputString.Chars[fCharIndex];
    Inc(fCharIndex);
    if lTmp = '@' then
    begin
      if fInputString.Chars[fCharIndex] = '@' then
      begin
        lTmp := '@@';
        Inc(fCharIndex);
      end;
    end;

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
        Error('Expected identifier after "' + aIdentifier + '"');
      end;
      aIdentifier := aIdentifier + '.' + lTmp;
    end;
  end;
end;

function TTProCompiler.MatchFilterParamValue(var aParamValue: string): Boolean;
var
  lTmp: String;
begin
  lTmp := '';
  Result := False;
  if CharInSet(fInputString.Chars[fCharIndex], IdenfierAllowedChars) then
  begin
    while CharInSet(fInputString.Chars[fCharIndex], ValueAllowedChars) do
    begin
      lTmp := lTmp + fInputString.Chars[fCharIndex];
      Inc(fCharIndex);
    end;
    Result := True;
    aParamValue := lTmp;
  end;
end;


function TTProCompiler.MatchReset(var aDataSet: string): Boolean;
begin
  if not MatchSymbol('reset') then
    Exit(False);
  Result := MatchSymbol('(') and MatchVariable(aDataSet) and MatchSymbol(')');
end;

function TTProCompiler.MatchStartTag: Boolean;
begin
  Result := MatchSymbol(START_TAG);
end;

function TTProCompiler.MatchString(out aStringValue: String): Boolean;
begin
  aStringValue := '';
  Result := MatchSymbol('"');
  if Result then
  begin
    while not MatchSymbol('"') do //no escape so far
    begin
      if CurrentChar = #0 then
      begin
        Error('Unclosed string at the end of file');
      end;
      aStringValue := aStringValue + CurrentChar;
      Step;
    end;
  end;
end;

function TTProCompiler.MatchSymbol(const aSymbol: string): Boolean;
var
  lSymbolIndex: Integer;
  lSavedCharIndex: Int64;
  lSymbolLength: Integer;
begin
  if aSymbol.IsEmpty then
    Exit(True);
  lSavedCharIndex := fCharIndex;
  lSymbolIndex := 0;
  lSymbolLength := Length(aSymbol);
  while (fInputString.Chars[fCharIndex] = aSymbol.Chars[lSymbolIndex]) and (lSymbolIndex < lSymbolLength) do
  begin
    Inc(fCharIndex);
    Inc(lSymbolIndex);
  end;
  Result := (lSymbolIndex > 0) and (lSymbolIndex = lSymbolLength);
  if not Result then
    fCharIndex := lSavedCharIndex;
end;

function TTProCompiler.Step: Char;
begin
  Inc(fCharIndex);
  Result := CurrentChar;
end;

function TTProCompiler.Compile(const aTemplate: string; const aFileNameRefPath: String): ITProCompiledTemplate;
var
  lTokens: TList<TToken>;
  lFileNameRefPath: string;
begin
  if aFileNameRefPath.IsEmpty then
  begin
    lFileNameRefPath := TPath.Combine(TPath.GetDirectoryName(GetModuleName(HInstance)), '<main>');
  end
  else
  begin
    lFileNameRefPath := TPath.GetFullPath(aFileNameRefPath);
  end;
  fCurrentFileName := lFileNameRefPath;
  lTokens := TList<TToken>.Create;
  try
    Compile(aTemplate, lTokens, fCurrentFileName);
    Result := TTProCompiledTemplate.Create(lTokens);
  except
    lTokens.Free;
    raise;
  end;
end;

procedure TTProCompiler.Compile(const aTemplate: string; const aTokens: TList<TToken>; const aFileNameRefPath: String);
var
  lSectionStack: array [0..49] of Integer; //max 50 nested loops
  lCurrentSectionIndex: Integer;

  lIfStatementStack: array [0..49] of TIfThenElseIndex; //max 50 nested ifs
  lCurrentIfIndex: Integer;
  lLastToken: TTokenType;
  lChar: Char;
  lVarName: string;
  lFuncName: string;
  lIdentifier: string;
  //lFuncParams: TArray<string>;
  lStartVerbatim: UInt64;
  lEndVerbatim: UInt64;
  lIndexOfLatestIfStatement: UInt64;
  lIndexOfLatestLoopStatement: Integer;
  lIndexOfLatestElseStatement: Int64;
  lNegation: Boolean;
  lFuncParams: TArray<String>;
  lFuncParamsCount: Integer;
  I: Integer;
  lIncludeFileContent: string;
  lCurrentFileName: string;
  lStringValue: string;
  lIdentifierFound: Boolean;
  lStringFound: Boolean;
  lRef2: Integer;
begin
  fCurrentFileName := aFileNameRefPath;
  fCharIndex := -1;
  fCurrentLine := 1;
  lCurrentIfIndex := -1;
  lCurrentSectionIndex := -1;
  fInputString := aTemplate;
  lStartVerbatim := 0;
  Step;
  while fCharIndex <= fInputString.Length do
  begin
    lChar := CurrentChar;
    if lChar = #0 then //eof
    begin
      lEndVerbatim := fCharIndex;
      if lEndVerbatim - lStartVerbatim > 0 then
      begin
        lLastToken := ttContent;
        aTokens.Add(TToken.Create(lLastToken, HTMLSpecialCharsEncode(fInputString.Substring(lStartVerbatim, lEndVerbatim - lStartVerbatim))));
      end;
      aTokens.Add(TToken.Create(ttEOF, ''));
      Break;
    end;

    if MatchSymbol(sLineBreak) then         {linebreak}
    begin
      lEndVerbatim := fCharIndex - Length(sLineBreak);
      if lEndVerbatim - lStartVerbatim > 0 then
      begin
        aTokens.Add(TToken.Create(ttContent, fInputString.Substring(lStartVerbatim, lEndVerbatim - lStartVerbatim)));
      end;
      lStartVerbatim := fCharIndex;
      lLastToken := ttLineBreak;
      aTokens.Add(TToken.Create(lLastToken, ''));
      Inc(fCurrentLine);
    end else if MatchStartTag then         {starttag}
    begin
      lEndVerbatim := fCharIndex - Length(START_TAG);

      if lEndVerbatim - lStartVerbatim > 0 then
      begin
        lLastToken := ttContent;
        aTokens.Add(TToken.Create(lLastToken, fInputString.Substring(lStartVerbatim, lEndVerbatim - lStartVerbatim)));
      end;

      if CurrentChar = START_TAG[1] then
      begin
        lLastToken := ttContent;
        aTokens.Add(TToken.Create(lLastToken, START_TAG));
        Inc(fCharIndex);
        lStartVerbatim := fCharIndex;
        Continue;
      end;

      if CurrentChar = ':' then //variable
      begin
        Step;
        if MatchVariable(lVarName) then {variable}
        begin
          if lVarName.IsEmpty then
            Error('Invalid variable name');
          lFuncName := '';
          lFuncParamsCount := -1; {-1 means "no filter applied to value"}
          lRef2 := IfThen(MatchSymbol('$'),1,-1); // {{value$}} means no escaping
          if MatchSymbol('|') then
          begin
            if not MatchVariable(lFuncName) then
              Error('Invalid function name applied to variable ' + lVarName);
            lFuncParams := GetFunctionParameters;
            lFuncParamsCount := Length(lFuncParams);
          end;

          if not MatchEndTag then
          begin
            Error('Expected end tag "' + END_TAG + '"');
          end;
          lStartVerbatim := fCharIndex;
          lLastToken := ttValue;
          aTokens.Add(TToken.Create(lLastToken, lVarName, lFuncParamsCount, lRef2));

          //add function with params
          if not lFuncName.IsEmpty then
          begin
            aTokens.Add(TToken.Create(ttFilterName, lFuncName, lFuncParamsCount));
            if lFuncParamsCount > 0 then
            begin
              for I := 0 to lFuncParamsCount -1 do
              begin
                aTokens.Add(TToken.Create(ttFilterParameter, lFuncParams[I]));
              end;
            end;
          end;
        end; //matchvariable
      end
      else
      begin
        if MatchSymbol('loop') then {loop}
        begin
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
          lSectionStack[lCurrentSectionIndex] := aTokens.Count;
          lLastToken := ttLoop;
          aTokens.Add(TToken.Create(lLastToken, lIdentifier));
          lStartVerbatim := fCharIndex;
        end else if MatchSymbol('endloop') then {endloop}
        begin
          if not MatchEndTag then
            Error('Expected closing tag');
          if lCurrentSectionIndex = -1 then
          begin
            Error('endloop without loop');
          end;
          lLastToken := ttEndLoop;
          aTokens.Add(TToken.Create(lLastToken, '', lSectionStack[lCurrentSectionIndex]));

          // let the loop know where the endloop is
          lIndexOfLatestLoopStatement := lSectionStack[lCurrentSectionIndex];
          aTokens[lIndexOfLatestLoopStatement] :=
            TToken.Create(ttLoop, aTokens[lIndexOfLatestLoopStatement].Value, aTokens.Count - 1);

          Dec(lCurrentSectionIndex);
          lStartVerbatim := fCharIndex;
        end else if MatchSymbol('endif') then {endif}
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
          aTokens.Add(TToken.Create(lLastToken, ''));

          // jumps handling...
          lIndexOfLatestIfStatement := lIfStatementStack[lCurrentIfIndex].IfIndex;

          //rewrite current "ifthen" references
          aTokens[lIndexOfLatestIfStatement] :=
            TToken.Create(ttIfThen,
              aTokens[lIndexOfLatestIfStatement].Value,
              aTokens[lIndexOfLatestIfStatement].Ref1,
              aTokens.Count - 1); {ttIfThen.Ref2 points always to relative "endif"}

          if aTokens[lIndexOfLatestIfStatement].Ref1 > -1 then
          begin
            lIndexOfLatestElseStatement := aTokens[lIndexOfLatestIfStatement].Ref1;
            aTokens[lIndexOfLatestElseStatement] :=
              TToken.Create(ttElse,
                aTokens[lIndexOfLatestElseStatement].Value,
                -1 {Ref1 is not used by ttElse},
                aTokens.Count - 1); {ttIfThen.Ref2 points always to relative "endif"}
          end;

          Dec(lCurrentIfIndex);
          lStartVerbatim := fCharIndex;
        end else if MatchSymbol('if') then
        begin
          if not MatchSymbol('(') then
            Error('Expected "("');
          lNegation := MatchSymbol('!');
          if not MatchVariable(lIdentifier) then
            Error('Expected identifier after "if("');
          if not MatchSymbol(')') then
            Error('Expected ")" after "' + lIdentifier + '"');
          if not MatchEndTag then
            Error('Expected closing tag for "if(' + lIdentifier + ')"');
          if lNegation then
          begin
            lIdentifier := '!' + lIdentifier;
          end;
          lLastToken := ttIfThen;
          aTokens.Add(TToken.Create(lLastToken, lIdentifier));
          Inc(lCurrentIfIndex);
          lIfStatementStack[lCurrentIfIndex].IfIndex := aTokens.Count - 1;
          lIfStatementStack[lCurrentIfIndex].ElseIndex := -1;
          lStartVerbatim := fCharIndex;
        end else if MatchSymbol('else') then
        begin
          if not MatchEndTag then
            Error('Expected closing tag for "else"');

          lLastToken := ttElse;
          aTokens.Add(TToken.Create(lLastToken, ''));

          // jumps handling...
          lIndexOfLatestIfStatement := lIfStatementStack[lCurrentIfIndex].IfIndex;
          lIfStatementStack[lCurrentIfIndex].ElseIndex := aTokens.Count - 1;
          aTokens[lIndexOfLatestIfStatement] := TToken.Create(ttIfThen,
            aTokens[lIndexOfLatestIfStatement].Value,
            lIfStatementStack[lCurrentIfIndex].ElseIndex, {ttIfThen.Ref1 points always to relative else (if present otherwise -1)}
            -1);
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('include') then {include}
        begin
          if not MatchSymbol('(') then
            Error('Expected "("');

          {In a future version we could implement a function call}
          if not MatchString(lStringValue) then
          begin
            Error('Expected string after "include("');
          end;

          if not MatchSymbol(')') then
            Error('Expected ")" after "' + lStringValue + '"');
          if not MatchEndTag then
            Error('Expected closing tag for "include(' + lStringValue + ')"');
          // create another element in the sections stack
          try
            if TDirectory.Exists(aFileNameRefPath) then
            begin
              lCurrentFileName := TPath.GetFullPath(TPath.Combine(aFileNameRefPath, lStringValue));
            end
            else
            begin
              lCurrentFileName := TPath.GetFullPath(TPath.Combine(TPath.GetDirectoryName(aFileNameRefPath), lStringValue));
            end;
            lIncludeFileContent := TFile.ReadAllText(lCurrentFileName, fEncoding);
          except
            on E: Exception do
            begin
              Error('Cannot read "' + lStringValue + '"');
            end;
          end;
          InternalCompileIncludedTemplate(lIncludeFileContent, aTokens, lCurrentFileName);
          lStartVerbatim := fCharIndex;
        end
        else if MatchReset(lIdentifier) then  {reset}
        begin
          if not MatchEndTag then
            Error('Expected closing tag');
          lLastToken := ttReset;
          aTokens.Add(TToken.Create(lLastToken, lIdentifier));
          lStartVerbatim := fCharIndex;
          Step;
        end
        else if MatchSymbol('exit') then {exit}
        begin
          lLastToken := ttEOF;
          aTokens.Add(TToken.Create(lLastToken, ''));
          Break;
        end
        else if MatchString(lStringValue) then {string}
        begin
          lRef2 := IfThen(MatchSymbol('$'),1,-1); // {{value$}} means no escaping
          InternalMatchFilter(lStringValue, lStartVerbatim, ttLiteralString, aTokens, lRef2);
        end
        else if MatchSymbol('#') then
        begin
          while not MatchEndTag do
          begin
            Step;
          end;
          lStartVerbatim := fCharIndex;
        end
        else
        begin
          lIdentifier := CurrentChar;
          Step;
          I := 0;
          while (CurrentChar <> #0) and (CurrentChar <> END_TAG[1]) and (I<20) do
          begin
            lIdentifier := lIdentifier + CurrentChar;
            Step;
            Inc(I);
          end;
          Error('Expected command, got "' + lIdentifier + '"');
        end;
      end;
    end
    else
    begin
      Step;
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

procedure TTProCompiler.Error(const aMessage: string);
begin
  raise ETProParserException.CreateFmt('%s - at line %d in file %s', [aMessage, fCurrentLine, fCurrentFileName]);
end;

function TTProCompiler.GetFunctionParameters: TArray<String>;
var
  lFuncPar: string;
begin
  Result := [];
  while MatchSymbol(':') do
  begin
    lFuncPar := '';
    if not MatchFilterParamValue(lFuncPar) then
      Error('Expected function parameter');
    Result := Result + [lFuncPar];
  end;
end;

procedure TTProCompiledTemplate.CheckParNumber(const aHowManyPars: Integer; const aParameters: TArray<string>);
begin
  CheckParNumber(aHowManyPars, aHowManyPars, aParameters);
end;

function TTProCompiledTemplate.ExecuteFilter(aFunctionName: string; aParameters: TArray<string>;
  aValue: TValue): string;
var
  lDateValue: TDate;
  lDateTimeValue: TDateTime;
  lStrValue: string;
  lDateAsString: string;
  lFunc: TTProTemplateFunction;
begin
  aFunctionName := lowercase(aFunctionName);
  if fTemplateFunctions.TryGetValue(aFunctionName, lFunc) then
  begin
    Exit(lFunc(aValue, aParameters));
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
    if aValue.IsType<String> then
    begin
      lDateAsString := aValue.AsString;
      //lDateTimeValue := ISO8601ToDate(aValue.AsString, False);
      lDateTimeValue := StrToDate(aValue.AsString);
    end
    else
    begin
      if not aValue.TryAsType<TDateTime>(lDateTimeValue) then
        Error('Invalid DateTime');
    end;
    Exit(FormatDateTime(aParameters[0], lDateTimeValue));
  end;

  Error(Format('Unknown function [%s]', [aFunctionName]));
end;

function HTMLEncode(s: string): string;
begin
  Result := TNetEncoding.HTML.Encode(s);
end;

function HTMLSpecialCharsEncode(s: string): string;
  procedure repl(var s: string; r: string; posi: Integer);
  begin
    delete(s, posi, 1);
    insert(r, s, posi);
  end;

var
  I: Integer;
  r: string;
begin
  I := 1;
  while I <= Length(s) do
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

class function TToken.Create(TokType: TTokenType; Value: String; Ref1: Integer; Ref2: Integer): TToken;
begin
  Result.TokenType:= TokType;
  Result.Value := Value;
  Result.Ref1 := Ref1;
  Result.Ref2 := Ref2;
end;

function TToken.TokenTypeAsString: String;
begin
  Result := TOKEN_TYPE_DESCR[self.TokenType];
end;

function TToken.ToString: String;
begin
  Result := Format('%15s | Ref1: %8d | Ref2: %8d | %-20s',[TokenTypeAsString, Ref1, Ref2, Value]);
end;

{ TTProCompiledTemplate }

constructor TTProCompiledTemplate.Create(Tokens: TList<TToken>);
begin
  inherited Create;
  fTokens := Tokens;
  fTemplateFunctions := TDictionary<string, TTProTemplateFunction>.Create;
end;

destructor TTProCompiledTemplate.Destroy;
begin
  fTemplateFunctions.Free;
  inherited;
end;

procedure TTProCompiledTemplate.DumpToFile(const FileName: String);
var
  lToken: TToken;
  lSW: TStreamWriter;
  lIdx: Integer;
begin
  lSW := TStreamWriter.Create(FileName);
  try
    lIdx := 0;
    for lToken in fTokens do
    begin
      lSW.WriteLine('%5d %s', [lIdx, lToken.ToString]);
      Inc(lIdx);
    end;
  finally
    lSW.Free;
  end;
end;

procedure TTProCompiledTemplate.Error(const aMessage: String);
begin
  raise ETProRenderException.Create(aMessage);
end;

procedure TTProCompiledTemplate.ForEachToken(
  const TokenProc: TTokenWalkProc);
var
  I: Integer;
begin
  for I := 0 to fTokens.Count - 1 do
  begin
    TokenProc(I, fTokens[I]);
  end;
end;

function TTProCompiledTemplate.Render: String;
var
  lIdx: UInt64;
  lBuff: TStringBuilder;
  lSectionStack: array[0..49] of String;
  lLoopStmIndex: Integer;
  lDataSourceName: string;
  lPieces: TArray<String>;
  lFieldName: string;
  lLastTag: TTokenType;
  lCurrTokenType: TTokenType;
  lVariable: TVarInfo;
  lWrapped: ITProWrappedList;
  lJumpTo: Integer;
  lFilterParCount: Integer;
  lFilterParameters: TArray<String>;
  I: Integer;
  lFilterName: string;
  lVarName: string;
  lVarValue: String;
  lRef2: Integer;
begin
  lLastTag := ttEOF;
  lBuff := TStringBuilder.Create;
  try
    lIdx := 0;
    while fTokens[lIdx].TokenType <> ttEOF do
    begin
      //Writeln(Format('%4d: %s', [lIdx, fTokens[lIdx].TokenTypeAsString]));
      //Readln;
      case fTokens[lIdx].TokenType of
        ttContent: begin
          lBuff.Append(fTokens[lIdx].Value);
        end;
        ttLoop: begin
          if GetVariables.TryGetValue(fTokens[lIdx].Value, lVariable) then
          begin
            if viDataSet in lVariable.VarOption then
            begin
              if TDataset(lVariable.VarValue.AsObject).Eof then
              begin
                lIdx := fTokens[lIdx].Ref1; //skip to endif
                Continue;
              end
            end else if viObject in lVariable.VarOption then
            begin
              Error(Format('Cannot iterate over a not iterable object [%s]', [fTokens[lIdx].Value]));
            end else if viListOfObject in lVariable.VarOption then
            begin
              lWrapped := WrapAsList(lVariable.VarValue.AsObject);
              if lVariable.VarIterator = lWrapped.Count - 1 then
              begin
                lIdx := fTokens[lIdx].Ref1; //skip to endif
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
          lLoopStmIndex := fTokens[lIdx].Ref1;
          lDataSourceName := fTokens[lLoopStmIndex].Value;
          if GetVariables.TryGetValue(lDataSourceName, lVariable) then
          begin
            if viDataSet in lVariable.VarOption then
            begin
              TDataset(lVariable.VarValue.AsObject).Next;
              if not TDataset(lVariable.VarValue.AsObject).Eof then
              begin
                lIdx := fTokens[lIdx].Ref1; //goto loop
                Continue;
              end;
            end else if viListOfObject in lVariable.VarOption then
            begin
              lWrapped := TTProDuckTypedList.Wrap(lVariable.VarValue.AsObject);
              if lVariable.VarIterator < lWrapped.Count - 1 then
              begin
                lIdx := fTokens[lIdx].Ref1; //skip to loop
                Continue;
              end;
            end
            else
            begin
              Error(Format('Cannot reset a not iterable object [%s]', [fTokens[lIdx].Value]));
            end;
          end;
        end;
        ttIfThen: begin
          if EvaluateIfExpression(fTokens[lIdx].Value) then
          begin
           //do nothing
          end
          else
          begin
            if fTokens[lIdx].Ref1 > -1 then
            begin
              lJumpTo := fTokens[lIdx].Ref1 + 1;
              //jump to the statement "after" ttElse (if it is ttLineBreak, jump it)
              if fTokens[lJumpTo].TokenType <> ttLineBreak then
                lIdx := lJumpTo
              else
                lIdx := lJumpTo + 1;
              Continue;
            end;
            lIdx := fTokens[lIdx].Ref2; //jump to "endif"
            Continue;
          end;
        end;
        ttElse: begin
          //always jump to ttEndIf which it reference is at ttElse.Ref2
          lIdx := fTokens[lIdx].Ref2;
          Continue;
        end;
        ttEndIf, ttStartTag, ttEndTag: begin end;
        ttInclude:
        begin
          Error('Invalid token in RENDER phase: ttInclude');
        end;
        ttValue, ttLiteralString: begin
          // Ref1 contains the optional filter parameter number (-1 if there isn't any filter)
          // Ref2 is -1 if the variable must be HTMLEncoded, while contains 1 is the value must not be HTMLEncoded
          lRef2 := fTokens[lIdx].Ref2;
          lCurrTokenType := fTokens[lIdx].TokenType;
          if fTokens[lIdx].Ref1 > -1 {has a function with Ref1 parameters} then
          begin
            lVarName := fTokens[lIdx].Value;
            Inc(lIdx);
            lFilterName := fTokens[lIdx].Value;
            lFilterParCount := fTokens[lIdx].Ref1;  // parameter count
            SetLength(lFilterParameters, lFilterParCount);
            for I := 0 to lFilterParCount - 1 do
            begin
              Inc(lIdx);
              Assert(fTokens[lIdx].TokenType = ttFilterParameter);
              lFilterParameters[I] := fTokens[lIdx].Value;
            end;
            if lCurrTokenType = ttValue then
            begin
              lVarValue := ExecuteFilter(lFilterName, lFilterParameters, GetVarAsTValue(lVarName));
            end
            else
            begin
              lVarValue := ExecuteFilter(lFilterName, lFilterParameters, lVarName);
            end;
          end
          else
          begin
            if lCurrTokenType = ttValue then
            begin
              lVarValue := GetVarAsString(fTokens[lIdx].Value);
            end
            else
            begin
              lVarValue := fTokens[lIdx].Value;
            end;
          end;
          if lRef2 = -1 {encoded} then
            lBuff.Append(HTMLEncode(lVarValue))
          else
            lBuff.Append(lVarValue);
        end;
        ttReset: begin
          if GetVariables.TryGetValue(fTokens[lIdx].Value, lVariable) then
          begin
            if viDataSet in lVariable.VarOption then
            begin
              TDataset(lVariable.VarValue.AsObject).First;
            end;
            lVariable.VarIterator := -1;
          end
        end;
        ttLineBreak: begin
          if not (lLastTag in [ttLoop, ttEndLoop, ttIfThen, ttEndIf, ttReset, ttElse]) then
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

function TTProCompiledTemplate.GetVarAsString(const aName: string): string;
var
  lValue: TValue;
begin
  lValue := GetVarAsTValue(aName);
  if lValue.IsEmpty then
  begin
    Exit('');
  end;

  if lValue.IsObject and (lValue.AsObject is TField) then
  begin
    Result := TField(lValue.AsObject).AsString;
  end
  else
  begin
    Result := lValue.ToString;
  end;
end;

function TTProCompiledTemplate.GetVarAsTValue(const aName: string): TValue;
var
  lVariable: TVarInfo;
  lPieces: TArray<String>;
  lField: TField;
  lHasMember: Boolean;
begin
  lPieces := aName.Split(['.']);
  Result := '';
  lHasMember := Length(lPieces) > 1;
  if GetVariables.TryGetValue(lPieces[0], lVariable) then
  begin
    if lVariable = nil then
    begin
      Exit(nil);
    end;
    if viDataSet in lVariable.VarOption then
    begin
      if lHasMember and lPieces[1].StartsWith('@@') then
      begin
        lVariable.VarIterator := TDataSet(lVariable.VarValue.AsObject).RecNo - 1;
        Result := GetPseudoVariable(lVariable, lPieces[1]);
      end
      else
      begin
        lField := TDataSet(lVariable.VarValue.AsObject).FieldByName(lPieces[1]);
        case lField.DataType of
          ftInteger: Result := lField.AsInteger;
          ftLargeint: Result := lField.AsLargeInt;
          ftString, ftWideString: Result := lField.AsWideString;
          else
            Error('Invalid data type for field ' + lPieces[1]);
        end;
      end;
    end
    else if viListOfObject in lVariable.VarOption then
    begin
      if lHasMember and lPieces[1].StartsWith('@@') then
      begin
        Result := GetPseudoVariable(lVariable, lPieces[1]);
      end
      else
      begin
        Result := TTProRTTIUtils.GetProperty(WrapAsList(lVariable.VarValue.AsObject).GetItem(lVariable.VarIterator), lPieces[1]);
      end;
    end
    else if viObject in lVariable.VarOption then
    begin
      Result := TTProRTTIUtils.GetProperty(lVariable.VarValue.AsObject, lPieces[1]);
    end
    else if viSimpleType in lVariable.VarOption then
    begin
      if lVariable.VarValue.IsEmpty then
        Result := TValue.Empty
      else
        Result := lVariable.VarValue;
    end;
  end
  else
  begin
    Result := TValue.Empty;
  end;
end;

function TTProCompiledTemplate.GetVariables: TTProVariables;
begin
  if not Assigned(fVariables) then
  begin
    fVariables := TTProVariables.Create;
  end;
  Result := fVariables;
end;
function TTProCompiledTemplate.IsTruthy(const Value: TValue): Boolean;
var
  lStrValue: String;
begin
  lStrValue := Value.ToString;
  Result := not (SameText(lStrValue,'false') or SameText(lStrValue,'0') or SameText(lStrValue,''));
end;

function TTProCompiledTemplate.EvaluateIfExpression(aIdentifier: string): Boolean;
var
  lVarValue: TValue;
  lNegation: Boolean;
  lVariable: TVarInfo;
  lPieces: TArray<String>;
  lTmp: Boolean;
  lHasMember: Boolean;
  lList: ITProWrappedList;
begin
  lNegation := aIdentifier.StartsWith('!');
  if lNegation then
    aIdentifier := aIdentifier.Remove(0,1);

  lPieces := aIdentifier.Split(['.']);
  aIdentifier := lPieces[0];
  lHasMember := Length(lPieces) > 1;

  if GetVariables.TryGetValue(aIdentifier, lVariable) then
  begin
    if lVariable = nil then
    begin
      Exit(lNegation xor False);
    end;
    if viDataSet in lVariable.VarOption then
    begin
      if lHasMember then
      begin
        if lPieces[1].StartsWith('@@') then
        begin
          lVarValue := GetPseudoVariable(lVariable, lPieces[1]);
        end
        else
        begin
          lVarValue := TValue.From<Variant>(TDataSet(lVariable.VarValue.AsObject).FieldByName(lPieces[1]).Value);
        end;
        lTmp := IsTruthy(lVarValue);
      end
      else
      begin
        lTmp := not TDataSet(lVariable.VarValue.AsObject).Eof;
      end;
      Exit(lNegation xor lTmp);
    end
    else if viListOfObject in lVariable.VarOption then
    begin
      lList := WrapAsList(lVariable.VarValue.AsObject);
      if lHasMember then
      begin
        if lPieces[1].StartsWith('@@') then
        begin
          lVarValue := GetPseudoVariable(lVariable, lPieces[1]);
        end
        else
        begin
          lVarValue := TTProRTTIUtils.GetProperty(lList.GetItem(lVariable.VarIterator), lPieces[1]);
        end;
        lTmp := IsTruthy(lVarValue);
      end
      else
      begin
        lTmp := lList.Count > 0;
      end;

      if lNegation then
      begin
        Exit(not lTmp);
      end;
      Exit(lTmp);
    end
    else if viObject in lVariable.VarOption then
    begin
      Exit(lNegation xor Assigned(lVariable));
    end
    else if viSimpleType in lVariable.VarOption then
    begin
      lTmp := IsTruthy(lVariable.VarValue);
      Exit(lNegation xor lTmp)
    end;
  end;
  Exit(lNegation xor False);
end;

procedure TTProCompiledTemplate.SetData(const Name: String; Value: TValue);
var
  lWrappedList: ITProWrappedList;
begin
  if Value.IsEmpty then
  begin
    GetVariables.Add(Name, nil);
    Exit;
  end;

  case Value.Kind of
    tkClass:
    begin
      if Value.AsObject is TDataSet then
      begin
        GetVariables.Add(Name, TVarInfo.Create(Value.AsObject, [viDataSet], -1));
      end
      else
      begin
        if TTProDuckTypedList.CanBeWrappedAsList(Value.AsObject, lWrappedList) then
        begin
          GetVariables.Add(Name, TVarInfo.Create(TTProDuckTypedList(Value.AsObject), [viListOfObject], -1));
        end
        else
        begin
          GetVariables.Add(Name, TVarInfo.Create(Value.AsObject, [viObject], -1));
        end;
      end;
    end;
    tkInteger, tkString, tkUString, tkFloat, tkEnumeration : GetVariables.Add(Name, TVarInfo.Create(Value, [viSimpleType], -1));
    else
      raise ETProException.Create('Invalid type for variable "' + Name + '": ' + TRttiEnumerationType.GetName<TTypeKind>(Value.Kind));
  end;

end;


procedure TTProCompiledTemplate.ClearData;
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


//////////////////////
/// UTILS

class function TTProRTTIUtils.GetProperty(AObject: TObject; const APropertyName: string): TValue;
var
  Prop: TRttiProperty;
  ARttiType: TRttiType;
begin
  ARttiType := GlContext.GetType(AObject.ClassType);
  if not Assigned(ARttiType) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [ARttiType.ToString]);
  Prop := ARttiType.GetProperty(APropertyName);
  if not Assigned(Prop) then
    raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]', [ARttiType.ToString, APropertyName]);
  if Prop.IsReadable then
    Result := Prop.GetValue(AObject)
  else
    raise Exception.CreateFmt('Property is not readable [%s.%s]', [ARttiType.ToString, APropertyName]);
end;

{ TDuckTypedList }

procedure TTProDuckTypedList.Add(const AObject: TObject);
begin
  if not Assigned(FAddMethod) then
    raise ETProDuckTypingException.Create('Cannot find method "Add" in the Duck Object.');
  FAddMethod.Invoke(FObjectAsDuck, [AObject]);
end;

class function TTProDuckTypedList.CanBeWrappedAsList(const AInterfaceAsDuck: IInterface): Boolean;
begin
  Result := CanBeWrappedAsList(TObject(AInterfaceAsDuck));
end;

class function TTProDuckTypedList.CanBeWrappedAsList(const AObjectAsDuck: TObject): Boolean;
var
  lList: ITProWrappedList;
begin
  Result := CanBeWrappedAsList(AObjectAsDuck, lList);
end;

class function TTProDuckTypedList.CanBeWrappedAsList(const AObjectAsDuck: TObject; out AMVCList: ITProWrappedList): Boolean;
var
  List: ITProWrappedList;
begin
  List := TTProDuckTypedList.Create(AObjectAsDuck);
  Result := List.IsWrappedList;
  if Result then
    AMVCList := List;
end;

procedure TTProDuckTypedList.Clear;
begin
  if not Assigned(FClearMethod) then
    raise ETProDuckTypingException.Create('Cannot find method "Clear" in the Duck Object.');
  FClearMethod.Invoke(FObjectAsDuck, []);
end;

function TTProDuckTypedList.Count: Integer;
begin
  Result := 0;

  if (not Assigned(FGetCountMethod)) and (not Assigned(FCountProperty)) then
    raise ETProDuckTypingException.Create('Cannot find property/method "Count" in the Duck Object.');

  if Assigned(FCountProperty) then
    Result := FCountProperty.GetValue(FObjectAsDuck).AsInteger
  else if Assigned(FGetCountMethod) then
    Result := FGetCountMethod.Invoke(FObjectAsDuck, []).AsInteger;
end;

constructor TTProDuckTypedList.Create(const AInterfaceAsDuck: IInterface);
begin
  Create(TObject(AInterfaceAsDuck));
end;

constructor TTProDuckTypedList.Create(const AObjectAsDuck: TObject);
begin
  inherited Create;
  FObjectAsDuck := AObjectAsDuck;

  if not Assigned(FObjectAsDuck) then
    raise ETProDuckTypingException.Create('Duck Object can not be null.');

  FObjType := GlContext.GetType(FObjectAsDuck.ClassInfo);

  FAddMethod := nil;
  FClearMethod := nil;
  FGetItemMethod := nil;
  FGetCountMethod := nil;
  FCountProperty := nil;

  if IsWrappedList then
  begin
    FAddMethod := FObjType.GetMethod('Add');
    FClearMethod := FObjType.GetMethod('Clear');

{$IF CompilerVersion >= 23}
    if Assigned(FObjType.GetIndexedProperty('Items')) then
      FGetItemMethod := FObjType.GetIndexedProperty('Items').ReadMethod;

{$IFEND}
    if not Assigned(FGetItemMethod) then
      FGetItemMethod := FObjType.GetMethod('GetItem');

    if not Assigned(FGetItemMethod) then
      FGetItemMethod := FObjType.GetMethod('GetElement');

    FGetCountMethod := nil;
    FCountProperty := FObjType.GetProperty('Count');
    if not Assigned(FCountProperty) then
      FGetCountMethod := FObjType.GetMethod('Count');
  end;
end;

function TTProDuckTypedList.GetItem(const AIndex: Integer): TObject;
var
  lValue: TValue;
begin
  if not Assigned(FGetItemMethod) then
    raise ETProDuckTypingException.Create
      ('Cannot find method Indexed property "Items" or method "GetItem" or method "GetElement" in the Duck Object.');
  GetItemAsTValue(AIndex, lValue);

  if lValue.Kind = tkInterface then
  begin
    Exit(TObject(lValue.AsInterface));
  end;
  if lValue.Kind = tkClass then
  begin
    Exit(lValue.AsObject);
  end;
  raise ETProDuckTypingException.Create('Items in list can be only objects or interfaces');
end;

procedure TTProDuckTypedList.GetItemAsTValue(const AIndex: Integer;
  out AValue: TValue);
begin
  AValue := FGetItemMethod.Invoke(FObjectAsDuck, [AIndex]);
end;

function TTProDuckTypedList.IsWrappedList: Boolean;
var
  ObjectType: TRttiType;
begin
  ObjectType := GlContext.GetType(FObjectAsDuck.ClassInfo);

  Result := (ObjectType.GetMethod('Add') <> nil) and (ObjectType.GetMethod('Clear') <> nil)

{$IF CompilerVersion >= 23}
    and (ObjectType.GetIndexedProperty('Items') <> nil) and (ObjectType.GetIndexedProperty('Items').ReadMethod <> nil)

{$IFEND}
    and (ObjectType.GetMethod('GetItem') <> nil) or (ObjectType.GetMethod('GetElement') <> nil) and
    (ObjectType.GetProperty('Count') <> nil);
end;

function TTProDuckTypedList.ItemIsObject(const AIndex: Integer; out AValue: TValue): Boolean;
begin
  GetItemAsTValue(AIndex, AValue);
  Result := AValue.IsObject;
end;

class function TTProDuckTypedList.Wrap(const AObjectAsDuck: TObject): ITProWrappedList;
var
  List: ITProWrappedList;
begin
  if AObjectAsDuck is TTProDuckTypedList then
    Exit(TTProDuckTypedList(AObjectAsDuck));
  Result := nil;
  List := TTProDuckTypedList.Create(AObjectAsDuck);
  if List.IsWrappedList then
    Result := List;
end;


initialization
GlContext := TRttiContext.Create;

finalization
GlContext.Free;

end.
