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
  System.TypInfo,
  Data.DB,
  System.DateUtils,
  System.RTTI;

type
  ETProException = class(Exception)

  end;

  ETProCompilerException = class(ETProException)

  end;

  ETProRenderException = class(ETProException)

  end;

  ETProDuckTypingException = class(ETProException)

  end;

  TIfThenElseIndex = record
    IfIndex, ElseIndex: Int64;
  end;

  TTokenType = (
    ttContent, ttInclude, ttLoop, ttEndLoop, ttIfThen, ttElse, ttEndIf, ttStartTag, ttComment,
    ttLiteralString, ttEndTag, ttValue, ttFilterName, ttFilterParameter, ttReset, ttLineBreak, ttEOF);
  const
    TOKEN_TYPE_DESCR: array [Low(TTokenType)..High(TTokenType)] of string =
      ('ttContent', 'ttInclude', 'ttLoop', 'ttEndLoop', 'ttIfThen', 'ttElse', 'ttEndIf', 'ttStartTag', 'ttComment',
       'ttLiteralString', 'ttEndTag', 'ttValue', 'ttFilterName', 'ttFilterParameter', 'ttReset', 'ttLineBreak', 'ttEOF');
  type
    TToken = packed record
      TokenType: TTokenType;
      Value1: String;
      Value2: String;
      Ref1, Ref2: Int64;
      class function Create(TokType: TTokenType; Value1: String; Value2: String; Ref1: Int64 = -1; Ref2: Int64 = -1): TToken; static;
      function TokenTypeAsString: String;
      function ToString: String;
      procedure SaveToBytes(const aBytes: TBinaryWriter);
      class function CreateFromBytes(const aBytes: TBinaryReader): TToken; static;
    end;

  TTokenWalkProc = reference to procedure(const Index: Integer; const Token: TToken);

  TTProTemplateFunction = function(const aValue: TValue; const aParameters: TArray<string>): string;

  TTProVariablesInfo = (viSimpleType, viObject, viDataSet, viListOfObject, viJSONObject, viIterable);
  TTProVariablesInfos = set of TTProVariablesInfo;

  TVarDataSource = class
    VarValue: TValue;
    VarOption: TTProVariablesInfos;
//    VarIterator: Int64;
    constructor Create(const VarValue: TValue; const VarOption: TTProVariablesInfos);
  end;

  TTProVariables = class(TObjectDictionary<string, TVarDataSource>)
  public
    constructor Create;
  end;

  ITProCompiledTemplate = interface
    ['{0BE04DE7-6930-456B-86EE-BFD407BA6C46}']
    function Render: String;
    procedure ForEachToken(const TokenProc: TTokenWalkProc);
    procedure ClearData;
    procedure SetData(const Name: String; Value: TValue); overload;
    procedure AddFilter(const FunctionName: string; const FunctionImpl: TTProTemplateFunction);
    procedure DumpToFile(const FileName: String);
    procedure SaveToFile(const FileName: String);
  end;

  TLoopStackItem = class
    DataSourceName: String;
    LoopExpression: String;
    FullPath: String;
    IteratorName: String;
    IteratorPosition: Integer;
    function IncrementIteratorPosition: Integer;
    constructor Create(DataSourceName: String; LoopExpression: String; FullPath: String; IteratorName: String);
  end;

  TTProCompiledTemplate = class(TInterfacedObject, ITProCompiledTemplate)
  private
    fTokens: TList<TToken>;
    fVariables: TTProVariables;
    fTemplateFunctions: TDictionary<string, TTProTemplateFunction>;
    fLoopsStack: TObjectList<TLoopStackItem>;
    function PeekLoop: TLoopStackItem;
    procedure PopLoop;
    procedure PushLoop(const LoopStackItem: TLoopStackItem);
    function LoopStackIsEmpty: Boolean;
    function WalkThroughLoopStack(const VarName: String; out BaseVarName: String; out FullPath: String): Boolean;
    constructor Create(Tokens: TList<TToken>);
    procedure Error(const aMessage: String);
    function IsTruthy(const Value: TValue): Boolean;
    function GetVarAsString(const aName: string): string;
    function GetVarAsTValue(const aName: string): TValue;
    function EvaluateIfExpression(aIdentifier: string): Boolean;
    function GetVariables: TTProVariables;
    procedure SplitVariableName(const VariableWithMember: String; out VarName, VarMembers: String);
    function ExecuteFilter(aFunctionName: string; aParameters: TArray<string>; aValue: TValue): string;
    procedure CheckParNumber(const aHowManyPars: Integer; const aParameters: TArray<string>); overload;
    procedure CheckParNumber(const aMinParNumber, aMaxParNumber: Integer; const aParameters: TArray<string>); overload;
    function GetPseudoVariable(const VarIterator: Integer; const PseudoVarName: String): TValue; overload;
    function IsAnIterator(const VarName: String; out DataSourceName: String; out CurrentIterator: TLoopStackItem): Boolean;
  public
    destructor Destroy; override;
    function Render: String;
    procedure ForEachToken(const TokenProc: TTokenWalkProc);
    procedure ClearData;
    procedure SaveToFile(const FileName: String);
    class function CreateFromFile(const FileName: String): ITProCompiledTemplate;
    procedure SetData(const Name: String; Value: TValue); overload;
    procedure AddFilter(const FunctionName: string; const FunctionImpl: TTProTemplateFunction);
    procedure DumpToFile(const FileName: String);
  end;

  TTProCompiler = class
  strict private
    function MatchStartTag: Boolean;
    function MatchEndTag: Boolean;
    function MatchVariable(var aIdentifier: string): Boolean;
    function MatchFilterParamValue(var aParamValue: string): Boolean;
    function MatchSymbol(const aSymbol: string): Boolean;
    function MatchSpace: Boolean;
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
    function GetSubsequentText: String;
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

  TTProCompiledTemplateEvent = reference to procedure(const TemplateProCompiledTemplate: ITProCompiledTemplate);

  TTProConfiguration = class sealed
  private
    class var fOnCustomFiltersRegistration: TTProCompiledTemplateEvent;
  protected
    class procedure RegisterHandlers(const TemplateProCompiledTemplate: ITProCompiledTemplate);
  public
    class property OnCustomFiltersRegistration: TTProCompiledTemplateEvent read fOnCustomFiltersRegistration write fOnCustomFiltersRegistration;
  end;



function HTMLEncode(s: string): string;
function HTMLSpecialCharsEncode(s: string): string;


implementation

uses
  System.StrUtils, System.IOUtils, System.NetEncoding, System.Math,
  JsonDataObjects, MVCFramework.Nullables;

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

procedure TTProCompiledTemplate.AddFilter(const FunctionName: string; const FunctionImpl: TTProTemplateFunction);
begin
  fTemplateFunctions.Add(FunctionName.ToLower, FunctionImpl);
end;

//function TTProCompiledTemplate.GetPseudoVariable(const Variable: TVarDataSource; const PseudoVarName: String): TValue;
//begin
//  Result := GetPseudoVariable(Variable.VarIterator, PseudoVarName);
//end;

function TTProCompiledTemplate.GetPseudoVariable(const VarIterator: Integer; const PseudoVarName: String): TValue;
begin
  if PseudoVarName = '@@index' then
  begin
    Result := VarIterator + 1;
  end
  else if PseudoVarName = '@@odd' then
  begin
    Result := (VarIterator + 1) mod 2 > 0;
  end
  else if PseudoVarName = '@@even' then
  begin
    Result := (VarIterator + 1) mod 2 = 0;
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
      Error(Format('Expected %d parameters, got %d' , [aMinParNumber, lParNumber]))
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
    if aTokens[aTokens.Count - 1].TokenType <> ttEOF then
    begin
      Error('Included file ' + aFileNameRefPath + ' doesn''t terminate with EOF');
    end;
    aTokens.Delete(aTokens.Count - 1); // remove the EOF
  finally
    lCompiler.Free;
  end;
end;

procedure TTProCompiler.InternalMatchFilter(lIdentifier: String; var lStartVerbatim: UInt64; const CurrToken: TTokenType; aTokens: TList<TToken>; const lRef2: Integer);
var
  lFilterName: string;
  lFilterParamsCount: Integer;
  lFilterParams: TArray<String>;
  I: Integer;
begin
  lFilterName := '';
  lFilterParamsCount := -1; {-1 means "no filter applied to value"}
  if MatchSymbol('|') then
  begin
    if not MatchVariable(lFilterName) then
      Error('Invalid function name applied to variable or literal string "' + lIdentifier + '"');
    lFilterParams := GetFunctionParameters;
    lFilterParamsCount := Length(lFilterParams);
  end;

  if not MatchEndTag then
  begin
    Error('Expected end tag "' + END_TAG + '" near ' + GetSubsequentText);
  end;
  lStartVerbatim := fCharIndex;
  aTokens.Add(TToken.Create(CurrToken, lIdentifier, '', lFilterParamsCount, lRef2));

  //add function with params
  if not lFilterName.IsEmpty then
  begin
    aTokens.Add(TToken.Create(ttFilterName, lFilterName, '', lFilterParamsCount));
    if lFilterParamsCount > 0 then
    begin
      for I := 0 to lFilterParamsCount -1 do
      begin
        aTokens.Add(TToken.Create(ttFilterParameter, lFilterParams[I], ''));
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
        Error('Expected identifier after "' + aIdentifier + '" - got ' + GetSubsequentText);
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
  if MatchString(aParamValue) then
  begin
    Result := True;
  end;
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

function TTProCompiler.MatchSpace: Boolean;
begin
  Result := MatchSymbol(' ');
  while MatchSymbol(' ') do;
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
    lFileNameRefPath := TPath.Combine(TPath.GetDirectoryName(GetModuleName(HInstance)), 'main.template');
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
  TTProConfiguration.RegisterHandlers(Result);
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
  lIteratorName: string;
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
  lRef2: Integer;
  lContentOnThisLine: Integer;
begin
  lLastToken := ttEOF;
  lContentOnThisLine := 0;
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
        aTokens.Add(TToken.Create(lLastToken, fInputString.Substring(lStartVerbatim, lEndVerbatim - lStartVerbatim), ''));
      end;
      aTokens.Add(TToken.Create(ttEOF, '', ''));
      Break;
    end;

    if MatchSymbol(sLineBreak) then         {linebreak}
    begin
      lEndVerbatim := fCharIndex - Length(sLineBreak);
      if lEndVerbatim - lStartVerbatim > 0 then
      begin
        Inc(lContentOnThisLine);
        aTokens.Add(TToken.Create(ttContent, fInputString.Substring(lStartVerbatim, lEndVerbatim - lStartVerbatim), ''));
      end;
      lStartVerbatim := fCharIndex;
      if lLastToken = ttLineBreak then Inc(lContentOnThisLine);
      lLastToken := ttLineBreak;
      if lContentOnThisLine > 0 then
      begin
        aTokens.Add(TToken.Create(lLastToken, '', ''));
      end;
      Inc(fCurrentLine);
      lContentOnThisLine := 0;
    end else if MatchStartTag then         {starttag}
    begin
      lEndVerbatim := fCharIndex - Length(START_TAG);

      if lEndVerbatim - lStartVerbatim > 0 then
      begin
        lLastToken := ttContent;
        aTokens.Add(TToken.Create(lLastToken, fInputString.Substring(lStartVerbatim, lEndVerbatim - lStartVerbatim), ''));
      end;

      if CurrentChar = START_TAG[1] then
      begin
        lLastToken := ttContent;
        aTokens.Add(TToken.Create(lLastToken, START_TAG, ''));
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
            Error('Expected end tag "' + END_TAG + '" near ' + GetSubsequentText);
          end;
          lStartVerbatim := fCharIndex;
          lLastToken := ttValue;
          aTokens.Add(TToken.Create(lLastToken, lVarName, '', lFuncParamsCount, lRef2));
          Inc(lContentOnThisLine);

          //add function with params
          if not lFuncName.IsEmpty then
          begin
            aTokens.Add(TToken.Create(ttFilterName, lFuncName, '', lFuncParamsCount));
            if lFuncParamsCount > 0 then
            begin
              for I := 0 to lFuncParamsCount -1 do
              begin
                aTokens.Add(TToken.Create(ttFilterParameter, lFuncParams[I], ''));
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
          if not MatchSpace then
            Error('Expected "space" after "loop(' + lIdentifier + ')');
          if not MatchSymbol('as') then
            Error('Expected "as" after "loop(' + lIdentifier + ')');
          if not MatchSpace then
            Error('Expected <space> after "loop(' + lIdentifier + ') - EXAMPLE: loop(' + lIdentifier + ') as myalias');
          if not MatchVariable(lIteratorName) then
            Error('Expected iterator name after "loop" - EXAMPLE: loop(' + lIdentifier + ') as myalias');
          if not MatchEndTag then
            Error('Expected closing tag for "loop(' + lIdentifier + ')"');
          // create another element in the sections stack
          Inc(lCurrentSectionIndex);
          lSectionStack[lCurrentSectionIndex] := aTokens.Count;
          lLastToken := ttLoop;
          if lIdentifier = lIteratorName then
          begin
            Error('loop data source and its iterator cannot have the same name: ' + lIdentifier)
          end;
          aTokens.Add(TToken.Create(lLastToken, lIdentifier, lIteratorName));
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
          aTokens.Add(TToken.Create(lLastToken, '', '', lSectionStack[lCurrentSectionIndex]));

          // let the loop know where the endloop is
          lIndexOfLatestLoopStatement := lSectionStack[lCurrentSectionIndex];
          aTokens[lIndexOfLatestLoopStatement] :=
            TToken.Create(ttLoop,
              aTokens[lIndexOfLatestLoopStatement].Value1,
              aTokens[lIndexOfLatestLoopStatement].Value2,
              aTokens.Count - 1);

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
          aTokens.Add(TToken.Create(lLastToken, '', ''));

          // jumps handling...
          lIndexOfLatestIfStatement := lIfStatementStack[lCurrentIfIndex].IfIndex;

          //rewrite current "ifthen" references
          aTokens[lIndexOfLatestIfStatement] :=
            TToken.Create(ttIfThen,
              aTokens[lIndexOfLatestIfStatement].Value1,
              '',
              aTokens[lIndexOfLatestIfStatement].Ref1,
              aTokens.Count - 1); {ttIfThen.Ref2 points always to relative "endif"}

          if aTokens[lIndexOfLatestIfStatement].Ref1 > -1 then
          begin
            lIndexOfLatestElseStatement := aTokens[lIndexOfLatestIfStatement].Ref1;
            aTokens[lIndexOfLatestElseStatement] :=
              TToken.Create(ttElse,
                aTokens[lIndexOfLatestElseStatement].Value1,
                '',
                -1 {Ref1 is not used by ttElse},
                aTokens.Count - 1); {ttIfThen.Ref2 points always to relative "endif"}
          end;

          Dec(lCurrentIfIndex);
          lStartVerbatim := fCharIndex;
        end else if MatchSymbol('if') then
        begin
          MatchSpace;
          if not MatchSymbol('(') then
            Error('Expected "("');
          MatchSpace;
          lNegation := MatchSymbol('!');
          MatchSpace;
          if not MatchVariable(lIdentifier) then
            Error('Expected identifier after "if("');
          MatchSpace;
          if not MatchSymbol(')') then
            Error('Expected ")" after "' + lIdentifier + '"');
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "if(' + lIdentifier + ')"');
          if lNegation then
          begin
            lIdentifier := '!' + lIdentifier;
          end;
          lLastToken := ttIfThen;
          aTokens.Add(TToken.Create(lLastToken, lIdentifier, ''));
          Inc(lCurrentIfIndex);
          lIfStatementStack[lCurrentIfIndex].IfIndex := aTokens.Count - 1;
          lIfStatementStack[lCurrentIfIndex].ElseIndex := -1;
          lStartVerbatim := fCharIndex;
        end else if MatchSymbol('else') then
        begin
          if not MatchEndTag then
            Error('Expected closing tag for "else"');

          lLastToken := ttElse;
          aTokens.Add(TToken.Create(lLastToken, '', ''));

          // jumps handling...
          lIndexOfLatestIfStatement := lIfStatementStack[lCurrentIfIndex].IfIndex;
          lIfStatementStack[lCurrentIfIndex].ElseIndex := aTokens.Count - 1;
          aTokens[lIndexOfLatestIfStatement] := TToken.Create(ttIfThen,
            aTokens[lIndexOfLatestIfStatement].Value1,
            '',
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
          Inc(lContentOnThisLine);
          InternalCompileIncludedTemplate(lIncludeFileContent, aTokens, lCurrentFileName);
          lStartVerbatim := fCharIndex;
        end
//        else if MatchReset(lIdentifier) then  {reset}
//        begin
//          if not MatchEndTag then
//            Error('Expected closing tag');
//          lLastToken := ttReset;
//          aTokens.Add(TToken.Create(lLastToken, lIdentifier, ''));
//          lStartVerbatim := fCharIndex;
//        end
        else if MatchSymbol('exit') then {exit}
        begin
          lLastToken := ttEOF;
          aTokens.Add(TToken.Create(lLastToken, '', ''));
          Break;
        end
        else if MatchString(lStringValue) then {string}
        begin
          lLastToken := ttLiteralString;
          Inc(lContentOnThisLine);
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
          lLastToken := ttComment; {will not added into compiled template}
        end
        else
        begin
          lIdentifier := GetSubsequentText;
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
  raise ETProCompilerException.CreateFmt('%s - at line %d in file %s', [aMessage, fCurrentLine, fCurrentFileName]);
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

function TTProCompiler.GetSubsequentText: String;
var
  I: Integer;
begin
  Result := CurrentChar;
  Step;
  I := 0;
  while (CurrentChar <> #0) and (CurrentChar <> END_TAG[1]) and (I<20) do
  begin
    Result := Result + CurrentChar;
    Step;
    Inc(I);
  end;
  Result := Result.QuotedString('"');
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
  Result := HTMLSpecialCharsEncode(s);
end;

function HTMLSpecialCharsEncode(s: string): string;
  procedure repl(var s: string; r: string; posi: Integer);
  begin
    Delete(s, posi, 1);
    Insert(r, s, posi);
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
      Ord('>'):
        r := 'gt';
      Ord('<'):
        r := 'lt';
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

class function TToken.Create(TokType: TTokenType; Value1, Value2: String; Ref1: Int64; Ref2: Int64): TToken;
begin
  Result.TokenType:= TokType;
  Result.Value1 := Value1;
  Result.Value2 := Value2;
  Result.Ref1 := Ref1;
  Result.Ref2 := Ref2;
end;

class function TToken.CreateFromBytes(const aBytes: TBinaryReader): TToken;
var
  //lSize: UInt32;
  lValue1Size: UInt32;
  lValue2Size: UInt32;
  lTokenAsByte: Byte;
begin
  {
    STORAGE FORMAT

    Bytes
    0:    Total record size as UInt32
    1:    Token Type as Byte
    2:    Value1 Size in bytes as UInt32
    3:    Value1 bytes
    4:    Value2 Size in bytes as UInt32
    5:    Value2 bytes
    6:    Ref1 (8 bytes) in bytes - Int64
    7:    Ref1 (8 bytes) in bytes - Int64
  }

  //lSize := aBytes.ReadUInt32;
  lTokenAsByte := aBytes.ReadByte;
  Result.TokenType := TTokenType(lTokenAsByte);
  lValue1Size := aBytes.ReadUInt32;
  Result.Value1 := TEncoding.Unicode.GetString(aBytes.ReadBytes(lValue1Size));

  lValue2Size := aBytes.ReadUInt32;
  Result.Value2 := TEncoding.Unicode.GetString(aBytes.ReadBytes(lValue2Size));

  Result.Ref1 := aBytes.ReadInt64;
  Result.Ref2 := aBytes.ReadInt64;
end;

procedure TToken.SaveToBytes(const aBytes: TBinaryWriter);
var
//  lSize: UInt32;
  lValue1Bytes: TArray<Byte>;
  lValue2Bytes: TArray<Byte>;
  lValue1Length: UInt32;
  lValue2Length: UInt32;
  lTokenAsByte: Byte;
begin

//  lSize :=
//    SizeOf(UInt32) + {total record size}
//    1 +  //Token Type as Byte
//    4 +  //Value1 Size in bytes as UInt32
//    Length(Value1) * SizeOf(Char) + //value1 bytes
//    4 +  //Value2 Size in bytes as UInt32
//    Length(Value2) * SizeOf(Char) + //value2 bytes
//    8 + //ref1
//    8;  //ref2
  //aBytes.Write(lSize);

  lTokenAsByte := Byte(TokenType);
  aBytes.Write(lTokenAsByte);

  lValue1Bytes := TEncoding.Unicode.GetBytes(Value1);
  lValue1Length := UInt16(Length(lValue1Bytes));
  aBytes.Write(lValue1Length);
  aBytes.Write(lValue1Bytes);

  lValue2Bytes := TEncoding.Unicode.GetBytes(Value2);
  lValue2Length := UInt16(Length(lValue2Bytes));
  aBytes.Write(lValue2Length);
  aBytes.Write(lValue2Bytes);

  aBytes.Write(Ref1);

  aBytes.Write(Ref2);
end;

function TToken.TokenTypeAsString: String;
begin
  Result := TOKEN_TYPE_DESCR[self.TokenType];
end;

function TToken.ToString: String;
begin
  Result := Format('%15s | Ref1: %8d | Ref2: %8d | Val1: %-20s| Val2: %-20s',[TokenTypeAsString, Ref1, Ref2, Value1, Value2]);
end;

{ TTProCompiledTemplate }

constructor TTProCompiledTemplate.Create(Tokens: TList<TToken>);
begin
  inherited Create;
  fLoopsStack := TObjectList<TLoopStackItem>.Create(True);
  fTokens := Tokens;
  fTemplateFunctions := TDictionary<string, TTProTemplateFunction>.Create;
end;

class function TTProCompiledTemplate.CreateFromFile(
  const FileName: String): ITProCompiledTemplate;
var
  lBR: TBinaryReader;
  lTokens: TList<TToken>;
begin
  lBR := TBinaryReader.Create(TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone), nil, True);
  try
    lTokens := TList<TToken>.Create;
    try
      while True do
      begin
        lTokens.Add(TToken.CreateFromBytes(lBR));
        if lTokens.Last.TokenType = ttEOF then
        begin
          Break;
        end;
      end;
      Result := TTProCompiledTemplate.Create(lTokens);
    except
      lTokens.Free;
      raise;
    end;
  finally
    lBR.Free;
  end;
end;

destructor TTProCompiledTemplate.Destroy;
begin
  fLoopsStack.Free;
  fTemplateFunctions.Free;
  fTokens.Free;
  fVariables.Free;
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
  lLoopStmIndex: Integer;
  lDataSourceName: string;
  lCurrTokenType: TTokenType;
  lVariable: TVarDataSource;
  lWrapped: ITProWrappedList;
  lJumpTo: Integer;
  lFilterParCount: Integer;
  lFilterParameters: TArray<String>;
  I: Integer;
  lFilterName: string;
  lVarName: string;
  lVarValue: String;
  lRef2: Integer;
  lJArr: TJDOJsonArray;
  lJObj: TJDOJsonObject;
  lVarMember: string;
  lBaseVarName: string;
  lFullPath: string;
  lLoopItem: TLoopStackItem;
  lJValue: TJsonDataValueHelper;
begin
  lBuff := TStringBuilder.Create;
  try
    lIdx := 0;
    while fTokens[lIdx].TokenType <> ttEOF do
    begin
      case fTokens[lIdx].TokenType of
        ttContent: begin
          lBuff.Append(fTokens[lIdx].Value1);
        end;
        ttLoop: begin
          lLoopItem := PeekLoop;
          if LoopStackIsEmpty or (lLoopItem.LoopExpression <> fTokens[lIdx].Value1) then
          begin //push a new loop stack item
            SplitVariableName(fTokens[lIdx].Value1, lVarName, lVarMember);
            {lVarName maybe an iterator, so I've to walk the stack to know
             the real information about the iterator}
            if WalkThroughLoopStack(lVarName, lBaseVarName, lFullPath) then
            begin
              lFullPath := lFullPath + '.' + lVarMember;
              PushLoop(TLoopStackItem.Create(lBaseVarName, fTokens[lIdx].Value1, lFullPath, fTokens[lIdx].Value2));
            end
            else
            begin
              PushLoop(TLoopStackItem.Create(lVarName, fTokens[lIdx].Value1, lVarMember, fTokens[lIdx].Value2));
            end;
          end;
          lLoopItem := PeekLoop;

          // Now, work with the stack head
          if GetVariables.TryGetValue(PeekLoop.DataSourceName, lVariable) then
          begin
            if lLoopItem.FullPath.IsEmpty then
            begin
              if not (viIterable in lVariable.VarOption) then
              begin
                Error(Format('Cannot iterate over a not iterable object [%s]', [fTokens[lIdx].Value1]));
              end;
            end;

            if viDataSet in lVariable.VarOption then
            begin
              if lLoopItem.IteratorPosition = -1 then
              begin
                TDataset(lVariable.VarValue.AsObject).First;
              end;

              if TDataset(lVariable.VarValue.AsObject).Eof then
              begin
                lIdx := fTokens[lIdx].Ref1; //skip to endloop
                Continue;
              end
            end else if viListOfObject in lVariable.VarOption then
            begin
              lWrapped := WrapAsList(lVariable.VarValue.AsObject);
              //if lVariable.VarIterator = lWrapped.Count - 1 then
              if lLoopItem.IteratorPosition = lWrapped.Count - 1 then
              begin
                lIdx := fTokens[lIdx].Ref1; //skip to endif
                Continue;
              end
              else
              begin
                PeekLoop.IncrementIteratorPosition; // lVariable.VarIterator := lVariable.VarIterator + 1;
              end;
            end else if viJSONObject in lVariable.VarOption then
            begin
              lJObj := TJDOJsonObject(lVariable.VarValue.AsObject);
              lLoopItem := PeekLoop;
              lJValue := lJObj.Path[lLoopItem.FullPath];

              case lJValue.Typ of
                jdtNone: begin
                  lIdx := fTokens[lIdx].Ref1; //skip to endloop
                  Continue;
                end;

                jdtArray: begin
                  if  lLoopItem.IteratorPosition = lJObj.Path[lLoopItem.FullPath].ArrayValue.Count - 1 then
                  begin
                    lIdx := fTokens[lIdx].Ref1; //skip to endloop
                    Continue;
                  end
                  else
                  begin
                    lLoopItem.IncrementIteratorPosition;
                  end;
                end;

                else
                begin
                  Error('Only JSON array can be iterated');
                end;
              end;
            end
            else
            begin
              Error('Iteration not allowed for "' + fTokens[lIdx].Value1 + '"');
            end;
          end
          else
          begin
            Error(Format('Unknown variable in loop statement [%s]', [fTokens[lIdx].Value1]));
          end;
        end;
        ttEndLoop: begin
          if LoopStackIsEmpty then
          begin
            raise ETProRenderException.Create('Inconsistent "endloop"');
          end;

          lLoopItem := PeekLoop;
          lLoopStmIndex := fTokens[lIdx].Ref1;
          lDataSourceName := lLoopItem.DataSourceName;
          if GetVariables.TryGetValue(lDataSourceName, lVariable) then
          begin
            if viDataSet in lVariable.VarOption then
            begin
              TDataset(lVariable.VarValue.AsObject).Next;
              lLoopItem.IteratorPosition := TDataset(lVariable.VarValue.AsObject).RecNo;
              if not TDataset(lVariable.VarValue.AsObject).Eof then
              begin
                lIdx := fTokens[lIdx].Ref1; //goto loop
                Continue;
              end
              else
              begin
                PopLoop;
              end;
            end
            else if viJSONObject in lVariable.VarOption then
            begin
              lJObj := TJDOJsonObject(lVariable.VarValue.AsObject);
              lJArr := lJObj.Path[lLoopItem.FullPath];
              if lLoopItem.IteratorPosition < lJArr.Count - 1 then
              begin
                lIdx := fTokens[lIdx].Ref1; //skip to loop
                Continue;
              end
              else
              begin
                PopLoop;
              end;
            end
            else if viListOfObject in lVariable.VarOption then
            begin
              lWrapped := TTProDuckTypedList.Wrap(lVariable.VarValue.AsObject);
              if lLoopItem.IteratorPosition < lWrapped.Count - 1 then
              begin
                lIdx := fTokens[lIdx].Ref1; //skip to loop
                Continue;
              end
              else
              begin
                PopLoop;
              end;
            end;
          end;
        end;
        ttIfThen: begin
          if EvaluateIfExpression(fTokens[lIdx].Value1) then
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
          if fTokens[lIdx].Ref1 > -1 {has a filter with Ref1 parameters} then
          begin
            lVarName := fTokens[lIdx].Value1;
            Inc(lIdx);
            lFilterName := fTokens[lIdx].Value1;
            lFilterParCount := fTokens[lIdx].Ref1;  // parameter count
            SetLength(lFilterParameters, lFilterParCount);
            for I := 0 to lFilterParCount - 1 do
            begin
              Inc(lIdx);
              Assert(fTokens[lIdx].TokenType = ttFilterParameter);
              lFilterParameters[I] := fTokens[lIdx].Value1;
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
              lVarValue := GetVarAsString(fTokens[lIdx].Value1);
            end
            else
            begin
              lVarValue := fTokens[lIdx].Value1;
            end;
          end;
          if lRef2 = -1 {encoded} then
            lBuff.Append(HTMLEncode(lVarValue))
          else
            lBuff.Append(lVarValue);
        end;
//        ttReset: begin
//          if GetVariables.TryGetValue(fTokens[lIdx].Value1, lVariable) then
//          begin
//            if viDataSet in lVariable.VarOption then
//            begin
//              TDataset(lVariable.VarValue.AsObject).First;
//            end
//            else if viListOfObject in lVariable.VarOption then
//            begin
//              //do nothing
//            end;
//            lVariable.VarIterator := -1;
//          end
//          else
//          begin
//            Error('Unknown variable in "reset(' + fTokens[lIdx].Value1 + ''')');
//          end;
//        end;
        ttLineBreak: begin
          lBuff.AppendLine;
        end;
        else
        begin
          Error('Invalid token: ' + fTokens[lIdx].TokenTypeAsString);
        end;
      end;
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
  lIsObject: Boolean;
  lAsObject: TObject;
begin
  lValue := GetVarAsTValue(aName);
  if lValue.IsEmpty then
  begin
    Exit('');
  end;

  lIsObject := False;
  lAsObject := nil;
  if lValue.IsObject then
  begin
    lIsObject := True;
    lAsObject := lValue.AsObject;
  end;

  if lIsObject then
  begin
    if lAsObject is TField then
      Result := TField(lValue.AsObject).AsString
    else if lAsObject is TJsonBaseObject then
      Result := TJsonBaseObject(lAsObject).ToJSON()
    else
      Result := lAsObject.ToString;
  end
  else
  begin
    if lValue.TypeInfo.Kind = tkRecord then
    begin
      if lValue.TypeInfo = TypeInfo(NullableInt32) then
      begin
        Result := lValue.AsType<NullableInt32>.Value.ToString;
      end
      else if lValue.TypeInfo = TypeInfo(NullableUInt32) then
      begin
        Result := lValue.AsType<NullableInt32>.Value.ToString;
      end
      else if lValue.TypeInfo = TypeInfo(NullableInt16) then
      begin
        Result := lValue.AsType<NullableInt16>.Value.ToString;
      end
      else if lValue.TypeInfo = TypeInfo(NullableUInt16) then
      begin
        Result := lValue.AsType<NullableUInt16>.Value.ToString;
      end
      else if lValue.TypeInfo = TypeInfo(NullableInt64) then
      begin
        Result := lValue.AsType<NullableInt64>.Value.ToString;
      end
      else if lValue.TypeInfo = TypeInfo(NullableUInt64) then
      begin
        Result := lValue.AsType<NullableUInt64>.Value.ToString;
      end
      else if lValue.TypeInfo = TypeInfo(NullableString) then
      begin
        Result := lValue.AsType<NullableString>.Value;
      end
      else if lValue.TypeInfo = TypeInfo(NullableCurrency) then
      begin
        Result := lValue.AsType<NullableCurrency>.Value.ToString;
      end
      else if lValue.TypeInfo = TypeInfo(NullableBoolean) then
      begin
        Result := lValue.AsType<NullableBoolean>.Value.ToString;
      end
      else if lValue.TypeInfo = TypeInfo(NullableTDate) then
      begin
        Result := DateToISO8601(lValue.AsType<NullableTDate>.Value);
      end
      else if lValue.TypeInfo = TypeInfo(NullableTTime) then
      begin
        Result := DateToISO8601(lValue.AsType<NullableTTime>.Value);
      end
      else if lValue.TypeInfo = TypeInfo(NullableTDateTime) then
      begin
        Result := DateToISO8601(lValue.AsType<NullableTDateTime>.Value);
      end
      else
      begin
        raise ETProException.Create('Unsupported type for variable "' + aName + '"');
      end;
    end
    else
    begin
      Result := lValue.ToString;
    end;
  end;
end;

function TTProCompiledTemplate.GetVarAsTValue(const aName: string): TValue;
var
  lVariable: TVarDataSource;
  lField: TField;
  lHasMember: Boolean;
  lJPath: string;
  lDataSource: string;
  lIsAnIterator: Boolean;
  lJObj: TJDOJsonObject;
  lVarName: string;
  lVarMembers: string;
  lCurrentIterator: TLoopStackItem;
  lPJSONDataValue: TJsonDataValueHelper;
begin
  lCurrentIterator := nil;
  SplitVariableName(aName, lVarName, lVarMembers);
  lHasMember := not lVarMembers.IsEmpty;
  lIsAnIterator := IsAnIterator(lVarName, lDataSource, lCurrentIterator);

  if not lIsAnIterator then
  begin
    lDataSource := lVarName;
  end;

  if GetVariables.TryGetValue(lDataSource, lVariable) then
  begin
    if lVariable = nil then
    begin
      Exit(nil);
    end;
    if viDataSet in lVariable.VarOption then
    begin
      if not lIsAnIterator then
      begin
        Error(lDataSource + ' is not an iterator');
      end;

      if lHasMember and lVarMembers.StartsWith('@@') then
      begin
        lCurrentIterator.IteratorPosition := TDataSet(lVariable.VarValue.AsObject).RecNo - 1;
        //lVariable.VarIterator := TDataSet(lVariable.VarValue.AsObject).RecNo - 1;
        Result := GetPseudoVariable(lCurrentIterator.IteratorPosition, lVarMembers);
      end
      else
      begin
        lField := TDataSet(lVariable.VarValue.AsObject).FieldByName(lVarMembers);
        case lField.DataType of
          ftInteger: Result := lField.AsInteger;
          ftLargeint, ftAutoInc: Result := lField.AsLargeInt;
          ftString, ftWideString, ftMemo, ftWideMemo: Result := lField.AsWideString;
          else
            Error('Invalid data type for field "' + lVarMembers + '": ' + TRttiEnumerationType.GetName<TFieldType>(lField.DataType));
        end;
      end;
    end
    else if viJSONObject in lVariable.VarOption then
    begin
      lJObj := TJDOJsonObject(lVariable.VarValue.AsObject);

      if lIsAnIterator then
      begin
        if lVarMembers.StartsWith('@@') then
        begin
          Result := GetPseudoVariable(lCurrentIterator.IteratorPosition, lVarMembers);
        end
        else
        begin
          lJPath := lCurrentIterator.FullPath;
          lPJSONDataValue := lJObj.Path[lJPath].ArrayValue[lCurrentIterator.IteratorPosition];
          if lPJSONDataValue.Typ in [jdtArray, jdtObject] then
          begin
            if not lVarMembers.IsEmpty then
              lPJSONDataValue := lPJSONDataValue.Path[lVarMembers];
            case lPJSONDataValue.Typ of
              jdtArray: begin
                Result := lPJSONDataValue.ArrayValue.ToJSON();
              end;
              jdtObject: begin
                Result := lPJSONDataValue.ObjectValue.ToJSON();
              end;
              else
                Result := lPJSONDataValue.Value;
            end;
          end
          else
          begin
            if lVarMembers.IsEmpty then
              Result := lPJSONDataValue.Value
            else
              Result := '';
          end;
        end;
      end
      else
      begin
        lJPath := aName.Remove(0, Length(lVarName) + 1);
        if lJPath.IsEmpty then
          Result := lJObj
        else
          Result := lJObj.Path[lJPath].Value;
      end;
    end
    else if viListOfObject in lVariable.VarOption then
    begin
      if lVarMembers.StartsWith('@@') then
      begin
        Result := GetPseudoVariable(lCurrentIterator.IteratorPosition, lVarMembers);
      end
      else
      begin
        if lIsAnIterator then
        begin
          if lHasMember then
            Result := TTProRTTIUtils.GetProperty(WrapAsList(lVariable.VarValue.AsObject).GetItem(lCurrentIterator.IteratorPosition), lVarMembers)
          else
            Result := WrapAsList(lVariable.VarValue.AsObject).GetItem(lCurrentIterator.IteratorPosition);
        end
        else
        begin
          if lHasMember then
            Error(lDataSource + ' can be used only with filters or iterated using its alias')
          else
          begin
            Result := lVariable.VarValue.AsObject;
          end;
        end;
      end;
    end
    else if viObject in lVariable.VarOption then
    begin
      if lHasMember then
        Result := TTProRTTIUtils.GetProperty(lVariable.VarValue.AsObject, lVarMembers)
      else
        Result := lVariable.VarValue;
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

function TTProCompiledTemplate.IsAnIterator(const VarName: String; out DataSourceName: String; out CurrentIterator: TLoopStackItem): Boolean;
var
  I: Integer;
begin
  Result := False;
  if not LoopStackIsEmpty then {search datasource using current iterators stack}
  begin
    for I := fLoopsStack.Count - 1 downto 0 do
    begin
      if fLoopsStack[I].IteratorName = VarName then
      begin
        Result := True;
        DataSourceName := fLoopsStack[I].DataSourceName;
        CurrentIterator := fLoopsStack[I];
        Break;
      end;
    end;
  end;
end;

function TTProCompiledTemplate.IsTruthy(const Value: TValue): Boolean;
var
  lStrValue: String;
begin
  lStrValue := Value.ToString;
  Result := not (SameText(lStrValue,'false') or SameText(lStrValue,'0') or SameText(lStrValue,''));
end;

function TTProCompiledTemplate.LoopStackIsEmpty: Boolean;
begin
  Result := fLoopsStack.Count = 0;
end;

function TTProCompiledTemplate.PeekLoop: TLoopStackItem;
begin
  if fLoopsStack.Count = 0 then
  begin
    Result := nil;
  end
  else
  begin
    Result := fLoopsStack.Last;
  end;
end;

procedure TTProCompiledTemplate.PopLoop;
begin
  fLoopsStack.Delete(fLoopsStack.Count - 1);
end;

procedure TTProCompiledTemplate.PushLoop(const LoopStackItem: TLoopStackItem);
begin
  fLoopsStack.Add(LoopStackItem);
end;

function TTProCompiledTemplate.EvaluateIfExpression(aIdentifier: string): Boolean;
var
  lVarValue: TValue;
  lNegation: Boolean;
  lVariable: TVarDataSource;
  lTmp: Boolean;
  lDataSourceName: String;
  lHasMember: Boolean;
  lList: ITProWrappedList;
  lVarName, lVarMembers: String;
  lCurrentIterator: TLoopStackItem;
  lIsAnIterator: Boolean;
begin
  lNegation := aIdentifier.StartsWith('!');
  if lNegation then
    aIdentifier := aIdentifier.Remove(0,1);

  SplitVariableName(aIdentifier, lVarName, lVarMembers);

  lHasMember := Length(lVarMembers) > 0;

  lIsAnIterator := IsAnIterator(lVarName, lDataSourceName, lCurrentIterator);

  if not lIsAnIterator then
  begin
    lDataSourceName := lVarName;
  end;

  if GetVariables.TryGetValue(lDataSourceName, lVariable) then
  begin
    if lVariable = nil then
    begin
      Exit(lNegation xor False);
    end;
    if viDataSet in lVariable.VarOption then
    begin
      if lHasMember then
      begin
        if lVarMembers.StartsWith('@@') then
        begin
          if not lIsAnIterator then
          begin
            Error('Pseudovariables (@@) can be used only on iterators');
          end;
          lVarValue := GetPseudoVariable(lCurrentIterator.IteratorPosition, lVarMembers);
        end
        else
        begin
          lVarValue := TValue.From<Variant>(TDataSet(lVariable.VarValue.AsObject).FieldByName(lVarMembers).Value);
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
        if lVarMembers.StartsWith('@@') then
        begin
          lVarValue := GetPseudoVariable(lCurrentIterator.IteratorPosition, lVarMembers);
        end
        else
        begin
          lVarValue := TTProRTTIUtils.GetProperty(lList.GetItem(lCurrentIterator.IteratorPosition), lVarMembers);
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
    else if [viObject, viJSONObject] * lVariable.VarOption <> [] then
    begin
      if lHasMember then
      begin
        if lVarMembers.StartsWith('@@') then
        begin
          lVarValue := GetPseudoVariable(lCurrentIterator.IteratorPosition, lVarMembers);
        end
        else
        begin
          lVarValue := GetVarAsTValue(lDataSourceName);
        end;
        lTmp := IsTruthy(lVarValue);
      end
      else
      begin
        lTmp := not lVarValue.IsEmpty;
      end;
      if lNegation then
      begin
        Exit(not lTmp);
      end;
      Exit(lTmp);
    end
    else if viSimpleType in lVariable.VarOption then
    begin
      lTmp := IsTruthy(lVariable.VarValue);
      Exit(lNegation xor lTmp)
    end;
  end;
  Exit(lNegation xor False);
end;

procedure TTProCompiledTemplate.SaveToFile(const FileName: String);
var
  lToken: TToken;
  lBW: TBinaryWriter;
begin
  lBW := TBinaryWriter.Create(TFileStream.Create(FileName, fmCreate or fmOpenWrite or fmShareDenyNone), nil, True);
  try
    for lToken in fTokens do
    begin
      lToken.SaveToBytes(lBW);
    end;
  finally
    lBW.Free;
  end;
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
        GetVariables.Add(Name, TVarDataSource.Create(Value.AsObject, [viDataSet, viIterable]));
      end
      else
      if Value.AsObject is TJDOJsonObject then
      begin
        GetVariables.Add(Name, TVarDataSource.Create(TJDOJsonObject(Value.AsObject), [viJSONObject]));
      end
      else
      if Value.AsObject is TJDOJsonObject then
      begin
        GetVariables.Add(Name, TVarDataSource.Create(TJDOJsonObject(Value.AsObject), [viJSONObject]));
      end
      else
      begin
        if TTProDuckTypedList.CanBeWrappedAsList(Value.AsObject, lWrappedList) then
        begin
          GetVariables.Add(Name, TVarDataSource.Create(TTProDuckTypedList(Value.AsObject), [viListOfObject, viIterable]));
        end
        else
        begin
          GetVariables.Add(Name, TVarDataSource.Create(Value.AsObject, [viObject]));
        end;
      end;
    end;
    tkInteger, tkString, tkUString, tkFloat, tkEnumeration : GetVariables.Add(Name, TVarDataSource.Create(Value, [viSimpleType]));
    else
      raise ETProException.Create('Invalid type for variable "' + Name + '": ' + TRttiEnumerationType.GetName<TTypeKind>(Value.Kind));
  end;

end;


procedure TTProCompiledTemplate.SplitVariableName(
  const VariableWithMember: String; out VarName, VarMembers: String);
var
  lDotPos: Integer;
begin
  VarName := VariableWithMember;
  VarMembers := '';
  lDotPos := VarName.IndexOf('.');
  if lDotPos > -1 then
  begin
    VarName := VariableWithMember.Substring(0, lDotPos);
    VarMembers := VariableWithMember.Substring(lDotPos + 1);
  end;
end;

function TTProCompiledTemplate.WalkThroughLoopStack(const VarName: String;
  out BaseVarName, FullPath: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := fLoopsStack.Count - 1 downto 0 do
  begin
    if VarName = fLoopsStack[I].IteratorName then
    begin
//      deve ritornare sempre un array
      BaseVarName := fLoopsStack[I].DataSourceName;
      FullPath := fLoopsStack[I].FullPath + '[' + fLoopsStack[I].IteratorPosition.ToString + ']';
      Result := True;
    end;
  end;
end;

procedure TTProCompiledTemplate.ClearData;
begin
  GetVariables.Clear;
end;

{ TVarInfo }

constructor TVarDataSource.Create(const VarValue: TValue;
  const VarOption: TTProVariablesInfos);
begin
  Self.VarValue := VarValue;
  Self.VarOption := VarOption;
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


{ TLoopStackItem }

constructor TLoopStackItem.Create(DataSourceName, LoopExpression,
  FullPath: String; IteratorName: String);
begin
  Self.DataSourceName := DataSourceName;
  Self.LoopExpression := LoopExpression;
  Self.FullPath := FullPath;
  Self.IteratorName := IteratorName;
  Self.IteratorPosition := -1;
end;

function TLoopStackItem.IncrementIteratorPosition: Integer;
begin
  Inc(IteratorPosition);
  Result := IteratorPosition;
end;

{ TTProConfiguration }

class procedure TTProConfiguration.RegisterHandlers(const TemplateProCompiledTemplate: ITProCompiledTemplate);
begin
  if Assigned(fOnCustomFiltersRegistration) then
  begin
    fOnCustomFiltersRegistration(TemplateProCompiledTemplate);
  end;
end;

initialization
GlContext := TRttiContext.Create;
JsonSerializationConfig.LineBreak := sLineBreak;

finalization
GlContext.Free;

end.
