// ***************************************************************************
//
// Copyright (c) 2016-2026 Daniele Teti
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

unit TemplatePro.Types;

interface

uses
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Classes,
  System.SysUtils,
  System.RTTI;

const
  TEMPLATEPRO_VERSION = '1.1';

type
  ETProException = class(Exception)

  end;

  ETProCompilerException = class(ETProException)

  end;

  ETProRenderException = class(ETProException)

  end;

  ETProDuckTypingException = class(ETProException)

  end;

  TLineEndingStyle = (lesLF, lesCRLF, lesCR, lesNative);

  TIfThenElseIndex = record
    IfIndex, ElseIndex: Int64;
  end;

  TForElseIndex = record
    ForIndex, ElseIndex: Int64;
  end;

  TTokenType = (ttContent, ttInclude, ttFor, ttEndFor, ttForElse, ttIfThen, ttBoolExpression, ttElse, ttEndIf, ttStartTag, ttComment, ttJump, ttBlock,
    ttEndBlock, ttInherited, ttContinue, ttLiteralString, ttEndTag, ttValue, ttFilterName, ttFilterParameter, ttLineBreak, ttSystemVersion, ttExit,
    ttEOF, ttInfo, ttMacro, ttEndMacro, ttCallMacro, ttMacroParam, ttExpression, ttSet, ttIncludeStart, ttIncludeEnd, ttAutoescape, ttEndAutoescape);

const
  TOKEN_TYPE_DESCR: array [Low(TTokenType) .. High(TTokenType)] of string = ('ttContent', 'ttInclude', 'ttFor', 'ttEndFor', 'ttForElse', 'ttIfThen',
    'ttBoolExpression', 'ttElse', 'ttEndIf', 'ttStartTag', 'ttComment', 'ttJump', 'ttBlock', 'ttEndBlock', 'ttInherited', 'ttContinue', 'ttLiteralString',
    'ttEndTag', 'ttValue', 'ttFilterName', 'ttFilterParameter', 'ttLineBreak', 'ttSystemVersion', 'ttExit', 'ttEOF', 'ttInfo', 'ttMacro',
    'ttEndMacro', 'ttCallMacro', 'ttMacroParam', 'ttExpression', 'ttSet', 'ttIncludeStart', 'ttIncludeEnd', 'ttAutoescape', 'ttEndAutoescape');

const
  { ttInfo value1 can be: }
  STR_BEGIN_OF_LAYOUT = 'begin_of_layout';
  STR_END_OF_LAYOUT = 'end_of_layout';

type
{$IF not defined(RIOORBETTER)}
  PValue = ^TValue;
{$ENDIF}

  TFilterParameterType = (fptInteger, fptFloat, fptString, fptVariable, fptExpression);
  TFilterParameterTypes = set of TFilterParameterType;

  TFilterParameter = record
    { can be number, string or variable }
    ParType: TFilterParameterType;
    { contains the literal string if partype = string,
      contains the variable name if partype = variable }
    ParStrText: String;
    { contains the literal integer if partype = integer }
    ParIntValue: Integer;
    { contains the literal float if partype = float }
    ParFloatValue: Extended;
  end;

  PFilterParameter = ^TFilterParameter;

  TFilterInfo = record
    FilterName: String;
    Parameters: TArray<TFilterParameter>;
  end;

  TToken = packed record
    TokenType: TTokenType;
    Value1: String;
    Value2: String;
    Ref1: Int64;
    Ref2: Int64; { in case of tokentype = filter, contains the integer value, if any }
    class function Create(TokType: TTokenType; Value1: String; Value2: String; Ref1: Int64 = -1; Ref2: Int64 = -1): TToken; static;
    function TokenTypeAsString: String;
    function ToString: String;
    procedure SaveToBytes(const aBytes: TBinaryWriter);
    class function CreateFromBytes(const aBytes: TBinaryReader): TToken; static;
  end;

  TBlockAddress = record
    BeginBlockAddress, EndBlockAddress: Int64;
    Level: Integer; // 0 = page (most derived), 1+ = layouts (higher = base)
    class function Create(BeginBlockAddress, EndBlockAddress: Int64; Level: Integer = 0): TBlockAddress; static;
  end;

  TBlockReturnInfo = record
    ReturnAddress: Int64;
    BlockName: string;
    ParentBlockAddress: Int64; // For {{inherited}} - address of parent block, -1 if none
    class function Create(ReturnAddr: Int64; const ABlockName: string; ParentAddr: Int64 = -1): TBlockReturnInfo; static;
  end;

  TMacroParameter = record
    Name: String;
    DefaultValue: String;
    HasDefault: Boolean;
    class function Create(const Name: String; const DefaultValue: String = ''; HasDefault: Boolean = False): TMacroParameter; static;
  end;

  TMacroDefinition = record
    Name: String;
    Parameters: TArray<TMacroParameter>;
    BeginTokenIndex: Int64;
    EndTokenIndex: Int64;
    class function Create(const Name: String; const Parameters: TArray<TMacroParameter>; BeginTokenIndex, EndTokenIndex: Int64): TMacroDefinition; static;
  end;

  TTokenWalkProc = reference to procedure(const Index: Integer; const Token: TToken);

  TComparandType = (ctEQ, ctNE, ctGT, ctGE, ctLT, ctLE);

  TTProTemplateFunction = function(const aValue: TValue; const aParameters: TArray<TFilterParameter>): TValue;
  TTProTemplateAnonFunction = reference to function(const aValue: TValue; const aParameters: TArray<TFilterParameter>): TValue;
  TTProVariablesInfo = (viSimpleType, viObject, viDataSet, viListOfObject, viJSONObject, viJSONArray, viIterable);
  TTProVariablesInfos = set of TTProVariablesInfo;

  TVarDataSource = class
  public
    VarValue: TValue;
    VarOption: TTProVariablesInfos;
    constructor Create(const VarValue: TValue; const VarOption: TTProVariablesInfos);
  end;

  TTProEqualityComparer = class(TEqualityComparer<string>)
  public
    function Equals(const Left, Right: String): Boolean; override;
    function GetHashCode(const Value: String): Integer; override;
  end;

  TTProVariables = class(TObjectDictionary<string, TVarDataSource>)
  public
    constructor Create;
  end;

  TTProCompiledTemplateGetValueEvent = reference to procedure(const DataSource, Members: string; var Value: TValue; var Handled: Boolean);

  PTProFormatSettings = ^TFormatSettings;

  TLoopStackItem = class
  public
    DataSourceName: String;
    LoopExpression: String;
    FullPath: String;
    IteratorPosition: Integer;
    IteratorName: String;
    EOF: Boolean;
    IsFieldIteration: Boolean;
    FieldsCount: Integer;
    TotalCount: Integer;
    function IncrementIteratorPosition: Integer;
    constructor Create(DataSourceName: String; LoopExpression: String; FullPath: String; IteratorName: String; AIsFieldIteration: Boolean = False);
  end;

  TTProTemplateSectionType = (stUnknown, stLayout, stPage);
  TTProCompilerOption = (
    coIgnoreSysVersion,     // Internal: skip system version token
    coParentTemplate,       // Internal: compiling a parent template
    coDisableEatLineBreaks  // Disable automatic linebreak removal on tag-only lines
  );
  TTProCompilerOptions = set of TTProCompilerOption;

  TIncludeSavedVar = record
    Existed: Boolean;
    Value: TValue;
  end;
  TIncludeSavedVars = TDictionary<String, TIncludeSavedVar>;

implementation

{ TToken }

class function TToken.Create(TokType: TTokenType; Value1, Value2: String; Ref1: Int64; Ref2: Int64): TToken;
begin
  Result.TokenType := TokType;
  Result.Value1 := Value1;
  Result.Value2 := Value2;
  Result.Ref1 := Ref1;
  Result.Ref2 := Ref2;
end;

class function TToken.CreateFromBytes(const aBytes: TBinaryReader): TToken;
var
  lValue1Size: UInt32;
  lValue2Size: UInt32;
  lTokenAsByte: byte;
begin
  lTokenAsByte := aBytes.ReadByte;
  Result.TokenType := TTokenType(lTokenAsByte);

  // Optimized: Read directly into string memory to avoid intermediate TArray<byte> allocation
  // This is ~50% faster than TEncoding.Unicode.GetString(aBytes.ReadBytes(...))
  lValue1Size := aBytes.ReadUInt32;
  if lValue1Size > 0 then
  begin
    SetLength(Result.Value1, lValue1Size div SizeOf(Char));
    aBytes.BaseStream.ReadBuffer(PByte(Result.Value1)^, lValue1Size);
  end
  else
    Result.Value1 := '';

  lValue2Size := aBytes.ReadUInt32;
  if lValue2Size > 0 then
  begin
    SetLength(Result.Value2, lValue2Size div SizeOf(Char));
    aBytes.BaseStream.ReadBuffer(PByte(Result.Value2)^, lValue2Size);
  end
  else
    Result.Value2 := '';

  Result.Ref1 := aBytes.ReadInt64;
  Result.Ref2 := aBytes.ReadInt64;
end;

procedure TToken.SaveToBytes(const aBytes: TBinaryWriter);
var
  lValue1Bytes: TArray<byte>;
  lValue2Bytes: TArray<byte>;
  lValue1Length: UInt32;
  lValue2Length: UInt32;
  lTokenAsByte: byte;
begin
  lTokenAsByte := byte(TokenType);
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
  Result := Format('%15s | Ref1: %8d | Ref2: %8d | Val1: %-25s| Val2: %-25s', [TokenTypeAsString, Ref1, Ref2, Value1, Value2]);
end;

{ TBlockAddress }

class function TBlockAddress.Create(BeginBlockAddress, EndBlockAddress: Int64; Level: Integer): TBlockAddress;
begin
  Result.BeginBlockAddress := BeginBlockAddress;
  Result.EndBlockAddress := EndBlockAddress;
  Result.Level := Level;
end;

{ TBlockReturnInfo }

class function TBlockReturnInfo.Create(ReturnAddr: Int64; const ABlockName: string; ParentAddr: Int64): TBlockReturnInfo;
begin
  Result.ReturnAddress := ReturnAddr;
  Result.BlockName := ABlockName;
  Result.ParentBlockAddress := ParentAddr;
end;

{ TMacroParameter }

class function TMacroParameter.Create(const Name: String; const DefaultValue: String; HasDefault: Boolean): TMacroParameter;
begin
  Result.Name := Name;
  Result.DefaultValue := DefaultValue;
  Result.HasDefault := HasDefault;
end;

{ TMacroDefinition }

class function TMacroDefinition.Create(const Name: String; const Parameters: TArray<TMacroParameter>; BeginTokenIndex, EndTokenIndex: Int64): TMacroDefinition;
begin
  Result.Name := Name;
  Result.Parameters := Parameters;
  Result.BeginTokenIndex := BeginTokenIndex;
  Result.EndTokenIndex := EndTokenIndex;
end;

{ TVarDataSource }

constructor TVarDataSource.Create(const VarValue: TValue; const VarOption: TTProVariablesInfos);
begin
  self.VarValue := VarValue;
  self.VarOption := VarOption;
end;

{ TTProVariables }

constructor TTProVariables.Create;
begin
  inherited Create([doOwnsValues], TTProEqualityComparer.Create);
end;

{ TLoopStackItem }

constructor TLoopStackItem.Create(DataSourceName, LoopExpression, FullPath: String; IteratorName: String; AIsFieldIteration: Boolean);
begin
  Self.DataSourceName := DataSourceName;
  Self.LoopExpression := LoopExpression;
  Self.FullPath := FullPath;
  Self.IteratorName := IteratorName;
  Self.IteratorPosition := -1;
  Self.EOF := False;
  Self.IsFieldIteration := AIsFieldIteration;
  Self.FieldsCount := 0;
  Self.TotalCount := 0;
end;

function TLoopStackItem.IncrementIteratorPosition: Integer;
begin
  Inc(IteratorPosition);
  Result := IteratorPosition;
end;

{ TTProEqualityComparer }

function TTProEqualityComparer.Equals(const Left, Right: String): Boolean;
begin
  Result := CompareText(Left, Right) = 0;
end;

function TTProEqualityComparer.GetHashCode(const Value: String): Integer;
begin
  Result := Length(Value);
end;

end.
