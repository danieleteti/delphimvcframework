// ***************************************************************************
//
// Copyright (c) 2016-2025 Daniele Teti
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
  System.Generics.Defaults,
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.DateUtils,
  System.RTTI,
  System.Variants,
  Data.DB,
  ExprEvaluator;

const
  TEMPLATEPRO_VERSION = '0.9.0';

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

  TTokenType = (ttContent, ttInclude, ttFor, ttEndFor, ttIfThen, ttBoolExpression, ttElse, ttEndIf, ttStartTag, ttComment, ttJump, ttBlock,
    ttEndBlock, ttInherited, ttContinue, ttLiteralString, ttEndTag, ttValue, ttFilterName, ttFilterParameter, ttLineBreak, ttSystemVersion, ttExit,
    ttEOF, ttInfo, ttMacro, ttEndMacro, ttCallMacro, ttMacroParam, ttExpression, ttSet, ttIncludeStart, ttIncludeEnd);

const
  TOKEN_TYPE_DESCR: array [Low(TTokenType) .. High(TTokenType)] of string = ('ttContent', 'ttInclude', 'ttFor', 'ttEndFor', 'ttIfThen',
    'ttBoolExpression', 'ttElse', 'ttEndIf', 'ttStartTag', 'ttComment', 'ttJump', 'ttBlock', 'ttEndBlock', 'ttInherited', 'ttContinue', 'ttLiteralString',
    'ttEndTag', 'ttValue', 'ttFilterName', 'ttFilterParameter', 'ttLineBreak', 'ttSystemVersion', 'ttExit', 'ttEOF', 'ttInfo', 'ttMacro',
    'ttEndMacro', 'ttCallMacro', 'ttMacroParam', 'ttExpression', 'ttSet', 'ttIncludeStart', 'ttIncludeEnd');

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
  TTProVariablesInfo = (viSimpleType, viObject, viDataSet, viListOfObject, viJSONObject, viIterable);
  TTProVariablesInfos = set of TTProVariablesInfo;

  TVarDataSource = class
  protected
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

  ITProCompiledTemplate = interface
    ['{0BE04DE7-6930-456B-86EE-BFD407BA6C46}']
    function Render: String;
    procedure ForEachToken(const TokenProc: TTokenWalkProc);
    procedure ClearData;
    procedure SetData(const Name: String; Value: TValue); overload;
    procedure AddFilter(const FunctionName: string; const FunctionImpl: TTProTemplateFunction); overload;
    procedure AddFilter(const FunctionName: string; const AnonFunctionImpl: TTProTemplateAnonFunction); overload;
    procedure DumpToFile(const FileName: String);
    procedure SaveToFile(const FileName: String);
    function GetOnGetValue: TTProCompiledTemplateGetValueEvent;
    procedure SetOnGetValue(const Value: TTProCompiledTemplateGetValueEvent);
    property OnGetValue: TTProCompiledTemplateGetValueEvent read GetOnGetValue write SetOnGetValue;
    function GetFormatSettings: PTProFormatSettings;
    procedure SetFormatSettings(const Value: PTProFormatSettings);
    property FormatSettings: PTProFormatSettings read GetFormatSettings write SetFormatSettings;
    /// <summary>
    /// Evaluates a complex expression using the ExpressionEvaluator engine.
    /// The expression can reference template variables using their names.
    /// Example: EvaluateExpression('price * qty * (1 - discount)')
    /// </summary>
    function EvaluateExpression(const Expression: string): TValue;
    function GetOutputLineEnding: TLineEndingStyle;
    procedure SetOutputLineEnding(const Value: TLineEndingStyle);
    property OutputLineEnding: TLineEndingStyle read GetOutputLineEnding write SetOutputLineEnding;
  end;

  TTProCompiledTemplateEvent = reference to procedure(const TemplateProCompiledTemplate: ITProCompiledTemplate);

  TLoopStackItem = class
  protected
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
  TTProCompilerOption = (coIgnoreSysVersion, coParentTemplate);
  TTProCompilerOptions = set of TTProCompilerOption;

  TIncludeSavedVar = record
    Existed: Boolean;
    Value: TValue;
  end;
  TIncludeSavedVars = TDictionary<String, TIncludeSavedVar>;

  TTProCompiledTemplate = class(TInterfacedObject, ITProCompiledTemplate)
  private
    fLocaleFormatSettings: TFormatSettings;
    fOutputLineEnding: TLineEndingStyle;
    fTokens: TList<TToken>;
    fVariables: TTProVariables;
    fEncoding: TEncoding;
    fDynamicIncludeCache: TDictionary<string, ITProCompiledTemplate>;
    fTemplateFunctions: TDictionary<string, TTProTemplateFunction>;
    fTemplateAnonFunctions: TDictionary<string, TTProTemplateAnonFunction>;
    fMacros: TDictionary<string, TMacroDefinition>;
    fLoopsStack: TObjectList<TLoopStackItem>;
    fIncludeSavedVarsStack: TObjectList<TIncludeSavedVars>;
    fOnGetValue: TTProCompiledTemplateGetValueEvent;
    fExprEvaluator: IExprEvaluator;
    function IsNullableType(const Value: PValue): Boolean;
    procedure InitTemplateAnonFunctions; inline;
    function PeekLoop: TLoopStackItem;
    procedure PopLoop;
    procedure PushLoop(const LoopStackItem: TLoopStackItem);
    function LoopStackIsEmpty: Boolean;
    function WalkThroughLoopStack(const VarName: String; out BaseVarName: String; out FullPath: String): Boolean;
    constructor Create(Tokens: TList<TToken>);
    procedure Error(const aMessage: String); overload;
    procedure Error(const aMessage: String; const Params: array of const); overload;
    function IsTruthy(const Value: TValue): Boolean;
    function GetVarAsString(const Name: string): string;
    function GetTValueVarAsString(const Value: PValue; out WasNull: Boolean; const VarName: string = ''): String;
    function GetTValueWithNullableTypeAsString(const Value: PValue; out WasNull: Boolean; const VarName: string = ''): String;
    function GetNullableTValueAsTValue(const Value: PValue; const VarName: string = ''): TValue;
    function GetVarAsTValue(const aName: string): TValue;
    function GetDataSetFieldAsTValue(const aDataSet: TDataSet; const FieldName: String): TValue;
    function EvaluateIfExpressionAt(var Idx: Int64): Boolean;
    function GetVariables: TTProVariables;
    procedure SplitVariableName(const VariableWithMember: String; out VarName, VarMembers: String);
    function ExecuteFilter(aFunctionName: string; var aParameters: TArray<TFilterParameter>; aValue: TValue;
      const aVarNameWhereShoudBeApplied: String): TValue;
    procedure CheckParNumber(const aHowManyPars: Integer; const aParameters: TArray<TFilterParameter>); overload;
    procedure CheckParNumber(const aMinParNumber, aMaxParNumber: Integer; const aParameters: TArray<TFilterParameter>); overload;
    function GetPseudoVariable(const VarIterator: Integer; const PseudoVarName: String): TValue; overload;
    function IsAnIterator(const VarName: String; out DataSourceName: String; out CurrentIterator: TLoopStackItem): Boolean;
    function GetOnGetValue: TTProCompiledTemplateGetValueEvent;
    function EvaluateValue(var Idx: Int64; out MustBeEncoded: Boolean): TValue;
    procedure SetOnGetValue(const Value: TTProCompiledTemplateGetValueEvent);
    procedure DoOnGetValue(const DataSource, Members: string; var Value: TValue; var Handled: Boolean);
    function GetFormatSettings: PTProFormatSettings;
    procedure SetFormatSettings(const Value: PTProFormatSettings);
    class procedure InternalDumpToFile(const FileName: String; const aTokens: TList<TToken>);
    function ComparandOperator(const aComparandType: TComparandType; const aValue: TValue; const aParameters: TArray<TFilterParameter>;
      const aLocaleFormatSettings: TFormatSettings): TValue;
    procedure RegisterMacro(const TokenIndex: Int64);
    function ExecuteMacro(const CallTokenIndex: Int64): String;
    procedure ProcessSetToken(var Idx: Int64);
    function ExecuteStringFilter(const aFunctionName: string; var aParameters: TArray<TFilterParameter>;
      const aValue: TValue; const aExecuteAsFilterOnAValue: Boolean; out aResult: TValue): Boolean;
    function ExecuteDateFilter(const aFunctionName: string; var aParameters: TArray<TFilterParameter>;
      const aValue: TValue; const aVarNameWhereShoudBeApplied: String; out aResult: TValue): Boolean;
    function GetExprEvaluator: IExprEvaluator;
    function TValueToVariant(const Value: TValue): Variant;
    function VariantToTValue(const Value: Variant): TValue;
    function GetFieldProperty(const AField: TField; const PropName: string): TValue;
    function EvaluateDataSetFieldMeta(const DataSetVarName, FieldMetaInfo: string): TValue;
    function GetOutputLineEnding: TLineEndingStyle;
    procedure SetOutputLineEnding(const Value: TLineEndingStyle);
    function GetLineEndingString: string;
  public
    function EvaluateExpression(const Expression: string): TValue;
    destructor Destroy; override;
    function Render: String;
    procedure ForEachToken(const TokenProc: TTokenWalkProc);
    procedure ClearData;
    procedure SaveToFile(const FileName: String);
    class function CreateFromFile(const FileName: String): ITProCompiledTemplate;
    procedure SetData(const Name: String; Value: TValue); overload;
    procedure AddFilter(const FunctionName: string; const FunctionImpl: TTProTemplateFunction); overload;
    procedure AddFilter(const FunctionName: string; const AnonFunctionImpl: TTProTemplateAnonFunction); overload;
    procedure DumpToFile(const FileName: String);
    property FormatSettings: PTProFormatSettings read GetFormatSettings write SetFormatSettings;
    property OnGetValue: TTProCompiledTemplateGetValueEvent read GetOnGetValue write SetOnGetValue;
    property OutputLineEnding: TLineEndingStyle read GetOutputLineEnding write SetOutputLineEnding;
  end;

  TTProCompiler = class
  strict private
    fOptions: TTProCompilerOptions;
    fInputString: string;
    fCharIndex: Int64;
    fCurrentLine: Integer;
    fEncoding: TEncoding;
    fCurrentFileName: String;
    fLastMatchedLineBreakLength: Integer;
    fInheritanceChain: TList<string>;
    function MatchLineBreak: Boolean;
    function MatchStartTag: Boolean;
    function MatchEndTag: Boolean;
    function MatchVariable(var aIdentifier: string): Boolean;
    function MatchFilterParamValue(var aParamValue: TFilterParameter): Boolean;
    function MatchSymbol(const aSymbol: string): Boolean;
    function MatchExpression(out aExpression: string): Boolean;
    function MatchSpace: Boolean;
    function MatchString(out aStringValue: string): Boolean;
    procedure InternalMatchFilter(lIdentifier: String; var lStartVerbatim: Int64; const CurrToken: TTokenType; aTokens: TList<TToken>;
      const lRef2: Integer);
    function GetFunctionParameters: TArray<TFilterParameter>;
    function GetMacroParameters: TArray<TFilterParameter>;
    function CreateFilterParameterToken(const FilterParameter: PFilterParameter): TToken;
    procedure Error(const aMessage: string);
    function Step: Char;
    function CurrentChar: Char;
    function GetSubsequentText: String;
    procedure InternalCompileIncludedTemplate(const aTemplate: string; const aTokens: TList<TToken>; const aFileNameRefPath: String;
      const aCompilerOptions: TTProCompilerOptions);
    procedure ProcessJumps(const aTokens: TList<TToken>);
    procedure Compile(const aTemplate: string; const aTokens: TList<TToken>; const aFileNameRefPath: String); overload;
    constructor Create(const aEncoding: TEncoding; const aOptions: TTProCompilerOptions = []); overload;
    procedure MatchFilters(lVarName: string; var lFilters: TArray<TFilterInfo>);
    procedure AddFilterTokens(aTokens: TList<TToken>; const aFilters: TArray<TFilterInfo>);
  public
    destructor Destroy; override;
    function Compile(const aTemplate: string; const aFileNameRefPath: String = ''): ITProCompiledTemplate; overload;
    constructor Create(aEncoding: TEncoding = nil); overload;
    class function CompileAndRender(const aTemplate: string; const VarNames: TArray<String>; const VarValues: TArray<TValue>): String;
  end;

  ITProWrappedList = interface
    ['{C1963FBF-1E42-4E2A-A17A-27F3945F13ED}']
    function GetItem(const AIndex: Integer): TObject;
    procedure Add(const AObject: TObject);
    function Count: Integer;
    procedure Clear;
    function IsWrappedList: Boolean; overload;
    function ItemIsObject(const AIndex: Integer; out aValue: TValue): Boolean;
  end;

  TTProConfiguration = class sealed
  private
    class var fOnContextConfiguration: TTProCompiledTemplateEvent;
  protected
    class procedure RegisterHandlers(const TemplateProCompiledTemplate: ITProCompiledTemplate);
  public
    class property OnContextConfiguration: TTProCompiledTemplateEvent read fOnContextConfiguration write fOnContextConfiguration;
  end;

function HTMLEncode(s: string): string;
function HandleTemplateSectionStateMachine(const aTokenValue1: String; var aTemplateSectionType: TTProTemplateSectionType;
  out aErrorMessage: String): Boolean;
function GetTValueFromPath(const aObject: TObject; FullPropertyPath: String): TValue;

implementation

uses
  System.StrUtils, System.IOUtils, System.NetEncoding, System.Math, System.Character,
  JsonDataObjects, MVCFramework.Nullables, Data.FmtBCD, Data.SqlTimSt;

const
  Sign = ['-', '+'];
  Numbers = ['0' .. '9'];
  SignAndNumbers = Sign + Numbers;
  IdenfierAllowedFirstChars = ['a' .. 'z', 'A' .. 'Z', '_', '@'];
  IdenfierAllowedChars = ['a' .. 'z', 'A' .. 'Z', '_'] + Numbers;
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
    FIsWrappedList: Boolean;
    function HookListMethods(const aObjType: TRttiType): Boolean;
  protected
    procedure Add(const AObject: TObject);
    procedure Clear;
    function ItemIsObject(const AIndex: Integer; out aValue: TValue): Boolean;
  public
    constructor Create(const AObjectAsDuck: TObject); overload;
    constructor Create(const AInterfaceAsDuck: IInterface); overload;

    function IsWrappedList: Boolean; overload;
    function Count: Integer;
    procedure GetItemAsTValue(const AIndex: Integer; out aValue: TValue);
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

procedure FunctionError(const aFunctionName, aErrMessage: string);
begin
  raise ETProRenderException.Create(Format('[%1:s] %0:s (error in filter call for function [%1:s])', [aErrMessage, aFunctionName]))
    at ReturnAddress;
end;

function TTProCompiledTemplate.ComparandOperator(const aComparandType: TComparandType; const aValue: TValue;
  const aParameters: TArray<TFilterParameter>; const aLocaleFormatSettings: TFormatSettings): TValue;
var
  lInt64Value: Int64;
  lStrValue: string;
  lExtendedValue: Extended;
  lValue, lTmp: TValue;
  function GetComparandResultStr(const aComparandType: TComparandType; const aLeftValue, aRightValue: String): TValue;
  begin
    case aComparandType of
      ctEQ:
        Result := aLeftValue = aRightValue;
      ctNE:
        Result := aLeftValue <> aRightValue;
      ctGT:
        Result := aLeftValue > aRightValue;
      ctGE:
        Result := aLeftValue >= aRightValue;
      ctLT:
        Result := aLeftValue < aRightValue;
      ctLE:
        Result := aLeftValue <= aRightValue;
    else
      raise ETProRenderException.Create('Invalid Comparand Type: ' + TRttiEnumerationType.GetName<TComparandType>(aComparandType));
    end;
  end;

begin
  if Length(aParameters) <> 1 then
    FunctionError(TRttiEnumerationType.GetName<TComparandType>(aComparandType), 'expected 1 parameter');
  if aValue.IsEmpty then
  begin
    FunctionError(TRttiEnumerationType.GetName<TComparandType>(aComparandType), 'Null variable for comparand');
  end;
  case aValue.TypeInfo.Kind of
    tkInteger, tkEnumeration, tkInt64:
      begin
        if aParameters[0].ParType = fptString then
        begin
          raise ETProRenderException.Create('Invalid type for comparand');
        end;
        if aParameters[0].ParType = fptInteger then
        begin
          lInt64Value := aParameters[0].ParIntValue
        end
        else
        begin
          lTmp := GetVarAsTValue(aParameters[0].ParStrText);
          if IsNullableType(@lTmp) then
          begin
            lTmp := GetNullableTValueAsTValue(@lTmp);
            if lTmp.IsEmpty then
            begin
              Exit(False);
            end;
          end;
          lInt64Value := lTmp.AsInt64;
        end;

        case aComparandType of
          ctEQ:
            Result := aValue.AsInt64 = lInt64Value;
          ctNE:
            Result := aValue.AsInt64 <> lInt64Value;
          ctGT:
            Result := aValue.AsInt64 > lInt64Value;
          ctGE:
            Result := aValue.AsInt64 >= lInt64Value;
          ctLT:
            Result := aValue.AsInt64 < lInt64Value;
          ctLE:
            Result := aValue.AsInt64 <= lInt64Value;
        else
          raise ETProRenderException.Create('Invalid Comparand Type: ' + TRttiEnumerationType.GetName<TComparandType>(aComparandType));
        end;
      end;
    tkFloat:
      begin
        if aValue.TypeInfo.Name = 'TDateTime' then
        begin
          lStrValue := DateTimeToStr(aValue.AsExtended, aLocaleFormatSettings);
          case aParameters[0].ParType of
            fptString:
              begin
                Result := GetComparandResultStr(aComparandType, lStrValue, aParameters[0].ParStrText);
              end;
            fptVariable:
              begin
                lValue := GetVarAsTValue(aParameters[0].ParStrText);
                Result := GetComparandResultStr(aComparandType, lStrValue, lValue.AsString);
              end;
          else
            Error('Invalid parameter type for ' + TRttiEnumerationType.GetName<TComparandType>(aComparandType));
          end;
        end
        else if aValue.TypeInfo.Name = 'TDate' then
        begin
          lStrValue := DateToStr(aValue.AsExtended, aLocaleFormatSettings);
          case aParameters[0].ParType of
            fptString:
              begin
                Result := GetComparandResultStr(aComparandType, lStrValue, aParameters[0].ParStrText)
              end;
            fptVariable:
              begin
                lValue := GetVarAsTValue(aParameters[0].ParStrText);
                Result := GetComparandResultStr(aComparandType, lStrValue, lValue.AsString);
              end;
          else
            Error('Invalid parameter type for ' + TRttiEnumerationType.GetName<TComparandType>(aComparandType));
          end;
        end
        else
        begin
          lExtendedValue := 0;
          case aParameters[0].ParType of
            fptFloat:
              begin
                lExtendedValue := aParameters[0].ParFloatValue;
              end;
            fptVariable:
              begin
                lValue := GetVarAsTValue(aParameters[0].ParStrText);
                lExtendedValue := lValue.AsExtended;
              end;
          else
            Error('Invalid parameter type for ' + TRttiEnumerationType.GetName<TComparandType>(aComparandType));
          end;
          case aComparandType of
            ctEQ:
              Result := aValue.AsExtended = lExtendedValue;
            ctNE:
              Result := aValue.AsExtended <> lExtendedValue;
            ctGT:
              Result := aValue.AsExtended > lExtendedValue;
            ctGE:
              Result := aValue.AsExtended >= lExtendedValue;
            ctLT:
              Result := aValue.AsExtended < lExtendedValue;
            ctLE:
              Result := aValue.AsExtended <= lExtendedValue;
          else
            raise ETProRenderException.Create('Invalid Comparand Type: ' + TRttiEnumerationType.GetName<TComparandType>(aComparandType));
          end
        end;
      end;
  else
    begin
      case aParameters[0].ParType of
        fptString:
          begin
            Result := GetComparandResultStr(aComparandType, aValue.AsString, aParameters[0].ParStrText)
          end;
        fptInteger:
          begin
            Result := GetComparandResultStr(aComparandType, aValue.AsString, aParameters[0].ParIntValue.ToString)
          end;
        fptVariable:
          begin
            lValue := GetVarAsTValue(aParameters[0].ParStrText);
            Result := GetComparandResultStr(aComparandType, aValue.AsString, lValue.AsString);
          end;
      else
        Error('Invalid parameter type for ' + TRttiEnumerationType.GetName<TComparandType>(aComparandType));
      end;
    end;
  end;
end;

{ TParser }

procedure TTProCompiledTemplate.AddFilter(const FunctionName: string; const FunctionImpl: TTProTemplateFunction);
begin
  fTemplateFunctions.Add(FunctionName.ToLower, FunctionImpl);
end;

function TTProCompiledTemplate.GetDataSetFieldAsTValue(const aDataSet: TDataSet; const FieldName: String): TValue;
var
  lField: TField;
begin
  lField := aDataSet.FindField(FieldName);
  if not Assigned(lField) then
  begin
    Exit(TValue.Empty);
  end;
  case lField.DataType of
    ftInteger, ftSmallInt, ftWord:
      Result := lField.AsInteger;
    ftLargeint, ftAutoInc:
      Result := lField.AsLargeInt;
    ftFloat:
      Result := lField.AsFloat;
    ftSingle:
      Result := lField.AsSingle;
    ftCurrency:
      Result := lField.AsCurrency;
    ftString, ftWideString, ftMemo, ftWideMemo:
      Result := lField.AsWideString;
    ftDate:
      Result := TDate(Trunc(lField.AsDateTime));
    ftDateTime, ftTimeStamp:
      Result := lField.AsDateTime;
    ftTimeStampOffset:
      Result := TValue.From<TSQLTimeStampOffset>(lField.AsSQLTimeStampOffset);
    ftTime:
      Result := lField.AsDateTime;
    ftBoolean:
      Result := lField.AsBoolean;
    ftFMTBcd, ftBcd:
      Result := TValue.From<TBCD>(lField.AsBCD);
  else
    Error('Invalid data type for field "%s": %s', [FieldName, TRttiEnumerationType.GetName<TFieldType>(lField.DataType)]);
  end;
end;

function TTProCompiledTemplate.GetFormatSettings: PTProFormatSettings;
begin
  Result := @fLocaleFormatSettings;
end;

function TTProCompiledTemplate.GetOutputLineEnding: TLineEndingStyle;
begin
  Result := fOutputLineEnding;
end;

procedure TTProCompiledTemplate.SetOutputLineEnding(const Value: TLineEndingStyle);
begin
  fOutputLineEnding := Value;
end;

function TTProCompiledTemplate.GetLineEndingString: string;
begin
  case fOutputLineEnding of
    lesLF: Result := #10;
    lesCRLF: Result := #13#10;
    lesCR: Result := #13;
    lesNative: Result := sLineBreak;
  else
    Result := sLineBreak;
  end;
end;

function TTProCompiledTemplate.GetNullableTValueAsTValue(const Value: PValue; const VarName: string): TValue;
var
  lNullableInt32: NullableInt32;
  lNullableUInt32: NullableUInt32;
  lNullableInt16: NullableInt16;
  lNullableUInt16: NullableUInt16;
  lNullableInt64: NullableInt64;
  lNullableUInt64: NullableUInt64;
  lNullableCurrency: NullableCurrency;
  lNullableBoolean: NullableBoolean;
  lNullableTDate: NullableTDate;
  lNullableTTime: NullableTTime;
  lNullableTDateTime: NullableTDateTime;
  lNullableString: NullableString;
begin
  Result := TValue.Empty;

  if Value.IsEmpty then
  begin
    Exit;
  end;

  if Value.TypeInfo.Kind = tkRecord then
  begin
    if Value.TypeInfo = TypeInfo(NullableInt32) then
    begin
      lNullableInt32 := Value.AsType<NullableInt32>;
      if lNullableInt32.HasValue then
        Exit(lNullableInt32.Value);
    end
    else if Value.TypeInfo = TypeInfo(NullableUInt32) then
    begin
      lNullableUInt32 := Value.AsType<NullableUInt32>;
      if lNullableUInt32.HasValue then
        Exit(lNullableUInt32.Value);
    end
    else if Value.TypeInfo = TypeInfo(NullableInt16) then
    begin
      lNullableInt16 := Value.AsType<NullableInt16>;
      if lNullableInt16.HasValue then
        Exit(lNullableInt16.Value);
    end
    else if Value.TypeInfo = TypeInfo(NullableUInt16) then
    begin
      lNullableUInt16 := Value.AsType<NullableUInt16>;
      if lNullableUInt16.HasValue then
        Exit(lNullableUInt16.Value);
    end
    else if Value.TypeInfo = TypeInfo(NullableInt64) then
    begin
      lNullableInt64 := Value.AsType<NullableInt64>;
      if lNullableInt64.HasValue then
        Exit(lNullableInt64.Value);
    end
    else if Value.TypeInfo = TypeInfo(NullableUInt64) then
    begin
      lNullableUInt64 := Value.AsType<NullableUInt64>;
      if lNullableUInt64.HasValue then
        Exit(lNullableUInt64.Value);
    end
    else if Value.TypeInfo = TypeInfo(NullableString) then
    begin
      lNullableString := Value.AsType<NullableString>;
      if lNullableString.HasValue then
        Exit(lNullableString.Value);
    end
    else if Value.TypeInfo = TypeInfo(NullableCurrency) then
    begin
      lNullableCurrency := Value.AsType<NullableCurrency>;
      if lNullableCurrency.HasValue then
        Exit(lNullableCurrency.Value);
    end
    else if Value.TypeInfo = TypeInfo(NullableBoolean) then
    begin
      lNullableBoolean := Value.AsType<NullableBoolean>;
      if lNullableBoolean.HasValue then
        Exit(lNullableBoolean.Value);
    end
    else if Value.TypeInfo = TypeInfo(NullableTDate) then
    begin
      lNullableTDate := Value.AsType<NullableTDate>;
      if lNullableTDate.HasValue then
        Exit(lNullableTDate.Value);
    end
    else if Value.TypeInfo = TypeInfo(NullableTTime) then
    begin
      lNullableTTime := Value.AsType<NullableTTime>;
      if lNullableTTime.HasValue then
        Exit(lNullableTTime.Value);
    end
    else if Value.TypeInfo = TypeInfo(NullableTDateTime) then
    begin
      lNullableTDateTime := Value.AsType<NullableTDateTime>;
      if lNullableTDateTime.HasValue then
        Exit(lNullableTDateTime.Value);
    end
    else
    begin
      raise ETProException.Create('Unsupported type for variable "' + VarName + '"');
    end;
  end
  else
  begin
    Result := Value^;
  end;
end;

function TTProCompiledTemplate.GetOnGetValue: TTProCompiledTemplateGetValueEvent;
begin
  Result := fOnGetValue;
end;

function TTProCompiledTemplate.GetPseudoVariable(const VarIterator: Integer; const PseudoVarName: String): TValue;
var
  lLoopItem: TLoopStackItem;
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
  else if PseudoVarName = '@@first' then
  begin
    Result := VarIterator = 0;
  end
  else if PseudoVarName = '@@last' then
  begin
    lLoopItem := PeekLoop;
    if Assigned(lLoopItem) and (lLoopItem.TotalCount > 0) then
      Result := VarIterator = lLoopItem.TotalCount - 1
    else
      Result := False;
  end
  else
  begin
    Result := TValue.Empty;
  end;
end;

function TTProCompiledTemplate.GetTValueVarAsString(const Value: PValue; out WasNull: Boolean; const VarName: string): String;
var
  lIsObject: Boolean;
  lAsObject: TObject;
  lVarName: string;
  lVarMember: string;
  lTmp: TValue;
  lIsNull: Boolean;
begin
  if Value.IsEmpty then
  begin
    Exit('');
  end;

  lIsObject := False;
  lAsObject := nil;
  if Value.IsObject then
  begin
    lIsObject := True;
    lAsObject := Value.AsObject;
  end;

  if lIsObject then
  begin
    if lAsObject is TField then
      Result := TField(Value.AsObject).AsString
    else if lAsObject is TJsonBaseObject then
      Result := TJsonBaseObject(lAsObject).ToJSON()
    else
    begin
      SplitVariableName(VarName, lVarName, lVarMember);
      if lVarMember.IsEmpty then
      begin
      Result := lAsObject.ToString;
  end
  else
  begin
        lTmp := GetTValueFromPath(lAsObject, lVarMember);
        if IsNullableType(@lTmp) then
        begin
          Result := GetTValueWithNullableTypeAsString(@lTmp, lIsNull, VarName);
        end
        else
        begin
          Result := lTmp.AsString;
        end;
      end;
    end;
  end
  else
  begin
    if IsNullableType(Value) then
    begin
      Result := GetTValueWithNullableTypeAsString(Value, WasNull, VarName);
    end
    else
    begin
      case Value.Kind of
        tkInteger:
          Result := Value.AsInteger.ToString;
        tkInt64:
          Result := Value.AsInt64.ToString;
        tkString, tkUString, tkWString, tkLString:
          Result := Value.AsString;
        tkWChar, tkChar:
          Result := Value.AsType<Char>;
        tkFloat:
          begin
            if Value.TypeInfo.Name = 'TDate' then
            begin
              Result := DateToStr(Value.AsExtended, fLocaleFormatSettings);
            end
            else if Value.TypeInfo.Name = 'TDateTime' then
            begin
              Result := DateTimeToStr(Value.AsExtended, fLocaleFormatSettings);
            end
            else
            begin
              Result := FloatToStr(Value.AsExtended, fLocaleFormatSettings);
            end;
          end;
        tkEnumeration:
          Result := Value.ToString;
        tkRecord:
          begin
            if Value.TypeInfo = TypeInfo(TBcd) then
            begin
              Result := BcdToStr(PBCD(Value.GetReferenceToRawData)^, fLocaleFormatSettings);
            end
            else if Value.TypeInfo = TypeInfo(TSQLTimeStampOffset) then
            begin
              Result := SQLTimeStampOffsetToStr(fLocaleFormatSettings.ShortDateFormat + fLocaleFormatSettings.ListSeparator + fLocaleFormatSettings.LongTimeFormat,
                PSQLTimeStampOffset(Value.GetReferenceToRawData)^, fLocaleFormatSettings);
            end
            else if Value.TypeInfo = TypeInfo(TSQLTimeStamp) then
            begin
              Result := SQLTimeStampToStr(fLocaleFormatSettings.ShortDateFormat + fLocaleFormatSettings.ListSeparator + fLocaleFormatSettings.LongTimeFormat,
                PSQLTimeStamp(Value.GetReferenceToRawData)^, fLocaleFormatSettings);
            end
            else
            begin
              raise ETProException.Create('Unsupported type for record variable "' + VarName + '"');
            end;
          end
      else
        raise ETProException.Create('Unsupported type for variable "' + VarName + '"');
      end;
      // Result := Value.ToString;
    end;
  end;

end;

function TTProCompiledTemplate.GetTValueWithNullableTypeAsString(const Value: PValue; out WasNull: Boolean; const VarName: string): String;
var
  lUnwrappedValue: TValue;
begin
  Result := '';
  WasNull := True;

  // Use GetNullableTValueAsTValue to extract the inner value
  lUnwrappedValue := GetNullableTValueAsTValue(Value, VarName);

  if lUnwrappedValue.IsEmpty then
    Exit;

  WasNull := False;

  // Apply specific formatting based on the original Nullable type
  if Value.TypeInfo = TypeInfo(NullableCurrency) then
    Result := FloatToStr(lUnwrappedValue.AsCurrency, fLocaleFormatSettings)
  else if Value.TypeInfo = TypeInfo(NullableTDate) then
    Result := DateToStr(lUnwrappedValue.AsExtended, fLocaleFormatSettings)
  else if Value.TypeInfo = TypeInfo(NullableTTime) then
    Result := TimeToStr(lUnwrappedValue.AsExtended, fLocaleFormatSettings)
  else if Value.TypeInfo = TypeInfo(NullableTDateTime) then
    Result := DateToISO8601(lUnwrappedValue.AsExtended, False)
  else if Value.TypeInfo = TypeInfo(NullableBoolean) then
    Result := BoolToStr(lUnwrappedValue.AsBoolean, True)
  else
    Result := lUnwrappedValue.ToString;
end;

procedure TTProCompiledTemplate.AddFilter(const FunctionName: string; const AnonFunctionImpl: TTProTemplateAnonFunction);
begin
  InitTemplateAnonFunctions;
  fTemplateAnonFunctions.Add(FunctionName.ToLower, AnonFunctionImpl);
end;

procedure TTProCompiledTemplate.CheckParNumber(const aMinParNumber, aMaxParNumber: Integer; const aParameters: TArray<TFilterParameter>);
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

procedure TTProCompiler.InternalCompileIncludedTemplate(const aTemplate: string; const aTokens: TList<TToken>;
  const aFileNameRefPath: String; const aCompilerOptions: TTProCompilerOptions);
var
  lCompiler: TTProCompiler;
  lFile: string;
begin
  lCompiler := TTProCompiler.Create(fEncoding, aCompilerOptions);
  try
    // Copy inheritance chain to sub-compiler for circular inheritance detection
    for lFile in fInheritanceChain do
      lCompiler.fInheritanceChain.Add(lFile);
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

procedure TTProCompiler.InternalMatchFilter(lIdentifier: String; var lStartVerbatim: Int64; const CurrToken: TTokenType;
  aTokens: TList<TToken>; const lRef2: Integer);
var
  lFilters: TArray<TFilterInfo>;
begin
  SetLength(lFilters, 0);
  if MatchSymbol('|') then
  begin
    MatchFilters(lIdentifier, lFilters);
  end;

  if not MatchEndTag then
  begin
    Error('Expected end tag "' + END_TAG + '"');
  end;
  lStartVerbatim := fCharIndex;
  { Ref1 now stores number of filters (0 = no filter, >0 = filter count) }
  aTokens.Add(TToken.Create(CurrToken, lIdentifier, '', Length(lFilters), lRef2));

  // add filter tokens
  AddFilterTokens(aTokens, lFilters);
end;

constructor TTProCompiler.Create(aEncoding: TEncoding = nil);
begin
  if aEncoding = nil then
    Create(TEncoding.UTF8, []) { default encoding }
  else
    Create(aEncoding, []);
end;

function TTProCompiler.CreateFilterParameterToken(const FilterParameter: PFilterParameter): TToken;
begin
  case FilterParameter.ParType of
    fptString:
      begin
        Result.TokenType := ttFilterParameter;
        Result.Value1 := FilterParameter.ParStrText;
        Result.Ref2 := Ord(FilterParameter.ParType);
      end;

    fptInteger:
      begin
        Result.TokenType := ttFilterParameter;
        Result.Value1 := FilterParameter.ParIntValue.ToString;
        Result.Ref2 := Ord(FilterParameter.ParType);
      end;

    fptVariable:
      begin
        Result.TokenType := ttFilterParameter;
        Result.Value1 := FilterParameter.ParStrText;
        Result.Ref2 := Ord(FilterParameter.ParType);
      end;

  else
    raise ETProCompilerException.Create('Invalid filter parameter type');
  end;

end;

procedure TTProCompiler.MatchFilters(lVarName: string; var lFilters: TArray<TFilterInfo>);
var
  lFuncName: string;
  lFuncParams: TArray<TFilterParameter>;
  lFilterInfo: TFilterInfo;
begin
  SetLength(lFilters, 0);
  while True do
  begin
    MatchSpace;
    if not MatchVariable(lFuncName) then
      Error('Invalid function name applied to variable ' + lVarName);
    MatchSpace;
    lFuncParams := GetFunctionParameters;
    MatchSpace;

    lFilterInfo.FilterName := lFuncName;
    lFilterInfo.Parameters := lFuncParams;
    SetLength(lFilters, Length(lFilters) + 1);
    lFilters[High(lFilters)] := lFilterInfo;

    if not MatchSymbol('|') then
      Break;
  end;
end;

procedure TTProCompiler.AddFilterTokens(aTokens: TList<TToken>; const aFilters: TArray<TFilterInfo>);
var
  I, J: Integer;
  lFilter: TFilterInfo;
begin
  for I := 0 to High(aFilters) do
  begin
    lFilter := aFilters[I];
    aTokens.Add(TToken.Create(ttFilterName, lFilter.FilterName, '', Length(lFilter.Parameters)));
    for J := 0 to High(lFilter.Parameters) do
    begin
      aTokens.Add(CreateFilterParameterToken(@lFilter.Parameters[J]));
    end;
  end;
end;

function TTProCompiler.CurrentChar: Char;
begin
  if fCharIndex < fInputString.Length then
    Result := fInputString.Chars[fCharIndex]
  else
    Result := #0;
end;

function TTProCompiler.MatchEndTag: Boolean;
begin
  Result := MatchSymbol(END_TAG);
end;

function TTProCompiler.MatchVariable(var aIdentifier: string): Boolean;
var
  lTmp: String;
begin
  aIdentifier := '';
  lTmp := '';
  Result := False;
  if (fCharIndex < fInputString.Length) and
     CharInSet(fInputString.Chars[fCharIndex], IdenfierAllowedFirstChars) then
  begin
    lTmp := fInputString.Chars[fCharIndex];
    Inc(fCharIndex);
    if (lTmp = '@') and (fCharIndex < fInputString.Length) then
    begin
      if fInputString.Chars[fCharIndex] = '@' then
      begin
        lTmp := '@@';
        Inc(fCharIndex);
      end;
    end;

    while (fCharIndex < fInputString.Length) and
          CharInSet(fInputString.Chars[fCharIndex], IdenfierAllowedChars) do
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

function TTProCompiler.MatchFilterParamValue(var aParamValue: TFilterParameter): Boolean;
var
  lTmp: String;
  lIntegerPart, lDecimalPart: Integer;
  lDigits: Integer;
  lTmpFloat: Extended;
begin
  lTmp := '';
  Result := False;
  if MatchString(lTmp) then
  begin
    aParamValue.ParType := fptString;
    aParamValue.ParStrText := lTmp;
    Result := True;
  end
  else if (fCharIndex < fInputString.Length) and
          CharInSet(fInputString.Chars[fCharIndex], SignAndNumbers) then
  begin
    lTmp := fInputString.Chars[fCharIndex];
    Inc(fCharIndex);
    while (fCharIndex < fInputString.Length) and
          CharInSet(fInputString.Chars[fCharIndex], Numbers) do
    begin
      lTmp := lTmp + fInputString.Chars[fCharIndex];
      Inc(fCharIndex);
    end;
    lIntegerPart := StrToInt(lTmp);
    if MatchSymbol('.') then
    begin
      lTmp := '';
      while (fCharIndex < fInputString.Length) and
            CharInSet(fInputString.Chars[fCharIndex], Numbers) do
      begin
        lTmp := lTmp + fInputString.Chars[fCharIndex];
        Inc(fCharIndex);
      end;
      lDigits := lTmp.Trim.Length;
      if lDigits = 0 then
      begin
        Error('Expected digit/s after "."');
      end;
      lDecimalPart := lTmp.Trim.ToInteger;
      lTmpFloat := Power(Double(10), lDigits);
      Result := True;
      aParamValue.ParType := fptFloat;
      aParamValue.ParFloatValue := lIntegerPart + lDecimalPart / lTmpFloat;
    end
    else
    begin
      Result := True;
      aParamValue.ParType := fptInteger;
      aParamValue.ParIntValue := lTmp.Trim.ToInteger
    end;
  end
  else if MatchExpression(lTmp) then
  begin
    Result := True;
    aParamValue.ParType := fptExpression;
    aParamValue.ParStrText := lTmp;
  end
  else if (fCharIndex < fInputString.Length) and
          CharInSet(fInputString.Chars[fCharIndex], IdenfierAllowedChars) then
  begin
    while (fCharIndex < fInputString.Length) and
          CharInSet(fInputString.Chars[fCharIndex], ValueAllowedChars) do
    begin
      lTmp := lTmp + fInputString.Chars[fCharIndex];
      Inc(fCharIndex);
    end;
    Result := True;
    aParamValue.ParType := fptVariable;
    aParamValue.ParStrText := lTmp.Trim;
  end;
end;

function TTProCompiler.MatchSpace: Boolean;
begin
  Result := MatchSymbol(' ');
  while MatchSymbol(' ') do;
end;

function TTProCompiler.MatchLineBreak: Boolean;
begin
  // Handle CRLF (Windows), LF (Unix), and CR (old Mac)
  fLastMatchedLineBreakLength := 0;
  Result := False;
  if CurrentChar = #13 then
  begin
    Step;
    fLastMatchedLineBreakLength := 1;
    if CurrentChar = #10 then
    begin
      Step;  // CRLF
      fLastMatchedLineBreakLength := 2;
    end;
    Result := True;
  end
  else if CurrentChar = #10 then
  begin
    Step;  // LF only
    fLastMatchedLineBreakLength := 1;
    Result := True;
  end;
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
    while not MatchSymbol('"') do // no escape so far
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
  while (fCharIndex < fInputString.Length) and
        (lSymbolIndex < lSymbolLength) and
        (fInputString.Chars[fCharIndex].ToLower = aSymbol.Chars[lSymbolIndex].ToLower) do
  begin
    Inc(fCharIndex);
    Inc(lSymbolIndex);
  end;
  Result := (lSymbolIndex > 0) and (lSymbolIndex = lSymbolLength);
  if not Result then
    fCharIndex := lSavedCharIndex;
end;

function TTProCompiler.MatchExpression(out aExpression: string): Boolean;
var
  lParenCount: Integer;
begin
  // Matches @(expression) and returns the expression content
  Result := MatchSymbol('@(');
  if not Result then
    Exit;
  aExpression := '';
  lParenCount := 1;
  while lParenCount > 0 do
  begin
    if fCharIndex > Length(fInputString) then
      Error('Unclosed expression @(...)');
    if CurrentChar = '(' then
      Inc(lParenCount)
    else if CurrentChar = ')' then
      Dec(lParenCount);
    if lParenCount > 0 then
      aExpression := aExpression + CurrentChar;
    Step;
  end;
  aExpression := aExpression.Trim;
  if aExpression.IsEmpty then
    Error('Empty expression in @(...)');
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
  // Clear inheritance chain for each new top-level compilation
  fInheritanceChain.Clear;
  lTokens := TList<TToken>.Create;
  try
    Compile(aTemplate, lTokens, fCurrentFileName);
    ProcessJumps(lTokens);
    Result := TTProCompiledTemplate.Create(lTokens);
  except
    lTokens.Free;
    raise;
  end;
end;

class function TTProCompiler.CompileAndRender(const aTemplate: String; const VarNames: TArray<String>;
  const VarValues: TArray<TValue>): String;
var
  lComp: TTProCompiler;
  lCompiledTemplate: ITProCompiledTemplate;
  I: Integer;
begin
  lComp := TTProCompiler.Create();
  try
    lCompiledTemplate := lComp.Compile(aTemplate);
    for I := 0 to Length(VarNames) - 1 do
    begin
      lCompiledTemplate.SetData(VarNames[I], VarValues[I]);
    end;
    Result := lCompiledTemplate.Render;
  finally
    lComp.Free;
  end;
end;

constructor TTProCompiler.Create(const aEncoding: TEncoding; const aOptions: TTProCompilerOptions);
begin
  inherited Create;
  fEncoding := aEncoding;
  fOptions := aOptions;
  fInheritanceChain := TList<string>.Create;
end;

destructor TTProCompiler.Destroy;
begin
  fInheritanceChain.Free;
  inherited;
end;

procedure TTProCompiler.Compile(const aTemplate: string; const aTokens: TList<TToken>; const aFileNameRefPath: String);
var
  lForStatementCount: Integer;
  lIfStatementCount: Integer;
  lLastToken: TTokenType;
  lChar: Char;
  lVarName: string;
  lIdentifier: string;
  lIteratorName: string;
  lStartVerbatim: Int64;
  lEndVerbatim: Int64;
  lNegation: Boolean;
  lFuncParams: TArray<TFilterParameter>;
  I: Integer;
  lTemplateSource: string;
  lCurrentFileName: string;
  lStringValue: string;
  lRef2: Integer;
  lContentOnThisLine: Integer;
  lStrVerbatim: string;
  lLayoutFound: Boolean;
  lFoundVar: Boolean;
  lFoundFilter: Boolean;
  lFilters: TArray<TFilterInfo>;
  // Variables for dataset field metadata (moved from inline declarations for Delphi 10 Seattle compatibility)
  lDataSetFieldMeta: string;
  lFieldName: string;
  lFieldIsLiteral: Boolean;
  lPropertyName: string;
  // Variables for include handling
  lIncludeFileName: string;
  lIsDynamicInclude: Boolean;
  lHasMappings: Boolean;
  lMappingTargets: TArray<string>;
  lMappingTokens: TList<TToken>;
  lTargetVar: string;
  lMappingToken: TToken;
  lSourceVar: string;
  lNumStr: string;
  lIsFloat: Boolean;
  lIsNeg: Boolean;
  lMapToken: TToken;
  lIncludeStartToken: TToken;
  lIncludeEndToken: TToken;
  lIsFieldIteration: Integer;
  lIncludeToken: TToken;
begin
  aTokens.Add(TToken.Create(ttSystemVersion, TEMPLATEPRO_VERSION, ''));
  lLastToken := ttEOF;
  lLayoutFound := False;
  lContentOnThisLine := 0;
  fCurrentFileName := aFileNameRefPath;
  fCharIndex := -1;
  fCurrentLine := 1;
  lIfStatementCount := -1;
  lForStatementCount := -1;
  fInputString := aTemplate;
  lStartVerbatim := 0;
  if fInputString.Length > 0 then
  begin
    Step;
  end
  else
  begin
    aTokens.Add(TToken.Create(ttEOF, '', ''));
    fCharIndex := 1; { doesnt' execute while }
  end;
  while fCharIndex <= fInputString.Length do
  begin
    lChar := CurrentChar;
    if lChar = #0 then // eof
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

    if MatchLineBreak then { linebreak - handles CRLF, LF, and CR }
    begin
      lEndVerbatim := fCharIndex - fLastMatchedLineBreakLength;
      if lEndVerbatim - lStartVerbatim > 0 then
      begin
        Inc(lContentOnThisLine);
        lStrVerbatim := fInputString.Substring(lStartVerbatim, lEndVerbatim - lStartVerbatim);
        aTokens.Add(TToken.Create(ttContent, lStrVerbatim, ''));
      end;
      lStartVerbatim := fCharIndex;
      if lLastToken = ttLineBreak then
        Inc(lContentOnThisLine);
      lLastToken := ttLineBreak;
      if lContentOnThisLine > 0 then
      begin
        aTokens.Add(TToken.Create(lLastToken, '', ''));
      end;
      Inc(fCurrentLine);
      lContentOnThisLine := 0;
    end
    else if MatchStartTag then { starttag }
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

      if CurrentChar = '@' then // expression {{@expr}}
      begin
        Step; // skip '@'
        MatchSpace; // skip optional spaces after '@'
        lVarName := '';
        // Read expression until end tag
        while not MatchEndTag do
        begin
          if fCharIndex > Length(fInputString) then
            Error('Unclosed expression tag');
          lVarName := lVarName + CurrentChar;
          Step;
        end;
        lVarName := lVarName.Trim;
        if lVarName.IsEmpty then
          Error('Empty expression after "@"');
        lLastToken := ttExpression;
        aTokens.Add(TToken.Create(lLastToken, lVarName, ''));
        lStartVerbatim := fCharIndex;
        Inc(lContentOnThisLine);
      end
      else if CurrentChar = ':' then // variable
      begin
        lFoundVar := False;
        lFoundFilter := False;
        Step;
        MatchSpace;
        lRef2 := -1;
        SetLength(lFilters, 0);
        lDataSetFieldMeta := '';  // stores "fieldname|PropertyName" for dataset field metadata
        if MatchVariable(lVarName) then { variable }
        begin
          lFoundVar := True;
          if lVarName.IsEmpty then
            Error('Invalid variable name');

          // Check for dataset field metadata syntax: [fieldname].Property or ["fieldname"].Property
          if MatchSymbol('[') then
          begin
            lFieldIsLiteral := False;
            if MatchString(lFieldName) then
              lFieldIsLiteral := True  // literal field name
            else if MatchVariable(lFieldName) then
              lFieldIsLiteral := False  // variable containing field name
            else
              Error('Expected field name or variable in brackets');

            if not MatchSymbol(']') then
              Error('Expected "]" after field name');
            if not MatchSymbol('.') then
              Error('Expected "." after "]" for field property access');

            if not MatchVariable(lPropertyName) then
              Error('Expected property name after "."');

            // Store as: "fieldname|PropertyName" (" prefix if literal)
            if lFieldIsLiteral then
              lDataSetFieldMeta := '"' + lFieldName + '|' + lPropertyName
            else
              lDataSetFieldMeta := lFieldName + '|' + lPropertyName;
          end;

          lRef2 := IfThen(MatchSymbol('$'), 1, -1); // {{value$}} means no escaping
          MatchSpace;
        end;

        if MatchSymbol('|') then
        begin
          if not lDataSetFieldMeta.IsEmpty then
            Error('Filters are not supported with dataset field metadata syntax');
          lFoundFilter := True;
          MatchFilters(lVarName, lFilters);
        end;

        if lFoundVar or lFoundFilter then
        begin
          if not MatchEndTag then
          begin
            Error('Expected end tag "' + END_TAG + '"');
          end;
          lStartVerbatim := fCharIndex;
          lLastToken := ttValue;
          { Ref1 now stores number of filters (0 = no filter, >0 = filter count) }
          aTokens.Add(TToken.Create(lLastToken, lVarName, lDataSetFieldMeta, Length(lFilters), lRef2));
          Inc(lContentOnThisLine);

          // add filter tokens
          AddFilterTokens(aTokens, lFilters);
        end
        else
        begin
          Error('Expected variable or filter');
        end;
      end
      else
      begin
        MatchSpace;
        if MatchSymbol('for') then { loop }
        begin
          if not MatchSpace then
            Error('Expected "space"');
          if not MatchVariable(lIteratorName) then
            Error('Expected iterator name after "for" - EXAMPLE: for iterator in iterable');
          if not MatchSpace then
            Error('Expected "space"');
          if not MatchSymbol('in') then
            Error('Expected "in" after "for" iterator');
          if not MatchSpace then
            Error('Expected "space"');
          if not MatchVariable(lIdentifier) then
            Error('Expected iterable "for"');
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "for"');

          // create another element in the sections stack
          Inc(lForStatementCount);
          lLastToken := ttFor;
          if lIdentifier = lIteratorName then
          begin
            Error('loop data source and its iterator cannot have the same name: ' + lIdentifier)
          end;
          // Check for .fields suffix for dataset field iteration
          lIsFieldIteration := 0;
          if lIdentifier.EndsWith('.fields', True) then
          begin
            lIdentifier := lIdentifier.Substring(0, lIdentifier.Length - 7); // Remove '.fields'
            lIsFieldIteration := 1;
          end;
          aTokens.Add(TToken.Create(lLastToken, lIdentifier, lIteratorName, -1, lIsFieldIteration));
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('endfor') then { endfor }
        begin
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag');
          if lForStatementCount = -1 then
          begin
            Error('endfor without loop');
          end;
          lLastToken := ttEndFor;
          aTokens.Add(TToken.Create(lLastToken, '', ''));
          Dec(lForStatementCount);
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('continue') then { continue }
        begin
          MatchSpace;
          lLastToken := ttContinue;
          aTokens.Add(TToken.Create(lLastToken, '', ''));
        end
        else if MatchSymbol('set') then { set variable }
        begin
          if not MatchSpace then
            Error('Expected <space> after "set"');
          if not MatchVariable(lIdentifier) then
            Error('Expected variable name after "set"');
          MatchSpace;
          if not MatchSymbol(':=') then
            Error('Expected ":=" after variable name in "set"');
          MatchSpace;

          // Check what follows: @(expr), "string", number, true/false, or variable
          if MatchExpression(lVarName) then
          begin
            // Expression @(...)
            MatchSpace;
            if not MatchEndTag then
              Error('Expected closing tag for "set"');
            lStartVerbatim := fCharIndex;
            lLastToken := ttSet;
            // Ref2=1 for expression
            aTokens.Add(TToken.Create(lLastToken, lIdentifier, lVarName, 0, 1));
          end
          else if MatchString(lVarName) then
          begin
            // String literal "..."
            MatchSpace;
            if not MatchEndTag then
              Error('Expected closing tag for "set"');
            lStartVerbatim := fCharIndex;
            lLastToken := ttSet;
            // Ref2=2 for string literal
            aTokens.Add(TToken.Create(lLastToken, lIdentifier, lVarName, 0, 2));
          end
          else if (fCharIndex < fInputString.Length) and
                  CharInSet(fInputString.Chars[fCharIndex], SignAndNumbers) then
          begin
            // Number literal (integer or float)
            lVarName := fInputString.Chars[fCharIndex];
            Inc(fCharIndex);
            while (fCharIndex < fInputString.Length) and
                  CharInSet(fInputString.Chars[fCharIndex], Numbers) do
            begin
              lVarName := lVarName + fInputString.Chars[fCharIndex];
              Inc(fCharIndex);
            end;
            if MatchSymbol('.') then
            begin
              // Float
              lVarName := lVarName + '.';
              while (fCharIndex < fInputString.Length) and
                    CharInSet(fInputString.Chars[fCharIndex], Numbers) do
              begin
                lVarName := lVarName + fInputString.Chars[fCharIndex];
                Inc(fCharIndex);
              end;
              MatchSpace;
              if not MatchEndTag then
                Error('Expected closing tag for "set"');
              lStartVerbatim := fCharIndex;
              lLastToken := ttSet;
              // Ref2=6 for float literal
              aTokens.Add(TToken.Create(lLastToken, lIdentifier, lVarName, 0, 6));
            end
            else
            begin
              // Integer
              MatchSpace;
              if not MatchEndTag then
                Error('Expected closing tag for "set"');
              lStartVerbatim := fCharIndex;
              lLastToken := ttSet;
              // Ref2=5 for integer literal
              aTokens.Add(TToken.Create(lLastToken, lIdentifier, lVarName, 0, 5));
            end;
          end
          else if MatchVariable(lVarName) then
          begin
            // Variable reference or boolean literal (case-insensitive)
            if SameText(lVarName, 'true') then
            begin
              MatchSpace;
              if not MatchEndTag then
                Error('Expected closing tag for "set"');
              lStartVerbatim := fCharIndex;
              lLastToken := ttSet;
              // Ref2=3 for boolean true
              aTokens.Add(TToken.Create(lLastToken, lIdentifier, 'true', 0, 3));
            end
            else if SameText(lVarName, 'false') then
            begin
              MatchSpace;
              if not MatchEndTag then
                Error('Expected closing tag for "set"');
              lStartVerbatim := fCharIndex;
              lLastToken := ttSet;
              // Ref2=4 for boolean false
              aTokens.Add(TToken.Create(lLastToken, lIdentifier, 'false', 0, 4));
            end
            else
            begin
              // Variable reference with optional filters
              SetLength(lFilters, 0);
              if MatchSymbol('|') then
                MatchFilters(lVarName, lFilters);
              MatchSpace;
              if not MatchEndTag then
                Error('Expected closing tag for "set"');
              lStartVerbatim := fCharIndex;
              lLastToken := ttSet;
              // Ref2=0 for variable reference, Ref1=filter count
              aTokens.Add(TToken.Create(lLastToken, lIdentifier, lVarName, Length(lFilters), 0));
              AddFilterTokens(aTokens, lFilters);
            end;
          end
          else
            Error('Expected literal value, variable reference, or expression (@(...)) in "set"');
        end
        else if MatchSymbol('endif') then { endif }
        begin
          MatchSpace;
          if lIfStatementCount = -1 then
          begin
            Error('"endif" without "if"');
          end;
          if not MatchEndTag then
          begin
            Error('Expected closing tag for "endif"');
          end;

          lLastToken := ttEndIf;
          aTokens.Add(TToken.Create(lLastToken, '', ''));

          Dec(lIfStatementCount);
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('if') then
        begin
          if not MatchSpace then
          begin
            Error('Expected <space> after "if"');
          end;

          // Check for expression syntax: @(expr)
          if MatchExpression(lIdentifier) then
          begin
            MatchSpace;
            if not MatchEndTag then
              Error('Expected closing tag for "if" after expression');

            lLastToken := ttIfThen;
            aTokens.Add(TToken.Create(lLastToken, '', ''));
            Inc(lIfStatementCount);
            lStartVerbatim := fCharIndex;

            // Use ttExpression for the condition (Ref2 = 1 marks it as expression-based)
            lLastToken := ttBoolExpression;
            aTokens.Add(TToken.Create(lLastToken, lIdentifier, '', 0, 1 { 1 = expression mode }));
          end
          else
          begin
            // Original variable-based condition
            lNegation := MatchSymbol('!');
            MatchSpace;
            if not MatchVariable(lIdentifier) then
              Error('Expected identifier after "if"');
            SetLength(lFilters, 0);
            if MatchSymbol('|') then
            begin
              MatchFilters(lIdentifier, lFilters);
            end;
            MatchSpace;
            if not MatchEndTag then
              Error('Expected closing tag for "if" after "' + lIdentifier + '"');
            if lNegation then
            begin
              lIdentifier := '!' + lIdentifier;
            end;
            lLastToken := ttIfThen;
            aTokens.Add(TToken.Create(lLastToken, '' { lIdentifier } , ''));
            Inc(lIfStatementCount);
            lStartVerbatim := fCharIndex;

            lLastToken := ttBoolExpression;
            { Ref1 now stores number of filters (0 = no filter, >0 = filter count) }
            aTokens.Add(TToken.Create(lLastToken, lIdentifier, '', Length(lFilters), -1 { no html escape } ));

            // add filter tokens
            AddFilterTokens(aTokens, lFilters);
          end;
        end
        else if MatchSymbol('else') then
        begin
          if not MatchEndTag then
            Error('Expected closing tag for "else"');

          lLastToken := ttElse;
          aTokens.Add(TToken.Create(lLastToken, '', ''));
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('include') then { include }
        begin
          if not MatchSpace then
            Error('Expected "space" after "include"');

          // Include can be: string literal or @(expression)
          // For dynamic includes use @(expression) only - e.g., @(varname) or @("prefix" + varname + ".tpro")
          lIsDynamicInclude := False;

          if MatchExpression(lStringValue) then
          begin
            // Expression for dynamic include
            lIncludeFileName := lStringValue;
            lIsDynamicInclude := True;
          end
          else if MatchString(lStringValue) then
          begin
            // Static string literal - compile time include
            lIncludeFileName := lStringValue;
            lIsDynamicInclude := False;
          end
          else
          begin
            Error('Expected string or @(expression) after "include"');
          end;

          MatchSpace;

          // Check for variable mappings: {{include "file", var1 = source1, var2 = source2}}
          lHasMappings := False;
          SetLength(lMappingTargets, 0); // Reset for each include
          lMappingTokens := TList<TToken>.Create;
          try
            if MatchSymbol(',') then
            begin
              lHasMappings := True;
              // Parse variable mappings
              repeat
                MatchSpace;
                if not MatchVariable(lTargetVar) then
                  Error('Expected variable name in include mapping');
                MatchSpace;
                if not MatchSymbol('=') then
                  Error('Expected "=" in include mapping');
                MatchSpace;

                // Parse source value (similar to set)
                // Order: string literal, expression, boolean, number, variable (identifier)
                lMappingToken.TokenType := ttSet;
                lMappingToken.Value1 := lTargetVar;
                lMappingToken.Ref1 := 0; // filter count

                if MatchString(lStringValue) then
                begin
                  // String literal
                  lMappingToken.Value2 := lStringValue;
                  lMappingToken.Ref2 := 2; // mode: string
                end
                else if MatchExpression(lStringValue) then
                begin
                  // Expression
                  lMappingToken.Value2 := lStringValue;
                  lMappingToken.Ref2 := 1; // mode: expression
                end
                else if MatchSymbol('true') then
                begin
                  lMappingToken.Ref2 := 3; // mode: bool true
                end
                else if MatchSymbol('false') then
                begin
                  lMappingToken.Ref2 := 4; // mode: bool false
                end
                else if CharInSet(CurrentChar, ['0'..'9', '-']) then
                begin
                  // Try to match a number (starts with digit or minus sign)
                  lNumStr := '';
                  lIsFloat := False;
                  lIsNeg := MatchSymbol('-');
                  while (fCharIndex <= Length(fInputString)) and (CharInSet(CurrentChar, ['0'..'9', '.'])) do
                  begin
                    if CurrentChar = '.' then
                      lIsFloat := True;
                    lNumStr := lNumStr + CurrentChar;
                    Step;
                  end;
                  if lNumStr = '' then
                    Error('Expected number after "-" in include mapping');
                  if lIsNeg then
                    lNumStr := '-' + lNumStr;
                  lMappingToken.Value2 := lNumStr;
                  if lIsFloat then
                    lMappingToken.Ref2 := 6 // mode: float
                  else
                    lMappingToken.Ref2 := 5; // mode: integer
                end
                else if MatchVariable(lSourceVar) then
                begin
                  // Variable reference (identifier)
                  lMappingToken.Value2 := lSourceVar;
                  lMappingToken.Ref2 := 0; // mode: variable
                end
                else
                  Error('Expected value in include mapping');

                // Check for duplicate target variable
                for I := 0 to High(lMappingTargets) do
                begin
                  if SameText(lMappingTargets[I], lTargetVar) then
                    Error('Duplicate variable "' + lTargetVar + '" in include mapping');
                end;

                lMappingTokens.Add(lMappingToken);
                SetLength(lMappingTargets, Length(lMappingTargets) + 1);
                lMappingTargets[High(lMappingTargets)] := lTargetVar;

                MatchSpace;
              until not MatchSymbol(',');
            end;

            if lIsDynamicInclude and lHasMappings then
              Error('Dynamic include does not support variable mappings');

            if not MatchEndTag then
              Error('Expected closing tag for "include"');

            if lIsDynamicInclude then
            begin
              // Dynamic include - emit ttInclude token for runtime evaluation
              lIncludeToken.TokenType := ttInclude;
              lIncludeToken.Value1 := lIncludeFileName; // expression to evaluate
              lIncludeToken.Value2 := aFileNameRefPath; // base path for resolving relative paths
              aTokens.Add(lIncludeToken);
              Inc(lContentOnThisLine);
            end
            else
            begin
              // Static include - compile at compile time
              // Read the included file
              try
                if TDirectory.Exists(aFileNameRefPath) then
                begin
                  lCurrentFileName := TPath.GetFullPath(TPath.Combine(aFileNameRefPath, lIncludeFileName));
                end
                else
                begin
                  lCurrentFileName := TPath.GetFullPath(TPath.Combine(TPath.GetDirectoryName(aFileNameRefPath), lIncludeFileName));
                end;
                lTemplateSource := TFile.ReadAllText(lCurrentFileName, fEncoding);
              except
                on E: Exception do
                begin
                  Error('Cannot read "' + lIncludeFileName + '"');
                end;
              end;
              Inc(lContentOnThisLine);

              // Generate tokens
              if lHasMappings then
              begin
                // Add ttIncludeStart with target variable names
                lIncludeStartToken.TokenType := ttIncludeStart;
                lIncludeStartToken.Value1 := String.Join(',', lMappingTargets);
                lIncludeStartToken.Ref1 := Length(lMappingTargets);
                aTokens.Add(lIncludeStartToken);

                // Add mapping tokens (ttSet)
                for lMapToken in lMappingTokens do
                  aTokens.Add(lMapToken);
              end;

              // Compile the included template
              InternalCompileIncludedTemplate(lTemplateSource, aTokens, lCurrentFileName, [coIgnoreSysVersion, coParentTemplate]);

              if lHasMappings then
              begin
                // Add ttIncludeEnd
                lIncludeEndToken.TokenType := ttIncludeEnd;
                aTokens.Add(lIncludeEndToken);
              end;
            end;
          finally
            lMappingTokens.Free;
          end;
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('extends') then { extends }
        begin
          if lLayoutFound then
            Error('Duplicated "extends"');
          lLayoutFound := True;
          // An included file cannot use extends (only parent templates from extends can)
          if coParentTemplate in fOptions then
            Error('An included file cannot use "extends"');

          if not MatchSpace then
            Error('Expected "space" after "extends"');

          if not MatchString(lStringValue) then
          begin
            Error('Expected string after "extends"');
          end;
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "extends"');
          if TDirectory.Exists(aFileNameRefPath) then
          begin
            lCurrentFileName := TPath.GetFullPath(TPath.Combine(aFileNameRefPath, lStringValue));
          end
          else
          begin
            lCurrentFileName := TPath.GetFullPath(TPath.Combine(TPath.GetDirectoryName(aFileNameRefPath), lStringValue));
          end;
          // Check for circular inheritance before reading file
          if fInheritanceChain.Contains(lCurrentFileName) then
            raise ETProCompilerException.Create('Circular template inheritance detected');
          fInheritanceChain.Add(lCurrentFileName);
          try
            lTemplateSource := TFile.ReadAllText(lCurrentFileName, fEncoding);
          except
            on E: Exception do
            begin
              Error('Cannot read "' + lStringValue + '"');
            end;
          end;
          Inc(lContentOnThisLine);
          aTokens.Add(TToken.Create(ttInfo, STR_BEGIN_OF_LAYOUT, ''));
          InternalCompileIncludedTemplate(lTemplateSource, aTokens, lCurrentFileName, [coIgnoreSysVersion]);
          aTokens.Add(TToken.Create(ttInfo, STR_END_OF_LAYOUT, ''));
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('block') then { block - parent }
        begin
          if not MatchSpace then
            Error('Expected "space" after "block"');
          if not MatchString(lStringValue) then
            Error('Expected string after "block"');
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "block"');
          lLastToken := ttBlock;
          aTokens.Add(TToken.Create(lLastToken, lStringValue, ''));
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('endblock') then { endblock - parent }
        begin
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "endblock"');
          lLastToken := ttEndBlock;
          aTokens.Add(TToken.Create(lLastToken, '', ''));
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('inherited') then { inherited - render parent block content }
        begin
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "inherited"');
          lLastToken := ttInherited;
          aTokens.Add(TToken.Create(lLastToken, '', ''));
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('macro') then { macro definition }
        begin
          if not MatchSpace then
            Error('Expected "space" after "macro"');
          if not MatchVariable(lIdentifier) then
            Error('Expected macro name after "macro"');

          // Parse macro parameters: macro name(param1, param2='default', ...)
          lFuncParams := GetMacroParameters;

          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "macro"');

          lLastToken := ttMacro;
          // Value1 = macro name, Ref1 = parameter count
          aTokens.Add(TToken.Create(lLastToken, lIdentifier, '', Length(lFuncParams), -1));

          // Add macro parameters as tokens
          for I := 0 to Length(lFuncParams) - 1 do
          begin
            aTokens.Add(CreateFilterParameterToken(@lFuncParams[I]));
          end;

          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('endmacro') then { endmacro }
        begin
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "endmacro"');
          lLastToken := ttEndMacro;
          aTokens.Add(TToken.Create(lLastToken, '', ''));
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('>') then // macro call: {{>macroname(args)}}
        begin
          MatchSpace;
          if not MatchVariable(lIdentifier) then
            Error('Expected macro name after ">"');

          // Parse call parameters
          lFuncParams := GetMacroParameters;

          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for macro call');

          lLastToken := ttCallMacro;
          Inc(lContentOnThisLine);
          // Value1 = macro name, Ref1 = parameter count
          aTokens.Add(TToken.Create(lLastToken, lIdentifier, '', Length(lFuncParams), -1));

          // Add call parameters as tokens
          for I := 0 to Length(lFuncParams) - 1 do
          begin
            aTokens.Add(CreateFilterParameterToken(@lFuncParams[I]));
          end;

          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('exit') then { exit }
        begin
          MatchSpace;
          lLastToken := ttExit;
          aTokens.Add(TToken.Create(lLastToken, '', ''));
          lLastToken := ttEOF;
          aTokens.Add(TToken.Create(lLastToken, '', ''));
          Break;
        end
        else if MatchString(lStringValue) then { string }
        begin
          lLastToken := ttLiteralString;
          Inc(lContentOnThisLine);
          lRef2 := IfThen(MatchSymbol('$'), 1, -1);
          // {{value$}} means no escaping
          MatchSpace;
          InternalMatchFilter(lStringValue, lStartVerbatim, ttLiteralString, aTokens, lRef2);
        end
        else if MatchSymbol('#') then
        begin
          while not MatchEndTag do
          begin
            Step;
          end;
          lStartVerbatim := fCharIndex;
          lLastToken := ttComment; { will not added into compiled template }
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
      else if Result[index] = ' ' then
      begin
        bCapitalizeNext := True;
      end;
    end; // for
  end; // if
end;

procedure TTProCompiler.Error(const aMessage: string);
begin
  raise ETProCompilerException.CreateFmt('%s - (got: "%s") at line %d in file %s',
    [aMessage, GetSubsequentText, fCurrentLine, fCurrentFileName]);
end;

procedure TTProCompiler.ProcessJumps(const aTokens: TList<TToken>);
var
  lForInStack: TStack<Int64>;
  lContinueStack: TStack<Int64>;
  lIfStatementStack: TStack<TIfThenElseIndex>;
  I, J: Int64;
  lToken: TToken;
  lForAddress: Int64;
  lIfStackItem: TIfThenElseIndex;
  lCheckForUnbalancedPair: Boolean;
  lTmpContinueAddress: Int64;
  lBlockDict: TObjectDictionary<string, TList<TBlockAddress>>;
  lBlockList: TList<TBlockAddress>;
  lBlockAddress: TBlockAddress;
  lBlockStack: TStack<string>; // Stack for nested blocks
  lCurrentLevel: Integer; // 0 = page, 1+ = layouts (higher = more base)
  lBlockName: string;
  // Variables for parent block lookup (moved from inline for Delphi 10 Seattle)
  K: Integer;
  lOtherLevel: Integer;
  lParentBlockAddr: Int64;
  lThisLevel: Integer;
  lMinParentLevel: Integer;
  lMostDerivedIdx: Integer;
  lMostDerivedLevel: Integer;
begin
  lCurrentLevel := 0; // Start at page level
  lCheckForUnbalancedPair := True;
  lBlockDict := TObjectDictionary<string, TList<TBlockAddress>>.Create([doOwnsValues], TTProEqualityComparer.Create);
  try
    lBlockStack := TStack<string>.Create;
    try
      lForInStack := TStack<Int64>.Create;
      try
        lContinueStack := TStack<Int64>.Create;
        try
          lIfStatementStack := TStack<TIfThenElseIndex>.Create;
          try
            // First pass: collect all blocks with their levels
            for I := 0 to aTokens.Count - 1 do
            begin
              case aTokens[I].TokenType of
                ttInfo:
                  begin
                    if aTokens[I].Value1 = STR_BEGIN_OF_LAYOUT then
                      Inc(lCurrentLevel)
                    else if aTokens[I].Value1 = STR_END_OF_LAYOUT then
                      Dec(lCurrentLevel);
                  end;

                ttFor:
                  begin
                    if lContinueStack.Count > 0 then
                    begin
                      Error('Continue stack corrupted');
                    end;
                    lForInStack.Push(I);
                  end;

                ttEndFor:
                  begin
                    { ttFor.Ref1 --> endfor }
                    lForAddress := lForInStack.Pop;
                    lToken := aTokens[lForAddress];
                    lToken.Ref1 := I;
                    aTokens[lForAddress] := lToken;

                    { ttEndFor.Ref1 --> for }
                    lToken := aTokens[I];
                    lToken.Ref1 := lForAddress;
                    aTokens[I] := lToken;

                    { if there's a ttContinue (or more than one), it must jump to endfor }
                    while lContinueStack.Count > 0 do
                    begin
                      lTmpContinueAddress := lContinueStack.Pop;
                      lToken := aTokens[lTmpContinueAddress];
                      lToken.Ref1 := I;
                      aTokens[lTmpContinueAddress] := lToken;
                    end;
                  end;

                ttContinue:
                  begin
                    lContinueStack.Push(I);
                  end;

                ttBlock:
                  begin
                    lBlockName := aTokens[I].Value1;
                    lBlockStack.Push(lBlockName);

                    // Get or create list for this block name
                    if not lBlockDict.TryGetValue(lBlockName, lBlockList) then
                    begin
                      lBlockList := TList<TBlockAddress>.Create;
                      lBlockDict.Add(lBlockName, lBlockList);
                    end;

                    // Check for duplicate block at same level
                    for J := 0 to lBlockList.Count - 1 do
                    begin
                      if lBlockList[J].Level = lCurrentLevel then
                        Error('Duplicated block "' + lBlockName + '" at level ' + IntToStr(lCurrentLevel));
                    end;

                    // Add this block to the list
                    lBlockList.Add(TBlockAddress.Create(I, 0, lCurrentLevel));
                  end;

                ttEndBlock:
                  begin
                    if lBlockStack.Count = 0 then
                    begin
                      Error('endblock without block');
                    end;
                    lBlockName := lBlockStack.Pop;

                    // Update EndBlockAddress for this block
                    if lBlockDict.TryGetValue(lBlockName, lBlockList) then
                    begin
                      for J := 0 to lBlockList.Count - 1 do
                      begin
                        lBlockAddress := lBlockList[J];
                        if (lBlockAddress.Level = lCurrentLevel) and (lBlockAddress.EndBlockAddress = 0) then
                        begin
                          lBlockAddress.EndBlockAddress := I;
                          lBlockList[J] := lBlockAddress;
                          // Also update ttBlock.Ref2 to point to endblock
                          lToken := aTokens[lBlockAddress.BeginBlockAddress];
                          lToken.Ref2 := I;
                          aTokens[lBlockAddress.BeginBlockAddress] := lToken;
                          Break;
                        end;
                      end;
                    end;
                  end;

                { ttIfThen.Ref1 points always to relative else (if present otherwise -1) }
                { ttIfThen.Ref2 points always to relative endif }

                ttIfThen:
                  begin
                    lIfStackItem.IfIndex := I;
                    lIfStackItem.ElseIndex := -1;
                    { -1 means: "there isn't ttElse" }
                    lIfStatementStack.Push(lIfStackItem);
                  end;
                ttElse:
                  begin
                    lIfStackItem := lIfStatementStack.Pop;
                    lIfStackItem.ElseIndex := I;
                    lIfStatementStack.Push(lIfStackItem);
                  end;
                ttEndIf:
                  begin
                    lIfStackItem := lIfStatementStack.Pop;

                    { fixup ifthen }
                    lToken := aTokens[lIfStackItem.IfIndex];
                    lToken.Ref2 := I;
                    { ttIfThen.Ref2 points always to relative endif }
                    lToken.Ref1 := lIfStackItem.ElseIndex;
                    { ttIfThen.Ref1 points always to relative else (if present, otherwise -1) }
                    aTokens[lIfStackItem.IfIndex] := lToken;

                    { fixup else }
                    if lIfStackItem.ElseIndex > -1 then
                    begin
                      lToken := aTokens[lIfStackItem.ElseIndex];
                      lToken.Ref2 := I;
                      { ttElse.Ref2 points always to relative endif }
                      aTokens[lIfStackItem.ElseIndex] := lToken;
                    end;
                  end;
                ttMacro:
                  begin
                    // Store macro start position, will be linked to ttEndMacro later
                    lForInStack.Push(I); // Reuse the same stack for simplicity
                  end;

                ttEndMacro:
                  begin
                    // Link ttMacro to ttEndMacro
                    lForAddress := lForInStack.Pop;
                    lToken := aTokens[lForAddress];
                    lToken.Ref2 := I; // ttMacro.Ref2 -> ttEndMacro
                    aTokens[lForAddress] := lToken;

                    lToken := aTokens[I];
                    lToken.Ref1 := lForAddress; // ttEndMacro.Ref1 -> ttMacro
                    aTokens[I] := lToken;
                  end;

                ttExit:
                  begin
                    lCheckForUnbalancedPair := False;
                  end;
              end;
            end; // for

            // Second pass: link blocks across levels
            // For each block name, find the most derived override (lowest level)
            // and set Ref1 of all ancestor blocks to point to it
            // Also set Value2 to store parent block address for {{inherited}}
            for lBlockName in lBlockDict.Keys do
            begin
              lBlockList := lBlockDict[lBlockName];

              // For blocks with no overrides (single instance), set Ref1 = -1
              if lBlockList.Count = 1 then
              begin
                lBlockAddress := lBlockList[0];
                lToken := aTokens[lBlockAddress.BeginBlockAddress];
                lToken.Ref1 := -1;  // No override
                aTokens[lBlockAddress.BeginBlockAddress] := lToken;
              end
              else if lBlockList.Count > 1 then
              begin
                // Sort by level (we need to process from highest to lowest)
                // Find the most derived block (lowest level)
                lMostDerivedIdx := 0;
                lMostDerivedLevel := lBlockList[0].Level;
                for J := 1 to lBlockList.Count - 1 do
                begin
                  if lBlockList[J].Level < lMostDerivedLevel then
                  begin
                    lMostDerivedLevel := lBlockList[J].Level;
                    lMostDerivedIdx := J;
                  end;
                end;

                // For all blocks that are not the most derived, set Ref1 to jump to most derived
                for J := 0 to lBlockList.Count - 1 do
                begin
                  if J <> lMostDerivedIdx then
                  begin
                    lBlockAddress := lBlockList[J];
                    lToken := aTokens[lBlockAddress.BeginBlockAddress];
                    lToken.Ref1 := lBlockList[lMostDerivedIdx].BeginBlockAddress;

                    // Find parent block (next level UP from this one) for {{inherited}}
                    // Higher level = more ancestral (base template)
                    lParentBlockAddr := -1;
                    lThisLevel := lBlockAddress.Level;
                    lMinParentLevel := MaxInt;
                    for K := 0 to lBlockList.Count - 1 do
                    begin
                      lOtherLevel := lBlockList[K].Level;
                      if (lOtherLevel > lThisLevel) and (lOtherLevel < lMinParentLevel) then
                      begin
                        lMinParentLevel := lOtherLevel;
                        lParentBlockAddr := lBlockList[K].BeginBlockAddress;
                      end;
                    end;
                    // Store parent block address in Value2 for {{inherited}}
                    lToken.Value2 := IntToStr(lParentBlockAddr);
                    aTokens[lBlockAddress.BeginBlockAddress] := lToken;
                  end;
                end;

                // Also set Ref1 = -1 and Value2 for the most derived block (no further override)
                lBlockAddress := lBlockList[lMostDerivedIdx];
                lToken := aTokens[lBlockAddress.BeginBlockAddress];
                lToken.Ref1 := -1;  // No further override
                lParentBlockAddr := -1;
                lThisLevel := lBlockAddress.Level;
                lMinParentLevel := MaxInt;
                for J := 0 to lBlockList.Count - 1 do
                begin
                  lOtherLevel := lBlockList[J].Level;
                  if (lOtherLevel > lThisLevel) and (lOtherLevel < lMinParentLevel) then
                  begin
                    lMinParentLevel := lOtherLevel;
                    lParentBlockAddr := lBlockList[J].BeginBlockAddress;
                  end;
                end;
                lToken.Value2 := IntToStr(lParentBlockAddr);
                aTokens[lBlockAddress.BeginBlockAddress] := lToken;
              end;
            end;

            if lCheckForUnbalancedPair and (lIfStatementStack.Count > 0) then
            begin
              Error('Unbalanced "if" - expected "endif"');
            end;
            if lCheckForUnbalancedPair and (lForInStack.Count > 0) then
            begin
              Error('Unbalanced "for" - expected "endfor"');
            end;
            if lBlockStack.Count > 0 then
            begin
              Error('Unbalanced "block" - expected "endblock" for block "' + lBlockStack.Peek + '"');
            end;
          finally
            lIfStatementStack.Free;
          end;
        finally
          lContinueStack.Free;
        end;
      finally
        lForInStack.Free;
      end;
    finally
      lBlockStack.Free;
    end;
  finally
    lBlockDict.Free;
  end;
  // TTProCompiledTemplate.InternalDumpToFile('debug.compiled.txt', aTokens);
end;

function TTProCompiler.GetFunctionParameters: TArray<TFilterParameter>;
var
  lFuncPar: TFilterParameter;
begin
  Result := [];
  while MatchSymbol(',') do
  begin
    MatchSpace;
    if not MatchFilterParamValue(lFuncPar) then
      Error('Expected function parameter');
    Result := Result + [lFuncPar];
    MatchSpace;
  end;
end;

function TTProCompiler.GetMacroParameters: TArray<TFilterParameter>;
var
  lFuncPar: TFilterParameter;
begin
  Result := [];
  MatchSpace;
  if not MatchSymbol('(') then
    Exit; // No parameters

  MatchSpace;
  // Check for empty parameter list ()
  if MatchSymbol(')') then
    Exit;

  // Parse first parameter
  if not MatchFilterParamValue(lFuncPar) then
    Error('Expected macro parameter');
  Result := Result + [lFuncPar];
  MatchSpace;

  // Parse remaining parameters
  while MatchSymbol(',') do
  begin
    MatchSpace;
    if not MatchFilterParamValue(lFuncPar) then
      Error('Expected macro parameter');
    Result := Result + [lFuncPar];
    MatchSpace;
  end;

  if not MatchSymbol(')') then
    Error('Expected ")" after macro parameters');
end;

function TTProCompiler.GetSubsequentText: String;
var
  I: Integer;
begin
  Result := CurrentChar;
  if Result = #0 then
  begin
    Result := '<eof>';
  end
  else
  begin
    Step;
    I := 0;
    while (CurrentChar <> #0) and (CurrentChar <> END_TAG[1]) and (I < 20) do
    begin
      Result := Result + CurrentChar;
      Step;
      Inc(I);
    end;
  end;
end;

procedure TTProCompiledTemplate.CheckParNumber(const aHowManyPars: Integer; const aParameters: TArray<TFilterParameter>);
begin
  CheckParNumber(aHowManyPars, aHowManyPars, aParameters);
end;

function TTProCompiledTemplate.ExecuteStringFilter(const aFunctionName: string;
  var aParameters: TArray<TFilterParameter>; const aValue: TValue;
  const aExecuteAsFilterOnAValue: Boolean; out aResult: TValue): Boolean;
var
  lStrValue: string;
  lValue: TValue;
  lVarValue: TValue;
  lIntegerPar1: Integer;
begin
  Result := True;
  if SameText(aFunctionName, 'uppercase') then
  begin
    if aExecuteAsFilterOnAValue then
    begin
      CheckParNumber(0, aParameters);
      aResult := UpperCase(aValue.AsString);
    end
    else
    begin
      CheckParNumber(1, aParameters);
      aResult := UpperCase(aParameters[0].ParStrText);
    end;
  end
  else if SameText(aFunctionName, 'lowercase') then
  begin
    if aExecuteAsFilterOnAValue then
    begin
      CheckParNumber(0, aParameters);
      aResult := lowercase(aValue.AsString);
    end
    else
    begin
      CheckParNumber(1, aParameters);
      aResult := lowercase(aParameters[0].ParStrText);
    end;
  end
  else if SameText(aFunctionName, 'capitalize') then
  begin
    if aExecuteAsFilterOnAValue then
    begin
      CheckParNumber(0, aParameters);
      aResult := CapitalizeString(aValue.AsString, True);
    end
    else
    begin
      CheckParNumber(1, aParameters);
      aResult := CapitalizeString(aParameters[0].ParStrText, True);
    end;
  end
  else if SameText(aFunctionName, 'trunc') then
  begin
    CheckParNumber(1, 1, aParameters);
    lStrValue := aValue.AsString.TrimRight;
    lIntegerPar1 := aParameters[0].ParIntValue;
    if Length(lStrValue) > lIntegerPar1 then
      aResult := lStrValue.Substring(0, aParameters[0].ParIntValue) + '...'
    else
      aResult := lStrValue;
  end
  else if SameText(aFunctionName, 'rpad') then
  begin
    if aValue.IsType<Integer> then
      lStrValue := aValue.AsInteger.ToString
    else if aValue.IsType<string> then
      lStrValue := aValue.AsString
    else
      FunctionError(aFunctionName, 'Invalid parameter/s');

    CheckParNumber(1, 2, aParameters);
    if Length(aParameters) = 1 then
      aResult := lStrValue.PadRight(aParameters[0].ParIntValue)
    else
      aResult := lStrValue.PadRight(aParameters[0].ParIntValue, aParameters[1].ParStrText.Chars[0]);
  end
  else if SameText(aFunctionName, 'lpad') then
  begin
    if not(aParameters[0].ParType in [fptInteger, fptVariable]) then
      FunctionError('lpad', 'Invalid parameter type');

    if aValue.IsType<Integer> then
      lStrValue := aValue.AsInteger.ToString
    else if aValue.IsType<string> then
      lStrValue := aValue.AsString
    else
      FunctionError(aFunctionName, 'Cannot apply function lpad on this value');

    CheckParNumber(1, 2, aParameters);
    if Length(aParameters) = 1 then
    begin
      if aParameters[0].ParType = fptVariable then
      begin
        lVarValue := GetVarAsTValue(aParameters[0].ParStrText);
        aResult := lStrValue.PadLeft(lVarValue.AsInteger);
      end
      else
        aResult := lStrValue.PadLeft(aParameters[0].ParIntValue);
    end
    else
      aResult := lStrValue.PadLeft(aParameters[0].ParIntValue, aParameters[1].ParStrText.Chars[0]);
  end
  else if SameText(aFunctionName, 'contains') then
  begin
    if Length(aParameters) <> 1 then
      FunctionError(aFunctionName, 'expected 1 parameter');
    if not(aParameters[0].ParType in [fptString, fptVariable]) then
      FunctionError(aFunctionName, 'Invalid parameter type');
    if aParameters[0].ParType = fptVariable then
    begin
      lValue := GetVarAsTValue(aParameters[0].ParStrText);
      lStrValue := GetNullableTValueAsTValue(@lValue, aParameters[0].ParStrText).AsString;
    end
    else
      lStrValue := aParameters[0].ParStrText;
    aResult := aValue.AsString.Contains(lStrValue);
  end
  else if SameText(aFunctionName, 'icontains') then
  begin
    if Length(aParameters) <> 1 then
      FunctionError(aFunctionName, 'expected 1 parameter');
    if not(aParameters[0].ParType in [fptString, fptVariable]) then
      FunctionError(aFunctionName, 'Invalid parameter type');
    if aParameters[0].ParType = fptVariable then
    begin
      lValue := GetVarAsTValue(aParameters[0].ParStrText);
      lStrValue := GetNullableTValueAsTValue(@lValue, aParameters[0].ParStrText).AsString;
    end
    else
      lStrValue := aParameters[0].ParStrText;
    aResult := aValue.AsString.ToLowerInvariant.Contains(lStrValue);
  end
  else
    Result := False;
end;

function TTProCompiledTemplate.ExecuteDateFilter(const aFunctionName: string;
  var aParameters: TArray<TFilterParameter>; const aValue: TValue;
  const aVarNameWhereShoudBeApplied: String; out aResult: TValue): Boolean;
var
  lDateValue: TDateTime;
  lNullableDate: NullableTDate;
  lSQLTimestampOffset: TSQLTimeStampOffset;
  lIsNull: Boolean;
begin
  Result := True;
  if SameText(aFunctionName, 'datetostr') then
  begin
    if aValue.IsEmpty then
      aResult := ''
    else if aValue.TryAsType<TDateTime>(lDateValue) then
    begin
      if Length(aParameters) = 0 then
        aResult := DateToStr(lDateValue, fLocaleFormatSettings)
      else
      begin
        CheckParNumber(1, aParameters);
        aResult := FormatDateTime(aParameters[0].ParStrText, lDateValue);
      end;
    end
    else if aValue.TypeInfo = TypeInfo(NullableTDate) then
    begin
      lNullableDate := aValue.AsType<NullableTDate>(True);
      if lNullableDate.IsNull then
        aResult := ''
      else
      begin
        lDateValue := lNullableDate.Value;
        if Length(aParameters) = 0 then
          aResult := DateToStr(lDateValue, fLocaleFormatSettings)
        else
        begin
          CheckParNumber(1, aParameters);
          aResult := FormatDateTime(aParameters[0].ParStrText, lDateValue);
        end;
      end;
    end
    else
      FunctionError(aFunctionName, 'Invalid date ' + GetTValueVarAsString(@aValue, lIsNull, aVarNameWhereShoudBeApplied));
  end
  else if SameText(aFunctionName, 'datetimetostr') or SameText(aFunctionName, 'formatdatetime') then
  begin
    if aValue.IsEmpty then
      aResult := ''
    else if aValue.TryAsType<TDateTime>(lDateValue) then
    begin
      if Length(aParameters) = 0 then
        aResult := DateTimeToStr(lDateValue, fLocaleFormatSettings)
      else
      begin
        CheckParNumber(1, aParameters);
        aResult := FormatDateTime(aParameters[0].ParStrText, lDateValue);
      end;
    end
    else if aValue.TryAsType<TSQLTimeStampOffset>(lSQLTimestampOffset) then
    begin
      lDateValue := SQLTimeStampOffsetToDateTime(lSQLTimestampOffset);
      if Length(aParameters) = 0 then
        aResult := DateTimeToStr(lDateValue, fLocaleFormatSettings)
      else
      begin
        CheckParNumber(1, aParameters);
        aResult := FormatDateTime(aParameters[0].ParStrText, lDateValue);
      end;
    end
    else
      FunctionError(aFunctionName, 'Invalid datetime ' + aValue.AsString.QuotedString);
  end
  else
    Result := False;
end;

function TTProCompiledTemplate.ExecuteFilter(aFunctionName: string; var aParameters: TArray<TFilterParameter>; aValue: TValue;
  const aVarNameWhereShoudBeApplied: String): TValue;
var
  lFunc: TTProTemplateFunction;
  lAnonFunc: TTProTemplateAnonFunction;
  lIntegerPar1: Integer;
  lDecimalMask: string;
  lExecuteAsFilterOnAValue: Boolean;
  lValue, lVarValue: TValue;
  lExtendedValue: Extended;
  lInt64: Int64;

  procedure CheckParamType(const FunctionName: String; const FilterParameter: PFilterParameter; const Types: TFilterParameterTypes);
  begin
    if not(FilterParameter.ParType in Types) then
    begin
      FunctionError(FunctionName, 'Invalid parameter type');
    end;
  end;

begin
  lExecuteAsFilterOnAValue := not aVarNameWhereShoudBeApplied.IsEmpty;
  aFunctionName := lowercase(aFunctionName);

  // Try string filters first
  if ExecuteStringFilter(aFunctionName, aParameters, aValue, lExecuteAsFilterOnAValue, Result) then
    Exit;

  // Try date filters
  if ExecuteDateFilter(aFunctionName, aParameters, aValue, aVarNameWhereShoudBeApplied, Result) then
    Exit;

  if SameText(aFunctionName, 'gt') then
  begin
    Result := ComparandOperator(ctGT, aValue, aParameters, fLocaleFormatSettings);
  end
  else if SameText(aFunctionName, 'ge') then
  begin
    Result := ComparandOperator(ctGE, aValue, aParameters, fLocaleFormatSettings);
  end
  else if SameText(aFunctionName, 'lt') then
  begin
    Result := ComparandOperator(ctLT, aValue, aParameters, fLocaleFormatSettings);
  end
  else if SameText(aFunctionName, 'le') then
  begin
    Result := ComparandOperator(ctLE, aValue, aParameters, fLocaleFormatSettings);
  end
  else if SameText(aFunctionName, 'eq') then
  begin
    Result := ComparandOperator(ctEQ, aValue, aParameters, fLocaleFormatSettings);
  end
  else if SameText(aFunctionName, 'ne') then
  begin
    Result := ComparandOperator(ctNE, aValue, aParameters, fLocaleFormatSettings);
  end
  else if SameText(aFunctionName, 'default') then
  begin
    CheckParNumber(1, aParameters);
    // Return the value if truthy, otherwise return the default parameter
    if IsTruthy(aValue) then
      Result := aValue
    else
    begin
      case aParameters[0].ParType of
        fptString:
          Result := aParameters[0].ParStrText;
        fptInteger:
          Result := aParameters[0].ParIntValue;
        fptFloat:
          Result := aParameters[0].ParFloatValue;
        fptVariable:
          Result := GetVarAsTValue(aParameters[0].ParStrText);
      else
        Result := aValue;
      end;
    end;
  end
  else if SameText(aFunctionName, 'mod') then
  begin
    if Length(aParameters) <> 1 then
      FunctionError(aFunctionName, 'expected 1 parameter');
    lValue := GetNullableTValueAsTValue(@aValue);
    if lValue.IsEmpty then
      Result := False
    else
    begin
      lInt64 := lValue.AsInt64;
      Result := lInt64 mod aParameters[0].ParIntValue;
    end;
  end
  else if SameText(aFunctionName, 'round') then
  begin
    CheckParNumber(1, aParameters);
    CheckParamType('round', @aParameters[0], [fptInteger, fptVariable]);
    lDecimalMask := '';

    if aParameters[0].ParType = fptVariable then
    begin
      lVarValue := GetVarAsTValue(aParameters[0].ParStrText);
      lIntegerPar1 := lVarValue.AsInteger;
    end
    else
    begin
      lIntegerPar1 := aParameters[0].ParIntValue;
    end;

    if lIntegerPar1 < 0 then
    begin
      lDecimalMask := '.' + StringOfChar('0', Abs(lIntegerPar1));
    end;
    lExtendedValue := RoundTo(aValue.AsExtended, lIntegerPar1);
    Result := FormatFloat('0' + lDecimalMask, lExtendedValue);
  end
  else if SameText(aFunctionName, 'formatfloat') then
  begin
    CheckParNumber(1, aParameters);
    CheckParamType('formatfloat', @aParameters[0], [TFilterParameterType.fptString]);
    if aValue.IsType<Integer> then
    begin
      Result := FormatFloat(aParameters[0].ParStrText, aValue.AsInteger, fLocaleFormatSettings);
    end
    else if aValue.IsType<Int64> then
    begin
      Result := FormatFloat(aParameters[0].ParStrText, aValue.AsInt64, fLocaleFormatSettings);
    end
    else if aValue.IsType<UInt64> then
    begin
      Result := FormatFloat(aParameters[0].ParStrText, aValue.AsUInt64, fLocaleFormatSettings);
    end
    else if aValue.IsType<TBcd> then
    begin
      Result := FormatFloat(aParameters[0].ParStrText, BcdToDouble(aValue.AsType<TBcd>), fLocaleFormatSettings);
    end
    else if aValue.IsType<Currency> then
    begin
      Result := FormatFloat(aParameters[0].ParStrText, aValue.AsCurrency, fLocaleFormatSettings);
    end
    else if aValue.IsType<Extended> or aValue.IsType<Double> then
    begin
    Result := FormatFloat(aParameters[0].ParStrText, aValue.AsExtended, fLocaleFormatSettings);
  end
    else
    begin
      Error('Invalid type passed to FormatFloat filter');
    end;
  end
  else if SameText(aFunctionName, 'totrue') then
  begin
    CheckParNumber(0, aParameters);
    Result := True;
  end
  else if SameText(aFunctionName, 'tofalse') then
  begin
    CheckParNumber(0, aParameters);
    Result := False;
  end
  else if SameText(aFunctionName, 'version') then
  begin
    if lExecuteAsFilterOnAValue then
    begin
      FunctionError(aFunctionName, 'cannot be applied to a value - [HINT] Use {{:|' + aFunctionName + '}}');
    end;
    CheckParNumber(0, aParameters);
    Result := TEMPLATEPRO_VERSION;
  end
  else if fTemplateFunctions.TryGetValue(aFunctionName, lFunc) then
  begin
    Result := lFunc(aValue, aParameters);
  end
  else if (fTemplateAnonFunctions <> nil) and fTemplateAnonFunctions.TryGetValue(aFunctionName, lAnonFunc) then
  begin
    Result := lAnonFunc(aValue, aParameters);
  end
  else
  begin
    Error(Format('Unknown function [%s]', [aFunctionName]));
  end;
end;

function HTMLEncode(s: string): string;
var
  I: Integer;
  r: string;
  b: UInt32;
  C4: UCS4Char;
begin
  I := 1;
  while I <= Length(s) do
  begin
    r := '';
    if (Char.IsHighSurrogate(S, I-1)) and (Char.IsLowSurrogate(S, I)) then
    begin
      C4 := Char.ConvertToUtf32(S, I-1);
      r := IntToStr(C4);
      s := s.Substring(0, I-1) + '&#' + r + ';' + s.Substring(I+1);
      Inc(I,r.Length + 3);
      Continue;
    end
    else
    begin
      b := Ord(S[I]);
      if b > 255 then
      begin
        if b = 8364 then
          r := 'euro'
        else
          r := '#' + IntToStr(b);
      end
      else
      begin
{$REGION 'entities'}
      case b of
        Ord('&'):
          r := 'amp';
        Ord('>'):
          r := 'gt';
        Ord('<'):
          r := 'lt';
        Ord('"'):
          r := 'quot';
        Ord(''''):
          r := '#39';
        160:
          r := 'nbsp';
        161:
          r := 'excl';
        162:
          r := 'cent';
        163:
          r := 'pound';
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
{$ENDREGION}
      end;
    end;

    if r <> '' then
    begin
      s := s.Substring(0, I-1) + '&' + r + ';' + s.Substring(I);
      Inc(I, Length(r) + 1);
    end;
    Inc(I);
  end;
  Result := s;
end;

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
  // lSize: UInt32;
  lValue1Size: UInt32;
  lValue2Size: UInt32;
  lTokenAsByte: byte;
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

  // lSize := aBytes.ReadUInt32;
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
  // lSize: UInt32;
  lValue1Bytes: TArray<byte>;
  lValue2Bytes: TArray<byte>;
  lValue1Length: UInt32;
  lValue2Length: UInt32;
  lTokenAsByte: byte;
begin

  // lSize :=
  // SizeOf(UInt32) + {total record size}
  // 1 +  //Token Type as Byte
  // 4 +  //Value1 Size in bytes as UInt32
  // Length(Value1) * SizeOf(Char) + //value1 bytes
  // 4 +  //Value2 Size in bytes as UInt32
  // Length(Value2) * SizeOf(Char) + //value2 bytes
  // 8 + //ref1
  // 8;  //ref2
  // aBytes.Write(lSize);

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

{ TTProCompiledTemplate }

constructor TTProCompiledTemplate.Create(Tokens: TList<TToken>);
begin
  inherited Create;
  fLoopsStack := TObjectList<TLoopStackItem>.Create(True);
  fIncludeSavedVarsStack := TObjectList<TIncludeSavedVars>.Create(True);
  fTokens := Tokens;
  fTemplateFunctions := TDictionary<string, TTProTemplateFunction>.Create(TTProEqualityComparer.Create);
  fTemplateAnonFunctions := nil;
  fMacros := TDictionary<string, TMacroDefinition>.Create(TTProEqualityComparer.Create);
  TTProConfiguration.RegisterHandlers(self);
  fLocaleFormatSettings := TFormatSettings.Invariant;
  fLocaleFormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  fEncoding := TEncoding.UTF8;
  fOutputLineEnding := lesLF;
  fDynamicIncludeCache := TDictionary<string, ITProCompiledTemplate>.Create;
end;

class function TTProCompiledTemplate.CreateFromFile(const FileName: String): ITProCompiledTemplate;
var
  lBR: TBinaryReader;
  lTokens: TList<TToken>;
begin
  lBR := TBinaryReader.Create(TBytesStream.Create(TFile.ReadAllBytes(FileName)), nil, True);
  try
    lTokens := TList<TToken>.Create;
    try
      try
        while True do
        begin
          lTokens.Add(TToken.CreateFromBytes(lBR));
          if lTokens.Last.TokenType = ttEOF then
          begin
            Break;
          end;
        end;
      except
        on E: Exception do
        begin
          raise ETProRenderException.CreateFmt
            ('Cannot load compiled template from [FILE: %s][CLASS: %s][MSG: %s] - consider to delete templates cache.',
            [FileName, E.ClassName, E.Message])
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
  fIncludeSavedVarsStack.Free;
  fTemplateFunctions.Free;
  fTemplateAnonFunctions.Free;
  fMacros.Free;
  fTokens.Free;
  fVariables.Free;
  fDynamicIncludeCache.Free;
  inherited;
end;

procedure TTProCompiledTemplate.DoOnGetValue(const DataSource, Members: string; var Value: TValue; var Handled: Boolean);
begin
  Handled := False;
  if Assigned(fOnGetValue) then
  begin
    fOnGetValue(DataSource, Members, Value, Handled);
  end;
end;

procedure TTProCompiledTemplate.DumpToFile(const FileName: String);
begin
  InternalDumpToFile(FileName, fTokens);
end;

procedure TTProCompiledTemplate.Error(const aMessage: String);
begin
  raise ETProRenderException.Create(aMessage)at ReturnAddress;
end;

procedure TTProCompiledTemplate.ForEachToken(const TokenProc: TTokenWalkProc);
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
  lIdx: Int64;
  lBuff: TStringBuilder;
  lVariable: TVarDataSource;
  lWrapped: ITProWrappedList;
  lJumpTo: Integer;
  lVarName: string;
  lVarValue: TValue;
  lJObj: TJDOJsonObject;
  lVarMember: string;
  lBaseVarName: string;
  lFullPath: string;
  lForLoopItem: TLoopStackItem;
  lJValue: TJsonDataValueHelper;
  lMustBeEncoded: Boolean;
  lSavedIdx: Int64;
  lCurrentLevel: Integer; // 0 = page level, 1+ = layout levels
  lBlockStack: TStack<TBlockReturnInfo>;
  lBlockReturnInfo: TBlockReturnInfo;
  lCurrentBlockName: string;
  lObj: TValue;
  lCount: Integer;
  lParentBlockAddr: Int64;
  // Variables moved from inline declarations for Delphi 10 Seattle compatibility
  lIsFieldIteration: Boolean;
  lDataSet: TDataSet;
  lVarPair: TPair<string, TVarDataSource>;
  lSavedVars: TIncludeSavedVars;
  lVarNames: TArray<string>;
  lVarNameItem: string;
  lVarDataSource: TVarDataSource;
  lSavedVar: TIncludeSavedVar;
  lPair: TPair<string, TIncludeSavedVar>;
  lParentBlockName: string;
  lInheritedReturn: TBlockReturnInfo;
  // Variables for dynamic include
  lDynIncludeFileName: String;
  lDynBasePath: String;
  lDynFullPath: String;
  lDynIncludeSource: String;
  lDynIncludeCompiler: TTProCompiler;
  lDynIncludeTemplate: ITProCompiledTemplate;
begin
  lCurrentLevel := 0;
  lBlockStack := TStack<TBlockReturnInfo>.Create;
  try
  lBuff := TStringBuilder.Create;
  try
    lIdx := 0;
    while fTokens[lIdx].TokenType <> ttEOF do
    begin
      //Writeln(fTokens[lIdx].ToString);
      case fTokens[lIdx].TokenType of
        ttContent:
          begin
            lBuff.Append(fTokens[lIdx].Value1);
          end;
        ttFor:
          begin
            lForLoopItem := PeekLoop;
            lIsFieldIteration := fTokens[lIdx].Ref2 = 1;
            if LoopStackIsEmpty or (lForLoopItem.LoopExpression <> fTokens[lIdx].Value1) then
            begin // push a new loop stack item
              SplitVariableName(fTokens[lIdx].Value1, lVarName, lVarMember);
              if WalkThroughLoopStack(lVarName, lBaseVarName, lFullPath) then
              begin
                if not lVarMember.IsEmpty then
                  lFullPath := lFullPath + '.' + lVarMember;
                PushLoop(TLoopStackItem.Create(lBaseVarName, fTokens[lIdx].Value1, lFullPath, fTokens[lIdx].Value2, lIsFieldIteration));
              end
              else
              begin
                PushLoop(TLoopStackItem.Create(lVarName, fTokens[lIdx].Value1, lVarMember, fTokens[lIdx].Value2, lIsFieldIteration));
              end;
            end;
            lForLoopItem := PeekLoop;

            // Now, work with the stack head
            if GetVariables.TryGetValue(PeekLoop.DataSourceName, lVariable) then
            begin
              if lForLoopItem.FullPath.IsEmpty and (not lForLoopItem.IsFieldIteration) then
              begin
                if not(viIterable in lVariable.VarOption) then
                begin
                  Error(Format('Cannot iterate over a not iterable object [%s]', [fTokens[lIdx].Value1]));
                end;
              end;

              if viDataSet in lVariable.VarOption then
              begin
                // Check if this is a field iteration (dataset.fields)
                if lForLoopItem.IsFieldIteration then
                begin
                  lDataSet := TDataSet(lVariable.VarValue.AsObject);
                  if lForLoopItem.IteratorPosition = -1 then
                  begin
                    lForLoopItem.FieldsCount := lDataSet.Fields.Count;
                    lForLoopItem.TotalCount := lDataSet.Fields.Count;
                  end;
                  lForLoopItem.IncrementIteratorPosition;
                  if lForLoopItem.IteratorPosition >= lForLoopItem.FieldsCount then
                  begin
                    lForLoopItem.EOF := True;
                    lIdx := fTokens[lIdx].Ref1; // skip to endfor
                    Continue;
                  end;
                end
                else
                begin
                  // Regular record iteration
                  if lForLoopItem.IteratorPosition = -1 then
                  begin
                    TDataSet(lVariable.VarValue.AsObject).First;
                    lForLoopItem.TotalCount := TDataSet(lVariable.VarValue.AsObject).RecordCount;
                  end
                  else
                  begin
                    TDataSet(lVariable.VarValue.AsObject).Next;
                  end;
                  lForLoopItem.IncrementIteratorPosition;
                  if TDataSet(lVariable.VarValue.AsObject).Eof then
                  begin
                    lForLoopItem.EOF := True;
                    lIdx := fTokens[lIdx].Ref1; // skip to endfor
                    Continue;
                  end;
                end;
              end
              else if [viObject, viListOfObject] * lVariable.VarOption <> [] then
              begin
                {TODO -oDanieleT -cGeneral : We need only .Count here. Could we use something lighter than WrapAsList?}
                lObj := GetTValueFromPath(lVariable.VarValue.AsObject, lForLoopItem.FullPath);
                lWrapped := WrapAsList(lObj.AsObject);
                lCount := lWrapped.Count;
                if lForLoopItem.IteratorPosition = -1 then
                  lForLoopItem.TotalCount := lCount;
                if (lCount = 0) or (lForLoopItem.IteratorPosition = lCount - 1) then
                begin
                  lForLoopItem.EOF := True;
                  lIdx := fTokens[lIdx].Ref1; // skip to endfor
                  Continue;
                end
                else
                begin
                  lForLoopItem.IncrementIteratorPosition;
                end;
              end
              else if viJSONObject in lVariable.VarOption then
              begin
                lJObj := TJDOJsonObject(lVariable.VarValue.AsObject);
                lForLoopItem := PeekLoop;
                lJValue := lJObj.Path[lForLoopItem.FullPath];

                case lJValue.Typ of
                  jdtNone:
                    begin
                      lForLoopItem.EOF := True;
                      lIdx := fTokens[lIdx].Ref1; // skip to endfor
                      Continue;
                    end;

                  jdtArray:
                    begin
                      if lForLoopItem.IteratorPosition = -1 then
                        lForLoopItem.TotalCount := lJObj.Path[lForLoopItem.FullPath].ArrayValue.Count;
                      if lForLoopItem.IteratorPosition = lJObj.Path[lForLoopItem.FullPath].ArrayValue.Count - 1 then
                      begin
                        lForLoopItem.EOF := True;
                        lIdx := fTokens[lIdx].Ref1; // skip to endfor
                        Continue;
                      end
                      else
                      begin
                        lForLoopItem.IncrementIteratorPosition;
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
              Error(Format('Unknown variable in for..in statement [%s]', [fTokens[lIdx].Value1]));
            end;
          end;

        ttEndFor:
          begin
            lForLoopItem := PeekLoop;
            if lForLoopItem = nil then
            begin
              raise ETProRenderException.Create('Inconsistent "endfor"');
            end;
            if lForLoopItem.EOF then
            begin
              PopLoop;
            end
            else
            begin
              lIdx := fTokens[lIdx].Ref1; // goto loop
              Continue;
            end;
          end;
        ttIfThen:
          begin
            lSavedIdx := lIdx;
            if EvaluateIfExpressionAt(lIdx) then
            begin
              // do nothing
            end
            else
            begin
              lIdx := lSavedIdx;
              if fTokens[lIdx].Ref1 > -1 then { there is an else }
              begin
                lJumpTo := fTokens[lIdx].Ref1 + 1;
                // jump to the statement "after" ttElse (if it is ttLineBreak, jump it)
                if fTokens[lJumpTo].TokenType <> ttLineBreak then
                  lIdx := lJumpTo
                else
                  lIdx := lJumpTo + 1;
                Continue;
              end;
              lIdx := fTokens[lIdx].Ref2; // jump to "endif"
              Continue;
            end;
          end;
        ttElse:
          begin
            // always jump to ttEndIf which it reference is at ttElse.Ref2
            lIdx := fTokens[lIdx].Ref2;
            Continue;
          end;
        ttEndIf, ttStartTag, ttEndTag:
          begin
          end;
        ttInclude:
          begin
            // Dynamic include - evaluate filename and compile/execute at runtime
            // Get filename from expression
            lDynIncludeFileName := EvaluateExpression(fTokens[lIdx].Value1).AsString;

            // Build full path
            lDynBasePath := fTokens[lIdx].Value2;
            if TDirectory.Exists(lDynBasePath) then
              lDynFullPath := TPath.GetFullPath(TPath.Combine(lDynBasePath, lDynIncludeFileName))
            else
              lDynFullPath := TPath.GetFullPath(TPath.Combine(TPath.GetDirectoryName(lDynBasePath), lDynIncludeFileName));

            // Check cache first
            if not fDynamicIncludeCache.TryGetValue(lDynFullPath, lDynIncludeTemplate) then
            begin
              // Load template source
              try
                lDynIncludeSource := TFile.ReadAllText(lDynFullPath, fEncoding);
              except
                on E: Exception do
                  Error('Cannot read dynamic include "' + lDynIncludeFileName + '": ' + E.Message);
              end;

              // Compile the included template
              lDynIncludeCompiler := TTProCompiler.Create(fEncoding);
              try
                lDynIncludeTemplate := lDynIncludeCompiler.Compile(lDynIncludeSource, lDynFullPath);
              finally
                lDynIncludeCompiler.Free;
              end;

              // Store in cache
              fDynamicIncludeCache.Add(lDynFullPath, lDynIncludeTemplate);
            end;

            // Copy all variables to the included template
            if fVariables <> nil then
            begin
              for lVarPair in fVariables do
                lDynIncludeTemplate.SetData(lVarPair.Key, lVarPair.Value.VarValue);
            end;

            // Execute and append output
            lBuff.Append(lDynIncludeTemplate.Render);
          end;
        ttBoolExpression:
          begin
            Error('Token ttBoolExpression cannot be at first RENDER level, should be handled by ttIfThen TOKEN');
          end;
        ttValue, ttLiteralString:
          begin
            lVarValue := EvaluateValue(lIdx, lMustBeEncoded { must be encoded } );
            if lMustBeEncoded { lRef2 = -1 // encoded } then
              lBuff.Append(HTMLEncode(lVarValue.ToString))
            else
              lBuff.Append(lVarValue.ToString);
            if lVarValue.IsObjectInstance then
            begin
              lVarValue.AsObject.Free;
            end;
          end;
        ttExpression:
          begin
            lVarValue := EvaluateExpression(fTokens[lIdx].Value1);
            lBuff.Append(lVarValue.ToString);
          end;
        ttSet:
          ProcessSetToken(lIdx);
        ttIncludeStart:
          begin
            // Save current values of variables that will be mapped
            lSavedVars := TIncludeSavedVars.Create;
            lVarNames := fTokens[lIdx].Value1.Split([',']);
            for lVarNameItem in lVarNames do
            begin
              if GetVariables.TryGetValue(lVarNameItem, lVarDataSource) and (lVarDataSource <> nil) then
              begin
                lSavedVar.Existed := True;
                lSavedVar.Value := lVarDataSource.VarValue;
              end
              else
              begin
                lSavedVar.Existed := False;
                lSavedVar.Value := TValue.Empty;
              end;
              lSavedVars.Add(lVarNameItem, lSavedVar);
            end;
            fIncludeSavedVarsStack.Add(lSavedVars);
          end;
        ttIncludeEnd:
          begin
            // Restore saved variables
            if fIncludeSavedVarsStack.Count > 0 then
            begin
              lSavedVars := fIncludeSavedVarsStack[fIncludeSavedVarsStack.Count - 1];
              for lPair in lSavedVars do
              begin
                if not lPair.Value.Existed then
                  GetVariables.Remove(lPair.Key)
                else
                  SetData(lPair.Key, lPair.Value.Value);
              end;
              fIncludeSavedVarsStack.Delete(fIncludeSavedVarsStack.Count - 1);
            end;
          end;
        ttLineBreak:
          begin
            lBuff.Append(GetLineEndingString);
          end;
        ttSystemVersion:
          begin
            if fTokens[lIdx].Value1 <> TEMPLATEPRO_VERSION then
            begin
              Error('Compiled template has been compiled with a different version. Expected ' + TEMPLATEPRO_VERSION + ' got ' +
                fTokens[lIdx].Value1);
            end;
          end;
        ttContinue:
          begin
            lIdx := fTokens[lIdx].Ref1;
            Continue;
          end;
        ttExit:
          begin
            // do nothing
          end;
        ttInfo:
          begin
            if fTokens[lIdx].Value1 = STR_BEGIN_OF_LAYOUT then
              Inc(lCurrentLevel)
            else if fTokens[lIdx].Value1 = STR_END_OF_LAYOUT then
            begin
              Dec(lCurrentLevel);
              // After ANY end_of_layout, skip to EOF
              // Intermediate templates (between base and page) should only be accessed via {{inherited}}
              // They should NOT be rendered linearly
              lIdx := fTokens.Count - 1;
              Continue;
            end;
          end;
        ttBlock:
          begin
            lCurrentBlockName := fTokens[lIdx].Value1;
            if lCurrentLevel > 0 then  // We're in a layout
            begin
              if fTokens[lIdx].Ref1 > -1 then
              begin
                // Block has been overridden, jump to the override
                // Parse parent block address from DESTINATION block's Value2 for {{inherited}}
                lParentBlockAddr := StrToInt64Def(fTokens[fTokens[lIdx].Ref1].Value2, -1);
                lBlockReturnInfo := TBlockReturnInfo.Create(
                  fTokens[lIdx].Ref2 + 1,  // Return address (after its endblock)
                  lCurrentBlockName,
                  lParentBlockAddr
                );
                lBlockStack.Push(lBlockReturnInfo);
                lIdx := fTokens[lIdx].Ref1;  // Jump to override
                Continue;
              end;
              // Block not overridden, render default content
            end;
            // At page level or not overridden, just continue rendering
          end;
        ttEndBlock:
          begin
            if (lBlockStack.Count > 0) and SameText(lBlockStack.Peek.BlockName, lCurrentBlockName) then
            begin
              // Return from override block or inherited call
              lBlockReturnInfo := lBlockStack.Pop;
              lIdx := lBlockReturnInfo.ReturnAddress;
              // Only clear block name if stack is empty (returning to layout)
              // Otherwise we're returning from an inherited call, still in a block
              if lBlockStack.Count = 0 then
                lCurrentBlockName := '';
              Continue;
            end;
            // In layout with no override, or nested block, just continue
            lCurrentBlockName := '';
          end;
        ttInherited:
          begin
            // Render the parent block content
            if lBlockStack.Count > 0 then
            begin
              lBlockReturnInfo := lBlockStack.Peek;
              lParentBlockAddr := lBlockReturnInfo.ParentBlockAddress;
              if lParentBlockAddr >= 0 then
              begin
                // Push return context for after inherited
                // Use the parent block's name so endblock matching works
                lParentBlockName := fTokens[lParentBlockAddr].Value1;
                lInheritedReturn := TBlockReturnInfo.Create(
                  lIdx + 1,  // Return to next token
                  lParentBlockName,
                  StrToInt64Def(fTokens[lParentBlockAddr].Value2, -1)  // Grandparent block
                );
                lBlockStack.Push(lInheritedReturn);
                // Set current block name so endblock matching works
                lCurrentBlockName := lParentBlockName;
                // Jump to parent block content (skip the ttBlock token itself)
                lIdx := lParentBlockAddr + 1;
                Continue;
              end;
            end;
            // No parent block, {{inherited}} produces no output
          end;
        ttMacro:
          begin
            // Register macro and skip to endmacro
            RegisterMacro(lIdx);
            lIdx := fTokens[lIdx].Ref2; // Jump to ttEndMacro
            Continue;
          end;
        ttEndMacro:
          begin
            // Do nothing, macro already registered
          end;
        ttCallMacro:
          begin
            // Execute macro
            lBuff.Append(ExecuteMacro(lIdx));
            // Skip the call parameters
            lIdx := lIdx + fTokens[lIdx].Ref1;
          end;
      else
        begin
          Error('Invalid token at index #' + lIdx.ToString + ': ' + fTokens[lIdx].TokenTypeAsString);
        end;
      end;
      Inc(lIdx);
    end;
    Result := lBuff.ToString;
  finally
    lBuff.Free;
  end;
  finally
    lBlockStack.Free;
  end;
end;

function TTProCompiledTemplate.GetVarAsString(const Name: string): string;
var
  lValue: TValue;
  lPValue: PValue;
  lIsNull: Boolean;
begin
  lValue := GetVarAsTValue(Name);
  lPValue := @lValue;
  Result := GetTValueVarAsString(lPValue, lIsNull, Name);
end;

function TTProCompiledTemplate.GetVarAsTValue(const aName: string): TValue;
var
  lVariable: TVarDataSource;
  lHasMember: Boolean;
  lJPath: string;
  lDataSource: string;
  lIsAnIterator: Boolean;
  lJObj: TJDOJsonObject;
  lVarName: string;
  lVarMembers: string;
  lCurrentIterator: TLoopStackItem;
  lPJSONDataValue: TJsonDataValueHelper;
  lHandled: Boolean;
  lFullPath: string;
  lValue: TValue;
  lTmpList: ITProWrappedList;
  lDataSet: TDataSet;
  lField: TField;
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
      if lIsAnIterator then
      begin
        // Check if this is a field iteration
        if lCurrentIterator.IsFieldIteration then
        begin
          lDataSet := TDataSet(lVariable.VarValue.AsObject);
          lField := lDataSet.Fields[lCurrentIterator.IteratorPosition];
          if lHasMember and lVarMembers.StartsWith('@@') then
          begin
            Result := GetPseudoVariable(lCurrentIterator.IteratorPosition, lVarMembers);
          end
          else if lVarMembers.IsEmpty then
          begin
            // Return field value
            Result := lField.AsString;
          end
          else
          begin
            // Return field property
            Result := GetFieldProperty(lField, lVarMembers);
          end;
        end
        else if lHasMember and lVarMembers.StartsWith('@@') then
        begin
          lCurrentIterator.IteratorPosition := TDataSet(lVariable.VarValue.AsObject).RecNo - 1;
          Result := GetPseudoVariable(lCurrentIterator.IteratorPosition, lVarMembers);
        end
        else
        begin
          if lVarMembers.IsEmpty then
          begin
            Error('Empty field name while reading from iterator "%s"', [lVarName]);
          end;
          Result := GetDataSetFieldAsTValue(TDataSet(lVariable.VarValue.AsObject), lVarMembers);
        end;
      end
      else
      begin
        { not an interator }
        if lHasMember then
        begin
          Result := GetDataSetFieldAsTValue(TDataSet(lVariable.VarValue.AsObject), lVarMembers);
        end
        else
        begin
          Result := lVariable.VarValue.AsObject;
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
              jdtArray:
                begin
                  Result := lPJSONDataValue.ArrayValue.ToJSON();
                end;
              jdtObject:
                begin
                  Result := lPJSONDataValue.ObjectValue.ToJSON();
                end;
              jdtFloat:
                begin
                  Result := lPJSONDataValue.FloatValue;
                end;
              jdtInt:
                begin
                  Result := lPJSONDataValue.IntValue;
                end;
              jdtLong:
                begin
                  Result := lPJSONDataValue.LongValue;
                end;
              jdtULong:
                begin
                  Result := lPJSONDataValue.ULongValue;
                end;
              jdtBool:
                begin
                  Result := lPJSONDataValue.BoolValue;
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
        begin
          lPJSONDataValue := lJObj.Path[lJPath];
          if lPJSONDataValue.Typ = jdtString then
          begin
            Result := lJObj.Path[lJPath].Value
          end
          else if lPJSONDataValue.Typ = jdtInt then
          begin
            Result := lPJSONDataValue.IntValue;
          end
          else if lPJSONDataValue.Typ = jdtLong then
          begin
            Result := lPJSONDataValue.LongValue;
          end
          else if lPJSONDataValue.Typ = jdtULong then
          begin
            Result := lPJSONDataValue.ULongValue;
          end
          else if lPJSONDataValue.Typ = jdtFloat then
          begin
            Result := lPJSONDataValue.FloatValue;
          end
          else if lPJSONDataValue.Typ = jdtArray then
          begin
            Result := lPJSONDataValue.ArrayValue;
          end
          else if lPJSONDataValue.Typ = jdtObject then
          begin
            Result := lPJSONDataValue.ObjectValue;
          end
          else if lPJSONDataValue.Typ = jdtNone then
          begin
            Result := '';
          end
          else
            raise ETProRenderException.Create('Unknown type for path ' + lJPath);
        end;
      end;
    end
    else if [viListOfObject, viObject] * lVariable.VarOption <> [] then
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
          begin
            if lCurrentIterator.FullPath.IsEmpty then
            begin
              Result := TTProRTTIUtils.GetProperty(WrapAsList(lVariable.VarValue.AsObject)
                .GetItem(lCurrentIterator.IteratorPosition), lVarMembers)
            end
            else
            begin
              lFullPath := lCurrentIterator.FullPath;
              lValue := GetTValueFromPath(lVariable.VarValue.AsObject, lFullPath);
              lTmpList := WrapAsList(lValue.AsObject);
              if Assigned(lTmpList)then
                Result := TTProRTTIUtils.GetProperty(lTmpList.GetItem(lCurrentIterator.IteratorPosition), lVarMembers)
              else
                Result := TTProRTTIUtils.GetProperty(lValue.AsObject, lVarMembers)
            end;
          end
          else
          begin
            if lCurrentIterator.FullPath.IsEmpty then
            begin
              Result := WrapAsList(lVariable.VarValue.AsObject).GetItem(lCurrentIterator.IteratorPosition);
            end
            else
            begin
              lValue := GetTValueFromPath(lVariable.VarValue.AsObject, lCurrentIterator.FullPath);
              lTmpList := WrapAsList(lValue.AsObject);
              if Assigned(lTmpList)then
                Result := TTProRTTIUtils.GetProperty(lTmpList.GetItem(lCurrentIterator.IteratorPosition), lVarMembers)
              else
                Result := TTProRTTIUtils.GetProperty(lValue.AsObject, lVarMembers)
            end;
          end;
        end
        else
        begin
      if lHasMember then
      begin
        Result := GetTValueFromPath(lVariable.VarValue.AsObject, lVarMembers);
      end
      else
      begin
        Result := lVariable.VarValue;
      end;
        end;
      end;
    end
//    else if viObject in lVariable.VarOption then
//    begin
//      if lHasMember then
//      begin
//        Result := GetTValueFromPath(lVariable.VarValue.AsObject, lVarMembers);
//      end
//      else
//      begin
//        Result := lVariable.VarValue;
//      end;
//    end
    else if viSimpleType in lVariable.VarOption then
    begin
      if lVariable.VarValue.IsEmpty then
      begin
        Result := TValue.Empty;
      end
      else
      begin
        Result := lVariable.VarValue;
      end;
    end;
  end
  else
  begin
    DoOnGetValue(lDataSource, lVarMembers, Result, lHandled);
    if not lHandled then
    begin
      Result := TValue.Empty;
    end;
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

procedure TTProCompiledTemplate.InitTemplateAnonFunctions;
begin
  if fTemplateAnonFunctions = nil then
  begin
    fTemplateAnonFunctions := TDictionary<string, TTProTemplateAnonFunction>.Create;
  end;
end;

class procedure TTProCompiledTemplate.InternalDumpToFile(const FileName: String; const aTokens: TList<TToken>);
var
  lToken: TToken;
  lSW: TStreamWriter;
  lIdx: Int64;
begin
  lSW := TStreamWriter.Create(FileName);
  try
    lIdx := 0;
    for lToken in aTokens do
    begin
      lSW.WriteLine('%5d %s', [lIdx, lToken.ToString]);
      Inc(lIdx);
    end;
    lSW.Close;
  finally
    lSW.Free;
  end;
end;

function TTProCompiledTemplate.IsAnIterator(const VarName: String; out DataSourceName: String; out CurrentIterator: TLoopStackItem)
  : Boolean;
var
  I: Integer;
begin
  Result := False;
  if not LoopStackIsEmpty then { search datasource using current iterators stack }
  begin
    for I := fLoopsStack.Count - 1 downto 0 do
    begin
      if SameText(fLoopsStack[I].IteratorName, VarName) then
      begin
        Result := True;
        DataSourceName := fLoopsStack[I].DataSourceName;
        CurrentIterator := fLoopsStack[I];
        Break;
      end;
    end;
  end;
end;

function TTProCompiledTemplate.IsNullableType(const Value: PValue): Boolean;
begin
  Result := (Value.TypeInfo.Kind = tkRecord) and String(Value.TypeInfo.Name).StartsWith('nullable', True);
end;

function TTProCompiledTemplate.IsTruthy(const Value: TValue): Boolean;
var
  lStrValue: String;
  lWrappedList: ITProWrappedList;
  lIsNull: Boolean;
  lIsFalsy: Boolean;
begin
  lIsNull := False;
  if Value.IsEmpty then
  begin
    Exit(False);
  end;
  lStrValue := Value.ToString;
  if Value.IsObjectInstance then
  begin
    if Value.AsObject = nil then
    begin
      lStrValue := '';
    end
    else if Value.AsObject is TDataSet then
    begin
      lStrValue := TDataSet(Value.AsObject).RecordCount.ToString;
    end
    else if Value.AsObject is TJsonArray then
    begin
      lStrValue := TJsonArray(Value.AsObject).Count.ToString;
    end
    else if Value.AsObject is TJsonObject then
    begin
      lStrValue := TJsonObject(Value.AsObject).Count.ToString;
    end
    else
    begin
      lWrappedList := TTProDuckTypedList.Wrap(Value.AsObject);
      if lWrappedList = nil then
      begin
        lStrValue := 'true'; //it is an object <> nil, so evaluates to true
      end
      else
      begin
        lStrValue := lWrappedList.Count.ToString;
      end;
    end;
  end
  else if Value.IsType<Boolean> then
  begin
    lStrValue := Value.AsType<Boolean>.ToString.ToLower;
  end
  else if IsNullableType(@Value) then
  begin
    lStrValue := GetTValueWithNullableTypeAsString(@Value, lIsNull, '<if_comparison>');
  end;
  lIsFalsy := lIsNull or SameText(lStrValue, 'false') or SameText(lStrValue, '0') or SameText(lStrValue, '');
  Result := not lIsFalsy;
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

procedure TTProCompiledTemplate.Error(const aMessage: String; const Params: array of const);
begin
  Error(Format(aMessage, Params));
end;

function TTProCompiledTemplate.EvaluateIfExpressionAt(var Idx: Int64): Boolean;
var
  lMustBeEncoded: Boolean;
  lExprResult: TValue;
begin
  Inc(Idx);
  if fTokens[Idx].TokenType <> ttBoolExpression then
  begin
    Error('Expected ttBoolExpression after ttIfThen');
  end;

  // Check if this is an expression-based condition (Ref2 = 1)
  if fTokens[Idx].Ref2 = 1 then
  begin
    // Evaluate using ExpressionEvaluator
    lExprResult := EvaluateExpression(fTokens[Idx].Value1);
    Result := IsTruthy(lExprResult);
  end
  else
  begin
    // Original variable-based evaluation
    Result := IsTruthy(EvaluateValue(Idx, lMustBeEncoded));
  end;
end;

function TTProCompiledTemplate.EvaluateValue(var Idx: Int64; out MustBeEncoded: Boolean): TValue;
var
  lCurrTokenType: TTokenType;
  lVarName: string;
  lFilterName: string;
  lFilterParCount: Int64;
  lFilterCount: Int64;
  lFilterParameters: TArray<TFilterParameter>;
  I, J: Integer;
  lNegated: Boolean;
  lCurrentValue: TValue;
  lDataSetFieldMeta: string;
begin
  // Ref1 contains the number of filters (0 if there isn't any filter)
  // Ref2 is -1 if the variable must be HTMLEncoded, while contains 1 is the value must not be HTMLEncoded
  MustBeEncoded := fTokens[Idx].Ref2 = -1;
  lCurrTokenType := fTokens[Idx].TokenType;
  lVarName := fTokens[Idx].Value1;
  lFilterCount := fTokens[Idx].Ref1;
  lNegated := lVarName.StartsWith('!');
  if lNegated then
  begin
    lVarName := lVarName.Substring(1);
  end;

  // Check for dataset field metadata syntax (Value2 not empty)
  lDataSetFieldMeta := fTokens[Idx].Value2;
  if (lCurrTokenType = ttValue) and (not lDataSetFieldMeta.IsEmpty) then
  begin
    Result := EvaluateDataSetFieldMeta(lVarName, lDataSetFieldMeta);
    if lNegated then
      Result := not Result.AsBoolean;
    Exit;
  end;

  if lFilterCount > 0 { has filters } then
  begin
    // Get initial value
    case lCurrTokenType of
      ttValue:
        lCurrentValue := GetVarAsTValue(lVarName);
      ttBoolExpression:
        lCurrentValue := GetVarAsTValue(lVarName);
      ttLiteralString:
        lCurrentValue := lVarName;
    else
      Error('Invalid token in EvaluateValue');
    end;

    // Apply each filter in sequence
    for J := 0 to lFilterCount - 1 do
    begin
      Inc(Idx);
      Assert(fTokens[Idx].TokenType = ttFilterName);
      lFilterName := fTokens[Idx].Value1;
      lFilterParCount := fTokens[Idx].Ref1; // parameter count for this filter
      SetLength(lFilterParameters, lFilterParCount);
      for I := 0 to lFilterParCount - 1 do
      begin
        Inc(Idx);
        Assert(fTokens[Idx].TokenType = ttFilterParameter);
        lFilterParameters[I].ParType := TFilterParameterType(fTokens[Idx].Ref2);

        case lFilterParameters[I].ParType of
          fptInteger:
            lFilterParameters[I].ParIntValue := fTokens[Idx].Value1.ToInteger;
          fptString, fptVariable:
            lFilterParameters[I].ParStrText := fTokens[Idx].Value1;
        end;
      end;

      try
        lCurrentValue := ExecuteFilter(lFilterName, lFilterParameters, lCurrentValue, lVarName);
      except
        on E: Exception do
        begin
          Error('Error while evaluating filter [%s] on variable [%s]- Inner Exception: [%s][%s]', [lFilterName, lVarName, E.ClassName, E.Message]);
        end;
      end;
    end;

    // For bool expressions, convert final result to boolean
    if lCurrTokenType = ttBoolExpression then
      Result := IsTruthy(lCurrentValue)
    else
      Result := lCurrentValue;
  end
  else
  begin
    case lCurrTokenType of
      ttValue:
        Result := GetVarAsString(lVarName);
      ttBoolExpression:
        Result := IsTruthy(GetVarAsTValue(lVarName));
      ttLiteralString:
        Result := lVarName;
    else
      Error('Invalid token in EvaluateValue');
    end;
  end;
  if lNegated then
  begin
    Result := not Result.AsBoolean;
  end;
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
  lObj: TObject;
begin
  if Value.IsEmpty then
  begin
    GetVariables.AddOrSetValue(Name, nil);
    Exit;
  end;

  case Value.Kind of
    tkClass:
      begin
        lObj := Value.AsObject;
        if lObj is TDataSet then
        begin
          GetVariables.AddOrSetValue(Name, TVarDataSource.Create(lObj, [viDataSet, viIterable]));
        end
        else if Value.TypeInfo = TypeInfo(TJDOJsonObject) then
        begin
          GetVariables.AddOrSetValue(Name, TVarDataSource.Create(TJDOJsonObject(lObj), [viJSONObject]));
        end
        else if Value.TypeInfo = TypeInfo(TJDOJsonArray) then
        begin
          raise ETProRenderException.Create
            ('JSONArray cannot be used directly [HINT] Define a JSONObject variable with a JSONArray property');
        end
        else if TTProDuckTypedList.CanBeWrappedAsList(lObj, lWrappedList) then
        begin
          GetVariables.AddOrSetValue(Name, TVarDataSource.Create(TTProDuckTypedList(lObj), [viListOfObject, viIterable]));
        end
        else
        begin
          GetVariables.AddOrSetValue(Name, TVarDataSource.Create(lObj, [viObject]));
        end;
      end;
    tkInterface:
      GetVariables.AddOrSetValue(Name, TVarDataSource.Create(Value.AsInterface as TObject, [viObject]));
    tkInteger, tkString, tkUString, tkFloat, tkEnumeration:
      GetVariables.AddOrSetValue(Name, TVarDataSource.Create(Value, [viSimpleType]));
  else
    raise ETProException.Create('Invalid type for variable "' + Name + '": ' + TRttiEnumerationType.GetName<TTypeKind>(Value.Kind));
  end;

end;

procedure TTProCompiledTemplate.SetFormatSettings(const Value: PTProFormatSettings);
begin
  fLocaleFormatSettings := Value^;
end;

procedure TTProCompiledTemplate.SetOnGetValue(const Value: TTProCompiledTemplateGetValueEvent);
begin
  fOnGetValue := Value;
end;

procedure TTProCompiledTemplate.SplitVariableName(const VariableWithMember: String; out VarName, VarMembers: String);
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

function TTProCompiledTemplate.WalkThroughLoopStack(const VarName: String; out BaseVarName, FullPath: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := fLoopsStack.Count - 1 downto 0 do
  begin
    if VarName = fLoopsStack[I].IteratorName then
    begin
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

/// ///////////////////
/// UTILS

class function TTProRTTIUtils.GetProperty(AObject: TObject; const APropertyName: string): TValue;
var
  Prop: TRttiProperty;
  ARttiType: TRttiType;
begin
  ARttiType := GlContext.GetType(AObject.ClassType);
  if not Assigned(ARttiType) then
    raise Exception.CreateFmt('Unknown type [%s]', [ARttiType.ToString]);
  Prop := ARttiType.GetProperty(APropertyName);
  if not Assigned(Prop) then
    raise Exception.CreateFmt('Unknown property [%s.%s]', [ARttiType.ToString, APropertyName]);
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

  FIsWrappedList := HookListMethods(FObjType);
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

procedure TTProDuckTypedList.GetItemAsTValue(const AIndex: Integer; out aValue: TValue);
begin
  aValue := FGetItemMethod.Invoke(FObjectAsDuck, [AIndex]);
end;

function TTProDuckTypedList.HookListMethods(const aObjType: TRttiType): Boolean;
begin
  Result := True;

  FAddMethod := aObjType.GetMethod('Add');
  if FAddMethod = nil then
    Exit(False);

  FClearMethod := aObjType.GetMethod('Clear');
  if FClearMethod = nil then
    Exit(False);

  if aObjType.GetIndexedProperty('Items') <> nil then
  begin
    FGetItemMethod := aObjType.GetIndexedProperty('Items').ReadMethod;
    if FGetItemMethod = nil then
    begin
      FGetItemMethod := FObjType.GetMethod('GetElement');
      if FGetItemMethod = nil then
      begin
        Exit(False);
      end;
    end;
  end
  else
  begin
    Exit(False);
  end;

  FCountProperty := FObjType.GetProperty('Count');
  if FCountProperty = nil then
  begin
    FGetCountMethod := FObjType.GetMethod('Count');
    if FGetCountMethod = nil then
    begin
      Exit(False);
    end;
  end;
end;

function TTProDuckTypedList.IsWrappedList: Boolean;
begin
  Result := FIsWrappedList;
end;

function TTProDuckTypedList.ItemIsObject(const AIndex: Integer; out aValue: TValue): Boolean;
begin
  GetItemAsTValue(AIndex, aValue);
  Result := aValue.IsObject;
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

{ TTProConfiguration }

class procedure TTProConfiguration.RegisterHandlers(const TemplateProCompiledTemplate: ITProCompiledTemplate);
begin
  if Assigned(fOnContextConfiguration) then
  begin
    fOnContextConfiguration(TemplateProCompiledTemplate);
  end;
end;

class function TBlockAddress.Create(BeginBlockAddress, EndBlockAddress: Int64; Level: Integer): TBlockAddress;
begin
  Result.BeginBlockAddress := BeginBlockAddress;
  Result.EndBlockAddress := EndBlockAddress;
  Result.Level := Level;
end;

class function TBlockReturnInfo.Create(ReturnAddr: Int64; const ABlockName: string; ParentAddr: Int64): TBlockReturnInfo;
begin
  Result.ReturnAddress := ReturnAddr;
  Result.BlockName := ABlockName;
  Result.ParentBlockAddress := ParentAddr;
end;

class function TMacroParameter.Create(const Name: String; const DefaultValue: String; HasDefault: Boolean): TMacroParameter;
begin
  Result.Name := Name;
  Result.DefaultValue := DefaultValue;
  Result.HasDefault := HasDefault;
end;

class function TMacroDefinition.Create(const Name: String; const Parameters: TArray<TMacroParameter>; BeginTokenIndex, EndTokenIndex: Int64): TMacroDefinition;
begin
  Result.Name := Name;
  Result.Parameters := Parameters;
  Result.BeginTokenIndex := BeginTokenIndex;
  Result.EndTokenIndex := EndTokenIndex;
end;

function HandleTemplateSectionStateMachine(const aTokenValue1: String; var aTemplateSectionType: TTProTemplateSectionType;
  out aErrorMessage: String): Boolean;
begin
  Result := True;
  if aTokenValue1 = STR_BEGIN_OF_LAYOUT then
  begin
    if aTemplateSectionType = stUnknown then
    begin
      aTemplateSectionType := stLayout;
    end
    else
    begin
      aErrorMessage := 'Unexpected ' + aTokenValue1;
      Result := False;
    end;
  end
  else if aTokenValue1 = STR_END_OF_LAYOUT then
  begin
    if aTemplateSectionType = stLayout then
      aTemplateSectionType := stPage
    else
    begin
      aErrorMessage := 'Unexpected ' + aTokenValue1;
      Result := False;
    end;
  end
  else
  begin
    aErrorMessage := 'Unknown ttInfo value: ' + aTokenValue1;
    Result := False;
  end;
end;

{ TTProVariablesEqualityComparer }

function TTProEqualityComparer.Equals(const Left, Right: String): Boolean;
begin
  Result := CompareText(Left, Right) = 0;
end;

function TTProEqualityComparer.GetHashCode(const Value: String): Integer;
begin
  // Result := BobJenkinsHash(Value[1], Length(Value) * SizeOf(Value[1]), 0);
  Result := Length(Value);
end;



function GetTValueFromPath(const aObject: TObject; FullPropertyPath: String): TValue;
var
  lObjAsList: ITProWrappedList;
  lIdx: Integer;
  lPropName: string;
  lTmpValue: TValue;
  function FetchUpTo(const aChar: Char): String;
  var
    lFirst: Integer;
  begin
    lFirst := FullPropertyPath.IndexOf(aChar);
    if lFirst = -1 then
    begin
      Result := FullPropertyPath;
    end
    else
    begin
      Result := FullPropertyPath.Substring(0, lFirst);
    end;
    FullPropertyPath := FullPropertyPath.Substring(Length(Result) + 1);
  end;
begin
  if FullPropertyPath = '.' then
  begin
    Exit(aObject);
  end;

  if FullPropertyPath.StartsWith('[') then //the main object must be a list!
  begin
    lObjAsList := WrapAsList(aObject);
    FullPropertyPath := FullPropertyPath.Remove(0,1);
    lIdx := FetchUpTo(']').ToInteger;
    Result := GetTValueFromPath(lObjAsList.GetItem(lIdx), FullPropertyPath);
  end
  else
  begin
    if FullPropertyPath.StartsWith('.') then
    begin
      FullPropertyPath := FullPropertyPath.Remove(0,1);
    end;
    if FullPropertyPath.StartsWith('[') then
    begin
      Result := GetTValueFromPath(aObject, FullPropertyPath);
    end
    else
    begin
      lPropName := FetchUpTo('.');
      if lPropName.IsEmpty then
      begin
        Result := aObject;
      end
      else
      begin
        lTmpValue := TTProRTTIUtils.GetProperty(aObject, lPropName);
        if (not FullPropertyPath.IsEmpty) then
        begin
          if not lTmpValue.IsObject then
            raise ETProException.Create('Invalid Path - cannot read property of a non object');
          Result := GetTValueFromPath(lTmpValue.AsObject, FullPropertyPath);
        end
        else
        begin
          Result := lTmpValue;
        end;
      end;
    end;
  end;
end;

procedure TTProCompiledTemplate.RegisterMacro(const TokenIndex: Int64);
var
  lMacroName: String;
  lParamCount: Integer;
  lParams: TArray<TMacroParameter>;
  lMacroDef: TMacroDefinition;
  I: Integer;
  lParamToken: TToken;
begin
  // Extract macro information from tokens
  lMacroName := fTokens[TokenIndex].Value1;
  lParamCount := fTokens[TokenIndex].Ref1;

  // Parse macro parameters
  SetLength(lParams, lParamCount);
  for I := 0 to lParamCount - 1 do
  begin
    lParamToken := fTokens[TokenIndex + 1 + I];
    if lParamToken.TokenType <> ttMacroParam then
    begin
      // For now, use ttFilterParameter as ttMacroParam
      if lParamToken.TokenType = ttFilterParameter then
      begin
        lParams[I].Name := lParamToken.Value1;
        // Check if it has a default value (string type means it's a default)
        if lParamToken.Ref2 = Ord(fptString) then
        begin
          lParams[I].DefaultValue := lParamToken.Value1;
          lParams[I].HasDefault := True;
        end
        else
        begin
          lParams[I].DefaultValue := '';
          lParams[I].HasDefault := False;
        end;
      end;
    end;
  end;

  // Create macro definition
  lMacroDef := TMacroDefinition.Create(
    lMacroName,
    lParams,
    TokenIndex + 1 + lParamCount, // Start after parameters
    fTokens[TokenIndex].Ref2 // End at ttEndMacro
  );

  // Register macro
  if fMacros.ContainsKey(lMacroName.ToLower) then
    fMacros.AddOrSetValue(lMacroName.ToLower, lMacroDef)
  else
    fMacros.Add(lMacroName.ToLower, lMacroDef);
end;

procedure TTProCompiledTemplate.ProcessSetToken(var Idx: Int64);
var
  lVarValue: TValue;
  lSetTargetVar: String;
  lSetSourceVar: String;
  lSetFilterCount: Integer;
  lSetFilterName: String;
  lSetFilterParCount: Integer;
  lSetFilterParams: TArray<TFilterParameter>;
  lSetJ, lSetI: Integer;
begin
  case fTokens[Idx].Ref2 of
    0: // Variable reference with optional filters
      begin
        lSetTargetVar := fTokens[Idx].Value1;
        lSetSourceVar := fTokens[Idx].Value2;
        // Get initial value from source variable (Value2)
        lVarValue := GetVarAsTValue(lSetSourceVar);
        // Apply filters if any (Ref1 = filter count)
        lSetFilterCount := fTokens[Idx].Ref1;
        for lSetJ := 0 to lSetFilterCount - 1 do
        begin
          Inc(Idx);
          Assert(fTokens[Idx].TokenType = ttFilterName);
          lSetFilterName := fTokens[Idx].Value1;
          lSetFilterParCount := fTokens[Idx].Ref1;
          SetLength(lSetFilterParams, lSetFilterParCount);
          for lSetI := 0 to lSetFilterParCount - 1 do
          begin
            Inc(Idx);
            Assert(fTokens[Idx].TokenType = ttFilterParameter);
            lSetFilterParams[lSetI].ParType := TFilterParameterType(fTokens[Idx].Ref2);
            case lSetFilterParams[lSetI].ParType of
              fptInteger:
                lSetFilterParams[lSetI].ParIntValue := fTokens[Idx].Value1.ToInteger;
              fptString, fptVariable:
                lSetFilterParams[lSetI].ParStrText := fTokens[Idx].Value1;
            end;
          end;
          lVarValue := ExecuteFilter(lSetFilterName, lSetFilterParams, lVarValue, lSetSourceVar);
        end;
        SetData(lSetTargetVar, lVarValue);
      end;
    1: // Expression
      begin
        lVarValue := EvaluateExpression(fTokens[Idx].Value2);
        SetData(fTokens[Idx].Value1, lVarValue);
      end;
    2: // String literal
      SetData(fTokens[Idx].Value1, fTokens[Idx].Value2);
    3: // Boolean true
      SetData(fTokens[Idx].Value1, True);
    4: // Boolean false
      SetData(fTokens[Idx].Value1, False);
    5: // Integer literal
      SetData(fTokens[Idx].Value1, StrToInt(fTokens[Idx].Value2));
    6: // Float literal
      SetData(fTokens[Idx].Value1, StrToFloat(fTokens[Idx].Value2, fLocaleFormatSettings));
  end;
end;

function TTProCompiledTemplate.ExecuteMacro(const CallTokenIndex: Int64): String;
var
  lMacroName: String;
  lMacroDef: TMacroDefinition;
  lCallParamCount: Integer;
  lCallParams: TArray<TValue>;
  I: Integer;
  lIdx: Int64;
  lBuff: TStringBuilder;
  lSavedVariables: TTProVariables;
  lParamToken: TToken;
  lMustBeEncoded: Boolean;
  lParamValue: TValue;
  lSavedIdx: Int64;
  lJumpTo: Integer;
begin
  // Get macro name and parameters from call
  lMacroName := fTokens[CallTokenIndex].Value1;
  lCallParamCount := fTokens[CallTokenIndex].Ref1;

  // Find macro definition
  if not fMacros.TryGetValue(lMacroName.ToLower, lMacroDef) then
  begin
    Error('Macro "' + lMacroName + '" not defined');
  end;

  // Evaluate call parameters
  SetLength(lCallParams, lCallParamCount);
  for I := 0 to lCallParamCount - 1 do
  begin
    lParamToken := fTokens[CallTokenIndex + 1 + I];
    case TFilterParameterType(lParamToken.Ref2) of
      fptString:
        lCallParams[I] := lParamToken.Value1;
      fptInteger:
        lCallParams[I] := StrToInt(lParamToken.Value1);
      fptFloat:
        lCallParams[I] := StrToFloat(lParamToken.Value1, fLocaleFormatSettings);
      fptVariable:
        begin
          // Handle boolean literals
          if SameText(lParamToken.Value1, 'true') then
            lCallParams[I] := True
          else if SameText(lParamToken.Value1, 'false') then
            lCallParams[I] := False
          else
            lCallParams[I] := GetVarAsTValue(lParamToken.Value1);
        end;
    end;
  end;

  // Save current variables and create new scope
  lSavedVariables := fVariables;
  try
    fVariables := TTProVariables.Create;
    try
      // Set macro parameters as variables in new scope
      for I := 0 to High(lMacroDef.Parameters) do
      begin
        if I < Length(lCallParams) then
        begin
          // Use provided parameter
          fVariables.Add(lMacroDef.Parameters[I].Name, TVarDataSource.Create(lCallParams[I], [viSimpleType]));
        end
        else if lMacroDef.Parameters[I].HasDefault then
        begin
          // Use default value
          fVariables.Add(lMacroDef.Parameters[I].Name, TVarDataSource.Create(lMacroDef.Parameters[I].DefaultValue, [viSimpleType]));
        end
        else
        begin
          Error('Missing required parameter "' + lMacroDef.Parameters[I].Name + '" for macro "' + lMacroName + '"');
        end;
      end;

      // Execute macro body
      lBuff := TStringBuilder.Create;
      try
        lIdx := lMacroDef.BeginTokenIndex;
        while (lIdx < lMacroDef.EndTokenIndex) and (lIdx < fTokens.Count) do
        begin
          case fTokens[lIdx].TokenType of
            ttContent:
              lBuff.Append(fTokens[lIdx].Value1);
            ttValue, ttLiteralString:
              begin
                lParamValue := EvaluateValue(lIdx, lMustBeEncoded);
                if lMustBeEncoded then
                  lBuff.Append(HTMLEncode(lParamValue.ToString))
                else
                  lBuff.Append(lParamValue.ToString);
              end;
            ttExpression:
              begin
                lParamValue := EvaluateExpression(fTokens[lIdx].Value1);
                lBuff.Append(lParamValue.ToString);
              end;
            ttSet:
              ProcessSetToken(lIdx);
            ttLineBreak:
              lBuff.Append(GetLineEndingString);
            ttCallMacro:
              begin
                // Nested macro call
                lBuff.Append(ExecuteMacro(lIdx));
                // Skip the call parameters
                lIdx := lIdx + fTokens[lIdx].Ref1;
              end;
            ttIfThen:
              begin
                lSavedIdx := lIdx;
                if EvaluateIfExpressionAt(lIdx) then
                begin
                  // condition is true, continue executing
                end
                else
                begin
                  lIdx := lSavedIdx;
                  if fTokens[lIdx].Ref1 > -1 then { there is an else }
                  begin
                    lJumpTo := fTokens[lIdx].Ref1 + 1;
                    // jump to the statement "after" ttElse (if it is ttLineBreak, jump it)
                    if fTokens[lJumpTo].TokenType <> ttLineBreak then
                      lIdx := lJumpTo
                    else
                      lIdx := lJumpTo + 1;
                    Continue;
                  end;
                  lIdx := fTokens[lIdx].Ref2; // jump to "endif"
                  Continue;
                end;
              end;
            ttElse:
              begin
                // always jump to ttEndIf which it reference is at ttElse.Ref2
                lIdx := fTokens[lIdx].Ref2;
                Continue;
              end;
            ttEndIf:
              begin
                // do nothing, just continue
              end;
          end;
          Inc(lIdx);
        end;
        Result := lBuff.ToString;
      finally
        lBuff.Free;
      end;
    finally
      fVariables.Free;
    end;
  finally
    fVariables := lSavedVariables;
  end;
end;

{ Expression Evaluator Integration }

function TTProCompiledTemplate.TValueToVariant(const Value: TValue): Variant;
begin
  if Value.IsEmpty then
    Result := Null
  else if Value.Kind = tkInteger then
    Result := Value.AsInteger
  else if Value.Kind = tkInt64 then
    Result := Value.AsInt64
  else if Value.Kind = tkFloat then
    Result := Value.AsExtended
  else if Value.Kind in [tkString, tkUString, tkLString, tkWString] then
    Result := Value.AsString
  else if Value.Kind = tkEnumeration then
  begin
    if Value.TypeInfo = TypeInfo(Boolean) then
      Result := Value.AsBoolean
    else
      Result := Value.AsOrdinal;
  end
  else if Value.Kind = tkVariant then
    Result := Value.AsVariant
  else
    Result := Value.ToString;
end;

function TTProCompiledTemplate.VariantToTValue(const Value: Variant): TValue;
begin
  case VarType(Value) and varTypeMask of
    varEmpty, varNull:
      Result := TValue.Empty;
    varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord:
      Result := TValue.From<Integer>(Value);
    varInt64, varUInt64:
      Result := TValue.From<Int64>(Value);
    varSingle, varDouble, varCurrency:
      Result := TValue.From<Double>(Value);
    varBoolean:
      Result := TValue.From<Boolean>(Value);
    varString, varUString, varOleStr:
      Result := TValue.From<string>(string(Value));
    varDate:
      Result := TValue.From<TDateTime>(Value);
  else
    Result := TValue.From<string>(VarToStr(Value));
  end;
end;

function TTProCompiledTemplate.GetFieldProperty(const AField: TField; const PropName: string): TValue;
begin
  // Case-insensitive property access for TField
  // Common properties
  if SameText(PropName, 'FieldName') then Result := AField.FieldName
  else if SameText(PropName, 'DisplayLabel') then Result := AField.DisplayLabel
  else if SameText(PropName, 'DisplayName') then Result := AField.DisplayName
  else if SameText(PropName, 'DisplayText') then Result := AField.DisplayText
  else if SameText(PropName, 'DisplayWidth') then Result := AField.DisplayWidth
  else if SameText(PropName, 'FieldNo') then Result := AField.FieldNo
  else if SameText(PropName, 'Index') then Result := AField.Index
  else if SameText(PropName, 'Size') then Result := AField.Size
  else if SameText(PropName, 'DataSize') then Result := AField.DataSize
  else if SameText(PropName, 'Offset') then Result := AField.Offset
  else if SameText(PropName, 'Tag') then Result := AField.Tag
  // Boolean properties
  else if SameText(PropName, 'Required') then Result := AField.Required
  else if SameText(PropName, 'ReadOnly') then Result := AField.ReadOnly
  else if SameText(PropName, 'Visible') then Result := AField.Visible
  else if SameText(PropName, 'IsNull') then Result := AField.IsNull
  else if SameText(PropName, 'IsIndexField') then Result := AField.IsIndexField
  else if SameText(PropName, 'CanModify') then Result := AField.CanModify
  else if SameText(PropName, 'Lookup') then Result := AField.Lookup
  else if SameText(PropName, 'LookupCache') then Result := AField.LookupCache
  else if SameText(PropName, 'HasConstraints') then Result := AField.HasConstraints
  // String properties
  else if SameText(PropName, 'DefaultExpression') then Result := AField.DefaultExpression
  else if SameText(PropName, 'Origin') then Result := AField.Origin
  else if SameText(PropName, 'FullName') then Result := AField.FullName
  else if SameText(PropName, 'EditMask') then Result := string(AField.EditMask)
  else if SameText(PropName, 'KeyFields') then Result := AField.KeyFields
  else if SameText(PropName, 'LookupKeyFields') then Result := AField.LookupKeyFields
  else if SameText(PropName, 'LookupResultField') then Result := AField.LookupResultField
  else if SameText(PropName, 'Text') then Result := AField.Text
  // Enum/type properties as string
  else if SameText(PropName, 'DataType') then Result := TRttiEnumerationType.GetName<TFieldType>(AField.DataType)
  else if SameText(PropName, 'FieldKind') then Result := TRttiEnumerationType.GetName<TFieldKind>(AField.FieldKind)
  else if SameText(PropName, 'Alignment') then Result := TRttiEnumerationType.GetName<TAlignment>(AField.Alignment)
  // Value properties
  else if SameText(PropName, 'Value') then Result := VariantToTValue(AField.Value)
  else if SameText(PropName, 'OldValue') then Result := VariantToTValue(AField.OldValue)
  else if SameText(PropName, 'NewValue') then Result := VariantToTValue(AField.NewValue)
  else if SameText(PropName, 'CurValue') then Result := VariantToTValue(AField.CurValue)
  // AsXxx methods
  else if SameText(PropName, 'AsString') then Result := AField.AsString
  else if SameText(PropName, 'AsInteger') then Result := AField.AsInteger
  else if SameText(PropName, 'AsFloat') then Result := AField.AsFloat
  else if SameText(PropName, 'AsBoolean') then Result := AField.AsBoolean
  else if SameText(PropName, 'AsDateTime') then Result := AField.AsDateTime
  else if SameText(PropName, 'AsCurrency') then Result := AField.AsCurrency
  else if SameText(PropName, 'AsVariant') then Result := VariantToTValue(AField.AsVariant)
  else
    Error('Unknown TField property: %s', [PropName]);
end;

function TTProCompiledTemplate.EvaluateDataSetFieldMeta(const DataSetVarName, FieldMetaInfo: string): TValue;
var
  lFieldName, lPropName: string;
  lIsLiteral: Boolean;
  lPipePos: Integer;
  lDataSet: TDataSet;
  lField: TField;
  lVarValue: TValue;
begin
  // FieldMetaInfo format: "fieldname|PropertyName (" prefix = literal) or fieldname|PropertyName (no prefix = variable)
  lIsLiteral := FieldMetaInfo.StartsWith('"');
  if lIsLiteral then
    lPipePos := Pos('|', FieldMetaInfo) - 1
  else
    lPipePos := Pos('|', FieldMetaInfo);

  if lIsLiteral then
    lFieldName := Copy(FieldMetaInfo, 2, lPipePos - 1)
  else
    lFieldName := Copy(FieldMetaInfo, 1, lPipePos - 1);

  lPropName := Copy(FieldMetaInfo, Pos('|', FieldMetaInfo) + 1, MaxInt);

  // If field name is from a variable, resolve it
  if not lIsLiteral then
    lFieldName := GetVarAsTValue(lFieldName).AsString;

  // Get the dataset
  lVarValue := GetVarAsTValue(DataSetVarName);
  if not lVarValue.IsObject then
    Error('Variable "%s" is not an object', [DataSetVarName]);

  if not (lVarValue.AsObject is TDataSet) then
    Error('Variable "%s" is not a TDataSet', [DataSetVarName]);

  lDataSet := TDataSet(lVarValue.AsObject);

  // Get the field
  lField := lDataSet.FindField(lFieldName);
  if lField = nil then
    Error('Field "%s" not found in dataset "%s"', [lFieldName, DataSetVarName]);

  // Get the property
  Result := GetFieldProperty(lField, lPropName);
end;

function TTProCompiledTemplate.GetExprEvaluator: IExprEvaluator;
var
  lSelf: TTProCompiledTemplate;
begin
  if fExprEvaluator = nil then
  begin
    fExprEvaluator := CreateExprEvaluator;
    lSelf := Self;
    fExprEvaluator.SetOnResolveExternalVariable(
      function(const VarName: string; out Value: Variant): Boolean
      var
        lTValue: TValue;
      begin
        try
          lTValue := lSelf.GetVarAsTValue(VarName);
          if not lTValue.IsEmpty then
          begin
            Value := lSelf.TValueToVariant(lTValue);
            Result := True;
          end
          else
            Result := False;
        except
          Result := False;
        end;
      end);
  end;
  Result := fExprEvaluator;
end;

function TTProCompiledTemplate.EvaluateExpression(const Expression: string): TValue;
var
  lEval: IExprEvaluator;
  lResult: Variant;
begin
  lEval := GetExprEvaluator;
  lResult := lEval.Evaluate(Expression);
  Result := VariantToTValue(lResult);
end;

initialization

GlContext := TRttiContext.Create;
JsonSerializationConfig.LineBreak := sLineBreak;

finalization

GlContext.Free;

end.
