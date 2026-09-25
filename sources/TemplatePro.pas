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
  ExprEvaluator,
  TemplatePro.Types;

type
  /// <summary>
  /// Callback procedure for custom template loading.
  /// Used to load templates from sources other than the file system (e.g., embedded resources, database, etc.)
  /// </summary>
  /// <param name="TemplateName">The name of the template as specified in the include/extends directive</param>
  /// <param name="TemplateContent">Output parameter: set to the content of the template if found</param>
  /// <param name="Handled">Output parameter: set to True if the template was loaded, False to fall back to file system</param>
  TTProTemplateResolver = reference to procedure(
    const TemplateName: string;
    var TemplateContent: string;
    var Handled: Boolean);

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
    function GetOnGetDynamicallyIncludedTemplate: TTProTemplateResolver;
    procedure SetOnGetDynamicallyIncludedTemplate(const Value: TTProTemplateResolver);
    /// <summary>
    /// Optional callback for custom template loading during dynamic includes at runtime.
    /// When set, this callback is invoked for runtime include directives like {{include @(expression)}}.
    /// If the callback sets Handled to True, the provided content is used.
    /// If Handled is False, the template falls back to loading from the file system.
    /// </summary>
    property OnGetDynamicallyIncludedTemplate: TTProTemplateResolver read GetOnGetDynamicallyIncludedTemplate write SetOnGetDynamicallyIncludedTemplate;
    function GetIncludeRootPath: string;
    procedure SetIncludeRootPath(const Value: string);
    /// <summary>
    /// Optional (empty by default). When set, dynamic includes loaded from the file system
    /// must resolve to a file inside this folder, otherwise rendering fails.
    /// </summary>
    property IncludeRootPath: string read GetIncludeRootPath write SetIncludeRootPath;
    /// <summary>
    /// True when a dependency recorded at compile time (static include, extends, import - also nested) changed:
    /// a file missing or with a different time or size, or a resolver-provided template whose version
    /// (see TTProConfiguration.TemplateChanged) changed. Files are checked by time and size only.
    /// The main template source is NOT a dependency: checking it is up to the caller.
    /// </summary>
    function IsStale: Boolean;
    /// <summary>
    /// The compiled form (the same format written by SaveToFile), to be loaded with TTProCompiledTemplate.CreateFromBytes.
    /// </summary>
    function SaveToBytes: TBytes;
  end;

  /// <summary>
  /// One field of {{for f in model.@@fields}}: for a dataset its TField answers the same names, for an object
  /// the engine creates one of these per property (and frees it). OnGetFieldMetadata can change it.
  /// </summary>
  TTProFieldMetadata = class
  private
    fFieldName: string;
    fDisplayLabel: string;
    fDataType: string;
    fRequired: Boolean;
    fReadOnly: Boolean;
    fSize: Integer;
    fVisible: Boolean;
    fHidden: Boolean;
    fValue: TValue;
  public
    property FieldName: string read fFieldName; // the property name
    property DisplayLabel: string read fDisplayLabel write fDisplayLabel; // default: "CustomerName" -> "Customer name"
    property DataType: string read fDataType write fDataType; // as TField.DataType: 'ftString', 'ftInteger', 'ftDate'...
    property Required: Boolean read fRequired write fRequired; // default False
    property ReadOnly: Boolean read fReadOnly write fReadOnly; // default: True when the property has no setter
    property Size: Integer read fSize write fSize; // default 0 (unknown)
    property Visible: Boolean read fVisible write fVisible; // default True
    property Hidden: Boolean read fHidden write fHidden; // default False
    property Value: TValue read fValue; // the current value (Nullable unwrapped: null -> empty)
  end;

  TTProCompiledTemplateEvent = reference to procedure(const TemplateProCompiledTemplate: ITProCompiledTemplate);

  TTProOpenPush = record
    Start: Integer; // position in the output buffer where the push content begins
    StackName: string;
    Once: Boolean;
  end;

  /// <summary>
  /// {{push}}/{{stack}} state of one Render, shared with the dynamically included templates
  /// </summary>
  TTProStacks = class
  private
    fContents: TObjectDictionary<string, TList<string>>;
    fPlaceholders: TList<TPair<Integer, string>>; // output position -> stack name
    fOpenPushes: TStack<TTProOpenPush>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteInto(const aBuff: TStringBuilder);
  end;

  /// <summary>
  /// A running macro: where its slots come from
  /// </summary>
  TTProSlotFrame = record
    CallTokenIndex: Int64; // the {{call}} (or {{>}}) running the macro
    CallerVariables: TTProVariables; // the slots are rendered in the caller's scope
    CallerLoops: TObjectList<TLoopStackItem>; // ...with the caller's loop variables
    ParentFrame: Integer; // the frame of the caller, -1 outside macros
  end;

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
    fAutoescapeStack: TStack<Boolean>;
    fOnGetValue: TTProCompiledTemplateGetValueEvent;
    fOnGetDynamicallyIncludedTemplate: TTProTemplateResolver;
    fIncludeRootPath: string;
    // Objects created by custom filters and stored with {{set}}: freed when Render ends
    fOwnedObjects: TObjectList<TObject>;
    // Macro calls + dynamic includes currently open (a dynamically included template starts from its parent's)
    fRenderNestingDepth: Integer;
    fStacks: TTProStacks; // set only while rendering
    fSlotFrames: TList<TTProSlotFrame>; // one per running macro call
    fCurrentSlotFrame: Integer; // the frame {{slot}} reads from, -1 outside macros
    fExprEvaluator: TExprEvaluator;
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
      const aVarNameWhereShoudBeApplied: String; out aIsCustomFilter: Boolean): TValue;
    procedure ExecuteFilterTrackingOwnership(const aFilterName: string; var aParameters: TArray<TFilterParameter>;
      var aValue: TValue; const aContextName: string; var aValueOwned: Boolean);
    procedure ReleaseOwnedObjects;
    procedure ResetRenderState;
    procedure CheckParNumber(const aHowManyPars: Integer; const aParameters: TArray<TFilterParameter>); overload;
    procedure CheckParNumber(const aMinParNumber, aMaxParNumber: Integer; const aParameters: TArray<TFilterParameter>); overload;
    function GetPseudoVariable(const VarIterator: Integer; const PseudoVarName: String): TValue; overload;
    function IsAnIterator(const VarName: String; out DataSourceName: String; out CurrentIterator: TLoopStackItem): Boolean;
    function GetOnGetValue: TTProCompiledTemplateGetValueEvent;
    function EvaluateValue(var Idx: Int64; out MustBeEncoded: Boolean; out ResultOwned: Boolean): TValue;
    function EvaluateExpressionToken(var Idx: Int64; out MustBeEncoded: Boolean; out ResultOwned: Boolean): TValue;
    procedure ApplyFilters(var Idx: Int64; var Value: TValue; FilterCount: Int64; const ContextName: string;
      out ValueOwned: Boolean; out ValueIsHTML: Boolean);
    function IsCustomFilter(const aFilterName: string): Boolean;
    function ResolveFilterParameter(const aToken: TToken): TFilterParameter;
    function GetParameterValue(const aParameter: TFilterParameter): TValue;
    function ValueAsString(const aValue: TValue): string;
    function OutputString(const aValue: TValue): string;
    function MemberValue(const aModel: TValue; const aName: string): TValue;
    function IsObjectForFields(const aExpression: string; out aObject: TValue): Boolean;
    function ObjectFieldsMetadata(const aObject: TObject): TObjectList<TTProFieldMetadata>;
    function ExecuteListAndTextFilter(const aFunctionName: string; var aParameters: TArray<TFilterParameter>;
      const aValue: TValue; out aResult: TValue): Boolean;
    procedure SetOnGetValue(const Value: TTProCompiledTemplateGetValueEvent);
    procedure DoOnGetValue(const DataSource, Members: string; var Value: TValue; var Handled: Boolean);
    function GetFormatSettings: PTProFormatSettings;
    procedure SetFormatSettings(const Value: PTProFormatSettings);
    class procedure InternalDumpToFile(const FileName: String; const aTokens: TList<TToken>);
    function ComparandOperator(const aComparandType: TComparandType; const aValue: TValue; const aParameters: TArray<TFilterParameter>;
      const aLocaleFormatSettings: TFormatSettings): TValue;
    procedure RegisterMacro(const TokenIndex: Int64);
    procedure ExecuteMacro(const CallTokenIndex: Int64; const aBuff: TStringBuilder);
    procedure ExecuteMacroBody(const CallTokenIndex: Int64; const aBuff: TStringBuilder);
    procedure RenderTo(const aBuff: TStringBuilder; const aStacks: TTProStacks);
    procedure RenderRange(const aBuff: TStringBuilder; const aFrom, aTo: Int64);
    function RenderSlot(const aSlotIdx: Int64; const aBuff: TStringBuilder): Int64;
    function FindSlotContent(const aCallIdx: Int64; const aSlotName: string; out aFrom, aTo: Int64): Boolean;
    function IsBlankRange(const aFrom, aTo: Int64): Boolean;
    function SlotsInfo(const aCallIdx: Int64): TObject; // a TJDOJsonObject
    function ResolveTokenName(const aToken: TToken): string;
    procedure BeginPush(const aIdx: Int64; const aBuff: TStringBuilder);
    procedure EndPush(const aBuff: TStringBuilder);
    procedure AddStackPlaceholder(const aIdx: Int64; const aBuff: TStringBuilder);
    function MacroArgumentValue(const aParameter: TFilterParameter): TValue;
    function MacroArgument(const aTokenIdx: Int64; const aParameter: TFilterParameter; var aOwned: TArray<TObject>): TValue;
    function LastFilterToken(const aIdx: Int64): Int64;
    procedure ProcessSetToken(var Idx: Int64);
    function SelectSwitchBranch(const aSwitchIdx: Int64): Int64;
    procedure InitRangeLoop(const aLoop: TLoopStackItem);
    function TokenToFilterParameter(const aToken: TToken): TFilterParameter;
    function ValueToFilterParameter(const aValue: TValue): TFilterParameter;
    function ExecuteStringFilter(const aFunctionName: string; var aParameters: TArray<TFilterParameter>;
      const aValue: TValue; const aExecuteAsFilterOnAValue: Boolean; out aResult: TValue): Boolean;
    function ExecuteDateFilter(const aFunctionName: string; var aParameters: TArray<TFilterParameter>;
      const aValue: TValue; const aVarNameWhereShoudBeApplied: String; out aResult: TValue): Boolean;
    function GetExprEvaluator: TExprEvaluator;
    function TValueToVariant(const Value: TValue): Variant;
    function VariantToTValue(const Value: Variant): TValue;
    function GetFieldProperty(const AField: TField; const PropName: string): TValue;
    function EvaluateDataSetFieldMeta(const DataSetVarName, FieldMetaInfo: string): TValue;
    function GetOutputLineEnding: TLineEndingStyle;
    procedure SetOutputLineEnding(const Value: TLineEndingStyle);
    function GetLineEndingString: string;
    function GetOnGetDynamicallyIncludedTemplate: TTProTemplateResolver;
    procedure SetOnGetDynamicallyIncludedTemplate(const Value: TTProTemplateResolver);
    function GetIncludeRootPath: string;
    procedure SetIncludeRootPath(const Value: string);
  public
    function EvaluateExpression(const Expression: string): TValue;
    destructor Destroy; override;
    function Render: String;
    function IsStale: Boolean;
    procedure ForEachToken(const TokenProc: TTokenWalkProc);
    procedure ClearData;
    procedure SaveToFile(const FileName: String);
    function SaveToBytes: TBytes;
    class function CreateFromFile(const FileName: String): ITProCompiledTemplate;
    /// <summary>
    /// A new, independent instance from the output of SaveToBytes. No file system access.
    /// </summary>
    class function CreateFromBytes(const aBytes: TBytes): ITProCompiledTemplate;
    procedure SetData(const Name: String; Value: TValue); overload;
    procedure AddFilter(const FunctionName: string; const FunctionImpl: TTProTemplateFunction); overload;
    procedure AddFilter(const FunctionName: string; const AnonFunctionImpl: TTProTemplateAnonFunction); overload;
    procedure DumpToFile(const FileName: String);
    property FormatSettings: PTProFormatSettings read GetFormatSettings write SetFormatSettings;
    property OnGetValue: TTProCompiledTemplateGetValueEvent read GetOnGetValue write SetOnGetValue;
    property OutputLineEnding: TLineEndingStyle read GetOutputLineEnding write SetOutputLineEnding;
    property OnGetDynamicallyIncludedTemplate: TTProTemplateResolver read GetOnGetDynamicallyIncludedTemplate write SetOnGetDynamicallyIncludedTemplate;
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
    fIncludeChain: TList<string>; // files being compiled, from the root down to the current one
    fStripNextLeadingWS: Boolean;  // For whitespace control: -}} strips leading WS from next content
    fOnGetIncludedTemplate: TTProTemplateResolver;
    // {{import}} namespace -> library full path, shared with the files included in the same template
    fNamespaces: TDictionary<string, string>;
    fOwnedNamespaces: TDictionary<string, string>;
    // ttDependency tokens of the template being compiled, shared with the compilers of its includes, layouts and libraries
    fDependencies: TList<TToken>;
    fOwnedDependencies: TList<TToken>;
    procedure AddDependency(const aName, aKind: string; const aRef1, aRef2: Int64);
    procedure ImportLibrary(const aLibraryName, aAlias: string; const aTokens: TList<TToken>; const aFileNameRefPath: string);
    function MatchLineBreak: Boolean;
    function MatchStartTag: Boolean;
    function MatchEndTag: Boolean;
    function MatchVariable(var aIdentifier: string): Boolean;
    function MatchFilterParamValue(var aParamValue: TFilterParameter): Boolean;
    function MatchSymbol(const aSymbol: string): Boolean;
    function MatchExpression(out aExpression: string): Boolean;
    function MatchRange(out aRangeExpression: string): Boolean;
    function MatchNameArgument(out aName, aKind: string): Boolean;
    function MatchSpace: Boolean;
    function MatchString(out aStringValue: string): Boolean;
    procedure InternalMatchFilter(lIdentifier: String; var lStartVerbatim: Int64; const CurrToken: TTokenType; aTokens: TList<TToken>;
      const lRef2: Integer);
    function GetFunctionParameters(const aStopAtNamedArg: Boolean = False): TArray<TFilterParameter>;
    function GetMacroParameters(out aNames: TArray<string>; out aFilters: TArray<TArray<TFilterInfo>>): TArray<TFilterParameter>;
    procedure AddMacroParameterTokens(aTokens: TList<TToken>; const aMacroTokenIdx: Integer; const aParamTokens: TArray<TToken>;
      const aFilters: TArray<TArray<TFilterInfo>>);
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
    procedure MatchFilters(lVarName: string; var lFilters: TArray<TFilterInfo>; const aStopAtNamedArg: Boolean = False);
    procedure AddFilterTokens(aTokens: TList<TToken>; const aFilters: TArray<TFilterInfo>);
    function LoadTemplateSource(const aTemplateName: string; const aFullPath: string): string;
  public
    destructor Destroy; override;
    function Compile(const aTemplate: string; const aFileNameRefPath: String = ''): ITProCompiledTemplate; overload;
    /// <summary>
    /// Compiles a template directly from a string.
    /// Useful for templates that don't come from files (e.g., from database, strings, etc.)
    /// </summary>
    /// <param name="aTemplateString">The template source code as a string</param>
    /// <returns>A compiled template ready for data binding and rendering</returns>
    function CompileFromString(const aTemplateString: string): ITProCompiledTemplate;
    constructor Create(aEncoding: TEncoding = nil); overload;
    class function CompileAndRender(const aTemplate: string; const VarNames: TArray<String>;
      const VarValues: TArray<TValue>; const aFileNameRefPath: String = ''): String;
    /// <summary>
    /// Optional callback for custom template loading.
    /// When set, this callback is invoked for include and extends directives.
    /// If the callback returns True, the provided content is used.
    /// If it returns False, the compiler falls back to loading from the file system.
    /// </summary>
    property OnGetIncludedTemplate: TTProTemplateResolver read fOnGetIncludedTemplate write fOnGetIncludedTemplate;
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

  TTProFieldMetadataEvent = reference to procedure(const aObject: TObject; const aPropertyName: string;
    const aMetadata: TTProFieldMetadata);

  TTProConfiguration = class sealed
  private
    class var fOnContextConfiguration: TTProCompiledTemplateEvent;
    class var fOnGetFieldMetadata: TTProFieldMetadataEvent;
    class var fOnGetTemplate: TTProTemplateResolver;
    class var fTemplateVersions: TDictionary<string, Int64>; // guarded by TMonitor
    class constructor Create;
    class destructor Destroy;
  protected
    class procedure RegisterHandlers(const TemplateProCompiledTemplate: ITProCompiledTemplate);
  public
    class property OnContextConfiguration: TTProCompiledTemplateEvent read fOnContextConfiguration write fOnContextConfiguration;
    /// <summary>
    /// Optional global resolver for static includes, extends, imports (compile time) and dynamic includes (render time),
    /// usable where the compiler is not accessible. It is asked after the instance resolver
    /// (TTProCompiler.OnGetIncludedTemplate / OnGetDynamicallyIncludedTemplate) and before the file system.
    /// Set it at startup: it is not synchronized.
    /// </summary>
    class property OnGetTemplate: TTProTemplateResolver read fOnGetTemplate write fOnGetTemplate;
    /// <summary>
    /// Called once per property when {{for f in obj.@@fields}} iterates an object (not a dataset): it can change
    /// DisplayLabel, DataType, Required, ReadOnly, Size, Visible, Hidden. Set it at startup: it is not synchronized.
    /// </summary>
    class property OnGetFieldMetadata: TTProFieldMetadataEvent read fOnGetFieldMetadata write fOnGetFieldMetadata;
    /// <summary>
    /// Call it when a template served by a resolver changed (e.g. in a database): the compiled templates
    /// depending on that name become stale (ITProCompiledTemplate.IsStale). Names are case-insensitive. Thread-safe.
    /// </summary>
    class procedure TemplateChanged(const Name: string);
    class function GetTemplateVersion(const Name: string): Int64;
  end;

function HTMLEncode(s: string): string;
function HandleTemplateSectionStateMachine(const aTokenValue1: String; var aTemplateSectionType: TTProTemplateSectionType;
  out aErrorMessage: String): Boolean;
function GetTValueFromPath(const aObject: TObject; FullPropertyPath: String): TValue;

implementation

uses
  System.StrUtils, System.IOUtils, System.NetEncoding, System.Math, System.Character, System.RegularExpressions,
  JsonDataObjects, MVCFramework.Nullables, Data.FmtBCD, Data.SqlTimSt;

const
  MAX_RENDER_NESTING = 64; // recursion through macros and dynamic includes, e.g. a tree 64 levels deep
  Sign = ['-', '+'];
  Numbers = ['0' .. '9'];
  SignAndNumbers = Sign + Numbers;
  IdenfierAllowedFirstChars = ['a' .. 'z', 'A' .. 'Z', '_', '@'];
  IdenfierAllowedChars = ['a' .. 'z', 'A' .. 'Z', '_'] + Numbers;
  ValueAllowedChars = IdenfierAllowedChars + [' ', '-', '+', '*', '.', '@', '/', '\']; // maybe a lot others
  START_TAG = '{{';
  END_TAG = '}}';
  RANGE_PREFIX = 'range(';
  RANGE_ARG_SEPARATOR = #1; // between the arguments of a compiled range(...)

type
  TTProRTTIUtils = class sealed
  public
    class function GetProperty(AObject: TObject; const APropertyName: string): TValue;
    class function ObjectToJSONString(AObject: TObject): string;
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

function GetFileStamp(const aFileName: string; out aTime, aSize: Int64): Boolean;
// last write time (as its bits) and size, without opening the file
var
  lSearchRec: TSearchRec;
  lDateTime: TDateTime;
begin
  Result := FindFirst(aFileName, faAnyFile, lSearchRec) = 0;
  if not Result then
    Exit;
  try
    lDateTime := lSearchRec.TimeStamp;
    aTime := PInt64(@lDateTime)^;
    aSize := lSearchRec.Size;
  finally
    FindClose(lSearchRec);
  end;
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
    Result := False;
    Exit;
  end;
  case aValue.TypeInfo.Kind of
    tkInteger, tkEnumeration, tkInt64:
      begin
        if aParameters[0].ParType = fptString then
        begin
          raise ETProRenderException.Create('Invalid type for comparand');
        end;
        if aParameters[0].ParType = fptFloat then
          Exit(ComparandOperator(aComparandType, TValue.From<Extended>(aValue.AsOrdinal), aParameters, aLocaleFormatSettings));
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
            fptInteger:
              begin
                lExtendedValue := aParameters[0].ParIntValue;
              end;
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

{ Cardinal (and other 32-bit unsigned types) and UInt64 must not be read with
  AsInteger/AsInt64: values above the signed maximum would come out negative. }
function IsUnsignedInteger(const aTypeInfo: PTypeInfo): Boolean;
begin
  Result := (aTypeInfo <> nil) and (aTypeInfo^.Kind = tkInteger) and
    (GetTypeData(aTypeInfo)^.OrdType = otULong);
end;

function IsUnsignedInt64(const aTypeInfo: PTypeInfo): Boolean;
begin
  // for an unsigned 64-bit type the range stored as Int64 is 0..-1
  Result := (aTypeInfo <> nil) and (aTypeInfo^.Kind = tkInt64) and
    (GetTypeData(aTypeInfo)^.MinInt64Value > GetTypeData(aTypeInfo)^.MaxInt64Value);
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
    ftInteger, ftSmallInt, ftWord, ftShortint, ftByte:
      Result := lField.AsInteger;
    ftLargeint, ftAutoInc:
      Result := lField.AsLargeInt;
{$IF CompilerVersion >= 37} // ftLargeUint and TField.AsLargeUInt exist since Delphi 13
    ftLargeUint:
      Result := lField.AsLargeUInt;
{$ENDIF}
    ftLongWord:
      Result := lField.AsLongWord;
    ftFloat:
      Result := lField.AsFloat;
    ftSingle:
      Result := lField.AsSingle;
    ftExtended:
      Result := lField.AsExtended;
    ftCurrency:
      Result := lField.AsCurrency;
    ftString, ftWideString, ftMemo, ftWideMemo, ftGuid, ftFixedChar, ftFixedWideChar:
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

function TTProCompiledTemplate.GetOnGetDynamicallyIncludedTemplate: TTProTemplateResolver;
begin
  Result := fOnGetDynamicallyIncludedTemplate;
end;

procedure TTProCompiledTemplate.SetOnGetDynamicallyIncludedTemplate(const Value: TTProTemplateResolver);
begin
  fOnGetDynamicallyIncludedTemplate := Value;
end;

function TTProCompiledTemplate.GetIncludeRootPath: string;
begin
  Result := fIncludeRootPath;
end;

procedure TTProCompiledTemplate.SetIncludeRootPath(const Value: string);
begin
  fIncludeRootPath := Value;
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

procedure ParseJSONArrayPath(const aPath: String; out aIndex: Integer; out aRemainingPath: String);
var
  lCloseBracketPos: Integer;
begin
  // Path format: [index].property.subproperty or [index]
  // Example: [0].devices or [0].car.brand
  aIndex := -1;
  aRemainingPath := '';

  if not aPath.StartsWith('[') then
    Exit;

  // Extract index from [index]
  lCloseBracketPos := aPath.IndexOf(']');
  if lCloseBracketPos < 0 then
    Exit;

  aIndex := StrToIntDef(aPath.Substring(1, lCloseBracketPos - 1), -1);

  // Get remaining path after ]
  aRemainingPath := aPath.Substring(lCloseBracketPos + 1);
  if aRemainingPath.StartsWith('.') then
    aRemainingPath := aRemainingPath.Substring(1);
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
          if IsUnsignedInteger(Value.TypeInfo) then
            Result := Value.AsType<Cardinal>.ToString
          else
            Result := Value.AsInteger.ToString;
        tkInt64:
          if IsUnsignedInt64(Value.TypeInfo) then
            Result := Value.AsType<UInt64>.ToString
          else
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

function TTProCompiler.LoadTemplateSource(const aTemplateName: string;
  const aFullPath: string): string;
var
  lHandled: Boolean;
  lTime, lSize: Int64;
begin
  // First the instance resolver, then the global one
  lHandled := False;
  if Assigned(fOnGetIncludedTemplate) then
    fOnGetIncludedTemplate(aTemplateName, Result, lHandled);
  if not lHandled and Assigned(TTProConfiguration.fOnGetTemplate) then
    TTProConfiguration.fOnGetTemplate(aTemplateName, Result, lHandled);
  if lHandled then
  begin
    AddDependency(aTemplateName, 'r', TTProConfiguration.GetTemplateVersion(aTemplateName), -1);
    Exit;
  end;
  // Fallback to file system using the pre-computed full path
  Result := TFile.ReadAllText(aFullPath, fEncoding);
  if GetFileStamp(aFullPath, lTime, lSize) then
    AddDependency(aFullPath, 'f', lTime, lSize);
end;

procedure TTProCompiler.AddDependency(const aName, aKind: string; const aRef1, aRef2: Int64);
// Value1 = full path ("f") or resolver name ("r"); Ref1 = file time or template version, Ref2 = file size
var
  lToken: TToken;
begin
  for lToken in fDependencies do
    if (lToken.Value2 = aKind) and SameText(lToken.Value1, aName) then
      Exit;
  fDependencies.Add(TToken.Create(ttDependency, aName, aKind, aRef1, aRef2));
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
    // Copy include chain (plus this file) for circular include detection
    lCompiler.fIncludeChain.AddRange(fIncludeChain);
    lCompiler.fIncludeChain.Add(aFileNameRefPath);
    // Propagate the template resolver callback
    lCompiler.fOnGetIncludedTemplate := fOnGetIncludedTemplate;
    // an included file or a layout belongs to the same template: same namespaces
    lCompiler.fNamespaces := fNamespaces;
    lCompiler.fDependencies := fDependencies;
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

procedure TTProCompiler.ImportLibrary(const aLibraryName, aAlias: string; const aTokens: TList<TToken>;
  const aFileNameRefPath: string);
// {{import "lib" as ns}}: the macros of the library are embedded in this template, named "ns.<macro>".
// Inside the library, calls to its own macros (and to the ones it imports) are qualified the same way.
var
  lFullPath: string;
  lChainFile: string;
  lImportedPath: string;
  lSource: string;
  lCompiler: TTProCompiler;
  lLibTokens: TList<TToken>;
  lNames: TDictionary<string, Boolean>;
  lToken: TToken;
  lDepth: Integer;
begin
  if TDirectory.Exists(aFileNameRefPath) then
    lFullPath := TPath.GetFullPath(TPath.Combine(aFileNameRefPath, aLibraryName))
  else
    lFullPath := TPath.GetFullPath(TPath.Combine(TPath.GetDirectoryName(aFileNameRefPath), aLibraryName));
  for lChainFile in fIncludeChain do
    if SameText(lChainFile, lFullPath) then
      Error('Circular import detected: "' + aLibraryName + '"');
  if fNamespaces.TryGetValue(aAlias, lImportedPath) and not SameText(lImportedPath, lFullPath) then
    Error('Namespace "' + aAlias + '" already imported');
  fNamespaces.AddOrSetValue(aAlias, lFullPath);
  try
    lSource := LoadTemplateSource(aLibraryName, lFullPath);
  except
    on E: Exception do
      Error('Cannot read "' + aLibraryName + '"');
  end;

  lLibTokens := TList<TToken>.Create;
  lNames := TDictionary<string, Boolean>.Create(TTProEqualityComparer.Create);
  try
    lCompiler := TTProCompiler.Create(fEncoding, [coIgnoreSysVersion, coParentTemplate] + (fOptions * [coDisableEatLineBreaks]));
    try
      lCompiler.fIncludeChain.AddRange(fIncludeChain);
      lCompiler.fIncludeChain.Add(lFullPath);
      lCompiler.fOnGetIncludedTemplate := fOnGetIncludedTemplate;
      lCompiler.fDependencies := fDependencies;
      // lCompiler keeps its own namespaces: the libraries imported by the library are internal to it
      lCompiler.Compile(lSource, lLibTokens, lFullPath);
    finally
      lCompiler.Free;
    end;

    // outside the macros only whitespace is allowed (comments and imports leave no tokens there)
    lDepth := 0;
    for lToken in lLibTokens do
    begin
      if lToken.TokenType = ttMacro then
      begin
        lNames.AddOrSetValue(lToken.Value1, True);
        Inc(lDepth);
      end
      else if (lToken.TokenType = ttEndMacro) and (lDepth > 0) then
        Dec(lDepth)
      else if (lDepth = 0) and not ((lToken.TokenType in [ttSystemVersion, ttLineBreak, ttEOF]) or
        ((lToken.TokenType = ttContent) and lToken.Value1.Trim.IsEmpty)) then
        Error('Library "' + aLibraryName + '" can contain only macros and imports');
    end;
    if lDepth <> 0 then
      Error('Library "' + aLibraryName + '": "macro" without "endmacro"');

    for lToken in lLibTokens do
    begin
      if lToken.TokenType = ttMacro then
        Inc(lDepth);
      if lDepth > 0 then
      begin
        if (lToken.TokenType = ttMacro) or ((lToken.TokenType = ttCallMacro) and lNames.ContainsKey(lToken.Value1)) then
          aTokens.Add(TToken.Create(lToken.TokenType, aAlias + '.' + lToken.Value1, lToken.Value2, lToken.Ref1, lToken.Ref2))
        else
          aTokens.Add(lToken);
      end;
      if lToken.TokenType = ttEndMacro then
        Dec(lDepth);
    end;
  finally
    lNames.Free;
    lLibTokens.Free;
  end;
end;

procedure TTProCompiler.InternalMatchFilter(lIdentifier: String; var lStartVerbatim: Int64; const CurrToken: TTokenType;
  aTokens: TList<TToken>; const lRef2: Integer);
var
  lFilters: TArray<TFilterInfo>;
begin
  SetLength(lFilters, 0);
  MatchSpace;
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
  Result := TToken.Create(ttFilterParameter, '', '', -1, -1);
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

    fptVariable, fptExpression:
      begin
        Result.TokenType := ttFilterParameter;
        Result.Value1 := FilterParameter.ParStrText;
        Result.Ref2 := Ord(FilterParameter.ParType);
      end;

    fptFloat:
      begin
        Result.TokenType := ttFilterParameter;
        Result.Value1 := FloatToStr(FilterParameter.ParFloatValue, TFormatSettings.Invariant);
        Result.Ref2 := Ord(FilterParameter.ParType);
      end;

  else
    raise ETProCompilerException.Create('Invalid filter parameter type');
  end;

end;

procedure TTProCompiler.MatchFilters(lVarName: string; var lFilters: TArray<TFilterInfo>; const aStopAtNamedArg: Boolean);
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
    lFuncParams := GetFunctionParameters(aStopAtNamedArg);
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
var
  lHasMinus: Boolean;
begin
  // Check for -}} (strip leading whitespace from next content)
  lHasMinus := (CurrentChar = '-');
  if lHasMinus then
    Step; // skip the '-'
  Result := MatchSymbol(END_TAG);
  if Result and lHasMinus then
    fStripNextLeadingWS := True
  else if not Result and lHasMinus then
  begin
    // We stepped past '-' but didn't find '}}'', need to step back
    Dec(fCharIndex);
  end;
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

function TTProCompiler.MatchRange(out aRangeExpression: string): Boolean;
// range(stop) or range(start, stop[, step]); every argument is an expression (a bare one or @(...)).
// Compiled as RANGE_PREFIX + the arguments separated by RANGE_ARG_SEPARATOR + ')'
var
  lSavedCharIndex: Int64;
  lDepth: Integer;
  lQuote: Char;
  lArg: string;
  lArgs: TArray<string>;
  I: Integer;
begin
  lSavedCharIndex := fCharIndex;
  Result := MatchSymbol('range');
  if Result then
  begin
    MatchSpace;
    Result := MatchSymbol('(');
  end;
  if not Result then
  begin
    fCharIndex := lSavedCharIndex;
    Exit;
  end;
  lArgs := [];
  lArg := '';
  lDepth := 0;
  lQuote := #0;
  while True do
  begin
    if CurrentChar = #0 then
      Error('Unclosed "range("');
    if lQuote <> #0 then
    begin
      if CurrentChar = lQuote then
        lQuote := #0;
    end
    else if CharInSet(CurrentChar, ['"', '''']) then
      lQuote := CurrentChar
    else if (CurrentChar = '@') and (fCharIndex + 1 < fInputString.Length) and
      (fInputString.Chars[fCharIndex + 1] = '(') then
    begin
      Step; // @(expr) is the same as (expr) here: drop the marker, keep the parenthesis
      Continue;
    end
    else if CurrentChar = '(' then
      Inc(lDepth)
    else if CurrentChar = ')' then
    begin
      if lDepth = 0 then
        Break;
      Dec(lDepth);
    end
    else if (CurrentChar = ',') and (lDepth = 0) then
    begin
      lArgs := lArgs + [lArg];
      lArg := '';
      Step;
      Continue;
    end;
    lArg := lArg + CurrentChar;
    Step;
  end;
  Step; // skip ')'
  lArgs := lArgs + [lArg];
  if Length(lArgs) > 3 then
    Error('range expects 1 to 3 arguments');
  for I := 0 to High(lArgs) do
  begin
    lArgs[I] := lArgs[I].Trim;
    if lArgs[I].IsEmpty then
      Error('range expects 1 to 3 arguments');
  end;
  aRangeExpression := RANGE_PREFIX + String.Join(RANGE_ARG_SEPARATOR, lArgs) + ')';
end;

function TTProCompiler.MatchNameArgument(out aName, aKind: string): Boolean;
// a name given as "literal", variable or @(expression); aKind is "s", "v" or "@"
begin
  Result := True;
  if MatchString(aName) then
    aKind := 's'
  else if MatchExpression(aName) then
    aKind := '@'
  else if MatchVariable(aName) then
    aKind := 'v'
  else
    Result := False;
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
  fIncludeChain.Clear;
  fNamespaces.Clear;
  fDependencies.Clear;
  if not aFileNameRefPath.IsEmpty then
    fIncludeChain.Add(lFileNameRefPath);
  lTokens := TList<TToken>.Create;
  try
    Compile(aTemplate, lTokens, fCurrentFileName);
    // the dependencies go just before the final EOF: saved with the template, skipped by Render
    lTokens.InsertRange(lTokens.Count - 1, fDependencies.ToArray);
    ProcessJumps(lTokens);
    Result := TTProCompiledTemplate.Create(lTokens);
  except
    lTokens.Free;
    raise;
  end;
end;

function TTProCompiler.CompileFromString(const aTemplateString: string): ITProCompiledTemplate;
begin
  Result := Compile(aTemplateString, '');
end;

class function TTProCompiler.CompileAndRender(const aTemplate: String; const VarNames: TArray<String>;
  const VarValues: TArray<TValue>; const aFileNameRefPath: String): String;
var
  lComp: TTProCompiler;
  lCompiledTemplate: ITProCompiledTemplate;
  I: Integer;
begin
  lComp := TTProCompiler.Create();
  try
    lCompiledTemplate := lComp.Compile(aTemplate, aFileNameRefPath);
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
  fIncludeChain := TList<string>.Create;
  fOwnedNamespaces := TDictionary<string, string>.Create(TTProEqualityComparer.Create);
  fNamespaces := fOwnedNamespaces;
  fOwnedDependencies := TList<TToken>.Create;
  fDependencies := fOwnedDependencies;
end;

destructor TTProCompiler.Destroy;
begin
  fInheritanceChain.Free;
  fIncludeChain.Free;
  fOwnedNamespaces.Free;
  fOwnedDependencies.Free;
  inherited;
end;

procedure TTProCompiler.Compile(const aTemplate: string; const aTokens: TList<TToken>; const aFileNameRefPath: String);
var
  lForStatementCount: Integer;
  lIfStatementCount: Integer;
  lElseIfPendingCounts: TArray<Integer>;  // Stack to track pending elseif endifs per if-level
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
  lIncludeChainFile: string;
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
  lBlockStack: string;  // Stack of 'F' (for) and 'I' (if) to track nesting for for-else
  // Variables for whitespace control
  lStripTrailingWS: Boolean; // Strip trailing whitespace from current content ({{-)
  lRawEndPos: Integer;       // Position of {{endraw}} for raw blocks
  lRawContent: string;       // Content inside raw block
  // switch: per open switch, 0 = before the first case, 1 = in a case, 2 = after default
  lSwitchStates: TArray<Integer>;
  lSwitchBodyStarts: TArray<Integer>;
  lCaseValue: TFilterParameter;
  lCaseValues: TArray<TFilterParameter>;
  lMacroArgNames: TArray<string>;
  lMacroArgFilters: TArray<TArray<TFilterInfo>>;
  lMacroParamToken: TToken;
  lMacroParamTokens: TArray<TToken>;
  lMacroDepth: Integer;
  lImportedAliases: TArray<string>;
  lIsBlockCall: Boolean;
  lFillNames: TArray<string>; // per open {{call}}: the literal names of its fills, as "|name1||name2|"

  procedure CloseSwitchBranch(const aTag: string);
  var
    J: Integer;
  begin
    if (Length(lBlockStack) = 0) or (lBlockStack[Length(lBlockStack)] <> 'S') then
      Error('"' + aTag + '" without "switch"');
    if lSwitchStates[High(lSwitchStates)] = 0 then
    begin
      for J := lSwitchBodyStarts[High(lSwitchBodyStarts)] to aTokens.Count - 1 do
        if not ((aTokens[J].TokenType = ttLineBreak) or
          ((aTokens[J].TokenType = ttContent) and aTokens[J].Value1.Trim.IsEmpty)) then
          Error('Only whitespace is allowed between "switch" and the first "case"');
      aTokens.DeleteRange(lSwitchBodyStarts[High(lSwitchBodyStarts)],
        aTokens.Count - lSwitchBodyStarts[High(lSwitchBodyStarts)]);
    end
    else if (lEndVerbatim > lStartVerbatim) and (aTokens.Last.TokenType = ttContent) and aTokens.Last.Value1.Trim.IsEmpty and
      ((lStartVerbatim = 0) or CharInSet(fInputString.Chars[lStartVerbatim - 1], [#10, #13])) then
      aTokens.Delete(aTokens.Count - 1); // indentation of a case/default/endswitch line is not part of the previous branch
  end;

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
  lBlockStack := '';  // Empty stack at start
  lMacroDepth := 0;
  lImportedAliases := [];
  lFillNames := [];
  SetLength(lElseIfPendingCounts, 0);
  fStripNextLeadingWS := False;  // Initialize whitespace control
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
        lStrVerbatim := fInputString.Substring(lStartVerbatim, lEndVerbatim - lStartVerbatim);
        // Handle whitespace stripping from previous -}} tag
        if fStripNextLeadingWS then
        begin
          lStrVerbatim := lStrVerbatim.TrimLeft;
          fStripNextLeadingWS := False;
        end;
        aTokens.Add(TToken.Create(lLastToken, lStrVerbatim, ''));
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
        // Handle whitespace stripping from previous -}} tag
        if fStripNextLeadingWS then
        begin
          lStrVerbatim := lStrVerbatim.TrimLeft;
          fStripNextLeadingWS := False;
        end;
        aTokens.Add(TToken.Create(ttContent, lStrVerbatim, ''));
      end
      else if fStripNextLeadingWS then
        fStripNextLeadingWS := False;  // Reset flag even if no content
      lStartVerbatim := fCharIndex;
      if lLastToken = ttLineBreak then
        Inc(lContentOnThisLine);
      lLastToken := ttLineBreak;
      // Add line break token if:
      // - coDisableEatLineBreaks is set (always preserve line breaks), or
      // - There was content on this line (default "eat linebreaks" behavior)
      if (coDisableEatLineBreaks in fOptions) or (lContentOnThisLine > 0) then
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
        lStrVerbatim := fInputString.Substring(lStartVerbatim, lEndVerbatim - lStartVerbatim);
        // Handle whitespace stripping from previous -}} tag
        if fStripNextLeadingWS then
        begin
          lStrVerbatim := lStrVerbatim.TrimLeft;
          fStripNextLeadingWS := False;
        end;
        aTokens.Add(TToken.Create(lLastToken, lStrVerbatim, ''));
      end
      else if fStripNextLeadingWS then
        fStripNextLeadingWS := False;  // Reset flag even if no content

      // Check for {{- (strip trailing whitespace from previous content)
      lStripTrailingWS := (CurrentChar = '-');
      if lStripTrailingWS then
      begin
        Step; // skip the '-'
        // Trim trailing whitespace from the last content token
        if (aTokens.Count > 0) and (aTokens[aTokens.Count - 1].TokenType = ttContent) then
        begin
          aTokens[aTokens.Count - 1] := TToken.Create(ttContent,
            aTokens[aTokens.Count - 1].Value1.TrimRight, '');
        end;
        MatchSpace; // skip whitespace after {{- before tag content
      end;

      if CurrentChar = START_TAG[1] then
      begin
        lLastToken := ttContent;
        aTokens.Add(TToken.Create(lLastToken, START_TAG, ''));
        Inc(fCharIndex);
        lStartVerbatim := fCharIndex;
        Continue;
      end;

      if CurrentChar = '@' then // expression {{@expr}} or {{@expr|filter}}
      begin
        Step; // skip '@'
        MatchSpace; // skip optional spaces after '@'
        lVarName := '';
        SetLength(lFilters, 0);
        lFoundFilter := False;
        // Read expression until pipe or end tag
        while True do
        begin
          if CurrentChar = '|' then
          begin
            lFoundFilter := True;
            Break; // found filter separator
          end;
          if MatchEndTag then
            Break; // found end tag (fCharIndex is now after }})
          if fCharIndex > Length(fInputString) then
            Error('Unclosed expression tag');
          lVarName := lVarName + CurrentChar;
          Step;
        end;
        lVarName := lVarName.Trim;
        if lVarName.IsEmpty then
          Error('Empty expression after "@"');
        // Parse filters if present
        if lFoundFilter then
        begin
          Step; // skip '|'
          MatchFilters(lVarName, lFilters);
          if not MatchEndTag then
            Error('Expected end tag "' + END_TAG + '"');
        end;
        lLastToken := ttExpression;
        aTokens.Add(TToken.Create(lLastToken, lVarName, '', Length(lFilters), -1));
        lStartVerbatim := fCharIndex;
        Inc(lContentOnThisLine);
        // add filter tokens
        AddFilterTokens(aTokens, lFilters);
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
        if MatchSymbol('raw') then { raw block - output content without processing }
        begin
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag after "raw"');
          // Find {{endraw}}
          lRawEndPos := Pos('{{endraw}}', fInputString, fCharIndex + 1); // Pos uses 1-based offset
          if lRawEndPos = 0 then
            Error('Missing {{endraw}} for raw block');
          // Extract raw content (lRawEndPos is 1-based, fCharIndex is 0-based)
          lRawContent := fInputString.Substring(fCharIndex, lRawEndPos - 1 - fCharIndex);
          // Handle whitespace stripping from previous -}} tag
          if fStripNextLeadingWS then
          begin
            lRawContent := lRawContent.TrimLeft;
            fStripNextLeadingWS := False;
          end;
          // Add as content token
          if not lRawContent.IsEmpty then
          begin
            lLastToken := ttContent;
            aTokens.Add(TToken.Create(lLastToken, lRawContent, ''));
            Inc(lContentOnThisLine);
          end;
          // Skip past {{endraw}} (convert 1-based lRawEndPos to 0-based fCharIndex)
          fCharIndex := lRawEndPos - 1 + Length('{{endraw}}');
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('for') then { loop }
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
          if not MatchRange(lIdentifier) then
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
          // Check for .fields (datasets) or .@@fields (datasets and objects) suffix for field iteration
          lIsFieldIteration := 0;
          if lIdentifier.EndsWith('.fields', True) then
          begin
            lIdentifier := lIdentifier.Substring(0, lIdentifier.Length - 7); // Remove '.fields'
            lIsFieldIteration := 1;
          end
          else if lIdentifier.EndsWith('.@@fields', True) then
          begin
            lIdentifier := lIdentifier.Substring(0, lIdentifier.Length - 9); // Remove '.@@fields'
            lIsFieldIteration := 1;
          end;
          aTokens.Add(TToken.Create(lLastToken, lIdentifier, lIteratorName, -1, lIsFieldIteration));
          lBlockStack := lBlockStack + 'F';  // Push 'F' for for-else tracking
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
          if Length(lBlockStack) > 0 then
            lBlockStack := Copy(lBlockStack, 1, Length(lBlockStack) - 1);  // Pop from block stack
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
              MatchSpace;
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

          // Emit extra ttEndIf tokens for pending elseif chains
          lLastToken := ttEndIf;
          if Length(lElseIfPendingCounts) > 0 then
          begin
            for I := 0 to lElseIfPendingCounts[High(lElseIfPendingCounts)] - 1 do
              aTokens.Add(TToken.Create(lLastToken, '', ''));
            // Pop the stack
            SetLength(lElseIfPendingCounts, Length(lElseIfPendingCounts) - 1);
          end;

          // Emit the main ttEndIf
          aTokens.Add(TToken.Create(lLastToken, '', ''));

          Dec(lIfStatementCount);
          if Length(lBlockStack) > 0 then
            lBlockStack := Copy(lBlockStack, 1, Length(lBlockStack) - 1);  // Pop from block stack
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
            lBlockStack := lBlockStack + 'I';  // Push 'I' for for-else tracking
            // Push 0 onto elseif pending stack for this if-level
            SetLength(lElseIfPendingCounts, Length(lElseIfPendingCounts) + 1);
            lElseIfPendingCounts[High(lElseIfPendingCounts)] := 0;
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
            MatchSpace;
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
            lBlockStack := lBlockStack + 'I';  // Push 'I' for for-else tracking
            // Push 0 onto elseif pending stack for this if-level
            SetLength(lElseIfPendingCounts, Length(lElseIfPendingCounts) + 1);
            lElseIfPendingCounts[High(lElseIfPendingCounts)] := 0;
            lStartVerbatim := fCharIndex;

            lLastToken := ttBoolExpression;
            { Ref1 now stores number of filters (0 = no filter, >0 = filter count) }
            aTokens.Add(TToken.Create(lLastToken, lIdentifier, '', Length(lFilters), -1 { no html escape } ));

            // add filter tokens
            AddFilterTokens(aTokens, lFilters);
          end;
        end
        else if MatchSymbol('elseif') or MatchSymbol('elif') then
        begin
          // elseif/elif is syntactic sugar for {{else}}{{if condition}}
          // The implicit endif will be emitted when we see the main endif
          if lIfStatementCount < 0 then
            Error('"elseif" without "if"');

          // Emit ttElse token
          lLastToken := ttElse;
          aTokens.Add(TToken.Create(lLastToken, '', ''));

          // Increment pending elseif count for current if level
          Inc(lElseIfPendingCounts[High(lElseIfPendingCounts)]);

          if not MatchSpace then
            Error('Expected <space> after "elseif"');

          // Parse the condition (same as if)
          if MatchExpression(lIdentifier) then
          begin
            MatchSpace;
            if not MatchEndTag then
              Error('Expected closing tag for "elseif" after expression');

            // Emit ttIfThen for the nested if
            lLastToken := ttIfThen;
            aTokens.Add(TToken.Create(lLastToken, '', ''));
            lStartVerbatim := fCharIndex;

            // Use ttExpression for the condition
            lLastToken := ttBoolExpression;
            aTokens.Add(TToken.Create(lLastToken, lIdentifier, '', 0, 1 { 1 = expression mode }));
          end
          else
          begin
            // Variable-based condition
            lNegation := MatchSymbol('!');
            MatchSpace;
            if not MatchVariable(lIdentifier) then
              Error('Expected identifier after "elseif"');
            SetLength(lFilters, 0);
            MatchSpace;
            if MatchSymbol('|') then
            begin
              MatchFilters(lIdentifier, lFilters);
            end;
            MatchSpace;
            if not MatchEndTag then
              Error('Expected closing tag for "elseif" after "' + lIdentifier + '"');
            if lNegation then
            begin
              lIdentifier := '!' + lIdentifier;
            end;

            // Emit ttIfThen for the nested if
            lLastToken := ttIfThen;
            aTokens.Add(TToken.Create(lLastToken, '', ''));
            lStartVerbatim := fCharIndex;

            lLastToken := ttBoolExpression;
            aTokens.Add(TToken.Create(lLastToken, lIdentifier, '', Length(lFilters), -1 { no html escape }));

            // add filter tokens
            AddFilterTokens(aTokens, lFilters);
          end;
        end
        else if MatchSymbol('else') then
        begin
          if not MatchEndTag then
            Error('Expected closing tag for "else"');

          // Check if we're inside a for or if block
          if (Length(lBlockStack) > 0) and (lBlockStack[Length(lBlockStack)] = 'F') then
          begin
            // Inside a for block - this is a for-else
            lLastToken := ttForElse;
            aTokens.Add(TToken.Create(lLastToken, '', ''));
          end
          else
          begin
            // Inside an if block or no context - this is a regular else
            lLastToken := ttElse;
            aTokens.Add(TToken.Create(lLastToken, '', ''));
          end;
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('switch') then { switch }
        begin
          if not MatchSpace then
            Error('Expected <space> after "switch"');
          lStringValue := '';
          if MatchExpression(lIdentifier) then
            lStringValue := '@'
          else if not MatchVariable(lIdentifier) then
            Error('Expected variable or @(expression) after "switch"');
          SetLength(lFilters, 0);
          MatchSpace;
          if MatchSymbol('|') then
            MatchFilters(lIdentifier, lFilters);
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "switch"');
          lLastToken := ttSwitch;
          // Value2 = '@' for an expression, Ref1 = filter count, Ref2 = first case/default/endswitch
          aTokens.Add(TToken.Create(lLastToken, lIdentifier, lStringValue, Length(lFilters), -1));
          AddFilterTokens(aTokens, lFilters);
          lBlockStack := lBlockStack + 'S';
          lSwitchStates := lSwitchStates + [0];
          lSwitchBodyStarts := lSwitchBodyStarts + [aTokens.Count];
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('case') then { case }
        begin
          CloseSwitchBranch('case');
          if lSwitchStates[High(lSwitchStates)] = 2 then
            Error('"case" after "default"');
          lSwitchStates[High(lSwitchStates)] := 1;
          if not MatchSpace then
            Error('Expected <space> after "case"');
          lCaseValues := [];
          repeat
            MatchSpace;
            lCaseValue := Default(TFilterParameter);
            if MatchString(lStringValue) then
            begin
              lCaseValue.ParType := fptString;
              lCaseValue.ParStrText := lStringValue;
            end
            else if MatchExpression(lStringValue) then
            begin
              lCaseValue.ParType := fptExpression;
              lCaseValue.ParStrText := lStringValue;
            end
            else if CharInSet(CurrentChar, SignAndNumbers) then
              MatchFilterParamValue(lCaseValue)
            else if MatchVariable(lStringValue) then
            begin
              lCaseValue.ParType := fptVariable;
              lCaseValue.ParStrText := lStringValue;
            end
            else
              Error('Expected value after "case"');
            lCaseValues := lCaseValues + [lCaseValue];
            MatchSpace;
          until not MatchSymbol(',');
          if not MatchEndTag then
            Error('Expected closing tag for "case"');
          lLastToken := ttCase;
          // Ref1 = endswitch, Ref2 = next case/default/endswitch; the values follow as ttFilterParameter
          aTokens.Add(TToken.Create(lLastToken, '', '', -1, -1));
          for I := 0 to High(lCaseValues) do
            aTokens.Add(CreateFilterParameterToken(@lCaseValues[I]));
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('default') then { default }
        begin
          CloseSwitchBranch('default');
          if lSwitchStates[High(lSwitchStates)] = 2 then
            Error('Duplicated "default" in "switch"');
          lSwitchStates[High(lSwitchStates)] := 2;
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "default"');
          lLastToken := ttDefault;
          aTokens.Add(TToken.Create(lLastToken, '', '', -1, -1));
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('endswitch') then { endswitch }
        begin
          CloseSwitchBranch('endswitch');
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "endswitch"');
          SetLength(lSwitchStates, Length(lSwitchStates) - 1);
          SetLength(lSwitchBodyStarts, Length(lSwitchBodyStarts) - 1);
          lBlockStack := Copy(lBlockStack, 1, Length(lBlockStack) - 1);
          lLastToken := ttEndSwitch;
          aTokens.Add(TToken.Create(lLastToken, '', ''));
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('push') then { push "stack name" [once] }
        begin
          if not MatchSpace then
            Error('Expected <space> after "push"');
          if not MatchNameArgument(lIdentifier, lStringValue) then
            Error('Expected stack name after "push"');
          MatchSpace;
          lRef2 := IfThen(MatchSymbol('once'), 1, 0);
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "push"');
          lLastToken := ttPush;
          // Value2 = kind of Value1 ("s" string, "v" variable, "@" expression), Ref2 = 1 for "once"
          aTokens.Add(TToken.Create(lLastToken, lIdentifier, lStringValue, -1, lRef2));
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('endpush') then { endpush }
        begin
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "endpush"');
          lLastToken := ttEndPush;
          aTokens.Add(TToken.Create(lLastToken, '', ''));
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('stack') then { stack "stack name" }
        begin
          if not MatchSpace then
            Error('Expected <space> after "stack"');
          if not MatchNameArgument(lIdentifier, lStringValue) then
            Error('Expected stack name after "stack"');
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "stack"');
          lLastToken := ttStack;
          aTokens.Add(TToken.Create(lLastToken, lIdentifier, lStringValue));
          Inc(lContentOnThisLine); // it outputs, like a variable
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('import') then { import "library" as namespace }
        begin
          if not MatchSpace then
            Error('Expected <space> after "import"');
          // compile time: the path is a literal, like the one of a static include
          if not MatchString(lStringValue) then
            Error('Expected string after "import"');
          if not (MatchSpace and MatchSymbol('as') and MatchSpace) then
            Error('Expected "as" after the library name');
          if not MatchVariable(lIdentifier) or lIdentifier.Contains('.') then
            Error('Expected namespace after "as"');
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "import"');
          if (lBlockStack <> '') or (lMacroDepth > 0) then
            Error('"import" is allowed only at the top level of a template');
          if MatchStr(lIdentifier.ToLower, lImportedAliases) then
            Error('Namespace "' + lIdentifier + '" already imported');
          lImportedAliases := lImportedAliases + [lIdentifier.ToLower];
          ImportLibrary(lStringValue, lIdentifier, aTokens, aFileNameRefPath);
          lLastToken := ttComment; // like a comment: no output, a line with only imports is eaten
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
              // Resolve full path for nested includes
              if TDirectory.Exists(aFileNameRefPath) then
                lCurrentFileName := TPath.GetFullPath(TPath.Combine(aFileNameRefPath, lIncludeFileName))
              else
                lCurrentFileName := TPath.GetFullPath(TPath.Combine(TPath.GetDirectoryName(aFileNameRefPath), lIncludeFileName));
              // A file already being compiled up the chain would recurse forever
              for lIncludeChainFile in fIncludeChain do
                if SameText(lIncludeChainFile, lCurrentFileName) then
                  Error('Circular include detected: "' + lIncludeFileName + '"');
              // Load template (via callback or file system)
              try
                lTemplateSource := LoadTemplateSource(lIncludeFileName, lCurrentFileName);
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

              // Compile the included template (propagate coDisableEatLineBreaks if set)
              InternalCompileIncludedTemplate(lTemplateSource, aTokens, lCurrentFileName,
                [coIgnoreSysVersion, coParentTemplate] + (fOptions * [coDisableEatLineBreaks]));

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
          // Resolve full path for nested includes
          if TDirectory.Exists(aFileNameRefPath) then
            lCurrentFileName := TPath.GetFullPath(TPath.Combine(aFileNameRefPath, lStringValue))
          else
            lCurrentFileName := TPath.GetFullPath(TPath.Combine(TPath.GetDirectoryName(aFileNameRefPath), lStringValue));
          // Check for circular inheritance before reading file
          if fInheritanceChain.Contains(lCurrentFileName) then
            raise ETProCompilerException.Create('Circular template inheritance detected');
          fInheritanceChain.Add(lCurrentFileName);
          // Load template (via callback or file system)
          try
            lTemplateSource := LoadTemplateSource(lStringValue, lCurrentFileName);
          except
            on E: Exception do
            begin
              Error('Cannot read "' + lStringValue + '"');
            end;
          end;
          Inc(lContentOnThisLine);
          aTokens.Add(TToken.Create(ttInfo, STR_BEGIN_OF_LAYOUT, ''));
          // Propagate coDisableEatLineBreaks if set
          InternalCompileIncludedTemplate(lTemplateSource, aTokens, lCurrentFileName,
            [coIgnoreSysVersion] + (fOptions * [coDisableEatLineBreaks]));
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
          // Note: {{inherited}} does not increment lContentOnThisLine to maintain
          // backward compatibility with "eat linebreaks" behavior
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('macro') then { macro definition }
        begin
          if not MatchSpace then
            Error('Expected "space" after "macro"');
          if not MatchVariable(lIdentifier) then
            Error('Expected macro name after "macro"');
          // "ns.name" is reserved to the macros of the imported libraries
          if lIdentifier.Contains('.') then
            Error('Macro name "' + lIdentifier + '" cannot contain "."');
          Inc(lMacroDepth);

          // Parse macro parameters: macro name(param1, param2="default", param3=variable, ...)
          lFuncParams := GetMacroParameters(lMacroArgNames, lMacroArgFilters);
          for I := 0 to High(lMacroArgNames) do
            if lMacroArgNames[I].IsEmpty and (Length(lMacroArgFilters[I]) > 0) then
              Error(Format('Macro "%s": filters are allowed only on a default value', [lIdentifier]));
          for I := 1 to High(lMacroArgNames) do
            if lMacroArgNames[I].IsEmpty and not lMacroArgNames[I - 1].IsEmpty then
              Error(Format('Macro "%s": parameter "%s" without default after a parameter with default',
                [lIdentifier, lFuncParams[I].ParStrText]));
          for I := 0 to High(lFuncParams) do
            if SameText(lMacroArgNames[I], 'slots') or (lMacroArgNames[I].IsEmpty and SameText(lFuncParams[I].ParStrText, 'slots')) then
              Error('A macro parameter cannot be named "slots"');

          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "macro"');

          lLastToken := ttMacro;
          // Value1 = macro name, Ref1 = parameter count
          aTokens.Add(TToken.Create(lLastToken, lIdentifier, '', Length(lFuncParams), -1));

          // Add macro parameters as tokens: Value1 = name; with a default, Value2 = its type ordinal as a digit + its value
          lMacroParamTokens := [];
          for I := 0 to Length(lFuncParams) - 1 do
          begin
            lMacroParamToken := CreateFilterParameterToken(@lFuncParams[I]);
            if not lMacroArgNames[I].IsEmpty then
              lMacroParamToken := TToken.Create(ttFilterParameter, lMacroArgNames[I],
                Chr(Ord('0') + lMacroParamToken.Ref2) + lMacroParamToken.Value1, -1, Ord(fptVariable));
            lMacroParamTokens := lMacroParamTokens + [lMacroParamToken];
          end;
          AddMacroParameterTokens(aTokens, aTokens.Count - 1, lMacroParamTokens, lMacroArgFilters);

          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('endmacro') then { endmacro }
        begin
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "endmacro"');
          if lMacroDepth > 0 then
            Dec(lMacroDepth);
          lLastToken := ttEndMacro;
          aTokens.Add(TToken.Create(lLastToken, '', ''));
          lStartVerbatim := fCharIndex;
        end
        else if (CurrentChar = '>') or MatchSymbol('call') then // macro call: {{>macroname(args)}} or {{call macroname(args)}}...{{endcall}}
        begin
          lIsBlockCall := not MatchSymbol('>');
          if lIsBlockCall and not MatchSpace then
            Error('Expected macro name after "call"');
          MatchSpace;
          if not MatchVariable(lIdentifier) then
            Error('Expected macro name after "' + IfThen(lIsBlockCall, 'call', '>') + '"');

          // Parse call parameters
          lFuncParams := GetMacroParameters(lMacroArgNames, lMacroArgFilters);
          for I := 1 to High(lMacroArgNames) do
            if lMacroArgNames[I].IsEmpty and not lMacroArgNames[I - 1].IsEmpty then
              Error('Positional argument after named argument in call to macro "' + lIdentifier + '"');

          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for macro call');

          lLastToken := ttCallMacro;
          if lIsBlockCall then
          begin
            // the body (the slots) follows: the output is complete at "endcall"
            lBlockStack := lBlockStack + 'C';
            lFillNames := lFillNames + [''];
            lRef2 := -2; // ProcessJumps links it to its endcall
          end
          else
          begin
            Inc(lContentOnThisLine);
            lRef2 := -1;
          end;
          // Value1 = macro name, Ref1 = tokens of the parameters and their filters, Ref2 = endcall (-1 = no body)
          aTokens.Add(TToken.Create(lLastToken, lIdentifier, '', Length(lFuncParams), lRef2));

          // Add call parameters as tokens (Value2 = parameter name for a named argument)
          lMacroParamTokens := [];
          for I := 0 to Length(lFuncParams) - 1 do
          begin
            lMacroParamToken := CreateFilterParameterToken(@lFuncParams[I]);
            lMacroParamToken.Value2 := lMacroArgNames[I];
            lMacroParamTokens := lMacroParamTokens + [lMacroParamToken];
          end;
          AddMacroParameterTokens(aTokens, aTokens.Count - 1, lMacroParamTokens, lMacroArgFilters);

          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('endcall') then { endcall }
        begin
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "endcall"');
          if (lBlockStack <> '') and (lBlockStack[Length(lBlockStack)] = 'L') then
            Error('Unbalanced "fill" - expected "endfill"');
          if (lBlockStack = '') or (lBlockStack[Length(lBlockStack)] <> 'C') then
            Error('"endcall" without "call"');
          lBlockStack := Copy(lBlockStack, 1, Length(lBlockStack) - 1);
          SetLength(lFillNames, Length(lFillNames) - 1);
          lLastToken := ttEndCall;
          // Ref1 = its call, Ref2 = its last fill (set by ProcessJumps)
          aTokens.Add(TToken.Create(lLastToken, '', '', -1, -1));
          Inc(lContentOnThisLine); // the output of the call ends here, like after {{>macro()}}
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('fill') then { fill "slot name" }
        begin
          if not MatchSpace then
            Error('Expected <space> after "fill"');
          if not MatchNameArgument(lIdentifier, lStringValue) then
            Error('Expected slot name after "fill"');
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "fill"');
          if (lBlockStack = '') or (lBlockStack[Length(lBlockStack)] <> 'C') then
            Error('"fill" must be directly inside "call"');
          if lStringValue = 's' then
          begin
            if SameText(lIdentifier, 'default') then
              Error('The slot name "default" is reserved to the content outside "fill"');
            if ContainsText(lFillNames[High(lFillNames)], '|' + lIdentifier + '|') then
              Error('Duplicated fill "' + lIdentifier + '"');
            lFillNames[High(lFillNames)] := lFillNames[High(lFillNames)] + '|' + lIdentifier + '|';
          end;
          lBlockStack := lBlockStack + 'L';
          lLastToken := ttFill;
          // Value2 = kind of Value1 ("s", "v", "@"), Ref1 = previous fill of the same call, Ref2 = endfill
          aTokens.Add(TToken.Create(lLastToken, lIdentifier, lStringValue, -1, -1));
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('endfill') then { endfill }
        begin
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "endfill"');
          if (lBlockStack = '') or (lBlockStack[Length(lBlockStack)] <> 'L') then
            Error('"endfill" without "fill"');
          lBlockStack := Copy(lBlockStack, 1, Length(lBlockStack) - 1);
          lLastToken := ttEndFill;
          aTokens.Add(TToken.Create(lLastToken, '', ''));
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('slot') then // slot, slot "name", slot "name" + fallback + endslot
        begin
          MatchSpace;
          if MatchEndTag then
          begin
            lIdentifier := 'default';
            lStringValue := 's';
          end
          else
          begin
            if not MatchNameArgument(lIdentifier, lStringValue) then
              Error('Expected slot name after "slot"');
            MatchSpace;
            if not MatchEndTag then
              Error('Expected closing tag for "slot"');
          end;
          if lMacroDepth = 0 then
            Error('"slot" can be used only inside a macro');
          lLastToken := ttSlot;
          // Value2 = kind of Value1 ("s", "v", "@"), Ref2 = endslot when there is a fallback (set by ProcessJumps)
          aTokens.Add(TToken.Create(lLastToken, lIdentifier, lStringValue, -1, -1));
          Inc(lContentOnThisLine); // it outputs, like a variable
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('endslot') then { endslot: closes the nearest open slot }
        begin
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag for "endslot"');
          lLastToken := ttEndSlot;
          aTokens.Add(TToken.Create(lLastToken, '', ''));
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
        else if MatchSymbol('autoescape') then { autoescape true/false }
        begin
          MatchSpace;
          if MatchSymbol('true') then
          begin
            lLastToken := ttAutoescape;
            aTokens.Add(TToken.Create(lLastToken, 'true', ''));
          end
          else if MatchSymbol('false') then
          begin
            lLastToken := ttAutoescape;
            aTokens.Add(TToken.Create(lLastToken, 'false', ''));
          end
          else
            Error('Expected "true" or "false" after autoescape');
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag');
          lStartVerbatim := fCharIndex;
        end
        else if MatchSymbol('endautoescape') then { endautoescape }
        begin
          MatchSpace;
          if not MatchEndTag then
            Error('Expected closing tag');
          lLastToken := ttEndAutoescape;
          aTokens.Add(TToken.Create(lLastToken, '', ''));
          lStartVerbatim := fCharIndex;
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
  lForElseStack: TStack<TForElseIndex>;
  lForElseItem: TForElseIndex;
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
  lSwitchStack: TStack<TIfThenElseIndex>;
  lSwitchItem: TIfThenElseIndex;
  lOpenPushCount: Integer;
  lCallStack: TStack<TIfThenElseIndex>; // IfIndex = the call, ElseIndex = its last fill so far
  lCallItem: TIfThenElseIndex;
  lFillStack: TStack<Int64>;
  lOpenSlots: TList<Int64>;
begin
  lCurrentLevel := 0; // Start at page level
  lOpenPushCount := 0;
  lCheckForUnbalancedPair := True;
  lBlockDict := TObjectDictionary<string, TList<TBlockAddress>>.Create([doOwnsValues], TTProEqualityComparer.Create);
  // IfIndex = the switch, ElseIndex = its last case/default seen so far
  lSwitchStack := TStack<TIfThenElseIndex>.Create;
  lCallStack := TStack<TIfThenElseIndex>.Create;
  lFillStack := TStack<Int64>.Create;
  lOpenSlots := TList<Int64>.Create;
  try
    lBlockStack := TStack<string>.Create;
    try
      lForInStack := TStack<Int64>.Create;
      try
        lContinueStack := TStack<Int64>.Create;
        try
          lIfStatementStack := TStack<TIfThenElseIndex>.Create;
          try
            lForElseStack := TStack<TForElseIndex>.Create;
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
                    // Initialize for-else tracking
                    lForElseItem.ForIndex := I;
                    lForElseItem.ElseIndex := -1;  // -1 means no else
                    lForElseStack.Push(lForElseItem);
                  end;

                ttEndFor:
                  begin
                    { ttFor.Ref1 --> endfor }
                    lForAddress := lForInStack.Pop;
                    lToken := aTokens[lForAddress];
                    lToken.Ref1 := I;
                    
                    { Handle for-else linking }
                    lForElseItem := lForElseStack.Pop;
                    { Encode isFieldIteration (bit 0) and elseAddress (bits 1+) in Ref2 }
                    { elseAddress + 1 is stored, so 0 means no else (-1 + 1 = 0) }
                    lToken.Ref2 := ((lForElseItem.ElseIndex + 1) shl 1) or (lToken.Ref2 and 1);
                    aTokens[lForAddress] := lToken;

                    { If there's an else, set ttForElse.Ref2 --> endfor }
                    if lForElseItem.ElseIndex > -1 then
                    begin
                      lToken := aTokens[lForElseItem.ElseIndex];
                      lToken.Ref2 := I;  // ttForElse.Ref2 points to endfor
                      aTokens[lForElseItem.ElseIndex] := lToken;
                    end;

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

                ttForElse:
                  begin
                    // Update the for-else tracking with the else index
                    lForElseItem := lForElseStack.Pop;
                    lForElseItem.ElseIndex := I;
                    lForElseStack.Push(lForElseItem);
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
                    lOpenSlots.Clear; // a slot without endslot has no fallback
                  end;

                { call with a body: ttCallMacro.Ref2 --> endcall; ttEndCall.Ref1 --> call, ttEndCall.Ref2 --> last fill }
                { ttFill.Ref1 --> previous fill of the same call (-1 = none), ttFill.Ref2 --> endfill }
                ttCallMacro:
                  if aTokens[I].Ref2 = -2 then
                  begin
                    lCallItem.IfIndex := I;
                    lCallItem.ElseIndex := -1;
                    lCallStack.Push(lCallItem);
                  end;

                ttFill:
                  begin
                    lCallItem := lCallStack.Pop;
                    lToken := aTokens[I];
                    lToken.Ref1 := lCallItem.ElseIndex;
                    aTokens[I] := lToken;
                    lCallItem.ElseIndex := I;
                    lCallStack.Push(lCallItem);
                    lFillStack.Push(I);
                  end;

                ttEndFill:
                  begin
                    J := lFillStack.Pop;
                    lToken := aTokens[J];
                    lToken.Ref2 := I;
                    aTokens[J] := lToken;
                  end;

                ttEndCall:
                  begin
                    lCallItem := lCallStack.Pop;
                    lToken := aTokens[lCallItem.IfIndex];
                    lToken.Ref2 := I;
                    aTokens[lCallItem.IfIndex] := lToken;
                    lToken := aTokens[I];
                    lToken.Ref1 := lCallItem.IfIndex;
                    lToken.Ref2 := lCallItem.ElseIndex;
                    aTokens[I] := lToken;
                  end;

                { ttSlot.Ref2 --> the endslot closing its fallback (-1 = no fallback): endslot closes the nearest open slot }
                ttSlot:
                  lOpenSlots.Add(I);

                ttEndSlot:
                  begin
                    if lOpenSlots.Count = 0 then
                      Error('"endslot" without "slot"');
                    J := lOpenSlots.Last;
                    lOpenSlots.Delete(lOpenSlots.Count - 1);
                    lToken := aTokens[J];
                    lToken.Ref2 := I;
                    aTokens[J] := lToken;
                  end;

                ttExit:
                  begin
                    lCheckForUnbalancedPair := False;
                  end;

                { ttSwitch.Ref2 and every ttCase/ttDefault.Ref2 --> next branch (the last one --> endswitch) }
                { ttCase.Ref1 and ttDefault.Ref1 --> endswitch }
                ttPush:
                  Inc(lOpenPushCount);
                ttEndPush:
                  begin
                    Dec(lOpenPushCount);
                    if lOpenPushCount < 0 then
                      Error('"endpush" without "push"');
                  end;

                ttSwitch:
                  begin
                    lSwitchItem.IfIndex := I;
                    lSwitchItem.ElseIndex := I;
                    lSwitchStack.Push(lSwitchItem);
                  end;

                ttCase, ttDefault, ttEndSwitch:
                  begin
                    lSwitchItem := lSwitchStack.Pop;
                    lToken := aTokens[lSwitchItem.ElseIndex];
                    lToken.Ref2 := I;
                    aTokens[lSwitchItem.ElseIndex] := lToken;
                    if aTokens[I].TokenType = ttEndSwitch then
                    begin
                      J := aTokens[lSwitchItem.IfIndex].Ref2;
                      while J <> I do
                      begin
                        lToken := aTokens[J];
                        lToken.Ref1 := I;
                        aTokens[J] := lToken;
                        J := lToken.Ref2;
                      end;
                    end
                    else
                    begin
                      lSwitchItem.ElseIndex := I;
                      lSwitchStack.Push(lSwitchItem);
                    end;
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
            if lCheckForUnbalancedPair and (lSwitchStack.Count > 0) then
              Error('Unbalanced "switch" - expected "endswitch"');
            if lCheckForUnbalancedPair and (lOpenPushCount > 0) then
              Error('Unbalanced "push" - expected "endpush"');
            if lCheckForUnbalancedPair and (lFillStack.Count > 0) then
              Error('Unbalanced "fill" - expected "endfill"');
            if lCheckForUnbalancedPair and (lCallStack.Count > 0) then
              Error('Unbalanced "call" - expected "endcall"');
            finally
              lForElseStack.Free;
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
    lOpenSlots.Free;
    lFillStack.Free;
    lCallStack.Free;
    lSwitchStack.Free;
    lBlockDict.Free;
  end;
  // TTProCompiledTemplate.InternalDumpToFile('debug.compiled.txt', aTokens);
end;

function TTProCompiler.GetFunctionParameters(const aStopAtNamedArg: Boolean): TArray<TFilterParameter>;
// aStopAtNamedArg: in a macro call ", name=" starts the next argument, it is not another filter parameter
var
  lFuncPar: TFilterParameter;
  lCommaIndex: Integer;
begin
  Result := [];
  while True do
  begin
    lCommaIndex := fCharIndex;
    if not MatchSymbol(',') then
      Break;
    MatchSpace;
    if not MatchFilterParamValue(lFuncPar) then
      Error('Expected function parameter');
    MatchSpace;
    if aStopAtNamedArg and (CurrentChar = '=') then
    begin
      fCharIndex := lCommaIndex;
      Break;
    end;
    Result := Result + [lFuncPar];
  end;
end;

function TTProCompiler.GetMacroParameters(out aNames: TArray<string>; out aFilters: TArray<TArray<TFilterInfo>>): TArray<TFilterParameter>;
// "(v1, name = v2|filter, ...)": aNames[I] is the name before "=" ('' when there isn't one), Result[I] the value,
// aFilters[I] the filters applied to it
var
  lFuncPar: TFilterParameter;
  lName: string;
  lFilters: TArray<TFilterInfo>;
begin
  Result := [];
  aNames := [];
  aFilters := [];
  MatchSpace;
  if not MatchSymbol('(') then
    Exit; // No parameters

  MatchSpace;
  // Check for empty parameter list ()
  if MatchSymbol(')') then
    Exit;

  repeat
    MatchSpace;
    if not MatchFilterParamValue(lFuncPar) then
      Error('Expected macro parameter');
    MatchSpace;
    lName := '';
    if MatchSymbol('=') then
    begin
      if lFuncPar.ParType <> fptVariable then
        Error('Expected a name before "="');
      lName := lFuncPar.ParStrText;
      MatchSpace;
      if not MatchFilterParamValue(lFuncPar) then
        Error('Expected value after "' + lName + '="');
      MatchSpace;
    end;
    lFilters := [];
    if MatchSymbol('|') then
      MatchFilters(lFuncPar.ParStrText, lFilters, True);
    Result := Result + [lFuncPar];
    aNames := aNames + [lName];
    aFilters := aFilters + [lFilters];
  until not MatchSymbol(',');

  if not MatchSymbol(')') then
    Error('Expected ")" after macro parameters');
end;

procedure TTProCompiler.AddMacroParameterTokens(aTokens: TList<TToken>; const aMacroTokenIdx: Integer;
  const aParamTokens: TArray<TToken>; const aFilters: TArray<TArray<TFilterInfo>>);
// each parameter token is followed by its filters (parameter Ref1 = filter count); the macro/call token
// Ref1 becomes the number of tokens that follow it (without filters: the parameter count, as before 1.2)
var
  I: Integer;
  lToken: TToken;
begin
  for I := 0 to High(aParamTokens) do
  begin
    lToken := aParamTokens[I];
    if Length(aFilters[I]) > 0 then
      lToken.Ref1 := Length(aFilters[I]);
    aTokens.Add(lToken);
    AddFilterTokens(aTokens, aFilters[I]);
  end;
  lToken := aTokens[aMacroTokenIdx];
  lToken.Ref1 := aTokens.Count - aMacroTokenIdx - 1;
  aTokens[aMacroTokenIdx] := lToken;
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

  function GetStringInput: string;
  begin
    if aExecuteAsFilterOnAValue then
    begin
      CheckParNumber(0, aParameters);
      if aValue.IsEmpty then
        Result := ''
      else
        Result := aValue.AsString;
    end
    else
    begin
      CheckParNumber(1, aParameters);
      Result := aParameters[0].ParStrText;
    end;
  end;

begin
  Result := True;
  if SameText(aFunctionName, 'uppercase') then
    aResult := UpperCase(GetStringInput)
  else if SameText(aFunctionName, 'lowercase') then
    aResult := lowercase(GetStringInput)
  else if SameText(aFunctionName, 'capitalize') then
    aResult := CapitalizeString(GetStringInput, True)
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
    else if aValue.IsType<Int64> then
      lStrValue := aValue.AsInt64.ToString
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
    else if aValue.IsType<Int64> then
      lStrValue := aValue.AsInt64.ToString
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
        aResult := lStrValue.PadLeft(lVarValue.AsInt64);
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
    aResult := ValueAsString(aValue).Contains(lStrValue);
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
    aResult := ValueAsString(aValue).ToLowerInvariant.Contains(lStrValue.ToLowerInvariant);
  end
  else if SameText(aFunctionName, 'urlencode') then
  begin
    CheckParNumber(0, 0, aParameters);
    aResult := TNetEncoding.URL.Encode(GetStringInput);
  end
  else if SameText(aFunctionName, 'truncate') then
  begin
    // truncate,maxlen[,"ellipsis"]  - default ellipsis is "..."
    CheckParNumber(1, 2, aParameters);
    lStrValue := aValue.AsString;
    lIntegerPar1 := aParameters[0].ParIntValue;
    if Length(lStrValue) > lIntegerPar1 then
    begin
      if Length(aParameters) > 1 then
        aResult := lStrValue.Substring(0, lIntegerPar1) + aParameters[1].ParStrText
      else
        aResult := lStrValue.Substring(0, lIntegerPar1) + '...';
    end
    else
      aResult := lStrValue;
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
  lNullableDateTime: NullableTDateTime;
  lSQLTimestampOffset: TSQLTimeStampOffset;
  lIsNull: Boolean;
begin
  Result := True;
  if SameText(aFunctionName, 'datetostr') then
  begin
    if aValue.IsEmpty then
      aResult := ''
    else if aValue.IsObject and (aValue.AsObject <> nil) and (aValue.AsObject is TField) then
    begin
      // Handle TField passed from macro or iteration
      if TField(aValue.AsObject).IsNull then
        aResult := ''
      else
      begin
        lDateValue := TField(aValue.AsObject).AsDateTime;
        if Length(aParameters) = 0 then
          aResult := FormatDateTime('yyyy-mm-dd', lDateValue)
        else
        begin
          CheckParNumber(1, aParameters);
          aResult := FormatDateTime(aParameters[0].ParStrText, lDateValue);
        end;
      end;
    end
    else if aValue.TryAsType<TDateTime>(lDateValue) then
    begin
      if Length(aParameters) = 0 then
        aResult := FormatDateTime('yyyy-mm-dd', lDateValue)  // ISO 8601 default
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
          aResult := FormatDateTime('yyyy-mm-dd', lDateValue)  // ISO 8601 default
        else
        begin
          CheckParNumber(1, aParameters);
          aResult := FormatDateTime(aParameters[0].ParStrText, lDateValue);
        end;
      end;
    end
    else if aValue.TypeInfo = TypeInfo(NullableTDateTime) then
    begin
      lNullableDateTime := aValue.AsType<NullableTDateTime>(True);
      if lNullableDateTime.IsNull then
        aResult := ''
      else
      begin
        lDateValue := lNullableDateTime.Value;
        if Length(aParameters) = 0 then
          aResult := FormatDateTime('yyyy-mm-dd', lDateValue)  // ISO 8601 default
        else
        begin
          CheckParNumber(1, aParameters);
          aResult := FormatDateTime(aParameters[0].ParStrText, lDateValue);
        end;
      end;
    end
    else if aValue.TypeInfo.Kind in [tkString, tkUString, tkLString, tkWString] then
    begin
      // Handle empty string - return empty result
      if aValue.AsString.IsEmpty then
        aResult := ''
      else
      begin
        // Try to parse ISO 8601 date string
        try
          lDateValue := ISO8601ToDate(aValue.AsString, False);
          if Length(aParameters) = 0 then
            aResult := FormatDateTime('yyyy-mm-dd', lDateValue)  // ISO 8601 default
          else
          begin
            CheckParNumber(1, aParameters);
            aResult := FormatDateTime(aParameters[0].ParStrText, lDateValue);
          end;
        except
          on E: Exception do
            FunctionError(aFunctionName, 'Invalid date string ' + aValue.AsString.QuotedString + ' - ' + E.Message);
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
    else if aValue.IsObject and (aValue.AsObject <> nil) and (aValue.AsObject is TField) then
    begin
      // Handle TField passed from macro or iteration
      if TField(aValue.AsObject).IsNull then
        aResult := ''
      else
      begin
        lDateValue := TField(aValue.AsObject).AsDateTime;
        if Length(aParameters) = 0 then
          aResult := FormatDateTime('yyyy-mm-dd hh:nn:ss', lDateValue)
        else
        begin
          CheckParNumber(1, aParameters);
          aResult := FormatDateTime(aParameters[0].ParStrText, lDateValue);
        end;
      end;
    end
    else if aValue.TryAsType<TDateTime>(lDateValue) then
    begin
      if Length(aParameters) = 0 then
        aResult := FormatDateTime('yyyy-mm-dd hh:nn:ss', lDateValue)  // ISO 8601 default
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
        aResult := FormatDateTime('yyyy-mm-dd hh:nn:ss', lDateValue)  // ISO 8601 default
      else
      begin
        CheckParNumber(1, aParameters);
        aResult := FormatDateTime(aParameters[0].ParStrText, lDateValue);
      end;
    end
    else if aValue.TypeInfo = TypeInfo(NullableTDateTime) then
    begin
      lNullableDateTime := aValue.AsType<NullableTDateTime>(True);
      if lNullableDateTime.IsNull then
        aResult := ''
      else
      begin
        lDateValue := lNullableDateTime.Value;
        if Length(aParameters) = 0 then
          aResult := FormatDateTime('yyyy-mm-dd hh:nn:ss', lDateValue)  // ISO 8601 default
        else
        begin
          CheckParNumber(1, aParameters);
          aResult := FormatDateTime(aParameters[0].ParStrText, lDateValue);
        end;
      end;
    end
    else if aValue.TypeInfo.Kind in [tkString, tkUString, tkLString, tkWString] then
    begin
      // Handle empty string - return empty result
      if aValue.AsString.IsEmpty then
        aResult := ''
      else
      begin
        // Try to parse ISO 8601 datetime string
        try
          lDateValue := ISO8601ToDate(aValue.AsString, False);
          if Length(aParameters) = 0 then
            aResult := FormatDateTime('yyyy-mm-dd hh:nn:ss', lDateValue)  // ISO 8601 default
          else
          begin
            CheckParNumber(1, aParameters);
            aResult := FormatDateTime(aParameters[0].ParStrText, lDateValue);
          end;
        except
          on E: Exception do
            FunctionError(aFunctionName, 'Invalid datetime string ' + aValue.AsString.QuotedString + ' - ' + E.Message);
        end;
      end;
    end
    else
      FunctionError(aFunctionName, 'Invalid datetime ' + aValue.AsString.QuotedString);
  end
  else
    Result := False;
end;

function TTProCompiledTemplate.ExecuteFilter(aFunctionName: string; var aParameters: TArray<TFilterParameter>; aValue: TValue;
  const aVarNameWhereShoudBeApplied: String; out aIsCustomFilter: Boolean): TValue;
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
  aIsCustomFilter := False;
  lExecuteAsFilterOnAValue := not aVarNameWhereShoudBeApplied.IsEmpty;
  aFunctionName := lowercase(aFunctionName);

  // Normalize TField to its actual value before applying filters
  // This ensures consistent behavior: TField.IsNull -> Empty, otherwise -> actual value
  if aValue.IsObject and (aValue.AsObject <> nil) and (aValue.AsObject is TField) then
  begin
    if TField(aValue.AsObject).IsNull then
      aValue := TValue.Empty
    else
      aValue := GetDataSetFieldAsTValue(
        TField(aValue.AsObject).DataSet,
        TField(aValue.AsObject).FieldName);
  end;

  // A custom filter registered with the name of a built-in replaces the built-in
  if fTemplateFunctions.TryGetValue(aFunctionName, lFunc) then
  begin
    aIsCustomFilter := True;
    Exit(lFunc(aValue, aParameters));
  end;
  if (fTemplateAnonFunctions <> nil) and fTemplateAnonFunctions.TryGetValue(aFunctionName, lAnonFunc) then
  begin
    aIsCustomFilter := True;
    Exit(lAnonFunc(aValue, aParameters));
  end;

  // Try string filters first
  if ExecuteStringFilter(aFunctionName, aParameters, aValue, lExecuteAsFilterOnAValue, Result) then
    Exit;

  // Try date filters
  if ExecuteDateFilter(aFunctionName, aParameters, aValue, aVarNameWhereShoudBeApplied, Result) then
    Exit;

  if ExecuteListAndTextFilter(aFunctionName, aParameters, aValue, Result) then
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
    if aValue.IsEmpty then
    begin
      Result := '';
    end
    else
    begin
      lDecimalMask := '';

      if aParameters[0].ParType = fptVariable then
      begin
        lVarValue := GetVarAsTValue(aParameters[0].ParStrText);
        lIntegerPar1 := lVarValue.AsInt64;
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
      Result := FormatFloat('0' + lDecimalMask, lExtendedValue, fLocaleFormatSettings);
    end;
  end
  else if SameText(aFunctionName, 'formatfloat') then
  begin
    CheckParNumber(1, aParameters);
    CheckParamType('formatfloat', @aParameters[0], [TFilterParameterType.fptString]);
    if aValue.IsEmpty then
    begin
      Result := '';
    end
    else if aValue.IsType<Integer> then
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
      Error('FormatFloat cannot format data of type: ' + String(aValue.TypeInfo.Name));
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
  else if SameText(aFunctionName, 'json') then
  begin
    // Serialize value to JSON string
    CheckParNumber(0, 0, aParameters);
    if aValue.IsObject then
    begin
      if aValue.AsObject is TJDOJsonObject then
        Result := TJDOJsonObject(aValue.AsObject).ToJSON
      else if aValue.AsObject is TJDOJsonArray then
        Result := TJDOJsonArray(aValue.AsObject).ToJSON
      else
        Result := TTProRTTIUtils.ObjectToJSONString(aValue.AsObject);
    end
    else if aValue.IsEmpty then
      Result := 'null'
    else
      Result := aValue.ToString;
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
  lSB: TStringBuilder;
begin
  lSB := TStringBuilder.Create(Length(s));
  try
    I := 1;
    while I <= Length(s) do
    begin
      r := '';
      if (Char.IsHighSurrogate(S, I-1)) and (Char.IsLowSurrogate(S, I)) then
      begin
        lSB.Append('&#').Append(Char.ConvertToUtf32(S, I-1)).Append(';');
        Inc(I, 2);
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
        lSB.Append('&').Append(r).Append(';')
      else
        lSB.Append(s[I]);
      Inc(I);
    end;
    Result := lSB.ToString;
  finally
    lSB.Free;
  end;
end;

{ TTProCompiledTemplate }

constructor TTProCompiledTemplate.Create(Tokens: TList<TToken>);
var
  I: Integer;
begin
  inherited Create;
  fLoopsStack := TObjectList<TLoopStackItem>.Create(True);
  fIncludeSavedVarsStack := TObjectList<TIncludeSavedVars>.Create(True);
  fAutoescapeStack := TStack<Boolean>.Create;
  fAutoescapeStack.Push(True); // Default: autoescape enabled
  fTokens := Tokens;
  fTemplateFunctions := TDictionary<string, TTProTemplateFunction>.Create(TTProEqualityComparer.Create);
  fTemplateAnonFunctions := nil;
  fMacros := TDictionary<string, TMacroDefinition>.Create(TTProEqualityComparer.Create);
  TTProConfiguration.RegisterHandlers(self);
  // the macros of the imported libraries are known from the start, wherever their definitions are
  for I := 0 to fTokens.Count - 1 do
    if (fTokens[I].TokenType = ttMacro) and fTokens[I].Value1.Contains('.') then
      RegisterMacro(I);
  fLocaleFormatSettings := TFormatSettings.Invariant;
  fLocaleFormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  fEncoding := TEncoding.UTF8;
  fOutputLineEnding := lesLF;
  fDynamicIncludeCache := TDictionary<string, ITProCompiledTemplate>.Create;
  fOwnedObjects := TObjectList<TObject>.Create(True);
  fSlotFrames := TList<TTProSlotFrame>.Create;
  fCurrentSlotFrame := -1;
end;

function LoadCompiledTemplate(const aStream: TStream; const aSource: string): ITProCompiledTemplate;
var
  lBR: TBinaryReader;
  lTokens: TList<TToken>;
begin
  lBR := TBinaryReader.Create(aStream, nil, False); // False = don't own stream
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
            ('Cannot load compiled template from [%s][CLASS: %s][MSG: %s] - consider to delete templates cache.',
            [aSource, E.ClassName, E.Message])
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

class function TTProCompiledTemplate.CreateFromFile(const FileName: String): ITProCompiledTemplate;
var
  lBufferedStream: TBufferedFileStream;
begin
  // Use TBufferedFileStream for ~50% faster loading compared to TFile.ReadAllBytes + TBytesStream
  lBufferedStream := TBufferedFileStream.Create(FileName, fmOpenRead or fmShareDenyNone, 65536);
  try
    Result := LoadCompiledTemplate(lBufferedStream, 'FILE: ' + FileName);
  finally
    lBufferedStream.Free;
  end;
end;

class function TTProCompiledTemplate.CreateFromBytes(const aBytes: TBytes): ITProCompiledTemplate;
var
  lStream: TBytesStream;
begin
  lStream := TBytesStream.Create(aBytes);
  try
    Result := LoadCompiledTemplate(lStream, 'BYTES');
  finally
    lStream.Free;
  end;
end;

destructor TTProCompiledTemplate.Destroy;
begin
  fOnGetValue := nil;
  fExprEvaluator.Free;
  fDynamicIncludeCache.Free;
  fOwnedObjects.Free;
  fSlotFrames.Free;
  fLoopsStack.Free;
  fIncludeSavedVarsStack.Free;
  fAutoescapeStack.Free;
  fTemplateFunctions.Free;
  fTemplateAnonFunctions.Free;
  fMacros.Free;
  fTokens.Free;
  fVariables.Free;
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

procedure TTProCompiledTemplate.RenderTo(const aBuff: TStringBuilder; const aStacks: TTProStacks);
begin
  ResetRenderState;
  fStacks := aStacks;
  try
    RenderRange(aBuff, 0, fTokens.Count);
  finally
    ReleaseOwnedObjects;
    fStacks := nil;
  end;
end;

procedure TTProCompiledTemplate.RenderRange(const aBuff: TStringBuilder; const aFrom, aTo: Int64);
// renders the tokens from aFrom to aTo (excluded): the whole template, or the content of a slot
var
  lIdx: Int64;
  lValueOwned: Boolean;
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
  lForElseAddress: Int64;  // Address of else block in for-else construct
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
  lDynHandled: Boolean;
  // Variables for JSON array path parsing
  lPathIndex: Integer;
  lPathRemainder: String;
begin
  lBuff := aBuff;
  lCurrentLevel := 0;
  lBlockStack := TStack<TBlockReturnInfo>.Create;
  try
    lIdx := aFrom;
    while (lIdx < aTo) and (fTokens[lIdx].TokenType <> ttEOF) do
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
            // Decode isFieldIteration (bit 0) and elseAddress (bits 1+) from Ref2
            lIsFieldIteration := (fTokens[lIdx].Ref2 and 1) = 1;
            lForElseAddress := (fTokens[lIdx].Ref2 shr 1) - 1;  // -1 means no else
            // nested ranges often share the expression, e.g. range(3) inside range(3): the iterator tells them apart
            if LoopStackIsEmpty or (lForLoopItem.LoopExpression <> fTokens[lIdx].Value1) or
              (lForLoopItem.IsRange and (lForLoopItem.IteratorName <> fTokens[lIdx].Value2)) then
            begin // push a new loop stack item
              SplitVariableName(fTokens[lIdx].Value1, lVarName, lVarMember);
              if fTokens[lIdx].Value1.StartsWith(RANGE_PREFIX) then
              begin
                lForLoopItem := TLoopStackItem.Create('', fTokens[lIdx].Value1, '', fTokens[lIdx].Value2);
                lForLoopItem.IsRange := True;
                PushLoop(lForLoopItem);
              end
              else if lIsFieldIteration and IsObjectForFields(fTokens[lIdx].Value1, lObj) then
              begin
                // the properties of an object: a metadata list in a hidden variable, iterated as any list
                lVarName := '@@fields' + lIdx.ToString;
                lForLoopItem := TLoopStackItem.Create(lVarName, fTokens[lIdx].Value1, '', fTokens[lIdx].Value2);
                lForLoopItem.OwnedData := ObjectFieldsMetadata(lObj.AsObject);
                PushLoop(lForLoopItem);
                SetData(lVarName, lForLoopItem.OwnedData);
              end
              else if WalkThroughLoopStack(lVarName, lBaseVarName, lFullPath) then
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
            if lForLoopItem.IsRange then
            begin
              if lForLoopItem.IteratorPosition = -1 then
                InitRangeLoop(lForLoopItem);
              if lForLoopItem.IteratorPosition >= lForLoopItem.TotalCount - 1 then
              begin
                lForLoopItem.EOF := True;
                if (lForLoopItem.IteratorPosition = -1) and (lForElseAddress > -1) then
                  lIdx := lForElseAddress + 1  // empty range: jump to else content
                else
                  lIdx := fTokens[lIdx].Ref1;  // skip to endfor
                Continue;
              end;
              lForLoopItem.IncrementIteratorPosition;
            end
            else if GetVariables.TryGetValue(PeekLoop.DataSourceName, lVariable) and (lVariable = nil) then
            begin
              // a variable set to an empty value (e.g. a macro parameter): nothing to iterate
              lForLoopItem.EOF := True;
              if lForElseAddress > -1 then
                lIdx := lForElseAddress + 1
              else
                lIdx := fTokens[lIdx].Ref1;
              Continue;
            end
            else if GetVariables.TryGetValue(PeekLoop.DataSourceName, lVariable) then
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
                    // Check if empty from start (first iteration, no fields)
                    if (lForLoopItem.IteratorPosition = 0) and (lForElseAddress > -1) then
                      lIdx := lForElseAddress + 1  // jump to else content
                    else
                      lIdx := fTokens[lIdx].Ref1;  // skip to endfor
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
                    // Check if empty from start (first iteration, Eof immediately after First)
                    if (lForLoopItem.IteratorPosition = 0) and (lForElseAddress > -1) then
                      lIdx := lForElseAddress + 1  // jump to else content
                    else
                      lIdx := fTokens[lIdx].Ref1;  // skip to endfor
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
                if lCount = 0 then
                begin
                  // Collection is empty from start - execute else if present
                  lForLoopItem.EOF := True;
                  if lForElseAddress > -1 then
                    lIdx := lForElseAddress + 1  // jump to else content
                  else
                    lIdx := fTokens[lIdx].Ref1;  // skip to endfor
                  Continue;
                end
                else if lForLoopItem.IteratorPosition = lCount - 1 then
                begin
                  // Exhausted all items - skip else, go to endfor
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
                      // Path doesn't exist - treat as empty, execute else if present
                      lForLoopItem.EOF := True;
                      if lForElseAddress > -1 then
                        lIdx := lForElseAddress + 1  // jump to else content
                      else
                        lIdx := fTokens[lIdx].Ref1;  // skip to endfor
                      Continue;
                    end;

                  jdtArray:
                    begin
                      lCount := lJObj.Path[lForLoopItem.FullPath].ArrayValue.Count;
                      if lForLoopItem.IteratorPosition = -1 then
                        lForLoopItem.TotalCount := lCount;
                      if lCount = 0 then
                      begin
                        // Array is empty from start - execute else if present
                        lForLoopItem.EOF := True;
                        if lForElseAddress > -1 then
                          lIdx := lForElseAddress + 1  // jump to else content
                        else
                          lIdx := fTokens[lIdx].Ref1;  // skip to endfor
                        Continue;
                      end
                      else if lForLoopItem.IteratorPosition = lCount - 1 then
                      begin
                        // Exhausted all items - skip else, go to endfor
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
              else if viJSONArray in lVariable.VarOption then
              begin
                lForLoopItem := PeekLoop;
                lCount := 0; // Initialize to avoid compiler warning (Error() raises exception)
                if lForLoopItem.FullPath.IsEmpty then
                begin
                  // Direct iteration over the JSON array
                  lCount := TJDOJsonArray(lVariable.VarValue.AsObject).Count;
                end
                else
                begin
                  // Nested iteration: path like [0].devices
                  // Parse path to get index and remaining path
                  ParseJSONArrayPath(lForLoopItem.FullPath, lPathIndex, lPathRemainder);
                  if (lPathIndex >= 0) and (lPathIndex < TJDOJsonArray(lVariable.VarValue.AsObject).Count) then
                  begin
                    if lPathRemainder.IsEmpty then
                      lJValue := TJDOJsonArray(lVariable.VarValue.AsObject)[lPathIndex]
                    else
                      lJValue := TJDOJsonArray(lVariable.VarValue.AsObject)[lPathIndex].ObjectValue.Path[lPathRemainder];
                    if lJValue.Typ = jdtArray then
                      lCount := lJValue.ArrayValue.Count
                    else if lJValue.Typ = jdtNone then
                      lCount := 0
                    else
                      Error('Cannot iterate over non-array property in JSONArray path: ' + lForLoopItem.FullPath);
                  end
                  else
                    lCount := 0;
                end;
                if lForLoopItem.IteratorPosition = -1 then
                  lForLoopItem.TotalCount := lCount;
                if lCount = 0 then
                begin
                  // Array is empty from start - execute else if present
                  lForLoopItem.EOF := True;
                  if lForElseAddress > -1 then
                    lIdx := lForElseAddress + 1  // jump to else content
                  else
                    lIdx := fTokens[lIdx].Ref1;  // skip to endfor
                  Continue;
                end
                else if lForLoopItem.IteratorPosition = lCount - 1 then
                begin
                  // Exhausted all items - skip else, go to endfor
                  lForLoopItem.EOF := True;
                  lIdx := fTokens[lIdx].Ref1; // skip to endfor
                  Continue;
                end
                else
                begin
                  lForLoopItem.IncrementIteratorPosition;
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
              if lForLoopItem.OwnedData <> nil then
                GetVariables.Remove(lForLoopItem.DataSourceName); // the hidden variable of @@fields
              PopLoop;
            end
            else
            begin
              lIdx := fTokens[lIdx].Ref1; // goto loop
              Continue;
            end;
          end;
        ttForElse:
          begin
            // During normal loop execution, skip the else block and jump to endfor
            lIdx := fTokens[lIdx].Ref2;  // ttForElse.Ref2 points to endfor
            Continue;
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
        ttEndIf, ttStartTag, ttEndTag, ttEndSwitch, ttEndCall, ttEndFill, ttEndSlot, ttDependency:
          begin
          end;
        ttFill:
          lIdx := fTokens[lIdx].Ref2; // a named slot is not part of the default one: skip to its endfill
        ttSlot:
          lIdx := RenderSlot(lIdx, lBuff);
        ttSwitch:
          begin
            lIdx := SelectSwitchBranch(lIdx);
            Continue;
          end;
        ttCase, ttDefault:
          begin
            // the previous branch is over: no fall-through
            lIdx := fTokens[lIdx].Ref1;
            Continue;
          end;
        ttInclude:
          begin
            // Dynamic include - evaluate filename and compile/execute at runtime
            // Get filename from expression
            lDynIncludeFileName := EvaluateExpression(fTokens[lIdx].Value1).AsString;

            // Build full path for file system fallback
            lDynBasePath := fTokens[lIdx].Value2;
            if TDirectory.Exists(lDynBasePath) then
              lDynFullPath := TPath.GetFullPath(TPath.Combine(lDynBasePath, lDynIncludeFileName))
            else
              lDynFullPath := TPath.GetFullPath(TPath.Combine(TPath.GetDirectoryName(lDynBasePath), lDynIncludeFileName));

            // Check cache first (use template name as key to support callback-provided templates)
            if not fDynamicIncludeCache.TryGetValue(lDynIncludeFileName, lDynIncludeTemplate) then
            begin
              // Try callback first if assigned
              lDynHandled := False;
              if Assigned(fOnGetDynamicallyIncludedTemplate) then
              begin
                fOnGetDynamicallyIncludedTemplate(lDynIncludeFileName, lDynIncludeSource, lDynHandled);
              end;
              if not lDynHandled and Assigned(TTProConfiguration.fOnGetTemplate) then
                TTProConfiguration.fOnGetTemplate(lDynIncludeFileName, lDynIncludeSource, lDynHandled);

              // Fallback to file system if not handled
              if not lDynHandled then
              begin
                if (not fIncludeRootPath.IsEmpty) and
                  (not lDynFullPath.StartsWith(IncludeTrailingPathDelimiter(TPath.GetFullPath(fIncludeRootPath)), True)) then
                  Error('Dynamic include "' + lDynIncludeFileName + '" resolves outside IncludeRootPath');
                try
                  lDynIncludeSource := TFile.ReadAllText(lDynFullPath, fEncoding);
                except
                  on E: Exception do
                    Error('Cannot read dynamic include "' + lDynIncludeFileName + '": ' + E.Message);
                end;
              end;

              // Compile the included template
              lDynIncludeCompiler := TTProCompiler.Create(fEncoding);
              try
                // Propagate the callback to the sub-compiler for any static includes in the dynamic template
                lDynIncludeCompiler.OnGetIncludedTemplate := fOnGetDynamicallyIncludedTemplate;
                lDynIncludeTemplate := lDynIncludeCompiler.Compile(lDynIncludeSource, lDynFullPath);
                // Propagate the callback to the compiled template for nested dynamic includes
                lDynIncludeTemplate.OnGetDynamicallyIncludedTemplate := fOnGetDynamicallyIncludedTemplate;
                lDynIncludeTemplate.IncludeRootPath := fIncludeRootPath;
              finally
                lDynIncludeCompiler.Free;
              end;

              // Store in cache
              fDynamicIncludeCache.Add(lDynIncludeFileName, lDynIncludeTemplate);
            end;

            // Copy all variables to the included template
            if fVariables <> nil then
            begin
              for lVarPair in fVariables do
                if lVarPair.Value = nil then // a variable set to an empty value
                  lDynIncludeTemplate.SetData(lVarPair.Key, TValue.Empty)
                else
                  lDynIncludeTemplate.SetData(lVarPair.Key, lVarPair.Value.VarValue);
            end;

            // Execute and append output
            if fRenderNestingDepth + 1 > MAX_RENDER_NESTING then
              Error(Format('Template nesting too deep (max %d levels of macro calls and dynamic includes)', [MAX_RENDER_NESTING]));
            (lDynIncludeTemplate as TTProCompiledTemplate).fRenderNestingDepth := fRenderNestingDepth + 1;
            // same buffer and stacks: its pushes and stacks belong to this render
            (lDynIncludeTemplate as TTProCompiledTemplate).RenderTo(lBuff, fStacks);
          end;
        ttBoolExpression:
          begin
            Error('Token ttBoolExpression cannot be at first RENDER level, should be handled by ttIfThen TOKEN');
          end;
        ttValue, ttLiteralString:
          begin
            lVarValue := EvaluateValue(lIdx, lMustBeEncoded { must be encoded }, lValueOwned);
            try
              // lMustBeEncoded = False means explicit raw ($) - never encode
              // lMustBeEncoded = True means follow autoescape stack
              if (not lMustBeEncoded) or (not fAutoescapeStack.Peek) then
                lBuff.Append(OutputString(lVarValue))
              else
                lBuff.Append(HTMLEncode(OutputString(lVarValue)));
            finally
              // only objects created by custom filters are freed, never the caller's ones
              if lValueOwned then
                lVarValue.AsObject.Free;
            end;
          end;
        ttExpression:
          begin
            lVarValue := EvaluateExpressionToken(lIdx, lMustBeEncoded, lValueOwned);
            try
              // Apply HTML encoding if required
              // lMustBeEncoded = False means explicit raw ($) - never encode
              // lMustBeEncoded = True means follow autoescape stack
              if (not lMustBeEncoded) or (not fAutoescapeStack.Peek) then
                lBuff.Append(OutputString(lVarValue))
              else
                lBuff.Append(HTMLEncode(OutputString(lVarValue)));
            finally
              if lValueOwned then
                lVarValue.AsObject.Free;
            end;
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
            ExecuteMacro(lIdx, lBuff);
            // Skip the call parameters, and the body (the slots) if any
            if fTokens[lIdx].Ref2 > -1 then
              lIdx := fTokens[lIdx].Ref2
            else
              lIdx := lIdx + fTokens[lIdx].Ref1;
          end;
        ttAutoescape:
          begin
            // Push autoescape state: Value1 is 'true' or 'false'
            fAutoescapeStack.Push(fTokens[lIdx].Value1 = 'true');
          end;
        ttEndAutoescape:
          begin
            // Pop autoescape state, but keep at least the default
            if fAutoescapeStack.Count > 1 then
              fAutoescapeStack.Pop;
          end;
        ttPush:
          BeginPush(lIdx, lBuff);
        ttEndPush:
          EndPush(lBuff);
        ttStack:
          AddStackPlaceholder(lIdx, lBuff);
      else
        begin
          Error('Invalid token at index #' + lIdx.ToString + ': ' + fTokens[lIdx].TokenTypeAsString);
        end;
      end;
      Inc(lIdx);
    end;
  finally
    lBlockStack.Free;
  end;
end;

function TTProCompiledTemplate.IsStale: Boolean;
// the main template source is not a dependency: the caller checks it
var
  I: Integer;
  lTime, lSize: Int64;
begin
  I := fTokens.Count - 2; // the dependencies are just before the final EOF
  while (I >= 0) and (fTokens[I].TokenType = ttDependency) do
  begin
    if fTokens[I].Value2 = 'f' then
    begin
      if not GetFileStamp(fTokens[I].Value1, lTime, lSize) or (lTime <> fTokens[I].Ref1) or (lSize <> fTokens[I].Ref2) then
        Exit(True);
    end
    else if TTProConfiguration.GetTemplateVersion(fTokens[I].Value1) <> fTokens[I].Ref1 then
      Exit(True);
    Dec(I);
  end;
  Result := False;
end;

function TTProCompiledTemplate.Render: String;
var
  lBuff: TStringBuilder;
  lStacks: TTProStacks;
begin
  lBuff := TStringBuilder.Create;
  try
    lStacks := TTProStacks.Create;
    try
      RenderTo(lBuff, lStacks);
      lStacks.WriteInto(lBuff);
    finally
      lStacks.Free;
    end;
    Result := lBuff.ToString;
  finally
    lBuff.Free;
  end;
end;

{ TTProStacks }

constructor TTProStacks.Create;
begin
  inherited Create;
  fContents := TObjectDictionary<string, TList<string>>.Create([doOwnsValues], TTProEqualityComparer.Create);
  fPlaceholders := TList<TPair<Integer, string>>.Create;
  fOpenPushes := TStack<TTProOpenPush>.Create;
end;

destructor TTProStacks.Destroy;
begin
  fOpenPushes.Free;
  fPlaceholders.Free;
  fContents.Free;
  inherited;
end;

procedure TTProStacks.WriteInto(const aBuff: TStringBuilder);
var
  I: Integer;
  lContent: TList<string>;
begin
  // placeholders are recorded in buffer order: filling them from the last keeps the other positions valid
  for I := fPlaceholders.Count - 1 downto 0 do
    if fContents.TryGetValue(fPlaceholders[I].Value, lContent) then
      aBuff.Insert(fPlaceholders[I].Key, String.Join('', lContent.ToArray));
end;

function TTProCompiledTemplate.ResolveTokenName(const aToken: TToken): string;
begin
  if aToken.Value2 = 'v' then
    Result := ValueAsString(GetVarAsTValue(aToken.Value1))
  else if aToken.Value2 = '@' then
    Result := ValueAsString(EvaluateExpression(aToken.Value1))
  else
    Result := aToken.Value1;
end;

procedure TTProCompiledTemplate.BeginPush(const aIdx: Int64; const aBuff: TStringBuilder);
var
  lOpenPush: TTProOpenPush;
begin
  // the content is rendered in place, then moved to the stack by EndPush
  lOpenPush.Start := aBuff.Length;
  lOpenPush.StackName := ResolveTokenName(fTokens[aIdx]);
  lOpenPush.Once := fTokens[aIdx].Ref2 = 1;
  fStacks.fOpenPushes.Push(lOpenPush);
end;

procedure TTProCompiledTemplate.EndPush(const aBuff: TStringBuilder);
var
  lOpenPush: TTProOpenPush;
  lContent: string;
  lStackContent: TList<string>;
begin
  lOpenPush := fStacks.fOpenPushes.Pop;
  lContent := aBuff.ToString(lOpenPush.Start, aBuff.Length - lOpenPush.Start);
  aBuff.Length := lOpenPush.Start;
  if not fStacks.fContents.TryGetValue(lOpenPush.StackName, lStackContent) then
  begin
    lStackContent := TList<string>.Create;
    fStacks.fContents.Add(lOpenPush.StackName, lStackContent);
  end;
  if not (lOpenPush.Once and lStackContent.Contains(lContent)) then
    lStackContent.Add(lContent);
end;

procedure TTProCompiledTemplate.AddStackPlaceholder(const aIdx: Int64; const aBuff: TStringBuilder);
begin
  // a position in the output, not a marker: rendered data can never be taken for a stack
  if fStacks.fOpenPushes.Count > 0 then
    Error('"stack" cannot be used inside "push"');
  fStacks.fPlaceholders.Add(TPair<Integer, string>.Create(aBuff.Length, ResolveTokenName(fTokens[aIdx])));
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
  lPathIndex: Integer;
  lPathRemainder: String;
begin
  lCurrentIterator := nil;
  SplitVariableName(aName, lVarName, lVarMembers);
  lHasMember := not lVarMembers.IsEmpty;
  lIsAnIterator := IsAnIterator(lVarName, lDataSource, lCurrentIterator);

  if not lIsAnIterator then
  begin
    lDataSource := lVarName;
  end
  else if lCurrentIterator.IsRange then
  begin
    if lVarMembers.StartsWith('@@') then
      Exit(GetPseudoVariable(lCurrentIterator.IteratorPosition, lVarMembers));
    if lHasMember then
      Error('A range value has no member "%s"', [lVarMembers]);
    Exit(TValue.From<Int64>(lCurrentIterator.RangeStart + lCurrentIterator.IteratorPosition * lCurrentIterator.RangeStep));
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
            // Return TField object (for passing to macros)
            // GetTValueVarAsString will convert it to AsString when needed for display
            Result := TValue.From<TObject>(lField);
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
              jdtArray: // an object for filters and macros, printed as JSON
                begin
                  Result := lPJSONDataValue.ArrayValue;
                end;
              jdtObject:
                begin
                  Result := lPJSONDataValue.ObjectValue;
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
          else if lPJSONDataValue.Typ = jdtBool then
          begin
            Result := lPJSONDataValue.BoolValue;
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
    else if viJSONArray in lVariable.VarOption then
    begin
      if lIsAnIterator then
      begin
        if lVarMembers.StartsWith('@@') then
        begin
          Result := GetPseudoVariable(lCurrentIterator.IteratorPosition, lVarMembers);
        end
        else
        begin
          // Build the full path to the current element
          if lCurrentIterator.FullPath.IsEmpty then
          begin
            // Direct iteration over JSON array
            lPJSONDataValue := TJDOJsonArray(lVariable.VarValue.AsObject)[lCurrentIterator.IteratorPosition];
          end
          else
          begin
            // Nested iteration: path like [0].devices
            // Build path with current iterator position appended
            lJPath := lCurrentIterator.FullPath + '[' + lCurrentIterator.IteratorPosition.ToString + ']';
            // Parse to get index and remaining path
            ParseJSONArrayPath(lJPath, lPathIndex, lPathRemainder);
            if (lPathIndex >= 0) and (lPathIndex < TJDOJsonArray(lVariable.VarValue.AsObject).Count) then
            begin
              if lPathRemainder.IsEmpty then
                lPJSONDataValue := TJDOJsonArray(lVariable.VarValue.AsObject)[lPathIndex]
              else
                lPJSONDataValue := TJDOJsonArray(lVariable.VarValue.AsObject)[lPathIndex].ObjectValue.Path[lPathRemainder];
            end;
          end;
          if lPJSONDataValue.Typ in [jdtArray, jdtObject] then
          begin
            if not lVarMembers.IsEmpty then
              lPJSONDataValue := lPJSONDataValue.Path[lVarMembers];
            case lPJSONDataValue.Typ of
              jdtArray: // an object for filters and macros, printed as JSON
                begin
                  Result := lPJSONDataValue.ArrayValue;
                end;
              jdtObject:
                begin
                  Result := lPJSONDataValue.ObjectValue;
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
        // Direct access to JSON array (not as iterator)
        Result := TJDOJsonArray(lVariable.VarValue.AsObject);
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
              Result := GetTValueFromPath(WrapAsList(lVariable.VarValue.AsObject)
                .GetItem(lCurrentIterator.IteratorPosition), lVarMembers)
            end
            else
            begin
              lFullPath := lCurrentIterator.FullPath;
              lValue := GetTValueFromPath(lVariable.VarValue.AsObject, lFullPath);
              lTmpList := WrapAsList(lValue.AsObject);
              if Assigned(lTmpList)then
                Result := GetTValueFromPath(lTmpList.GetItem(lCurrentIterator.IteratorPosition), lVarMembers)
              else
                Result := GetTValueFromPath(lValue.AsObject, lVarMembers)
            end;
          end
          else
          begin
            if lCurrentIterator.FullPath.IsEmpty then
            begin
              // the item as it is: an object, or a simple value (e.g. of a TList<string>)
              WrapAsList(lVariable.VarValue.AsObject).ItemIsObject(lCurrentIterator.IteratorPosition, Result);
              if Result.Kind = tkInterface then
                Result := TObject(Result.AsInterface); // as before 1.2
            end
            else
            begin
              lValue := GetTValueFromPath(lVariable.VarValue.AsObject, lCurrentIterator.FullPath);
              lTmpList := WrapAsList(lValue.AsObject);
              if Assigned(lTmpList)then
                Result := GetTValueFromPath(lTmpList.GetItem(lCurrentIterator.IteratorPosition), lVarMembers)
              else
                Result := GetTValueFromPath(lValue.AsObject, lVarMembers)
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
      else if lHasMember then
      begin
        // Handle member access for objects stored in simple types (e.g., TField passed to macro)
        if lVariable.VarValue.IsObject and (lVariable.VarValue.AsObject <> nil) then
        begin
          if lVariable.VarValue.AsObject is TField then
            Result := GetFieldProperty(TField(lVariable.VarValue.AsObject), lVarMembers)
          else
            Result := GetTValueFromPath(lVariable.VarValue.AsObject, lVarMembers);
        end
        else
        begin
          Result := TValue.Empty;
        end;
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
    else if Value.AsObject is TField then
    begin
      // TField with null value is falsy
      if TField(Value.AsObject).IsNull then
        lStrValue := ''
      else
        lStrValue := 'true';
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
  lValue: TValue;
  lValueOwned: Boolean;
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
    lValue := EvaluateValue(Idx, lMustBeEncoded, lValueOwned);
    try
      Result := IsTruthy(lValue);
    finally
      if lValueOwned then
        lValue.AsObject.Free;
    end;
  end;
end;

function TTProCompiledTemplate.EvaluateValue(var Idx: Int64; out MustBeEncoded: Boolean; out ResultOwned: Boolean): TValue;
var
  lCurrTokenType: TTokenType;
  lVarName: string;
  lFilterCount: Int64;
  lNegated: Boolean;
  lCurrentValue: TValue;
  lDataSetFieldMeta: string;
  lValueIsHTML: Boolean;
begin
  // Ref1 contains the number of filters (0 if there isn't any filter)
  // Ref2 is -1 if the variable must be HTMLEncoded, while contains 1 is the value must not be HTMLEncoded
  MustBeEncoded := fTokens[Idx].Ref2 = -1;
  ResultOwned := False;
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

    // Unwrap Nullable types before passing to filters
    if (not lCurrentValue.IsEmpty) and IsNullableType(@lCurrentValue) then
      lCurrentValue := GetNullableTValueAsTValue(@lCurrentValue, lVarName);

    // Apply filters
    ApplyFilters(Idx, lCurrentValue, lFilterCount, lVarName, ResultOwned, lValueIsHTML);
    if lValueIsHTML then
      MustBeEncoded := False;

    // For bool expressions, convert final result to boolean
    if lCurrTokenType = ttBoolExpression then
    begin
      Result := IsTruthy(lCurrentValue);
      if ResultOwned then
      begin
        lCurrentValue.AsObject.Free;
        ResultOwned := False;
      end;
    end
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
    if ResultOwned then
    begin
      Result.AsObject.Free;
      ResultOwned := False;
      Error('Cannot negate an object returned by a filter');
    end;
    Result := not Result.AsBoolean;
  end;
end;

procedure TTProCompiledTemplate.ApplyFilters(var Idx: Int64; var Value: TValue; FilterCount: Int64; const ContextName: string;
  out ValueOwned: Boolean; out ValueIsHTML: Boolean);
var
  lFilterName: string;
  lFilterParCount: Int64;
  lFilterParameters: TArray<TFilterParameter>;
  I, J: Integer;
begin
  ValueOwned := False; // the initial value always belongs to the caller
  ValueIsHTML := False;
  for J := 0 to FilterCount - 1 do
  begin
    Inc(Idx);
    Assert(fTokens[Idx].TokenType = ttFilterName);
    lFilterName := fTokens[Idx].Value1;
    lFilterParCount := fTokens[Idx].Ref1;
    SetLength(lFilterParameters, lFilterParCount);
    for I := 0 to lFilterParCount - 1 do
    begin
      Inc(Idx);
      Assert(fTokens[Idx].TokenType = ttFilterParameter);
      lFilterParameters[I] := ResolveFilterParameter(fTokens[Idx]);
    end;
    // the built-in nl2br encodes its input itself: its result, if not changed by another filter, is emitted as is
    ValueIsHTML := SameText(lFilterName, 'nl2br') and not IsCustomFilter(lFilterName);
    try
      ExecuteFilterTrackingOwnership(lFilterName, lFilterParameters, Value, ContextName, ValueOwned);
    except
      on E: Exception do
      begin
        Error('Error while evaluating filter [%s] on variable [%s]- Inner Exception: [%s][%s]', [lFilterName, ContextName, E.ClassName, E.Message]);
      end;
    end;
  end;
end;

function TTProCompiledTemplate.EvaluateExpressionToken(var Idx: Int64; out MustBeEncoded: Boolean; out ResultOwned: Boolean): TValue;
// {{@expr|filters}}: Idx is left on the last filter token
var
  lValueIsHTML: Boolean;
begin
  Result := EvaluateExpression(fTokens[Idx].Value1);
  MustBeEncoded := fTokens[Idx].Ref2 = -1;
  ResultOwned := False;
  if fTokens[Idx].Ref1 > 0 then
  begin
    ApplyFilters(Idx, Result, fTokens[Idx].Ref1, 'expression', ResultOwned, lValueIsHTML);
    if lValueIsHTML then
      MustBeEncoded := False;
  end;
end;

function TTProCompiledTemplate.IsCustomFilter(const aFilterName: string): Boolean;
begin
  Result := fTemplateFunctions.ContainsKey(aFilterName.ToLower) or
    ((fTemplateAnonFunctions <> nil) and fTemplateAnonFunctions.ContainsKey(aFilterName.ToLower));
end;

function TTProCompiledTemplate.ResolveFilterParameter(const aToken: TToken): TFilterParameter;
// an @(expression) parameter is evaluated now and passed to the filter as a literal
begin
  Result := TokenToFilterParameter(aToken);
  if Result.ParType = fptExpression then
    Result := ValueToFilterParameter(EvaluateExpression(Result.ParStrText));
end;

function TTProCompiledTemplate.GetParameterValue(const aParameter: TFilterParameter): TValue;
begin
  case aParameter.ParType of
    fptInteger:
      Result := aParameter.ParIntValue;
    fptFloat:
      Result := aParameter.ParFloatValue;
    fptVariable:
      begin
        Result := GetVarAsTValue(aParameter.ParStrText);
        if (not Result.IsEmpty) and IsNullableType(@Result) then
          Result := GetNullableTValueAsTValue(@Result, aParameter.ParStrText);
      end;
    fptExpression:
      Result := EvaluateExpression(aParameter.ParStrText);
  else
    Result := aParameter.ParStrText;
  end;
end;

function TTProCompiledTemplate.ValueAsString(const aValue: TValue): string;
var
  lValue: TValue;
  lIsNull: Boolean;
begin
  if aValue.IsEmpty then
    Exit('');
  if aValue.Kind in [tkString, tkUString, tkWString, tkLString] then
    Exit(aValue.AsString);
  lValue := aValue;
  Result := GetTValueVarAsString(@lValue, lIsNull);
end;

function TTProCompiledTemplate.MemberValue(const aModel: TValue; const aName: string): TValue;
// the member aName of an object (RTTI property), dataset (field of the current record), JSON object (key),
// dictionary with string keys (TryGetValue) or TStrings (Values[]); empty when the model or the member is missing
var
  lObj: TObject;
  lType: TRttiType;
  lProp: TRttiProperty;
  lMethod: TRttiMethod;
  lParams: TArray<TRttiParameter>;
  lArgs: TArray<TValue>;
  lField: TField;
  lJSON: TJDOJsonObject;
begin
  Result := TValue.Empty;
  if aModel.IsEmpty or not aModel.IsObject or (aModel.AsObject = nil) or aName.IsEmpty then
    Exit;
  lObj := aModel.AsObject;
  if lObj is TDataSet then
  begin
    lField := TDataSet(lObj).FindField(aName);
    if (lField <> nil) and not lField.IsNull then
      Result := GetDataSetFieldAsTValue(TDataSet(lObj), lField.FieldName);
  end
  else if lObj is TJDOJsonObject then
  begin
    lJSON := TJDOJsonObject(lObj);
    case lJSON.Types[aName] of
      jdtString: Result := lJSON.S[aName];
      jdtInt: Result := lJSON.I[aName];
      jdtLong, jdtULong: Result := lJSON.L[aName];
      jdtFloat: Result := lJSON.F[aName];
      jdtBool: Result := lJSON.B[aName];
      jdtObject: Result := lJSON.O[aName];
      jdtArray: Result := lJSON.A[aName];
    end; // jdtNone (missing or null): empty
  end
  else if lObj is TStrings then
  begin
    if TStrings(lObj).IndexOfName(aName) > -1 then
      Result := TStrings(lObj).Values[aName];
  end
  else
  begin
    lType := GlContext.GetType(lObj.ClassType);
    lMethod := lType.GetMethod('TryGetValue');
    if lMethod <> nil then
    begin
      // a dictionary: only string keys
      lParams := lMethod.GetParameters;
      if (Length(lParams) <> 2) or (lParams[0].ParamType.Handle <> TypeInfo(string)) then
        Exit;
      SetLength(lArgs, 2);
      lArgs[0] := aName;
      TValue.Make(nil, lParams[1].ParamType.Handle, lArgs[1]);
      if not lMethod.Invoke(lObj, lArgs).AsBoolean then
        Exit;
      Result := lArgs[1];
      if Result.TypeInfo = TypeInfo(TValue) then
        Result := Result.AsType<TValue>;
    end
    else
    begin
      lProp := lType.GetProperty(aName); // case-insensitive
      if (lProp = nil) or not lProp.IsReadable then
        Exit;
      Result := lProp.GetValue(lObj);
    end;
  end;
  if (not Result.IsEmpty) and IsNullableType(@Result) then
    Result := GetNullableTValueAsTValue(@Result, aName);
end;

function FieldDataTypeOf(const aType: TRttiType): string;
// the TField.DataType name for a simple property type, '' for the types @@fields skips
var
  lField: TRttiField;
begin
  Result := '';
  if aType = nil then
    Exit;
  case aType.TypeKind of
    tkString, tkLString, tkWString, tkUString, tkChar, tkWChar:
      Result := 'ftString';
    tkInteger:
      Result := 'ftInteger';
    tkInt64:
      Result := 'ftLargeint';
    tkEnumeration:
      if aType.Handle = TypeInfo(Boolean) then
        Result := 'ftBoolean'
      else
        Result := 'ftString'; // the value is the name of the enumeration item
    tkFloat:
      if aType.Handle = TypeInfo(TDate) then
        Result := 'ftDate'
      else if aType.Handle = TypeInfo(TTime) then
        Result := 'ftTime'
      else if aType.Handle = TypeInfo(TDateTime) then
        Result := 'ftDateTime'
      else if aType.Handle = TypeInfo(Currency) then
        Result := 'ftCurrency'
      else
        Result := 'ftFloat';
    tkRecord:
      if string(aType.Name).StartsWith('Nullable', True) then
      begin
        lField := aType.GetField('fValue'); // MVCFramework.Nullables: the type of the wrapped value
        if lField <> nil then
          Result := FieldDataTypeOf(lField.FieldType);
      end;
  end;
end;

function ReadableLabel(const aName: string): string;
// "CustomerName" -> "Customer name", "VAT_Number" -> "Vat number", "VATNumber" -> "Vat number":
// words split at underscores and at lower/upper case changes, then all lower case with the first letter upper
var
  I: Integer;
  lSB: TStringBuilder;
  C: Char;
begin
  lSB := TStringBuilder.Create;
  try
    for I := 0 to aName.Length - 1 do
    begin
      C := aName.Chars[I];
      if C = '_' then
        C := ' '
      else if C.IsUpper and (I > 0) and
        (aName.Chars[I - 1].IsLower or aName.Chars[I - 1].IsDigit or
        (aName.Chars[I - 1].IsUpper and (I < aName.Length - 1) and aName.Chars[I + 1].IsLower)) then
        lSB.Append(' ');
      lSB.Append(C);
    end;
    Result := string.Join(' ', lSB.ToString.ToLower.Split([' '], TStringSplitOptions.ExcludeEmpty));
  finally
    lSB.Free;
  end;
  if not Result.IsEmpty then
    Result := Result.Chars[0].ToUpper + Result.Substring(1);
end;

function TTProCompiledTemplate.IsObjectForFields(const aExpression: string; out aObject: TValue): Boolean;
// {{for f in x.@@fields}} iterates the properties of x when x is an object (not a dataset, a list or JSON)
begin
  aObject := GetVarAsTValue(aExpression);
  Result := aObject.IsObject and (aObject.AsObject <> nil) and not (aObject.AsObject is TDataSet) and
    not (aObject.AsObject is TJsonBaseObject) and (WrapAsList(aObject.AsObject) = nil);
end;

function TTProCompiledTemplate.ObjectFieldsMetadata(const aObject: TObject): TObjectList<TTProFieldMetadata>;
// the readable public/published properties with a simple type, base class first, in declaration order
var
  lTypes: TList<TRttiType>;
  lType: TRttiType;
  lProp: TRttiProperty;
  lDataType: string;
  lMeta: TTProFieldMetadata;
  I: Integer;
begin
  Result := TObjectList<TTProFieldMetadata>.Create(True);
  try
    lTypes := TList<TRttiType>.Create;
    try
      lType := GlContext.GetType(aObject.ClassType);
      while (lType <> nil) and (lType.Handle <> TypeInfo(TObject)) do
      begin
        lTypes.Insert(0, lType);
        lType := lType.BaseType;
      end;
      for I := 0 to lTypes.Count - 1 do
        for lProp in lTypes[I].GetDeclaredProperties do
        begin
          if (lProp.Visibility < mvPublic) or not lProp.IsReadable then
            Continue;
          lDataType := FieldDataTypeOf(lProp.PropertyType);
          if lDataType.IsEmpty then
            Continue; // objects, arrays, records...
          lMeta := TTProFieldMetadata.Create;
          Result.Add(lMeta);
          lMeta.fFieldName := lProp.Name;
          lMeta.fDisplayLabel := ReadableLabel(lProp.Name);
          lMeta.fDataType := lDataType;
          lMeta.fReadOnly := not lProp.IsWritable;
          lMeta.fVisible := True;
          lMeta.fValue := MemberValue(aObject, lProp.Name);
          if Assigned(TTProConfiguration.fOnGetFieldMetadata) then
            TTProConfiguration.fOnGetFieldMetadata(aObject, lProp.Name, lMeta);
        end;
    finally
      lTypes.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TTProCompiledTemplate.OutputString(const aValue: TValue): string;
// how {{:x|filter}} and {{@expr}} print: null -> nothing, floats and dates with the template FormatSettings (as {{:x}})
begin
  if aValue.IsEmpty then
    Result := ''
  else if aValue.Kind = tkFloat then
    Result := ValueAsString(aValue)
  else
    Result := aValue.ToString;
end;

function TTProCompiledTemplate.ExecuteListAndTextFilter(const aFunctionName: string;
  var aParameters: TArray<TFilterParameter>; const aValue: TValue; out aResult: TValue): Boolean;
var
  lObj: TObject;
  lList: ITProWrappedList;
  lStr, lSeparator, lPropName, lLine, lCurrent, lWord, lLineBreak: string;
  lParts: TArray<string>;
  lWidth, lCount: Int64;
  I: Integer;

  function JsonItem(const aArray: TJDOJsonArray; const aIndex: Integer): TValue;
  begin
    case aArray.Types[aIndex] of
      jdtObject:
        Result := aArray.O[aIndex];
      jdtArray:
        Result := aArray.A[aIndex];
      jdtInt:
        Result := aArray.I[aIndex];
      jdtLong, jdtULong:
        Result := aArray.L[aIndex];
      jdtFloat:
        Result := aArray.F[aIndex];
      jdtBool:
        Result := aArray.B[aIndex];
      jdtNone:
        Result := TValue.Empty;
    else
      Result := aArray.S[aIndex];
    end;
  end;

  function ItemAsString(const aItem: TValue): string;
  begin
    if lPropName.IsEmpty then
      Result := ValueAsString(aItem)
    else if aItem.IsObject and (aItem.AsObject is TJDOJsonObject) then
      Result := TJDOJsonObject(aItem.AsObject).Path[lPropName].Value
    else if aItem.IsObject and (aItem.AsObject <> nil) then
      Result := ValueAsString(GetTValueFromPath(aItem.AsObject, lPropName))
    else
      Result := ValueAsString(aItem);
  end;

  // the list elements of aValue (JSON array, duck-typed list, dynamic array), False if it is not a list
  function GetElements(out aElements: TArray<TValue>): Boolean;
  var
    J: Integer;
  begin
    aElements := [];
    Result := True;
    if aValue.IsArray then
    begin
      SetLength(aElements, aValue.GetArrayLength);
      for J := 0 to High(aElements) do
        aElements[J] := aValue.GetArrayElement(J);
      Exit;
    end;
    lObj := nil;
    if aValue.IsObject then
      lObj := aValue.AsObject;
    if lObj is TJDOJsonArray then
    begin
      SetLength(aElements, TJDOJsonArray(lObj).Count);
      for J := 0 to High(aElements) do
        aElements[J] := JsonItem(TJDOJsonArray(lObj), J);
      Exit;
    end;
    if lObj <> nil then
    begin
      lList := TTProDuckTypedList.Wrap(lObj);
      if lList <> nil then
      begin
        SetLength(aElements, lList.Count);
        for J := 0 to High(aElements) do
          lList.ItemIsObject(J, aElements[J]);
        Exit;
      end;
    end;
    Result := False;
  end;

var
  lElements: TArray<TValue>;
begin
  Result := True;
  if SameText(aFunctionName, 'trim') then
  begin
    CheckParNumber(0, aParameters);
    aResult := ValueAsString(aValue).Trim;
  end
  else if SameText(aFunctionName, 'replace') then
  begin
    CheckParNumber(2, aParameters);
    aResult := StringReplace(ValueAsString(aValue), ValueAsString(GetParameterValue(aParameters[0])),
      ValueAsString(GetParameterValue(aParameters[1])), [rfReplaceAll]);
  end
  else if SameText(aFunctionName, 'join') then
  begin
    CheckParNumber(1, 2, aParameters);
    lSeparator := ValueAsString(GetParameterValue(aParameters[0]));
    lPropName := '';
    if Length(aParameters) = 2 then
      lPropName := ValueAsString(GetParameterValue(aParameters[1]));
    if aValue.IsEmpty then
      aResult := ''
    else if GetElements(lElements) then
    begin
      SetLength(lParts, Length(lElements));
      for I := 0 to High(lElements) do
        lParts[I] := ItemAsString(lElements[I]);
      aResult := String.Join(lSeparator, lParts);
    end
    else
      FunctionError(aFunctionName, 'can be applied only to a list');
  end
  else if SameText(aFunctionName, 'length') then
  begin
    CheckParNumber(0, aParameters);
    if aValue.IsEmpty then
      aResult := 0
    else if aValue.IsObject and (aValue.AsObject is TDataSet) then
      aResult := TDataSet(aValue.AsObject).RecordCount
    else if GetElements(lElements) then
      aResult := Length(lElements)
    else
      aResult := Length(ValueAsString(aValue));
  end
  else if SameText(aFunctionName, 'first') or SameText(aFunctionName, 'last') then
  begin
    // built-in: the element returned still belongs to the list, nothing is freed
    CheckParNumber(0, aParameters);
    if aValue.IsEmpty then
      aResult := TValue.Empty
    else if GetElements(lElements) then
    begin
      if Length(lElements) = 0 then
        aResult := TValue.Empty
      else if SameText(aFunctionName, 'first') then
        aResult := lElements[0]
      else
        aResult := lElements[High(lElements)];
    end
    else
    begin
      lStr := ValueAsString(aValue);
      if lStr.IsEmpty then
        aResult := ''
      else if SameText(aFunctionName, 'first') then
        aResult := lStr.Substring(0, 1)
      else
        aResult := lStr.Substring(lStr.Length - 1);
    end;
  end
  else if SameText(aFunctionName, 'striptags') then
  begin
    CheckParNumber(0, aParameters);
    aResult := TRegEx.Replace(ValueAsString(aValue), '<[^>]*>', '');
  end
  else if SameText(aFunctionName, 'wordwrap') then
  begin
    CheckParNumber(1, aParameters);
    lWidth := GetParameterValue(aParameters[0]).AsInt64;
    if lWidth < 1 then
      FunctionError(aFunctionName, 'width must be greater than zero');
    lLineBreak := GetLineEndingString;
    lParts := [];
    // existing line breaks are kept, every line is wrapped at word boundaries
    for lLine in ValueAsString(aValue).Replace(#13#10, #10).Replace(#13, #10).Split([#10]) do
    begin
      lCurrent := '';
      for lWord in lLine.Split([' '], TStringSplitOptions.ExcludeEmpty) do
      begin
        if lCurrent.IsEmpty then
          lCurrent := lWord
        else if lCurrent.Length + 1 + lWord.Length <= lWidth then
          lCurrent := lCurrent + ' ' + lWord
        else
        begin
          lParts := lParts + [lCurrent];
          lCurrent := lWord; // a word longer than the width stays whole
        end;
      end;
      lParts := lParts + [lCurrent];
    end;
    aResult := String.Join(lLineBreak, lParts);
  end
  else if SameText(aFunctionName, 'pluralize') then
  begin
    CheckParNumber(2, aParameters);
    if aValue.IsEmpty then
      aResult := ''
    else
    begin
      if aValue.Kind = tkFloat then
        lCount := IfThen(aValue.AsExtended = 1, 1, 0)
      else
        lCount := StrToInt64Def(ValueAsString(aValue), 0);
      if lCount = 1 then
        aResult := ValueAsString(GetParameterValue(aParameters[0]))
      else
        aResult := ValueAsString(GetParameterValue(aParameters[1]));
    end;
  end
  else if SameText(aFunctionName, 'attr') then
  begin
    // built-in: returns data owned by the model, never frees
    CheckParNumber(1, aParameters);
    aResult := MemberValue(aValue, ValueAsString(GetParameterValue(aParameters[0])));
  end
  else if SameText(aFunctionName, 'nl2br') then
  begin
    // the result is HTML: see ValueIsHTML in ApplyFilters
    CheckParNumber(0, aParameters);
    aResult := HTMLEncode(ValueAsString(aValue)).Replace(#13#10, '<br>').Replace(#10, '<br>').Replace(#13, '<br>');
  end
  else
    Result := False;
end;

procedure TTProCompiledTemplate.ExecuteFilterTrackingOwnership(const aFilterName: string;
  var aParameters: TArray<TFilterParameter>; var aValue: TValue; const aContextName: string; var aValueOwned: Boolean);
// Ownership rule: an object returned by a custom filter, different from the object it received,
// belongs to the engine (v1.1 contract). Anything else - SetData objects, objects returned by
// built-in filters such as "default", objects passed through unchanged - belongs to the caller.
var
  lInput: TValue;
  lIsCustomFilter: Boolean;
  lSameObject: Boolean;
begin
  lInput := aValue;
  try
    aValue := ExecuteFilter(aFilterName, aParameters, lInput, aContextName, lIsCustomFilter);
  except
    if aValueOwned then
    begin
      aValue := TValue.Empty;
      aValueOwned := False;
      lInput.AsObject.Free;
    end;
    raise;
  end;
  lSameObject := lInput.IsObjectInstance and aValue.IsObjectInstance and (lInput.AsObject = aValue.AsObject);
  if lSameObject then
    Exit; // passed through: ownership unchanged
  if aValueOwned then
  begin
    // engine-owned input consumed by the filter; an object result may live inside it (e.g. "first"
    // on a filter-created list), so in that case the input is kept until Render ends
    if aValue.IsObjectInstance then
      fOwnedObjects.Add(lInput.AsObject)
    else
      lInput.AsObject.Free;
  end;
  aValueOwned := lIsCustomFilter and aValue.IsObjectInstance and (aValue.AsObject <> nil);
end;

procedure TTProCompiledTemplate.ResetRenderState;
begin
  // A render interrupted by an exception leaves these stacks half-filled: every render starts clean
  fLoopsStack.Clear;
  fIncludeSavedVarsStack.Clear;
  fAutoescapeStack.Clear;
  fAutoescapeStack.Push(True); // Default: autoescape enabled
  fSlotFrames.Clear;
  fCurrentSlotFrame := -1;
end;

procedure TTProCompiledTemplate.ReleaseOwnedObjects;
var
  lPair: TPair<string, TVarDataSource>;
  lVarsToRemove: TList<string>;
begin
  if fOwnedObjects.Count = 0 then
    Exit;
  // no variable may keep pointing to an object that is about to be freed
  if fVariables <> nil then
  begin
    lVarsToRemove := TList<string>.Create;
    try
      for lPair in fVariables do
        if (lPair.Value <> nil) and lPair.Value.VarValue.IsObjectInstance and
          (fOwnedObjects.IndexOf(lPair.Value.VarValue.AsObject) > -1) then
          lVarsToRemove.Add(lPair.Key);
      for var lVarName in lVarsToRemove do
        fVariables.Remove(lVarName);
    finally
      lVarsToRemove.Free;
    end;
  end;
  fOwnedObjects.Clear;
end;

procedure SaveCompiledTemplate(const aTokens: TList<TToken>; const aStream: TStream);
var
  lToken: TToken;
  lBW: TBinaryWriter;
begin
  lBW := TBinaryWriter.Create(aStream, nil, False);
  try
    for lToken in aTokens do
    begin
      lToken.SaveToBytes(lBW);
    end;
  finally
    lBW.Free;
  end;
end;

procedure TTProCompiledTemplate.SaveToFile(const FileName: String);
var
  lStream: TFileStream;
begin
  lStream := TFileStream.Create(FileName, fmCreate or fmOpenWrite or fmShareDenyNone);
  try
    SaveCompiledTemplate(fTokens, lStream);
  finally
    lStream.Free;
  end;
end;

function TTProCompiledTemplate.SaveToBytes: TBytes;
var
  lStream: TBytesStream;
begin
  lStream := TBytesStream.Create;
  try
    SaveCompiledTemplate(fTokens, lStream);
    Result := Copy(lStream.Bytes, 0, lStream.Size);
  finally
    lStream.Free;
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
          GetVariables.AddOrSetValue(Name, TVarDataSource.Create(TJDOJsonArray(lObj), [viJSONArray, viIterable]));
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
    tkInteger, tkInt64, tkString, tkUString, tkFloat, tkEnumeration, tkRecord:
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

class function TTProRTTIUtils.ObjectToJSONString(AObject: TObject): string;
var
  lRttiType: TRttiType;
  lProp: TRttiProperty;
  lValue: TValue;
  lJSON: TJDOJsonObject;
begin
  if AObject = nil then
    Exit('null');

  lJSON := TJDOJsonObject.Create;
  try
    lRttiType := GlContext.GetType(AObject.ClassType);
    for lProp in lRttiType.GetProperties do
    begin
      if lProp.IsReadable and (lProp.Visibility in [mvPublic, mvPublished]) then
      begin
        lValue := lProp.GetValue(AObject);
        case lProp.PropertyType.TypeKind of
          tkInteger, tkInt64:
            lJSON.I[lProp.Name] := lValue.AsInt64;
          tkFloat:
            if lProp.PropertyType.Handle = TypeInfo(TDateTime) then
              lJSON.S[lProp.Name] := DateToISO8601(lValue.AsExtended)
            else
              lJSON.F[lProp.Name] := lValue.AsExtended;
          tkString, tkLString, tkWString, tkUString:
            lJSON.S[lProp.Name] := lValue.AsString;
          tkEnumeration:
            if lProp.PropertyType.Handle = TypeInfo(Boolean) then
              lJSON.B[lProp.Name] := lValue.AsBoolean
            else
              lJSON.S[lProp.Name] := lValue.ToString;
        end;
      end;
    end;
    Result := lJSON.ToJSON;
  finally
    lJSON.Free;
  end;
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

{ TTProConfiguration }

class constructor TTProConfiguration.Create;
begin
  fTemplateVersions := TDictionary<string, Int64>.Create(TTProEqualityComparer.Create);
end;

class destructor TTProConfiguration.Destroy;
begin
  fTemplateVersions.Free;
end;

class procedure TTProConfiguration.TemplateChanged(const Name: string);
var
  lVersion: Int64;
begin
  TMonitor.Enter(fTemplateVersions);
  try
    if not fTemplateVersions.TryGetValue(Name, lVersion) then
      lVersion := 0;
    fTemplateVersions.AddOrSetValue(Name, lVersion + 1);
  finally
    TMonitor.Exit(fTemplateVersions);
  end;
end;

class function TTProConfiguration.GetTemplateVersion(const Name: string): Int64;
begin
  TMonitor.Enter(fTemplateVersions);
  try
    if not fTemplateVersions.TryGetValue(Name, Result) then
      Result := 0;
  finally
    TMonitor.Exit(fTemplateVersions);
  end;
end;

class procedure TTProConfiguration.RegisterHandlers(const TemplateProCompiledTemplate: ITProCompiledTemplate);
begin
  if Assigned(fOnContextConfiguration) then
  begin
    fOnContextConfiguration(TemplateProCompiledTemplate);
  end;
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
        if lTmpValue.TypeInfo = TypeInfo(TValue) then
          lTmpValue := lTmpValue.AsType<TValue>; // e.g. TTProFieldMetadata.Value
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
  lIdx: Int64;
  lParamToken: TToken;
begin
  // Extract macro information from tokens
  lMacroName := fTokens[TokenIndex].Value1;
  lParamCount := fTokens[TokenIndex].Ref1;

  // Parse macro parameters (Ref1 counts the tokens of the parameters and of their filters)
  lParams := [];
  lIdx := TokenIndex + 1;
  while lIdx <= TokenIndex + lParamCount do
  begin
    SetLength(lParams, Length(lParams) + 1);
    I := High(lParams);
    lParamToken := fTokens[lIdx];
    lParams[I].TokenIndex := lIdx;
    lIdx := LastFilterToken(lIdx) + 1;
    lParams[I].Name := lParamToken.Value1;
    // Value2 = '' (no default) or the default's TFilterParameterType ordinal as a digit followed by its value
    lParams[I].HasDefault := not lParamToken.Value2.IsEmpty;
    if lParams[I].HasDefault then
      lParams[I].DefaultValue := TokenToFilterParameter(TToken.Create(ttFilterParameter,
        lParamToken.Value2.Substring(1), '', -1, Ord(lParamToken.Value2.Chars[0]) - Ord('0')))
    else
      lParams[I].DefaultValue := Default(TFilterParameter);
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
  lValueOwned: Boolean;
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
        lValueOwned := False;
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
            lSetFilterParams[lSetI] := ResolveFilterParameter(fTokens[Idx]);
          end;
          ExecuteFilterTrackingOwnership(lSetFilterName, lSetFilterParams, lVarValue, lSetSourceVar, lValueOwned);
        end;
        // a filter-created object must outlive the variable's uses: it is freed when Render ends
        if lValueOwned then
          fOwnedObjects.Add(lVarValue.AsObject);
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

function TTProCompiledTemplate.TokenToFilterParameter(const aToken: TToken): TFilterParameter;
begin
  Result := Default(TFilterParameter);
  Result.ParType := TFilterParameterType(aToken.Ref2);
  case Result.ParType of
    fptInteger:
      Result.ParIntValue := aToken.Value1.ToInteger;
    fptFloat:
      Result.ParFloatValue := StrToFloat(aToken.Value1, TFormatSettings.Invariant);
    fptString, fptVariable, fptExpression:
      Result.ParStrText := aToken.Value1;
  end;
end;

function TTProCompiledTemplate.ValueToFilterParameter(const aValue: TValue): TFilterParameter;
// a computed value (e.g. the result of an @(expression)) as a literal parameter
begin
  Result := Default(TFilterParameter);
  if aValue.IsEmpty then
    Result.ParType := fptString
  else if aValue.IsType<Boolean> then
  begin
    Result.ParType := fptInteger;
    Result.ParIntValue := Ord(aValue.AsBoolean);
  end
  else if aValue.Kind in [tkInteger, tkInt64] then
  begin
    Result.ParType := fptInteger;
    Result.ParIntValue := aValue.AsInt64;
  end
  else if aValue.Kind = tkFloat then
  begin
    Result.ParType := fptFloat;
    Result.ParFloatValue := aValue.AsExtended;
  end
  else
  begin
    Result.ParType := fptString;
    Result.ParStrText := aValue.ToString;
  end;
end;

procedure TTProCompiledTemplate.InitRangeLoop(const aLoop: TLoopStackItem);
var
  lArgs: TArray<string>;
  lStart, lStop, lStep, lCount: Int64;

  function EvaluateArg(const aExpression: string): Int64;
  var
    lValue: TValue;
  begin
    lValue := EvaluateExpression(aExpression);
    if lValue.Kind in [tkInteger, tkInt64] then
      Result := lValue.AsInt64
    else if (lValue.Kind = tkFloat) and (Frac(lValue.AsExtended) = 0) then
      Result := Trunc(lValue.AsExtended)
    else
    begin
      Error('range arguments must be integers, got [%s] from [%s]', [lValue.ToString, aExpression]);
      Result := 0;
    end;
  end;

begin
  lArgs := aLoop.LoopExpression.Substring(Length(RANGE_PREFIX),
    Length(aLoop.LoopExpression) - Length(RANGE_PREFIX) - 1).Split([RANGE_ARG_SEPARATOR]);
  lStart := 0;
  lStep := 1;
  if Length(lArgs) = 1 then
    lStop := EvaluateArg(lArgs[0])
  else
  begin
    lStart := EvaluateArg(lArgs[0]);
    lStop := EvaluateArg(lArgs[1]);
    if Length(lArgs) = 3 then
      lStep := EvaluateArg(lArgs[2]);
  end;
  if lStep = 0 then
    Error('range step cannot be zero');
  // Python semantics: the stop value is excluded
  if (lStep > 0) and (lStop > lStart) then
    lCount := (lStop - lStart + lStep - 1) div lStep
  else if (lStep < 0) and (lStart > lStop) then
    lCount := (lStart - lStop - lStep - 1) div -lStep
  else
    lCount := 0;
  if lCount > MaxInt then
    Error('range too large: %d items', [lCount]);
  aLoop.RangeStart := lStart;
  aLoop.RangeStep := lStep;
  aLoop.TotalCount := lCount;
end;

function TTProCompiledTemplate.SelectSwitchBranch(const aSwitchIdx: Int64): Int64;
// returns the index of the first token to execute: the body of the matching case, of default, or after endswitch
var
  lIdx, lBranch, lParIdx: Int64;
  lValue: TValue;
  lValueOwned, lValueIsHTML: Boolean;
  lMatched: Boolean;
  lParam: TFilterParameter;
begin
  lIdx := aSwitchIdx;
  if fTokens[lIdx].Value2 = '@' then
    lValue := EvaluateExpression(fTokens[lIdx].Value1)
  else
    lValue := GetVarAsTValue(fTokens[lIdx].Value1);
  if (not lValue.IsEmpty) and IsNullableType(@lValue) then
    lValue := GetNullableTValueAsTValue(@lValue, fTokens[lIdx].Value1);
  lValueOwned := False;
  if fTokens[lIdx].Ref1 > 0 then
    ApplyFilters(lIdx, lValue, fTokens[lIdx].Ref1, fTokens[aSwitchIdx].Value1, lValueOwned, lValueIsHTML);
  try
    if (not lValue.IsEmpty) and lValue.IsType<Boolean> then
      lValue := Ord(lValue.AsBoolean);
    lBranch := fTokens[aSwitchIdx].Ref2;
    while fTokens[lBranch].TokenType = ttCase do
    begin
      lMatched := False;
      lParIdx := lBranch + 1;
      while fTokens[lParIdx].TokenType = ttFilterParameter do
      begin
        if not lMatched then
        begin
          lParam := ResolveFilterParameter(fTokens[lParIdx]);
          if (lParam.ParType = fptVariable) and (SameText(lParam.ParStrText, 'true') or SameText(lParam.ParStrText, 'false')) then
            lParam := ValueToFilterParameter(SameText(lParam.ParStrText, 'true'));
          // same semantics as the "eq" filter; a null value matches nothing
          lMatched := ComparandOperator(ctEQ, lValue, [lParam], fLocaleFormatSettings).AsBoolean;
        end;
        Inc(lParIdx);
      end;
      if lMatched then
        Exit(lParIdx);
      lBranch := fTokens[lBranch].Ref2;
    end;
    Result := lBranch + 1; // body of default, or the token after endswitch
  finally
    if lValueOwned then
      lValue.AsObject.Free;
  end;
end;

function TTProCompiledTemplate.LastFilterToken(const aIdx: Int64): Int64;
// the last token of the filters that follow a macro parameter token (Ref1 = filter count, -1 = none)
var
  J: Integer;
begin
  Result := aIdx;
  for J := 1 to fTokens[aIdx].Ref1 do
  begin
    Inc(Result); // ttFilterName, Ref1 = its parameter count
    Inc(Result, fTokens[Result].Ref1);
  end;
end;

function TTProCompiledTemplate.MacroArgument(const aTokenIdx: Int64; const aParameter: TFilterParameter;
  var aOwned: TArray<TObject>): TValue;
// the value of a macro argument (or default) with the filters that follow its token; objects created
// by the filters are added to aOwned, to be freed after the macro
var
  lIdx: Int64;
  lOwned, lIsHTML: Boolean;
begin
  Result := MacroArgumentValue(aParameter);
  if fTokens[aTokenIdx].Ref1 <= 0 then
    Exit;
  if (not Result.IsEmpty) and IsNullableType(@Result) then
    Result := GetNullableTValueAsTValue(@Result, aParameter.ParStrText);
  lIdx := aTokenIdx;
  ApplyFilters(lIdx, Result, fTokens[aTokenIdx].Ref1, 'macro argument', lOwned, lIsHTML);
  if lOwned then
    aOwned := aOwned + [Result.AsObject];
end;

function TTProCompiledTemplate.MacroArgumentValue(const aParameter: TFilterParameter): TValue;
begin
  if (aParameter.ParType = fptVariable) and SameText(aParameter.ParStrText, 'true') then
    Result := True
  else if (aParameter.ParType = fptVariable) and SameText(aParameter.ParStrText, 'false') then
    Result := False
  else if aParameter.ParType = fptVariable then
    Result := GetVarAsTValue(aParameter.ParStrText) // objects and lists are passed as they are
  else
    Result := GetParameterValue(aParameter);
end;

procedure TTProCompiledTemplate.ExecuteMacro(const CallTokenIndex: Int64; const aBuff: TStringBuilder);
var
  lFrame: TTProSlotFrame;
  lSavedFrame: Integer;
begin
  Inc(fRenderNestingDepth);
  try
    if fRenderNestingDepth > MAX_RENDER_NESTING then
      Error(Format('Template nesting too deep (max %d levels of macro calls and dynamic includes)', [MAX_RENDER_NESTING]));
    lFrame.CallTokenIndex := CallTokenIndex;
    lFrame.CallerVariables := GetVariables; // never nil: a slot can {{set}} in the caller's scope
    lFrame.CallerLoops := fLoopsStack;
    lFrame.ParentFrame := fCurrentSlotFrame;
    fSlotFrames.Add(lFrame);
    lSavedFrame := fCurrentSlotFrame;
    fCurrentSlotFrame := fSlotFrames.Count - 1;
    try
      ExecuteMacroBody(CallTokenIndex, aBuff);
    finally
      fCurrentSlotFrame := lSavedFrame;
      fSlotFrames.Delete(fSlotFrames.Count - 1);
    end;
  finally
    Dec(fRenderNestingDepth);
  end;
end;

function TTProCompiledTemplate.IsBlankRange(const aFrom, aTo: Int64): Boolean;
// only whitespace and line breaks (the named slots, when the range is a call body, do not count)
var
  I: Int64;
begin
  I := aFrom;
  while I < aTo do
  begin
    case fTokens[I].TokenType of
      ttFill:
        I := fTokens[I].Ref2;
      ttLineBreak:
        ;
      ttContent:
        if not fTokens[I].Value1.Trim.IsEmpty then
          Exit(False);
    else
      Exit(False);
    end;
    Inc(I);
  end;
  Result := True;
end;

function TTProCompiledTemplate.FindSlotContent(const aCallIdx: Int64; const aSlotName: string; out aFrom, aTo: Int64): Boolean;
// the tokens of a filled slot. Fill names are resolved in the current scope: call it in the caller's one.
var
  lFill: Int64;
begin
  Result := False;
  aFrom := -1;
  aTo := -1;
  if fTokens[aCallIdx].Ref2 < 0 then
    Exit; // {{>macro()}}: no body
  if SameText(aSlotName, 'default') then
  begin
    aFrom := aCallIdx + 1 + fTokens[aCallIdx].Ref1; // after the call parameters
    aTo := fTokens[aCallIdx].Ref2;
  end
  else
  begin
    lFill := fTokens[fTokens[aCallIdx].Ref2].Ref2; // the last fill, then back to the first
    while (lFill > -1) and not SameText(ResolveTokenName(fTokens[lFill]), aSlotName) do
      lFill := fTokens[lFill].Ref1;
    if lFill = -1 then
      Exit;
    aFrom := lFill + 1;
    aTo := fTokens[lFill].Ref2;
  end;
  Result := not IsBlankRange(aFrom, aTo);
end;

function TTProCompiledTemplate.SlotsInfo(const aCallIdx: Int64): TObject;
var
  lFill, lFrom, lTo: Int64;
  lName: string;
  lSlots: TJDOJsonObject;
begin
  lSlots := TJDOJsonObject.Create;
  Result := lSlots;
  try
    lSlots.B['default'] := FindSlotContent(aCallIdx, 'default', lFrom, lTo);
    lFill := fTokens[fTokens[aCallIdx].Ref2].Ref2;
    while lFill > -1 do
    begin
      lName := ResolveTokenName(fTokens[lFill]);
      lSlots.B[lName] := FindSlotContent(aCallIdx, lName, lFrom, lTo);
      lFill := fTokens[lFill].Ref1;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TTProCompiledTemplate.RenderSlot(const aSlotIdx: Int64; const aBuff: TStringBuilder): Int64;
// {{slot "name"}}: renders the content the caller gave for that slot, in the caller's scope.
// Returns the last token handled: the slot itself (its fallback, if any, follows) or its endslot.
var
  lName: string;
  lFrame: TTProSlotFrame;
  lFrom, lTo: Int64;
  lSavedVariables: TTProVariables;
  lSavedLoops: TObjectList<TLoopStackItem>;
  lSavedFrame: Integer;
begin
  Result := aSlotIdx;
  lName := ResolveTokenName(fTokens[aSlotIdx]); // in the macro's scope
  lFrame := fSlotFrames[fCurrentSlotFrame];
  lSavedVariables := fVariables;
  lSavedLoops := fLoopsStack;
  lSavedFrame := fCurrentSlotFrame;
  fVariables := lFrame.CallerVariables;
  fLoopsStack := lFrame.CallerLoops;
  fCurrentSlotFrame := lFrame.ParentFrame; // a {{slot}} inside the content refers to the caller's own slots
  Inc(fRenderNestingDepth);
  try
    if FindSlotContent(lFrame.CallTokenIndex, lName, lFrom, lTo) then
    begin
      if fRenderNestingDepth > MAX_RENDER_NESTING then
        Error(Format('Template nesting too deep (max %d levels of macro calls and dynamic includes)', [MAX_RENDER_NESTING]));
      RenderRange(aBuff, lFrom, lTo);
      if fTokens[aSlotIdx].Ref2 > -1 then
        Result := fTokens[aSlotIdx].Ref2; // filled: skip the fallback
    end;
  finally
    Dec(fRenderNestingDepth);
    fCurrentSlotFrame := lSavedFrame;
    fLoopsStack := lSavedLoops;
    fVariables := lSavedVariables;
  end;
end;

procedure TTProCompiledTemplate.ExecuteMacroBody(const CallTokenIndex: Int64; const aBuff: TStringBuilder);
// the macro writes straight into the caller's output, so that {{stack}} positions stay absolute
var
  lMacroName: String;
  lMacroDef: TMacroDefinition;
  lCallParamCount: Integer;
  lCallParams: TArray<TValue>;
  lBound: TArray<Boolean>;
  lParamIndex: Integer;
  I, J: Integer;
  lSavedVariables: TTProVariables;
  lSavedLoops: TObjectList<TLoopStackItem>;
  lParamToken: TToken;
  lSlots: TJDOJsonObject;
  lIdx, lArgIdx: Int64;
  lOwned: TArray<TObject>;
  lObj: TObject;
begin
  // Get macro name and parameters from call
  lMacroName := fTokens[CallTokenIndex].Value1;
  lCallParamCount := fTokens[CallTokenIndex].Ref1;

  // Find macro definition
  if not fMacros.TryGetValue(lMacroName.ToLower, lMacroDef) then
  begin
    Error('Macro "' + lMacroName + '" not defined');
  end;

  // Bind the arguments (positional first, then named) to the parameters.
  // Everything - defaults included - is evaluated here, in the caller's scope.
  SetLength(lCallParams, Length(lMacroDef.Parameters));
  SetLength(lBound, Length(lMacroDef.Parameters));
  lOwned := [];
  lSlots := nil;
  lSavedVariables := fVariables;
  try
    // lCallParamCount counts the tokens of the arguments and of their filters
    I := -1;
    lIdx := CallTokenIndex + 1;
    while lIdx <= CallTokenIndex + lCallParamCount do
    begin
      Inc(I);
      lArgIdx := lIdx;
      lIdx := LastFilterToken(lIdx) + 1;
      lParamToken := fTokens[lArgIdx];
      lParamIndex := I; // positional
      if not lParamToken.Value2.IsEmpty then
      begin
        lParamIndex := -1;
        for J := 0 to High(lMacroDef.Parameters) do
          if SameText(lMacroDef.Parameters[J].Name, lParamToken.Value2) then
            lParamIndex := J;
        if lParamIndex = -1 then
          Error('Unknown parameter "%s" for macro "%s"', [lParamToken.Value2, lMacroName]);
        if lBound[lParamIndex] then
          Error('Parameter "%s" passed twice to macro "%s"', [lParamToken.Value2, lMacroName]);
      end;
      if lParamIndex > High(lMacroDef.Parameters) then
        Continue; // extra positional arguments are ignored (as in 1.1)
      lCallParams[lParamIndex] := MacroArgument(lArgIdx, TokenToFilterParameter(lParamToken), lOwned);
      lBound[lParamIndex] := True;
    end;
    for I := 0 to High(lMacroDef.Parameters) do
      if not lBound[I] then
      begin
        if not lMacroDef.Parameters[I].HasDefault then
          Error('Missing required parameter "' + lMacroDef.Parameters[I].Name + '" for macro "' + lMacroName + '"');
        lCallParams[I] := MacroArgument(lMacroDef.Parameters[I].TokenIndex, lMacroDef.Parameters[I].DefaultValue, lOwned);
      end;

    // slots.<name>: which slots the caller filled (fill names are resolved in the caller's scope)
    if fTokens[CallTokenIndex].Ref2 > -1 then
      lSlots := TJDOJsonObject(SlotsInfo(CallTokenIndex));

    // new scope for the macro (the caller's one is restored in finally)
    fVariables := TTProVariables.Create;
    try
      // Set macro parameters as variables in new scope
      // Use SetData to properly detect type (object, list, JSON, etc.)
      for I := 0 to High(lMacroDef.Parameters) do
        SetData(lMacroDef.Parameters[I].Name, lCallParams[I]);
      if lSlots <> nil then
        SetData('slots', TValue.From<TJDOJsonObject>(lSlots))
      else
        SetData('slots', TValue.Empty);

      // Execute macro body: the full renderer, with the macro's own loops (the caller's ones are not visible)
      lSavedLoops := fLoopsStack;
      fLoopsStack := TObjectList<TLoopStackItem>.Create(True);
      try
        RenderRange(aBuff, lMacroDef.BeginTokenIndex, lMacroDef.EndTokenIndex);
      finally
        fLoopsStack.Free;
        fLoopsStack := lSavedLoops;
      end;
    finally
      fVariables.Free;
    end;
  finally
    fVariables := lSavedVariables;
    lSlots.Free;
    for lObj in lOwned do
      lObj.Free;
  end;
end;

{ Expression Evaluator Integration }

function TTProCompiledTemplate.TValueToVariant(const Value: TValue): Variant;
begin
  if Value.IsEmpty then
    Result := Null
  else if Value.Kind = tkInteger then
  begin
    if IsUnsignedInteger(Value.TypeInfo) then
      Result := Int64(Value.AsType<Cardinal>)
    else
      Result := Value.AsInteger;
  end
  else if Value.Kind = tkInt64 then
  begin
    if IsUnsignedInt64(Value.TypeInfo) then
      Result := Value.AsType<UInt64>
    else
      Result := Value.AsInt64;
  end
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
  else if SameText(PropName, 'Hidden') then Result := False // as the @@fields metadata of objects
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

function TTProCompiledTemplate.GetExprEvaluator: TExprEvaluator;
begin
  if fExprEvaluator = nil then
  begin
    fExprEvaluator := TExprEvaluator.Create;
    fExprEvaluator.SetOnResolveExternalVariable(
      function(const VarName: string; out Value: Variant): Boolean
      var
        lTValue: TValue;
      begin
        try
          lTValue := Self.GetVarAsTValue(VarName);
          if not lTValue.IsEmpty then
          begin
            Value := Self.TValueToVariant(lTValue);
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
  lEval: TExprEvaluator;
  lResult: Variant;
begin
  lEval := GetExprEvaluator;
  try
    lResult := lEval.Evaluate(Expression);
  except
    on E: ETProException do
      raise;
    on E: Exception do
      raise ETProRenderException.CreateFmt('Error evaluating expression [%s]: %s', [Expression, E.Message]);
  end;
  Result := VariantToTValue(lResult);
end;

initialization

GlContext := TRttiContext.Create;
JsonSerializationConfig.LineBreak := sLineBreak;

finalization

GlContext.Free;

end.
