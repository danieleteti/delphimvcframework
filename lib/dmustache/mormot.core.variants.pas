/// Framework Core Low-Level Variants / TDocVariant process
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.variants;

{
  *****************************************************************************

  Variant / TDocVariant feature shared by all framework units
  - Low-Level Variant Wrappers
  - Custom Variant Types with JSON support
  - TDocVariant Object/Array Document Holder with JSON support
  - IDocList/IDocDict advanced Wrappers of TDocVariant Documents
  - JSON Parsing into Variant
  - Variant Binary Serialization

  *****************************************************************************
}

interface

{$I \mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.data, // already included in mormot.core.json
  mormot.core.buffers,
  mormot.core.rtti,
  mormot.core.json;


{ ************** Low-Level Variant Wrappers }

type
  /// exception class raised by this unit during raw Variant process
  ESynVariant = class(ESynException);

const
  {$ifdef HASVARUSTRING}
  varFirstCustom = varUString + 1;
  {$else}
  varFirstCustom = varAny + 1;
  {$endif HASVARUSTRING}

/// fastcheck if a variant hold a value
// - varEmpty, varNull or a '' string would be considered as void
// - varBoolean=false or varDate=0 would be considered as void
// - a TDocVariantData with Count=0 would be considered as void
// - any other value (e.g. floats or integer) would be considered as not void
function VarIsVoid(const V: Variant): boolean;

/// returns a supplied string as variant, or null if v is void ('')
function VarStringOrNull(const v: RawUtf8): variant;

type
  /// a set of simple TVarData.VType, as specified to VarIs()
  TVarDataTypes = set of 0..255;

/// allow to check for a specific set of TVarData.VType
function VarIs(const V: Variant; const VTypes: TVarDataTypes): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// same as Dest := Source, but copying by reference
// - i.e. VType is defined as varVariant or varByRef / varVariantByRef
// - for instance, it will be used for late binding of TDocVariant properties,
// to let following statements work as expected:
// ! V := _Json('{arr:[1,2]}');
// ! V.arr.Add(3);   // will work, since V.arr will be returned by reference
// ! writeln(V);     // will write '{"arr":[1,2,3]}'
procedure SetVariantByRef(const Source: Variant; var Dest: Variant);

/// same as Dest := Source, but copying by value
// - will unreference any varByRef content
// - will convert any string value into RawUtf8 (varString) for consistency
procedure SetVariantByValue(const Source: Variant; var Dest: Variant);

/// same as FillChar(Value^,SizeOf(TVarData),0)
// - so can be used for TVarData or Variant
// - it will set V.VType := varEmpty, so Value will be Unassigned
// - it won't call VarClear(variant(Value)): it should have been cleaned before
procedure ZeroFill(Value: PVarData);
  {$ifdef HASINLINE}inline;{$endif}

/// fill all bytes of the value's memory buffer with zeros, i.e. 'toto' -> #0#0#0#0
// - may be used to cleanup stack-allocated content
procedure FillZero(var value: variant); overload;

/// convert an UTF-8 encoded text buffer into a variant RawUtf8 varString
// - this overloaded version expects a destination variant type (e.g. varString
// varOleStr / varUString) - if the type is not handled, will raise an
// EVariantTypeCastError
procedure RawUtf8ToVariant(const Txt: RawUtf8; var Value: TVarData;
  ExpectedValueType: cardinal); overload;

/// convert an open array (const Args: array of const) argument to a variant
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
// - vt*String or vtVariant arguments are returned as varByRef
procedure VarRecToVariant(const V: TVarRec; var result: variant); overload;

/// convert an open array (const Args: array of const) argument to a variant
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
// - vt*String or vtVariant arguments are returned as varByRef
function VarRecToVariant(const V: TVarRec): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a variant to an open array (const Args: array of const) argument
// - variant is accessed by reference as vtVariant so should remain available
procedure VariantToVarRec(const V: variant; var result: TVarRec);
  {$ifdef HASINLINE}inline;{$endif}

/// convert a variant array to open array (const Args: array of const) arguments
// - variants are accessed by reference as vtVariant so should remain available
procedure VariantsToArrayOfConst(const V: array of variant; VCount: PtrInt;
  out result: TTVarRecDynArray); overload;

/// convert a variant array to open array (const Args: array of const) arguments
// - variants are accessed by reference as vtVariant so should remain available
function VariantsToArrayOfConst(const V: array of variant): TTVarRecDynArray; overload;

/// convert an array of RawUtf8 to open array (const Args: array of const) arguments
// - RawUtf8 are accessed by reference as vtAnsiString so should remain available
function RawUtf8DynArrayToArrayOfConst(const V: array of RawUtf8): TTVarRecDynArray;

/// convert any Variant into a RTL string type
// - expects any varString value to be stored as a RawUtf8
// - prior to Delphi 2009, use VariantToString(aVariant) instead of
// string(aVariant) to safely retrieve a string=AnsiString value from a variant
// generated by our framework units - otherwise, you may loose encoded characters
// - for Unicode versions of Delphi, there won't be any potential data loss,
// but this version may be slightly faster than a string(aVariant)
function VariantToString(const V: Variant): string; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any Variant into a RTL string type
procedure VariantToString(const V: Variant; var result: string); overload;

/// convert a dynamic array of variants into its JSON serialization
// - will use a TDocVariantData temporary storage
function VariantDynArrayToJson(const V: TVariantDynArray): RawUtf8;

/// convert a dynamic array of variants into its text values
function VariantDynArrayToRawUtf8DynArray(const V: TVariantDynArray): TRawUtf8DynArray;

/// convert a JSON array into a dynamic array of variants
// - will use a TDocVariantData temporary storage
function JsonToVariantDynArray(const Json: RawUtf8): TVariantDynArray;

/// convert an open array list into a dynamic array of variants
// - will use a TDocVariantData temporary storage
function ValuesToVariantDynArray(const items: array of const): TVariantDynArray;

type
  /// function prototype used internally for variant comparison
  // - as used e.g. by TDocVariantData.SortByValue
  TVariantCompare = function(const V1, V2: variant): PtrInt;
  /// function prototype used internally for extended variant comparison
  // - as used by TDocVariantData.SortByRow
  TVariantComparer = function(const V1, V2: variant): PtrInt of object;
  /// function prototype used internally for extended variant comparison
  // - as used by TDocVariantData.SortArrayByFields
  TVariantCompareField = function(const FieldName: RawUtf8;
    const V1, V2: variant): PtrInt of object;

/// internal function as called by inlined VariantCompare/VariantCompareI and
// the SortDynArrayVariantComp() function overriden by this unit
function FastVarDataComp(A, B: PVarData; caseInsensitive: boolean): integer;

/// TVariantCompare-compatible case-sensitive comparison function
// - just a wrapper around FastVarDataComp(caseInsensitive=false)
function VariantCompare(const V1, V2: variant): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}

/// TVariantCompare-compatible case-insensitive comparison function
// - just a wrapper around FastVarDataComp(caseInsensitive=true)
function VariantCompareI(const V1, V2: variant): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}

/// fast comparison of a Variant and UTF-8 encoded String (or number)
// - slightly faster than plain V=Str, which computes a temporary variant
// - here Str='' equals unassigned (varEmpty), null or false
// - if CaseSensitive is false, will use PropNameEquals() for comparison
function VariantEquals(const V: Variant; const Str: RawUtf8;
  CaseSensitive: boolean = true): boolean; overload;

/// return the TVarData.VType as text, e.g. 'Integer', 'String' or 'DocVariant'
function VariantTypeName(V: PVarData): PShortString; overload;

/// return the variant VType as text, e.g. 'Integer', 'String' or 'DocVariant'
function VariantTypeName(const V: variant): PShortString; overload;
  {$ifdef HASINLINE}inline;{$endif}


{ ************** Custom Variant Types with JSON support }

type
  /// define how our custom variant types behave, i.e. its methods featureset
  TSynInvokeableVariantTypeOptions = set of (
    sioHasTryJsonToVariant,
    sioHasToJson,
    sioCanIterate);

  /// custom variant handler with easier/faster access of variant properties,
  // and JSON serialization support
  // - default GetProperty/SetProperty methods are called via some protected
  // virtual IntGet/IntSet methods, with less overhead (to be overriden)
  // - these kind of custom variants will be faster than the default
  // TInvokeableVariantType for properties getter/setter, but you should
  // manually register each type by calling SynRegisterCustomVariantType()
  // - also feature custom JSON parsing, via TryJsonToVariant() protected method
  TSynInvokeableVariantType = class(TInvokeableVariantType)
  protected
    fOptions: TSynInvokeableVariantTypeOptions;
    {$ifdef ISDELPHI}
    /// our custom call backs do not want the function names to be uppercased
    function FixupIdent(const AText: string): string; override;
    {$endif ISDELPHI}
    // intercept for a faster direct IntGet/IntSet calls
    // - note: SetProperty/GetProperty are never called by this class/method
    // - also circumvent FPC 3.2+ inverted parameters order
    {$ifdef FPC_VARIANTSETVAR}
    procedure DispInvoke(Dest: PVarData; var Source: TVarData;
      CallDesc: PCallDesc; Params: Pointer); override;
    {$else} // see http://mantis.freepascal.org/view.php?id=26773
    {$ifdef ISDELPHIXE7}
    procedure DispInvoke(Dest: PVarData; [ref] const Source: TVarData;
      CallDesc: PCallDesc; Params: Pointer); override;
    {$else}
    procedure DispInvoke(Dest: PVarData; const Source: TVarData;
      CallDesc: PCallDesc; Params: Pointer); override;
    {$endif ISDELPHIXE7}
    {$endif FPC_VARIANTSETVAR}
  public
    /// virtual constructor which should set the custom type Options
    constructor Create; virtual;
    /// search of a registered custom variant type from its low-level VarType
    // - will first compare with its own VarType for efficiency
    // - returns true and set the matching CustomType if found, false otherwise
    function FindSynVariantType(aVarType: cardinal;
      out CustomType: TSynInvokeableVariantType): boolean; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// search of a registered custom variant type from its low-level VarType
    // - will first compare with its own VarType for efficiency
    function FindSynVariantType(aVarType: cardinal): TSynInvokeableVariantType; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// customization of JSON parsing into variants
    // - is enabled only if the sioHasTryJsonToVariant option is set
    // - will be called by e.g. by VariantLoadJson() or GetVariantFromJsonField()
    // with Options: PDocVariantOptions parameter not nil
    // - this default implementation will always returns FALSE,
    // meaning that the supplied JSON is not to be handled by this custom
    // (abstract) variant type
    // - this method could be overridden to identify any custom JSON content
    // and convert it into a dedicated variant instance, then return TRUE
    // - warning: should NOT modify JSON buffer in-place, unless it returns true
    function TryJsonToVariant(var Json: PUtf8Char; var Value: variant;
      EndOfObject: PUtf8Char): boolean; virtual;
    /// customization of variant into JSON serialization
    procedure ToJson(W: TJsonWriter; Value: PVarData); overload; virtual;
    /// save a variant as UTF-8 encoded JSON
    // - implemented as a wrapper around ToJson()
    procedure ToJson(Value: PVarData; var Json: RawUtf8;
      const Prefix: RawUtf8 = ''; const Suffix: RawUtf8 = '';
      Format: TTextWriterJsonFormat = jsonCompact); overload; virtual;
    /// clear the content
    // - this default implementation will set VType := varEmpty
    // - override it if your custom type needs to manage its internal memory
    procedure Clear(var V: TVarData); override;
    /// compare two items - this overriden method will redirect to Compare()
    // - Delphi RTL does this redirection, where FPC does not (but should)
    function CompareOp(const Left, Right: TVarData;
      const Operation: TVarOp): boolean; override;
    /// compare two items - overriden method calling case-sensitive IntCompare()
    procedure Compare(const Left, Right: TVarData;
      var Relationship: TVarCompareResult); override;
    /// copy two variant content
    // - this default implementation will copy the TVarData memory
    // - override it if your custom type needs to manage its internal structure
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: boolean); override;
    /// copy two variant content by value
    // - this default implementation will call the Copy() method
    // - override it if your custom types may use a by reference copy pattern
    procedure CopyByValue(var Dest: TVarData;
      const Source: TVarData); virtual;
    /// this method will allow to look for dotted name spaces, e.g. 'parent.child'
    // - return Unassigned (varEmpty) if the FullName does not match any value
    // - will identify TDocVariant storage, or resolve and call the generic
    // TSynInvokeableVariantType.IntGet() method until nested value match
    // - you can set e.g. PathDelim = '/' to search e.g. for 'parent/child'
    procedure Lookup(var Dest: TVarData; const Instance: TVarData;
      FullName: PUtf8Char; PathDelim: AnsiChar = '.');
    /// will check if the value is an array, and return the number of items
    // - if the document is an array, will return the items count (0 meaning
    // void array) - used e.g. by TSynMustacheContextVariant
    // - this default implementation will return -1 (meaning this is not an array)
    // - overridden method could implement it, e.g. for TDocVariant of kind
    // dvArray - or dvObject (ignoring names) if GetObjectAsValues is true
    function IterateCount(const V: TVarData; GetObjectAsValues: boolean): integer; virtual;
    /// allow to loop over an array document
    // - Index should be in 0..IterateCount-1 range
    // - this default implementation will do nothing
    procedure Iterate(var Dest: TVarData; const V: TVarData;
      Index: integer); virtual;
    /// returns TRUE if the supplied variant is of the exact custom type
    function IsOfType(const V: variant): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns TRUE if the supplied custom variant is void
    // - e.g. returns true for a TDocVariant or TBsonVariant with Count = 0
    // - caller should have ensured that it is of the exact custom type
    function IsVoid(const V: TVarData): boolean; virtual;
    /// override this abstract method for actual getter by name implementation
    function IntGet(var Dest: TVarData; const Instance: TVarData;
      Name: PAnsiChar; NameLen: PtrInt; NoException: boolean): boolean; virtual;
    /// override this abstract method for actual setter by name implementation
    function IntSet(const Instance, Value: TVarData;
      Name: PAnsiChar; NameLen: PtrInt): boolean; virtual;
    /// override this method if default VariantCompAsText() call is not optimal
    function IntCompare(const Instance, Another: TVarData;
      CaseInsensitive: boolean): integer; virtual;
    /// identify how this custom type behave
    // - as set by the class constructor, to avoid calling any virtual method
    property Options: TSynInvokeableVariantTypeOptions
      read fOptions;
  end;

  /// class-reference type (metaclass) of custom variant type definition
  // - used by SynRegisterCustomVariantType() function
  TSynInvokeableVariantTypeClass = class of TSynInvokeableVariantType;

var
  /// internal list of our TSynInvokeableVariantType instances
  // - SynVariantTypes[0] is always DocVariantVType
  // - SynVariantTypes[1] is e.g. BsonVariantType from mormot.db.nosql.bson
  // - instances are owned by Variants.pas as TInvokeableVariantType instances
  // - is defined here for proper FindSynVariantType inlining
  SynVariantTypes: array of TSynInvokeableVariantType;

/// register a custom variant type to handle properties
// - the registration process is thread-safe
// - this will implement an internal mechanism used to bypass the default
// _DispInvoke() implementation in Variant.pas, to use a faster version
// - is called in case of TDocVariant, TBsonVariant or TSqlDBRowVariant
function SynRegisterCustomVariantType(
  aClass: TSynInvokeableVariantTypeClass): TSynInvokeableVariantType;

/// search of a registered custom variant type from its low-level VarType
// - returns the matching custom variant type, nil if not found
function FindSynVariantType(aVarType: cardinal): TSynInvokeableVariantType;
  {$ifdef HASINLINE}inline;{$endif}

/// try to serialize a custom variant value into JSON
// - as used e.g. by TJsonWriter.AddVariant
function CustomVariantToJson(W: TJsonWriter; Value: PVarData;
  Escape: TTextWriterKind): boolean;

/// low-level conversion of a Compare() result to TCustomVariantType.Compare
function SortCompTo(cmp: integer): TVarCompareResult;
  {$ifdef HASINLINE}inline;{$endif}


{ ************** TDocVariant Object/Array Document Holder with JSON support }

type
  /// JSON_[] constant convenient TDocVariant options
  // - mVoid defines a safe (and slow) full-copy behavior with [] (no option)
  // - mDefault defines a safe (and slow) full-copy behavior, returning null
  // for unknown fields, as defined e.g. by _Json() and _JsonFmt() functions
  // or JSON_OPTIONS[false]
  // - mFast will copy-by-reference any TDocVariantData content, as defined
  // e.g. by _JsonFast() and _JsonFastFmt() functions or JSON_OPTIONS[true]
  // - mFastFloat will copy-by-reference and can parse floating points as double
  // - mFastStrict will copy-by-reference and only parse strict (quoted) JSON,
  // as defined by JSON_FAST_STRICT global variable
  // - mFastExtended will copy-by-reference and write extended (unquoted) JSON,
  // as defined by JSON_FAST_EXTENDED global variable
  // - mFastExtendedIntern will copy-by-reference, write extended JSON and
  // intern names and values, as defined by JSON_FAST_EXTENDEDINTERN variable
  // - mNameValue will copy-by-reference and check field names case-sensitively,
  // as defined by JSON_NAMEVALUE[false] global variable
  // - mNameValueExtended will copy-by-reference, check field names
  // case-sensitively and write extended (unquoted) JSON,
  // as defined by JSON_NAMEVALUE[true] global variable
  // - mNameValueIntern will copy-by-reference, check field names
  // case-sensitively and intern names and values,
  // as defined by JSON_NAMEVALUEINTERN[false] global variable
  // - mNameValueInternExtended will copy-by-reference, check field names
  // case-sensitively, write extended JSON and intern names and values,
  // as defined by JSON_NAMEVALUEINTERN[true] global variable
  TDocVariantModel = (
    mVoid,
    mDefault,
    mFast,
    mFastFloat,
    mFastStrict,
    mFastExtended,
    mFastExtendedIntern,
    mNameValue,
    mNameValueExtended,
    mNameValueIntern,
    mNameValueInternExtended);

var
  /// some convenient TDocVariant options, e.g. as JSON_[fDefault]
  JSON_: array[TDocVariantModel] of TDocVariantOptions = (
    // mVoid
    [],
    // mDefault
    [dvoReturnNullForUnknownProperty],
    // mFast
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference],
    // mFastFloat
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference,
     dvoAllowDoubleValue],
    // mFastStrict
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference,
     dvoJsonParseDoNotTryCustomVariants],
    // mFastExtended
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference,
     dvoSerializeAsExtendedJson],
    // mFastExtendedIntern
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference,
     dvoSerializeAsExtendedJson,
     dvoJsonParseDoNotTryCustomVariants,
     dvoInternNames,
     dvoInternValues],
    // mNameValue
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference,
     dvoNameCaseSensitive],
    // mNameValueExtended
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference,
     dvoNameCaseSensitive,
     dvoSerializeAsExtendedJson],
    // mNameValueIntern
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference,
     dvoNameCaseSensitive,
     dvoInternNames,
     dvoInternValues],
    // mNameValueInternExtended
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference,
     dvoNameCaseSensitive,
     dvoInternNames,
     dvoInternValues,
     dvoSerializeAsExtendedJson]
    );

const
  /// same as JSON_[mFast], but can not be used as PDocVariantOptions
  // - handle only currency for floating point values: use JSON_FAST_FLOAT
  // if you want to support double values, with potential precision loss
  JSON_FAST =
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference];

  /// same as JSON_FAST, but including dvoAllowDoubleValue for floating
  // point values parsing into double, with potential precision loss
  JSON_FAST_FLOAT =
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference,
     dvoAllowDoubleValue];

var
  /// TDocVariant options which may be used for plain JSON parsing
  // - this won't recognize any extended syntax
  JSON_FAST_STRICT: TDocVariantOptions;

  /// TDocVariant options to be used so that JSON serialization would
  // use the unquoted JSON syntax for field names
  // - you could use it e.g. on a TOrm variant published field to
  // reduce the JSON escape process during storage in the database, by
  // customizing your TOrmModel instance:
  // !  (aModel.Props[TOrmMyRecord]['VariantProp'] as TOrmPropInfoRttiVariant).
  // !    DocVariantOptions := JSON_FAST_EXTENDED;
  // or - in a cleaner way - by overriding TOrm.InternalDefineModel():
  // ! class procedure TOrmMyRecord.InternalDefineModel(Props: TOrmProperties);
  // ! begin
  // !   (Props.Fields.ByName('VariantProp') as TOrmPropInfoRttiVariant).
  // !     DocVariantOptions := JSON_FAST_EXTENDED;
  // ! end;
  // or to set all variant fields at once:
  // ! class procedure TOrmMyRecord.InternalDefineModel(Props: TOrmProperties);
  // ! begin
  // !   Props.SetVariantFieldsDocVariantOptions(JSON_FAST_EXTENDED);
  // ! end;
  // - consider using JSON_NAMEVALUE[true] for case-sensitive
  // TSynNameValue-like storage, or JSON_FAST_EXTENDEDINTERN if you
  // expect RawUtf8 names and values interning
  JSON_FAST_EXTENDED: TDocVariantOptions;

  /// TDocVariant options for JSON serialization with efficient storage
  // - i.e. unquoted JSON syntax for field names and RawUtf8 interning
  // - may be used e.g. for efficient persistence of similar data
  // - consider using JSON_FAST_EXTENDED if you don't expect
  // RawUtf8 names and values interning, or need BSON variants parsing
  JSON_FAST_EXTENDEDINTERN: TDocVariantOptions;

  /// TDocVariant options to be used for case-sensitive TSynNameValue-like
  // storage, with optional extended JSON syntax serialization
  // - consider using JSON_FAST_EXTENDED for case-insensitive objects
  JSON_NAMEVALUE: TDocVariantOptionsBool;

  /// TDocVariant options to be used for case-sensitive TSynNameValue-like
  // storage, RawUtf8 interning and optional extended JSON syntax serialization
  // - consider using JSON_FAST_EXTENDED for case-insensitive objects,
  // or JSON_NAMEVALUE[] if you don't expect names and values interning
  JSON_NAMEVALUEINTERN: TDocVariantOptionsBool;

  // - JSON_OPTIONS[false] is e.g. _Json() and _JsonFmt() functions default
  // - JSON_OPTIONS[true] are used e.g. by _JsonFast() and _JsonFastFmt() functions
  // - handle only currency for floating point values: use JSON_FAST_FLOAT/JSON_[mFastFloat]
  // if you want to support double values, with potential precision loss
  JSON_OPTIONS: TDocVariantOptionsBool;

// some slightly more verbose backward compatible options
{$ifndef PUREMORMOT2}
  JSON_OPTIONS_FAST_STRICT: TDocVariantOptions
    absolute JSON_FAST_STRICT;
  JSON_OPTIONS_NAMEVALUE: TDocVariantOptionsBool
    absolute JSON_NAMEVALUE;
  JSON_OPTIONS_NAMEVALUEINTERN: TDocVariantOptionsBool
    absolute JSON_NAMEVALUEINTERN;
  JSON_OPTIONS_FAST_EXTENDED: TDocVariantOptions
    absolute JSON_FAST_EXTENDED;
  JSON_OPTIONS_FAST_EXTENDEDINTERN: TDocVariantOptions
    absolute JSON_FAST_EXTENDEDINTERN;

const
  JSON_OPTIONS_FAST = JSON_FAST;
  JSON_OPTIONS_FAST_FLOAT = JSON_FAST_FLOAT;
{$endif PUREMORMOT2}


type
  /// pointer to a TDocVariant storage
  // - since variants may be stored by reference (i.e. as varByRef), it may
  // be a good idea to use such a pointer via DocVariantData(aVariant)^ or
  // _Safe(aVariant)^ instead of TDocVariantData(aVariant),
  // if you are not sure how aVariant was allocated (may be not _Obj/_Json)
  // - note: due to a local variable lifetime change in Delphi 11, don't use
  // this function with a temporary variant (e.g. from TList<variant>.GetItem) -
  // call _DV() and a local TDocVariantData instead of a PDocVariantData
  PDocVariantData = ^TDocVariantData;

  /// pointer to a dynamic array of TDocVariant storage
  PDocVariantDataDynArray = array of PDocVariantData;

  /// define the TDocVariant storage layout
  // - if it has no name property, it is a dvArray
  // - if it has one or more named properties, it is a dvObject
  TDocVariantKind = (
    dvUndefined,
    dvArray,
    dvObject);

  /// exception class associated to TDocVariant JSON/BSON document
  EDocVariant = class(ESynException)
  protected
    class procedure RaiseSafe(Kind: TDocVariantKind);
  end;

  /// a custom variant type used to store any JSON/BSON document-based content
  // - i.e. name/value pairs for objects, or an array of values (including
  // nested documents), stored in a TDocVariantData memory structure
  // - you can use _Obj()/_ObjFast() _Arr()/_ArrFast() _Json()/_JsonFast() or
  // _JsonFmt()/_JsonFastFmt() functions to create instances of such variants
  // - property access may be done via late-binding - with some restrictions
  // for older versions of FPC, e.g. allowing to write:
  // ! TDocVariant.NewFast(aVariant);
  // ! aVariant.Name := 'John';
  // ! aVariant.Age := 35;
  // ! writeln(aVariant.Name,' is ',aVariant.Age,' years old');
  // - it also supports a small set of pseudo-properties or pseudo-methods:
  // ! aVariant._Count = DocVariantData(aVariant).Count
  // ! aVariant._Kind = ord(DocVariantData(aVariant).Kind)
  // ! aVariant._JSON = DocVariantData(aVariant).JSON
  // ! aVariant._(i) = DocVariantData(aVariant).Value[i]
  // ! aVariant.Value(i) = DocVariantData(aVariant).Value[i]
  // ! aVariant.Value(aName) = DocVariantData(aVariant).Value[aName]
  // ! aVariant.Name(i) = DocVariantData(aVariant).Name[i]
  // ! aVariant.Add(aItem) = DocVariantData(aVariant).AddItem(aItem)
  // ! aVariant._ := aItem = DocVariantData(aVariant).AddItem(aItem)
  // ! aVariant.Add(aName,aValue) = DocVariantData(aVariant).AddValue(aName,aValue)
  // ! aVariant.Exists(aName) = DocVariantData(aVariant).GetValueIndex(aName)>=0
  // ! aVariant.Delete(i) = DocVariantData(aVariant).Delete(i)
  // ! aVariant.Delete(aName) = DocVariantData(aVariant).Delete(aName)
  // ! aVariant.NameIndex(aName) = DocVariantData(aVariant).GetValueIndex(aName)
  // - it features direct JSON serialization/unserialization, e.g.:
  // ! assert(_Json('["one",2,3]')._JSON='["one",2,3]');
  // - it features direct trans-typing into a string encoded as JSON, e.g.:
  // ! assert(_Json('["one",2,3]')='["one",2,3]');
  TDocVariant = class(TSynInvokeableVariantType)
  protected
    /// name and values interning are shared among all TDocVariantData instances
    fInternNames, fInternValues: TRawUtf8Interning;
    fInternSafe: TLightLock; // just protect TRawUtf8Interning initialization
    function CreateInternNames: TRawUtf8Interning;
    function CreateInternValues: TRawUtf8Interning;
  public
    /// initialize a variant instance to store some document-based content
    // - by default, every internal value will be copied, so access of nested
    // properties can be slow - if you expect the data to be read-only or not
    // propagated into another place, set aOptions=[dvoValueCopiedByReference]
    // will increase the process speed a lot
    class procedure New(out aValue: variant;
      aOptions: TDocVariantOptions = []); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize a variant instance to store per-reference document-based content
    // - same as New(aValue, JSON_FAST);
    // - to be used e.g. as
    // !var
    // !  v: variant;
    // !begin
    // !  TDocVariant.NewFast(v);
    // !  ...
    class procedure NewFast(out aValue: variant;
      aKind: TDocVariantKind = dvUndefined); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// ensure a variant is a TDocVariant instance
    // - if aValue is not a TDocVariant, will create a new JSON_FAST
    class procedure IsOfTypeOrNewFast(var aValue: variant);
    /// initialize several variant instances to store document-based content
    // - replace several calls to TDocVariantData.InitFast
    // - to be used e.g. as
    // !var
    // !  v1, v2, v3: TDocVariantData;
    // !begin
    // !  TDocVariant.NewFast([@v1,@v2,@v3]);
    // !  ...
    class procedure NewFast(const aValues: array of PDocVariantData;
      aKind: TDocVariantKind = dvUndefined); overload;
    /// initialize a variant instance to store some document-based content
    // - you can use this function to create a variant, which can be nested into
    // another document, e.g.:
    // ! aVariant := TDocVariant.New;
    // ! aVariant.id := 10;
    // - by default, every internal value will be copied, so access of nested
    // properties can be slow - if you expect the data to be read-only or not
    // propagated into another place, set Options=[dvoValueCopiedByReference]
    // will increase the process speed a lot
    // - in practice, you should better use _Obj()/_ObjFast() _Arr()/_ArrFast()
    // functions or TDocVariant.NewFast()
    class function New(Options: TDocVariantOptions = []): variant; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize a variant instance to store some document-based object content
    // - object will be initialized with data supplied two by two, as Name,Value
    // pairs, e.g.
    // ! aVariant := TDocVariant.NewObject(['name','John','year',1972]);
    // which is the same as:
    // ! TDocVariant.New(aVariant);
    // ! TDocVariantData(aVariant).AddValue('name','John');
    // ! TDocVariantData(aVariant).AddValue('year',1972);
    // - by default, every internal value will be copied, so access of nested
    // properties can be slow - if you expect the data to be read-only or not
    // propagated into another place, set Options=[dvoValueCopiedByReference]
    // will increase the process speed a lot
    // - in practice, you should better use the function _Obj() which is a
    // wrapper around this class method
    class function NewObject(const NameValuePairs: array of const;
      Options: TDocVariantOptions = []): variant;
    /// initialize a variant instance to store some document-based array content
    // - array will be initialized with data supplied as parameters, e.g.
    // ! aVariant := TDocVariant.NewArray(['one',2,3.0]);
    // which is the same as:
    // ! TDocVariant.New(aVariant);
    // ! TDocVariantData(aVariant).AddItem('one');
    // ! TDocVariantData(aVariant).AddItem(2);
    // ! TDocVariantData(aVariant).AddItem(3.0);
    // - by default, every internal value will be copied, so access of nested
    // properties can be slow - if you expect the data to be read-only or not
    // propagated into another place, set aOptions=[dvoValueCopiedByReference]
    // will increase the process speed a lot
    // - in practice, you should better use the function _Arr() which is a
    // wrapper around this class method
    class function NewArray(const Items: array of const;
      Options: TDocVariantOptions = []): variant; overload;
    /// initialize a variant instance to store some document-based array content
    // - array will be initialized with data supplied dynamic array of variants
    class function NewArray(const Items: TVariantDynArray;
      Options: TDocVariantOptions = []): variant; overload;
    /// initialize a variant instance to store some document-based object content
    // from a supplied (extended) JSON content
    // - in addition to the JSON RFC specification strict mode, this method will
    // handle some BSON-like extensions, e.g. unquoted field names
    // - a private copy of the incoming JSON buffer will be used, then
    // it will call the TDocVariantData.InitJsonInPlace() method
    // - to be used e.g. as:
    // ! var V: variant;
    // ! begin
    // !   V := TDocVariant.NewJson('{"id":10,"doc":{"name":"John","birthyear":1972}}');
    // !   assert(V.id=10);
    // !   assert(V.doc.name='John');
    // !   assert(V.doc.birthYear=1972);
    // !   // and also some pseudo-properties:
    // !   assert(V._count=2);
    // !   assert(V.doc._kind=ord(dvObject));
    // - or with a JSON array:
    // !   V := TDocVariant.NewJson('["one",2,3]');
    // !   assert(V._kind=ord(dvArray));
    // !   for i := 0 to V._count-1 do
    // !     writeln(V._(i));
    // - by default, every internal value will be copied, so access of nested
    // properties can be slow - if you expect the data to be read-only or not
    // propagated into another place, add dvoValueCopiedByReference in Options
    // will increase the process speed a lot
    // - in practice, you should better use the function _Json()/_JsonFast()
    // which are handy wrappers around this class method
    class function NewJson(const Json: RawUtf8;
      Options: TDocVariantOptions = [dvoReturnNullForUnknownProperty]): variant;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize a variant instance to store some document-based object content
    // from a supplied existing TDocVariant instance
    // - use it on a value returned as varByRef (e.g. by _() pseudo-method),
    // to ensure the returned variant will behave as a stand-alone value
    // - for instance, the following:
    // !  oSeasons := TDocVariant.NewUnique(o.Seasons);
    // is the same as:
    // ! 	oSeasons := o.Seasons;
    // !  _Unique(oSeasons);
    // or even:
    // !  oSeasons := _Copy(o.Seasons);
    class function NewUnique(const SourceDocVariant: variant;
      Options: TDocVariantOptions = [dvoReturnNullForUnknownProperty]): variant;
      {$ifdef HASINLINE}inline;{$endif}
    /// will return the unique element of a TDocVariant array or a default
    // - if the value is a dvArray with one single item, returns this value
    // - if the value is not a TDocVariant nor a dvArray with one single item,
    // it wil return the default value
    class procedure GetSingleOrDefault(const docVariantArray, default: variant;
      var result: variant);

    /// finalize the stored information
    destructor Destroy; override;
    /// used by dvoInternNames for string interning of all Names[] values
    function InternNames: TRawUtf8Interning;
      {$ifdef HASINLINE}inline;{$endif}
    /// used by dvoInternValues for string interning of all RawUtf8 Values[]
    function InternValues: TRawUtf8Interning;
      {$ifdef HASINLINE}inline;{$endif}
    // this implementation will write the content as JSON object or array
    procedure ToJson(W: TJsonWriter; Value: PVarData); override;
    /// will check if the value is an array, and return the number of items
    // - if the document is an array, will return the items count (0 meaning
    // void array) - used e.g. by TSynMustacheContextVariant
    // - this overridden method will implement it for dvArray instance kind
    function IterateCount(const V: TVarData;
      GetObjectAsValues: boolean): integer; override;
    /// allow to loop over an array document
    // - Index should be in 0..IterateCount-1 range
    // - this default implementation will do handle dvArray instance kind
    procedure Iterate(var Dest: TVarData; const V: TVarData;
      Index: integer); override;
    /// returns true if this document has Count = 0
    function IsVoid(const V: TVarData): boolean; override;
    /// low-level callback to access internal pseudo-methods
    // - mainly the _(Index: integer): variant method to retrieve an item
    // if the document is an array
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): boolean; override;
    /// low-level callback to access internal pseudo-methods
    function DoProcedure(const V: TVarData; const Name: string;
      const Arguments: TVarDataArray): boolean; override;
    /// low-level callback to clear the content
    procedure Clear(var V: TVarData); override;
    /// low-level callback to copy two variant content
    // - such copy will by default be done by-value, for safety
    // - if you are sure you will use the variants as read-only, you can set
    // the dvoValueCopiedByReference Option to use faster by-reference copy
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: boolean); override;
    /// copy two variant content by value
    // - overridden method since instance may use a by-reference copy pattern
    procedure CopyByValue(var Dest: TVarData; const Source: TVarData); override;
    /// handle type conversion
    // - only types processed by now are string/OleStr/UnicodeString/date
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    /// handle type conversion
    // - only types processed by now are string/OleStr/UnicodeString/date
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;
    /// overriden method for actual getter by name implementation
    function IntGet(var Dest: TVarData; const Instance: TVarData;
      Name: PAnsiChar; NameLen: PtrInt; NoException: boolean): boolean; override;
    /// overriden method for actual setter by name implementation
    function IntSet(const Instance, Value: TVarData;
      Name: PAnsiChar; NameLen: PtrInt): boolean; override;
    /// overriden method redirecting to TDocVariantData.Compare()
    function IntCompare(const Instance, Another: TVarData;
      CaseInsensitive: boolean): integer; override;
  end;

  /// method used by TDocVariantData.ReduceAsArray to filter each object
  // - should return TRUE if the item match the expectations
  TOnReducePerItem = function(Item: PDocVariantData): boolean of object;

  /// method used by TDocVariantData.ReduceAsArray to filter each object
  // - should return TRUE if the item match the expectations
  TOnReducePerValue = function(const Value: variant): boolean of object;

  {$ifdef HASITERATORS}
  /// internal state engine used by TDocVariant enumerators records
  TDocVariantEnumeratorState = record
  private
    Curr, After: PVariant;
  public
    procedure Init(Values: PVariantArray; Count: PtrUInt); inline;
    procedure Void; inline;
    function MoveNext: boolean; inline;
  end;

  /// local iterated name/value pair as returned by TDocVariantData.GetEnumerator
  // and TDocVariantData.Fields
  // - we use pointers for best performance - but warning: Name may be nil for
  // TDocVariantData.GetEnumerator over an array
  TDocVariantFields = record
    /// points to current Name[] - nil if the TDocVariantData is an array
    Name: PRawUtf8;
    /// points to the current Value[] - never nil
    Value: PVariant;
  end;

  /// low-level Enumerator as returned by TDocVariantData.GetEnumerator
  // (default "for .. in dv do") and TDocVariantData.Fields
  TDocVariantFieldsEnumerator = record
  private
    State: TDocVariantEnumeratorState;
    Name: PRawUtf8;
    function GetCurrent: TDocVariantFields; inline;
  public
    function MoveNext: boolean; inline;
    function GetEnumerator: TDocVariantFieldsEnumerator; inline;
    /// returns the current Name/Value or Value as pointers in TDocVariantFields
    property Current: TDocVariantFields
      read GetCurrent;
  end;

  /// low-level Enumerator as returned by TDocVariantData.FieldNames
  TDocVariantFieldNamesEnumerator = record
  private
    Curr, After: PRawUtf8;
  public
    function MoveNext: boolean; inline;
    function GetEnumerator: TDocVariantFieldNamesEnumerator; inline;
    /// returns the current Name/Value or Value as pointers in TDocVariantFields
    property Current: PRawUtf8
      read Curr;
  end;

  /// low-level Enumerator as returned by TDocVariantData.Items and FieldValues
  TDocVariantItemsEnumerator = record
  private
    State: TDocVariantEnumeratorState;
  public
    function MoveNext: boolean; inline;
    function GetEnumerator: TDocVariantItemsEnumerator; inline;
    /// returns the current Value as pointer
    property Current: PVariant
      read State.Curr;
  end;

  /// low-level Enumerator as returned by TDocVariantData.Objects
  TDocVariantObjectsEnumerator = record
  private
    State: TDocVariantEnumeratorState;
    Value: PDocVariantData;
  public
    function MoveNext: boolean; {$ifdef HASSAFEINLINE} inline; {$endif}
    function GetEnumerator: TDocVariantObjectsEnumerator; inline;
    /// returns the current Value as pointer to each TDocVariantData object
    property Current: PDocVariantData
      read Value;
  end;
  {$endif HASITERATORS}

  /// how duplicated values could be searched
  TSearchDuplicate = (
    sdNone,
    sdCaseSensitive,
    sdCaseInsensitive);

  {$A-} { packet object not allowed since Delphi 2009 :( }
  /// memory structure used for TDocVariant storage of any JSON/BSON
  // document-based content as variant
  // - i.e. name/value pairs for objects, or an array of values (including
  // nested documents)
  // - you can use _Obj()/_ObjFast() _Arr()/_ArrFast() _Json()/_JsonFast() or
  // _JsonFmt()/_JsonFastFmt() functions to create instances of such variants
  // - you can transtype such an allocated variant into TDocVariantData
  // to access directly its internals (like Count or Values[]/Names[]):
  // ! aVariantObject := TDocVariant.NewObject(['name','John','year',1972]);
  // ! aVariantObject := _ObjFast(['name','John','year',1972]);
  // ! with _Safe(aVariantObject)^ do
  // !   for i := 0 to Count-1 do
  // !     writeln(Names[i],'=',Values[i]); // for an object
  // ! aVariantArray := TDocVariant.NewArray(['one',2,3.0]);
  // ! aVariantArray := _JsonFast('["one",2,3.0]');
  // ! with _Safe(aVariantArray)^ do
  // !   for i := 0 to Count-1 do
  // !     writeln(Values[i]); // for an array
  // - use "with _Safe(...)^ do"  and not "with TDocVariantData(...) do" as the
  // former will handle internal variant redirection (varByRef), e.g. from late
  // binding or assigned another TDocVariant
  // - Delphi "object" is buggy on stack -> also defined as record with methods
  {$ifdef USERECORDWITHMETHODS}
  TDocVariantData = record
  {$else}
  TDocVariantData = object
  {$endif USERECORDWITHMETHODS}
  private
    // note: this structure uses all TVarData available space: no filler needed!
    VType: TVarType;              // 16-bit
    VOptions: TDocVariantOptions; // 16-bit
    VCount: integer;              // 32-bit
    VName: TRawUtf8DynArray;      // pointer
    VValue: TVariantDynArray;     // pointer
    // retrieve the value as varByRef
    function GetValueOrItem(const aNameOrIndex: variant): variant;
    procedure SetValueOrItem(const aNameOrIndex, aValue: variant);
    // kind is stored as dvoIsArray/dvoIsObject within VOptions
    function GetKind: TDocVariantKind;
      {$ifdef HASINLINE}inline;{$endif}
    function Has(dvo: TDocVariantOption): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    procedure Include(dvo: TDocVariantOption);
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetOptions(const opt: TDocVariantOptions); // keep dvoIsObject/Array
      {$ifdef HASINLINE}inline;{$endif}
    // capacity is Length(VValue) and Length(VName)
    procedure SetCapacity(aValue: integer);
    function GetCapacity: integer;
      {$ifdef HASINLINE}inline;{$endif}
    // implement U[] I[] B[] D[] O[] O_[] A[] A_[] _[] properties
    function GetOrAddIndexByName(const aName: RawUtf8): integer;
      {$ifdef HASINLINE}inline;{$endif}
    function GetOrAddPVariantByName(const aName: RawUtf8): PVariant;
    function GetPVariantByName(const aName: RawUtf8): PVariant;
    function GetRawUtf8ByName(const aName: RawUtf8): RawUtf8;
    procedure SetRawUtf8ByName(const aName, aValue: RawUtf8);
    function GetStringByName(const aName: RawUtf8): string;
    procedure SetStringByName(const aName: RawUtf8; const aValue: string);
    function GetInt64ByName(const aName: RawUtf8): Int64;
    procedure SetInt64ByName(const aName: RawUtf8; const aValue: Int64);
    function GetBooleanByName(const aName: RawUtf8): boolean;
    procedure SetBooleanByName(const aName: RawUtf8; aValue: boolean);
    function GetDoubleByName(const aName: RawUtf8): Double;
    procedure SetDoubleByName(const aName: RawUtf8; const aValue: Double);
    function GetDocVariantExistingByName(const aName: RawUtf8;
      aNotMatchingKind: TDocVariantKind): PDocVariantData;
    function GetObjectExistingByName(const aName: RawUtf8): PDocVariantData;
    function GetDocVariantOrAddByName(const aName: RawUtf8;
      aKind: TDocVariantKind): PDocVariantData;
    function GetObjectOrAddByName(const aName: RawUtf8): PDocVariantData;
    function GetArrayExistingByName(const aName: RawUtf8): PDocVariantData;
    function GetArrayOrAddByName(const aName: RawUtf8): PDocVariantData;
    function GetAsDocVariantByIndex(aIndex: integer): PDocVariantData;
    function GetVariantByPath(const aNameOrPath: RawUtf8): Variant;
      {$ifdef HASINLINE}inline;{$endif}
    function GetObjectProp(const aName: RawUtf8; out aFound: PVariant;
      aPreviousIndex: PInteger): boolean;
    function InternalAddBuf(aName: PUtf8Char; aNameLen: integer): integer;
    procedure InternalSetValue(aIndex: PtrInt; const aValue: variant);
      {$ifdef HASINLINE}inline;{$endif}
    procedure InternalSetVarRec(aIndex: PtrInt; const aValue: TVarRec);
      {$ifdef HASSAFEINLINE}inline;{$endif}
    procedure InternalUniqueValueAt(aIndex: PtrInt);
    function InternalNextPath(var aCsv: PUtf8Char; aName: PShortString;
      aPathDelim: AnsiChar): PtrInt;
      {$ifdef FPC}inline;{$endif}
    procedure InternalNotFound(var Dest: variant; aName: PUtf8Char); overload;
    procedure InternalNotFound(var Dest: variant; aIndex: integer); overload;
    function InternalNotFound(aName: PUtf8Char): PVariant; overload;
    function InternalNotFound(aIndex: integer): PDocVariantData; overload;
    function RangeVoid(var Offset, Limit: integer): boolean;
    procedure ClearFast;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// initialize a TDocVariantData to store some document-based content
    // - can be used with a stack-allocated TDocVariantData variable:
    // !var
    // !  Doc: TDocVariantData; // stack-allocated variable
    // !begin
    // !  Doc.Init;
    // !  Doc.AddValue('name','John');
    // !  assert(Doc.Value['name']='John');
    // !  assert(variant(Doc).name='John');
    // !end;
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure Init(const aOptions: TDocVariantOptions = []); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize a TDocVariantData to store a content of some known type
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure Init(const aOptions: TDocVariantOptions;
      aKind: TDocVariantKind); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize a TDocVariantData to store some document-based content
    // - use the options corresponding to the supplied TDocVariantModel
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure Init(aModel: TDocVariantModel;
      aKind: TDocVariantKind = dvUndefined); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize a TDocVariantData to store per-reference document-based content
    // - same as Doc.Init(JSON_FAST);
    // - can be used with a stack-allocated TDocVariantData variable:
    // !var
    // !  Doc: TDocVariantData; // stack-allocated variable
    // !begin
    // !  Doc.InitFast;
    // !  Doc.AddValue('name','John');
    // !  assert(Doc.Value['name']='John');
    // !  assert(variant(Doc).name='John');
    // !end;
    // - see also TDocVariant.NewFast() if you want to initialize several
    // TDocVariantData variable instances at once
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitFast(aKind: TDocVariantKind = dvUndefined); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize a TDocVariantData to store per-reference document-based content
    // - this overloaded method allows to specify an estimation of how many
    // properties or items this aKind document would contain
    procedure InitFast(InitialCapacity: integer; aKind: TDocVariantKind); overload;
    /// initialize a TDocVariantData to store document-based object content
    // - object will be initialized with data supplied two by two, as Name,Value
    // pairs, e.g.
    // !var
    // !  Doc: TDocVariantData; // stack-allocated variable
    // !begin
    // !  Doc.InitObject(['name','John','year',1972]);
    // which is the same as:
    // ! var Doc: TDocVariantData;
    // !begin
    // !  Doc.Init;
    // !  Doc.AddValue('name','John');
    // !  Doc.AddValue('year',1972);
    // - this method is called e.g. by _Obj() and _ObjFast() global functions
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitObject(const NameValuePairs: array of const;
      aOptions: TDocVariantOptions = []); overload;
    /// initialize a TDocVariantData to store document-based object content
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitObject(const NameValuePairs: array of const;
      Model: TDocVariantModel); overload;
    /// initialize a variant instance to store some document-based array content
    // - array will be initialized with data supplied as parameters, e.g.
    // !var
    // !  Doc: TDocVariantData; // stack-allocated variable
    // !begin
    // !  Doc.InitArray(['one',2,3.0]);
    // !  assert(Doc.Count=3);
    // !end;
    // which is the same as:
    // ! var Doc: TDocVariantData;
    // !     i: integer;
    // !begin
    // !  Doc.Init;
    // !  Doc.AddItem('one');
    // !  Doc.AddItem(2);
    // !  Doc.AddItem(3.0);
    // !  assert(Doc.Count=3);
    // !  for i := 0 to Doc.Count-1 do
    // !    writeln(Doc.Value[i]);
    // !end;
    // - this method is called e.g. by _Arr() and _ArrFast() global functions
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitArray(const aItems: array of const;
      aOptions: TDocVariantOptions = []); overload;
    /// initialize a variant instance to store some document-based array content
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitArray(const aItems: array of const;
      aModel: TDocVariantModel); overload;
    /// initialize a variant instance to store some document-based array content
    // - array will be initialized with data supplied as variant dynamic array
    // - if Items is [], the variant will be set as null
    // - will be almost immediate, since TVariantDynArray is reference-counted,
    // unless ItemsCopiedByReference is set to FALSE
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitArrayFromVariants(const aItems: TVariantDynArray;
      aOptions: TDocVariantOptions = [];
      aItemsCopiedByReference: boolean = true; aCount: integer = -1);
    /// initialize a variant array instance from an object Values[]
    procedure InitArrayFromObjectValues(const aObject: variant;
      aOptions: TDocVariantOptions = []; aItemsCopiedByReference: boolean = true);
    /// initialize a variant array instance from an object Names[]
    procedure InitArrayFromObjectNames(const aObject: variant;
      aOptions: TDocVariantOptions = []; aItemsCopiedByReference: boolean = true);
    /// initialize a variant instance from some 'a,b,c' CSV one-line content
    // - is by default separator tolerant, i.e. will detect ',' ';' or #9 in text
    procedure InitArrayFromCsv(const aCsv: RawUtf8;
      aOptions: TDocVariantOptions; aSeparator: AnsiChar = #0;
      aTrimItems: boolean = false; aAddVoidItems: boolean = false;
      aQuote: AnsiChar = #0);
    /// initialize a variant instance from a CSV file content with header
    // - stored objects names will be retrieved from the first CSV line
    // - is by default separator tolerant, i.e. will detect ',' ';' or #9 in text
    procedure InitArrayFromCsvFile(const aCsv: RawUtf8;
      aOptions: TDocVariantOptions; aSeparator: AnsiChar = #0;
      aQuote: AnsiChar = #0);
    /// create a TDocVariant array, from a sub-range of this document array
    // - returned variant instance is a dvArray containing only the specified rows
    // $ new.InitArrayFrom(src) returns a copy of src array
    // $ new.InitArrayFrom(src, 10) returns items 10..Count-1 of src
    // $ new.InitArrayFrom(src, 0, 10) returns first 0..9 items of src
    // $ new.InitArrayFrom(src, 10, 20) returns items 10..29 - truncated if Count<30
    // $ new.InitArrayFrom(src, -10) returns last Count-10..Count-1 items of src
    procedure InitArrayFrom(const aSource: TDocVariantData;
      aOptions: TDocVariantOptions; aOffset: integer = 0; aLimit: integer = 0); overload;
    /// initialize a variant instance to store some RawUtf8 array content
    procedure InitArrayFrom(const aItems: TRawUtf8DynArray;
      aOptions: TDocVariantOptions; aCount: integer = -1); overload;
    /// initialize a variant instance to store some 32-bit integer array content
    procedure InitArrayFrom(const aItems: TIntegerDynArray;
      aOptions: TDocVariantOptions; aCount: integer = -1); overload;
    /// initialize a variant instance to store some 64-bit integer array content
    procedure InitArrayFrom(const aItems: TInt64DynArray;
      aOptions: TDocVariantOptions; aCount: integer = -1); overload;
    /// initialize a variant instance to store some double array content
    procedure InitArrayFrom(const aItems: TDoubleDynArray;
      aOptions: TDocVariantOptions; aCount: integer = -1); overload;
    /// initialize a variant instance to store some dynamic array content
    procedure InitArrayFrom(var aItems; ArrayInfo: PRttiInfo;
      aOptions: TDocVariantOptions; ItemsCount: PInteger = nil); overload;
    /// initialize a variant instance to store some TDynArray content
    procedure InitArrayFrom(const aItems: TDynArray;
      aOptions: TDocVariantOptions = JSON_FAST_FLOAT); overload;
    /// initialize a variant instance to store a T*ObjArray content
    // - will call internally ObjectToVariant() to make the conversion
    procedure InitArrayFromObjArray(const ObjArray; aOptions: TDocVariantOptions;
      aWriterOptions: TTextWriterWriteObjectOptions = [woDontStoreDefault];
      aCount: integer = -1);
    /// fill a TDocVariant array from standard or non-expanded JSON ORM/DB result
    // - accept the ORM/DB results dual formats as recognized by TOrmTableJson,
    // i.e. both [{"f1":"1v1","f2":1v2},{"f2":"2v1","f2":2v2}...] and
    // {"fieldCount":2,"values":["f1","f2","1v1",1v2,"2v1",2v2...],"rowCount":20}
    // - about 2x (expanded) or 3x (non-expanded) faster than Doc.InitJsonInPlace()
    // - will also use less memory, because all object field names will be shared
    // - in expanded mode, the fields order won't be checked, as with TOrmTableJson
    // - warning: the incoming JSON buffer will be modified in-place: so you should
    // make a private copy before running this method, as overloaded procedures do
    // - some numbers on a Core i5-13500, extracted from our regression tests:
    // $ TDocVariant InitJsonInPlace in 72.91ms i.e. 2.1M rows/s, 268.8 MB/s
    // $ TDocVariant InitJsonInPlace no guess in 69.49ms i.e. 2.2M rows/s, 282 MB/s
    // $ TDocVariant InitJsonInPlace dvoIntern in 68.41ms i.e. 2.2M rows/s, 286.5 MB/s
    // $ TDocVariant FromResults exp in 31.69ms i.e. 4.9M rows/s, 618.6 MB/s
    // $ TDocVariant FromResults not exp in 24.48ms i.e. 6.4M rows/s, 352.1 MB/s
    function InitArrayFromResults(Json: PUtf8Char; JsonLen: PtrInt;
      aOptions: TDocVariantOptions = JSON_FAST_FLOAT): boolean; overload;
    /// fill a TDocVariant array from standard or non-expanded JSON ORM/DB result
    // - accept the ORM/DB results dual formats as recognized by TOrmTableJson
    // - about 2x (expanded) or 3x (non-expanded) faster than Doc.InitJson()
    // - will also use less memory, because all object field names will be shared
    // - in expanded mode, the fields order won't be checked, as with TOrmTableJson
    // - a private copy of the incoming JSON buffer will be used before parsing
    function InitArrayFromResults(const Json: RawUtf8;
      aOptions: TDocVariantOptions = JSON_FAST_FLOAT): boolean; overload;
    /// fill a TDocVariant array from standard or non-expanded JSON ORM/DB result
    // - accept the ORM/DB results dual formats as recognized by TOrmTableJson
    // - about 2x (expanded) or 3x (non-expanded) faster than Doc.InitJson()
    // - will also use less memory, because all object field names will be shared
    // - in expanded mode, the fields order won't be checked, as with TOrmTableJson
    // - a private copy of the incoming JSON buffer will be used before parsing
    function InitArrayFromResults(const Json: RawUtf8;
      aModel: TDocVariantModel): boolean; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// initialize a variant instance to store some document-based object content
    // - object will be initialized with names and values supplied as dynamic arrays
    // - if aNames and aValues are [] or do have matching sizes, the variant
    // will be set as null
    // - will be almost immediate, since Names and Values are reference-counted
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitObjectFromVariants(const aNames: TRawUtf8DynArray;
       const aValues: TVariantDynArray; aOptions: TDocVariantOptions = []);
    /// initialize a variant instance to store a document-based object with a
    // single property
    // - the supplied path could be 'Main.Second.Third', to create nested
    // objects, e.g. {"Main":{"Second":{"Third":value}}}
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitObjectFromPath(const aPath: RawUtf8; const aValue: variant;
      aOptions: TDocVariantOptions = []; aPathDelim: AnsiChar = '.');
    /// initialize a variant instance to store some document-based object content
    // from a supplied JSON array or JSON object content
    // - warning: the incoming JSON buffer will be modified in-place: so you should
    // make a private copy before running this method, as InitJson() does
    // - this method is called e.g. by _JsonFmt() _JsonFastFmt() global functions
    // with a temporary JSON buffer content created from a set of parameters
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    // - consider the faster InitArrayFromResults() from ORM/SQL JSON results
    function InitJsonInPlace(Json: PUtf8Char;
      aOptions: TDocVariantOptions = []; aEndOfObject: PUtf8Char = nil): PUtf8Char;
    /// initialize a variant instance to store some document-based object content
    // from a supplied JSON array or JSON object content
    // - a private copy of the incoming JSON buffer will be used, then
    // it will call the other overloaded InitJsonInPlace() method
    // - this method is called e.g. by _Json() and _JsonFast() global functions
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    // - handle only currency for floating point values: set JSON_FAST_FLOAT
    // or dvoAllowDoubleValue option to support double, with potential precision loss
    // - consider the faster InitArrayFromResults() from ORM/SQL JSON results
    function InitJson(const Json: RawUtf8;
      aOptions: TDocVariantOptions = []): boolean; overload;
    /// initialize a variant instance to store some document-based object content
    // from a supplied JSON array or JSON object content
    // - use the options corresponding to the supplied TDocVariantModel
    // - a private copy of the incoming JSON buffer will be made
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    // - handle only currency for floating point values unless you set mFastFloat
    // - consider the faster InitArrayFromResults() from ORM/SQL JSON results
    function InitJson(const Json: RawUtf8; aModel: TDocVariantModel): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize a variant instance to store some document-based object content
    // from a file containing some JSON array or JSON object
    // - file may have been serialized using the SaveToJsonFile() method
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    // - handle only currency for floating point values: set JSON_FAST_FLOAT
    // or dvoAllowDoubleValue option to support double, with potential precision loss
    // - will assume text file with no BOM is already UTF-8 encoded
    function InitJsonFromFile(const FileName: TFileName;
      aOptions: TDocVariantOptions = []): boolean;
    /// ensure a document-based variant instance will have one unique options set
    // - this will create a copy of the supplied TDocVariant instance, forcing
    // all nested events to have the same set of Options
    // - you can use this function to ensure that all internal properties of this
    // variant will be copied e.g. per-reference (if you set JSON_[mDefault])
    // or per-value (if you set JSON_[mDefault]) whatever options the nested
    // objects or arrays were created with
    // - will raise an EDocVariant if the supplied variant is not a TDocVariant
    // - you may rather use _Unique() or _UniqueFast() wrappers if you want to
    // ensure that a TDocVariant instance is unique
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitCopy(const SourceDocVariant: variant;
      aOptions: TDocVariantOptions);
    /// clone a document-based variant with the very same options but no data
    // - the same options will be used, without the dvArray/dvObject flags
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitClone(const CloneFrom: TDocVariantData);
      {$ifdef HASINLINE}inline;{$endif}
    /// low-level copy a document-based variant with the very same options and count
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    // - will copy Count and Names[] by reference, but Values[] only if CloneValues
    // - returns the first item in Values[]
    function InitFrom(const CloneFrom: TDocVariantData; CloneValues: boolean;
      MakeUnique: boolean = false): PVariant;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize a variant instance to store some document-based object content
    // from a supplied name=value list of UTF-8 encoded text (e.g. .ini file)
    // - the supplied content may have been generated by ToTextPairs() method
    // - if ItemSep=#10, then any kind of line feed (CRLF or LF) will be handled
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitFromPairs(aPairs: PUtf8Char; aOptions: TDocVariantOptions;
      NameValueSep: AnsiChar = '='; ItemSep: AnsiChar = #10;
      DoTrim: boolean = true); overload;
    /// initialize a variant instance to store some document-based object content
    // from a supplied name=value list of UTF-8 encoded text (e.g. .ini file)
    // - the supplied content may have been generated by ToTextPairs() method
    // - if ItemSep = #10, then any kind of line feed (CRLF or LF) will be handled
    // - if you call Init*() methods in a row, ensure you call Clear in-between
    procedure InitFromPairs(const aPairs: RawUtf8; aOptions: TDocVariantOptions;
      NameValueSep: AnsiChar = '='; ItemSep: AnsiChar = #10;
      DoTrim: boolean = true); overload;
       {$ifdef HASINLINE}inline;{$endif}

    /// to be called before any Init*() method call, when a previous Init*()
    // has already be performed on the same instance, to avoid memory leaks
    // - for instance:
    // !var
    // !  Doc: TDocVariantData; // stack-allocated variable
    // !begin
    // !  Doc.InitArray(['one',2,3.0]); // no need of any Doc.Clear here
    // !  assert(Doc.Count=3);
    // !  Doc.Clear; // to release memory before following InitObject()
    // !  Doc.InitObject(['name','John','year',1972]);
    // !end;
    // - will check the VType, and call ClearFast private method
    procedure Clear;
    /// delete all internal stored values
    // - like Clear + Init() with the same options
    // - will reset Kind to dvUndefined
    procedure Reset;
    /// keep the current Options and Kind, but reset all data and VCount to 0
    procedure Void;
    /// fill all Values[] with #0, then delete all values
    // - could be used to specifically remove sensitive information from memory
    procedure FillZero;
    /// check if the Document is an object - i.e. Kind = dvObject
    function IsObject: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// check if the Document is an array - i.e. Kind = dvArray
    function IsArray: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// check if names lookups are case sensitive in this object Document
    function IsCaseSensitive: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// guess the TDocVariantModel corresponding to the current document Options
    // - returns true if model has been found and set
    // - returns false if no JSON_[] matches the current options
    function GetModel(out model: TDocVariantModel): boolean;
    /// low-level method to force a number of items
    // - could be used to fast add items to the internal Values[]/Names[] arrays
    // - just set protected VCount field, do not resize the arrays: caller
    // should ensure that Capacity is big enough and to call Void if aCount=0
    procedure SetCount(aCount: integer);
      {$ifdef HASINLINE}inline;{$endif}
    /// efficient comparison of two TDocVariantData content
    // - will return the same result than JSON comparison, but more efficiently
    function Compare(const Another: TDocVariantData;
      CaseInsensitive: boolean = false): integer; overload;
    /// efficient comparison of two TDocVariantData objects
    // - will always ensure that both this instance and Another are Objects
    // - will compare all supplied Fields values in their specified order
    // - if ObjFields is void, will fallback to regular Compare()
    function CompareObject(const ObjFields: array of RawUtf8;
      const Another: TDocVariantData; CaseInsensitive: boolean = false): integer;
    /// efficient equality comparison of two TDocVariantData content
    // - just a wrapper around Compare(Another)=0
    function Equals(const Another: TDocVariantData;
      CaseInsensitive: boolean = false): boolean; overload;
      {$ifdef HASSAFEINLINE}inline;{$endif}
    /// compare a TTDocVariantData object property with a given value
    // - returns -1 if this instance is not a dvObject or has no aName property
    function Compare(const aName: RawUtf8; const aValue: variant;
      aCaseInsensitive: boolean = false): integer; overload;
      {$ifdef ISDELPHI}{$ifdef HASINLINE}inline;{$endif}{$endif}
    /// efficient equality comparison a TTDocVariantData object property
    function Equals(const aName: RawUtf8; const aValue: variant;
      aCaseInsensitive: boolean = false): boolean; overload;
      {$ifdef ISDELPHI}{$ifdef HASINLINE}inline;{$endif}{$endif}
    /// low-level method called internally to reserve place for new values
    // - returns the index of the newly created item in Values[]/Names[] arrays
    // - you should not have to use it, unless you want to add some items
    // directly within the Values[]/Names[] arrays, using e.g.
    // InitFast(InitialCapacity) to initialize the document
    // - if aName='', append a dvArray item, otherwise append a dvObject field
    // - you can specify an optional aIndex value to Insert instead of Add
    // - warning: FPC optimizer is confused by Values[InternalAdd(name)] so
    // you should call InternalAdd() in an explicit previous step
    function InternalAdd(const aName: RawUtf8; aIndex: integer = -1): integer; 
    {$ifdef HASITERATORS}
    /// an enumerator able to compile "for .. in dv do" statements
    // - returns pointers over all Names[] and Values[]
    // - warning: if the document is an array, returned Name is nil:
    // ! var e: TDocVariantFields;
    // ! ...
    // !    dv.InitArray([1, 3, 3, 4]);
    // !    for e in dv do
    // !      // here e.Name = nil
    // !      writeln(e.Value^);
    // ! // output  1  2  3  4
    function GetEnumerator: TDocVariantFieldsEnumerator;
    /// an enumerator able to compile "for .. in dv.Fields do" for objects
    // - returns pointers over all Names[] and Values[]
    // - don't iterate if the document is an array - so Name is never nil:
    // ! var e: TDocVariantFields;
    // ! ...
    // !   dv.InitJson('{a:1,b:2,c:3}');
    // !   for e in dv.Fields do
    // !     writeln(e.Name^, ':', e.Value^);
    // ! // output  a:1  b:2  c:3
    function Fields: TDocVariantFieldsEnumerator;
    /// an enumerator able to compile "for .. in dv.FieldNames do" for objects
    // - returns pointers over all Names[]
    // - don't iterate if the document is an array - so n is never nil:
    // ! var n: PRawUtf8;
    // ! ...
    // !   dv.InitJson('{a:1,b:2,c:3}');
    // !   for n in dv.FieldNames do
    // !     writeln(n^);
    // ! // output  a  b  c
    function FieldNames: TDocVariantFieldNamesEnumerator;
    /// an enumerator able to compile "for .. in dv.FieldValues do" for objects
    // - returns pointers over all Values[]
    // - don't iterate if the document is an array:
    // ! var v: PVariant;
    // ! ...
    // !   dv.InitJson('{a:1,b:2,c:3}');
    // !   for v in dv.FieldValues do
    // !     writeln(v^);
    // ! // output  1  2  3
    function FieldValues: TDocVariantItemsEnumerator;
    /// an enumerator able to compile "for .. in dv.Items do" for arrays
    // - returns a PVariant over all Values[] of a document array
    // - don't iterate if the document is an object
    // - for instance:
    // ! var v: PVariant;
    // ! ...
    // !    dv.InitArray([1, 3, 3, 4]);
    // !    for v in dv.Items do
    // !      writeln(v^);
    // ! // output  1  2  3  4
    function Items: TDocVariantItemsEnumerator;
    /// an enumerator able to compile "for .. dv.Objects do" for array of objects
    // - returns all Values[] of a document array which are a TDocVariantData
    // - don't iterate if the document is an object, or if an item is not a
    // TDocVariantData:
    // ! var d: PDocVariantData;
    // ! ...
    // !    dv.InitJson('[{a:1,b:1},1,"no object",{a:2,b:2}]');
    // !    for d in dv.Objects do
    // !      writeln(d^.ToJson);
    // ! // output {"a":1,"b":1} and {"a":2,"b":2} only
    // ! // (ignoring 1 and "no object" items)
    function Objects: TDocVariantObjectsEnumerator;
    {$endif HASITERATORS}

    /// save a document as UTF-8 encoded JSON
    // - will write either a JSON object or array, depending of the internal
    // layout of this instance (i.e. Kind property value)
    // - will write  'null'  if Kind is dvUndefined
    // - implemented as just a wrapper around DocVariantType.ToJson()
    function ToJson: RawUtf8; overload;
    /// save a document as UTF-8 encoded JSON
    function ToJson(const Prefix, Suffix: RawUtf8;
      Format: TTextWriterJsonFormat): RawUtf8; overload;
    /// save a document as UTF-8 encoded JSON file
    // - you may then use InitJsonFromFile() to load and parse this file
    procedure SaveToJsonFile(const FileName: TFileName);
    /// save an array of objects as UTF-8 encoded non expanded layout JSON
    // - returned content would be a JSON object in mORMot's TOrmTableJson non
    // expanded format, with reduced JSON size, i.e.
    // $ {"fieldCount":2,"values":["f1","f2","1v1",1v2,"2v1",2v2...],"rowCount":20}
    // - will write '' if Kind is dvUndefined or dvObject
    // - will raise an exception if the array document is not an array of
    // objects with identical field names
    // - can be unserialized using the InitArrayFromResults() method
    function ToNonExpandedJson: RawUtf8;
    /// save a document as an array of UTF-8 encoded JSON
    // - will expect the document to be a dvArray - otherwise, will raise a
    // EDocVariant exception
    // - will use VariantToUtf8() to populate the result array: as a consequence,
    // any nested custom variant types (e.g. TDocVariant) will be stored as JSON
    procedure ToRawUtf8DynArray(out Result: TRawUtf8DynArray); overload;
    /// save a document as an array of UTF-8 encoded JSON
    // - will expect the document to be a dvArray - otherwise, will raise a
    // EDocVariant exception
    // - will use VariantToUtf8() to populate the result array: as a consequence,
    // any nested custom variant types (e.g. TDocVariant) will be stored as JSON
    function ToRawUtf8DynArray: TRawUtf8DynArray; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// save a document as an CSV of UTF-8 encoded JSON
    // - will expect the document to be a dvArray - otherwise, will raise a
    // EDocVariant exception
    // - will use VariantToUtf8() to populate the result array: as a consequence,
    // any nested custom variant types (e.g. TDocVariant) will be stored as JSON
    function ToCsv(const Separator: RawUtf8 = ','): RawUtf8;
    /// save a document as UTF-8 encoded Name=Value pairs
    // - will follow by default the .INI format, but you can specify your
    // own expected layout
    procedure ToTextPairsVar(out result: RawUtf8;
      const NameValueSep: RawUtf8 = '='; const ItemSep: RawUtf8 = #13#10;
      Escape: TTextWriterKind = twJsonEscape);
    /// save a document as UTF-8 encoded Name=Value pairs
    // - will follow by default the .INI format, but you can specify your
    // own expected layout
    function ToTextPairs(const NameValueSep: RawUtf8 = '=';
      const ItemSep: RawUtf8 = #13#10;
      Escape: TTextWriterKind = twJsonEscape): RawUtf8;
       {$ifdef HASINLINE}inline;{$endif}
    /// save an array document as an array of TVarRec, i.e. an array of const
    // - will expect the document to be a dvArray - otherwise, will raise a
    // EDocVariant exception
    // - values will be passed by referenced as vtVariant to @VValue[ndx]
    // - would allow to write code as such:
    // !  Doc.InitArray(['one',2,3]);
    // !  Doc.ToArrayOfConst(vr);
    // !  s := FormatUtf8('[%,%,%]',vr,[],true);
    // !  // here s='[one,2,3]') since % would be replaced by Args[] parameters
    // !  s := FormatUtf8('[?,?,?]',[],vr,true);
    // !  // here s='["one",2,3]') since ? would be escaped by Params[] parameters
    procedure ToArrayOfConst(out Result: TTVarRecDynArray); overload;
    /// save an array document as an array of TVarRec, i.e. an array of const
    // - will expect the document to be a dvArray - otherwise, will raise a
    // EDocVariant exception
    // - values will be passed by referenced as vtVariant to @VValue[ndx]
    // - would allow to write code as such:
    // !  Doc.InitArray(['one',2,3]);
    // !  s := FormatUtf8('[%,%,%]',Doc.ToArrayOfConst,[],true);
    // !  // here s='[one,2,3]') since % would be replaced by Args[] parameters
    // !  s := FormatUtf8('[?,?,?]',[],Doc.ToArrayOfConst,true);
    // !  // here s='["one",2,3]') since ? would be escaped by Params[] parameters
    function ToArrayOfConst: TTVarRecDynArray; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// save an object document as an URI-encoded list of parameters
    // - object field names should be plain ASCII-7 RFC compatible identifiers
    // (0..9a..zA..Z_.~), otherwise their values are skipped
    function ToUrlEncode(const UriRoot: RawUtf8): RawUtf8;

    /// returns true if this is not a true TDocVariant, or Count equals 0
    function IsVoid: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// search if a given Name do exists in this document
    // - just a wrapper around GetValueIndex(aName) >= 0
    function Exists(const aName: RawUtf8): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// find an item index in this document from its name
    // - search will follow dvoNameCaseSensitive option of this document
    // - lookup the value by name for an object document, or accept an integer
    // text as index for an array document
    // - returns -1 if not found
    function GetValueIndex(const aName: RawUtf8): integer; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// find an item index in this document from its name
    // - lookup the value by name for an object document, or accept an integer
    // text as index for an array document
    // - returns -1 if not found
    function GetValueIndex(aName: PUtf8Char; aNameLen: PtrInt;
      aCaseSensitive: boolean): integer; overload;
    /// find an item in this document, and returns its value
    // - raise an EDocVariant if not found and dvoReturnNullForUnknownProperty
    // is not set in Options (in this case, it will return Null)
    function GetValueOrRaiseException(const aName: RawUtf8): variant;
    /// find an item in this document, and returns its value
    // - return the supplied default if aName is not found, or if the instance
    // is not a TDocVariant
    function GetValueOrDefault(const aName: RawUtf8;
      const aDefault: variant): variant;
    /// find an item in this document, and returns its value
    // - return null if aName is not found, or if the instance is not a TDocVariant
    function GetValueOrNull(const aName: RawUtf8): variant;
    /// find an item in this document, and returns its value
    // - return a cleared variant if aName is not found, or if the instance is
    // not a TDocVariant
    function GetValueOrEmpty(const aName: RawUtf8): variant;
    /// find an item in this document, and returns its value as enumerate
    // - return false if aName is not found, if the instance is not a TDocVariant,
    // or if the value is not a string corresponding to the supplied enumerate
    // - return true if the name has been found, and aValue stores the value
    // - will call Delete() on the found entry, if aDeleteFoundEntry is true
    function GetValueEnumerate(const aName: RawUtf8; aTypeInfo: PRttiInfo;
      out aValue; aDeleteFoundEntry: boolean = false): boolean;
    /// returns a JSON object containing all properties matching the
    // first characters of the supplied property name
    // - returns null if the document is not a dvObject
    // - will use IdemPChar(), so search would be case-insensitive
    function GetJsonByStartName(const aStartName: RawUtf8): RawUtf8;
    /// find an item in this document, and returns its value as TVarData
    // - return false if aName is not found, or if the instance is not a TDocVariant
    // - return true and set aValue if the name has been found
    // - will use simple loop lookup to identify the name, unless aSortedCompare is
    // set, and would let use a faster O(log(n)) binary search after a SortByName()
    function GetVarData(const aName: RawUtf8; var aValue: TVarData;
      aSortedCompare: TUtf8Compare = nil): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// find an item in this document, and returns its value as TVarData pointer
    // - return nil if aName is not found, or if the instance is not a TDocVariant
    // - return a pointer to the value if the name has been found, and optionally
    // fill aFoundIndex^ with its index in Values[]
    // - after a SortByName(aSortedCompare), could use faster binary search
    function GetVarData(const aName: RawUtf8; aSortedCompare: TUtf8Compare = nil;
      aFoundIndex: PInteger = nil): PVarData; overload;
    /// find an item in this document, and returns its value as boolean
    // - return false if aName is not found, or if the instance is not a TDocVariant
    // - return true if the name has been found, and aValue stores the value
    // - after a SortByName(aSortedCompare), could use faster binary search
    // - consider using B[] property if you want simple read/write typed access
    function GetAsBoolean(const aName: RawUtf8; out aValue: boolean;
      aSortedCompare: TUtf8Compare = nil): boolean;
    /// find an item in this document, and returns its value as integer
    // - return false if aName is not found, or if the instance is not a TDocVariant
    // - return true if the name has been found, and aValue stores the value
    // - after a SortByName(aSortedCompare), could use faster binary search
    // - consider using I[] property if you want simple read/write typed access
    function GetAsInteger(const aName: RawUtf8; out aValue: integer;
      aSortedCompare: TUtf8Compare = nil): boolean;
    /// find an item in this document, and returns its value as integer
    // - return false if aName is not found, or if the instance is not a TDocVariant
    // - return true if the name has been found, and aValue stores the value
    // - after a SortByName(aSortedCompare), could use faster binary search
    // - consider using I[] property if you want simple read/write typed access
    function GetAsInt64(const aName: RawUtf8; out aValue: Int64;
      aSortedCompare: TUtf8Compare = nil): boolean;
    /// find an item in this document, and returns its value as floating point
    // - return false if aName is not found, or if the instance is not a TDocVariant
    // - return true if the name has been found, and aValue stores the value
    // - after a SortByName(aSortedCompare), could use faster binary search
    // - consider using D[] property if you want simple read/write typed access
    function GetAsDouble(const aName: RawUtf8; out aValue: double;
      aSortedCompare: TUtf8Compare = nil): boolean;
    /// find an item in this document, and returns its value as RawUtf8
    // - return false if aName is not found, or if the instance is not a TDocVariant
    // - return true if the name has been found, and aValue stores the value
    // - after a SortByName(aSortedCompare), could use faster binary search
    // - consider using U[] property if you want simple read/write typed access
    function GetAsRawUtf8(const aName: RawUtf8; out aValue: RawUtf8;
      aSortedCompare: TUtf8Compare = nil): boolean;
    /// find an item in this document, and returns its value as a TDocVariantData
    // - return false if aName is not found, or if the instance is not a TDocVariant
    // - return true if the name has been found and points to a TDocVariant:
    // then aValue stores a pointer to the value
    // - after a SortByName(aSortedCompare), could use faster binary search
    function GetAsDocVariant(const aName: RawUtf8; out aValue: PDocVariantData;
      aSortedCompare: TUtf8Compare = nil): boolean;
    /// find a non-void array item in this document, and returns its value
    // - return false if aName is not found, or if not a TDocVariant array
    // - return true if the name was found as non-void array and set to aArray
    // - after a SortByName(aSortedCompare), could use faster binary search
    function GetAsArray(const aName: RawUtf8; out aArray: PDocVariantData;
      aSortedCompare: TUtf8Compare = nil): boolean;
    /// find a non-void object item in this document, and returns its value
    // - return false if aName is not found, or if not a TDocVariant object
    // - return true if the name was found as non-void object and set to aObject
    // - after a SortByName(aSortedCompare), could use faster binary search
    function GetAsObject(const aName: RawUtf8; out aObject: PDocVariantData;
      aSortedCompare: TUtf8Compare = nil): boolean;
    /// find an item in this document, and returns its value as a TDocVariantData
    // - returns a void TDocVariant if aName is not a document
    // - after a SortByName(aSortedCompare), could use faster binary search
    // - consider using O[] or A[] properties if you want simple read-only
    // access, or O_[] or A_[] properties if you want the ability to add
    // a missing object or array in the document
    function GetAsDocVariantSafe(const aName: RawUtf8;
      aSortedCompare: TUtf8Compare = nil): PDocVariantData;
    /// find an item in this document, and returns pointer to its value
    // - return false if aName is not found
    // - return true if the name has been found: then aValue stores a pointer
    // to the value
    // - after a SortByName(aSortedCompare), could use faster binary search
    function GetAsPVariant(const aName: RawUtf8; out aValue: PVariant;
      aSortedCompare: TUtf8Compare = nil): boolean; overload;
       {$ifdef HASINLINE}inline;{$endif}
    /// find an item in this document, and returns pointer to its value
    // - lookup the value by aName/aNameLen for an object document, or accept
    // an integer text as index for an array document
    // - return nil if aName is not found, or if the instance is not a TDocVariant
    // - return a pointer to the stored variant, if the name has been found
    function GetAsPVariant(aName: PUtf8Char; aNameLen: PtrInt): PVariant; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve a value, given its path
    // - path is defined as a dotted name-space, e.g. 'doc.glossary.title'
    // - return Unassigned (varEmpty) if there is no item at the supplied aPath
    // - you can set e.g. aPathDelim = '/' to search e.g. for 'parent/child'
    // - see also the P[] property if the default aPathDelim = '.' is enough
    function GetValueByPath(
      const aPath: RawUtf8; aPathDelim: AnsiChar = '.'): variant; overload;
    /// retrieve a value, given its path
    // - path is defined as a dotted name-space, e.g. 'doc.glossary.title'
    // - returns FALSE if there is no item at the supplied aPath
    // - returns TRUE and set the found value in aValue
    // - you can set e.g. aPathDelim = '/' to search e.g. for 'parent/child'
    // - see also the P[] property if the default aPathDelim = '.' is enough
    function GetValueByPath(const aPath: RawUtf8; out aValue: variant;
      aPathDelim: AnsiChar = '.'): boolean; overload;
    /// retrieve a value, given its path
    // - path is defined as a list of names, e.g. ['doc','glossary','title']
    // - return Unassigned (varEmpty) if there is no item at the supplied aPath
    // - this method will only handle nested TDocVariant values: use the
    // slightly slower GetValueByPath() overloaded method, if any nested object
    // may be of another type (e.g. a TBsonVariant)
    function GetValueByPath(
      const aDocVariantPath: array of RawUtf8): variant; overload;
    /// retrieve a reference to a value, given its path
    // - path is defined as a dotted name-space, e.g. 'doc.glossary.title'
    // - if the supplied aPath does not match any object, it will return nil
    // - if aPath is found, returns a pointer to the corresponding value
    // - you can set e.g. aPathDelim = '/' to search e.g. for 'parent/child'
    function GetPVariantByPath(const aPath: RawUtf8;
      aPathDelim: AnsiChar = '.'): PVariant;
    /// retrieve a reference to a value, given its path
    // - if the supplied aPath does not match any object, it will follow
    // dvoReturnNullForUnknownProperty option
    function GetPVariantExistingByPath(const aPath: RawUtf8;
      aPathDelim: AnsiChar = '.'): PVariant;
    /// retrieve a reference to a TDocVariant, given its path
    // - path is defined as a dotted name-space, e.g. 'doc.glossary.title'
    // - if the supplied aPath does not match any object, it will return false
    // - if aPath stores a valid TDocVariant, returns true and a pointer to it
    // - you can set e.g. aPathDelim = '/' to search e.g. for 'parent/child'
    function GetDocVariantByPath(const aPath: RawUtf8;
      out aValue: PDocVariantData; aPathDelim: AnsiChar = '.'): boolean;
    /// retrieve a dvObject in the dvArray, from a property value
    // - {aPropName:aPropValue} will be searched within the stored array,
    // and the corresponding item will be copied into Dest, on match
    // - returns FALSE if no match is found, TRUE if found and copied
    // - create a copy of the variant by default, unless DestByRef is TRUE
    // - will call VariantEquals() for value comparison
    function GetItemByProp(const aPropName, aPropValue: RawUtf8;
      aPropValueCaseSensitive: boolean; var Dest: variant;
      DestByRef: boolean = false): boolean;
    /// retrieve a reference to a dvObject in the dvArray, from a property value
    // - {aPropName:aPropValue} will be searched within the stored array,
    // and the corresponding item will be copied into Dest, on match
    // - returns FALSE if no match is found, TRUE if found and copied by reference
    function GetDocVariantByProp(const aPropName, aPropValue: RawUtf8;
      aPropValueCaseSensitive: boolean; out Dest: PDocVariantData): boolean;
    /// find an item in this document, and returns its value
    // - raise an EDocVariant if not found and dvoReturnNullForUnknownProperty
    // is not set in Options (in this case, it will return Null)
    // - create a copy of the variant by default, unless DestByRef is TRUE
    function RetrieveValueOrRaiseException(aName: PUtf8Char; aNameLen: integer;
      aCaseSensitive: boolean; var Dest: variant; DestByRef: boolean): boolean; overload;
    /// retrieve an item in this document from its index, and returns its value
    // - raise an EDocVariant if the supplied Index is not in the 0..Count-1
    // range and dvoReturnNullForUnknownProperty is set in Options
    // - create a copy of the variant by default, unless DestByRef is TRUE
    procedure RetrieveValueOrRaiseException(Index: integer;
     var Dest: variant; DestByRef: boolean); overload;
    /// retrieve an item in this document from its index, and returns its Name
    // - raise an EDocVariant if the supplied Index is not in the 0..Count-1
    // range and dvoReturnNullForUnknownProperty is set in Options
    procedure RetrieveNameOrRaiseException(Index: integer; var Dest: RawUtf8);
    /// returns a TDocVariant object containing all properties matching the
    // first characters of the supplied property name
    // - returns null if the document is not a dvObject
    // - will use IdemPChar(), so search would be case-insensitive
    function GetValuesByStartName(const aStartName: RawUtf8;
      TrimLeftStartName: boolean = false): variant;
    /// set an item in this document from its index
    // - raise an EDocVariant if the supplied Index is not in 0..Count-1 range
    procedure SetValueOrRaiseException(Index: integer; const NewValue: variant);
    /// set a value, given its path
    // - path is defined as a dotted name-space, e.g. 'doc.glossary.title'
    // - aCreateIfNotExisting=true will force missing nested objects creation
    // - returns FALSE if there is no item to be set at the supplied aPath
    // - returns TRUE and set the found value in aValue
    // - you can set e.g. aPathDelim = '/' to search e.g. for 'parent/child'
    function SetValueByPath(const aPath: RawUtf8; const aValue: variant;
      aCreateIfNotExisting: boolean = false; aPathDelim: AnsiChar = '.'): boolean;

    /// add a value in this document
    // - if aName is set, if dvoCheckForDuplicatedNames option is set, any
    // existing duplicated aName will raise an EDocVariant; if instance's
    // kind is dvArray and aName is defined, it will raise an EDocVariant
    // - aName may be '' e.g. if you want to store an array: in this case,
    // dvoCheckForDuplicatedNames option should not be set; if instance's Kind
    // is dvObject, it will raise an EDocVariant exception
    // - if aValueOwned is true, then the supplied aValue will be assigned to
    // the internal values - by default, it will use SetVariantByValue()
    // - you can therefore write e.g.:
    // ! TDocVariant.New(aVariant);
    // ! Assert(TDocVariantData(aVariant).Kind=dvUndefined);
    // ! TDocVariantData(aVariant).AddValue('name','John');
    // ! Assert(TDocVariantData(aVariant).Kind=dvObject);
    // - you can specify an optional index in the array where to insert
    // - returns the index of the corresponding newly added value
    function AddValue(const aName: RawUtf8; const aValue: variant;
      aValueOwned: boolean = false; aIndex: integer = -1): integer; overload;
    /// add a value in this document
    // - overloaded function accepting a UTF-8 encoded buffer for the name
    function AddValue(aName: PUtf8Char; aNameLen: integer; const aValue: variant;
      aValueOwned: boolean = false; aIndex: integer = -1): integer; overload;
    /// add a value in this document, or update an existing entry
    // - if instance's Kind is dvArray, it will raise an EDocVariant exception
    // - any existing Name would be updated with the new Value, unless
    // OnlyAddMissing is set to TRUE, in which case existing values would remain
    // - returns the index of the corresponding value, which may be just added
    function AddOrUpdateValue(const aName: RawUtf8; const aValue: variant;
      wasAdded: PBoolean = nil; OnlyAddMissing: boolean = false): integer;
    /// add a value in this document, from its text representation
    // - this function expects a UTF-8 text for the value, which would be
    // converted to a variant number, if possible (as varInt/varInt64/varCurrency
    // and/or as varDouble is dvoAllowDoubleValue option is set)
    // - if Update=TRUE, will set the property, even if it is existing
    function AddValueFromText(const aName, aValue: RawUtf8;
      DoUpdate: boolean = false): integer;
    /// add some properties to a TDocVariantData dvObject
    // - data is supplied two by two, as Name,Value pairs
    // - caller should ensure that Kind=dvObject, otherwise it won't do anything
    // - any existing Name would be duplicated - use Update() if you want to
    // replace any existing value
    procedure AddNameValuesToObject(const NameValuePairs: array of const);
    /// merge some properties to a TDocVariantData dvObject
    // - data is supplied two by two, as Name,Value pairs
    // - caller should ensure that Kind=dvObject, otherwise it won't do anything
    // - any existing Name would be updated with the new Value
    procedure Update(const NameValuePairs: array of const);
    {$ifndef PUREMORMOT2}
    /// deprecated method which redirects to Update()
    procedure AddOrUpdateNameValuesToObject(const NameValuePairs: array of const);
    {$endif PUREMORMOT2}
    /// merge some TDocVariantData dvObject properties to a TDocVariantData dvObject
    // - data is supplied two by two, as Name,Value pairs
    // - caller should ensure that both variants have Kind=dvObject, otherwise
    // it won't do anything
    // - any existing Name would be updated with the new Value, unless
    // OnlyAddMissing is set to TRUE, in which case existing values would remain
    procedure AddOrUpdateObject(const NewValues: variant;
      OnlyAddMissing: boolean = false; RecursiveUpdate: boolean = false);
    /// add a value to this document, handled as array
    // - if instance's Kind is dvObject, it will raise an EDocVariant exception
    // - you can therefore write e.g.:
    // ! TDocVariant.New(aVariant);
    // ! Assert(TDocVariantData(aVariant).Kind=dvUndefined);
    // ! TDocVariantData(aVariant).AddItem('one');
    // ! Assert(TDocVariantData(aVariant).Kind=dvArray);
    // - you can specify an optional index in the array where to insert
    // - returns the index of the corresponding newly added item
    function AddItem(const aValue: variant; aIndex: integer = -1): integer; overload;
    /// add a TDocVariant value to this document, handled as array
    function AddItem(const aValue: TDocVariantData; aIndex: integer = -1): integer; overload;
    /// add a value to this document, handled as array, from its text representation
    // - this function expects a UTF-8 text for the value, which would be
    // converted to a variant number, if possible (as varInt/varInt64/varCurrency
    // and/or as varDouble is dvoAllowDoubleValue option is set)
    // - if instance's Kind is dvObject, it will raise an EDocVariant exception
    // - you can specify an optional index in the array where to insert
    // - returns the index of the corresponding newly added item
    function AddItemFromText(const aValue: RawUtf8;
      aIndex: integer = -1): integer;
    /// add a RawUtf8 value to this document, handled as array
    // - if instance's Kind is dvObject, it will raise an EDocVariant exception
    // - you can specify an optional index in the array where to insert
    // - returns the index of the corresponding newly added item
    function AddItemText(const aValue: RawUtf8; aIndex: integer = -1): integer;
    /// add one or several values to this document, handled as array
    // - if instance's Kind is dvObject, it will raise an EDocVariant exception
    procedure AddItems(const aValue: array of const);
    /// add one object document to this document
    // - if the document is an array, keep aName=''
    // - if the document is an object, set the new object property as aName
    // - new object will keep the same options as this document
    // - slightly faster than AddItem(_Obj(...)) or AddValue(aName, _Obj(...))
    procedure AddObject(const aNameValuePairs: array of const;
      const aName: RawUtf8 = '');
    /// add one or several values from another document
    // - supplied document should be of the same kind than the current one,
    // otherwise nothing is added
    // - for an object, dvoCheckForDuplicatedNames flag is used: use
    // AddOrUpdateFrom() to force objects merging
    procedure AddFrom(const aDocVariant: Variant);
    /// merge (i.e. add or update) several values from another object
    // - current document should be an object
    procedure AddOrUpdateFrom(const aDocVariant: Variant;
      aOnlyAddMissing: boolean = false);
    /// add one or several properties, specified by path, from another object
    // - path are defined as open array, e.g. ['doc','glossary','title'], but
    // could also contained nested paths, e.g. ['doc.glossary', title'] or
    // ['doc', 'glossary/title'] of aPathDelim is '/'
    // - matching values would be added as root values, with the path as name
    // - instance and supplied aSource should be a dvObject
    procedure AddByPath(const aSource: TDocVariantData;
      const aPaths: array of RawUtf8; aPathDelim: AnsiChar = '.');
    /// delete a value/item in this document, from its index
    // - return TRUE on success, FALSE if the supplied index is not correct
    function Delete(Index: PtrInt): boolean; overload;
    /// delete a value/item in this document, from its name
    // - return TRUE on success, FALSE if the supplied name does not exist
    function Delete(const aName: RawUtf8; aValue: PVariant = nil): boolean; overload;
    /// delete/filter some values/items in this document, from their name
    // - return the number of deleted items
    function Delete(const aNames: array of RawUtf8): integer; overload;
    /// delete a value/item in this document, from its name
    // - path is defined as a dotted name-space, e.g. 'doc.glossary.title'
    // - return TRUE on success, FALSE if the supplied name does not exist
    // - you can set e.g. aPathDelim = '/' to search e.g. for 'parent/child'
    function DeleteByPath(const aPath: RawUtf8; aPathDelim: AnsiChar = '.';
      aDeletedValue: PVariant = nil): boolean;
    /// delete a value in this document, by property name match
    // - {aPropName:aPropValue} will be searched within the stored array or
    // object, and the corresponding item will be deleted, on match
    // - returns FALSE if no match is found, TRUE if found and deleted
    // - will call VariantEquals() for value comparison
    function DeleteByProp(const aPropName, aPropValue: RawUtf8;
      aPropValueCaseSensitive: boolean): boolean;
    /// delete one or several value/item in this document, from its value
    // - returns the number of deleted items
    // - returns 0 if the document is not a dvObject, or if no match was found
    // - if the value exists several times, all occurrences would be removed
    // - is optimized for DeleteByValue(null) call
    function DeleteByValue(const aValue: Variant;
      CaseInsensitive: boolean = false): integer;
    /// delete all values matching the first characters of a property name
    // - returns the number of deleted items
    // - returns 0 if the document is not a dvObject, or if no match was found
    // - will use IdemPChar(), so search would be case-insensitive
    function DeleteByStartName(aStartName: PUtf8Char;
      aStartNameLen: integer): integer;
    /// retrieve a value at a given index, and delete it from the array
    // - negative aIndex are from VCount, i.e. Extract(-1) pop the last item
    function Extract(aIndex: integer; var aValue: variant;
      aName: PRawUtf8 = nil): boolean;
    /// search a property match in this document, handled as array or object
    // - {aPropName:aPropValue} will be searched within the stored array or
    // object, and the corresponding item index will be returned, on match
    // - returns -1 if no match is found
    // - will call VariantEquals() for value comparison
    function SearchItemByProp(const aPropName, aPropValue: RawUtf8;
      aPropValueCaseSensitive: boolean): integer; overload;
    /// search a property match in this document, handled as array or object
    // - {aPropName:aPropValue} will be searched within the stored array or
    // object, and the corresponding item index will be returned, on match
    // - returns -1 if no match is found
    // - will call VariantEquals() for value comparison
    function SearchItemByProp(const aPropNameFmt: RawUtf8;
      const aPropNameArgs: array of const; const aPropValue: RawUtf8;
      aPropValueCaseSensitive: boolean): integer; overload;
    /// search a value in this document, handled as array
    // - aValue will be searched within the stored array
    // and the corresponding item index will be returned, on match
    // - returns -1 if no match is found
    // - you could make several searches, using the StartIndex optional parameter
    function SearchItemByValue(const aValue: Variant;
      CaseInsensitive: boolean = false; StartIndex: PtrInt = 0): PtrInt;
    /// search and count occurences of one value in this document, handled as array
    function CountItemByValue(const aValue: Variant;
      CaseInsensitive: boolean = false; StartIndex: integer = 0): integer;
    /// sort the document object values by name
    // - do nothing if the document is not a dvObject
    // - will follow case-insensitive order (@StrIComp) by default, but you
    // can specify @StrComp as comparer function for case-sensitive ordering
    // - once sorted, you can use GetVarData(..,Compare) or GetAs*(..,Compare)
    // methods for much faster O(log(n)) binary search
    procedure SortByName(SortCompare: TUtf8Compare = nil;
      SortCompareReversed: boolean = false);
    /// sort the document object values by value using a comparison function
    // - work for both dvObject and dvArray documents
    // - will sort by UTF-8 text (VariantCompare) if no custom aCompare is supplied
    procedure SortByValue(SortCompare: TVariantCompare = nil;
      SortCompareReversed: boolean = false);
    /// sort the document object values by value using a comparison method
    // - work for both dvObject and dvArray documents
    // - you should supply a TVariantComparer callback method
    procedure SortByRow(const SortComparer: TVariantComparer;
      SortComparerReversed: boolean = false);
    /// sort the document array values by a field of some stored objet values
    // - do nothing if the document is not a dvArray, or if the items are no dvObject
    // - aValueCompare will be called with the aItemPropName values, not row
    // - will sort by UTF-8 text (VariantCompare) if no custom aValueCompare is supplied
    // - this method is faster than SortByValue/SortByRow
    procedure SortArrayByField(const aItemPropName: RawUtf8;
      aValueCompare: TVariantCompare = nil;
      aValueCompareReverse: boolean = false;
      aNameSortedCompare: TUtf8Compare = nil);
    /// sort the document array values by field(s) of some stored objet values
    // - allow up to 4 fields (aItemPropNames[0]..aItemPropNames[3])
    // - do nothing if the document is not a dvArray, or if the items are no dvObject
    // - will sort by UTF-8 text (VariantCompare) if no aValueCompareField is supplied
    procedure SortArrayByFields(const aItemPropNames: array of RawUtf8;
      aValueCompare: TVariantCompare = nil;
      const aValueCompareField: TVariantCompareField = nil;
      aValueCompareReverse: boolean = false; aNameSortedCompare: TUtf8Compare = nil);
    /// inverse the order of Names and Values of this document
    // - could be applied after a content sort if needed
    procedure Reverse;
    /// create a TDocVariant object, from a selection of properties of the
    // objects of this document array, by property name
    // - if the document is a dvObject, to reduction will be applied to all
    // its properties
    // - if the document is a dvArray, the reduction will be applied to each
    // stored item, if it is a document
    procedure Reduce(const aPropNames: array of RawUtf8; aCaseSensitive: boolean;
      var result: TDocVariantData; aDoNotAddVoidProp: boolean = false); overload;
    /// create a TDocVariant object, from a selection of properties of the
    // objects of this document array, by property name
    // - always returns a TDocVariantData, even if no property name did match
    // (in this case, it is dvUndefined)
    function Reduce(const aPropNames: array of RawUtf8; aCaseSensitive: boolean;
      aDoNotAddVoidProp: boolean = false): variant; overload;
    /// create a TDocVariant array, matching a filtering expression
    // - expressions are e.g. 'name=Synopse' or 'price<100'
    procedure ReduceFilter(const aExpression: RawUtf8; var result: TDocVariantData;
      aLimit: integer = 0; aCompare: TVariantCompare = nil); overload;
    /// create a TDocVariant array, matching a filtering expression
    // - expressions are e.g. 'name=Synopse' or 'price<100'
    function ReduceFilter(const aExpression: RawUtf8; aLimit: integer = 0): variant; overload;
    /// create a TDocVariant array, matching a filtering expression
    // - e.g. ReduceFilter('name=','Synopse') or ReduceFilter('price<',MaxPrice)
    procedure ReduceFilter(const aExpression: RawUtf8; const aValue: variant;
      var result: TDocVariantData; aCompare: TVariantCompare = nil;
      aLimit: integer = 0); overload;
    /// create a TDocVariant array, matching a filtering expression
    // - e.g. ReduceFilter('name=','Synopse') or ReduceFilter('price<',MaxPrice)
    function ReduceFilter(const aExpression: RawUtf8; const aValue: variant;
      aLimit: integer = 0): variant; overload;
    /// create a TDocVariant array, matching a filtering set of raw parameters
    procedure ReduceFilter(const aKey: RawUtf8; const aValue: variant;
      aMatch: TCompareOperator; aCompare: TVariantCompare; aLimit: integer;
      var result: TDocVariantData); overload;
    /// create a TDocVariant array, from the values of a single property of the
    // objects of this document array, specified by name
    // - you can optionally apply an additional filter to each reduced item
    procedure ReduceAsArray(const aPropName: RawUtf8;
      var result: TDocVariantData;
      const OnReduce: TOnReducePerItem = nil); overload;
    /// create a TDocVariant array, from the values of a single property of the
    // objects of this document array, specified by name
    // - always returns a TDocVariantData, even if no property name did match
    // (in this case, it is dvUndefined)
    // - you can optionally apply an additional filter to each reduced item
    function ReduceAsArray(const aPropName: RawUtf8;
      const OnReduce: TOnReducePerItem = nil): variant; overload;
    /// create a TDocVariant array, from the values of a single property of the
    // objects of this document array, specified by name
    // - this overloaded method accepts an additional filter to each reduced item
    procedure ReduceAsArray(const aPropName: RawUtf8;
      var result: TDocVariantData;
      const OnReduce: TOnReducePerValue); overload;
    /// create a TDocVariant array, from the values of a single property of the
    // objects of this document array, specified by name
    // - always returns a TDocVariantData, even if no property name did match
    // (in this case, it is dvUndefined)
    // - this overloaded method accepts an additional filter to each reduced item
    function ReduceAsArray(const aPropName: RawUtf8;
      const OnReduce: TOnReducePerValue): variant; overload;
    /// return the variant values of a single property of the objects of this
    // document array, specified by name
    // - returns nil if the document is not a dvArray
    function ReduceAsVariantArray(const aPropName: RawUtf8;
      aDuplicates: TSearchDuplicate = sdNone): TVariantDynArray;
    /// rename some properties of a TDocVariant object
    // - returns the number of property names modified
    function Rename(const aFromPropName, aToPropName: TRawUtf8DynArray): integer;
    /// return a dynamic array with all dvObject Names, and length() = Count
    // - since length(Names) = Capacity, you can use this method to retrieve
    // all the object keys
    // - consider using FieldNames iterator or Names[0..Count-1] if you need
    // to iterate on the key names
    // - will internally force length(Names)=length(Values)=Capacity=Count and
    // return the Names[] instance with no memory (re)allocation
    // - if the document is not a dvObject, will return nil
    function GetNames: TRawUtf8DynArray;
    /// map {"obj.prop1"..,"obj.prop2":..} into {"obj":{"prop1":..,"prop2":...}}
    // - the supplied aObjectPropName should match the incoming dotted value
    // of all properties (e.g. 'obj' for "obj.prop1")
    // - if any of the incoming property is not of "obj.prop#" form, the
    // whole process would be ignored
    // - return FALSE if the TDocVariant did not change
    // - return TRUE if the TDocVariant has been flattened
    function FlattenAsNestedObject(const aObjectPropName: RawUtf8): boolean;

    /// how this document will behave
    // - those options are set when creating the instance
    // - dvoArray and dvoObject are not options, but define the document Kind,
    // so those items are ignored when assigned to this property
    property Options: TDocVariantOptions
      read VOptions write SetOptions;
    /// returns the document internal layout
    // - just after initialization, it will return dvUndefined
    // - most of the time, you will add named values with AddValue() or by
    // setting the variant properties: it will return dvObject
    // - but is you use AddItem(), values will have no associated names: the
    // document will be a dvArray
    // - is computed from the dvoArray or dvoObject flags presence in Options
    property Kind: TDocVariantKind
      read GetKind;
    /// return the custom variant type identifier, i.e. DocVariantType.VarType
    property VarType: word
      read VType;
    /// number of items stored in this document
    // - always 0 for Kind=dvUndefined
    // - the number of name/value pairs for Kind=dvObject (may be 0 if void)
    // - the number of items for Kind=dvArray (may be 0 if void)
    property Count: integer
      read VCount;
    /// the current capacity of this document
    // - allow direct access to VValue[] length
    property Capacity: integer
      read GetCapacity write SetCapacity;
    /// direct acces to the low-level internal array of values
    // - note that length(Values)=Capacity and not Count, so copy(Values, 0, Count)
    // or use FieldValues iterator if you want the exact count
    // - transtyping a variant and direct access to TDocVariantData is the
    // fastest way of accessing all properties of a given dvObject:
    // ! with _Safe(aVariantObject)^ do
    // !   for i := 0 to Count-1 do
    // !     writeln(Names[i],'=',Values[i]);
    // - or to access a dvArray items (e.g. a MongoDB collection):
    // ! with TDocVariantData(aVariantArray) do
    // !   for i := 0 to Count-1 do
    // !     writeln(Values[i]);
    property Values: TVariantDynArray
      read VValue;
    /// direct acces to the low-level internal array of names
    // - is void (nil) if Kind is not dvObject
    // - note that length(Names)=Capacity and not Count, so copy(Names, 0, Count)
    // or use FieldNames iterator or GetNames if you want the exact count
    // - transtyping a variant and direct access to TDocVariantData is the
    // fastest way of accessing all properties of a given dvObject:
    // ! with _Safe(aVariantObject)^ do
    // !   for i := 0 to Count-1 do
    // !     writeln(Names[i],'=',Values[i]);
    property Names: TRawUtf8DynArray
      read VName;
    /// find an item in this document, and returns its value
    // - raise an EDocVariant if aNameOrIndex is neither an integer nor a string
    // - raise an EDocVariant if Kind is dvArray and aNameOrIndex is a string
    // or if Kind is dvObject and aNameOrIndex is an integer
    // - raise an EDocVariant if Kind is dvObject and if aNameOrIndex is a
    // string, which is not found within the object property names and
    // dvoReturnNullForUnknownProperty is set in Options
    // - raise an EDocVariant if Kind is dvArray and if aNameOrIndex is a
    // integer, which is not within 0..Count-1 and dvoReturnNullForUnknownProperty
    // is set in Options
    // - so you can use directly:
    // ! // for an array document:
    // ! aVariant := TDocVariant.NewArray(['one',2,3.0]);
    // ! for i := 0 to TDocVariantData(aVariant).Count-1 do
    // !   aValue := TDocVariantData(aVariant).Value[i];
    // ! // for an object document:
    // ! aVariant := TDocVariant.NewObject(['name','John','year',1972]);
    // ! assert(aVariant.Name=TDocVariantData(aVariant)['name']);
    // ! assert(aVariant.year=TDocVariantData(aVariant)['year']);
    // - due to the internal implementation of variant execution (somewhat
    // slow _DispInvoke() function), it is a bit faster to execute:
    // ! aValue := TDocVariantData(aVariant).Value['name'];
    // or
    // ! aValue := _Safe(aVariant).Value['name'];
    // instead of
    // ! aValue := aVariant.name;
    // but of course, if want to want to access the content by index (typically
    // for a dvArray), using Values[] - and Names[] - properties is much faster
    // than this variant-indexed pseudo-property:
    // ! with TDocVariantData(aVariant) do
    // !   for i := 0 to Count-1 do
    // !     Writeln(Values[i]);
    // is faster than:
    // ! with TDocVariantData(aVariant) do
    // !   for i := 0 to Count-1 do
    // !     Writeln(Value[i]);
    // which is faster than:
    // ! for i := 0 to aVariant.Count-1 do
    // !   Writeln(aVariant._(i));
    // - this property will return the value as varByRef (just like with
    // variant late binding of any TDocVariant instance), so you can write:
    // !var
    // !  Doc: TDocVariantData; // stack-allocated variable
    // !begin
    // !  Doc.InitJson('{arr:[1,2]}');
    // !  assert(Doc.Count=2);
    // !  Doc.Value['arr'].Add(3);  // works since Doc.Value['arr'] is varByRef
    // !  writeln(Doc.ToJson);      // will write '{"arr":[1,2,3]}'
    // !end;
    // - if you want to access a property as a copy, i.e. to assign it to a
    // variant variable which will stay alive after this TDocVariant instance
    // is release, you should not use Value[] but rather
    // GetValueOrRaiseException or GetValueOrNull/GetValueOrEmpty
    // - see U[] I[] B[] D[] O[] O_[] A[] A_[] _[] properties for direct access
    // of strong typed values, or P[] to retrieve a variant from its path
    property Value[const aNameOrIndex: Variant]: Variant
      read GetValueOrItem write SetValueOrItem; default;

    /// direct access to a dvObject UTF-8 stored property value from its name
    // - slightly faster than the variant-based Value[] default property
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - use GetAsRawUtf8() if you want to check the availability of the field
    // - U['prop'] := 'value' would add a new property, or overwrite an existing
    property U[const aName: RawUtf8]: RawUtf8
      read GetRawUtf8ByName write SetRawUtf8ByName;
    /// direct string access to a dvObject UTF-8 stored property value from its name
    // - just a wrapper around U[] property, to avoid a compilation warning when
    // using plain string variables (internally, RawUtf8 will be used for storage)
    // - slightly faster than the variant-based Value[] default property
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - use GetAsRawUtf8() if you want to check the availability of the field
    // - S['prop'] := 'value' would add a new property, or overwrite an existing
    property S[const aName: RawUtf8]: string
      read GetStringByName write SetStringByName;
    /// direct access to a dvObject integer stored property value from its name
    // - slightly faster than the variant-based Value[] default property
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - use GetAsInt/GetAsInt64 if you want to check the availability of the field
    // - I['prop'] := 123 would add a new property, or overwrite an existing
    property I[const aName: RawUtf8]: Int64
      read GetInt64ByName write SetInt64ByName;
    /// direct access to a dvObject boolean stored property value from its name
    // - slightly faster than the variant-based Value[] default property
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - use GetAsBoolean if you want to check the availability of the field
    // - B['prop'] := true would add a new property, or overwrite an existing
    property B[const aName: RawUtf8]: boolean
      read GetBooleanByName write SetBooleanByName;
    /// direct access to a dvObject floating-point stored property value from its name
    // - slightly faster than the variant-based Value[] default property
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - use GetAsDouble if you want to check the availability of the field
    // - D['prop'] := 1.23 would add a new property, or overwrite an existing
    property D[const aName: RawUtf8]: Double
      read GetDoubleByName write SetDoubleByName;
    /// direct access to a dvObject existing dvObject property from its name
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - O['prop'] would return a fake void TDocVariant if the property is not
    // existing or not a dvObject, just like GetAsDocVariantSafe()
    // - use O_['prop'] to force adding any missing property
    property O[const aName: RawUtf8]: PDocVariantData
      read GetObjectExistingByName;
    /// direct access or add a dvObject's dvObject property from its name
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - O_['prop'] would add a new property if there is none existing, or
    // overwrite an existing property which is not a dvObject
    // - the new property object would inherit from the Options of this instance
    property O_[const aName: RawUtf8]: PDocVariantData
      read GetObjectOrAddByName;
    /// direct access to a dvObject existing dvArray property from its name
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - A['prop'] would return a fake void TDocVariant if the property is not
    // existing or not a dvArray, just like GetAsDocVariantSafe()
    // - use A_['prop'] to force adding any missing property
    property A[const aName: RawUtf8]: PDocVariantData
      read GetArrayExistingByName;
    /// direct access or add a dvObject's dvArray property from its name
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - A_['prop'] would add a new property if there is none existing, or
    // overwrite an existing property which is not a dvArray
    // - the new property array would inherit from the Options of this instance
    property A_[const aName: RawUtf8]: PDocVariantData
      read GetArrayOrAddByName;
    /// direct access to a dvArray's TDocVariant property from its index
    // - simple values may directly use Values[] dynamic array, but to access
    // a TDocVariantData members, this property is safer
    // - follows dvoReturnNullForUnknownProperty option to raise an exception
    // - _[ndx] would return a fake void TDocVariant if aIndex is out of range,
    // if the property is not existing or not a TDocVariantData (just like
    // GetAsDocVariantSafe)
    property _[aIndex: integer]: PDocVariantData
      read GetAsDocVariantByIndex;
    /// direct access to a dvObject value stored property value from its path name
    // - default Value[] will check only names in the current object properties,
    // whereas this property will recognize e.g. 'parent.child' nested objects
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    property P[const aNameOrPath: RawUtf8]: Variant
      read GetVariantByPath;
  end;
  {$A+} { packet object not allowed since Delphi 2009 :( }

var
  /// the internal custom variant type used to register TDocVariant
  DocVariantType: TDocVariant;

  /// copy of DocVariantType.VarType
  // - as used by inlined functions of TDocVariantData
  DocVariantVType: cardinal;

  // defined here for inlining - properly filled in initialization section below
  DV_FAST: array[TDocVariantKind] of TVarData;


/// retrieve the text representation of a TDocVairnatKind
function ToText(kind: TDocVariantKind): PShortString; overload;

/// direct access to a TDocVariantData from a given variant instance
// - return a pointer to the TDocVariantData corresponding to the variant
// instance, which may be of kind varByRef (e.g. when retrieved by late binding)
// - raise an EDocVariant exception if the instance is not a TDocVariant
// - the following direct trans-typing may fail, e.g. for varByRef value:
// ! TDocVariantData(aVarDoc.ArrayProp).Add('new item');
// - so you can write the following:
// ! DocVariantData(aVarDoc.ArrayProp).AddItem('new item');
// - note: due to a local variable lifetime change in Delphi 11, don't use
// this function with a temporary variant (e.g. from TList<variant>.GetItem) -
// call _DV() and a local TDocVariantData instead of a PDocVariantData
function DocVariantData(const DocVariant: variant): PDocVariantData;

const
  /// constant used e.g. by _Safe() and _DV() overloaded functions
  // - will be in code section of the exe, so will be read-only by design
  // - would have Kind=dvUndefined and Count=0, so _Safe() would return
  // a valid, but void document
  // - its VType is varNull, so would be viewed as a null variant
  // - dvoReturnNullForUnknownProperty is defined, so that U[]/I[]... methods
  // won't raise any exception about unexpected field name
  DocVariantDataFake: TDocVariantData = (
    VType: varNull;
    VOptions: [dvoReturnNullForUnknownProperty]{%H-});

/// direct access to a TDocVariantData from a given variant instance
// - return a pointer to the TDocVariantData corresponding to the variant
// instance, which may be of kind varByRef (e.g. when retrieved by late binding)
// - will return a read-only fake TDocVariantData with Kind=dvUndefined if the
// supplied variant is not a TDocVariant instance, so could be safely used
// in a with block (use "with" moderation, of course):
// ! with _Safe(aDocVariant)^ do
// !   for ndx := 0 to Count-1 do // here Count=0 for the "fake" result
// !     writeln(Names[ndx]);
// or excluding the "with" statement, as more readable code:
// ! var dv: PDocVariantData;
// !     ndx: PtrInt;
// ! begin
// !   dv := _Safe(aDocVariant);
// !   for ndx := 0 to dv.Count-1 do // here Count=0 for the "fake" result
// !     writeln(dv.Names[ndx]);
// - note: due to a local variable lifetime change in Delphi 11, don't use
// this function with a temporary variant (e.g. from TList<variant>.GetItem) -
// call _DV() and a local TDocVariantData instead of a PDocVariantData
function _Safe(const DocVariant: variant): PDocVariantData; overload;
  {$ifdef FPC}inline;{$endif} // Delphi has problems inlining this :(

/// direct access to a TDocVariantData from a given variant instance
// - return a pointer to the TDocVariantData corresponding to the variant
// instance, which may be of kind varByRef (e.g. when retrieved by late binding)
// - will check the supplied document kind, i.e. either dvObject or dvArray and
// raise a EDocVariant exception if it does not match
// - note: due to a local variable lifetime change in Delphi 11, don't use
// this function with a temporary variant (e.g. from TList<variant>.GetItem) -
// call _DV() and a local TDocVariantData instead of a PDocVariantData
function _Safe(const DocVariant: variant;
  ExpectedKind: TDocVariantKind): PDocVariantData; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct access to a TDocVariantData from a given variant instance
// - return true and set DocVariant with a pointer to the TDocVariantData
// corresponding to the variant instance, which may be of kind varByRef
// (e.g. when retrieved by late binding)
// - return false if the supplied Value is not a TDocVariant, but e.g. a string,
// a number or another type of custom variant
// - note: due to a local variable lifetime change in Delphi 11, don't use
// this function with a temporary variant (e.g. from TList<variant>.GetItem) -
// call _DV() and a local TDocVariantData instead of a PDocVariantData
function _Safe(const DocVariant: variant; out DV: PDocVariantData): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct access to a TDocVariantData array from a given variant instance
// - return true and set DV with a pointer to the TDocVariantData
// corresponding to the variant instance, if it is a dvArray
// - return false if the supplied Value is not an array TDocVariant
// - note: due to a local variable lifetime change in Delphi 11, don't use
// this function with a temporary variant (e.g. from TList<variant>.GetItem) -
// call _DV() and a local TDocVariantData instead of a PDocVariantData
function _SafeArray(const Value: variant; out DV: PDocVariantData): boolean; overload;

/// direct access to a TDocVariantData array from a given variant instance
// - overload to check for a given number of itemsin the array
function _SafeArray(const Value: variant; ExpectedCount: integer;
  out DV: PDocVariantData): boolean; overload;

/// direct access to a TDocVariantData object from a given variant instance
// - return true and set DV with a pointer to the TDocVariantData
// corresponding to the variant instance, if it is a dvObject
// - return false if the supplied Value is not an object TDocVariant
// - note: due to a local variable lifetime change in Delphi 11, don't use
// this function with a temporary variant (e.g. from TList<variant>.GetItem) -
// call _DV() and a local TDocVariantData instead of a PDocVariantData
function _SafeObject(const Value: variant; out DV: PDocVariantData): boolean;

/// direct copy of a TDocVariantData from a given variant instance
// - slower, but maybe used instead of _Safe() e.g. on Delphi 11
function _DV(const DocVariant: variant): TDocVariantData; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct copy of a TDocVariantData from a given variant instance
// - slower, but maybe used instead of _Safe() e.g. on Delphi 11
function _DV(const DocVariant: variant;
  ExpectedKind: TDocVariantKind): TDocVariantData; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct copy of a TDocVariantData from a given variant instance
// - slower, but maybe used instead of _Safe() e.g. on Delphi 11
function _DV(const DocVariant: variant;
  var DV: TDocVariantData): boolean; overload;
  {$ifdef FPC}inline;{$endif} // Delphi has troubles inlining goto/label

/// initialize a variant instance to store some document-based object content
// - object will be initialized with data supplied two by two, as Name,Value
// pairs, e.g.
// ! aVariant := _Obj(['name','John','year',1972]);
// or even with nested objects:
// ! aVariant := _Obj(['name','John','doc',_Obj(['one',1,'two',2.0])]);
// - this global function is an alias to TDocVariant.NewObject()
// - by default, every internal value will be copied, so access of nested
// properties can be slow - if you expect the data to be read-only or not
// propagated into another place, set Options=[dvoValueCopiedByReference]
// or using _ObjFast() will increase the process speed a lot
function _Obj(const NameValuePairs: array of const;
  Options: TDocVariantOptions = []): variant;

/// add a property value to a document-based object content
// - if Obj is a TDocVariant object, will add the Name/Value pair
// - if Obj is not a TDocVariant, will create a new fast document,
// initialized with supplied the Name/Value pairs
// - this function will also ensure that ensure Obj is not stored by reference,
// but as a true TDocVariantData
procedure _ObjAddProp(const Name: RawUtf8; const Value: variant;
  var Obj: variant); overload;

/// add a document property value to a document-based object content
procedure _ObjAddProp(const Name: RawUtf8; const Value: TDocVariantData;
  var Obj: variant); overload;
  {$ifdef HASINLINE} inline; {$endif}

/// add a RawUtf8 property value to a document-based object content
procedure _ObjAddPropU(const Name: RawUtf8; const Value: RawUtf8;
  var Obj: variant);

/// add some property values to a document-based object content
// - if Obj is a TDocVariant object, will add the Name/Value pairs
// - if Obj is not a TDocVariant, will create a new fast document,
// initialized with supplied the Name/Value pairs
// - this function will also ensure that ensure Obj is not stored by reference,
// but as a true TDocVariantData
procedure _ObjAddProps(const NameValuePairs: array of const;
  var Obj: variant); overload;

/// add the property values of a document to a document-based object content
// - if Document is not a TDocVariant object, will do nothing
// - if Obj is a TDocVariant object, will add Document fields to its content
// - if Obj is not a TDocVariant object, Document will be copied to Obj
procedure _ObjAddProps(const Document: variant;
  var Obj: variant); overload;

/// initialize a variant instance to store some document-based array content
// - array will be initialized with data supplied as parameters, e.g.
// ! aVariant := _Arr(['one',2,3.0]);
// - this global function is an alias to TDocVariant.NewArray()
// - by default, every internal value will be copied, so access of nested
// properties can be slow - if you expect the data to be read-only or not
// propagated into another place, set Options = [dvoValueCopiedByReference]
// or using _ArrFast() will increase the process speed a lot
function _Arr(const Items: array of const;
  Options: TDocVariantOptions = []): variant;

/// initialize a variant instance to store some document-based content
// from a supplied (extended) JSON content
// - this global function is an alias to TDocVariant.NewJson(), and return
// an Unassigned (varEmpty) variant if JSON content was not correctly converted
// - object or array will be initialized from the supplied JSON content, e.g.
// ! aVariant := _Json('{"id":10,"doc":{"name":"John","birthyear":1972}}');
// ! // now you can access to the properties via late binding
// ! assert(aVariant.id=10);
// ! assert(aVariant.doc.name='John');
// ! assert(aVariant.doc.birthYear=1972);
// ! // and also some pseudo-properties:
// ! assert(aVariant._count=2);
// ! assert(aVariant.doc._kind=ord(dvObject));
// ! // or with a JSON array:
// ! aVariant := _Json('["one",2,3]');
// ! assert(aVariant._kind=ord(dvArray));
// ! for i := 0 to aVariant._count-1 do
// !   writeln(aVariant._(i));
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names:
// ! aVariant := _Json('{id:10,doc:{name:"John",birthyear:1972}}');
// - if the mormot.db.nosql.bson unit is used in the application, the MongoDB
// Shell syntax will also be recognized to create TBsonVariant, like
// ! new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
// - by default, every internal value will be copied, so access of nested
// properties can be slow - if you expect the data to be read-only or not
// propagated into another place, add dvoValueCopiedByReference in Options
// will increase the process speed a lot, or use _JsonFast()
// - handle only currency for floating point values: call _JsonFastFloat or set
// dvoAllowDoubleValue option to support double, with potential precision loss
function _Json(const Json: RawUtf8;
  Options: TDocVariantOptions = [dvoReturnNullForUnknownProperty]): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// initialize a variant instance to store some document-based content
// from a supplied (extended) JSON content, with parameters formating
// - wrapper around the _Json(FormatUtf8(...,JsonFormat=true)) function,
// i.e. every Args[] will be inserted for each % and Params[] for each ?,
// with proper JSON escaping of string values, and writing nested _Obj() /
// _Arr() instances as expected JSON objects / arrays
// - typical use (in the context of mormot.db.nosql.bson unit) could be:
// ! aVariant := _JsonFmt('{%:{$in:[?,?]}}',['type'],['food','snack']);
// ! aVariant := _JsonFmt('{type:{$in:?}}',[],[_Arr(['food','snack'])]);
// ! // which are the same as:
// ! aVariant := _JsonFmt('{type:{$in:["food","snack"]}}');
// ! // in this context:
// ! u := VariantSaveJson(aVariant);
// ! assert(u='{"type":{"$in":["food","snack"]}}');
// ! u := VariantSaveMongoJson(aVariant,modMongoShell);
// ! assert(u='{type:{$in:["food","snack"]}}');
// - by default, every internal value will be copied, so access of nested
// properties can be slow - if you expect the data to be read-only or not
// propagated into another place, add dvoValueCopiedByReference in Options
// will increase the process speed a lot, or use _JsonFast()
function _JsonFmt(const Format: RawUtf8; const Args, Params: array of const;
  Options: TDocVariantOptions = [dvoReturnNullForUnknownProperty]): variant; overload;

/// initialize a variant instance to store some document-based content
// from a supplied (extended) JSON content, with parameters formating
// - this overload function will set directly a local variant variable,
// and would be used by inlined _JsonFmt/_JsonFastFmt functions
procedure _JsonFmt(const Format: RawUtf8; const Args, Params: array of const;
  Options: TDocVariantOptions; out Result: variant); overload;

/// initialize a variant instance to store some document-based content
// from a supplied (extended) JSON content
// - this global function is an alias to TDocVariant.NewJson(), and
// will return TRUE if JSON content was correctly converted into a variant
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names or ObjectID()
// - by default, every internal value will be copied, so access of nested
// properties can be slow - if you expect the data to be read-only or not
// propagated into another place, add dvoValueCopiedByReference in Options
// will increase the process speed a lot, or use _JsonFast()
function _Json(const Json: RawUtf8; var Value: variant;
  Options: TDocVariantOptions = [dvoReturnNullForUnknownProperty]): boolean; overload;

/// initialize a variant instance to store some document-based object content
// - this global function is an handy alias to:
// ! Obj(NameValuePairs, JSON_FAST);
// - so all created objects and arrays will be handled by reference, for best
// speed - but you should better write on the resulting variant tree with caution
function _ObjFast(const NameValuePairs: array of const): variant; overload;

/// initialize a variant instance to store any object as a TDocVariant
// - is a wrapper around ObjectToVariant(aObject, result, aOptions)
function _ObjFast(aObject: TObject;
   aOptions: TTextWriterWriteObjectOptions = [woDontStoreDefault]): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// initialize a variant instance to store some document-based array content
// - this global function is an handy alias to:
// ! _Array(Items, JSON_FAST);
// - so all created objects and arrays will be handled by reference, for best
// speed - but you should better write on the resulting variant tree with caution
function _ArrFast(const Items: array of const): variant; overload;

/// initialize a variant instance to store some document-based content
// from a supplied (extended) JSON content
// - this global function is an handy alias to:
// ! _Json(JSON, JSON_FAST);
// so returns an Unassigned (varEmpty) variant if JSON content was not correct
// - so all created objects and arrays will be handled by reference, for best
// speed - but you should better write on the resulting variant tree with caution
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names or ObjectID()
// - will handle only currency for floating point values to avoid precision
// loss: use _JsonFastFloat() instead if you want to support double values
function _JsonFast(const Json: RawUtf8): variant;

/// initialize a variant instance to store some document-based content
// from a supplied (extended) JSON content, with double conversion
// - _JsonFast() will support only currency floats: use this method instead
// if your JSON input is likely to require double values - with potential
// precision loss
function _JsonFastFloat(const Json: RawUtf8): variant;

/// initialize a variant instance to store some extended document-based content
// - this global function is an handy alias to:
// ! _Json(JSON, JSON_FAST_EXTENDED);
function _JsonFastExt(const Json: RawUtf8): variant;

/// initialize a variant instance to store some document-based content
// from a supplied (extended) JSON content, with parameters formating
// - this global function is an handy alias e.g. to:
// ! aVariant := _JsonFmt('{%:{$in:[?,?]}}',['type'],['food','snack'], JSON_FAST);
// - so all created objects and arrays will be handled by reference, for best
// speed - but you should better write on the resulting variant tree with caution
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names or ObjectID():
function _JsonFastFmt(const Format: RawUtf8;
   const Args, Params: array of const): variant;

/// ensure a document-based variant instance will have only per-value nested
// objects or array documents
// - is just a wrapper around:
// ! TDocVariantData(DocVariant).InitCopy(DocVariant, JSON_[mDefault])
// - you can use this function to ensure that all internal properties of this
// variant will be copied per-value whatever options the nested objects or
// arrays were created with
// - for huge document with a big depth of nested objects or arrays, a full
// per-value copy may be time and resource consuming, but will be also safe
// - will raise an EDocVariant if the supplied variant is not a TDocVariant or
// a varByRef pointing to a TDocVariant
procedure _Unique(var DocVariant: variant);

/// ensure a document-based variant instance will have only per-value nested
// objects or array documents
// - is just a wrapper around:
// ! TDocVariantData(DocVariant).InitCopy(DocVariant, JSON_FAST)
// - you can use this function to ensure that all internal properties of this
// variant will be copied per-reference whatever options the nested objects or
// arrays were created with
// - for huge document with a big depth of nested objects or arrays, it will
// first create a whole copy of the document nodes, but further assignments
// of the resulting value will be per-reference, so will be almost instant
// - will raise an EDocVariant if the supplied variant is not a TDocVariant or
// a varByRef pointing to a TDocVariant
procedure _UniqueFast(var DocVariant: variant);

/// return a full nested copy of a document-based variant instance
// - is just a wrapper around:
// ! TDocVariant.NewUnique(DocVariant,JSON_[mDefault])
// - you can use this function to ensure that all internal properties of this
// variant will be copied per-value whatever options the nested objects or
// arrays were created with: to be used on a value returned as varByRef
// (e.g. by _() pseudo-method)
// - for huge document with a big depth of nested objects or arrays, a full
// per-value copy may be time and resource consuming, but will be also safe -
// consider using _ByRef() instead if a fast copy-by-reference is enough
// - will raise an EDocVariant if the supplied variant is not a TDocVariant or
// a varByRef pointing to a TDocVariant
function _Copy(const DocVariant: variant): variant;

/// return a full nested copy of a document-based variant instance
// - is just a wrapper around:
// ! TDocVariant.NewUnique(DocVariant, JSON_FAST)
// - you can use this function to ensure that all internal properties of this
// variant will be copied per-value whatever options the nested objects or
// arrays were created with: to be used on a value returned as varByRef
// (e.g. by _() pseudo-method)
// - for huge document with a big depth of nested objects or arrays, a full
// per-value copy may be time and resource consuming, but will be also safe -
// consider using _ByRef() instead if a fast copy-by-reference is enough
// - will raise an EDocVariant if the supplied variant is not a TDocVariant or
// a varByRef pointing to a TDocVariant
function _CopyFast(const DocVariant: variant): variant;

/// copy a TDocVariant to another variable, changing the options on the fly
// - note that the content (items or properties) is copied by reference,
// so consider using _Copy() instead if you expect to safely modify its content
// - will return null if the supplied variant is not a TDocVariant
function _ByRef(const DocVariant: variant;
   Options: TDocVariantOptions): variant; overload;

/// copy a TDocVariant to another variable, changing the options on the fly
// - note that the content (items or properties) is copied by reference,
// so consider using _Copy() instead if you expect to safely modify its content
// - will return null if the supplied variant is not a TDocVariant
procedure _ByRef(const DocVariant: variant; out Dest: variant;
  Options: TDocVariantOptions); overload;

/// convert a TDocVariantData array or a string value into a CSV
// - will call either TDocVariantData.ToCsv, or return the string
// - returns '' if the supplied value is neither a TDocVariant or a string
// - could be used e.g. to store either a JSON CSV string or a JSON array of
// strings in a settings property
function _Csv(const DocVariantOrString: variant): RawUtf8;

/// will convert any TObject into a TDocVariant document instance
// - fast processing function as used by _ObjFast(Value)
// - note that the result variable should already be cleared: no VarClear()
// is done by this function
// - would be used e.g. by VarRecToVariant() function
// - if you expect lazy-loading of a TObject, see TObjectVariant.New()
procedure ObjectToVariant(Value: TObject; var result: variant;
  Options: TTextWriterWriteObjectOptions = [woDontStoreDefault]); overload;

/// will convert any TObject into a TDocVariant document instance
// - convenient overloaded function to include woEnumSetsAsText option
function ObjectToVariant(Value: TObject; EnumSetsAsText: boolean): variant; overload;

/// will serialize any TObject into a TDocVariant debugging document
// - just a wrapper around _JsonFast(ObjectToJsonDebug()) with an optional
// "Context":"..." text message
// - if the supplied context format matches '{....}' then it will be added
// as a corresponding TDocVariant JSON object
function ObjectToVariantDebug(Value: TObject;
  const ContextFormat: RawUtf8; const ContextArgs: array of const;
  const ContextName: RawUtf8 = 'context'): variant; overload;

/// get the enumeration names corresponding to a set value, as a JSON array
function SetNameToVariant(Value: cardinal; Info: TRttiCustom;
  FullSetsAsStar: boolean = false): variant; overload;

/// get the enumeration names corresponding to a set value, as a JSON array
function SetNameToVariant(Value: cardinal; Info: PRttiInfo;
  FullSetsAsStar: boolean = false): variant; overload;

/// fill a class instance from a TDocVariant object document properties
// - returns FALSE if the variant is not a dvObject, TRUE otherwise
function DocVariantToObject(var doc: TDocVariantData; obj: TObject;
  objRtti: TRttiCustom = nil): boolean;

/// fill a T*ObjArray variable from a TDocVariant array document values
// - will always erase the T*ObjArray instance, and fill it from arr values
procedure DocVariantToObjArray(var arr: TDocVariantData; var objArray;
  objClass: TClass);

/// will convert a blank TObject into a TDocVariant document instance
function ObjectDefaultToVariant(aClass: TClass;
  aOptions: TDocVariantOptions): variant; overload;



{ ************** JSON Parsing into Variant }

/// low-level function to set a variant from an unescaped JSON number or string
// - expect the JSON input buffer to be already unescaped and #0 terminated,
// e.g. by TGetJsonField, and having set properly the wasString flag
// - set the varString or call GetVariantFromNotStringJson() if TryCustomVariants=nil
// - or call JsonToAnyVariant() to support TryCustomVariants^ complex input
procedure GetVariantFromJsonField(Json: PUtf8Char; wasString: boolean;
  var Value: variant; TryCustomVariants: PDocVariantOptions = nil;
  AllowDouble: boolean = false; JsonLen: integer = 0);

/// low-level function to set a variant from an unescaped JSON non string
// - expect the JSON input buffer to be already unescaped and #0 terminated,
// e.g. by TGetJsonField, and having returned wasString=false
// - is called e.g. by function GetVariantFromJsonField()
// - will recognize null, boolean, integer, Int64, currency, double
// (if AllowDouble is true) input, then set Value and return TRUE
// - returns FALSE if the supplied input has no expected JSON format
function GetVariantFromNotStringJson(Json: PUtf8Char;
  var Value: TVarData; AllowDouble: boolean): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// low-level function to parse a JSON buffer content into a variant
// - warning: will decode in the Json buffer memory itself (no memory
// allocation or copy), for faster process - so take care that it is not shared
// - internal method used by VariantLoadJson(), GetVariantFromJsonField() and
// TDocVariantData.InitJson()
// - will instantiate either an integer, Int64, currency, double or string value
// (as RawUtf8), guessing the best numeric type according to the textual content,
// and string in all other cases, except TryCustomVariants points to some options
// (e.g. @JSON_[mFast] for fast instance) and input is a known object or
// array, either encoded as strict-JSON (i.e. {..} or [..]), or with some
// extended (e.g. BSON) syntax
procedure JsonToAnyVariant(var Value: variant; var Info: TGetJsonField;
  Options: PDocVariantOptions; AllowDouble: boolean = false);

{$ifndef PUREMORMOT2}
/// low-level function to parse a JSON content into a variant
procedure GetJsonToAnyVariant(var Value: variant; var Json: PUtf8Char;
  EndOfObject: PUtf8Char; Options: PDocVariantOptions; AllowDouble: boolean);
    overload; {$ifdef HASINLINE}inline;{$endif}
{$endif PUREMORMOT2}

/// identify either varInt64, varDouble, varCurrency types following JSON format
// - any non valid number is returned as varString
// - warning: supplied JSON is expected to be not nil
function TextToVariantNumberType(Json: PUtf8Char): cardinal;

/// identify either varInt64 or varCurrency types following JSON format
// - this version won't return varDouble, i.e. won't handle more than 4 exact
// decimals (as varCurrency), nor scientific notation with exponent (1.314e10)
// - this will ensure that any incoming JSON will converted back with its exact
// textual representation, without digit truncation due to limited precision
// - any non valid number is returned as varString
// - warning: supplied JSON is expected to be not nil
function TextToVariantNumberTypeNoDouble(Json: PUtf8Char): cardinal;

/// low-level function to parse a variant from an unescaped JSON number
// - returns the position after the number, and set Value to a variant of type
// varInteger/varInt64/varCurrency (or varDouble if AllowVarDouble is true)
// - returns nil if JSON can't be converted to a number - it is likely a string
// - handle only up to 4 decimals (i.e. currency) if AllowVarDouble is false
// - matches TextToVariantNumberType/TextToVariantNumberTypeNoDouble() logic
// - see GetVariantFromNotStringJson() to check the whole Json input, and
// parse null/false/true values
function GetNumericVariantFromJson(Json: PUtf8Char;
  var Value: TVarData; AllowVarDouble: boolean): PUtf8Char;

/// convert some UTF-8 into a variant, detecting JSON numbers or constants
// - first try GetVariantFromNotStringJson() then fallback to RawUtf8ToVariant()
procedure TextToVariant(const aValue: RawUtf8; AllowVarDouble: boolean;
  out aDest: variant);

/// convert some UTF-8 buffer into a variant, detecting JSON numbers or constants
// - first try GetVariantFromNotStringJson() then fallback to RawUtf8ToVariant()
procedure TextBufferToVariant(aValue: PUtf8Char; AllowVarDouble: boolean;
  out aDest: variant);

/// convert some UTF-8 text buffer into a variant, with string interning
// - similar to TextToVariant(), but with string interning (if Interning<>nil)
// - first try GetVariantFromNotStringJson() then fallback to RawUtf8ToVariant()
procedure UniqueVariant(Interning: TRawUtf8Interning;
  var aResult: variant; aText: PUtf8Char; aTextLen: PtrInt;
  aAllowVarDouble: boolean = false); overload;

/// convert the next CSV item into a variant number or RawUtf8 varString
// - just a wrapper around GetNextItem() + TextToVariant()
function GetNextItemToVariant(var P: PUtf8Char;
  out Value: Variant; Sep: AnsiChar = ','; AllowDouble: boolean = true): boolean;

/// retrieve a variant value from a JSON number or string
// - follows TJsonWriter.AddVariant() format (calls JsonToAnyVariant)
// - make a temporary copy before parsing - use JsonToAnyVariant() on a buffer
// - return true and set Value on success, or false and empty Value on error
function VariantLoadJson(var Value: Variant; const Json: RawUtf8;
  TryCustomVariants: PDocVariantOptions = nil;
  AllowDouble: boolean = false): boolean; overload;

/// retrieve a variant value from a JSON number or string
// - just wrap VariantLoadJson(Value,Json...) procedure as a function
function VariantLoadJson(const Json: RawUtf8;
  TryCustomVariants: PDocVariantOptions = nil;
  AllowDouble: boolean = false): variant; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// just a wrapper around VariantLoadJson() with some TDocVariantOptions
// - make a temporary copy of the input Json before parsing
function JsonToVariant(const Json: RawUtf8;
  Options: TDocVariantOptions = [dvoReturnNullForUnknownProperty];
  AllowDouble: boolean = false): variant;
  {$ifdef HASINLINE} inline; {$endif}

/// just a wrapper around JsonToAnyVariant() with some TDocVariantOptions
function JsonToVariantInPlace(var Value: Variant; Json: PUtf8Char;
  Options: TDocVariantOptions = [dvoReturnNullForUnknownProperty];
  AllowDouble: boolean = false): PUtf8Char;
  {$ifdef HASINLINE} inline; {$endif}

/// decode multipart/form-data POST request content into a TDocVariantData
// - following RFC 1867
// - decoded sections are encoded as Doc JSON object with its textual values,
// or with nested objects, if the data was supplied as binary:
// ! {"name1":{"data":..,"filename":...,"contenttype":...},"name2":...}
procedure MultiPartToDocVariant(const MultiPart: TMultiPartDynArray;
  var Doc: TDocVariantData; Options: PDocVariantOptions = nil);

/// parse a "key<value" or "key<" expression for SortMatch() comparison
function ParseSortMatch(Expression: PUtf8Char; out Key: RawUtf8;
  out Match: TCompareOperator; Value: PVariant): boolean;


{ ************** Variant Binary Serialization }

{$ifndef PUREMORMOT2}

/// compute the number of bytes needed to save a Variant content
// using the VariantSave() function
// - will return 0 in case of an invalid (not handled) Variant type
// - deprecated function - use overloaded BinarySave() functions instead
function VariantSaveLength(const Value: variant): integer; deprecated;
  {$ifdef HASINLINE}inline;{$endif}


/// save a Variant content into a destination memory buffer
// - Dest must be at least VariantSaveLength() bytes long
// - will handle standard Variant types and custom types (serialized as JSON)
// - will return nil in case of an invalid (not handled) Variant type
// - will use a proprietary binary format, with some variable-length encoding
// of the string length
// - warning: will encode RTL string fields as within the variant type
// itself: using this function between UNICODE and NOT UNICODE
// versions of Delphi, will propably fail - you have been warned!
// - deprecated function - use overloaded BinarySave() functions instead
function VariantSave(const Value: variant; Dest: PAnsiChar): PAnsiChar;
  overload; deprecated;   {$ifdef HASINLINE}inline;{$endif}

{$endif PUREMORMOT2}

/// save a Variant content into a binary buffer
// - will handle standard Variant types and custom types (serialized as JSON)
// - will return '' in case of an invalid (not handled) Variant type
// - just a wrapper around VariantSaveLength()+VariantSave()
// - warning: will encode RTL string fields as within the variant type
// itself: using this function between UNICODE and NOT UNICODE
// versions of Delphi, will propably fail - you have been warned!
// - is a wrapper around BinarySave(rkVariant)
function VariantSave(const Value: variant): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve a variant value from our optimized binary serialization format
// - follow the data layout as used by RecordLoad() or VariantSave() function
// - return nil if the Source buffer is incorrect
// - in case of success, return the memory buffer pointer just after the
// read content
// - how custom type variants are created can be defined via CustomVariantOptions
// - is a wrapper around BinaryLoad(rkVariant)
function VariantLoad(var Value: variant; Source: PAnsiChar;
  CustomVariantOptions: PDocVariantOptions;
  SourceMax: PAnsiChar {$ifndef PUREMORMOT2} = nil {$endif}): PAnsiChar; overload;

/// retrieve a variant value from our optimized binary serialization format
// - follow the data layout as used by RecordLoad() or VariantSave() function
// - return varEmpty if the Source buffer is incorrect
// - just a wrapper around VariantLoad()
// - how custom type variants are created can be defined via CustomVariantOptions
// - is a wrapper around BinaryLoad(rkVariant)
function VariantLoad(const Bin: RawByteString;
  CustomVariantOptions: PDocVariantOptions): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve a variant value from variable-length buffer
// - matches TFileBufferWriter.Write()
// - how custom type variants are created can be defined via CustomVariantOptions
// - is just a wrapper around VariantLoad/BinaryLoad
procedure FromVarVariant(var Source: PByte; var Value: variant;
  CustomVariantOptions: PDocVariantOptions; SourceMax: PByte);
  {$ifdef HASINLINE}inline;{$endif}


{ ************** IDocList/IDocDict advanced Wrappers of TDocVariant Documents }

type
  IDocList = interface;
  IDocDict = interface;

  {$ifdef HASIMPLICITOPERATOR}

  /// internal map of a variant memory structure for IDocList/IDocDict wrappers
  // - implicit class operators are used to ease its work with high-level result
  // variables like RawUtf8 or IDocList/IDocDict - even as iterator variables
  // - warning: is a weak reference to the main IDocList/IDocDict, so you need to
  // explicitly convert it into stand-alone variables to use outside of the loop
  TDocValue = record
  public
    /// raw pointer access to the corresponting value IDocList/IDocDict
    // - you should not use this field, but implicit operators
    V: PVariant;
    /// returns true if the current value is a string
    function IsString: boolean; inline;
    /// returns the high-level TDocVariant type of the current value
    // - i.e. dvArray for a IDocList, dvObject for a IDocDict and dvUndefined
    // for any other value (e.g. string, number...)
    function Kind: TDocVariantKind; { better not inlined }
  public
    class operator Implicit(const A: TDocValue): boolean;  inline;
    class operator Implicit(const A: TDocValue): integer;  inline;
    class operator Implicit(const A: TDocValue): Int64;    inline;
    class operator Implicit(const A: TDocValue): string;   inline;
    class operator Implicit(const A: TDocValue): RawUtf8;  inline;
    class operator Implicit(const A: TDocValue): IDocList; inline;
    class operator Implicit(const A: TDocValue): IDocDict; inline;
    class operator Implicit(const A: TDocValue): variant;  inline;
    class operator Implicit(const A: TDocValue): PVarData; inline;
    class operator Implicit(const A: TDocValue): PDocVariantData;
  end;

  /// low-level Enumerator as returned by IDocList.GetEnumerator and
  // IDocDict.Values
  // - warning: weak reference to the main IDocList/IDocDict, so you need to
  // explicitly convert it into stand-alone variables to use outside of the loop
  TDocValueEnumerator = record
  private
    Curr, After: TDocValue;
  public
    function MoveNext: boolean; inline;
    function GetEnumerator: TDocValueEnumerator; inline;
    property Current: TDocValue
      read Curr;
  end;

  /// low-level Enumerator as returned by IDocList.Objects
  // - warning: weak reference to the main IDocList/IDocDict, so you need to
  // explicitly call IDocDict.Copy to use outside of the loop
  TDocObjectEnumerator = record
  private
    CurrDict: IDocDict; // a single instance reused during whole iteration
    Curr, After: PVariant;
    CurrDictValue: ^PDocVariantData;
    CompKey: RawUtf8;
    CompValue: Variant;
    CompFunc: TVariantCompare;
    CompMatch: TCompareOperator;
    CompKeyHasPath: boolean;
    CompKeyPrev: integer;
  public
    function MoveNext: boolean; { too complex to be inlined }
    function GetEnumerator: TDocObjectEnumerator;
    property Current: IDocDict
      read CurrDict;
  end;

  /// internal map of a key name for IDocDict wrappers
  // - warning: weak reference to the main IDocList/IDocDict, so you need to
  // explicitly convert it into stand-alone variables to use outside of the loop
  TDocKey = record
  public
    /// raw pointer access to the corresponting name
    V: PRawUtf8;
    function Equals(const txt: RawUtf8): boolean; inline;
    function Utf8: RawUtf8; inline;
  public
    class operator Implicit(const A: TDocKey): string;   inline;
    class operator Implicit(const A: TDocKey): RawUtf8;  inline;
  end;

  /// low-level Enumerator as returned by IDocDict.Keys
  // - warning: weak reference to the main IDocList/IDocDict, so you need to
  // explicitly convert it into stand-alone variables to use outside of the loop
  TDocKeyEnumerator = record
  private
    Curr, After: TDocKey;
  public
    function MoveNext: boolean; inline;
    function GetEnumerator: TDocKeyEnumerator; inline;
    property Current: TDocKey
      read Curr;
  end;

  /// store one key/value pair as returned by IDocDict.GetEnumerator
  // - thanks to TDocKey/TDocValue wrappers, no transient copy is involved
  // - warning: weak reference to the main IDocList/IDocDict, so you need to
  // explicitly convert it into stand-alone variables to use outside of the loop
  TDocDictFields = record
  public
    /// efficient/indirect access to one key of the current IDocDict element
    Key: TDocKey;
    /// efficient/indirect access to one value of the current IDocDict element
    Value: TDocValue;
    /// returns 'Key=Value' text
    function KeyValue(const separator: RawUtf8 = '='): RawUtf8;
  end;

  /// low-level Enumerator as returned by IDocDict.GetEnumerator
  // - warning: weak reference to the main IDocList/IDocDict, so you need to
  // explicitly convert it into stand-alone variables to use outside of the loop
  TDocDictEnumerator = record
  private
    Curr: TDocDictFields;
    AfterValue: TDocValue;
  public
    function MoveNext: boolean; inline;
    function GetEnumerator: TDocDictEnumerator; inline;
    property Current: TDocDictFields
      read Curr;
  end;

  {$endif HASIMPLICITOPERATOR}

  /// abstract parent with common methods to IDocList/IDocDict wrappers
  IDocAny = interface(ISerializable)
    /// remove all elements from the list/dictionary
    procedure Clear;
    /// equals dvArray for IDocList, dvObject for IDocDict
    function Kind: TDocVariantKind;
    /// the known model of the internal TDocVariantData storage
    function Model: TDocVariantModel;
    /// how many items or name/value pairs are stored in this instance
    function Len: integer;
    /// returns itself as a IDocList, or nil if is a IDocDict
    function AsList: IDocList;
    /// returns itself as a IDocDic, or nil if is a IDocList
    function AsDict: IDocDict;
    /// returns the associated TDocVariant instance
    function AsVariant: variant;
    /// low-level access to the internal TDocVariantData storage
    // - warning: is a weak reference pointer to the main IDocList/IDocDict, so
    // you need to copy it to use it outside of this instance
    function Value: PDocVariantData;
  end;

  /// exception raised by IDocList
  EDocList = class(EDocVariant)
  public
    class procedure GetRaise(method: AnsiChar; pos: integer; const v: variant);
  end;

  /// exception raised by IDocDict
  EDocDict = class(EDocVariant)
  public
    class procedure Error(method: AnsiChar; const key: RawUtf8; const v: variant);
  end;

  /// a dynamic array of IDocDict instances
  IDocDictDynArray = array of IDocDict;

  /// a List, used to store multiple values
  // - implemented via an internal TDocVariantData dvArray
  // - follows most Python Lists naming and conventions
  IDocList = interface(IDocAny)
    ['{8186E3D3-3C9C-46E6-89D1-82ADDB741AAB}']
    // methods used as getter/setter for properties
    function GetB(position: integer): boolean;
    function GetC(position: integer): currency;
    function GetD(position: integer): IDocDict;
    function GetF(position: integer): double;
    function GetI(position: integer): Int64;
    function GetItem(position: integer): variant;
    function GetL(position: integer): IDocList;
    function GetS(position: integer): string;
    function GetU(position: integer): RawUtf8;
    {$ifdef HASIMPLICITOPERATOR}
    function GetV(position: integer): TDocValue;
    {$endif HASIMPLICITOPERATOR}
    procedure SetB(position: integer; value: boolean);
    procedure SetC(position: integer; const value: currency);
    procedure SetD(position: integer; const value: IDocDict);
    procedure SetF(position: integer; const value: double);
    procedure SetI(position: integer; value: Int64);
    procedure SetItem(position: integer; const value: variant);
    procedure SetL(position: integer; const value: IDocList);
    procedure SetS(position: integer; const value: string);
    procedure SetU(position: integer; const value: RawUtf8);
    /// adds an element at the end of the list
    function Append(const value: variant): integer; overload;
    /// adds an UTF-8 text element at the end of the list
    function Append(const value: RawUtf8): integer; overload;
    /// adds an IDocList/IDocDict element at the end of the list
    function AppendDoc(const value: IDocAny): integer;
    /// compare this IDocList value with another instance
    // - each element of the list will be compared, in their expected order
    function Compare(const another: IDocList;
      caseinsensitive: boolean = false): integer;
    /// return a of a (sub-range) copy of this IDocList
    // - consider Range() if you want just to loop over some sub-range, and
    // do not need to allocate a new IDocList instance
    // - returns a new IDocList instance as Python list[start:stop] range,
    // stop position being *excluded* to the result:
    // $ list.Copy returns a copy of the whole list
    // $ list.Copy(10) returns elements #10..#Count-1, i.e. list[10:]
    // $ list.Copy(-2) returns last #Count-2..#Count-1 items, i.e. list[-2:]
    // $ list.Copy(1, 9) returns elements #1..#8, i.e. list[1..9]
    // $ list.Copy(1, -2) returns elements #1..#Count-3, i.e. list[1:-2]
    function Copy(start: integer = 0; stop: integer = 0): IDocList;
    /// counts the number of elements with the specified value
    function Count(const value: variant): integer; overload;
    /// counts the number of elements with the specified value
    function Count(const value: RawUtf8): integer; overload;
    /// removes the element at the specified position, not returning it
    function Del(position: integer): boolean;
    /// check if a specified value is present in the list
    function Exists(const value: variant): boolean; overload;
    /// check if a specified text value is present in the list
    function Exists(const value: RawUtf8;
      caseinsensitive: boolean = false): boolean; overload;
    /// add the elements of a list, to the end of the current list
    procedure Extend(const value: IDocList); overload;
    /// add the supplied elements, to the end of the current list
    procedure Extend(const value: array of const); overload;
    /// search matching expression over IDocDict kind of elements in this list
    // - expressions are e.g. 'name=Synopse' or 'info.price<100'
    function Filter(const expression: RawUtf8): IDocList; overload;
    /// search matching expression over IDocDict kind of elements in this list
    // - use e.g. Filter('name=', 'Synopse') or Filter('info.price<', MaxPrice)
    function Filter(const expression: RawUtf8; const value: variant;
      limit: integer = 0): IDocList; overload;
    /// search matching key/value over IDocDict kind of elements in this list
    // - raw search for compare(object.key,value)=match
    // - default compare=nil will use VariantCompare
    function Filter(const key: RawUtf8; const value: variant; limit: integer;
      match: TCompareOperator; compare: TVariantCompare): IDocList; overload;
    /// search the first matching expression over IDocDict kind of elements
    function First(const expression: RawUtf8): variant; overload;
    /// search the first matching expression over IDocDict kind of elements
    function First(const expression: RawUtf8; const value: variant): variant; overload;
    /// returns the position at the first occurrence of the specified value
    function Index(const value: variant): integer; overload;
    /// returns the position at the first occurrence of the specified text value
    function Index(const value: RawUtf8;
      caseinsensitive: boolean = false): integer; overload;
    /// inserts the specified value at the specified position
    function Insert(position: integer; const value: variant): integer; overload;
    /// inserts the specified value at the specified position
    function Insert(position: integer; const value: RawUtf8): integer; overload;
    /// returns all IDocDict kind of elements of this IDocList
    // - the list should consist e.g. of a JSON array of JSON objects
    // - will just ignore any element of the IDocList which is not a IDocDict
    function ObjectsDictDynArray: IDocDictDynArray;
    /// removes the element at the specified position, and returns it
    // - raise an EDocList on invalid supplied position
    function Pop(position: integer = -1): variant;
    /// removes the last inserted element into a value-owned IDocDict
    // - returns false if there is no value at this position (raise no EDocList)
    // - you may change the extraction position (negatives from Len)
    function PopItem(out value: variant; position: integer = -1): boolean; overload;
    /// removes the last inserted IDocDict, with no out-of-range error raised
    // - you may change the extraction position (negatives from Len)
    // - returns false if there is no value at this position, or it is no IDocDict
    function PopItem(out value: IDocDict; position: integer = -1): boolean; overload;
    /// extract a list of IDocDict elements which contains only specified keys
    // - could be used to filter a list of objects into a smaller dataset
    function Reduce(const keys: array of RawUtf8): IDocList;
    /// removes the first occurrence of the element with the specified value
    function Remove(const value: variant): integer; overload;
    /// removes the first occurrence of the element with the specified value
    function Remove(const value: RawUtf8;
      caseinsensitive: boolean = false): integer; overload;
    /// reverses the sorting order of the elements
    procedure Reverse;
    /// sorts the list ascending by default
    procedure Sort(reverse: boolean = false; compare: TVariantCompare = nil);
    /// sorts the IDocDict objects in the list by a given key name
    procedure SortByKeyValue(const key: RawUtf8; reverse: boolean = false;
      valuecompare: TVariantCompare = nil); overload;
    /// sorts the IDocDict objects in the list by several key names
    procedure SortByKeyValue(const keys: array of RawUtf8;
      reverse: boolean = false; valuecompare: TVariantCompare = nil); overload;
    /// low-level direct access to a stored element in TDocVariantData.Value[]
    function ValueAt(position: integer): PVariant;
    {$ifdef HASIMPLICITOPERATOR}
    /// default iterator over all elements of this IDocList, returning TDocValue
    // - warning: weak reference to the main list, unless you explicitly Copy it
    function GetEnumerator: TDocValueEnumerator;
    /// allow to iterate over a specific range of elements of this IDocList
    // - elements are returned directly from the main list as weak references
    // $ var v: TDocValue;
    // $ "for v in list.Range" returns all elements, i.e. maps "for v in list"
    // $ "for v in list.Range(10)" returns #10..#Count-1, i.e. list[10:]
    // $ "for v in list.Range(-2)" returns last #Count-2..#Count-1, i.e. list[-2:]
    // $ "for v in list.Range(1, 9)" returns #1..#8, i.e. list[1..9]
    // $ "for v in list.Range(1, -2)" returns #1..#Count-3, i.e. list[1:-2]
    function Range(start: integer = 0; stop: integer = 0): TDocValueEnumerator;
    /// iterate over IDocDict kind of elements in this IDocList
    // - the list should consist e.g. of a JSON array of JSON objects
    // - will just ignore any element of the IDocList which is not a IDocDict
    // - TDocObjectEnumerator will maintain and reuse a single IDocDict instance
    // - warning: IDocDict is a weak reference, explicitly Copy outside the loop
    function Objects: TDocObjectEnumerator; overload;
    /// iterate matching expression over IDocDict kind of elements in this IDocList
    // - expressions are e.g. 'name=Synopse' or 'info.price<100'
    // - warning: IDocDict is a weak reference, explicitly Copy outside the loop
    function Objects(const expression: RawUtf8): TDocObjectEnumerator; overload;
    /// iterate matching expression over IDocDict kind of elements in this IDocList
    // - use e.g. Objects('name =', 'Synopse') or Objects('info.price<', MaxPrice)
    // - warning: IDocDict is a weak reference, explicitly Copy outside the loop
    function Objects(const expression: RawUtf8;
      const value: variant): TDocObjectEnumerator; overload;
    /// iterate matching key/value over IDocDict kind of elements in this IDocList
    // - raw search for compare(object.key,value)=match
    // - warning: IDocDict is a weak reference, explicitly Copy outside the loop
    function Objects(const key: RawUtf8; const value: variant;
      match: TCompareOperator; compare: TVariantCompare): TDocObjectEnumerator; overload;
    /// read access of one element in the list, as TDocValue
    // - may be more convenient than the plain variant as returned by Item[]
    // - warning: weak reference to the main list, unless you explicitly Copy it
    property V[position: integer]: TDocValue
      read GetV;
    {$endif HASIMPLICITOPERATOR}
    /// access one element in the list, as variant
    // - this is the default property of this instance so list[n] gives
    // direct access to the element at index 0 <= n < len -1 in the IDocList
    // - negative positions are retrieved from the end, e.g. list[-1] maps
    // the last element of the list
    // - raise an EDocList exception on out-of-range position
    // - U[] S[] I[] F[] C[] B[] L[] D[] are faster and safer if you expect
    // to retrieve a specific value type
    property Item[position: integer]: variant
      read GetItem write SetItem; default;
    /// access one element in the list, as UTF-8 text
    property U[position: integer]: RawUtf8
      read GetU write SetU;
    /// access one element in the list, as RTL String text
    property S[position: integer]: string
      read GetS write SetS;
    /// access one element in the list, as Integer
    property I[position: integer]: Int64
      read GetI write SetI;
    /// access one element in the list, as floating-point Double
    property F[position: integer]: double
      read GetF write SetF;
    /// access one element in the list, as fixed precision Currency
    property C[position: integer]: currency
      read GetC write SetC;
    /// access one element in the list, as boolean
    property B[position: integer]: boolean
      read GetB write SetB;
    /// access one element in the list, as IDocList (List)
    // - warning: weak reference to the main list, unless you explicitly Copy it
    property L[position: integer]: IDocList
      read GetL write SetL;
    /// access one element in the list, as IDocDict (Dictionary)
    // - warning: weak reference to the main list, unless you explicitly Copy it
    property D[position: integer]: IDocDict
      read GetD write SetD;
    /// access one element in the list, as IDocList/IDocArray
    // - property alias, for compatibility with existing code
    // - warning: weak reference to the main list, unless you explicitly Copy it
    property A[position: integer]: IDocList
      read GetL write SetL;
    /// access one element in the list, as IDocDict/IDocObject
    // - property alias, for compatibility with existing code
    // - warning: weak reference to the main list, unless you explicitly Copy it
    property O[position: integer]: IDocDict
      read GetD write SetD;
  end;

  /// a Dictionary, used to store key:value pairs
  // - implemented via an internal TDocVariantData dvObject
  // - follows most Python Dictionaries naming and conventions
  // - keys are by default searched on the ground object, but you can set
  // PathDelim e.g. to '.' to enable sub-object location (and insertion)
  IDocDict = interface(IDocAny)
    ['{E4176BFF-AFDC-4A63-8DBD-D4F495D56B1A}']
    // methods used as getter/setter for properties
    function GetB(const key: RawUtf8): boolean;
    function GetC(const key: RawUtf8): currency;
    function GetD(const key: RawUtf8): IDocDict;
    function GetF(const key: RawUtf8): double;
    function GetI(const key: RawUtf8): Int64;
    function GetItem(const key: RawUtf8): variant;
    function GetL(const key: RawUtf8): IDocList;
    function GetS(const key: RawUtf8): string;
    function GetU(const key: RawUtf8): RawUtf8;
    {$ifdef HASIMPLICITOPERATOR}
    function GetV(const key: RawUtf8): TDocValue;
    {$endif HASIMPLICITOPERATOR}
    procedure SetB(const key: RawUtf8; const value: boolean);
    procedure SetC(const key: RawUtf8; const value: currency);
    procedure SetD(const key: RawUtf8; const value: IDocDict);
    procedure SetF(const key: RawUtf8; const value: double);
    procedure SetI(const key: RawUtf8; const value: Int64);
    procedure SetItem(const key: RawUtf8; const value: variant);
    procedure SetL(const key: RawUtf8; const value: IDocList);
    procedure SetS(const key: RawUtf8; const value: string);
    procedure SetU(const key: RawUtf8; const value: RawUtf8);
    function GetPathDelim: AnsiChar;
    procedure SetPathDelim(value: AnsiChar);
    /// compare this IDocDict value with another instance
    // - each key/value pair will be compared, in their expected order
    function Compare(const another: IDocDict;
      caseinsensitive: boolean = false): integer; overload;
    /// compare the specified keys of this IDocDict value with another instance
    function Compare(const another: IDocDict; const keys: array of RawUtf8;
      caseinsensitive: boolean = false): integer; overload;
    /// returns a copy of the specified dictionary
    function Copy: IDocDict;
    /// removes the element at the specified key, not returning it
    function Del(const key: RawUtf8): boolean;
    /// check if the specified key exists in the dictionary
    function Exists(const key: RawUtf8): boolean;
    /// access one element in the dictionary, as variant
    // - if the key does not exist, returns varEmpty and raise no exception
    function Get(const key: RawUtf8): variant; overload;
    /// access one element in the dictionary, as variant
    // - if the key does not exist, returns the supplied default value
    function GetDef(const key: RawUtf8; const default: variant): variant; overload;
    /// access one element in the dictionary, as variant
    // - if the key does not exist, returns the supplied default value
    function GetDef(const key: RawUtf8; const default: RawUtf8): variant; overload;
    /// access one element in the dictionary, as variant
    // - if the key does not exist, returns false
    function Get(const key: RawUtf8; var value: variant): boolean; overload;
    /// access one element in the dictionary, as UTF-8 text
    // - if the key does not exist, returns false
    function Get(const key: RawUtf8; var value: RawUtf8): boolean; overload;
    /// access one element in the dictionary, as RTL string text
    // - if the key does not exist, returns false
    function Get(const key: RawUtf8; var value: string): boolean; overload;
    /// access one element in the dictionary, as boolean
    // - if the key does not exist or is not an integer, returns false
    function Get(const key: RawUtf8; var value: boolean): boolean; overload;
    /// access one element in the dictionary, as 32-bit integer
    // - if the key does not exist or is not an integer, returns false
    function Get(const key: RawUtf8; var value: integer): boolean; overload;
    /// access one element in the dictionary, as 64-bit integer
    // - if the key does not exist or is not an integer, returns false
    function Get(const key: RawUtf8; var value: Int64): boolean; overload;
    /// access one element in the dictionary, as floating-point double
    // - if the key does not exist or can not be converted, returns false
    function Get(const key: RawUtf8; var value: double): boolean; overload;
    /// access one element in the dictionary, fixed precision currency
    // - if the key does not exist or can not be converted, returns false
    function Get(const key: RawUtf8; var value: currency): boolean; overload;
    /// access one element in the dictionary, as by-reference IDocList
    // - if the key does not exist or is not a IDocList, returns false
    function Get(const key: RawUtf8; var value: IDocList): boolean; overload;
    /// access one element in the dictionary, as a by-reference IDocDict
    // - if the key does not exist or is not a IDocDict, returns false
    function Get(const key: RawUtf8; var value: IDocDict): boolean; overload;
    /// access one element in the dictionary, as by-reference TDocVariantData
    // - if the key does not exist or is not a IDocList, returns false
    function Get(const key: RawUtf8; var value: PDocVariantData): boolean; overload;
    /// removes the specified element from the dictionary
    // - if the key does not exist, raise a EDocDict exception
    function Pop(const key: RawUtf8): variant; overload;
    /// removes the specified element from the dictionary
    // - if the key does not exist, returns the supplied default value
    function Pop(const key: RawUtf8; const default: variant): variant; overload;
    /// removes the last inserted key-value pair into the dictionary
    // - returns false if the dictionary is empty
    // - you may change the extraction position (negatives from Len)
    function PopItem(out key: RawUtf8; out value: variant;
      position: integer = -1): boolean;
    /// extract into a new IDocDict which contains only specified keys
    // - could be used to filter unneeded fields in an object
    function Reduce(const keys: array of RawUtf8): IDocDict;
    /// returns the value of the specified key, or insert null for this key
    function SetDefault(const key: RawUtf8): variant; overload;
    /// returns the value of the specified key, or insert the specified value
    function SetDefault(const key: RawUtf8; const default: variant): variant; overload;
    /// sorts the dictionary content by their key names
    // - follow dvoNameCaseSensitive option by default, or supplied keycompare
    // - once sorted, key lookup will use O(log(n)) - faster than default O(n)
    procedure Sort(reverse: boolean = false; keycompare: TUtf8Compare = nil);
    /// updates (or inserts) the specified key/value pair
    procedure Update(const key: RawUtf8; const value: variant); overload;
    /// updates (or inserts) the specified key/value pairs
    procedure Update(const keyvalues: array of const); overload;
    /// updates (or inserts) the specified key/value pairs of another IDocDict
    procedure Update(const source: IDocDict; addonlymissing: boolean = false); overload;
    /// low-level direct access to a stored element in TDocVariantData.Value[]
    function ValueAt(const key: RawUtf8): PVariant;
    {$ifdef HASIMPLICITOPERATOR}
    /// default iterator over all key/value pairs of this IDocDict
    function GetEnumerator: TDocDictEnumerator;
    /// allow to iterate over all keys of this IDocDict
    function Keys: TDocKeyEnumerator;
    /// allow to iterate over all values of this IDocDict
    function Values: TDocValueEnumerator;
    /// read access of one element in the dictionary from its key, as TDocValue
    // - may be slightly more convenient than the plain variant returned by Item[]
    property V[const key: RawUtf8]: TDocValue
      read GetV;
    {$endif HASIMPLICITOPERATOR}
    /// enable nested objects location in keys for Get() and Item[]
    // - equals #0 by default, meaning only root object keys are located, e.g.
    // dict.D['child2']
    // - if PathDelim is e.g. set to '.', then dict.U['child2.name'] matches
    // dict.D['child2'].U['name']
    // - note that if the sub object does not exist, setting a value will force
    // its creation (and all needed hierarchy)
    property PathDelim: AnsiChar
      read GetPathDelim write SetPathDelim;
    /// access one element in the dictionary from its key, as variant
    // - this is the default property of this instance so dict[name] gives
    // direct access to each value in the IDocDict
    // - follows dvoNameCaseSensitive and dvoReturnNullForUnknownProperty options
    // - see Get() overloaded methods to silently check for a key existence
    // - PathDelim can be set to locate (and create) the key by its full path
    // - U[] S[] I[] F[] C[] B[] L[] D[] are faster and safer if you expect
    // to retrieve a specific value type
    property Item[const key: RawUtf8]: variant
      read GetItem write SetItem; default;
    /// access one element in the dictionary from its key, as UTF-8 text
    property U[const key: RawUtf8]: RawUtf8
      read GetU write SetU;
    /// access one element in the dictionary from its key, as RTL string text
    property S[const key: RawUtf8]: string
      read GetS write SetS;
    /// access one element in the dictionary from its key, as integer
    property I[const key: RawUtf8]: Int64
      read GetI write SetI;
    /// access one element in the dictionary from its key, as floating-point double
    property F[const key: RawUtf8]: double
      read GetF write SetF;
    /// access one element in the dictionary from its key, as fixed precision currency
    property C[const key: RawUtf8]: currency
      read GetC write SetC;
    /// access one element in the dictionary from its key, as boolean
    property B[const key: RawUtf8]: boolean
      read GetB write SetB;
    /// access one element in the dictionary from its key, as IDocList
    property L[const key: RawUtf8]: IDocList
      read GetL write SetL;
    /// access one element in the dictionary from its key, as IDocDict
    property D[const key: RawUtf8]: IDocDict
      read GetD write SetD;
    /// access one element in the dictionary from its key, as IDocList/IDocArray
    // - property alias, for compatibility with existing code
    property A[const key: RawUtf8]: IDocList
      read GetL write SetL;
    /// access one element in the dictionary from its key, as IDocDict/IDocObject
    // - property alias, for compatibility with existing code
    property O[const key: RawUtf8]: IDocDict
      read GetD write SetD;
  end;

  /// alias to our interface list type, for compatibility with existing code
  IDocArray = IDocList;
  /// alias to our interface dictionary type, for compatibility with existing code
  IDocObject = IDocDict;


/// create a self-owned void IDocList
function DocList(model: TDocVariantModel = mFastFloat): IDocList; overload;

/// create a self-owned IDocList from a JSON array
function DocList(const json: RawUtf8;
  model: TDocVariantModel = mFastFloat): IDocList; overload;

/// create a self-owned IDocList from TOrmTableJson ORM/DB dual JSON formats
function DocListFromResults(const json: RawUtf8;
  model: TDocVariantModel = mFastFloat): IDocList;

/// create a self-owned IDocList from a set of values
function DocList(const values: array of const;
  model: TDocVariantModel = mFastFloat): IDocList; overload;

/// create a IDocList as weak reference to a TDocVariant dvArray
function DocListFrom(const v: variant): IDocList; overload;

/// create a self-owned IDocList from a dynamic array of IDocDict values
function DocListFrom(const dictarray: IDocDictDynArray): IDocList; overload;

/// create a IDocList as weak reference to a TDocVariantData dvArray
function DocList(const dv: TDocVariantData): IDocList; overload;

/// create a self-owned IDocList as full copy of a TDocVariantData dvArray
function DocListCopy(const dv: TDocVariantData): IDocList; overload;

/// create a self-owned IDocList as full copy of a TDocVariant dvArray
function DocListCopy(const v: variant): IDocList; overload;

/// create a self-owned IDocList as full copy of a TDocVariantData dvArray
// and a specific options model
function DocListCopy(const dv: TDocVariantData;
  model: TDocVariantModel): IDocList; overload;


/// create a self-owned void IDocDict
function DocDict(model: TDocVariantModel = mFastFloat): IDocDict; overload;

/// create a self-owned IDocDict from a JSON object
function DocDict(const json: RawUtf8;
  model: TDocVariantModel = mFastFloat): IDocDict; overload;

/// create an array of self-owned IDocDict from a JSON array of JSON objects
// - set jsonfromresults=true if input is TOrmTableJson ORM/DB dual JSON formats
// - any element of the JSON array which is not a JSON object will be ignored
function DocDictDynArray(const json: RawUtf8;
  model: TDocVariantModel = mFastFloat;
  jsonfromresults: boolean = false): IDocDictDynArray;

/// create a self-owned IDocDict from a set of key,value pairs
function DocDict(const keyvalues: array of const;
  model: TDocVariantModel = mFastFloat): IDocDict; overload;

/// create a self-owned IDocDict from a set of keys - values will be Null
function DocDictFromKeys(const keys: array of RawUtf8;
  model: TDocVariantModel = mFastFloat): IDocDict; overload;

/// create a self-owned IDocDict from a set of keys and a gien value
function DocDictFromKeys(const keys: array of RawUtf8; const value: variant;
  model: TDocVariantModel = mFastFloat): IDocDict; overload;

/// create a IDocDict as weak reference to a TDocVariant dvObject
function DocDictFrom(const v: variant): IDocDict;

/// create a IDocDict as weak reference to a TDocVariantData dvObject
function DocDict(const dv: TDocVariantData): IDocDict; overload;

/// create a self-owned IDocDict as full copy of a TDocVariantData dvObject
function DocDictCopy(const dv: TDocVariantData): IDocDict; overload;

/// create a self-owned IDocDict as full copy of a TDocVariant dvObject
function DocDictCopy(const v: variant): IDocDict; overload;

/// create a self-owned IDocDict as full copy of a TDocVariantData dvObject
// and a specific options model
function DocDictCopy(const dv: TDocVariantData;
  model: TDocVariantModel): IDocDict; overload;

var
  /// default TDocVariant model for IDocList/IDocDict
  DocAnyDefaultModel: TDocVariantModel = mFastFloat;

/// internal initialization function called from mormot.core.json
procedure InitializeVariantsJson;


implementation

// some early methods implementation, defined here for proper inlining
// 32-bit PInteger() is faster than 16-bit (dvoXXX in VOptions) on Intel CPUs

const
  _DVO = 1 shl ord(dvoIsArray) + 1 shl ord(dvoIsObject);

function TDocVariantData.GetKind: TDocVariantKind;
begin // [dvoIsArray]=1 [dvoIsObject]=2 -> dvUndefined=0 dvArray=1 dvObject=2
  result := TDocVariantKind((TRttiVarData(self).VType shr 16) and _DVO);
end;

function TDocVariantData.Has(dvo: TDocVariantOption): boolean;
begin
  result := (TRttiVarData(self).VType and (1 shl (ord(dvo) + 16))) <> 0;
end;

function TDocVariantData.IsObject: boolean;
begin
  result := Has(dvoIsObject);
end;

function TDocVariantData.IsArray: boolean;
begin
  result := Has(dvoIsArray);
end;

function TDocVariantData.IsCaseSensitive: boolean;
begin
  result := Has(dvoNameCaseSensitive);
end;

procedure TDocVariantData.ClearFast;
begin
  TRttiVarData(self).VType := 0; // clear VType and VOptions
  Void;
end;

procedure TDocVariantData.InternalSetValue(aIndex: PtrInt; const aValue: variant);
begin
  SetVariantByValue(aValue, VValue[aIndex]); // caller ensured that aIndex is OK
  if Has(dvoInternValues) then
    InternalUniqueValueAt(aIndex);
end;


{ ************** Low-Level Variant Wrappers }

function VarIs(const V: Variant; const VTypes: TVarDataTypes): boolean;
var
  vd: PVarData;
  vt: cardinal;
begin
  vd := @V;
  repeat
    vt := vd^.VType;
    if vt <> varVariantByRef then
      break;
    vd := vd^.VPointer;
    if vd = nil then
    begin
      result := false;
      exit;
    end;
  until false;
  result := vt in VTypes;
end;

function VarIsVoid(const V: Variant): boolean;
var
  vt: cardinal;
  custom: TSynInvokeableVariantType;
begin
  vt := TVarData(V).VType;
  with TVarData(V) do
    case vt of
      varEmpty,
      varNull:
        result := true;
      varBoolean:
        result := not VBoolean;
      {$ifdef HASVARUSTRING}
      varUString,
      {$endif HASVARUSTRING}
      varString,
      varOleStr:
        result := VAny = nil;
      varDate:
        result := VInt64 = 0;
      // note: 0 as integer or float is considered as non-void
    else
      if vt = varVariantByRef then
        result := VarIsVoid(PVariant(VPointer)^)
      else if (vt = varStringByRef) or
              (vt = varOleStrByRef)
              {$ifdef HASVARUSTRING} or
              (vt = varUStringByRef)
              {$endif HASVARUSTRING} then
        result := PPointer(VAny)^ = nil
      else if vt = DocVariantVType then
        result := TDocVariantData(V).Count = 0
      else
      begin
        custom := FindSynVariantType(vt);
        result := (custom <> nil) and
                  custom.IsVoid(TVarData(V)); // e.g. TBsonVariant.IsVoid
      end;
    end;
end;

function VarStringOrNull(const v: RawUtf8): variant;
begin
  if v = '' then
    SetVariantNull(result{%H-})
  else
    RawUtf8ToVariant(v, result);
end;

procedure SetVariantByRef(const Source: Variant; var Dest: Variant);
var
  vt: cardinal;
begin
  if PInteger(@Dest)^ <> 0 then // VarClear() is not always inlined :(
    VarClear(Dest);
  vt := TVarData(Source).VType;
  if ((vt and varByRef) <> 0) or
     (vt in VTYPE_SIMPLE) then
    TVarData(Dest) := TVarData(Source)
  else if not SetVariantUnRefSimpleValue(Source, TVarData(Dest)) then
  begin
    TRttiVarData(Dest).VType := varVariantByRef;
    TVarData(Dest).VPointer := @Source;
  end;
end;

procedure SetVariantByValue(const Source: Variant; var Dest: Variant);
var
  s: PVarData;
  d: TVarData absolute Dest;
  dt: cardinal absolute Dest;
  vt: cardinal;
  ct: TSynInvokeableVariantType;
begin
  s := @Source;
  if PInteger(@Dest)^ <> 0 then // VarClear() is not always inlined :(
    VarClear(Dest);
  vt := s^.VType;
  while vt = varVariantByRef do
  begin
    s := s^.VPointer;
    vt := s^.VType;
  end;
  case vt of
    varEmpty..varDate,
    varBoolean,
    varShortInt..varWord64:
      begin
        dt := vt;
        d.VInt64 := s^.VInt64;
      end;
    varString:
      begin
        dt := varString;
        d.VAny := nil;
        RawByteString(d.VAny) := RawByteString(s^.VAny);
      end;
    varStringByRef:
      begin
        dt := varString;
        d.VAny := nil;
        RawByteString(d.VAny) := PRawByteString(s^.VAny)^;
      end;
    {$ifdef HASVARUSTRING}
    varUString,
    varUStringByRef,
    {$endif HASVARUSTRING}
    varOleStr,
    varOleStrByRef:
      begin
        dt := varString;
        d.VAny := nil;
        VariantToUtf8(PVariant(s)^, RawUtf8(d.VAny)); // store as RawUtf8
      end;
  else // note: varVariant should not happen here
    if DocVariantType.FindSynVariantType(vt, ct) then
      ct.CopyByValue(d, s^) // needed e.g. for TBsonVariant
    else
      SetVariantUnRefSimpleValue(PVariant(s)^, d);
  end;
end;

procedure ZeroFill(Value: PVarData);
begin
  // slightly faster than FillChar(Value,SizeOf(Value),0);
  PInt64Array(Value)^[0] := 0;
  PInt64Array(Value)^[1] := 0;
  {$ifdef CPU64}
  PInt64Array(Value)^[2] := 0;
  {$endif CPU64}
end;

procedure FillZero(var value: variant);
begin
  if TRttiVarData(value).VType and $ffff = varString then
    FillZero(RawByteString(TVarData(value).VAny));
  VarClear(value);
end;

procedure _VariantClearSeveral(V: PVarData; n: integer);
var
  vt, docv: cardinal;
  handler: TCustomVariantType;
  clearproc: procedure(V: PVarData);
label
  clr, hdr;
begin
  handler := nil;
  docv := DocVariantVType;
  clearproc := @VarClearProc;
  repeat
    vt := V^.VType;
    if vt <= varWord64 then
    begin
      if (vt >= varOleStr) and
         (vt <= varError) then
        if vt = varOleStr then
          WideString(V^.VAny) := ''
        else
          goto clr; // varError/varDispatch
    end // note: varVariant/varUnknown are not handled because should not appear
    else if vt = varString then
      {$ifdef FPC}
      FastAssignNew(V^.VAny)
      {$else}
      RawUtf8(V^.VAny) := ''
      {$endif FPC}
    else if vt < varByRef then // varByRef has no refcount -> nothing to clear
      if vt = docv then
        PDocVariantData(V)^.ClearFast // faster than Clear
      {$ifdef HASVARUSTRING}
      else if vt = varUString then
        UnicodeString(V^.VAny) := ''
      {$endif HASVARUSTRING}
      else if vt >= varArray then // custom types are below varArray
clr:    clearproc(V)
      else if handler = nil then
        if FindCustomVariantType(vt, handler) then
hdr:      handler.Clear(V^)
        else
          goto clr
      else if vt = handler.VarType then
        goto hdr
      else
        goto clr;
    PInteger(V)^ := varEmpty; // reset VType
    inc(V);
    dec(n);
  until n = 0;
end;

procedure RawUtf8ToVariant(const Txt: RawUtf8; var Value: TVarData;
  ExpectedValueType: cardinal);
begin
  if ExpectedValueType = varString then
  begin
    RawUtf8ToVariant(Txt, variant(Value));
    exit;
  end;
  VarClearAndSetType(variant(Value), ExpectedValueType);
  Value.VAny := nil; // avoid GPF below
  if Txt <> '' then
    case ExpectedValueType of
      varOleStr:
        Utf8ToWideString(Txt, WideString(Value.VAny));
      {$ifdef HASVARUSTRING}
      varUString:
        Utf8DecodeToUnicodeString(
          pointer(Txt), length(Txt), UnicodeString(Value.VAny));
      {$endif HASVARUSTRING}
    else
      raise ESynVariant.CreateUtf8('RawUtf8ToVariant(%)?', [ExpectedValueType]);
    end;
end;

function VariantToString(const V: Variant): string;
begin
  VariantToString(V, result);
end;

procedure VariantToString(const V: Variant; var result: string);
var
  wasString: boolean;
  tmp: RawUtf8;
  vt: cardinal;
begin
  vt := TVarData(V).VType;
  case vt of
    varEmpty,
    varNull:
      result := ''; // default VariantToUtf8(null)='null'
    {$ifdef UNICODE} // not HASVARUSTRING: here we handle string=UnicodeString
    varOleStr:
      SetString(result, PWideChar(TVarData(V).VAny), length(WideString(TVarData(V).VAny)));
    varUString:
      result := UnicodeString(TVarData(V).VAny);
    varUStringByRef:
      result := PUnicodeString(TVarData(V).VAny)^;
    varOleStrByRef:
      SetString(result, PPWideChar(TVarData(V).VAny)^,
        length(PWideString(TVarData(V).VAny)^));
    {$endif UNICODE}
  else
    begin
      VariantToUtf8(V, tmp, wasString);
      if tmp = '' then
        result := ''
      else
      {$ifndef UNICODE}
      if not wasString or
         (Unicode_CodePage = CP_UTF8) or
         IsAnsiCompatible(tmp) then
        result := tmp
      else
      {$endif UNICODE}
        Utf8ToStringVar(tmp, result);
    end;
  end;
end;

procedure VariantToVarRec(const V: variant; var result: TVarRec);
begin
  result.VType := vtVariant;
  if TVarData(V).VType = varVariantByRef then
    result.VVariant := TVarData(V).VPointer
  else
    result.VVariant := @V;
end;

procedure VariantsToArrayOfConst(const V: array of variant; VCount: PtrInt;
  out result: TTVarRecDynArray);
var
  i: PtrInt;
begin
  SetLength(result, VCount);
  for i := 0 to VCount - 1 do
    VariantToVarRec(V[i], result[i]);
end;

function VariantsToArrayOfConst(const V: array of variant): TTVarRecDynArray;
begin
  VariantsToArrayOfConst(V, length(V), result);
end;

function RawUtf8DynArrayToArrayOfConst(const V: array of RawUtf8): TTVarRecDynArray;
var
  i: PtrInt;
begin
  result := nil;
  SetLength(result, Length(V));
  for i := 0 to Length(V) - 1 do
  begin
    result[i].VType := vtAnsiString;
    result[i].VAnsiString := pointer(V[i]);
  end;
end;

function VarRecToVariant(const V: TVarRec): variant;
begin
  VarRecToVariant(V, result);
end;

procedure VarRecToVariant(const V: TVarRec; var result: variant);
begin
  VarClear(result{%H-});
  with TRttiVarData(result) do
    case V.VType of
      vtPointer:
        VType := varNull;
      vtBoolean:
        begin
          VType := varBoolean;
          Data.VBoolean := V.VBoolean;
        end;
      vtInteger:
        begin
          VType := varInteger;
          Data.VInteger := V.VInteger;
        end;
      vtInt64:
        begin
          VType := varInt64;
          Data.VInt64 := V.VInt64^;
        end;
      {$ifdef FPC}
      vtQWord:
        begin
          VType := varWord64;
          Data.VQWord := V.VQWord^;
        end;
      {$endif FPC}
      vtCurrency:
        begin
          VType := varCurrency;
          Data.VInt64 := PInt64(V.VCurrency)^;
        end;
      vtExtended:
        begin
          VType := varDouble;
          Data.VDouble := V.VExtended^;
        end;
      vtVariant:
        result := V.VVariant^;
      // warning: use varStringByRef makes GPF -> safe and fast refcount
      vtAnsiString:
        begin
          VType := varString;
          Data.VAny := nil;
          RawByteString(Data.VAny) := RawByteString(V.VAnsiString);
        end;
      {$ifdef HASVARUSTRING}
      vtUnicodeString,
      {$endif HASVARUSTRING}
      vtWideString,
      vtString,
      vtPChar,
      vtChar,
      vtWideChar,
      vtClass:
        begin
          VType := varString;
          Data.VString := nil; // avoid GPF on next line
          VarRecToUtf8(V, RawUtf8(Data.VString)); // decode as new RawUtf8
        end;
      vtObject:
        // class instance will be serialized as a TDocVariant
        ObjectToVariant(V.VObject, result, [woDontStoreDefault]);
    else
      raise ESynVariant.CreateUtf8('Unhandled TVarRec.VType=%', [V.VType]);
    end;
end;

function VariantDynArrayToJson(const V: TVariantDynArray): RawUtf8;
var
  tmp: TDocVariantData;
begin
  tmp.InitArrayFromVariants(V);
  result := tmp.ToJson;
end;

function VariantDynArrayToRawUtf8DynArray(const V: TVariantDynArray): TRawUtf8DynArray;
var
  i: PtrInt;
  ws: boolean;
begin
  result := nil;
  if V = nil then
    exit;
  SetLength(result, length(V));
  for i := 0 to length(V) - 1 do
    VariantToUtf8(V[i], result[i], ws);
end;

function JsonToVariantDynArray(const Json: RawUtf8): TVariantDynArray;
var
  tmp: TDocVariantData;
begin
  tmp.InitJson(Json, JSON_FAST);
  result := tmp.VValue;
  if result <> nil then
    DynArrayFakeLength(result, tmp.Count);
end;

function ValuesToVariantDynArray(const items: array of const): TVariantDynArray;
var
  tmp: TDocVariantData;
begin
  tmp.InitArray(items, JSON_FAST);
  result := tmp.VValue;
  if result <> nil then
    DynArrayFakeLength(result, tmp.Count);
end;


function SortDynArrayEmptyNull(const A, B): integer;
begin
  result := 0; // VType=varEmpty/varNull are always equal
end;

function SortDynArrayWordBoolean(const A, B): integer;
begin
  if WordBool(A) then // normalize
    if WordBool(B) then
      result := 0
    else
      result := 1
  else if WordBool(B) then
    result := -1
  else
    result := 0;
end;

const
  _VARDATATEXT: array[0.. varWord64 + 5] of string[15] = (
    'Empty', 'Null', 'SmallInt', 'Integer', 'Single', 'Double', 'Currency',
    'Date', 'OleStr', 'Dispatch', 'Error', 'Boolean', 'Variant', 'Unknown',
    'Decimal', '15', 'ShortInt', 'Byte', 'Word', 'LongWord', 'Int64', 'QWord',
    'String', 'UString', 'Any', 'Array', 'DocVariant');
var
  _VariantTypeNameAsInt: shortstring; // seldom called

function VariantTypeName(V: PVarData): PShortString;
var
  vt: PtrUInt;
  ct: TSynInvokeableVariantType;
  tmp: TVarData;
begin
  vt := V.VType;
  if vt > varWord64 then
  repeat
    if SetVariantUnRefSimpleValue(PVariant(V)^, tmp{%H-}) then
    begin
      V := @tmp;
      vt := tmp.VType;
      if vt <= varWord64 then
        break;
    end;
    case vt of
      varStrArg,
      varString,
      varStringByRef:
        vt := varWord64 + 1;
      {$ifdef HASVARUSTRARG}
      varUStrArg,
      {$endif HASVARUSTRARG}
      {$ifdef HASVARUSTRING}
      varUString,
      varUStringByRef:
        vt := varWord64 + 2;
      {$endif HASVARUSTRING}
      varAny:
        vt := varWord64 + 3;
      varArray:
        vt := varWord64 + 4;
      varVariantByRef:
        begin
          result := VariantTypeName(V^.VPointer);
          exit;
        end;
    else
      if vt = DocVariantVType then
        vt := varWord64 + 5
      else
      begin
        ct := FindSynVariantType(vt);
        if ct = nil then
        begin
          str(vt, _VariantTypeNameAsInt);
          result := @_VariantTypeNameAsInt; // return VType as number
        end
        else
          result := PPointer(PPtrInt(ct)^ + vmtClassName)^;
        exit;
      end
    end;
    break;
  until false;
  result := @_VARDATATEXT[vt];
end;

function VariantTypeName(const V: variant): PShortString;
begin
  result := VariantTypeName(@V);
end;

const
  _CMP2SORT: array[0..18] of TDynArraySortCompare = (
    nil,                         // 0
    SortDynArrayEmptyNull,       // 1
    SortDynArraySmallInt,        // 2
    SortDynArrayInteger,         // 3
    SortDynArraySingle,          // 4
    SortDynArrayDouble,          // 5
    SortDynArrayInt64,           // 6
    SortDynArrayDouble,          // 7
    SortDynArrayShortInt,        // 8
    SortDynArrayByte,            // 9
    SortDynArrayWord,            // 10
    SortDynArrayCardinal,        // 11
    SortDynArrayInt64,           // 12
    SortDynArrayQWord,           // 13
    SortDynArrayWordBoolean,     // 14
    {$ifdef CPUINTEL}
    SortDynArrayAnsiString,      // 15
    {$else}
    SortDynArrayRawByteString,
    {$endif CPUINTEL}
    SortDynArrayAnsiStringI,     // 16
    SortDynArrayUnicodeString,   // 17
    SortDynArrayUnicodeStringI); // 18
var
  // FastVarDataComp() efficient lookup for per-VType comparison function
  _VARDATACMP: array[0 .. $102 {varUString}, boolean] of byte; // _CMP2SORT[]

function FastVarDataComp(A, B: PVarData; caseInsensitive: boolean): integer;
var
  at, bt, cmp2sort: PtrUInt;
  ah, bh: TSynInvokeableVariantType;
begin
  at := PtrUInt(A); // A=nil -> at=varEmpty
  if at <> 0 then
    repeat
      at := PVarData(at)^.VType;
      if at <> varVariantByRef then
        break;
      at := PtrUInt(A.VPointer);
      A := pointer(at);
    until at = 0;
  bt := PtrUInt(B);
  if bt <> 0 then
    repeat
      bt := PVarData(bt)^.VType;
      if bt <> varVariantByRef then
        break;
      bt := PtrUInt(B.VPointer);
      B := pointer(bt);
    until bt = 0;
  if at = bt then
    // optimized comparison if A and B share the same type (most common case)
    if at <= high(_VARDATACMP) then
    begin
      cmp2sort := _VARDATACMP[at, caseInsensitive];
      if cmp2sort <> 0 then
        result := _CMP2SORT[cmp2sort](A^.VAny, B^.VAny)
      else
        result := VariantCompSimple(PVariant(A)^, PVariant(B)^)
    end
    else if at = varStringByRef then
      // e.g. from TRttiVarData / TRttiCustomProp.CompareValue
      result := _CMP2SORT[_VARDATACMP[varString, caseInsensitive]](
        PPointer(A^.VAny)^, PPointer(B^.VAny)^)
    else if at = varSynUnicode or varByRef then
      result := _CMP2SORT[_VARDATACMP[varSynUnicode, caseInsensitive]](
         PPointer(A^.VAny)^, PPointer(B^.VAny)^)
    else if at < varFirstCustom then
      result := VariantCompSimple(PVariant(A)^, PVariant(B)^)
    else if at = DocVariantVType then
      // direct TDocVariantDat.VName/VValue comparison with no serialization
      result := PDocVariantData(A)^.Compare(PDocVariantData(B)^, caseInsensitive)
    else
    begin
      ah := FindSynVariantType(at);
      if ah = nil then
        // compare from custom types UTF-8 text representation/serialization
        result := VariantCompAsText(A, B, caseInsensitive)
      else
        // use proper virtual comparison method
        result := ah.IntCompare(A^, B^, caseInsensitive);
    end
  // A and B do not share the same type
  else if (at <= varNull) or
          (bt <= varNull) then
    result := ord(at > varNull) - ord(bt > varNull)
  else if (at < varString) and
          (at <> varOleStr) and
          (bt < varString) and
          (bt <> varOleStr) then
    result := VariantCompSimple(PVariant(A)^, PVariant(B)^)
  else if (at < varFirstCustom) and
          (bt < varFirstCustom) then
    result := VariantCompAsText(A, B, caseInsensitive) // RawUtf8 convert
  else
  begin
    ah := FindSynVariantType(at);
    bh := FindSynVariantType(bt);
    if ah <> nil then
      result := ah.IntCompare(A^, B^, caseInsensitive)
    else if bh <> nil then
      result := - bh.IntCompare(B^, A^, caseInsensitive)
    else
      result := VariantCompAsText(A, B, caseInsensitive); // RawUtf8 convert
  end;
end;

function VariantCompare(const V1, V2: variant): PtrInt;
begin
  result := FastVarDataComp(@V1, @V2, {caseins=}false);
end;

function VariantCompareI(const V1, V2: variant): PtrInt;
begin
  result := FastVarDataComp(@V1, @V2, {caseins=}true);
end;

function VariantEquals(const V: Variant; const Str: RawUtf8;
  CaseSensitive: boolean): boolean;

  function Complex: boolean;
  var
    wasString: boolean;
    tmp: RawUtf8;
  begin
    VariantToUtf8(V, tmp, wasString);
    if CaseSensitive then
      result := (tmp = Str)
    else
      result := PropNameEquals(tmp, Str);
  end;

var
  v1, v2: Int64;
  vt: cardinal;
begin
  vt := TVarData(V).VType;
  with TVarData(V) do
    case vt of
      varEmpty,
      varNull:
        result := Str = '';
      varBoolean:
        result := VBoolean = (Str <> '');
      varString:
        if CaseSensitive then
          result := RawUtf8(VString) = Str
        else
          result := PropNameEquals(RawUtf8(VString), Str);
    else
      if VariantToInt64(V, v1) then
      begin
        SetInt64(pointer(Str), v2);
        result := v1 = v2;
      end
      else
        result := Complex;
    end;
end;


{ ************** Custom Variant Types with JSON support }

var
  SynVariantTypesSafe: TLightLock; // protects only SynRegisterCustomVariantType

  /// list of custom types (but not DocVariantVType) supporting TryJsonToVariant
  SynVariantTryJsonTypes: array of TSynInvokeableVariantType;

function FindSynVariantType(aVarType: cardinal): TSynInvokeableVariantType;
var
  n: integer;
  t: ^TSynInvokeableVariantType;
begin
  if (aVarType >= varFirstCustom) and
     (aVarType < varArray) then
  begin
    t := pointer(SynVariantTypes);
    n := PDALen(PAnsiChar(t) - _DALEN)^ + _DAOFF;
    repeat
      result := t^;
      if result.VarType = aVarType then
        exit;
      inc(t);
      dec(n);
    until n = 0;
  end;
  result := nil;
end;

function SynRegisterCustomVariantType(
  aClass: TSynInvokeableVariantTypeClass): TSynInvokeableVariantType;
var
  i: PtrInt;
begin
  SynVariantTypesSafe.Lock;
  try
    for i := 0 to length(SynVariantTypes) - 1 do
    begin
      result := SynVariantTypes[i];
      if PPointer(result)^ = pointer(aClass) then
        // returns already registered instance
        exit;
    end;
    result := aClass.Create; // register variant type
    ObjArrayAdd(SynVariantTypes, result);
    if sioHasTryJsonToVariant in result.Options then
      ObjArrayAdd(SynVariantTryJsonTypes, result);
  finally
    SynVariantTypesSafe.UnLock;
  end;
end;

function SortCompTo(cmp: integer): TVarCompareResult;
begin
  if cmp = 0 then
    result := crEqual
  else if cmp > 0 then
    result:= crGreaterThan
  else
    result := crLessThan;
end;


{ TSynInvokeableVariantType }

constructor TSynInvokeableVariantType.Create;
begin
  inherited Create; // call RegisterCustomVariantType(self)
end;

function TSynInvokeableVariantType.IterateCount(const V: TVarData;
  GetObjectAsValues: boolean): integer;
begin
  result := -1; // this is not an array
end;

procedure TSynInvokeableVariantType.Iterate(var Dest: TVarData;
  const V: TVarData; Index: integer);
begin
  // do nothing
end;

{$ifdef ISDELPHI}
function TSynInvokeableVariantType.FixupIdent(const AText: string): string;
begin
  result := AText; // NO uppercased identifier for our custom types!
end;
{$endif ISDELPHI}

function TSynInvokeableVariantType.{%H-}IntGet(var Dest: TVarData;
  const Instance: TVarData; Name: PAnsiChar; NameLen: PtrInt;
  NoException: boolean): boolean;
begin
  raise ESynVariant.CreateUtf8('Unexpected %.IntGet(%): this kind of ' +
    'custom variant does not support fields', [self, Name]);
end;

function TSynInvokeableVariantType.{%H-}IntSet(const Instance, Value: TVarData;
  Name: PAnsiChar; NameLen: PtrInt): boolean;
begin
  raise ESynVariant.CreateUtf8('Unexpected %.IntSet(%): this kind of ' +
    'custom variant is read-only', [self, Name]);
end;

function TSynInvokeableVariantType.IntCompare(
  const Instance, Another: TVarData; CaseInsensitive: boolean): integer;
begin
  result := VariantCompAsText(@Instance, @Another, CaseInsensitive);
end;

const
  FROM_VAROP: array[opcmpeq .. opcmpge, TVarCompareResult] of boolean = (
    (false,  true,   false), // opcmpeq
    (true,   false,  true),  // opcmpne
    (true,   false,  false), // opcmplt
    (true,   true,   false), // opcmple
    (false,  false,  true),  // opcmpgt
    (false,  true,   true)); // opcmpge
    // crLessThan crEqual crGreaterThan

function TSynInvokeableVariantType.CompareOp(const Left, Right: TVarData;
  const Operation: TVarOp): boolean;
var
  vcr: TVarCompareResult;
begin
  // redirect to Compare() as Delphi RTL does (but not the FPC RTL)
  if not (Operation in [low(FROM_VAROP) .. high(FROM_VAROP)]) then
    raise ESynVariant.CreateUtf8('Unexpected %.CompareOp(%)', [self, ord(Operation)]);
  Compare(Left, Right, vcr);
  result := FROM_VAROP[Operation, vcr];
end;

procedure TSynInvokeableVariantType.Compare(const Left, Right: TVarData;
  var Relationship: TVarCompareResult);
begin
  Relationship := SortCompTo(IntCompare(Left, Right, {CaseInsen=}false));
end;

const
  DISPATCH_METHOD      = 1;
  DISPATCH_PROPERTYGET = 2; // in practice, never generated by the FPC compiler
  DISPATCH_PROPERTYPUT = 4;
  ARGTYPE_MASK         = $7f;
  ARGREF_MASK          = $80;
  VAR_PARAMNOTFOUND    = HRESULT($80020004);

{$ifdef FPC}
var
  DispInvokeArgOrderInverted: boolean; // circumvent FPC 3.2+ breaking change
{$endif FPC}

{$ifdef FPC_VARIANTSETVAR}
procedure TSynInvokeableVariantType.DispInvoke(
  Dest: PVarData; var Source: TVarData; CallDesc: PCallDesc; Params: Pointer);
{$else} // see http://mantis.freepascal.org/view.php?id=26773
  {$ifdef ISDELPHIXE7}
procedure TSynInvokeableVariantType.DispInvoke(
  Dest: PVarData; [ref] const Source: TVarData; // why not just "var" ????
  CallDesc: PCallDesc; Params: Pointer);
  {$else}
procedure TSynInvokeableVariantType.DispInvoke(
  Dest: PVarData; const Source: TVarData; CallDesc: PCallDesc; Params: Pointer);
  {$endif ISDELPHIXE7}
{$endif FPC_VARIANTSETVAR}
var
  name: string;
  res: TVarData;
  namelen, i, asize, n: PtrInt;
  nameptr, a: PAnsiChar;
  v: PVarData;
  args: TVarDataArray; // DoProcedure/DoFunction require a dynamic array
  t: cardinal;
  {$ifdef FPC}
  inverted: boolean;
  {$endif FPC}

  procedure RaiseInvalid;
  begin
    raise ESynVariant.CreateUtf8('%.DispInvoke: invalid %(%) call',
      [self, name, CallDesc^.ArgCount]);
  end;

begin
  // circumvent https://bugs.freepascal.org/view.php?id=38653 and
  // inverted args order FPC bugs, avoid unneeded conversion to varOleString
  // for Delphi, and implement direct IntGet/IntSet calls for all
  n := CallDesc^.ArgCount;
  nameptr := @CallDesc^.ArgTypes[n];
  namelen := StrLen(nameptr);
  // faster direct property getter
  if (Dest <> nil) and
     (n = 0) and
     (CallDesc^.CallType in [DISPATCH_METHOD, DISPATCH_PROPERTYGET]) and
     IntGet(Dest^, Source, nameptr, namelen, {noexception=}false) then
    exit;
  Ansi7ToString(pointer(nameptr), namelen, name);
  if n > 0 then
  begin
    // convert varargs Params buffer into an array of TVarData
    SetLength(args, n);
    {$ifdef FPC} // circumvent FPC 3.2+ inverted order
    inverted := (n > 1) and
                DispInvokeArgOrderInverted;
    if inverted then
      v := @args[n - 1]
    else
    {$endif FPC}
      v := pointer(args);
    a := Params;
    for i := 0 to n - 1 do
    begin
      asize := SizeOf(pointer);
      t := cardinal(CallDesc^.ArgTypes[i]) and ARGTYPE_MASK;
      case t of
        {$ifdef HASVARUSTRARG}
        varUStrArg:
          t := varUString;
        {$endif HASVARUSTRARG}
        varStrArg:
          t := varString;
      end;
      if CallDesc^.ArgTypes[i] and ARGREF_MASK <> 0 then
      begin
        TRttiVarData(v^).VType := t or varByRef;
        v^.VPointer := PPointer(a)^;
      end
      else
      begin
        TRttiVarData(v^).VType := t;
        case t of
          varError:
            begin
              v^.VError := VAR_PARAMNOTFOUND;
              asize := 0;
            end;
          varVariant:
            {$ifdef CPU32DELPHI}
            begin
              v^ := PVarData(a)^;
              asize := SizeOf(TVarData); // pushed by value
            end;
            {$else}
            v^ := PPVarData(a)^^; // pushed by reference (as other parameters)
            {$endif CPU32DELPHI}
          varDouble,
          varCurrency,
          varDate,
          varInt64,
          varWord64:
            begin
              v^.VInt64 := PInt64(a)^;
              asize := SizeOf(Int64);
            end;
          // small values are stored as pointers on stack but pushed as 32-bit
          varSingle,
          varSmallint,
          varInteger,
          varLongWord,
          varBoolean,
          varShortInt,
          varByte,
          varWord:
            v^.VInteger := PInteger(a)^; // we assume little endian
        else
          v^.VAny := PPointer(a)^; // e.g. varString or varOleStr
        end;
      end;
      inc(a, asize);
      {$ifdef FPC}
      if inverted then
        dec(v)
      else
      {$endif FPC}
        inc(v);
    end;
  end;
  case CallDesc^.CallType of
    // note: IntGet was already tried in function trailer
    DISPATCH_METHOD:
      if Dest <> nil then
      begin
        if not DoFunction(Dest^, Source, name, args) then
          RaiseInvalid;
      end
      else if not DoProcedure(Source, name, args) then
      begin
        PCardinal(@res)^ := varEmpty;
        try
          if not DoFunction(Dest^, Source, name, args) then
            RaiseInvalid;
        finally
          VarClearProc(res);
        end;
      end;
    DISPATCH_PROPERTYGET:
      if (Dest = nil) or
         not DoFunction(Dest^, Source, name, args) then
        RaiseInvalid;
    DISPATCH_PROPERTYPUT:
      if (Dest <> nil) or
         (n <> 1) or
         not IntSet(Source, args[0], nameptr, namelen) then
        RaiseInvalid;
  else
    RaiseInvalid;
  end;
end;

procedure TSynInvokeableVariantType.Clear(var V: TVarData);
begin
  ZeroFill(@V); // will set V.VType := varEmpty
end;

procedure TSynInvokeableVariantType.Copy(var Dest: TVarData;
  const Source: TVarData; const Indirect: boolean);
begin
  if Indirect then
    SetVariantByRef(variant(Source), variant(Dest))
  else
  begin
    VarClear(variant(Dest)); // Dest may be a complex type
    Dest := Source;
  end;
end;

procedure TSynInvokeableVariantType.CopyByValue(
  var Dest: TVarData; const Source: TVarData);
begin
  Copy(Dest, Source, {Indirect=} false);
end;

function TSynInvokeableVariantType.TryJsonToVariant(var Json: PUtf8Char;
  var Value: variant; EndOfObject: PUtf8Char): boolean;
begin
  result := false;
end;

procedure TSynInvokeableVariantType.ToJson(W: TJsonWriter; Value: PVarData);
begin
  raise ESynVariant.CreateUtf8('%.ToJson is not implemented', [self]);
end;

procedure TSynInvokeableVariantType.ToJson(Value: PVarData;
  var Json: RawUtf8; const Prefix, Suffix: RawUtf8; Format: TTextWriterJsonFormat);
var
  W: TJsonWriter;
  temp: TTextWriterStackBuffer;
begin
  W := TJsonWriter.CreateOwnedStream(temp);
  try
    if Prefix <> '' then
      W.AddString(Prefix);
    ToJson(W, Value); // direct TSynInvokeableVariantType serialization
    if Suffix <> '' then
      W.AddString(Suffix);
    W.SetText(Json, Format);
  finally
    W.Free;
  end;
end;

function TSynInvokeableVariantType.IsOfType(const V: variant): boolean;
var
  vt: cardinal;
  vd: PVarData;
{%H-}begin
  if self <> nil then
  begin
    vd := @V;
    repeat
      vt := vd^.VType;
      if vt <> varVariantByRef then
        break;
      vd := vd^.VPointer;
    until false;
    result := vt = VarType;
  end
  else
    result := false;
end;

function TSynInvokeableVariantType.IsVoid(const V: TVarData): boolean;
begin
  result := false; // not void by default
end;

function TSynInvokeableVariantType.FindSynVariantType(aVarType: cardinal;
  out CustomType: TSynInvokeableVariantType): boolean;
var
  ct: TSynInvokeableVariantType;
begin
  if (self <> nil) and
     (aVarType = VarType) then
    ct := self
  else
    ct := mormot.core.variants.FindSynVariantType(aVarType);
  CustomType := ct;
  result := ct <> nil;
end;

function TSynInvokeableVariantType.FindSynVariantType(
  aVarType: cardinal): TSynInvokeableVariantType;
begin
  if aVarType = VarType then
    result := self
  else
    result := mormot.core.variants.FindSynVariantType(aVarType);
end;

procedure TSynInvokeableVariantType.Lookup(var Dest: TVarData;
  const Instance: TVarData; FullName: PUtf8Char; PathDelim: AnsiChar);
var
  handler: TSynInvokeableVariantType;
  v, tmp: TVarData; // PVarData wouldn't store e.g. RowID/count
  vt: cardinal;
  n: ShortString;
begin
  TRttiVarData(Dest).VType := varEmpty; // left to Unassigned if not found
  v := Instance;
  repeat
    vt := v.VType;
    if vt <> varVariantByRef then
      break;
    v := PVarData(v.VPointer)^;
  until false;
  repeat
    if vt < varFirstCustom then
      exit; // we need a complex type to lookup
    GetNextItemShortString(FullName, @n, PathDelim); // n will end with #0
    if n[0] = #0 then
      exit;
    handler := self;
    if vt <> VarType then
    begin
      handler := mormot.core.variants.FindSynVariantType(vt);
      if handler = nil then
        exit;
    end;
    tmp := v; // v will be modified in-place
    TRttiVarData(v).VType := varEmpty; // IntGet() would clear it otherwise!
    if not handler.IntGet(v, tmp, @n[1], ord(n[0]), {noexc=}true) then
      exit; // property not found (no exception should be raised in Lookup)
    repeat
      vt := v.VType;
      if vt <> varVariantByRef then
        break;
      v := PVarData(v.VPointer)^;
    until false;
    if (vt = DocVariantVType) and
       (TDocVariantData(v).VCount = 0) then
      // recognize void TDocVariant as null
      v.VType := varNull; // do not use PCardinal/TRttiVarData(v).VType here
  until FullName = nil;
  Dest := v;
end;

function CustomVariantToJson(W: TJsonWriter; Value: PVarData;
  Escape: TTextWriterKind): boolean;
var
  v: TCustomVariantType;
  tmp: variant;
begin
  result := true;
  if FindCustomVariantType(Value.VType, v) then
    if v.InheritsFrom(TSynInvokeableVariantType) then
      TSynInvokeableVariantType(v).ToJson(W, Value)
    else
      try
        v.CastTo(TVarData(tmp), Value^, varNativeString);
        W.AddVariant(tmp, Escape);
      except
        result := false;
      end
  else
    result := false;
end;


function ToText(kind: TDocVariantKind): PShortString;
begin
  result := GetEnumName(TypeInfo(TDocVariantKind), ord(kind));
end;

procedure __VariantSaveJsonEscape(const Value: variant; var Json: RawUtf8;
  Escape: TTextWriterKind);
var
  temp: TTextWriterStackBuffer;
begin
  with TJsonWriter.CreateOwnedStream(temp) do
    try
      AddVariant(Value, Escape); // will use mormot.core.json serialization
      SetText(Json, jsonCompact);
    finally
      Free;
    end;
end;

procedure __VariantSaveJson(V: PVarData; Escape: TTextWriterKind;
  var result: RawUtf8);
var
  cv: TSynInvokeableVariantType;
  vt: cardinal;
  dummy: boolean;
begin
  // is likely to be called from AddVariant() but can be used for simple values
  if cardinal(V.VType) = varVariantByRef then
    V := V^.VPointer;
  cv := FindSynVariantType(V.VType);
  if cv = nil then
  begin
    vt := V.VType;
    if (vt >= varFirstCustom) or
       ((Escape <> twNone) and
        not (vt in [varEmpty..varDate, varBoolean, varShortInt..varWord64])) then
      __VariantSaveJsonEscape(PVariant(V)^, result, Escape)
    else
      VariantToUtf8(PVariant(V)^, result, dummy); // no escape for simple values
  end
  else
    cv.ToJson(V, result);
end;


{ EDocVariant }

class procedure EDocVariant.RaiseSafe(Kind: TDocVariantKind);
begin
  raise CreateUtf8('_Safe(%)?', [ToText(Kind)^]);
end;

{ TDocVariant }

destructor TDocVariant.Destroy;
begin
  inherited Destroy;
  fInternNames.Free;
  fInternValues.Free;
end;

const
  _GETMETHOD: array[0..3] of PAnsiChar = (
    'COUNT', // 0
    'KIND',  // 1
    'JSON',  // 2
    nil);

function IntGetPseudoProp(ndx: PtrInt; const source: TDocVariantData;
  var Dest: variant): boolean;
begin
  // sub-function to avoid temporary RawUtf8 for source.ToJson
  result := true;
  case ndx of
    0:
      Dest := source.Count;
    1:
      Dest := ord(source.GetKind);
    2:
      RawUtf8ToVariant(source.ToJson, Dest);
  else
    result := false;
  end;
end;

function TDocVariant.IntGet(var Dest: TVarData; const Instance: TVarData;
  Name: PAnsiChar; NameLen: PtrInt; NoException: boolean): boolean;
var
  dv: TDocVariantData absolute Instance;
  ndx: integer;
begin
  if Name = nil then
    result := false
  else if (NameLen > 4) and
          (Name[0] = '_') and
          IntGetPseudoProp(IdemPPChar(@Name[1], @_GETMETHOD), dv, variant(Dest)) then
    result := true
  else
  begin
    ndx := dv.GetValueIndex(pointer(Name), NameLen, dv.IsCaseSensitive);
    if ndx < 0 then
      if NoException or
         dv.Has(dvoReturnNullForUnknownProperty) then
      begin
        SetVariantNull(PVariant(@Dest)^);
        result := false;
      end
      else
        raise EDocVariant.CreateUtf8('[%] property not found', [Name])
    else
    begin
      SetVariantByRef(dv.VValue[ndx], PVariant(@Dest)^);
      result := true;
    end;
  end;
end;

function TDocVariant.IntSet(const Instance, Value: TVarData;
  Name: PAnsiChar; NameLen: PtrInt): boolean;
var
  ndx: PtrInt;
  dv: TDocVariantData absolute Instance;
begin
  result := true;
  if dv.IsArray and
     (PWord(Name)^ = ord('_')) then
  begin
    dv.AddItem(variant(Value));
    exit;
  end;
  ndx := dv.GetValueIndex(pointer(Name), NameLen, dv.IsCaseSensitive);
  if ndx < 0 then
    ndx := dv.InternalAddBuf(pointer(Name), NameLen);
  dv.InternalSetValue(ndx, variant(Value));
end;

function TDocVariant.IntCompare(const Instance, Another: TVarData;
  CaseInsensitive: boolean): integer;
var
  l, r: PDocVariantData;
begin
  if _Safe(variant(Instance), l) and // is likely to be a TDocVariant
     _Safe(variant(Another), r) then
    result := l^.Compare(r^, CaseInsensitive)
  else // inlined inherited
    result := VariantCompAsText(@Instance, @Another, CaseInsensitive);
end;

function TDocVariant.IterateCount(const V: TVarData;
  GetObjectAsValues: boolean): integer;
var
  Data: TDocVariantData absolute V;
begin
  if Data.IsArray or
     (GetObjectAsValues and
      Data.IsObject) then
    result := Data.VCount
  else
    result := -1;
end;

procedure TDocVariant.Iterate(var Dest: TVarData;
  const V: TVarData; Index: integer);
var
  Data: TDocVariantData absolute V;
begin // note: IterateCount() may accept IsObject values[]
  if cardinal(Index) < cardinal(Data.VCount) then
    Dest := TVarData(Data.VValue[Index])
  else
    TRttiVarData(Dest).VType := varEmpty;
end;

function TDocVariant.IsVoid(const V: TVarData): boolean;
begin
  result := TDocVariantData(V).Count > 0;
end;

function TDocVariant.DoProcedure(const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): boolean;
var
  Data: PDocVariantData;
begin
  result := false;
  Data := @V; // allow to modify a const argument
  case length(Arguments) of
    0:
      if SameText(Name, 'Clear') then
      begin
        Data^.Reset;
        result := true;
      end;
    1:
      if SameText(Name, 'Add') then
      begin
        Data^.AddItem(variant(Arguments[0]));
        result := true;
      end
      else if SameText(Name, 'Delete') then
      begin
        Data^.Delete(Data^.GetValueIndex(ToUtf8(Arguments[0])));
        result := true;
      end;
    2:
      if SameText(Name, 'Add') then
      begin
        Data^.AddValue(ToUtf8(Arguments[0]), variant(Arguments[1]));
        result := true;
      end;
  end;
end;

function TDocVariant.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): boolean;
var
  ndx: integer;
  Data: PDocVariantData;
  temp: RawUtf8;
begin
  result := true;
  Data := @V; // allow to modify a const argument
  case length(Arguments) of
    1:
      if SameText(Name, 'Exists') then
      begin
        variant(Dest) := Data.GetValueIndex(ToUtf8(Arguments[0])) >= 0;
        exit;
      end
      else if SameText(Name, 'NameIndex') then
      begin
        variant(Dest) := Data.GetValueIndex(ToUtf8(Arguments[0]));
        exit;
      end
      else if VariantToInteger(variant(Arguments[0]), ndx) then
      begin
        if (Name = '_') or
           SameText(Name, 'Value') then
        begin
          Data.RetrieveValueOrRaiseException(ndx, variant(Dest), true);
          exit;
        end
        else if SameText(Name, 'Name') then
        begin
          Data.RetrieveNameOrRaiseException(ndx, temp);
          RawUtf8ToVariant(temp, variant(Dest));
          exit;
        end;
      end
      else if (Name = '_') or
              SameText(Name, 'Value') then
      begin
        temp := ToUtf8(Arguments[0]);
        Data.RetrieveValueOrRaiseException(pointer(temp), length(temp),
          Data.IsCaseSensitive, variant(Dest), true);
        exit;
      end;
  end;
  result := Data.Has(dvoReturnNullForUnknownProperty); // to avoid error
end;

procedure TDocVariant.ToJson(W: TJsonWriter; Value: PVarData);
var
  forced: TTextWriterOptions;
  nam: PPUtf8Char;
  val: PVariant;
  vt: cardinal;
  n: integer;
  checkExtendedPropName: boolean;
begin
  repeat
    vt := Value^.VType;
    if vt <> varVariantByRef then
      break;
    Value := Value^.VPointer;
  until false;
  if vt <> DocVariantVType then
  begin
    W.AddNull;
    exit;
  end;
  forced := [];
  if [twoForceJsonExtended, twoForceJsonStandard] * W.CustomOptions = [] then
  begin
    if PDocVariantData(Value)^.Has(dvoSerializeAsExtendedJson) then
      forced := [twoForceJsonExtended]
    else
      forced := [twoForceJsonStandard];
    W.CustomOptions := W.CustomOptions + forced;
  end;
  n := PDocVariantData(Value)^.VCount;
  val := pointer(PDocVariantData(Value)^.VValue);
  if PDocVariantData(Value)^.IsObject then
  begin
    checkExtendedPropName := twoForceJsonExtended in W.CustomOptions;
    W.Add('{');
    nam := pointer(PDocVariantData(Value)^.VName);
    if n <> 0 then
      repeat
        if checkExtendedPropName and
           JsonPropNameValid(nam^) then
          W.AddShort(nam^, PStrLen(nam^ - _STRLEN)^)
        else
        begin
          W.AddDirect('"');
          W.AddJsonEscape(nam^);
          W.AddDirect('"');
        end;
        W.AddDirect(':');
        W.AddVariant(val^, twJsonEscape);
        dec(n);
        if n = 0 then
          break;
        W.AddComma;
        inc(nam);
        inc(val);
      until false;
    W.AddDirect('}');
  end
  else if PDocVariantData(Value)^.IsArray then
  begin
    W.Add('[');
    if n <> 0 then
      repeat
        W.AddVariant(val^, twJsonEscape);
        dec(n);
        if n = 0 then
          break;
        W.AddComma;
        inc(val);
      until false;
    W.AddDirect(']');
  end
  else
    W.AddNull;
  if forced <> [] then
    W.CustomOptions := W.CustomOptions - forced;
end;

procedure TDocVariant.Clear(var V: TVarData);
begin
  //Assert(V.VType=DocVariantVType);
  TDocVariantData(V).ClearFast;
end;

procedure TDocVariant.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: boolean);
begin
  //Assert(Source.VType=DocVariantVType);
  if Indirect then
    SetVariantByRef(variant(Source), variant(Dest))
  else
    CopyByValue(Dest, Source);
end;

procedure TDocVariant.CopyByValue(var Dest: TVarData; const Source: TVarData);
var
  S: TDocVariantData absolute Source;
  D: TDocVariantData absolute Dest;
begin
  //Assert(Source.VType=DocVariantVType);
  VarClearAndSetType(variant(Dest), PCardinal(@S)^); // VType + VOptions
  pointer(D.VName) := nil; // avoid GPF
  pointer(D.VValue) := nil;
  D.VCount := S.VCount;
  if S.VCount = 0 then
    exit; // no data to copy
  D.VName := S.VName;
  if S.Has(dvoValueCopiedByReference) then
    D.VValue := S.VValue // byref copy of the whole array
  else
    D.VValue := system.copy(S.VValue); // new array, but byref values
end;

procedure TDocVariant.Cast(var Dest: TVarData; const Source: TVarData);
begin
  CastTo(Dest, Source, VarType);
end;

procedure TDocVariant.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
var
  json: RawUtf8;
  wasString: boolean;
begin
  if AVarType = VarType then
  begin
    VariantToUtf8(Variant(Source), json, wasString);
    if wasString then
    begin
      VarClear(variant(Dest));
      variant(Dest) := _JsonFast(json); // convert from JSON text
      exit;
    end;
    RaiseCastError;
  end
  else
  begin
    if Source.VType <> VarType then
      RaiseCastError;
    DocVariantType.ToJson(@Source, json);
    RawUtf8ToVariant(json, Dest, AVarType); // convert to JSON text
  end;
end;

class procedure TDocVariant.New(out aValue: variant;
  aOptions: TDocVariantOptions);
begin
  TDocVariantData(aValue).Init(aOptions);
end;

class procedure TDocVariant.NewFast(out aValue: variant;
  aKind: TDocVariantKind);
begin
  TVarData(aValue) := DV_FAST[aKind];
end;

class procedure TDocVariant.IsOfTypeOrNewFast(var aValue: variant);
begin
  if DocVariantType.IsOfType(aValue) then
    exit;
  VarClear(aValue);
  TVarData(aValue) := DV_FAST[dvUndefined];
end;

class procedure TDocVariant.NewFast(const aValues: array of PDocVariantData;
  aKind: TDocVariantKind);
var
  i: PtrInt;
  def: PDocVariantData;
begin
  def := @DV_FAST[aKind];
  for i := 0 to high(aValues) do
    aValues[i]^ := def^;
end;

class function TDocVariant.New(Options: TDocVariantOptions): Variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).Init(Options);
end;

class function TDocVariant.NewObject(const NameValuePairs: array of const;
  Options: TDocVariantOptions): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitObject(NameValuePairs, Options);
end;

class function TDocVariant.NewArray(const Items: array of const;
  Options: TDocVariantOptions): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitArray(Items, Options);
end;

class function TDocVariant.NewArray(const Items: TVariantDynArray;
  Options: TDocVariantOptions): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitArrayFromVariants(Items, Options);
end;

class function TDocVariant.NewJson(const Json: RawUtf8;
  Options: TDocVariantOptions): variant;
begin
  _Json(Json, result, Options);
end;

class function TDocVariant.NewUnique(const SourceDocVariant: variant;
  Options: TDocVariantOptions): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitCopy(SourceDocVariant, Options);
end;

class procedure TDocVariant.GetSingleOrDefault(
  const docVariantArray, default: variant; var result: variant);
var
  vt: cardinal;
begin
  vt := TVarData(docVariantArray).VType;
  if vt = varVariantByRef then
    GetSingleOrDefault(
      PVariant(TVarData(docVariantArray).VPointer)^, default, result)
  else if (vt <> DocVariantVType) or
          (TDocVariantData(docVariantArray).Count <> 1) or
          not TDocVariantData(docVariantArray).IsArray then
    result := default
  else
    result := TDocVariantData(docVariantArray).Values[0];
end;

function DocVariantData(const DocVariant: variant): PDocVariantData;
var
  docv, vt: cardinal;
begin
  result := @DocVariant;
  docv := DocVariantVType;
  vt := result^.VType;
  if vt = docv then
    exit
  else if vt = varVariantByRef then
  begin
    result := PVarData(result)^.VPointer;
    if cardinal(result^.VType) = docv then
      exit;
  end;
  raise EDocVariant.CreateUtf8('Unexpected DocVariantData(var%)',
    [VariantTypeName(PVarData(result))^]);
end;

{$ifdef FPC_OR_UNICODE} // Delphi has problems inlining this :(
function _Safe(const DocVariant: variant): PDocVariantData;
var
  docv, vt: cardinal;
begin
  result := @DocVariant;
  docv := DocVariantVType;
  vt := result^.VType;
  if vt = docv then
    exit
  else if vt = varVariantByRef then
  begin
    result := PVarData(result)^.VPointer;
    if cardinal(result^.VType) = docv then
      exit;
  end;
  result := @DocVariantDataFake;
end;
{$else} // fallback for Delphi 7/2007
function _Safe(const DocVariant: variant): PDocVariantData;
asm
        mov     ecx, DocVariantVType
        movzx   edx, word ptr [eax].TVarData.VType
        cmp     edx, ecx
        jne     @by
        ret
@ptr:   mov     eax, [eax].TVarData.VPointer
        movzx   edx, word ptr [eax].TVarData.VType
        cmp     edx, ecx
        je      @ok
@by:    cmp     edx, varVariantByRef
        je      @ptr
        lea     eax, [DocVariantDataFake]
@ok:
end;
{$endif FPC_OR_UNICODE}

function _Safe(const DocVariant: variant; out DV: PDocVariantData): boolean;
var
  docv, vt: cardinal;
  v: PDocVariantData;
{$ifdef FPC} // latest Delphi compilers have problems inlining labels
label
  no;
{$endif FPC}
begin
  docv := DocVariantVType;
  v := @DocVariant;
  vt := v^.VType;
  {$ifdef ISDELPHI}
  result := false;
  {$endif ISDELPHI}
  if vt <> docv then
    if vt <> varVariantByRef then
    begin
{$ifdef FPC}
no:  result := false;
{$endif FPC}
     exit;
    end
    else
    begin
      v := PVarData(v)^.VPointer;
      if cardinal(v^.VType) <> docv then
      {$ifdef FPC}
        goto no;
      {$else}
        exit;
      {$endif FPC}
    end;
  DV := v;
  result := true;
end;

function _SafeArray(const Value: variant; out DV: PDocVariantData): boolean;
begin
  result := _Safe(Value, DV) and
            not {%H-}DV^.IsObject;
end;

function _SafeArray(const Value: variant; ExpectedCount: integer;
  out DV: PDocVariantData): boolean;
begin
  result := _Safe(Value, DV) and
            {%H-}DV^.IsArray and
            (DV^.Count = ExpectedCount);
end;

function _SafeObject(const Value: variant; out DV: PDocVariantData): boolean;
begin
  result := _Safe(Value, DV) and
            not {%H-}DV^.IsArray;
end;

function _Safe(const DocVariant: variant;
  ExpectedKind: TDocVariantKind): PDocVariantData;
begin
  if ExpectedKind = dvArray then
  begin
    if _SafeArray(DocVariant, result) then
      exit;
  end
  else if (ExpectedKind = dvObject) and
          _SafeObject(DocVariant, result) then
    exit;
  EDocVariant.RaiseSafe(ExpectedKind);
end;

function _DV(const DocVariant: variant): TDocVariantData;
begin
  result := _Safe(DocVariant)^;
end;

function _DV(const DocVariant: variant;
  ExpectedKind: TDocVariantKind): TDocVariantData;
begin
  result := _Safe(DocVariant, ExpectedKind)^;
end;

function _DV(const DocVariant: variant; var DV: TDocVariantData): boolean;
var
  docv, vt: cardinal;
  v: PDocVariantData;
label
  no;
begin
  docv := DocVariantVType;
  v := @DocVariant;
  vt := v^.VType;
  if vt <> docv then
    if vt <> varVariantByRef then
    begin
no:   result := false;
      exit;
    end
    else
    begin
      v := PVarData(v)^.VPointer;
      if cardinal(v^.VType) <> docv then
        goto no;
    end;
  DV := v^;
  result := true;
end;

function _Csv(const DocVariantOrString: variant): RawUtf8;
begin
  with _Safe(DocVariantOrString)^ do
    if IsArray then
      result := ToCsv
    else if IsObject or
            not VariantToText(DocVariantOrString, result) then
      result := '';
end;

function ObjectToVariant(Value: TObject; EnumSetsAsText: boolean): variant;
const
  OPTIONS: array[boolean] of TTextWriterWriteObjectOptions = (
     [woDontStoreDefault], [woDontStoreDefault, woEnumSetsAsText]);
begin
  ObjectToVariant(Value, result, OPTIONS[EnumSetsAsText]);
end;

function ObjectToVariantDebug(Value: TObject;
  const ContextFormat: RawUtf8; const ContextArgs: array of const;
  const ContextName: RawUtf8): variant;
begin
  ObjectToVariant(Value, result, [woDontStoreDefault, woEnumSetsAsText]);
  if ContextFormat <> '' then
    if ContextFormat[1] = '{' then
      _ObjAddProps([ContextName,
        _JsonFastFmt(ContextFormat, [], ContextArgs)], result)
    else
      _ObjAddProps([ContextName,
        FormatUtf8(ContextFormat, ContextArgs)], result);
end;

procedure ObjectToVariant(Value: TObject; var result: variant;
  Options: TTextWriterWriteObjectOptions);
var
  json: RawUtf8;
begin
  VarClear(result{%H-});
  json := ObjectToJson(Value, Options);
  if PDocVariantData(@result)^.InitJsonInPlace(
      pointer(json), JSON_FAST) = nil then
    VarClear(result);
end;

function SetNameToVariant(Value: cardinal; Info: TRttiCustom;
  FullSetsAsStar: boolean): variant;
var
  bit: PtrInt;
  PS: PShortString;
  arr: TDocVariantData;
begin
  TVarData(arr) := DV_FAST[dvArray];
  if FullSetsAsStar and
     GetAllBits(Value, Info.Cache.EnumMax + 1) then
    arr.AddItem('*')
  else
    with Info.Cache do
    begin
      PS := EnumList;
      for bit := EnumMin to EnumMax do
      begin
        if GetBitPtr(@Value, bit) then
          arr.AddItem(PS^);
        inc(PByte(PS), ord(PS^[0]) + 1); // next item
      end;
    end;
  result := variant(arr);
end;

function SetNameToVariant(Value: cardinal; Info: PRttiInfo;
  FullSetsAsStar: boolean): variant;
begin
  result := SetNameToVariant(Value, Rtti.RegisterType(Info), FullSetsAsStar);
end;

function DocVariantToObject(var doc: TDocVariantData; obj: TObject;
  objRtti: TRttiCustom): boolean;
var
  p: PtrInt;
  prop: PRttiCustomProp;
begin
  if doc.IsObject and
     (doc.Count > 0) and
     (obj <> nil) then
  begin
    if objRtti = nil then
      objRtti := Rtti.RegisterClass(PClass(obj)^);
    for p := 0 to doc.Count - 1 do
    begin
      prop := objRtti.Props.Find(doc.Names[p]);
      if prop <> nil then
        prop^.Prop.SetValue(obj, doc.Values[p]);
    end;
    result := true;
  end
  else
    result := false;
end;

procedure DocVariantToObjArray(var arr: TDocVariantData; var objArray;
  objClass: TClass);
var
  info: TRttiCustom;
  i: PtrInt;
  obj: TObjectDynArray absolute objArray;
begin
  if objClass = nil then
    exit;
  ObjArrayClear(obj);
  if (not arr.IsArray) or
     (arr.Count = 0) then
    exit;
  info := Rtti.RegisterClass(objClass);
  SetLength(obj, arr.Count);
  for i := 0 to arr.Count - 1 do
  begin
    obj[i] := info.ClassNewInstance;
    DocVariantToObject(_Safe(arr.Values[i])^, obj[i], info);
  end;
end;

function ObjectDefaultToVariant(aClass: TClass;
  aOptions: TDocVariantOptions): variant;
var
  tempvoid: TObject;
  json: RawUtf8;
begin
  VarClear(result);
  tempvoid := Rtti.RegisterClass(aClass).ClassNewInstance;
  try
    json := ObjectToJson(tempvoid, [woDontStoreDefault]);
    PDocVariantData(@result)^.InitJsonInPlace(pointer(json), aOptions);
  finally
    tempvoid.Free;
  end;
end;


{$ifdef HASITERATORS}

{ TDocVariantEnumeratorState }

procedure TDocVariantEnumeratorState.Void;
begin
  After := nil;
  Curr := nil;
end;

procedure TDocVariantEnumeratorState.Init(Values: PVariantArray; Count: PtrUInt);
begin
  if Count = 0 then
    Void
  else
  begin
    Curr := pointer(Values);
    After := @Values[Count];
    dec(Curr);
  end;
end;

function TDocVariantEnumeratorState.MoveNext: boolean;
begin
   inc(Curr);
   result := PtrUInt(Curr) < PtrUInt(After); // Void = nil+1<nil = false
end;

{ TDocVariantFieldsEnumerator }

function TDocVariantFieldsEnumerator.GetCurrent: TDocVariantFields;
begin
  result.Name := Name;
  result.Value := State.Curr;
end;

function TDocVariantFieldsEnumerator.MoveNext: boolean;
begin
  result := State.MoveNext;
  if result and
     Assigned(Name) then
    inc(Name);
end;

function TDocVariantFieldsEnumerator.GetEnumerator: TDocVariantFieldsEnumerator;
begin
  result := self;
end;

{ TDocVariantFieldNamesEnumerator }

function TDocVariantFieldNamesEnumerator.MoveNext: boolean;
begin
  inc(Curr);
  result := PtrUInt(Curr) < PtrUInt(After);
end;

function TDocVariantFieldNamesEnumerator.GetEnumerator: TDocVariantFieldNamesEnumerator;
begin
  result := self;
end;

{ TDocVariantItemsEnumerator }

function TDocVariantItemsEnumerator.MoveNext: boolean;
begin
   result := State.MoveNext;
end;

function TDocVariantItemsEnumerator.GetEnumerator: TDocVariantItemsEnumerator;
begin
  result := self;
end;

{ TDocVariantObjectsEnumerator }

function TDocVariantObjectsEnumerator.MoveNext: boolean;
var
  vt: cardinal;
  vd: PVarData; // inlined while not DocVariant.IsOfType() + Value := _Safe()
begin
  repeat
    inc(State.Curr);
    vd := pointer(State.Curr);
    if PtrUInt(vd) >= PtrUInt(State.After) then
      break;
    repeat
      vt := vd^.VType;
      if vt = DocVariantVType then
      begin
        Value := pointer(vd);
        result := true;
        exit;
      end;
      if vt <> varVariantByRef then
        break;
      vd := vd^.VPointer;
    until false;
  until false;
  result := false;
end;

function TDocVariantObjectsEnumerator.GetEnumerator: TDocVariantObjectsEnumerator;
begin
  result := self;
end;

{$endif HASITERATORS}


{ TDocVariantData }

function TDocVariantData.GetValueIndex(const aName: RawUtf8): integer;
begin
  result := GetValueIndex(Pointer(aName), Length(aName), IsCaseSensitive);
end;

function TDocVariantData.GetCapacity: integer;
begin
  result := length(VValue);
end;

function TDocVariant.InternNames: TRawUtf8Interning;
begin
  result := fInternNames;
  if result = nil then
    result := CreateInternNames;
end;

function TDocVariant.CreateInternNames: TRawUtf8Interning;
begin
  fInternSafe.Lock;
  try
    if fInternNames = nil then
      fInternNames := TRawUtf8Interning.Create;
  finally
    fInternSafe.UnLock;
  end;
  result := fInternNames;
end;

function TDocVariant.InternValues: TRawUtf8Interning;
begin
  result := fInternValues;
  if fInternValues = nil then
    result := CreateInternValues;
end;

function TDocVariant.CreateInternValues: TRawUtf8Interning;
begin
  fInternSafe.Lock;
  try
    if fInternValues = nil then
      fInternValues := TRawUtf8Interning.Create;
  finally
    fInternSafe.UnLock;
  end;
  result := fInternValues;
end;

procedure TDocVariantData.InternalUniqueValueAt(aIndex: PtrInt);
begin
  DocVariantType.InternValues.UniqueVariant(VValue[aIndex]);
end;

procedure InternalUniqueValue(aValue: PVariant); // local to this unit
begin
  DocVariantType.InternValues.UniqueVariant(aValue^);
end;

procedure TDocVariantData.SetOptions(const opt: TDocVariantOptions);
begin
  VOptions := TDocVariantOptions(word(cardinal(word(opt) and not _DVO) +
                                      cardinal(word(VOptions) and _DVO)));
end;

procedure TDocVariantData.InitClone(const CloneFrom: TDocVariantData);
begin
  TRttiVarData(self).VType := TRttiVarData(CloneFrom).VType and not (_DVO shl 16);
  VCount := 0;
  pointer(VName)  := nil; // to avoid GPF
  pointer(VValue) := nil;
end;

function TDocVariantData.InitFrom(const CloneFrom: TDocVariantData;
  CloneValues, MakeUnique: boolean): PVariant;
begin
  TRttiVarData(self).VType := TRttiVarData(CloneFrom).VType; // VType+VOptions
  VCount := CloneFrom.VCount;
  if MakeUnique then             // new array, but byref names
    DynArrayCopy(@VName, @CloneFrom.VName, TypeInfo(TRawUtf8DynArray))
  else
    VName := CloneFrom.VName;    // byref copy of the whole array
  if CloneValues then
    if MakeUnique then           // new array, but byref values
      DynArrayCopy(@VValue, @CloneFrom.VValue, TypeInfo(TVariantDynArray))
    else
      VValue := CloneFrom.VValue // byref copy of the whole array
  else
    SetLength(VValue, VCount);   // setup void values
  result := pointer(VValue);
end;

procedure TDocVariantData.Init(const aOptions: TDocVariantOptions);
begin
  TRttiVarData(self).VType := DocVariantVType + // VType+VOptions
    cardinal(word(aOptions) and not _DVO) shl 16;
  VCount := 0;
  pointer(VName)  := nil; // to avoid GPF when mapped within a TVarData/variant
  pointer(VValue) := nil;
end;

procedure TDocVariantData.Init(const aOptions: TDocVariantOptions;
  aKind: TDocVariantKind);
begin // dvUndefined=0 dvArray=1 dvObject=2 -> [dvoIsArray]=1 [dvoIsObject]=2
  TRttiVarData(self).VType := DocVariantVType + // VType+VOptions
    cardinal((word(aOptions) and not _DVO) + ord(aKind)) shl 16;
  VCount := 0;
  pointer(VName)  := nil; // to avoid GPF
  pointer(VValue) := nil;
end;

procedure TDocVariantData.Init(aModel: TDocVariantModel; aKind: TDocVariantKind);
begin
  Init(JSON_[aModel], aKind);
end;

procedure TDocVariantData.InitFast(aKind: TDocVariantKind);
begin
  TVarData(self) := DV_FAST[aKind];
end;

procedure TDocVariantData.InitFast(InitialCapacity: integer;
  aKind: TDocVariantKind);
begin
  TVarData(self) := DV_FAST[aKind];
  if aKind = dvObject then
    SetLength(VName, InitialCapacity);
  SetLength(VValue, InitialCapacity);
end;

procedure TDocVariantData.InitObject(const NameValuePairs: array of const;
  aOptions: TDocVariantOptions);
begin
  Init(aOptions, dvObject);
  AddNameValuesToObject(NameValuePairs);
end;

procedure TDocVariantData.InitObject(const NameValuePairs: array of const;
  Model: TDocVariantModel);
begin
  Init(Model, dvObject);
  AddNameValuesToObject(NameValuePairs);
end;

procedure TDocVariantData.InternalSetVarRec(aIndex: PtrInt; const aValue: TVarRec);
var
  v: PVariant;
begin
  v := @VValue[aIndex];
  if Has(dvoValueCopiedByReference) or
     (aValue.VType <> vtVariant) then
    VarRecToVariant(aValue, v^)
  else
    SetVariantByValue(aValue.VVariant^, v^);
  if Has(dvoInternValues) then
    InternalUniqueValueAt(aIndex);
end;

procedure TDocVariantData.Include(dvo: TDocVariantOption);
begin
  TRttiVarData(self).VType := TRttiVarData(self).VType or
                              cardinal(1 shl (ord(dvo) + 16));
end;

procedure TDocVariantData.AddNameValuesToObject(
  const NameValuePairs: array of const);
var
  n, arg, ndx: PtrInt;
begin
  n := length(NameValuePairs);
  if (n = 0) or
     (n and 1 = 1) or
     IsArray then
    exit; // nothing to add
  Include(dvoIsObject);
  n := n shr 1;
  ndx := n + VCount;
  if length(VValue) < ndx then
  begin
    SetLength(VValue, ndx);
    SetLength(VName, ndx);
  end;
  ndx := VCount;
  for arg := 0 to n - 1 do
  begin
    VarRecToUtf8(NameValuePairs[arg * 2], VName[ndx]);
    if Has(dvoInternNames) then
      DocVariantType.InternNames.UniqueText(VName[ndx]);
    InternalSetVarRec(ndx, NameValuePairs[arg * 2 + 1]);
    inc(ndx);
  end;
  inc(VCount, n);
end;

{$ifndef PUREMORMOT2}
procedure TDocVariantData.AddOrUpdateNameValuesToObject(
  const NameValuePairs: array of const);
begin
  Update(NameValuePairs);
end;
{$endif PUREMORMOT2}

procedure TDocVariantData.Update(const NameValuePairs: array of const);
var
  n, arg: PtrInt;
  nam: RawUtf8;
  val: Variant;
begin
  n := length(NameValuePairs);
  if (n = 0) or
     (n and 1 = 1) or
     IsArray then
    exit; // nothing to add
  for arg := 0 to (n shr 1) - 1 do
  begin
    VarRecToUtf8(NameValuePairs[arg * 2], nam);
    VarRecToVariant(NameValuePairs[arg * 2 + 1], val);
    AddOrUpdateValue(nam, val)
  end;
end;

procedure TDocVariantData.AddOrUpdateObject(const NewValues: variant;
  OnlyAddMissing: boolean; RecursiveUpdate: boolean);
var
  n, idx: PtrInt;
  new: PDocVariantData;
  wasAdded: boolean;
begin
  new := _Safe(NewValues);
  if not IsArray and
     not new^.IsArray then
    for n := 0 to new^.Count - 1 do
    begin
      idx := AddOrUpdateValue(
        new^.names[n], new^.Values[n], @wasAdded, OnlyAddMissing);
      if RecursiveUpdate and
         not wasAdded then
        TDocVariantData(Values[idx]).AddOrUpdateObject(
          new^.Values[n], OnlyAddMissing, true);
    end;
end;

procedure TDocVariantData.InitArray(const aItems: array of const;
  aOptions: TDocVariantOptions);
var
  arg: PtrInt;
begin
  Init(aOptions, dvArray);
  if high(aItems) < 0 then
    exit;
  VCount := length(aItems);
  SetLength(VValue, VCount);
  for arg := 0 to high(aItems) do
    InternalSetVarRec(arg, aItems[arg]);
end;

procedure TDocVariantData.InitArray(const aItems: array of const;
  aModel: TDocVariantModel);
begin
  InitArray(aItems, JSON_[aModel]);
end;

procedure TDocVariantData.InitArrayFromVariants(const aItems: TVariantDynArray;
  aOptions: TDocVariantOptions; aItemsCopiedByReference: boolean; aCount: integer);
begin
  if aItems = nil then
    TRttiVarData(self).VType := varNull
  else
  begin
    Init(aOptions, dvArray);
    if aCount < 0 then
      VCount := length(aItems)
    else
      VCount := aCount;
    VValue := aItems; // fast by-reference copy of VValue[] array
    if not aItemsCopiedByReference then
      InitCopy(variant(self), aOptions);
  end;
end;

procedure TDocVariantData.InitArrayFromObjectValues(const aObject: variant;
  aOptions: TDocVariantOptions; aItemsCopiedByReference: boolean);
var
  dv: PDocVariantData;
begin
  if _SafeObject(aObject, dv) then
    InitArrayFromVariants(dv^.Values, aOptions, aItemsCopiedByReference, dv^.Count)
  else
    TRttiVarData(self).VType := varNull;
end;

procedure TDocVariantData.InitArrayFromObjectNames(const aObject: variant;
  aOptions: TDocVariantOptions; aItemsCopiedByReference: boolean);
var
  dv: PDocVariantData;
begin
  if _SafeObject(aObject, dv) then
    InitArrayFrom(dv^.Names, aOptions, dv^.Count)
  else
    TRttiVarData(self).VType := varNull;
end;

procedure TDocVariantData.InitArrayFromCsv(const aCsv: RawUtf8;
  aOptions: TDocVariantOptions; aSeparator: AnsiChar;
  aTrimItems, aAddVoidItems: boolean; aQuote: AnsiChar);
var
  tmp: TRawUtf8DynArray;
begin
  if aSeparator = #0 then
    aSeparator := CsvGuessSeparator(aCsv); // separator-tolerant
  CsvToRawUtf8DynArray(
    pointer(aCsv), tmp, aSeparator, aTrimItems, aAddVoidItems, aQuote);
  InitArrayFrom(tmp, aOptions);
end;

procedure _FromText(opt: TDocVariantOptions; v: PVariant; const t: RawUtf8);
begin
  if not GetVariantFromNotStringJson(
           pointer(t), PVarData(v)^, dvoAllowDoubleValue in opt) then
    if dvoInternValues in opt then
      DocVariantType.InternValues.UniqueVariant(v^, t)
    else
      RawUtf8ToVariant(t, v^);
end;

procedure TDocVariantData.InitArrayFromCsvFile(const aCsv: RawUtf8;
  aOptions: TDocVariantOptions; aSeparator, aQuote: AnsiChar);
var
  c: PUtf8Char;
  nam, tmp: TRawUtf8DynArray;
  v: PDocVariantData;
  line: RawUtf8;
  n, j, t: PtrInt;
begin
  n := 0;
  Init(aOptions, dvArray);
  c := pointer(aCsv);
  while c <> nil do
  begin
    line := GetNextLine(c, c);
    if line = '' then
      continue;
    if (tmp = nil) and
       (aSeparator = #0) then
      aSeparator := CsvGuessSeparator(line); // separator-tolerant
    tmp := nil;
    CsvToRawUtf8DynArray(pointer(line), tmp, aSeparator, false, true, aQuote);
    if tmp <> nil then
      if nam = nil then
      begin
        nam := tmp; // first line are field/column names
        n := length(nam);
      end
      else
      begin
        j := InternalAdd('', -1); // new row of data
        v := @VValue[j];      // in two lines for FPC
        v^.Init(aOptions, dvObject);
        v^.VName := nam;
        v^.VCount := n;
        SetLength(v^.VValue, n);
        t := length(tmp);
        if t > n then
          t := n; // allow too many or missing last columns
        for j := 0 to t - 1 do
          _FromText(aOptions, @v^.VValue[j], tmp[j]); // recognize numbers
      end;
  end;
end;

procedure TDocVariantData.InitArrayFrom(const aSource: TDocVariantData;
  aOptions: TDocVariantOptions; aOffset, aLimit: integer);
begin
  Init(aOptions, dvArray);
  VCount := aLimit;
  if not aSource.RangeVoid(aOffset, VCount) then // not void
    VValue := copy(aSource.VValue, aOffset, VCount); // new array, byref values
end;

function _InitArray(out aDest: TDocVariantData; aOptions: TDocVariantOptions;
  aCount: integer; const aItems): PRttiVarData;
begin
  if aCount < 0 then
    aCount := length(TByteDynArray(aItems));
  if aCount = 0 then
  begin
    TRttiVarData(aDest).VType := varNull;
    result := nil;
    exit;
  end;
  {%H-}aDest.Init(aOptions, dvArray);
  aDest.VCount := aCount;
  SetLength(aDest.VValue, aCount);
  result := pointer(aDest.VValue);
end;

procedure TDocVariantData.InitArrayFromObjArray(const ObjArray;
  aOptions: TDocVariantOptions; aWriterOptions: TTextWriterWriteObjectOptions;
  aCount: integer);
var
  ndx: PtrInt;
  aItems: TObjectDynArray absolute ObjArray;
begin
  _InitArray(self, aOptions, aCount, aItems);
  for ndx := 0 to VCount - 1 do
    ObjectToVariant(aItems[ndx], VValue[ndx], aWriterOptions);
end;

procedure TDocVariantData.InitArrayFrom(const aItems: TRawUtf8DynArray;
  aOptions: TDocVariantOptions; aCount: integer);
var
  ndx: PtrInt;
  v: PRttiVarData;
begin
  v := _InitArray(self, aOptions, aCount, aItems);
  for ndx := 0 to VCount - 1 do
  begin
    v^.VType := varString;
    RawUtf8(v^.Data.VAny) := aItems[ndx];
    inc(v);
  end;
end;

procedure TDocVariantData.InitArrayFrom(const aItems: TIntegerDynArray;
  aOptions: TDocVariantOptions; aCount: integer);
var
  ndx: PtrInt;
  v: PRttiVarData;
begin
  v := _InitArray(self, aOptions, aCount, aItems);
  for ndx := 0 to VCount - 1 do
  begin
    v^.VType := varInteger;
    v^.Data.VInteger := aItems[ndx];
    inc(v);
  end;
end;

procedure TDocVariantData.InitArrayFrom(const aItems: TInt64DynArray;
  aOptions: TDocVariantOptions; aCount: integer);
var
  ndx: PtrInt;
  v: PRttiVarData;
begin
  v := _InitArray(self, aOptions, aCount, aItems);
  for ndx := 0 to VCount - 1 do
  begin
    v^.VType := varInt64;
    v^.Data.VInt64 := aItems[ndx];
    inc(v);
  end;
end;

procedure TDocVariantData.InitArrayFrom(const aItems: TDoubleDynArray;
  aOptions: TDocVariantOptions; aCount: integer);
var
  ndx: PtrInt;
  v: PRttiVarData;
begin
  v := _InitArray(self, aOptions, aCount, aItems);
  for ndx := 0 to VCount - 1 do
  begin
    v^.VType := varDouble;
    v^.Data.VDouble := aItems[ndx];
    inc(v);
  end;
end;

procedure TDocVariantData.InitArrayFrom(var aItems; ArrayInfo: PRttiInfo;
  aOptions: TDocVariantOptions; ItemsCount: PInteger);
var
  da: TDynArray;
begin
  da.Init(ArrayInfo, aItems, ItemsCount);
  InitArrayFrom(da, aOptions);
end;

procedure TDocVariantData.InitArrayFrom(const aItems: TDynArray;
  aOptions: TDocVariantOptions);
var
  n: integer;
  pb: PByte;
  v: PVarData;
  item: TRttiCustom;
  json: RawUtf8;
begin
  Init(aOptions, dvArray);
  n := aItems.Count;
  item := aItems.Info.ArrayRtti;
  if (n = 0) or
     (item = nil) then
    exit;
  if item.Kind in (rkRecordOrDynArrayTypes + [rkClass]) then
  begin
    // use temporary non-expanded JSON conversion for complex nested content
    aItems.SaveToJson(json, [twoNonExpandedArrays]);
    if (json <> '') and
       (json[1] = '{') then
      // should be a non-expanded array, not JSON_BASE64_MAGIC_QUOTE_C
      InitArrayFromResults(pointer(json), length(json), aOptions);
  end
  else
  begin
    // handle array of simple types
    VCount := n;
    SetLength(VValue, n);
    pb := aItems.Value^;
    v := pointer(VValue);
    repeat
      inc(pb, item.ValueToVariant(pb, v^));
      inc(v);
      dec(n);
    until n = 0;
  end;
end;

function TDocVariantData.InitArrayFromResults(Json: PUtf8Char; JsonLen: PtrInt;
  aOptions: TDocVariantOptions): boolean;
var
  J: PUtf8Char;
  fieldcount, rowcount, capa, r, f: PtrInt;
  info: TGetJsonField;
  dv: PDocVariantData;
  val: PVariant;
  proto: TDocVariantData;
begin
  result := false;
  Init(aOptions, dvArray);
  info.Json := GotoNextNotSpace(Json);
  if IsNotExpandedBuffer(info.Json, Json + JsonLen, fieldcount, rowcount) then
  begin
    // A. Not Expanded (more optimized) format as array of values
    // {"fieldCount":2,"values":["f1","f2","1v1",1v2,"2v1",2v2...],"rowCount":20}
    // 1. check rowcount and fieldcount
    if (rowcount < 0) or // IsNotExpandedBuffer() detected invalid input
       (fieldcount = 0) then
      exit;
    // 2. initialize the object prototype with the trailing field names
    proto.Init(aOptions, dvObject);
    proto.Capacity := fieldcount;
    for f := 1 to fieldcount do
    begin
      info.GetJsonField;
      if not info.WasString then
        exit; // should start with field names
      proto.AddValue(info.Value, info.ValueLen, null); // set proper field name
    end;
    // 3. fill all nested objects from incoming values
    SetLength(VValue, rowcount);
    dv := pointer(VValue);
    for r := 1 to rowcount do
    begin
      val := dv^.InitFrom(proto, {values=}false); // names byref + void values
      for f := 1 to fieldcount do
      begin
        JsonToAnyVariant(val^, info, @aOptions);
        inc(val);
      end;
      if info.Json = nil then
        exit;
      inc(dv); // next object
    end;
  end
  else
  begin
    // B. Expanded format as array of objects (each with field names)
    // [{"f1":"1v1","f2":1v2},{"f2":"2v1","f2":2v2}...]
    // 1. get first object (will reuse its field names)
    info.Json := GotoFieldCountExpanded(info.Json);
    if (info.Json = nil) or
       (info.Json^ = ']') then
      exit; // [] -> valid, but void data
    info.Json := proto.InitJsonInPlace(info.Json, aOptions, @info.EndOfObject);
    if info.Json = nil then
      exit;
    if info.EndOfObject = ']' then
    begin
      AddItem(variant(proto)); // single item array
      result := true;
      exit;
    end;
    rowcount := 0;
    capa := 16;
    SetLength(VValue, capa);
    dv := pointer(VValue);
    dv^ := proto;
    // 2. get values (assume fieldcount are always the same as in the first object)
    repeat
      J := info.Json;
      while (J^ <> '{') and
            (J^ <> ']') do // go to next object beginning
        if J^ = #0 then
          exit
        else
          inc(J);
      inc(rowcount);
      if J^ = ']' then
        break;
      info.Json := J + 1; // jmp '}'
      if rowcount = capa then
      begin
        capa := NextGrow(capa);
        SetLength(VValue, capa);
        dv := @VValue[rowcount];
      end
      else
        inc(dv);
      val := dv^.InitFrom(proto, {values=}false);
      for f := 1 to proto.Count do
      begin
        info.Json := GotoEndJsonItemString(info.Json); // ignore field names
        if info.Json = nil then
          exit;
        inc(info.Json); // ignore jcEndOfJsonFieldOr0
        JsonToAnyVariant(val^, info, @aOptions);
        if info.Json = nil then
          exit;
        inc(val);
      end;
      if info.EndOfObject<> '}' then
       exit;
    until false;
  end;
  VCount := rowcount;
  result := true;
end;

function TDocVariantData.InitArrayFromResults(const Json: RawUtf8;
  aOptions: TDocVariantOptions): boolean;
var
  tmp: TSynTempBuffer;
begin
  tmp.Init(Json);
  try
    result := InitArrayFromResults(tmp.buf, tmp.len, aOptions);
  finally
    tmp.Done;
  end;
end;

function TDocVariantData.InitArrayFromResults(const Json: RawUtf8;
  aModel: TDocVariantModel): boolean;
begin
  result := InitArrayFromResults(Json, JSON_[aModel]);
end;

procedure TDocVariantData.InitObjectFromVariants(const aNames: TRawUtf8DynArray;
  const aValues: TVariantDynArray; aOptions: TDocVariantOptions);
begin
  if (aNames = nil) or
     (length(aValues) <> PDALen(PAnsiChar(aNames) - _DALEN)^ + _DAOFF) then
    TRttiVarData(self).VType := varNull
  else
  begin
    Init(aOptions, dvObject);
    VCount := PDALen(PAnsiChar(aNames) - _DALEN)^ + _DAOFF;
    VName := aNames; // fast by-reference copy of VName[] and VValue[]
    VValue := aValues;
  end;
end;

procedure TDocVariantData.InitObjectFromPath(const aPath: RawUtf8;
  const aValue: variant; aOptions: TDocVariantOptions; aPathDelim: AnsiChar);
var
  right: RawUtf8;
begin
  if aPath <> '' then
  begin
    Init(aOptions, dvObject);
    VCount := 1;
    SetLength(VName, 1);
    SetLength(VValue, 1);
    Split(aPath, aPathDelim, VName[0], right);
    if right = '' then
      VValue[0] := aValue
    else
      PDocVariantData(@VValue[0])^.InitObjectFromPath(
        right, aValue, aOptions, aPathDelim);
    exit;
  end;
  TRttiVarData(self).VType := varNull;
end;

function TDocVariantData.InitJsonInPlace(Json: PUtf8Char;
  aOptions: TDocVariantOptions; aEndOfObject: PUtf8Char): PUtf8Char;
var
  info: TGetJsonField;
  Name: PUtf8Char;
  NameLen: integer;
  n, cap: PtrInt;
  Val: PVariant;
  intnames, intvalues: TRawUtf8Interning;
begin
  Init(aOptions);
  result := nil;
  if Json = nil then
    exit;
  if Has(dvoInternValues) then
    intvalues := DocVariantType.InternValues
  else
    intvalues := nil;
  while (Json^ <= ' ') and
        (Json^ <> #0) do
    inc(Json);
  case Json^ of
    '[':
      begin
        repeat
          inc(Json);
          if Json^ = #0 then
            exit;
        until Json^ > ' ';
        Include(dvoIsArray);
        if Json^ = ']' then
          // void but valid input array
          Json := GotoNextNotSpace(Json + 1)
        else
        begin
          if Has(dvoJsonParseDoNotGuessCount) then
            cap := 8 // with a lot of nested objects -> best to ignore
          else
          begin
            // guess of the Json array items count - prefetch up to 64KB of input
            cap := abs(JsonArrayCount(Json, Json + JSON_PREFETCH));
            if cap = 0 then
              exit; // invalid content
          end;
          SetLength(VValue, cap);
          Val := pointer(VValue);
          n := 0;
          info.Json := Json;
          repeat
            if n = cap then
            begin
              // grow if our initial guess was aborted due to huge input
              cap := NextGrow(cap);
              SetLength(VValue, cap);
              Val := @VValue[n];
            end;
            // unserialize the next item
            JsonToAnyVariant(val^, info, @VOptions);
            if info.Json = nil then
              break; // invalid input
            if intvalues <> nil then
              intvalues.UniqueVariant(val^);
            inc(Val);
            inc(n);
          until info.EndOfObject = ']';
          Json := info.Json;
          if Json = nil then
          begin
            // invalid input
            VValue := nil;
            exit;
          end;
          // ok - but no SetLength(..,VCount) if NextGrow() on huge input
          VCount := n;
        end;
      end;
    '{':
      begin
        repeat
          inc(Json);
          if Json^ = #0 then
            exit;
        until Json^ > ' ';
        Include(dvoIsObject);
        if Json^ = '}' then
          // void but valid input object
          Json := GotoNextNotSpace(Json + 1)
        else
        begin
          if Has(dvoJsonParseDoNotGuessCount) then
            cap := 4 // with a lot of nested documents -> best to ignore
          else
          begin
            // guess of the Json object properties count - prefetch up to 64KB
            cap := JsonObjectPropCount(Json, Json + JSON_PREFETCH);
            if cap = 0 then
              exit // invalid content (was <0 if early abort)
            else if cap < 0 then
            begin // nested or huge objects are evil -> no more guess
              cap := -cap;
              Include(dvoJsonParseDoNotGuessCount);
            end;
          end;
          if Has(dvoInternNames) then
            intnames := DocVariantType.InternNames
          else
            intnames := nil;
          SetLength(VValue, cap);
          Val := pointer(VValue);
          SetLength(VName, cap);
          n := 0;
          info.Json := Json;
          repeat
            // see http://docs.mongodb.org/manual/reference/mongodb-extended-Json
            Name := GetJsonPropName(info.Json, @NameLen);
            if Name = nil then
              break; // invalid input
            if n = cap then
            begin
              // grow if our initial guess was aborted due to huge input
              cap := NextGrow(cap);
              SetLength(VName, cap);
              SetLength(VValue, cap);
              Val := @VValue[n];
            end;
            JsonToAnyVariant(Val^, info, @VOptions);
            if info.Json = nil then
              if info.EndOfObject = '}' then // valid object end
                info.Json := @NULCHAR
              else
                break; // invalid input
            if NameLen <> 0 then // we just ignore void "":xxx field names
            begin
              if intnames <> nil then
                intnames.Unique(VName[n], Name, NameLen)
              else
                FastSetString(VName[n], Name, NameLen);
              if intvalues <> nil then
                intvalues.UniqueVariant(Val^);
              inc(n);
              inc(Val);
            end;
          until info.EndOfObject = '}';
          Json := info.Json;
          if (Name = nil) or
             (Json = nil) then
          begin
            // invalid input
            VName := nil;
            VValue := nil;
            exit;
          end;
          // ok - but no SetLength(..,VCount) if NextGrow() on huge input
          VCount := n;
        end;
      end;
    'n',
    'N':
      begin
        if IdemPChar(Json + 1, 'ULL') then
        begin
          Include(dvoIsObject);
          result := GotoNextNotSpace(Json + 4);
        end;
        exit;
      end;
  else
    exit;
  end;
  while (Json^ <= ' ') and
        (Json^ <> #0) do
    inc(Json);
  if aEndOfObject <> nil then
    aEndOfObject^ := Json^;
  if Json^ <> #0 then
    repeat
      inc(Json)
    until (Json^ = #0) or
          (Json^ > ' ');
  result := Json; // indicates successfully parsed
end;

function TDocVariantData.InitJson(const Json: RawUtf8;
  aOptions: TDocVariantOptions): boolean;
var
  tmp: TSynTempBuffer;
begin
  if Json = '' then
    result := false
  else
  begin
    tmp.Init(Json);
    try
      result := InitJsonInPlace(tmp.buf, aOptions) <> nil;
    finally
      tmp.Done;
    end;
  end;
end;

function TDocVariantData.InitJson(const Json: RawUtf8; aModel: TDocVariantModel): boolean;
begin
  result := InitJson(Json, JSON_[aModel]);
end;

function TDocVariantData.InitJsonFromFile(const FileName: TFileName;
  aOptions: TDocVariantOptions): boolean;
begin
  result := InitJsonInPlace(pointer(RawUtf8FromFile(FileName)), aOptions) <> nil;
end;

procedure TDocVariantData.InitFromPairs(aPairs: PUtf8Char;
  aOptions: TDocVariantOptions; NameValueSep, ItemSep: AnsiChar; DoTrim: boolean);
var
  n, v: RawUtf8;
  val: variant;
begin
  Init(aOptions, dvObject);
  while aPairs <> nil do
  begin
    GetNextItem(aPairs, NameValueSep, n);
    if ItemSep = #10 then
      GetNextItemTrimedCRLF(aPairs, v)
    else
      GetNextItem(aPairs, ItemSep, v);
    if DoTrim then
      TrimSelf(v);
    if n = '' then
      break;
    RawUtf8ToVariant(v, val);
    AddValue(n, val);
  end;
end;

procedure TDocVariantData.InitFromPairs(const aPairs: RawUtf8;
  aOptions: TDocVariantOptions; NameValueSep, ItemSep: AnsiChar; DoTrim: boolean);
begin
  InitFromPairs(pointer(aPairs), aOptions, NameValueSep, ItemSep, DoTrim);
end;

procedure TDocVariantData.InitCopy(const SourceDocVariant: variant;
  aOptions: TDocVariantOptions);
var
  ndx: PtrInt;
  vt: cardinal;
  Source: PDocVariantData;
  SourceVValue: TVariantDynArray;
  Handler: TCustomVariantType;
  v: PVarData;
  vv: PVariant;
begin
  with TVarData(SourceDocVariant) do
    if cardinal(VType) = varVariantByRef then
      Source := VPointer
    else
      Source := @SourceDocVariant;
  if cardinal(Source^.VType) <> DocVariantVType then
    raise EDocVariant.CreateUtf8(
      'Unexpected InitCopy(var%)', [VariantTypeName(PVarData(Source))^]);
  SourceVValue := Source^.VValue; // local fast per-reference copy
  if Source <> @self then
  begin
    VType := Source^.VType;
    VCount := Source^.VCount;
    pointer(VName) := nil;  // avoid GPF
    pointer(VValue) := nil;
    aOptions := aOptions - [dvoIsArray, dvoIsObject]; // may not match Source
    if Source^.IsArray then
      system.include(aOptions, dvoIsArray)
    else if Source^.IsObject then
    begin
      system.include(aOptions, dvoIsObject);
      SetLength(VName, VCount);
      for ndx := 0 to VCount - 1 do
        VName[ndx] := Source^.VName[ndx]; // manual copy is needed
      if (dvoInternNames in aOptions) and
         not Source^.Has(dvoInternNames) then
        with DocVariantType.InternNames do
          for ndx := 0 to VCount - 1 do
            UniqueText(VName[ndx]);
    end;
    VOptions := aOptions;
  end
  else
  begin
    SetOptions(aOptions);
    VariantDynArrayClear(VValue); // full copy of all values
  end;
  if VCount > 0 then
  begin
    SetLength(VValue, VCount);
    v := pointer(SourceVValue);
    vv := pointer(VValue);
    ndx := VCount;
    repeat
      repeat
        vt := v^.VType;
        if vt <> varVariantByRef then
          break;
        v := v^.VPointer;
      until false;
      if vt < varFirstCustom then
        // simple string/number types copy
        vv^ := variant(v^)
      else if vt = DocVariantVType then
        // direct recursive copy for TDocVariant
        PDocVariantData(vv)^.InitCopy(variant(v^), VOptions)
      else if FindCustomVariantType(vt, Handler) then
        if Handler.InheritsFrom(TSynInvokeableVariantType) then
          TSynInvokeableVariantType(Handler).CopyByValue(PVarData(vv)^, v^)
        else
          Handler.Copy(PVarData(vv)^, v^, false)
      else
        vv^ := variant(v^); // default copy
      inc(v);
      inc(vv);
      dec(ndx);
    until ndx = 0;
    if Has(dvoInternValues) then
    begin
      ndx := VCount;
      vv := pointer(VValue);
      with DocVariantType.InternValues do
        repeat
          UniqueVariant(vv^);
          inc(vv);
          dec(ndx);
        until ndx = 0;
    end;
  end;
  VariantDynArrayClear(SourceVValue);
end;

procedure TDocVariantData.Void;
begin
  VCount := 0;
  if VName <> nil then
    FastDynArrayClear(@VName, TypeInfo(RawUtf8));
  if VValue <> nil then
    FastDynArrayClear(@VValue, TypeInfo(variant));
end;

procedure TDocVariantData.Clear;
begin
  if cardinal(VType) = DocVariantVType then
    ClearFast
  else
    VarClear(variant(self));
end;

procedure TDocVariantData.Reset;
begin
  VOptions := VOptions - [dvoIsArray, dvoIsObject];
  Void;
end;

procedure TDocVariantData.FillZero;
var
  n: integer;
  v: PVariant;
begin
  n := VCount;
  v := pointer(VValue);
  if n <> 0 then
    repeat
      mormot.core.variants.FillZero(v^);
      inc(v);
      dec(n);
    until n = 0;
  Reset;
end;

function TDocVariantData.GetModel(out model: TDocVariantModel): boolean;
var
  opt: TDocVariantOptions;
  ndx: PtrInt;
begin
  opt := VOptions - [dvoIsArray, dvoIsObject, dvoJsonParseDoNotGuessCount];
  ndx := WordScanIndex(@JSON_, ord(high(TDocVariantModel)) + 1, word(opt));
  if ndx < 0 then
    result := false
  else
  begin
    model := TDocVariantModel(ndx);
    result := true;
  end;
end;

function TDocVariantData.RangeVoid(var Offset, Limit: integer): boolean;
var
  n, l: integer;
begin
  result := true; // void
  n := Count;
  if Offset < 0 then
  begin
    inc(Offset, n);
    if Offset < 0 then
      Offset := 0;
  end;
  if Limit = 0 then
    Limit := n;
  l := n - Offset;
  if l <= 0 then
    exit;
  if Limit > l then
    Limit := l;
  result := false; // not void
end;

procedure TDocVariantData.SetCount(aCount: integer);
begin
  VCount := aCount;
end;

function TDocVariantData.Compare(const Another: TDocVariantData;
  CaseInsensitive: boolean): integer;
var
  n: integer;
  ndx: PtrInt;
  v1, v2: PVarData;
  nameCmp: TDynArraySortCompare;
begin
  // first validate the type: as { or [ in JSON
  result := -1;
  nameCmp := nil;
  if IsArray then
  begin
    if not Another.IsArray then
      exit;
  end
  else if IsObject then
    if not Another.IsObject then
      exit
    else
      nameCmp := SortDynArrayAnsiStringByCase[not IsCaseSensitive];
  // compare as many in-order content as possible
  n := Another.VCount;
  if VCount < n then
    n := VCount;
  v1 := pointer(VValue);
  v2 := pointer(Another.VValue);
  for ndx := 0 to n - 1 do
  begin
    if Assigned(nameCmp) then
    begin // each name should match
      result := nameCmp(VName[ndx], Another.VName[ndx]);
      if result <> 0 then
        exit;
    end;
    result := FastVarDataComp(v1, v2, CaseInsensitive);
    if result <> 0 then // each value should match
      exit;
    inc(v1);
    inc(v2);
  end;
  // all content did match -> difference is now about the document count
  result := VCount - Another.VCount;
end;

function TDocVariantData.CompareObject(const ObjFields: array of RawUtf8;
  const Another: TDocVariantData; CaseInsensitive: boolean): integer;
var
  f: PtrInt;
  prev: integer;
  v1, v2: PVariant;
begin
  if IsObject then
    if Another.IsObject then // compare Object, possibly by specified fields
    begin
      if high(ObjFields) < 0 then
      begin
        result := Compare(Another, CaseInsensitive);
        exit;
      end;
      for f := 0 to high(ObjFields) do
      begin
        prev := -1; // optimistic: fields may be in the same position
        GetObjectProp(ObjFields[f], v1, @prev);
        Another.GetObjectProp(ObjFields[f], v2, @prev);
        result := FastVarDataComp(pointer(v1), pointer(v2), CaseInsensitive);
        if result <> 0 then // each value should match
          exit;
      end;
      result := 0; // all supplied fields did match
    end
    else
      result := 1   // Object, not Object
  else if Another.IsObject then
    result := -1  // not Object, Object
  else
    result := 0;  // not Object, not Object
end;

function TDocVariantData.Equals(const Another: TDocVariantData;
  CaseInsensitive: boolean): boolean;
begin
  result := Compare(Another, CaseInsensitive) = 0;
end;

function TDocVariantData.Compare(const aName: RawUtf8; const aValue: variant;
  aCaseInsensitive: boolean): integer;
var
  v: PVariant;
begin
  if (cardinal(VType) = DocVariantVType) and
     GetObjectProp(aName, v{%H-}, nil) then
    result := FastVarDataComp(pointer(v), @aValue, aCaseInsensitive)
  else
    result := -1;
end;

function TDocVariantData.Equals(const aName: RawUtf8; const aValue: variant;
  aCaseInsensitive: boolean): boolean;
var
  v: PVariant;
begin
  result := (cardinal(VType) = DocVariantVType) and
            GetObjectProp(aName, v{%H-}, nil) and
            (FastVarDataComp(@aValue, pointer(v), aCaseInsensitive) = 0);
end;

function TDocVariantData.InternalAddBuf(aName: PUtf8Char; aNameLen: integer): integer;
var
  tmp: RawUtf8; // so that the caller won't need to reserve such a temp var
begin
  FastSetString(tmp, aName, aNameLen);
  result := InternalAdd(tmp, -1);
end;

function TDocVariantData.InternalAdd(
  const aName: RawUtf8; aIndex: integer): integer;
var
  len: integer;
  v: PVariantArray;
  k: PRawUtf8Array;
begin
  // validate consistent add/insert
  if aName <> '' then
  begin
    if IsArray then
      raise EDocVariant.CreateUtf8(
        'Add: Unexpected [%] object property in an array', [aName]);
    if not IsObject then
    begin
      VType := DocVariantVType; // may not be set yet
      Include(dvoIsObject);
    end;
  end
  else
  begin
    if IsObject then
      raise EDocVariant.Create('Add: Unexpected array item in an object');
    if not IsArray then
    begin
      VType := DocVariantVType; // may not be set yet
      Include(dvoIsArray);
    end;
  end;
  // grow up memory if needed
  len := length(VValue);
  if VCount >= len then
  begin
    len := NextGrow(VCount);
    SetLength(VValue, len);
  end;
  result := VCount;
  inc(VCount);
  if cardinal(aIndex) < cardinal(result) then
  begin
    // reserve space for the inserted new item within existing data
    dec(result, aIndex);
    v := @VValue[aIndex];
    MoveFast(v[0], v[1], result * SizeOf(variant));
    PInteger(v)^ := varEmpty; // avoid GPF
    if aName <> '' then
    begin
      if Length(VName) <> len then
        SetLength(VName, len);
      k := @VName[aIndex];
      MoveFast(k[0], k[1], result * SizeOf(pointer));
      PPointer(k)^ := nil; // avoid GPF
    end;
    result := aIndex;
  end;
  if aName = '' then
    exit;
  // store the object field name
  if Length(VName) <> len then
    SetLength(VName, len);
  if Has(dvoInternNames) then
    DocVariantType.InternNames.Unique(VName[result], aName)
  else
    VName[result] := aName;
end;

{$ifdef HASITERATORS}

function TDocVariantData.GetEnumerator: TDocVariantFieldsEnumerator;
begin
  result.State.Init(pointer(Values), VCount);
  if IsObject then
  begin
    result.Name := pointer(Names);
    dec(result.Name);
  end
  else
    result.Name := nil;
end;

function TDocVariantData.Items: TDocVariantItemsEnumerator;
begin
  if IsObject then
    result{%H-}.State.Void
  else
    result.State.Init(pointer(Values), VCount);
end;

function TDocVariantData.Objects: TDocVariantObjectsEnumerator;
begin
  if IsObject then
    result{%H-}.State.Void
  else
    result.State.Init(pointer(Values), VCount);
end;

function TDocVariantData.Fields: TDocVariantFieldsEnumerator;
begin
  if IsArray then
    result{%H-}.State.Void
  else
    result := GetEnumerator;
end;

function TDocVariantData.FieldNames: TDocVariantFieldNamesEnumerator;
begin
  if IsArray or
     (VCount = 0) then
  begin
    result.Curr := nil;
    result.After := nil;
  end
  else
  begin
    result.Curr := pointer(Names);
    result.After := @Names[VCount];
    dec(result.Curr);
  end;
end;

function TDocVariantData.FieldValues: TDocVariantItemsEnumerator;
begin
  if IsArray then
    result{%H-}.State.Void
  else
    result.State.Init(pointer(Values), VCount);
end;

{$endif HASITERATORS}

procedure TDocVariantData.SetCapacity(aValue: integer);
begin
  if IsObject then
    SetLength(VName, aValue);
  SetLength(VValue, aValue);
end;

function TDocVariantData.AddValue(const aName: RawUtf8; const aValue: variant;
  aValueOwned: boolean; aIndex: integer): integer;
var
  v: PVariant;
begin
  if aName = '' then
  begin
    result := -1;
    exit;
  end;
  if Has(dvoCheckForDuplicatedNames) then
    if GetValueIndex(aName) >= 0 then
      raise EDocVariant.CreateUtf8('AddValue: Duplicated [%] name', [aName]);
  result := InternalAdd(aName, aIndex);
  v := @VValue[result];
  if aValueOwned then
    v^ := aValue
  else
    SetVariantByValue(aValue, v^);
  if Has(dvoInternValues) then
    InternalUniqueValue(v);
end;

function TDocVariantData.AddValue(aName: PUtf8Char; aNameLen: integer;
  const aValue: variant; aValueOwned: boolean; aIndex: integer): integer;
var
  tmp: RawUtf8;
begin
  FastSetString(tmp, aName, aNameLen);
  result := AddValue(tmp, aValue, aValueOwned, aIndex);
end;

function TDocVariantData.AddValueFromText(const aName, aValue: RawUtf8;
  DoUpdate: boolean): integer;
var
  v: PVariant;
begin
  if aName = '' then
  begin
    result := -1;
    exit;
  end;
  result := GetValueIndex(aName);
  if not DoUpdate and
     (Has(dvoCheckForDuplicatedNames)) and
     (result >= 0) then
    raise EDocVariant.CreateUtf8(
      'AddValueFromText: Duplicated [%] name', [aName]);
  if result < 0 then
    result := InternalAdd(aName);
  v := @VValue[result];
  VarClear(v^);
  _FromText(VOptions, v, aValue); // recognize numbers
end;

procedure TDocVariantData.AddByPath(const aSource: TDocVariantData;
  const aPaths: array of RawUtf8; aPathDelim: AnsiChar);
var
  ndx, added: PtrInt;
  v: TVarData;
begin
  if (aSource.Count = 0) or
     (not aSource.IsObject) or
     IsArray then
    exit;
  for ndx := 0 to High(aPaths) do
  begin
    DocVariantType.Lookup(v, TVarData(aSource), pointer(aPaths[ndx]), aPathDelim);
    if cardinal(v.VType) < varNull then
      continue; // path not found
    added := InternalAdd(aPaths[ndx]);
    PVarData(@VValue[added])^ := v;
    if Has(dvoInternValues) then
      InternalUniqueValueAt(added);
  end;
end;

procedure TDocVariantData.AddFrom(const aDocVariant: Variant);
var
  src: PDocVariantData;
  n: integer;
  v: PVariant;
  k: PRawUtf8;
begin
  src := _Safe(aDocVariant);
  n := src^.Count;
  if n = 0 then
    exit; // nothing to add
  v := pointer(src^.VValue);
  k := pointer(src^.VName);
  if k = nil then // source aDocVariant is a dvArray
    // add array items
    if IsObject then
      // types should match
      exit
    else
      repeat
        AddItem(v^);
        inc(v);
        dec(n)
      until n = 0
  else
    // add object items
    if IsArray then
      // types should match
      exit
    else if Has(dvoCheckForDuplicatedNames) then
      repeat
        AddOrUpdateValue(k^, v^);
        inc(k);
        inc(v);
        dec(n)
      until n = 0
    else
      repeat
        AddValue(k^, v^);
        inc(k);
        inc(v);
        dec(n)
      until n = 0;
end;

procedure TDocVariantData.AddOrUpdateFrom(const aDocVariant: Variant;
  aOnlyAddMissing: boolean);
var
  src: PDocVariantData;
  n: integer;
  v: PVariant;
  k: PRawUtf8;
begin
  src := _Safe(aDocVariant, dvObject);
  n := src^.Count;
  if n = 0 then
    exit; // nothing to add
  v := pointer(src^.VValue);
  k := pointer(src^.VName);
  repeat
    AddOrUpdateValue(k^, v^, nil, aOnlyAddMissing);
    inc(k);
    inc(v);
    dec(n)
  until n = 0;
end;

function TDocVariantData.AddItem(const aValue: variant; aIndex: integer): integer;
begin
  result := InternalAdd('', aIndex);
  InternalSetValue(result, aValue);
end;

function TDocVariantData.AddItem(const aValue: TDocVariantData; aIndex: integer): integer;
begin
  result := InternalAdd('', aIndex);
  InternalSetValue(result, variant(aValue));
end;

function TDocVariantData.AddItemFromText(const aValue: RawUtf8; aIndex: integer): integer;
begin
  result := InternalAdd('', aIndex);
  _FromText(VOptions, @VValue[result], aValue); // recognize numbers
end;

function TDocVariantData.AddItemText(
  const aValue: RawUtf8; aIndex: integer): integer;
begin
  result := InternalAdd('', aIndex);
  if Has(dvoInternValues) then
    DocVariantType.InternValues.UniqueVariant(VValue[result], aValue)
  else
    RawUtf8ToVariant(aValue, VValue[result]);
end;

procedure TDocVariantData.AddItems(const aValue: array of const);
var
  ndx, added: PtrInt;
begin
  for ndx := 0 to high(aValue) do
  begin
    added := InternalAdd('');
    VarRecToVariant(aValue[ndx], VValue[added]);
    if Has(dvoInternValues) then
      InternalUniqueValueAt(added);
  end;
end;

procedure TDocVariantData.AddObject(const aNameValuePairs: array of const;
  const aName: RawUtf8);
var
  added: PtrInt;
  obj: PDocVariantData;
begin
  if (aName <> '') and
     (Has(dvoCheckForDuplicatedNames)) then
    if GetValueIndex(aName) >= 0 then
      raise EDocVariant.CreateUtf8('AddObject: Duplicated [%] name', [aName]);
  added := InternalAdd(aName);
  obj := @VValue[added];
  if PInteger(obj)^ = 0 then // most common case is adding a new value
    obj^.InitClone(self)     // same options than owner document
  else if (obj^.VType <> VType) or
          not obj^.IsObject then
    raise EDocVariant.CreateUtf8('AddObject: wrong existing [%]', [aName]);
  obj^.AddNameValuesToObject(aNameValuePairs);
  if Has(dvoInternValues) then
    InternalUniqueValueAt(added);
end;

function TDocVariantData.GetObjectProp(const aName: RawUtf8;
  out aFound: PVariant; aPreviousIndex: PInteger): boolean;
var
  ndx, n: PtrInt;
begin
  result := false;
  aFound := nil;
  n := VCount;
  if (n = 0) or
     (aName = '') or
     not IsObject then
    exit;
  ndx := -1;
  if aPreviousIndex <> nil then
  begin // optimistic try if this field is in the same place
    ndx := aPreviousIndex^;
    if (PtrUInt(ndx) >= PtrUInt(n)) or
       (SortDynArrayAnsiStringByCase[not IsCaseSensitive](
         VName[ndx], aName) <> 0) then
      ndx := -1;
  end;
  if ndx < 0 then
    ndx := FindNonVoid[IsCaseSensitive](
          pointer(VName), pointer(aName), length(aName), n);
  if ndx < 0 then
    exit;
  if aPreviousIndex <> nil then
    aPreviousIndex^ := ndx;
  aFound := @VValue[ndx];
  result  := true;
end;

function TDocVariantData.SearchItemByProp(const aPropName, aPropValue: RawUtf8;
  aPropValueCaseSensitive: boolean): integer;
var
  v: PVariant;
  prev: integer;
begin
  if IsObject then
  begin
    result := GetValueIndex(aPropName);
    if (result >= 0) and
       VariantEquals(VValue[result], aPropValue, aPropValueCaseSensitive) then
      exit;
  end
  else if IsArray then
  begin
    prev := -1; // optimistic search aPropName at the previous field position
    for result := 0 to VCount - 1 do
      if _Safe(VValue[result])^.GetObjectProp(aPropName, v, @prev) and
         VariantEquals({%H-}v^, aPropValue, aPropValueCaseSensitive) then
        exit;
  end;
  result := -1;
end;

function TDocVariantData.SearchItemByProp(const aPropNameFmt: RawUtf8;
  const aPropNameArgs: array of const; const aPropValue: RawUtf8;
  aPropValueCaseSensitive: boolean): integer;
var
  name: RawUtf8;
begin
  FormatUtf8(aPropNameFmt, aPropNameArgs, name);
  result := SearchItemByProp(name, aPropValue, aPropValueCaseSensitive);
end;

function TDocVariantData.SearchItemByValue(const aValue: Variant;
  CaseInsensitive: boolean; StartIndex: PtrInt): PtrInt;
var
  v: PVarData;
  tmp: variant;
begin
  SetVariantByValue(aValue, tmp); // ensure text is RawUtf8
  v := @VValue[StartIndex];
  for result := StartIndex to VCount - 1 do
    if FastVarDataComp(v, @tmp, CaseInsensitive) = 0 then
      exit
    else
      inc(v);
  result := -1;
end;

function TDocVariantData.CountItemByValue(const aValue: Variant;
  CaseInsensitive: boolean; StartIndex: integer): integer;
var
  v: PVarData;
  ndx: integer;
  tmp: variant;
begin
  result := 0; // returns the number of occurences of this value
  SetVariantByValue(aValue, tmp); // ensure text is RawUtf8
  v := @VValue[StartIndex];
  for ndx := StartIndex to VCount - 1 do
  begin
    if FastVarDataComp(v, @tmp, CaseInsensitive) = 0 then
      inc(result);
    inc(v);
  end;
end;

type
  {$ifdef USERECORDWITHMETHODS}
  TQuickSortDocVariant = record
  {$else}
  TQuickSortDocVariant = object
  {$endif USERECORDWITHMETHODS}
  public
    names: PPointerArray;
    values: PVariantArray;
    nameCompare: TUtf8Compare;
    valueCompare: TVariantCompare;
    valueComparer: TVariantComparer;
    reversed: PtrInt;
    procedure SortByName(L, R: PtrInt);
    procedure SortByValue(L, R: PtrInt);
  end;

procedure TQuickSortDocVariant.SortByName(L, R: PtrInt);
var
  I, J, P: PtrInt;
  pivot: pointer;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        pivot := names[P];
        while nameCompare(names[I], pivot) * reversed < 0 do
          inc(I);
        while nameCompare(names[J], pivot) * reversed > 0 do
          dec(J);
        if I <= J then
        begin
          if I <> J then
          begin
            ExchgPointer(@names[I], @names[J]);
            ExchgVariant(@values[I], @values[J]);
          end;
          if P = I then
            P := J
          else if P = J then
            P := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin
        // use recursion only for smaller range
        if L < J then
          SortByName(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          SortByName(I, R);
        R := J;
      end;
    until L >= R;
end;

procedure TQuickSortDocVariant.SortByValue(L, R: PtrInt);
var
  I, J, P: PtrInt;
  pivot: PVariant;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        pivot := @values[P];
        if Assigned(valueCompare) then
        begin // called from SortByValue
          while valueCompare(values[I], pivot^) * reversed < 0 do
            inc(I);
          while valueCompare(values[J], pivot^) * reversed > 0 do
            dec(J);
        end
        else
        begin // called from SortByRow
          while valueComparer(values[I], pivot^) * reversed < 0 do
            inc(I);
          while valueComparer(values[J], pivot^) * reversed > 0 do
            dec(J);
        end;
        if I <= J then
        begin
          if I <> J then
          begin
            if names <> nil then
              ExchgPointer(@names[I], @names[J]);
            ExchgVariant(@values[I], @values[J]);
          end;
          if P = I then
            P := J
          else if P = J then
            P := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin
        // use recursion only for smaller range
        if L < J then
          SortByValue(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          SortByValue(I, R);
        R := J;
      end;
    until L >= R;
end;

procedure TDocVariantData.SortByName(
  SortCompare: TUtf8Compare; SortCompareReversed: boolean);
var
  qs: TQuickSortDocVariant;
begin
  if (not IsObject) or
     (VCount <= 1) then
    exit;
  if Assigned(SortCompare) then
    qs.nameCompare := SortCompare
  else
    qs.nameCompare := @StrIComp;
  qs.names := pointer(VName);
  qs.values := pointer(VValue);
  if SortCompareReversed then
    qs.reversed := -1
  else
    qs.reversed := 1;
  qs.SortByName(0, VCount - 1);
end;

procedure TDocVariantData.SortByValue(SortCompare: TVariantCompare;
  SortCompareReversed: boolean);
var
  qs: TQuickSortDocVariant;
begin
  if VCount <= 1 then
    exit;
  if Assigned(SortCompare) then
    qs.valueCompare := SortCompare
  else
    qs.valueCompare := @VariantCompare;
  qs.valueComparer := nil;
  qs.names := pointer(VName);
  qs.values := pointer(VValue);
  if SortCompareReversed then
    qs.reversed := -1
  else
    qs.reversed := 1;
  qs.SortByValue(0, VCount - 1);
end;

procedure TDocVariantData.SortByRow(const SortComparer: TVariantComparer;
  SortComparerReversed: boolean);
var
  qs: TQuickSortDocVariant;
begin
  if (VCount <= 1) or
     (not Assigned(SortComparer)) then
    exit;
  qs.valueCompare := nil;
  qs.valueComparer := SortComparer;
  qs.names := pointer(VName);
  qs.values := pointer(VValue);
  if SortComparerReversed then
    qs.reversed := -1
  else
    qs.reversed := 1;
  qs.SortByValue(0, VCount - 1);
end;

type
  TQuickSortByFieldLookup = array[0..3] of PVariant;
  PQuickSortByFieldLookup = ^TQuickSortByFieldLookup;

  {$ifdef USERECORDWITHMETHODS}
  TQuickSortDocVariantValuesByField = record
  {$else}
  TQuickSortDocVariantValuesByField = object
  {$endif USERECORDWITHMETHODS}
  public
    Lookup: array of TQuickSortByFieldLookup; // per-name values
    Compare: TVariantCompare;
    CompareField: TVariantCompareField;
    Fields: PRawUtf8Array;
    P: PtrInt;
    Pivot: PQuickSortByFieldLookup;
    Doc: PDocVariantData;
    TempExch: TQuickSortByFieldLookup;
    Reverse: boolean;
    Depth: integer; // = high(Lookup)
    procedure Init(const aPropNames: array of RawUtf8;
      aNameSortedCompare: TUtf8Compare);
    function DoComp(Value: PQuickSortByFieldLookup): PtrInt;
      {$ifndef CPUX86} inline; {$endif}
    procedure Sort(L, R: PtrInt);
  end;

procedure TQuickSortDocVariantValuesByField.Init(
  const aPropNames: array of RawUtf8; aNameSortedCompare: TUtf8Compare);
var
  namecomp: TUtf8Compare;
  v: pointer;
  row, f: PtrInt;
  rowdata: PDocVariantData;
  ndx: integer;
begin
  Depth := high(aPropNames);
  if (Depth < 0) or
     (Depth > high(TQuickSortByFieldLookup)) then
    raise EDocVariant.CreateUtf8('TDocVariantData.SortByFields(%)', [Depth]);
  // resolve GetPVariantByName(aPropNames) once into Lookup[]
  SetLength(Lookup, Doc^.VCount);
  if Assigned(aNameSortedCompare) then // just like GetVarData() searches names
    namecomp := aNameSortedCompare
  else
    namecomp := StrCompByCase[not Doc^.IsCaseSensitive];
  for f := 0 to Depth do
  begin
    if aPropNames[f] = '' then
      raise EDocVariant.CreateUtf8('TDocVariantData.SortByFields(%=void)', [f]);
    ndx := -1;
    for row := 0 to Doc^.VCount - 1 do
    begin
      rowdata := _Safe(Doc^.VValue[row]);
      if (cardinal(ndx) < cardinal(rowdata^.VCount)) and
         (namecomp(pointer(rowdata^.VName[ndx]), pointer(aPropNames[f])) = 0) then
        v := @rowdata^.VValue[ndx] // get the value at the (likely) same position
      else
      begin
        v := rowdata^.GetVarData(aPropNames[f], aNameSortedCompare, @ndx);
        if v = nil then
          v := @NullVarData;
      end;
      Lookup[row, f] := v;
    end;
  end;
end;

function TQuickSortDocVariantValuesByField.DoComp(
  Value: PQuickSortByFieldLookup): PtrInt;
begin
  if Assigned(Compare) then
  begin
    result := Compare(Value[0]^, Pivot[0]^);
    if (result = 0) and
       (depth > 0) then
    begin
      result := Compare(Value[1]^, Pivot[1]^);
      if (result = 0) and
         (depth > 1) then
      begin
        result := Compare(Value[2]^, Pivot[2]^);
        if (result = 0) and
           (depth > 2) then
         result := Compare(Value[3]^, Pivot[3]^);
      end;
    end;
  end
  else
  begin
    result := CompareField(Fields[0], Value[0]^, Pivot[0]^);
    if (result = 0) and
       (depth > 0) then
    begin
      result := CompareField(Fields[1], Value[1]^, Pivot[1]^);
      if (result = 0) and
         (depth > 1) then
      begin
        result := CompareField(Fields[2], Value[2]^, Pivot[2]^);
        if (result = 0) and
           (depth > 2) then
         result := CompareField(Fields[3], Value[3]^, Pivot[3]^);
      end;
    end;
  end;
  if Reverse then
    result := -result;
end;

procedure TQuickSortDocVariantValuesByField.Sort(L, R: PtrInt);
var
  I, J: PtrInt;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        Pivot := @Lookup[P];
        while DoComp(@Lookup[I]) < 0 do
          inc(I);
        while DoComp(@Lookup[J]) > 0 do
          dec(J);
        if I <= J then
        begin
          if I <> J then
          begin
            if Doc.VName <> nil then
              ExchgPointer(@Doc.VName[I], @Doc.VName[J]);
            ExchgVariant(@Doc.VValue[I], @Doc.VValue[J]);
            ExchgPointers(@Lookup[I], @Lookup[J], Depth + 1);
          end;
          if P = I then
            P := J
          else if P = J then
            P := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin
        // use recursion only for smaller range
        if L < J then
          Sort(L, J);
        L := I;
      end
      else
      begin
        if I < R then
          Sort(I, R);
        R := J;
      end;
    until L >= R;
end;

procedure TDocVariantData.SortArrayByField(const aItemPropName: RawUtf8;
  aValueCompare: TVariantCompare; aValueCompareReverse: boolean;
  aNameSortedCompare: TUtf8Compare);
var
  QS: TQuickSortDocVariantValuesByField;
begin
  if (VCount <= 0) or
     (aItemPropName = '') or
     not IsArray then
    exit;
  if not Assigned(aValueCompare) then
    aValueCompare := VariantCompare;
  QS.Compare := aValueCompare;
  QS.Doc := @self;
  QS.Init([aItemPropName], aNameSortedCompare);
  QS.Reverse := aValueCompareReverse;
  QS.Sort(0, VCount - 1);
end;

procedure TDocVariantData.SortArrayByFields(
  const aItemPropNames: array of RawUtf8; aValueCompare: TVariantCompare;
  const aValueCompareField: TVariantCompareField;
  aValueCompareReverse: boolean; aNameSortedCompare: TUtf8Compare);
var
  QS: TQuickSortDocVariantValuesByField;
begin
  if (VCount <= 0) or
     not IsArray then
    exit;
  if Assigned(aValueCompareField) then
  begin
    QS.Compare := nil;
    QS.Fields := @aItemPropNames[0];
    QS.CompareField := aValueCompareField;
  end
  else if Assigned(aValueCompare) then
      QS.Compare := aValueCompare
    else
      QS.Compare := VariantCompare;
  QS.Doc := @self;
  QS.Init(aItemPropNames, aNameSortedCompare);
  QS.Reverse := aValueCompareReverse;
  QS.Sort(0, VCount - 1);
end;

procedure TDocVariantData.Reverse;
begin
  if VCount <= 0 then
    exit;
  if VName <> nil then
  begin
    DynArrayFakeLength(VName, VCount);
    DynArray(TypeInfo(TRawUtf8DynArray), VName).Reverse;
  end;
  DynArrayFakeLength(VValue, VCount);
  DynArray(TypeInfo(TVariantDynArray), VValue).Reverse;
end;

function TDocVariantData.Reduce(const aPropNames: array of RawUtf8;
  aCaseSensitive, aDoNotAddVoidProp: boolean): variant;
begin
  VarClear(result{%H-});
  Reduce(aPropNames, aCaseSensitive, PDocVariantData(@result)^, aDoNotAddVoidProp);
end;

procedure TDocVariantData.Reduce(const aPropNames: array of RawUtf8;
  aCaseSensitive: boolean; var result: TDocVariantData; aDoNotAddVoidProp: boolean);
var
  ndx, j: PtrInt;
  reduced: TDocVariantData;
begin
  result.Init(VOptions); // same options than the main document
  if (VCount = 0) or
     (high(aPropNames) < 0) then
    exit;
  if IsObject then
    for j := 0 to high(aPropNames) do
    begin
      if aPropNames[j] = '' then
        continue; // avoid GPF in FindNonVoid()
      ndx := FindNonVoid[aCaseSensitive](
        pointer(VName), pointer(aPropNames[j]), length(aPropNames[j]), VCount);
      if ndx >= 0 then
        if not aDoNotAddVoidProp or
           not VarIsVoid(VValue[ndx]) then
          result.AddValue(VName[ndx], VValue[ndx]);
    end
  else if IsArray then
    for ndx := 0 to VCount - 1 do
    begin
      _Safe(VValue[ndx])^.Reduce(
        aPropNames, aCaseSensitive, reduced, aDoNotAddVoidProp);
      if not reduced.IsObject then
        continue;
      result.AddItem(variant(reduced));
      reduced.Clear;
    end;
end;

procedure TDocVariantData.ReduceFilter(const aKey: RawUtf8;
  const aValue: variant; aMatch: TCompareOperator; aCompare: TVariantCompare;
  aLimit: integer; var result: TDocVariantData);
var
  n, prev: integer;
  v, obj: PVariant;
  haspath: boolean;
  dv: PDocVariantData;
begin
  result.Init(VOptions, dvArray); // same options than the main document
  n := VCount;
  if (n = 0) or
     (aKey = '') or
     not IsArray then
    exit;
  if not Assigned(aCompare) then
    aCompare := @VariantCompare;
  prev := -1; // optimistic search aPropName at the previous field position
  haspath := PosExChar('.', aKey) <> 0;
  v := pointer(VValue);
  repeat
    dv := _Safe(v^);
    if haspath then
      obj := dv^.GetPVariantByPath(aKey, '.')
    else
      dv^.GetObjectProp(aKey, obj, @prev);
    if (obj <> nil) and
       SortMatch(aCompare({%H-}obj^, aValue), aMatch) then
    begin
      if result.VCount = 0 then
        SetLength(result.VValue, n); // prepare for maximum capacity
      result.AddItem(PVariant(dv)^);
    end;
    dec(aLimit);
    if aLimit = 0 then
      exit;
    inc(v);
    dec(n);
  until n = 0;
end;

procedure TDocVariantData.ReduceFilter(const aExpression: RawUtf8;
  var result: TDocVariantData; aLimit: integer; aCompare: TVariantCompare);
var
  k: RawUtf8;
  v: variant;
  m: TCompareOperator;
begin
  ParseSortMatch(pointer(aExpression), k, m, @v);
  ReduceFilter(k, v, m, aCompare, aLimit, result);
end;

procedure ToSingle(result: PRttiVarData);
var
  tmp: TDocVariantData;
begin
  PRttiVarData(@tmp)^ := result^; // main dvArray to be finalized at exit
  result^.VType := varEmpty;
  if tmp.VCount <> 0 then
    PVariant(result)^ := tmp.VValue[0]; // return the first (and unique) item
end;

function TDocVariantData.ReduceFilter(const aExpression: RawUtf8;
  aLimit: integer): variant;
begin
  VarClear(result{%H-});
  ReduceFilter(aExpression, PDocVariantData(@result)^, aLimit);
  if aLimit = 1 then
    ToSingle(@result);
end;

procedure TDocVariantData.ReduceFilter(const aExpression: RawUtf8;
  const aValue: variant; var result: TDocVariantData;
  aCompare: TVariantCompare; aLimit: integer);
var
  k: RawUtf8;
  m: TCompareOperator;
begin
  ParseSortMatch(pointer(aExpression), k, m, nil);
  ReduceFilter(k, aValue, m, aCompare, aLimit, result);
end;

function TDocVariantData.ReduceFilter(const aExpression: RawUtf8;
  const aValue: variant; aLimit: integer): variant;
begin
  VarClear(result{%H-});
  ReduceFilter(aExpression, aValue, PDocVariantData(@result)^);
  if aLimit = 1 then
    ToSingle(@result);
end;

function TDocVariantData.ReduceAsArray(const aPropName: RawUtf8;
  const OnReduce: TOnReducePerItem): variant;
begin
  VarClear(result{%H-});
  ReduceAsArray(aPropName, PDocVariantData(@result)^, OnReduce);
end;

procedure TDocVariantData.ReduceAsArray(const aPropName: RawUtf8;
  var result: TDocVariantData; const OnReduce: TOnReducePerItem);
var
  ndx: PtrInt;
  prev: integer;
  item: PDocVariantData;
  v: PVariant;
begin
  result.Init(VOptions, dvArray); // same options than the main document
  if (VCount = 0) or
     (aPropName = '') or
     not IsArray then
    exit;
  prev := -1; // optimistic search aPropName at the previous field position
  for ndx := 0 to VCount - 1 do
    if _Safe(VValue[ndx], item) and
       {%H-}item^.GetObjectProp(aPropName, v, @prev) then
      if (not Assigned(OnReduce)) or
         OnReduce(item) then
        result.AddItem(v^);
end;

function TDocVariantData.ReduceAsArray(const aPropName: RawUtf8;
  const OnReduce: TOnReducePerValue): variant;
begin
  VarClear(result{%H-});
  ReduceAsArray(aPropName, PDocVariantData(@result)^, OnReduce);
end;

procedure TDocVariantData.ReduceAsArray(const aPropName: RawUtf8;
  var result: TDocVariantData; const OnReduce: TOnReducePerValue);
var
  ndx: PtrInt;
  prev: integer;
  v: PVariant;
begin
  result.Init(VOptions, dvArray); // same options than the main document
  if (VCount = 0) or
     (aPropName = '') or
     not IsArray then
    exit;
  prev := -1; // optimistic search aPropName at the previous field position
  for ndx := 0 to VCount - 1 do
    if _Safe(VValue[ndx])^.GetObjectProp(aPropName, v, @prev) then
      if (not Assigned(OnReduce)) or
         OnReduce(v^) then
        result.AddItem(v^);
end;

function NotIn(a, v: PVarData; n: integer; caseins: boolean): boolean;
begin
  result := false;
  if n <> 0 then
    repeat
      if FastVarDataComp(a, v, caseins) = 0 then
        exit;
      inc(a);
      dec(n);
    until n = 0;
  result := true;
end;

function TDocVariantData.ReduceAsVariantArray(const aPropName: RawUtf8;
  aDuplicates: TSearchDuplicate): TVariantDynArray;
var
  n, ndx: PtrInt;
  prev: integer;
  v: PVariant;
begin
  result := nil;
  if (VCount = 0) or
     (aPropName = '') or
     not IsArray then
    exit;
  prev := -1; // optimistic search aPropName at the previous field position
  n := 0;
  for ndx := 0 to VCount - 1 do
    if _Safe(VValue[ndx])^.GetObjectProp(aPropName, v, @prev) then
      if (aDuplicates = sdNone) or
         NotIn(pointer(result), pointer(v), n, aDuplicates = sdCaseInsensitive) then
      begin
        if length(result) = n then
          SetLength(result, NextGrow(n));
        SetVariantByValue(PVariant(v)^, result[n]);
        inc(n);
      end;
  if n <> 0 then
    DynArrayFakeLength(result, n);
end;

function TDocVariantData.Rename(
  const aFromPropName, aToPropName: TRawUtf8DynArray): integer;
var
  n, prop, ndx: PtrInt;
begin
  result := 0;
  n := length(aFromPropName);
  if length(aToPropName) = n then
    for prop := 0 to n - 1 do
    begin
      ndx := GetValueIndex(aFromPropName[prop]);
      if ndx >= 0 then
      begin
        VName[ndx] := aToPropName[prop];
        inc(result);
      end;
    end;
end;

function TDocVariantData.GetNames: TRawUtf8DynArray;
begin
  if IsObject and
     (VCount > 0) then
  begin
    DynArrayFakeLength(VName, VCount);
    DynArrayFakeLength(VValue, VCount);
    result := VName; // truncate with no memory (re)allocation
  end
  else
    result := nil;
end;

function TDocVariantData.FlattenAsNestedObject(
  const aObjectPropName: RawUtf8): boolean;
var
  ndx, len: PtrInt;
  Up: array[byte] of AnsiChar;
  nested: TDocVariantData;
begin
  // {"p.a1":5,"p.a2":"dfasdfa"} -> {"p":{"a1":5,"a2":"dfasdfa"}}
  result := false;
  if (VCount = 0) or
     (aObjectPropName = '') or
     (not IsObject) then
    exit;
  PWord(UpperCopy255(Up{%H-}, aObjectPropName))^ := ord('.'); // e.g. 'P.'
  for ndx := 0 to Count - 1 do
    if not IdemPChar(pointer(VName[ndx]), Up) then
      exit; // all fields should match "p.####"
  len := length(aObjectPropName) + 1;
  for ndx := 0 to Count - 1 do
    system.delete(VName[ndx], 1, len);
  nested := self;
  ClearFast;
  InitObject([aObjectPropName, variant(nested)]);
  result := true;
end;

function TDocVariantData.Delete(Index: PtrInt): boolean;
var
  n: PtrInt;
  v: PVariantArray;
  k: PRawUtf8Array;
begin
  result := cardinal(Index) < cardinal(VCount);
  if not result then
    exit;
  dec(VCount);
  if VCount = 0 then
  begin
    Void; // reset all in-memory storage and capacity
    exit;
  end;
  k := pointer(VName);
  if k <> nil then
  begin
    EnsureUnique(VName);
    k := @VName[Index];
    FastAssignNew(k[0]);
  end;
  EnsureUnique(VValue);
  v := @VValue[Index];
  VarClear(v[0]);
  n := VCount - Index;
  if n = 0 then
    exit;
  if k <> nil then
  begin
    MoveFast(k[1], k[0], n * SizeOf(pointer));
    pointer(k[n]) := nil; // avoid GPF
  end;
  MoveFast(v[1], v[0], n * SizeOf(variant));
  TRttiVarData(v[n]).VType := varEmpty; // avoid GPF
end;

function TDocVariantData.Extract(aIndex: integer; var aValue: variant;
  aName: PRawUtf8): boolean;
var
  v: pointer;
begin
  result := false;
  if aIndex < 0 then
    inc(aIndex, VCount);
  if cardinal(aIndex) >= cardinal(VCount) then
    exit;
  EnsureUnique(VValue);
  VarClear(aValue);
  v := @VValue[aIndex];
  PRttiVarData(@aValue)^ := PRttiVarData(v)^; // no refcount
  PRttiVarData(v)^.VType := varEmpty;         // no VarClear(v^)
  if aName <> nil then
    if VName = nil then
      FastAssignNew(aName^)
    else
    begin
      EnsureUnique(VName);
      v := @VName[aIndex];
      FastAssignNew(aName^, PPointer(v)^); // no refcount
      PPointer(v)^ := nil;
    end;
  result := Delete(aIndex);
end;

function TDocVariantData.Delete(const aName: RawUtf8; aValue: PVariant): boolean;
var
  ndx: PtrInt;
begin
  result := false;
  ndx := GetValueIndex(aName);
  if ndx >= 0 then
    if aValue <> nil then
      result := Extract(ndx, aValue^)
    else
      result := Delete(ndx);
end;

function TDocVariantData.Delete(const aNames: array of RawUtf8): integer;
var
  n: PtrInt;
begin
  result := 0;
  for n := 0 to high(aNames) do
    inc(result, ord(Delete(aNames[n])));
end;

function TDocVariantData.InternalNextPath(
  var aCsv: PUtf8Char; aName: PShortString; aPathDelim: AnsiChar): PtrInt;
begin
  GetNextItemShortString(aCsv, aName, aPathDelim);
  if (VCount <> 0) and
     (aName^[0] <> #0) then
    if VName <> nil then
    begin
      result := FindNonVoid[IsCaseSensitive](
        pointer(VName), @aName^[1], ord(aName^[0]), VCount);
      exit;
    end
    else
    begin
      result := GetCardinalDef(@aName^[1], PtrUInt(-1));
      if PtrUInt(result) < PtrUInt(VCount) then // array index integer as text
        exit;
    end;
  result := -1;
end;

procedure TDocVariantData.InternalNotFound(var Dest: variant; aName: PUtf8Char);
begin
  if Has(dvoReturnNullForUnknownProperty) then
    SetVariantNull(Dest)
  else
    raise EDocVariant.CreateUtf8('[%] property not found', [aName])
end;

procedure TDocVariantData.InternalNotFound(var Dest: variant; aIndex: integer);
begin
  if Has(dvoReturnNullForUnknownProperty) then
    SetVariantNull(Dest)
  else
    raise EDocVariant.CreateUtf8('Out of range [%] (count=%)', [aIndex, VCount]);
end;

function TDocVariantData.InternalNotFound(aName: PUtf8Char): PVariant;
begin
  if Has(dvoReturnNullForUnknownProperty) then
    result := @DocVariantDataFake
  else
    raise EDocVariant.CreateUtf8('[%] property not found', [aName])
end;

function TDocVariantData.InternalNotFound(aIndex: integer): PDocVariantData;
begin
  if Has(dvoReturnNullForUnknownProperty) then
    result := @DocVariantDataFake
  else
    raise EDocVariant.CreateUtf8('Out of range [%] (count=%)', [aIndex, VCount]);
end;

function TDocVariantData.DeleteByPath(const aPath: RawUtf8;
  aPathDelim: AnsiChar; aDeletedValue: PVariant): boolean;
var
  csv: PUtf8Char;
  dv: PDocVariantData;
  ndx: PtrInt;
  n: ShortString;
begin
  result := false;
  if IsArray then
    exit;
  csv := pointer(aPath);
  dv := @self;
  repeat
    ndx := dv^.InternalNextPath(csv, @n, aPathDelim);
    if csv = nil then
    begin
      // we reached the last item of the path, which is to be deleted
      if aDeletedValue <> nil then
        aDeletedValue^ := dv^.VValue[ndx];
      result := dv^.Delete(ndx);
      exit;
    end;
  until (ndx < 0) or
       not _SafeObject(dv^.VValue[ndx], dv);
end;

function TDocVariantData.DeleteByProp(const aPropName, aPropValue: RawUtf8;
  aPropValueCaseSensitive: boolean): boolean;
begin
  result := Delete(SearchItemByProp(aPropName, aPropValue, aPropValueCaseSensitive));
end;

function TDocVariantData.DeleteByValue(const aValue: Variant;
  CaseInsensitive: boolean): integer;
var
  ndx: PtrInt;
begin
  result := 0;
  if VarIsEmptyOrNull(aValue) then
  begin
    for ndx := VCount - 1 downto 0 do
      if VarDataIsEmptyOrNull(@VValue[ndx]) then
      begin
        Delete(ndx);
        inc(result);
      end;
  end
  else
    for ndx := VCount - 1 downto 0 do
      if FastVarDataComp(@VValue[ndx], @aValue, CaseInsensitive) = 0 then
      begin
        Delete(ndx);
        inc(result);
      end;
end;

function TDocVariantData.DeleteByStartName(
  aStartName: PUtf8Char; aStartNameLen: integer): integer;
var
  ndx: PtrInt;
  upname: array[byte] of AnsiChar;
begin
  result := 0;
  if aStartNameLen = 0 then
    aStartNameLen := StrLen(aStartName);
  if (VCount = 0) or
     (not IsObject) or
     (aStartNameLen = 0) then
    exit;
  UpperCopy255Buf(upname{%H-}, aStartName, aStartNameLen)^ := #0;
  for ndx := Count - 1 downto 0 do
    if IdemPChar(pointer(names[ndx]), upname) then
    begin
      Delete(ndx);
      inc(result);
    end;
end;

function TDocVariantData.IsVoid: boolean;
begin
  result := (cardinal(VType) <> DocVariantVType) or
            (VCount = 0);
end;

function TDocVariantData.Exists(const aName: RawUtf8): boolean;
begin
  result := GetValueIndex(Pointer(aName), Length(aName), IsCaseSensitive) >= 0;
end;

function TDocVariantData.GetValueIndex(aName: PUtf8Char; aNameLen: PtrInt;
  aCaseSensitive: boolean): integer;
var
  err: integer;
begin
  if (cardinal(VType) = DocVariantVType) and
     (aNameLen > 0) and
     (aName <> nil) and
     (VCount > 0) then
    if IsArray then
    begin
      // try index integer as text, for lookup in array document
      result := GetInteger(aName, err);
      if (err <> 0) or
         (cardinal(result) >= cardinal(VCount)) then
        result := -1;
    end
    else
      // O(n) lookup for name -> efficient brute force sub-functions
      result := FindNonVoid[IsCaseSensitive](
        pointer(VName), aName, aNameLen, VCount)
  else
    result := -1;
end;

function TDocVariantData.GetValueOrRaiseException(
  const aName: RawUtf8): variant;
begin
  RetrieveValueOrRaiseException(
    pointer(aName), length(aName), IsCaseSensitive, result, false);
end;

function TDocVariantData.GetValueOrDefault(const aName: RawUtf8;
  const aDefault: variant): variant;
var
  v: PVariant;
begin
  if (cardinal(VType) <> DocVariantVType) or
     not GetObjectProp(aName, v{%H-}, nil) then
    result := aDefault
  else
    SetVariantByValue(v^, result);
end;

function TDocVariantData.GetValueOrNull(const aName: RawUtf8): variant;
var
  v: PVariant;
begin
  if (cardinal(VType) <> DocVariantVType) or
     not GetObjectProp(aName, v{%H-}, nil) then
    SetVariantNull(result{%H-})
  else
    SetVariantByValue(v^, result);
end;

function TDocVariantData.GetValueOrEmpty(const aName: RawUtf8): variant;
var
  v: PVariant;
begin
  if (cardinal(VType) <> DocVariantVType) or
     not GetObjectProp(aName, v{%H-}, nil) then
   VarClear(result{%H-})
  else
    SetVariantByValue(v^, result);
end;

function TDocVariantData.GetAsBoolean(const aName: RawUtf8; out aValue: boolean;
  aSortedCompare: TUtf8Compare): boolean;
var
  found: PVarData;
begin
  found := GetVarData(aName, aSortedCompare);
  if found = nil then
    result := false
  else
    result := VariantToBoolean(PVariant(found)^, aValue)
end;

function TDocVariantData.GetAsInteger(const aName: RawUtf8; out aValue: integer;
  aSortedCompare: TUtf8Compare): boolean;
var
  found: PVarData;
begin
  found := GetVarData(aName, aSortedCompare);
  if found = nil then
    result := false
  else
    result := VariantToInteger(PVariant(found)^, aValue);
end;

function TDocVariantData.GetAsInt64(const aName: RawUtf8; out aValue: Int64;
  aSortedCompare: TUtf8Compare): boolean;
var
  found: PVarData;
begin
  found := GetVarData(aName, aSortedCompare);
  if found = nil then
    result := false
  else
    result := VariantToInt64(PVariant(found)^, aValue)
end;

function TDocVariantData.GetAsDouble(const aName: RawUtf8; out aValue: double;
  aSortedCompare: TUtf8Compare): boolean;
var
  found: PVarData;
begin
  found := GetVarData(aName, aSortedCompare);
  if found = nil then
    result := false
  else
    result := VariantToDouble(PVariant(found)^, aValue);
end;

function TDocVariantData.GetAsRawUtf8(const aName: RawUtf8; out aValue: RawUtf8;
  aSortedCompare: TUtf8Compare): boolean;
var
  found: PVarData;
  wasString: boolean;
begin
  found := GetVarData(aName, aSortedCompare);
  if found = nil then
    result := false
  else
  begin
    if cardinal(found^.VType) > varNull then
      // avoid default VariantToUtf8(null)='null'
      VariantToUtf8(PVariant(found)^, aValue, wasString);
    result := true;
  end;
end;

function TDocVariantData.GetValueEnumerate(const aName: RawUtf8;
  aTypeInfo: PRttiInfo; out aValue; aDeleteFoundEntry: boolean): boolean;
var
  text: RawUtf8;
  ndx, ord: integer;
begin
  result := false;
  ndx := GetValueIndex(aName);
  if (ndx < 0) or
     not VariantToText(Values[ndx], text) then
    exit;
  ord := GetEnumNameValue(aTypeInfo, text, true);
  if ord < 0 then
    exit;
  byte(aValue) := ord;
  if aDeleteFoundEntry then
    Delete(ndx);
  result := true;
end;

function TDocVariantData.GetAsDocVariant(const aName: RawUtf8;
  out aValue: PDocVariantData; aSortedCompare: TUtf8Compare): boolean;
var
  found: PVarData;
begin
  found := GetVarData(aName, aSortedCompare);
  result := (found <> nil) and
            _Safe(PVariant(found)^, aValue);
end;

function TDocVariantData.GetAsArray(const aName: RawUtf8;
  out aArray: PDocVariantData; aSortedCompare: TUtf8Compare): boolean;
begin
  result := GetAsDocVariant(aName, aArray, aSortedCompare) and
            aArray^.IsArray and
            (aArray^.Count > 0);
end;

function TDocVariantData.GetAsObject(const aName: RawUtf8;
  out aObject: PDocVariantData; aSortedCompare: TUtf8Compare): boolean;
begin
  result := GetAsDocVariant(aName, aObject, aSortedCompare) and
            aObject^.IsObject and
            (aObject^.Count > 0);
end;

function TDocVariantData.GetAsDocVariantSafe(const aName: RawUtf8;
  aSortedCompare: TUtf8Compare): PDocVariantData;
var
  found: PVarData;
begin
  found := GetVarData(aName, aSortedCompare);
  if found = nil then
    result := @DocVariantDataFake
  else
    result := _Safe(PVariant(found)^);
end;

function TDocVariantData.GetAsPVariant(const aName: RawUtf8;
  out aValue: PVariant; aSortedCompare: TUtf8Compare): boolean;
begin
  aValue := pointer(GetVarData(aName, aSortedCompare));
  result := aValue <> nil;
end;

function TDocVariantData.GetAsPVariant(
  aName: PUtf8Char; aNameLen: PtrInt): PVariant;
var
  ndx: PtrInt;
begin
  ndx := GetValueIndex(aName, aNameLen, IsCaseSensitive);
  if ndx >= 0 then
    result := @VValue[ndx]
  else
    result := nil;
end;

function TDocVariantData.GetVarData(const aName: RawUtf8;
  aSortedCompare: TUtf8Compare; aFoundIndex: PInteger): PVarData;
var
  ndx: PtrInt;
begin
  if (cardinal(VType) <> DocVariantVType) or
     (not IsObject) or
     (VCount = 0) or
     (aName = '') then
  begin
    result := nil;
    if aFoundIndex <> nil then
      aFoundIndex^ := -1;
  end
  else
  begin
    if Assigned(aSortedCompare) then
      if @aSortedCompare = @StrComp then
        // use dedicated (branchless x86_64 asm) function for StrComp()
        ndx := FastFindPUtf8CharSorted(pointer(VName), VCount - 1, pointer(aName))
      else
        ndx := FastFindPUtf8CharSorted(
          pointer(VName), VCount - 1, pointer(aName), aSortedCompare)
    else
      ndx := FindNonVoid[IsCaseSensitive](
        pointer(VName), pointer(aName), length(aName), VCount);
    if aFoundIndex <> nil then
      aFoundIndex^ := ndx;
    if ndx >= 0 then
      result := @VValue[ndx]
    else
      result := nil;
  end;
end;

function TDocVariantData.GetVarData(const aName: RawUtf8; var aValue: TVarData;
  aSortedCompare: TUtf8Compare): boolean;
var
  found: PVarData;
begin
  found := GetVarData(aName, aSortedCompare);
  if found = nil then
    result := false
  else
  begin
    aValue := found^;
    result := true;
  end;
end;

function TDocVariantData.GetValueByPath(
  const aPath: RawUtf8; aPathDelim: AnsiChar): variant;
var
  Dest: TVarData;
begin
  VarClear(result{%H-});
  if (cardinal(VType) <> DocVariantVType) or
     (VCount = 0) then
    exit;
  DocVariantType.Lookup(Dest, TVarData(self), pointer(aPath), aPathDelim);
  if cardinal(Dest.VType) >= varNull then
    result := variant(Dest); // copy
end;

function TDocVariantData.GetValueByPath(const aPath: RawUtf8;
  out aValue: variant; aPathDelim: AnsiChar): boolean;
var
  Dest: TVarData;
begin
  result := false;
  if (cardinal(VType) <> DocVariantVType) or
     (VCount = 0) then
    exit;
  DocVariantType.Lookup(Dest, TVarData(self), pointer(aPath), aPathDelim);
  if Dest.VType = varEmpty then
    exit;
  aValue := variant(Dest); // copy
  result := true;
end;

function TDocVariantData.GetPVariantByPath(
  const aPath: RawUtf8; aPathDelim: AnsiChar): PVariant;
var
  ndx: PtrInt;
  vt: cardinal;
  csv: PUtf8Char;
  n: ShortString;
begin
  result := @self;
  csv := pointer(aPath);
  if aPath <> '' then
    repeat
      repeat
        vt := PVarData(result)^.VType; // inlined dv := _Safe(result^)
        if vt <> varVariantByRef then
          break;
        result := PVarData(result)^.VPointer;
      until false;
      if vt <> DocVariantVType then
        break;
      ndx := PDocVariantData(result)^.InternalNextPath(csv, @n, aPathDelim);
      if ndx < 0 then
        break;
      result := @PDocVariantData(result)^.VValue[ndx];
      if csv = nil then
        exit; // exhausted all path, so result is the found item
    until false;
  result := nil;
end;

function TDocVariantData.GetPVariantExistingByPath(const aPath: RawUtf8;
  aPathDelim: AnsiChar): PVariant;
begin
  result := GetPVariantByPath(aPath, aPathDelim);
  if result = nil then
    result := InternalNotFound(pointer(aPath));
end;

function TDocVariantData.GetVariantByPath(const aNameOrPath: RawUtf8): Variant;
var
  v: PVariant;
begin
  v := GetPVariantByPath(aNameOrPath, '.');
  if v <> nil then
    SetVariantByValue(v^, result)
  else
    InternalNotFound(result, pointer(aNameOrPath));
end;

function TDocVariantData.GetDocVariantByPath(const aPath: RawUtf8;
  out aValue: PDocVariantData; aPathDelim: AnsiChar): boolean;
var
  v: PVariant;
begin
  v := GetPVariantByPath(aPath, aPathDelim);
  result := (v <> nil) and
            _Safe(v^, aValue);
end;

function TDocVariantData.GetValueByPath(
  const aDocVariantPath: array of RawUtf8): variant;
var
  found, res: PVarData;
  vt: cardinal;
  ndx: integer;
begin
  VarClear(result{%H-});
  if (cardinal(VType) <> DocVariantVType) or
     (not IsObject) or
     (high(aDocVariantPath) < 0) then
    exit;
  found := @self;
  ndx := 0;
  repeat
    found := PDocVariantData(found).GetVarData(aDocVariantPath[ndx]);
    if found = nil then
      exit;
    if ndx = high(aDocVariantPath) then
      break; // we found the item!
    inc(ndx);
    // if we reached here, we should try for the next scope within Dest
    repeat
      vt := found^.VType;
      if vt <> varVariantByRef then
        break;
      found := found^.VPointer;
    until false;
    if vt = VType then
      continue;
    exit;
  until false;
  res := found;
  while cardinal(res^.VType) = varVariantByRef do
    res := res^.VPointer;
  if (cardinal(res^.VType) = VType) and
     (PDocVariantData(res)^.VCount = 0) then
    // return void TDocVariant as null
    TRttiVarData(result).VType := varNull
  else
    // copy found value
    result := PVariant(found)^;
end;

function TDocVariantData.GetItemByProp(const aPropName, aPropValue: RawUtf8;
  aPropValueCaseSensitive: boolean; var Dest: variant; DestByRef: boolean): boolean;
var
  ndx: integer;
begin
  result := false;
  if not IsArray then
    exit;
  ndx := SearchItemByProp(aPropName, aPropValue, aPropValueCaseSensitive);
  if ndx < 0 then
    exit;
  RetrieveValueOrRaiseException(ndx, Dest, DestByRef);
  result := true;
end;

function TDocVariantData.GetDocVariantByProp(
  const aPropName, aPropValue: RawUtf8; aPropValueCaseSensitive: boolean;
  out Dest: PDocVariantData): boolean;
var
  ndx: PtrInt;
begin
  result := false;
  if not IsArray then
    exit;
  ndx := SearchItemByProp(aPropName, aPropValue, aPropValueCaseSensitive);
  if ndx >= 0 then
    result := _Safe(VValue[ndx], Dest);
end;

function TDocVariantData.GetJsonByStartName(const aStartName: RawUtf8): RawUtf8;
var
  Up: array[byte] of AnsiChar;
  temp: TTextWriterStackBuffer;
  n: integer;
  nam: PPUtf8Char;
  val: PVariant;
  W: TJsonWriter;
begin
  if (not IsObject) or
     (VCount = 0) then
  begin
    result := NULL_STR_VAR;
    exit;
  end;
  UpperCopy255(Up, aStartName)^ := #0;
  W := TJsonWriter.CreateOwnedStream(temp);
  try
    W.Add('{');
    n := VCount;
    nam := pointer(VName);
    val := pointer(VValue);
    repeat
      if IdemPChar(nam^, Up) then
      begin
        if Has(dvoSerializeAsExtendedJson) and
           JsonPropNameValid(nam^) then
          W.AddShort(nam^, PStrLen(nam^ - _STRLEN)^)
        else
        begin
          W.AddDirect('"');
          W.AddJsonEscape(nam^);
          W.AddDirect('"');
        end;
        W.AddDirect(':');
        W.AddVariant(val^, twJsonEscape);
        W.AddComma;
      end;
      dec(n);
      if n = 0 then
        break;
      inc(nam);
      inc(val);
    until false;
    W.CancelLastComma('}');
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TDocVariantData.GetValuesByStartName(const aStartName: RawUtf8;
  TrimLeftStartName: boolean): variant;
var
  Up: array[byte] of AnsiChar;
  ndx: PtrInt;
  name: RawUtf8;
begin
  if aStartName = '' then
  begin
    result := Variant(self);
    exit;
  end;
  if (not IsObject) or
     (VCount = 0) then
  begin
    SetVariantNull(result{%H-});
    exit;
  end;
  TDocVariant.NewFast(result);
  UpperCopy255(Up{%H-}, aStartName)^ := #0;
  for ndx := 0 to VCount - 1 do
    if IdemPChar(Pointer(VName[ndx]), Up) then
    begin
      name := VName[ndx];
      if TrimLeftStartName then
        system.delete(name, 1, length(aStartName));
      TDocVariantData(result).AddValue(name, VValue[ndx]);
    end;
end;

procedure TDocVariantData.SetValueOrRaiseException(Index: integer;
  const NewValue: variant);
begin
  if cardinal(Index) >= cardinal(VCount) then
    raise EDocVariant.CreateUtf8(
      'Out of range Values[%] (count=%)', [Index, VCount])
  else
    VValue[Index] := NewValue;
end;

function TDocVariantData.SetValueByPath(const aPath: RawUtf8;
  const aValue: variant; aCreateIfNotExisting: boolean; aPathDelim: AnsiChar): boolean;
var
  csv: PUtf8Char;
  v: PDocVariantData;
  ndx: PtrInt;
  n: ShortString;
begin
  result := false;
  if IsArray then
    exit;
  csv := pointer(aPath);
  v := @self;
  repeat
    ndx := v^.InternalNextPath(csv, @n, aPathDelim);
    if csv = nil then
      break; // we reached the last item of the path, which is the value to set
    if ndx < 0 then
      if aCreateIfNotExisting then
      begin
        ndx := v^.InternalAddBuf(@n[1], ord(n[0])); // in two steps for FPC
        v := @v^.VValue[ndx];
        v^.InitClone(self); // same Options than root but with no Kind
      end
      else
        exit
    else if not _SafeObject(v^.VValue[ndx], v) then
      exit; // incorrect path
  until false;
  if ndx < 0 then
    ndx := v^.InternalAddBuf(@n[1], ord(n[0]));
  v^.InternalSetValue(ndx, aValue);
  result := true;
end;

procedure TDocVariantData.RetrieveNameOrRaiseException(
  Index: integer; var Dest: RawUtf8);
begin
  if (cardinal(Index) >= cardinal(VCount)) or
     (VName = nil) then
    if Has(dvoReturnNullForUnknownProperty) then
      Dest := ''
    else
      raise EDocVariant.CreateUtf8(
        'Out of range Names[%] (count=%)', [Index, VCount])
  else
    Dest := VName[Index];
end;

procedure TDocVariantData.RetrieveValueOrRaiseException(Index: integer;
  var Dest: variant; DestByRef: boolean);
var
  Source: PVariant;
begin
  if cardinal(Index) >= cardinal(VCount) then
    InternalNotFound(Dest, Index)
  else if DestByRef then
    SetVariantByRef(VValue[Index], Dest)
  else
  begin
    Source := @VValue[Index];
    while PVarData(Source)^.VType = varVariantByRef do
      Source := PVarData(Source)^.VPointer;
    Dest := Source^;
  end;
end;

function TDocVariantData.RetrieveValueOrRaiseException(
  aName: PUtf8Char; aNameLen: integer; aCaseSensitive: boolean;
  var Dest: variant; DestByRef: boolean): boolean;
var
  ndx: integer;
begin
  ndx := GetValueIndex(aName, aNameLen, aCaseSensitive);
  if ndx < 0 then
    InternalNotFound(Dest, aName)
  else
    RetrieveValueOrRaiseException(ndx, Dest, DestByRef);
  result := ndx >= 0;
end;

function TDocVariantData.GetValueOrItem(const aNameOrIndex: variant): variant;
var
  wasString: boolean;
  Name: RawUtf8;
begin
  if IsArray then
    // fast index lookup e.g. for Value[1]
    RetrieveValueOrRaiseException(
      VariantToIntegerDef(aNameOrIndex, -1), result, true)
  else
  begin
    // by name lookup e.g. for Value['abc']
    VariantToUtf8(aNameOrIndex, Name, wasString);
    if wasString then
      RetrieveValueOrRaiseException(
        pointer(Name), length(Name), IsCaseSensitive, result, true)
    else
      RetrieveValueOrRaiseException(
        GetIntegerDef(pointer(Name), -1), result, true);
  end;
end;

procedure TDocVariantData.SetValueOrItem(const aNameOrIndex, aValue: variant);
var
  wasString: boolean;
  ndx: integer;
  Name: RawUtf8;
begin
  if IsArray then
    // fast index lookup e.g. for Value[1]
    SetValueOrRaiseException(VariantToIntegerDef(aNameOrIndex, -1), aValue)
  else
  begin
    // by name lookup e.g. for Value['abc']
    VariantToUtf8(aNameOrIndex, Name, wasString);
    if wasString then
    begin
      ndx := GetValueIndex(Name);
      if ndx < 0 then
        ndx := InternalAdd(Name);
      InternalSetValue(ndx, aValue);
    end
    else
      SetValueOrRaiseException(
        VariantToIntegerDef(aNameOrIndex, -1), aValue);
  end;
end;

function TDocVariantData.AddOrUpdateValue(const aName: RawUtf8;
  const aValue: variant; wasAdded: PBoolean; OnlyAddMissing: boolean): integer;
begin
  if IsArray then
    raise EDocVariant.CreateUtf8(
      'AddOrUpdateValue("%") on an array', [aName]);
  result := GetValueIndex(aName);
  if result < 0 then
  begin
    result := InternalAdd(aName);
    if wasAdded <> nil then
      wasAdded^ := true;
  end
  else
  begin
    if wasAdded <> nil then
      wasAdded^ := false;
    if OnlyAddMissing then
      exit;
  end;
  InternalSetValue(result, aValue);
end;

function TDocVariantData.ToJson: RawUtf8;
begin // note: FPC has troubles inlining this, but it is a slow method anyway
  DocVariantType.ToJson(@self, result, '', '', jsonCompact);
end;

function TDocVariantData.ToJson(const Prefix, Suffix: RawUtf8;
  Format: TTextWriterJsonFormat): RawUtf8;
begin
  DocVariantType.ToJson(@self, result, Prefix, Suffix, Format);
end;

procedure TDocVariantData.SaveToJsonFile(const FileName: TFileName);
var
  F: TStream;
  W: TJsonWriter;
begin
  if cardinal(VType) <> DocVariantVType then
    exit;
  F := TFileStreamEx.Create(FileName, fmCreate);
  try
    W := TJsonWriter.Create(F, 65536);
    try
      DocVariantType.ToJson(W, @self);
      W.FlushFinal;
    finally
      W.Free;
    end;
  finally
    F.Free;
  end;
end;

function TDocVariantData.ToNonExpandedJson: RawUtf8;
var
  field: TRawUtf8DynArray;
  fieldCount, r, f: PtrInt;
  W: TJsonWriter;
  row: PDocVariantData;
  temp: TTextWriterStackBuffer;
begin
  if not IsArray then
  begin
    result := '';
    exit;
  end;
  if VCount = 0 then
  begin
    result := '[]';
    exit;
  end;
  fieldCount := 0;
  with _Safe(VValue[0])^ do
    if IsObject then
    begin
      field := VName;
      fieldCount := VCount;
    end;
  if fieldCount = 0 then
    raise EDocVariant.Create('ToNonExpandedJson: Value[0] is not an object');
  W := TJsonWriter.CreateOwnedStream(temp);
  try
    W.Add('{"fieldCount":%,"rowCount":%,"values":[', [fieldCount, VCount]);
    for f := 0 to fieldCount - 1 do
    begin
      W.Add('"');
      W.AddJsonEscape(pointer(field[f]));
      W.Add('"', ',');
    end;
    for r := 0 to VCount - 1 do
    begin
      row := _Safe(VValue[r]);
      if (r > 0) and
         ((not row^.IsObject) or
          (row^.VCount <> fieldCount)) then
        raise EDocVariant.CreateUtf8(
          'ToNonExpandedJson: Value[%] not expected object', [r]);
      for f := 0 to fieldCount - 1 do
        if (r > 0) and
           not PropNameEquals(row^.VName[f], field[f]) then
          raise EDocVariant.CreateUtf8(
            'ToNonExpandedJson: Value[%] field=% expected=%',
            [r, row^.VName[f], field[f]])
        else
        begin
          W.AddVariant(row^.VValue[f], twJsonEscape);
          W.AddComma;
        end;
    end;
    W.CancelLastComma;
    W.AddDirect(']', '}');
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure TDocVariantData.ToRawUtf8DynArray(out Result: TRawUtf8DynArray);
var
  ndx: PtrInt;
  wasString: boolean;
begin
  if IsObject then
    raise EDocVariant.Create('ToRawUtf8DynArray expects a dvArray');
  if IsArray then
  begin
    SetLength(Result, VCount);
    for ndx := 0 to VCount - 1 do
      VariantToUtf8(VValue[ndx], Result[ndx], wasString);
  end;
end;

function TDocVariantData.ToRawUtf8DynArray: TRawUtf8DynArray;
begin
  ToRawUtf8DynArray(result);
end;

function TDocVariantData.ToCsv(const Separator: RawUtf8): RawUtf8;
var
  tmp: TRawUtf8DynArray; // fast enough in practice
begin
  ToRawUtf8DynArray(tmp);
  result := RawUtf8ArrayToCsv(tmp, Separator);
end;

procedure TDocVariantData.ToTextPairsVar(out Result: RawUtf8;
  const NameValueSep, ItemSep: RawUtf8; escape: TTextWriterKind);
var
  ndx: PtrInt;
  temp: TTextWriterStackBuffer;
begin
  if IsArray then
    raise EDocVariant.Create('ToTextPairs expects a dvObject');
  if (VCount > 0) and
     IsObject then
    with TJsonWriter.CreateOwnedStream(temp) do
      try
        ndx := 0;
        repeat
          AddString(VName[ndx]);
          AddString(NameValueSep);
          AddVariant(VValue[ndx], escape);
          inc(ndx);
          if ndx = VCount then
            break;
          AddString(ItemSep);
        until false;
        SetText(Result);
      finally
        Free;
      end;
end;

function TDocVariantData.ToTextPairs(const NameValueSep: RawUtf8;
  const ItemSep: RawUtf8; Escape: TTextWriterKind): RawUtf8;
begin
  ToTextPairsVar(result, NameValueSep, ItemSep, Escape);
end;

procedure TDocVariantData.ToArrayOfConst(out Result: TTVarRecDynArray);
begin
  if IsObject then
    raise EDocVariant.Create('ToArrayOfConst expects a dvArray');
  if IsArray then
    VariantsToArrayOfConst(VValue, VCount, Result);
end;

function TDocVariantData.ToArrayOfConst: TTVarRecDynArray;
begin
  ToArrayOfConst(result);
end;

function TDocVariantData.ToUrlEncode(const UriRoot: RawUtf8): RawUtf8;
var
  json: RawUtf8; // temporary in-place modified buffer
begin
  DocVariantType.ToJson(@self, json);
  result := UrlEncodeJsonObject(UriRoot, Pointer(json), []);
end;

function TDocVariantData.GetOrAddIndexByName(const aName: RawUtf8): integer;
begin
  result := GetValueIndex(aName);
  if result < 0 then
    result := InternalAdd(aName);
end;

function TDocVariantData.GetOrAddPVariantByName(const aName: RawUtf8): PVariant;
var
  ndx: PtrInt;
begin
  ndx := GetOrAddIndexByName(aName); // in two steps for FPC
  result := @VValue[ndx];
end;

function TDocVariantData.GetPVariantByName(const aName: RawUtf8): PVariant;
var
  ndx: PtrInt;
begin
  ndx := GetValueIndex(aName);
  if ndx < 0 then
    result := InternalNotFound(pointer(aName))
  else
    result := @VValue[ndx];
end;

function TDocVariantData.GetInt64ByName(const aName: RawUtf8): Int64;
begin
  if not VariantToInt64(GetPVariantByName(aName)^, result) then
    result := 0;
end;

function TDocVariantData.GetRawUtf8ByName(const aName: RawUtf8): RawUtf8;
var
  wasString: boolean;
  v: PVariant;
begin
  v := GetPVariantByName(aName);
  if PVarData(v)^.VType <= varNull then // default VariantToUtf8(null)='null'
    result := ''
  else
    VariantToUtf8(v^, result, wasString);
end;

function TDocVariantData.GetStringByName(const aName: RawUtf8): string;
begin
  result := VariantToString(GetPVariantByName(aName)^);
end;

procedure TDocVariantData.SetInt64ByName(const aName: RawUtf8;
  const aValue: Int64);
begin
  GetOrAddPVariantByName(aName)^ := aValue;
end;

procedure TDocVariantData.SetRawUtf8ByName(const aName, aValue: RawUtf8);
begin
  RawUtf8ToVariant(aValue, GetOrAddPVariantByName(aName)^);
end;

procedure TDocVariantData.SetStringByName(const aName: RawUtf8;
  const aValue: string);
begin
  StringToVariant(aValue, GetOrAddPVariantByName(aName)^);
end;

function TDocVariantData.GetBooleanByName(const aName: RawUtf8): boolean;
begin
  if not VariantToBoolean(GetPVariantByName(aName)^, result) then
    result := false;
end;

procedure TDocVariantData.SetBooleanByName(const aName: RawUtf8;
  aValue: boolean);
begin
  GetOrAddPVariantByName(aName)^ := aValue;
end;

function TDocVariantData.GetDoubleByName(const aName: RawUtf8): Double;
begin
  if not VariantToDouble(GetPVariantByName(aName)^, result) then
    result := 0;
end;

procedure TDocVariantData.SetDoubleByName(const aName: RawUtf8;
  const aValue: Double);
begin
  GetOrAddPVariantByName(aName)^ := aValue;
end;

function TDocVariantData.GetDocVariantExistingByName(const aName: RawUtf8;
  aNotMatchingKind: TDocVariantKind): PDocVariantData;
begin
  result := GetAsDocVariantSafe(aName);
  if result^.GetKind = aNotMatchingKind then
    result := @DocVariantDataFake;
end;

function TDocVariantData.GetDocVariantOrAddByName(const aName: RawUtf8;
  aKind: TDocVariantKind): PDocVariantData;
var
  ndx: PtrInt;
begin
  ndx := GetOrAddIndexByName(aName);
  result := _Safe(VValue[ndx]);
  if result^.Kind <> aKind then
  begin
    result := @VValue[ndx];
    VarClear(PVariant(result)^);
    result^.Init(VOptions, aKind);
  end;
end;

function TDocVariantData.GetObjectExistingByName(
  const aName: RawUtf8): PDocVariantData;
begin
  result := GetDocVariantExistingByName(aName, dvArray);
end;

function TDocVariantData.GetObjectOrAddByName(
  const aName: RawUtf8): PDocVariantData;
begin
  result := GetDocVariantOrAddByName(aName, dvObject);
end;

function TDocVariantData.GetArrayExistingByName(
  const aName: RawUtf8): PDocVariantData;
begin
  result := GetDocVariantExistingByName(aName, dvObject);
end;

function TDocVariantData.GetArrayOrAddByName(
  const aName: RawUtf8): PDocVariantData;
begin
  result := GetDocVariantOrAddByName(aName, dvArray);
end;

function TDocVariantData.GetAsDocVariantByIndex(
  aIndex: integer): PDocVariantData;
begin
  if cardinal(aIndex) < cardinal(VCount) then
    result := _Safe(VValue[aIndex])
  else
    result := InternalNotFound(aIndex);
end;

function _Obj(const NameValuePairs: array of const;
  Options: TDocVariantOptions): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitObject(NameValuePairs, Options);
end;

function _Arr(const Items: array of const;
  Options: TDocVariantOptions): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitArray(Items, Options);
end;

procedure _ObjAddProp(const Name: RawUtf8; const Value: variant;
  var Obj: variant);
var
  o: PDocVariantData;
begin
  if _SafeObject(Obj, o) then
  begin
    // append new names/values to existing object
    if o <> @Obj then
      // ensure not stored by reference
      TVarData(Obj) := PVarData(o)^;
    o^.AddOrUpdateValue(Name, Value);
  end
  else
  begin
    // create new object
    VarClear(Obj);
    TDocVariantData(Obj).InitObject([Name, Value], JSON_FAST);
  end
end;

procedure _ObjAddProp(const Name: RawUtf8; const Value: TDocVariantData;
  var Obj: variant);
begin
  _ObjAddProp(Name, variant(Value), Obj);
end;

procedure _ObjAddPropU(const Name: RawUtf8; const Value: RawUtf8;
  var Obj: variant);
var
  v: variant;
begin
  RawUtf8ToVariant(Value, v);
  _ObjAddProp(Name, v, Obj);
end;

procedure _ObjAddProps(const NameValuePairs: array of const;
  var Obj: variant);
var
  o: PDocVariantData;
begin
  if _SafeObject(Obj, o) then
  begin
    // append new names/values to existing object
    if o <> @Obj then
      // ensure not stored by reference
      TVarData(Obj) := PVarData(o)^;
    o^.AddNameValuesToObject(NameValuePairs);
  end
  else
  begin
    // create new object
    VarClear(Obj);
    TDocVariantData(Obj).InitObject(NameValuePairs, JSON_FAST);
  end
end;

procedure _ObjAddProps(const Document: variant; var Obj: variant);
var
  ndx: PtrInt;
  d, o: PDocVariantData;
begin
  o := _Safe(Obj);
  if _SafeObject(Document, d) then
    if not o.IsObject then
      Obj := Document
    else
      for ndx := 0 to d^.VCount - 1 do
        o^.AddOrUpdateValue(d^.VName[ndx], d^.VValue[ndx]);
end;

function _ObjFast(const NameValuePairs: array of const): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitObject(NameValuePairs, JSON_FAST);
end;

function _ObjFast(aObject: TObject;
  aOptions: TTextWriterWriteObjectOptions): variant;
begin
  ObjectToVariant(aObject, result, aOptions);
end;

function _ArrFast(const Items: array of const): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitArray(Items, JSON_FAST);
end;

function _Json(const Json: RawUtf8; Options: TDocVariantOptions): variant;
begin
  _Json(Json, result, Options);
end;

function _JsonFast(const Json: RawUtf8): variant;
begin
  _Json(Json, result, JSON_FAST);
end;

function _JsonFastFloat(const Json: RawUtf8): variant;
begin
  _Json(Json, result, JSON_FAST_FLOAT);
end;

function _JsonFastExt(const Json: RawUtf8): variant;
begin
  _Json(Json, result, JSON_FAST_EXTENDED);
end;

function _JsonFmt(const Format: RawUtf8; const Args, Params: array of const;
  Options: TDocVariantOptions): variant;
begin
  _JsonFmt(Format, Args, Params, Options, result);
end;

procedure _JsonFmt(const Format: RawUtf8; const Args, Params: array of const;
  Options: TDocVariantOptions; out Result: variant);
var
  temp: RawUtf8;
begin
  FormatParams(Format, Args, Params, {json=}true, temp);
  if TDocVariantData(Result).InitJsonInPlace(pointer(temp), Options) = nil then
    TDocVariantData(Result).ClearFast;
end;

function _JsonFastFmt(const Format: RawUtf8;
  const Args, Params: array of const): variant;
begin
  _JsonFmt(Format, Args, Params, JSON_FAST, result);
end;

function _Json(const Json: RawUtf8; var Value: variant;
  Options: TDocVariantOptions): boolean;
begin
  VarClear(Value);
  if not TDocVariantData(Value).InitJson(Json, Options) then
  begin
    TDocVariantData(Value).ClearFast;
    result := false;
  end
  else
    result := true;
end;

procedure _Unique(var DocVariant: variant);
begin
  // TDocVariantData(DocVariant): InitCopy() will check the DocVariant type
  TDocVariantData(DocVariant).InitCopy(DocVariant, JSON_[mDefault]);
end;

procedure _UniqueFast(var DocVariant: variant);
begin
  // TDocVariantData(DocVariant): InitCopy() will check the DocVariant type
  TDocVariantData(DocVariant).InitCopy(DocVariant, JSON_[mFast]);
end;

function _Copy(const DocVariant: variant): variant;
begin
  result := TDocVariant.NewUnique(DocVariant, JSON_[mDefault]);
end;

function _CopyFast(const DocVariant: variant): variant;
begin
  result := TDocVariant.NewUnique(DocVariant, JSON_[mFast]);
end;

function _ByRef(const DocVariant: variant; Options: TDocVariantOptions): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result) := _Safe(DocVariant)^; // fast byref copy
  TDocVariantData(result).SetOptions(Options);
end;

procedure _ByRef(const DocVariant: variant; out Dest: variant;
  Options: TDocVariantOptions);
begin
  TDocVariantData(Dest) := _Safe(DocVariant)^; // fast byref copy
  TDocVariantData(Dest).SetOptions(Options);
end;


{ ************** JSON Parsing into Variant }

function GetVariantFromNotStringJson(Json: PUtf8Char; var Value: TVarData;
  AllowDouble: boolean): boolean;
begin
  if Json <> nil then
    Json := GotoNextNotSpace(Json);
  if (Json = nil) or
     ((PInteger(Json)^ = NULL_LOW) and
      (jcEndOfJsonValueField in JSON_CHARS[Json[4]])) then
    TRttiVarData(Value).VType := varNull
  else if (PInteger(Json)^ = FALSE_LOW) and
          (Json[4] = 'e') and
          (jcEndOfJsonValueField in JSON_CHARS[Json[5]]) then
  begin
    TRttiVarData(Value).VType := varBoolean;
    Value.VInteger := ord(false);
  end
  else if (PInteger(Json)^ = TRUE_LOW) and
          (jcEndOfJsonValueField in JSON_CHARS[Json[4]]) then
  begin
    TRttiVarData(Value).VType := varBoolean;
    Value.VInteger := ord(true);
  end
  else
  begin
    Json := GetNumericVariantFromJson(Json, Value, AllowDouble);
    if (Json = nil) or
       (GotoNextNotSpace(Json)^ <> #0) then
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

function GotoEndOfJsonNumber(P: PUtf8Char; var PEndNum: PUtf8Char): PUtf8Char;
  {$ifdef HASINLINE} inline; {$endif} // inlined for better code generation
var
  tab: PJsonCharSet;
begin
  result := P;
  tab := @JSON_CHARS;
  repeat
    inc(result);
  until not (jcDigitFloatChar in tab[result^]);
  PEndNum := result;
  while not (jcEndOfJsonFieldNotName in tab[result^]) do
    inc(result); // #0, ',', ']', '}'
end;

{$ifndef PUREMORMOT2}
procedure GetJsonToAnyVariant(var Value: variant; var Json: PUtf8Char;
  EndOfObject: PUtf8Char; Options: PDocVariantOptions; AllowDouble: boolean);
var
  info: TGetJsonField;
begin
  info.Json := Json;
  JsonToAnyVariant(Value, Info, Options, AllowDouble);
  if EndOfObject <> nil then
    EndOfObject^ := info.EndOfObject;
  Json := info.Json;
end;
{$endif PUREMORMOT2}

procedure JsonToAnyVariant(var Value: variant; var Info: TGetJsonField;
  Options: PDocVariantOptions; AllowDouble: boolean);
var
  V: TVarData absolute Value;
  n: integer;
  t: ^TSynInvokeableVariantType;
  J, J2: PUtf8Char;
  EndOfObject2: AnsiChar;
  wasParsedWithinString: boolean;
label
  parse, parsed, astext, endobj;
begin
  if PInteger(@V)^ <> 0 then
    VarClearProc(V);
  if Info.Json = nil then
    exit;
  Info.EndOfObject := ' ';
  if (Options <> nil) and
     (dvoAllowDoubleValue in Options^) then
    AllowDouble := true;
  wasParsedWithinString := false;
  J := Info.Json;
  while (J^ <= ' ') and
        (J^ <> #0) do
    inc(J);
  case JSON_TOKENS[J^] of
    jtFirstDigit:  // '-', '0'..'9': numbers are directly processed
      begin
        Info.Value := J;
        J := GetNumericVariantFromJson(J, V, AllowDouble);
        if J = nil then
        begin
          // not a supported number
          if AllowDouble then
          begin
            Info.Json := nil; // we expected the precision to be enough
            exit;
          end;
          // it may be a double value, but we didn't allow them -> store as text
          J := Info.Value;
          repeat
            inc(J); // #0, ',', ']', '}'
          until not (jcDigitFloatChar in JSON_CHARS[J^]);
          Info.ValueLen := J - Info.Value;
          J := GotoNextNotSpace(J);
          Info.EndOfObject := J^;
          if J^ <> #0 then
            inc(J);
          Info.Json := J;
          goto astext;
        end;
        // we parsed a full number as variant
endobj: Info.ValueLen := J - Info.Value;
        while (J^ <= ' ') and
              (J^ <> #0) do
          inc(J);
        Info.EndOfObject := J^;
        if J^ <> #0 then
          inc(J);
        Info.Json := J;
        exit;
      end;
    jtDoubleQuote:
      begin
        Info.Json := J;
        if (Options <> nil) and
           (dvoJsonObjectParseWithinString in Options^) then
        begin
          Info.GetJsonField;
          J := Info.Value;
          wasParsedWithinString := true;
        end
        else
        begin
          // parse string/numerical values (or true/false/null constants)
parse:    Info.GetJsonField;
parsed:   if Info.WasString or
             not GetVariantFromNotStringJson(Info.Value, V, AllowDouble) then
          begin
astext:     TRttiVarData(V).VType := varString;
            V.VAny := nil; // avoid GPF below
            FastSetString(RawUtf8(V.VAny), Info.Value, Info.Valuelen);
          end;
          exit;
        end;
      end;
    jtNullFirstChar:
      if (PInteger(J)^ = NULL_LOW) and
         (jcEndOfJsonValueField in JSON_CHARS[J[4]]) then
      begin
        Info.Value := J;
        TRttiVarData(V).VType := varNull;
        inc(J, 4);
        goto endobj;
      end;
    jtFalseFirstChar:
      if (PInteger(J + 1)^ = FALSE_LOW2) and
         (jcEndOfJsonValueField in JSON_CHARS[J[5]]) then
      begin
        Info.Value := J;
        TRttiVarData(V).VType := varBoolean;
        V.VInteger := ord(false);
        inc(J, 5);
        goto endobj;
      end;
    jtTrueFirstChar:
      if (PInteger(J)^ = TRUE_LOW) and
         (jcEndOfJsonValueField in JSON_CHARS[J[4]]) then
      begin
        Info.Value := J;
        TRttiVarData(V).VType := varBoolean;
        V.VInteger := ord(true);
        inc(J, 4);
        goto endobj;
      end;
  end;
  // if we reach here, input Json may be some complex value
  if Options = nil then
  begin
    Info.Json := nil;
    exit; // clearly invalid basic JSON
  end;
  if not (dvoJsonParseDoNotTryCustomVariants in Options^) then
  begin
    // first call TryJsonToVariant() overriden method for any complex content
    t := pointer(SynVariantTryJsonTypes);
    if t <> nil then
    begin
      n := PDALen(PAnsiChar(t) - _DALEN)^ + _DAOFF; // call all TryJsonToVariant()
      repeat
        J2 := J;
        // currently, only implemented by mormot.db.nosql.bson BsonVariantType
        if t^.TryJsonToVariant(J2, Value, @EndOfObject2) then
        begin
          if not wasParsedWithinString then
          begin
            Info.EndOfObject := EndOfObject2;
            Info.Json := J2;
          end;
          exit;
        end;
        dec(n);
        if n = 0 then
          break;
        inc(t);
      until false;
    end;
  end;
  if J^ in ['{', '['] then
  begin
    // default Json parsing and conversion to TDocVariant instance
    J := TDocVariantData(Value).InitJsonInPlace(J, Options^, @EndOfObject2);
    if J = nil then
    begin
      TDocVariantData(Value).ClearFast;
      Info.Json := nil;
      exit; // error parsing
    end;
    if not wasParsedWithinString then
    begin
      Info.EndOfObject := EndOfObject2;
      Info.Json := J;
    end;
  end
  else // back to simple variant types
    if wasParsedWithinString then
      goto parsed
    else
    begin
      Info.Json := J;
      goto parse;
    end;
end;

function TextToVariantNumberTypeNoDouble(Json: PUtf8Char): cardinal;
var
  start: PUtf8Char;
  c: AnsiChar;
begin
  result := varString;
  c := Json[0];
  if (jcDigitFirstChar in JSON_CHARS[c]) and // ['-', '0'..'9']
     (((c >= '1') and
       (c <= '9')) or      // is first char numeric?
     ((c = '0') and
      ((Json[1] = '.') or
       (Json[1] = #0))) or // '012' is not Json, but '0.xx' and '0' are
     ((c = '-') and
      (Json[1] >= '0') and
      (Json[1] <= '9'))) then  // negative number
  begin
    start := Json;
    repeat
      inc(Json)
    until (Json^ < '0') or
          (Json^ > '9'); // check digits
    case Json^ of
      #0:
        if Json - start <= 19 then
          // no decimal, and matcthing signed Int64 precision
          result := varInt64;
      '.':
        if (Json[1] >= '0') and
           (Json[1] <= '9') and
           (Json[2] in [#0, '0'..'9']) then
          if (Json[2] = #0) or
             (Json[3] = #0) or
             ((Json[3] >= '0') and
              (Json[3] <= '9') and
              (Json[4] = #0) or
             ((Json[4] >= '0') and
              (Json[4] <= '9') and
              (Json[5] = #0))) then
            result := varCurrency; // currency ###.1234 number
    end;
  end;
end;

function TextToVariantNumberType(Json: PUtf8Char): cardinal;
var
  start: PUtf8Char;
  exp: PtrInt;
  c: AnsiChar;
label
  exponent;
begin
  result := varString;
  c := Json[0];
  if (jcDigitFirstChar in JSON_CHARS[c]) and // ['-', '0'..'9']
     (((c >= '1') and
       (c <= '9')) or      // is first char numeric?
     ((c = '0') and
      ((Json[1] = '.') or
       (Json[1] = #0))) or // '012' is not Json, but '0.xx' and '0' are
     ((c = '-') and
      (Json[1] >= '0') and
      (Json[1] <= '9'))) then  // negative number
  begin
    start := Json;
    repeat
      inc(Json)
    until (Json^ < '0') or
          (Json^ > '9'); // check digits
    case Json^ of
      #0:
        if Json - start <= 19 then // signed Int64 precision
          result := varInt64
        else
          result := varDouble; // we may loose precision, but still a number
      '.':
        if (Json[1] >= '0') and
           (Json[1] <= '9') and
           (Json[2] in [#0, '0'..'9']) then
          if (Json[2] = #0) or
             (Json[3] = #0) or
             ((Json[3] >= '0') and
              (Json[3] <= '9') and
              (Json[4] = #0) or
             ((Json[4] >= '0') and
              (Json[4] <= '9') and
              (Json[5] = #0))) then
            result := varCurrency // currency ###.1234 number
          else
          begin
            repeat // more than 4 decimals
              inc(Json)
            until (Json^ < '0') or
                  (Json^ > '9');
            case Json^ of
              #0:
                result := varDouble;
              'e',
              'E':
                begin
exponent:         inc(Json); // inlined custom GetInteger()
                  start := Json;
                  c := Json^;
                  if (c = '-') or
                     (c = '+') then
                  begin
                    inc(Json);
                    c := Json^;
                  end;
                  inc(Json);
                  dec(c, 48);
                  if c > #9 then
                    exit;
                  exp := ord(c);
                  c := Json^;
                  dec(c, 48);
                  if c <= #9 then
                  begin
                    inc(Json);
                    exp := exp * 10 + ord(c);
                    c := Json^;
                    dec(c, 48);
                    if c <= #9 then
                    begin
                      inc(Json);
                      exp := exp * 10 + ord(c);
                    end;
                  end;
                  if Json^ <> #0 then
                    exit;
                  if start^ = '-' then
                    exp := -exp;
                  if (exp > -324) and
                     (exp < 308) then
                    result := varDouble; // 5.0 x 10^-324 .. 1.7 x 10^308
                end;
            end;
          end;
      'e',
      'E':
        goto exponent;
    end;
  end;
end;

const
  CURRENCY_FACTOR: array[-4 .. -1] of integer = (1, 10, 100, 1000);

function GetNumericVariantFromJson(Json: PUtf8Char; var Value: TVarData;
  AllowVarDouble: boolean): PUtf8Char;
var
  // logic below is extracted from mormot.core.base.pas' GetExtended()
  remdigit: integer;
  frac, exp: PtrInt;
  c: AnsiChar;
  flags: set of (fNeg, fNegExp, fValid);
  v64: Int64; // allows 64-bit resolution for the digits (match 80-bit extended)
  d: double;
begin
  // 1. parse input text as number into v64, frac, digit, exp
  result := nil; // return nil to indicate parsing error
  byte(flags) := 0;
  v64 := 0;
  frac := 0;
  if Json = nil then
    exit;
  c := Json^;
  if c = '-' then // note: '+xxx' is not valid Json so is not handled here
  begin
    c := Json[1];
    inc(Json);
    include(flags, fNeg);
  end;
  if (c = '0') and
     (Json[1] >= '0') and
     (Json[1] <= '9') then // '012' is not Json, but '0.xx' and '0' are
    exit;
  remdigit := 19;    // max Int64 resolution
  repeat
    if (c >= '0') and
       (c <= '9') then
    begin
      inc(Json);
      dec(remdigit); // over-required digits are just ignored
      if remdigit >= 0 then
      begin
        dec(c, ord('0'));
        {$ifdef CPU64}
        v64 := v64 * 10;
        {$else}
        v64 := v64 shl 3 + v64 + v64;
        {$endif CPU64}
        inc(v64, byte(c));
        c := Json^;
        include(flags, fValid);
        if frac <> 0 then
          dec(frac); // frac<0 for digits after '.'
        continue;
      end;
      c := Json^;
      if frac >= 0 then
        inc(frac);   // frac>0 to handle #############00000
      continue;
    end;
    if c <> '.' then
      break;
    c := Json[1];
    if (frac > 0) or
       (c = #0) then // avoid ##.
      exit;
    inc(json);
    dec(frac);
  until false;
  if frac < 0 then
    inc(frac);       // adjust digits after '.'
  if (c = 'E') or
     (c = 'e') then
  begin
    c := Json[1];
    inc(Json);
    exp := 0;
    exclude(flags, fValid);
    if c = '+' then
      inc(Json)
    else if c = '-' then
    begin
      inc(Json);
      include(flags, fNegExp);
    end;
    repeat
      c := Json^;
      if (c < '0') or
         (c > '9') then
        break;
      inc(Json);
      dec(c, ord('0'));
      exp := (exp * 10) + byte(c);
      include(flags, fValid);
    until false;
    if fNegExp in flags then
      dec(frac, exp)
    else
      inc(frac, exp);
  end;
  if not (fValid in flags) then
    exit;
  if fNeg in flags then
    v64 := -v64;
  // 2. now v64, frac, digit, exp contain number parsed from Json
  if (frac = 0) and
     (remdigit >= 0) then
  begin
    // return an integer or Int64 value
    Value.VInt64 := v64;
    if remdigit <= 9 then
      TRttiVarData(Value).VType := varInt64
    else
      TRttiVarData(Value).VType := varInteger;
  end
  else if (frac < 0) and
          (frac >= -4) then
  begin
    // currency as ###.0123
    TRttiVarData(Value).VType := varCurrency;
    Value.VInt64 := v64 * CURRENCY_FACTOR[frac]; // as round(CurrValue*10000)
  end
  else if AllowVarDouble and
          (frac > -324) then // 5.0 x 10^-324 .. 1.7 x 10^308
  begin
    // converted into a double value
    exp := PtrUInt(@POW10);
    if frac >= -31 then
      if frac <= 31 then
        d := PPow10(exp)[frac]                 // -31 .. + 31
      else if (18 - remdigit) + integer(frac) >= 308 then
        exit                                   // +308 ..
      else
        d := HugePower10Pos(frac, PPow10(exp)) // +32 .. +307
    else
      d := HugePower10Neg(frac, PPow10(exp));  // .. -32
    Value.VDouble := d * v64;
    TRttiVarData(Value).VType := varDouble;
  end
  else
    exit;
  result := Json; // returns the first char after the parsed number
end;

procedure UniqueVariant(Interning: TRawUtf8Interning; var aResult: variant;
  aText: PUtf8Char; aTextLen: PtrInt; aAllowVarDouble: boolean);
var
  tmp: RawUtf8;
begin
  if not GetVariantFromNotStringJson(
           aText, TVarData(aResult), aAllowVarDouble) then
  begin
    FastSetString(tmp, aText, aTextLen);
    if Interning = nil then
      RawUtf8ToVariant(tmp, aResult)
    else
      Interning.UniqueVariant(aResult, tmp);
  end;
end;

procedure TextToVariant(const aValue: RawUtf8; AllowVarDouble: boolean;
  out aDest: variant);
begin
  try
    if GetVariantFromNotStringJson(pointer(aValue), TVarData(aDest), AllowVarDouble) then
      exit;
  except // some obscure floating point exception may occur
  end;
  RawUtf8ToVariant(aValue, aDest);
end;

procedure TextBufferToVariant(aValue: PUtf8Char; AllowVarDouble: boolean;
  out aDest: variant);
begin
  try
    if GetVariantFromNotStringJson(aValue, TVarData(aDest), AllowVarDouble) then
      exit;
  except // some obscure floating point exception may occur
  end;
  RawUtf8ToVariant(aValue, StrLen(aValue), aDest);
end;

function GetNextItemToVariant(var P: PUtf8Char; out Value: Variant;
  Sep: AnsiChar; AllowDouble: boolean): boolean;
var
  temp: RawUtf8;
begin
  if P = nil then
    result := false
  else
  begin
    GetNextItem(P, Sep, temp);
    TextToVariant(temp, AllowDouble, Value);
    result := true;
  end;
end;

procedure GetVariantFromJsonField(Json: PUtf8Char; wasString: boolean;
  var Value: variant; TryCustomVariants: PDocVariantOptions;
  AllowDouble: boolean; JsonLen: integer);
var
  V: TVarData absolute Value;
  info: TGetJsonField;
begin
  // first handle any strict-Json syntax objects or arrays into custom variants
  if (TryCustomVariants <> nil) and
     (Json <> nil) then
    if (GotoNextNotSpace(Json)^ in ['{', '[']) and
       not wasString then
    begin // also supports dvoJsonObjectParseWithinString
      info.Json := Json;
      JsonToAnyVariant(Value, info, TryCustomVariants, AllowDouble);
      exit;
    end
    else if dvoAllowDoubleValue in TryCustomVariants^ then
      AllowDouble := true;
  // handle simple text or numerical values
  VarClear(Value);
  // try any numerical or true/false/null value
  if wasString or
     not GetVariantFromNotStringJson(Json, V, AllowDouble) then
  begin
    // found no numerical value -> return a string in the expected format
    TRttiVarData(Value).VType := varString;
    V.VString := nil; // avoid GPF below
    if JsonLen = 0 then
      JsonLen := StrLen(Json);
    FastSetString(RawUtf8(V.VString), Json, JsonLen);
  end;
end;

procedure _BinaryVariantLoadAsJson(var Value: variant; Json: PUtf8Char;
  TryCustomVariant: pointer);
var
  info: TGetJsonField;
begin
  if TryCustomVariant = nil then
    TryCustomVariant := @JSON_[mFast];
  info.Json := Json;
  JsonToAnyVariant(Value, info, TryCustomVariant, {double=}true);
end;

function VariantLoadJson(var Value: Variant; const Json: RawUtf8;
  TryCustomVariants: PDocVariantOptions; AllowDouble: boolean): boolean;
var
  tmp: TSynTempBuffer;
  info: TGetJsonField;
begin
  tmp.Init(Json); // temp copy before in-place decoding
  try
    info.Json := tmp.buf;
    JsonToAnyVariant(Value, info, TryCustomVariants, AllowDouble);
    result := info.Json <> nil;
  finally
    tmp.Done;
  end;
end;

function VariantLoadJson(const Json: RawUtf8;
  TryCustomVariants: PDocVariantOptions; AllowDouble: boolean): variant;
begin
  VariantLoadJson(result, Json, TryCustomVariants, AllowDouble);
end;

function JsonToVariantInPlace(var Value: Variant; Json: PUtf8Char;
  Options: TDocVariantOptions; AllowDouble: boolean): PUtf8Char;
var
  info: TGetJsonField;
begin
  info.Json := Json;
  JsonToAnyVariant(Value, info, @Options, AllowDouble);
  result := info.Json;
end;

function JsonToVariant(const Json: RawUtf8; Options: TDocVariantOptions;
  AllowDouble: boolean): variant;
begin
  VariantLoadJson(result, Json, @Options, AllowDouble);
end;

procedure MultiPartToDocVariant(const MultiPart: TMultiPartDynArray;
  var Doc: TDocVariantData; Options: PDocVariantOptions);
var
  ndx: PtrInt;
  v: variant;
begin
  if Options = nil then
    Doc.InitFast(dvObject)
  else
    Doc.Init(Options^, dvObject);
  for ndx := 0 to high(multipart) do
    with MultiPart[ndx] do
      if ContentType = TEXT_CONTENT_TYPE then
      begin
        // append as regular "Name":"TextValue" field
        RawUtf8ToVariant(Content, v);
        Doc.AddValue(name, v);
      end
      else
        // append binary file as an object, with Base64-encoded data
        Doc.AddValue(name, _ObjFast([
          'data',        BinToBase64(Content),
          'filename',    FileName,
          'contenttype', ContentType]));
end;

function ParseSortMatch(Expression: PUtf8Char; out Key: RawUtf8;
  out Match: TCompareOperator; Value: PVariant): boolean;
var
  KB, KE, B: PUtf8Char;
begin
  result := false;
  if Expression = nil then
    exit;
  Expression := GotoNextNotSpace(Expression);
  KB := Expression;
  while jcJsonIdentifier in JSON_CHARS[Expression^] do
    inc(Expression);
  if Expression^ = #0 then
    exit;
  KE := Expression;
  Expression := GotoNextNotSpace(Expression);
  B := Expression;
  while Expression^ in ['<', '>', '='] do
    inc(Expression);
  case Expression - B of
    1:
      case B^ of
        '=':
          Match := coEqualTo;
        '<':
          Match := coLessThan;
        '>':
          Match := coGreaterThan
      else
        exit;
      end;
    2:
      case PWord(B)^ of
        ord('=') + ord('=') shl 8: // c-style
          Match := coEqualTo;
        ord('!') + ord('=') shl 8, // c-style
        ord('<') + ord('>') shl 8:
          Match := coNotEqualTo;
        ord('>') + ord('=') shl 8:
          Match := coGreaterThanOrEqualTo;
        ord('<') + ord('=') shl 8:
          Match := coLessThanOrEqualTo;
      else
        exit;
      end;
  else
    exit;
  end;
  FastSetString(Key, KB, KE - KB);
  if Value <> nil then
    TextBufferToVariant(GotoNextNotSpace(Expression), {allowdouble=}true, Value^);
  result := true;
end;

{ ************** Variant Binary Serialization }

{$ifndef PUREMORMOT2}

function VariantSaveLength(const Value: variant): integer;
begin
  result := {%H-}BinarySaveLength(@Value, TypeInfo(Variant), nil, [rkVariant]);
end;

function VariantSave(const Value: variant; Dest: PAnsiChar): PAnsiChar;
var
  dummy: integer;
begin
  result := {%H-}BinarySave(@Value, Dest, TypeInfo(Variant), dummy, [rkVariant]);
end;

{$endif PUREMORMOT2}

function VariantSave(const Value: variant): RawByteString;
begin
  result := BinarySave(@Value, TypeInfo(Variant), [rkVariant]);
end;

function VariantLoad(var Value: variant; Source: PAnsiChar;
  CustomVariantOptions: PDocVariantOptions; SourceMax: PAnsiChar): PAnsiChar;
begin
  {$ifndef PUREMORMOT2}
  if SourceMax = nil then
    // mORMot 1 unsafe backward compatible: assume fake 100MB Source input
    SourceMax := Source + 100 shl 20;
  {$endif PUREMORMOT2}
  result := BinaryLoad(@Value, Source, TypeInfo(Variant), nil, SourceMax,
    [rkVariant], CustomVariantOptions);
end;

function VariantLoad(const Bin: RawByteString;
  CustomVariantOptions: PDocVariantOptions): variant;
begin
  BinaryLoad(@result, Bin, TypeInfo(Variant),
    [rkVariant], CustomVariantOptions);
end;

procedure FromVarVariant(var Source: PByte; var Value: variant;
  CustomVariantOptions: PDocVariantOptions; SourceMax: PByte);
begin
  Source := PByte(VariantLoad(Value, pointer(Source),
    CustomVariantOptions, pointer(SourceMax)));
end;


{ ************** IDocList/IDocDict advanced Wrappers of TDocVariant Documents }

type
  TDocAny = class(TInterfacedSerializable)
  protected
    fValue: PDocVariantData;
    fValueOwned: TVarData;
  public
    constructor CreateOwned;
    constructor CreateNew(const dv: TDocVariantData; m: TDocVariantModel); reintroduce;
    constructor CreateCopy(const dv: TDocVariantData); reintroduce;
    constructor CreateByRef(dv: PDocVariantData); reintroduce;
    destructor Destroy; override;
    procedure OwnedAs(opt: PDocVariantOptions; added: TDocVariantOption);
      {$ifdef HASINLINE} inline; {$endif}
    procedure Clear;
    function Kind: TDocVariantKind;
    function Model: TDocVariantModel;
    function Len: integer;
    procedure ToJson(W: TJsonWriter; Options: TTextWriterWriteObjectOptions); override;
    function Value: PDocVariantData;
    function AsList: IDocList;
    function AsDict: IDocDict;
    function AsVariant: variant;
  end;

  TDocList = class(TDocAny, IDocList)
  public
    // TInterfacedSerializable methods
    constructor Create(options: PDocVariantOptions); override;
    procedure FromJson(var context: TJsonParserContext); override;
    // IDocList methods
    function GetB(position: integer): boolean;
    function GetC(position: integer): currency;
    function GetD(position: integer): IDocDict;
    function GetF(position: integer): double;
    function GetI(position: integer): Int64;
    function GetItem(position: integer): variant;
    function GetL(position: integer): IDocList;
    function GetS(position: integer): string;
    function GetU(position: integer): RawUtf8;
    procedure SetB(position: integer; value: boolean);
    procedure SetC(position: integer; const value: currency);
    procedure SetD(position: integer; const value: IDocDict);
    procedure SetF(position: integer; const value: double);
    procedure SetI(position: integer; value: Int64);
    procedure SetItem(position: integer; const value: variant);
    procedure SetL(position: integer; const value: IDocList);
    procedure SetS(position: integer; const value: string);
    procedure SetU(position: integer; const value: RawUtf8);
    //procedure SetJson(const value: RawUtf8); override;
    function Append(const value: variant): integer; overload;
    function Append(const value: RawUtf8): integer; overload;
    function AppendDoc(const value: IDocAny): integer;
    function Copy(start, stop: integer): IDocList;
    function Compare(const another: IDocList; caseinsensitive: boolean): integer;
    function Count(const value: variant): integer; overload;
    function Count(const value: RawUtf8): integer; overload;
    procedure Extend(const value: IDocList); overload;
    procedure Extend(const value: array of const); overload;
    function Filter(const key: RawUtf8; const value: variant; limit: integer;
      match: TCompareOperator; compare: TVariantCompare): IDocList; overload;
    function Filter(const expression: RawUtf8): IDocList; overload;
    function Filter(const expression: RawUtf8; const value: variant;
      limit: integer): IDocList; overload;
    function First(const expression: RawUtf8): variant; overload;
    function First(const expression: RawUtf8; const value: variant): variant; overload;
    function Index(const value: variant): integer; overload;
    function Index(const value: RawUtf8; caseinsensitive: boolean): integer; overload;
    function Exists(const value: variant): boolean; overload;
    function Exists(const value: RawUtf8; caseinsensitive: boolean): boolean; overload;
    function Insert(position: integer; const value: variant): integer; overload;
    function Insert(position: integer; const value: RawUtf8): integer; overload;
    function ObjectsDictDynArray: IDocDictDynArray;
    function Pop(position: integer): variant;
    function PopItem(out value: variant; position: integer): boolean; overload;
    function PopItem(out value: IDocDict; position: integer): boolean; overload;
    function Del(position: integer): boolean;
    function Reduce(const keys: array of RawUtf8): IDocList;
    function Remove(const value: variant): integer; overload;
    function Remove(const value: RawUtf8; caseinsensitive: boolean): integer; overload;
    procedure Reverse;
    procedure Sort(reverse: boolean; compare: TVariantCompare);
    procedure SortByKeyValue(const key: RawUtf8; reverse: boolean;
      compare: TVariantCompare); overload;
    procedure SortByKeyValue(const keys: array of RawUtf8; reverse: boolean;
      compare: TVariantCompare); overload;
    function ValueAt(position: integer): PVariant;
    {$ifdef HASIMPLICITOPERATOR}
    function GetV(position: integer): TDocValue;
    function GetEnumerator: TDocValueEnumerator;
    function Range(start, stop: integer): TDocValueEnumerator;
    function Objects: TDocObjectEnumerator; overload;
    function Objects(const key: RawUtf8; const value: variant;
      match: TCompareOperator; compare: TVariantCompare): TDocObjectEnumerator; overload;
    function Objects(const expression: RawUtf8): TDocObjectEnumerator; overload;
    function Objects(const expression: RawUtf8;
      const value: variant): TDocObjectEnumerator; overload;
    {$endif HASIMPLICITOPERATOR}
  end;

  TDocDict = class(TDocAny, IDocDict)
  protected
    fPathDelim: AnsiChar; // some additional parameters to this IDocDict state
    fSorted: TUtf8Compare;
    function GetValueAt(const key: RawUtf8; out value: PVariant): boolean;
    function SetValueAt(const key: RawUtf8; const value: variant): boolean;
    function GetExistingValueAt(const key, method: RawUtf8): PVariant;
    function PopAt(const key: RawUtf8; value: PVariant): boolean;
  public
    // TInterfacedSerializable methods
    constructor Create(options: PDocVariantOptions); override;
    procedure FromJson(var context: TJsonParserContext); override;
    // IDocDict methods
    function GetB(const key: RawUtf8): boolean;
    function GetC(const key: RawUtf8): currency;
    function GetD(const key: RawUtf8): IDocDict;
    function GetF(const key: RawUtf8): double;
    function GetI(const key: RawUtf8): Int64;
    function GetItem(const key: RawUtf8): variant;
    function GetL(const key: RawUtf8): IDocList;
    function GetS(const key: RawUtf8): string;
    function GetU(const key: RawUtf8): RawUtf8;
    procedure SetB(const key: RawUtf8; const value: boolean);
    procedure SetC(const key: RawUtf8; const value: currency);
    procedure SetD(const key: RawUtf8; const value: IDocDict);
    procedure SetF(const key: RawUtf8; const value: double);
    procedure SetI(const key: RawUtf8; const value: Int64);
    procedure SetItem(const key: RawUtf8; const value: variant);
    procedure SetL(const key: RawUtf8; const value: IDocList);
    procedure SetS(const key: RawUtf8; const value: string);
    procedure SetU(const key: RawUtf8; const value: RawUtf8);
    function Get(const key: RawUtf8): variant; overload;
    function GetDef(const key: RawUtf8; const default: variant): variant; overload;
    function GetDef(const key: RawUtf8; const default: RawUtf8): variant; overload;
    function Get(const key: RawUtf8; var value: variant): boolean; overload;
    function Get(const key: RawUtf8; var value: RawUtf8): boolean; overload;
    function Get(const key: RawUtf8; var value: string): boolean; overload;
    function Get(const key: RawUtf8; var value: boolean): boolean; overload;
    function Get(const key: RawUtf8; var value: integer): boolean; overload;
    function Get(const key: RawUtf8; var value: Int64): boolean; overload;
    function Get(const key: RawUtf8; var value: double): boolean; overload;
    function Get(const key: RawUtf8; var value: currency): boolean; overload;
    function Get(const key: RawUtf8; var value: IDocList): boolean; overload;
    function Get(const key: RawUtf8; var value: IDocDict): boolean; overload;
    function Get(const key: RawUtf8; var value: PDocVariantData): boolean; overload;
    function GetPathDelim: AnsiChar;
    procedure SetPathDelim(value: AnsiChar);
    function Compare(const another: IDocDict; caseinsensitive: boolean): integer; overload;
    function Compare(const another: IDocDict; const keys: array of RawUtf8;
      caseinsensitive: boolean = false): integer; overload;
    function Copy: IDocDict;
    function Del(const key: RawUtf8): boolean;
    function Exists(const key: RawUtf8): boolean;
    function Pop(const key: RawUtf8): variant; overload;
    function Pop(const key: RawUtf8; const default: variant): variant; overload;
    function PopItem(out key: RawUtf8; out value: variant; position: integer): boolean;
    function Reduce(const keys: array of RawUtf8): IDocDict;
    function SetDefault(const key: RawUtf8): variant; overload;
    function SetDefault(const key: RawUtf8; const default: variant): variant; overload;
    procedure Sort(reverse: boolean; keycompare: TUtf8Compare);
    procedure Update(const key: RawUtf8; const value: variant); overload;
    procedure Update(const keyvalues: array of const); overload;
    procedure Update(const source: IDocDict; addonlymissing: boolean); overload;
    function ValueAt(const key: RawUtf8): PVariant;
    {$ifdef HASIMPLICITOPERATOR}
    function GetV(const key: RawUtf8): TDocValue;
    function GetEnumerator: TDocDictEnumerator;
    function Keys: TDocKeyEnumerator;
    function Values: TDocValueEnumerator;
    {$endif HASIMPLICITOPERATOR}
  end;

{$ifdef HASIMPLICITOPERATOR}

{ TDocValue }

function TDocValue.IsString: boolean;
begin
  result := VarIsStr(V^);
end;

function TDocValue.Kind: TDocVariantKind;
begin
  result := _Safe(V^).GetKind;
end;

class operator TDocValue.Implicit(const A: TDocValue): boolean;
begin
  if not VariantToBoolean(A.V^, result) then
    result := false;
end;

class operator TDocValue.Implicit(const A: TDocValue): integer;
begin
  if not VariantToInteger(A.V^, result) then
    result := 0;
end;

class operator TDocValue.Implicit(const A: TDocValue): Int64;
begin
  if not VariantToInt64(A.V^, result) then
    result := 0;
end;

class operator TDocValue.Implicit(const A: TDocValue): string;
begin
  VariantToString(A.V^, result);
end;

class operator TDocValue.Implicit(const A: TDocValue): RawUtf8;
var
  wasString: boolean;
begin
  VariantToUtf8(A.V^, RawUtf8(result), wasString);
end;

class operator TDocValue.Implicit(const A: TDocValue): IDocList;
begin
  result := DocListFrom(A.V^);
end;

class operator TDocValue.Implicit(const A: TDocValue): IDocDict;
begin
  result := DocDictFrom(A.V^);
end;

class operator TDocValue.Implicit(const A: TDocValue): variant;
begin
  result := A.V^;
end;

class operator TDocValue.Implicit(const A: TDocValue): PVarData;
begin
  result := pointer(A.V);
end;

class operator TDocValue.Implicit(const A: TDocValue): PDocVariantData;
begin
  result := _Safe(A.V^); // better not inlined at TDocValue level
end;

{ TDocValueEnumerator }

function TDocValueEnumerator.MoveNext: boolean;
var
  c: PVariant;
begin
  c := Curr.V;
  inc(c);
  Curr.V := c;
  result := PtrUInt(c) < PtrUInt(After.V);
end;

function TDocValueEnumerator.GetEnumerator: TDocValueEnumerator;
begin
  result := self; // just copy 2 pointers
end;

{ TDocObjectEnumerator }

function TDocObjectEnumerator.MoveNext: boolean;
var
  c, o: PVariant;
  v: TDocDict;
  dv: PDocVariantData;
begin
  result := false;
  repeat
    c := Curr;
    inc(c);
    Curr := c;
    if PtrUInt(c) >= PtrUInt(After) then
      exit; // reached end of list
    if not _Safe(c^, dv) then
      continue
    else if CompKey = '' then
    begin
      if not dv.IsObject then
        continue; // ignore any list element which is not a IDocDict
    end
    else
    begin
      if CompKeyHasPath then
      begin
        o := dv^.GetPVariantByPath(CompKey, '.');
        if o = nil then
          continue;
      end
      else if not dv^.GetObjectProp(CompKey, o, @CompKeyPrev) then
        continue;
      if not SortMatch(CompFunc({%H-}o^, CompValue), CompMatch) then
        continue;
    end;
    if CurrDict = nil then
    begin
      v := TDocDict.CreateByRef(nil);
      CurrDictValue := @v.fValue;
      CurrDict := v; // share a single TDocDict instance during loop
    end;
    CurrDictValue^ := dv; // directly change TDocDict.fValue
    result := true;
    exit;
  until false;
end;

function TDocObjectEnumerator.GetEnumerator: TDocObjectEnumerator;
begin
  result := self;
end;

{ TDocKey }

function TDocKey.Equals(const txt: RawUtf8): boolean;
begin
  result := txt = V^;
end;

function TDocKey.Utf8: RawUtf8;
begin
  result := V^;
end;

class operator TDocKey.Implicit(const A: TDocKey): string;
begin
  Utf8ToStringVar(A.V^, result);
end;

class operator TDocKey.Implicit(const A: TDocKey): RawUtf8;
begin
  result := A.V^;
end;

{ TDocDictFields }

function TDocDictFields.KeyValue(const separator: RawUtf8): RawUtf8;
begin
  Make([Key.V^, separator, Value.V^], result);
end;

{ TDocKeyEnumerator }

function TDocKeyEnumerator.MoveNext: boolean;
var
  c: PRawUtf8;
begin
  c := Curr.V;
  inc(c);
  Curr.V := c;
  result := PtrUInt(c) < PtrUInt(After.V);
end;

function TDocKeyEnumerator.GetEnumerator: TDocKeyEnumerator;
begin
  result := self; // just copy 2 pointers
end;

{ TDocDictEnumerator }

function TDocDictEnumerator.MoveNext: boolean;
var
  v: PVariant;
begin
  inc(Curr.Key.V);
  v := Curr.Value.V;
  inc(v);
  Curr.Value.V := v;
  result := PtrUInt(v) < PtrUInt(AfterValue.V);
end;

function TDocDictEnumerator.GetEnumerator: TDocDictEnumerator;
begin
  result := self; // just copy 3 pointers
end;

{$endif HASIMPLICITOPERATOR}


{ IDocList factories functions }

function DocList(model: TDocVariantModel): IDocList;
var
  v: TDocList;
begin
  v := TDocList.CreateOwned;
  TDocVariantData(v.fValueOwned).Init(model, dvArray);
  result := v;
end;

function DocList(const json: RawUtf8; model: TDocVariantModel): IDocList;
begin
  result := DocList(model);
  result.SetJson(json);
end;

function DocListFromResults(const json: RawUtf8;
  model: TDocVariantModel): IDocList;
begin
  result := TDocList.CreateOwned;
  if not result.Value^.InitArrayFromResults(json, model) then
    result := nil;
end;

function DocList(const dv: TDocVariantData): IDocList;
var
  v: TDocList;
begin
  v := nil;
  if dv.IsArray then
    v := TDocList.CreateByRef(@dv);
  result := v;
end;

function DocListCopy(const dv: TDocVariantData): IDocList;
var
  v: TDocList;
begin
  v := nil;
  if dv.IsArray then
    v := TDocList.CreateCopy(dv);
  result := v;
end;

function DocListCopy(const v: variant): IDocList;
begin
  result := DocListCopy(_Safe(v)^);
end;

function DocListCopy(const dv: TDocVariantData; model: TDocVariantModel): IDocList;
var
  v: TDocList;
begin
  v := nil;
  if dv.IsArray then
    v := TDocList.CreateNew(dv, model);
  result := v;
end;

function DocList(const values: array of const; model: TDocVariantModel): IDocList;
begin
  result := TDocList.CreateOwned;
  result.Value^.InitArray(values, model);
end;

function DocListFrom(const v: variant): IDocList;
var
  dv: PDocVariantData;
  d: TDocList;
begin
  d := nil;
  if _SafeArray(variant(v), dv) then
    d := TDocList.CreateByRef(dv);
  result := d;
end;

function DocListFrom(const dictarray: IDocDictDynArray): IDocList;
var
  i: PtrInt;
begin
  result := DocList(DocAnyDefaultModel);
  for i := 0 to length(dictarray) - 1 do
    result.AppendDoc(dictarray[i]);
end;

{ IDocDict factories functions }

function DocDict(model: TDocVariantModel): IDocDict;
var
  v: TDocDict;
begin
  v := TDocDict.CreateOwned;
  TDocVariantData(v.fValueOwned).Init(model, dvObject);
  result := v;
end;

function DocDict(const json: RawUtf8; model: TDocVariantModel): IDocDict;
begin
  result := DocDict(model);
  result.SetJson(json);
end;

function DocDictDynArray(const json: RawUtf8;
  model: TDocVariantModel; jsonfromresults: boolean): IDocDictDynArray;
var
  main: TDocVariantData;
  n, i: PtrInt;
  p: PVariant;
  dv: PDocVariantData;
  v: TDocDict;
begin
  result := nil;
  if jsonfromresults then
  begin
    if not main.InitArrayFromResults(json, model) then
      exit;
  end
  else if not main.InitJson(json, model) or
          not main.IsArray then
    exit;
  n := main.Count;
  if n = 0 then
    exit;
  p := pointer(main.VValue);
  i := 0;
  repeat
    if _SafeObject(p^, dv) then
    begin
      if result = nil then
        SetLength(result, n); // allocate only when needed
      v := TDocDict.CreateOwned;
      v.fValueOwned := PVarData(dv)^; // raw copy with no refcount
      PRttiVarData(dv)^.VType := varEmpty; // not in main any more
      result[i] := v;
      inc(i);
    end;
    inc(p);
    dec(n);
  until n = 0;
  if i <> 0 then
    DynArrayFakeLength(result, i); // no realloc
end;

function DocDictFrom(const v: variant): IDocDict;
var
  dv: PDocVariantData;
  d: TDocDict;
begin
  d := nil;
  if _SafeObject(variant(v), dv) then
    d := TDocDict.CreateByRef(dv);
  result := d;
end;

function DocDict(const dv: TDocVariantData): IDocDict;
begin
  if dv.IsObject then
    result := TDocDict.CreateByRef(@dv)
  else
    result := nil;
end;

function DocDictCopy(const dv: TDocVariantData): IDocDict;
var
  d: TDocDict;
begin
  d := nil;
  if dv.IsObject then
    d := TDocDict.CreateCopy(dv);
  result := d;
end;

function DocDictCopy(const v: variant): IDocDict;
begin
  result := DocDictCopy(_Safe(v)^);
end;

function DocDictCopy(const dv: TDocVariantData; model: TDocVariantModel): IDocDict;
var
  d: TDocDict;
begin
  d := nil;
  if dv.IsObject then
    d := TDocDict.CreateNew(dv, model);
  result := d;
end;

function DocDict(const keyvalues: array of const; model: TDocVariantModel): IDocDict;
begin
  result := TDocDict.CreateOwned;
  result.Value^.InitObject(keyvalues, model);
end;

function DocDictFromKeys(const keys: array of RawUtf8;
  model: TDocVariantModel): IDocDict;
begin
  result := DocDictFromKeys(keys, Null, model);
end;

function DocDictFromKeys(const keys: array of RawUtf8; const value: variant;
  model: TDocVariantModel): IDocDict;
var
  i: PtrInt;
  dv: PDocVariantData;
begin
  result := TDocDict.CreateOwned;
  dv := result.Value;
  dv^.Init(model, dvObject);
  dv^.SetCapacity(length(keys));
  for i := 0 to high(keys) do
    dv^.AddOrUpdateValue(keys[i], value);
end;


{ TDocAny }

constructor TDocAny.CreateOwned;
begin
  fValue := @fValueOwned;
end;

procedure TDocAny.OwnedAs(opt: PDocVariantOptions; added: TDocVariantOption);
begin
  fValue := @fValueOwned;
  if opt = nil then
    opt := @JSON_[DocAnyDefaultModel];
  TRttiVarData(fValueOwned).VType := DocVariantVType +
    cardinal(PWord(opt)^ + 1 shl ord(added)) shl 16; // VType+VOptions
end;

constructor TDocAny.CreateNew(const dv: TDocVariantData; m: TDocVariantModel);
begin
  fValue := @fValueOwned;
  TDocVariantData(fValueOwned).Init(m, dv.Kind); // new arrays, but byref values
  if dv.Count = 0 then
    exit;
  DynArrayCopy(@fValue^.VName, @dv.VName, TypeInfo(TRawUtf8DynArray), @dv.Count);
  DynArrayCopy(@fValue^.VValue, @dv.VValue, TypeInfo(TVariantDynArray), @dv.Count);
  TDocVariantData(fValueOwned).VCount := dv.Count;
end;

constructor TDocAny.CreateCopy(const dv: TDocVariantData);
begin
  fValue := @fValueOwned;
  TDocVariantData(fValueOwned).InitFrom(dv, true, true); // new arrays, but byref values
end;

constructor TDocAny.CreateByRef(dv: PDocVariantData);
begin
  fValue := dv;
end;

destructor TDocAny.Destroy;
begin
  inherited Destroy;
  if fValue = @fValueOwned then
    TDocVariantData(fValueOwned).Void;
end;

function TDocAny.Kind: TDocVariantKind;
begin
  result := fValue^.GetKind;
end;

function TDocAny.Model: TDocVariantModel;
begin
  if not fValue^.GetModel(result) then
    result := DocAnyDefaultModel; // default value if not exactly found
end;

function TDocAny.Len: integer;
begin
  result := fValue^.VCount;
end;

procedure TDocAny.ToJson(W: TJsonWriter; Options: TTextWriterWriteObjectOptions);
begin
  DocVariantType.ToJson(W, PVarData(fValue));
end;

function TDocAny.Value: PDocVariantData;
begin
  result := fValue;
end;

procedure TDocAny.Clear;
begin
  fValue^.Void; // keep Options and Kind
end;

function TDocAny.AsList: IDocList;
begin
  if fValue^.IsArray then
    result := self as TDocList
  else
    result := nil;
end;

function TDocAny.AsDict: IDocDict;
begin
  if fValue^.IsObject then
    result := self as TDocDict
  else
    result := nil;
end;

function TDocAny.AsVariant: variant;
begin
  result := PVariant(fValue)^;
end;

procedure JL_IDocAny(var Context: TJsonParserContext;
  Doc: PDocVariantData; Token: TJsonToken);
var
  ctx: TGetJsonField absolute Context; // circumvent USERECORDWITHMETHODS
  opt: PDocVariantOptions;
begin
  Doc^.Void; // IDocList/IDocDict may be existing and with some previous data
  if GetFirstJsonToken(ctx.Json) <> Token then
  begin
    Context.Valid := (ctx.Json <> nil) and Context.ParseNull;
    exit;
  end;
  opt := Context.CustomVariant;
  if opt = nil then
    opt := @Doc^.VOptions;
  ctx.Json := Doc^.InitJsonInPlace(ctx.Json, opt^, @ctx.EndOfObject);
  Context.Valid := ctx.Json <> nil;
end;


{ EDocList }

class procedure EDocList.GetRaise(method: AnsiChar; pos: integer; const v: variant);
begin
  raise CreateUtf8('%[%] on a var%', [method, pos, VariantTypeName(v)^]);
end;

{ TDocList }

constructor TDocList.Create(options: PDocVariantOptions);
begin
  OwnedAs(options, dvoIsArray);
end;

procedure TDocList.FromJson(var context: TJsonParserContext);
begin
  JL_IDocAny(context, fValue, jtArrayStart);
end;

function TDocList.ValueAt(position: integer): PVariant;
var
  ndx, n: PtrUInt;
begin
  ndx := position;
  n := fValue^.VCount;
  if position < 0 then
    inc(ndx, n);
  if ndx >= n then
    raise EDocList.CreateUtf8('Index % out of range (len=%)', [position, n]);
  result := @fValue^.VValue[ndx];
  // setters should not call EnsureUnique() because is done in constructor
end;

function TDocList.GetItem(position: integer): variant;
begin
  result := ValueAt(position)^;
end;

procedure TDocList.SetItem(position: integer; const value: variant);
var
  v: PVariant;
begin
  v := ValueAt(position);
  SetVariantByValue(value, v^); // may convert to RawUtf8/varString
  if (PVarData(v)^.VType = varString) and
     fValue^.Has(dvoInternValues) then
    InternalUniqueValue(v);
end;

function TDocList.GetU(position: integer): RawUtf8;
begin
  VariantToUtf8(ValueAt(position)^, result);
end;

procedure TDocList.SetU(position: integer; const value: RawUtf8);
var
  v: PVariant;
begin
  v := ValueAt(position);
  RawUtf8ToVariant(value, v^);
  if fValue^.Has(dvoInternValues) then
    InternalUniqueValue(v);
end;

function TDocList.GetS(position: integer): string;
begin
  VariantToString(ValueAt(position)^, result);
end;

procedure TDocList.SetS(position: integer; const value: string);
var
  v: PVariant;
begin
  v := ValueAt(position);
  StringToVariant(value, v^); // convert and store as RawUtf8/varString
  if fValue^.Has(dvoInternValues) then
    InternalUniqueValue(v);
end;

function TDocList.GetI(position: integer): Int64;
var
  v: PVariant;
begin
  v := ValueAt(position);
  if not VariantToInt64(v^, result) then
    EDocList.GetRaise('I', position, v^);
end;

function TDocList.GetF(position: integer): double;
var
  v: PVariant;
begin
  v := ValueAt(position);
  if not VariantToDouble(v^, result) then
    EDocList.GetRaise('F', position, v^);
end;

function TDocList.GetC(position: integer): currency;
var
  v: PVariant;
begin
  v := ValueAt(position);
  if not VariantToCurrency(v^, result) then
    EDocList.GetRaise('C', position, v^);
end;

procedure TDocList.SetI(position: integer; value: Int64);
begin
  ValueAt(position)^ := value;
end;

procedure TDocList.SetF(position: integer; const value: double);
begin
  ValueAt(position)^ := value;
end;

procedure TDocList.SetC(position: integer; const value: currency);
begin
  ValueAt(position)^ := value;
end;

function TDocList.GetB(position: integer): boolean;
var
  v: PVariant;
begin
  v := ValueAt(position);
  if not VariantToBoolean(v^, result) then
    EDocList.GetRaise('B', position, v^);
end;

function TDocList.GetL(position: integer): IDocList;
begin
  result := TDocList.CreateByRef(_Safe(ValueAt(position)^, dvArray));
end;

function TDocList.GetD(position: integer): IDocDict;
begin
  result := TDocDict.CreateByRef(_Safe(ValueAt(position)^, dvObject));
end;

procedure TDocList.SetB(position: integer; value: boolean);
begin
  ValueAt(position)^ := value;
end;

procedure TDocList.SetL(position: integer; const value: IDocList);
var
  v: PVariant;
begin
  v := ValueAt(position);
  if value = nil then
    VarClear(v^)
  else
    v^ := PVariant(value.Value)^;
end;

procedure TDocList.SetD(position: integer; const value: IDocDict);
var
  v: PVariant;
begin
  v := ValueAt(position);
  if value = nil then
    VarClear(v^)
  else
    v^ := PVariant(value.Value)^;
end;

function TDocList.Append(const value: variant): integer;
begin
  result := fValue^.AddItem(value);
end;

function TDocList.Append(const value: RawUtf8): integer;
begin
  result := fValue^.AddItemText(value);
end;

function TDocList.AppendDoc(const value: IDocAny): integer;
begin
  result := fValue^.AddItem(PVariant(value.Value)^);
end;

function DocListRangeVoid(var start, stop: integer; n: integer): boolean;
begin
  result := true;
  if n = 0 then
    exit;
  if start < 0 then
    inc(start, n);
  if stop <> 0 then
  begin
    if stop < 0 then
      inc(stop, n);
    dec(stop, start); // from index to limit, excluding stop position
    if stop <= 0 then
      exit;
  end;
  result := false; // not void
end;

function TDocList.Copy(start, stop: integer): IDocList;
begin
  result := TDocList.CreateOwned;
  if DocListRangeVoid(start, stop, fValue^.Count) then
    result.Value^.Init(fValue^.VOptions, dvArray)
  else
    result.Value^.InitArrayFrom(fValue^, fValue^.VOptions, start, stop);
end;

function TDocList.Compare(const another: IDocList; caseinsensitive: boolean): integer;
begin
  if another = nil then
    result := 1
  else if another.Value = fValue then
    result := 0 // same reference
  else
    result := fValue^.Compare(another.Value^, caseinsensitive);
end;

function TDocList.Count(const value: variant): integer;
begin
  result := fValue^.CountItemByValue(value);
end;

function TDocList.Count(const value: RawUtf8): integer;
var
  v: TRttiVarData;
begin
  v.VType := varString;
  v.Data.VAny := pointer(value); // direct set to our RawUtf8 searched value
  result := fValue^.CountItemByValue(variant(v));
end;

procedure TDocList.Extend(const value: IDocList);
begin
  if value <> nil then
    fValue^.AddFrom(variant(value.Value^));
end;

procedure TDocList.Extend(const value: array of const);
begin
  fValue^.AddItems(value);
end;

function TDocList.Index(const value: variant): integer;
begin
  result := fValue^.SearchItemByValue(value);
end;

function TDocList.Index(const value: RawUtf8; caseinsensitive: boolean): integer;
var
  v: TRttiVarData;
begin
  v.VType := varString;
  v.Data.VAny := pointer(value); // direct set to our RawUtf8 searched value
  result := fValue^.SearchItemByValue(variant(v), caseinsensitive);
end;

function TDocList.Exists(const value: variant): boolean;
begin
  result := fValue^.SearchItemByValue(value) >= 0;
end;

function TDocList.Exists(const value: RawUtf8; caseinsensitive: boolean): boolean;
begin
  result := Index(value, caseinsensitive) >= 0;
end;

function TDocList.Insert(position: integer; const value: variant): integer;
begin
  result := fValue^.AddItem(value, position);
end;

function TDocList.Insert(position: integer; const value: RawUtf8): integer;
begin
  result := fValue^.AddItemText(value, position);
end;

function TDocList.ObjectsDictDynArray: IDocDictDynArray;
var
  n, i: PtrInt;
  p: PVariant;
  dv: PDocVariantData;
begin
  result := nil;
  n := fValue^.Count;
  if n = 0 then
    exit;
  p := pointer(fValue^.VValue);
  i := 0;
  repeat
    if _SafeObject(p^, dv) then
    begin
      if result = nil then
        SetLength(result, n); // allocate only when needed
      result[i] := TDocDict.CreateByRef(dv);
      inc(i);
    end;
    inc(p);
    dec(n);
  until n = 0;
  if i <> 0 then
    DynArrayFakeLength(result, i); // no realloc
end;

function TDocList.Pop(position: integer): variant;
begin
  if not fValue^.Extract(position, result) then
    raise EDocList.CreateUtf8('Pop index % out of range', [position]);
end;

function TDocList.PopItem(out value: variant; position: integer): boolean;
begin
  result := fValue^.Extract(position, value);
end;

function TDocList.PopItem(out value: IDocDict; position: integer): boolean;
begin
  result := false;
  if position < 0 then
    inc(position, fValue^.Count);
  if (cardinal(position) >= cardinal(fValue^.Count)) or
     not _Safe(fValue^.VValue[position]).IsObject then
    exit;
  value := TDocDict.CreateOwned;
  result := fValue^.Extract(position, PVariant(value.Value)^);
end;

function TDocList.Del(position: integer): boolean;
begin
  if position < 0 then
    inc(position, fValue^.Count);
  result := fValue^.Delete(position);
end;

function TDocList.Reduce(const keys: array of RawUtf8): IDocList;
begin
  result := DocList(Model);
  fValue^.Reduce(keys, fValue^.IsCaseSensitive, result.Value^);
end;

function TDocList.Remove(const value: variant): integer;
begin
  result := fValue^.SearchItemByValue(value);
  if result >= 0 then
    fValue^.Delete(result);
end;

function TDocList.Remove(const value: RawUtf8; caseinsensitive: boolean): integer;
begin
  result := Index(value, caseinsensitive);
  if result >= 0 then
    fValue^.Delete(result);
end;

procedure TDocList.Reverse;
begin
  fValue^.Reverse;
end;

procedure TDocList.Sort(reverse: boolean; compare: TVariantCompare);
begin
  fValue^.SortByValue(compare, reverse);
end;

procedure TDocList.SortByKeyValue(const key: RawUtf8; reverse: boolean;
  compare: TVariantCompare);
begin
  fValue^.SortArrayByField(key, compare, reverse);
end;

procedure TDocList.SortByKeyValue(const keys: array of RawUtf8;
  reverse: boolean; compare: TVariantCompare);
begin
  fValue^.SortArrayByFields(keys, compare, nil, reverse);
end;

function TDocList.Filter(const key: RawUtf8; const value: variant;
  limit: integer; match: TCompareOperator; compare: TVariantCompare): IDocList;
begin
  result := TDocList.CreateOwned;
  fValue^.ReduceFilter(key, value, match, compare, limit, result.Value^);
end;

function TDocList.Filter(const expression: RawUtf8): IDocList;
begin // no limit here to avoid confusion between overloads
  result := TDocList.CreateOwned;
  fValue^.ReduceFilter(expression, result.Value^);
end;

function TDocList.Filter(const expression: RawUtf8; const value: variant;
  limit: integer): IDocList;
begin
  result := TDocList.CreateOwned;
  fValue^.ReduceFilter(expression, value, result.Value^, nil, limit);
end;

function TDocList.First(const expression: RawUtf8): variant;
begin
  result := fValue^.ReduceFilter(expression, {limit=} 1);
end;

function TDocList.First(const expression: RawUtf8; const value: variant): variant;
begin
  result := fValue^.ReduceFilter(expression, value, {limit=} 1);
end;

{$ifdef HASIMPLICITOPERATOR}

function TDocList.GetV(position: integer): TDocValue;
begin
  result.V := ValueAt(position);
end;

procedure SetValueEnumerator(dv: PDocVariantData; var res: TDocValueEnumerator);
  {$ifdef HASINLINE} inline; {$endif}
var
  v: PVariant;
begin
  v := pointer(dv^.VValue);
  res.Curr.V := v;
  res.After.V := v;
  if v = nil then
    exit;
  inc(res.After.V, dv^.VCount);
  dec(res.Curr.V); // for the first MoveNext
end;

function TDocList.GetEnumerator: TDocValueEnumerator;
begin
  SetValueEnumerator(fValue, result{%H-}); // shared with IDocDict.Values
end;

function TDocList.Range(start, stop: integer): TDocValueEnumerator;
var
  v: PVariant;
begin
  result.Curr.V := nil;
  result.After.V := nil; // ensure MoveNext=false on void range
  if DocListRangeVoid(start, stop, fValue^.Count) or
     fValue^.RangeVoid(start, stop) then
    exit;
  v := pointer(fValue^.VValue);
  inc(v, start);
  dec(v); // for the first MoveNext
  result.Curr.V := v;
  inc(v, stop + 1);
  result.After.V := v;
end;

function TDocList.Objects: TDocObjectEnumerator;
var
  v: PVariant;
begin
  v := pointer(fValue^.VValue);
  result.Curr := v;
  result.After := v;
  if v = nil then
    exit;
  inc(result.After, fValue^.VCount);
  dec(result.Curr); // for the first MoveNext
end;

function TDocList.Objects(const key: RawUtf8; const value: variant;
  match: TCompareOperator; compare: TVariantCompare): TDocObjectEnumerator;
begin
  if key = '' then
    raise EDocList.Create('Invalid expression on Objects()');
  result := Objects;
  result.CompKey := key;
  result.CompValue := value;
  if not Assigned(compare) then
    compare := @VariantCompare;
  result.CompFunc := compare;
  result.CompMatch := match;
  result.CompKeyHasPath := PosExChar('.', key) <> 0;
  result.CompKeyPrev := -1; // optimistic key search in previous position
end;

function TDocList.Objects(const expression: RawUtf8): TDocObjectEnumerator;
var
  k: RawUtf8;
  v: variant;
  m: TCompareOperator;
begin
  ParseSortMatch(pointer(expression), k, m, @v);
  result := Objects(k, v, m, nil);
end;

function TDocList.Objects(const expression: RawUtf8;
  const value: variant): TDocObjectEnumerator;
var
  k: RawUtf8;
  m: TCompareOperator;
begin
  ParseSortMatch(pointer(expression), k, m, nil);
  result := Objects(k, value, m, nil);
end;

{$endif HASIMPLICITOPERATOR}




{ EDocDict }

class procedure EDocDict.Error(method: AnsiChar; const key: RawUtf8; const v: variant);
begin
  raise CreateUtf8('%[%] on a var%', [method, key, VariantTypeName(v)^]);
end;

{ TDocDict }

constructor TDocDict.Create(options: PDocVariantOptions);
begin
  OwnedAs(options, dvoIsObject);
end;

procedure TDocDict.FromJson(var context: TJsonParserContext);
begin
  JL_IDocAny(context, fValue, jtObjectStart);
end;

function TDocDict.GetPathDelim: AnsiChar;
begin
  result := fPathDelim;
end;

procedure TDocDict.SetPathDelim(value: AnsiChar);
begin
  fPathDelim := value;
end;

function TDocDict.Compare(const another: IDocDict; caseinsensitive: boolean): integer;
begin
  result := fValue^.Compare(another.Value^, caseinsensitive);
end;

function TDocDict.Compare(const another: IDocDict;
  const keys: array of RawUtf8; caseinsensitive: boolean): integer;
begin
  result := fValue^.CompareObject(keys, another.Value^, caseinsensitive);
end;

function TDocDict.GetValueAt(const key: RawUtf8; out value: PVariant): boolean;
begin
  if fPathDelim = #0 then
    value := pointer(fValue^.GetVarData(key, fSorted)) // faster
  else
    value := fValue^.GetPVariantByPath(key, fPathDelim);
  result := value <> nil; // return false if not found
end;

function TDocDict.GetExistingValueAt(const key, method: RawUtf8): PVariant;
begin
  if not GetValueAt(key, result) then
    if fValue^.Has(dvoReturnNullForUnknownProperty) then
      result := @DocVariantDataFake
    else
      raise EDocDict.CreateUtf8('%[''%''] key not found', [method, key]);
end;

function TDocDict.ValueAt(const key: RawUtf8): PVariant;
begin
  result := GetExistingValueAt(key, 'ValueAt');
end;

function TDocDict.SetValueAt(const key: RawUtf8; const value: variant): boolean;
begin
  if fPathDelim = #0 then
    result := fValue^.AddOrUpdateValue(key, value) >= 0
  else
    result := fValue^.SetValueByPath(key, value, {create=}true, fPathDelim);
  if result then
    fSorted := nil;
end;

function TDocDict.PopAt(const key: RawUtf8; value: PVariant): boolean;
begin
  if fPathDelim = #0 then
    result := fValue.Delete(key, value)
  else
    result := fValue.DeleteByPath(key, fPathDelim, value);
end;

function TDocDict.GetB(const key: RawUtf8): boolean;
var
  v: PVariant;
begin
  v := GetExistingValueAt(key, 'B');
  if not VariantToBoolean(v^, result) then
    EDocDict.Error('B', key, v^);
end;

function TDocDict.GetC(const key: RawUtf8): currency;
var
  v: PVariant;
begin
  v := GetExistingValueAt(key, 'C');
  if not VariantToCurrency(v^, result) then
    EDocDict.Error('C', key, v^);
end;

function TDocDict.GetD(const key: RawUtf8): IDocDict;
begin
  result := TDocDict.CreateByRef(
    _Safe(GetExistingValueAt(key, 'D')^, dvObject));
end;

function TDocDict.GetF(const key: RawUtf8): double;
var
  v: PVariant;
begin
  v := GetExistingValueAt(key, 'F');
  if not VariantToDouble(v^, result) then
    EDocDict.Error('F', key, v^);
end;

function TDocDict.GetI(const key: RawUtf8): Int64;
var
  v: PVariant;
begin
  v := GetExistingValueAt(key, 'I');
  if not VariantToInt64(v^, result) then
    EDocDict.Error('I', key, v^);
end;

function TDocDict.GetItem(const key: RawUtf8): variant;
begin
  result := GetExistingValueAt(key, 'Item')^;
end;

function TDocDict.GetL(const key: RawUtf8): IDocList;
begin
  result := TDocList.CreateByRef(
    _Safe(GetExistingValueAt(key, 'B')^, dvArray));
end;

function TDocDict.GetS(const key: RawUtf8): string;
begin
  VariantToString(GetExistingValueAt(key, 'S')^, result);
end;

function TDocDict.GetU(const key: RawUtf8): RawUtf8;
begin
  VariantToUtf8(GetExistingValueAt(key, 'U')^, result);
end;

procedure TDocDict.SetB(const key: RawUtf8; const value: boolean);
begin
  SetValueAt(key, value);
end;

procedure TDocDict.SetC(const key: RawUtf8; const value: currency);
begin
  SetValueAt(key, value);
end;

procedure TDocDict.SetD(const key: RawUtf8; const value: IDocDict);
begin
  if value = nil then
    EDocDict.Error('D', key, Null);
  SetValueAt(key, PVariant(value.Value)^)
end;

procedure TDocDict.SetF(const key: RawUtf8; const value: double);
begin
  SetValueAt(key, value);
end;

procedure TDocDict.SetI(const key: RawUtf8; const value: Int64);
begin
  SetValueAt(key, value);
end;

procedure TDocDict.SetItem(const key: RawUtf8; const value: variant);
begin
  SetValueAt(key, value);
end;

procedure TDocDict.SetL(const key: RawUtf8; const value: IDocList);
begin
  if value = nil then
    EDocDict.Error('D', key, Null);
  SetValueAt(key, PVariant(value.Value)^)
end;

procedure TDocDict.SetS(const key: RawUtf8; const value: string);
var
  v: variant;
begin
  StringToVariant(value, v);
  SetValueAt(key, v);
end;

procedure TDocDict.SetU(const key: RawUtf8; const value: RawUtf8);
var
  v: variant;
begin
  RawUtf8ToVariant(value, v);
  SetValueAt(key, v);
end;

function TDocDict.Get(const key: RawUtf8): variant;
var
  v: PVariant;
begin
  if GetValueAt(key, v) then
    result := v^
  else
    VarClear(result);
end;

function TDocDict.GetDef(const key: RawUtf8; const default: variant): variant;
var
  v: PVariant;
begin
  if GetValueAt(key, v) then
    result := v^
  else
    result := default;
end;

function TDocDict.GetDef(const key: RawUtf8; const default: RawUtf8): variant;
var
  v: PVariant;
begin
  if GetValueAt(key, v) then
    result := v^
  else
    RawUtf8ToVariant(default, result);
end;

function TDocDict.Get(const key: RawUtf8; var value: variant): boolean;
var
  v: PVariant;
begin
  result := GetValueAt(key, v);
  if result then
    value := v^;
end;

function TDocDict.Get(const key: RawUtf8; var value: RawUtf8): boolean;
var
  v: PVariant;
begin
  result := GetValueAt(key, v);
  if result then
    VariantToUtf8(v^, value, {wasstring=}result);
end;

function TDocDict.Get(const key: RawUtf8; var value: string): boolean;
var
  v: PVariant;
begin
  result := GetValueAt(key, v);
  if result then
    VariantToString(v^, value);
end;

function TDocDict.Get(const key: RawUtf8; var value: boolean): boolean;
var
  v: PVariant;
begin
  result := GetValueAt(key, v) and
            VariantToBoolean(v^, value);
end;

function TDocDict.Get(const key: RawUtf8; var value: integer): boolean;
var
  v: PVariant;
begin
  result := GetValueAt(key, v) and
            VariantToInteger(v^, value);
end;

function TDocDict.Get(const key: RawUtf8; var value: Int64): boolean;
var
  v: PVariant;
begin
  result := GetValueAt(key, v) and
            VariantToInt64(v^, value);
end;

function TDocDict.Get(const key: RawUtf8; var value: double): boolean;
var
  v: PVariant;
begin
  result := GetValueAt(key, v) and
            VariantToDouble(v^, value);
end;

function TDocDict.Get(const key: RawUtf8; var value: currency): boolean;
var
  v: PVariant;
begin
  result := GetValueAt(key, v) and
            VariantToCurrency(v^, value);
end;

function TDocDict.Get(const key: RawUtf8; var value: IDocList): boolean;
var
  v: PVariant;
  dv: PDocVariantData;
begin
  result := GetValueAt(key, v) and
            _SafeArray(v^, dv);
  if result then
    value := TDocList.CreateByRef(dv);
end;

function TDocDict.Get(const key: RawUtf8; var value: IDocDict): boolean;
var
  v: PVariant;
  dv: PDocVariantData;
begin
  result := GetValueAt(key, v) and
            _SafeObject(v^, dv);
  if result then
    value := TDocDict.CreateByRef(dv);
end;

function TDocDict.Get(const key: RawUtf8; var value: PDocVariantData): boolean;
var
  v: PVariant;
begin
  result := GetValueAt(key, v) and
            _Safe(v^, value);
end;

function TDocDict.Copy: IDocDict;
var
  v: TDocDict;
begin
  v := TDocDict.CreateCopy(fValue^);
  v.fPathDelim := fPathDelim; // also include additional parameters
  v.fSorted := fSorted;
  result := v;
end;

function TDocDict.Del(const key: RawUtf8): boolean;
begin
  result := PopAt(key, nil);
end;

function TDocDict.Exists(const key: RawUtf8): boolean;
begin
  if fPathDelim = #0 then
    result := fValue^.GetVarData(key, fSorted) <> nil // faster
  else
    result := fValue^.GetPVariantByPath(key, fPathDelim) <> nil;
end;

function TDocDict.Pop(const key: RawUtf8): variant;
begin
  if not PopAt(key, @result) then
    raise EDocDict.CreateUtf8('Pop with unknown key [%]', [key]);
end;

function TDocDict.Pop(const key: RawUtf8; const default: variant): variant;
begin
  if not PopAt(key, @result) then
    result := default;
end;

function TDocDict.PopItem(out key: RawUtf8; out value: variant;
  position: integer): boolean;
begin
  result := fValue^.Extract(position, value, @key);
end;

function TDocDict.Reduce(const keys: array of RawUtf8): IDocDict;
begin
  result := TDocDict.CreateOwned;
  fValue^.Reduce(keys, fValue^.IsCaseSensitive, result.Value^);
end;

function TDocDict.SetDefault(const key: RawUtf8): variant;
begin
  result := SetDefault(key, Null);
end;

function TDocDict.SetDefault(const key: RawUtf8; const default: variant): variant;
begin
  if Get(key, result) then
    exit;
  SetValueAt(key, default);
  result := default;
end;

procedure TDocDict.Sort(reverse: boolean; keycompare: TUtf8Compare);
begin
  if not Assigned(keycompare) then
    keycompare := StrCompByCase[fValue^.IsCaseSensitive];
  fValue^.SortByName(keycompare, reverse);
  if reverse then
    fSorted := nil
  else
    fSorted := keycompare; // for O(log(n)) binary search on key lookup
end;

procedure TDocDict.Update(const key: RawUtf8; const value: variant);
begin
  SetValueAt(key, value);
end;

procedure TDocDict.Update(const keyvalues: array of const);
begin
  fValue^.Update(keyvalues);
end;

procedure TDocDict.Update(const source: IDocDict; addonlymissing: boolean);
begin
  if source <> nil then
    fValue^.AddOrUpdateFrom(PVariant(source.Value)^, addonlymissing);
end;

{$ifdef HASIMPLICITOPERATOR}

function TDocDict.GetV(const key: RawUtf8): TDocValue;
begin
  result.V := GetExistingValueAt(key, 'V');
end;

function TDocDict.GetEnumerator: TDocDictEnumerator;
var
  v: PVariant;
begin
  v := pointer(fValue^.VValue);
  result.Curr.Value.V := v;
  result.AfterValue.V := v;
  if v = nil then
    exit;
  inc(result.AfterValue.V, fValue^.VCount);
  result.Curr.Key.V := pointer(fValue^.VName);
  dec(result.Curr.Value.V); // for the first MoveNext
  dec(result.Curr.Key.V);
end;

function TDocDict.Keys: TDocKeyEnumerator;
var
  v: PRawUtf8;
begin
  v := pointer(fValue^.VName);
  result.Curr.V := v;
  result.After.V := v;
  if v = nil then
    exit;
  inc(result.After.V, fValue^.VCount);
  dec(result.Curr.V); // for the first MoveNext
end;

function TDocDict.Values: TDocValueEnumerator;
begin
  SetValueEnumerator(fValue, result{%H-}); // shared with IDocList
end;

{$endif HASIMPLICITOPERATOR}

procedure InitializeVariantsJson;
begin
  // called from mormot.core.json once TRttiJson is set as global RTTI class
  TDocList.RegisterToRtti(TypeInfo(IDocList));
  TDocDict.RegisterToRtti(TypeInfo(IDocDict));
end;


var
  // naive but efficient type cache - e.g. for TBsonVariant or TQuickJsVariant
  LastDispInvoke: TSynInvokeableVariantType;

// sysdispinvoke() replacement to meet TSynInvokeableVariantType expectations
procedure NewDispInvoke(Dest: PVarData;
{$ifdef FPC_VARIANTSETVAR}
  var Source: TVarData;
{$else} // see http://mantis.freepascal.org/view.php?id=26773
  const Source: TVarData; // "[ref] const" on modern Delphi
{$endif FPC_VARIANTSETVAR}
  CallDesc: PCallDesc; Params: pointer); cdecl;
// warning: Delphi OSX64 LINUX ANDROID64 expects Params := @VAList
var
  v: TVarData;
  vp: PVariant;
  t: cardinal;
  ct: TSynInvokeableVariantType;
label
  direct;
begin
  t := Source.vType;
  if t = varVariantByRef then
    NewDispInvoke(Dest, PVarData(Source.VPointer)^, calldesc, params)
  else
  begin
    TRttiVarData(v).VType := varEmpty;
    vp := @v;
    if Dest = nil then
      vp := nil;
    ct := nil;
    try
      case t of
        varDispatch,
        varAny,
        varUnknown,
        varDispatch or varByRef,
        varAny or varByRef,
        varUnknown or varByRef:
          if Assigned(VarDispProc) and
             Assigned(VarCopyProc) then
            // standard Windows ComObj unit call
            VarDispProc(vp, variant(Source), CallDesc, Params)
          else
            VarInvalidOp;
        CFirstUserType .. varTypeMask:
          begin
            ct := DocVariantType; // recognize our TDocVariant
            if t = ct.VarType then
              goto direct;
            ct := LastDispInvoke; // atomic pointer load
            if (ct <> nil) and
               (ct.VarType = t) then
              // most calls are grouped within the same custom variant type
              goto direct;
            // FindCustomVariantType() is O(1) but has a global lock
            if FindCustomVariantType(t, TCustomVariantType(ct)) then
              if ct.InheritsFrom(TSynInvokeableVariantType) then
              begin
                // direct access of our custom variants without any temp copy
                LastDispInvoke := ct;
direct:         if Dest <> nil then
                  VarClear(PVariant(Dest)^); // no temp copy, but Dest cleanup
                ct.DispInvoke(Dest, Source, CallDesc, Params);
                Dest := nil;
              end
              else if ct.InheritsFrom(TInvokeableVariantType) then
                // use standard RTL behavior for non-mORMot custom variants
                ct.DispInvoke(pointer(vp), Source, CallDesc, Params)
              else
                VarInvalidOp
            else
              VarInvalidOp;
          end;
      else
        VarInvalidOp;
      end;
    finally
      if Dest <> nil then
      begin
        if (ct <> nil) and
           (v.VType = ct.VarType) then // don't search twice if we got it
          ct.Copy(Dest^, v, {indirect=}false)
        else
          VarCopyProc(Dest^, v);
        VarClear(vp^);
      end;
    end;
  end;
end;

const
  // _CMP2SORT[] comparison of simple types - as copied to _VARDATACMP[]
  _VARDATACMPNUM1: array[varEmpty..varDate] of byte = (
    1, 1, 2, 3, 4, 5, 6, 7);
  _VARDATACMPNUM2: array[varShortInt..varWord64] of byte = (
    8, 9, 10, 11, 12, 13);

procedure InitializeUnit;
var
  vm: TVariantManager; // available since Delphi 7
  vt: cardinal;
  ins: boolean;
  i: PtrUInt;
  {$ifdef FPC}
  test: variant;
  {$endif FPC}
begin
  // register the TDocVariant custom type
  DocVariantType := TDocVariant(SynRegisterCustomVariantType(TDocVariant));
  vt := DocVariantType.VarType;
  DocVariantVType := vt;
  PCardinal(@DV_FAST[dvUndefined])^ := vt;
  PCardinal(@DV_FAST[dvArray])^ := vt;
  PCardinal(@DV_FAST[dvObject])^ := vt;
  assert({%H-}SynVariantTypes[0].VarType = vt);
  PDocVariantData(@DV_FAST[dvUndefined])^.VOptions := JSON_FAST;
  PDocVariantData(@DV_FAST[dvArray])^.VOptions := JSON_FAST + [dvoIsArray];
  PDocVariantData(@DV_FAST[dvObject])^.VOptions := JSON_FAST + [dvoIsObject];
  // FPC allows to define variables with absolute JSON_[...] but Delphi doesn't
  JSON_FAST_STRICT := JSON_[mFastStrict];
  JSON_FAST_EXTENDED := JSON_[mFastExtended];
  JSON_FAST_EXTENDEDINTERN := JSON_[mFastExtendedIntern];
  JSON_NAMEVALUE := PDocVariantOptionsBool(@JSON_[mNameValue])^;
  JSON_NAMEVALUEINTERN := PDocVariantOptionsBool(@JSON_[mNameValueIntern])^;
  JSON_OPTIONS := PDocVariantOptionsBool(@JSON_[mDefault])^;
  // redirect to the feature complete variant wrapper functions
  BinaryVariantLoadAsJson := _BinaryVariantLoadAsJson;
  VariantClearSeveral := _VariantClearSeveral;
  _VariantSaveJson := @__VariantSaveJson;
  SortDynArrayVariantComp := pointer(@FastVarDataComp);
  // setup FastVarDataComp() efficient lookup comparison functions
  for ins := false to true do
  begin
    for i := low(_VARDATACMPNUM1) to high(_VARDATACMPNUM1) do
      _VARDATACMP[i, ins] := _VARDATACMPNUM1[i];
    _VARDATACMP[varBoolean, ins] := 14;
    for i := low(_VARDATACMPNUM2) to high(_VARDATACMPNUM2) do
      _VARDATACMP[i, ins] := _VARDATACMPNUM2[i];
  end;
  _VARDATACMP[varString, false] := 15;
  _VARDATACMP[varString, true]  := 16;
  _VARDATACMP[varOleStr, false] := 17;
  _VARDATACMP[varOleStr, true]  := 18;
  {$ifdef HASVARUSTRING}
  _VARDATACMP[varUString, false] := 17;
  _VARDATACMP[varUString, true]  := 18;
  {$endif HASVARUSTRING}
  // patch DispInvoke for performance and to circumvent RTL inconsistencies
  GetVariantManager(vm);
  vm.DispInvoke := NewDispInvoke;
  SetVariantManager(vm);
  {$ifdef FPC}
  // circumvent FPC 3.2+ inverted parameters order - may be fixed in later FPC
  test := _ObjFast([]);
  try
    test.Add('nam', 'val'); // late binding DispInvoke() call
    DispInvokeArgOrderInverted := (_Safe(test)^.Names[0] = 'val');
  except // paranoid to avoid fatal exception during process initialization
  end;
  {$endif FPC}
end;


initialization
  InitializeUnit;

end.

