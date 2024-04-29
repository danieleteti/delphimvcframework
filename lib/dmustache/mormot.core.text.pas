/// Framework Core Low-Level Text Processing
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.text;

{
  *****************************************************************************

   Text Processing functions shared by all framework units
    - CSV-like Iterations over Text Buffers
    - TTextWriter parent class for Text Generation
    - Numbers (integers or floats) and Variants to Text Conversion
    - Text Formatting functions
    - Resource and Time Functions
    - ESynException class
    - Hexadecimal Text And Binary Conversion

  *****************************************************************************
}

interface

{$I mormot.defines.inc}

uses
  classes,
  contnrs,
  types,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode;



{ ************ CSV-like Iterations over Text Buffers }

/// return true if IdemPChar(source,searchUp) matches, and retrieve the value item
// - typical use may be:
// ! if IdemPCharAndGetNextItem(P,
// !   'CONTENT-DISPOSITION: FORM-DATA; NAME="',Name,'"') then ...
function IdemPCharAndGetNextItem(var source: PUtf8Char; const searchUp: RawUtf8;
  var Item: RawUtf8; Sep: AnsiChar = #13): boolean;

/// return next CSV string from P
// - P=nil after call when end of text is reached
function GetNextItem(var P: PUtf8Char; Sep: AnsiChar = ','): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// return next CSV string from P
// - P=nil after call when end of text is reached
procedure GetNextItem(var P: PUtf8Char; Sep: AnsiChar;
  var result: RawUtf8); overload;

/// return next CSV string (unquoted if needed) from P
// - P=nil after call when end of text is reached
procedure GetNextItem(var P: PUtf8Char; Sep, Quote: AnsiChar;
  var result: RawUtf8); overload;

/// return next CSV string from P from several separator characters
// - P=nil after call when end of text is reached
// - returns the character which ended the result string, i.e. #0 or one of Sep
function GetNextItemMultiple(var P: PUtf8Char; const Sep: RawUtf8;
  var Next: RawUtf8): AnsiChar; overload;

/// return trimmed next CSV string from P
// - P=nil after call when end of text is reached
procedure GetNextItemTrimed(var P: PUtf8Char; Sep: AnsiChar;
  var result: RawUtf8);

/// return next CRLF separated value string from P, ending #10 or #13#10 trimmed
// - any kind of line feed (CRLF or LF) will be handled, on all operating systems
// - as used e.g. by TSynNameValue.InitFromCsv and TDocVariantData.InitCsv
// - P=nil after call when end of text is reached
procedure GetNextItemTrimedCRLF(var P: PUtf8Char; var result: RawUtf8);

/// return next CSV string from P, nil if no more
// - this function returns the RTL string type of the compiler, and
// therefore can be used with ready to be displayed text (e.g. for the UI)
function GetNextItemString(var P: PChar; Sep: Char = ','): string;

/// extract a file extension from a file name, then compare with a comma
// separated list of extensions
// - e.g. GetFileNameExtIndex('test.log','exe,log,map')=1
// - will return -1 if no file extension match
// - will return any matching extension, starting count at 0
// - extension match is case-insensitive
function GetFileNameExtIndex(const FileName, CsvExt: TFileName): integer;

/// return next CSV string from P, nil if no more
// - output text would be trimmed from any left or right space
// - will always append a trailing #0 - excluded from Dest length (0..254)
procedure GetNextItemShortString(var P: PUtf8Char; Dest: PShortString;
  Sep: AnsiChar = ',');

/// append some text lines with the supplied Values[]
// - if any Values[] item is '', no line is added
// - otherwise, appends 'Caption: Value', with Caption taken from CSV
procedure AppendCsvValues(const Csv: string; const Values: array of string;
  var result: string; const AppendBefore: string = #13#10);

/// return a CSV list of the iterated same value
// - e.g. CsvOfValue('?',3)='?,?,?'
function CsvOfValue(const Value: RawUtf8; Count: cardinal; const Sep: RawUtf8 = ','): RawUtf8;

 /// retrieve the next CSV separated bit index
// - each bit was stored as BitIndex+1, i.e. 0 to mark end of CSV chunk
// - several bits set to one can be regrouped via 'first-last,' syntax
procedure SetBitCsv(var Bits; BitsCount: integer; var P: PUtf8Char);

/// convert a set of bit into a CSV content
// - each bit is stored as BitIndex+1, and separated by a ','
// - several bits set to one can be regrouped via 'first-last,' syntax
// - ',0' is always appended at the end of the CSV chunk to mark its end
function GetBitCsv(const Bits; BitsCount: integer): RawUtf8;

/// decode next CSV hexadecimal string from P, nil if no more or not matching BinBytes
// - Bin is filled with 0 if the supplied CSV content is invalid
// - if Sep is #0, it will read the hexadecimal chars until a whitespace is reached
function GetNextItemHexDisplayToBin(var P: PUtf8Char; Bin: PByte; BinBytes: PtrInt;
  Sep: AnsiChar = ','): boolean;

type
  /// some stack-allocated zero-terminated character buffer
  // - as used by GetNextTChar64
  TChar64 = array[0..63] of AnsiChar;

/// return next CSV string from P as a #0-ended buffer, false if no more
// - if Sep is #0, will copy all characters until next whitespace char
// - returns the number of bytes stored into Buf[]
function GetNextTChar64(var P: PUtf8Char; Sep: AnsiChar; out Buf: TChar64): PtrInt;

/// return next CSV string as unsigned integer from P, 0 if no more
// - if Sep is #0, it won't be searched for
function GetNextItemCardinal(var P: PUtf8Char; Sep: AnsiChar = ','): PtrUInt;

/// return next CSV string as signed integer from P, 0 if no more
// - if Sep is #0, it won't be searched for
function GetNextItemInteger(var P: PUtf8Char; Sep: AnsiChar = ','): PtrInt;

/// return next CSV string as 64-bit signed integer from P, 0 if no more
// - if Sep is #0, it won't be searched for
function GetNextItemInt64(var P: PUtf8Char; Sep: AnsiChar = ','): Int64;

/// return next CSV string as 64-bit unsigned integer from P, 0 if no more
// - if Sep is #0, it won't be searched for
function GetNextItemQWord(var P: PUtf8Char; Sep: AnsiChar = ','): QWord;

/// return next CSV hexadecimal string as 64-bit unsigned integer from P
// - returns 0 if no valid hexadecimal text is available in P
// - if Sep is #0, it won't be searched for
// - will first fill the 64-bit value with 0, then decode each two hexadecimal
// characters available in P
// - could be used to decode TTextWriter.AddBinToHexDisplayMinChars() output
function GetNextItemHexa(var P: PUtf8Char; Sep: AnsiChar = ','): QWord;

/// return next CSV string as unsigned integer from P, 0 if no more
// - P^ will point to the first non digit character (the item separator, e.g.
// ',' for CSV)
function GetNextItemCardinalStrict(var P: PUtf8Char): PtrUInt;

/// return next CSV string as unsigned integer from P, 0 if no more
// - this version expects P^ to point to an Unicode char array
function GetNextItemCardinalW(var P: PWideChar; Sep: WideChar = ','): PtrUInt;

/// return next CSV string as double from P, 0.0 if no more
// - if Sep is #0, will return all characters until next whitespace char
function GetNextItemDouble(var P: PUtf8Char; Sep: AnsiChar = ','): double;

/// return next CSV string as currency from P, 0.0 if no more
// - if Sep is #0, will return all characters until next whitespace char
function GetNextItemCurrency(var P: PUtf8Char; Sep: AnsiChar = ','): currency; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// return next CSV string as currency from P, 0.0 if no more
// - if Sep is #0, will return all characters until next whitespace char
procedure GetNextItemCurrency(var P: PUtf8Char; out result: currency;
  Sep: AnsiChar = ','); overload;

/// return n-th indexed CSV string in P, starting at Index=0 for first one
function GetCsvItem(P: PUtf8Char; Index: PtrUInt; Sep: AnsiChar = ','): RawUtf8; overload;

/// return n-th indexed CSV string (unquoted if needed) in P, starting at Index=0 for first one
function GetUnQuoteCsvItem(P: PUtf8Char; Index: PtrUInt; Sep: AnsiChar = ',';
  Quote: AnsiChar = ''''): RawUtf8; overload;

/// return n-th indexed CSV string in P, starting at Index=0 for first one
// - this function return the RTL string type of the compiler, and
// therefore can be used with ready to be displayed text
function GetCsvItemString(P: PChar; Index: PtrUInt; Sep: Char = ','): string;

/// return first CSV string in the supplied UTF-8 content
function GetFirstCsvItem(const Csv: RawUtf8; Sep: AnsiChar = ','): RawUtf8;
  {$ifdef HASINLINE} inline; {$endif}

/// return last CSV string in the supplied UTF-8 content
function GetLastCsvItem(const Csv: RawUtf8; Sep: AnsiChar = ','): RawUtf8;
  {$ifdef HASINLINE} inline; {$endif}

/// quickly check if Value is in Csv with no temporary memory allocation
function CsvContains(const Csv, Value: RawUtf8; Sep: AnsiChar = ',';
  CaseSensitive: boolean = true): boolean;

/// return the index of a Value in a CSV string
// - start at Index=0 for first one
// - return -1 if specified Value was not found in CSV items
function FindCsvIndex(Csv: PUtf8Char; const Value: RawUtf8; Sep: AnsiChar = ',';
  CaseSensitive: boolean = true; TrimValue: boolean = false): integer;

/// add the strings in the specified CSV text into a dynamic array of UTF-8 strings
// - warning: will add the strings, so List := nil may be needed before call
procedure CsvToRawUtf8DynArray(Csv: PUtf8Char; var List: TRawUtf8DynArray;
  Sep: AnsiChar = ','; TrimItems: boolean = false; AddVoidItems: boolean = false;
  Quote: AnsiChar = #0); overload;

/// add the strings in the specified CSV text into a dynamic array of UTF-8 strings
// - warning: will add the strings, so List := nil may be needed before call
procedure CsvToRawUtf8DynArray(const Csv, Sep, SepEnd: RawUtf8;
  var List: TRawUtf8DynArray); overload;

/// convert the strings in the specified CSV text into a dynamic array of UTF-8 strings
function CsvToRawUtf8DynArray(const Csv: RawUtf8; const Sep: RawUtf8 = ',';
  const SepEnd: RawUtf8 = ''): TRawUtf8DynArray; overload;

/// return the corresponding CSV text from a dynamic array of UTF-8 strings
function RawUtf8ArrayToCsv(const Values: array of RawUtf8;
  const Sep: RawUtf8 = ','; HighValues: integer = -1): RawUtf8;

/// return the corresponding CSV quoted text from a dynamic array of UTF-8 strings
// - apply QuoteStr() function to each Values[] item
function RawUtf8ArrayToQuotedCsv(const Values: array of RawUtf8;
  const Sep: RawUtf8 = ','; Quote: AnsiChar = ''''): RawUtf8;

/// append some prefix to all CSV values
// ! AddPrefixToCsv('One,Two,Three','Pre')='PreOne,PreTwo,PreThree'
function AddPrefixToCsv(Csv: PUtf8Char; const Prefix: RawUtf8;
  Sep: AnsiChar = ','): RawUtf8;

/// append a Value to a CSV string
procedure AddToCsv(const Value: RawUtf8; var Csv: RawUtf8; const Sep: RawUtf8 = ',');
  {$ifdef HASINLINE}inline;{$endif}

/// change a Value within a CSV string
function RenameInCsv(const OldValue, NewValue: RawUtf8; var Csv: RawUtf8;
  const Sep: RawUtf8 = ','): boolean;

/// recognize #9 ';' or ',' as separator in a CSV text
// - to implement a separator-tolerant CSV parser
function CsvGuessSeparator(const Csv: RawUtf8): AnsiChar;

/// append the strings in the specified CSV text into a dynamic array of integer
procedure CsvToIntegerDynArray(Csv: PUtf8Char; var List: TIntegerDynArray;
  Sep: AnsiChar = ',');

/// append the strings in the specified CSV text into a dynamic array of integer
procedure CsvToInt64DynArray(Csv: PUtf8Char; var List: TInt64DynArray;
  Sep: AnsiChar = ','); overload;

/// convert the strings in the specified CSV text into a dynamic array of integer
function CsvToInt64DynArray(Csv: PUtf8Char; Sep: AnsiChar = ','): TInt64DynArray; overload;

/// return the corresponding CSV text from a dynamic array of 32-bit integer
// - you can set some custom Prefix and Suffix text
function IntegerDynArrayToCsv(Values: PIntegerArray; ValuesCount: integer;
  const Prefix: RawUtf8 = ''; const Suffix: RawUtf8 = '';
  InlinedValue: boolean = false; SepChar: AnsiChar = ','): RawUtf8; overload;

/// return the corresponding CSV text from a dynamic array of 32-bit integer
// - you can set some custom Prefix and Suffix text
function IntegerDynArrayToCsv(const Values: TIntegerDynArray;
  const Prefix: RawUtf8 = ''; const Suffix: RawUtf8 = '';
  InlinedValue: boolean = false; SepChar: AnsiChar = ','): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// return the corresponding CSV text from a dynamic array of 64-bit integers
// - you can set some custom Prefix and Suffix text
function Int64DynArrayToCsv(Values: PInt64Array; ValuesCount: integer;
  const Prefix: RawUtf8 = ''; const Suffix: RawUtf8 = '';
  InlinedValue: boolean = false; SepChar: AnsiChar = ','): RawUtf8; overload;

/// return the corresponding CSV text from a dynamic array of 64-bit integers
// - you can set some custom Prefix and Suffix text
function Int64DynArrayToCsv(const Values: TInt64DynArray;
  const Prefix: RawUtf8 = ''; const Suffix: RawUtf8 = '';
  InlinedValue: boolean = false; SepChar: AnsiChar = ','): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}


{ ************ TTextWriter parent class for Text Generation }

type
  /// event signature for TTextWriter.OnFlushToStream callback
  TOnTextWriterFlush = procedure(Text: PUtf8Char; Len: PtrInt) of object;

  /// defines how text is to be added into TTextWriter / TJsonWriter
  // - twNone will write the supplied text with no escaping
  // - twJsonEscape will properly escape " and \ as expected by JSON
  // - twOnSameLine will convert any line feeds or control chars into spaces
  TTextWriterKind = (
    twNone,
    twJsonEscape,
    twOnSameLine);

  /// available global options for a TTextWriter / TTextWriter instance
  // - TTextWriter.WriteObject() method behavior would be set via their own
  // TTextWriterWriteObjectOptions, and work in conjunction with those settings
  // - twoStreamIsOwned would be set if the associated TStream is owned by the
  // TTextWriter instance  - as a TRawByteStringStream if twoStreamIsRawByteString
  // - twoFlushToStreamNoAutoResize would forbid FlushToStream to resize the
  // internal memory buffer when it appears undersized - FlushFinal will set it
  // before calling a last FlushToStream
  // - by default, custom serializers set via TRttiJson.RegisterCustomSerializer()
  // would let AddRecordJson() and AddDynArrayJson() write enumerates and sets
  // as integer numbers, unless twoEnumSetsAsTextInRecord or
  // twoEnumSetsAsBooleanInRecord (exclusively) are set - for Mustache data
  // context, twoEnumSetsAsBooleanInRecord will return a JSON object with
  // "setname":true/false fields
  // - variants and nested objects would be serialized with their default
  // JSON serialization options, unless twoForceJsonExtended or
  // twoForceJsonStandard is defined
  // - when enumerates and sets are serialized as text into JSON, you may force
  // the identifiers to be left-trimed for all their lowercase characters
  // (e.g. sllError -> 'Error') by setting twoTrimLeftEnumSets: this option
  // may default to the deprecated global TTextWriter.SetDefaultEnumTrim setting
  // - twoEndOfLineCRLF would reflect the TEchoWriter.EndOfLineCRLF property
  // - twoBufferIsExternal would be set if the temporary buffer is not handled
  // by the instance, but specified at constructor, maybe from the stack
  // - twoIgnoreDefaultInRecord will force custom record serialization to avoid
  // writing the fields with default values, i.e. enable soWriteIgnoreDefault
  // when published properties are serialized
  // - twoDateTimeWithZ appends an ending 'Z' to TDateTime/TDateTimeMS values
  // - twoNonExpandedArrays will force the 'non expanded' optimized JSON layout
  // for array of records or classes, ignoring other formatting options:
  // $ {"fieldCount":2,"values":["f1","f2","1v1",1v2,"2v1",2v2...],"rowCount":20}
  // - twoNoSharedStream will force to create a new stream for each instance
  // - twoNoWriteToStreamException let TTextWriter.WriteToStream silently fail
  TTextWriterOption = (
    twoStreamIsOwned,
    twoStreamIsRawByteString,
    twoFlushToStreamNoAutoResize,
    twoEnumSetsAsTextInRecord,
    twoEnumSetsAsBooleanInRecord,
    twoFullSetsAsStar,
    twoTrimLeftEnumSets,
    twoForceJsonExtended,
    twoForceJsonStandard,
    twoEndOfLineCRLF,
    twoBufferIsExternal,
    twoIgnoreDefaultInRecord,
    twoDateTimeWithZ,
    twoNonExpandedArrays,
    twoNoSharedStream,
    twoNoWriteToStreamException);

  /// options set for a TTextWriter / TTextWriter instance
  // - allows to override e.g. AddRecordJson() and AddDynArrayJson() behavior;
  // or set global process customization for a TTextWriter
  TTextWriterOptions = set of TTextWriterOption;

  /// may be used to allocate on stack a 8KB work buffer for a TTextWriter
  // - via the TTextWriter.CreateOwnedStream overloaded constructor
  TTextWriterStackBuffer = array[0..8191] of AnsiChar;
  PTextWriterStackBuffer = ^TTextWriterStackBuffer;

  /// available options for TTextWriter.WriteObject() method
  // - woHumanReadable will add some line feeds and indentation to the content,
  // to make it more friendly to the human eye
  // - woDontStoreDefault (which is set by default for WriteObject method) will
  // avoid serializing properties including a default value (JsonToObject function
  // will set the default values, so it may help saving some bandwidth or storage)
  // - woFullExpand will generate a debugger-friendly layout, including instance
  // class name, sets/enumerates as text, and reference pointer - as used by
  // TSynLog and ObjectToJsonFull()
  // - woStoreClassName will add a "ClassName":"TMyClass" field
  // - woStorePointer will add a "Address":"0431298A" field, and .map/.dbg/.mab
  // source code line number corresponding to ESynException.RaisedAt
  // - woStoreStoredFalse will write the 'stored false' properties, even
  // if they are marked as such (used e.g. to persist all settings on file,
  // but disallow the sensitive - password - fields be logged)
  // - woHumanReadableFullSetsAsStar will store an human-readable set with
  // all its enumerates items set to be stored as ["*"]
  // - woHumanReadableEnumSetAsComment will add a comment at the end of the
  // line, containing all available values of the enumaration or set, e.g:
  // $ "Enum": "Destroying", // Idle,Started,Finished,Destroying
  // - woEnumSetsAsText will store sets and enumerables as text (is also
  // included in woFullExpand or woHumanReadable)
  // - woDateTimeWithMagic will append the JSON_SQLDATE_MAGIC_C (i.e. U+FFF1)
  // before the ISO-8601 encoded TDateTime value
  // - woDateTimeWithZSuffix will append the Z suffix to the ISO-8601 encoded
  // TDateTime value, to identify the content as strict UTC value
  // - TTimeLog would be serialized as Int64, unless woTimeLogAsText is defined
  // - since TOrm.ID could be huge Int64 numbers, they may be truncated
  // on client side, e.g. to 53-bit range in JavaScript: you could define
  // woIDAsIDstr to append an additional "ID_str":"##########" field
  // - by default, RawBlob properties are serialized as null, unless
  // woRawBlobAsBase64 is defined or a custom serialization is used (e.g. TOrm)
  // - if woHideSensitivePersonalInformation is set, rcfSpi types (e.g. the
  // TSynPersistentWithPassword.Password field) will be serialized as "***"
  // to prevent security issues (e.g. in log)
  // - by default, TObjectList will set the woStoreClassName for its nested
  // objects, unless woObjectListWontStoreClassName is defined
  // - all inherited properties would be serialized, unless woDontStoreInherited
  // is defined, and only the topmost class level properties would be serialized
  // - woInt64AsHex will force Int64/QWord to be written as hexadecimal string -
  // see j2oAllowInt64Hex reverse option fot Json2Object
  // - woDontStoreVoid will avoid serializing numeric properties equal to 0 and
  // string properties equal to '' (replace both deprecated woDontStore0 and
  // woDontStoreEmptyString flags)
  // - woPersistentLock paranoid setting will call TSynPersistentLock.Lock/Unlock
  // during serialization
  TTextWriterWriteObjectOption = (
    woHumanReadable,
    woDontStoreDefault,
    woFullExpand,
    woStoreClassName,
    woStorePointer,
    woStoreStoredFalse,
    woHumanReadableFullSetsAsStar,
    woHumanReadableEnumSetAsComment,
    woEnumSetsAsText,
    woDateTimeWithMagic,
    woDateTimeWithZSuffix,
    woTimeLogAsText,
    woIDAsIDstr,
    woRawBlobAsBase64,
    woHideSensitivePersonalInformation,
    woObjectListWontStoreClassName,
    woDontStoreInherited,
    woInt64AsHex,
    woDontStoreVoid,
    woPersistentLock);

  /// options set for TTextWriter.WriteObject() method
  TTextWriterWriteObjectOptions = set of TTextWriterWriteObjectOption;

  /// the potential places were TJsonWriter.AddHtmlEscape should process
  // proper HTML string escaping, unless hfNone is used
  // $  < > & "  ->   &lt; &gt; &amp; &quote;
  // by default (hfAnyWhere)
  // $  < > &  ->   &lt; &gt; &amp;
  // outside HTML attributes (hfOutsideAttributes)
  // $  & "  ->   &amp; &quote;
  // within HTML attributes (hfWithinAttributes)
  TTextWriterHtmlFormat = (
    hfNone,
    hfAnyWhere,
    hfOutsideAttributes,
    hfWithinAttributes);

  /// the available JSON format, for TTextWriter.AddJsonReformat() and its
  // JsonBufferReformat() and JsonReformat() wrappers
  // - jsonCompact is the default machine-friendly single-line layout
  // - jsonHumanReadable will add line feeds and indentation, for a more
  // human-friendly result
  // - jsonUnquotedPropName will emit the jsonHumanReadable layout, but
  // with all property names being quoted only if necessary: this format
  // could be used e.g. for configuration files - this format, similar to the
  // one used in the MongoDB extended syntax, is not JSON compatible: do not
  // use it e.g. with AJAX clients, but is would be handled as expected by all
  // our units as valid JSON input, without previous correction
  // - jsonUnquotedPropNameCompact will emit single-line layout with unquoted
  // property names, which is the smallest data output within mORMot instances
  // - by default we rely on UTF-8 encoding (which is mandatory in the RFC 8259)
  // but you can use jsonEscapeUnicode to produce pure 7-bit ASCII output,
  // with \u#### escape of non-ASCII chars, e.g. as default python json.dumps
  // - jsonNoEscapeUnicode will search for any \u#### pattern and generate pure
  // UTF-8 output instead
  // - those features are not implemented in this unit, but in mormot.core.json
  TTextWriterJsonFormat = (
    jsonCompact,
    jsonHumanReadable,
    jsonUnquotedPropName,
    jsonUnquotedPropNameCompact,
    jsonEscapeUnicode,
    jsonNoEscapeUnicode);

  /// parent to T*Writer text processing classes, with the minimum set of methods
  // - use an internal buffer, so much faster than naive string+string
  // - see TTextDateWriter in mormot.core.datetime for date/time methods
  // - see TJsonWriter in mormot.core.json for proper JSON support
  // - see TResultsWriter in mormot.db.core for SQL resultset export
  // - see TOrmWriter in mormot.orm.core for ORM oriented serialization
  // - note: mORMot 1.18 TTextWriter.RegisterCustomJSONSerializerFromText()
  // are moved into Rtti.RegisterFromText() as other similar methods
  TTextWriter = class
  protected
    fStream: TStream;
    fInitialStreamPosition: PtrUInt;
    fTotalFileSize: PtrUInt;
    fHumanReadableLevel: integer;
    // internal temporary buffer
    fTempBufSize: integer;
    fTempBuf: PUtf8Char;
    fOnFlushToStream: TOnTextWriterFlush;
    fCustomOptions: TTextWriterOptions;
    function GetTextLength: PtrUInt;
    procedure SetStream(aStream: TStream);
    procedure SetBuffer(aBuf: pointer; aBufSize: integer);
    procedure WriteToStream(data: pointer; len: PtrUInt); virtual;
    procedure InternalSetBuffer(aBuf: PUtf8Char; const aBufSize: PtrUInt);
      {$ifdef FPC} inline; {$endif}
  public
    /// direct access to the low-level current position in the buffer
    // - you should not use this field directly
    B: PUtf8Char;
    /// direct access to the low-level last position in the buffer
    // - you should not use this field directly
    // - points in fact to 16 bytes before the buffer ending
    BEnd: PUtf8Char;
    /// the data will be written to the specified Stream
    // - aStream may be nil: in this case, it MUST be set before using any
    // Add*() method
    // - default internal buffer size if 8192
    constructor Create(aStream: TStream; aBufSize: integer = 8192); overload;
    /// the data will be written to the specified Stream
    // - aStream may be nil: in this case, it MUST be set before using any
    // Add*() method
    // - will use an external buffer (which may be allocated on stack)
    constructor Create(aStream: TStream; aBuf: pointer; aBufSize: integer); overload;
    /// the data will be written to an internal TRawByteStringStream
    // - default internal buffer size if 4096 (enough for most JSON objects)
    // - consider using a stack-allocated buffer and the overloaded method
    constructor CreateOwnedStream(aBufSize: integer = 4096;
      NoSharedStream: boolean = false); overload;
    /// the data will be written to an internal TRawByteStringStream
    // - will use an external buffer (which may be allocated on stack)
    constructor CreateOwnedStream(aBuf: pointer; aBufSize: integer;
      NoSharedStream: boolean = false); overload;
    /// the data will be written to an internal TRawByteStringStream
    // - will use the stack-allocated TTextWriterStackBuffer if possible
    constructor CreateOwnedStream(var aStackBuf: TTextWriterStackBuffer;
      aBufSize: integer; NoSharedStream: boolean = false); overload;
    /// the data will be written to an internal TRawByteStringStream
    // - will use the stack-allocated TTextWriterStackBuffer
    constructor CreateOwnedStream(var aStackBuf: TTextWriterStackBuffer;
      NoSharedStream: boolean = false); overload;
    /// the data will be written to an external file
    // - you should call explicitly FlushFinal or FlushToStream to write
    // any pending data to the file
    constructor CreateOwnedFileStream(const aFileName: TFileName;
      aBufSize: integer = 16384);
    /// release all internal structures
    // - e.g. free fStream if the instance was owned by this class
    destructor Destroy; override;
    {$ifndef PUREMORMOT2}
    /// allow to override the default (JSON) serialization of enumerations and
    // sets as text, which would write the whole identifier (e.g. 'sllError')
    // - calling SetDefaultEnumTrim(true) would force the enumerations to
    // be trimmed for any lower case char, e.g. sllError -> 'Error'
    // - this is global to the current process, and should be use mainly for
    // compatibility purposes for the whole process
    // - you may change the default behavior by setting twoTrimLeftEnumSets
    // in the TTextWriter.CustomOptions property of a given serializer
    // - note that unserialization process would recognize both formats
    class procedure SetDefaultEnumTrim(aShouldTrimEnumsAsText: boolean);
    {$endif PUREMORMOT2}

    /// write pending data, then retrieve the whole text as a UTF-8 string
    function Text: RawUtf8;
      {$ifdef HASINLINE}inline;{$endif}
    /// write pending data, then retrieve the whole text as a UTF-8 string
    procedure SetText(var result: RawUtf8; reformat: TTextWriterJsonFormat = jsonCompact);
    /// set the internal stream content with the supplied UTF-8 text
    procedure ForceContent(const text: RawUtf8);
    /// write pending data to the Stream, with automatic buffer resizal
    // - you should not have to call FlushToStream in most cases, but FlushFinal
    // at the end of the process, just before using the resulting Stream
    // - FlushToStream may be used to force immediate writing of the internal
    // memory buffer to the destination Stream
    // - you can set FlushToStreamNoAutoResize=true or call FlushFinal if you
    // do not want the automatic memory buffer resizal to take place
    procedure FlushToStream; virtual;
    /// write pending data to the Stream, without automatic buffer resizal
    // - will append the internal memory buffer to the Stream
    // - in short, FlushToStream may be called during the adding process, and
    // FlushFinal at the end of the process, just before using the resulting Stream
    // - if you don't call FlushToStream or FlushFinal, some pending characters
    // may not be copied to the Stream: you should call it before using the Stream
    procedure FlushFinal;
      {$ifdef HASINLINE}inline;{$endif}

    /// append one ASCII char to the buffer
    procedure Add(c: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append one ASCII char to the buffer with no buffer check
    // - to be called after a regular Add(), within the 16 bytes buffer overhead
    procedure AddDirect(c: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append one ASCII char to the buffer with no buffer check
    // - to be called after a regular Add(), within the 16 bytes buffer overhead
    procedure AddDirect(c1, c2: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append one comma (',') character
    // - to be called after a regular Add(), within the 16 bytes buffer overhead
    procedure AddComma;
      {$ifdef HASINLINE}inline;{$endif}
    /// append one ASCII char to the buffer, if not already there as LastChar
    procedure AddOnce(c: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append two chars to the buffer
    procedure Add(c1, c2: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    {$ifdef CPU32} // already implemented by Add(Value: PtrInt) method on CPU64
    /// append a 64-bit signed integer Value as text
    procedure Add(Value: Int64); overload;
    {$endif CPU32}
    /// append a 32-bit signed integer Value as text
    procedure Add(Value: PtrInt); overload;
      {$ifdef FPC_OR_DELPHIXE4}{$ifdef ASMINTEL}inline;{$endif}{$endif} // URW1111
    /// append a boolean Value as text
    // - write either 'true' or 'false'
    procedure Add(Value: boolean); overload;
    /// append a Currency from its Int64 in-memory representation
    // - expects a PInt64 to avoid ambiguity with the AddCurr() method
    procedure AddCurr64(Value: PInt64);
    /// append a Currency value
    // - just an inlined wrapper around AddCurr64(PInt64(@Value))
    procedure AddCurr(const Value: currency); 
      {$ifdef HASINLINE}inline;{$endif}
    /// append an Unsigned 32-bit integer Value as a String
    procedure AddU(Value: cardinal);
      {$ifdef FPC_OR_DELPHIXE4}{$ifdef ASMINTEL}inline;{$endif}{$endif} // URW1111
    /// append an Unsigned 32-bit integer Value as a quoted hexadecimal String
    procedure AddUHex(Value: cardinal; QuotedChar: AnsiChar = '"');
      {$ifdef HASINLINE}inline;{$endif}
    /// append an Unsigned 64-bit integer Value as a String
    procedure AddQ(Value: QWord);
    /// append an Unsigned 64-bit integer Value as a quoted hexadecimal String
    procedure AddQHex(Value: Qword; QuotedChar: AnsiChar = '"');
      {$ifdef HASINLINE}inline;{$endif}
    /// append a GUID value, encoded as text without any {}
    // - will store e.g. '3F2504E0-4F89-11D3-9A0C-0305E82C3301'
    procedure Add(Value: PGuid; QuotedChar: AnsiChar = #0); overload;
    /// append a floating-point Value as a String
    // - write "Infinity", "-Infinity", and "NaN" for corresponding IEEE values
    // - noexp=true will call ExtendedToShortNoExp() to avoid any scientific
    // notation in the resulting text
    procedure AddDouble(Value: double; noexp: boolean = false);
    /// append a floating-point Value as a String
    // - write "Infinity", "-Infinity", and "NaN" for corresponding IEEE values
    // - noexp=true will call ExtendedToShortNoExp() to avoid any scientific
    // notation in the resulting text
    procedure AddSingle(Value: single; noexp: boolean = false);
    /// append a floating-point Value as a String
    // - write "Infinity", "-Infinity", and "NaN" for corresponding IEEE values
    // - noexp=true will call ExtendedToShortNoExp() to avoid any scientific
    // notation in the resulting text
    procedure Add(Value: Extended; precision: integer; noexp: boolean = false); overload;
    /// append a floating-point text buffer
    // - will correct on the fly '.5' -> '0.5' and '-.5' -> '-0.5'
    // - is used when the input comes from a third-party source with no regular
    // output, e.g. a database driver
    procedure AddFloatStr(P: PUtf8Char);
    /// append CR+LF (#13#10) chars
    // - this method won't call TEchoWriter.EchoAdd() registered events - use
    // TEchoWriter.AddEndOfLine() method instead
    // - TEchoWriter.AddEndOfLine() will append either CR+LF (#13#10) or
    // only LF (#10) depending on its internal options
    procedure AddCR;
      {$ifdef HASINLINE}inline;{$endif}
    /// append CR+LF (#13#10) chars and #9 indentation
    // - indentation depth is defined by the HumanReadableLevel value
    procedure AddCRAndIndent; virtual;
    /// write the same character multiple times
    procedure AddChars(aChar: AnsiChar; aCount: PtrInt);
    /// append an integer Value as a 2 digits text with comma
    procedure Add2(Value: PtrUInt);
    /// append an integer Value as a 3 digits text without any comma
    procedure Add3(Value: cardinal);
    /// append an integer Value as a 4 digits text with comma
    procedure Add4(Value: PtrUInt);
    /// append a time period, specified in micro seconds, in 00.000.000 TSynLog format
    procedure AddMicroSec(MicroSec: cardinal);
    /// append an array of integers as CSV
    procedure AddCsvInteger(const Integers: array of integer);
    /// append an array of doubles as CSV
    procedure AddCsvDouble(const Doubles: array of double);
    /// append some UTF-8 chars to the buffer
    // - input length is calculated from zero-ended char
    // - don't escapes chars according to the JSON RFC
    procedure AddNoJsonEscape(P: Pointer); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append some UTF-8 chars to the buffer
    // - don't escapes chars according to the JSON RFC
    // - called by inlined AddNoJsonEscape() if Len >= fTempBufSize
    procedure AddNoJsonEscapeBig(P: Pointer; Len: PtrInt);
    /// append some UTF-8 chars to the buffer - inlined for small content
    // - don't escapes chars according to the JSON RFC
    procedure AddNoJsonEscape(P: Pointer; Len: PtrInt); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append some UTF-8 chars to the buffer
    // - don't escapes chars according to the JSON RFC
    procedure AddNoJsonEscapeUtf8(const text: RawByteString);
      {$ifdef HASINLINE}inline;{$endif}
    /// append some UTF-8 encoded chars to the buffer, from a RTL string type
    // - don't escapes chars according to the JSON RFC
    // - if s is a UnicodeString, will convert UTF-16 into UTF-8
    procedure AddNoJsonEscapeString(const s: string);
    /// append some unicode chars to the buffer
    // - WideCharCount is the unicode chars count, not the byte size; if it is
    // 0, then it will convert until an ending #0 (fastest way)
    // - don't escapes chars according to the JSON RFC
    // - will convert the Unicode chars into UTF-8
    procedure AddNoJsonEscapeW(WideChar: PWord; WideCharCount: integer);
    /// append some Ansi text as UTF-8 chars to the buffer
    // - don't escapes chars according to the JSON RFC
    procedure AddNoJsonEscape(P: PAnsiChar; Len: PtrInt; CodePage: cardinal); overload;
    /// append some UTF-8 content to the buffer, with no JSON escape
    // - if supplied json is '', will write 'null' so that valid JSON is written
    // - redirect to AddNoJsonEscape() otherwise
    procedure AddRawJson(const json: RawJson);
    /// append a line of text with CR+LF at the end
    procedure AddLine(const Text: ShortString);
    /// append some chars to the buffer in one line
    // - P should be ended with a #0
    // - will write #1..#31 chars as spaces (so content will stay on the same line)
    procedure AddOnSameLine(P: PUtf8Char); overload;
    /// append some chars to the buffer in one line
    // - will write #0..#31 chars as spaces (so content will stay on the same line)
    procedure AddOnSameLine(P: PUtf8Char; Len: PtrInt); overload;
    /// append some wide chars to the buffer in one line
    // - will write #0..#31 chars as spaces (so content will stay on the same line)
    procedure AddOnSameLineW(P: PWord; Len: PtrInt);
    /// append some RTL string to the buffer in one line
    // - will write #0..#31 chars as spaces (so content will stay on the same line)
    procedure AddOnSameLineString(const Text: string);
    /// append an UTF-8 String, with no JSON escaping
    procedure AddString(const Text: RawUtf8);
    /// append several UTF-8 strings
    procedure AddStrings(const Text: array of RawUtf8); overload;
    /// append an UTF-8 string several times
    procedure AddStrings(const Text: RawUtf8; count: PtrInt); overload;
    /// append a ShortString
    procedure AddShort(const Text: ShortString); overload;
    /// append a ShortString - or at least a small buffer < 256 chars
    procedure AddShort(Text: PUtf8Char; TextLen: PtrInt); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append a TShort8 - Text should be not '', and up to 8 chars long
    // - this method is aggressively inlined, so may be preferred to AddShort()
    // for appending simple UTF-8 constant text
    procedure AddShorter(const Short8: TShort8);
      {$ifdef HASINLINE}inline;{$endif}
    /// append 'null' as text
    procedure AddNull;
      {$ifdef HASINLINE}inline;{$endif}
    /// append a sub-part of an UTF-8 String
    // - emulates AddString(copy(Text,start,len))
    procedure AddStringCopy(const Text: RawUtf8; start, len: PtrInt);
    /// append after trim first lowercase chars ('otDone' will add 'Done' e.g.)
    procedure AddTrimLeftLowerCase(Text: PShortString);
    /// append a UTF-8 String excluding any space or control char
    // - this won't escape the text as expected by JSON
    procedure AddTrimSpaces(const Text: RawUtf8); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append a #0-terminated UTF-8 buffer excluding any space or control char
    // - this won't escape the text as expected by JSON
    procedure AddTrimSpaces(P: PUtf8Char); overload;
    /// append some text with left-filled spaces up to Width characters count
    procedure AddSpaced(const Text: RawUtf8; Width: PtrInt;
      SepChar: AnsiChar = #0); overload;
    /// append some text with left-filled spaces up to Width characters count
    // - if the value too big to fit, will truncate up to the first Width chars
    procedure AddSpaced(Text: PUtf8Char; TextLen, Width: PtrInt); overload;
    /// append some number with left-filled spaces up to Width characters count
    // - if the value too big to fit in Width, will append K(Value) abbreviation
    procedure AddSpaced(Value: QWord; Width: PtrInt;
      SepChar: AnsiChar = #0); overload;
    /// append some UTF-8 chars, replacing a given character with another
    procedure AddReplace(Text: PUtf8Char; Orig, Replaced: AnsiChar);
    /// append some UTF-8 chars, quoting all " chars
    // - same algorithm than AddString(QuotedStr()) - without memory allocation,
    // and with an optional maximum text length (truncated with ending '...')
    // - this function implements what is specified in the official SQLite3
    // documentation: "A string constant is formed by enclosing the string in
    // single quotes ('). A single quote within the string can be encoded by
    // putting two single quotes in a row - as in Pascal."
    procedure AddQuotedStr(Text: PUtf8Char; TextLen: PtrUInt; Quote: AnsiChar;
      TextMaxLen: PtrInt = 0);
    /// append an URI-decoded domain name, also normalizing dual // into /
    // - only parameters - i.e. after '?' - may have ' ' replaced by '+'
    procedure AddUrlNameNormalize(U: PUtf8Char; L: PtrInt);
    /// append some UTF-8 chars, escaping all HTML special chars as expected
    procedure AddHtmlEscape(Text: PUtf8Char; Fmt: TTextWriterHtmlFormat = hfAnyWhere); overload;
    /// append some UTF-8 chars, escaping all HTML special chars as expected
    procedure AddHtmlEscape(Text: PUtf8Char; TextLen: PtrInt;
      Fmt: TTextWriterHtmlFormat = hfAnyWhere); overload;
    /// append some UTF-16 chars, escaping all HTML special chars as expected
    procedure AddHtmlEscapeW(Text: PWideChar;
      Fmt: TTextWriterHtmlFormat = hfAnyWhere); overload;
    /// append some RTL string chars, escaping all HTML special chars as expected
    procedure AddHtmlEscapeString(const Text: string;
      Fmt: TTextWriterHtmlFormat = hfAnyWhere);
    /// append some UTF-8 chars, escaping all HTML special chars as expected
    procedure AddHtmlEscapeUtf8(const Text: RawUtf8;
      Fmt: TTextWriterHtmlFormat = hfAnyWhere);
    /// append some chars, escaping all XML special chars as expected
    // - i.e.   < > & " '  as   &lt; &gt; &amp; &quote; &apos;
    // - and all control chars (i.e. #1..#31) as &#..;
    // - see @http://www.w3.org/TR/xml/#syntax
    procedure AddXmlEscape(Text: PUtf8Char);
    /// append a property name, as '"PropName":'
    // - PropName content should not need any JSON escape (e.g. no " within,
    // and only ASCII 7-bit characters)
    // - if twoForceJsonExtended is defined in CustomOptions, it would append
    // 'PropName:' without the double quotes
    procedure AddProp(PropName: PUtf8Char; PropNameLen: PtrInt); overload;
    /// append a property name, as '"PropName":'
    // - just a wrapper around AddProp(PropName, StrLen(PropName))
    procedure AddProp(PropName: PUtf8Char); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append a ShortString property name, as '"PropName":'
    // - PropName content should not need any JSON escape (e.g. no " within,
    // and only ASCII 7-bit characters)
    // - if twoForceJsonExtended is defined in CustomOptions, it would append
    // 'PropName:' without the double quotes
    // - is a wrapper around AddProp()
    procedure AddPropName(const PropName: ShortString);
      {$ifdef HASINLINE}inline;{$endif}
    /// append a JSON field name, followed by a number value and a comma (',')
    procedure AddPropInt64(const PropName: ShortString; Value: Int64;
      WithQuote: AnsiChar = #0);
    /// append a RawUtf8 property name, as '"FieldName":'
    // - FieldName content should not need any JSON escape (e.g. no " within)
    // - if twoForceJsonExtended is defined in CustomOptions, it would append
    // 'PropName:' without the double quotes
    // - is a wrapper around AddProp()
    procedure AddFieldName(const FieldName: RawUtf8);
      {$ifdef HASINLINE}inline;{$endif}
    /// append a RawUtf8 property name, as '"FieldName"
    // - FieldName content should not need any JSON escape (e.g. no " within)
    procedure AddQuotedFieldName(const FieldName: RawUtf8;
      const VoidPlaceHolder: RawUtf8 = ''); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append a RawUtf8 property name, as '"FieldName"
    // - FieldName content should not need any JSON escape (e.g. no " within)
    procedure AddQuotedFieldName(FieldName: PUtf8Char; FieldNameLen: PtrInt;
      const VoidPlaceHolder: RawUtf8 = ''); overload;
    /// append the class name of an Object instance as text
    procedure AddClassName(aClass: TClass);
    /// append an Instance name and pointer, as '"TObjectList(00425E68)"'+SepChar
    // - append "void" if Instance = nil
    procedure AddInstanceName(Instance: TObject; SepChar: AnsiChar);
    /// append an Instance name and pointer, as 'TObjectList(00425E68)'+SepChar
    procedure AddInstancePointer(Instance: TObject; SepChar: AnsiChar;
      IncludeUnitName, IncludePointer: boolean);
    /// append some binary data as hexadecimal text conversion
    procedure AddBinToHex(Bin: Pointer; BinBytes: PtrInt; LowerHex: boolean = false);
    /// append some binary data as hexadecimal text conversion
    // - append its minimal chars, i.e. excluding last bytes containing 0
    procedure AddBinToHexMinChars(Bin: Pointer; BinBytes: PtrInt; LowerHex: boolean = false);
    /// fast conversion from binary data into hexa chars, ready to be displayed
    // - using this function with Bin^ as an integer value will serialize it
    // in big-endian order (most-significant byte first), as used by humans
    // - up to the internal buffer bytes may be converted
    procedure AddBinToHexDisplay(Bin: pointer; BinBytes: PtrInt);
    /// fast conversion from binary data into MSB hexa chars
    // - up to the internal buffer bytes may be converted
    procedure AddBinToHexDisplayLower(Bin: pointer; BinBytes: PtrInt;
      QuotedChar: AnsiChar = #0);
    /// fast conversion from binary data into quoted MSB lowercase hexa chars
    // - up to the internal buffer bytes may be converted
    procedure AddBinToHexDisplayQuoted(Bin: pointer; BinBytes: PtrInt);
      {$ifdef HASINLINE}inline;{$endif}
    /// append a Value as significant hexadecimal text
    // - expects BinBytes to be > 0
    // - append its minimal chars, i.e. excluding highest bytes containing 0
    // - use GetNextItemHexa() to decode such a text value
    procedure AddBinToHexDisplayMinChars(Bin: pointer; BinBytes: PtrInt;
      QuotedChar: AnsiChar = #0);
    /// add the pointer into significant hexa chars, ready to be displayed
    // - append its minimal chars i.e. excluding highest bytes containing 0
    procedure AddPointer(P: PtrUInt; QuotedChar: AnsiChar = #0);
    /// write a byte as two hexa chars
    procedure AddByteToHex(Value: PtrUInt);
      {$ifdef HASINLINE}inline;{$endif}
    /// write a byte as two hexa chars
    procedure AddByteToHexLower(Value: PtrUInt);
      {$ifdef HASINLINE}inline;{$endif}
    /// write a Int18 value (0..262143) as 3 chars
    // - this encoding is faster than Base64, and has spaces on the left side
    // - use function Chars3ToInt18() to decode the textual content
    procedure AddInt18ToChars3(Value: cardinal);

    /// append strings or integers with a specified format
    // - this class implementation will raise an exception for twJsonEscape,
    // and simply call FormatUtf8() over a temp RawUtf8 for twNone/twOnSameLine
    // - use faster and more complete overriden TJsonWriter.Add instead!
    procedure Add(const Format: RawUtf8; const Values: array of const;
      Escape: TTextWriterKind = twNone;
      WriteObjectOptions: TTextWriterWriteObjectOptions = [woFullExpand]); overload; virtual;
    /// this class implementation will raise an exception
    // - use overriden TJsonWriter version instead!
    function AddJsonReformat(Json: PUtf8Char; Format: TTextWriterJsonFormat;
      EndOfObject: PUtf8Char): PUtf8Char; virtual;
    /// this class implementation will raise an exception
    // - use overriden TJsonWriter version instead!
    procedure AddVariant(const Value: variant; Escape: TTextWriterKind = twJsonEscape;
      WriteOptions: TTextWriterWriteObjectOptions = []); virtual;
    /// this class implementation will raise an exception
    // - use overriden TJsonWriter version instead!
    // - TypeInfo is a PRttiInfo instance - but not available in this early unit
    procedure AddTypedJson(Value: pointer; TypeInfo: pointer;
      WriteOptions: TTextWriterWriteObjectOptions = []); virtual;
    /// write some #0 ended UTF-8 text, according to the specified format
    // - use overriden TJsonWriter version instead!
    procedure Add(P: PUtf8Char; Escape: TTextWriterKind); overload; virtual;
    /// write some #0 ended UTF-8 text, according to the specified format
    // - use overriden TJsonWriter version instead!
    procedure Add(P: PUtf8Char; Len: PtrInt; Escape: TTextWriterKind); overload; virtual;
    /// write some data Base64 encoded
    // - use overriden TJsonWriter version instead!
    procedure WrBase64(P: PAnsiChar; Len: PtrUInt; withMagic: boolean); virtual;

    /// serialize as JSON the given object
    // - use overriden TJsonWriter version instead!
    procedure WriteObject(Value: TObject;
      WriteOptions: TTextWriterWriteObjectOptions = [woDontStoreDefault]); virtual;
    /// append a T*ObjArray dynamic array as a JSON array
    // - for proper serialization on Delphi 7-2009, use Rtti.RegisterObjArray()
    procedure AddObjArrayJson(const aObjArray;
      aOptions: TTextWriterWriteObjectOptions = [woDontStoreDefault]);
    /// return the last char appended
    // - returns #0 if no char has been written yet, or the buffer has been just
    // flushed: so this method is to be handled only in some particular usecases
    function LastChar: AnsiChar;
    /// how many bytes are currently in the internal buffer and not on disk/stream
    // - see TextLength for the total number of bytes, on both stream and memory
    function PendingBytes: PtrUInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// how many bytes were currently written on disk/stream
    // - excluding the bytes in the internal buffer (see PendingBytes)
    // - see TextLength for the total number of bytes, on both stream and memory
    property WrittenBytes: PtrUInt
      read fTotalFileSize;
    /// low-level access to the current indentation level
    property HumanReadableLevel: integer
      read fHumanReadableLevel write fHumanReadableLevel;
    /// the last char appended is canceled
    // - only one char cancelation is allowed at the same position: don't call
    // CancelLastChar/CancelLastComma more than once without appending text inbetween
    procedure CancelLastChar; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// the last char appended is canceled, if match the supplied one
    // - only one char cancelation is allowed at the same position: don't call
    // CancelLastChar/CancelLastComma more than once without appending text inbetween
    procedure CancelLastChar(aCharToCancel: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// the last char appended is canceled if it was a ','
    // - only one char cancelation is allowed at the same position: don't call
    // CancelLastChar/CancelLastComma more than once without appending text inbetween
    procedure CancelLastComma; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// the last char appended is canceled if it was a ',' and replaced
    // - only one char cancelation is allowed at the same position: don't call
    // CancelLastChar/CancelLastComma more than once without appending text inbetween
    procedure CancelLastComma(aReplaceChar: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// rewind the Stream to the position when Create() was called
    // - note that this does not clear the Stream content itself, just
    // move back its writing position to its initial place
    procedure CancelAll;
    /// same as CancelAll, and also reset the CustomOptions
    procedure CancelAllAsNew;
      {$ifdef HASINLINE}inline;{$endif}
    /// same as CancelAll, and also use a new local TTextWriterStackBuffer
    procedure CancelAllWith(var temp: TTextWriterStackBuffer);

    /// count of added bytes to the stream
    // - see PendingBytes for the number of bytes currently in the memory buffer
    // or WrittenBytes for the number of bytes already written to disk/stream
    property TextLength: PtrUInt
      read GetTextLength;
    /// the internal TStream used for storage
    // - you should call the FlushFinal (or FlushToStream) methods before using
    // this TStream content, to flush all pending characters
    // - if the TStream instance has not been specified when calling the
    // TTextWriter constructor, it can be forced via this property, before
    // any writing
    property Stream: TStream
      read fStream write SetStream;
    /// global options to customize this TTextWriter instance process
    // - allows to override e.g. AddRecordJson() and AddDynArrayJson() behavior
    property CustomOptions: TTextWriterOptions
      read fCustomOptions write fCustomOptions;
    /// optional event called before FlushToStream method process
    // - used e.g. by TEchoWriter to perform proper content echoing
    property OnFlushToStream: TOnTextWriterFlush
      read fOnFlushToStream write fOnFlushToStream;
  end;

  /// class of our simple TEXT format writer to a Stream
  TBaseWriterClass = class of TTextWriter;

var
  /// contains the default JSON serialization class for the framework
  // - used internally by ObjectToJson/VariantSaveJson to avoid circular references
  // - will be set to TJsonWriter by mormot.core.json; default TTextWriter
  // would raise an exception on any JSON processing attempt
  DefaultJsonWriter: TBaseWriterClass = TTextWriter;

/// will serialize any TObject into its UTF-8 JSON representation
/// - serialize as JSON the published integer, Int64, floating point values,
// TDateTime (stored as ISO 8601 text), string, variant and enumerate
// (e.g. boolean) properties of the object (and its parents)
// - would set twoForceJsonStandard to force standard (non-extended) JSON
// - the enumerates properties are stored with their integer index value
// - will write also the properties published in the parent classes
// - nested properties are serialized as nested JSON objects
// - any TCollection property will also be serialized as JSON arrays
// - you can add some custom serializers for ANY class, via mormot.core.json.pas
// TRttiJson.RegisterCustomSerializer() class method
// - call internally TTextWriter.WriteObject() method from DefaultJsonWriter
function ObjectToJson(Value: TObject;
  Options: TTextWriterWriteObjectOptions = [woDontStoreDefault]): RawUtf8; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// will serialize any TObject into its UTF-8 JSON representation
procedure ObjectToJson(Value: TObject; var result: RawUtf8;
  Options: TTextWriterWriteObjectOptions = [woDontStoreDefault]); overload;

/// will serialize any TObject into its expanded UTF-8 JSON representation
// - includes debugger-friendly information, similar to TSynLog, i.e.
// class name and sets/enumerates as text
// - redirect to ObjectToJson() with the proper TTextWriterWriteObjectOptions,
// since our JSON serialization detects and serialize Exception.Message
function ObjectToJsonDebug(Value: TObject;
  Options: TTextWriterWriteObjectOptions = [woDontStoreDefault,
    woHumanReadable, woStoreClassName, woStorePointer,
    woHideSensitivePersonalInformation]): RawUtf8;

/// a wrapper around ConsoleWrite(ObjectToJson(Value))
procedure ConsoleObject(Value: TObject;
  Options: TTextWriterWriteObjectOptions = [woHumanReadable]);

/// check if some UTF-8 text would need HTML escaping
function NeedsHtmlEscape(text: PUtf8Char; fmt: TTextWriterHtmlFormat): boolean;

/// escape some UTF-8 text into HTML
// - just a wrapper around TTextWriter.AddHtmlEscape() process,
// replacing < > & " chars depending on the HTML layer
function HtmlEscape(const text: RawUtf8;
  fmt: TTextWriterHtmlFormat = hfAnyWhere): RawUtf8;

/// escape some RTL string text into UTF-8 HTML
// - just a wrapper around TTextWriter.AddHtmlEscapeString() process,
// replacing < > & " chars depending on the HTML layer
function HtmlEscapeString(const text: string;
  fmt: TTextWriterHtmlFormat = hfAnyWhere): RawUtf8;

/// escape as \xx hexadecimal some chars from a set into a pre-allocated buffer
// - dest^ should have at least srclen * 3 bytes, for \## trios
function EscapeHexBuffer(src, dest: PUtf8Char; srclen: integer;
  const toescape: TSynAnsicharSet; escape: AnsiChar = '\'): PUtf8Char;

/// escape as \xx hexadecimal some chars from a set into a new RawUtf8 string
// - as used e.g. by LdapEscape()
function EscapeHex(const src: RawUtf8;
  const toescape: TSynAnsicharSet; escape: AnsiChar = '\'): RawUtf8;

/// un-escape \xx or \c encoded chars from a pre-allocated buffer
// - any CR/LF after \ will also be ignored
// - dest^ should have at least the same length than src^
function UnescapeHexBuffer(src, dest: PUtf8Char; escape: AnsiChar = '\'): PUtf8Char;

/// un-escape \xx or \c encoded chars into a new RawUtf8 string
// - any CR/LF after \ will also be ignored
function UnescapeHex(const src: RawUtf8; escape: AnsiChar = '\'): RawUtf8;

/// escape as \char pair some chars from a set into a pre-allocated buffer
// - dest^ should have at least srclen * 2 bytes, for \char pairs
// - by definition, escape should be part of the toescape set
function EscapeCharBuffer(src, dest: PUtf8Char; srclen: integer;
  const toescape: TSynAnsicharSet; escape: AnsiChar = '\'): PUtf8Char;

/// escape as \char pair some chars from a set into a new RawUtf8 string
// - by definition, escape should be part of the toescape set
function EscapeChar(const src: RawUtf8;
  const toescape: TSynAnsicharSet; escape: AnsiChar = '\'): RawUtf8;

const
  /// TTextWriter JSON serialization options focusing of sets support
  // - as used e.g. by TJsonWriter.AddRecordJson/AddDynArrayJson and
  // TDynArray.SaveJson methods, and SaveJson/RecordSaveJson functions
  // - to be used as TEXTWRITEROPTIONS_TEXTSET[EnumSetsAsText]
  TEXTWRITEROPTIONS_SETASTEXT: array[boolean] of TTextWriterOptions = (
    [twoFullSetsAsStar],
    [twoFullSetsAsStar, twoEnumSetsAsTextInRecord]);

  /// TTextWriter JSON serialization options including twoEnumSetsAsTextInRecord
  TEXTWRITEROPTIONS_ENUMASTEXT: array[boolean] of TTextWriterOptions = (
    [],
    [twoEnumSetsAsTextInRecord]);

  /// TTextWriter JSON serialization options including woEnumSetsAsText
  TEXTWRITEROBJECTOPTIONS_ENUMASTEXT: array[boolean] of TTextWriterWriteObjectOptions = (
    [],
    [woEnumSetsAsText]);

  /// TTextWriter JSON serialization options which should be preserved
  // - used e.g. by TTextWriter.CancelAllAsNew to reset its CustomOptions
  TEXTWRITEROPTIONS_RESET =
    [twoStreamIsOwned, twoStreamIsRawByteString, twoBufferIsExternal];

type
  TEchoWriter = class;

  /// callback used to echo each line of TEchoWriter class
  // - should return TRUE on success, FALSE if the log was not echoed: but
  // TSynLog will continue logging, even if this event returned FALSE
  TOnTextWriterEcho = function(Sender: TEchoWriter; Level: TSynLogLevel;
    const Text: RawUtf8): boolean of object;

  TEchoWriterBack = record
    Level: TSynLogLevelDynArray;
    Text: TRawUtf8DynArray;
    Count: PtrInt;
  end;

  /// add optional echoing of the lines to TTextWriter
  // - as used e.g. by TSynLog writer for log optional redirection
  // - is defined as a nested class to reduce plain TTextWriter scope, and
  // better follow the SOLID principles
  TEchoWriter = class
  protected
    fWriter: TTextWriter;
    fEchoStart: PtrInt;
    fEchoBuf: RawUtf8;
    fEchos: array of TOnTextWriterEcho;
    fBack: TEchoWriterBack;
    fBackSafe: TLightLock; // protect fBack.Level/Text
    fEchoPendingExecuteBackground: boolean;
    function EchoFlush: PtrInt;
    procedure EchoPendingToBackground(aLevel: TSynLogLevel);
    function GetEndOfLineCRLF: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetEndOfLineCRLF(aEndOfLineCRLF: boolean);
  public
    /// prepare for the echoing process
    constructor Create(Owner: TTextWriter); reintroduce;
    /// end the echoing process
    destructor Destroy; override;
    /// should be called from TTextWriter.FlushToStream
    // - write pending data to the Stream, with automatic buffer resizal and echoing
    // - this overriden method will handle proper echoing
    procedure FlushToStream(Text: PUtf8Char; Len: PtrInt);
    /// mark an end of line, ready to be "echoed" to registered listeners
    // - append a LF (#10) char or CR+LF (#13#10) chars to the buffer, depending
    // on the EndOfLineCRLF property value (default is LF, to minimize storage)
    // - any callback registered via EchoAdd() will monitor this line in the
    // current thread, or calling EchoPendingExecute from a background thread
    // - used e.g. by TSynLog for console output, as stated by Level parameter
    procedure AddEndOfLine(aLevel: TSynLogLevel = sllNone);
    /// add a callback to echo each line written by this class
    // - this class expects AddEndOfLine to mark the end of each line
    procedure EchoAdd(const aEcho: TOnTextWriterEcho);
    /// remove a callback to echo each line written by this class
    // - event should have been previously registered by a EchoAdd() call
    procedure EchoRemove(const aEcho: TOnTextWriterEcho);
    /// reset the internal buffer used for echoing content
    procedure EchoReset;
    /// run all pending EchoPendingExecuteBackground notifications
    // - should be executed from a background thread
    procedure EchoPendingExecute;
    /// the associated TTextWriter instance
    property Writer: TTextWriter
      read fWriter;
    /// define how AddEndOfLine method stores its line feed characters
    // - by default (FALSE), it will append a LF (#10) char to the buffer
    // - you can set this property to TRUE, so that CR+LF (#13#10) chars will
    // be appended instead
    // - is just a wrapper around twoEndOfLineCRLF item in CustomOptions
    property EndOfLineCRLF: boolean
      read GetEndOfLineCRLF write SetEndOfLineCRLF;
    /// if EchoPendingExecute is about to be executed in the background
    property EchoPendingExecuteBackground: boolean
      read fEchoPendingExecuteBackground write fEchoPendingExecuteBackground;
  end;



{ ************ Numbers (integers or floats) and Variants to Text Conversion }

var
  /// naive but efficient cache to avoid string memory allocation for
  // 0..999 small numbers by Int32ToUtf8/UInt32ToUtf8
  // - use around 16KB of heap (since each item consumes 16 bytes), but increase
  // overall performance and reduce memory allocation (and fragmentation),
  // especially during multi-threaded execution
  // - noticeable when strings are used as array indexes (e.g.
  // in mormot.db.nosql.bson)
  // - is defined globally, since may be used from an inlined function
  SmallUInt32Utf8: array[0..999] of RawUtf8;

/// fast RawUtf8 version of 32-bit IntToStr()
function Int32ToUtf8(Value: PtrInt): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast RawUtf8 version of 32-bit IntToStr()
// - result as var parameter saves a local assignment and a try..finally
procedure Int32ToUtf8(Value: PtrInt; var result: RawUtf8); overload;

/// fast RawUtf8 version of 64-bit IntToStr()
function Int64ToUtf8(Value: Int64): RawUtf8; overload;
  {$ifdef HASSAFEINLINE}inline;{$endif} // Delphi 2007 has trouble inlining this

/// fast RawUtf8 version of 64-bit IntToStr()
// - result as var parameter saves a local assignment and a try..finally
procedure Int64ToUtf8(Value: Int64; var result: RawUtf8); overload;

/// fast RawUtf8 version of 32-bit IntToStr()
function ToUtf8(Value: PtrInt): RawUtf8; overload;

{$ifdef CPU32}
/// fast RawUtf8 version of 64-bit IntToStr()
function ToUtf8(Value: Int64): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}
{$endif CPU32}

/// optimized conversion of a cardinal into RawUtf8
function UInt32ToUtf8(Value: PtrUInt): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// optimized conversion of a cardinal into RawUtf8
procedure UInt32ToUtf8(Value: PtrUInt; var result: RawUtf8); overload;

/// fast RawUtf8 version of 64-bit IntToStr(), with proper QWord support
procedure UInt64ToUtf8(Value: QWord; var result: RawUtf8);

/// convert a string into its INTEGER Curr64 (value*10000) representation
// - this type is compatible with currency memory mapping with PInt64(@Curr)^
// - fast conversion, using only integer operations
// - if NoDecimal is defined, will be set to TRUE if there is no decimal, AND
// the returned value will be an Int64 (not a PInt64(@Curr)^)
function StrToCurr64(P: PUtf8Char; NoDecimal: PBoolean = nil): Int64;

/// convert a string into its currency representation
// - will call StrToCurr64()
function StrToCurrency(P: PUtf8Char): currency;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a currency value into a string
// - fast conversion, using only integer operations
// - decimals are joined by 2 (no decimal, 2 decimals, 4 decimals)
function CurrencyToStr(const Value: currency): RawUtf8;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an INTEGER Curr64 (value*10000) into a string
// - this type is compatible with currency memory mapping with PInt64(@Curr)^
// - fast conversion, using only integer operations
// - decimals are joined by 2 (no decimal, 2 decimals, 4 decimals)
function Curr64ToStr(const Value: Int64): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an INTEGER Curr64 (value*10000) into a string
// - this type is compatible with currency memory mapping with PInt64(@Curr)^
// - fast conversion, using only integer operations
// - decimals are joined by 2 (no decimal, 2 decimals, 4 decimals)
procedure Curr64ToStr(const Value: Int64; var result: RawUtf8); overload;

/// convert an INTEGER Curr64 (value*10000) into a string
// - this type is compatible with currency memory mapping with PInt64(@Curr)^
// - fast conversion, using only integer operations
// - decimals are joined by 2 (no decimal, 2 decimals, 4 decimals)
// - return the number of chars written to Dest^
function Curr64ToPChar(const Value: Int64; Dest: PUtf8Char): PtrInt;

/// internal fast INTEGER Curr64 (value*10000) value to text conversion
// - expect the last available temporary char position in P
// - return the last written char position (write in reverse order in P^)
// - will return 0 for Value=0, or a string representation with always 4 decimals
//   (e.g. 1->'0.0001' 500->'0.0500' 25000->'2.5000' 30000->'3.0000')
// - is called by Curr64ToPChar() and Curr64ToStr() functions
function StrCurr64(P: PAnsiChar; const Value: Int64): PAnsiChar;

/// faster than default SysUtils.IntToStr implementation
function IntToString(Value: integer): string; overload;

/// faster than default SysUtils.IntToStr implementation
function IntToString(Value: cardinal): string; overload;

/// faster than default SysUtils.IntToStr implementation
function IntToString(Value: Int64): string; overload;

/// convert a floating-point value to its numerical text equivalency
function DoubleToString(Value: Double): string;

/// convert a currency value from its Int64 binary representation into
// its numerical text equivalency
// - decimals are joined by 2 (no decimal, 2 decimals, 4 decimals)
function Curr64ToString(Value: Int64): string;

/// convert a floating-point value to its numerical text equivalency
// - on Delphi Win32, calls FloatToText() in ffGeneral mode; on FPC uses str()
// - DOUBLE_PRECISION will redirect to DoubleToShort() and its faster Fabian
// Loitsch's Grisu algorithm if available
// - returns the count of chars stored into S, i.e. length(S)
function ExtendedToShort(S: PShortString;
  Value: TSynExtended; Precision: integer): integer;

/// convert a floating-point value to its numerical text equivalency without
// scientification notation
// - DOUBLE_PRECISION will redirect to DoubleToShortNoExp() and its faster Fabian
// Loitsch's Grisu algorithm if available - or calls str(Value:0:precision,S)
// - returns the count of chars stored into S, i.e. length(S)
function ExtendedToShortNoExp(S: PShortString; Value: TSynExtended;
  Precision: integer): integer;

/// check if the supplied text is NAN/INF/+INF/-INF, i.e. not a number
// - as returned by ExtendedToShort/DoubleToShort textual conversion
// - such values do appear as IEEE floating points, but are not defined in JSON
function FloatToShortNan(const s: ShortString): TFloatNan;
  {$ifdef HASINLINE}inline;{$endif}

/// check if the supplied text is NAN/INF/+INF/-INF, i.e. not a number
// - as returned e.g. by ExtendedToStr/DoubleToStr textual conversion
// - such values do appear as IEEE floating points, but are not defined in JSON
function FloatToStrNan(const s: RawUtf8): TFloatNan;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a floating-point value to its numerical text equivalency
function ExtendedToStr(Value: TSynExtended; Precision: integer): RawUtf8; overload;

/// convert a floating-point value to its numerical text equivalency
procedure ExtendedToStr(Value: TSynExtended; Precision: integer;
  var result: RawUtf8); overload;

/// recognize if the supplied text is NAN/INF/+INF/-INF, i.e. not a number
// - returns the number as text (stored into tmp variable), or "Infinity",
// "-Infinity", and "NaN" for corresponding IEEE special values
// - result is a PShortString either over tmp, or JSON_NAN[]
function FloatToJsonNan(s: PShortString): PShortString;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a floating-point value to its JSON text equivalency
// - depending on the platform, it may either call str() or FloatToText()
// in ffGeneral mode (the shortest possible decimal string using fixed or
// scientific format)
// - returns the number as text (stored into tmp variable), or "Infinity",
// "-Infinity", and "NaN" for corresponding IEEE special values
// - result is a PShortString either over tmp, or JSON_NAN[]
function ExtendedToJson(tmp: PShortString; Value: TSynExtended;
  Precision: integer; NoExp: boolean): PShortString;

/// convert a 64-bit floating-point value to its numerical text equivalency
// - on Delphi Win32, calls FloatToText() in ffGeneral mode
// - on other platforms, i.e. Delphi Win64 and all FPC targets, will use our own
// faster Fabian Loitsch's Grisu algorithm implementation
// - returns the count of chars stored into S, i.e. length(S)
function DoubleToShort(S: PShortString; const Value: double): integer;

/// convert a 64-bit floating-point value to its numerical text equivalency
// without scientific notation
// - on Delphi Win32, calls FloatToText() in ffGeneral mode
// - on other platforms, i.e. Delphi Win64 and all FPC targets, will use our own
// faster Fabian Loitsch's Grisu algorithm implementation
// - returns the count of chars stored into S, i.e. length(S)
function DoubleToShortNoExp(S: PShortString; const Value: double): integer;

{$ifdef DOUBLETOSHORT_USEGRISU}
const
  // special text returned if the double is not a number
  C_STR_INF: string[3] = 'Inf';
  C_STR_QNAN: string[3] = 'Nan';

  // min_width parameter special value, as used internally by FPC for str(d,s)
  // - DoubleToAscii() only accept C_NO_MIN_WIDTH or 0 for min_width: space
  // trailing has been removed in this cut-down version
  C_NO_MIN_WIDTH = -32767;

/// raw function to convert a 64-bit double into a ShortString, stored in str
// - implements Fabian Loitsch's Grisu algorithm dedicated to double values
// - currently, this unit only set min_width=0 (for DoubleToShortNoExp to avoid
// any scientific notation ) or min_width=C_NO_MIN_WIDTH (for DoubleToShort to
// force the scientific notation when the double cannot be represented as
// a simple fractinal number)
procedure DoubleToAscii(min_width, frac_digits: integer;
  const v: double; str: PAnsiChar);
{$endif DOUBLETOSHORT_USEGRISU}

/// convert a 64-bit floating-point value to its JSON text equivalency
// - on Delphi Win32, calls FloatToText() in ffGeneral mode
// - on other platforms, i.e. Delphi Win64 and all FPC targets, will use our own
// faster Fabian Loitsch's Grisu algorithm
// - returns the number as text (stored into tmp variable), or "Infinity",
// "-Infinity", and "NaN" for corresponding IEEE special values
// - result is a PShortString either over tmp, or JSON_NAN[]
function DoubleToJson(tmp: PShortString; Value: double;
  NoExp: boolean): PShortString;

/// convert a 64-bit floating-point value to its numerical text equivalency
function DoubleToStr(Value: Double): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a 64-bit floating-point value to its numerical text equivalency
procedure DoubleToStr(Value: Double; var result: RawUtf8); overload;

/// copy a floating-point text buffer with proper correction and validation
// - will correct on the fly '.5' -> '0.5' and '-.5' -> '-0.5'
// - will end not only on #0 but on any char not matching 1[.2[e[-]3]] pattern
// - is used when the input comes from a third-party source with no regular
// output, e.g. a database driver, via TTextWriter.AddFloatStr
function FloatStrCopy(s, d: PUtf8Char): PUtf8Char;

/// fast conversion of 2 digit characters into a 0..99 value
// - returns FALSE on success, TRUE if P^ is not correct
function Char2ToByte(P: PUtf8Char; out Value: cardinal;
   ConvertHexToBinTab: PByteArray): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion of 3 digit characters into a 0..9999 value
// - returns FALSE on success, TRUE if P^ is not correct
function Char3ToWord(P: PUtf8Char; out Value: cardinal;
   ConvertHexToBinTab: PByteArray): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion of 4 digit characters into a 0..9999 value
// - returns FALSE on success, TRUE if P^ is not correct
function Char4ToWord(P: PUtf8Char; out Value: cardinal;
   ConvertHexToBinTab: PByteArray): boolean;
  {$ifdef HASINLINE}inline;{$endif}


/// convert any Variant into UTF-8 encoded String
// - use VariantSaveJson() instead if you need a conversion to JSON with
// custom parameters
// - note: null will be returned as 'null'
function VariantToUtf8(const V: Variant): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any Variant into UTF-8 encoded String
// - use VariantSaveJson() instead if you need a conversion to JSON with
// custom parameters
// - note: null will be returned as 'null'
function ToUtf8(const V: Variant): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any Variant/TVarData into UTF-8 encoded String
// - use VariantSaveJson() instead if you need a conversion to JSON with
// custom parameters
// - note: null will be returned as 'null'
function ToUtf8(const V: TVarData): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any Variant into UTF-8 encoded String
// - use VariantSaveJson() instead if you need a conversion to JSON with
// custom parameters
// - wasString is set if the V value was a text
// - empty and null variants will be stored as 'null' text - as expected by JSON
// - custom variant types (e.g. TDocVariant) will be stored as JSON
procedure VariantToUtf8(const V: Variant; var result: RawUtf8;
   var wasString: boolean); overload;

/// convert any Variant into UTF-8 encoded String
// - use VariantSaveJson() instead if you need a conversion to JSON with
// custom parameters
// - returns TRUE if the V value was a text, FALSE if was not (e.g. a number)
// - empty and null variants will be stored as 'null' text - as expected by JSON
// - custom variant types (e.g. TDocVariant) will be stored as JSON
function VariantToUtf8(const V: Variant; var Text: RawUtf8): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any non-null Variant into UTF-8 encoded String
// - empty and null variants will return false
function VariantToText(const V: Variant; var Text: RawUtf8): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// save a variant value into a JSON content
// - just a wrapper around the _VariantSaveJson procedure redirection
function VariantSaveJson(const Value: variant;
  Escape: TTextWriterKind = twJsonEscape): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// save a variant value into a JSON content
// - just a wrapper around the _VariantSaveJson procedure redirection
procedure VariantSaveJson(const Value: variant; Escape: TTextWriterKind;
  var result: RawUtf8); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// internal low-level function to compare two variants with RawUt8 conversion
// - as used e.g. by FastVarDataComp() for complex or diverse VType
function VariantCompAsText(A, B: PVarData; caseInsensitive: boolean): integer;

var
  /// save a variant value into a JSON content
  // - is implemented by mormot.core.json.pas and mormot.core.variants.pas:
  // will raise an exception if none of these units is included in the project
  // - follows the TTextWriter.AddVariant() and VariantLoadJson() format
  // - is able to handle simple and custom variant types, for instance:
  // !  VariantSaveJson(1.5)='1.5'
  // !  VariantSaveJson('test')='"test"'
  // !  o := _Json('{ BSON: [ "test", 5.05, 1986 ] }');
  // !  VariantSaveJson(o)='{"BSON":["test",5.05,1986]}'
  // !  o := _Obj(['name','John','doc',_Obj(['one',1,'two',_Arr(['one',2])])]);
  // !  VariantSaveJson(o)='{"name":"John","doc":{"one":1,"two":["one",2]}}'
  // - note that before Delphi 2009, any varString value is expected to be
  // a RawUtf8 instance - which does make sense in the mORMot area
  _VariantSaveJson: procedure(const Value: variant; Escape: TTextWriterKind;
    var result: RawUtf8);

  /// unserialize a JSON content into a variant
  // - is properly implemented by mormot.core.json.pas: if this unit is not
  // included in the project, this function is nil
  // - used by mormot.core.data.pas RTTI_BINARYLOAD[tkVariant]() for complex types
  BinaryVariantLoadAsJson: procedure(var Value: variant; Json: PUtf8Char;
    TryCustomVariant: pointer);

  /// write a TDateTime into strict ISO-8601 date and/or time text
  // - is implemented by DateTimeToIso8601TextVar from mormot.core.datetime.pas:
  // if this unit is not included in the project, an ESynException is raised
  // - used by VariantToUtf8() for TDateTime conversion
  _VariantToUtf8DateTimeToIso8601: procedure(DT: TDateTime; FirstChar: AnsiChar;
    var result: RawUtf8; WithMS: boolean);

  /// Date/Time conversion from ISO-8601 text
  // - is implemented by Iso8601ToDateTime() from mormot.core.datetime.pas:
  // if this unit is not included in the project, this function is nil
  // - used by TRttiProp.SetValue() for TDateTime properties with a getter
  _Iso8601ToDateTime: function(const iso: RawByteString): TDateTime;


type
  /// used e.g. by UInt4DigitsToShort/UInt3DigitsToShort/UInt2DigitsToShort
  // - such result type would avoid a string allocation on heap
  TShort4 = string[4];

/// revert the value as encoded by TTextWriter.AddInt18ToChars3() or Int18ToChars3()
// - no range check is performed: you should ensure that the incoming text
// follows the expected 3-chars layout
function Chars3ToInt18(P: pointer): cardinal;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the value as encoded by TTextWriter.AddInt18ToChars3() method
function Int18ToChars3(Value: cardinal): RawUtf8; overload;

/// compute the value as encoded by TTextWriter.AddInt18ToChars3() method
procedure Int18ToChars3(Value: cardinal; var result: RawUtf8); overload;

/// creates a 3 digits string from a 0..999 value as '000'..'999'
// - consider using UInt3DigitsToShort() to avoid temporary memory allocation,
// e.g. when used as FormatUtf8() parameter
function UInt3DigitsToUtf8(Value: cardinal): RawUtf8;

/// creates a 4 digits string from a 0..9999 value as '0000'..'9999'
// - consider using UInt4DigitsToShort() to avoid temporary memory allocation,
// e.g. when used as FormatUtf8() parameter
function UInt4DigitsToUtf8(Value: cardinal): RawUtf8;

  /// creates a 4 digits short string from a 0..9999 value
// - using TShort4 as returned string would avoid a string allocation on heap
// - could be used e.g. as parameter to FormatUtf8()
function UInt4DigitsToShort(Value: cardinal): TShort4;

/// creates a 3 digits short string from a 0..999 value
// - using TShort4 as returned string would avoid a string allocation on heap
// - could be used e.g. as parameter to FormatUtf8()
function UInt3DigitsToShort(Value: cardinal): TShort4;

/// creates a 2 digits short string from a 0..99 value
// - using TShort4 as returned string would avoid a string allocation on heap
// - could be used e.g. as parameter to FormatUtf8()
function UInt2DigitsToShort(Value: byte): TShort4;
  {$ifdef HASINLINE}inline;{$endif}

/// creates a 2 digits short string from a 0..99 value
// - won't test Value>99 as UInt2DigitsToShort()
function UInt2DigitsToShortFast(Value: byte): TShort4;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an IPv4 'x.x.x.x' text into its 32-bit value
// - result is in little endian order, not network order: 1.2.3.4 becomes $04030201
// - returns TRUE if the text was a valid IPv4 text, unserialized as 32-bit aValue
// - returns FALSE on parsing error, also setting aValue=0
// - '' or '127.0.0.1' will also return false
function IPToCardinal(aIP: PUtf8Char; out aValue: cardinal): boolean; overload;

/// convert an IPv4 'x.x.x.x' text into its 32-bit value
// - result is in little endian order, not network order: 1.2.3.4 becomes $04030201
// - returns TRUE if the text was a valid IPv4 text, unserialized as 32-bit aValue
// - returns FALSE on parsing error, also setting aValue=0
// - '' or '127.0.0.1' will also return false
function IPToCardinal(const aIP: RawUtf8; out aValue: cardinal): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an IPv4 'x.x.x.x' text into its 32-bit value, 0 or localhost
// - result is in little endian order, not network order: 1.2.3.4 becomes $04030201
// - returns <> 0 value if the text was a valid IPv4 text, 0 on parsing error
// - '' or '127.0.0.1' will also return 0
function IPToCardinal(const aIP: RawUtf8): cardinal; overload;
  {$ifdef HASINLINE}inline;{$endif}


{ ************ Text Formatting functions }

type
  /// a memory structure which avoids a temporary RawUtf8 allocation
  // - used by VarRecToTempUtf8/VariantToTempUtf8 and FormatUtf8/FormatShort
  TTempUtf8 = record
    Len: PtrInt;
    Text: PUtf8Char;
    TempRawUtf8: pointer;
    Temp: array[0..23] of AnsiChar;
  end;
  PTempUtf8 = ^TTempUtf8;

/// convert any Variant into a JSON-compatible UTF-8 encoded temporary buffer
// - this function would allocate a RawUtf8 in Res.TempRawUtf8 only if needed,
// but use the supplied Res.Temp[] buffer for numbers to text conversion -
// caller should ensure to make RawUtf8(Res.TempRawUtf8) := '' once done with it
// - wasString is set if the V value was a text
// - empty and null variants will be stored as 'null' text - as expected by JSON
// - booleans will be stored as 'true' or 'false' - as expected by JSON
// - custom variant types (e.g. TDocVariant) will be stored as JSON
procedure VariantToTempUtf8(const V: variant; var Res: TTempUtf8;
  var wasString: boolean);

const
  /// which TVarRec.VType are numbers, i.e. don't need to be quoted
  // - vtVariant is a number by default, unless detected e.g. by VariantToUtf8()
  vtNotString = [vtBoolean, vtInteger, vtInt64, {$ifdef FPC} vtQWord, {$endif}
                 vtCurrency, vtExtended, vtVariant];

/// convert an open array (const Args: array of const) argument to an UTF-8
// encoded text
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
// - any supplied TObject instance will be written as their class name
procedure VarRecToUtf8(const V: TVarRec; var result: RawUtf8;
  wasString: PBoolean = nil);

/// convert an open array (const Args: array of const) argument to an UTF-8
// encoded text, using a specified temporary buffer
// - this function would allocate a RawUtf8 in Res.TempRawUtf8 only if needed,
// but use the supplied Res.Temp[] buffer for numbers to text conversion -
// caller should ensure to make RawUtf8(Res.TempRawUtf8) := '' once done with it
// - it would return the number of UTF-8 bytes, i.e. Res.Len
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
// - any supplied TObject instance will be written as their class name
function VarRecToTempUtf8(const V: TVarRec; var Res: TTempUtf8;
  wasString: PBoolean = nil): PtrInt;

/// convert an open array (const Args: array of const) argument to an UTF-8
// encoded text, returning FALSE if the argument was not a string value
function VarRecToUtf8IsString(const V: TVarRec; var value: RawUtf8): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an open array (const Args: array of const) argument to an Int64
// - returns TRUE and set Value if the supplied argument is a vtInteger, vtInt64
// or vtBoolean
// - returns FALSE if the argument is not an integer
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
function VarRecToInt64(const V: TVarRec; out value: Int64): boolean;

/// convert an open array (const Args: array of const) argument to a floating
// point value
// - returns TRUE and set Value if the supplied argument is a number (e.g.
// vtInteger, vtInt64, vtCurrency or vtExtended)
// - returns FALSE if the argument is not a number
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
function VarRecToDouble(const V: TVarRec; out value: double): boolean;

/// convert an open array (const Args: array of const) argument to a value
// encoded as with :(...): inlined parameters in FormatUtf8(Format,Args,Params)
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
// - any supplied TObject instance will be written as their class name
procedure VarRecToInlineValue(const V: TVarRec; var result: RawUtf8);

/// get an open array (const Args: array of const) character argument
// - only handle varChar and varWideChar kind of arguments
function VarRecAsChar(const V: TVarRec): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a supplied "array of const" argument is an instance of a given class
function VarRecAs(const aArg: TVarRec; aClass: TClass): pointer;

/// fast Format() function replacement, optimized for RawUtf8
// - only supported token is %, which will be written in the resulting string
// according to each Args[] supplied items - so you will never get any exception
// as with the SysUtils.Format() when a specifier is incorrect
// - resulting string has no length limit and uses fast concatenation
// - there is no escape char, so to output a '%' character, you need to use '%'
// as place-holder, and specify '%' as value in the Args array
// - note that, due to a Delphi compiler limitation, cardinal values should be
// type-casted to Int64() (otherwise the integer mapped value will be converted)
// - any supplied TObject instance will be written as their class name
// - see FormatSql() and FormatJson() from mormot.core.json for ? placeholders
function FormatUtf8(const Format: RawUtf8; const Args: array of const): RawUtf8; overload;

/// fast Format() function replacement, optimized for RawUtf8
// - overloaded function, which avoid a temporary RawUtf8 instance on stack
procedure FormatUtf8(const Format: RawUtf8; const Args: array of const;
  out result: RawUtf8); overload;

/// fast Format() function replacement, tuned for direct memory buffer write
// - use the same single token % (and implementation) than FormatUtf8()
// - returns the number of UTF-8 bytes appended to Dest^
function FormatBuffer(const Format: RawUtf8; const Args: array of const;
  Dest: pointer; DestLen: PtrInt): PtrInt;

/// fast Format() function replacement, for UTF-8 content stored in ShortString
// - use the same single token % (and implementation) than FormatUtf8()
// - ShortString allows fast stack allocation, so is perfect for small content
// - truncate result if the text size exceeds 255 bytes
procedure FormatShort(const Format: RawUtf8; const Args: array of const;
  var result: ShortString);

/// fast Format() function replacement, for UTF-8 content stored in ShortString
function FormatToShort(const Format: RawUtf8; const Args: array of const): ShortString;

/// fast Format() function replacement, tuned for small content
// - use the same single token % (and implementation) than FormatUtf8()
procedure FormatString(const Format: RawUtf8; const Args: array of const;
  out result: string); overload;

/// fast Format() function replacement, tuned for small content
// - use the same single token % (and implementation) than FormatUtf8()
function FormatString(const Format: RawUtf8; const Args: array of const): string; overload;
  {$ifdef FPC}inline;{$endif} // Delphi don't inline "array of const" parameters

/// fast Format() function replacement, for UTF-8 content stored in TShort16
// - truncate result if the text size exceeds 16 bytes
procedure FormatShort16(const Format: RawUtf8; const Args: array of const;
  var result: TShort16);

/// fast Format() function replacement, for UTF-8 content stored in variant
function FormatVariant(const Format: RawUtf8; const Args: array of const): variant;

/// concatenate several arguments into an UTF-8 string
function Make(const Args: array of const): RawUtf8; overload;

/// concatenate several arguments into an UTF-8 string
procedure Make(const Args: array of const; var Result: RawUtf8); overload;

/// concatenate several arguments into a RTL string
function MakeString(const Args: array of const): string;

/// append some text items to a RawUtf8 variable
// - see also AppendLine() below if you need a separator
procedure Append(var Text: RawUtf8; const Args: array of const); overload;

/// append one text item to a RawUtf8 variable with no code page conversion
procedure Append(var Text: RawUtf8; const Added: RawByteString); overload;
  {$ifdef HASINLINE} inline; {$endif}

/// append two text items to a RawUtf8 variable with no code page conversion
procedure Append(var Text: RawUtf8; const Added1, Added2: RawByteString); overload;

/// append one char to a RawUtf8 variable with no code page conversion
procedure Append(var Text: RawUtf8; Added: AnsiChar); overload;
  {$ifdef HASINLINE} inline; {$endif}

/// append one text buffer to a RawUtf8 variable with no code page conversion
procedure Append(var Text: RawUtf8; Added: pointer; AddedLen: PtrInt); overload;
  {$ifdef HASINLINE} inline; {$endif}

/// append some text items to a RawByteString variable
procedure Append(var Text: RawByteString; const Args: array of const); overload;

/// append one text item to a RawByteString variable with no code page conversion
procedure Append(var Text: RawByteString; const Added: RawByteString); overload;
  {$ifdef HASINLINE} inline; {$endif}

/// append one text buffer to a RawByteString variable with no code page conversion
procedure Append(var Text: RawByteString; Added: pointer; AddedLen: PtrInt); overload;

/// prepend some text to a RawByteString variable with no code page conversion
procedure Prepend(var Text: RawByteString; const Added: RawByteString); overload;

/// prepend one char to a RawByteString variable with no code page conversion
procedure Prepend(var Text: RawByteString; Added: AnsiChar); overload;

/// prepend some text items at the beginning of a RawUtf8 variable
procedure Prepend(var Text: RawUtf8; const Args: array of const); overload;

/// prepend some text items at the beginning of a RawByteString variable
procedure Prepend(var Text: RawByteString; const Args: array of const); overload;

/// append some text to a RawUtf8, ensuring previous text is separated with CRLF
// - could be used e.g. to update HTTP headers
procedure AppendLine(var Text: RawUtf8; const Args: array of const;
  const Separator: shortstring = #13#10);

/// append some path parts into a single file name with proper path delimiters
// - set EndWithDelim=true if you want to create e.g. a full folder name
// - similar to os.path.join() in the Python RTL
// - e.g. on Windows: MakePath(['abc', 1, 'toto.json']) = 'abc\1\toto.json'
function MakePath(const Part: array of const; EndWithDelim: boolean = false;
  Delim: AnsiChar = PathDelim): TFileName;

/// MakePath() variant which can handle the file extension specifically
function MakeFileName(const Part: array of const; LastIsExt: boolean = true): TFileName;

/// create a CSV text from some values
function MakeCsv(const Value: array of const; EndWithComma: boolean = false;
  Comma: AnsiChar = ','): RawUtf8;

/// direct conversion of a RTL string into a console OEM-encoded String
// - under Windows, will use the CP_OEMCP encoding
// - under Linux, will expect the console to be defined with UTF-8 encoding
function StringToConsole(const S: string): RawByteString;

/// write some text to the console using a given color
procedure ConsoleWrite(const Fmt: RawUtf8; const Args: array of const;
  Color: TConsoleColor = ccLightGray; NoLineFeed: boolean = false); overload;

/// write some text to the console using a given color
procedure ConsoleWrite(const Args: array of const;
  Color: TConsoleColor = ccLightGray; NoLineFeed: boolean = false); overload;

/// could be used in the main program block of a console application to
// handle unexpected fatal exceptions
// - WaitForEnterKey=true won't do anything on POSIX (to avoid locking a daemon)
// - typical use may be:
// !begin
// !  try
// !    ... // main console process
// !  except
// !    on E: Exception do
// !      ConsoleShowFatalException(E);
// !  end;
// !end.
procedure ConsoleShowFatalException(E: Exception; WaitForEnterKey: boolean = true);


{ ************ Resource and Time Functions }

/// convert a size to a human readable value power-of-two metric value
// - append EB, PB, TB, GB, MB, KB or B symbol with or without preceding space
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
procedure KB(bytes: Int64; out result: TShort16; nospace: boolean); overload;

/// convert a size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol with preceding space
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
function KB(bytes: Int64): TShort16; overload;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol without preceding space
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
function KBNoSpace(bytes: Int64): TShort16;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol with or without preceding space
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
function KB(bytes: Int64; nospace: boolean): TShort16; overload;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a string size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
function KB(const buffer: RawByteString): TShort16; overload;
  {$ifdef FPC_OR_UNICODE}inline;{$endif}

/// convert a size to a human readable value
// - append EB, PB, TB, GB, MB, KB or B symbol
// - for EB, PB, TB, GB, MB and KB, add one fractional digit
procedure KBU(bytes: Int64; var result: RawUtf8);

/// convert a count to a human readable value power-of-two metric value
// - append E, P, T, G, M, K symbol, with one fractional digit
procedure K(value: Int64; out result: TShort16); overload;

/// convert a count to a human readable value power-of-two metric value
// - append E, P, T, G, M, K symbol, with one fractional digit
function K(value: Int64): TShort16; overload;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a seconds elapsed time into a human readable value
// - append 's', 'm', 'h' and 'd' symbol for the given value range,
// with two fractional digits
function SecToString(S: QWord): TShort16;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a milliseconds elapsed time into a human readable value
// - append 'ms', 's', 'm', 'h' and 'd' symbol for the given value range,
// with two fractional digits
function MilliSecToString(MS: QWord): TShort16;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a micro seconds elapsed time into a human readable value
// - append 'us', 'ms', 's', 'm', 'h' and 'd' symbol for the given value range,
// with two fractional digits
function MicroSecToString(Micro: QWord): TShort16; overload;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// compute elapsed time into a human readable value, from a Start value
// - will get current QueryPerformanceMicroSeconds() and compute against Start
// - append 'us', 'ms', 's', 'm', 'h' and 'd' symbol for the given value range,
// with two fractional digits
function MicroSecFrom(Start: QWord): TShort16;
  {$ifdef FPC_OR_UNICODE}inline;{$endif} // Delphi 2007 is buggy as hell

/// convert a micro seconds elapsed time into a human readable value
// - append 'us', 'ms', 's', 'm', 'h' and 'd' symbol for the given value range,
// with two fractional digits
procedure MicroSecToString(Micro: QWord; out result: TShort16); overload;

/// convert a nano seconds elapsed time into a human readable value
// - append 'ns', 'us', 'ms', 's', 'm', 'h' and 'd' symbol for the given value
// range, with two fractional digits
procedure NanoSecToString(Nano: QWord; out result: TShort16);

/// convert "valueunit" values into x or x.xx text with up to 2 digits
// - supplied value should be the actual unit value * 100
procedure By100ToTwoDigitString(value: cardinal; const valueunit: ShortString;
  var result: TShort16);

/// convert an integer value into its textual representation with thousands marked
// - ThousandSep is the character used to separate thousands in numbers with
// more than three digits to the left of the decimal separator
function IntToThousandString(Value: integer;
  const ThousandSep: TShort4 = ','): ShortString;


{ ************ ESynException class }

{$ifndef NOEXCEPTIONINTERCEPT}

type
  /// global hook callback to customize exceptions logged by TSynLog
  // - should return TRUE if all needed information has been logged by the
  // event handler
  // - should return FALSE if Context.EAddr and Stack trace is to be appended
  TSynLogExceptionToStr = function(WR: TTextWriter;
    const Context: TSynLogExceptionContext): boolean;

var
  /// allow to customize the ESynException logging message
  TSynLogExceptionToStrCustom: TSynLogExceptionToStr = nil;

/// the default Exception handler for logging
// - defined here to be called e.g. by ESynException.CustomLog() as default
function DefaultSynLogExceptionToStr(WR: TTextWriter;
  const Context: TSynLogExceptionContext): boolean;

{$endif NOEXCEPTIONINTERCEPT}


type
  {$M+}
  /// generic parent class of all custom Exception types of this unit
  // - all our classes inheriting from ESynException are serializable,
  // so you could use ObjectToJsonDebug(anyESynException) to retrieve some
  // extended information
  ESynException = class(Exception)
  protected
    fRaisedAt: pointer;
    fMessageUtf8: RawUtf8;
    procedure CreateAfterSetMessageUtf8; virtual;
  public
    /// constructor which will use FormatUtf8() instead of Format()
    // - expect % as delimiter, so is less error prone than %s %d %g
    // - will handle vtPointer/vtClass/vtObject/vtVariant kind of arguments,
    // appending class name for any class or object, the hexa value for a
    // pointer, or the JSON representation of any supplied TDocVariant
    constructor CreateUtf8(const Format: RawUtf8; const Args: array of const);
    /// constructor will accept RawUtf8 instead of string as message text
    constructor CreateU(const Msg: RawUtf8);
    /// constructor appending some FormatUtf8() content to the GetLastError
    // - message will contain GetLastError value followed by the formatted text
    // - expect % as delimiter, so is less error prone than %s %d %g
    // - will handle vtPointer/vtClass/vtObject/vtVariant kind of arguments,
    // appending class name for any class or object, the hexa value for a
    // pointer, or the JSON representation of any supplied TDocVariant
    constructor CreateLastOSError(const Format: RawUtf8; const Args: array of const;
      const Trailer: ShortString = 'OSError');
    {$ifndef NOEXCEPTIONINTERCEPT}
    /// can be used to customize how the exception is logged
    // - this default implementation will call the TSynLogExceptionToStrCustom
    // global callback, if defined, or a default handler internal to this unit
    // - override this method to provide a custom logging content
    // - should return TRUE if Context.EAddr and Stack trace is not to be
    // written (i.e. as for any TSynLogExceptionToStr callback)
    function CustomLog(WR: TTextWriter;
      const Context: TSynLogExceptionContext): boolean; virtual;
    {$endif NOEXCEPTIONINTERCEPT}
    /// the code location when this exception was triggered
    // - populated by mormot.core.log unit, during interception - so may be nil
    // - you can use TDebugFile.FindLocation(ESynException) class function to
    // guess the corresponding source code line
    // - will be serialized as "Address": hexadecimal and source code location,
    // using TDebugFile .map/.dbg/.mab information, by JSON WriteObject
    // when woStorePointer option is defined - e.g. with ObjectToJsonDebug()
    property RaisedAt: pointer
      read fRaisedAt write fRaisedAt;
    /// the Exception Message UTF-8 text, as generated by CreateUtf8()
    property MessageUtf8: RawUtf8
      read fMessageUtf8;
  published
    /// the Exception Message string, as defined in parent Exception class
    property Message;
  end;
  {$M-}

  /// meta-class of the ESynException hierarchy
  ESynExceptionClass = class of ESynException;

/// convert any HTTP_* constant to an integer error code and its English text
// - returns e.g. 'HTTP Error 404 - Not Found', calling StatusCodeToText()
function StatusCodeToErrorMsg(Code: integer): RawUtf8;


{ **************** Hexadecimal Text And Binary Conversion }

type
  /// type of a lookup table used for fast hexadecimal conversion
  THexToDualByte = packed array[0..511] of byte;
  /// type of a lookup table used for fast XML/HTML conversion
  TAnsiCharToByte = array[AnsiChar] of byte;
  PAnsiCharToByte = ^TAnsiCharToByte;
  /// type of a lookup table used for fast two-digit chars conversion
  TAnsiCharToWord = array[AnsiChar] of word;
  PAnsiCharToWord = ^TAnsiCharToWord;
  /// type of a lookup table used for fast two-digit chars conversion
  TByteToWord = array[byte] of word;
  PByteToWord = ^TByteToWord;

var
  /// a conversion table from hexa chars into binary data
  // - [0..255] range maps the 0..15 binary, [256..511] maps 0..15 binary shl 4
  // - returns 255 for any character out of 0..9,A..Z,a..z range
  // - used e.g. by HexToBin() function
  // - is defined globally, since may be used from an inlined function
  ConvertHexToBin: THexToDualByte;

  /// fast lookup table for converting hexadecimal numbers from 0 to 15
  // into their ASCII equivalence
  // - is local for better code generation
  TwoDigitsHex: array[byte] of array[1..2] of AnsiChar;
  TwoDigitsHexW: TAnsiCharToWord absolute TwoDigitsHex;
  TwoDigitsHexWB: TByteToWord absolute TwoDigitsHex;
  /// lowercase hexadecimal lookup table
  TwoDigitsHexLower: array[byte] of array[1..2] of AnsiChar;
  TwoDigitsHexWLower: TAnsiCharToWord absolute TwoDigitsHexLower;
  TwoDigitsHexWBLower: TByteToWord absolute TwoDigitsHexLower;

/// fast conversion from hexa chars into binary data
// - BinBytes contain the bytes count to be converted: Hex^ must contain
//  at least BinBytes*2 chars to be converted, and Bin^ enough space
// - if Bin=nil, no output data is written, but the Hex^ format is checked
// - return false if any invalid (non hexa) char is found in Hex^
// - using this function with Bin^ as an integer value will decode in big-endian
// order (most-signignifican byte first)
function HexToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: PtrInt): boolean; overload;

/// fast conversion with no validity check from hexa chars into binary data
procedure HexToBinFast(Hex: PAnsiChar; Bin: PByte; BinBytes: PtrInt);

/// fast conversion from one hexa char pair into a 8-bit AnsiChar
// - return false if any invalid (non hexa) char is found in Hex^
// - similar to HexToBin(Hex,nil,1)
function HexToCharValid(Hex: PAnsiChar): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// internal conversion from hexa pair into a AnsiChar for PIC, ARM and x86_64
function HexToCharValid(Hex: PAnsiChar; HexToBin: PByteArray): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast check if the supplied Hex buffer is an hexadecimal representation
// of a binary buffer of a given number of bytes
function IsHex(const Hex: RawByteString; BinBytes: PtrInt): boolean;

/// fast conversion from one hexa char pair into a 8-bit AnsiChar
// - return false if any invalid (non hexa) char is found in Hex^
// - similar to HexToBin(Hex,Bin,1) but with Bin<>nil
// - use HexToCharValid if you want to check a hexadecimal char content
function HexToChar(Hex: PAnsiChar; Bin: PUtf8Char): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// internal conversion from hexa pair into a AnsiChar for PIC, ARM and x86_64
function HexToChar(Hex: PAnsiChar; Bin: PUtf8Char; HexToBin: PByteArray): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from two hexa bytes into a 16-bit UTF-16 WideChar
// - as used by JsonUnicodeEscapeToUtf8() for \u#### chars unescape
// - similar to HexDisplayToBin(Hex,@wordvar,2)
// - returns 0 on malformated input
function HexToWideChar(Hex: PUtf8Char): cardinal;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from binary data into hexa chars
// - BinBytes contain the bytes count to be converted: Hex^ must contain
// enough space for at least BinBytes*2 chars
// - using this function with BinBytes^ as an integer value will encode it
// in low-endian order (less-signignifican byte first): don't use it for display
procedure BinToHex(Bin, Hex: PAnsiChar; BinBytes: PtrInt); overload;

/// fast conversion from hexa chars into binary data
function HexToBin(const Hex: RawUtf8): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from hexa chars into binary data
function HexToBin(Hex: PAnsiChar; HexLen: PtrInt;
  var Bin: RawByteString): boolean; overload;

/// fast conversion from ToHumanHex() hexa chars into binary data
function HumanHexToBin(const hex: RawUtf8; var Bin: RawByteString): boolean; overload;

/// fast conversion from ToHumanHex() hexa chars into binary data
function HumanHexToBin(const hex: RawUtf8): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast comparison between two ToHumanHex() hexa values
function HumanHexCompare(const a, b: RawUtf8): integer; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast comparison between two ToHumanHex() hexa values
function HumanHexCompare(a, b: PUtf8Char): integer; overload;

/// fast conversion from binary data into hexa chars
function BinToHex(const Bin: RawByteString): RawUtf8; overload;

/// fast conversion from binary data into hexa chars
function BinToHex(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8; overload;

/// fast conversion from binary data into hexa chars, ready to be displayed
// - BinBytes contain the bytes count to be converted: Hex^ must contain
// enough space for at least BinBytes*2 chars
// - using this function with Bin^ as an integer value will encode it
// in big-endian order (most-signignifican byte first): use it for display
procedure BinToHexDisplay(Bin, Hex: PAnsiChar; BinBytes: PtrInt); overload;

/// fast conversion from binary data into hexa chars, ready to be displayed
function BinToHexDisplay(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8; overload;

/// fast conversion from binary data into lowercase hexa chars
// - BinBytes contain the bytes count to be converted: Hex^ must contain
// enough space for at least BinBytes*2 chars
// - using this function with BinBytes^ as an integer value will encode it
// in low-endian order (less-signignifican byte first): don't use it for display
procedure BinToHexLower(Bin, Hex: PAnsiChar; BinBytes: PtrInt); overload;

/// fast conversion from binary data into lowercase hexa chars
function BinToHexLower(const Bin: RawByteString): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from binary data into lowercase hexa chars
function BinToHexLower(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from binary data into lowercase hexa chars
procedure BinToHexLower(Bin: PAnsiChar; BinBytes: PtrInt; var result: RawUtf8); overload;

/// fast conversion from binary data into lowercase hexa chars
// - BinBytes contain the bytes count to be converted: Hex^ must contain
// enough space for at least BinBytes*2 chars
// - using this function with Bin^ as an integer value will encode it
// in big-endian order (most-signignifican byte first): use it for display
procedure BinToHexDisplayLower(Bin, Hex: PAnsiChar; BinBytes: PtrInt); overload;

/// fast conversion from binary data into lowercase hexa chars
function BinToHexDisplayLower(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8; overload;

/// fast conversion from up to 127 bytes of binary data into lowercase hexa chars
function BinToHexDisplayLowerShort(Bin: PAnsiChar; BinBytes: PtrInt): ShortString;

/// fast conversion from up to 64-bit of binary data into lowercase hexa chars
function BinToHexDisplayLowerShort16(Bin: Int64; BinBytes: PtrInt): TShort16;

/// fast conversion from up to 64-bit of binary data into lowercase hexa chars
// - warning: here binary size is in bits (typically 1..64), not bytes
procedure BinBitsToHexDisplayLowerShort16(Bin: Int64; BinBits: PtrInt;
  var Result: TShort16);

/// fast conversion from binary data into hexa lowercase chars, ready to be
// used as a convenient TFileName prefix
function BinToHexDisplayFile(Bin: PAnsiChar; BinBytes: PtrInt): TFileName;

/// append one byte as hexadecimal char pairs, into a text buffer
function ByteToHex(P: PAnsiChar; Value: byte): PAnsiChar;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from a pointer data into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
function PointerToHex(aPointer: Pointer): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from a pointer data into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
procedure PointerToHex(aPointer: Pointer; var result: RawUtf8); overload;

/// fast conversion from a pointer data into hexa chars, ready to be displayed
// - use internally DisplayMinChars() and BinToHexDisplay()
// - such result type would avoid a string allocation on heap
function PointerToHexShort(aPointer: Pointer): TShort16; overload;

/// fast conversion from a cardinal value into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
// - reverse function of HexDisplayToCardinal()
function CardinalToHex(aCardinal: cardinal): RawUtf8;

/// fast conversion from a cardinal value into hexa chars, ready to be displayed
// - use internally BinToHexDisplayLower()
// - reverse function of HexDisplayToCardinal()
function CardinalToHexLower(aCardinal: cardinal): RawUtf8;

/// fast conversion from a cardinal value into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
// - such result type would avoid a string allocation on heap
function CardinalToHexShort(aCardinal: cardinal): TShort16;

/// compute the hexadecimal representation of the crc32 checkum of a given text
// - wrapper around CardinalToHex(crc32c(...))
function crc32cUtf8ToHex(const str: RawUtf8): RawUtf8;

/// fast conversion from a Int64 value into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
// - reverse function of HexDisplayToInt64()
function Int64ToHex(aInt64: Int64): RawUtf8; overload;

/// fast conversion from a Int64 value into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
// - reverse function of HexDisplayToInt64()
procedure Int64ToHex(aInt64: Int64; var result: RawUtf8); overload;

/// fast conversion from a Int64 value into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
// - such result type would avoid a string allocation on heap
procedure Int64ToHexShort(aInt64: Int64; out result: TShort16); overload;

/// fast conversion from a Int64 value into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
// - such result type would avoid a string allocation on heap
function Int64ToHexShort(aInt64: Int64): TShort16; overload;

/// fast conversion for up to 256-bit of little-endian input into non-zero hexa
// - Len should be <= 32 bytes, to fit in a TShort64 result
// - use internally DisplayMinChars() and BinToHexDisplay()
function ToHexShort(P: pointer; Len: PtrInt): TShort64;

/// fast conversion from a pointer data into hexa chars, ready to be displayed
// - use internally DisplayMinChars() and BinToHexDisplay()
function Int64ToHexLower(aInt64: Int64): RawUtf8; overload;

/// fast conversion from a Int64 value into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
// - reverse function of HexDisplayToInt64()
function Int64ToHexString(aInt64: Int64): string;

/// fast conversion from hexa chars in reverse order into a binary buffer
function HexDisplayToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: PtrInt): boolean;

/// fast conversion from hexa chars in reverse order into a cardinal
// - reverse function of CardinalToHex()
// - returns false and set aValue=0 if Hex is not a valid hexadecimal 32-bit
// unsigned integer
// - returns true and set aValue with the decoded number, on success
function HexDisplayToCardinal(Hex: PAnsiChar; out aValue: cardinal): boolean;
  {$ifdef ISDELPHI}{$ifdef HASINLINE}inline;{$endif}{$endif}
  // inline gives an error under release conditions with (old?) FPC

/// fast conversion from hexa chars in reverse order into a cardinal
// - reverse function of Int64ToHex()
// - returns false and set aValue=0 if Hex is not a valid hexadecimal 64-bit
// signed integer
// - returns true and set aValue with the decoded number, on success
function HexDisplayToInt64(Hex: PAnsiChar; out aValue: Int64): boolean; overload;
    {$ifdef ISDELPHI}{$ifdef HASINLINE}inline;{$endif}{$endif}
    { inline gives an error under release conditions with FPC }

/// fast conversion from hexa chars in reverse order into a cardinal
// - reverse function of Int64ToHex()
// - returns 0 if the supplied text buffer is not a valid hexadecimal 64-bit
// signed integer
function HexDisplayToInt64(const Hex: RawByteString): Int64; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// conversion from octal C-like escape into binary data
// - \xxx is converted into a single xxx byte from octal, and \\ into \
// - will stop the conversion when Oct^=#0 or when invalid \xxx is reached
// - returns the number of bytes written to Bin^
function OctToBin(Oct: PAnsiChar; Bin: PByte): PtrInt; overload;

/// conversion from octal C-like escape into binary data
// - \xxx is converted into a single xxx byte from octal, and \\ into \
function OctToBin(const Oct: RawUtf8): RawByteString; overload;

/// append a TGuid binary content as 36 chars text
// - will store e.g. '3F2504E0-4F89-11D3-9A0C-0305E82C3301' (without any {})
// - this will be the format used for JSON encoding, e.g.
// $ { "UID": "C9A646D3-9C61-4CB7-BFCD-EE2522C8F633" }
function GuidToText(P: PUtf8Char; guid: PByteArray): PUtf8Char;

/// convert a TGuid into 38 chars encoded { text } as RawUtf8
// - will return e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with the {})
// - if you do not need the embracing { }, use ToUtf8() overloaded function
function GuidToRawUtf8(const guid: TGuid): RawUtf8;

/// convert a TGuid into 36 chars encoded text as RawUtf8
// - will return e.g. '3F2504E0-4F89-11D3-9A0C-0305E82C3301' (without the {})
// - if you need the embracing { }, use GuidToRawUtf8() function instead
function ToUtf8(const guid: TGuid): RawUtf8; overload;

/// convert a TGuid into into 38 chars encoded { text } as RTL string
// - will return e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with the {})
// - this version is faster than the one supplied by SysUtils
function GuidToString(const guid: TGuid): string;

type
  /// stack-allocated ASCII string, used by GuidToShort() function
  TGuidShortString = string[38];
  PGuidShortString = ^TGuidShortString;

/// convert a TGuid into text
// - will return e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with the {})
// - using a ShortString will allow fast allocation on the stack, so is
// preferred e.g. when providing a Guid to a ESynException.CreateUtf8()
function GuidToShort(const guid: TGuid): TGuidShortString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a TGuid into text
// - will return e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with the {})
// - using a ShortString will allow fast allocation on the stack, so is
// preferred e.g. when providing a Guid to a ESynException.CreateUtf8()
procedure GuidToShort(const
  guid: TGuid; out dest: TGuidShortString); overload;

/// convert some text into its TGuid binary value
// - expect e.g. '3F2504E0-4F89-11D3-9A0C-0305E82C3301' (without any {}) but
// will ignore internal '-' so '3F2504E04F8911D39A0C0305E82C3301' is also fine
// - note: TGuid binary order does not follow plain HexToBin or HexDisplayToBin
// - return nil if the supplied text buffer is not a valid TGuid
// - this will be the format used for JSON encoding, e.g.
// $ { "Uid": "C9A646D3-9C61-4CB7-BFCD-EE2522C8F633" }
function TextToGuid(P: PUtf8Char; Guid: PByteArray): PUtf8Char;

/// convert some RTL string text into a TGuid
// - expect e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with the {})
// - return {00000000-0000-0000-0000-000000000000} if the supplied text buffer
// is not a valid TGuid
function StringToGuid(const text: string): TGuid;

/// convert some UTF-8 encoded text into a TGuid
// - expect e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with the {})
// or '3F2504E0-4F89-11D3-9A0C-0305E82C3301' (without the {}) or even
// '3F2504E04F8911D39A0C0305E82C3301' following TGuid order (not HexToBin)
// - return {00000000-0000-0000-0000-000000000000} if the supplied text buffer
// is not a valid TGuid
function RawUtf8ToGuid(const text: RawByteString): TGuid; overload;

/// convert some UTF-8 encoded text into a TGuid
// - expect e.g. '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' (with the {})
// or '3F2504E0-4F89-11D3-9A0C-0305E82C3301' (without the {}) or even
// '3F2504E04F8911D39A0C0305E82C3301' following TGuid order (not HexToBin)
function RawUtf8ToGuid(const text: RawByteString; out guid: TGuid): boolean; overload;

/// trim any space and '{' '-' '}' chars from input to get a 32-char TGuid hexa
// - change in-place the text into lowercase hexadecimal
// - returns true if resulting text is a 128-bit cleaned hexa, false otherwise
function TrimGuid(var text: RawUtf8): boolean;

/// read a TStream content into a String
// - it will read binary or text content from the current position until the
// end (using TStream.Size)
// - uses RawByteString for byte storage, whatever the codepage is
function StreamToRawByteString(aStream: TStream; aSize: Int64 = -1;
  aCodePage: integer = CP_RAWBYTESTRING): RawByteString;

/// iterative function to retrieve the new content appended to a stream
// - aPosition should be set to 0 before the initial call
function StreamChangeToRawByteString(
  aStream: TStream; var aPosition: Int64): RawByteString;

/// create a TStream from a string content
// - uses RawByteString for byte storage, whatever the codepage is
// - in fact, the returned TStream is a TRawByteString instance, since this
// function is just a wrapper around:
// ! result := TRawByteStringStream.Create(aString);
function RawByteStringToStream(const aString: RawByteString): TStream;
  {$ifdef HASINLINE}inline;{$endif}

/// read UTF-8 text from a TStream saved with len prefix by WriteStringToStream
// - format is Length(integer):Text - use StreamToRawByteString for raw data
// - will return '' if there is no such text in the stream
// - you can set a MaxAllowedSize value, if you know how long the size should be
// - it will read from the current position in S: so if you just write into S,
// it could be a good idea to rewind it before call, e.g.:
// !  WriteStringToStream(Stream,aUtf8Text);
// !  Stream.Seek(0,soBeginning);
// !  str := ReadStringFromStream(Stream);
function ReadStringFromStream(S: TStream; MaxAllowedSize: integer = 255): RawUtf8;

/// write an UTF-8 text into a TStream with a len prefix - see ReadStringFromStream
// - format is Length(integer):Text - use RawByteStringToStream for raw data
function WriteStringToStream(S: TStream; const Text: RawUtf8): boolean;


implementation

{$ifdef FPC}
  // globally disable some FPC paranoid warnings - rely on x86_64 as reference
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable }
{$endif FPC}


{ ************ CSV-like Iterations over Text Buffers }

function IdemPCharAndGetNextItem(var source: PUtf8Char; const searchUp: RawUtf8;
  var Item: RawUtf8; Sep: AnsiChar): boolean;
begin
  if source <> nil then
    if IdemPChar(source, Pointer(searchUp)) then
    begin
      inc(source, Length(searchUp));
      GetNextItem(source, Sep, Item);
      result := true;
      exit;
    end;
  result := false;
end;

function GetNextItem(var P: PUtf8Char; Sep: AnsiChar): RawUtf8;
begin
  GetNextItem(P, Sep, result);
end;

procedure GetNextItem(var P: PUtf8Char; Sep: AnsiChar; var result: RawUtf8);
var
  S: PUtf8Char;
begin
  if P = nil then
    result := ''
  else
  begin
    S := P;
    {$ifdef CPUINTEL}
    S := PosChar(S, Sep); // SSE2 asm on i386 and x86_64
    if S = nil then
      S := P + mormot.core.base.StrLen(P);
    {$else}
    while (S^ <> #0) and
          (S^ <> Sep) do
      inc(S);
    {$endif CPUINTEL}
    FastSetString(result, P, S - P);
    if S^ <> #0 then
      P := S + 1
    else
      P := nil;
  end;
end;

function GetNextItemMultiple(var P: PUtf8Char; const Sep: RawUtf8;
  var Next: RawUtf8): AnsiChar;
var
  len: PtrInt;
begin
  if P = nil then
  begin
    Next := '';
    result := #0;
  end
  else
  begin
    len := strcspn(P, pointer(Sep)); // search size of P which are not in Sep
    FastSetString(Next, P, len);
    inc(P, len);
    result := P^;
    if result <> #0 then
      inc(P)
    else
      P := nil;
  end;
end;

procedure GetNextItem(var P: PUtf8Char; Sep, Quote: AnsiChar; var result: RawUtf8);
begin
  if P = nil then
    result := ''
  else if P^ = Quote then
  begin
    P := UnQuoteSqlStringVar(P, result);
    if P = nil then
      result := ''
    else if P^ = #0 then
      P := nil
    else
      inc(P);
  end
  else
    GetNextItem(P, Sep, result);
end;

procedure GetNextItemTrimed(var P: PUtf8Char; Sep: AnsiChar; var result: RawUtf8);
var
  S, E: PUtf8Char;
begin
  if (P = nil) or
     (Sep <= ' ') then
    result := ''
  else
  begin
    while (P^ <= ' ') and
          (P^ <> #0) do
      inc(P); // trim left
    S := P;
    while (S^ <> #0) and
          (S^ <> Sep) do
      inc(S);
    E := S;
    while (E > P) and
          (E[-1] in [#1..' ']) do
      dec(E); // trim right
    FastSetString(result, P, E - P);
    if S^ <> #0 then
      P := S + 1
    else
      P := nil;
  end;
end;

procedure GetNextItemTrimedCRLF(var P: PUtf8Char; var result: RawUtf8);
var
  S, E: PUtf8Char;
begin
  if P = nil then
    result := ''
  else
  begin
    S := P;
    while (S^ <> #0) and
          (S^ <> #10) do
      inc(S);
    E := S;
    if (E > P) and
       (E[-1] = #13) then
      dec(E);
    FastSetString(result, P, E - P);
    if S^ <> #0 then
      P := S + 1
    else
      P := nil;
  end;
end;

function GetNextItemString(var P: PChar; Sep: Char): string;
var
  S: PChar;
begin
  if P = nil then
    result := ''
  else
  begin
    S := P;
    while (S^ <> #0) and
          (S^ <> Sep) do
      inc(S);
    SetString(result, P, S - P);
    if S^ <> #0 then
      P := S + 1
    else
      P := nil;
  end;
end;

function GetFileNameExtIndex(const FileName, CsvExt: TFileName): integer;
var
  Ext: TFileName;
  P: PChar;
begin
  result := -1;
  P := pointer(CsvExt);
  Ext := ExtractFileExt(FileName);
  if (P = nil) or
     (Ext = '') or
     (Ext[1] <> '.') then
    exit;
  delete(Ext, 1, 1);
  repeat
    inc(result);
    if SameText(GetNextItemString(P), Ext) then
      exit;
  until P = nil;
  result := -1;
end;

procedure AppendCsvValues(const Csv: string; const Values: array of string;
  var Result: string; const AppendBefore: string);
var
  s: string;
  i, bool: integer;
  P: PChar;
  first: boolean;
begin
  P := pointer(Csv);
  if P = nil then
    exit;
  first := True;
  for i := 0 to high(Values) do
  begin
    s := GetNextItemString(P);
    if Values[i] <> '' then
    begin
      if first then
      begin
        Result := Result + #13#10;
        first := false;
      end
      else
        Result := Result + AppendBefore;
      bool := FindCsvIndex('0,-1', RawUtf8(Values[i]));
      Result := Result + s + ': ';
      if bool < 0 then
        Result := Result + Values[i]
      else
        Result := Result + GetCsvItemString(pointer(GetNextItemString(P)), bool, '/');
    end;
  end;
end;

procedure GetNextItemShortString(var P: PUtf8Char; Dest: PShortString; Sep: AnsiChar);
var
  S, D: PUtf8Char;
  c: AnsiChar;
  len: PtrInt;
begin
  S := P;
  D := pointer(Dest); // better FPC codegen with a dedicated variable
  if S <> nil then
  begin
    len := 0;
    if S^ <= ' ' then
      while (S^ <= ' ') and
            (S^ <> #0) do
        inc(S); // trim left space
    repeat
      c := S^;
      inc(S);
      if c = Sep then
        break;
      if c <> #0 then
        if len < 254 then // avoid shortstring buffer overflow
        begin
          inc(len);
          D[len] := c;
          continue;
        end
        else
          len := 0;
      S := nil; // reached #0: end of input
      break;
    until false;
    if len <> 0 then
      repeat
        if D[len] >= ' ' then
          break;
        dec(len); // trim right space
      until len = 0;
    D[0] := AnsiChar(len);
    D[len + 1] := #0; // trailing #0
    P := S;
  end
  else
    PCardinal(D)^ := 0 // Dest='' with trailing #0
end;

function GetNextItemHexDisplayToBin(var P: PUtf8Char;
  Bin: PByte; BinBytes: PtrInt; Sep: AnsiChar): boolean;
var
  S: PUtf8Char;
  len: integer;
begin
  result := false;
  FillCharFast(Bin^, BinBytes, 0);
  if P = nil then
    exit;
  while (P^ <= ' ') and
        (P^ <> #0) do
    inc(P);
  S := P;
  if Sep = #0 then
    while S^ > ' ' do
      inc(S)
  else
    while (S^ <> #0) and
          (S^ <> Sep) do
      inc(S);
  len := S - P;
  while (P[len - 1] in [#1..' ']) and
        (len > 0) do
    dec(len); // trim right spaces
  if len <> BinBytes * 2 then
    exit;
  if not HexDisplayToBin(PAnsiChar(P), Bin, BinBytes) then
    FillCharFast(Bin^, BinBytes, 0)
  else
  begin
    if S^ = #0 then
      P := nil
    else if Sep <> #0 then
      P := S + 1
    else
      P := S;
    result := true;
  end;
end;

function GetNextItemCardinal(var P: PUtf8Char; Sep: AnsiChar): PtrUInt;
var
  c: PtrUInt;
begin
  if P = nil then
  begin
    result := 0;
    exit;
  end;
  if P^ = ' ' then
    repeat
      inc(P)
    until P^ <> ' ';
  c := byte(P^) - 48;
  if c > 9 then
    result := 0
  else
  begin
    result := c;
    inc(P);
    repeat
      c := byte(P^) - 48;
      if c > 9 then
        break
      else
        result := result * 10 + c;
      inc(P);
    until false;
  end;
  if Sep <> #0 then
    while (P^ <> #0) and
          (P^ <> Sep) do
      inc(P); // go to end of CSV item (ignore any decimal)
  if P^ = #0 then
    P := nil
  else if Sep <> #0 then
    inc(P);
end;

function GetNextItemCardinalStrict(var P: PUtf8Char): PtrUInt;
var
  c: PtrUInt;
begin
  if P = nil then
  begin
    result := 0;
    exit;
  end;
  c := byte(P^) - 48;
  if c > 9 then
    result := 0
  else
  begin
    result := c;
    inc(P);
    repeat
      c := byte(P^) - 48;
      if c > 9 then
        break
      else
        result := result * 10 + c;
      inc(P);
    until false;
  end;
  if P^ = #0 then
    P := nil;
end;

function CsvOfValue(const Value: RawUtf8; Count: cardinal; const Sep: RawUtf8): RawUtf8;
var
  ValueLen, SepLen: PtrUInt;
  i: cardinal;
  P: PAnsiChar;
begin
  // CsvOfValue('?',3)='?,?,?'
  result := '';
  if Count = 0 then
    exit;
  ValueLen := length(Value);
  SepLen := Length(Sep);
  FastSetString(result, ValueLen * Count + SepLen * pred(Count));
  P := pointer(result);
  i := 1;
  repeat
    if ValueLen = 1 then
    begin
      P^ := Value[1]; // optimized for the Value='?' common case
      inc(P);
    end
    else
    begin
      MoveFast(Pointer(Value)^, P^, ValueLen);
      inc(P, ValueLen);
    end;
    if i = Count then
      break;
    if SepLen = 1 then
    begin
      P^ := Sep[1]; // optimized for the Sep=',' most common case
      inc(P);
      inc(i);
    end
    else if SepLen > 0 then
    begin
      MoveFast(Pointer(Sep)^, P^, SepLen);
      inc(P, SepLen);
      inc(i);
    end;
  until false;
  // assert(P-pointer(result)=length(result));
end;

procedure SetBitCsv(var Bits; BitsCount: integer; var P: PUtf8Char);
var
  bit, last: cardinal;
begin
  while P <> nil do
  begin
    bit := GetNextItemCardinalStrict(P) - 1; // '0' marks end of list
    if bit >= cardinal(BitsCount) then
      break; // avoid GPF
    if (P = nil) or
       (P^ = ',') then
      SetBitPtr(@Bits, bit)
    else if P^ = '-' then
    begin
      inc(P);
      last := GetNextItemCardinalStrict(P) - 1; // '0' marks end of list
      if last >= cardinal(BitsCount) then
        exit;
      while bit <= last do
      begin
        SetBitPtr(@Bits, bit);
        inc(bit);
      end;
    end;
    if (P <> nil) and
       (P^ = ',') then
      inc(P);
  end;
  if (P <> nil) and
     (P^ = ',') then
    inc(P);
end;

function GetBitCsv(const Bits; BitsCount: integer): RawUtf8;
var
  i, j: integer;
begin
  result := '';
  i := 0;
  while i < BitsCount do
    if GetBitPtr(@Bits, i) then
    begin
      j := i;
      while (j + 1 < BitsCount) and
            GetBitPtr(@Bits, j + 1) do
        inc(j);
      result := result + UInt32ToUtf8(i + 1);
      if j = i then
        result := result + ','
      else if j = i + 1 then
        result := result + ',' + UInt32ToUtf8(j + 1) + ','
      else
        result := result + '-' + UInt32ToUtf8(j + 1) + ',';
      i := j + 1;
    end
    else
      inc(i);
  result := result + '0'; // '0' marks end of list
end;

function GetNextItemCardinalW(var P: PWideChar; Sep: WideChar): PtrUInt;
var
  c: PtrUInt;
begin
  if P = nil then
  begin
    result := 0;
    exit;
  end;
  c := word(P^) - 48;
  if c > 9 then
    result := 0
  else
  begin
    result := c;
    inc(P);
    repeat
      c := word(P^) - 48;
      if c > 9 then
        break
      else
        result := result * 10 + c;
      inc(P);
    until false;
  end;
  while (P^ <> #0) and
        (P^ <> Sep) do // go to end of CSV item (ignore any decimal)
    inc(P);
  if P^ = #0 then
    P := nil
  else
    inc(P);
end;

function GetNextItemInteger(var P: PUtf8Char; Sep: AnsiChar): PtrInt;
var
  minus: boolean;
begin
  if P = nil then
  begin
    result := 0;
    exit;
  end;
  if P^ = ' ' then
    repeat
      inc(P)
    until P^ <> ' ';
  if P^ in ['+', '-'] then
  begin
    minus := P^ = '-';
    inc(P);
  end
  else
    minus := false;
  result := PtrInt(GetNextItemCardinal(P, Sep));
  if minus then
    result := -result;
end;

function GetNextTChar64(var P: PUtf8Char; Sep: AnsiChar; out Buf: TChar64): PtrInt;
var
  S: PUtf8Char;
  c: AnsiChar;
begin
  result := 0;
  S := P;
  if S = nil then
    exit;
  if Sep = #0 then
    repeat // store up to next whitespace
      c := S[result];
      if c <= ' ' then
        break;
      Buf[result] := c;
      inc(result);
      if result >= SizeOf(Buf) then
        exit; // avoid buffer overflow
    until false
  else
    repeat // store up to Sep or end of string
      c := S[result];
      if (c = #0) or
         (c = Sep) then
        break;
      Buf[result] := c;
      inc(result);
      if result >= SizeOf(Buf) then
        exit; // avoid buffer overflow
    until false;
  Buf[result] := #0; // make asciiz
  inc(S, result); // S[result]=Sep or #0
  if S^ = #0 then
    P := nil
  else if Sep = #0 then
    P := S
  else
    P := S + 1;
end;

{$ifdef CPU64}

function GetNextItemInt64(var P: PUtf8Char; Sep: AnsiChar): Int64;
begin
  result := GetNextItemInteger(P, Sep); // PtrInt=Int64
end;

function GetNextItemQWord(var P: PUtf8Char; Sep: AnsiChar): QWord;
begin
  result := GetNextItemCardinal(P, Sep); // PtrUInt=QWord
end;

{$else}

function GetNextItemInt64(var P: PUtf8Char; Sep: AnsiChar): Int64;
var
  tmp: TChar64;
begin
  if GetNextTChar64(P, Sep, tmp) > 0 then
    SetInt64(tmp, result)
  else
    result := 0;
end;

function GetNextItemQWord(var P: PUtf8Char; Sep: AnsiChar): QWord;
var
  tmp: TChar64;
begin
  if GetNextTChar64(P, Sep, tmp) > 0 then
    SetQWord(tmp, result)
  else
    result := 0;
end;

{$endif CPU64}

function GetNextItemHexa(var P: PUtf8Char; Sep: AnsiChar): QWord;
var
  tmp: TChar64;
  L: integer;
begin
  result := 0;
  L := GetNextTChar64(P, Sep, tmp);
  if (L > 0) and
     (L and 1 = 0) then
    if not HexDisplayToBin(@tmp, @result, L shr 1) then
      result := 0;
end;

function GetNextItemDouble(var P: PUtf8Char; Sep: AnsiChar): double;
var
  tmp: TChar64;
  err: integer;
begin
  if GetNextTChar64(P, Sep, tmp) > 0 then
  begin
    result := GetExtended(tmp, err);
    if err <> 0 then
      result := 0;
  end
  else
    result := 0;
end;

function GetNextItemCurrency(var P: PUtf8Char; Sep: AnsiChar): currency;
begin
  GetNextItemCurrency(P, result, Sep);
end;

procedure GetNextItemCurrency(var P: PUtf8Char; out result: currency; Sep: AnsiChar);
var
  tmp: TChar64;
begin
  if GetNextTChar64(P, Sep, tmp) > 0 then
    PInt64(@result)^ := StrToCurr64(tmp)
  else
    result := 0;
end;

function GetCsvItem(P: PUtf8Char; Index: PtrUInt; Sep: AnsiChar): RawUtf8;
var
  i: PtrUInt;
begin
  if P = nil then
    result := ''
  else
    for i := 0 to Index do
      GetNextItem(P, Sep, result);
end;

function GetUnQuoteCsvItem(P: PUtf8Char; Index: PtrUInt; Sep, Quote: AnsiChar): RawUtf8;
var
  i: PtrUInt;
begin
  if P = nil then
    result := ''
  else
    for i := 0 to Index do
      GetNextItem(P, Sep, Quote, result);
end;

function GetFirstCsvItem(const Csv: RawUtf8; Sep: AnsiChar): RawUtf8;
var
  i: PtrInt;
begin
  i := PosExChar(Sep, Csv);
  if i = 0 then
    result := Csv
  else
    FastSetString(result, pointer(Csv), i - 1);
end;

function GetLastCsvItem(const Csv: RawUtf8; Sep: AnsiChar): RawUtf8;
begin
  result := SplitRight(Csv, Sep, nil);
end;

function GetCsvItemString(P: PChar; Index: PtrUInt; Sep: Char): string;
var
  i: PtrUInt;
begin
  if P = nil then
    result := ''
  else
    for i := 0 to Index do
      result := GetNextItemString(P, Sep);
end;

function CsvContains(const Csv, Value: RawUtf8; Sep: AnsiChar;
  CaseSensitive: boolean): boolean;
var
  i, l: PtrInt;
  p, s: PUtf8Char;
  match: TIdemPropNameUSameLen;
begin
  if (Csv = '') or
     (Value = '') then
  begin
    result := false;
    exit;
  end;
  // note: all search sub-functions do use fast SSE2 asm on i386 and x86_64
  match := IdemPropNameUSameLen[CaseSensitive];
  p := pointer(Csv);
  l := PStrLen(PAnsiChar(pointer(Value)) - _STRLEN)^;
  if l >= PStrLen(p - _STRLEN)^ then
    result := (l = PStrLen(p - _STRLEN)^) and
              match(p, pointer(Value), l)
  else
  begin
    i := PosExChar(Sep, Csv);
    if i <> 0 then
    begin
      result := true;
      s := p + i - 1;
      repeat
        if (s - p = l) and
           match(p, pointer(Value), l) then
          exit;
        p := s + 1;
        s := PosChar(p, Sep);
        if s <> nil then
          continue;
        if (PStrLen(PAnsiChar(pointer(Csv)) - _STRLEN)^ - (p - pointer(Csv)) = l) and
           match(p, pointer(Value), l) then
          exit;
        break;
      until false;
    end;
    result := false;
  end;
end;

function FindCsvIndex(Csv: PUtf8Char; const Value: RawUtf8; Sep: AnsiChar;
  CaseSensitive, TrimValue: boolean): integer;
var
  s: RawUtf8;
begin
  result := 0;
  while Csv <> nil do
  begin
    GetNextItem(Csv, Sep, s);
    if TrimValue then
      TrimSelf(s);
    if CaseSensitive then
    begin
      if SortDynArrayRawByteString(s, Value) = 0 then
        exit;
    end
    else if SameTextU(s, Value) then
      exit;
    inc(result);
  end;
  result := -1; // not found
end;

procedure CsvToRawUtf8DynArray(Csv: PUtf8Char; var List: TRawUtf8DynArray;
  Sep: AnsiChar; TrimItems, AddVoidItems: boolean; Quote: AnsiChar);
var
  s: RawUtf8;
  n: integer;
begin
  n := length(List);
  while (Csv <> nil) and
        (Csv^ <> #0) do
  begin
    if Quote <> #0 then
    begin
      GetNextItem(Csv, Sep, Quote, s);
      if TrimItems then
        TrimSelf(s);
    end
    else if TrimItems then
      GetNextItemTrimed(Csv, Sep, s)
    else
      GetNextItem(Csv, Sep, s);
    if (s <> '') or
       AddVoidItems then
      AddRawUtf8(List, n, s);
  end;
  if List <> nil then
    DynArrayFakeLength(List, n);
end;

procedure CsvToRawUtf8DynArray(const Csv, Sep, SepEnd: RawUtf8;
  var List: TRawUtf8DynArray);
var
  offs, i, n: integer;
  s: RawUtf8;
begin
  n := length(List);
  offs := 1;
  while offs <= length(Csv) do
  begin
    i := PosEx(Sep, Csv, offs);
    if i = 0 then
    begin
      i := PosEx(SepEnd, Csv, offs);
      if i = 0 then
        i := length(csv) + 1;
      FastSetString(s, @PByteArray(Csv)[offs - 1], i - offs);
      AddRawUtf8(List, n, s);
      break;
    end;
    FastSetString(s, @PByteArray(Csv)[offs - 1], i - offs);
    AddRawUtf8(List, n, s);
    offs := i + length(Sep);
  end;
  if List <> nil then
    DynArrayFakeLength(List, n);
end;

function CsvToRawUtf8DynArray(const Csv, Sep, SepEnd: RawUtf8): TRawUtf8DynArray;
begin
  result := nil;
  CsvToRawUtf8DynArray(Csv, Sep, SepEnd, result);
end;

function AddPrefixToCsv(Csv: PUtf8Char; const Prefix: RawUtf8; Sep: AnsiChar): RawUtf8;
var
  s: RawUtf8;
begin
  GetNextItem(Csv, Sep, result);
  if result = '' then
    exit;
  result := Prefix + result;
  while Csv <> nil do
  begin
    GetNextItem(Csv, Sep, s);
    if s <> '' then
      result := result + ',' + Prefix + s;
  end;
end;

procedure AddToCsv(const Value: RawUtf8; var Csv: RawUtf8; const Sep: RawUtf8);
begin
  if Csv = '' then
    Csv := Value
  else
    Csv := Csv + Sep + Value;
end;

function RenameInCsv(const OldValue, NewValue: RawUtf8; var Csv: RawUtf8;
  const Sep: RawUtf8): boolean;
var
  pattern: RawUtf8;
  i, j: integer;
begin
  result := OldValue = NewValue;
  i := length(OldValue);
  if result or
     (length(Sep) <> 1) or
     (length(Csv) < i) or
     (PosEx(Sep, OldValue) > 0) or
     (PosEx(Sep, NewValue) > 0) then
    exit;
  if CompareMem(pointer(OldValue), pointer(Csv), i) and // first (or unique) item
    ((Csv[i + 1] = Sep[1]) or
     (Csv[i + 1] = #0)) then
    i := 1
  else
  begin
    j := 1;
    pattern := Sep + OldValue;
    repeat
      i := PosEx(pattern, Csv, j);
      if i = 0 then
        exit;
      j := i + length(pattern);
    until (Csv[j] = Sep[1]) or
          (Csv[j] = #0);
    inc(i);
  end;
  delete(Csv, i, length(OldValue));
  insert(NewValue, Csv, i);
  result := true;
end;

function CsvGuessSeparator(const Csv: RawUtf8): AnsiChar;
begin
  if PosExChar(#9, Csv) <> 0 then
    result := #9
  else if PosExChar(';', Csv) <> 0 then
    result := ';'
  else if PosExChar(',', Csv) <> 0 then
    result := ','
  else
    result := #0;
end;

function RawUtf8ArrayToCsv(const Values: array of RawUtf8; const Sep: RawUtf8;
  HighValues: integer): RawUtf8;
var
  i, len, seplen, L: integer;
  P: PAnsiChar;
begin
  result := '';
  if HighValues < 0 then
    HighValues := high(Values);
  if HighValues < 0 then
    exit;
  seplen := length(Sep);
  len := seplen * HighValues;
  for i := 0 to HighValues do
    inc(len, length(Values[i]));
  FastSetString(result, len); // allocate the result buffer as once
  P := pointer(result);
  i := 0;
  repeat
    L := length(Values[i]);
    if L > 0 then
    begin
      MoveFast(pointer(Values[i])^, P^, L);
      inc(P, L);
    end;
    if i = HighValues then
      break;
    if seplen > 0 then
    begin
      MoveFast(pointer(Sep)^, P^, seplen);
      inc(P, seplen);
    end;
    inc(i);
  until false;
end;

function RawUtf8ArrayToQuotedCsv(const Values: array of RawUtf8;
  const Sep: RawUtf8; Quote: AnsiChar): RawUtf8;
var
  i: integer;
  tmp: TRawUtf8DynArray;
begin
  SetLength(tmp, length(Values));
  for i := 0 to High(Values) do
    QuotedStr(Values[i], Quote, tmp[i]);
  result := RawUtf8ArrayToCsv(tmp, Sep);
end;

procedure CsvToIntegerDynArray(Csv: PUtf8Char; var List: TIntegerDynArray;
  Sep: AnsiChar);
var
  n: integer;
begin
  n := length(List);
  while (Csv <> nil) and
        (Csv^ <> #0) do
    AddInteger(List, n, GetNextItemInteger(Csv, Sep));
  if List <> nil then
    DynArrayFakeLength(List, n);
end;

procedure CsvToInt64DynArray(Csv: PUtf8Char; var List: TInt64DynArray;
  Sep: AnsiChar);
var
  n: integer;
begin
  n := length(List);
  while (Csv <> nil) and
        (Csv^ <> #0) do
    AddInt64(List, n, GetNextItemInt64(Csv, Sep));
  if List <> nil then
    DynArrayFakeLength(List, n);
end;

function CsvToInt64DynArray(Csv: PUtf8Char; Sep: AnsiChar): TInt64DynArray;
var
  n: integer;
begin
  result := nil;
  n := 0;
  while (Csv <> nil) and
        (Csv^ <> #0) do
    AddInt64(result, n, GetNextItemInt64(Csv, Sep));
  if result <> nil then
    DynArrayFakeLength(result, n);
end;

const // first byte is the len, then 20 bytes buffer for the 64-bit integer text
  I2T_SIZE = 21; // as TSynTempBuffer = up to 194 integers on stack

procedure IntToText(int: PAnsiChar; len, n: PtrInt; const pref, suf: RawUtf8;
  inlin: boolean; sep: AnsiChar; var result: RawUtf8);
var
  L: PtrUInt;
  P: PAnsiChar;
begin
  inc(len, (n - 1) + length(pref) + length(suf));
  if inlin then
    inc(len, n * 4); // :( ): markers
  FastSetString(result, len);
  P := pointer(result);
  if pref <> '' then
  begin
    L := length(pref);
    MoveFast(pointer(pref)^, P^, L);
    inc(P, L);
  end;
  if inlin then
    repeat
      PCardinal(P)^ := ord(':') + ord('(') shl 8;
      inc(P, 2);
      MoveFast(int[I2T_SIZE - ord(int^)], P^, ord(int^));
      inc(P, ord(int^));
      PCardinal(P)^ := ord(')') + ord(':') shl 8;
      inc(P, 2);
      dec(n);
      if n = 0 then
        break;
      inc(int, I2T_SIZE);
      P^ := sep;
      inc(P);
    until false
  else
    repeat
      L := ord(int^);
      MoveFast(PAnsiChar(int)[I2T_SIZE - L], P^, L);
      inc(P, L);
      dec(n);
      if n = 0 then
        break;
      inc(int, I2T_SIZE);
      P^ := sep;
      inc(P);
    until false;
  if suf <> '' then
    MoveFast(pointer(suf)^, P^, length(suf));
end;

function IntegerDynArrayToCsv(Values: PIntegerArray; ValuesCount: integer;
  const Prefix, Suffix: RawUtf8; InlinedValue: boolean; SepChar: AnsiChar): RawUtf8;
var
  i, L, Len: PtrInt;
  int, P: PAnsiChar;
  temp: TSynTempBuffer; // faster than a dynamic array
begin
  result := '';
  if ValuesCount = 0 then
    exit;
  int := temp.Init(ValuesCount * I2T_SIZE);
  try
    Len := 0;
    for i := 0 to ValuesCount - 1 do
    begin
      P := StrInt32(int + I2T_SIZE, Values[i]);
      L := int + I2T_SIZE - P;
      int^ := AnsiChar(L);
      inc(Len, L);
      inc(int, I2T_SIZE);
    end;
    IntToText(temp.buf, Len, ValuesCount, Prefix, Suffix, InlinedValue, SepChar, result);
  finally
    temp.Done;
  end;
end;

function Int64DynArrayToCsv(Values: PInt64Array; ValuesCount: integer;
  const Prefix, Suffix: RawUtf8; InlinedValue: boolean; SepChar: AnsiChar): RawUtf8;
var
  i, L, Len: PtrInt;
  int, P: PAnsiChar;
  temp: TSynTempBuffer; // faster than a dynamic array
begin
  result := '';
  if ValuesCount = 0 then
    exit;
  int := temp.Init(ValuesCount * I2T_SIZE);
  try
    Len := 0;
    for i := 0 to ValuesCount - 1 do
    begin
      P := StrInt64(int + I2T_SIZE, Values[i]);
      L := int + I2T_SIZE - P;
      int^ := AnsiChar(L);
      inc(Len, L);
      inc(int, I2T_SIZE);
    end;
    IntToText(temp.buf, Len, ValuesCount, Prefix, Suffix, InlinedValue, SepChar, result);
  finally
    temp.Done;
  end;
end;

function IntegerDynArrayToCsv(const Values: TIntegerDynArray;
  const Prefix, Suffix: RawUtf8; InlinedValue: boolean; SepChar: AnsiChar): RawUtf8;
begin
  result := IntegerDynArrayToCsv(pointer(Values), length(Values),
    Prefix, Suffix, InlinedValue, SepChar);
end;

function Int64DynArrayToCsv(const Values: TInt64DynArray;
  const Prefix, Suffix: RawUtf8; InlinedValue: boolean; SepChar: AnsiChar): RawUtf8;
begin
  result := Int64DynArrayToCsv(pointer(Values), length(Values),
    Prefix, Suffix, InlinedValue, SepChar);
end;


{ ************ TTextWriter parent class for Text Generation }

function HexToChar(Hex: PAnsiChar; Bin: PUtf8Char): boolean; // for inlining
var
  b, c: byte;
  {$ifdef CPUX86NOTPIC}
  tab: THexToDualByte absolute ConvertHexToBin;
  {$else}
  tab: PByteArray; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  if Hex <> nil then
  begin
    {$ifndef CPUX86NOTPIC}
    tab := @ConvertHexToBin;
    {$endif CPUX86NOTPIC}
    b := tab[ord(Hex[0]) + 256]; // + 256 for shl 4
    c := tab[ord(Hex[1])];
    if (b <> 255) and
       (c <> 255) then
    begin
      if Bin <> nil then
      begin
        inc(c, b);
        Bin^ := AnsiChar(c);
      end;
      result := true;
      exit;
    end;
  end;
  result := false; // return false if any invalid char
end;


{ TTextWriter }

{$ifndef PUREMORMOT2}
var
  DefaultTextWriterTrimEnum: boolean; // see TTextWriter.SetDefaultEnumTrim()

class procedure TTextWriter.SetDefaultEnumTrim(aShouldTrimEnumsAsText: boolean);
begin
  DefaultTextWriterTrimEnum := aShouldTrimEnumsAsText;
end;
{$endif PUREMORMOT2}

procedure TTextWriter.InternalSetBuffer(aBuf: PUtf8Char; const aBufSize: PtrUInt);
begin
  fTempBufSize := aBufSize;
  fTempBuf := aBuf;
  dec(aBuf);
  B := aBuf; // Add() methods will append at B+1
  BEnd := @aBuf[aBufSize - 15]; // BEnd := B-16 to avoid overwrite/overread
  {$ifndef PUREMORMOT2}
  if DefaultTextWriterTrimEnum then
    Include(fCustomOptions, twoTrimLeftEnumSets);
  {$endif PUREMORMOT2}
end;

constructor TTextWriter.Create(aStream: TStream; aBufSize: integer);
begin
  SetStream(aStream);
  if aBufSize < 256 then
    aBufSize := 256;
  SetBuffer(nil, aBufSize);
end;

constructor TTextWriter.Create(aStream: TStream; aBuf: pointer; aBufSize: integer);
begin
  SetStream(aStream);
  SetBuffer(aBuf, aBufSize);
end;

var
  TextWriterSharedStreamSafe: TLightLock; // thread-safe instance acquisition
  TextWriterSharedStream: TRawByteStringStream;

constructor TTextWriter.CreateOwnedStream(
  aBuf: pointer; aBufSize: integer; NoSharedStream: boolean);
begin
  if (not NoSharedStream) and TextWriterSharedStreamSafe.TryLock then
    fStream := TextWriterSharedStream
  else
    fStream := TRawByteStringStream.Create; // inlined SetStream()
  fCustomOptions := [twoStreamIsOwned, twoStreamIsRawByteString];
  SetBuffer(aBuf, aBufSize); // aBuf may be nil
end;

constructor TTextWriter.CreateOwnedStream(aBufSize: integer; NoSharedStream: boolean);
begin
  CreateOwnedStream(nil, aBufSize, NoSharedStream);
end;

constructor TTextWriter.CreateOwnedStream(var aStackBuf: TTextWriterStackBuffer;
  aBufSize: integer; NoSharedStream: boolean);
begin
  if aBufSize > SizeOf(aStackBuf) then // too small -> allocate on heap
    CreateOwnedStream(nil, aBufSize, NoSharedStream)
  else
    CreateOwnedStream(aStackBuf, NoSharedStream);
end;

constructor TTextWriter.CreateOwnedStream(
  var aStackBuf: TTextWriterStackBuffer; NoSharedStream: boolean);
begin
  if (not NoSharedStream) and TextWriterSharedStreamSafe.TryLock then
    fStream := TextWriterSharedStream
  else
    fStream := TRawByteStringStream.Create; // inlined SetStream()
  fCustomOptions := [twoStreamIsOwned, twoStreamIsRawByteString, twoBufferIsExternal];
  InternalSetBuffer(@aStackBuf, SizeOf(aStackBuf));
end;

constructor TTextWriter.CreateOwnedFileStream(
  const aFileName: TFileName; aBufSize: integer);
begin
  DeleteFile(aFileName);
  fStream := TFileStreamEx.Create(aFileName, fmCreate or fmShareRead);
  fCustomOptions := [twoStreamIsOwned];
  SetBuffer(nil, aBufSize);
end;

destructor TTextWriter.Destroy;
begin
  if twoStreamIsOwned in fCustomOptions then
    if fStream = TextWriterSharedStream then
    begin
      TRawByteStringStream(fStream).Clear; // for proper reuse
      TextWriterSharedStreamSafe.UnLock;
    end
    else
      fStream.Free;
  if not (twoBufferIsExternal in fCustomOptions) then
    FreeMem(fTempBuf);
  inherited Destroy;
end;

function TTextWriter.PendingBytes: PtrUInt;
begin
  result := B - fTempBuf + 1;
end;

procedure TTextWriter.Add(c: AnsiChar);
begin
  if B >= BEnd then
    FlushToStream; // may rewind B -> not worth any local PUtf8Char variable
  B[1] := c;
  inc(B);
end;

procedure TTextWriter.AddDirect(c: AnsiChar);
begin
  B[1] := c;
  inc(B);
end;

procedure TTextWriter.AddDirect(c1, c2: AnsiChar);
begin
  PCardinal(B + 1)^ := byte(c1) + PtrUInt(byte(c2)) shl 8;
  inc(B, 2); // with proper constant propagation above when inlined
end;

procedure TTextWriter.AddComma;
begin
  B[1] := ',';
  inc(B);
end;

procedure TTextWriter.Add(c1, c2: AnsiChar);
begin
  if B >= BEnd then
    FlushToStream;
  PCardinal(B + 1)^ := byte(c1) + PtrUInt(byte(c2)) shl 8;
  inc(B, 2); // with proper constant propagation above when inlined
end;

procedure TTextWriter.Add(const Format: RawUtf8; const Values: array of const;
  Escape: TTextWriterKind; WriteObjectOptions: TTextWriterWriteObjectOptions);
var
  tmp: RawUtf8;
begin
  // basic implementation: see faster and more complete version in TJsonWriter
  FormatUtf8(Format, Values, tmp);
  case Escape of
    twNone:
      AddString(tmp);
    twOnSameLine:
      AddOnSameLine(pointer(tmp)); // minimalistic version for TSynLog
    twJsonEscape:
      raise ESynException.CreateUtf8(
        '%.Add(twJsonEscape) unimplemented: use TJsonWriter', [self]);
  end;
end;

procedure TTextWriter.AddVariant(const Value: variant; Escape: TTextWriterKind;
  WriteOptions: TTextWriterWriteObjectOptions);
begin
  raise ESynException.CreateUtf8(
    '%.AddVariant unimplemented: use TJsonWriter', [self]);
end;

procedure TTextWriter.AddTypedJson(Value, TypeInfo: pointer;
  WriteOptions: TTextWriterWriteObjectOptions);
begin
  raise ESynException.CreateUtf8(
    '%.AddTypedJson unimplemented: use TJsonWriter', [self]);
end;

function TTextWriter.{%H-}AddJsonReformat(Json: PUtf8Char;
  Format: TTextWriterJsonFormat; EndOfObject: PUtf8Char): PUtf8Char;
begin
  raise ESynException.CreateUtf8(
    '%.AddJsonReformat unimplemented: use TJsonWriter', [self]);
end;

procedure TTextWriter.Add(P: PUtf8Char; Escape: TTextWriterKind);
begin
  raise ESynException.CreateUtf8(
    '%.Add(..,Escape: TTextWriterKind) unimplemented: use TJsonWriter', [self]);
end;

procedure TTextWriter.Add(P: PUtf8Char; Len: PtrInt; Escape: TTextWriterKind);
begin
  raise ESynException.CreateUtf8(
    '%.Add(..,Escape: TTextWriterKind) unimplemented: use TJsonWriter', [self]);
end;

procedure TTextWriter.WrBase64(P: PAnsiChar; Len: PtrUInt; withMagic: boolean);
begin
  raise ESynException.CreateUtf8(
    '%.WrBase64() unimplemented: use TJsonWriter', [self]);
end;

procedure TTextWriter.AddShorter(const Short8: TShort8);
begin
  if B >= BEnd then
    FlushToStream;
  PInt64(B + 1)^ := PInt64(@Short8[1])^;
  inc(B, ord(Short8[0]));
end;

procedure TTextWriter.AddNull;
begin
  if B >= BEnd then
    FlushToStream;
  PCardinal(B + 1)^ := NULL_LOW;
  inc(B, 4);
end;

procedure TTextWriter.WriteObject(Value: TObject;
  WriteOptions: TTextWriterWriteObjectOptions);
begin
  raise ESynException.CreateUtf8(
    '%.WriteObject unimplemented: use TJsonWriter', [self]);
end;

procedure TTextWriter.AddObjArrayJson(const aObjArray;
  aOptions: TTextWriterWriteObjectOptions);
var
  i: PtrInt;
  a: TObjectDynArray absolute aObjArray;
begin
  Add('[');
  for i := 0 to length(a) - 1 do
  begin
    WriteObject(a[i], aOptions);
    AddComma;
  end;
  CancelLastComma(']');
end;

procedure TTextWriter.WriteToStream(data: pointer; len: PtrUInt);
var
  written: PtrUInt;
begin
  if Assigned(fOnFlushToStream) then
    fOnFlushToStream(data, len);
  if (len <> 0) and
     Assigned(fStream) then
    repeat
      written := fStream.Write(data^, len);
      if PtrInt(written) <= 0 then
        if twoNoWriteToStreamException in fCustomOptions then
          break // silent failure
        else
          raise ESynException.CreateUtf8(
            '%.WriteToStream failed on %', [self, fStream]);
      inc(fTotalFileSize, written);
      dec(len, written);
      if len = 0 then
        break;
      inc(PByte(data), written); // several calls to Write() may be needed
    until false;
end;

function TTextWriter.GetTextLength: PtrUInt;
begin
  result := PtrUInt(self);
  if self <> nil then
    result := PtrUInt(B - fTempBuf + 1) + fTotalFileSize - fInitialStreamPosition;
end;

procedure TTextWriter.SetBuffer(aBuf: pointer; aBufSize: integer);
begin
  if aBufSize <= 16 then
    raise ESynException.CreateUtf8('%.SetBuffer(size=%)', [self, aBufSize]);
  if aBuf = nil then
    GetMem(aBuf, aBufSize)
  else
    Include(fCustomOptions, twoBufferIsExternal);
  InternalSetBuffer(aBuf, aBufSize);
end;

procedure TTextWriter.SetStream(aStream: TStream);
begin
  exclude(fCustomOptions, twoStreamIsRawByteString);
  if fStream <> nil then
    if twoStreamIsOwned in fCustomOptions then
    begin
      if fStream = TextWriterSharedStream then
      begin
        TRawByteStringStream(fStream).Clear; // for proper reuse
        TextWriterSharedStreamSafe.UnLock;
        fStream := nil;
      end
      else
        FreeAndNilSafe(fStream);
      exclude(fCustomOptions, twoStreamIsOwned);
    end;
  if aStream = nil then
    exit;
  fStream := aStream;
  fInitialStreamPosition := fStream.Position;
  fTotalFileSize := fInitialStreamPosition;
  if aStream.InheritsFrom(TRawByteStringStream) then
    include(fCustomOptions, twoStreamIsRawByteString);
end;

procedure TTextWriter.FlushFinal;
var
  len: PtrInt;
begin // don't mess with twoFlushToStreamNoAutoResize: it may not be final
  len := B - fTempBuf + 1;
  if len > 0 then
    WriteToStream(fTempBuf, len);
  B := fTempBuf - 1;
end;

procedure TTextWriter.FlushToStream;
var
  tmp, written: PtrUInt;
begin
  FlushFinal;
  if twoFlushToStreamNoAutoResize in fCustomOptions then
    exit;
  written := fTotalFileSize - fInitialStreamPosition;
  tmp := fTempBufSize;
  if (tmp < 49152) and
     (written > PtrUInt(tmp) * 4) then
    // tune small (stack-allocated?) buffer to grow by twice its size
    fTempBufSize := fTempBufSize * 2
  else if (written > 40 shl 20) and
          (tmp < 1 shl 20) then
    // total > 40MB -> grow internal buffer to 1MB
    fTempBufSize := 1 shl 20
  else
    // nothing to change about internal buffer size
    exit;
  if twoBufferIsExternal in fCustomOptions then
    // use heap, not stack from now on
    exclude(fCustomOptions, twoBufferIsExternal)
  else
    // from big content comes bigger buffer - but no need to realloc/move
    FreeMem(fTempBuf);
  GetMem(fTempBuf, fTempBufSize);
  BEnd := fTempBuf + (fTempBufSize - 16); // as in SetBuffer()
  B := fTempBuf - 1;
end;

procedure TTextWriter.ForceContent(const text: RawUtf8);
begin
  CancelAll;
  if (fInitialStreamPosition = 0) and
     (twoStreamIsRawByteString in fCustomOptions) then
    TRawByteStringStream(fStream).DataString := text
  else
    fStream.WriteBuffer(pointer(text)^, length(text));
  fTotalFileSize := fInitialStreamPosition + PtrUInt(length(text));
end;

procedure TTextWriter.SetText(var result: RawUtf8; reformat: TTextWriterJsonFormat);
var
  Len: PtrUInt;
  temp: TTextWriter;
begin
  FlushFinal;
  Len := fTotalFileSize - fInitialStreamPosition;
  if Len = 0 then
  begin
    result := '';
    exit;
  end;
  if twoStreamIsRawByteString in fCustomOptions then
    TRawByteStringStream(fStream).GetAsText(fInitialStreamPosition, Len, result)
  else if fStream.InheritsFrom(TCustomMemoryStream) then
    with TCustomMemoryStream(fStream) do
      FastSetString(result, PAnsiChar(Memory) + fInitialStreamPosition, Len)
  else
  begin
    FastSetString(result, Len);
    fStream.Seek(fInitialStreamPosition, soBeginning);
    fStream.Read(pointer(result)^, Len);
  end;
  if reformat <> jsonCompact then
  begin
    // reformat using the very same temp buffer but not the same RawUtf8
    temp := DefaultJsonWriter.CreateOwnedStream(fTempBuf, fTempBufSize);
    try
      temp.AddJsonReformat(pointer(result), reformat, nil);
      temp.SetText(result);
    finally
      temp.Free;
    end;
  end;
end;

function TTextWriter.Text: RawUtf8;
begin
  SetText(result);
end;

procedure TTextWriter.CancelAll;
begin
  if self = nil then
    exit; // avoid GPF
  if fTotalFileSize <> 0 then
    fTotalFileSize := fStream.Seek(fInitialStreamPosition, soBeginning);
  B := fTempBuf - 1;
end;

procedure TTextWriter.CancelAllAsNew;
begin
  CancelAll;
  fCustomOptions := fCustomOptions * TEXTWRITEROPTIONS_RESET;
end;

procedure TTextWriter.CancelAllWith(var temp: TTextWriterStackBuffer);
begin
  if fTotalFileSize <> 0 then
    fTotalFileSize := fStream.Seek(fInitialStreamPosition, soBeginning);
  InternalSetBuffer(@temp, SizeOf(temp));
end;

procedure TTextWriter.CancelLastChar(aCharToCancel: AnsiChar);
var
  P: PUtf8Char;
begin
  P := B;
  if (P >= fTempBuf) and
     (P^ = aCharToCancel) then
    dec(B);
end;

procedure TTextWriter.CancelLastChar;
begin
  if B >= fTempBuf then // Add() methods append at B+1
    dec(B);
end;

procedure TTextWriter.CancelLastComma;
var
  P: PUtf8Char;
begin
  P := B;
  if (P >= fTempBuf) and
     (P^ = ',') then
    dec(B);
end;

procedure TTextWriter.CancelLastComma(aReplaceChar: AnsiChar);
var
  P: PUtf8Char;
begin
  P := B;
  if (P < fTempBuf) or
     (P^ <> ',') then
  begin
    inc(P);
    B := P;
  end;
  P^ := aReplaceChar;
end;

function TTextWriter.LastChar: AnsiChar;
begin
  if B >= fTempBuf then
    result := B^
  else
    result := #0;
end;

procedure TTextWriter.AddOnce(c: AnsiChar);
begin
  if (B >= fTempBuf) and
     (B^ = c) then
    exit; // no duplicate
  if B >= BEnd then
    FlushToStream;
  B[1] := c;
  inc(B);
end;

procedure TTextWriter.Add(Value: PtrInt);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
  Len: PtrInt;
begin
  if BEnd - B <= 23 then
    FlushToStream;
  {$ifndef ASMINTEL} // our StrInt32 asm has less CPU cache pollution
  if PtrUInt(Value) <= high(SmallUInt32Utf8) then
  begin
    P := pointer(SmallUInt32Utf8[Value]);
    Len := PStrLen(P - _STRLEN)^;
  end
  else
  {$endif ASMINTEL}
  begin
    P := StrInt32(@tmp[23], Value);
    Len := @tmp[23] - P;
  end;
  MoveFast(P^, B[1], Len);
  inc(B, Len);
end;

{$ifdef CPU32} // Add(Value: PtrInt) already implemented it for CPU64
procedure TTextWriter.Add(Value: Int64);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
  Len: integer;
begin
  if BEnd - B <= 24 then
    FlushToStream;
  if Value < 0 then
  begin
    P := StrUInt64(@tmp[23], -Value) - 1;
    P^ := '-';
    Len := @tmp[23] - P;
  end
  {$ifndef ASMINTEL} // our StrUInt32 asm has less CPU cache pollution
  else if Value <= high(SmallUInt32Utf8) then
  begin
    P := pointer(SmallUInt32Utf8[Value]);
    Len := PStrLen(P - _STRLEN)^;
  end
  {$endif ASMINTEL} // our StrInt32 asm has less CPU cache pollution
  else
  begin
    P := StrUInt64(@tmp[23], Value);
    Len := @tmp[23] - P;
  end;
  MoveByOne(P, B + 1, Len);
  inc(B, Len);
end;
{$endif CPU32}

procedure TTextWriter.AddCurr64(Value: PInt64);
var
  tmp: array[0..31] of AnsiChar;
  P: PAnsiChar;
  Len: PtrInt;
begin
  if BEnd - B <= 31 then
    FlushToStream;
  P := StrCurr64(@tmp[31], Value^);
  Len := @tmp[31] - P;
  if Len > 4 then
    if P[Len - 1] = '0' then
      if P[Len - 2] = '0' then
        if P[Len - 3] = '0' then
          if P[Len - 4] = '0' then
            dec(Len, 5) // 'xxx.0000' -> 'xxx'
          else
            dec(Len, 3) // 'xxx.1000' -> 'xxx.1'
        else
          dec(Len, 2) // 'xxx.1200' -> 'xxx.12'
      else
        dec(Len); // 'xxx.1220' -> 'xxx.123'
  MoveFast(P^, B[1], Len);
  inc(B, Len);
end;

procedure TTextWriter.AddCurr(const Value: currency);
begin
  AddCurr64(PInt64(@Value));
end;

procedure TTextWriter.AddU(Value: cardinal);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
  Len: PtrInt;
begin
  if BEnd - B <= 24 then
    FlushToStream;
  {$ifndef ASMINTEL} // our StrUInt32 asm has less CPU cache pollution
  if Value <= high(SmallUInt32Utf8) then
  begin
    P := pointer(SmallUInt32Utf8[Value]);
    Len := PStrLen(P - _STRLEN)^;
  end
  else
  {$endif ASMINTEL}
  begin
    P := StrUInt32(@tmp[23], Value);
    Len := @tmp[23] - P;
  end;
  MoveFast(P^, B[1], Len);
  inc(B, Len);
end;

procedure TTextWriter.AddUHex(Value: cardinal; QuotedChar: AnsiChar);
begin
  AddBinToHexDisplayLower(@Value, SizeOf(Value), QuotedChar);
end;

procedure TTextWriter.AddQ(Value: QWord);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
  Len: PtrInt;
begin
  if BEnd - B <= 32 then
    FlushToStream;
  {$ifndef ASMINTEL} // our StrInt32 asm has less CPU cache pollution
  if Value <= high(SmallUInt32Utf8) then
  begin
    P := pointer(SmallUInt32Utf8[Value]);
    Len := PStrLen(P - _STRLEN)^;
  end
  else
  {$endif ASMINTEL}
  begin
    P := StrUInt64(@tmp[23], Value);
    Len := @tmp[23] - P;
  end;
  MoveFast(P^, B[1], Len);
  inc(B, Len);
end;

procedure TTextWriter.AddQHex(Value: Qword; QuotedChar: AnsiChar);
begin
  AddBinToHexDisplayLower(@Value, SizeOf(Value), QuotedChar);
end;

procedure TTextWriter.Add(Value: Extended; precision: integer; noexp: boolean);
var
  tmp: ShortString;
begin
  AddShort(ExtendedToJson(@tmp, Value, precision, noexp)^);
end;

procedure TTextWriter.AddDouble(Value: double; noexp: boolean);
var
  tmp: ShortString;
begin
  AddShort(DoubleToJson(@tmp, Value, noexp)^);
end;

procedure TTextWriter.AddSingle(Value: single; noexp: boolean);
var
  tmp: ShortString;
begin
  AddShort(ExtendedToJson(@tmp, Value, SINGLE_PRECISION, noexp)^);
end;

procedure TTextWriter.Add(Value: boolean);
var
  PS: PShortString;
begin
  if Value then // normalize: boolean may not be in the expected [0,1] range
    PS := @BOOL_STR[true]
  else
    PS := @BOOL_STR[false];
  AddShorter(PS^);
end;

procedure TTextWriter.AddFloatStr(P: PUtf8Char);
begin
  if mormot.core.base.StrLen(P) > 127 then
    exit; // clearly invalid input
  if BEnd - B <= 127 then
    FlushToStream;
  inc(B);
  if P <> nil then
    B := FloatStrCopy(P, B) - 1
  else
    B^ := '0';
end;

procedure TTextWriter.Add(Value: PGuid; QuotedChar: AnsiChar);
begin
  if BEnd - B <= 38 then
    FlushToStream;
  inc(B);
  if QuotedChar <> #0 then
  begin
    B^ := QuotedChar;
    inc(B);
  end;
  B := GuidToText(B, pointer(Value));
  if QuotedChar <> #0 then
    B^ := QuotedChar
  else
    dec(B);
end;

procedure TTextWriter.AddCR;
begin
  if B >= BEnd then
    FlushToStream;
  PCardinal(B + 1)^ := 13 + 10 shl 8; // CR + LF
  inc(B, 2);
end;

procedure TTextWriter.AddCRAndIndent;
var
  ntabs: cardinal;
begin
  if B^ = #9 then
    // we just already added an indentation level - do it once
    exit;
  ntabs := fHumanReadableLevel;
  if ntabs >= cardinal(fTempBufSize) then
    ntabs := 0; // fHumanReadableLevel=-1 after the last level of a document
  if BEnd - B <= PtrInt(ntabs) then
    FlushToStream;
  PCardinal(B + 1)^ := 13 + 10 shl 8; // CR + LF
  if ntabs > 0 then
    FillCharFast(B[3], ntabs, 9); // #9=tab
  inc(B, ntabs + 2);
end;

procedure TTextWriter.AddChars(aChar: AnsiChar; aCount: PtrInt);
var
  n: PtrInt;
begin
  while aCount > 0 do
  begin
    n := BEnd - B;
    if n <= aCount then
    begin
      FlushToStream;
      n := BEnd - B;
    end;
    if aCount < n then
      n := aCount;
    FillCharFast(B[1], n, ord(aChar));
    inc(B, n);
    dec(aCount, n);
  end;
end;

procedure TTextWriter.Add2(Value: PtrUInt);
begin
  if B >= BEnd then
    FlushToStream;
  if Value > 99 then
    PCardinal(B + 1)^ := $3030 + ord(',') shl 16
  else     // '00,' if overflow
    PCardinal(B + 1)^ := TwoDigitLookupW[Value] + ord(',') shl 16;
  inc(B, 3);
end;

procedure TTextWriter.Add3(Value: cardinal);
var
  V: cardinal;
begin
  if B >= BEnd then
    FlushToStream;
  if Value > 999 then
    PCardinal(B + 1)^ := $303030 // '000,' if overflow
  else
  begin
    V := Value div 10;
    PCardinal(B + 1)^ := TwoDigitLookupW[V] + (Value - V * 10 + 48) shl 16;
  end;
  inc(B, 4);
  B^ := ',';
end;

procedure TTextWriter.Add4(Value: PtrUInt);
begin
  if B >= BEnd then
    FlushToStream;
  if Value > 9999 then
    PCardinal(B + 1)^ := $30303030 // '0000,' if overflow
  else
    YearToPChar(Value, B + 1);
  inc(B, 5);
  B^ := ',';
end;

function Value3Digits(V: cardinal; P: PUtf8Char; W: PWordArray): cardinal;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := V div 100;
  PWord(P + 1)^ := W[V - result * 100];
  V := result;
  result := result div 10;
  P^ := AnsiChar(V - result * 10 + 48);
end;

procedure TTextWriter.AddMicroSec(MicroSec: cardinal);
var
  W: PWordArray;
begin
  // in 00.000.000 TSynLog format
  if B >= BEnd then
    FlushToStream;
  B[3] := '.';
  B[7] := '.';
  inc(B);
  W := @TwoDigitLookupW;
  MicroSec := Value3Digits(Value3Digits(MicroSec, B + 7, W), B + 3, W);
  if MicroSec > 99 then
    MicroSec := $3939
  else
    MicroSec := W[MicroSec];
  PWord(B)^ := MicroSec;
  inc(B, 9);
end;

procedure TTextWriter.AddCsvInteger(const Integers: array of integer);
var
  i: PtrInt;
begin
  if length(Integers) = 0 then
    exit;
  for i := 0 to high(Integers) do
  begin
    Add(Integers[i]);
    AddComma;
  end;
  CancelLastComma;
end;

procedure TTextWriter.AddCsvDouble(const Doubles: array of double);
var
  i: PtrInt;
begin
  if length(Doubles) = 0 then
    exit;
  for i := 0 to high(Doubles) do
  begin
    AddDouble(Doubles[i]);
    AddComma;
  end;
  CancelLastComma;
end;

procedure TTextWriter.AddNoJsonEscapeBig(P: Pointer; Len: PtrInt);
var
  direct: PtrInt;
  D: PUtf8Char;
  comma: boolean;
begin
  if (P <> nil) and
     (Len > 0) then
    if Len < fTempBufSize * 2 then
      repeat
        D := B + 1;
        direct := BEnd - D; // guess biggest size available in fTempBuf at once
        if direct > 0 then  // 0..-15 may happen because Add up to BEnd + 16
        begin
          if Len < direct then
            direct := Len;
          // append UTF-8 bytes to fTempBuf
          if direct > 0 then
          begin
            MoveFast(P^, D^, direct);
            inc(B, direct);
          end;
          dec(Len, direct);
          if Len = 0 then
            break;
          inc(PByte(P), direct);
        end;
        FlushToStream;
      until false
    else
    begin
      FlushFinal; // no auto-resize if content is really huge
      comma := PAnsiChar(P)[Len - 1] = ',';
      if comma then
        dec(Len);
      WriteToStream(P, Len); // no need to transit huge content into fTempBuf
      if comma then
        AddDirect(','); // but we need the last comma to be cancelable
    end;
end;

procedure TTextWriter.AddNoJsonEscape(P: Pointer; Len: PtrInt);
begin
  if (P <> nil) and
     (Len > 0) then
    if Len < fTempBufSize then // inlined for small chunk
    begin
      if BEnd - B <= Len then
        FlushToStream;
      MoveFast(P^, B[1], Len);
      inc(B, Len);
    end
    else
      AddNoJsonEscapeBig(P, Len); // big chunks
end;

procedure TTextWriter.AddNoJsonEscape(P: Pointer);
begin
  if P <> nil then
    AddNoJsonEscape(P, mormot.core.base.StrLen(PUtf8Char(P)));
end;

procedure EngineAppendUtf8(W: TTextWriter; Engine: TSynAnsiConvert;
  P: PAnsiChar; Len: PtrInt);
var
  tmp: TSynTempBuffer;
begin
  // explicit conversion using a temporary buffer on stack
  Len := Engine.AnsiBufferToUtf8(tmp.Init(Len * 3), P, Len) - PUtf8Char({%H-}tmp.buf);
  W.AddNoJsonEscape(tmp.buf, Len);
  tmp.Done;
end;

procedure TTextWriter.AddNoJsonEscape(P: PAnsiChar; Len: PtrInt; CodePage: cardinal);
var
  B: PAnsiChar;
begin
  if Len > 0 then
    case CodePage of
      CP_UTF8, CP_RAWBYTESTRING, CP_RAWBLOB:
        AddNoJsonEscape(P, Len);
      CP_UTF16:
        AddNoJsonEscapeW(PWord(P), 0);
    else
      begin
        // first handle trailing 7-bit ASCII chars, by quad
        B := P;
        if Len >= 4 then
          repeat
            if PCardinal(P)^ and $80808080 <> 0 then
              break; // break on first non ASCII quad
            inc(P, 4);
            dec(Len, 4);
          until Len < 4;
        if (Len > 0) and
           (P^ <= #127) then
          repeat
            inc(P);
            dec(Len);
          until (Len = 0) or
                (P^ > #127);
        if P <> B then
          AddNoJsonEscape(B, P - B);
        if Len > 0 then
          // rely on explicit conversion for all remaining ASCII characters
          EngineAppendUtf8(self, TSynAnsiConvert.Engine(CodePage), P, Len);
      end;
    end;
end;

procedure TTextWriter.AddNoJsonEscapeUtf8(const text: RawByteString);
begin
  AddNoJsonEscape(pointer(text), length(text));
end;

procedure TTextWriter.AddRawJson(const json: RawJson);
begin
  if json = '' then
    AddNull
  else
    AddNoJsonEscape(pointer(json), length(json));
end;

procedure TTextWriter.AddNoJsonEscapeString(const s: string);
begin
  if s <> '' then
    {$ifdef UNICODE}
    AddNoJsonEscapeW(pointer(s), 0);
    {$else}
    AddNoJsonEscape(pointer(s), length(s),
      Unicode_CodePage); // =CurrentAnsiConvert.CodePage
    {$endif UNICODE}
end;

procedure TTextWriter.AddNoJsonEscapeW(WideChar: PWord; WideCharCount: integer);
var
  PEnd: PtrUInt;
  c: cardinal;
begin
  if WideChar = nil then
    exit;
  if WideCharCount = 0 then
    repeat
      if B >= BEnd then
        FlushToStream;
      c := WideChar^;
      if c = 0 then
        break
      else if c <= 127 then
      begin
        B[1] := AnsiChar(c);
        inc(WideChar);
        inc(B);
      end
      else
        inc(B, Utf16CharToUtf8(B + 1, WideChar));
    until false
  else
  begin
    PEnd := PtrUInt(WideChar) + PtrUInt(WideCharCount) * SizeOf(WideChar^);
    repeat
      if B >= BEnd then
        FlushToStream;
      c := WideChar^;
      if c = 0 then
        break
      else if c <= 127 then
      begin
        B[1] := AnsiChar(c);
        inc(WideChar);
        inc(B);
        if PtrUInt(WideChar) < PEnd then
          continue
        else
          break;
      end;
      inc(B, Utf16CharToUtf8(B + 1, WideChar));
      if PtrUInt(WideChar) < PEnd then
        continue
      else
        break;
    until false;
  end;
end;

procedure TTextWriter.AddProp(PropName: PUtf8Char);
begin
  AddProp(PropName, mormot.core.base.StrLen(PropName));
end;

procedure TTextWriter.AddProp(PropName: PUtf8Char; PropNameLen: PtrInt);
begin // not faster with a local P: PUtf8Char temp pointer instead of B
  if PropNameLen <= 0 then
    exit; // paranoid check
  if BEnd - B <= PropNameLen then
    FlushToStream;
  if twoForceJsonExtended in fCustomOptions then
  begin
    MoveFast(PropName^, B[1], PropNameLen);
    inc(B, PropNameLen + 1);
    B^ := ':';
  end
  else
  begin
    B[1] := '"';
    MoveFast(PropName^, B[2], PropNameLen);
    inc(B, PropNameLen + 2);
    PCardinal(B)^ := ord('"') + ord(':') shl 8;
    inc(B);
  end;
end;

procedure TTextWriter.AddPropName(const PropName: ShortString);
begin
  AddProp(@PropName[1], ord(PropName[0]));
end;

procedure TTextWriter.AddPropInt64(const PropName: ShortString;
  Value: Int64; WithQuote: AnsiChar);
begin
  AddProp(@PropName[1], ord(PropName[0]));
  if WithQuote <> #0 then
  begin
    B[1] := WithQuote;
    inc(B);
  end;
  Add(Value);
  inc(B);
  if WithQuote <> #0 then
  begin
    B^ := WithQuote;
    inc(B);
  end;
  B^ := ',';
end;

procedure TTextWriter.AddFieldName(const FieldName: RawUtf8);
begin
  AddProp(Pointer(FieldName), length(FieldName));
end;

procedure TTextWriter.AddQuotedFieldName(const FieldName, VoidPlaceHolder: RawUtf8);
begin
  AddQuotedFieldName(pointer(FieldName), length(FieldName), VoidPlaceHolder);
end;

procedure TTextWriter.AddQuotedFieldName(
   FieldName: PUtf8Char; FieldNameLen: PtrInt; const VoidPlaceHolder: RawUtf8);
begin
  if FieldNameLen = 0 then
  begin
    FieldName := pointer(VoidPlaceHolder);
    FieldNameLen := length(VoidPlaceHolder);
  end;
  if BEnd - B <= FieldNameLen then
    FlushToStream;
  B[1] := '"';
  MoveFast(FieldName^, B[2], FieldNameLen);
  inc(B, FieldNameLen + 2);
  B^ := '"';
end;

procedure TTextWriter.AddClassName(aClass: TClass);
begin
  if aClass <> nil then
    AddShort(ClassNameShort(aClass)^);
end;

procedure TTextWriter.AddInstanceName(Instance: TObject; SepChar: AnsiChar);
begin
  Add('"');
  if Instance = nil then
    AddShorter('void')
  else
    AddShort(ClassNameShort(Instance)^);
  AddDirect('(');
  AddPointer(PtrUInt(Instance));
  AddDirect(')', '"');
  if SepChar <> #0 then
    AddDirect(SepChar);
end;

procedure TTextWriter.AddInstancePointer(Instance: TObject; SepChar: AnsiChar;
  IncludeUnitName, IncludePointer: boolean);
var
  u: PShortString;
begin
  if IncludeUnitName and
     Assigned(ClassUnit) then
  begin
    u := ClassUnit(PClass(Instance)^);
    if u^[0] <> #0 then
    begin
      AddShort(u^);
      AddDirect('.');
    end;
  end;
  AddShort(PPShortString(PPAnsiChar(Instance)^ + vmtClassName)^^);
  if IncludePointer then
  begin
    AddDirect('(');
    AddPointer(PtrUInt(Instance));
    AddDirect(')');
  end;
  if SepChar<>#0 then
    AddDirect(SepChar);
end;

procedure TTextWriter.AddShort(Text: PUtf8Char; TextLen: PtrInt);
begin
  if TextLen <= 0 then
    exit;
  if BEnd - B <= TextLen then
    FlushToStream;
  MoveFast(Text^, B[1], TextLen);
  inc(B, TextLen);
end;

procedure TTextWriter.AddShort(const Text: ShortString);
begin
  if BEnd - B <= 255 then
    FlushToStream;
  MoveFast(Text[1], B[1], ord(Text[0]));
  inc(B, ord(Text[0]));
end;

procedure TTextWriter.AddLine(const Text: ShortString);
var
  L: PtrInt;
begin
  L := ord(Text[0]);
  if BEnd - B <= L then
    FlushToStream;
  inc(B);
  if L > 0 then
  begin
    MoveFast(Text[1], B^, L);
    inc(B, L);
  end;
  PCardinal(B)^ := 13 + 10 shl 8; // CR + LF
  inc(B);
end;

procedure TTextWriter.AddOnSameLine(P: PUtf8Char);
var
  D: PUtf8Char;
  c: AnsiChar;
begin
  if P = nil then
    exit;
  D := B + 1;
  if P^ <> #0 then
    repeat
      if D >= BEnd then
      begin
        B := D - 1;
        FlushToStream;
        D := B + 1;
      end;
      c := P^;
      if c < ' ' then
        if c = #0 then
          break
        else
          c := ' ';
      D^ := c;
      inc(P);
      inc(D);
    until false;
  B := D - 1;
end;

procedure TTextWriter.AddOnSameLine(P: PUtf8Char; Len: PtrInt);
var
  D: PUtf8Char;
  c: AnsiChar;
begin
  if (P = nil) or
     (Len <= 0) then
    exit;
  D := B + 1;
  repeat
    if D >= BEnd then
    begin
      B := D - 1;
      FlushToStream;
      D := B + 1;
    end;
    c := P^;
    if c < ' ' then
      c := ' ';
    D^ := c;
    inc(D);
    inc(P);
    dec(Len);
  until Len = 0;
  B := D - 1;
end;

procedure TTextWriter.AddOnSameLineW(P: PWord; Len: PtrInt);
var
  PEnd: PtrUInt;
  c: cardinal;
begin
  if P = nil then
    exit;
  if Len = 0 then
    PEnd := 0
  else
    PEnd := PtrUInt(P) + PtrUInt(Len) * SizeOf(WideChar);
  while (Len = 0) or
        (PtrUInt(P) < PEnd) do
  begin
    if B >= BEnd then
      FlushToStream;
    // escape chars, so that all content will stay on the same text line
    c := P^;
    case c of
      0:
        break;
      1..32:
        begin
          B[1] := ' ';
          inc(B);
          inc(P);
        end;
      33..127:
        begin
          B[1] := AnsiChar(c); // direct store 7-bit ASCII
          inc(B);
          inc(P);
        end;
    else // characters higher than #127 -> UTF-8 encode
      inc(B, Utf16CharToUtf8(B + 1, P));
    end;
  end;
end;

procedure TTextWriter.AddOnSameLineString(const Text: string);
begin
  {$ifdef UNICODE}
  AddOnSameLineW(pointer(Text), length(Text));
  {$else}
  AddOnSameLine(pointer(Text), length(Text));
  {$endif UNICODE}
end;

procedure TTextWriter.AddTrimLeftLowerCase(Text: PShortString);
var
  P: PUtf8Char;
  L: PtrInt;
begin
  L := ord(Text^[0]);
  P := @Text^[1];
  while (L > 0) and
        (P^ in ['a'..'z']) do
  begin
    inc(P);
    dec(L);
  end;
  if L = 0 then
  begin
    L := ord(Text^[0]);
    P := @Text^[1];
  end;
  AddShort(P, L);
end;

procedure TTextWriter.AddTrimSpaces(const Text: RawUtf8);
begin
  AddTrimSpaces(pointer(Text));
end;

procedure TTextWriter.AddTrimSpaces(P: PUtf8Char);
var
  c: AnsiChar;
begin
  if P <> nil then
    repeat
      c := P^;
      inc(P);
      if c > ' ' then
        Add(c);
    until c = #0;
end;

procedure TTextWriter.AddReplace(Text: PUtf8Char; Orig, Replaced: AnsiChar);
begin
  if Text <> nil then
    while Text^ <> #0 do
    begin
      if Text^ = Orig then
        Add(Replaced)
      else
        Add(Text^);
      inc(Text);
    end;
end;

procedure TTextWriter.AddByteToHex(Value: PtrUInt);
begin
  if B >= BEnd then
    FlushToStream;
  PCardinal(B + 1)^ := TwoDigitsHexWB[Value];
  inc(B, 2);
end;

procedure TTextWriter.AddByteToHexLower(Value: PtrUInt);
begin
  if B >= BEnd then
    FlushToStream;
  PCardinal(B + 1)^ := TwoDigitsHexWBLower[Value];
  inc(B, 2);
end;

procedure TTextWriter.AddInt18ToChars3(Value: cardinal);
begin
  if B >= BEnd then
    FlushToStream;
  PCardinal(B + 1)^ := ((Value shr 12) and $3f) or
                       ((Value shr 6) and $3f) shl 8 or
                       (Value and $3f) shl 16 + $202020;
  inc(B, 3);
end;

procedure TTextWriter.AddString(const Text: RawUtf8);
var
  L: PtrInt;
begin
  L := PtrInt(Text);
  if L <> 0 then
    AddNoJsonEscape(pointer(Text), PStrLen(L - _STRLEN)^);
end;

procedure TTextWriter.AddSpaced(Text: PUtf8Char; TextLen, Width: PtrInt);
begin
  if Width <= TextLen then
    TextLen := Width // truncate text right
  else
    AddChars(' ', Width - TextLen);
  AddNoJsonEscape(Text, TextLen);
end;

procedure TTextWriter.AddSpaced(const Text: RawUtf8; Width: PtrInt;
  SepChar: AnsiChar);
begin
  AddSpaced(pointer(Text), length(Text), Width);
  if SepChar <> #0 then
    Add(SepChar);
end;

procedure TTextWriter.AddSpaced(Value: QWord; Width: PtrInt; SepChar: AnsiChar);
var
  tmp: array[0..23] of AnsiChar;
  alt: TShort16;
  p: PAnsiChar;
  len: PtrInt;
begin
  p := StrUInt64(@tmp[23], Value);
  len := @tmp[23] - p;
  if len > Width then
  begin
    K(Value, alt); // truncate to xxxK or xxxM
    p := @alt[1];
    len := ord(alt[0]);
  end;
  AddSpaced(p, len);
  if SepChar <> #0 then
    Add(SepChar);
end;

procedure TTextWriter.AddStringCopy(const Text: RawUtf8; start, len: PtrInt);
var
  L: PtrInt;
begin
  L := PtrInt(Text);
  if (len <= 0) or
     (L = 0) then
    exit;
  if start < 0 then
    start := 0
  else
    dec(start);
  L := PStrLen(L - _STRLEN)^;
  dec(L, start);
  if L > 0 then
  begin
    if len < L then
      L := len;
    AddNoJsonEscape(@PByteArray(Text)[start], L);
  end;
end;

procedure TTextWriter.AddStrings(const Text: array of RawUtf8);
var
  i: PtrInt;
begin
  for i := 0 to high(Text) do
    AddString(Text[i]);
end;

procedure TTextWriter.AddStrings(const Text: RawUtf8; count: PtrInt);
var
  i, L, siz: PtrInt;
begin
  L := length(Text);
  siz := L * count;
  if siz > 0 then
    if siz > fTempBufSize then
      for i := 1 to count do
        AddString(Text) // would overfill our buffer -> manual append
    else
    begin
      if BEnd - B <= siz then
        FlushToStream;
      for i := 1 to count do
      begin
        MoveFast(pointer(Text)^, B[1], L); // direct in-memory append
        inc(B, L);
      end;
    end;
end;

procedure TTextWriter.AddBinToHexDisplay(Bin: pointer; BinBytes: PtrInt);
begin
  if cardinal(BinBytes * 2 - 1) >= cardinal(fTempBufSize) then
    exit;
  if BEnd - B <= BinBytes * 2 then
    FlushToStream;
  BinToHexDisplay(Bin, PAnsiChar(B + 1), BinBytes);
  inc(B, BinBytes * 2);
end;

procedure TTextWriter.AddBinToHexDisplayLower(Bin: pointer; BinBytes: PtrInt;
  QuotedChar: AnsiChar);
var
  max: PtrUInt;
begin
  max := PtrUInt(BinBytes) * 2 + 1;
  if PtrUInt(BEnd - B) <= max then
    if max >= cardinal(fTempBufSize) then
      exit // too big for a single call
    else
      FlushToStream;
  inc(B);
  if QuotedChar <> #0 then
  begin
    B^ := QuotedChar;
    inc(B);
  end;
  BinToHexDisplayLower(Bin, pointer(B), BinBytes);
  inc(B, BinBytes * 2);
  if QuotedChar <> #0 then
    B^ := QuotedChar
  else
    dec(B);
end;

procedure TTextWriter.AddBinToHexDisplayQuoted(Bin: pointer; BinBytes: PtrInt);
begin
  AddBinToHexDisplayLower(Bin, BinBytes, '"');
end;

function DisplayMinChars(Bin: PByteArray; BinBytes: PtrInt): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := BinBytes;
  repeat // append hexa chars up to the last non zero byte
    dec(result);
  until (result = 0) or
        (Bin[result] <> 0);
  inc(result);
end;

procedure TTextWriter.AddBinToHexDisplayMinChars(Bin: pointer; BinBytes: PtrInt;
  QuotedChar: AnsiChar);
begin
  if BinBytes > 0 then
    AddBinToHexDisplayLower(Bin, DisplayMinChars(Bin, BinBytes), QuotedChar);
end;

procedure TTextWriter.AddPointer(P: PtrUInt; QuotedChar: AnsiChar);
begin
  AddBinToHexDisplayLower(@P, DisplayMinChars(@P, SizeOf(P)), QuotedChar);
end;

procedure TTextWriter.AddBinToHex(Bin: Pointer; BinBytes: PtrInt; LowerHex: boolean);
var
  chunk: PtrInt;
begin
  if BinBytes <= 0 then
    exit;
  if B >= BEnd then
    FlushToStream;
  inc(B);
  repeat
    // guess biggest size to be added into buf^ at once
    chunk := (BEnd - B) shr 1; // div 2 -> two hexa chars per byte
    if BinBytes < chunk then
      chunk := BinBytes;
    // add hexa characters
    if LowerHex then
      mormot.core.text.BinToHexLower(PAnsiChar(Bin), PAnsiChar(B), chunk)
    else
      mormot.core.text.BinToHex(PAnsiChar(Bin), PAnsiChar(B), chunk);
    inc(B, chunk * 2);
    inc(PByte(Bin), chunk);
    dec(BinBytes, chunk);
    if BinBytes = 0 then
      break;
    // FlushToStream writes B-fTempBuf+1 -> special one below:
    WriteToStream(fTempBuf, B - fTempBuf);
    B := fTempBuf;
  until false;
  dec(B); // allow CancelLastChar
end;

procedure TTextWriter.AddBinToHexMinChars(Bin: Pointer; BinBytes: PtrInt;
  LowerHex: boolean);
begin
  if BinBytes > 0 then
    AddBinToHex(Bin, DisplayMinChars(Bin, BinBytes), LowerHex);
end;

procedure TTextWriter.AddQuotedStr(Text: PUtf8Char; TextLen: PtrUInt;
  Quote: AnsiChar; TextMaxLen: PtrInt);
var
  q: PtrInt;
begin
  Add(Quote);
  if (TextMaxLen > 5) and
     (TextLen > PtrUInt(TextMaxLen)) then
    TextLen := TextMaxLen - 5
  else
    TextMaxLen := 0;
  inc(TextLen, PtrUInt(Text)); // PUtf8Char(TextLen)=TextEnd
  if Text <> nil then
  begin
    repeat
      q := ByteScanIndex(pointer(Text), PUtf8Char(TextLen) - Text, byte(Quote));
      if q < 0 then
      begin
        AddNoJsonEscape(Text, PUtf8Char(TextLen) - Text); // no double quote
        break;
      end;
      inc(q); // include first Quote
      AddNoJsonEscape(Text, q);
      Add(Quote); // double Quote
      inc(Text, q); // continue
    until false;
    if TextMaxLen <> 0 then
      AddShorter('...');
  end;
  Add(Quote);
end;

procedure TTextWriter.AddUrlNameNormalize(U: PUtf8Char; L: PtrInt);
begin
  if L <= 0 then
    exit;
  repeat
    if B >= BEnd then
      FlushToStream; // inlined Add() in the loop
    inc(B);
    case U^ of
      #0:
        begin
          dec(B); // reached end of URI (should not happen if L is accurate)
          break;
        end;
      '%':
        if (L <= 2) or
           not HexToChar(PAnsiChar(U + 1), B) then
          B^ := '%'  // browsers may not follow the RFC (e.g. encode % as % !)
        else
        begin
          inc(U, 2); // jump %xx
          dec(L, 2);
        end;
      '/':
         if (L = 1) or
            (U[1] <> '/') then
           B^ := '/'
         else
           dec(B); // normalize URI by ignoring this first /
    else
      B^ := U^;
    end;
    inc(U);
    dec(L);
  until L = 0;
end;

var
  HTML_ESC: array[hfAnyWhere..hfWithinAttributes] of TAnsiCharToByte;
  HTML_ESCAPED: array[1..4] of string[7] = (
    '&lt;', '&gt;', '&amp;', '&quot;');

procedure TTextWriter.AddHtmlEscape(Text: PUtf8Char; Fmt: TTextWriterHtmlFormat);
var
  beg: PUtf8Char;
  esc: PAnsiCharToByte;
begin
  if Text = nil then
    exit;
  if Fmt <> hfNone then
  begin
    esc := @HTML_ESC[Fmt];
    beg := Text;
    repeat
      while esc[Text^] = 0 do
        inc(Text);
      AddNoJsonEscape(beg, Text - beg);
      if Text^ = #0 then
        exit
      else
        AddShorter(HTML_ESCAPED[esc[Text^]]);
      inc(Text);
      beg := Text;
    until Text^ = #0;
  end
  else
    AddNoJsonEscape(Text, mormot.core.base.StrLen(Text)); // hfNone
end;

function HtmlEscape(const text: RawUtf8; fmt: TTextWriterHtmlFormat): RawUtf8;
var
  temp: TTextWriterStackBuffer;
  W: TTextWriter;
begin
  if NeedsHtmlEscape(pointer(text), fmt) then
  begin
    W := TTextWriter.CreateOwnedStream(temp);
    try
      W.AddHtmlEscape(pointer(text), fmt);
      W.SetText(result);
    finally
      W.Free;
    end;
  end
  else
    result := text;
end;

function HtmlEscapeString(const text: string; fmt: TTextWriterHtmlFormat): RawUtf8;
var
  temp: TTextWriterStackBuffer;
  W: TTextWriter;
begin
  {$ifdef UNICODE}
  if fmt = hfNone then
  {$else}
  if not NeedsHtmlEscape(pointer(text), fmt) then // work for any AnsiString
  {$endif UNICODE}
  begin
    StringToUtf8(text, result);
    exit;
  end;
  W := TTextWriter.CreateOwnedStream(temp);
  try
    W.AddHtmlEscapeString(text, fmt);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function NeedsHtmlEscape(Text: PUtf8Char; Fmt: TTextWriterHtmlFormat): boolean;
var
  esc: PAnsiCharToByte;
begin
  if (Text <> nil) and
     (Fmt <> hfNone) then
  begin
    result := true;
    esc := @HTML_ESC[Fmt];
    repeat
      if esc[Text^] <> 0 then
        if Text^ = #0 then
          break
        else
          exit;
      inc(Text);
    until false;
  end;
  result := false;
end;

procedure TTextWriter.AddHtmlEscape(Text: PUtf8Char; TextLen: PtrInt;
  Fmt: TTextWriterHtmlFormat);
var
  beg: PUtf8Char;
  esc: PAnsiCharToByte;
begin
  if (Text = nil) or
     (TextLen <= 0) then
    exit;
  if Fmt = hfNone then
  begin
    AddNoJsonEscape(Text, TextLen);
    exit;
  end;
  inc(TextLen, PtrInt(Text)); // TextLen = final PtrInt(Text)
  esc := @HTML_ESC[Fmt];
  repeat
    beg := Text;
    while (PtrUInt(Text) < PtrUInt(TextLen)) and
          (esc[Text^] = 0) do
      inc(Text);
    AddNoJsonEscape(beg, Text - beg);
    if (PtrUInt(Text) = PtrUInt(TextLen)) or
       (Text^ = #0) then
      exit
    else
      AddShorter(HTML_ESCAPED[esc[Text^]]);
    inc(Text);
  until false;
end;

procedure TTextWriter.AddHtmlEscapeW(Text: PWideChar;
  Fmt: TTextWriterHtmlFormat);
var
  tmp: TSynTempBuffer;
begin
  if (Text = nil) or
     (Fmt = hfNone) then
  begin
    AddNoJsonEscapeW(pointer(Text), 0);
    exit;
  end;
  RawUnicodeToUtf8(Text, StrLenW(Text), tmp, [ccfNoTrailingZero]);
  AddHtmlEscape(tmp.buf, tmp.Len, Fmt);
  tmp.Done;
end;

procedure TTextWriter.AddHtmlEscapeString(const Text: string; Fmt: TTextWriterHtmlFormat);
var
  tmp: TSynTempBuffer;
  len: integer;
begin
  len := StringToUtf8(Text, tmp);
  AddHtmlEscape(tmp.buf, len, Fmt);
  tmp.Done;
end;

procedure TTextWriter.AddHtmlEscapeUtf8(const Text: RawUtf8; Fmt: TTextWriterHtmlFormat);
begin
  AddHtmlEscape(pointer(Text), length(Text), Fmt);
end;

var
  XML_ESC: TAnsiCharToByte;

procedure TTextWriter.AddXmlEscape(Text: PUtf8Char);
var
  i, beg: PtrInt;
  esc: PAnsiCharToByte;
begin
  if Text = nil then
    exit;
  esc := @XML_ESC;
  i := 0;
  repeat
    if esc[Text[i]] = 0 then
    begin
      beg := i;
      repeat // it is faster to handle all not-escaped chars at once
        inc(i);
      until esc[Text[i]] <> 0;
      AddNoJsonEscape(Text + beg, i - beg);
    end;
    repeat
      case Text[i] of
        #0:
          exit;
        #1..#8, #11, #12, #14..#31:
          ; // ignore invalid character - see http://www.w3.org/TR/xml/#NT-Char
        #9, #10, #13:
          begin
            // characters below ' ', #9 e.g. -> // '&#x09;'
            AddShorter('&#x');
            AddByteToHex(ord(Text[i]));
            AddDirect(';');
          end;
        '<':
          AddShorter('&lt;');
        '>':
          AddShorter('&gt;');
        '&':
          AddShorter('&amp;');
        '"':
          AddShorter('&quot;');
        '''':
          AddShorter('&apos;');
      else
        break; // should match XML_ESC[] lookup table
      end;
      inc(i);
    until false;
  until false;
end;


{ TEchoWriter }

constructor TEchoWriter.Create(Owner: TTextWriter);
begin
  fWriter := Owner;
  if Assigned(fWriter.OnFlushToStream) then
    raise ESynException.CreateUtf8('Unexpected %.Create', [self]);
  fWriter.OnFlushToStream := FlushToStream; // register
end;

destructor TEchoWriter.Destroy;
begin
  if (fWriter <> nil) and
     (TMethod(fWriter.OnFlushToStream).Data = self) then
    fWriter.OnFlushToStream := nil; // unregister
  inherited Destroy;
end;

procedure TEchoWriter.EchoPendingToBackground(aLevel: TSynLogLevel);
var
  n, cap: PtrInt;
begin
  fBackSafe.Lock;
  try
    n := fBack.Count;
    if length(fBack.Level) = n then
    begin
      cap := NextGrow(n);
      SetLength(fBack.Level, cap);
      SetLength(fBack.Text, cap);
    end;
    fBack.Level[n] := aLevel;
    fBack.Text[n] := fEchoBuf;
  finally
    fBackSafe.UnLock;
  end;
end;

procedure TEchoWriter.AddEndOfLine(aLevel: TSynLogLevel);
var
  e: PtrInt;
begin
  if twoEndOfLineCRLF in fWriter.CustomOptions then
    fWriter.AddCR
  else
    fWriter.Add(#10);
  if fEchos = nil then
    exit; // no redirection yet
  fEchoStart := EchoFlush;
  if fEchoPendingExecuteBackground then
    EchoPendingToBackground(aLevel)
  else
    for e := length(fEchos) - 1 downto 0 do // for MultiEventRemove() below
      try
        fEchos[e](self, aLevel, fEchoBuf);
      except // remove callback in case of exception during echoing
        MultiEventRemove(fEchos, e);
      end;
  fEchoBuf := '';
end;

procedure TEchoWriter.EchoPendingExecute;
var
  todo: TEchoWriterBack; // thread-safe per reference copy
  i, e: PtrInt;
begin
  if fBack.Count = 0 then
    exit;
  fBackSafe.Lock;
  MoveFast(fBack, todo, SizeOf(fBack)); // fast copy without refcount
  FillCharFast(fBack, SizeOf(fBack), 0);
  fBackSafe.UnLock;
  for i := 0 to todo.Count - 1 do
    for e := length(fEchos) - 1 downto 0 do // for MultiEventRemove() below
      try
        fEchos[e](self, todo.Level[i], todo.Text[i]);
      except // remove callback in case of exception during echoing in user code
        MultiEventRemove(fEchos, e);
        if fEchos = nil then
          break;
      end;
end;

procedure TEchoWriter.FlushToStream(Text: PUtf8Char; Len: PtrInt);
begin
  if fEchos = nil then
    exit;
  EchoFlush;
  fEchoStart := 0;
end;

procedure TEchoWriter.EchoAdd(const aEcho: TOnTextWriterEcho);
begin
  if self <> nil then
    if MultiEventAdd(fEchos, TMethod(aEcho)) then
      if fEchos <> nil then
        fEchoStart := fWriter.B - fWriter.fTempBuf + 1; // ignore any previous buffer
end;

procedure TEchoWriter.EchoRemove(const aEcho: TOnTextWriterEcho);
begin
  if self <> nil then
    MultiEventRemove(fEchos, TMethod(aEcho));
end;

function TEchoWriter.EchoFlush: PtrInt;
var
  L, LI: PtrInt;
  P: PUtf8Char;
begin
  P := fWriter.fTempBuf;
  result := fWriter.B - P + 1;
  L := result - fEchoStart;
  inc(P, fEchoStart);
  while (L > 0) and
        (P[L - 1] in [#10, #13]) do // trim right CR/LF chars
    dec(L);
  if L = 0 then
    exit;
  LI := length(fEchoBuf); // fast append to fEchoBuf
  SetLength(fEchoBuf, LI + L);
  MoveFast(P^, PByteArray(fEchoBuf)[LI], L);
end;

procedure TEchoWriter.EchoReset;
begin
  fEchoBuf := '';
end;

function TEchoWriter.GetEndOfLineCRLF: boolean;
begin
  result := twoEndOfLineCRLF in fWriter.CustomOptions;
end;

procedure TEchoWriter.SetEndOfLineCRLF(aEndOfLineCRLF: boolean);
begin
  if aEndOfLineCRLF then
    fWriter.CustomOptions := fWriter.CustomOptions + [twoEndOfLineCRLF]
  else
    fWriter.CustomOptions := fWriter.CustomOptions - [twoEndOfLineCRLF];
end;


function ObjectToJson(Value: TObject; Options: TTextWriterWriteObjectOptions): RawUtf8;
begin
  ObjectToJson(Value, result, Options);
end;

procedure ObjectToJson(Value: TObject; var Result: RawUtf8;
  Options: TTextWriterWriteObjectOptions);
var
  temp: TTextWriterStackBuffer;
begin
  if Value = nil then
    Result := NULL_STR_VAR
  else
    with DefaultJsonWriter.CreateOwnedStream(temp) do
    try
      include(fCustomOptions, twoForceJsonStandard);
      WriteObject(Value, Options);
      SetText(Result);
    finally
      Free;
    end;
end;

function ObjectToJsonDebug(Value: TObject;
  Options: TTextWriterWriteObjectOptions): RawUtf8;
begin
  // our JSON serialization detects and serialize Exception.Message
  result := ObjectToJson(Value, Options);
end;

procedure ConsoleObject(Value: TObject; Options: TTextWriterWriteObjectOptions);
begin
  ConsoleWrite(ObjectToJson(Value, Options));
end;

function EscapeHexBuffer(src, dest: PUtf8Char; srclen: integer;
  const toescape: TSynAnsicharSet; escape: AnsiChar): PUtf8Char;
begin
  result := dest;
  if srclen > 0 then
    repeat
      if src^ in toescape then
      begin
        result^ := escape;
        result := pointer(ByteToHex(pointer(result + 1), ord(src^)));
      end
      else
      begin
        result^ := src^;
        inc(result);
      end;
      inc(src);
      dec(srclen);
    until srclen = 0;
end;

function EscapeHex(const src: RawUtf8;
  const toescape: TSynAnsicharSet; escape: AnsiChar): RawUtf8;
var
  l: PtrInt;
begin
  l := length(src);
  if l <> 0 then
  begin
    FastSetString(result, l * 3); // allocate maximum size
    l := EscapeHexBuffer(pointer(src), pointer(result), l,
      toescape, escape) - pointer(result);
  end;
  FakeSetLength(result, l); // return in-place with no realloc
end;

function UnescapeHexBuffer(src, dest: PUtf8Char; escape: AnsiChar): PUtf8Char;
var
  c: AnsiChar;
begin
  result := dest;
  if src <> nil then
    while src^ <> #0 do
    begin
      if src^ = escape then
      begin
        inc(src);
        if src^ in [#10, #13] then // \CRLF or \LF
        begin
          repeat
            inc(src);
          until not (src^ in [#10, #13]);
          continue;
        end
        else if HexToChar(PAnsiChar(src), @c) then // \xx
        begin
          result^ := c;
          inc(src, 2);
          inc(result);
          continue;
        end;
        if src^ = #0 then // expect valid \c
          break;
      end;
      result^ := src^;
      inc(src);
      inc(result);
    end;
end;

function UnescapeHex(const src: RawUtf8; escape: AnsiChar): RawUtf8;
begin
  if PosExChar(escape, src) = 0 then
    result := src // no unescape needed
  else
  begin
    FastSetString(result, length(src)); // allocate maximum size
    FakeSetLength(result, UnescapeHexBuffer(
      pointer(src), pointer(result), escape) - pointer(result));
  end;
end;

function EscapeCharBuffer(src, dest: PUtf8Char; srclen: integer;
  const toescape: TSynAnsicharSet; escape: AnsiChar): PUtf8Char;
begin
  result := dest;
  if srclen > 0 then
    repeat
      if src^ in toescape then
      begin
        result^ := escape;
        inc(result);
      end;
      result^ := src^;
      inc(result);
      inc(src);
      dec(srclen);
    until srclen = 0;
end;

function EscapeChar(const src: RawUtf8;
  const toescape: TSynAnsicharSet; escape: AnsiChar): RawUtf8;
var
  l: PtrInt;
begin
  l := length(src);
  if l <> 0 then
  begin
    FastSetString(result, l * 2); // allocate maximum size
    l := EscapeCharBuffer(pointer(src), pointer(result), l,
      toescape, escape) - pointer(result);
  end;
  FakeSetLength(result, l); // return in-place with no realloc
end;


{ ************ Numbers (integers or floats) to Text Conversion }

procedure Int32ToUtf8(Value: PtrInt; var result: RawUtf8);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  if PtrUInt(Value) <= high(SmallUInt32Utf8) then
    result := SmallUInt32Utf8[Value]
  else
  begin
    P := StrInt32(@tmp[23], Value);
    FastSetString(result, P, @tmp[23] - P);
  end;
end;

function Int32ToUtf8(Value: PtrInt): RawUtf8;
begin
  Int32ToUtf8(Value, result);
end;

procedure Int64ToUtf8(Value: Int64; var result: RawUtf8);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  {$ifdef CPU64}
  if PtrUInt(Value) <= high(SmallUInt32Utf8) then
  {$else} // Int64Rec gives compiler internal error C4963
  if (PCardinalArray(@Value)^[0] <= high(SmallUInt32Utf8)) and
     (PCardinalArray(@Value)^[1] = 0) then
  {$endif CPU64}
    result := SmallUInt32Utf8[Value]
  else
  begin
    {$ifdef CPU64}
    P := StrInt32(@tmp[23], Value);
    {$else}
    P := StrInt64(@tmp[23], Value);
    {$endif CPU64}
    FastSetString(result, P, @tmp[23] - P);
  end;
end;

procedure UInt64ToUtf8(Value: QWord; var result: RawUtf8);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  {$ifdef CPU64}
  if Value <= high(SmallUInt32Utf8) then
  {$else} // Int64Rec gives compiler internal error C4963
  if (PCardinalArray(@Value)^[0] <= high(SmallUInt32Utf8)) and
     (PCardinalArray(@Value)^[1] = 0) then
  {$endif CPU64}
    result := SmallUInt32Utf8[Value]
  else
  begin
    {$ifdef CPU64}
    P := StrUInt32(@tmp[23], Value);
    {$else}
    P := StrUInt64(@tmp[23], Value);
    {$endif CPU64}
    FastSetString(result, P, @tmp[23] - P);
  end;
end;

function Int64ToUtf8(Value: Int64): RawUtf8; // faster than SysUtils.IntToStr
begin
  Int64ToUtf8(Value, result);
end;

{$ifdef CPU32} // already implemented by ToUtf8(Value: PtrInt) below for CPU64
function ToUtf8(Value: Int64): RawUtf8;
begin
  Int64ToUtf8(Value, result);
end;
{$endif CPU32}

function ToUtf8(Value: PtrInt): RawUtf8;
begin
  Int32ToUtf8(Value, result);
end;

procedure UInt32ToUtf8(Value: PtrUInt; var result: RawUtf8);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  if Value <= high(SmallUInt32Utf8) then
    result := SmallUInt32Utf8[Value]
  else
  begin
    P := StrUInt32(@tmp[23], Value);
    FastSetString(result, P, @tmp[23] - P);
  end;
end;

function UInt32ToUtf8(Value: PtrUInt): RawUtf8;
begin
  UInt32ToUtf8(Value, result);
end;

function StrCurr64(P: PAnsiChar; const Value: Int64): PAnsiChar;
var
  c: QWord;
  d: cardinal;
begin
  if Value = 0 then
  begin
    result := P - 1;
    result^ := '0';
    exit;
  end;
  if Value < 0 then
    c := -Value
  else
    c := Value;
  if c < 10000 then
  begin
    result := P - 6; // only decimals -> append '0.xxxx'
    PCardinal(result)^ := ord('0') + ord('.') shl 8;
    YearToPChar(c, PUtf8Char(P) - 4);
  end
  else
  begin
    result := StrUInt64(P - 1, c);
    d := PCardinal(P - 5)^; // in two explit steps for CPUARM (alf)
    PCardinal(P - 4)^ := d;
    P[-5] := '.'; // insert '.' just before last 4 decimals
  end;
  if Value < 0 then
  begin
    dec(result);
    result^ := '-';
  end;
end;

procedure Curr64ToStr(const Value: Int64; var result: RawUtf8);
var
  tmp: array[0..31] of AnsiChar;
  P: PAnsiChar;
  Decim, L: cardinal;
begin
  if Value = 0 then
    result := SmallUInt32Utf8[0]
  else
  begin
    P := StrCurr64(@tmp[31], Value);
    L := @tmp[31] - P;
    if L > 4 then
    begin
      Decim := PCardinal(P + L - SizeOf(cardinal))^; // 4 last digits = 4 decimals
      if Decim = ord('0') + ord('0') shl 8 + ord('0') shl 16 + ord('0') shl 24 then
        dec(L, 5)
      else // no decimal
      if Decim and $ffff0000 = ord('0') shl 16 + ord('0') shl 24 then
        dec(L, 2); // 2 decimals
    end;
    FastSetString(result, P, L);
  end;
end;

function Curr64ToStr(const Value: Int64): RawUtf8;
begin
  Curr64ToStr(Value, result);
end;

function CurrencyToStr(const Value: currency): RawUtf8;
begin
  result := Curr64ToStr(PInt64(@Value)^);
end;

function Curr64ToPChar(const Value: Int64; Dest: PUtf8Char): PtrInt;
var
  tmp: array[0..31] of AnsiChar;
  P: PAnsiChar;
  Decim: cardinal;
begin
  P := StrCurr64(@tmp[31], Value);
  result := @tmp[31] - P;
  if result > 4 then
  begin
    // Decim = 4 last digits = 4 decimals
    Decim := PCardinal(P + result - SizeOf(cardinal))^;
    if Decim = ord('0') + ord('0') shl 8 + ord('0') shl 16 + ord('0') shl 24 then
      // no decimal -> trunc trailing *.0000 chars
      dec(result, 5)
    else if Decim and $ffff0000 = ord('0') shl 16 + ord('0') shl 24 then
      // 2 decimals -> trunc trailing *.??00 chars
      dec(result, 2);
  end;
  MoveFast(P^, Dest^, result);
end;

function StrToCurr64(P: PUtf8Char; NoDecimal: PBoolean): Int64;
var
  c: cardinal;
  minus: boolean;
  Dec: cardinal;
begin
  result := 0;
  if P = nil then
    exit;
  while (P^ <= ' ') and
        (P^ <> #0) do
    inc(P);
  if P^ = '-' then
  begin
    minus := true;
    repeat
      inc(P)
    until P^ <> ' ';
  end
  else
  begin
    minus := false;
    if P^ = '+' then
      repeat
        inc(P)
      until P^ <> ' ';
  end;
  if P^ = '.' then
  begin
    // '.5' -> 500
    Dec := 2;
    inc(P);
  end
  else
    Dec := 0;
  c := byte(P^) - 48;
  if c > 9 then
    exit;
  PCardinal(@result)^ := c;
  inc(P);
  repeat
    if P^ <> '.' then
    begin
      c := byte(P^) - 48;
      if c > 9 then
        break;
      {$ifdef CPU32DELPHI}
      result := result shl 3 + result + result;
      {$else}
      result := result * 10;
      {$endif CPU32DELPHI}
      inc(result, c);
      inc(P);
      if Dec <> 0 then
      begin
        inc(Dec);
        if Dec < 5 then
          continue
        else
          break;
      end;
    end
    else
    begin
      inc(Dec);
      inc(P);
    end;
  until false;
  if NoDecimal <> nil then
    if Dec = 0 then
    begin
      NoDecimal^ := true;
      if minus then
        result := -result;
      exit;
    end
    else
      NoDecimal^ := false;
  if Dec <> 5 then
    // Dec=5 most of the time
    case Dec of
      0, 1:
        result := result * 10000;
      {$ifdef CPU32DELPHI}
      2:
        result := result shl 10 - result shl 4 - result shl 3;
      3:
        result := result shl 6 + result shl 5 + result shl 2;
      4:
        result := result shl 3 + result + result;
      {$else}
      2:
        result := result * 1000;
      3:
        result := result * 100;
      4:
        result := result * 10;
      {$endif CPU32DELPHI}
    end;
  if minus then
    result := -result;
end;

function StrToCurrency(P: PUtf8Char): currency;
begin
  PInt64(@result)^ := StrToCurr64(P, nil);
end;

{$ifdef UNICODE}

function IntToString(Value: integer): string;
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  P := StrInt32(@tmp[23], Value);
  Ansi7ToString(PWinAnsiChar(P), @tmp[23] - P, result);
end;

function IntToString(Value: cardinal): string;
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  P := StrUInt32(@tmp[23], Value);
  Ansi7ToString(PWinAnsiChar(P), @tmp[23] - P, result);
end;

function IntToString(Value: Int64): string;
var
  tmp: array[0..31] of AnsiChar;
  P: PAnsiChar;
begin
  P := StrInt64(@tmp[31], Value);
  Ansi7ToString(PWinAnsiChar(P), @tmp[31] - P, result);
end;

function DoubleToString(Value: Double): string;
var
  tmp: ShortString;
begin
  if Value = 0 then
    result := '0'
  else
    Ansi7ToString(PWinAnsiChar(@tmp[1]), DoubleToShort(@tmp, Value), result);
end;

function Curr64ToString(Value: Int64): string;
var
  tmp: array[0..31] of AnsiChar;
begin
  Ansi7ToString(tmp, Curr64ToPChar(Value, tmp), result);
end;

{$else UNICODE}

function IntToString(Value: integer): string;
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  if cardinal(Value) <= high(SmallUInt32Utf8) then
    result := SmallUInt32Utf8[Value]
  else
  begin
    P := StrInt32(@tmp[23], Value);
    SetString(result, P, @tmp[23] - P);
  end;
end;

function IntToString(Value: cardinal): string;
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  if Value <= high(SmallUInt32Utf8) then
    result := SmallUInt32Utf8[Value]
  else
  begin
    P := StrUInt32(@tmp[23], Value);
    SetString(result, P, @tmp[23] - P);
  end;
end;

function IntToString(Value: Int64): string;
var
  tmp: array[0..31] of AnsiChar;
  P: PAnsiChar;
begin
  if (Value >= 0) and
     (Value <= high(SmallUInt32Utf8)) then
    result := SmallUInt32Utf8[Value]
  else
  begin
    P := StrInt64(@tmp[31], Value);
    SetString(result, P, @tmp[31] - P);
  end;
end;

function DoubleToString(Value: Double): string;
var
  tmp: ShortString;
begin
  if Value = 0 then
    result := '0'
  else
    SetString(result, PAnsiChar(@tmp[1]), DoubleToShort(@tmp, Value));
end;

function Curr64ToString(Value: Int64): string;
begin
  result := Curr64ToStr(Value);
end;

{$endif UNICODE}

{$ifndef EXTENDEDTOSHORT_USESTR}
var // standard FormatSettings (US)
  SettingsUS: TFormatSettings;
{$endif EXTENDEDTOSHORT_USESTR}

// used ExtendedToShortNoExp / DoubleToShortNoExp from str/DoubleToAscii output
function FloatStringNoExp(S: PAnsiChar; Precision: PtrInt): PtrInt;
var
  i, prec: PtrInt;
  c: AnsiChar;
begin
  result := ord(S[0]);
  prec := result; // if no decimal
  if S[1] = '-' then
    dec(prec);
  // test if scientific format -> return as this
  for i := 2 to result do
  begin
    c := S[i];
    if c = 'E' then // should not appear
      exit
    else if c = '.' then
      if i >= Precision then
      begin
        // return huge decimal number as is
        result := i - 1;
        exit;
      end
      else
        dec(prec);
  end;
  if (prec >= Precision) and
     (prec <> result) then
  begin
    dec(result, prec - Precision);
    if S[result + 1] > '5' then
    begin
      // manual rounding
      prec := result;
      repeat
        c := S[prec];
        if c <> '.' then
          if c = '9' then
          begin
            S[prec] := '0';
            if ((prec = 2) and
                (S[1] = '-')) or
               (prec = 1) then
            begin
              i := result;
              inc(S, prec);
              repeat
                // inlined MoveFast(S[prec],S[prec+1],result);
                S[i] := S[i - 1];
                dec(i);
              until i = 0;
              S^ := '1';
              dec(S, prec);
              break;
            end;
          end
          else if (c >= '0') and
                  (c <= '8') then
          begin
            inc(S[prec]);
            break;
          end
          else
            break;
        dec(prec);
      until prec = 0;
    end; // note: this fixes http://stackoverflow.com/questions/2335162
  end;
  if S[result] = '0' then
    repeat
      // trunc any trailing 0
      dec(result);
      c := S[result];
      if c <> '.' then
        if c <> '0' then
          break
        else
          continue
      else
      begin
        dec(result);
        if (result = 2) and
           (S[1] = '-') and
           (S[2] = '0') then
        begin
          result := 1;
          S[1] := '0'; // '-0.000' -> '0'
        end;
        break; // if decimal are all '0' -> return only integer part
      end;
    until false;
end;

function ExtendedToShortNoExp(S: PShortString; Value: TSynExtended;
  Precision: integer): integer;
begin
  {$ifdef DOUBLETOSHORT_USEGRISU}
  if Precision = DOUBLE_PRECISION then
    DoubleToAscii(0, DOUBLE_PRECISION, Value, pointer(S))
  else
  {$endif DOUBLETOSHORT_USEGRISU}
    str(Value: 0: Precision, S^); // not str(Value:0,S) -> '  0.0E+0000'
  result := FloatStringNoExp(pointer(S), Precision);
  S^[0] := AnsiChar(result);
end;

const // range when to switch into scientific notation - minimal 6 digits
  SINGLE_HI = 1E3;
  SINGLE_LO = 1E-3;
  DOUBLE_HI = 1E9;
  DOUBLE_LO = 1E-9;
  {$ifdef TSYNEXTENDED80}
  EXT_HI = 1E12;
  EXT_LO = 1E-12;
  {$endif TSYNEXTENDED80}

{$ifdef EXTENDEDTOSHORT_USESTR}
function ExtendedToShort(S: PShortString; Value: TSynExtended; Precision: integer): integer;
var
  scientificneeded: boolean;
  valueabs: TSynExtended;
begin
  {$ifdef DOUBLETOSHORT_USEGRISU}
  if Precision = DOUBLE_PRECISION then
  begin
    result := DoubleToShort(S, Value);
    exit;
  end;
  {$endif DOUBLETOSHORT_USEGRISU}
  if Value = 0 then
  begin
    PCardinal(S)^ := 1 + ord('0') shl 8;
    result := 1;
    exit;
  end;
  scientificneeded := false;
  valueabs := abs(Value);
  if Precision <= SINGLE_PRECISION then
  begin
    if (valueabs > SINGLE_HI) or
       (valueabs < SINGLE_LO) then
      scientificneeded := true;
  end
  else
  {$ifdef TSYNEXTENDED80}
  if Precision > DOUBLE_PRECISION then
  begin
    if (valueabs > EXT_HI) or
       (valueabs < EXT_LO) then
      scientificneeded := true;
  end
  else
  {$endif TSYNEXTENDED80}
  if (valueabs > DOUBLE_HI) or
     (valueabs < DOUBLE_LO) then
    scientificneeded := true;
  if scientificneeded then
  begin
    str(Value, S^);
    if S^[1] = ' ' then
    begin
      dec(S^[0]);
      MoveFast(S^[2], S^[1], ord(S^[0]));
    end;
    result := ord(S^[0]);
  end
  else
  begin
    str(Value: 0:Precision, S^); // not str(Value:0,S) -> '  0.0E+0000'
    result := FloatStringNoExp(pointer(S), Precision);
    S^[0] := AnsiChar(result);
  end;
end;

{$else not EXTENDEDTOSHORT_USESTR}

function ExtendedToShort(S: PShortString; Value: TSynExtended; Precision: integer): integer;
{$ifdef UNICODE}
var
  i: PtrInt;
{$endif UNICODE}
begin
  // use ffGeneral: see https://synopse.info/forum/viewtopic.php?pid=442#p442
  result := FloatToText(PChar(@S^[1]), Value, fvExtended, ffGeneral, Precision, 0, SettingsUS);
  {$ifdef UNICODE} // FloatToText(PWideChar) is faster than FloatToText(PAnsiChar)
  for i := 1 to result do
    PByteArray(S)[i] := PWordArray(PtrInt(S) - 1)[i];
  {$endif UNICODE}
  S^[0] := AnsiChar(result);
end;

{$endif EXTENDEDTOSHORT_USESTR}

function FloatToShortNan(const s: ShortString): TFloatNan;
begin
  case PInteger(@s)^ and $ffdfdfdf of
    3 + ord('N') shl 8 + ord('A') shl 16 + ord('N') shl 24:
      result := fnNan;
    3 + ord('I') shl 8 + ord('N') shl 16 + ord('F') shl 24,
    4 + ord('+') shl 8 + ord('I') shl 16 + ord('N') shl 24:
      result := fnInf;
    4 + ord('-') shl 8 + ord('I') shl 16 + ord('N') shl 24:
      result := fnNegInf;
  else
    result := fnNumber;
  end;
end;

function FloatToStrNan(const s: RawUtf8): TFloatNan;
begin
  case length(s) of
    3:
      case PInteger(s)^ and $dfdfdf of
        ord('N') + ord('A') shl 8 + ord('N') shl 16:
          result := fnNan;
        ord('I') + ord('N') shl 8 + ord('F') shl 16:
          result := fnInf;
      else
        result := fnNumber;
      end;
    4:
      case PInteger(s)^ and $dfdfdfdf of
        ord('+') + ord('I') shl 8 + ord('N') shl 16 + ord('F') shl 24:
          result := fnInf;
        ord('-') + ord('I') shl 8 + ord('N') shl 16 + ord('F') shl 24:
          result := fnNegInf;
      else
        result := fnNumber;
      end;
  else
    result := fnNumber;
  end;
end;

function ExtendedToStr(Value: TSynExtended; Precision: integer): RawUtf8;
begin
  ExtendedToStr(Value, Precision, result);
end;

procedure ExtendedToStr(Value: TSynExtended; Precision: integer; var result: RawUtf8);
var
  tmp: ShortString;
begin
  if Value = 0 then
    result := SmallUInt32Utf8[0]
  else
    FastSetString(result, @tmp[1], ExtendedToShort(@tmp, Value, Precision));
end;

function FloatToJsonNan(s: PShortString): PShortString;
begin
  case PInteger(s)^ and $ffdfdfdf of
    3 + ord('N') shl 8 + ord('A') shl 16 + ord('N') shl 24:
      result := @JSON_NAN[fnNan];
    3 + ord('I') shl 8 + ord('N') shl 16 + ord('F') shl 24,
    4 + ord('+') shl 8 + ord('I') shl 16 + ord('N') shl 24:
      result := @JSON_NAN[fnInf];
    4 + ord('-') shl 8 + ord('I') shl 16 + ord('N') shl 24:
      result := @JSON_NAN[fnNegInf];
  else
    result := s;
  end;
end;

function ExtendedToJson(tmp: PShortString; Value: TSynExtended;
  Precision: integer; NoExp: boolean): PShortString;
begin
  if Value = 0 then
    result := @JSON_NAN[fnNumber]
  else
  begin
    if NoExp then
      ExtendedToShortNoExp(tmp, Value, Precision)
    else
      ExtendedToShort(tmp, Value, Precision);
    result := FloatToJsonNan(tmp);
  end;
end;

{$ifdef DOUBLETOSHORT_USEGRISU}

{
    Implement 64-bit floating point (double) to ASCII conversion using the
    GRISU-1 efficient algorithm.

    Original Code in flt_core.inc flt_conv.inc flt_pack.inc from FPC RTL.
    Copyright (C) 2013 by Max Nazhalov
    Licenced with LGPL 2 with the linking exception.
    If you don't agree with these License terms, disable this feature
    by undefining DOUBLETOSHORT_USEGRISU in Synopse.inc

    GRISU Original Algorithm
    Copyright (c) 2009 Florian Loitsch

    We extracted a double-to-ascii only cut-down version of those files,
    and made a huge refactoring to reach the best performance, especially
    tuning the Intel target with some dedicated asm and code rewrite.

  With Delphi 10.3 on Win32:
   100000 FloatToText    in 38.11ms i.e. 2,623,570/s, aver. 0us, 47.5 MB/s
   100000 str            in 43.19ms i.e. 2,315,082/s, aver. 0us, 50.7 MB/s
   100000 DoubleToShort  in 45.50ms i.e. 2,197,367/s, aver. 0us, 43.8 MB/s
   100000 DoubleToAscii  in 42.44ms i.e. 2,356,045/s, aver. 0us, 47.8 MB/s

  With Delphi 10.3 on Win64:
   100000 FloatToText    in 61.83ms i.e. 1,617,233/s, aver. 0us, 29.3 MB/s
   100000 str            in 53.20ms i.e. 1,879,663/s, aver. 0us, 41.2 MB/s
   100000 DoubleToShort  in 18.45ms i.e. 5,417,998/s, aver. 0us, 108 MB/s
   100000 DoubleToAscii  in 18.19ms i.e. 5,496,921/s, aver. 0us, 111.5 MB/s

  With FPC on Win32:
   100000 FloatToText    in 115.62ms i.e.  864,842/s, aver. 1us, 15.6 MB/s
   100000 str            in 57.30ms i.e. 1,745,109/s, aver. 0us, 39.9 MB/s
   100000 DoubleToShort  in 23.88ms i.e. 4,187,078/s, aver. 0us, 83.5 MB/s
   100000 DoubleToAscii  in 23.34ms i.e. 4,284,490/s, aver. 0us, 86.9 MB/s

  With FPC on Win64:
   100000 FloatToText    in 76.92ms i.e. 1,300,052/s, aver. 0us, 23.5 MB/s
   100000 str            in 27.70ms i.e. 3,609,456/s, aver. 0us, 82.6 MB/s
   100000 DoubleToShort  in 14.73ms i.e. 6,787,944/s, aver. 0us, 135.4 MB/s
   100000 DoubleToAscii  in 13.78ms i.e. 7,253,735/s, aver. 0us, 147.2 MB/s

  With FPC on Linux x86_64:
   100000 FloatToText    in 81.48ms i.e. 1,227,249/s, aver. 0us, 22.2 MB/s
   100000 str            in 36.98ms i.e. 2,703,871/s, aver. 0us, 61.8 MB/s
   100000 DoubleToShort  in 13.11ms i.e. 7,626,601/s, aver. 0us, 152.1 MB/s
   100000 DoubleToAscii  in 12.59ms i.e. 7,942,180/s, aver. 0us, 161.2 MB/s

  - Our rewrite is twice faster than original flt_conv.inc from FPC RTL (str)
  - Delphi Win32 has trouble making 64-bit computation - no benefit since it
    has good optimized i87 asm (but slower than our code with FPC/Win32)
  - FPC is more efficient when compiling integer arithmetic; we avoided slow
    division by calling our Div100(), but Delphi Win64 is still far behind
  - Delphi Win64 has very slow FloatToText and str()

}

// Controls printing of NaN-sign.
// Undefine to print NaN sign during float->ASCII conversion.
// IEEE does not interpret the sign of a NaN, so leave it defined.
{$define GRISU1_F2A_NAN_SIGNLESS}

// Controls rounding of generated digits when formatting with narrowed
// width (either fixed or exponential notation).
// Traditionally, FPC and BP7/Delphi use "roundTiesToAway" mode.
// Undefine to use "roundTiesToEven" approach.
{$define GRISU1_F2A_HALF_ROUNDUP}

// This one is a hack against Grusu sub-optimality.
// It may be used only strictly together with GRISU1_F2A_HALF_ROUNDUP.
// It does not violate most general rules due to the fact that it is
// applicable only when formatting with narrowed width, where the fine
// view is more desirable, and the precision is already lost, so it can
// be used in general-purpose applications.
// Refer to its implementation.
{$define GRISU1_F2A_AGRESSIVE_ROUNDUP} // Defining this fixes several tests.

// Undefine to enable SNaN support.
// Note: IEEE [754-2008, page 31] requires (1) to recognize "SNaN" during
// ASCII->float, and (2) to generate the "invalid FP operation" exception
// either when SNaN is printed as "NaN", or "SNaN" is evaluated to QNaN,
// so it would be preferable to undefine these settings,
// but the FPC RTL is not ready for this right now..
{$define GRISU1_F2A_NO_SNAN}

/// If Value=0 would just store '0', whatever frac_digits is supplied.
{$define GRISU1_F2A_ZERONOFRACT}

var
  /// fast lookup table for converting any decimal number from
  // 0 to 99 into their byte digits (00..99) equivalence
  // - used e.g. by DoubleToAscii() implementing Grisu algorithm
  TwoDigitByteLookupW: packed array[0..99] of word;

const
  // TFloatFormatProfile for double
  nDig_mantissa = 17;
  nDig_exp10 = 3;

type
  // "Do-It-Yourself Floating-Point" structures
  TDIY_FP = record
    f: qword;
    e: integer;
  end;

  TDIY_FP_Power_of_10 = record
    c: TDIY_FP;
    e10: integer;
  end;
  PDIY_FP_Power_of_10 = ^TDIY_FP_Power_of_10;

const
  ROUNDER = $80000000;

{$ifdef CPUINTEL} // our faster version using 128-bit x86_64 multiplication

procedure d2a_diy_fp_multiply(var x, y: TDIY_FP; normalize: boolean;
  out result: TDIY_FP); {$ifdef HASINLINE}inline;{$endif}
var
  p: THash128Rec;
begin
  mul64x64(x.f, y.f, p); // fast x86_64 / i386 asm
  if (p.c1 and ROUNDER) <>  0 then
    inc(p.h);
  result.f := p.h;
  result.e := PtrInt(x.e) + PtrInt(y.e) + 64;
  if normalize then
    if (PQWordRec(@result.f)^.h and ROUNDER) = 0 then
    begin
      result.f := result.f * 2;
      dec(result.e);
    end;
end;

{$else} // regular Grisu method - optimized for 32-bit CPUs

procedure d2a_diy_fp_multiply(var x, y: TDIY_FP; normalize: boolean; out result: TDIY_FP);
var
  _x: TQWordRec absolute x;
  _y: TQWordRec absolute y;
  r: TQWordRec absolute result;
  ac, bc, ad, bd, t1: TQWordRec;
begin
  ac.v := qword(_x.h) * _y.h;
  bc.v := qword(_x.l) * _y.h;
  ad.v := qword(_x.h) * _y.l;
  bd.v := qword(_x.l) * _y.l;
  t1.v := qword(ROUNDER) + bd.h + bc.l + ad.l;
  result.f := ac.v + ad.h + bc.h + t1.h;
  result.e := x.e + y.e + 64;
  if normalize then
    if (r.h and ROUNDER) = 0 then
    begin
      inc(result.f, result.f);
      dec(result.e);
    end;
end;

{$endif CPUINTEL}

const
  // alpha =-61; gamma = 0
  // full cache: 1E-450 .. 1E+432, step = 1E+18
  // sparse = 1/10
  C_PWR10_DELTA = 18;
  C_PWR10_COUNT = 50;

type
  TDIY_FP_Cached_Power10 = record
    base:         array [ 0 .. 9 ] of TDIY_FP_Power_of_10;
    factor_plus:  array [ 0 .. 1 ] of TDIY_FP_Power_of_10;
    factor_minus: array [ 0 .. 1 ] of TDIY_FP_Power_of_10;
    // extra mantissa correction [ulp; signed]
    corrector:    array [ 0 .. C_PWR10_COUNT - 1 ] of shortint;
  end;

const
  CACHED_POWER10: TDIY_FP_Cached_Power10 = (
    base: (
        ( c: ( f: qword($825ECC24C8737830); e: -362 ); e10:  -90 ),
        ( c: ( f: qword($E2280B6C20DD5232); e: -303 ); e10:  -72 ),
        ( c: ( f: qword($C428D05AA4751E4D); e: -243 ); e10:  -54 ),
        ( c: ( f: qword($AA242499697392D3); e: -183 ); e10:  -36 ),
        ( c: ( f: qword($9392EE8E921D5D07); e: -123 ); e10:  -18 ),
        ( c: ( f: qword($8000000000000000); e:  -63 ); e10:    0 ),
        ( c: ( f: qword($DE0B6B3A76400000); e:   -4 ); e10:   18 ),
        ( c: ( f: qword($C097CE7BC90715B3); e:   56 ); e10:   36 ),
        ( c: ( f: qword($A70C3C40A64E6C52); e:  116 ); e10:   54 ),
        ( c: ( f: qword($90E40FBEEA1D3A4B); e:  176 ); e10:   72 )
    );
    factor_plus: (
        ( c: ( f: qword($F6C69A72A3989F5C); e:   534 ); e10:  180 ),
        ( c: ( f: qword($EDE24AE798EC8284); e:  1132 ); e10:  360 )
    );
    factor_minus: (
        ( c: ( f: qword($84C8D4DFD2C63F3B); e:  -661 ); e10: -180 ),
        ( c: ( f: qword($89BF722840327F82); e: -1259 ); e10: -360 )
    );
    corrector: (
        0,  0,  0,  0,  1,  0,  0,  0,  1, -1,
        0,  1,  1,  1, -1,  0,  0,  1,  0, -1,
        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
       -1,  0,  0, -1,  0,  0,  0,  0,  0, -1,
        0,  0,  0,  0,  1,  0,  0,  0, -1,  0
    ));
  CACHED_POWER10_MIN10 = -90 -360;
  // = ref.base[low(ref.base)].e10 + ref.factor_minus[high(ref.factor_minus)].e10

// return normalized correctly rounded approximation of the power of 10
// scaling factor, intended to shift a binary exponent of the original number
// into selected [ alpha .. gamma ] range
procedure d2a_diy_fp_cached_power10(exp10: integer; out factor: TDIY_FP_Power_of_10);
var
  i, xmul: integer;
  A, B: PDIY_FP_Power_of_10;
  cx: PtrInt;
  ref: ^TDIY_FP_Cached_Power10;
begin
  ref := @CACHED_POWER10; // much better code generation on PIC/x86_64
  // find non-sparse index
  if exp10 <= CACHED_POWER10_MIN10 then
    i := 0
  else
  begin
    i := (exp10 - CACHED_POWER10_MIN10) div C_PWR10_DELTA;
    if i * C_PWR10_DELTA + CACHED_POWER10_MIN10 <> exp10 then
      inc(i); // round-up
    if i > C_PWR10_COUNT - 1 then
      i := C_PWR10_COUNT - 1;
  end;
  // generate result
  xmul := i div length(ref.base);
  A := @ref.base[i - (xmul * length(ref.base))]; // fast mod
  dec(xmul, length(ref.factor_minus));
  if xmul = 0 then
  begin
    // base
    factor := A^;
    exit;
  end;
  // surrogate
  if xmul > 0 then
  begin
    dec(xmul);
    B := @ref.factor_plus[xmul];
  end
  else
  begin
    xmul := -(xmul + 1);
    B := @ref.factor_minus[xmul];
  end;
  factor.e10 := A.e10 + B.e10;
  if A.e10 <> 0 then
  begin
    d2a_diy_fp_multiply(A.c, B.c, true, factor.c);
    // adjust mantissa
    cx := ref.corrector[i];
    if cx <> 0 then
      inc(int64(factor.c.f), int64(cx));
  end
  else
    // exact
    factor.c := B^.c;
end;

procedure d2a_unpack_float(const f: double; out minus: boolean;
  out result: TDIY_FP);   {$ifdef HASINLINE}inline;{$endif}
type
  TSplitFloat = packed record
    case byte of
      0: (f: double);
      1: (b: array[0..7] of byte);
      2: (w: array[0..3] of word);
      3: (d: array[0..1] of cardinal);
      4: (l: qword);
  end;
var
  doublebits: TSplitFloat;
begin
{$ifdef FPC_DOUBLE_HILO_SWAPPED}
  // high and low cardinal are swapped when using the arm fpa
  doublebits.d[0] := TSplitFloat(f).d[1];
  doublebits.d[1] := TSplitFloat(f).d[0];
{$else not FPC_DOUBLE_HILO_SWAPPED}
  doublebits.f := f;
{$endif FPC_DOUBLE_HILO_SWAPPED}
{$ifdef endian_big}
  minus := (doublebits.b[0] and $80 <> 0);
  result.e := (doublebits.w[0] shr 4) and $7FF;
{$else endian_little}
  minus := (doublebits.b[7] and $80 <> 0);
  result.e := (doublebits.w[3] shr 4) and $7FF;
{$endif endian}
  result.f := doublebits.l and $000FFFFFFFFFFFFF;
end;

const
  C_FRAC2_BITS = 52;
  C_EXP2_BIAS = 1023;
  C_DIY_FP_Q = 64;
  C_GRISU_ALPHA = -61;
  C_GRISU_GAMMA = 0;

  C_EXP2_SPECIAL = C_EXP2_BIAS * 2 + 1;
  C_MANT2_INTEGER = qword(1) shl C_FRAC2_BITS;

type
  TAsciiDigits = array[0..39] of byte;
  PAsciiDigits = ^TAsciiDigits;

// convert unsigned integers into decimal digits

{$ifdef FPC_64} // leverage efficient FPC 64-bit division as mul reciprocal

function d2a_gen_digits_64(buf: PAsciiDigits; x: qword): PtrInt;
var
  tab: PWordArray;
  P: PAnsiChar;
  c100: qword;
begin
  tab := @TwoDigitByteLookupW; // 0..99 value -> two byte digits (00..99)
  P := PAnsiChar(@buf[24]); // append backwards
  repeat
    if x >= 100 then
    begin
      dec(P, 2);
      c100 := x div 100;
      dec(x, c100 * 100);
      PWord(P)^ := tab[x]; // 2 digits per loop
      if c100 = 0 then
        break;
      x := c100;
      continue;
    end;
    if x < 10 then
    begin
      dec(P);
      P^ := AnsiChar(x); // 0..9
      break;
    end;
    dec(P, 2);
    PWord(P)^ := tab[x]; // 10..99
    break;
  until false;
  PHash192(buf)^ := PHash192(P)^; // faster than MoveByOne(P,buf,result)
  result := PAnsiChar(@buf[24]) - P;
end;

{$else not FPC_64} // use three 32-bit groups of digit

function d2a_gen_digits_32(buf: PAsciiDigits; x: dword; pad_9zero: boolean): PtrInt;
const
  digits: array[0..9] of cardinal = (
    0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000);
var
  n: PtrInt;
  m: cardinal;
  {$ifdef FPC}
  z: cardinal;
  {$else}
  d100: TDiv100Rec;
  {$endif FPC}
  tab: PWordArray;
begin
  // Calculate amount of digits
  if x = 0 then
    n := 0  // emit nothing if padding is not required
  else
  begin
    n := integer((BSRdword(x) + 1) * 1233) shr 12;
    if x >= digits[n] then
      inc(n);
  end;
  if pad_9zero and (n < 9) then
    n := 9;
  result := n;
  if n = 0 then
    exit;
  // Emit digits
  dec(PByte(buf));
  tab := @TwoDigitByteLookupW;
  m := x;
  while (n >= 2) and (m <> 0) do
  begin
    dec(n);
    {$ifdef FPC} // FPC will use fast mul reciprocal
    z := m div 100; // compute two 0..9 digits
    PWord(@buf[n])^ := tab^[m - z * 100];
    m := z;
    {$else}
    Div100(m, d100); // our asm is faster than Delphi div operation
    PWord(@buf[n])^ := tab^[d100.M];
    m := d100.D;
    {$endif FPC}
    dec(n);
  end;
  if n = 0 then
    exit;
  if m <> 0 then
  begin
    if m > 9 then
      m := m mod 10; // compute last 0..9 digit
    buf[n] := m;
    dec(n);
    if n = 0 then
      exit;
  end;
  repeat
    buf[n] := 0; // padding with 0
    dec(n);
  until n = 0;
end;

function d2a_gen_digits_64(buf: PAsciiDigits; const x: qword): PtrInt;
var
  n_digits: PtrInt;
  temp: qword;
  splitl, splitm, splith: cardinal;
begin
  // Split X into 3 unsigned 32-bit integers; lower two should be < 10 digits long
  n_digits := 0;
  if x < 1000000000 then
    splitl := x
  else
  begin
    temp := x div 1000000000;
    splitl := x - temp * 1000000000;
    if temp < 1000000000 then
      splitm := temp
    else
    begin
      splith := temp div 1000000000;
      splitm := cardinal(temp) - splith * 1000000000;
      n_digits := d2a_gen_digits_32(buf, splith, false); // Generate hi digits
    end;
    inc(n_digits, d2a_gen_digits_32(@buf[n_digits], splitm, n_digits <> 0));
  end;
  // Generate digits
  inc(n_digits, d2a_gen_digits_32(@buf[n_digits], splitl, n_digits <> 0));
  result := n_digits;
end;

{$endif FPC_64}

// Performs digit sequence rounding, returns decimal point correction
function d2a_round_digits(var buf: TAsciiDigits; var n_current: integer;
  n_max: PtrInt; half_round_to_even: boolean = true): PtrInt;
var
  n: PtrInt;
  dig_round, dig_sticky: byte;
  {$ifdef GRISU1_F2A_AGRESSIVE_ROUNDUP}
  i: PtrInt;
  {$endif GRISU1_F2A_AGRESSIVE_ROUNDUP}
begin
  result := 0;
  n := n_current;
  n_current := n_max;
  // Get round digit
  dig_round := buf[n_max];
{$ifdef GRISU1_F2A_AGRESSIVE_ROUNDUP}
  // Detect if rounding-up the second last digit turns the "dig_round"
  // into "5"; also make sure we have at least 1 digit between "dig_round"
  // and the second last.
  if not half_round_to_even then
    if (dig_round = 4) and
       (n_max < n - 3) then
      if buf[n - 2] >= 8 then // somewhat arbitrary...
      begin
        // check for only "9" are in between
        i := n - 2;
        repeat
          dec(i);
        until (i = n_max) or
              (buf[i] <> 9);
        if i = n_max then
          // force round-up
          dig_round := 9; // any value ">=5"
      end;
{$endif GRISU1_F2A_AGRESSIVE_ROUNDUP}
  if dig_round < 5 then
    exit;
  // Handle "round half to even" case
  if (dig_round = 5) and
     half_round_to_even and
     ((n_max = 0) or
      (buf[n_max - 1] and 1 = 0)) then
  begin
    // even and a half: check if exactly the half
    dig_sticky := 0;
    while (n > n_max + 1) and (dig_sticky = 0) do
    begin
      dec(n);
      dig_sticky := buf[n];
    end;
    if dig_sticky = 0 then
      exit; // exactly a half -> no rounding is required
  end;
  // Round-up
  while n_max > 0 do
  begin
    dec(n_max);
    inc(buf[n_max]);
    if buf[n_max] < 10 then
    begin
      // no more overflow: stop now
      n_current := n_max + 1;
      exit;
    end;
    // continue rounding
  end;
  // Overflow out of the 1st digit, all n_max digits became 0
  buf[0] := 1;
  n_current := 1;
  result := 1;
end;

// format the number in the fixed-point representation
procedure d2a_return_fixed(str: PAnsiChar; minus: boolean;
  var digits: TAsciiDigits; n_digits_have, fixed_dot_pos, frac_digits: integer);
var
  p: PAnsiChar;
  d: PByte;
  cut_digits_at, n_before_dot, n_before_dot_pad0, n_after_dot_pad0,
  n_after_dot, n_tail_pad0: integer;
begin
  // Round digits if necessary
  cut_digits_at := fixed_dot_pos + frac_digits;
  if cut_digits_at < 0 then
    // zero
    n_digits_have := 0
  else if cut_digits_at < n_digits_have then
    // round digits
    inc(fixed_dot_pos, d2a_round_digits(digits, n_digits_have, cut_digits_at
      {$ifdef GRISU1_F2A_HALF_ROUNDUP}, false {$endif} ));
  // Before dot: digits, pad0
  if (fixed_dot_pos <= 0) or
     (n_digits_have = 0) then
  begin
    n_before_dot := 0;
    n_before_dot_pad0 := 1;
  end
  else if fixed_dot_pos > n_digits_have then
  begin
    n_before_dot := n_digits_have;
    n_before_dot_pad0 := fixed_dot_pos - n_digits_have;
  end
  else
  begin
    n_before_dot := fixed_dot_pos;
    n_before_dot_pad0 := 0;
  end;
  // After dot: pad0, digits, pad0
  if fixed_dot_pos < 0 then
    n_after_dot_pad0 := -fixed_dot_pos
  else
    n_after_dot_pad0 := 0;
  if n_after_dot_pad0 > frac_digits then
    n_after_dot_pad0 := frac_digits;
  n_after_dot := n_digits_have - n_before_dot;
  n_tail_pad0 := frac_digits - n_after_dot - n_after_dot_pad0;
  p := str + 1;
  // Sign
  if minus then
  begin
    p^ := '-';
    inc(p);
  end;
  // integer significant digits
  d := @digits;
  if n_before_dot > 0 then
    repeat
      p^ := AnsiChar(d^ + ord('0'));
      inc(p);
      inc(d);
      dec(n_before_dot);
    until n_before_dot = 0;
  // integer 0-padding
  if n_before_dot_pad0 > 0 then
    repeat
      p^ := '0';
      inc(p);
      dec(n_before_dot_pad0);
    until n_before_dot_pad0 = 0;
  // Fractional part
  if frac_digits <> 0 then
  begin
    // Dot
    p^ := '.';
    inc(p);
    // Pre-fraction 0-padding
    if n_after_dot_pad0 > 0 then
      repeat
        p^ := '0';
        inc(p);
        dec(n_after_dot_pad0);
      until n_after_dot_pad0 = 0;
    // Fraction significant digits
    if n_after_dot > 0 then
      repeat
        p^ := AnsiChar(d^ + ord('0'));
        inc(p);
        inc(d);
        dec(n_after_dot);
      until n_after_dot = 0;
    // Tail 0-padding
    if n_tail_pad0 > 0 then
      repeat
        p^ := '0';
        inc(p);
        dec(n_tail_pad0);
      until n_tail_pad0 = 0;
  end;
  // Store length
  str[0] := AnsiChar(p - str - 1);
end;

// formats the number as exponential representation
procedure d2a_return_exponential(str: PAnsiChar; minus: boolean;
  digits: PByte; n_digits_have, n_digits_req, d_exp: PtrInt);
var
  p, exp: PAnsiChar;
begin
  p := str + 1;
  // Sign
  if minus then
  begin
    p^ := '-';
    inc(p);
  end;
  // integer part
  if n_digits_have > 0 then
  begin
    p^ := AnsiChar(digits^ + ord('0'));
    dec(n_digits_have);
  end
  else
    p^ := '0';
  inc(p);
  // Dot
  if n_digits_req > 1 then
  begin
    p^ := '.';
    inc(p);
  end;
  // Fraction significant digits
  if n_digits_req < n_digits_have then
    n_digits_have := n_digits_req;
  if n_digits_have > 0 then
  begin
    repeat
      inc(digits);
      p^ := AnsiChar(digits^ + ord('0'));
      inc(p);
      dec(n_digits_have);
    until n_digits_have = 0;
    while p[-1] = '0' do
      dec(p); // trim #.###00000 -> #.###
    if p[-1] = '.' then
      dec(p); // #.0 -> #
  end;
  // Exponent designator
  p^ := 'E';
  inc(p);
  // Exponent sign (+ is not stored, as in Delphi)
  if d_exp < 0 then
  begin
    p^ := '-';
    d_exp := -d_exp;
    inc(p);
  end;
  // Exponent digits
  exp := pointer(SmallUInt32Utf8[d_exp]); // 0..999 range is fine
  PCardinal(p)^ := PCardinal(exp)^;
  inc(p, PStrLen(exp - _STRLEN)^);
  // Store length
  str[0] := AnsiChar(p - str - 1);
end;

/// set one of special results with proper sign
procedure d2a_return_special(str: PAnsiChar; sign: integer;
  const spec: ShortString);
begin
  // Compute length
  str[0] := spec[0];
  if sign <> 0 then
    inc(str[0]);
  inc(str);
  // Sign
  if sign <> 0 then
  begin
    if sign > 0 then
      str^ := '+'
    else
      str^ := '-';
    inc(str);
  end;
  // Special text (3 chars)
  PCardinal(str)^ := PCardinal(@spec[1])^;
end;


// Calculates the exp10 of a factor required to bring the binary exponent
// of the original number into selected [ alpha .. gamma ] range:
// result := ceiling[ ( alpha - e ) * log10(2) ]
function d2a_k_comp(e, alpha{, gamma}: integer): integer;
var
  dexp: double;
const
  D_LOG10_2: double = 0.301029995663981195213738894724493027; // log10(2)
var
  x, n: integer;
begin
  x := alpha - e;
  dexp := x * D_LOG10_2;
  // ceil( dexp )
  n := trunc(dexp);
  if x > 0 then
    if dexp <> n then
      inc(n); // round-up
  result := n;
end;

procedure DoubleToAscii(min_width, frac_digits: integer; const v: double;
  str: PAnsiChar);
var
  w, D: TDIY_FP;
  c_mk: TDIY_FP_Power_of_10;
  n, mk, dot_pos, n_digits_need, n_digits_have: integer;
  n_digits_req, n_digits_sci: integer;
  minus: boolean;
  fl, one_maskl: qword;
  one_e: integer;
  {$ifdef CPU32}
  one_mask, f: cardinal; // run a 2nd loop with 32-bit range
  {$endif CPU32}
  buf: TAsciiDigits;
begin
  // Limit parameters
  if frac_digits > 216 then
    frac_digits := 216; // Delphi compatible
  if min_width <= C_NO_MIN_WIDTH then
    min_width := -1 // no minimal width
  else if min_width < 0 then
    min_width := 0; // minimal width is as short as possible
  // Format profile: select "n_digits_need" (and "n_digits_exp")
  n_digits_req := nDig_mantissa;
  // number of digits to be calculated by Grisu
  n_digits_need := nDig_mantissa;
  if n_digits_req < n_digits_need then
    n_digits_need := n_digits_req;
  // number of mantissa digits to be printed in exponential notation
  if min_width < 0 then
    n_digits_sci := n_digits_req
  else
  begin
    n_digits_sci := min_width -1 {sign} -1 {dot} -1 {E} -1 {E-sign} - nDig_exp10;
    if n_digits_sci < 2 then
      n_digits_sci := 2; // at least 2 digits
    if n_digits_sci > n_digits_req then
      n_digits_sci := n_digits_req; // at most requested by real_type
  end;
  // Float -> DIY_FP
  d2a_unpack_float(v, minus, w);
  // Handle Zero
  if (w.e = 0) and
     (w.f = 0) then
  begin
    {$ifdef GRISU1_F2A_ZERONOFRACT}
    PCardinal(str)^ := 1 + ord('0') shl 8; // just return '0'
    {$else}
    if frac_digits >= 0 then
      d2a_return_fixed(str, minus, buf, 0, 1, frac_digits)
    else
      d2a_return_exponential(str, minus, @buf, 0, n_digits_sci, 0);
    {$endif GRISU1_F2A_ZERONOFRACT}
    exit;
  end;
  // Handle specials
  if w.e = C_EXP2_SPECIAL then
  begin
    n := 1 - ord(minus) * 2; // default special sign [-1|+1]
    if w.f = 0 then
      d2a_return_special(str, n, C_STR_INF)
    else
    begin
      // NaN [also pseudo-NaN, pseudo-Inf, non-normal for floatx80]
      {$ifdef GRISU1_F2A_NAN_SIGNLESS}
      n := 0;
      {$endif GRISU1_F2A_NAN_SIGNLESS}
      {$ifndef GRISU1_F2A_NO_SNAN}
      if (w.f and (C_MANT2_INTEGER shr 1)) = 0 then
        return_special(str, n, C_STR_SNAN)
      else
      {$endif GRISU1_F2A_NO_SNAN}
        d2a_return_special(str, n, C_STR_QNAN);
    end;
    exit;
  end;
  // Handle denormals
  if w.e <> 0 then
  begin
    // normal
    w.f := w.f or C_MANT2_INTEGER;
    n := C_DIY_FP_Q - C_FRAC2_BITS - 1;
  end
  else
  begin
    // denormal
    n := 63 - BSRqword(w.f);
    inc(w.e);
  end;
  // Final normalization
  w.f := w.f shl n;
  dec(w.e, C_EXP2_BIAS + n + C_FRAC2_BITS);
  // 1. Find the normalized "c_mk = f_c * 2^e_c" such that
  //    "alpha <= e_c + e_w + q <= gamma"
  // 2. Define "V = D * 10^k": multiply the input number by "c_mk", do not
  //    normalize to land into [ alpha .. gamma ]
  // 3. Generate digits ( n_digits_need + "round" )
  if (C_GRISU_ALPHA <= w.e) and
     (w.e <= C_GRISU_GAMMA) then
  begin
    // no scaling required
    D := w;
    c_mk.e10 := 0;
  end
  else
  begin
    mk := d2a_k_comp(w.e, C_GRISU_ALPHA{, C_GRISU_GAMMA} );
    d2a_diy_fp_cached_power10(mk, c_mk);
    // Let "D = f_D * 2^e_D := w (*) c_mk"
    if c_mk.e10 = 0 then
      D := w
    else
      d2a_diy_fp_multiply(w, c_mk.c, false, D);
  end;
  // Generate digits: integer part
  n_digits_have := d2a_gen_digits_64(@buf, D.f shr (-D.e));
  dot_pos := n_digits_have;
  // Generate digits: fractional part
  {$ifdef CPU32}
  f := 0; // "sticky" digit
  {$endif CPU32}
  if D.e < 0 then
    repeat
      // MOD by ONE
      one_e := D.e;
      one_maskl := qword(1) shl (-D.e) - 1;
      fl := D.f and one_maskl;
      // 64-bit loop (very efficient on x86_64, slower on i386)
      while {$ifdef CPU32} (one_e < -29) and {$endif}
            (n_digits_have < n_digits_need + 1) and (fl <> 0) do
      begin
        // f := f * 5;
        inc(fl, fl shl 2);
        // one := one / 2
        one_maskl := one_maskl shr 1;
        inc(one_e);
        // DIV by one
        buf[n_digits_have] := fl shr (-one_e);
        // MOD by one
        fl := fl and one_maskl;
        // next
        inc(n_digits_have);
      end;
      {$ifdef CPU32}
      if n_digits_have >= n_digits_need + 1 then
      begin
        // only "sticky" digit remains
        f := ord(fl <> 0);
        break;
      end;
      one_mask := cardinal(one_maskl);
      f := cardinal(fl);
      // 32-bit loop
      while (n_digits_have < n_digits_need + 1) and (f <> 0) do
      begin
        // f := f * 5;
        inc(f, f shl 2);
        // one := one / 2
        one_mask := one_mask shr 1;
        inc(one_e);
        // DIV by one
        buf[n_digits_have] := f shr (-one_e);
        // MOD by one
        f := f and one_mask;
        // next
        inc(n_digits_have);
      end;
      {$endif CPU32}
    until true;
  {$ifdef CPU32}
  // Append "sticky" digit if any
  if (f <> 0) and
     (n_digits_have >= n_digits_need + 1) then
  begin
    // single "<>0" digit is enough
    n_digits_have := n_digits_need + 2;
    buf[n_digits_need + 1] := 1;
  end;
  {$endif CPU32}
  // Round to n_digits_need using "roundTiesToEven"
  if n_digits_have > n_digits_need then
    inc(dot_pos, d2a_round_digits(buf, n_digits_have, n_digits_need));
  // Generate output
  if frac_digits >= 0 then
  begin
    d2a_return_fixed(str, minus, buf, n_digits_have, dot_pos - c_mk.e10,
      frac_digits);
    exit;
  end;
  if n_digits_have > n_digits_sci then
    inc(dot_pos, d2a_round_digits(buf, n_digits_have, n_digits_sci
      {$ifdef GRISU1_F2A_HALF_ROUNDUP}, false {$endif} ));
  d2a_return_exponential(str, minus, @buf, n_digits_have, n_digits_sci,
    dot_pos - c_mk.e10 - 1);
end;

function DoubleToShort(S: PShortString; const Value: double): integer;
var
  valueabs: double;
begin
  valueabs := abs(Value);
  if (valueabs > {$ifdef FPC}double{$endif}(DOUBLE_HI)) or
     (valueabs < {$ifdef FPC}double{$endif}(DOUBLE_LO)) then
    // = str(Value,S) for scientific notation outside of 1E-9<Value<1E9 range
    DoubleToAscii(C_NO_MIN_WIDTH, -1, Value, pointer(S))
  else
  begin
    // inlined DoubleToShortNoExp() = str(Value:0:15,S^)
    DoubleToAscii(0, DOUBLE_PRECISION, Value, pointer(S));
    S^[0] := AnsiChar(FloatStringNoExp(pointer(S), DOUBLE_PRECISION));
  end;
  result := ord(S^[0]);
end;

function DoubleToShortNoExp(S: PShortString; const Value: double): integer;
begin
  DoubleToAscii(0, DOUBLE_PRECISION, Value, pointer(S)); // = str(Value:0:15,S^)
  result := FloatStringNoExp(pointer(S), DOUBLE_PRECISION);
  S^[0] := AnsiChar(result);
end;

{$else} // use regular Extended version

function DoubleToShort(S: PShortString; const Value: double): integer;
begin
  result := ExtendedToShort(S, Value, DOUBLE_PRECISION);
end;

function DoubleToShortNoExp(S: PShortString; const Value: double): integer;
begin
  result := ExtendedToShortNoExp(S, Value, DOUBLE_PRECISION);
end;

{$endif DOUBLETOSHORT_USEGRISU}

function DoubleToJson(tmp: PShortString; Value: double;
  NoExp: boolean): PShortString;
begin
  if Value = 0 then
    result := @JSON_NAN[fnNumber]
  else
  begin
    if NoExp then
      DoubleToShortNoExp(tmp, Value)
    else
      DoubleToShort(tmp, Value);
    result := FloatToJsonNan(tmp);
  end;
end;

function DoubleToStr(Value: Double): RawUtf8;
begin
  DoubleToStr(Value, result);
end;

procedure DoubleToStr(Value: Double; var result: RawUtf8);
var
  tmp: ShortString;
begin
  if Value = 0 then
    result := SmallUInt32Utf8[0]
  else
    FastSetString(result, @tmp[1], DoubleToShort(@tmp, Value));
end;

function FloatStrCopy(s, d: PUtf8Char): PUtf8Char;
var
  c: AnsiChar;
begin
  while s^=' ' do
    inc(s);
  c := s^;
  if (c='+') or
     (c='-') then
  begin
    inc(s);
    d^ := c;
    inc(d);
    c := s^;
  end;
  if c = '.' then
  begin
    PCardinal(d)^ := ord('0')+ord('.')shl 8; // '.5' -> '0.5'
    inc(d,2);
    inc(s);
    c := s^;
  end;
  if (c >= '0') and
     (c <= '9') then
    repeat
      inc(s);
      d^ := c;
      inc(d);
      c := s^;
      if ((c >= '0') and
          (c <= '9')) or
         (c = '.') then
        continue;
      if (c <> 'e') and
         (c <> 'E') then
        break;
      inc(s);
      d^ := c; // 1.23e120 or 1.23e-45
      inc(d);
      c := s^;
      if c = '-' then
      begin
        inc(s);
        d^ := c;
        inc(d);
        c := s^;
      end;
      while (c >= '0') and
            (c <= '9') do
      begin
        inc(s);
        d^ := c;
        inc(d);
        c := s^;
      end;
      break;
    until false;
  result := d;
end;

function Char2ToByte(P: PUtf8Char; out Value: cardinal;
   ConvertHexToBinTab: PByteArray): boolean;
var
  B: PtrUInt;
begin
  B := ConvertHexToBinTab[ord(P[0])];
  if B <= 9 then
  begin
    Value := B;
    B := ConvertHexToBinTab[ord(P[1])];
    if B <= 9 then
    begin
      Value := Value * 10 + B;
      result := false;
      exit;
    end;
  end;
  result := true; // error
end;

function Char3ToWord(P: PUtf8Char; out Value: cardinal;
   ConvertHexToBinTab: PByteArray): boolean;
var
  B: PtrUInt;
begin
  B := ConvertHexToBinTab[ord(P[0])];
  if B <= 9 then
  begin
    Value := B;
    B := ConvertHexToBinTab[ord(P[1])];
    if B <= 9 then
    begin
      Value := Value * 10 + B;
      B := ConvertHexToBinTab[ord(P[2])];
      if B <= 9 then
      begin
        Value := Value * 10 + B;
        result := false;
        exit;
      end;
    end;
  end;
  result := true; // error
end;

function Char4ToWord(P: PUtf8Char; out Value: cardinal;
   ConvertHexToBinTab: PByteArray): boolean;
var
  B: PtrUInt;
begin
  B := ConvertHexToBinTab[ord(P[0])];
  if B <= 9 then
  begin
    Value := B;
    B := ConvertHexToBinTab[ord(P[1])];
    if B <= 9 then
    begin
      Value := Value * 10 + B;
      B := ConvertHexToBinTab[ord(P[2])];
      if B <= 9 then
      begin
        Value := Value * 10 + B;
        B := ConvertHexToBinTab[ord(P[3])];
        if B <= 9 then
        begin
          Value := Value * 10 + B;
          result := false;
          exit;
        end;
      end;
    end;
  end;
  result := true; // error
end;

procedure VariantToUtf8(const V: Variant; var result: RawUtf8;
  var wasString: boolean);
var
  tmp: TVarData;
  vt: cardinal;
begin
  wasString := false;
  vt := TVarData(V).VType;
  with TVarData(V) do
    case vt of
      varEmpty, varNull:
        result := NULL_STR_VAR;
      varSmallint:
        Int32ToUtf8(VSmallInt, result);
      varShortInt:
        Int32ToUtf8(VShortInt, result);
      varWord:
        UInt32ToUtf8(VWord, result);
      varLongWord:
        UInt32ToUtf8(VLongWord, result);
      varByte:
        result := SmallUInt32Utf8[VByte];
      varBoolean:
        if VBoolean then
          result := SmallUInt32Utf8[1]
        else
          result := SmallUInt32Utf8[0];
      varInteger:
        Int32ToUtf8(VInteger, result);
      varInt64:
        Int64ToUtf8(VInt64, result);
      varWord64:
        UInt64ToUtf8(VInt64, result);
      varSingle:
        ExtendedToStr(VSingle, SINGLE_PRECISION, result);
      varDouble:
        DoubleToStr(VDouble, result);
      varCurrency:
        Curr64ToStr(VInt64, result);
      varDate:
        begin
          _VariantToUtf8DateTimeToIso8601(VDate, 'T', result, {withms=}false);
          wasString := true;
        end;
      varString:
        begin
          wasString := true;
          {$ifdef HASCODEPAGE}
          AnyAnsiToUtf8(RawByteString(VString), result);
          {$else}
          result := RawUtf8(VString);
          {$endif HASCODEPAGE}
        end;
      {$ifdef HASVARUSTRING}
      varUString:
        begin
          wasString := true;
          RawUnicodeToUtf8(VAny, length(UnicodeString(VAny)), result);
        end;
      {$endif HASVARUSTRING}
      varOleStr:
        begin
          wasString := true;
          RawUnicodeToUtf8(VAny, length(WideString(VAny)), result);
        end;
      varOlePAnsiChar: // = VT_LPSTR
        begin
          wasString := true;
          CurrentAnsiConvert.AnsiBufferToRawUtf8(VString, StrLen(VString), result);
        end;
      varOlePWideChar: // = VT_LPWSTR
        begin
          wasString := true;
          RawUnicodeToUtf8(VAny, StrLenW(VAny), result);
        end;
    else
      if SetVariantUnRefSimpleValue(V, tmp{%H-}) then
        // simple varByRef
        VariantToUtf8(Variant(tmp), result, wasString)
      else if vt = varVariantByRef then{%H-}
        // complex varByRef
        VariantToUtf8(PVariant(VPointer)^, result, wasString)
      else if vt = varStringByRef then
      begin
        wasString := true;
        {$ifdef HASCODEPAGE}
        AnyAnsiToUtf8(PRawByteString(VString)^, result);
        {$else}
        result := PRawUtf8(VString)^;
        {$endif HASCODEPAGE}
      end
      else if vt = varOleStrByRef then
      begin
        wasString := true;
        RawUnicodeToUtf8(pointer(PWideString(VAny)^),
          length(PWideString(VAny)^), result);
      end
      else
      {$ifdef HASVARUSTRING}
      if vt = varUStringByRef then
      begin
        wasString := true;
        RawUnicodeToUtf8(pointer(PUnicodeString(VAny)^),
          length(PUnicodeString(VAny)^), result);
      end
      else
      {$endif HASVARUSTRING}
        // not recognizable vt -> seralize as JSON to handle also custom types
        _VariantSaveJson(V, twJsonEscape, result); // = mormot.core.variants.pas
    end;
end;

function VariantToUtf8(const V: Variant): RawUtf8;
var
  wasString: boolean;
begin
  VariantToUtf8(V, result, wasString);
end;

function ToUtf8(const V: Variant): RawUtf8;
var
  wasString: boolean;
begin
  VariantToUtf8(V, result, wasString);
end;

function ToUtf8(const V: TVarData): RawUtf8; overload;
var
  wasString: boolean;
begin
  VariantToUtf8(PVariant(@V)^, result, wasString);
end;

function VariantToUtf8(const V: Variant; var Text: RawUtf8): boolean;
begin
  VariantToUtf8(V, Text, result);
end;

function VariantToText(const V: Variant; var Text: RawUtf8): boolean;
begin
  result := not VarIsEmptyOrNull(V) and
            VariantToUtf8(V, Text);
end;

function VariantSaveJson(const Value: variant; Escape: TTextWriterKind): RawUtf8;
begin
  _VariantSaveJson(Value, Escape, result);
end;

procedure VariantSaveJson(const Value: variant; Escape: TTextWriterKind;
  var result: RawUtf8);
begin
  _VariantSaveJson(Value, Escape, result);
end;

procedure __VariantSaveJson(const Value: variant; Escape: TTextWriterKind;
  var result: RawUtf8);
begin
  raise ESynException.Create('VariantSaveJson() unsupported:' +
    ' please include mormot.core.variants to your uses clause');
end;

procedure __VariantToUtf8DateTimeToIso8601(DT: TDateTime; FirstChar: AnsiChar;
  var result: RawUtf8; WithMS: boolean);
begin
  raise ESynException.Create('VariantToUtf8(varDate) unsupported:' +
    ' please include mormot.core.datetime to your uses clause');
end;

function VariantCompAsText(A, B: PVarData; caseInsensitive: boolean): integer;
var
  au, bu: pointer;
  wasString: boolean;
begin
  au := nil; // no try..finally for local RawUtf8 variables
  bu := nil;
  VariantToUtf8(PVariant(A)^, RawUtf8(au), wasString);
  VariantToUtf8(PVariant(B)^, RawUtf8(bu), wasString);
  result := SortDynArrayAnsiStringByCase[caseInsensitive](au, bu);
  FastAssignNew(au);
  FastAssignNew(bu);
end;

function Int18ToChars3(Value: cardinal): RawUtf8;
begin
  FastSetString(result, 3);
  PCardinal(result)^ := ((Value shr 12) and $3f) or
                        ((Value shr 6) and $3f) shl 8 or
                        (Value and $3f) shl 16 + $202020;
end;

procedure Int18ToChars3(Value: cardinal; var result: RawUtf8);
begin
  FastSetString(result, 3);
  PCardinal(result)^ := ((Value shr 12) and $3f) or
                        ((Value shr 6) and $3f) shl 8 or
                        (Value and $3f) shl 16 + $202020;
end;

function Chars3ToInt18(P: pointer): cardinal;
begin
  result := PCardinal(P)^ - $202020;
  result := ((result shr 16) and $3f) or
            ((result shr 8) and $3f) shl 6 or
            (result and $3f) shl 12;
end;

function UInt3DigitsToUtf8(Value: cardinal): RawUtf8;
begin
  FastSetString(result, 3);
  PWordArray(result)[0] := TwoDigitLookupW[Value div 10];
  PByteArray(result)[2] := (Value mod 10) + 48;
end;

function UInt4DigitsToUtf8(Value: cardinal): RawUtf8;
begin
  FastSetString(result, 4);
  if Value > 9999 then
    Value := 9999;
  YearToPChar(Value, pointer(result));
end;

function UInt4DigitsToShort(Value: cardinal): TShort4;
begin
  result[0] := #4;
  if Value > 9999 then
    Value := 9999;
  YearToPChar(Value, @result[1]);
end;

function UInt3DigitsToShort(Value: cardinal): TShort4;
begin
  if Value > 999 then
    Value := 999;
  YearToPChar(Value, @result[0]);
  result[0] := #3; // override first digit
end;

function UInt2DigitsToShort(Value: byte): TShort4;
begin
  result[0] := #2;
  if Value > 99 then
    Value := 99;
  PCardinal(@result[1])^ := TwoDigitLookupW[Value];
end;

function UInt2DigitsToShortFast(Value: byte): TShort4;
begin
  result[0] := #2;
  PCardinal(@result[1])^ := TwoDigitLookupW[Value];
end;

function IPToCardinal(aIP: PUtf8Char; out aValue: cardinal): boolean;
var
  i, c: cardinal;
  b: array[0..3] of byte;
begin
  aValue := 0;
  result := false;
  if (aIP = nil) or
     (IdemPChar(aIP, '127.0.0.1') and
      (aIP[9] = #0)) then
    exit;
  for i := 0 to 3 do
  begin
    c := GetNextItemCardinal(aIP, '.');
    if (c > 255) or
       ((aIP = nil) and
        (i < 3)) then
      exit;
    b[i] := c;
  end;
  if PCardinal(@b)^ <> $0100007f then // may be e.g. '127.000.000.001'
  begin
    aValue := PCardinal(@b)^;
    result := true;
  end;
end;

function IPToCardinal(const aIP: RawUtf8; out aValue: cardinal): boolean;
begin
  result := IPToCardinal(pointer(aIP), aValue);
end;

function IPToCardinal(const aIP: RawUtf8): cardinal;
begin
  IPToCardinal(pointer(aIP), result);
end;


{ ************ Text Formatting functions }

function VarRecAsChar(const V: TVarRec): integer;
begin
  case V.VType of
    vtChar:
      result := ord(V.VChar);
    vtWideChar:
      result := ord(V.VWideChar);
  else
    result := 0;
  end;
end;

function VarRecAs(const aArg: TVarRec; aClass: TClass): pointer;
begin
  if (aArg.VType = vtObject) and
     (aArg.VObject <> nil) and
     aArg.VObject.InheritsFrom(aClass) then
    result := aArg.VObject
  else
    result := nil;
end;

function VarRecToInt64(const V: TVarRec; out value: Int64): boolean;
begin
  case V.VType of
    vtInteger:
      value := V.VInteger;
    {$ifdef FPC} vtQWord, {$endif}
    vtInt64:
      value := V.VInt64^;
    vtBoolean:
      if V.VBoolean then
        value := 1
      else
        value := 0; // normalize
    vtVariant:
      value := V.VVariant^;
  else
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

function VarRecToDouble(const V: TVarRec; out value: double): boolean;
begin
  case V.VType of
    vtInteger:
      value := V.VInteger;
    vtInt64:
      value := V.VInt64^;
    {$ifdef FPC}
    vtQWord:
      value := V.VQWord^;
    {$endif FPC}
    vtBoolean:
      if V.VBoolean then
        value := 1
      else
        value := 0; // normalize
    vtExtended:
      value := V.VExtended^;
    vtCurrency:
      value := V.VCurrency^;
    vtVariant:
      value := V.VVariant^;
  else
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

procedure BufToTempUtf8(Buf: PUtf8Char; var Res: TTempUtf8);
begin // Res.Len has been set by caller
  if Res.Len > SizeOf(Res.Temp) then
  begin
    FastSetString(RawUtf8(Res.TempRawUtf8), Buf, Res.Len); // new RawUtf8
    Res.Text := Res.TempRawUtf8;
  end
  else
  begin
    {$ifdef CPUX86}
    MoveFast(Buf^, Res.Temp, Res.Len);    // avoid slow "rep movsd" on FPC i386
    {$else}
    THash192(Res.Temp) := PHash192(Buf)^; // faster than MoveByOne/MoveFast
    {$endif CPUX86}
    Res.Text := @Res.Temp; // no RawUtf8 memory allocation
  end;
end;

procedure DoubleToTempUtf8(V: double; var Res: TTempUtf8);
var
  tmp: shortstring;
begin
  Res.Len := DoubleToShort(@tmp, V);
  BufToTempUtf8(@tmp[1], Res);
end;

procedure WideToTempUtf8(WideChar: PWideChar; WideCharCount: integer;
  var Res: TTempUtf8);
var
  tmp: TSynTempBuffer;
begin
  if (WideChar = nil) or
     (WideCharCount = 0) then
  begin
    Res.Text := nil;
    Res.Len := 0;
  end
  else
  begin
    tmp.Init(WideCharCount * 3);
    Res.Len := RawUnicodeToUtf8(tmp.buf, tmp.len + 1,
      WideChar, WideCharCount, [ccfNoTrailingZero]);
    BufToTempUtf8(tmp.buf, Res);
    tmp.Done;
  end;
end;

procedure PtrIntToTempUtf8(V: PtrInt; var Res: TTempUtf8);
  {$ifdef HASINLINE} inline; {$endif}
begin
  {$ifndef ASMINTEL} // our StrInt32 asm has less CPU cache pollution
  if PtrUInt(V) <= high(SmallUInt32Utf8) then
  begin
    Res.Text := pointer(SmallUInt32Utf8[V]);
    Res.Len := PStrLen(Res.Text - _STRLEN)^;
  end
  else
  {$endif ASMINTEL}
  begin
    Res.Text := PUtf8Char(StrInt32(@Res.Temp[23], V));
    Res.Len := @Res.Temp[23] - Res.Text;
  end;
end;

procedure Int64ToTempUtf8(V: PInt64; var Res: TTempUtf8);
  {$ifdef HASINLINE} inline; {$endif}
begin
{$ifdef CPU64}
  PtrIntToTempUtf8(V^, Res);
{$else}
  if (PCardinalArray(V)^[0] <= high(SmallUInt32Utf8)) and
     (PCardinalArray(V)^[1] = 0) then
  begin
    Res.Text := pointer(SmallUInt32Utf8[PPtrInt(V)^]);
    Res.Len := PStrLen(Res.Text - _STRLEN)^;
  end
  else
  begin
    Res.Text := PUtf8Char(StrInt64(@Res.Temp[23], V^));
    Res.Len := @Res.Temp[23] - Res.Text;
  end;
{$endif CPU64}
end;

procedure QWordToTempUtf8(V: PQWord; var Res: TTempUtf8);
  {$ifdef HASINLINE} inline; {$endif}
begin
  {$ifndef ASMINTEL} // our StrUInt64 asm has less CPU cache pollution
  if V^ <= high(SmallUInt32Utf8) then
  begin
    Res.Text := pointer(SmallUInt32Utf8[PPtrInt(V)^]);
    Res.Len := PStrLen(Res.Text - _STRLEN)^;
  end
  else
  {$endif ASMINTEL}
  begin
    Res.Text := PUtf8Char(StrUInt64(@Res.Temp[23], V^));
    Res.Len := @Res.Temp[23] - Res.Text;
  end;
end;

procedure VariantToTempUtf8(const V: variant; var Res: TTempUtf8;
  var wasString: boolean);
var
  tmp: TVarData;
  vt: cardinal;
begin
  wasString := false;
  Res.TempRawUtf8 := nil; // no allocation by default - and avoid GPF
  vt := TVarData(V).VType;
  with TVarData(V) do
    case vt of
      varEmpty,
      varNull:
        begin
          Res.Text := pointer(NULL_STR_VAR);
          Res.Len := 4;
        end;
      varSmallint:
        PtrIntToTempUtf8(VSmallInt, Res);
      varShortInt:
        PtrIntToTempUtf8(VShortInt, Res);
      varWord:
        PtrIntToTempUtf8(VWord, Res);
      varLongWord:
        {$ifdef CPU32}
        if VLongWord > high(SmallUInt32Utf8) then
        begin
          Res.Text := PUtf8Char(StrUInt32(@Res.Temp[23], VLongWord));
          Res.Len := @Res.Temp[23] - Res.Text;
        end
        else
        {$endif CPU32}
          PtrIntToTempUtf8(VLongWord, Res);
      varByte:
        PtrIntToTempUtf8(VByte, Res);
      varBoolean:
        if VBoolean then
        begin
          Res.Text := @BOOL_STR[true][1];
          Res.Len := 4;
        end
        else
        begin
          Res.Text := @BOOL_STR[false][1];
          Res.Len := 5;
        end;
      varInteger:
        PtrIntToTempUtf8(VInteger, Res);
      varInt64:
        Int64ToTempUtf8(@VInt64, Res);
      varWord64:
        QWordToTempUtf8(@VInt64, Res);
      varSingle:
        DoubleToTempUtf8(VSingle, Res);
      varDouble:
        DoubleToTempUtf8(VDouble, Res);
      varCurrency:
        begin
          Res.Len := Curr64ToPChar(VInt64, @Res.Temp);
          Res.Text := @Res.Temp;
        end;
      varDate:
        begin
          wasString := true;
          _VariantToUtf8DateTimeToIso8601(VDate, 'T', RawUtf8(Res.TempRawUtf8), false);
          Res.Text := pointer(Res.TempRawUtf8);
          Res.Len := length(RawUtf8(Res.TempRawUtf8));
        end;
      varString:
        begin
          wasString := true;
          Res.Text := VString; // assume RawUtf8
          Res.Len := length(RawUtf8(VString));
        end;
      {$ifdef HASVARUSTRING}
      varUString:
        begin
          wasString := true;
          WideToTempUtf8(VAny, length(UnicodeString(VAny)), Res);
        end;
      {$endif HASVARUSTRING}
      varOleStr:
        begin
          wasString := true;
          WideToTempUtf8(VAny, length(WideString(VAny)), Res);
        end;
    else
      if SetVariantUnRefSimpleValue(V, tmp{%H-}) then
        // simple varByRef
        VariantToTempUtf8(Variant(tmp), Res, wasString)
      else if vt = varVariantByRef then{%H-}
        // complex varByRef
        VariantToTempUtf8(PVariant(VPointer)^, Res, wasString)
      else if vt = varStringByRef then
      begin
        wasString := true;
        Res.Text := PPointer(VString)^; // assume RawUtf8
        Res.Len := length(PRawUtf8(VString)^);
      end
      else if vt = varOleStrByRef then
      begin
        wasString := true;
        WideToTempUtf8(PPointer(VAny)^, length(PWideString(VAny)^), Res);
      end
      else
      {$ifdef HASVARUSTRING}
      if vt = varUStringByRef then
      begin
        wasString := true;
        WideToTempUtf8(PPointer(VAny)^, length(PUnicodeString(VAny)^), Res);
      end
      else
      {$endif HASVARUSTRING}
      begin
        // not recognizable vt -> seralize as JSON to handle also custom types
        wasString := true;
        _VariantSaveJson(V, twJsonEscape, RawUtf8(Res.TempRawUtf8));
        Res.Text := pointer(Res.TempRawUtf8);
        Res.Len := length(RawUtf8(Res.TempRawUtf8));
      end;
   end;
end;

function VarRecToTempUtf8(const V: TVarRec; var Res: TTempUtf8;
  wasString: PBoolean): PtrInt;
var
  isString: boolean;
begin
  isString := true;
  Res.TempRawUtf8 := nil; // no allocation by default - and avoid GPF
  case V.VType of
    vtString:
      if V.VString = nil then
        Res.Len := 0
      else
      begin
        Res.Text := @V.VString^[1];
        Res.Len := ord(V.VString^[0]);
      end;
    vtAnsiString:
      begin
        // expect UTF-8 content
        Res.Text := pointer(V.VAnsiString);
        Res.Len := length(RawUtf8(V.VAnsiString));
      end;
    {$ifdef HASVARUSTRING}
    vtUnicodeString:
      WideToTempUtf8(V.VPWideChar, length(UnicodeString(V.VUnicodeString)), Res);
    {$endif HASVARUSTRING}
    vtWideString:
      WideToTempUtf8(V.VPWideChar, length(WideString(V.VWideString)), Res);
    vtPChar:
      begin
        // expect UTF-8 content
        Res.Text := V.VPointer;
        Res.Len := mormot.core.base.StrLen(V.VPointer);
      end;
    vtChar:
      begin
        Res.Temp[0] := V.VChar; // V may be on transient stack (alf: FPC)
        Res.Text := @Res.Temp;
        Res.Len := 1;
      end;
    vtPWideChar:
      WideToTempUtf8(V.VPWideChar, StrLenW(V.VPWideChar), Res);
    vtWideChar:
      WideToTempUtf8(@V.VWideChar, 1, Res);
    vtBoolean:
      begin
        isString := false;
        if V.VBoolean then // normalize
          Res.Text := pointer(SmallUInt32Utf8[1])
        else
          Res.Text := pointer(SmallUInt32Utf8[0]);
        Res.Len := 1;
      end;
    vtInteger:
      begin
        isString := false;
        PtrIntToTempUtf8(V.VInteger, Res);
      end;
    vtInt64:
      begin
        isString := false;
        Int64ToTempUtf8(V.VInt64, Res);
      end;
    {$ifdef FPC}
    vtQWord:
      begin
        isString := false;
        QwordToTempUtf8(V.VQWord, Res);
      end;
    {$endif FPC}
    vtCurrency:
      begin
        isString := false;
        Res.Text := @Res.Temp;
        Res.Len := Curr64ToPChar(V.VInt64^, Res.Temp);
      end;
    vtExtended:
      begin
        isString := false;
        DoubleToTempUtf8(V.VExtended^, Res);
      end;
    vtPointer, vtInterface:
      begin
        Res.Text := @Res.Temp;
        Res.Len := DisplayMinChars(@V.VPointer, SizeOf(pointer)) * 2;
        BinToHexDisplayLower(@V.VPointer, @Res.Temp, Res.Len shr 1);
      end;
    vtClass:
      if V.VClass = nil then
        Res.Len := 0
      else
      begin
        Res.Text := PPUtf8Char(PtrInt(PtrUInt(V.VClass)) + vmtClassName)^ + 1;
        Res.Len := ord(Res.Text[-1]);
      end;
    vtObject:
      if V.VObject = nil then
        Res.Len := 0
      else
      begin
        Res.Text := PPUtf8Char(PPtrInt(V.VObject)^ + vmtClassName)^ + 1;
        Res.Len := ord(Res.Text[-1]);
      end;
    vtVariant:
      VariantToTempUtf8(V.VVariant^, Res, isString);
  else
    Res.Len := 0;
  end;
  result := Res.Len;
  if wasString <> nil then
    wasString^ := isString;
end;

procedure VarRecToUtf8(const V: TVarRec; var result: RawUtf8; wasString: PBoolean);
var
  isString: boolean;
label
  none;
begin
  isString := false;
  with V do
    case V.VType of
      vtString:
        begin
          isString := true;
          if VString = nil then
            goto none;
          FastSetString(result, @VString^[1], ord(VString^[0]));
        end;
      vtAnsiString:
        begin
          isString := true;
          result := RawUtf8(VAnsiString); // expect UTF-8 content
        end;
      {$ifdef HASVARUSTRING}
      vtUnicodeString:
        begin
          isString := true;
          RawUnicodeToUtf8(VUnicodeString,
            length(UnicodeString(VUnicodeString)), result);
        end;
      {$endif HASVARUSTRING}
      vtWideString:
        begin
          isString := true;
          RawUnicodeToUtf8(VWideString, length(WideString(VWideString)), result);
        end;
      vtPChar:
        begin
          isString := true;
          FastSetString(result, VPChar, mormot.core.base.StrLen(VPChar));
        end;
      vtChar:
        begin
          isString := true;
          FastSetString(result, PAnsiChar(@VChar), 1);
        end;
      vtPWideChar:
        begin
          isString := true;
          RawUnicodeToUtf8(VPWideChar, StrLenW(VPWideChar), result);
        end;
      vtWideChar:
        begin
          isString := true;
          RawUnicodeToUtf8(@VWideChar, 1, result);
        end;
      vtBoolean:
        if VBoolean then // normalize
          result := SmallUInt32Utf8[1]
        else
          result := SmallUInt32Utf8[0];
      vtInteger:
        Int32ToUtf8(VInteger, result);
      vtInt64:
        Int64ToUtf8(VInt64^, result);
      {$ifdef FPC}
      vtQWord:
        UInt64ToUtf8(VQWord^, result);
      {$endif FPC}
      vtCurrency:
        Curr64ToStr(VInt64^, result);
      vtExtended:
        DoubleToStr(VExtended^,result);
      vtPointer:
        begin
          isString := true;
          PointerToHex(VPointer, result);
        end;
      vtClass:
        begin
          isString := true;
          if VClass <> nil then
            ClassToText(VClass, result)
          else
none:       result := '';
        end;
      vtObject:
        if VObject <> nil then
          ClassToText(PClass(VObject)^, result)
        else
          goto none;
      vtInterface:
      {$ifdef HASINTERFACEASTOBJECT}
        if VInterface <> nil then
          ClassToText((IInterface(VInterface) as TObject).ClassType, result)
        else
          goto none;
      {$else}
        PointerToHex(VInterface,result);
      {$endif HASINTERFACEASTOBJECT}
      vtVariant:
        VariantToUtf8(VVariant^, result, isString);
    else
      goto none;
    end;
  if wasString <> nil then
    wasString^ := isString;
end;

function VarRecToUtf8IsString(const V: TVarRec; var value: RawUtf8): boolean;
begin
  VarRecToUtf8(V, value, @result);
end;

procedure VarRecToInlineValue(const V: TVarRec; var result: RawUtf8);
var
  wasString: boolean;
  tmp: RawUtf8;
begin
  VarRecToUtf8(V, tmp, @wasString);
  if wasString then
    QuotedStr(tmp, '"', result)
  else
    result := tmp;
end;

function FormatUtf8(const Format: RawUtf8; const Args: array of const): RawUtf8;
begin
  FormatUtf8(Format, Args, result);
end;

function FormatVariant(const Format: RawUtf8; const Args: array of const): variant;
begin
  ClearVariantForString(result);
  FormatUtf8(Format, Args, RawUtf8(TVarData(result).VString));
end;

type
  // 3KB info on stack - only supported token is %, with any const arguments
  {$ifdef USERECORDWITHMETHODS}
  TFormatUtf8 = record
  {$else}
  TFormatUtf8 = object
  {$endif USERECORDWITHMETHODS}
  public
    last: PTempUtf8;
    L: PtrInt;
    blocks: array[0..63] of TTempUtf8; // to avoid most heap allocations
    procedure TooManyArgs;
    procedure Parse(const Format: RawUtf8; Arg: PVarRec; ArgCount: PtrInt);
    procedure Add(const SomeText: RawUtf8);
    procedure DoDelim(Arg: PVarRec; ArgCount: integer; EndWithDelim: boolean;
      Delim: AnsiChar);
    procedure DoAdd(Arg: PVarRec; ArgCount: integer);
      {$ifdef HASINLINE} inline; {$endif}
    procedure DoAppendLine(var Text: RawUtf8; Arg: PVarRec; ArgCount: PtrInt;
      const Separator: shortstring);
    procedure DoPrepend(var Text: RawUtf8; Arg: PVarRec;
      ArgCount, CodePage: PtrInt);
    procedure Write(Dest: PUtf8Char);
    procedure WriteString(var result: string);
    function WriteMax(Dest: PUtf8Char; Max: PtrUInt): PUtf8Char;
  end;

procedure TFormatUtf8.TooManyArgs;
begin
  raise ESynException.Create('TFormatUtf8: too many arguments');
end;

procedure TFormatUtf8.Parse(const Format: RawUtf8; Arg: PVarRec; ArgCount: PtrInt);
var
  F, FDeb: PUtf8Char;
  c: PTempUtf8;
begin
  if ArgCount >= length(blocks) div 2 then
    TooManyArgs;
  L := 0;
  c := @blocks;
  F := pointer(Format);
  repeat
    if F^ = #0 then
      break
    else if F^ <> '%' then
    begin
      FDeb := F;
      repeat
        inc(F);
      until (F^ = '%') or
            (F^ = #0);
      c^.Text := FDeb;
      c^.Len := F - FDeb;
      inc(L, c^.Len);
      c^.TempRawUtf8 := nil;
      inc(c);
      if F^ = #0 then
        break;
    end;
    inc(F); // jump '%'
    if ArgCount <> 0 then
    begin
      inc(L, VarRecToTempUtf8(Arg^, c^));
      if c^.Len > 0 then
        inc(c);
      inc(Arg);
      dec(ArgCount);
      if F^ = #0 then
        break;
    end
    else // no more available Args -> add all remaining text
    if F^ = #0 then
      break
    else
    begin
      c^.Text := F;
      c^.Len := length(Format) - (F - pointer(Format));
      inc(L, c^.Len);
      c^.TempRawUtf8 := nil;
      inc(c);
      break;
    end;
  until false;
  last := c;
end;

procedure TFormatUtf8.DoDelim(Arg: PVarRec; ArgCount: integer;
  EndWithDelim: boolean; Delim: AnsiChar);
var
  c: PTempUtf8;
begin
  L := 0;
  if ArgCount > 0 then
    if ArgCount >= length(blocks) div 2 then
      TooManyArgs
    else
    begin
      c := @blocks;
      repeat
        inc(L, VarRecToTempUtf8(Arg^, c^));
        inc(Arg);
        if (EndWithDelim and
            (ArgCount = 1)) or
           ((ArgCount <> 1) and
            (c^.Len <> 0) and
            (c^.Text[c^.Len - 1] <> Delim)) then
        begin
          inc(c);
          c^.Len := 1;
          c^.Text := @c^.Temp;
          c^.Temp[0] := Delim;
          c^.TempRawUtf8 := nil;
          inc(L);
        end;
        inc(c);
        dec(ArgCount);
      until ArgCount = 0;
      last := c;
    end;
end;

procedure TFormatUtf8.Add(const SomeText: RawUtf8);
begin
  if PtrUInt(last) > PtrUInt(@blocks[high(blocks)]) then
    TooManyArgs;
  with last^ do
  begin
    Len := length(SomeText);
    inc(L, Len);
    Text := pointer(SomeText);
    TempRawUtf8 := nil;
  end;
  inc(last);
end;

procedure TFormatUtf8.DoAdd(Arg: PVarRec; ArgCount: integer);
begin
  L := 0;
  if ArgCount <= 0 then
    exit
  else if ArgCount > length(blocks) then
    TooManyArgs;
  last := @blocks;
  repeat
    inc(L, VarRecToTempUtf8(Arg^, last^));
    inc(Arg);
    inc(last);
    dec(ArgCount)
  until ArgCount = 0;
end;

procedure TFormatUtf8.DoAppendLine(var Text: RawUtf8;
  Arg: PVarRec; ArgCount: PtrInt; const Separator: shortstring);
var
  c: PTempUtf8;
begin
  if ArgCount <= 0 then
    exit
  else if ArgCount >= length(blocks) then
    TooManyArgs;
  L := length(Text);
  c := @blocks;
  if (Text <> '') and
     (Separator[0] <> #0) then
  begin
    c^.Len := ord(Separator[0]);
    inc(L, c^.Len);
    c^.Text := @Separator[1];
    c^.TempRawUtf8 := nil;
    inc(c);
  end;
  repeat
    inc(L, VarRecToTempUtf8(Arg^, c^));
    inc(Arg);
    inc(c);
    dec(ArgCount)
  until ArgCount = 0;
  last := c;
  ArgCount := length(Text);
  SetLength(Text, L); // realloc in-place and append the new text
  Write(PUtf8Char(@PByteArray(Text)[ArgCount]));
end;

procedure TFormatUtf8.DoPrepend(var Text: RawUtf8; Arg: PVarRec;
  ArgCount, CodePage: PtrInt);
var
  c: PTempUtf8;
  new: PUtf8Char;
begin
  if ArgCount <= 0 then
    exit;
  L := length(Text);
  c := @blocks;
  repeat
    inc(L, VarRecToTempUtf8(Arg^, c^));
    inc(Arg);
    inc(c);
    dec(ArgCount)
  until ArgCount = 0;
  last := c;
  ArgCount := length(Text);
  new := pointer(FastNewString(L, CodePage));
  MoveFast(pointer(Text)^, new[L - ArgCount], ArgCount);
  FastAssignNew(Text, new);
  Write(new);
end;

procedure TFormatUtf8.Write(Dest: PUtf8Char);
var
  d: PTempUtf8;
begin
  if L = 0 then
    exit;
  d := @blocks;
  repeat
    MoveFast(d^.Text^, Dest^, d^.Len); // no MoveByOne() - may be huge result
    inc(Dest, d^.Len);
    if d^.TempRawUtf8 <> nil then
      {$ifdef FPC}
      FastAssignNew(d^.TempRawUtf8);
      {$else}
      RawUtf8(d^.TempRawUtf8) := '';
      {$endif FPC}
    inc(d);
  until d = last;
end;

function TFormatUtf8.WriteMax(Dest: PUtf8Char; Max: PtrUInt): PUtf8Char;
var
  d: PTempUtf8;
begin
  if (Max > 0) and
     (L <> 0) and
     (Dest <> nil) then
  begin
    inc(Max, PtrUInt(Dest));
    d := @blocks;
    repeat
      if PtrUInt(Dest) + PtrUInt(d^.Len) > Max then
      begin
        // avoid buffer overflow
        MoveFast(d^.Text^, Dest^, Max - PtrUInt(Dest));
        repeat
          if d^.TempRawUtf8 <> nil then
            {$ifdef FPC}
            FastAssignNew(d^.TempRawUtf8); // release temp RawUtf8
            {$else}
            RawUtf8(d^.TempRawUtf8) := '';
            {$endif FPC}
          inc(d);
        until d = last; // avoid memory leak
        result := PUtf8Char(Max);
        exit;
      end;
      MoveFast(d^.Text^, Dest^, d^.Len);
      inc(Dest, d^.Len);
      if d^.TempRawUtf8 <> nil then
        {$ifdef FPC}
        FastAssignNew(d^.TempRawUtf8);
        {$else}
        RawUtf8(d^.TempRawUtf8) := '';
        {$endif FPC}
      inc(d);
    until d = last;
  end;
  result := Dest;
end;

procedure TFormatUtf8.WriteString(var result: string);
var
  temp: TSynTempBuffer; // will avoid most memory allocations
begin
  result := '';
  if L = 0 then
    exit;
  {$ifndef UNICODE}
  if Unicode_CodePage = CP_UTF8 then // e.g. on POSIX or Windows + Lazarus
  begin
    FastSetString(RawUtf8(result), L);
    Write(pointer(result)); // here string=UTF8String=RawUtf8
    exit;
  end;
  {$endif UNICODE}
  temp.Init(L);
  Write(temp.buf);
  Utf8DecodeToString(temp.buf, L, result);
  temp.Done;
end;

procedure FormatUtf8(const Format: RawUtf8; const Args: array of const;
  out result: RawUtf8);
var
  f: TFormatUtf8;
begin
  if (Format = '') or
     (high(Args) < 0) then // no formatting needed
    result := Format
  else if PWord(Format)^ = ord('%') then // optimize raw conversion
    VarRecToUtf8(Args[0], result)
  else
  begin
    f.Parse(Format, @Args[0], length(Args));
    FastSetString(result, f.L);
    f.Write(pointer(result));
  end;
end;

procedure FormatShort(const Format: RawUtf8; const Args: array of const;
  var result: ShortString);
var
  f: TFormatUtf8;
begin
  if (Format = '') or
     (high(Args) < 0) then // no formatting needed
    SetString(result, PAnsiChar(pointer(Format)), length(Format))
  else
  begin
    f.Parse(Format, @Args[0], length(Args));
    result[0] := AnsiChar(f.WriteMax(@result[1], 255) - @result[1]);
  end;
end;

function FormatBuffer(const Format: RawUtf8; const Args: array of const;
  Dest: pointer; DestLen: PtrInt): PtrInt;
var
  f: TFormatUtf8;
begin
  if (Dest = nil) or
     (DestLen <= 0) then
  begin
    result := 0;
    exit; // avoid buffer overflow
  end;
  f.Parse(Format, @Args[0], length(Args));
  result := PtrUInt(f.WriteMax(Dest, DestLen)) - PtrUInt(Dest);
end;

function FormatToShort(const Format: RawUtf8;
  const Args: array of const): ShortString;
var
  f: TFormatUtf8;
begin
  f.Parse(Format, @Args[0], length(Args));
  result[0] := AnsiChar(f.WriteMax(@result[1], 255) - @result[1]);
end;

procedure FormatShort16(const Format: RawUtf8; const Args: array of const;
  var result: TShort16);
var
  f: TFormatUtf8;
begin
  if (Format = '') or
     (high(Args) < 0) then // no formatting needed
    SetString(result, PAnsiChar(pointer(Format)), length(Format))
  else
  begin
    f.Parse(Format, @Args[0], length(Args));
    result[0] := AnsiChar(f.WriteMax(@result[1], 16) - @result[1]);
  end;
end;

procedure FormatString(const Format: RawUtf8; const Args: array of const;
  out result: string);
var
  f: TFormatUtf8;
begin
  if (Format = '') or
     (high(Args) < 0) then
    // no formatting needed
    Utf8ToStringVar(Format, result)
  else
  begin
    f.Parse(Format, @Args[0], length(Args));
    f.WriteString(result);
  end;
end;

function FormatString(const Format: RawUtf8; const Args: array of const): string;
begin
  FormatString(Format, Args, result);
end;

procedure AppendLine(var Text: RawUtf8; const Args: array of const;
  const Separator: shortstring);
var
  f: TFormatUtf8;
begin
  {%H-}f.DoAppendLine(Text, @Args[0], length(Args), Separator);
end;

procedure Append(var Text: RawUtf8; const Args: array of const);
var
  f: TFormatUtf8;
begin
  {%H-}f.DoAppendLine(Text, @Args[0], length(Args), '');
end;

procedure Append(var Text: RawByteString; const Args: array of const);
var
  f: TFormatUtf8;
begin
  {%H-}f.DoAppendLine(RawUtf8(Text), @Args[0], length(Args), '');
  if Text <> '' then
    FakeCodePage(Text, CP_RAWBYTESTRING);
end;

procedure Append(var Text: RawUtf8; const Added: RawByteString);
begin
  if Added <> '' then
    Append(Text, pointer(Added), PStrLen(PtrUInt(Added) - _STRLEN)^);
end;

procedure Append(var Text: RawUtf8; const Added1, Added2: RawByteString);
var
  l, a1, a2: PtrInt;
begin
  l := length(Text);
  a1 := length(Added1);
  a2 := length(Added2);
  SetLength(Text, l + a1 + a2);
  MoveFast(pointer(Added1)^, PByteArray(Text)[l], a1);
  MoveFast(pointer(Added2)^, PByteArray(Text)[l + a1], a2);
end;

procedure Append(var Text: RawUtf8; Added: AnsiChar);
begin
  Append(Text, @Added, 1);
end;

procedure Append(var Text: RawUtf8; Added: pointer; AddedLen: PtrInt);
var
  t: PtrInt;
begin
  if (Added = nil) or (AddedLen <= 0) then
    exit;
  t := length(Text);
  SetLength(Text, t + AddedLen);
  MoveFast(pointer(Added)^, PByteArray(Text)[t], AddedLen);
end;

procedure Append(var Text: RawByteString; const Added: RawByteString);
begin
  if Added <> '' then
    Append(Text, pointer(Added), PStrLen(PtrUInt(Added) - _STRLEN)^);
end;

procedure Append(var Text: RawByteString; Added: pointer; AddedLen: PtrInt);
var
  t: PtrInt;
begin
  if (Added = nil) or
     (AddedLen <= 0) then
    exit;
  t := length(Text);
  SetLength(Text, t + AddedLen);
  MoveFast(Added^, PByteArray(Text)^[t], AddedLen);
  if Text <> '' then
    FakeCodePage(Text, CP_RAWBYTESTRING);
end;

procedure Prepend(var Text: RawUtf8; const Args: array of const);
var
  f: TFormatUtf8;
begin
  {%H-}f.DoPrepend(Text, @Args[0], length(Args), CP_UTF8);
end;

procedure Prepend(var Text: RawByteString; const Added: RawByteString);
var
  t, a: PtrInt;
  new: PAnsiChar;
begin
  t := length(Text);
  a := length(Added);
  if a <> 0 then
    if t = 0 then
      Text := Added
    else
    begin
      new := FastNewString(t + a, CP_RAWBYTESTRING);
      MoveFast(PByteArray(Text)[0], new[a], t);
      MoveFast(PByteArray(Added)[0], new[0], a);
      FastAssignNew(Text, new);
    end;
end;

procedure Prepend(var Text: RawByteString; Added: AnsiChar);
var
  t: PtrInt;
begin
  t := length(Text);
  SetLength(Text, t + 1); // is likely to avoid any reallocmem
  MoveFast(PByteArray(Text)[0], PByteArray(Text)[1], t);
  PByteArray(Text)[0] := ord(Added);
end;

procedure Prepend(var Text: RawByteString; const Args: array of const);
var
  f: TFormatUtf8;
begin
  {%H-}f.DoPrepend(RawUtf8(Text), @Args[0], length(Args), CP_RAWBYTESTRING);
end;

function Make(const Args: array of const): RawUtf8;
var
  f: TFormatUtf8;
begin
  {%H-}f.DoAdd(@Args[0], length(Args));
  FastSetString(result, f.L);
  f.Write(pointer(result));
end;

procedure Make(const Args: array of const; var Result: RawUtf8);
var
  f: TFormatUtf8;
begin
  {%H-}f.DoAdd(@Args[0], length(Args));
  FastSetString(result, f.L);
  f.Write(pointer(result));
end;

function MakeString(const Args: array of const): string;
var
  f: TFormatUtf8;
begin
  {%H-}f.DoAdd(@Args[0], length(Args));
  f.WriteString(result);
end;

function MakePath(const Part: array of const; EndWithDelim: boolean;
  Delim: AnsiChar): TFileName;
var
  f: TFormatUtf8;
begin
  {%H-}f.DoDelim(@Part[0], length(Part), EndWithDelim, Delim);
  f.WriteString(string(result));
end;

function MakeFileName(const Part: array of const; LastIsExt: boolean): TFileName;
var
  f: TFormatUtf8;
  ext: RawUtf8;
  hipart: integer;
begin
  hipart := High(Part);
  if LastIsExt then
    if (hipart > 0) and
       VarRecToUtf8IsString(Part[hipart], ext) then
      dec(hipart)
    else
      LastIsExt := false;
  f.DoDelim(@Part[0], hipart + 1, false, PathDelim);
  if LastIsExt and
     (ext <> '') then
  begin
    if ext[1] <> '.' then
      f.Add('.');
    f.Add(ext);
  end;
  f.WriteString(string(result));
end;

function MakeCsv(const Value: array of const; EndWithComma: boolean;
  Comma: AnsiChar): RawUtf8;
var
  f: TFormatUtf8;
begin
  f.DoDelim(@Value[0], length(Value), EndWithComma, Comma);
  FastSetString(result, f.L);
  f.Write(pointer(result));
end;

function StringToConsole(const S: string): RawByteString;
begin
  result := Utf8ToConsole(StringToUtf8(S));
end;

procedure ConsoleWrite(const Fmt: RawUtf8; const Args: array of const;
  Color: TConsoleColor; NoLineFeed: boolean);
var
  tmp: RawUtf8;
begin
  FormatUtf8(Fmt, Args, tmp);
  ConsoleWrite(tmp, Color, NoLineFeed);
end;

procedure ConsoleWrite(const Args: array of const;
  Color: TConsoleColor; NoLineFeed: boolean);
var
  tmp: RawUtf8;
begin
  Append(tmp, Args);
  ConsoleWrite(tmp, Color, NoLineFeed);
end;

procedure ConsoleShowFatalException(E: Exception; WaitForEnterKey: boolean);
begin
  ConsoleWrite(#13#10'Fatal exception ', ccLightRed, true);
  ConsoleWrite('%', [E.ClassType], ccWhite, true);
  ConsoleWrite(' raised with message ', ccLightRed);
  ConsoleWrite('  %', [E.Message], ccLightMagenta);
  TextColor(ccLightGray);
  if WaitForEnterKey then
  begin
    ConsoleWrite(#13#10'Program will now abort');
    {$ifndef OSPOSIX}
    ConsoleWrite('Press [Enter] to quit');
    ConsoleWaitForEnterKey;
    {$endif OSPOSIX}
  end;
end;


{ ************ Resource and Time Functions }

procedure KB(bytes: Int64; out result: TShort16; nospace: boolean);
type
  TUnits = (kb, mb, gb, tb, pb, eb, b);
const
  TXT: array[{nospace:}boolean, TUnits] of RawUtf8 = (
    (' KB', ' MB', ' GB', ' TB', ' PB', ' EB', '% B'),
    ( 'KB',  'MB',  'GB',  'TB',  'PB',  'EB', '%B'));
var
  hi, rem: cardinal;
  u: TUnits;
begin
  if bytes < 1 shl 10 - (1 shl 10) div 10 then
  begin
    FormatShort16(TXT[nospace, b], [integer(bytes)], result);
    exit;
  end;
  if bytes < 1 shl 20 - (1 shl 20) div 10 then
  begin
    u := kb;
    rem := bytes;
    hi  := bytes shr 10;
  end
  else if bytes < 1 shl 30 - (1 shl 30) div 10 then
  begin
    u := mb;
    rem := bytes shr 10;
    hi  := bytes shr 20;
  end
  else if bytes < Int64(1) shl 40 - (Int64(1) shl 40) div 10 then
  begin
    u := gb;
    rem := bytes shr 20;
    hi  := bytes shr 30;
  end
  else if bytes < Int64(1) shl 50 - (Int64(1) shl 50) div 10 then
  begin
    u := tb;
    rem := bytes shr 30;
    hi  := bytes shr 40;
  end
  else if bytes < Int64(1) shl 60 - (Int64(1) shl 60) div 10 then
  begin
    u := pb;
    rem := bytes shr 40;
    hi  := bytes shr 50;
  end
  else
  begin
    u := eb;
    rem := bytes shr 50;
    hi  := bytes shr 60;
  end;
  rem := rem and 1023;
  if rem <> 0 then
    rem := rem div 102;
  if rem = 10 then
  begin
    rem := 0;
    inc(hi); // round up as expected by (most) human beings
  end;
  if rem <> 0 then
    FormatShort16('%.%%', [hi, rem, TXT[nospace, u]], result)
  else
    FormatShort16('%%', [hi, TXT[nospace, u]], result);
end;

function KB(bytes: Int64): TShort16;
begin
  KB(bytes, result, {nospace=}false);
end;

function KBNoSpace(bytes: Int64): TShort16;
begin
  KB(bytes, result, {nospace=}true);
end;

function KB(bytes: Int64; nospace: boolean): TShort16;
begin
  KB(bytes, result, nospace);
end;

function KB(const buffer: RawByteString): TShort16;
begin
  KB(length(buffer), result, {nospace=}false);
end;

procedure KBU(bytes: Int64; var result: RawUtf8);
var
  tmp: TShort16;
begin
  KB(bytes, tmp, {nospace=}false);
  FastSetString(result, @tmp[1], ord(tmp[0]));
end;

procedure K(value: Int64; out result: TShort16);
begin
  KB(Value, result, {nospace=}true);
  if result[0] <> #0 then
    dec(result[0]); // just trim last 'B' ;)
end;

function K(value: Int64): TShort16;
begin
  K(Value, result);
end;

function IntToThousandString(Value: integer;
  const ThousandSep: TShort4): ShortString;
var
  i, L, Len: cardinal;
begin
  str(Value, result);
  L := length(result);
  Len := L + 1;
  if Value < 0 then
    // ignore '-' sign
    dec(L, 2)
  else
    dec(L);
  for i := 1 to L div 3 do
    insert(ThousandSep, result, Len - i * 3);
end;

function SecToString(S: QWord): TShort16;
begin
  MicroSecToString(S * 1000000, result);
end;

function MilliSecToString(MS: QWord): TShort16;
begin
  MicroSecToString(MS * 1000, result);
end;

function MicroSecToString(Micro: QWord): TShort16;
begin
  MicroSecToString(Micro, result);
end;

function MicroSecFrom(Start: QWord): TShort16;
var
  stop: Int64;
begin
  QueryPerformanceMicroSeconds(stop);
  MicroSecToString(stop - Int64(Start), result);
end;

procedure By100ToTwoDigitString(value: cardinal; const valueunit: ShortString;
  var result: TShort16);
var
  d100: TDiv100Rec;
begin
  if value < 100 then
    FormatShort16('0.%%', [UInt2DigitsToShortFast(value), valueunit], result)
  else
  begin
    Div100(value, d100{%H-});
    if d100.m = 0 then
      FormatShort16('%%', [d100.d, valueunit], result)
    else
      FormatShort16('%.%%', [d100.d, UInt2DigitsToShortFast(d100.m), valueunit], result);
  end;
end;

procedure _TimeToString(value: cardinal; const u: ShortString;
  var result: TShort16);
var
  d: cardinal;
begin
  d := value div 60;
  FormatShort16('%%%',
    [d, u, UInt2DigitsToShortFast(value - (d * 60))], result);
end;

procedure MicroSecToString(Micro: QWord; out result: TShort16);
begin
  if Int64(Micro) <= 0 then
    PCardinal(@result)^ := 3 + ord('0') shl 8 + ord('u') shl 16 + ord('s') shl 24
  else if Micro < 1000 then
    FormatShort16('%us', [Micro], result)
  else if Micro < 1000000 then
    By100ToTwoDigitString(
      {$ifdef CPU32} PCardinal(@Micro)^ {$else} Micro {$endif} div 10, 'ms', result)
  else if Micro < 60000000 then
    By100ToTwoDigitString(
      {$ifdef CPU32} PCardinal(@Micro)^ {$else} Micro {$endif} div 10000, 's', result)
  else if Micro < QWord(3600000000) then
    _TimeToString(
      {$ifdef CPU32} PCardinal(@Micro)^ {$else} Micro {$endif} div 1000000, 'm', result)
  else if Micro < QWord(86400000000 * 2) then
    _TimeToString(Micro div 60000000, 'h', result)
  else
    FormatShort16('%d', [Micro div QWord(86400000000)], result)
end;

procedure NanoSecToString(Nano: QWord; out result: TShort16);
begin
  if Int64(Nano) <= 0 then
    PCardinal(@result)^ := 3 + ord('0') shl 8 + ord('n') shl 16 + ord('s') shl 24
  else if Nano > 9900 then
    MicroSecToString(Nano div 1000, result)
  else if Nano >= 1000 then
    By100ToTwoDigitString(
      {$ifdef CPU32} PCardinal(@Nano)^ {$else} Nano {$endif} div 10, 'us', result)
  else
    By100ToTwoDigitString(
      {$ifdef CPU32} PCardinal(@Nano)^ {$else} Nano {$endif} * 100, 'ns', result);
end;


{ ************ ESynException class }

{ ESynException }

procedure ESynException.CreateAfterSetMessageUtf8;
begin
  inherited Create(Utf8ToString(fMessageUtf8));
end;

constructor ESynException.CreateUtf8(const Format: RawUtf8;
  const Args: array of const);
begin
  FormatUtf8(Format, Args, fMessageUtf8);
  CreateAfterSetMessageUtf8;
end;

constructor ESynException.CreateU(const Msg: RawUtf8);
begin
  fMessageUtf8 := Msg;
  CreateAfterSetMessageUtf8;
end;

constructor ESynException.CreateLastOSError(const Format: RawUtf8;
  const Args: array of const; const Trailer: ShortString);
var
  error: integer;
  fmt: RawUtf8;
begin
  error := GetLastError;
  FormatUtf8('% 0x% [%] %', [Trailer, CardinalToHexShort(error),
    StringReplaceAll(GetErrorText(error), '%', '#'), Format], fmt);
  CreateUtf8(fmt, Args);
end;

{$ifndef NOEXCEPTIONINTERCEPT}

function DefaultSynLogExceptionToStr(WR: TTextWriter;
  const Context: TSynLogExceptionContext): boolean;
var
  extcode: cardinal;
  extnames: TPUtf8CharDynArray;
  i: PtrInt;
begin
  WR.AddClassName(Context.EClass);
  if (Context.ELevel = sllException) and
     (Context.EInstance <> nil) and
     (Context.EClass <> EExternalException) then
  begin
    extcode := Context.AdditionalInfo(extnames);
    if extcode <> 0 then
    begin
      WR.AddShorter(' 0x');
      WR.AddBinToHexDisplayLower(@extcode, SizeOf(extcode));
      for i := 0 to high(extnames) do
      begin
        {$ifdef OSWINDOWS}
        WR.AddShort(' [.NET/CLR unhandled ');
        {$else}
        WR.AddShort(' [unhandled ');
        {$endif OSWINDOWS}
        WR.AddNoJsonEscape(extnames[i]);
        WR.AddShort('Exception]');
      end;
    end;
    WR.AddDirect(' ');
    if WR.ClassType = TTextWriter then
      {$ifdef UNICODE}
      WR.AddOnSameLineW(pointer(Context.EInstance.Message), 0)
      {$else}
      WR.AddOnSameLine(pointer(Context.EInstance.Message))
      {$endif UNICODE}
    else
      WR.WriteObject(Context.EInstance); // use RTTI for JSON serialization
  end
  else if Context.ECode <> 0 then
  begin
    WR.AddDirect(' ', '(');
    WR.AddPointer(Context.ECode);
    WR.AddDirect(')');
  end;
  result := false; // caller should append "at EAddr" and the stack trace
end;

function ESynException.CustomLog(WR: TTextWriter;
  const Context: TSynLogExceptionContext): boolean;
begin
  if Assigned(TSynLogExceptionToStrCustom) then
    result := TSynLogExceptionToStrCustom(WR, Context)
  else
    result := DefaultSynLogExceptionToStr(WR, Context);
end;

{$endif NOEXCEPTIONINTERCEPT}


function StatusCodeToErrorMsg(Code: integer): RawUtf8;
begin
  FormatUtf8('HTTP Error % - %', [Code, StatusCodeToText(Code)^], result);
end;


{ **************** Hexadecimal Text And Binary Conversion }

procedure BinToHex(Bin, Hex: PAnsiChar; BinBytes: PtrInt);
var
  {$ifdef CPUX86NOTPIC}
  tab: TAnsiCharToWord absolute TwoDigitsHexW;
  {$else}
  tab: PAnsiCharToWord; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  tab := @TwoDigitsHexW;
  {$endif CPUX86NOTPIC}
  if BinBytes > 0 then
    repeat
      PWord(Hex)^ := tab[Bin^];
      inc(Bin);
      inc(Hex, 2);
      dec(BinBytes);
    until BinBytes = 0;
end;

function BinToHex(const Bin: RawByteString): RawUtf8;
var
  L: integer;
begin
  L := length(Bin);
  FastSetString(result, L * 2);
  mormot.core.text.BinToHex(pointer(Bin), pointer(result), L);
end;

function BinToHex(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8;
begin
  FastSetString(result, BinBytes * 2);
  mormot.core.text.BinToHex(Bin, pointer(result), BinBytes);
end;

function HexToBin(Hex: PAnsiChar; HexLen: PtrInt;
  var Bin: RawByteString): boolean;
begin
  Bin := '';
  if HexLen and 1 <> 0 then
  begin
    result := false;
    exit; // hexadecimal should be in char pairs
  end;
  HexLen := HexLen shr 1;
  pointer(Bin) := FastNewString(HexLen, CP_RAWBYTESTRING);
  result := mormot.core.text.HexToBin(Hex, pointer(Bin), HexLen);
  if not result then
    Bin := '';
end;

function HexToBin(const Hex: RawUtf8): RawByteString;
begin
  HexToBin(pointer(Hex), length(Hex), result);
end;

function HexaToByte(P: PUtf8Char; var Dest: byte; tab: PByteArray): boolean;
  {$ifdef HASINLINE}inline;{$endif}
var
  b, c: byte;
begin
  b := tab[Ord(P[0]) + 256]; // + 256 for shl 4
  if b <> 255 then
  begin
    c := tab[Ord(P[1])];
    if c <> 255 then
    begin
      inc(b, c);
      Dest := b;
      result := true;
      exit;
    end;
  end;
  result := false; // mark error
end;

function HumanHexToBin(const hex: RawUtf8; var Bin: RawByteString): boolean;
var
  len: PtrInt;
  h, p: PAnsiChar;
  tab: PByteArray;
begin
  Bin := '';
  result := false;
  len := length(hex);
  if len = 0 then
    exit;
  p := FastNewString(len shr 1, CP_RAWBYTESTRING); // shr 1 = maximum length
  pointer(Bin) := p;
  h := pointer(hex);
  tab := @ConvertHexToBin;
  repeat
    while h^ = ' ' do
      inc(h);
    if not HexaToByte(pointer(h), PByte(p)^, tab) then
      break; // invalid 'xx' pair - may be len < 2
    inc(p);
    inc(h, 2);
    dec(len, 2);
    if len = 0 then
    begin
      result := true; // properly ended with 'xx' last hexa byte
      break;
    end;
    while h^ = ' ' do
      inc(h);
    if h^ <> ':' then
      continue;
    dec(len);
    if len = 0 then
      break; // should not end with ':'
    inc(h);
  until false;
  if result then
    FakeLength(Bin, p - pointer(Bin))
  else
    Bin := '';
end;

function HumanHexCompare(a, b: PUtf8Char): integer;
var
  ca, cb: byte;
  tab: PByteArray;
begin
  result := 0;
  if a <> b then
    if a <> nil then
      if b <> nil then
      begin
        tab := @ConvertHexToBin;
        repeat
          while a^ = ' ' do
            inc(a);
          while b^ = ' ' do
            inc(b);
          if not HexaToByte(pointer(a), ca{%H-}, tab) or
             not HexaToByte(pointer(b), cb{%H-}, tab) then
          begin
            result := ComparePointer(a, b); // consistent but not zero
            break;
          end;
          result := ca - cb;
          if result <> 0 then
            break;
          inc(a, 2);
          inc(b, 2);
          while a^ = ' ' do
            inc(a);
          while b^ = ' ' do
            inc(b);
          case a^ of
            #0:
              begin
                if b^ <> #0 then
                  dec(result);
                break;
              end;
            ':':
              inc(a);
          end;
          case b^ of
            #0:
              begin
                inc(result); // we know a^<>#0
                break;
              end;
            ':':
              inc(b);
          end;
        until false;
      end
      else
        inc(result)
    else
      dec(result);
end;

function HumanHexCompare(const a, b: RawUtf8): integer;
begin
  result := HumanHexCompare(pointer(a), pointer(b));
end;

function HumanHexToBin(const hex: RawUtf8): RawByteString;
begin
  HumanHexToBin(hex, result);
end;

function ByteToHex(P: PAnsiChar; Value: byte): PAnsiChar;
begin
  PWord(P)^ := TwoDigitsHexWB[Value];
  result := P + 2;
end;

procedure BinToHexDisplay(Bin, Hex: PAnsiChar; BinBytes: PtrInt);
var
  {$ifdef CPUX86NOTPIC}
  tab: TAnsiCharToWord absolute TwoDigitsHexW;
  {$else}
  tab: PAnsiCharToWord; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  tab := @TwoDigitsHexW;
  {$endif CPUX86NOTPIC}
  inc(Hex, BinBytes * 2);
  if BinBytes > 0 then
    repeat
      dec(Hex, 2);
      PWord(Hex)^ := tab[Bin^];
      inc(Bin);
      dec(BinBytes);
    until BinBytes = 0;
end;

function BinToHexDisplay(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8;
begin
  FastSetString(result, BinBytes * 2);
  BinToHexDisplay(Bin, pointer(result), BinBytes);
end;

procedure BinToHexLower(Bin, Hex: PAnsiChar; BinBytes: PtrInt);
var
  {$ifdef CPUX86NOTPIC}
  tab: TAnsiCharToWord absolute TwoDigitsHexWLower;
  {$else}
  tab: PAnsiCharToWord; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  tab := @TwoDigitsHexWLower;
  {$endif CPUX86NOTPIC}
  if BinBytes > 0 then
    repeat
      PWord(Hex)^ := tab[Bin^];
      inc(Bin);
      inc(Hex, 2);
      dec(BinBytes);
    until BinBytes = 0;
end;

function BinToHexLower(const Bin: RawByteString): RawUtf8;
begin
  BinToHexLower(pointer(Bin), length(Bin), result);
end;

procedure BinToHexLower(Bin: PAnsiChar; BinBytes: PtrInt; var result: RawUtf8);
begin
  FastSetString(result, BinBytes * 2);
  BinToHexLower(Bin, pointer(result), BinBytes);
end;

function BinToHexLower(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8;
begin
  BinToHexLower(Bin, BinBytes, result);
end;

procedure BinToHexDisplayLower(Bin, Hex: PAnsiChar; BinBytes: PtrInt);
var
  {$ifdef CPUX86NOTPIC}
  tab: TAnsiCharToWord absolute TwoDigitsHexWLower;
  {$else}
  tab: PAnsiCharToWord; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  if (Bin = nil) or
     (Hex = nil) or
     (BinBytes <= 0) then
    exit;
  {$ifndef CPUX86NOTPIC}
  tab := @TwoDigitsHexWLower;
  {$endif CPUX86NOTPIC}
  inc(Hex, BinBytes * 2);
  repeat
    dec(Hex, 2);
    PWord(Hex)^ := tab[Bin^];
    inc(Bin);
    dec(BinBytes);
  until BinBytes = 0;
end;

function BinToHexDisplayLower(Bin: PAnsiChar; BinBytes: PtrInt): RawUtf8;
begin
  FastSetString(result, BinBytes * 2);
  BinToHexDisplayLower(Bin, pointer(result), BinBytes);
end;

function BinToHexDisplayLowerShort(Bin: PAnsiChar; BinBytes: PtrInt): ShortString;
begin
  if BinBytes > 127 then
    BinBytes := 127;
  result[0] := AnsiChar(BinBytes * 2);
  BinToHexDisplayLower(Bin, @result[1], BinBytes);
end;

function {%H-}BinToHexDisplayLowerShort16(Bin: Int64; BinBytes: PtrInt): TShort16;
begin
  if BinBytes > 8 then
    BinBytes := 8;
  result[0] := AnsiChar(BinBytes * 2);
  BinToHexDisplayLower(@Bin, @result[1], BinBytes);
end;

procedure BinBitsToHexDisplayLowerShort16(Bin: Int64; BinBits: PtrInt;
  var Result: TShort16);
begin
  Result[0] := AnsiChar(BitsToBytes(BinBits) * 2);
  if Result[0] > #16 then
    Result[0] := #16;
  BinToHexDisplayLower(@Bin, @Result[1], ord(Result[0]) shr 1);
end;

{$ifdef UNICODE}
function BinToHexDisplayFile(Bin: PAnsiChar; BinBytes: PtrInt): TFileName;
var
  temp: TSynTempBuffer;
begin
  temp.Init(BinBytes * 2);
  BinToHexDisplayLower(Bin, temp.Buf, BinBytes);
  Ansi7ToString(PWinAnsiChar(temp.buf), BinBytes * 2, string(result));
  temp.Done;
end;
{$else}
function BinToHexDisplayFile(Bin: PAnsiChar; BinBytes: PtrInt): TFileName;
begin
  SetString(result, nil, BinBytes * 2);
  BinToHexDisplayLower(Bin, pointer(result), BinBytes);
end;
{$endif UNICODE}

procedure PointerToHex(aPointer: Pointer; var result: RawUtf8);
begin
  FastSetString(result, SizeOf(Pointer) * 2);
  BinToHexDisplay(@aPointer, pointer(result), SizeOf(Pointer));
end;

function PointerToHex(aPointer: Pointer): RawUtf8;
begin
  PointerToHex(aPointer, result);
end;

function CardinalToHex(aCardinal: cardinal): RawUtf8;
begin
  FastSetString(result, SizeOf(aCardinal) * 2);
  BinToHexDisplay(@aCardinal, pointer(result), SizeOf(aCardinal));
end;

function CardinalToHexLower(aCardinal: cardinal): RawUtf8;
begin
  FastSetString(result, SizeOf(aCardinal) * 2);
  BinToHexDisplayLower(@aCardinal, pointer(result), SizeOf(aCardinal));
end;

function Int64ToHex(aInt64: Int64): RawUtf8;
begin
  FastSetString(result, SizeOf(Int64) * 2);
  BinToHexDisplay(@aInt64, pointer(result), SizeOf(Int64));
end;

procedure Int64ToHex(aInt64: Int64; var result: RawUtf8);
begin
  FastSetString(result, SizeOf(Int64) * 2);
  BinToHexDisplay(@aInt64, pointer(result), SizeOf(Int64));
end;

function PointerToHexShort(aPointer: Pointer): TShort16;
begin
  result[0] := AnsiChar(DisplayMinChars(@aPointer, SizeOf(aPointer)) * 2);
  BinToHexDisplayLower(@aPointer, @result[1], ord(result[0]) shr 1);
end;

function CardinalToHexShort(aCardinal: cardinal): TShort16;
begin
  result[0] := AnsiChar(SizeOf(aCardinal) * 2);
  BinToHexDisplay(@aCardinal, @result[1], SizeOf(aCardinal));
end;

function crc32cUtf8ToHex(const str: RawUtf8): RawUtf8;
begin
  result := CardinalToHex(crc32c(0, pointer(str), length(str)));
end;

function Int64ToHexShort(aInt64: Int64): TShort16;
begin
  result[0] := AnsiChar(SizeOf(aInt64) * 2);
  BinToHexDisplay(@aInt64, @result[1], SizeOf(aInt64));
end;

function ToHexShort(P: pointer; Len: PtrInt): TShort64;
begin
  if Len = 0 then
  begin
    result[0] := AnsiChar(Len);
    exit;
  end;
  if Len > 32 then
    Len := 32;
  Len := DisplayMinChars(P, Len);
  result[0] := AnsiChar(Len * 2);
  BinToHexDisplay(P, @result[1], Len);
end;

function Int64ToHexLower(aInt64: Int64): RawUtf8;
var
  L: PtrInt;
begin
  L := DisplayMinChars(@aInt64, SizeOf(Int64));
  FastSetString(result, L * 2);
  BinToHexDisplay(@aInt64, pointer(result), L);
end;

procedure Int64ToHexShort(aInt64: Int64; out result: TShort16);
begin
  result[0] := AnsiChar(SizeOf(aInt64) * 2);
  BinToHexDisplay(@aInt64, @result[1], SizeOf(aInt64));
end;

function Int64ToHexString(aInt64: Int64): string;
var
  temp: TShort16;
begin
  Int64ToHexShort(aInt64, temp);
  Ansi7ToString(@temp[1], ord(temp[0]), result);
end;

function HexDisplayToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: PtrInt): boolean;
var
  b, c: byte;
  {$ifdef CPUX86NOTPIC}
  tab: THexToDualByte absolute ConvertHexToBin;
  {$else}
  tab: PByteArray; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  result := false; // return false if any invalid char
  if (Hex = nil) or
     (Bin = nil) then
    exit;
  {$ifndef CPUX86NOTPIC}
  tab := @ConvertHexToBin;
  {$endif CPUX86NOTPIC}
  if BinBytes > 0 then
  begin
    inc(Bin, BinBytes - 1); // display = reverse order
    repeat
      b := tab[Ord(Hex[0]) + 256]; // + 256 for shl 4
      if b = 255 then
        exit;
      c := tab[Ord(Hex[1])];
      if c = 255 then
        exit;
      Bin^ := b or c;
      dec(Bin);
      inc(Hex, 2);
      dec(BinBytes);
    until BinBytes = 0;
  end;
  result := true; // correct content in Hex
end;

function HexDisplayToCardinal(Hex: PAnsiChar; out aValue: cardinal): boolean;
begin
  result := HexDisplayToBin(Hex, @aValue, SizeOf(aValue));
  if not result then
    aValue := 0;
end;

function HexDisplayToInt64(Hex: PAnsiChar; out aValue: Int64): boolean;
begin
  result := HexDisplayToBin(Hex, @aValue, SizeOf(aValue));
  if not result then
    aValue := 0;
end;

function HexDisplayToInt64(const Hex: RawByteString): Int64;
begin
  if not HexDisplayToBin(pointer(Hex), @result, SizeOf(result)) then
    result := 0;
end;

function HexToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: PtrInt): boolean;
var
  b, c: byte;
  {$ifdef CPUX86NOTPIC}
  tab: THexToDualByte absolute ConvertHexToBin;
  {$else}
  tab: PByteArray; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  result := false; // return false if any invalid char
  if Hex = nil then
    exit;
  {$ifndef CPUX86NOTPIC}
  tab := @ConvertHexToBin;
  {$endif CPUX86NOTPIC}
  if BinBytes > 0 then
    if Bin <> nil then
      repeat
        b := tab[Ord(Hex[0]) + 256]; // + 256 for shl 4
        if b = 255 then
          exit;
        c := tab[Ord(Hex[1])];
        if c = 255 then
          exit;
        inc(Hex, 2);
        Bin^ := b or c;
        inc(Bin);
        dec(BinBytes);
      until BinBytes = 0
    else
      repeat // Bin=nil -> validate Hex^ input
        if (tab[Ord(Hex[0])] > 15) or
           (tab[Ord(Hex[1])] > 15) then
          exit;
        inc(Hex, 2);
        dec(BinBytes);
      until BinBytes = 0;
  result := true; // conversion OK
end;

procedure HexToBinFast(Hex: PAnsiChar; Bin: PByte; BinBytes: PtrInt);
var
  {$ifdef CPUX86NOTPIC}
  tab: THexToDualByte absolute ConvertHexToBin;
  {$else}
  tab: PByteArray; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
  c: byte;
begin
  {$ifndef CPUX86NOTPIC}
  tab := @ConvertHexToBin;
  {$endif CPUX86NOTPIC}
  if BinBytes > 0 then
    repeat
      c := tab[ord(Hex[0]) + 256]; // + 256 for shl 4
      c := tab[ord(Hex[1])] or c;
      Bin^ := c;
      inc(Hex, 2);
      inc(Bin);
      dec(BinBytes);
    until BinBytes = 0;
end;

function IsHex(const Hex: RawByteString; BinBytes: PtrInt): boolean;
begin
  result := (length(Hex) = BinBytes * 2) and
    mormot.core.text.HexToBin(pointer(Hex), nil, BinBytes);
end;

function HexToCharValid(Hex: PAnsiChar): boolean;
begin
  result := (ConvertHexToBin[Ord(Hex[0])] <= 15) and
            (ConvertHexToBin[Ord(Hex[1])] <= 15);
end;

function HexToCharValid(Hex: PAnsiChar; HexToBin: PByteArray): boolean;
begin
  result := (HexToBin[Ord(Hex[0])] <= 15) and
            (HexToBin[Ord(Hex[1])] <= 15);
end;

function HexToChar(Hex: PAnsiChar; Bin: PUtf8Char; HexToBin: PByteArray): boolean;
var
  b, c: byte;
begin
  if Hex <> nil then
  begin
    b := HexToBin[ord(Hex[0]) + 256]; // + 256 for shl 4
    c := HexToBin[ord(Hex[1])];
    if (b <> 255) and
       (c <> 255) then
    begin
      if Bin <> nil then
      begin
        inc(c, b);
        Bin^ := AnsiChar(c);
      end;
      result := true;
      exit;
    end;
  end;
  result := false; // return false if any invalid char
end;

function HexToWideChar(Hex: PUtf8Char): cardinal;
var
  B: cardinal;
  {$ifdef CPUX86NOTPIC}
  tab: THexToDualByte absolute ConvertHexToBin;
  {$else}
  tab: PByteArray; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  {$ifndef CPUX86NOTPIC}
  tab := @ConvertHexToBin;
  {$endif CPUX86NOTPIC}
  result := tab[ord(Hex[0])];
  if result <= 15 then
  begin
    result := result shl 12;
    B := tab[ord(Hex[1])];
    if B <= 15 then
    begin
      B := B shl 8;
      inc(result, B);
      B := tab[ord(Hex[2])];
      if B <= 15 then
      begin
        B := B shl 4;
        inc(result, B);
        B := tab[ord(Hex[3])];
        if B <= 15 then
        begin
          inc(result, B);
          exit;
        end;
      end;
    end;
  end;
  result := 0;
end;

function OctToBin(Oct: PAnsiChar; Bin: PByte): PtrInt;
var
  c, v: byte;
label
  _nxt;
begin
  result := PtrInt(Bin);
  if Oct <> nil then
    repeat
      c := ord(Oct^);
      inc(Oct);
      if c <> ord('\') then
      begin
        if c = 0 then
          break;
_nxt:   Bin^ := c;
        inc(Bin);
        continue;
      end;
      c := ord(Oct^);
      inc(Oct);
      if c = ord('\') then
        goto _nxt;
      dec(c, ord('0'));
      if c > 3 then
        // stop at malformated input (includes #0)
        break;
      c := c shl 6;
      v := c;
      c := ord(Oct[0]);
      dec(c, ord('0'));
      if c > 7 then
        break;
      c := c shl 3;
      v := v or c;
      c := ord(Oct[1]);
      dec(c, ord('0'));
      if c > 7 then
        break;
      c := c or v;
      Bin^ := c;
      inc(Bin);
      inc(Oct, 2);
    until false;
  result := PAnsiChar(Bin) - PAnsiChar(result);
end;

function OctToBin(const Oct: RawUtf8): RawByteString;
var
  tmp: TSynTempBuffer;
  L: integer;
begin
  tmp.Init(length(Oct));
  try
    L := OctToBin(pointer(Oct), tmp.buf);
    FastSetRawByteString(result, tmp.buf, L);
  finally
    tmp.Done;
  end;
end;

function GuidToText(P: PUtf8Char; guid: PByteArray): PUtf8Char;
var
  i: PtrInt;
  tab: PWordArray;
begin
  // encode as '3F2504E0-4F89-11D3-9A0C-0305E82C3301'
  tab := @TwoDigitsHexWB;
  for i := 3 downto 0 do
  begin
    PWord(P)^ := tab[guid[i]];
    inc(P, 2);
  end;
  inc(PByte(guid), 4);
  for i := 1 to 2 do
  begin
    P[0] := '-';
    PWord(P + 1)^ := tab[guid[1]];
    PWord(P + 3)^ := tab[guid[0]];
    inc(PByte(guid), 2);
    inc(P, 5);
  end;
  P[0] := '-';
  PWord(P + 1)^ := tab[guid[0]];
  PWord(P + 3)^ := tab[guid[1]];
  P[5] := '-';
  inc(PByte(guid), 2);
  inc(P, 6);
  for i := 0 to 5 do
  begin
    PWord(P)^ := tab[guid[i]];
    inc(P, 2);
  end;
  result := P;
end;

function GuidToRawUtf8(const guid: TGuid): RawUtf8;
var
  P: PUtf8Char;
begin
  FastSetString(result, 38);
  P := pointer(result);
  P^ := '{';
  GuidToText(P + 1, @guid)^ := '}';
end;

function ToUtf8(const guid: TGuid): RawUtf8;
begin
  FastSetString(result, 36);
  GuidToText(pointer(result), @Guid);
end;

function GuidToShort(const guid: TGuid): TGuidShortString;
begin
  GuidToShort(Guid, result);
end;

procedure GuidToShort(const guid: TGuid; out dest: TGuidShortString);
begin
  dest[0] := #38;
  dest[1] := '{';
  GuidToText(@dest[2], @guid)^ := '}';
end;

{$ifdef UNICODE}
function GuidToString(const guid: TGuid): string;
var
  tmp: TGuidShortString;
begin
  GuidToShort(guid, tmp);
  Ansi7ToString(@tmp[1], 38, result);
end;
{$else}
function GuidToString(const guid: TGuid): string;
begin
  result := GuidToRawUtf8(guid);
end;
{$endif UNICODE}

function TextToGuid(P: PUtf8Char; guid: PByteArray): PUtf8Char;
var
  i: PtrInt;
  tab: PByteArray;
begin
  // decode from '3F2504E0-4F89-11D3-9A0C-0305E82C3301'
  result := nil;
  tab := @ConvertHexToBin;
  for i := 3 downto 0 do
  begin
    if not HexaToByte(P, guid[i], tab) then
      exit;
    inc(P, 2);
  end;
  inc(PByte(guid), 4);
  for i := 1 to 2 do
  begin
    if P^ = '-' then // '-' separators are optional
      inc(P);
    if not HexaToByte(P, guid[1], tab) or
       not HexaToByte(P + 2, guid[0], tab) then
      exit;
    inc(P, 4);
    inc(PByte(guid), 2);
  end;
  if P^ = '-' then
    inc(P);
  if not HexaToByte(P, guid[0], tab) or // in reverse order than the previous loop
     not HexaToByte(P + 2, guid[1], tab) then
    exit;
  inc(P, 4);
  inc(PByte(guid), 2);
  if P^ = '-' then
    inc(P);
  for i := 0 to 5 do
    if HexaToByte(P, guid[i], tab) then
      inc(P, 2)
    else
      exit;
  result := P;
end;

function StringToGuid(const text: string): TGuid;
{$ifdef UNICODE}
var
  tmp: array[0..35] of byte;
  i: integer;
{$endif UNICODE}
begin
  if (length(text) = 38) and
     (text[1] = '{') and
     (text[38] = '}') then
  begin
    {$ifdef UNICODE}
    for i := 0 to 35 do
      tmp[i] := PWordArray(text)[i + 1];
    if TextToGuid(@tmp, @result) <> nil then
    {$else}
    if TextToGuid(@text[2], @result) <> nil then
    {$endif UNICODE}
      exit; // conversion OK
  end;
  FillZero(PHash128(@result)^);
end;

function RawUtf8ToGuid(const text: RawByteString): TGuid;
begin
  if not RawUtf8ToGuid(text, result) then
    FillZero(PHash128(@result)^);
end;

function RawUtf8ToGuid(const text: RawByteString; out guid: TGuid): boolean;
begin
  result := true;
  case length(text) of
    32, // '3F2504E04F8911D39A0C0305E82C3301' TextToGuid() order, not HexToBin()
    36: // '3F2504E0-4F89-11D3-9A0C-0305E82C3301' JSON compatible layout
      if TextToGuid(pointer(text), @guid) <> nil then
        exit;
    38: // '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}' regular layout
      if (text[1] <> '{') or
         (text[38] <> '}') or
         (TextToGuid(@text[2], @guid) <> nil) then
        exit;
  end;
  result := false;
end;

function TrimGuid(var text: RawUtf8): boolean;
var
  s, d: PUtf8Char;
  L: PtrInt;
  c: AnsiChar;
begin
  s := UniqueRawUtf8(text);
  if s = nil then
  begin
    result := false;
    exit;
  end;
  result := true;
  d := s;
  repeat
    c := s^;
    inc(s);
    case c of
      #0:
        break;
      #1..' ', '-', '{', '}': // trim spaces and GUID/UUID separators
        continue;
      'A'..'F':
        inc(c, 32);    // convert to lower-case
      'a'..'f', '0'..'9':
        ;              // valid hexadecimal char
    else
      result := false; // not a true hexadecimal content
    end;
    d^ := c;
    inc(d);
  until false;
  L := d - pointer(text);
  if L = 0 then
  begin
    FastAssignNew(text);
    result := false;
  end
  else
  begin
    FakeLength(text, L);
    result := result and (L = 32);
  end;
end;

function StreamToRawByteString(aStream: TStream; aSize: Int64;
  aCodePage: integer): RawByteString;
var
  current: Int64;
begin
  result := '';
  if aStream = nil then
    exit;
  current := aStream.Position;
  if (current = 0) and
     aStream.InheritsFrom(TRawByteStringStream) and
     ((aSize < 0) or
      (aSize = length(TRawByteStringStream(aStream).DataString))) then
  begin
    result := TRawByteStringStream(aStream).DataString; // fast COW
    exit;
  end;
  if aSize < 0 then
    aSize := aStream.Size - current;
  if (aSize = 0) or
     (aSize > maxInt) then
    exit;
  pointer(result) := FastNewString(aSize, aCodePage);
  aStream.ReadBuffer(pointer(result)^, aSize);
  aStream.Position := current;
end;

function StreamChangeToRawByteString(aStream: TStream; var aPosition: Int64): RawByteString;
var
  current, size: Int64;
begin
  result := '';
  if aStream = nil then
    exit;
  size := aStream.Size - aPosition;
  if size <= 0 then
    exit; // nothing new
  pointer(result) := FastNewString(size, CP_RAWBYTESTRING);
  current := aStream.Position;
  aStream.Position := aPosition;
  aStream.ReadBuffer(pointer(result)^, size);
  aStream.Position := current;
  aPosition := current;
end;

function RawByteStringToStream(const aString: RawByteString): TStream;
begin
  result := TRawByteStringStream.Create(aString);
end;

function ReadStringFromStream(S: TStream; MaxAllowedSize: integer): RawUtf8;
var
  L: integer;
begin
  result := '';
  L := 0;
  if (S.Read(L, 4) <> 4) or
     (L <= 0) or
     (L > MaxAllowedSize) then
    exit;
  FastSetString(result, L);
  if S.Read(pointer(result)^, L) <> L then
    result := '';
end;

function WriteStringToStream(S: TStream; const Text: RawUtf8): boolean;
var
  L: integer;
begin
  L := length(Text);
  if L = 0 then
    result := S.Write(L, 4) = 4
  else
    {$ifdef FPC}
    result := (S.Write(L, 4) = 4) and
              (S.Write(pointer(Text)^, L) = L);
    {$else}
    result := S.Write(pointer(PtrInt(Text) - SizeOf(integer))^, L + 4) = L + 4;
    {$endif FPC}
end;

const // should be local for better code generation
  HexChars:      array[0..15] of AnsiChar = '0123456789ABCDEF';
  HexCharsLower: array[0..15] of AnsiChar = '0123456789abcdef';

procedure InitializeUnit;
var
  i: PtrInt;
  v: byte;
  c: AnsiChar;
  P: PAnsiChar;
  B: PByteArray;
  tmp: array[0..15] of AnsiChar;
begin
  // initialize internal lookup tables for various text conversions
  for i := 0 to 255 do
  begin
    TwoDigitsHex[i][1]      := HexChars[i shr 4];
    TwoDigitsHex[i][2]      := HexChars[i and $f];
    TwoDigitsHexLower[i][1] := HexCharsLower[i shr 4];
    TwoDigitsHexLower[i][2] := HexCharsLower[i and $f];
  end;
  {$ifndef EXTENDEDTOSHORT_USESTR}
  {$ifdef ISDELPHIXE}
  SettingsUS := TFormatSettings.Create(ENGLISH_LANGID);
  {$else}
  GetLocaleFormatSettings(ENGLISH_LANGID, SettingsUS);
  {$endif ISDELPHIXE}
  SettingsUS.DecimalSeparator := '.'; // value may have been overriden :(
  {$endif EXTENDEDTOSHORT_USESTR}
  {$ifdef DOUBLETOSHORT_USEGRISU}
  MoveFast(TwoDigitLookup[0], TwoDigitByteLookupW[0], SizeOf(TwoDigitLookup));
  for i := 0 to 199 do
    dec(PByteArray(@TwoDigitByteLookupW)[i], ord('0')); // '0'..'9' -> 0..9
  {$endif DOUBLETOSHORT_USEGRISU}
  FillcharFast(ConvertHexToBin[0], SizeOf(ConvertHexToBin), 255); // all to 255
  B := @ConvertHexToBin;
  v := 0;
  for i := ord('0') to ord('9') do
  begin
    B[i] := v;
    B[i + 256] := v shl 4;
    inc(v);
  end;
  for i := ord('A') to ord('F') do
  begin
    B[i] := v;
    B[i + 256] := v shl 4;
    B[i + (ord('a') - ord('A'))] := v;
    B[i + (ord('a') - ord('A') + 256)] := v shl 4;
    inc(v);
  end;
  for i := 0 to high(SmallUInt32Utf8) do
  begin
    P := StrUInt32(@tmp[15], i);
    FastSetString(SmallUInt32Utf8[i], P, @tmp[15] - P);
  end;
  for c := #0 to #127 do
  begin
    XML_ESC[c] := ord(c in [#0..#31, '<', '>', '&', '"', '''']);
    case c of // HTML_ESCAPED: array[1..4] = '&lt;', '&gt;', '&amp;', '&quot;'
      #0,
      '<':
        v := 1;
      '>':
        v := 2;
      '&':
        v := 3;
      '"':
        v := 4;
    else
      v := 0;
    end;
    HTML_ESC[hfAnyWhere, c] := v;
    if c in [#0, '&', '<', '>'] then
      HTML_ESC[hfOutsideAttributes, c] := v;
    if c in [#0, '&', '"'] then
      HTML_ESC[hfWithinAttributes, c] := v;
  end;
  _VariantToUtf8DateTimeToIso8601 := __VariantToUtf8DateTimeToIso8601;
  _VariantSaveJson := __VariantSaveJson;
  TextWriterSharedStream := TRawByteStringStream.Create;
end;



initialization
  InitializeUnit;

finalization
  TextWriterSharedStream.Free;

end.

