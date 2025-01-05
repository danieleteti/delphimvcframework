/// Framework Core Text, Binary and Time Search Engines
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.search;

{
  *****************************************************************************

   Several Indexing and Search Engines, as used by other parts of the framework
    - Files Search in Folders
    - ScanUtf8, GLOB and SOUNDEX Text Search
    - Efficient CSV Parsing using RTTI
    - Versatile Expression Search Engine
    - Bloom Filter Probabilistic Index
    - Binary Buffers Delta Compression
    - TDynArray Low-Level Binary Search and Iteration
    - TSynFilter and TSynValidate Processing Classes
    - Cross-Platform TSynTimeZone Time Zones

  *****************************************************************************
}

interface

{$I mormot.defines.inc}

uses
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.json;


{ ****************** Files Search in Folders }

type
  {$A-}
  /// define one file found result item, as returned by FindFiles()
  // - Delphi "object" is buggy on stack -> also defined as record with methods
  {$ifdef USERECORDWITHMETHODS}
  TFindFiles = record
  {$else}
  TFindFiles = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the matching file name
    // - including its folder name unless ffoExcludesDir is set
    Name: TFileName;
    /// the matching file attributes
    Attr: integer;
    /// the matching file size
    Size: Int64;
    /// the matching file local date/time
    Timestamp: TDateTime;
    /// fill the item properties from a FindFirst/FindNext's TSearchRec
    procedure FromSearchRec(const Directory: TFileName; const F: TSearchRec);
    /// returns some ready-to-be-loggued text
    function ToText: ShortString;
  end;
  {$A+}

  /// result list, as returned by FindFiles()
  TFindFilesDynArray = array of TFindFiles;

  /// one optional feature of FindFiles()
  // - ffoSortByName will sort the result files by extension then name
  // - ffoExcludesDir won't include the path in TFindFiles.Name
  // - ffoSubFolder will search within nested folders
  TFindFilesOption = (
    ffoSortByName,
    ffoExcludesDir,
    ffoSubFolder);
  /// the optional features of FindFiles()
  TFindFilesOptions = set of TFindFilesOption;

/// search for matching files by names
// - just an enhanced wrapper around FindFirst/FindNext with some options
// - you may specify several masks in Mask, e.g. as '*.jpg;*.jpeg'
function FindFiles(const Directory: TFileName;
  const Mask: TFileName = FILES_ALL; const IgnoreFileName: TFileName = '';
  Options: TFindFilesOptions = []): TFindFilesDynArray;

/// search for matching file names
// - just a wrapper around FindFilesDynArrayToFileNames(FindFiles())
function FileNames(const Directory: TFileName;
  const Mask: TFileName = FILES_ALL; Options: TFindFilesOptions = [];
  const IgnoreFileName: TFileName = ''): TFileNameDynArray; overload;

/// search for matching file names from path-delimited content
// - is a wrapper around FindFileNames(MakePath())
function FileNames(const Path: array of const; const Mask: TFileName = FILES_ALL;
  Options: TFindFilesOptions = []): TFileNameDynArray; overload;

/// convert a result list, as returned by FindFiles(), into an array of Files[].Name
function FindFilesDynArrayToFileNames(const Files: TFindFilesDynArray): TFileNameDynArray;

/// sort a FindFiles() result list by its TFindFiles[].Timestamp field
procedure FindFilesSortByTimestamp(var Files: TFindFilesDynArray);

type
  /// one optional feature of SynchFolders()
  // - process recursively nested folders if sfoSubFolder is included
  // - use file content instead of file date check if sfoByContent is included
  // - display synched file name on console if sfoWriteFileNameToConsole is included
  TSynchFoldersOption = (
    sfoSubFolder,
    sfoByContent,
    sfoWriteFileNameToConsole);
  /// the optional features of SynchFolders()
  TSynchFoldersOptions = set of TSynchFoldersOption;

/// ensure all files in Dest folder(s) do match the one in Reference
// - won't copy all files from Reference folders, but will update files already
// existing in Dest, which did change since last synchronization
// - file copy will use in-memory loading, so won't work well with huge files
// - returns the number of files copied during the process
function SynchFolders(const Reference, Dest: TFileName;
  Options: TSynchFoldersOptions = []): integer;

/// copy all files from a source folder to a destination folder
// - will copy only new or changed files, keeping existing identical files
// - file copy will use stream loading, so would cope with huge files
// - returns the number of fields copied during the process, -1 on error
function CopyFolder(const Source, Dest: TFileName;
  Options: TSynchFoldersOptions = []): integer;


{ ****************** ScanUtf8, GLOB and SOUNDEX Text Search }

/// read and store text into values[] according to fmt specifiers
// - %d as PInteger, %D as PInt64, %u as PCardinal, %U as PQWord, %f as PDouble,
// %F as PCurrency, %x as 8 hexa chars to PInteger, %X as 16 hexa chars to PInt64,
// %s as PShortString (UTF-8 encoded), %S as PRawUtf8, %L as PRawUtf8 (getting
// all text until the end of the line)
// - optionally, specifiers and any whitespace separated identifiers may be
// extracted and stored into the ident[] array, e.g. '%dFirstInt %s %DOneInt64'
// will store ['dFirstInt','s','DOneInt64'] into ident[] dynamic array
function ScanUtf8(const text, fmt: RawUtf8; const values: array of pointer;
  ident: PRawUtf8DynArray = nil): integer; overload;

/// read text from P/PLen and store it into values[] according to fmt specifiers
function ScanUtf8(P: PUtf8Char; PLen: PtrInt; const fmt: RawUtf8;
  const values: array of pointer; ident: PRawUtf8DynArray): integer; overload;


type
  PMatch = ^TMatch;

  // used when inlining TMatch.Match
  TMatchSearchFunction = function(aMatch: PMatch;
    aText: PUtf8Char; aTextLen: PtrInt): boolean;

  /// low-level structure used by IsMatch() for actual GLOB search
  // - you can use this object to prepare a given pattern, e.g. in a loop
  // - implemented as a fast brute-force state-machine without any heap allocation
  // - some common patterns ('exactmatch', 'startwith*', '*endwith', '*contained*')
  // are handled with dedicated code, optionally with case-insensitive search
  // - PrepareContains() is the most efficient method for '*contained*' search
  // - consider using TMatchs (or SetMatchs/TMatchDynArray) if you expect to
  // search for several patterns, or even TExprParserMatch for expression search
  {$ifdef USERECORDWITHMETHODS}
  TMatch = record
  {$else}
  TMatch = object
  {$endif USERECORDWITHMETHODS}
  private
    Pattern, Text: PUtf8Char;
    P, T, PMax, TMax: PtrInt;
    Upper: PNormTable;
    State: (sNONE, sABORT, sEND, sLITERAL, sPATTERN, sRANGE, sVALID);
    procedure MatchAfterStar;
    procedure MatchMain;
  public
    /// published for proper inlining
    Search: TMatchSearchFunction;
    /// initialize the internal fields for a given glob search pattern
    // - note that the aPattern instance should remain in memory, since it will
    // be pointed to by the PatternText private field of this object
    procedure Prepare(const aPattern: RawUtf8;
      aCaseInsensitive, aReuse: boolean); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize the internal fields for a given glob search pattern
    // - note that the aPattern buffer should remain in memory, since it will
    // be pointed to by the PatternText private field of this object
    procedure Prepare(aPattern: PUtf8Char; aPatternLen: integer;
      aCaseInsensitive, aReuse: boolean); overload;
    /// initialize low-level internal fields for'*aPattern*' search
    // - this method is faster than a regular Prepare('*' + aPattern + '*'),
    // since it may use the SBNDMQ2 algorithm for patterns of length 2..31
    // - warning: the supplied aPattern variable may be modified in-place to be
    // filled with some lookup buffer, when SBNDMQ2 is triggered
    procedure PrepareContains(var aPattern: RawUtf8;
      aCaseInsensitive: boolean); overload;
    /// initialize low-level internal fields for a custom search algorithm
    procedure PrepareRaw(aPattern: PUtf8Char; aPatternLen: integer;
      aSearch: TMatchSearchFunction);
    /// returns TRUE if the supplied content matches the prepared glob pattern
    // - this method is not thread-safe
    function Match(const aText: RawUtf8): boolean; overload;
      {$ifdef FPC} inline;{$endif}
    /// returns TRUE if the supplied content matches the prepared glob pattern
    // - this method is not thread-safe
    function Match(aText: PUtf8Char; aTextLen: PtrInt): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns TRUE if the supplied content matches the prepared glob pattern
    // - this method IS thread-safe, and won't lock
    function MatchThreadSafe(const aText: RawUtf8): boolean;
    /// returns TRUE if the supplied VCL/LCL content matches the prepared glob pattern
    // - this method IS thread-safe, will use on-stack UTF-8 temporary conversion
    // if possible, and won't lock
    function MatchString(const aText: string): boolean;
    /// returns TRUE if this search pattern matches another
    function Equals(const aAnother: TMatch): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// access to the pattern length as stored in PMax + 1
    function PatternLength: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// access to the pattern text as stored in Pattern
    function PatternText: PUtf8Char;
      {$ifdef HASINLINE}inline;{$endif}
    /// check if the pattern search was defined as case-insensitive
    function CaseInsensitive: boolean;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// stores an array of GLOB search engines
  // - use SetMatchs() to initialize such an array from a CSV pattern text
  TMatchDynArray = array of TMatch;

  /// TMatch descendant owning a copy of the Pattern string to avoid GPF issues
  TMatchStore = record
    /// access to the research criteria
    // - defined as a nested record (and not an object) to circumvent Delphi bug
    Pattern: TMatch;
    /// Pattern.Pattern PUtf8Char will point to this instance
    PatternInstance: RawUtf8;
  end;

  TMatchStoreDynArray = array of TMatchStore;

  /// stores several TMatch instances, from a set of glob patterns
  TMatchs = class(TSynPersistent)
  protected
    fMatch: TMatchStoreDynArray;
    fMatchCount: integer;
  public
    /// add once some glob patterns to the internal TMach list
    // - aPatterns[] follows the IsMatch() syntax
    constructor Create(const aPatterns: TRawUtf8DynArray;
      CaseInsensitive: boolean); reintroduce; overload;
    /// add once some glob patterns to the internal TMach list
    // - aPatterns[] follows the IsMatch() syntax
    procedure Subscribe(const aPatterns: TRawUtf8DynArray;
      CaseInsensitive: boolean); overload; virtual;
    /// add once some glob patterns to the internal TMach list
    // - each CSV item in aPatterns follows the IsMatch() syntax
    procedure Subscribe(const aPatternsCsv: RawUtf8;
      CaseInsensitive: boolean); overload;
    /// search patterns in the supplied UTF-8 text
    // - returns -1 if no filter has been subscribed
    // - returns -2 if there is no match on any previous pattern subscription
    // - returns fMatch[] index, i.e. >= 0 number on first matching pattern
    // - this method is thread-safe
    function Match(const aText: RawUtf8): integer; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// search patterns in the supplied UTF-8 text buffer
    function Match(aText: PUtf8Char; aLen: integer): integer; overload;
    /// search patterns in the supplied RTL string text
    // - could be used on a TFileName for instance
    // - will avoid any memory allocation if aText is small enough
    function MatchString(const aText: string): integer;
  end;

  /// store a decoded URI as full path and file/resource name
  {$ifdef USERECORDWITHMETHODS}
  TUriMatchName = record
  {$else}
  TUriMatchName = object
  {$endif USERECORDWITHMETHODS}
  public
    Path, Name: TValuePUtf8Char;
    /// to be called once Path has been populated to compute Name
    procedure ParsePath;
  end;

  /// efficient GLOB path or resource name lockup for an URI
  // - using mORMot fast TMatch engine
  {$ifdef USERECORDWITHMETHODS}
  TUriMatch = record
  {$else}
  TUriMatch = object
  {$endif USERECORDWITHMETHODS}
  private
    Init: TLightLock;
    Names, Paths: TMatchDynArray;
    procedure DoInit(csv: PUtf8Char; caseinsensitive: boolean);
  public
    /// main entry point of the GLOB resource/path URI pattern matching
    // - will thread-safe initialize the internal TMatch instances if necessary
    function Check(const csv: RawUtf8; const uri: TUriMatchName;
      caseinsensitive: boolean): boolean;
  end;



/// fill the Match[] dynamic array with all glob patterns supplied as CSV
// - returns how many patterns have been set in Match[|]
// - note that the CsvPattern instance should remain in memory, since it will
// be pointed to by the Match[].Pattern private field
function SetMatchs(const CsvPattern: RawUtf8; CaseInsensitive: boolean;
  out Match: TMatchDynArray; CsvSep: AnsiChar = ','): integer; overload;

/// fill the Match[0..MatchMax] static array with all glob patterns supplied as CSV
// - note that the CsvPattern instance should remain in memory, since it will
// be pointed to by the Match[].Pattern private field
function SetMatchs(CsvPattern: PUtf8Char; CaseInsensitive: boolean;
  Match: PMatch; MatchMax: integer; CsvSep: AnsiChar = ','): integer; overload;

/// fill a TMatch instance with the next glob pattern supplied as CSV
function SetNextMatch(P: PUtf8Char; var Dest: TMatch;
  CaseInsensitive, Reuse: boolean; CsvSep: AnsiChar): PUtf8Char;

/// search if one TMach is already registered in the Several[] dynamic array
function MatchExists(const One: TMatch; const Several: TMatchDynArray): boolean;

/// add one TMach if not already registered in the Several[] dynamic array
function MatchAdd(const One: TMatch; var Several: TMatchDynArray): boolean;

/// allocate one TMach in the Several[] dynamic array
function MatchNew(var Several: TMatchDynArray): PMatch;

/// returns TRUE if Match=nil or if any Match[].Match(Text) is TRUE
function MatchAny(const Match: TMatchDynArray; const Text: RawUtf8): boolean; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// returns TRUE if Match=nil or if any Match[].Match(Text, TextLen) is TRUE
function MatchAny(Match: PMatch; Text: PUtf8Char; TextLen: PtrInt): boolean; overload;

/// apply the CSV-supplied glob patterns to an array of RawUtf8
// - any text not matching the pattern will be deleted from the array
// - the patterns are specified as CSV, separated by ','
procedure FilterMatchs(const CsvPattern: RawUtf8; CaseInsensitive: boolean;
  var Values: TRawUtf8DynArray; CsvSep: AnsiChar = ','); overload;

/// apply the CSV-supplied glob patterns to an array of string
// - any text not matching the pattern will be deleted from the array
// - the patterns are specified as CSV, separated by ','
procedure FilterMatchs(const CsvPattern: RawUtf8; CaseInsensitive: boolean;
  var Values: TStringDynArray; CsvSep: AnsiChar = ','); overload;

/// return TRUE if the supplied content matches a glob pattern
// - ?  Matches any single character
// - *	Matches any contiguous characters
// - [abc]  Matches a or b or c at that position
// - [^abc]	Matches anything but a or b or c at that position
// - [!abc]	Matches anything but a or b or c at that position
// - [a-e]  Matches a through e at that position
// - [abcx-z]  Matches a or b or c or x or y or or z, as does [a-cx-z]
// - 'ma?ch.*'	would match match.exe, mavch.dat, march.on, etc..
// - 'this [e-n]s a [!zy]est' would match 'this is a test', but would not
// match 'this as a test' nor 'this is a zest'
// - consider using TMatch or TMatchs if you expect to reuse the pattern
function IsMatch(const Pattern, Text: RawUtf8;
  CaseInsensitive: boolean = false): boolean;

/// return TRUE if the supplied content matches a glob pattern, using RTL strings
// - is a wrapper around IsMatch() with fast UTF-8 conversion
function IsMatchString(const Pattern, Text: string;
  CaseInsensitive: boolean = false): boolean;

/// return TRUE if the supplied content matches one or several glob patterns
// - the patterns are specified as CSV, separated by ','
function IsMatchs(const CsvPattern, Text: RawUtf8;
  CaseInsensitive: boolean = false; CsvSep: AnsiChar = ','): boolean; overload;

/// return TRUE if the supplied content matches one or several glob patterns
// - the patterns are specified as CSV, separated by ','
function IsMatchs(CsvPattern, Text: PUtf8Char; TextLen: PtrInt;
  CaseInsensitive: boolean = false; CsvSep: AnsiChar = ','): boolean; overload;

type
  /// available pronunciations for our fast Soundex implementation
  TSynSoundExPronunciation = (
    sndxEnglish,
    sndxFrench,
    sndxSpanish,
    sndxNone);

  TSoundExValues = array[0..ord('Z') - ord('B')] of byte;

  PSoundExValues = ^TSoundExValues;
  PSynSoundEx = ^TSynSoundEx;

  /// fast search of a text value, using the Soundex approximation mechanism
  // - Soundex is a phonetic algorithm for indexing names by sound,
  //  as pronounced in a given language. The goal is for homophones to be
  //  encoded to the same representation so that they can be matched despite
  //  minor differences in spelling
  // - this implementation is very fast and can be used e.g. to parse and search
  //  in a huge text buffer
  // - this version also handles french and spanish pronunciations on request,
  //  which differs from default Soundex, i.e. English
  {$ifdef USERECORDWITHMETHODS}
  TSynSoundEx = record
  {$else}
  TSynSoundEx = object
  {$endif USERECORDWITHMETHODS}
  private
    Search, FirstChar: cardinal;
    fValues: PSoundExValues;
  public
    /// prepare for a Soundex search
    // - you can specify another language pronunciation than default english
    function Prepare(UpperValue: PAnsiChar;
      Lang: TSynSoundExPronunciation = sndxEnglish): boolean; overload;
    /// prepare for a custom Soundex search
    // - you can specify any language pronunciation from raw TSoundExValues array
    function Prepare(UpperValue: PAnsiChar; Lang: PSoundExValues): boolean; overload;
    /// return true if prepared value is contained in a text buffer
    // (UTF-8 encoded), by using the SoundEx comparison algorithm
    // - search prepared value at every word beginning in U^
    function Utf8(U: PUtf8Char): boolean;
    /// return true if prepared value is contained in a ANSI text buffer
    // by using the SoundEx comparison algorithm
    // - search prepared value at every word beginning in A^
    function Ansi(A: PAnsiChar): boolean;
  end;


/// Retrieve the Soundex value of a text word, from Ansi buffer
// - Return the soundex value as an easy to use cardinal value, 0 if the
// incoming string contains no valid word
// - if next is defined, its value is set to the end of the encoded word
// (so that you can call again this function to encode a full sentence)
function SoundExAnsi(A: PAnsiChar; next: PPAnsiChar = nil;
  Lang: TSynSoundExPronunciation = sndxEnglish): cardinal; overload;

/// Retrieve the Soundex value of a text word, from Ansi buffer
// - Return the soundex value as an easy to use cardinal value, 0 if the
// incoming string contains no valid word
// - if next is defined, its value is set to the end of the encoded word
// (so that you can call again this function to encode a full sentence)
function SoundExAnsi(A: PAnsiChar; next: PPAnsiChar;
  Lang: PSoundExValues): cardinal; overload;

/// Retrieve the Soundex value of a text word, from UTF-8 buffer
// - Return the soundex value as an easy to use cardinal value, 0 if the
// incoming string contains no valid word
// - if next is defined, its value is set to the end of the encoded word
// (so that you can call again this function to encode a full sentence)
// - very fast: all UTF-8 decoding is handled on the fly
function SoundExUtf8(U: PUtf8Char; next: PPUtf8Char = nil;
  Lang: TSynSoundExPronunciation = sndxEnglish): cardinal;

const
  /// number of bits to use for each interesting soundex char
  // - default is to use 8-bit, i.e. 4 soundex chars, which is the
  // standard approach
  // - for a more detailed soundex, use 4 bits resolution, which will
  // compute up to 7 soundex chars in a cardinal (that's our choice)
  SOUNDEX_BITS = 4;


{ ******************  Efficient CSV Parsing using RTTI }

/// parse a CSV buffer into a TDynArray of records using its RTTI fields
// - TypeInfo should have proper fields description, e.g. from Delphi 2010
// extended RTTI or mormot.core.rtti.pas' Rtti.RegisterFromText()
// - first CSV line has headers matching the needed case-insensitive field names
// - following CSV lines will be read and parsed into the dynamic array records
// - any unknown header name within the RTTI fields will be ignored
// - you can optionally intern all RawUtf8 values to reduce memory consumption
function TDynArrayLoadCsv(var Value: TDynArray; Csv: PUtf8Char;
  Intern: TRawUtf8Interning = nil): boolean;

/// parse a CSV UTF-8 string into a dynamic array of records using its RTTI fields
// - just a wrapper around DynArrayLoadCsv() with a temporary TDynArray
function DynArrayLoadCsv(var Value; const Csv: RawUtf8; TypeInfo: PRttiInfo;
  Intern: TRawUtf8Interning = nil): boolean;


{ ****************** Versatile Expression Search Engine }

type
  /// exception type used by TExprParser
  EExprParser = class(ESynException);

  /// identify an expression search engine node type, as used by TExprParser
  TExprNodeType = (
    entWord,
    entNot,
    entOr,
    entAnd);

  /// results returned by TExprParserAbstract.Parse method
  TExprParserResult = (
    eprSuccess,
    eprNoExpression,
    eprMissingParenthesis,
    eprTooManyParenthesis,
    eprMissingFinalWord,
    eprInvalidExpression,
    eprUnknownVariable,
    eprUnsupportedOperator,
    eprInvalidConstantOrVariable);

  TParserAbstract = class;

  /// stores an expression search engine node, as used by TExprParser
  TExprNode = class(TSynPersistent)
  protected
    fNext: TExprNode;
    fNodeType: TExprNodeType;
    function Append(node: TExprNode): boolean;
  public
    /// initialize a node for the search engine
    constructor Create(nodeType: TExprNodeType); reintroduce;
    /// recursively destroys the linked list of nodes (i.e. Next)
    destructor Destroy; override;
    /// browse all nodes until Next = nil
    function Last: TExprNode;
    /// points to the next node in the parsed tree
    property Next: TExprNode
      read fNext;
    /// what is actually stored in this node
    property NodeType: TExprNodeType
      read fNodeType;
  end;

  /// abstract class to handle word search, as used by TExprParser
  TExprNodeWordAbstract = class(TExprNode)
  protected
    fOwner: TParserAbstract;
    fWord: RawUtf8;
    /// should be set from actual data before TExprParser.Found is called
    fFound: boolean;
    function ParseWord: TExprParserResult; virtual; abstract;
  public
    /// you should override this virtual constructor for proper initialization
    constructor Create(aOwner: TParserAbstract; const aWord: RawUtf8); reintroduce; virtual;
  end;

  /// class-reference type (metaclass) for a TExprNode
  // - allow to customize the actual searching process for entWord
  TExprNodeWordClass = class of TExprNodeWordAbstract;

  /// parent class of TExprParserAbstract
  TParserAbstract = class(TSynPersistent)
  protected
    fExpression, fCurrentWord, fAndWord, fOrWord, fNotWord: RawUtf8;
    fCurrent: PUtf8Char;
    fCurrentError: TExprParserResult;
    fFirstNode: TExprNode;
    fWordClass: TExprNodeWordClass;
    fWords: array of TExprNodeWordAbstract;
    fWordCount: integer;
    fNoWordIsAnd: boolean;
    fFoundStack: array[byte] of boolean; // simple stack-based virtual machine
    procedure ParseNextCurrentWord; virtual; abstract;
    function ParseExpr: TExprNode;
    function ParseFactor: TExprNode;
    function ParseTerm: TExprNode;
    procedure Clear; virtual;
    // override this method to initialize fWordClass and fAnd/Or/NotWord
    procedure Initialize; virtual; abstract;
    /// perform the expression search over TExprNodeWord.fFound flags
    // - warning: caller should check that fFirstNode<>nil (e.g. WordCount>0)
    function Execute: boolean;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// initialize an expression parser
    constructor Create; override;
    /// finalize the expression parser
    destructor Destroy; override;
    /// initialize the parser from a given text expression
    function Parse(const aExpression: RawUtf8): TExprParserResult;
    /// try this parser class on a given text expression
    // - returns '' on success, or an explicit error message (e.g.
    // 'Missing parenthesis')
    class function ParseError(const aExpression: RawUtf8): RawUtf8;
    /// the associated text expression used to define the search
    property Expression: RawUtf8
      read fExpression;
    /// how many words did appear in the search expression
    property WordCount: integer
      read fWordCount;
  end;

  /// abstract class to parse a text expression into nodes
  // - you should inherit this class to provide actual text search
  // - searched expressions can use parenthesis and &=AND -=WITHOUT +=OR operators,
  // e.g. '((w1 & w2) - w3) + w4' means ((w1 and w2) without w3) or w4
  // - no operator is handled like a AND, e.g. 'w1 w2' = 'w1 & w2'
  TExprParserAbstract = class(TParserAbstract)
  protected
    procedure ParseNextCurrentWord; override;
    // may be overridden to provide custom words escaping (e.g. handle quotes)
    procedure ParseNextWord; virtual;
    procedure Initialize; override;
  end;

  /// search expression engine using TMatch for the actual word searches
  TExprParserMatch = class(TExprParserAbstract)
  protected
    fCaseSensitive: boolean;
    fMatchedLastSet: integer;
    procedure Initialize; override;
  public
    /// initialize the search engine
    constructor Create(aCaseSensitive: boolean = true); reintroduce;
    /// returns TRUE if the expression is within the text buffer
    function Search(aText: PUtf8Char; aTextLen: PtrInt): boolean; overload;
    /// returns TRUE if the expression is within the text buffer
    function Search(const aText: RawUtf8): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
  end;


const
  /// may be used when overriding TExprParserAbstract.ParseNextWord method
  PARSER_STOPCHAR = ['&', '+', '-', '(', ')'];

function ToText(r: TExprParserResult): PShortString; overload;
function ToUtf8(r: TExprParserResult): RawUtf8; overload;



{ ****************** Bloom Filter Probabilistic Index }

type
  /// implements a thread-safe Bloom Filter storage
  // - a "Bloom Filter" is a space-efficient probabilistic data structure,
  // that is used to test whether an element is a member of a set. False positive
  // matches are possible, but false negatives are not. Elements can be added to
  // the set, but not removed. Typical use cases are to avoid unnecessary
  // slow disk or network access if possible, when a lot of items are involved.
  // - memory use is very low, when compared to storage of all values: fewer
  // than 10 bits per element are required for a 1% false positive probability,
  // independent of the size or number of elements in the set - for instance,
  // storing 10,000,000 items presence with 1% of false positive ratio
  // would consume only 11.5 MB of memory, using 7 hash functions
  // - use Insert() methods to add an item to the internal bits array, and
  // Reset() to clear all bits array, if needed
  // - MayExist() function would check if the supplied item was probably set
  // - SaveTo() and LoadFrom() methods allow transmission of the bits array,
  // for a disk/database storage or transmission over a network
  // - internally, several (hardware-accelerated) crc32c hash functions will be
  // used, with some random seed values, to simulate several hashing functions;
  // you can customize the hash function if needed
  // - all methods are thread-safe, and MayExist can be concurrent (via a TRWLock)
  TSynBloomFilter = class(TSynPersistent)
  private
    fSafe: TRWLock; // need an upgradable lock for TSynBloomFilterDiff
    fHasher: THasher;
    fSize: cardinal;
    fBits: cardinal;
    fHashFunctions: cardinal;
    fInserted: cardinal;
    fFalsePositivePercent: double;
    fStore: RawByteString;
  public
    /// don't call this raw constructor, but its overloads
    constructor Create; overload; override;
    /// initialize the internal bits storage for a given number of items
    // - by default, internal bits array size will be guess from a 1 % false
    // positive rate - but you may specify another value, to reduce memory use
    // - this constructor would compute and initialize Bits and HashFunctions
    // corresponding to the expected false positive ratio
    // - you can specify a custom hash function if you find that the default
    // crc32c() has too many collisions: but SaveTo/LoadFrom will be tied to it;
    // see e.g. CryptCrc32(caMd5/caSha1) from mormot.crypt.secure
    constructor Create(aSize: integer; aFalsePositivePercent: double = 1;
      aHasher: THasher = nil); reintroduce; overload;
    /// initialize the internal bits storage from a SaveTo() binary buffer
    // - this constructor will initialize the internal bits array calling LoadFrom()
    // - you can specify a custom hash function to match with the one used before
    constructor Create(const aSaved: RawByteString; aMagic: cardinal = $B1003F11;
      aHasher: THasher = nil); reintroduce; overload;
    /// add an item in the internal bits array storage
    // - this method is thread-safe
    procedure Insert(const aValue: RawByteString); overload;
    /// add an item in the internal bits array storage
    // - this method is thread-safe
    procedure Insert(aValue: pointer; aValueLen: integer); overload; virtual;
    /// clear the internal bits array storage
    // - you may call this method after some time, if some items may have
    // been removed, to reduce false positives
    // - this method is thread-safe
    procedure Reset; virtual;
    /// returns TRUE if the supplied items was probably set via Insert()
    // - some false positive may occur, but not much than FalsePositivePercent
    // - this method is thread-safe, and allow concurrent calls (via a TRWLock)
    function MayExist(const aValue: RawByteString): boolean; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// returns TRUE if the supplied items was probably set via Insert()
    // - some false positive may occur, but not much than FalsePositivePercent
    // - this method is thread-safe, and allow concurrent calls (via a TRWLock)
    function MayExist(aValue: pointer; aValueLen: integer): boolean; overload;
    /// store the internal bits array into a binary buffer
    // - may be used to transmit or store the state of a dataset, avoiding
    // to recompute all Insert() at program startup, or to synchronize
    // networks nodes information and reduce the number of remote requests
    // - this method is thread-safe, and won't block MayExist (via a TRWLock)
    function SaveTo(aMagic: cardinal = $B1003F11): RawByteString; overload;
    /// store the internal bits array into a binary buffer
    // - may be used to transmit or store the state of a dataset, avoiding
    // to recompute all Insert() at program startup, or to synchronize
    // networks nodes information and reduce the number of remote requests
    // - this method is thread-safe, and won't block MayExist (via a TRWLock)
    procedure SaveTo(aDest: TBufferWriter;
      aMagic: cardinal = $B1003F11); overload;
    /// read the internal bits array from a binary buffer
    // - as previously serialized by the SaveTo method
    // - may be used to transmit or store the state of a dataset
    function LoadFrom(const aSaved: RawByteString;
      aMagic: cardinal = $B1003F11): boolean; overload;
    /// read the internal bits array from a binary buffer
    // - as previously serialized by the SaveTo method
    // - may be used to transmit or store the state of a dataset
    function LoadFrom(P: PByte; PLen: integer;
      aMagic: cardinal = $B1003F11): boolean; overload; virtual;
  published
    /// maximum number of items which are expected to be inserted
    property Size: cardinal
      read fSize;
    /// expected percentage (1..100) of false positive results for MayExists()
    property FalsePositivePercent: double
      read fFalsePositivePercent;
    /// number of bits stored in the internal bits array
    property Bits: cardinal
      read fBits;
    /// how many hash functions would be applied for each Insert()
    property HashFunctions: cardinal
      read fHashFunctions;
    /// how many times the Insert() method has been called
    property Inserted: cardinal
      read fInserted;
  end;

  /// implements a thread-safe differential Bloom Filter storage
  // - this inherited class is able to compute incremental serialization of
  // its internal bits array, to reduce network use
  // - an obfuscated revision counter is used to identify storage history
  TSynBloomFilterDiff = class(TSynBloomFilter)
  protected
    fRevision: Int64;
    fSnapShotAfterMinutes: cardinal;
    fSnapshotAfterInsertCount: cardinal;
    fSnapshotTimestamp: Int64;
    fSnapshotInsertCount: cardinal;
    fKnownRevision: Int64;
    fKnownStore: RawByteString;
  public
    /// add an item in the internal bits array storage
    // - this overloaded thread-safe method would compute fRevision
    procedure Insert(aValue: pointer; aValueLen: integer); override;
    /// clear the internal bits array storage
    // - this overloaded thread-safe method would reset fRevision
    procedure Reset; override;
    /// store the internal bits array into an incremental binary buffer
    // - here the difference from a previous SaveToDiff revision will be computed
    // - if aKnownRevision is outdated (e.g. if equals 0), the whole bits array
    // would be returned, and around 10 bits per item would be transmitted
    // (for 1% false positive ratio)
    // - incremental retrieval would then return around 10 bytes per newly added
    // item since the last snapshot reference state (with 1% ratio, i.e. 7 hash
    // functions)
    function SaveToDiff(const aKnownRevision: Int64): RawByteString;
    /// use the current internal bits array state as known revision
    // - is done the first time SaveToDiff() is called, then after 1/32th of
    // the filter size has been inserted (see SnapshotAfterInsertCount property),
    // or after SnapShotAfterMinutes property timeout period
    procedure DiffSnapshot;
    /// retrieve the revision number from an incremental binary buffer
    // - returns 0 if the supplied binary buffer does not match this bloom filter
    function DiffKnownRevision(const aDiff: RawByteString): Int64;
    /// read the internal bits array from an incremental binary buffer
    // - as previously serialized by the SaveToDiff() method
    // - may be used to transmit or store the state of a dataset
    // - returns false if the supplied content is incorrect, e.g. if the known
    // revision is deprecated
    function LoadFromDiff(const aDiff: RawByteString): boolean;
    /// the opaque revision number of this internal storage
    // - is in fact the Unix timestamp shifted by 31 bits, and an incremental
    // counter: this pattern will allow consistent IDs over several ServPanels
    property Revision: Int64
      read fRevision;
    /// after how many Insert() the internal bits array storage should be
    // promoted as known revision
    // - equals Size div 32 by default
    property SnapshotAfterInsertCount: cardinal
      read fSnapshotAfterInsertCount write fSnapshotAfterInsertCount;
    /// after how many time the internal bits array storage should be
    // promoted as known revision
    // - equals 30 minutes by default
    property SnapShotAfterMinutes: cardinal
      read fSnapShotAfterMinutes write fSnapShotAfterMinutes;
  end;


/// RLE compression of a memory buffer containing mostly zeros
// - will store the number of consecutive zeros instead of plain zero bytes
// - used for spare bit sets, e.g. TSynBloomFilter serialization
// - will also compute the crc32c of the supplied content
// - use ZeroDecompress() to expand the compressed result
// - resulting content would be at most 14 bytes bigger than the input
// - you may use this function before SynLZ compression
procedure ZeroCompress(P: PAnsiChar; Len: integer; Dest: TBufferWriter);

/// RLE uncompression of a memory buffer containing mostly zeros
// - returns Dest='' if P^ is not a valid ZeroCompress() function result
// - used for spare bit sets, e.g. TSynBloomFilter serialization
// - will also check the crc32c of the supplied content
procedure ZeroDecompress(P: PByte; Len: integer; var Dest: RawByteString);

/// RLE compression of XORed memory buffers resulting in mostly zeros
// - will perform ZeroCompress(Dest^ := New^ xor Old^) without any temporary
// memory allocation
// - is used  e.g. by TSynBloomFilterDiff.SaveToDiff() in incremental mode
// - will also compute the crc32c of the supplied content
procedure ZeroCompressXor(New, Old: PAnsiChar; Len: cardinal;
  Dest: TBufferWriter);

/// RLE uncompression and ORing of a memory buffer containing mostly zeros
// - will perform Dest^ := Dest^ or ZeroDecompress(P^) without any temporary
// memory allocation
// - is used  e.g. by TSynBloomFilterDiff.LoadFromDiff() in incremental mode
// - returns false if P^ is not a valid ZeroCompress/ZeroCompressXor() result
// - will also check the crc32c of the supplied content
function ZeroDecompressOr(P, Dest: PAnsiChar; Len, DestLen: integer): boolean;


{ ****************** Binary Buffers Delta Compression }

const
  /// normal pattern search depth for DeltaCompress()
  // - gives good results on most content
  DELTA_LEVEL_FAST = 100;
  /// brutal pattern search depth for DeltaCompress()
  // - may become very slow, with minor benefit, on huge content
  DELTA_LEVEL_BEST = 500;
  /// 2MB as internal chunks/window default size for DeltaCompress()
  // - will use up to 9 MB of RAM during DeltaCompress() - none in DeltaExtract()
  DELTA_BUF_DEFAULT = 2 shl 20;

/// compute difference of two binary buffers
// - returns '=' for equal buffers, or an optimized binary delta
// - DeltaExtract() could be used later on to compute New from Old + Delta
function DeltaCompress(const New, Old: RawByteString;
  Level: integer = DELTA_LEVEL_FAST;
  BufSize: integer = DELTA_BUF_DEFAULT): RawByteString; overload;

/// compute difference of two binary buffers
// - returns '=' for equal buffers, or an optimized binary delta
// - DeltaExtract() could be used later on to compute New from Old
function DeltaCompress(New, Old: PAnsiChar; NewSize, OldSize: integer;
  Level: integer = DELTA_LEVEL_FAST;
  BufSize: integer = DELTA_BUF_DEFAULT): RawByteString; overload;

/// compute difference of two binary buffers
// - returns '=' for equal buffers, or an optimized binary delta
// - DeltaExtract() could be used later on to compute New from Old + Delta
// - caller should call Freemem(Delta) once finished with the output buffer
function DeltaCompress(New, Old: PAnsiChar; NewSize, OldSize: integer;
  out Delta: PAnsiChar; Level: integer = DELTA_LEVEL_FAST;
  BufSize: integer = DELTA_BUF_DEFAULT): integer; overload;

type
  /// result of function DeltaExtract()
  TDeltaError = (
    dsSuccess,
    dsCrcCopy,
    dsCrcComp,
    dsCrcBegin,
    dsCrcEnd,
    dsCrcExtract,
    dsFlag,
    dsLen);

/// returns how many bytes a DeltaCompress() result will expand to
function DeltaExtractSize(const Delta: RawByteString): integer; overload;

/// returns how many bytes a DeltaCompress() result will expand to
function DeltaExtractSize(Delta: PAnsiChar): integer; overload;

/// apply the delta binary as computed by DeltaCompress()
// - decompression don't use any RAM, will perform crc32c check, and is very fast
// - return dsSuccess if was uncompressed to aOutUpd as expected
function DeltaExtract(const Delta, Old: RawByteString;
  out New: RawByteString): TDeltaError; overload;

/// low-level apply the delta binary as computed by DeltaCompress()
// - New should already be allocated with DeltaExtractSize(Delta) bytes
// - as such, expect Delta, Old and New to be <> nil, and Delta <> '='
// - return dsSuccess if was uncompressed to aOutUpd as expected
function DeltaExtract(Delta, Old, New: PAnsiChar): TDeltaError; overload;

function ToText(err: TDeltaError): PShortString; overload;


{ ****************** TDynArray Low-Level Binary Search and Iteration }

/// wrap a simple dynamic array BLOB content as stored by TDynArray.SaveTo
// - a "simple" dynamic array contains data with no reference count, e.g. byte,
// word, integer, cardinal, Int64, double or Currency
// - same as TDynArray.LoadFrom() with no memory allocation nor memory copy: so
// is much faster than creating a temporary dynamic array to load the data
// - will return nil if no or invalid data, or a pointer to the data
// array otherwise, with the items number stored in Count and the individual
// element size in ElemSize (e.g. 2 for a TWordDynArray)
// - note: mORMot 1.18 Hash32 is not stored any more
function SimpleDynArrayLoadFrom(Source: PAnsiChar; aTypeInfo: PRttiInfo;
  out Count, ElemSize: PtrInt): pointer;

/// wrap an integer dynamic array BLOB content as stored by TDynArray.SaveTo
// - same as TDynArray.LoadFrom() with no memory allocation nor memory copy: so
// is much faster than creating a temporary dynamic array to load the data
// - will return nil if no or invalid data, or a pointer to the integer
// array otherwise, with the items number stored in Count
// - slightly faster than SimpleDynArrayLoadFrom(Source,TypeInfo(TIntegerDynArray),Count)
function IntegerDynArrayLoadFrom(Source: PAnsiChar; var Count: integer): PIntegerArray;

/// search in a RawUtf8 dynamic array BLOB content as stored by TDynArray.SaveTo
// - same as search within TDynArray.LoadFrom() with no memory allocation nor
// memory copy: so is much faster
// - will return -1 if no match or invalid data, or the matched entry index
function RawUtf8DynArrayLoadFromContains(Source: PAnsiChar;
  Value: PUtf8Char; ValueLen: PtrInt; CaseSensitive: boolean): PtrInt;

type
  /// allows to iterate over a TDynArray.SaveTo binary buffer
  // - may be used as alternative to TDynArray.LoadFrom, if you don't want
  // to allocate all items at once, but retrieve items one by one
  {$ifdef USERECORDWITHMETHODS}
  TDynArrayLoadFrom = record
  {$else}
  TDynArrayLoadFrom = object
  {$endif USERECORDWITHMETHODS}
  private
    ArrayLoad: TRttiBinaryLoad;
  public
    /// how many items were saved in the TDynArray.SaveTo binary buffer
    // - equals -1 if Init() failed to deserialize its header
    Count: integer;
    /// the zero-based index of the current item pointed by next Step() call
    // - is in range 0..Count-1 until Step() returns false
    Current: integer;
    /// current read position in the TDynArray.SaveTo binary buffer
    // - after Step() returned false, points just after the binary buffer,
    // like a regular TDynArray.LoadFrom
    Reader: TFastReader;
    /// RTTI information of the deserialized dynamic array
    ArrayRtti: TRttiCustom;
    /// initialize iteration over a TDynArray.SaveTo binary buffer
    // - returns true on success, with Count and Position being set
    // - returns false if the supplied binary buffer is not correct
    // - you should specify SourceMaxLen to avoid any buffer overflow
    function Init(ArrayTypeInfo: PRttiInfo; Source: PAnsiChar;
      SourceMaxLen: PtrInt): boolean; overload;
    /// initialize iteration over a TDynArray.SaveTo binary buffer
    // - returns true on success, with Count and Position being set
    // - returns false if the supplied binary buffer is not correct
    function Init(ArrayTypeInfo: PRttiInfo;
      const Source: RawByteString): boolean; overload;
    /// iterate over the current stored item
    // - Item should point to a variable of the exact item type stored in this
    // dynamic array
    // - returns true if Item was filled with one value, or false if all
    // items were read, and Position contains the end of the binary buffer
    function Step(Item: pointer): boolean;
    /// extract the first field value of the current stored item
    // - this function won't increase the internal Current pointer
    // - returns true if Field was filled with one value, or false if all
    // items were read, and Position contains the end of the binary buffer
    // - Field is expected to be of ArrayRtti.ArrayFirstField type
    // - could be called before Step(), to pre-allocate a new item instance,
    // or update an existing instance
    function FirstField(Field: pointer): boolean;
  end;


{ ****************** TSynFilter and TSynValidate Processing Classes }

type
  TSynFilterOrValidate = class;

  TSynFilterOrValidateObjArray = array of TSynFilterOrValidate;
  TSynFilterOrValidateObjArrayArray = array of TSynFilterOrValidateObjArray;

  /// will define a filter (transformation) or a validation process to be
  // applied to a database Record content (typically a TOrm)
  // - the optional associated parameters are to be supplied JSON-encoded
  TSynFilterOrValidate = class
  protected
    fParameters: RawUtf8;
    /// children must override this method in order to parse the JSON-encoded
    // parameters, and store it in protected field values
    procedure SetParameters(const Value: RawUtf8); virtual;
  public
    /// add the filter or validation process to a list, checking if not present
    // - if an instance with the same class type and parameters is already
    // registered, will call aInstance.Free and return the exising instance
    // - if there is no similar instance, will add it to the list and return it
    function AddOnce(var aObjArray: TSynFilterOrValidateObjArray;
      aFreeIfAlreadyThere: boolean = true): TSynFilterOrValidate;
  public
    /// initialize the filter (transformation) or validation instance
    // - most of the time, optional parameters may be specified as JSON,
    // possibly with the extended MongoDB syntax
    constructor Create(const aParameters: RawUtf8 = ''); overload; virtual;
    /// initialize the filter or validation instance
    /// - this overloaded constructor will allow to easily set the parameters
    constructor CreateUtf8(const Format: RawUtf8; const Args, Params: array of const); overload;
    /// the optional associated parameters, supplied as JSON-encoded
    property Parameters: RawUtf8
      read fParameters write SetParameters;
  end;

  /// will define a validation to be applied to a Record (typically a TOrm)
  // field content
  // - a typical usage is to validate an email or IP address e.g.
  // - the optional associated parameters are to be supplied JSON-encoded
  TSynValidate = class(TSynFilterOrValidate)
  public
    /// perform the validation action to the specified value
    // - the value is expected by be UTF-8 text, as generated by
    // TPropInfo.GetValue e.g.
    // - if the validation failed, must return FALSE and put some message in
    // ErrorMsg (translated into the current language: you could e.g. use
    // a resourcestring and a SysUtils.Format() call for automatic translation
    // via the mORMoti18n unit - you can leave ErrorMsg='' to trigger a
    // generic error message from clas name ('"Validate email" rule failed'
    // for TSynValidateEmail class e.g.)
    // - if the validation passed, will return TRUE
    function Process(aFieldIndex: integer; const Value: RawUtf8; var ErrorMsg: string): boolean;
      virtual; abstract;
  end;

  /// points to a TSynValidate variable
  // - used e.g. as optional parameter to TOrm.Validate/FilterAndValidate
  PSynValidate = ^TSynValidate;

  /// IP v4 address validation to be applied to a Record field content
  // (typically a TOrm)
  // - this versions expect no parameter
  TSynValidateIPAddress = class(TSynValidate)
  protected
  public
    /// perform the IP Address validation action to the specified value
    function Process(aFieldIndex: integer; const Value: RawUtf8;
      var ErrorMsg: string): boolean; override;
  end;

  /// IP address validation to be applied to a Record field content
  // (typically a TOrm)
  // - optional JSON encoded parameters are "AllowedTLD" or "ForbiddenTLD",
  // expecting a CSV lis of Top-Level-Domain (TLD) names, e.g.
  // $ '{"AllowedTLD":"com,org,net","ForbiddenTLD":"fr"}'
  // $ '{AnyTLD:true,ForbiddenDomains:"mailinator.com,yopmail.com"}'
  // - this will process a validation according to RFC 822 (calling the
  // IsValidEmail() function) then will check for the TLD to be in one of
  // the Top-Level domains ('.com' and such) or a two-char country, and
  // then will check the TLD according to AllowedTLD and ForbiddenTLD
  TSynValidateEmail = class(TSynValidate)
  private
    fAllowedTLD: RawUtf8;
    fForbiddenTLD: RawUtf8;
    fForbiddenDomains: RawUtf8;
    fAnyTLD: boolean;
  protected
    /// decode all published properties from their JSON representation
    procedure SetParameters(const Value: RawUtf8); override;
  public
    /// perform the Email Address validation action to the specified value
    // - call IsValidEmail() function and check for the supplied TLD
    function Process(aFieldIndex: integer; const Value: RawUtf8; var ErrorMsg: string): boolean; override;
    /// allow any TLD to be allowed, even if not a generic TLD (.com,.net ...)
    // - this may be mandatory since already over 1,300 new gTLD names or
    // "strings" could become available in the next few years: there is a
    // growing list of new gTLDs available at
    // @http://newgtlds.icann.org/en/program-status/delegated-strings
    // - the only restriction is that it should be ascii characters
    property AnyTLD: boolean
      read fAnyTLD write fAnyTLD;
    /// a CSV list of allowed TLD
    // - if accessed directly, should be set as lower case values
    // - e.g. 'com,org,net'
    property AllowedTLD: RawUtf8
      read fAllowedTLD write fAllowedTLD;
    /// a CSV list of forbidden TLD
    // - if accessed directly, should be set as lower case values
    // - e.g. 'fr'
    property ForbiddenTLD: RawUtf8
      read fForbiddenTLD write fForbiddenTLD;
    /// a CSV list of forbidden domain names
    // - if accessed directly, should be set as lower case values
    // - not only the TLD, but whole domains like 'cracks.ru,hotmail.com' or such
    property ForbiddenDomains: RawUtf8
      read fForbiddenDomains write fForbiddenDomains;
  end;

  /// glob case-sensitive pattern validation of a Record field content
  // - parameter is NOT JSON encoded, but is some basic TMatch glob pattern
  // - ?	   	Matches any single character
  // - *	   	Matches any contiguous characters
  // - [abc]  Matches a or b or c at that position
  // - [^abc]	Matches anything but a or b or c at that position
  // - [!abc]	Matches anything but a or b or c at that position
  // - [a-e]  Matches a through e at that position
  // - [abcx-z] Matches a or b or c or x or y or or z, as does [a-cx-z]
  // - 'ma?ch.*'	would match match.exe, mavch.dat, march.on, etc..
  // - 'this [e-n]s a [!zy]est' would match 'this is a test', but would not
  //   match 'this as a test' nor 'this is a zest'
  // - pattern check IS case sensitive (TSynValidatePatternI is not)
  // - this class is not as complete as PCRE regex for example,
  // but code overhead is very small, and speed good enough in practice
  TSynValidatePattern = class(TSynValidate)
  protected
    fMatch: TMatch;
    procedure SetParameters(const Value: RawUtf8); override;
  public
    /// perform the pattern validation to the specified value
    // - pattern can be e.g. '[0-9][0-9]:[0-9][0-9]:[0-9][0-9]'
    // - this method will implement both TSynValidatePattern and
    // TSynValidatePatternI, checking the current class
    function Process(aFieldIndex: integer; const Value: RawUtf8;
      var ErrorMsg: string): boolean; override;
  end;

  /// glob case-insensitive pattern validation of a text field content
  // (typically a TOrm)
  // - parameter is NOT JSON encoded, but is some basic TMatch glob pattern
  // - same as TSynValidatePattern, but is NOT case sensitive
  TSynValidatePatternI = class(TSynValidatePattern);

  /// text validation to ensure that to any text field would not be ''
  TSynValidateNonVoidText = class(TSynValidate)
  public
    /// perform the non void text validation action to the specified value
    function Process(aFieldIndex: integer; const Value: RawUtf8;
      var ErrorMsg: string): boolean; override;
  end;

  TSynValidateTextProps = array[0..15] of cardinal;

{$M+} // to have existing RTTI for published properties
  /// text validation to be applied to any Record field content
  // - default MinLength value is 1, MaxLength is maxInt: so a blank
  // TSynValidateText.Create('') is the same as TSynValidateNonVoidText
  // - MinAlphaCount, MinDigitCount, MinPunctCount, MinLowerCount and
  // MinUpperCount allow you to specify the minimal count of respectively
  // alphabetical [a-zA-Z], digit [0-9], punctuation [_!;.,/:?%$="#@(){}+-*],
  // lower case or upper case characters
  // - expects optional JSON parameters of the allowed text length range as
  // $ '{"MinLength":5,"MaxLength":10,"MinAlphaCount":1,"MinDigitCount":1,
  // $ "MinPunctCount":1,"MinLowerCount":1,"MinUpperCount":1}
  TSynValidateText = class(TSynValidate)
  private
    /// used to store all associated validation properties by index
    fProps: TSynValidateTextProps;
    fUtf8Length: boolean;
  protected
    /// use sInvalidTextChar resourcestring to create a translated error message
    procedure SetErrorMsg(fPropsIndex, InvalidTextIndex, MainIndex: integer;
      var result: string);
    /// decode "MinLength", "MaxLength", and other parameters into fProps[]
    procedure SetParameters(const Value: RawUtf8); override;
  public
    /// perform the text length validation action to the specified value
    function Process(aFieldIndex: integer; const Value: RawUtf8;
      var ErrorMsg: string): boolean; override;
  published
    /// Minimal length value allowed for the text content
    // - the length is calculated with UTF-16 Unicode codepoints, unless
    // Utf8Length has been set to TRUE so that the UTF-8 byte count is checked
    // - default is 1, i.e. a void text will not pass the validation
    property MinLength: cardinal
      read fProps[0] write fProps[0];
    /// Maximal length value allowed for the text content
    // - the length is calculated with UTF-16 Unicode codepoints, unless
    // Utf8Length has been set to TRUE so that the UTF-8 byte count is checked
    // - default is maxInt, i.e. no maximum length is set
    property MaxLength: cardinal
      read fProps[1] write fProps[1];
    /// Minimal alphabetical character [a-zA-Z] count
    // - default is 0, i.e. no minimum set
    property MinAlphaCount: cardinal
      read fProps[2] write fProps[2];
    /// Maximal alphabetical character [a-zA-Z] count
    // - default is maxInt, i.e. no Maximum set
    property MaxAlphaCount: cardinal
      read fProps[10] write fProps[10];
    /// Minimal digit character [0-9] count
    // - default is 0, i.e. no minimum set
    property MinDigitCount: cardinal
      read fProps[3] write fProps[3];
    /// Maximal digit character [0-9] count
    // - default is maxInt, i.e. no Maximum set
    property MaxDigitCount: cardinal
      read fProps[11] write fProps[11];
    /// Minimal punctuation sign [_!;.,/:?%$="#@(){}+-*] count
    // - default is 0, i.e. no minimum set
    property MinPunctCount: cardinal
      read fProps[4] write fProps[4];
    /// Maximal punctuation sign [_!;.,/:?%$="#@(){}+-*] count
    // - default is maxInt, i.e. no Maximum set
    property MaxPunctCount: cardinal
      read fProps[12] write fProps[12];
    /// Minimal alphabetical lower case character [a-z] count
    // - default is 0, i.e. no minimum set
    property MinLowerCount: cardinal
      read fProps[5] write fProps[5];
    /// Maximal alphabetical lower case character [a-z] count
    // - default is maxInt, i.e. no Maximum set
    property MaxLowerCount: cardinal
      read fProps[13] write fProps[13];
    /// Minimal alphabetical upper case character [A-Z] count
    // - default is 0, i.e. no minimum set
    property MinUpperCount: cardinal
      read fProps[6] write fProps[6];
    /// Maximal alphabetical upper case character [A-Z] count
    // - default is maxInt, i.e. no Maximum set
    property MaxUpperCount: cardinal
      read fProps[14] write fProps[14];
    /// Minimal space count inside the value text
    // - default is 0, i.e. any space number allowed
    property MinSpaceCount: cardinal
      read fProps[7] write fProps[7];
    /// Maximal space count inside the value text
    // - default is maxInt, i.e. any space number allowed
    property MaxSpaceCount: cardinal
      read fProps[15] write fProps[15];
    /// Maximal space count allowed on the Left side
    // - default is maxInt, i.e. any Left space allowed
    property MaxLeftTrimCount: cardinal
      read fProps[8] write fProps[8];
    /// Maximal space count allowed on the Right side
    // - default is maxInt, i.e. any Right space allowed
    property MaxRightTrimCount: cardinal
      read fProps[9] write fProps[9];
    /// defines if lengths parameters expects UTF-8 or UTF-16 codepoints number
    // - with default FALSE, the length is calculated with UTF-16 Unicode
    // codepoints - MaxLength may not match the UCS4 CodePoint, in case of
    // UTF-16 surrogates
    // - you can set this property to TRUE so that the UTF-8 byte count would
    // be used for truncation against the MaxLength parameter
    property Utf8Length: boolean
      read fUtf8Length write fUtf8Length;
  end;
{$M-}

  /// strong password validation for a Record field content (typically a TOrm)
  // - the following parameters are set by default to
  // $ '{"MinLength":5,"MaxLength":20,"MinAlphaCount":1,"MinDigitCount":1,
  // $ "MinPunctCount":1,"MinLowerCount":1,"MinUpperCount":1,"MaxSpaceCount":0}'
  // - you can specify some JSON encoded parameters to change this default
  // values, which will validate the text field only if it contains from 5 to 10
  // characters, with at least one digit, one upper case letter, one lower case
  // letter, and one punctuation sign, with no space allowed inside
  TSynValidatePassWord = class(TSynValidateText)
  protected
    /// set password specific parameters
    procedure SetParameters(const Value: RawUtf8); override;
  end;

  { C++Builder doesn't support array elements as properties (RSP-12595).
    For now, simply exclude the relevant classes from C++Builder. }
  {$NODEFINE TSynValidateTextProps}
  {$NODEFINE TSynValidateText }
  {$NODEFINE TSynValidatePassWord }

  /// will define a transformation to be applied to a Record field content
  // (typically a TOrm)
  // - here "filter" means that content would be transformed according to a
  // set of defined rules
  // - a typical usage is to convert to lower or upper case, or
  // trim any time or date value in a TDateTime field
  // - the optional associated parameters are to be supplied JSON-encoded
  TSynFilter = class(TSynFilterOrValidate)
  protected
  public
    /// perform the transformation to the specified value
    // - the value is converted into UTF-8 text, as expected by
    // TPropInfo.GetValue / TPropInfo.SetValue e.g.
    procedure Process(aFieldIndex: integer; var Value: RawUtf8); virtual; abstract;
  end;

  /// class-reference type (metaclass) for a TSynFilter or a TSynValidate
  TSynFilterOrValidateClass = class of TSynFilterOrValidate;

  /// class-reference type (metaclass) of a record filter (transformation)
  TSynFilterClass = class of TSynFilter;

  /// convert the value into ASCII Upper Case characters
  // - UpperCase conversion is made for ASCII-7 only, i.e. 'a'..'z' characters
  // - this version expects no parameter
  TSynFilterUpperCase = class(TSynFilter)
  public
    /// perform the case conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUtf8); override;
  end;

  /// convert the value into WinAnsi Upper Case characters
  // - UpperCase conversion is made for all latin characters in the WinAnsi
  // code page only, e.g. 'e' acute will be converted to 'E'
  // - this version expects no parameter
  TSynFilterUpperCaseU = class(TSynFilter)
  public
    /// perform the case conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUtf8); override;
  end;

  /// convert the value into ASCII Lower Case characters
  // - LowerCase conversion is made for ASCII-7 only, i.e. 'A'..'Z' characters
  // - this version expects no parameter
  TSynFilterLowerCase = class(TSynFilter)
  public
    /// perform the case conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUtf8); override;
  end;

  /// convert the value into WinAnsi Lower Case characters
  // - LowerCase conversion is made for all latin characters in the WinAnsi
  // code page only, e.g. 'E' acute will be converted to 'e'
  // - this version expects no parameter
  TSynFilterLowerCaseU = class(TSynFilter)
  public
    /// perform the case conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUtf8); override;
  end;

  /// trim any space character left or right to the value
  // - this versions expect no parameter
  TSynFilterTrim = class(TSynFilter)
  public
    /// perform the space trimming conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUtf8); override;
  end;

  /// truncate a text above a given maximum length
  // - expects optional JSON parameters of the allowed text length range as
  // $ '{MaxLength":10}
  TSynFilterTruncate = class(TSynFilter)
  protected
    fMaxLength: cardinal;
    fUtf8Length: boolean;
    /// decode the MaxLength: and Utf8Length: parameters
    procedure SetParameters(const Value: RawUtf8); override;
  public
    /// perform the length truncation of the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUtf8); override;
    /// Maximal length value allowed for the text content
    // - the length is calculated with UTF-16 Unicode codepoints, unless
    // Utf8Length has been set to TRUE so that the UTF-8 byte count is checked
    // - default is 0, i.e. no maximum length is forced
    property MaxLength: cardinal
      read fMaxLength write fMaxLength;
    /// defines if MaxLength is stored as UTF-8 or UTF-16 codepoints number
    // - with default FALSE, the length is calculated with UTF-16 Unicode
    // codepoints - MaxLength may not match the UCS4 CodePoint, in case of
    // UTF-16 surrogates
    // - you can set this property to TRUE so that the UTF-8 byte count would
    // be used for truncation against the MaxLength parameter
    property Utf8Length: boolean
      read fUtf8Length write fUtf8Length;
  end;

{$ifdef ISDELPHI}
resourcestring
{$else}
const
{$endif ISDELPHI}
  sInvalidIPAddress = '"%s" is an invalid IP v4 address';
  sInvalidEmailAddress = '"%s" is an invalid email address';
  sInvalidPattern = '"%s" does not match the expected pattern';
  sCharacter01n = 'character,character,characters';
  sInvalidTextLengthMin = 'Expect at least %d %s';
  sInvalidTextLengthMax = 'Expect up to %d %s';
  sInvalidTextChar = 'Expect at least %d %s %s,Expect up to %d %s %s,'+
    'alphabetical,digital,punctuation,lowercase,uppercase,space,'+
    'Too much spaces on the left,Too much spaces on the right';
  sValidationFailed = '"%s" rule failed';
  sValidationFieldVoid = 'An unique key field must not be void';
  sValidationFieldDuplicate = 'Value already used for this unique key field';


/// return TRUE if the supplied content is a valid IP v4 address
function IsValidIP4Address(P: PUtf8Char): boolean;

/// return TRUE if the supplied content is a valid email address
// - follows RFC 822, to validate local-part@domain email format
function IsValidEmail(P: PUtf8Char): boolean;


{ ***************** Cross-Platform TSynTimeZone Time Zones }

type
  {$A-} { make all records packed for cross-platform binary serialization }

  /// used to store Time Zone bias in TSynTimeZone
  // - map how low-level information is stored in the Windows Registry
  TTimeZoneInfo = record
    Bias: integer;
    bias_std: integer;
    bias_dlt: integer;
    change_time_std: TSynSystemTime;
    change_time_dlt: TSynSystemTime;
  end;
  PTimeZoneInfo = ^TTimeZoneInfo;

  /// text identifier of a Time Zone, following Microsoft Windows naming
  TTimeZoneID = type RawUtf8;

  /// used to store Time Zone information for a single area in TSynTimeZone
  // - Delphi "object" is buggy on stack -> also defined as record with methods
  {$ifdef USERECORDWITHMETHODS}
  TTimeZoneData = record
  {$else}
  TTimeZoneData = object
  {$endif USERECORDWITHMETHODS}
  public
    id: TTimeZoneID;
    display: RawUtf8;
    tzi: TTimeZoneInfo;
    dyn: array of packed record
      year: integer;
      tzi: TTimeZoneInfo;
    end;
    /// search for the TTimeZoneInfo of a given year
    function GetTziFor(year: integer): PTimeZoneInfo;
  end;

  /// used to store the Time Zone information of a TSynTimeZone class
  TTimeZoneDataDynArray = array of TTimeZoneData;

  {$A+}

  /// handle cross-platform time conversions, following Microsoft time zones
  // - is able to retrieve accurate information from the Windows registry,
  // or from a binary compressed file on other platforms (which should have been
  // saved from a Windows system first)
  // - for Linux/POSIX our mORMot 2 repository supplies a ready-to-use
  // ! {$R mormot.tz.res}
  // - each time zone will be identified by its TzId string, as defined by
  // Microsoft for its Windows Operating system
  // - note that each instance is thread-safe
  TSynTimeZone = class
  protected
    fSafe: TRWLightLock;
    fZone: TTimeZoneDataDynArray;
    fZoneCount: integer;
    fZones: TDynArrayHashed;
    fLastZone: TTimeZoneID;
    fLastIndex: integer;
    fIds: TStringList;
    fDisplays: TStringList;
    function LockedFindZoneIndex(const TzId: TTimeZoneID): PtrInt;
  public
    /// initialize the internal storage
    // - but no data is available, until Load* methods are called
    constructor Create;
    /// retrieve the time zones from Windows registry, or from a local file
    // - under Linux, the file should be located with the executable, renamed
    // with a .tz extension - may have been created via SaveToFile(''), or
    // from a 'TSynTimeZone' bound resource
    // - "dummycpp" parameter exists only to disambiguate constructors for C++
    constructor CreateDefault(dummycpp: integer = 0);
    /// finalize the instance
    destructor Destroy; override;
    /// will retrieve the default shared TSynTimeZone instance
    // - locally created via the CreateDefault constructor
    // - see also the NowToLocal/LocalToUtc/UtcToLocal global functions
    class function Default: TSynTimeZone;
    {$ifdef OSWINDOWS}
    /// read time zone information from the Windows registry
    procedure LoadFromRegistry;
    {$endif OSWINDOWS}
    /// read time zone information from a compressed file
    // - if no file name is supplied, a ExecutableName.tz file would be used
    procedure LoadFromFile(const FileName: TFileName = '');
    /// read time zone information from a compressed memory buffer
    procedure LoadFromBuffer(const Buffer: RawByteString);
    /// read time zone information from a 'TSynTimeZone' resource
    // - the resource should contain the SaveToBuffer compressed binary content
    // - is no resource matching the TSynTimeZone class name and ResType=10
    // do exist, nothing would be loaded
    // - the resource could be created as such, from a Windows system:
    // ! TSynTimeZone.Default.SaveToFile('TSynTimeZone.data');
    // then compile the resource as expected, with a brcc32 .rc entry:
    // ! TSynTimeZone 10 "TSynTimeZone.data"
    // - you can specify a library (dll) resource instance handle, if needed
    // - for Linux/POSIX our mORMot 2 repository supplies a ready-to-use
    // ! {$R mormot.tz.res}
    procedure LoadFromResource(Instance: TLibHandle = 0);
    /// write then time zone information into a compressed file
    // - if no file name is supplied, a ExecutableName.tz file would be created
    procedure SaveToFile(const FileName: TFileName);
    /// write then time zone information into a compressed memory buffer
    function SaveToBuffer: RawByteString;
    /// retrieve the time bias (in minutes) for a given date/time on a TzId
    function GetBiasForDateTime(const Value: TDateTime; const TzId: TTimeZoneID;
      out Bias: integer; out HaveDaylight: boolean; ValueIsUtc: boolean = false): boolean;
    /// retrieve the display text corresponding to a TzId
    // - returns '' if the supplied TzId is not recognized
    function GetDisplay(const TzId: TTimeZoneID): RawUtf8;
    /// compute the UTC date/time corrected for a given TzId
    function UtcToLocal(const UtcDateTime: TDateTime; const TzId: TTimeZoneID): TDateTime;
    /// compute the current date/time corrected for a given TzId
    function NowToLocal(const TzId: TTimeZoneID): TDateTime;
    /// compute the UTC date/time for a given local TzId value
    // - by definition, a local time may correspond to two UTC times, during the
    // time bias period, so the returned value is informative only, and any
    // stored value should be following UTC
    function LocalToUtc(const LocalDateTime: TDateTime; const TzID: TTimeZoneID): TDateTime;
    /// direct access to the low-level time zone information
    property Zone: TTimeZoneDataDynArray
      read fZone;
    /// direct access to the wrapper over the time zone information array
    property Zones: TDynArrayHashed
      read fZones;
    /// returns a TStringList of all TzID values
    // - could be used to fill any UI component to select the time zone
    // - order in Ids[] array follows the Zone[].id information
    function Ids: TStrings;
    /// returns a TStringList of all Display text values
    // - could be used to fill any UI component to select the time zone
    // - order in Displays[] array follows the Zone[].display information
    function Displays: TStrings;
  end;

/// retrieve the time bias (in minutes) for a given date/time on a TzId
// - will use a global shared thread-safe TSynTimeZone instance for the request
function GetBiasForDateTime(const Value: TDateTime; const TzId: TTimeZoneID;
  out Bias: integer; out HaveDaylight: boolean; ValueIsUtc: boolean = false): boolean;

/// retrieve the display text corresponding to a TzId
// - returns '' if the supplied TzId is not recognized
// - will use a global shared thread-safe TSynTimeZone instance for the request
function GetDisplay(const TzId: TTimeZoneID): RawUtf8;

/// compute the UTC date/time corrected for a given TzId
// - will use a global shared thread-safe TSynTimeZone instance for the request
function UtcToLocal(const UtcDateTime: TDateTime; const TzId: TTimeZoneID): TDateTime;
  {$ifdef HASINLINE} inline; {$endif}

/// compute the current date/time corrected for a given TzId
// - will use a global shared thread-safe TSynTimeZone instance for the request
function NowToLocal(const TzId: TTimeZoneID): TDateTime;
  {$ifdef HASINLINE} inline; {$endif}

/// compute the UTC date/time for a given local TzId value
// - by definition, a local time may correspond to two UTC times, during the
// time bias period, so the returned value is informative only, and any
// stored value should be following UTC
// - will use a global shared thread-safe TSynTimeZone instance for the request
function LocalToUtc(const LocalDateTime: TDateTime; const TzID: TTimeZoneID): TDateTime;
  {$ifdef HASINLINE} inline; {$endif}


implementation


{ ****************** Files Search in Folders }

procedure TFindFiles.FromSearchRec(const Directory: TFileName; const F: TSearchRec);
begin
  Name := Directory + TFileName(F.Name);
  {$ifdef OSWINDOWS}
  {$ifdef HASINLINE} // FPC or Delphi 2006+
  Size := F.Size;
  {$else} // F.Size was limited to 32-bit on older Delphi
  PInt64Rec(@Size)^.Lo := F.FindData.nFileSizeLow;
  PInt64Rec(@Size)^.Hi := F.FindData.nFileSizeHigh;
  {$endif HASINLINE}
  {$else}
  Size := F.Size;
  {$endif OSWINDOWS}
  Attr := F.Attr;
  Timestamp := SearchRecToDateTime(F);
end;

function TFindFiles.ToText: ShortString;
begin
  FormatShort('% % %', [Name, KB(Size), DateTimeToFileShort(Timestamp)], result);
end;

function FindFiles(const Directory, Mask, IgnoreFileName: TFileName;
  Options: TFindFilesOptions): TFindFilesDynArray;
var
  m, count: integer;
  dir: TFileName;
  da: TDynArray;
  masks: TRawUtf8DynArray;
  masked: TFindFilesDynArray;

  procedure SearchFolder(const folder: TFileName);
  var
    F: TSearchRec;
    ff: TFindFiles;
    fold, name: TFileName; // FPC requires these implicit local variables :(
  begin
    fold := dir + folder;
    name := fold + Mask;
    if FindFirst(name, faAnyfile - faDirectory, F) = 0 then
    begin
      repeat
        if SearchRecValidFile(F) and
           ((IgnoreFileName = '') or
            (AnsiCompareFileName(F.Name, IgnoreFileName) <> 0)) then
        begin
          if ffoExcludesDir in Options then
            ff.FromSearchRec(folder, F)
          else
            ff.FromSearchRec(fold, F);
          da.Add(ff);
        end;
      until FindNext(F) <> 0;
      FindClose(F);
    end;
    if (ffoSubFolder in Options) and
       (FindFirst(fold + '*', faDirectory, F) = 0) then
    begin
      // recursive SearchFolder() call for nested directories
      repeat
        if SearchRecValidFolder(F) and
           ((IgnoreFileName = '') or
            (AnsiCompareFileName(F.Name, IgnoreFileName) <> 0)) then
          SearchFolder(IncludeTrailingPathDelimiter(folder + F.Name));
      until FindNext(F) <> 0;
      FindClose(F);
    end;
  end;

begin
  Finalize(result);
  da.Init(TypeInfo(TFindFilesDynArray), result, @count);
  if Pos(';', Mask) > 0 then
    CsvToRawUtf8DynArray(pointer(StringToUtf8(Mask)), masks, ';');
  if masks <> nil then
  begin
    // recursive calls for each masks[]
    if ffoSortByName in Options then
      QuickSortRawUtf8(masks, length(masks), nil,
        {$ifdef OSWINDOWS} @StrIComp {$else} @StrComp {$endif});
    for m := 0 to length(masks) - 1 do
    begin
      masked := FindFiles(
        Directory, Utf8ToString(masks[m]), IgnoreFileName, Options);
      da.AddArray(masked);
    end;
  end
  else
  begin
    // single mask search
    if Directory <> '' then
      dir := IncludeTrailingPathDelimiter(Directory);
    SearchFolder('');
    if (ffoSortByName in Options) and
       (da.Count > 1) then
      da.Sort(SortDynArrayFileName);
  end;
  if count <> 0 then
    DynArrayFakeLength(result, count);
end;

function FileNames(const Directory, Mask: TFileName;
  Options: TFindFilesOptions; const IgnoreFileName: TFileName): TFileNameDynArray;
begin
  result := FindFilesDynArrayToFileNames(
    FindFiles(Directory, Mask, IgnoreFileName, Options));
end;

function FileNames(const Path: array of const; const Mask: TFileName;
  Options: TFindFilesOptions): TFileNameDynArray;
var
  dir: TFileName;
begin
  dir := MakePath(Path, {endwithdelim=}true);
  result := FileNames(dir, Mask, Options);
end;

function FindFilesDynArrayToFileNames(const Files: TFindFilesDynArray): TFileNameDynArray;
var
  i, n: PtrInt;
begin
  Finalize(result);
  if Files = nil then
    exit;
  n := length(Files);
  SetLength(result, n);
  for i := 0 to n - 1 do
    result[i] := Files[i].Name;
end;

function SortFindFileTimestamp(const A, B): integer;
begin
  result := CompareFloat(TFindFiles(A).Timestamp, TFindFiles(B).Timestamp);
end;

procedure FindFilesSortByTimestamp(var Files: TFindFilesDynArray);
begin
  DynArray(TypeInfo(TFindFilesDynArray), Files).Sort(SortFindFileTimestamp);
end;

function SynchFolders(const Reference, Dest: TFileName;
  Options: TSynchFoldersOptions): integer;
var
  ref, dst, reffn, dstfn: TFileName;
  fdst: TSearchRec;
  refsize: Int64;
  reftime: TUnixMSTime;
  s: RawByteString;
begin
  result := 0;
  ref := IncludeTrailingPathDelimiter(Reference);
  dst := IncludeTrailingPathDelimiter(Dest);
  if DirectoryExists(ref) and
     (FindFirst(dst + FILES_ALL, faAnyFile, fdst) = 0) then
  begin
    repeat
      if SearchRecValidFile(fdst) then
      begin
        reffn := ref + fdst.Name;
        if not FileInfoByName(reffn, refsize, reftime) then
          continue; // only update existing files
        if not (sfoByContent in Options) then
          if (refsize = fdst.Size) and
             (reftime = SearchRecToUnixTimeUtc(fdst)) then
            continue;
        dstfn := dst + fdst.Name;
        s := StringFromFile(reffn);
        if (s = '') or
           ((sfoByContent in Options) and
            (length(s) = fdst.Size) and
            (DefaultHasher(0, pointer(s), fdst.Size) = HashFile(dstfn))) then
          continue;
        FileFromString(s, dstfn);
        FileSetDateFromUnixUtc(dstfn, reftime div MSecsPerSec);
        inc(result);
        if sfoWriteFileNameToConsole in Options then
          ConsoleWrite('synched %', [dstfn]);
      end
      else if (sfoSubFolder in Options) and
              SearchRecValidFolder(fdst) then
        inc(result, SynchFolders(ref + fdst.Name, dst + fdst.Name, Options));
    until FindNext(fdst) <> 0;
    FindClose(fdst);
  end;
end;

function CopyFolder(const Source, Dest: TFileName;
  Options: TSynchFoldersOptions): integer;
var
  src, dst, reffn, dstfn: TFileName;
  sr: TSearchRec;
  dsize: Int64;
  dtime: TUnixMSTime;
  nested: integer;
begin
  result := 0;
  src := IncludeTrailingPathDelimiter(Source);
  if not DirectoryExists(src) then
    exit;
  dst := EnsureDirectoryExists(Dest);
  if (dst = '') or
     (FindFirst(src + FILES_ALL, faAnyFile, sr) <> 0) then
    exit;
  repeat
    reffn := src + sr.Name;
    dstfn := dst + sr.Name;
    if SearchRecValidFile(sr) then
    begin
      if FileInfoByName(dstfn, dsize, dtime) and // fast single syscall
         (sr.Size = dsize) then
        if sfoByContent in Options then
        begin
          if SameFileContent(reffn, dstfn) then
            continue;
        end
        else if abs(SearchRecToUnixTimeUtc(sr) * 1000 - dtime) < 1000 then
          continue; // allow error of 1 second timestamp resolution
      if not CopyFile(reffn, dstfn, {failsifexists=}false) then
        result := -1;
    end
    else if not SearchRecValidFolder(sr) then
      continue
    else if sfoSubFolder in Options then
    begin
      nested := CopyFolder(reffn, dstfn, Options);
      if nested < 0 then
        result := nested
      else
        inc(result, nested);
    end;
    if result < 0 then
      break;
    inc(result);
    if sfoWriteFileNameToConsole in Options then
      ConsoleWrite('copied %', [reffn]);
  until (FindNext(sr) <> 0);
  FindClose(sr);
end;


{ ****************** ScanUtf8, GLOB and SOUNDEX Text Search }

function ScanUtf8(P: PUtf8Char; PLen: PtrInt; const fmt: RawUtf8;
  const values:  array of pointer; ident: PRawUtf8DynArray): integer;
var
  v, w: PtrInt;
  F, FEnd, PEnd: PUtf8Char;
  tab: PTextCharSet;
label
  next;
begin
  result := 0;
  if (fmt = '') or
     (P = nil) or
     (PLen <= 0) or
     (high(values) < 0) then
    exit;
  if ident <> nil then
    SetLength(ident^, length(values));
  F := pointer(fmt);
  FEnd := F + length(fmt);
  PEnd := P + PLen;
  for v := 0 to high(values) do
    repeat
      if (P^ <= ' ') and
         (P^ <> #0) then
        // ignore any whitespace char in text
        repeat
          inc(P);
          if P = PEnd then
            exit;
        until (P^ > ' ') or
              (P^ = #0);
      while (F^ <= ' ') and
            (F^ <> #0) do
      begin
        // ignore any whitespace char in fmt
        inc(F);
        if F = FEnd then
          exit;
      end;
      if F^ = '%' then
      begin
        // handle format specifier
        inc(F);
        if F = FEnd then
          exit;
        case F^ of
          'd':
            PInteger(values[v])^ := GetNextItemInteger(P, #0);
          'D':
            PInt64(values[v])^ := GetNextItemInt64(P, #0);
          'u':
            PCardinal(values[v])^ := GetNextItemCardinal(P, #0);
          'U':
            PQword(values[v])^ := GetNextItemQword(P, #0);
          'f':
            unaligned(PDouble(values[v])^) := GetNextItemDouble(P, #0);
          'F':
            GetNextItemCurrency(P, PCurrency(values[v])^, #0);
          'x':
            if not GetNextItemHexDisplayToBin(P, values[v], 4, #0) then
              exit;
          'X':
            if not GetNextItemHexDisplayToBin(P, values[v], 8, #0) then
              exit;
          's', 'S':
            begin
              w := 0;
              while (P[w] > ' ') and
                    (P + w <= PEnd) do
                inc(w);
              if F^ = 's' then
                SetString(PShortString(values[v])^, PAnsiChar(P), w)
              else
                FastSetString(PRawUtf8(values[v])^, P, w);
              inc(P, w);
              while (P^ <= ' ') and
                    (P^ <> #0) and
                    (P <= PEnd) do
                inc(P);
            end;
          'L':
            begin
              w := 0;
              tab := @TEXT_CHARS;
              while (tcNot01013 in tab[P[w]]) and
                    (P + w <= PEnd) do
                inc(w);
              FastSetString(PRawUtf8(values[v])^, P, w);
              inc(P, w);
            end;
          '%':
            goto next;
        else
          raise ESynException.CreateUtf8(
            'ScanUtf8: unknown ''%'' specifier [%]', [F^, fmt]);
        end;
        inc(result);
        tab := @TEXT_CHARS;
        if (tcIdentifier in tab[F[1]]) or
           (ident <> nil) then
        begin
          w := 0;
          repeat
            inc(w)
          until not (tcIdentifier in tab[F[w]]) or
                (F + w = FEnd);
          if ident <> nil then
            FastSetString(ident^[v], F, w);
          inc(F, w);
        end
        else
          inc(F);
        if (F >= FEnd) or
           (P >= PEnd) then
          exit;
        break;
      end
      else
      begin
next:   while (P^ <> F^) and
              (P <= PEnd) do
          inc(P);
        inc(F);
        inc(P);
        if (F >= FEnd) or
           (P >= PEnd) then
          exit;
      end;
    until false;
end;

function ScanUtf8(const text, fmt: RawUtf8; const values: array of pointer;
  ident: PRawUtf8DynArray): integer;
begin
  result := ScanUtf8(pointer(text), length(text), fmt, values, ident);
end;


// inspired by ZMatchPattern.pas - http://www.zeoslib.sourceforge.net
procedure TMatch.MatchMain;
var
  RangeStart, RangeEnd: PtrInt;
  c: AnsiChar;
  flags: set of (Invert, MemberMatch);
begin
  while ((State = sNONE) and
        (P <= PMax)) do
  begin
    c := Upper[Pattern[P]];
    if T > TMax then
    begin
      if (c = '*') and
         (P + 1 > PMax) then
        State := sVALID
      else
        State := sABORT;
      exit;
    end
    else
      case c of
        '?':
          ;
        '*':
          MatchAfterStar;
        '[':
          begin
            inc(P);
            byte(flags) := 0;
            if Pattern[P] in ['!', '^'] then
            begin
              include(flags, Invert);
              inc(P);
            end;
            if Pattern[P] = ']' then
            begin
              State := sPATTERN;
              exit;
            end;
            c := Upper[Text[T]];
            while Pattern[P] <> ']' do
            begin
              RangeStart := P;
              RangeEnd := P;
              inc(P);
              if P > PMax then
              begin
                State := sPATTERN;
                exit;
              end;
              if Pattern[P] = '-' then
              begin
                inc(P);
                RangeEnd := P;
                if (P > PMax) or
                   (Pattern[RangeEnd] = ']') then
                begin
                  State := sPATTERN;
                  exit;
                end;
                inc(P);
              end;
              if P > PMax then
              begin
                State := sPATTERN;
                exit;
              end;
              if RangeStart < RangeEnd then
              begin
                if (c >= Upper[Pattern[RangeStart]]) and
                   (c <= Upper[Pattern[RangeEnd]]) then
                begin
                  include(flags, MemberMatch);
                  break;
                end;
              end
              else if (c >= Upper[Pattern[RangeEnd]]) and
                      (c <= Upper[Pattern[RangeStart]]) then
              begin
                include(flags, MemberMatch);
                break;
              end;
            end;
            if ((Invert in flags) and
                (MemberMatch in flags)) or
               not ((Invert in flags) or
                    (MemberMatch in flags)) then
            begin
              State := sRANGE;
              exit;
            end;
            if MemberMatch in flags then
              while (P <= PMax) and
                    (Pattern[P] <> ']') do
                inc(P);
            if P > PMax then
            begin
              State := sPATTERN;
              exit;
            end;
          end;
      else
        if c <> Upper[Text[T]] then
          State := sLITERAL;
      end;
    inc(P);
    inc(T);
  end;
  if State = sNONE then
    if T <= TMax then
      State := sEND
    else
      State := sVALID;
end;

procedure TMatch.MatchAfterStar;
var
  retryT, retryP: PtrInt;
begin
  if (TMax = 1) or
     (P = PMax) then
  begin
    State := sVALID;
    exit;
  end
  else if (PMax = 0) or
          (TMax = 0) then
  begin
    State := sABORT;
    exit;
  end;
  while (T <= TMax) and
        (P < PMax) and
        (Pattern[P] in ['?', '*']) do
  begin
    if Pattern[P] = '?' then
      inc(T);
    inc(P);
  end;
  if T >= TMax then
  begin
    State := sABORT;
    exit;
  end
  else if P >= PMax then
  begin
    State := sVALID;
    exit;
  end;
  repeat
    if (Upper[Pattern[P]] = Upper[Text[T]]) or
       (Pattern[P] = '[') then
    begin
      retryT := T;
      retryP := P;
      MatchMain;
      if State = sVALID then
        break;
      State := sNONE; // retry until end of Text, (check below) or State valid
      T := retryT;
      P := retryP;
    end;
    inc(T);
    if (T > TMax) or
       (P > PMax) then
    begin
      State := sABORT;
      exit;
    end;
  until State <> sNONE;
end;

function SearchAny(aMatch: PMatch; aText: PUtf8Char; aTextLen: PtrInt): boolean;
begin
  aMatch.State := sNONE;
  aMatch.P := 0;
  aMatch.T := 0;
  aMatch.Text := aText;
  aMatch.TMax := aTextLen - 1;
  aMatch.MatchMain;
  result := aMatch.State = sVALID;
end;

// faster alternative (without recursion) for only * ? (but not [...])

{$ifdef CPU32} // less registers on this CPU - also circumvent ARM problems (Alf)

function SearchNoRange(aMatch: PMatch; aText: PUtf8Char; aTextLen: PtrInt): boolean;
var
  c: AnsiChar;
  pat, txt: PtrInt; // use local registers
begin
  aMatch.T := 0; // aMatch.P/T are used for retry positions after *
  aMatch.Text := aText;
  aMatch.TMax := aTextLen - 1;
  pat := 0;
  txt := 0;
  repeat
    if pat <= aMatch.PMax then
    begin
      c := aMatch.Pattern[pat];
      case c of
        '?':
          if txt <= aMatch.TMax then
          begin
            inc(pat);
            inc(txt);
            continue;
          end;
        '*':
          begin
            aMatch.P := pat;
            aMatch.T := txt + 1;
            inc(pat);
            continue;
          end;
      else
        if (txt <= aMatch.TMax) and
           (c = aMatch.Text[txt]) then
        begin
          inc(pat);
          inc(txt);
          continue;
        end;
      end;
    end
    else if txt > aMatch.TMax then
      break;
    txt := aMatch.T;
    if (txt > 0) and
       (txt <= aMatch.TMax + 1) then
    begin
      inc(aMatch.T);
      pat := aMatch.P + 1;
      continue;
    end;
    result := false;
    exit;
  until false;
  result := true;
end;

{$else} // optimized for x86_64/ARM with more registers

function SearchNoRange(aMatch: PMatch; aText: PUtf8Char; aTextLen: PtrInt): boolean;
var
  c: AnsiChar;
  pat, patend, txtend, txtretry, patretry: PUtf8Char;
label
  fin;
begin
  pat := pointer(aMatch.Pattern);
  if pat = nil then
    goto fin;
  patend := pat + aMatch.PMax;
  patretry := nil;
  txtend := aText + aTextLen - 1;
  txtretry := nil;
  repeat
    if pat <= patend then
    begin
      c := pat^;
      if c <> '*' then
        if c <> '?' then
        begin
          if (aText <= txtend) and
             (c = aText^) then
          begin
            inc(pat);
            inc(aText);
            continue;
          end;
        end
        else
        begin
          // '?'
          if aText <= txtend then
          begin
            inc(pat);
            inc(aText);
            continue;
          end;
        end
      else
      begin
        // '*'
        inc(pat);
        txtretry := aText + 1;
        patretry := pat;
        continue;
      end;
    end
    else if aText > txtend then
      break;
    if (PtrInt(PtrUInt(txtretry)) > 0) and
       (txtretry <= txtend + 1) then
    begin
      aText := txtretry;
      inc(txtretry);
      pat := patretry;
      continue;
    end;
fin:result := false;
    exit;
  until false;
  result := true;
end;

{$endif CPUX86}

function SearchNoRangeU(aMatch: PMatch; aText: PUtf8Char; aTextLen: PtrInt): boolean;
var
  c: AnsiChar;
  pat, txt: PtrInt;
begin
  aMatch.T := 0;
  aMatch.Text := aText;
  aMatch.TMax := aTextLen - 1;
  pat := 0;
  txt := 0;
  repeat
    if pat <= aMatch.PMax then
    begin
      c := aMatch.Pattern[pat];
      case c of
        '?':
          if txt <= aMatch.TMax then
          begin
            inc(pat);
            inc(txt);
            continue;
          end;
        '*':
          begin
            aMatch.P := pat;
            aMatch.T := txt + 1;
            inc(pat);
            continue;
          end;
      else
        if (txt <= aMatch.TMax) and
           (aMatch.Upper[c] = aMatch.Upper[aMatch.Text[txt]]) then
        begin
          inc(pat);
          inc(txt);
          continue;
        end;
      end;
    end
    else if txt > aMatch.TMax then
      break;
    txt := aMatch.T;
    if (txt > 0) and
       (txt <= aMatch.TMax + 1) then
    begin
      inc(aMatch.T);
      pat := aMatch.P + 1;
      continue;
    end;
    result := false;
    exit;
  until false;
  result := true;
end;

function SimpleContainsU(t, tend, p: PUtf8Char; pmax: PtrInt; up: PNormTable): boolean;
  {$ifdef FPC}inline;{$endif} // Delphi has troubles inlining goto/label
// brute force case-insensitive search p[0..pmax] in t..tend-1
var
  first: AnsiChar;
  i: PtrInt;
label
  next;
begin
  first := up[p^];
  repeat
    if up[t^] <> first then
    begin
next: inc(t);
      if t < tend then
        continue
      else
        break;
    end;
    for i := 1 to pmax do
      if up[t[i]] <> up[p[i]] then
        goto next;
    result := true;
    exit;
  until false;
  result := false;
end;

{$ifdef CPU64} // naive but very efficient code generation on FPC x86-64

function SimpleContains8(t, tend, p: PUtf8Char; pmax: PtrInt): boolean;
  {$ifdef FPC}inline;{$endif} // Delphi has troubles inlining goto/label
label
  next;
var
  i, first: PtrInt;
begin
  first := PPtrInt(p)^;
  repeat
    if PPtrInt(t)^ <> first then
    begin
next: inc(t);
      if t < tend then
        continue
      else
        break;
    end;
    for i := 8 to pmax do
      if t[i] <> p[i] then
        goto next;
    result := true;
    exit;
  until false;
  result := false;
end;

{$endif CPU64}


{$ifdef CPUX86}

function SimpleContains1(t, tend, p: PUtf8Char; pmax: PtrInt): boolean;
  {$ifdef FPC}inline;{$endif} // Delphi has troubles inlining goto/label
label
  next;
var
  i: PtrInt;
begin
  repeat
    if t^ <> p^ then
    begin
next: inc(t);
      if t < tend then
        continue
      else
        break;
    end;
    for i := 1 to pmax do
      if t[i] <> p[i] then
        goto next;
    result := true;
    exit;
  until false;
  result := false;
end;

function SimpleContains4(t, tend, p: PUtf8Char; pmax: PtrInt): boolean;
  {$ifdef FPC}inline;{$endif} // Delphi has troubles inlining goto/label
label
  next;
var
  i: PtrInt;
begin
  repeat
    if PCardinal(t)^ <> PCardinal(p)^ then
    begin
next: inc(t);
      if t < tend then
        continue
      else
        break;
    end;
    for i := 1 to pmax do
      if t[i] <> p[i] then
        goto next;
    result := true;
    exit;
  until false;
  result := false;
end;

{$else}

function SimpleContains1(t, tend, p: PUtf8Char; pmax: PtrInt): boolean;
  {$ifdef FPC}inline;{$endif} // Delphi has troubles inlining goto/label
label
  next;
var
  i: PtrInt;
  first: AnsiChar;
begin
  first := p^;
  repeat
    if t^ <> first then
    begin
next: inc(t);
      if t < tend then
        continue
      else
        break;
    end;
    for i := 1 to pmax do
      if t[i] <> p[i] then
        goto next;
    result := true;
    exit;
  until false;
  result := false;
end;

function SimpleContains4(t, tend, p: PUtf8Char; pmax: PtrInt): boolean;
  {$ifdef FPC}inline;{$endif} // Delphi has troubles inlining goto/label
label
  next;
var
  i: PtrInt;
  first: cardinal;
begin
  first := PCardinal(p)^;
  repeat
    if PCardinal(t)^ <> first then
    begin
next: inc(t);
      if t < tend then
        continue
      else
        break;
    end;
    for i := 1 to pmax do
      if t[i] <> p[i] then
        goto next;
    result := true;
    exit;
  until false;
  result := false;
end;

{$endif CPUX86}

function CompareMemU(P1, P2: PUtf8Char; len: PtrInt; U: PNormTable): boolean;
  {$ifdef FPC} inline;{$endif}
begin
  // here we know that len>0
  result := false;
  repeat
    dec(len);
    if U[P1[len]] <> U[P2[len]] then
      exit;
  until len = 0;
  result := true;
end;

function SearchVoid(aMatch: PMatch; aText: PUtf8Char; aTextLen: PtrInt): boolean;
begin
  result := aTextLen = 0;
end;

function SearchNoPattern(aMatch: PMatch; aText: PUtf8Char; aTextLen: PtrInt): boolean;
begin
  result := (aMatch.PMax + 1 = aTextLen) and
            mormot.core.base.CompareMem(aText, aMatch.Pattern, aTextLen);
end;

function SearchNoPatternU(aMatch: PMatch; aText: PUtf8Char; aTextLen: PtrInt): boolean;
begin
  result := (aMatch.PMax + 1 = aTextLen) and
            CompareMemU(aText, aMatch.Pattern, aTextLen, aMatch.Upper);
end;

function SearchContainsValid(aMatch: PMatch; aText: PUtf8Char; aTextLen: PtrInt): boolean;
begin
  result := true;
end;

function SearchContainsU(aMatch: PMatch; aText: PUtf8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  if aTextLen > 0 then
    result := SimpleContainsU(aText, aText + aTextLen,
      aMatch.Pattern, aMatch.PMax, aMatch.Upper)
  else
    result := false;
end;

function SearchContains1(aMatch: PMatch; aText: PUtf8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  if aTextLen > 0 then
    result := SimpleContains1(aText, aText + aTextLen, aMatch.Pattern, aMatch.PMax)
  else
    result := false;
end;

function SearchContains4(aMatch: PMatch; aText: PUtf8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  if aTextLen > 0 then
    result := SimpleContains4(aText, aText + aTextLen, aMatch.Pattern, aMatch.PMax)
  else
    result := false;
end;

{$ifdef CPU64}
function SearchContains8(aMatch: PMatch; aText: PUtf8Char; aTextLen: PtrInt): boolean;
begin
  // optimized e.g. to search an IP address as '*12.34.56.78*' in logs
  dec(aTextLen, aMatch.PMax);
  if aTextLen > 0 then
    result := SimpleContains8(aText, aText + aTextLen, aMatch.Pattern, aMatch.PMax)
  else
    result := false;
end;
{$endif CPU64}

function SearchStartWith(aMatch: PMatch; aText: PUtf8Char; aTextLen: PtrInt): boolean;
begin
  result := (aMatch.PMax < aTextLen) and
    mormot.core.base.CompareMem(aText, aMatch.Pattern, aMatch.PMax + 1);
end;

function SearchStartWithU(aMatch: PMatch; aText: PUtf8Char; aTextLen: PtrInt): boolean;
begin
  result := (aMatch.PMax < aTextLen) and
            CompareMemU(aText, aMatch.Pattern, aMatch.PMax + 1, aMatch.Upper);
end;

function SearchEndWith(aMatch: PMatch; aText: PUtf8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  result := (aTextLen >= 0) and
    mormot.core.base.CompareMem(aText + aTextLen, aMatch.Pattern, aMatch.PMax);
end;

function SearchEndWithU(aMatch: PMatch; aText: PUtf8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  result := (aTextLen >= 0) and
            CompareMemU(aText + aTextLen, aMatch.Pattern, aMatch.PMax, aMatch.Upper);
end;

procedure TMatch.Prepare(const aPattern: RawUtf8;
  aCaseInsensitive, aReuse: boolean);
begin
  Prepare(pointer(aPattern), length(aPattern), aCaseInsensitive, aReuse);
end;

procedure TMatch.Prepare(aPattern: PUtf8Char; aPatternLen: integer;
  aCaseInsensitive, aReuse: boolean);
const
  SPECIALS: PUtf8Char = '*?[';
begin
  Pattern := aPattern;
  pmax := aPatternLen - 1; // search in Pattern[0..PMax]
  if Pattern = nil then
  begin
    Search := SearchVoid;
    exit;
  end;
  if aCaseInsensitive and
     not IsCaseSensitive(aPattern, aPatternLen) then
    aCaseInsensitive := false; // don't slow down e.g. number or IP search
  if aCaseInsensitive then
    Upper := @NormToUpperAnsi7
  else
    Upper := @NormToNorm;
  Search := nil;
  if aReuse then
    if strcspn(Pattern, SPECIALS) > pmax then
      if aCaseInsensitive then
        Search := SearchNoPatternU
      else
        Search := SearchNoPattern
    else if pmax > 0 then
    begin
      if Pattern[pmax] = '*' then
      begin
        if strcspn(Pattern + 1, SPECIALS) = pmax - 1 then
          case Pattern[0] of
            '*':
              begin
                // *something*
                inc(Pattern);
                dec(pmax, 2); // trim trailing and ending *
                if pmax < 0 then
                  Search := SearchContainsValid
                else if aCaseInsensitive then
                  Search := SearchContainsU
              {$ifdef CPU64}
                else if pmax >= 7 then
                  Search := SearchContains8
              {$endif CPU64}
                else if pmax >= 3 then
                  Search := SearchContains4
                else
                  Search := SearchContains1;
              end;
            '?':
              // ?something*
              if aCaseInsensitive then
                Search := SearchNoRangeU
              else
                Search := SearchNoRange;
            '[':
              Search := SearchAny;
          else
            begin
              dec(pmax); // trim trailing *
              if aCaseInsensitive then
                Search := SearchStartWithU
              else
                Search := SearchStartWith;
            end;
          end;
      end
      else if (Pattern[0] = '*') and
              (strcspn(Pattern + 1, SPECIALS) >= pmax) then
      begin
        inc(Pattern); // jump leading *
        if aCaseInsensitive then
          Search := SearchEndWithU
        else
          Search := SearchEndWith;
      end;
    end
    else if Pattern[0] in ['*', '?'] then
      Search := SearchContainsValid;
  if not Assigned(Search) then
  begin
    aPattern := PosChar(Pattern, '[');
    if (aPattern = nil) or
       (aPattern - Pattern > pmax) then
      if aCaseInsensitive then
        Search := SearchNoRangeU
      else
        Search := SearchNoRange
    else
      Search := SearchAny;
  end;
end;

type
  // Holub and Durian (2005) SBNDM2 algorithm
  // see http://www.cri.haifa.ac.il/events/2005/string/presentations/Holub.pdf
  TSBNDMQ2Mask = array[AnsiChar] of cardinal;
  PSBNDMQ2Mask = ^TSBNDMQ2Mask;

function SearchSBNDMQ2ComputeMask(const Pattern: RawUtf8;
  u: PNormTable): RawByteString;
var
  i: PtrInt;
  p: PAnsiChar absolute Pattern;
  m: PSBNDMQ2Mask absolute result;
  c: PCardinal;
begin
  FastNewRawByteString(result, SizeOf(m^));
  FillCharFast(m^, SizeOf(m^), 0);
  for i := 0 to length(Pattern) - 1 do
  begin
    c := @m^[u[p[i]]]; // for FPC code generation
    c^ := c^ or cardinal(1 shl i);
  end;
end;

function SearchSBNDMQ2(aMatch: PMatch;
  aText: PUtf8Char; aTextLen: PtrInt): boolean;
var
  mask: PSBNDMQ2Mask;
  max, i, j: PtrInt;
  state: cardinal;
begin
  mask := pointer(aMatch^.Pattern); // was filled by SearchSBNDMQ2ComputeMask()
  max := aMatch^.pmax;
  i := max - 1;
  dec(aTextLen);
  if i < aTextLen then
  begin
    repeat
      state := mask[aText[i + 1]] shr 1; // in two steps for better FPC codegen
      state := state and mask[aText[i]];
      if state = 0 then
      begin
        inc(i, max); // fast skip
        if i >= aTextLen then
          break;
        continue;
      end;
      j := i - max;
      repeat
        dec(i);
        if i < 0 then
          break;
        state := (state shr 1) and mask[aText[i]];
      until state = 0;
      if i = j then
      begin
        result := true;
        exit;
      end;
      inc(i, max);
      if i >= aTextLen then
        break;
    until false;
  end;
  result := false;
end;

function SearchSBNDMQ2U(aMatch: PMatch;
  aText: PUtf8Char; aTextLen: PtrInt): boolean;
var
  u: PNormTable;
  mask: PSBNDMQ2Mask;
  max, i, j: PtrInt;
  state: cardinal;
begin
  mask := pointer(aMatch^.Pattern);
  max := aMatch^.pmax;
  u := aMatch^.Upper;
  i := max - 1;
  dec(aTextLen);
  if i < aTextLen then
  begin
    repeat
      state := mask[u[aText[i + 1]]] shr 1;
      state := state and mask[u[aText[i]]];
      if state = 0 then
      begin
        inc(i, max);
        if i >= aTextLen then
          break;
        continue;
      end;
      j := i - max;
      repeat
        dec(i);
        if i < 0 then
          break;
        state := (state shr 1) and mask[u[aText[i]]];
      until state = 0;
      if i = j then
      begin
        result := true;
        exit;
      end;
      inc(i, max);
      if i >= aTextLen then
        break;
    until false;
  end;
  result := false;
end;

procedure TMatch.PrepareContains(var aPattern: RawUtf8;
  aCaseInsensitive: boolean);
begin
  pmax := length(aPattern) - 1;
  if aCaseInsensitive and
     not IsCaseSensitive(pointer(aPattern), pmax + 1) then
    aCaseInsensitive := false;
  if aCaseInsensitive then
    Upper := @NormToUpperAnsi7
  else
    Upper := @NormToNorm;
  if pmax < 0 then
    Search := SearchContainsValid
  else if pmax > 30 then
    if aCaseInsensitive then
      Search := SearchContainsU
    else
      {$ifdef CPU64}
      Search := SearchContains8
      {$else}
      Search := SearchContains4
      {$endif CPU64}
  else if pmax >= 1 then
  begin
    // PMax=[1..30] -> len=[2..31] -> aPattern becomes a SBNDMQ2 lookup table
    aPattern := SearchSBNDMQ2ComputeMask(aPattern, Upper);
    if aCaseInsensitive then
      Search := SearchSBNDMQ2U
    else
      Search := SearchSBNDMQ2;
  end
  else if aCaseInsensitive then
    Search := SearchContainsU
  else
    Search := SearchContains1; // todo: use ByteScanIndex() asm?
  Pattern := pointer(aPattern);
end;

procedure TMatch.PrepareRaw(aPattern: PUtf8Char; aPatternLen: integer;
  aSearch: TMatchSearchFunction);
begin
  Pattern := aPattern;
  pmax := aPatternLen - 1; // search in Pattern[0..PMax]
  Search := aSearch;
end;

function TMatch.Match(const aText: RawUtf8): boolean;
begin
  if pointer(aText) <> nil then
    result := Search(@self,
                pointer(aText), PStrLen(PAnsiChar(pointer(aText)) - _STRLEN)^)
  else
    result := pmax < 0;
end;

function TMatch.Match(aText: PUtf8Char; aTextLen: PtrInt): boolean;
begin
  if (aText <> nil) and
     (aTextLen > 0) then
    result := Search(@self, aText, aTextLen)
  else
    result := pmax < 0;
end;

function TMatch.MatchThreadSafe(const aText: RawUtf8): boolean;
var
  local: TMatch; // thread-safe with no lock!
begin
  local := self;
  if aText <> '' then
    result := local.Search(@local, pointer(aText), length(aText))
  else
    result := local.PMax < 0;
end;

function TMatch.MatchString(const aText: string): boolean;
var
  local: TMatch; // thread-safe with no lock!
  temp: TSynTempBuffer;
  len: integer;
begin
  if aText = '' then
  begin
    result := pmax < 0;
    exit;
  end;
  len := length(aText);
  temp.Init(len * 3);
  {$ifdef UNICODE}
  len := RawUnicodeToUtf8(temp.buf, temp.len + 1, pointer(aText), len, [ccfNoTrailingZero]);
  {$else}
  len := CurrentAnsiConvert.AnsiBufferToUtf8(temp.buf, pointer(aText), len) - temp.buf;
  {$endif UNICODE}
  local := self;
  result := local.Search(@local, temp.buf, len);
  temp.Done;
end;

function TMatch.Equals(const aAnother: TMatch): boolean;
begin
  result := (pmax = TMatch(aAnother).pmax) and
            (Upper = TMatch(aAnother).Upper) and
    mormot.core.base.CompareMem(Pattern, TMatch(aAnother).Pattern, pmax + 1);
end;

function TMatch.PatternLength: integer;
begin
  result := pmax + 1;
end;

function TMatch.PatternText: PUtf8Char;
begin
  result := Pattern;
end;

function TMatch.CaseInsensitive: boolean;
begin
  result := Upper = @NormToUpperAnsi7;
end;


{ TUriMatchName }

procedure TUriMatchName.ParsePath;
var
  i: PtrInt;
begin
  Name := Path;
  i := Name.Len;
  while i > 0 do // retrieve
  begin
    dec(i);
    if Name.Text[i] <> '/' then
      continue;
    inc(i);
    inc(Name.Text, i);
    dec(Name.Len, i);
    break;
  end;
end;


{ TUriMatch }

procedure TUriMatch.DoInit(csv: PUtf8Char; caseinsensitive: boolean);
var
  s: PUtf8Char;
  m: ^TMatchDynArray;
begin
  if csv <> nil then
    repeat
      m := @Names; // default 'file.ext' pattern
      csv := GotoNextNotSpace(csv);
      s := csv;
      repeat
        case csv^ of
          #0,
          ',':
            break;
          '/':
            m := @Paths; // is a 'path/to/file.ext' pattern
        end;
        inc(csv);
      until false;
      if csv <> s then
        MatchNew(m^)^.Prepare(s, csv - s, caseinsensitive, true);
      if csv^ = #0 then
        break;
      inc(csv);
    until false;
end;

function TUriMatch.Check(const csv: RawUtf8;
  const uri: TUriMatchName; caseinsensitive: boolean): boolean;
begin
  if Init.TryLock then // thread-safe init once from supplied csv
    DoInit(pointer(csv), caseinsensitive);
  result := ((Names <> nil) and
             MatchAny(pointer(Names), uri.Name.Text, uri.Name.Len)) or
            ((Paths <> nil) and
             MatchAny(pointer(Paths), uri.Path.Text, uri.Path.Len));
end;


function IsMatch(const Pattern, Text: RawUtf8; CaseInsensitive: boolean): boolean;
var
  match: TMatch;
begin
  match.Prepare(pointer(Pattern), length(Pattern), CaseInsensitive, {reuse=}false);
  result := match.Match(Text);
end;

function IsMatchString(const Pattern, Text: string;
  CaseInsensitive: boolean): boolean;
var
  match: TMatch;
  pat, txt: RawUtf8;
begin
  StringToUtf8(Pattern, pat); // local variable is mandatory for FPC
  StringToUtf8(Text, txt);
  match.Prepare(pat, CaseInsensitive, {reuse=}false);
  result := match.Match(txt);
end;

function SetNextMatch(P: PUtf8Char; var Dest: TMatch;
  CaseInsensitive, Reuse: boolean; CsvSep: AnsiChar): PUtf8Char;
begin
  result := P;
  repeat
    while not (result^ in [#0, CsvSep]) do
      inc(result);
    if result <> P then
    begin
      Dest.Prepare(P, result - P, CaseInsensitive, Reuse);
      if result^ = CsvSep then
        inc(result); // go to next CSV
      exit;
    end;
  until result^ = #0;
  result := nil; // indicates Dest.Prepare() was not called
end;

function IsMatchs(CsvPattern, Text: PUtf8Char; TextLen: PtrInt;
  CaseInsensitive: boolean; CsvSep: AnsiChar): boolean;
var
  match: TMatch;
begin
  result := (CsvPattern <> nil) and (TextLen > 0);
  if not result then
    exit;
  repeat
    CsvPattern := SetNextMatch(
      CsvPattern, match, CaseInsensitive, {reuse=}false, CsvSep);
    if CsvPattern = nil then
      break;
    if match.Search(@match, Text, TextLen) then
      exit;
  until CsvPattern^ = #0;
  result := false;
end;

function IsMatchs(const CsvPattern, Text: RawUtf8; CaseInsensitive: boolean;
  CsvSep: AnsiChar): boolean;
begin
  result := IsMatchs(pointer(CsvPattern), pointer(Text), length(Text),
    CaseInsensitive, CsvSep);
end;

function SetMatchs(const CsvPattern: RawUtf8; CaseInsensitive: boolean;
  out Match: TMatchDynArray; CsvSep: AnsiChar): integer;
var
  P, S: PUtf8Char;
begin
  P := pointer(CsvPattern);
  if P <> nil then
    repeat
      S := P;
      while not (P^ in [#0, CsvSep]) do
        inc(P);
      if P <> S then
        MatchNew(Match)^.Prepare(S, P - S, CaseInsensitive, {reuse=}true);
      if P^ = #0 then
        break;
      inc(P);
    until false;
  result := length(Match);
end;

function SetMatchs(CsvPattern: PUtf8Char; CaseInsensitive: boolean;
  Match: PMatch; MatchMax: integer; CsvSep: AnsiChar): integer;
var
  S: PUtf8Char;
begin
  result := 0;
  if (CsvPattern <> nil) and
     (MatchMax >= 0) then
    repeat
      S := CsvPattern;
      while not (CsvPattern^ in [#0, CsvSep]) do
        inc(CsvPattern);
      if CsvPattern <> S then
      begin
        Match^.Prepare(S, CsvPattern - S, CaseInsensitive, {reuse=}true);
        inc(result);
        if result > MatchMax then
          break;
        inc(Match);
      end;
      if CsvPattern^ = #0 then
        break;
      inc(CsvPattern);
    until false;
end;

function MatchExists(const One: TMatch; const Several: TMatchDynArray): boolean;
var
  i: PtrInt;
begin
  result := true;
  for i := 0 to length(Several) - 1 do
    if Several[i].Equals(One) then
      exit;
  result := false;
end;

function MatchAdd(const One: TMatch; var Several: TMatchDynArray): boolean;
begin
  result := not MatchExists(One, Several);
  if result then
    MatchNew(Several)^ := One;
end;

function MatchNew(var Several: TMatchDynArray): PMatch;
var
  n: PtrInt;
begin
  n := length(Several);
  SetLength(Several, n + 1);
  result := @Several[n];
end;

function MatchAny(const Match: TMatchDynArray; const Text: RawUtf8): boolean;
begin
  result := MatchAny(pointer(Match), pointer(Text), length(Text));
end;

function MatchAny(Match: PMatch; Text: PUtf8Char; TextLen: PtrInt): boolean;
var
  n: integer;
begin
  result := true;
  if Match = nil then
    exit;
  if TextLen <= 0 then
    Text := nil;
  n := PDALen(PAnsiChar(pointer(Match)) - _DALEN)^ + (_DAOFF - 1);
  repeat
    // inlined Match^.Match() to avoid internal error on Delphi
    if Text <> nil then
    begin
      if Match^.Search(Match, Text, TextLen) then
        exit;
    end
    else if Match^.pmax < 0 then
      exit;
    inc(Match);
    dec(n);
  until n = 0;
  result := false;
end;

procedure FilterMatchs(const CsvPattern: RawUtf8; CaseInsensitive: boolean;
  var Values: TRawUtf8DynArray; CsvSep: AnsiChar);
var
  match: TMatchDynArray;
  m, n, i: PtrInt;
begin
  if SetMatchs(CsvPattern, CaseInsensitive, match, CsvSep) = 0 then
    exit;
  n := 0;
  for i := 0 to high(Values) do
    for m := 0 to high(match) do
      if match[m].Match(Values[i]) then
      begin
        if i <> n then
          Values[n] := Values[i];
        inc(n);
        break;
      end;
  if n <> length(Values) then
    SetLength(Values, n);
end;

procedure FilterMatchs(const CsvPattern: RawUtf8; CaseInsensitive: boolean;
  var Values: TStringDynArray; CsvSep: AnsiChar);
var
  match: TMatchDynArray;
  m, n, i: PtrInt;
begin
  if SetMatchs(CsvPattern, CaseInsensitive, match, CsvSep) = 0 then
    exit;
  n := 0;
  for i := 0 to high(Values) do
    for m := 0 to high(match) do
      if match[m].MatchString(Values[i]) then
      begin
        if i <> n then
          Values[n] := Values[i];
        inc(n);
        break;
      end;
  if n <> length(Values) then
    SetLength(Values, n);
end;


{ TMatchs }

constructor TMatchs.Create(const aPatterns: TRawUtf8DynArray; CaseInsensitive: boolean);
begin
  inherited Create; // may have been overriden
  Subscribe(aPatterns, CaseInsensitive);
end;

function TMatchs.Match(const aText: RawUtf8): integer;
begin
  result := Match(pointer(aText), length(aText));
end;

function TMatchs.Match(aText: PUtf8Char; aLen: integer): integer;
var
  one: ^TMatchStore;
  local: TMatch; // thread-safe with no lock!
begin
  if (self = nil) or
     (fMatch = nil) then
    result := -1 // no filter by name -> allow e.g. to process everything
  else
  begin
    one := pointer(fMatch);
    if aLen <> 0 then
    begin
      for result := 0 to fMatchCount - 1 do
      begin
        local := one^.Pattern;
        if local.Search(@local, aText, aLen) then
          exit;
        inc(one);
      end;
    end
    else
      for result := 0 to fMatchCount - 1 do
        if one^.Pattern.PMax < 0 then
          exit
        else
          inc(one);
    result := -2;
  end;
end;

function TMatchs.MatchString(const aText: string): integer;
var
  temp: TSynTempBuffer;
  len: integer;
begin
  len := StringToUtf8(aText, temp);
  result := Match(temp.buf, len);
  temp.Done;
end;

procedure TMatchs.Subscribe(const aPatternsCsv: RawUtf8; CaseInsensitive: boolean);
var
  patterns: TRawUtf8DynArray;
begin
  CsvToRawUtf8DynArray(pointer(aPatternsCsv), patterns);
  Subscribe(patterns, CaseInsensitive);
end;

procedure TMatchs.Subscribe(const aPatterns: TRawUtf8DynArray; CaseInsensitive: boolean);
var
  i, j, m, n: integer;
  found: ^TMatchStore;
  pat: PRawUtf8;
begin
  m := length(aPatterns);
  if m = 0 then
    exit;
  n := fMatchCount;
  SetLength(fMatch, n + m);
  pat := pointer(aPatterns);
  for i := 1 to m do
  begin
    found := pointer(fMatch);
    for j := 1 to n do
      if StrComp(pointer(found^.PatternInstance), pointer(pat^)) = 0 then
      begin
        found := nil;
        break;
      end
      else
        inc(found);
    if found <> nil then
      with fMatch[n] do
      begin
        PatternInstance := pat^; // avoid GPF if aPatterns[] is released
        Pattern.Prepare(PatternInstance, CaseInsensitive, {reuse=}true);
        inc(n);
      end;
    inc(pat);
  end;
  fMatchCount := n;
  if n <> length(fMatch) then
    SetLength(fMatch, n);
end;



procedure SoundExComputeAnsi(var p: PAnsiChar; var result: cardinal;
  Values: PSoundExValues);
var
  n, v, old: PtrUInt;
begin
  n := 0;
  old := 0;
  if Values <> nil then
    repeat
      v := NormToUpperByte[ord(p^)]; // also handle 8-bit WinAnsi (1252 accents)
      if not (tcWord in TEXT_BYTES[v]) then
        break;
      inc(p);
      dec(v, ord('B'));
      if v > high(TSoundExValues) then
        continue;
      v := Values[v]; // get soundex value
      if (v = 0) or
         (v = old) then
        continue; // invalid or dopple value
      old := v;
      result := result shl SOUNDEX_BITS;
      inc(result, v);
      inc(n);
      if n = ((32 - 8) div SOUNDEX_BITS) then // first char use up to 8-bit
        break; // result up to a cardinal size
    until false;
end;

function SoundExComputeFirstCharAnsi(var p: PAnsiChar): cardinal;
label
  err;
begin
  if p = nil then
  begin
err:result := 0;
    exit;
  end;
  repeat
    result := NormToUpperByte[ord(p^)]; // also handle 8-bit WinAnsi (CP 1252)
    if result = 0 then
      goto err; // end of input text, without a word
    inc(p);
    // trim initial spaces or 'H'
  until AnsiChar(result) in ['A'..'G', 'I'..'Z'];
end;

procedure SoundExComputeUtf8(var U: PUtf8Char; var result: cardinal; Values: PSoundExValues);
var
  n, v, old: cardinal;
begin
  n := 0;
  old := 0;
  if Values <> nil then
    repeat
      v := GetNextUtf8Upper(U);
      if not (tcWord in TEXT_BYTES[v]) then
        break;
      dec(v, ord('B'));
      if v > high(TSoundExValues) then
        continue;
      v := Values[v]; // get soundex value
      if (v = 0) or
         (v = old) then
        continue; // invalid or dopple value
      old := v;
      result := result shl SOUNDEX_BITS;
      inc(result, v);
      inc(n);
      if n = ((32 - 8) div SOUNDEX_BITS) then // first char use up to 8-bit
        break; // result up to a cardinal size
    until false;
end;

function SoundExComputeFirstCharUtf8(var U: PUtf8Char): cardinal;
label
  err;
begin
  if U = nil then
  begin
err:result := 0;
    exit;
  end;
  repeat
    result := GetNextUtf8Upper(U);
    if result = 0 then
      goto err; // end of input text, without a word
    // trim initial spaces or 'H'
  until AnsiChar(result) in ['A'..'G', 'I'..'Z'];
end;


{ TSynSoundEx }

const
  /// english Soundex pronunciation scores
  // - defines the default values used for the SoundEx() function below
  // (used if Values parameter is nil)
  ValueEnglish: TSoundExValues =
  // B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z
    (1, 2, 3, 0, 1, 2, 0, 0, 2, 2, 4, 5, 5, 0, 1, 2, 6, 2, 3, 0, 1, 0, 2, 0, 2);

  /// french Soundex pronunciation scores
  // - can be used to override default values used for the SoundEx()
  // function below
  ValueFrench: TSoundExValues =
  // B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z
    (1, 2, 3, 0, 9, 7, 0, 0, 7, 2, 4, 5, 5, 0, 1, 2, 6, 8, 3, 0, 9, 0, 8, 0, 8);

  /// spanish Soundex pronunciation scores
  // - can be used to override default values used for the SoundEx()
  // function below
  ValueSpanish: TSoundExValues =
  // B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z
    (1, 2, 3, 0, 1, 2, 0, 0, 0, 2, 0, 5, 5, 0, 1, 2, 6, 2, 3, 0, 1, 0, 2, 0, 2);

  SOUNDEXVALUES: array[TSynSoundExPronunciation] of PSoundExValues = (
    @ValueEnglish,
    @ValueFrench,
    @ValueSpanish,
    @ValueEnglish);


function TSynSoundEx.Ansi(A: PAnsiChar): boolean;
var
  Value, c: cardinal;
begin
  result := false;
  if A = nil then
    exit;
  repeat
    // test beginning of word
    c := SoundExComputeFirstCharAnsi(A);
    if c = 0 then
      exit
    else if c = FirstChar then
    begin
      // here we had the first char match -> check if word match UpperValue
      Value := c - (ord('A') - 1);
      SoundExComputeAnsi(A, Value, fValues);
      if Value = search then
      begin
        result := true; // UpperValue found!
        exit;
      end;
    end
    else
      repeat
        if A^ = #0 then
          exit
        else if not (tcWord in TEXT_CHARS[NormToUpper[A^]]) then
          break
        else
          inc(A);
      until false;
    // find beginning of next word
    repeat
      if A^ = #0 then
        exit
      else if tcWord in TEXT_CHARS[NormToUpper[A^]] then
        break
      else
        inc(A);
    until false;
  until false;
end;

function TSynSoundEx.Utf8(U: PUtf8Char): boolean;
var
  Value, c: cardinal;
  V: PUtf8Char;
begin
  result := false;
  if U = nil then
    exit;
  repeat
    // find beginning of word
    c := SoundExComputeFirstCharUtf8(U);
    if c = 0 then
      exit
    else if c = FirstChar then
    begin
      // here we had the first char match -> check if word match UpperValue
      Value := c - (ord('A') - 1);
      SoundExComputeUtf8(U, Value, fValues);
      if Value = search then
      begin
        result := true; // UpperValue found!
        exit;
      end;
    end
    else
      repeat
        c := GetNextUtf8Upper(U);
        if c = 0 then
          exit;
      until not (tcWord in TEXT_BYTES[c]);
    // find beginning of next word
    repeat
      if U = nil then
        exit;
      V := U;
      c := GetNextUtf8Upper(U);
      if c = 0 then
        exit;
    until tcWord in TEXT_BYTES[c];
    U := V;
  until U = nil;
end;

function TSynSoundEx.Prepare(UpperValue: PAnsiChar; Lang: PSoundExValues): boolean;
begin
  fValues := Lang;
  Search := SoundExAnsi(UpperValue, nil, Lang);
  if Search = 0 then
    result := false
  else
  begin
    FirstChar := SoundExComputeFirstCharAnsi(UpperValue);
    result := true;
  end;
end;

function TSynSoundEx.Prepare(UpperValue: PAnsiChar;
  Lang: TSynSoundExPronunciation): boolean;
begin
  result := Prepare(UpperValue, SOUNDEXVALUES[Lang]);
end;

function SoundExAnsi(A: PAnsiChar; next: PPAnsiChar; Lang: PSoundExValues): cardinal;
begin
  result := SoundExComputeFirstCharAnsi(A);
  if result <> 0 then
  begin
    dec(result, ord('A') - 1);   // first Soundex char is first char
    SoundExComputeAnsi(A, result, Lang);
  end;
  if next <> nil then
  begin
    while tcWord in TEXT_CHARS[NormToUpper[A^]] do
      inc(A); // go to end of word
    next^ := A;
  end;
end;

function SoundExAnsi(A: PAnsiChar; next: PPAnsiChar;
  Lang: TSynSoundExPronunciation): cardinal;
begin
  result := SoundExAnsi(A, next, SOUNDEXVALUES[Lang]);
end;

function SoundExUtf8(U: PUtf8Char; next: PPUtf8Char;
  Lang: TSynSoundExPronunciation): cardinal;
begin
  result := SoundExComputeFirstCharUtf8(U);
  if result <> 0 then
  begin
    dec(result, ord('A') - 1);   // first Soundex char is first char
    SoundExComputeUtf8(U, result, SOUNDEXVALUES[Lang]);
  end;
  if next <> nil then
    next^ := FindNextUtf8WordBegin(U);
end;


{ ******************  Efficient CSV Parsing using RTTI }

function TDynArrayLoadCsv(var Value: TDynArray; Csv: PUtf8Char;
  Intern: TRawUtf8Interning): boolean;
var
  rt: TRttiCustom;
  pr: PRttiCustomProp;
  p, v: PUtf8Char;
  s: RawUtf8;
  mapcount, mapped: PtrInt;
  rec: PAnsiChar;
  map: PRttiCustomPropDynArray;
  m: ^PRttiCustomProp;
  extcount, mcount: integer;
  ext: PInteger;
begin
  result := false;
  rt := Value.Info.ArrayRtti;
  if (rt = nil) or
     (rt.Parser <> ptRecord) or
     (rt.Props.Count = 0) then
    exit;
  // parse the CSV headers
  mapped := 0;
  mapcount := 0;
  SetLength(map, 32);
  p := pointer(GetNextLine(Csv, Csv));
  if Csv = nil then
    exit; // no data
  while p <> nil do
  begin
    GetNextItem(p, ',', '"', s);
    if s = '' then
      exit; // we don't support void headers
    if mapcount = length(map) then
      SetLength(map, NextGrow(mapcount));
    pr := rt.Props.Find(s);
    if pr <> nil then
    begin
      map[mapcount] := pr; // found a matching field
      inc(mapped);
    end;
    inc(mapcount);
  end;
  if mapped = 0 then
    exit; // no field matching any header
  // parse the value rows
  extcount := 0;
  ext := Value.CountExternal;
  if ext = nil then
    Value.UseExternalCount(@extcount); // faster Value.NewPtr
  v := Csv;
  while v^ in [#10, #13] do
    inc(v);
  while v^ <> #0 do
  begin
    rec := Value.NewPtr;
    m := pointer(map);
    mcount := mapcount;
    repeat
      // parse next value
      Csv := v;
      if v^ = '"' then
        v := GotoEndOfQuotedString(v); // special handling of double quotes
      while (v^ <> ',') and
            (v^ > #13) do
        inc(v);
      if mcount <> 0 then
      begin
        if m^ <> nil then // not matching fields are just ignored
        begin
          if Csv^ = '"' then
          begin
            UnQuoteSqlStringVar(Csv, s);
            if Intern <> nil then
              Intern.UniqueText(s);
          end
          else
            Intern.Unique(s, Csv, v - Csv);
          m^.Value.ValueSetText(rec + m^.OffsetSet, s);
        end;
        inc(m);
        dec(mcount);
      end;
      if v^ <> ',' then
        break;
      inc(v);
    until v^ in [#0, #10, #13];
    // go to next row
    while v^ in [#10, #13] do
      inc(v);
  end;
  if Value.Count = 0 then
    Value.Capacity := 0
  else
    DynArrayFakeLength(Value.Value^, Value.Count);
  Value.UseExternalCount(ext); // restore fCountP if local n count was used
  result := true;
end;

function DynArrayLoadCsv(var Value; const Csv: RawUtf8; TypeInfo: PRttiInfo;
  Intern: TRawUtf8Interning): boolean;
var
  da: TDynArray;
begin
  da.Init(TypeInfo, Value);
  result := TDynArrayLoadCsv(da, pointer(CSV), Intern);
end;


{ ****************** Versatile Expression Search Engine }


function ToText(r: TExprParserResult): PShortString;
begin
  result := GetEnumName(TypeInfo(TExprParserResult), ord(r));
end;

function ToUtf8(r: TExprParserResult): RawUtf8;
begin
  result := UnCamelCase(TrimLeftLowerCaseShort(ToText(r)));
end;


{ TExprNode }

function TExprNode.Append(node: TExprNode): boolean;
begin
  result := node <> nil;
  if result then
    Last.fNext := node;
end;

constructor TExprNode.Create(nodeType: TExprNodeType);
begin
  inherited Create; // may have been overriden
  fNodeType := nodeType;
end;

destructor TExprNode.Destroy;
begin
  fNext.Free;
  inherited Destroy;
end;

function TExprNode.Last: TExprNode;
begin
  result := self;
  while result.Next <> nil do
    result := result.Next;
end;


{ TParserAbstract }

constructor TParserAbstract.Create;
begin
  inherited Create; // may have been overriden
  Initialize;
end;

destructor TParserAbstract.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TParserAbstract.Clear;
begin
  fWordCount := 0;
  fWords := nil;
  fExpression := '';
  FreeAndNil(fFirstNode);
end;

function TParserAbstract.ParseExpr: TExprNode;
begin
  result := ParseFactor;
  ParseNextCurrentWord;
  if (fCurrentWord = '') or
     (fCurrentWord = ')') then
    exit;
  if PropNameEquals(fCurrentWord, fAndWord) then
  begin
    // w1 & w2 = w1 AND w2
    ParseNextCurrentWord;
    if result.Append(ParseExpr) then
      result.Append(TExprNode.Create(entAnd));
    exit;
  end
  else if PropNameEquals(fCurrentWord, fOrWord) then
  begin
    // w1 + w2 = w1 OR w2
    ParseNextCurrentWord;
    if result.Append(ParseExpr) then
      result.Append(TExprNode.Create(entOr));
    exit;
  end
  else if fNoWordIsAnd and result.Append(ParseExpr) then
    // 'w1 w2' = 'w1 & w2'
    result.Append(TExprNode.Create(entAnd));
end;

function TParserAbstract.ParseFactor: TExprNode;
begin
  if fCurrentError <> eprSuccess then
    result := nil
  else if PropNameEquals(fCurrentWord, fNotWord) then
  begin
    ParseNextCurrentWord;
    result := ParseFactor;
    if fCurrentError <> eprSuccess then
      exit;
    result.Append(TExprNode.Create(entNot));
  end
  else
    result := ParseTerm;
end;

function TParserAbstract.ParseTerm: TExprNode;
begin
  result := nil;
  if fCurrentError <> eprSuccess then
    exit;
  if fCurrentWord = '(' then
  begin
    ParseNextCurrentWord;
    result := ParseExpr;
    if fCurrentError <> eprSuccess then
      exit;
    if fCurrentWord <> ')' then
    begin
      FreeAndNil(result);
      fCurrentError := eprMissingParenthesis;
    end;
  end
  else if fCurrentWord = '' then
  begin
    result := nil;
    fCurrentError := eprMissingFinalWord;
  end
  else
  try // calls meta-class overriden constructor
    result := fWordClass.Create(self, fCurrentWord);
    fCurrentError := TExprNodeWordAbstract(result).ParseWord;
    if fCurrentError <> eprSuccess then
    begin
      FreeAndNil(result);
      exit;
    end;
    SetLength(fWords, fWordCount + 1);
    fWords[fWordCount] := TExprNodeWordAbstract(result);
    inc(fWordCount);
  except
    FreeAndNil(result);
    fCurrentError := eprInvalidExpression;
  end;
end;

function TParserAbstract.Parse(const aExpression: RawUtf8): TExprParserResult;
var
  depth: integer;
  n: TExprNode;
begin
  Clear;
  fCurrentError := eprSuccess;
  fCurrent := pointer(aExpression);
  ParseNextCurrentWord;
  if fCurrentWord = '' then
  begin
    result := eprNoExpression;
    exit;
  end;
  fFirstNode := ParseExpr;
  result := fCurrentError;
  if result = eprSuccess then
  begin
    depth := 0;
    n := fFirstNode;
    while n <> nil do
    begin
      case n.NodeType of
        entWord:
          begin
            inc(depth);
            if depth > high(fFoundStack) then
            begin
              result := eprTooManyParenthesis;
              break;
            end;
          end;
        entOr, entAnd:
          dec(depth);
      end;
      n := n.Next;
    end;
  end;
  if result = eprSuccess then
    fExpression := aExpression
  else
    Clear;
  fCurrent := nil;
end;

class function TParserAbstract.ParseError(const aExpression: RawUtf8): RawUtf8;
var
  parser: TParserAbstract;
  res: TExprParserResult;
begin
  parser := Create;
  try
    res := parser.Parse(aExpression);
    if res = eprSuccess then
      result := ''
    else
      result := ToUtf8(res);
  finally
    parser.Free;
  end;
end;

function TParserAbstract.Execute: boolean;
var
  n: TExprNode;
  st: PBoolean;
begin
  // code below compiles very efficiently on FPC/x86-64
  st := @fFoundStack;
  n := fFirstNode;
  repeat
    case n.NodeType of
      entWord:
        begin
          st^ := TExprNodeWordAbstract(n).fFound;
          inc(st); // see eprTooManyParenthesis above to avoid buffer overflow
        end;
      entNot:
        PAnsiChar(st)[-1] := AnsiChar(ord(PAnsiChar(st)[-1]) xor 1);
      entOr:
        begin
          dec(st);
          PAnsiChar(st)[-1] := AnsiChar(st^ or boolean(PAnsiChar(st)[-1]));
        end; { TODO : optimize TExprParser OR when left member is already TRUE }
      entAnd:
        begin
          dec(st);
          PAnsiChar(st)[-1] := AnsiChar(st^ and boolean(PAnsiChar(st)[-1]));
        end;
    end;
    n := n.Next;
  until n = nil;
  result := boolean(PAnsiChar(st)[-1]);
end;


{ TExprParserAbstract }

procedure TExprParserAbstract.Initialize;
begin
  fAndWord := '&';
  fOrWord := '+';
  fNotWord := '-';
  fNoWordIsAnd := true;
end;

procedure TExprParserAbstract.ParseNextCurrentWord;
var
  P: PUtf8Char;
begin
  fCurrentWord := '';
  P := fCurrent;
  if P = nil then
    exit;
  while P^ in [#1..' '] do
    inc(P);
  if P^ = #0 then
    exit;
  if P^ in PARSER_STOPCHAR then
  begin
    FastSetString(fCurrentWord, P, 1);
    fCurrent := P + 1;
  end
  else
  begin
    fCurrent := P;
    ParseNextWord;
  end;
end;

procedure TExprParserAbstract.ParseNextWord;
const
  STOPCHAR = PARSER_STOPCHAR + [#0, ' '];
var
  P: PUtf8Char;
begin
  P := fCurrent;
  while not (P^ in STOPCHAR) do
    inc(P);
  FastSetString(fCurrentWord, fCurrent, P - fCurrent);
  fCurrent := P;
end;


{ TExprNodeWordAbstract }

constructor TExprNodeWordAbstract.Create(aOwner: TParserAbstract;
  const aWord: RawUtf8);
begin
  inherited Create(entWord);
  fWord := aWord;
  fOwner := aOwner;
end;


{ TExprParserMatchNode }

type
  TExprParserMatchNode = class(TExprNodeWordAbstract)
  protected
    fMatch: TMatch;
    function ParseWord: TExprParserResult; override;
  end;

function TExprParserMatchNode.ParseWord: TExprParserResult;
begin
  fMatch.Prepare(fWord, (fOwner as TExprParserMatch).fCaseSensitive, {reuse=}true);
  result := eprSuccess;
end;


{ TExprParserMatch }

var
  // equals 1 for ['0'..'9', 'A'..'Z', 'a'..'z', #$80..#$ff]
  ROUGH_UTF8: TAnsiCharToByte;

constructor TExprParserMatch.Create(aCaseSensitive: boolean);
var
  c: AnsiChar;
begin
  inherited Create;
  if ROUGH_UTF8['0'] = 0 then // ensure is initialized for Search()
    for c := low(c) to high(c) do
      if c in ['0'..'9', 'A'..'Z', 'a'..'z', #$80..#$ff] then
        ROUGH_UTF8[c] := 1;
  fCaseSensitive := aCaseSensitive;
end;

procedure TExprParserMatch.Initialize;
begin
  inherited Initialize;
  fWordClass := TExprParserMatchNode;
end;

function TExprParserMatch.Search(const aText: RawUtf8): boolean;
begin
  result := Search(pointer(aText), length(aText));
end;

function TExprParserMatch.Search(aText: PUtf8Char; aTextLen: PtrInt): boolean;
var
  P, PEnd: PUtf8Char;
  n: PtrInt;
  tab: PAnsiCharToByte;
begin
  P := aText;
  if (P = nil) or
     (fWords = nil) then
  begin
    result := false;
    exit;
  end;
  // reset any previous resultset
  if fMatchedLastSet > 0 then
  begin
    n := fWordCount;
    repeat
      dec(n);
      fWords[n].fFound := false;
    until n = 0;
    fMatchedLastSet := 0;
  end;
  PEnd := P + aTextLen;
  while (P < PEnd) and
        (fMatchedLastSet < fWordCount) do
  begin
    // recognize next word boudaries
    tab := @ROUGH_UTF8;
    while tab[P^] = 0 do
    begin
      inc(P);
      if P = PEnd then
        break;
    end;
    if P = PEnd then
      break;
    aText := P;
    repeat
      inc(P);
    until (P = PEnd) or
          (tab[P^] = 0);
    // apply the expression nodes to this word
    aTextLen := P - aText; // now aText/aTextLen point to a word
    n := fWordCount;
    repeat
      dec(n);
      with TExprParserMatchNode(fWords[n]) do
        if not fFound and
           fMatch.Match(aText, aTextLen) then
        begin
          fFound := true;
          inc(fMatchedLastSet);
        end;
    until n = 0;
  end;
  result := Execute;
end;



{ ****************** Bloom Filter Probabilistic Index }

{ TSynBloomFilter }

const
  BLOOM_VERSION = 0;
  BLOOM_MAXHASH = 32; // only 7 is needed for 1% false positive ratio

constructor TSynBloomFilter.Create;
begin
  fHasher := @crc32c; // default/standard/mORMot1 hash function
end;

constructor TSynBloomFilter.Create(aSize: integer;
  aFalsePositivePercent: double; aHasher: THasher);
const
  LN2 = 0.69314718056;
begin
  Create; // set fHasher := crc32c + may have been overriden
  if aSize < 0 then
    fSize := 1000
  else
    fSize := aSize;
  if aFalsePositivePercent <= 0 then
    fFalsePositivePercent := 1
  else if aFalsePositivePercent > 100 then
    fFalsePositivePercent := 100
  else
    fFalsePositivePercent := aFalsePositivePercent;
  if @aHasher <> nil then
    fHasher := aHasher;
  // see http://stackoverflow.com/a/22467497
  fBits := Round(-ln(fFalsePositivePercent / 100) * aSize / (LN2 * LN2));
  fHashFunctions := Round(fBits / fSize * LN2);
  if fHashFunctions = 0 then
    fHashFunctions := 1
  else if fHashFunctions > BLOOM_MAXHASH then
    fHashFunctions := BLOOM_MAXHASH;
  Reset;
end;

constructor TSynBloomFilter.Create(const aSaved: RawByteString;
  aMagic: cardinal; aHasher: THasher);
begin
  Create; // set fHasher := crc32c + may have been overriden
  if @aHasher <> nil then
    fHasher := aHasher;
  if not LoadFrom(aSaved, aMagic) then // will load fSize+fBits+fHashFunctions
    raise ESynException.CreateUtf8('%.Create with invalid aSaved content', [self]);
end;

procedure TSynBloomFilter.Insert(const aValue: RawByteString);
begin
  Insert(pointer(aValue), length(aValue));
end;

procedure TSynBloomFilter.Insert(aValue: pointer; aValueLen: integer);
var
  h: integer;
  h1, h2: cardinal; // https://goo.gl/Pls5wi
begin
  if (self = nil) or
     (aValueLen <= 0) or
     (fBits = 0) then
    exit;
  h1 := fHasher(0, aValue, aValueLen);
  if fHashFunctions = 1 then
    h2 := 0
  else
    h2 := fHasher(h1, aValue, aValueLen);
  fSafe.WriteLock;
  try
    for h := 0 to fHashFunctions - 1 do
    begin
      SetBitPtr(pointer(fStore), h1 mod fBits);
      inc(h1, h2);
    end;
    inc(fInserted);
  finally
    fSafe.WriteUnLock;
  end;
end;

function TSynBloomFilter.MayExist(const aValue: RawByteString): boolean;
begin
  result := MayExist(pointer(aValue), length(aValue));
end;

function TSynBloomFilter.MayExist(aValue: pointer; aValueLen: integer): boolean;
var
  h: integer;
  h1, h2: cardinal; // https://goo.gl/Pls5wi
begin
  result := false;
  if (self = nil) or
     (aValueLen <= 0) or
     (fBits = 0) then
    exit;
  h1 := fHasher(0, aValue, aValueLen);
  if fHashFunctions = 1 then
    h2 := 0
  else
    h2 := fHasher(h1, aValue, aValueLen);
  fSafe.ReadOnlyLock; // allow concurrent reads
  try
    for h := 0 to fHashFunctions - 1 do
      if GetBitPtr(pointer(fStore), h1 mod fBits) then
        inc(h1, h2)
      else
        exit;
  finally
    fSafe.ReadOnlyUnLock;
  end;
  result := true;
end;

procedure TSynBloomFilter.Reset;
begin
  fSafe.WriteLock;
  try
    if fStore = '' then
      SetLength(fStore, (fBits shr 3) + 1);
    FillcharFast(pointer(fStore)^, length(fStore), 0);
    fInserted := 0;
  finally
    fSafe.WriteUnLock;
  end;
end;

function TSynBloomFilter.SaveTo(aMagic: cardinal): RawByteString;
var
  W: TBufferWriter;
  BufLen: integer;
  temp: array[word] of byte;
begin
  BufLen := length(fStore) + 100;
  if BufLen <= SizeOf(temp) then
    W := TBufferWriter.Create(TRawByteStringStream, @temp, SizeOf(temp))
  else
    W := TBufferWriter.Create(TRawByteStringStream, BufLen);
  try
    SaveTo(W, aMagic);
    W.Flush;
    result := TRawByteStringStream(W.Stream).DataString;
  finally
    W.Free;
  end;
end;

procedure TSynBloomFilter.SaveTo(aDest: TBufferWriter; aMagic: cardinal);
begin
  aDest.Write4(aMagic);
  aDest.Write1(BLOOM_VERSION);
  fSafe.ReadOnlyLock;
  try
    aDest.Write8(@fFalsePositivePercent);
    aDest.Write4(fSize);
    aDest.Write4(fBits);
    aDest.Write1(fHashFunctions);
    aDest.Write4(fInserted);
    // warning: fHasher is NOT persisted yet
    ZeroCompress(pointer(fStore), Length(fStore), aDest);
  finally
    fSafe.ReadOnlyUnLock;
  end;
end;

function TSynBloomFilter.LoadFrom(const aSaved: RawByteString; aMagic: cardinal): boolean;
begin
  result := LoadFrom(pointer(aSaved), length(aSaved));
end;

function TSynBloomFilter.LoadFrom(P: PByte; PLen: integer; aMagic: cardinal): boolean;
var
  start: PByte;
  version: integer;
begin
  result := false;
  start := P;
  if (P = nil) or
     (PLen < 32) or
     (PCardinal(P)^ <> aMagic) then
    exit;
  inc(P, 4);
  version := P^;
  inc(P);
  if version > BLOOM_VERSION then
    exit;
  fSafe.WriteLock;
  try
    fFalsePositivePercent := unaligned(PDouble(P)^);
    inc(P, 8);
    if (fFalsePositivePercent <= 0) or
       (fFalsePositivePercent > 100) then
      exit;
    fSize := PCardinal(P)^;
    inc(P, 4);
    fBits := PCardinal(P)^;
    inc(P, 4);
    if fBits < fSize then
      exit;
    fHashFunctions := P^;
    inc(P);
    if fHashFunctions - 1 >= BLOOM_MAXHASH then
      exit;
    Reset;
    fInserted := PCardinal(P)^;
    inc(P, 4);
    ZeroDecompress(P, PLen - (PAnsiChar(P) - PAnsiChar(start)), fStore);
    result := length(fStore) = integer(fBits shr 3) + 1;
  finally
    fSafe.WriteUnLock;
  end;
end;


{ TSynBloomFilterDiff }

type
  TBloomDiffHeaderKind = (bdDiff, bdFull, bdUpToDate);
  TBloomDiffHeader = packed record
    kind: TBloomDiffHeaderKind;
    size: cardinal;
    inserted: cardinal;
    revision: Int64;
    crc: cardinal;
  end;

procedure TSynBloomFilterDiff.Insert(aValue: pointer; aValueLen: integer);
begin
  fSafe.WriteLock;
  try
    inherited Insert(aValue, aValueLen);
    inc(fRevision);
    inc(fSnapshotInsertCount);
  finally
    fSafe.WriteUnLock;
  end;
end;

procedure TSynBloomFilterDiff.Reset;
begin
  fSafe.WriteLock;
  try
    inherited Reset;
    fSnapshotAfterInsertCount := fSize shr 5;
    fSnapShotAfterMinutes := 30;
    fSnapshotTimestamp := 0;
    fSnapshotInsertCount := 0;
    fRevision := UnixTimeUtc shl 31;
    fKnownRevision := 0;
    fKnownStore := '';
  finally
    fSafe.WriteUnLock;
  end;
end;

procedure TSynBloomFilterDiff.DiffSnapshot;
begin
  fSafe.WriteLock;
  try
    fKnownRevision := fRevision;
    fSnapshotInsertCount := 0;
    FastSetRawByteString(fKnownStore, pointer(fStore), length(fStore));
    if fSnapShotAfterMinutes = 0 then
      fSnapshotTimestamp := 0
    else
      fSnapshotTimestamp := GetTickCount64 + fSnapShotAfterMinutes * 60000;
  finally
    fSafe.WriteUnLock;
  end;
end;

function TSynBloomFilterDiff.SaveToDiff(const aKnownRevision: Int64): RawByteString;
var
  head: TBloomDiffHeader;
  W: TBufferWriter;
  temp: array[word] of byte;
begin
  fSafe.ReadWriteLock; // DiffSnapshot makes a WriteLock
  try
    if aKnownRevision = fRevision then
      head.kind := bdUpToDate
    else if (fKnownRevision = 0) or
            (fSnapshotInsertCount > fSnapshotAfterInsertCount) or
            ((fSnapshotInsertCount > 0) and
             (fSnapshotTimestamp <> 0) and
             (GetTickCount64 > fSnapshotTimestamp)) then
    begin
      DiffSnapshot;
      head.kind := bdFull;
    end
    else if (aKnownRevision < fKnownRevision) or
            (aKnownRevision > fRevision) then
      head.kind := bdFull
    else
      head.kind := bdDiff;
    head.size := length(fStore);
    head.inserted := fInserted;
    head.revision := fRevision;
    head.crc := fHasher(0, @head, SizeOf(head) - SizeOf(head.crc));
    if head.kind = bdUpToDate then
    begin
      FastSetRawByteString(result, @head, SizeOf(head));
      exit;
    end;
    if head.size + 100 <= SizeOf(temp) then
      W := TBufferWriter.Create(TRawByteStringStream, @temp, SizeOf(temp))
    else
      W := TBufferWriter.Create(TRawByteStringStream, head.size + 100);
    try
      W.Write(@head, SizeOf(head));
      case head.kind of
        bdFull:
          SaveTo(W);
        bdDiff:
          ZeroCompressXor(pointer(fStore), pointer(fKnownStore), head.size, W);
      end;
      result := W.FlushTo;
    finally
      W.Free;
    end;
  finally
    fSafe.ReadWriteUnLock;
  end;
end;

function TSynBloomFilterDiff.DiffKnownRevision(const aDiff: RawByteString): Int64;
var
  head: ^TBloomDiffHeader absolute aDiff;
begin
  if (length(aDiff) < SizeOf(head^)) or
     (head.kind > high(TBloomDiffHeaderKind)) or
     (head.size <> cardinal(length(fStore))) or
     (head.crc <> fHasher(0, pointer(head), SizeOf(head^) - SizeOf(head.crc))) then
    result := 0
  else
    result := head.Revision;
end;

function TSynBloomFilterDiff.LoadFromDiff(const aDiff: RawByteString): boolean;
var
  head: ^TBloomDiffHeader absolute aDiff;
  P: PByte;
  PLen: integer;
begin
  result := false;
  P := pointer(aDiff);
  PLen := length(aDiff);
  if (PLen < SizeOf(head^)) or
     (head.kind > high(head.kind)) or
     (head.crc <> fHasher(0, pointer(head), SizeOf(head^) - SizeOf(head.crc))) then
    exit;
  if (fStore <> '') and
     (head.size <> cardinal(length(fStore))) then
    exit;
  inc(P, SizeOf(head^));
  dec(PLen, SizeOf(head^));
  fSafe.WriteLock;
  try
    case head.kind of
      bdFull:
        result := LoadFrom(P, PLen);
      bdDiff:
        if fStore <> '' then
          result := ZeroDecompressOr(pointer(P), Pointer(fStore), PLen, head.size);
      bdUpToDate:
        result := true;
    end;
    if result then
    begin
      fRevision := head.revision;
      fInserted := head.inserted;
    end;
  finally
    fSafe.WriteUnLock;
  end;
end;


procedure ZeroCompress(P: PAnsiChar; Len: integer; Dest: TBufferWriter);
var
  PEnd, beg, zero: PAnsiChar;
  crc: cardinal;
begin
  Dest.WriteVarUInt32(Len);
  PEnd := P + Len;
  beg := P;
  crc := 0;
  while P < PEnd do
  begin
    while (P^ <> #0) and
          (P < PEnd) do
      inc(P);
    zero := P;
    while (P^ = #0) and
          (P < PEnd) do
      inc(P);
    if P - zero > 3 then
    begin
      Len := zero - beg;
      crc := crc32c(crc, beg, Len);
      Dest.WriteVarUInt32(Len);
      Dest.Write(beg, Len);
      Len := P - zero;
      crc := crc32c(crc, @Len, SizeOf(Len));
      Dest.WriteVarUInt32(Len - 3);
      beg := P;
    end;
  end;
  Len := P - beg;
  if Len > 0 then
  begin
    crc := crc32c(crc, beg, Len);
    Dest.WriteVarUInt32(Len);
    Dest.Write(beg, Len);
  end;
  Dest.Write4(crc);
end;

procedure ZeroCompressXor(New, Old: PAnsiChar; Len: cardinal; Dest: TBufferWriter);
var
  beg, same, index, crc, L: cardinal;
begin
  Dest.WriteVarUInt32(Len);
  beg := 0;
  index := 0;
  crc := 0;
  while index < Len do
  begin
    while (New[index] <> Old[index]) and
          (index < Len) do
      inc(index);
    same := index;
    while (New[index] = Old[index]) and
          (index < Len) do
      inc(index);
    L := index - same;
    if L > 3 then
    begin
      Dest.WriteVarUInt32(same - beg);
      Dest.WriteXor(New + beg, Old + beg, same - beg, @crc);
      crc := crc32c(crc, @L, SizeOf(L));
      Dest.WriteVarUInt32(L - 3);
      beg := index;
    end;
  end;
  L := index - beg;
  if L > 0 then
  begin
    Dest.WriteVarUInt32(L);
    Dest.WriteXor(New + beg, Old + beg, L, @crc);
  end;
  Dest.Write4(crc);
end;

procedure ZeroDecompress(P: PByte; Len: integer; var Dest: RawByteString);
var
  PEnd, D, DEnd: PAnsiChar;
  DestLen, crc: cardinal;
begin
  PEnd := PAnsiChar(P) + Len - 4;
  DestLen := FromVarUInt32(P);
  FastNewRawByteString(Dest, DestLen);
  D := pointer(Dest);
  DEnd := D + DestLen;
  crc := 0;
  while PAnsiChar(P) < PEnd do
  begin
    Len := FromVarUInt32(P);
    if D + Len > DEnd then
      break;
    MoveFast(P^, D^, Len);
    crc := crc32c(crc, D, Len);
    inc(P, Len);
    inc(D, Len);
    if PAnsiChar(P) >= PEnd then
      break;
    Len := FromVarUInt32(P) + 3;
    if D + Len > DEnd then
      break;
    FillCharFast(D^, Len, 0);
    crc := crc32c(crc, @Len, SizeOf(Len));
    inc(D, Len);
  end;
  if crc <> PCardinal(P)^ then
    Dest := '';
end;

function ZeroDecompressOr(P, Dest: PAnsiChar; Len, DestLen: integer): boolean;
var
  PEnd, DEnd: PAnsiChar;
  crc: cardinal;
begin
  PEnd := P + Len - 4;
  if cardinal(DestLen) <> FromVarUInt32(PByte(P)) then
  begin
    result := false;
    exit;
  end;
  DEnd := Dest + DestLen;
  crc := 0;
  while (P < PEnd) and
        (Dest < DEnd) do
  begin
    Len := FromVarUInt32(PByte(P));
    if Dest + Len > DEnd then
      break;
    crc := crc32c(crc, P, Len);
    OrMemory(pointer(Dest), pointer(P), Len);
    inc(P, Len);
    inc(Dest, Len);
    if P >= PEnd then
      break;
    Len := FromVarUInt32(PByte(P)) + 3;
    crc := crc32c(crc, @Len, SizeOf(Len));
    inc(Dest, Len);
  end;
  result := crc = PCardinal(P)^;
end;


{ ****************** Binary Buffers Delta Compression }

function Max(a, b: PtrInt): PtrInt; {$ifdef HASINLINE}inline;{$endif}
begin
  if a > b then
    result := a
  else
    result := b;
end;

function Min(a, b: PtrInt): PtrInt; {$ifdef HASINLINE}inline;{$endif}
begin
  if a < b then
    result := a
  else
    result := b;
end;

{$ifdef HASINLINE}
function Comp(a, b: PAnsiChar; len: PtrInt): PtrInt; inline;
var
  lenptr: PtrInt;
begin
  result := 0;
  lenptr := len - SizeOf(PtrInt);
  if lenptr >= 0 then
    repeat
      if PPtrInt(a + result)^ <> PPtrInt(b + result)^ then
        break;
      inc(result, SizeOf(PtrInt));
    until result > lenptr;
  if result < len then
    repeat
      if a[result] <> b[result] then
        exit;
      inc(result);
    until result = len;
end;
{$else} // eax = a, edx = b, ecx = len
function Comp(a, b: PAnsiChar; len: PtrInt): PtrInt;
asm // the 'rep cmpsb' version is slower on Intel Core CPU (not AMD)
        or      ecx, ecx
        push    ebx
        push    ecx
        jz      @ok

@1:     mov     bx, [eax]
        lea     eax, [eax + 2]
        cmp     bl, [edx]
        jne     @ok
        dec     ecx
        jz      @ok
        cmp     bh, [edx + 1]
        lea     edx, [edx + 2]
        jne     @ok
        dec     ecx
        jnz     @1

@ok:    pop     eax
        sub     eax, ecx
        pop     ebx
end;
{$endif HASINLINE}

function CompReverse(a, b: pointer; len: PtrInt): PtrInt;
begin
  result := 0;
  if len > 0 then
    repeat
      if PByteArray(a)[-result] <> PByteArray(b)[-result] then
        exit;
      inc(result);
    until result = len;
end;

function WriteCurOfs(curofs, curlen, curofssize: integer; sp: PAnsiChar): PAnsiChar;
begin
  if curlen = 0 then
  begin
    sp^ := #0;
    inc(sp);
  end
  else
  begin
    sp := Pointer(ToVarUInt32(curlen, PByte(sp)));
    PInteger(sp)^ := curofs;
    inc(sp, curofssize);
  end;
  result := sp;
end;

{$ifdef CPUINTEL}
// crc32c SSE4.2 hardware accellerated dword hash
{$ifdef CPUX86}
function crc32c32sse42(buf: pointer): cardinal;
{$ifdef FPC} nostackframe; assembler; {$endif}
asm
        mov     edx, eax
        xor     eax, eax
        {$ifdef HASAESNI}
        crc32   eax, dword ptr [edx]
        {$else}
        db $F2, $0F, $38, $F1, $02
        {$endif HASAESNI}
end;
{$else}
function crc32c32sse42(buf: pointer): cardinal;
{$ifdef FPC}nostackframe; assembler; asm {$else}
asm // ecx=buf (Linux: edi=buf)
        .noframe
{$endif FPC}
        xor     eax, eax
        crc32   eax, dword ptr [buf]
end;
{$endif CPUX86}
{$endif CPUINTEL}

function hash32prime(buf: pointer): cardinal;
begin
  // inlined xxHash32Mixup - won't pollute L1 cache with crc lookup tables
  result := PCardinal(buf)^;
  result := result xor (result shr 15);
  result := result * 2246822519;
  result := result xor (result shr 13);
  result := result * 3266489917;
  result := result xor (result shr 16);
end;

const
  HTabBits = 18; // fits well with DeltaCompress(..,BufSize=2MB)
  HTabMask = (1 shl HTabBits) - 1; // =$3ffff
  HListMask = $ffffff; // HTab[]=($ff,$ff,$ff)

type
  PHTab = ^THTab; // HTabBits=18 -> SizeOf=767KB
  THTab = packed array[0..HTabMask] of array[0..2] of byte;

function DeltaCompute(NewBuf, OldBuf, OutBuf, WorkBuf: PAnsiChar;
  NewBufSize, OldBufSize, MaxLevel: PtrInt; HList, HTab: PHTab): PAnsiChar;
var
  i, curofs, curlen, curlevel, match, curofssize, h, oldh: PtrInt;
  sp, pInBuf, pOut: PAnsiChar;
  ofs: cardinal;
  spb: PByte absolute sp;
  hash: function(buf: pointer): cardinal;
begin
  // 1. fill HTab[] with hashes for all old data
  {$ifdef CPUINTEL}
  if cfSSE42 in CpuFeatures then
    hash := @crc32c32sse42
  else
  {$endif CPUINTEL}
    hash := @hash32prime;
  FillCharFast(HTab^, SizeOf(HTab^), $ff); // HTab[]=HListMask by default
  pInBuf := OldBuf;
  oldh := -1; // force calculate first hash
  sp := pointer(HList);
  for i := 0 to OldBufSize - 3 do
  begin
    h := hash(pInBuf) and HTabMask;
    inc(pInBuf);
    if h = oldh then
      continue;
    oldh := h;
    h := PtrInt(@HTab^[h]); // fast 24-bit data process
    PCardinal(sp)^ := PCardinal(h)^;
    PCardinal(h)^ := cardinal(i) or (PCardinal(h)^ and $ff000000);
    inc(sp, 3);
  end;
  // 2. compression init
  if OldBufSize <= $ffff then
    curofssize := 2
  else
    curofssize := 3;
  curlen := -1;
  curofs := 0;
  pOut := OutBuf + 7;
  sp := WorkBuf;
  // 3. handle identical leading bytes
  match := Comp(OldBuf, NewBuf, Min(OldBufSize, NewBufSize));
  if match > 2 then
  begin
    sp := WriteCurOfs(0, match, curofssize, sp);
    sp^ := #0;
    inc(sp);
    inc(NewBuf, match);
    dec(NewBufSize, match);
  end;
  pInBuf := NewBuf;
  // 4. main loop: identify longest sequences using hash, and store reference
  if NewBufSize >= 8 then
    repeat
      // hash 4 next bytes from NewBuf, and find longest match in OldBuf
      ofs := PCardinal(@HTab^[hash(NewBuf) and HTabMask])^ and HListMask;
      if ofs <> HListMask then
      begin
        // brute force search loop of best hash match
        curlevel := MaxLevel;
        repeat
          with PHash128Rec(OldBuf + ofs)^ do
            // always test 8 bytes at once
            {$ifdef CPU64}
            if PHash128Rec(NewBuf)^.Lo = Lo then
            {$else}
            if (PHash128Rec(NewBuf)^.c0 = c0) and
               (PHash128Rec(NewBuf)^.c1 = c1) then
            {$endif CPU64}
            begin
              // test remaining bytes
              match := Comp(@PHash128Rec(NewBuf)^.c2, @c2,
                         Min(PtrUInt(OldBufSize) - ofs, NewBufSize) - 8);
              if match > curlen then
              begin
                // found a longer sequence
                curlen := match;
                curofs := ofs;
              end;
            end;
          dec(curlevel);
          ofs := PCardinal(@HList^[ofs])^ and HListMask;
        until (ofs = HListMask) or
              (curlevel = 0);
      end;
      // curlen = longest sequence length
      if curlen < 0 then
      begin
       // no sequence found -> copy one byte
        dec(NewBufSize);
        pOut^ := NewBuf^;
        inc(NewBuf);
        inc(pOut);
        if NewBufSize > 8 then // >=8 may overflow
          continue
        else
          break;
      end;
      inc(curlen, 8);
      sp := WriteCurOfs(curofs, curlen, curofssize, sp);
      spb := ToVarUInt32(NewBuf - pInBuf, spb);
      inc(NewBuf, curlen); // continue to search after the sequence
      dec(NewBufSize, curlen);
      curlen := -1;
      pInBuf := NewBuf;
      if NewBufSize > 8 then // >=8 may overflow
        continue
      else
        break;
    until false;
  // 5. write remaining bytes
  if NewBufSize > 0 then
  begin
    MoveFast(NewBuf^, pOut^, NewBufSize);
    inc(pOut, NewBufSize);
    inc(NewBuf, NewBufSize);
  end;
  sp^ := #0;
  inc(sp);
  spb := ToVarUInt32(NewBuf - pInBuf, spb);
  // 6. write header
  PInteger(OutBuf)^ := pOut - OutBuf - 7;
  h := sp - WorkBuf;
  PInteger(OutBuf + 3)^ := h;
  OutBuf[6] := AnsiChar(curofssize);
  // 7. copy commands
  MoveFast(WorkBuf^, pOut^, h);
  result := pOut + h;
end;

function ExtractBuf(GoodCRC: cardinal; p: PAnsiChar;
  var aUpd, Delta: PAnsiChar; Old: PAnsiChar): TDeltaError;
var
  pEnd, buf, upd, src: PAnsiChar;
  bufsize, datasize, leading, srclen: PtrUInt;
  curofssize: byte;
begin
  // 1. decompression init
  upd := aUpd;
  bufsize := PCardinal(p)^ and $00ffffff;
  inc(p, 3);
  datasize := PCardinal(p)^ and $00ffffff;
  inc(p, 3);
  curofssize := ord(p^);
  inc(p);
  buf := p;
  inc(p, bufsize);
  pEnd := p + datasize;
  src := nil;
  // 2. main loop
  while p < pEnd do
  begin
    // src/srclen = sequence to be copied
    srclen := FromVarUInt32(PByte(p));
    if srclen > 0 then
      if curofssize = 2 then
      begin
        src := Old + PWord(p)^;
        inc(p, 2);
      end
      else
      begin
        src := Old + PCardinal(p)^ and $00ffffff;
        inc(p, 3);
      end;
    // copy leading bytes
    leading := FromVarUInt32(PByte(p));
    if leading <> 0 then
    begin
      MoveFast(buf^, upd^, leading);
      inc(buf, leading);
      inc(upd, leading);
    end;
    // copy sequence
    if srclen <> 0 then
    begin
      if PtrUInt(upd - src) < srclen then
        MoveByOne(src, upd, srclen)
      else
        MoveFast(src^, upd^, srclen);
      inc(upd, srclen);
    end;
  end;
  // 3. result check
  Delta := p;
  if (p = pEnd) and
     (crc32c(0, aUpd, upd - aUpd) = GoodCRC) then
    // whole CRC is faster than incremental
    result := dsSuccess
  else
    result := dsCrcExtract;
  aUpd := upd;
end;

procedure WriteByte(var P: PAnsiChar; V: byte);
  {$ifdef HASINLINE}inline;{$endif}
begin
  PByte(P)^ := V;
  inc(P);
end;

procedure WriteInt(var P: PAnsiChar; V: cardinal);
  {$ifdef HASINLINE}inline;{$endif}
begin
  PCardinal(P)^ := V;
  inc(P, 4);
end;

const
  FLAG_COPIED = 0;
  FLAG_COMPRESS = 1;
  FLAG_BEGIN = 2;
  FLAG_END = 3;

function DeltaCompress(New, Old: PAnsiChar; NewSize, OldSize: integer;
  out Delta: PAnsiChar; Level, BufSize: integer): integer;
var
  HTab, HList: PHTab;
  d, workbuf: PAnsiChar;
  db: PByte absolute d;
  BufRead, OldRead, Trailing, NewSizeSave: PtrInt;
  bigfile: boolean;

  procedure CreateCopied;
  begin
    Getmem(Delta, NewSizeSave + 17);  // 17 = 4*integer + 1*byte
    d := Delta;
    db := ToVarUInt32(0, ToVarUInt32(NewSizeSave, db));
    WriteByte(d, FLAG_COPIED); // block copied flag
    db := ToVarUInt32(NewSizeSave, db);
    WriteInt(d, crc32c(0, New, NewSizeSave));
    MoveFast(New^, d^, NewSizeSave);
    inc(d, NewSizeSave);
    result := d - Delta;
  end;

begin
  // 1. special cases
  if (NewSize = OldSize) and
     mormot.core.base.CompareMem(Old, New, NewSize) then
  begin
    Getmem(Delta, 1);
    Delta^ := '=';
    result := 1;
    exit;
  end;
  NewSizeSave := NewSize;
  if OldSize = 0 then
  begin
    // Delta from nothing -> direct copy of whole block
    CreateCopied;
    exit;
  end;
  // 2. compression init
  bigfile := OldSize > BufSize;
  if BufSize > NewSize then
    BufSize := NewSize;
  if BufSize > HListMask then
    BufSize := HListMask; // we store offsets with 2..3 bytes -> max 16MB chunk
  Trailing := 0;
  Getmem(workbuf, BufSize); // compression temporary buffers
  Getmem(HList, BufSize * SizeOf({%H-}HList[0]));
  Getmem(HTab, SizeOf({%H-}HTab^));
  Getmem(Delta, Max(NewSize, OldSize) + 4096); // Delta size max evalulation
  try
    d := Delta;
    db := ToVarUInt32(NewSize, db); // Destination Size
    // 3. handle leading and trailing identical bytes (for biggest files)
    if bigfile then
    begin
      // test initial same chars
      BufRead := Comp(New, Old, Min(NewSize, OldSize));
      if BufRead > 9 then
      begin
        // it happens very often: modification is usually in the middle/end
        db := ToVarUInt32(BufRead, db); // blockSize = Size BufIdem
        WriteByte(d, FLAG_BEGIN);
        WriteInt(d, crc32c(0, New, BufRead));
        inc(New, BufRead);
        dec(NewSize, BufRead);
        inc(Old, BufRead);
        dec(OldSize, BufRead);
      end;
      // test trailing same chars
      BufRead := CompReverse(New + NewSize - 1, Old + OldSize - 1,
        Min(NewSize, OldSize));
      if BufRead > 5 then
      begin
        if NewSize = BufRead then
          dec(BufRead); // avoid block overflow
        dec(OldSize, BufRead);
        dec(NewSize, BufRead);
        Trailing := BufRead;
      end;
    end;
    // 4. main loop
    repeat
      BufRead := Min(BufSize, NewSize);
      dec(NewSize, BufRead);
      if (BufRead = 0) and
         (Trailing > 0) then
      begin
        db := ToVarUInt32(Trailing, db);
        WriteByte(d, FLAG_END); // block idem end flag
        WriteInt(d, crc32c(0, New, Trailing));
        break;
      end;
      OldRead := Min(BufSize, OldSize);
      dec(OldSize, OldRead);
      db := ToVarUInt32(OldRead, db);
      if (BufRead < 4) or
         (OldRead < 4) or
         (BufRead shr 2 > OldRead) then
      begin
        WriteByte(d, FLAG_COPIED); // block copied flag
        db := ToVarUInt32(BufRead, db);
        if BufRead = 0 then
          break;
        WriteInt(d, crc32c(0, New, BufRead));
        MoveFast(New^, d^, BufRead);
        inc(New, BufRead);
        inc(d, BufRead);
      end
      else
      begin
        WriteByte(d, FLAG_COMPRESS); // block compressed flag
        WriteInt(d, crc32c(0, New, BufRead));
        WriteInt(d, crc32c(0, Old, OldRead));
        d := DeltaCompute(New, Old, d, workbuf, BufRead, OldRead, Level, HList, HTab);
        inc(New, BufRead);
        inc(Old, OldRead);
      end;
    until false;
  // 5. release temp memory
  finally
    result := d - Delta;
    Freemem(HTab);
    Freemem(HList);
    Freemem(workbuf);
  end;
  if result >= NewSizeSave + 17 then
  begin
    // Delta didn't compress well -> store it (with up to 17 bytes overhead)
    Freemem(Delta);
    CreateCopied;
  end;
end;

function DeltaCompress(const New, Old: RawByteString;
  Level, BufSize: integer): RawByteString;
begin
  result := DeltaCompress(pointer(New), pointer(Old),
    length(New), length(Old), Level, BufSize);
end;

function DeltaCompress(New, Old: PAnsiChar; NewSize, OldSize,
  Level, BufSize: integer): RawByteString;
var
  Delta: PAnsiChar;
  DeltaLen: integer;
begin
  DeltaLen := DeltaCompress(New, Old, NewSize, OldSize, Delta, Level, BufSize);
  FastSetRawByteString(result, Delta, DeltaLen);
  Freemem(Delta);
end;

function DeltaExtract(Delta, Old, New: PAnsiChar): TDeltaError;
var
  BufCRC: cardinal;
  Code: byte;
  Len, BufRead, OldRead: PtrInt;
  db: PByte absolute Delta;
  Upd: PAnsiChar;
begin
  result := dsSuccess;
  Len := FromVarUInt32(db);
  Upd := New;
  repeat
    OldRead := FromVarUInt32(db);
    Code := db^;
    inc(db);
    case Code of
      FLAG_COPIED:
        begin
          // block copied flag - copy new from Delta
          BufRead := FromVarUInt32(db);
          if BufRead = 0 then
            break;
          if crc32c(0, Delta + 4, BufRead) <> PCardinal(Delta)^ then
          begin
            result := dsCrcCopy;
            exit;
          end;
          inc(Delta, 4);
          MoveFast(Delta^, New^, BufRead);
          if BufRead >= Len then
            exit; // if Old=nil -> only copy new
          inc(Delta, BufRead);
          inc(New, BufRead);
        end;
      FLAG_COMPRESS:
        begin
          // block compressed flag - extract Delta from Old
          BufCRC := PCardinal(Delta)^;
          inc(Delta, 4);
          if crc32c(0, Old, OldRead) <> PCardinal(Delta)^ then
          begin
            result := dsCrcComp;
            exit;
          end;
          inc(Delta, 4);
          result := ExtractBuf(BufCRC, Delta, New, Delta, Old);
          if result <> dsSuccess then
            exit;
        end;
      FLAG_BEGIN:
        begin
          // block idem begin flag
          if crc32c(0, Old, OldRead) <> PCardinal(Delta)^ then
          begin
            result := dsCrcBegin;
            exit;
          end;
          inc(Delta, 4);
          MoveFast(Old^, New^, OldRead);
          inc(New, OldRead);
        end;
      FLAG_END:
        begin
          // block idem end flag
          if crc32c(0, Old, OldRead) <> PCardinal(Delta)^ then
            result := dsCrcEnd;
          MoveFast(Old^, New^, OldRead);
          inc(New, OldRead);
          break;
        end;
    else
      begin
        result := dsFlag;
        exit;
      end;
    end; // Case Code of
    inc(Old, OldRead);
  until false;
  if New - Upd <> Len then
    result := dsLen;
end;

function DeltaExtract(const Delta, Old: RawByteString;
  out New: RawByteString): TDeltaError;
begin
  if (Delta = '') or
     (Delta = '=') then
  begin
    New := Old;
    result := dsSuccess;
  end
  else
  begin
    SetLength(New, DeltaExtractSize(pointer(Delta)));
    result := DeltaExtract(pointer(Delta), pointer(Old), pointer(New));
  end;
end;

function DeltaExtractSize(const Delta: RawByteString): integer;
begin
  result := DeltaExtractSize(pointer(Delta));
end;

function DeltaExtractSize(Delta: PAnsiChar): integer;
begin
  if Delta = nil then
    result := 0
  else
    result := FromVarUInt32(PByte(Delta));
end;

function ToText(err: TDeltaError): PShortString;
begin
  result := GetEnumName(TypeInfo(TDeltaError), ord(err));
end;


{ ****************** TDynArray Low-Level Binary Search }

function SimpleDynArrayLoadFrom(Source: PAnsiChar; aTypeInfo: PRttiInfo;
  out Count, ElemSize: PtrInt): pointer;
var
  Hash: PCardinalArray absolute Source;
  iteminfo: PRttiInfo;
begin
  result := nil;
  if (aTypeInfo = nil) or
     (aTypeInfo^.Kind <> rkDynArray) then
    exit;
  iteminfo := aTypeInfo^.DynArrayItemType(ElemSize);
  if (iteminfo <> nil) or
     (Source = nil) or
     // (Source[0] <> AnsiChar(ElemSize)) or mORMot 2 stores elemsize=0
     (Source[1] <> #0) then
    exit; // invalid type information or Source content
  inc(Source,2);
  Count := FromVarUInt32(PByte(Source)); // dynamic array count
  if Count <> 0 then
    result := @Hash[1]; // returns valid Source content
end;

function IntegerDynArrayLoadFrom(Source: PAnsiChar;
  var Count: integer): PIntegerArray;
var
  Hash: PCardinalArray absolute Source;
begin
  result := nil;
  if (Source = nil) or
     // (Source[0] <> #4) or mORMot 2 stores elemsize=0
     (Source[1] <> #0) then
    exit; // invalid Source content
  inc(Source, 2);
  Count := FromVarUInt32(PByte(Source)); // dynamic array count
  if Count <> 0 then
    result := @Hash[1]; // returns valid Source content
end;

function RawUtf8DynArrayLoadFromContains(Source: PAnsiChar;
  Value: PUtf8Char; ValueLen: PtrInt; CaseSensitive: boolean): PtrInt;
var
  Count, Len: PtrInt;
begin
  if (Value = nil) or
     (ValueLen = 0) or
     (Source = nil) then
     // (Source[0] <> AnsiChar(SizeOf(PtrInt))) mORMot 2 stores elemsize=0
     // {$ifdef ISDELPHI} or (Source[1] <> AnsiChar(rkLString)){$endif}
  begin
    result := -1;
    exit; // invalid Source or Value content
  end;
  inc(Source, 2);
  Count := FromVarUInt32(PByte(Source)); // dynamic array count
  inc(Source, SizeOf(cardinal)); // ignore Hash32 security checksum
  for result := 0 to Count - 1 do
  begin
    Len := FromVarUInt32(PByte(Source));
    if CaseSensitive then
    begin
      if (Len = ValueLen) and
         CompareMemFixed(Value, Source, Len) then
        exit;
    end
    else if Utf8ILComp(Value, pointer(Source), ValueLen, Len) = 0 then
      exit;
   inc(Source, Len);
  end;
  result := -1;
end;


{ TDynArrayLoadFrom }

function TDynArrayLoadFrom.Init(ArrayTypeInfo: PRttiInfo; Source: PAnsiChar;
  SourceMaxLen: PtrInt): boolean;
begin
  result := false;
  Count := 0;
  Current := 0;
  Reader.Init(Source, SourceMaxLen);
  ArrayRtti := Rtti.RegisterType(ArrayTypeInfo);
  if (ArrayRtti.Parser <> ptDynArray) or
     Reader.EOF then
    exit;
  if ArrayRtti.Cache.ItemInfo = nil then
    ArrayLoad := nil
  else
    ArrayLoad := RTTI_BINARYLOAD[ArrayRtti.Cache.ItemInfo^.Kind];
  Count := DynArrayLoadHeader(Reader, ArrayRtti.Info, ArrayRtti.Cache.ItemInfo);
  result := true;
end;

function TDynArrayLoadFrom.Init(ArrayTypeInfo: PRttiInfo;
  const Source: RawByteString): boolean;
begin
  result := Init(ArrayTypeInfo, pointer(Source), length(Source));
end;

function TDynArrayLoadFrom.Step(Item: pointer): boolean;
begin
  if (Current < Count) and
     not Reader.EOF then
  begin
    if Assigned(ArrayLoad) then
      ArrayLoad(Item, Reader, ArrayRtti.Cache.ItemInfo)
    else
      Reader.Copy(Item, ArrayRtti.Cache.ItemSize);
    inc(Current);
    result := true;
  end
  else
    result := false;
end;

function TDynArrayLoadFrom.FirstField(Field: pointer): boolean;
var
  load: TRttiBinaryLoad;
  info: PRttiInfo;
  noiteration: TFastReader;
begin
  if (Current < Count) and
     not Reader.EOF then
  begin
    info := PT_INFO[ArrayRtti.ArrayFirstField];
    if info <> nil then
    begin
      load := RTTI_BINARYLOAD[info^.Kind];
      if Assigned(load) then
      begin
        noiteration := Reader;
        load(Field, noiteration, info);
        result := true;
        exit;
      end;
    end;
  end;
  result := false;
end;



{ ****************** TSynFilter and TSynValidate Processing Classes }

function IsValidIP4Address(P: PUtf8Char): boolean;
var
  ndot: PtrInt;
  V: PtrUInt;
begin
  result := false;
  if (P = nil) or
     not (P^ in ['0'..'9']) then
    exit;
  V := 0;
  ndot := 0;
  repeat
    case P^ of
      #0:
        break;
      '.':
        if (P[-1] = '.') or
           (V > 255) then
          exit
        else
        begin
          inc(ndot);
          V := 0;
        end;
      '0'..'9':
        V := (V * 10) + ord(P^) - 48;
    else
      exit;
    end;
    inc(P);
  until false;
  if (ndot = 3) and
     (V <= 255) and
     (P[-1] <> '.') then
    result := true;
end;

function IsValidEmail(P: PUtf8Char): boolean;
// Initial Author: Ernesto D'Spirito - UTF-8 version by AB
// http://www.howtodothings.com/computers/a1169-validating-email-addresses-in-delphi.html
const
  // Valid characters in an "atom"
  atom_chars: TSynAnsicharSet = [#33..#255] -
    ['(', ')', '<', '>', '@', ',', ';', ':', '\', '/', '"', '.', '[', ']', #127];
  // Valid characters in a "quoted-string"
  quoted_string_chars: TSynAnsicharSet =
    [#0..#255] - ['"', #13, '\'];
  // Valid characters in a subdomain
  letters_digits: TSynAnsicharSet =
    ['0'..'9', 'A'..'Z', 'a'..'z'];
type
  States = (
    STATE_BEGIN,
    STATE_ATOM,
    STATE_QTEXT,
    STATE_QCHAR,
    STATE_QUOTE,
    STATE_LOCAL_PERIOD,
    STATE_EXPECTING_SUBDOMAIN,
    STATE_SUBDOMAIN,
    STATE_HYPHEN);
var
  State: States;
  subdomains: integer;
  c: AnsiChar;
  ch: PtrInt;
begin
  State := STATE_BEGIN;
  subdomains := 1;
  if P <> nil then
    repeat
      ch := ord(P^);
      if ch and $80 = 0 then
        inc(P)
      else
        ch := UTF8_TABLE.GetHighUtf8Ucs4(P);
      if (ch <= 255) and
         (WinAnsiConvert.AnsiToWide[ch] <= 255) then
        // convert into WinAnsi char
        c := AnsiChar(ch)
      else
        // invalid char
        c := #127;
      case State of
        STATE_BEGIN:
          if c in atom_chars then
            State := STATE_ATOM
          else if c = '"' then
            State := STATE_QTEXT
          else
            break;
        STATE_ATOM:
          if c = '@' then
            State := STATE_EXPECTING_SUBDOMAIN
          else if c = '.' then
            State := STATE_LOCAL_PERIOD
          else if not (c in atom_chars) then
            break;
        STATE_QTEXT:
          if c = '\' then
            State := STATE_QCHAR
          else if c = '"' then
            State := STATE_QUOTE
          else if not (c in quoted_string_chars) then
            break;
        STATE_QCHAR:
          State := STATE_QTEXT;
        STATE_QUOTE:
          if c = '@' then
            State := STATE_EXPECTING_SUBDOMAIN
          else if c = '.' then
            State := STATE_LOCAL_PERIOD
          else
            break;
        STATE_LOCAL_PERIOD:
          if c in atom_chars then
            State := STATE_ATOM
          else if c = '"' then
            State := STATE_QTEXT
          else
            break;
        STATE_EXPECTING_SUBDOMAIN:
          if c in letters_digits then
            State := STATE_SUBDOMAIN
          else
            break;
        STATE_SUBDOMAIN:
          if c = '.' then
          begin
            inc(subdomains);
            State := STATE_EXPECTING_SUBDOMAIN
          end
          else if c = '-' then
            State := STATE_HYPHEN
          else if not (c in letters_digits) then
            break;
        STATE_HYPHEN:
          if c in letters_digits then
            State := STATE_SUBDOMAIN
          else if c <> '-' then
            break;
      end;
      if P^ = #0 then
      begin
        P := nil;
        break;
      end;
    until false;
  result := (State = STATE_SUBDOMAIN) and
            (subdomains >= 2);
end;


{ TSynFilterOrValidate }

constructor TSynFilterOrValidate.Create(const aParameters: RawUtf8);
begin
  inherited Create;
  SetParameters(aParameters); // should parse the JSON-encoded parameters
end;

constructor TSynFilterOrValidate.CreateUtf8(const Format: RawUtf8; const Args,
  Params: array of const);
begin
  Create(FormatJson(Format, Args, Params));
end;

procedure TSynFilterOrValidate.SetParameters(const value: RawUtf8);
begin
  fParameters := value;
end;

function TSynFilterOrValidate.AddOnce(var aObjArray: TSynFilterOrValidateObjArray;
  aFreeIfAlreadyThere: boolean): TSynFilterOrValidate;
var
  i: integer;
begin
  if self <> nil then
  begin
    for i := 0 to length(aObjArray) - 1 do
      if (PPointer(aObjArray[i])^ = PPointer(self)^) and
         (aObjArray[i].fParameters = fParameters) then
      begin
        if aFreeIfAlreadyThere then
          Free;
        result := aObjArray[i];
        exit;
      end;
    ObjArrayAdd(aObjArray, self);
  end;
  result := self;
end;


{ TSynFilterUpperCase }

procedure TSynFilterUpperCase.Process(aFieldIndex: integer; var value: RawUtf8);
begin
  value := mormot.core.unicode.UpperCase(value);
end;


{ TSynFilterUpperCaseU }

procedure TSynFilterUpperCaseU.Process(aFieldIndex: integer; var value: RawUtf8);
begin
  value := UpperCaseU(value);
end;


{ TSynFilterLowerCase }

procedure TSynFilterLowerCase.Process(aFieldIndex: integer; var value: RawUtf8);
begin
  value := LowerCase(value);
end;


{ TSynFilterLowerCaseU }

procedure TSynFilterLowerCaseU.Process(aFieldIndex: integer; var value: RawUtf8);
begin
  value := LowerCaseU(value);
end;


{ TSynFilterTrim }

procedure TSynFilterTrim.Process(aFieldIndex: integer; var value: RawUtf8);
begin
  TrimSelf(value);
end;


{ TSynFilterTruncate}

procedure TSynFilterTruncate.SetParameters(const value: RawUtf8);
var
  V: array[0..1] of TValuePUtf8Char;
  tmp: TSynTempBuffer;
begin
  tmp.Init(value);
  JsonDecode(tmp.buf, [
    'MaxLength', // 0
    'Utf8Length' // 1
    ], @V);
  fMaxLength := V[0].ToCardinal(0);
  fUtf8Length := V[1].ToBoolean;
  tmp.Done;
end;

procedure TSynFilterTruncate.Process(aFieldIndex: integer; var value: RawUtf8);
begin
  if fMaxLength - 1 < cardinal(maxInt) then
    if fUtf8Length then
      Utf8TruncateToLength(value, fMaxLength)
    else
      Utf8TruncateToUnicodeLength(value, fMaxLength);
end;


{ TSynValidateIPAddress }

function TSynValidateIPAddress.Process(aFieldIndex: integer; const value:
  RawUtf8; var ErrorMsg: string): boolean;
begin
  result := IsValidIP4Address(pointer(value));
  if not result then
    ErrorMsg := Format(sInvalidIPAddress, [Utf8ToString(value)]);
end;


{ TSynValidateEmail }

function TSynValidateEmail.Process(aFieldIndex: integer; const value: RawUtf8;
  var ErrorMsg: string): boolean;
var
  TLD, DOM: RawUtf8;
  i: integer;
const
  TopLevelTLD: array[0..20] of PUtf8Char = (
    // see http://en.wikipedia.org/wiki/List_of_Internet_top-level_domains
    'aero', 'asia', 'biz', 'cat', 'com', 'coop', 'edu', 'gov', 'info', 'int',
    'jobs', 'mil', 'mobi', 'museum', 'name', 'net', 'org', 'pro', 'site', 'tel',
    'travel'); // no xxx !
begin
  if IsValidEmail(pointer(value)) then
    repeat
      DOM := lowercase(copy(value, PosExChar('@', value) + 1, 100));
      if length(DOM) > 63 then
        break; // exceeded 63-character limit of a DNS name
      if (ForbiddenDomains <> '') and
         CsvContains(ForbiddenDomains, DOM) then
        break;
      i := length(value);
      while (i > 0) and
            (value[i] <> '.') do
        dec(i);
      TLD := lowercase(copy(value, i + 1, 100));
      if (AllowedTLD <> '') and
         not CsvContains(AllowedTLD, TLD) then
        break;
      if (ForbiddenTLD <> '') and
         CsvContains(ForbiddenTLD, TLD) then
        break;
      if not fAnyTLD then
        if FastFindPUtf8CharSorted(@TopLevelTLD, high(TopLevelTLD), pointer(TLD)) < 0 then
          if length(TLD) <> 2 then
            break; // assume a two chars string is a ISO 3166-1 alpha-2 code
      result := true;
      exit;
    until true;
  ErrorMsg := Format(sInvalidEmailAddress, [Utf8ToString(value)]);
  result := false;
end;

procedure TSynValidateEmail.SetParameters(const value: RawUtf8);
var
  V: array[0..3] of TValuePUtf8Char;
  tmp: TSynTempBuffer;
begin
  inherited;
  tmp.Init(value);
  JsonDecode(tmp.buf, [
    'AllowedTLD',        // 0
    'ForbiddenTLD',      // 1
    'ForbiddenDomains',  // 2
    'AnyTLD'             // 3
    ], @V);
  LowerCaseCopy(V[0].Text, V[0].Len, fAllowedTLD);
  LowerCaseCopy(V[1].Text, V[1].Len, fForbiddenTLD);
  LowerCaseCopy(V[2].Text, V[2].Len, fForbiddenDomains);
  AnyTLD := V[3].ToBoolean;
  tmp.Done;
end;


{ TSynValidatePattern }

procedure TSynValidatePattern.SetParameters(const Value: RawUtf8);
begin
  inherited SetParameters(Value);
  fMatch.Prepare(Value, ClassType = TSynValidatePatternI, {reuse=}true);
end;

function TSynValidatePattern.Process(aFieldIndex: integer; const value: RawUtf8;
  var ErrorMsg: string): boolean;

  procedure SetErrorMsg;
  begin
    ErrorMsg := Format(sInvalidPattern, [Utf8ToString(value)]);
  end;

begin
  result := fMatch.Match(value);
  if not result then
    SetErrorMsg;
end;


{ TSynValidateNonVoidText }

function Character01n(n: integer): string;
begin
  if n < 0 then
    n := 0
  else if n > 1 then
    n := 2;
  result := GetCsvItemString(pointer(string(sCharacter01n)), n);
end;

procedure InvalidTextLengthMin(min: integer; var result: string);
begin
  result := Format(sInvalidTextLengthMin, [min, Character01n(min)]);
end;

function TSynValidateNonVoidText.Process(aFieldIndex: integer; const value:
  RawUtf8; var ErrorMsg: string): boolean;
begin
  if value = '' then
  begin
    InvalidTextLengthMin(1, ErrorMsg);
    result := false;
  end
  else
    result := true;
end;


{ TSynValidateText }

procedure TSynValidateText.SetErrorMsg(fPropsIndex, InvalidTextIndex, MainIndex:
  integer; var result: string);
var
  P: PChar;
begin
  P := pointer(string(sInvalidTextChar));
  result := GetCsvItemString(P, MainIndex);
  if fPropsIndex > 0 then
    result := Format(result, [fProps[fPropsIndex],
      GetCsvItemString(P, InvalidTextIndex), Character01n(fProps[fPropsIndex])]);
end;

function TSynValidateText.Process(aFieldIndex: integer; const value: RawUtf8;
  var ErrorMsg: string): boolean;
var
  i, L: cardinal;
  Min: array[2..7] of cardinal;
begin
  result := false;
  if fUtf8Length then
    L := length(value)
  else
    L := Utf8ToUnicodeLength(pointer(value));
  if L < MinLength then
    InvalidTextLengthMin(MinLength, ErrorMsg)
  else if L > MaxLength then
    ErrorMsg := Format(sInvalidTextLengthMax, [MaxLength, Character01n(MaxLength)])
  else
  begin
    FillCharFast(Min, SizeOf(Min), 0);
    L := length(value);
    for i := 1 to L do
      case value[i] of
        ' ':
          inc(Min[7]);
        'a'..'z':
          begin
            inc(Min[2]);
            inc(Min[5]);
          end;
        'A'..'Z':
          begin
            inc(Min[2]);
            inc(Min[6]);
          end;
        '0'..'9':
          inc(Min[3]);
        '_', '!', ';', '.', ',', '/', ':', '?', '%', '$', '=', '"', '#', '@',
        '(', ')', '{', '}', '+', '''', '-', '*':
          inc(Min[4]);
      end;
    for i := 2 to 7 do
      if Min[i] < fProps[i] then
      begin
        SetErrorMsg(i, i, 0, ErrorMsg);
        exit;
      end
      else if Min[i] > fProps[i + 8] then
      begin
        SetErrorMsg(i + 8, i, 1, ErrorMsg);
        exit;
      end;
    if value <> '' then
    begin
      if MaxLeftTrimCount < cardinal(maxInt) then
      begin
        // if MaxLeftTrimCount is set, check against Value
        i := 0;
        while (i < L) and
              (value[i + 1] = ' ') do
          inc(i);
        if i > MaxLeftTrimCount then
        begin
          SetErrorMsg(0, 0, 8, ErrorMsg);
          exit;
        end;
      end;
      if MaxRightTrimCount < cardinal(maxInt) then
      begin
        // if MaxRightTrimCount is set, check against Value
        i := 0;
        while (i < L) and
              (value[L - i] = ' ') do
          dec(i);
        if i > MaxRightTrimCount then
        begin
          SetErrorMsg(0, 0, 9, ErrorMsg);
          exit;
        end;
      end;
    end;
    result := true;
  end;
end;

procedure TSynValidateText.SetParameters(const value: RawUtf8);
var
  V: array[0..high(TSynValidateTextProps) + {Utf8Length} 1] of TValuePUtf8Char;
  i: PtrInt;
  tmp: TSynTempBuffer;
const
  DEFAULT: TSynValidateTextProps = (
    1,       //  MinLength
    maxInt,  //  MaxLength
    0,       //  MinAlphaCount
    0,       //  MinDigitCount
    0,       //  MinPunctCount
    0,       //  MinLowerCount
    0,       //  MinUpperCount
    0,       //  MinSpaceCount
    maxInt,  //  MaxLeftTrimCount
    maxInt,  //  MaxRightTrimCount
    maxInt,  //  MaxAlphaCount
    maxInt,  //  MaxDigitCount
    maxInt,  //  MaxPunctCount
    maxInt,  //  MaxLowerCount
    maxInt,  //  MaxUpperCount
    maxInt); //  MaxSpaceCount
begin
  if (MinLength = 0) and
     (MaxLength = 0) then  // if not previously set
    fProps := DEFAULT;
  inherited SetParameters(value);
  if value = '' then
    exit;
  tmp.Init(value);
  try
    JsonDecode(tmp.buf, [
      'MinLength',
      'MaxLength',
      'MinAlphaCount',
      'MinDigitCount',
      'MinPunctCount',
      'MinLowerCount',
      'MinUpperCount',
      'MinSpaceCount',
      'MaxLeftTrimCount',
      'MaxRightTrimCount',
      'MaxAlphaCount',
      'MaxDigitCount',
      'MaxPunctCount',
      'MaxLowerCount',
      'MaxUpperCount',
      'MaxSpaceCount',
      'Utf8Length'], @V);
    for i := 0 to high(fProps) do
      fProps[i] := V[i].ToCardinal(fProps[i]);
    with V[high(V)] do
      fUtf8Length := ToBoolean;
  finally
    tmp.Done;
  end;
end;


{ TSynValidatePassWord }

procedure TSynValidatePassWord.SetParameters(const value: RawUtf8);
const
  DEFAULT: TSynValidateTextProps = (
    5,        //  MinLength
    20,       //  MaxLength
    1,        //  MinAlphaCount
    1,        //  MinDigitCount
    1,        //  MinPunctCount
    1,        //  MinLowerCount
    1,        //  MinUpperCount
    0,        //  MinSpaceCount
    maxInt,   //  MaxLeftTrimCount
    maxInt,   //  MaxRightTrimCount
    maxInt,   //  MaxAlphaCount
    maxInt,   //  MaxDigitCount
    maxInt,   //  MaxPunctCount
    maxInt,   //  MaxLowerCount
    maxInt,   //  MaxUpperCount
    0);       //  MaxSpaceCount
begin
  // set default values for validating a strong password
  fProps := DEFAULT;
  fUtf8Length := false;
  // read custom parameters
  inherited;
end;



{ ***************** Cross-Platform TSynTimeZone Time Zones }

{ TTimeZoneData }

function TTimeZoneData.GetTziFor(year: integer): PTimeZoneInfo;
var
  i, last: PtrInt;
begin
  if dyn = nil then
    result := @tzi
  else if year <= dyn[0].year then
    result := @dyn[0].tzi
  else
  begin
    last := high(dyn);
    if year >= dyn[last].year then
      result := @dyn[last].tzi
    else
    begin
      for i := 1 to last do
        if year < dyn[i].year then
        begin
          result := @dyn[i - 1].tzi;
          exit;
        end;
      result := @tzi; // should never happen, but makes compiler happy
    end;
  end;
end;


{ TTimeZoneInformation }

constructor TSynTimeZone.Create;
begin
  fZones.InitSpecific(TypeInfo(TTimeZoneDataDynArray),
    fZone, ptRawUtf8, @fZoneCount);
end;

constructor TSynTimeZone.CreateDefault(dummycpp: integer);
begin
  Create;
  {$ifdef OSWINDOWS}
  LoadFromRegistry;
  {$else}
  LoadFromFile;
  if fZoneCount = 0 then
    LoadFromResource; // if no .tz file is available, try if bound to executable
  {$endif OSWINDOWS}
end;

destructor TSynTimeZone.Destroy;
begin
  inherited Destroy;
  fIds.Free;
  fDisplays.Free;
end;

var
  SharedSynTimeZone: TSynTimeZone;

class function TSynTimeZone.Default: TSynTimeZone;
begin
  if SharedSynTimeZone = nil then
  begin
    GlobalLock; // RegisterGlobalShutdownRelease() will use it anyway
    try
      if SharedSynTimeZone = nil then
        SharedSynTimeZone :=
          RegisterGlobalShutdownRelease(TSynTimeZone.CreateDefault);
    finally
      GlobalUnLock;
    end;
  end;
  result := SharedSynTimeZone;
end;

function TSynTimeZone.SaveToBuffer: RawByteString;
begin
  fSafe.ReadLock;
  try
    result := AlgoSynLZ.Compress(fZones.SaveTo);
  finally
    fSafe.ReadUnLock;
  end;
end;

procedure TSynTimeZone.SaveToFile(const FileName: TFileName);
var
  FN: TFileName;
begin
  if FileName = '' then
    FN := ChangeFileExt(Executable.ProgramFileName, '.tz')
  else
    FN := FileName;
  FileFromString(SaveToBuffer, FN);
end;

procedure TSynTimeZone.LoadFromBuffer(const Buffer: RawByteString);
begin
  if Buffer = '' then
   exit;
  fSafe.WriteLock;
  try
    fZones.LoadFromBinary(AlgoSynLZ.Decompress(Buffer));
    fZones.ForceReHash;
    FreeAndNil(fIds);
    FreeAndNil(fDisplays);
  finally
    fSafe.WriteUnLock;
  end;
end;

procedure TSynTimeZone.LoadFromFile(const FileName: TFileName);
var
  FN: TFileName;
begin
  if FileName = '' then
    FN := ChangeFileExt(Executable.ProgramFileName, '.tz')
  else
    FN := FileName;
  LoadFromBuffer(StringFromFile(FN));
end;

procedure TSynTimeZone.LoadFromResource(Instance: TLibHandle);
var
  buf: RawByteString;
begin
  ResourceToRawByteString(ClassName, PChar(10), buf, Instance);
  if buf <> '' then
    LoadFromBuffer(buf);
end;

{$ifdef OSWINDOWS}

procedure TSynTimeZone.LoadFromRegistry;
const
  REGKEY = 'Software\Microsoft\Windows NT\CurrentVersion\Time Zones\';
var
  reg: TWinRegistry;
  keys: TRawUtf8DynArray;
  i, first, last, year, n: integer;
  item: TTimeZoneData;
begin
  fSafe.WriteLock;
  try
    fZones.Clear;
    if reg.ReadOpen(wrLocalMachine, REGKEY) then
      keys := reg.ReadEnumEntries
    else
      keys := nil; // make Delphi 6 happy
    n := length(keys);
    fZones.Capacity := n;
    for i := 0 to n - 1 do
    begin
      Finalize(item);
      FillcharFast(item.tzi, SizeOf(item.tzi), 0);
      if reg.ReadOpen(wrLocalMachine, REGKEY + keys[i], {reopen=}true) then
      begin
        item.id := keys[i]; // registry keys are genuine by definition
        item.display := reg.ReadString('Display');
        reg.ReadBuffer('TZI', @item.tzi, SizeOf(item.tzi));
        if reg.ReadOpen(wrLocalMachine, REGKEY + keys[i] + '\Dynamic DST', true) then
        begin
          // warning: never defined on XP/2003, and not for all entries
          first := reg.ReadDword('FirstEntry');
          last  := reg.ReadDword('LastEntry');
          if (first > 0) and
             (last >= first) then
          begin
            n := 0;
            SetLength(item.dyn, last - first + 1);
            for year := first to last do
              if reg.ReadBuffer(Utf8ToSynUnicode(UInt32ToUtf8(year)),
                @item.dyn[n].tzi, SizeOf(TTimeZoneInfo)) then
              begin
                item.dyn[n].year := year;
                inc(n);
              end;
            SetLength(item.dyn, n);
          end;
        end;
        fZones.Add(item);
      end;
    end;
    reg.Close;
    fZones.ForceReHash;
    FreeAndNil(fIds);
    FreeAndNil(fDisplays);
  finally
    fSafe.WriteUnLock;
  end;
end;

{$endif OSWINDOWS}

function TSynTimeZone.LockedFindZoneIndex(const TzId: TTimeZoneID): PtrInt;
begin
  if TzId = '' then
    result := -1
  else
  begin
    if TzId = fLastZone then
      result := fLastIndex
    else
    begin
      result := fZones.FindHashed(TzId);
      fLastZone := TzId;
      flastIndex := result;
    end;
  end;
end;

function TSynTimeZone.GetDisplay(const TzId: TTimeZoneID): RawUtf8;
var
  ndx: PtrInt;
begin
  fSafe.ReadLock;
  ndx := LockedFindZoneIndex(TzId);
  if ndx < 0 then
    if TzId = 'UTC' then // e.g. on XP
      result := TzId
    else
      result := ''
  else
    result := fZone[ndx].display;
  fSafe.ReadUnLock;
end;

function TSynTimeZone.GetBiasForDateTime(const Value: TDateTime;
  const TzId: TTimeZoneID; out Bias: integer; out HaveDaylight: boolean;
  ValueIsUtc: boolean): boolean;
var
  ndx: PtrInt;
  d: TSynSystemTime;
  tzi: PTimeZoneInfo;
  std, dlt: TDateTime;
begin
  fSafe.ReadLock;
  try
    ndx := LockedFindZoneIndex(TzId);
    if ndx < 0 then
    begin
      Bias := 0;
      HaveDaylight := false;
      result := TzId = 'UTC'; // e.g. on XP
      exit;
    end;
    d.FromDate(Value); // faster than DecodeDate
    tzi := fZone[ndx].GetTziFor(d.Year);
    if tzi.change_time_std.IsZero then
    begin
      HaveDaylight := false;
      Bias := tzi.Bias + tzi.bias_std;
    end
    else
    begin
      HaveDaylight := true;
      std := tzi.change_time_std.EncodeForTimeChange(d.Year);
      dlt := tzi.change_time_dlt.EncodeForTimeChange(d.Year);
      if ValueIsUtc then
      begin
        // STD shifts by the DLT bias to convert to UTC
        std := ((std * MinsPerDay) + tzi.Bias + tzi.bias_dlt) / MinsPerDay;
        // DLT shifts by the STD bias
        dlt := ((dlt * MinsPerDay) + tzi.Bias + tzi.bias_std) / MinsPerDay;
      end;
      if std < dlt then
        if (std <= Value) and
           (Value < dlt) then
          Bias := tzi.Bias + tzi.bias_std
        else
          Bias := tzi.Bias + tzi.bias_dlt
      else if (dlt <= Value) and
              (Value < std) then
        Bias := tzi.Bias + tzi.bias_dlt
      else
        Bias := tzi.Bias + tzi.bias_std;
    end;
    result := true;
  finally
    fSafe.ReadUnLock;
  end;
end;

function TSynTimeZone.UtcToLocal(const UtcDateTime: TDateTime;
  const TzId: TTimeZoneID): TDateTime;
var
  Bias: integer;
  HaveDaylight: boolean;
begin
  if (self = nil) or
     (TzId = '') then
    result := UtcDateTime
  else
  begin
    GetBiasForDateTime(UtcDateTime, TzId, Bias, HaveDaylight, {fromutc=}true);
    result := ((UtcDateTime * MinsPerDay) - Bias) / MinsPerDay;
  end;
end;

function TSynTimeZone.NowToLocal(const TzId: TTimeZoneID): TDateTime;
begin
  result := UtcToLocal(NowUtc, TzId);
end;

function TSynTimeZone.LocalToUtc(const LocalDateTime: TDateTime;
  const TzID: TTimeZoneID): TDateTime;
var
  Bias: integer;
  HaveDaylight: boolean;
begin
  if (self = nil) or
     (TzID = '') then
    result := LocalDateTime
  else
  begin
    GetBiasForDateTime(LocalDateTime, TzID, Bias, HaveDaylight);
    result := ((LocalDateTime * MinsPerDay) + Bias) / MinsPerDay;
  end;
end;

function TSynTimeZone.Ids: TStrings;
var
  i: PtrInt;
begin
  if fIDs = nil then
  begin
    fIDs := TStringList.Create;
    fSafe.ReadLock;
    for i := 0 to length(fZone) - 1 do
      fIDs.Add(Utf8ToString(RawUtf8(fZone[i].id)));
    fSafe.ReadUnLock;
  end;
  result := fIDs;
end;

function TSynTimeZone.Displays: TStrings;
var
  i: PtrInt;
begin
  if fDisplays = nil then
  begin
    fDisplays := TStringList.Create;
    fSafe.ReadLock;
    for i := 0 to length(fZone) - 1 do
      fDisplays.Add(Utf8ToString(fZone[i].display));
    fSafe.ReadUnLock;
  end;
  result := fDisplays;
end;


function GetBiasForDateTime(const Value: TDateTime; const TzId: TTimeZoneID;
  out Bias: integer; out HaveDaylight: boolean; ValueIsUtc: boolean): boolean;
begin
  result := TSynTimeZone.Default.
    GetBiasForDateTime(Value, TzId, Bias, HaveDaylight, ValueIsUtc);
end;

function GetDisplay(const TzId: TTimeZoneID): RawUtf8;
begin
  result := TSynTimeZone.Default.GetDisplay(TzId);
end;

function UtcToLocal(const UtcDateTime: TDateTime; const TzId: TTimeZoneID): TDateTime;
begin
  result := TSynTimeZone.Default.UtcToLocal(UtcDateTime, TzId);
end;

function NowToLocal(const TzId: TTimeZoneID): TDateTime;
begin
  result := TSynTimeZone.Default.NowToLocal(TzId);
end;

function LocalToUtc(const LocalDateTime: TDateTime; const TzID: TTimeZoneID): TDateTime;
begin
  result := TSynTimeZone.Default.LocalToUtc(LocalDateTime, TzId);
end;


end.
