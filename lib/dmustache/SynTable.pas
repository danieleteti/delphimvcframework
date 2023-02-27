/// filter/database/cache/buffer/security/search/multithread/OS features
// - as a complement to SynCommons, which tended to increase too much
// - licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynTable;

(*
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2022 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2022
  the Initial Developer. All Rights Reserved.

  Contributor(s):

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****


   A lot of code has moved from SynCommons.pas and mORMot.pas, to reduce the
   number of source code lines of those units, and circumvent Delphi 5/6/7
   limitations (e.g. internal error PRO-3006)

*)

interface

{$I Synopse.inc} // define HASINLINE CPU32 CPU64

uses
  {$ifdef MSWINDOWS}
    Windows,
    Messages,
  {$else}
    {$ifdef KYLIX3}
      Types,
      LibC,
      SynKylix,
    {$endif KYLIX3}
    {$ifdef FPC}
      BaseUnix,
      Unix,
    {$endif FPC}
  {$endif MSWINDOWS}
  SysUtils,
  Classes,
  {$ifndef LVCL}
    SyncObjs, // for TEvent and TCriticalSection
    Contnrs,  // for TObjectList
  {$endif}
  {$ifndef NOVARIANTS}
    Variants,
  {$endif}
  SynCommons;



{ ************ text search and functions ****************** }

type
  PMatch = ^TMatch;
  TMatchSearchFunction = function(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;

  /// low-level structure used by IsMatch() for actual glob search
  // - you can use this object to prepare a given pattern, e.g. in a loop
  // - implemented as a fast brute-force state-machine without any heap allocation
  // - some common patterns ('exactmatch', 'startwith*', '*endwith', '*contained*')
  // are handled with dedicated code, optionally with case-insensitive search
  // - consider using TMatchs (or SetMatchs/TMatchDynArray) if you expect to
  // search for several patterns, or even TExprParserMatch for expression search
  {$ifdef USERECORDWITHMETHODS}TMatch = record
    {$else}TMatch = object{$endif}
  private
    Pattern, Text: PUTF8Char;
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
    // be pointed to by the Pattern private field of this object
    procedure Prepare(const aPattern: RawUTF8; aCaseInsensitive, aReuse: boolean); overload;
    /// initialize the internal fields for a given glob search pattern
    // - note that the aPattern buffer should remain in memory, since it will
    // be pointed to by the Pattern private field of this object
    procedure Prepare(aPattern: PUTF8Char; aPatternLen: integer;
      aCaseInsensitive, aReuse: boolean); overload;
    /// initialize low-level internal fields for'*aPattern*' search
    // - this method is faster than a regular Prepare('*' + aPattern + '*')
    // - warning: the supplied aPattern variable may be modified in-place to be
    // filled with some lookup buffer, for length(aPattern) in [2..31] range
    procedure PrepareContains(var aPattern: RawUTF8; aCaseInsensitive: boolean); overload;
    /// initialize low-level internal fields for a custom search algorithm
    procedure PrepareRaw(aPattern: PUTF8Char; aPatternLen: integer;
      aSearch: TMatchSearchFunction);
    /// returns TRUE if the supplied content matches the prepared glob pattern
    // - this method is not thread-safe
    function Match(const aText: RawUTF8): boolean; overload;
      {$ifdef FPC}inline;{$endif}
    /// returns TRUE if the supplied content matches the prepared glob pattern
    // - this method is not thread-safe
    function Match(aText: PUTF8Char; aTextLen: PtrInt): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns TRUE if the supplied content matches the prepared glob pattern
    // - this method IS thread-safe, and won't lock
    function MatchThreadSafe(const aText: RawUTF8): boolean;
    /// returns TRUE if the supplied VCL/LCL content matches the prepared glob pattern
    // - this method IS thread-safe, will use stack to UTF-8 temporary conversion
    // if possible, and won't lock
    function MatchString(const aText: string): boolean;
    /// returns TRUE if this search pattern matches another
    function Equals(const aAnother{$ifndef DELPHI5OROLDER}: TMatch{$endif}): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// access to the pattern length as stored in PMax + 1
    function PatternLength: integer; {$ifdef HASINLINE}inline;{$endif}
    /// access to the pattern text as stored in Pattern
    function PatternText: PUTF8Char; {$ifdef HASINLINE}inline;{$endif}
  end;
  /// use SetMatchs() to initialize such an array from a CSV pattern text
  TMatchDynArray = array of TMatch;

  /// TMatch descendant owning a copy of the Pattern string to avoid GPF issues
  TMatchStore = record
    /// access to the research criteria
    // - defined as a nested record (and not an object) to circumvent Delphi bug
    Pattern: TMatch;
    /// Pattern.Pattern PUTF8Char will point to this instance
    PatternInstance: RawUTF8;
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
    constructor Create(const aPatterns: TRawUTF8DynArray; CaseInsensitive: Boolean); reintroduce; overload;
    /// add once some glob patterns to the internal TMach list
    // - aPatterns[] follows the IsMatch() syntax
    procedure Subscribe(const aPatterns: TRawUTF8DynArray; CaseInsensitive: Boolean); overload; virtual;
    /// add once some glob patterns to the internal TMach list
    // - each CSV item in aPatterns follows the IsMatch() syntax
    procedure Subscribe(const aPatternsCSV: RawUTF8; CaseInsensitive: Boolean); overload;
    /// search patterns in the supplied UTF-8 text
    // - returns -1 if no filter has been subscribed
    // - returns -2 if there is no match on any previous pattern subscription
    // - returns fMatch[] index, i.e. >= 0 number on first matching pattern
    // - this method is thread-safe
    function Match(const aText: RawUTF8): integer; overload; {$ifdef HASINLINE}inline;{$endif}
    /// search patterns in the supplied UTF-8 text buffer
    function Match(aText: PUTF8Char; aLen: integer): integer; overload;
    /// search patterns in the supplied VCL/LCL text
    // - could be used on a TFileName for instance
    // - will avoid any memory allocation if aText is small enough
    function MatchString(const aText: string): integer;
  end;

/// fill the Match[] dynamic array with all glob patterns supplied as CSV
// - returns how many patterns have been set in Match[|]
// - note that the CSVPattern instance should remain in memory, since it will
// be pointed to by the Match[].Pattern private field
function SetMatchs(const CSVPattern: RawUTF8; CaseInsensitive: boolean;
  out Match: TMatchDynArray): integer; overload;

/// fill the Match[0..MatchMax] static array with all glob patterns supplied as CSV
// - note that the CSVPattern instance should remain in memory, since it will
// be pointed to by the Match[].Pattern private field
function SetMatchs(CSVPattern: PUTF8Char; CaseInsensitive: boolean;
  Match: PMatch; MatchMax: integer): integer; overload;

/// search if one TMach is already registered in the Several[] dynamic array
function MatchExists(const One: TMatch; const Several: TMatchDynArray): boolean;

/// add one TMach if not already registered in the Several[] dynamic array
function MatchAdd(const One: TMatch; var Several: TMatchDynArray): boolean;

/// returns TRUE if Match=nil or if any Match[].Match(Text) is TRUE
function MatchAny(const Match: TMatchDynArray; const Text: RawUTF8): boolean;

/// apply the CSV-supplied glob patterns to an array of RawUTF8
// - any text not maching the pattern will be deleted from the array
procedure FilterMatchs(const CSVPattern: RawUTF8; CaseInsensitive: boolean;
  var Values: TRawUTF8DynArray);

/// return TRUE if the supplied content matchs a glob pattern
// - ?  Matches any single characer
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
function IsMatch(const Pattern, Text: RawUTF8; CaseInsensitive: boolean=false): boolean;

/// return TRUE if the supplied content matchs a glob pattern, using VCL strings
// - is a wrapper around IsMatch() with fast UTF-8 conversion
function IsMatchString(const Pattern, Text: string; CaseInsensitive: boolean=false): boolean;

  
type
  /// available pronunciations for our fast Soundex implementation
  TSynSoundExPronunciation =
    (sndxEnglish, sndxFrench, sndxSpanish, sndxNone);

  TSoundExValues = array[0..ord('Z')-ord('B')] of byte;
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
  TSynSoundEx = object
  protected
    Search, FirstChar: cardinal;
    fValues: PSoundExValues;
  public
    /// prepare for a Soundex search
    // - you can specify another language pronunciation than default english
    function Prepare(UpperValue: PAnsiChar;
      Lang: TSynSoundExPronunciation=sndxEnglish): boolean; overload;
    /// prepare for a custom Soundex search
    // - you can specify any language pronunciation from raw TSoundExValues array
    function Prepare(UpperValue: PAnsiChar; Lang: PSoundExValues): boolean; overload;
    /// return true if prepared value is contained in a text buffer
    // (UTF-8 encoded), by using the SoundEx comparison algorithm
    // - search prepared value at every word beginning in U^
    function UTF8(U: PUTF8Char): boolean;
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
function SoundExAnsi(A: PAnsiChar; next: PPAnsiChar=nil;
  Lang: TSynSoundExPronunciation=sndxEnglish): cardinal; overload;

/// Retrieve the Soundex value of a text word, from Ansi buffer
// - Return the soundex value as an easy to use cardinal value, 0 if the
// incoming string contains no valid word
// - if next is defined, its value is set to the end of the encoded word
// (so that you can call again this function to encode a full sentence)
function SoundExAnsi(A: PAnsiChar; next: PPAnsiChar; Lang: PSoundExValues): cardinal; overload;

/// Retrieve the Soundex value of a text word, from UTF-8 buffer
// - Return the soundex value as an easy to use cardinal value, 0 if the
// incoming string contains no valid word
// - if next is defined, its value is set to the end of the encoded word
// (so that you can call again this function to encode a full sentence)
// - very fast: all UTF-8 decoding is handled on the fly
function SoundExUTF8(U: PUTF8Char; next: PPUTF8Char=nil;
  Lang: TSynSoundExPronunciation=sndxEnglish): cardinal;

const
  /// number of bits to use for each interresting soundex char
  // - default is to use 8 bits, i.e. 4 soundex chars, which is the
  // standard approach
  // - for a more detailled soundex, use 4 bits resolution, which will
  // compute up to 7 soundex chars in a cardinal (that's our choice)
  SOUNDEX_BITS = 4;

var
  DoIsValidUTF8: function(source: PUTF8Char): Boolean;
  DoIsValidUTF8Len: function(source: PUTF8Char; sourcelen: PtrInt): Boolean;

/// returns TRUE if the supplied buffer has valid UTF-8 encoding
// - will stop when the buffer contains #0
// - on Haswell AVX2 Intel/AMD CPUs, will use very efficient ASM
function IsValidUTF8(source: PUTF8Char): Boolean; overload; {$ifdef HASINLINE}inline;{$endif}

/// returns TRUE if the supplied buffer has valid UTF-8 encoding
// - will also refuse #0 characters within the buffer
// - on Haswell AVX2 Intel/AMD CPUs, will use very efficient ASM
function IsValidUTF8(source: PUTF8Char; sourcelen: PtrInt): Boolean; overload; {$ifdef HASINLINE}inline;{$endif}

/// returns TRUE if the supplied buffer has valid UTF-8 encoding
// - will also refuse #0 characters within the buffer
// - on Haswell AVX2 Intel/AMD CPUs, will use very efficient ASM
function IsValidUTF8(const source: RawUTF8): Boolean; overload;



{ ************ filtering and validation classes and functions ************** }

/// convert an IPv4 'x.x.x.x' text into its 32-bit value
// - returns TRUE if the text was a valid IPv4 text, unserialized as 32-bit aValue
// - returns FALSE on parsing error, also setting aValue=0
// - '' or '127.0.0.1' will also return false
function IPToCardinal(P: PUTF8Char; out aValue: cardinal): boolean; overload;

/// convert an IPv4 'x.x.x.x' text into its 32-bit value
// - returns TRUE if the text was a valid IPv4 text, unserialized as 32-bit aValue
// - returns FALSE on parsing error, also setting aValue=0
// - '' or '127.0.0.1' will also return false
function IPToCardinal(const aIP: RawUTF8; out aValue: cardinal): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an IPv4 'x.x.x.x' text into its 32-bit value, 0 or localhost
// - returns <> 0 value if the text was a valid IPv4 text, 0 on parsing error
// - '' or '127.0.0.1' will also return 0
function IPToCardinal(const aIP: RawUTF8): cardinal; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// return TRUE if the supplied content is a valid email address
// - follows RFC 822, to validate local-part@domain email format
function IsValidEmail(P: PUTF8Char): boolean;

/// return TRUE if the supplied content is a valid IP v4 address
function IsValidIP4Address(P: PUTF8Char): boolean;


type
  TSynFilterOrValidate = class;

  TSynFilterOrValidateObjArray = array of TSynFilterOrValidate;
  TSynFilterOrValidateObjArrayArray = array of TSynFilterOrValidateObjArray;

  /// will define a filter (transformation) or a validation process to be
  // applied to a database Record content (typicaly a TSQLRecord)
  // - the optional associated parameters are to be supplied JSON-encoded
  TSynFilterOrValidate = class
  protected
    fParameters: RawUTF8;
    /// children must override this method in order to parse the JSON-encoded
    // parameters, and store it in protected field values
    procedure SetParameters(const Value: RawUTF8); virtual;
  public
    /// add the filter or validation process to a list, checking if not present
    // - if an instance with the same class type and parameters is already
    // registered, will call aInstance.Free and return the exising instance
    // - if there is no similar instance, will add it to the list and return it
    function AddOnce(var aObjArray: TSynFilterOrValidateObjArray;
      aFreeIfAlreadyThere: boolean=true): TSynFilterOrValidate;
  public
    /// initialize the filter (transformation) or validation instance
    // - most of the time, optional parameters may be specified as JSON,
    // possibly with the extended MongoDB syntax
    constructor Create(const aParameters: RawUTF8=''); overload; virtual;
    /// initialize the filter or validation instance
    /// - this overloaded constructor will allow to easily set the parameters
    constructor CreateUTF8(const Format: RawUTF8; const Args, Params: array of const); overload;
    /// the optional associated parameters, supplied as JSON-encoded
    property Parameters: RawUTF8 read fParameters write SetParameters;
  end;

  /// will define a validation to be applied to a Record (typicaly a TSQLRecord)
  // field content
  // - a typical usage is to validate an email or IP adress e.g.
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
    function Process(FieldIndex: integer; const Value: RawUTF8; var ErrorMsg: string): boolean;
      virtual; abstract;
  end;

  /// points to a TSynValidate variable
  // - used e.g. as optional parameter to TSQLRecord.Validate/FilterAndValidate
  PSynValidate = ^TSynValidate;

  /// IP v4 address validation to be applied to a Record field content
  // (typicaly a TSQLRecord)
  // - this versions expect no parameter
  TSynValidateIPAddress = class(TSynValidate)
  protected
  public
    /// perform the IP Address validation action to the specified value
    function Process(aFieldIndex: integer; const Value: RawUTF8;
      var ErrorMsg: string): boolean; override;
  end;

  /// IP address validation to be applied to a Record field content
  // (typicaly a TSQLRecord)
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
    fAllowedTLD: RawUTF8;
    fForbiddenTLD: RawUTF8;
    fForbiddenDomains: RawUTF8;
    fAnyTLD: boolean;
  protected
    /// decode all published properties from their JSON representation
    procedure SetParameters(const Value: RawUTF8); override;
  public
    /// perform the Email Address validation action to the specified value
    // - call IsValidEmail() function and check for the supplied TLD
    function Process(aFieldIndex: integer; const Value: RawUTF8; var ErrorMsg: string): boolean; override;
    /// allow any TLD to be allowed, even if not a generic TLD (.com,.net ...)
    // - this may be mandatory since already over 1,300 new gTLD names or
    // "strings" could become available in the next few years: there is a
    // growing list of new gTLDs available at
    // @http://newgtlds.icann.org/en/program-status/delegated-strings
    // - the only restriction is that it should be ascii characters
    property AnyTLD: boolean read fAnyTLD write fAnyTLD;
    /// a CSV list of allowed TLD
    // - if accessed directly, should be set as lower case values
    // - e.g. 'com,org,net'
    property AllowedTLD: RawUTF8 read fAllowedTLD write fAllowedTLD;
    /// a CSV list of forbidden TLD
    // - if accessed directly, should be set as lower case values
    // - e.g. 'fr'
    property ForbiddenTLD: RawUTF8 read fForbiddenTLD write fForbiddenTLD;
    /// a CSV list of forbidden domain names
    // - if accessed directly, should be set as lower case values
    // - not only the TLD, but whole domains like 'cracks.ru,hotmail.com' or such
    property ForbiddenDomains: RawUTF8 read fForbiddenDomains write fForbiddenDomains;
  end;

  /// glob case-sensitive pattern validation of a Record field content
  // - parameter is NOT JSON encoded, but is some basic TMatch glob pattern
  // - ?	   	Matches any single characer
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
    procedure SetParameters(const Value: RawUTF8); override;
  public
    /// perform the pattern validation to the specified value
    // - pattern can be e.g. '[0-9][0-9]:[0-9][0-9]:[0-9][0-9]'
    // - this method will implement both TSynValidatePattern and
    // TSynValidatePatternI, checking the current class
    function Process(aFieldIndex: integer; const Value: RawUTF8;
      var ErrorMsg: string): boolean; override;
  end;

  /// glob case-insensitive pattern validation of a text field content
  // (typicaly a TSQLRecord)
  // - parameter is NOT JSON encoded, but is some basic TMatch glob pattern
  // - same as TSynValidatePattern, but is NOT case sensitive
  TSynValidatePatternI = class(TSynValidatePattern);

  /// text validation to ensure that to any text field would not be ''
  TSynValidateNonVoidText = class(TSynValidate)
  public
    /// perform the non void text validation action to the specified value
    function Process(aFieldIndex: integer; const Value: RawUTF8;
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
    fUTF8Length: boolean;
  protected
    /// use sInvalidTextChar resourcestring to create a translated error message
    procedure SetErrorMsg(fPropsIndex, InvalidTextIndex, MainIndex: integer;
      var result: string);
    /// decode "MinLength", "MaxLength", and other parameters into fProps[]
    procedure SetParameters(const Value: RawUTF8); override;
  public
    /// perform the text length validation action to the specified value
    function Process(aFieldIndex: integer; const Value: RawUTF8;
      var ErrorMsg: string): boolean; override;
  published
    /// Minimal length value allowed for the text content
    // - the length is calculated with UTF-16 Unicode codepoints, unless
    // UTF8Length has been set to TRUE so that the UTF-8 byte count is checked
    // - default is 1, i.e. a void text will not pass the validation
    property MinLength: cardinal read fProps[0] write fProps[0];
    /// Maximal length value allowed for the text content
    // - the length is calculated with UTF-16 Unicode codepoints, unless
    // UTF8Length has been set to TRUE so that the UTF-8 byte count is checked
    // - default is maxInt, i.e. no maximum length is set
    property MaxLength: cardinal read fProps[1] write fProps[1];
    /// Minimal alphabetical character [a-zA-Z] count
    // - default is 0, i.e. no minimum set
    property MinAlphaCount: cardinal read fProps[2] write fProps[2];
    /// Maximal alphabetical character [a-zA-Z] count
    // - default is maxInt, i.e. no Maximum set
    property MaxAlphaCount: cardinal read fProps[10] write fProps[10];
    /// Minimal digit character [0-9] count
    // - default is 0, i.e. no minimum set
    property MinDigitCount: cardinal read fProps[3] write fProps[3];
    /// Maximal digit character [0-9] count
    // - default is maxInt, i.e. no Maximum set
    property MaxDigitCount: cardinal read fProps[11] write fProps[11];
    /// Minimal punctuation sign [_!;.,/:?%$="#@(){}+-*] count
    // - default is 0, i.e. no minimum set
    property MinPunctCount: cardinal read fProps[4] write fProps[4];
    /// Maximal punctuation sign [_!;.,/:?%$="#@(){}+-*] count
    // - default is maxInt, i.e. no Maximum set
    property MaxPunctCount: cardinal read fProps[12] write fProps[12];
    /// Minimal alphabetical lower case character [a-z] count
    // - default is 0, i.e. no minimum set
    property MinLowerCount: cardinal read fProps[5] write fProps[5];
    /// Maximal alphabetical lower case character [a-z] count
    // - default is maxInt, i.e. no Maximum set
    property MaxLowerCount: cardinal read fProps[13] write fProps[13];
    /// Minimal alphabetical upper case character [A-Z] count
    // - default is 0, i.e. no minimum set
    property MinUpperCount: cardinal read fProps[6] write fProps[6];
    /// Maximal alphabetical upper case character [A-Z] count
    // - default is maxInt, i.e. no Maximum set
    property MaxUpperCount: cardinal read fProps[14] write fProps[14];
    /// Minimal space count inside the value text
    // - default is 0, i.e. any space number allowed
    property MinSpaceCount: cardinal read fProps[7] write fProps[7];
    /// Maximal space count inside the value text
    // - default is maxInt, i.e. any space number allowed
    property MaxSpaceCount: cardinal read fProps[15] write fProps[15];
    /// Maximal space count allowed on the Left side
    // - default is maxInt, i.e. any Left space allowed
    property MaxLeftTrimCount: cardinal read fProps[8] write fProps[8];
    /// Maximal space count allowed on the Right side
    // - default is maxInt, i.e. any Right space allowed
    property MaxRightTrimCount: cardinal read fProps[9] write fProps[9];
    /// defines if lengths parameters expects UTF-8 or UTF-16 codepoints number
    // - with default FALSE, the length is calculated with UTF-16 Unicode
    // codepoints - MaxLength may not match the UCS4 glyphs number, in case of
    // UTF-16 surrogates
    // - you can set this property to TRUE so that the UTF-8 byte count would
    // be used for truncation againts the MaxLength parameter
    property UTF8Length: boolean read fUTF8Length write fUTF8Length;
  end;
{$M-}

  /// strong password validation for a Record field content (typicaly a TSQLRecord)
  // - the following parameters are set by default to
  // $ '{"MinLength":5,"MaxLength":20,"MinAlphaCount":1,"MinDigitCount":1,
  // $ "MinPunctCount":1,"MinLowerCount":1,"MinUpperCount":1,"MaxSpaceCount":0}'
  // - you can specify some JSON encoded parameters to change this default
  // values, which will validate the text field only if it contains from 5 to 10
  // characters, with at least one digit, one upper case letter, one lower case
  // letter, and one ponctuation sign, with no space allowed inside
  TSynValidatePassWord = class(TSynValidateText)
  protected
    /// set password specific parameters
    procedure SetParameters(const Value: RawUTF8); override;
  end;

  { C++Builder doesn't support array elements as properties (RSP-12595).
    For now, simply exclude the relevant classes from C++Builder. }
  {$NODEFINE TSynValidateTextProps}
  {$NODEFINE TSynValidateText }
  {$NODEFINE TSynValidatePassWord }

  /// will define a transformation to be applied to a Record field content
  // (typicaly a TSQLRecord)
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
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); virtual; abstract;
  end;

  /// class-refrence type (metaclass) for a TSynFilter or a TSynValidate
  TSynFilterOrValidateClass = class of TSynFilterOrValidate;

  /// class-reference type (metaclass) of a record filter (transformation)
  TSynFilterClass = class of TSynFilter;

  /// convert the value into ASCII Upper Case characters
  // - UpperCase conversion is made for ASCII-7 only, i.e. 'a'..'z' characters
  // - this version expects no parameter
  TSynFilterUpperCase = class(TSynFilter)
  public
    /// perform the case conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
  end;

  /// convert the value into WinAnsi Upper Case characters
  // - UpperCase conversion is made for all latin characters in the WinAnsi
  // code page only, e.g. 'e' acute will be converted to 'E'
  // - this version expects no parameter
  TSynFilterUpperCaseU = class(TSynFilter)
  public
    /// perform the case conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
  end;

  /// convert the value into ASCII Lower Case characters
  // - LowerCase conversion is made for ASCII-7 only, i.e. 'A'..'Z' characters
  // - this version expects no parameter
  TSynFilterLowerCase = class(TSynFilter)
  public
    /// perform the case conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
  end;

  /// convert the value into WinAnsi Lower Case characters
  // - LowerCase conversion is made for all latin characters in the WinAnsi
  // code page only, e.g. 'E' acute will be converted to 'e'
  // - this version expects no parameter
  TSynFilterLowerCaseU = class(TSynFilter)
  public
    /// perform the case conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
  end;

  /// trim any space character left or right to the value
  // - this versions expect no parameter
  TSynFilterTrim = class(TSynFilter)
  public
    /// perform the space triming conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
  end;

  /// truncate a text above a given maximum length
  // - expects optional JSON parameters of the allowed text length range as
  // $ '{MaxLength":10}
  TSynFilterTruncate = class(TSynFilter)
  protected
    fMaxLength: cardinal;
    fUTF8Length: boolean;
    /// decode the MaxLength: and UTF8Length: parameters
    procedure SetParameters(const Value: RawUTF8); override;
  public
    /// perform the length truncation of the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
    /// Maximal length value allowed for the text content
    // - the length is calculated with UTF-16 Unicode codepoints, unless
    // UTF8Length has been set to TRUE so that the UTF-8 byte count is checked
    // - default is 0, i.e. no maximum length is forced
    property MaxLength: cardinal read fMaxLength write fMaxLength;
    /// defines if MaxLength is stored as UTF-8 or UTF-16 codepoints number
    // - with default FALSE, the length is calculated with UTF-16 Unicode
    // codepoints - MaxLength may not match the UCS4 glyphs number, in case of
    // UTF-16 surrogates
    // - you can set this property to TRUE so that the UTF-8 byte count would
    // be used for truncation againts the MaxLength parameter
    property UTF8Length: boolean read fUTF8Length write fUTF8Length;
  end;

resourcestring
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


{ ************ Database types and classes ************************** }

type
  /// handled field/parameter/column types for abstract database access
  // - this will map JSON-compatible low-level database-level access types, not
  // high-level Delphi types as TSQLFieldType defined in mORMot.pas
  // - it does not map either all potential types as defined in DB.pas (which
  // are there for compatibility with old RDBMS, and are not abstract enough)
  // - those types can be mapped to standard SQLite3 generic types, i.e.
  // NULL, INTEGER, REAL, TEXT, BLOB (with the addition of a ftCurrency and
  // ftDate type, for better support of most DB engines)
  // see @http://www.sqlite.org/datatype3.html
  // - the only string type handled here uses UTF-8 encoding (implemented
  // using our RawUTF8 type), for cross-Delphi true Unicode process
  TSQLDBFieldType =
    (ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency, ftDate, ftUTF8, ftBlob);

  /// set of field/parameter/column types for abstract database access
  TSQLDBFieldTypes = set of TSQLDBFieldType;

  /// array of field/parameter/column types for abstract database access
  TSQLDBFieldTypeDynArray = array of TSQLDBFieldType;

  /// array of field/parameter/column types for abstract database access
  // - this array as a fixed size, ready to handle up to MAX_SQLFIELDS items
  TSQLDBFieldTypeArray = array[0..MAX_SQLFIELDS-1] of TSQLDBFieldType;
  PSQLDBFieldTypeArray = ^TSQLDBFieldTypeArray;

  /// how TSQLVar may be processed
  // - by default, ftDate will use seconds resolution unless svoDateWithMS is set
  TSQLVarOption = (svoDateWithMS);

  /// defines how TSQLVar may be processed
  TSQLVarOptions = set of TSQLVarOption;

  /// memory structure used for database values by reference storage
  // - used mainly by SynDB, mORMot, mORMotDB and mORMotSQLite3 units
  // - defines only TSQLDBFieldType data types (similar to those handled by
  // SQLite3, with the addition of ftCurrency and ftDate)
  // - cleaner/lighter dedicated type than TValue or variant/TVarData, strong
  // enough to be marshalled as JSON content
  // - variable-length data (e.g. UTF-8 text or binary BLOB) are never stored
  // within this record, but VText/VBlob will point to an external (temporary)
  // memory buffer
  // - date/time is stored as ISO-8601 text (with milliseconds if svoDateWithMS
  // option is set and the database supports it), and currency as double or BCD
  // in most databases
  TSQLVar = record
    /// how this value should be processed
    Options: TSQLVarOptions;
    /// the type of the value stored
    case VType: TSQLDBFieldType of
    ftInt64: (
      VInt64: Int64);
    ftDouble: (
      VDouble: double);
    ftDate: (
      VDateTime: TDateTime);
    ftCurrency: (
      VCurrency: Currency);
    ftUTF8: (
      VText: PUTF8Char);
    ftBlob: (
      VBlob: pointer;
      VBlobLen: Integer)
  end;

  /// dynamic array of database values by reference storage
  TSQLVarDynArray = array of TSQLVar;

  /// used to store bit set for all available fields in a Table
  // - with current MAX_SQLFIELDS value, 64 bits uses 8 bytes of memory
  // - see also IsZero() and IsEqual() functions
  // - you can also use ALL_FIELDS as defined in mORMot.pas
  TSQLFieldBits = set of 0..MAX_SQLFIELDS-1;

  /// used to store a field index in a Table
  // - note that -1 is commonly used for the ID/RowID field so the values should
  // be signed
  // - even if ShortInt (-128..127) may have been enough, we define a 16 bit
  // safe unsigned integer to let the source compile with Delphi 5
  TSQLFieldIndex = SmallInt; // -32768..32767

  /// used to store field indexes in a Table
  // - same as TSQLFieldBits, but allowing to store the proper order
  TSQLFieldIndexDynArray = array of TSQLFieldIndex;

  /// points to a bit set used for all available fields in a Table
  PSQLFieldBits = ^TSQLFieldBits;

  /// generic parameter types, as recognized by SQLParamContent() and
  // ExtractInlineParameters() functions
  TSQLParamType = (sptUnknown, sptInteger, sptFloat, sptText, sptBlob, sptDateTime);

  /// array of parameter types, as recognized by SQLParamContent() and
  // ExtractInlineParameters() functions
  TSQLParamTypeDynArray = array of TSQLParamType;

  /// simple writer to a Stream, specialized for the JSON format and SQL export
  // - i.e. define some property/method helpers to export SQL resultset as JSON
  // - see mORMot.pas for proper class serialization via TJSONSerializer.WriteObject
  TJSONWriter = class(TTextWriterWithEcho)
  protected
    /// used to store output format
    fExpand: boolean;
    /// used to store output format for TSQLRecord.GetJSONValues()
    fWithID: boolean;
    /// used to store field for TSQLRecord.GetJSONValues()
    fFields: TSQLFieldIndexDynArray;
    /// if not Expanded format, contains the Stream position of the first
    // useful Row of data; i.e. ',val11' position in:
    // & { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
    fStartDataPosition: integer;
  public
    /// used internally to store column names and count for AddColumns
    ColNames: TRawUTF8DynArray;
    /// the data will be written to the specified Stream
    // - if no Stream is supplied, a temporary memory stream will be created
    // (it's faster to supply one, e.g. any TSQLRest.TempMemoryStream)
    constructor Create(aStream: TStream; Expand, withID: boolean;
      const Fields: TSQLFieldBits; aBufSize: integer=8192); overload;
    /// the data will be written to the specified Stream
    // - if no Stream is supplied, a temporary memory stream will be created
    // (it's faster to supply one, e.g. any TSQLRest.TempMemoryStream)
    constructor Create(aStream: TStream; Expand, withID: boolean;
      const Fields: TSQLFieldIndexDynArray=nil; aBufSize: integer=8192;
      aStackBuffer: PTextWriterStackBuffer=nil); overload;
    /// rewind the Stream position and write void JSON object
    procedure CancelAllVoid;
    /// write or init field names for appropriate JSON Expand later use
    // - ColNames[] must have been initialized before calling this procedure
    // - if aKnownRowsCount is not null, a "rowCount":... item will be added
    // to the generated JSON stream (for faster unserialization of huge content)
    procedure AddColumns(aKnownRowsCount: integer=0);
    /// allow to change on the fly an expanded format column layout
    // - by definition, a non expanded format will raise a ESynException
    // - caller should then set ColNames[] and run AddColumns()
    procedure ChangeExpandedFields(aWithID: boolean; const aFields: TSQLFieldIndexDynArray); overload;
    /// end the serialized JSON object
    // - cancel last ','
    // - close the JSON object ']' or ']}'
    // - write non expanded postlog (,"rowcount":...), if needed
    // - flush the internal buffer content if aFlushFinal=true
    procedure EndJSONObject(aKnownRowsCount,aRowsCount: integer; aFlushFinal: boolean=true);
      {$ifdef HASINLINE}inline;{$endif}
    /// the first data row is erased from the content
    // - only works if the associated storage stream is TMemoryStream
    // - expect not Expanded format
    procedure TrimFirstRow;
    /// is set to TRUE in case of Expanded format
    property Expand: boolean read fExpand write fExpand;
    /// is set to TRUE if the ID field must be appended to the resulting JSON
    // - this field is used only by TSQLRecord.GetJSONValues
    // - this field is ignored by TSQLTable.GetJSONValues
    property WithID: boolean read fWithID;
    /// Read-Only access to the field bits set for each column to be stored
    property Fields: TSQLFieldIndexDynArray read fFields;
    /// if not Expanded format, contains the Stream position of the first
    // useful Row of data; i.e. ',val11' position in:
    // & { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
    property StartDataPosition: integer read fStartDataPosition;
  end;

/// returns TRUE if no bit inside this TSQLFieldBits is set
// - is optimized for 64, 128, 192 and 256 max bits count (i.e. MAX_SQLFIELDS)
// - will work also with any other value
function IsZero(const Fields: TSQLFieldBits): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast comparison of two TSQLFieldBits values
// - is optimized for 64, 128, 192 and 256 max bits count (i.e. MAX_SQLFIELDS)
// - will work also with any other value
function IsEqual(const A,B: TSQLFieldBits): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast initialize a TSQLFieldBits with 0
// - is optimized for 64, 128, 192 and 256 max bits count (i.e. MAX_SQLFIELDS)
// - will work also with any other value
procedure FillZero(var Fields: TSQLFieldBits); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a TSQLFieldBits set of bits into an array of integers
procedure FieldBitsToIndex(const Fields: TSQLFieldBits;
  var Index: TSQLFieldIndexDynArray; MaxLength: integer=MAX_SQLFIELDS;
  IndexStart: integer=0); overload;

/// convert a TSQLFieldBits set of bits into an array of integers
function FieldBitsToIndex(const Fields: TSQLFieldBits;
  MaxLength: integer=MAX_SQLFIELDS): TSQLFieldIndexDynArray; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// add a field index to an array of field indexes
// - returns the index in Indexes[] of the newly appended Field value
function AddFieldIndex(var Indexes: TSQLFieldIndexDynArray; Field: integer): integer;

/// convert an array of field indexes into a TSQLFieldBits set of bits
procedure FieldIndexToBits(const Index: TSQLFieldIndexDynArray; var Fields: TSQLFieldBits); overload;

// search a field index in an array of field indexes
// - returns the index in Indexes[] of the given Field value, -1 if not found
function SearchFieldIndex(var Indexes: TSQLFieldIndexDynArray; Field: integer): integer;

/// convert an array of field indexes into a TSQLFieldBits set of bits
function FieldIndexToBits(const Index: TSQLFieldIndexDynArray): TSQLFieldBits; overload;
  {$ifdef HASINLINE}inline;{$endif}
  
/// returns the stored size of a TSQLVar database value
// - only returns VBlobLen / StrLen(VText) size, 0 otherwise
function SQLVarLength(const Value: TSQLVar): integer;

{$ifndef NOVARIANTS}

/// convert any Variant into a database value
// - ftBlob kind won't be handled by this function
// - complex variant types would be converted into ftUTF8 JSON object/array
procedure VariantToSQLVar(const Input: variant; var temp: RawByteString;
  var Output: TSQLVar);

/// guess the correct TSQLDBFieldType from a variant type
function VariantVTypeToSQLDBFieldType(VType: cardinal): TSQLDBFieldType;

/// guess the correct TSQLDBFieldType from a variant value
function VariantTypeToSQLDBFieldType(const V: Variant): TSQLDBFieldType;
  {$ifdef HASINLINE}inline;{$endif}

/// guess the correct TSQLDBFieldType from the UTF-8 representation of a value
function TextToSQLDBFieldType(json: PUTF8Char): TSQLDBFieldType;

type
  /// define a variant published property as a nullable integer
  // - either a varNull or a varInt64 value will be stored in the variant
  // - either a NULL or an INTEGER value will be stored in the database
  // - the property should be defined as such:
  // ! property Int: TNullableInteger read fInt write fInt;
  TNullableInteger = type variant;
  /// define a variant published property as a nullable boolean
  // - either a varNull or a varBoolean value will be stored in the variant
  // - either a NULL or a 0/1 INTEGER value will be stored in the database
  // - the property should be defined as such:
  // ! property Bool: TNullableBoolean read fBool write fBool;
  TNullableBoolean = type variant;
  /// define a variant published property as a nullable floating point value
  // - either a varNull or a varDouble value will be stored in the variant
  // - either a NULL or a FLOAT value will be stored in the database
  // - the property should be defined as such:
  // ! property Flt: TNullableFloat read fFlt write fFlt;
  TNullableFloat = type variant;
  /// define a variant published property as a nullable decimal value
  // - either a varNull or a varCurrency value will be stored in the variant
  // - either a NULL or a FLOAT value will be stored in the database
  // - the property should be defined as such:
  // ! property Cur: TNullableCurrency read fCur write fCur;
  TNullableCurrency = type variant;
  /// define a variant published property as a nullable date/time value
  // - either a varNull or a varDate value will be stored in the variant
  // - either a NULL or a ISO-8601 TEXT value will be stored in the database
  // - the property should be defined as such:
  // ! property Dat: TNullableDateTime read fDat write fDat;
  TNullableDateTime = type variant;
  /// define a variant published property as a nullable timestamp value
  // - either a varNull or a varInt64 value will be stored in the variant
  // - either a NULL or a TTimeLog INTEGER value will be stored in the database
  // - the property should be defined as such:
  // ! property Tim: TNullableTimrency read fTim write fTim;
  TNullableTimeLog = type variant;
  /// define a variant published property as a nullable UTF-8 encoded text
  // - either a varNull or varString (RawUTF8) will be stored in the variant
  // - either a NULL or a TEXT value will be stored in the database
  // - the property should be defined as such:
  // ! property Txt: TNullableUTF8Text read fTxt write fTxt;
  // or for a fixed-width VARCHAR (in external databases), here of 32 max chars:
  // ! property Txt: TNullableUTF8Text index 32 read fTxt write fTxt;
  // - warning: prior to Delphi 2009, since the variant will be stored as
  // RawUTF8 internally, you should not use directly the field value as a
  // VCL string=AnsiString like string(aField) but use VariantToString(aField)
  TNullableUTF8Text = type variant;

var
  /// a nullable integer value containing null
  NullableIntegerNull: TNullableInteger absolute NullVarData;
  /// a nullable boolean value containing null
  NullableBooleanNull: TNullableBoolean absolute NullVarData;
  /// a nullable float value containing null
  NullableFloatNull: TNullableFloat absolute NullVarData;
  /// a nullable currency value containing null
  NullableCurrencyNull: TNullableCurrency absolute NullVarData;
  /// a nullable TDateTime value containing null
  NullableDateTimeNull: TNullableDateTime absolute NullVarData;
  /// a nullable TTimeLog value containing null
  NullableTimeLogNull: TNullableTimeLog absolute NullVarData;
  /// a nullable UTF-8 encoded text value containing null
  NullableUTF8TextNull: TNullableUTF8Text absolute NullVarData;

/// creates a nullable integer value from a supplied constant
// - FPC does not allow direct assignment to a TNullableInteger = type variant
// variable: use this function to circumvent it
function NullableInteger(const Value: Int64): TNullableInteger;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableInteger = type variant variable: use this
// function to circumvent those limitations
function NullableIntegerIsEmptyOrNull(const V: TNullableInteger): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableInteger is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the Integer value
function NullableIntegerToValue(const V: TNullableInteger; out Value: Int64): Boolean;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableInteger is null, or return its value
// - returns 0 if V is null or empty, or the stored Integer value
function NullableIntegerToValue(const V: TNullableInteger): Int64;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// creates a nullable Boolean value from a supplied constant
// - FPC does not allow direct assignment to a TNullableBoolean = type variant
// variable: use this function to circumvent it
function NullableBoolean(Value: boolean): TNullableBoolean;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableBoolean = type variant variant: use this
// function to circumvent those limitations
function NullableBooleanIsEmptyOrNull(const V: TNullableBoolean): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableBoolean is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the Boolean value
function NullableBooleanToValue(const V: TNullableBoolean; out Value: Boolean): Boolean;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableBoolean is null, or return its value
// - returns false if V is null or empty, or the stored Boolean value
function NullableBooleanToValue(const V: TNullableBoolean): Boolean;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// creates a nullable floating-point value from a supplied constant
// - FPC does not allow direct assignment to a TNullableFloat = type variant
// variable: use this function to circumvent it
function NullableFloat(const Value: double): TNullableFloat;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableFloat = type variant variable: use this
// function to circumvent those limitations
function NullableFloatIsEmptyOrNull(const V: TNullableFloat): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableFloat is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the Float value
function NullableFloatToValue(const V: TNullableFloat; out Value: double): boolean;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableFloat is null, or return its value
// - returns 0 if V is null or empty, or the stored Float value
function NullableFloatToValue(const V: TNullableFloat): double;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// creates a nullable Currency value from a supplied constant
// - FPC does not allow direct assignment to a TNullableCurrency = type variant
// variable: use this function to circumvent it
function NullableCurrency(const Value: currency): TNullableCurrency;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableCurrency = type variant variable: use this
// function to circumvent those limitations
function NullableCurrencyIsEmptyOrNull(const V: TNullableCurrency): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableCurrency is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the Currency value
function NullableCurrencyToValue(const V: TNullableCurrency; out Value: currency): boolean;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableCurrency is null, or return its value
// - returns 0 if V is null or empty, or the stored Currency value
function NullableCurrencyToValue(const V: TNullableCurrency): currency;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// creates a nullable TDateTime value from a supplied constant
// - FPC does not allow direct assignment to a TNullableDateTime = type variant
// variable: use this function to circumvent it
function NullableDateTime(const Value: TDateTime): TNullableDateTime;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableDateTime = type variant variable: use this
// function to circumvent those limitations
function NullableDateTimeIsEmptyOrNull(const V: TNullableDateTime): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableDateTime is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the DateTime value
function NullableDateTimeToValue(const V: TNullableDateTime; out Value: TDateTime): boolean;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableDateTime is null, or return its value
// - returns 0 if V is null or empty, or the stored DateTime value
function NullableDateTimeToValue(const V: TNullableDateTime): TDateTime;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// creates a nullable TTimeLog value from a supplied constant
// - FPC does not allow direct assignment to a TNullableTimeLog = type variant
// variable: use this function to circumvent it
function NullableTimeLog(const Value: TTimeLog): TNullableTimeLog;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableTimeLog = type variant variable: use this
// function to circumvent those limitations
function NullableTimeLogIsEmptyOrNull(const V: TNullableTimeLog): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableTimeLog is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the TimeLog value
function NullableTimeLogToValue(const V: TNullableTimeLog; out Value: TTimeLog): boolean;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableTimeLog is null, or return its value
// - returns 0 if V is null or empty, or the stored TimeLog value
function NullableTimeLogToValue(const V: TNullableTimeLog): TTimeLog;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// creates a nullable UTF-8 encoded text value from a supplied constant
// - FPC does not allow direct assignment to a TNullableUTF8 = type variant
// variable: use this function to circumvent it
function NullableUTF8Text(const Value: RawUTF8): TNullableUTF8Text;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableUTF8Text = type variant variable: use this
// function to circumvent those limitations
function NullableUTF8TextIsEmptyOrNull(const V: TNullableUTF8Text): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableUTF8Text is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the UTF8Text value
function NullableUTF8TextToValue(const V: TNullableUTF8Text; out Value: RawUTF8): boolean;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableUTF8Text is null, or return its value
// - returns '' if V is null or empty, or the stored UTF8-encoded text value
function NullableUTF8TextToValue(const V: TNullableUTF8Text): RawUTF8;
  overload; {$ifdef HASINLINE}inline;{$endif}

{$endif NOVARIANTS}

  /// convert a date to a ISO-8601 string format for SQL '?' inlined parameters
// - will return the date encoded as '\uFFF1YYYY-MM-DD' - therefore
// ':("\uFFF12012-05-04"):' pattern will be recognized as a sftDateTime
// inline parameter in  SQLParamContent() / ExtractInlineParameters() functions
// (JSON_SQLDATE_MAGIC will be used as prefix to create '\uFFF1...' pattern)
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum=?',[DateToSQL(EncodeDate(2012,5,4))]);
function DateToSQL(Date: TDateTime): RawUTF8; overload;

/// convert a date to a ISO-8601 string format for SQL '?' inlined parameters
// - will return the date encoded as '\uFFF1YYYY-MM-DD' - therefore
// ':("\uFFF12012-05-04"):' pattern will be recognized as a sftDateTime
// inline parameter in  SQLParamContent() / ExtractInlineParameters() functions
// (JSON_SQLDATE_MAGIC will be used as prefix to create '\uFFF1...' pattern)
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum=?',[DateToSQL(2012,5,4)]);
function DateToSQL(Year,Month,Day: cardinal): RawUTF8; overload;

/// convert a date/time to a ISO-8601 string format for SQL '?' inlined parameters
// - if DT=0, returns ''
// - if DT contains only a date, returns the date encoded as '\uFFF1YYYY-MM-DD'
// - if DT contains only a time, returns the time encoded as '\uFFF1Thh:mm:ss'
// - otherwise, returns the ISO-8601 date and time encoded as '\uFFF1YYYY-MM-DDThh:mm:ss'
// (JSON_SQLDATE_MAGIC will be used as prefix to create '\uFFF1...' pattern)
// - if WithMS is TRUE, will append '.sss' for milliseconds resolution
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum<=?',[DateTimeToSQL(Now)]);
// - see TimeLogToSQL() if you are using TTimeLog/TModTime/TCreateTime values
function DateTimeToSQL(DT: TDateTime; WithMS: boolean=false): RawUTF8;

/// decode a SQL '?' inlined parameter (i.e. with JSON_SQLDATE_MAGIC prefix)
// - as generated by DateToSQL/DateTimeToSQL/TimeLogToSQL functions
function SQLToDateTime(const ParamValueWithMagic: RawUTF8): TDateTime;

/// convert a TTimeLog value into a ISO-8601 string format for SQL '?' inlined
// parameters
// - handle TTimeLog bit-encoded Int64 format
// - follows the same pattern as DateToSQL or DateTimeToSQL functions, i.e.
// will return the date or time encoded as '\uFFF1YYYY-MM-DDThh:mm:ss' -
// therefore ':("\uFFF12012-05-04T20:12:13"):' pattern will be recognized as a
// sftDateTime inline parameter in  SQLParamContent() / ExtractInlineParameters()
// (JSON_SQLDATE_MAGIC will be used as prefix to create '\uFFF1...' pattern)
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum<=?',[TimeLogToSQL(TimeLogNow)]);
function TimeLogToSQL(const Timestamp: TTimeLog): RawUTF8;

/// convert a Iso8601 encoded string into a ISO-8601 string format for SQL
// '?' inlined parameters
// - follows the same pattern as DateToSQL or DateTimeToSQL functions, i.e.
// will return the date or time encoded as '\uFFF1YYYY-MM-DDThh:mm:ss' -
// therefore ':("\uFFF12012-05-04T20:12:13"):' pattern will be recognized as a
// sftDateTime inline parameter in  SQLParamContent() / ExtractInlineParameters()
// (JSON_SQLDATE_MAGIC will be used as prefix to create '\uFFF1...' pattern)
// - in practice, just append the JSON_SQLDATE_MAGIC prefix to the supplied text
function Iso8601ToSQL(const S: RawByteString): RawUTF8;


/// guess the content type of an UTF-8 SQL value, in :(....): format
// - will be used e.g. by ExtractInlineParameters() to un-inline a SQL statement
// - sftInteger is returned for an INTEGER value, e.g. :(1234):
// - sftFloat is returned for any floating point value (i.e. some digits
// separated by a '.' character), e.g. :(12.34): or :(12E-34):
// - sftUTF8Text is returned for :("text"): or :('text'):, with double quoting
// inside the value
// - sftBlob will be recognized from the ':("\uFFF0base64encodedbinary"):'
// pattern, and return raw binary (for direct blob parameter assignment)
// - sftDateTime will be recognized from ':(\uFFF1"2012-05-04"):' pattern,
// i.e. JSON_SQLDATE_MAGIC-prefixed string as returned by DateToSQL() or
// DateTimeToSQL() functions
// - sftUnknown is returned on invalid content, or if wasNull is set to TRUE
// - if ParamValue is not nil, the pointing RawUTF8 string is set with the
// value inside :(...): without double quoting in case of sftUTF8Text
// - wasNull is set to TRUE if P was ':(null):' and ParamType is sftUnknwown
function SQLParamContent(P: PUTF8Char; out ParamType: TSQLParamType; out ParamValue: RawUTF8;
  out wasNull: boolean): PUTF8Char;

/// this function will extract inlined :(1234): parameters into Types[]/Values[]
// - will return the generic SQL statement with ? place holders for inlined
// parameters and setting Values with SQLParamContent() decoded content
// - will set maxParam=0 in case of no inlined parameters
// - recognized types are sptInteger, sptFloat, sptDateTime ('\uFFF1...'),
// sptUTF8Text and sptBlob ('\uFFF0...')
// - sptUnknown is returned on invalid content
function ExtractInlineParameters(const SQL: RawUTF8;
  var Types: TSQLParamTypeDynArray; var Values: TRawUTF8DynArray;
  var maxParam: integer; var Nulls: TSQLFieldBits): RawUTF8;

/// returns a 64-bit value as inlined ':(1234):' text
function InlineParameter(ID: Int64): shortstring; overload;

/// returns a string value as inlined ':("value"):' text
function InlineParameter(const value: RawUTF8): RawUTF8; overload;

type
  /// SQL Query comparison operators
  // - used e.g. by CompareOperator() functions in SynTable.pas or vt_BestIndex()
  // in mORMotSQLite3.pas
  TCompareOperator = (
     soEqualTo,
     soNotEqualTo,
     soLessThan,
     soLessThanOrEqualTo,
     soGreaterThan,
     soGreaterThanOrEqualTo,
     soBeginWith,
     soContains,
     soSoundsLikeEnglish,
     soSoundsLikeFrench,
     soSoundsLikeSpanish);

const
  /// convert identified field types into high-level ORM types
  // - as will be implemented in unit mORMot.pas
  SQLDBFIELDTYPE_TO_DELPHITYPE: array[TSQLDBFieldType] of RawUTF8 = (
    '???','???', 'Int64', 'Double', 'Currency', 'TDateTime', 'RawUTF8', 'TSQLRawBlob');


{ ************ low-level buffer processing functions ************************* }

type
  /// safe decoding of a TFileBufferWriter content
  // - similar to TFileBufferReader, but faster and only for in-memory buffer
  // - is also safer, since will check for reaching end of buffer
  // - raise a EFastReader exception on decoding error (e.g. if a buffer
  // overflow may occur) or call OnErrorOverflow/OnErrorData event handlers
  {$ifdef USERECORDWITHMETHODS}TFastReader = record
    {$else}TFastReader = object{$endif}
  public
    /// the current position in the memory
    P: PAnsiChar;
    /// the last position in the buffer
    Last: PAnsiChar;
    /// use this event to customize the ErrorOverflow process
    OnErrorOverflow: procedure of object;
    /// use this event to customize the ErrorData process
    OnErrorData: procedure(const fmt: RawUTF8; const args: array of const) of object;
    /// some opaque value, which may be a version number to define the binary layout
    Tag: PtrInt;
    /// initialize the reader from a memory block
    procedure Init(Buffer: pointer; Len: integer); overload;
    /// initialize the reader from a RawByteString content
    procedure Init(const Buffer: RawByteString); overload;
    /// raise a EFastReader with an "overflow" error message
    procedure ErrorOverflow;
    /// raise a EFastReader with an "incorrect data" error message
    procedure ErrorData(const fmt: RawUTF8; const args: array of const);
    /// read the next 32-bit signed value from the buffer
    function VarInt32: integer;    {$ifdef HASINLINE}inline;{$endif}
    /// read the next 32-bit unsigned value from the buffer
    function VarUInt32: cardinal;
    /// try to read the next 32-bit signed value from the buffer
    // - don't change the current position
    function PeekVarInt32(out value: PtrInt): boolean; {$ifdef HASINLINE}inline;{$endif}
    /// try to read the next 32-bit unsigned value from the buffer
    // - don't change the current position
    function PeekVarUInt32(out value: PtrUInt): boolean;
    /// read the next 32-bit unsigned value from the buffer
    // - this version won't call ErrorOverflow, but return false on error
    // - returns true on read success
    function VarUInt32Safe(out Value: cardinal): boolean;
    /// read the next 64-bit signed value from the buffer
    function VarInt64: Int64; {$ifdef HASINLINE}inline;{$endif}
    /// read the next 64-bit unsigned value from the buffer
    function VarUInt64: QWord;
    /// read the next RawUTF8 value from the buffer
    function VarUTF8: RawUTF8; overload;
    /// read the next RawUTF8 value from the buffer
    procedure VarUTF8(out result: RawUTF8); overload;
    /// read the next RawUTF8 value from the buffer
    // - this version won't call ErrorOverflow, but return false on error
    // - returns true on read success
    function VarUTF8Safe(out Value: RawUTF8): boolean;
    /// read the next RawByteString value from the buffer
    function VarString: RawByteString; {$ifdef HASINLINE}inline;{$endif}
    /// read the next pointer and length value from the buffer
    procedure VarBlob(out result: TValueResult); overload; {$ifdef HASINLINE}inline;{$endif}
    /// read the next pointer and length value from the buffer
    function VarBlob: TValueResult; overload;  {$ifdef HASINLINE}inline;{$endif}
    /// read the next ShortString value from the buffer
    function VarShortString: shortstring; {$ifdef HASINLINE}inline;{$endif}
    /// fast ignore the next VarUInt32/VarInt32/VarUInt64/VarInt64 value
    // - don't raise any exception, so caller could check explicitly for any EOF
    procedure VarNextInt; overload; {$ifdef HASINLINE}inline;{$endif}
    /// fast ignore the next count VarUInt32/VarInt32/VarUInt64/VarInt64 values
    // - don't raise any exception, so caller could check explicitly for any EOF
    procedure VarNextInt(count: integer); overload;
    /// read the next byte from the buffer
    function NextByte: byte; {$ifdef HASINLINE}inline;{$endif}
    /// read the next byte from the buffer, checking
    function NextByteSafe(dest: pointer): boolean; {$ifdef HASINLINE}inline;{$endif}
    /// read the next 4 bytes from the buffer as a 32-bit unsigned value
    function Next4: cardinal; {$ifdef HASINLINE}inline;{$endif}
    /// read the next 8 bytes from the buffer as a 64-bit unsigned value
    function Next8: Qword; {$ifdef HASINLINE}inline;{$endif}
    /// consumes the next byte from the buffer, if matches a given value
    function NextByteEquals(Value: byte): boolean; {$ifdef HASINLINE}inline;{$endif}
    /// returns the current position, and move ahead the specified bytes
    function Next(DataLen: PtrInt): pointer;   {$ifdef HASINLINE}inline;{$endif}
    /// returns the current position, and move ahead the specified bytes
    function NextSafe(out Data: Pointer; DataLen: PtrInt): boolean; {$ifdef HASINLINE}inline;{$endif}
    {$ifndef NOVARIANTS}
    /// read the next variant from the buffer
    // - is a wrapper around VariantLoad()
    procedure NextVariant(var Value: variant; CustomVariantOptions: PDocVariantOptions);
    /// read the JSON-serialized TDocVariant from the buffer
    // - matches TFileBufferWriter.WriteDocVariantData format
    procedure NextDocVariantData(out Value: variant; CustomVariantOptions: PDocVariantOptions);
    {$endif NOVARIANTS}
    /// copy data from the current position, and move ahead the specified bytes
    procedure Copy(out Dest; DataLen: PtrInt); {$ifdef HASINLINE}inline;{$endif}
    /// copy data from the current position, and move ahead the specified bytes
    // - this version won't call ErrorOverflow, but return false on error
    // - returns true on read success
    function CopySafe(out Dest; DataLen: PtrInt): boolean;
    /// apply TDynArray.LoadFrom on the buffer
    // - will unserialize a previously appended dynamic array, e.g. as
    // ! aWriter.WriteDynArray(DA);
    procedure Read(var DA: TDynArray; NoCheckHash: boolean=false);
    /// retrieved cardinal values encoded with TFileBufferWriter.WriteVarUInt32Array
    // - only supports wkUInt32, wkVarInt32, wkVarUInt32 kind of encoding
    function ReadVarUInt32Array(var Values: TIntegerDynArray): PtrInt;
    /// retrieve some TAlgoCompress buffer, appended via Write()
    // - BufferOffset could be set to reserve some bytes before the uncompressed buffer
    function ReadCompressed(Load: TAlgoCompressLoad=aclNormal; BufferOffset: integer=0): RawByteString;
    /// returns TRUE if the current position is the end of the input stream
    function EOF: boolean; {$ifdef HASINLINE}inline;{$endif}
    /// returns remaining length (difference between Last and P)
    function RemainingLength: PtrUInt; {$ifdef HASINLINE}inline;{$endif}
  end;

  /// implements a stack-based writable storage of binary content
  // - memory allocation is performed via a TSynTempBuffer
  TSynTempWriter = object
  public
    /// the current writable position in tmp.buf
    pos: PAnsiChar;
    /// initialize a new temporary buffer of a given number of bytes
    // - if maxsize is left to its 0 default value, the default stack-allocated
    // memory size is used, i.e. 4 KB
    procedure Init(maxsize: integer=0);
    /// finalize the temporary storage
    procedure Done;
    /// append some binary to the internal buffer
    // - will raise an ESynException in case of potential overflow
    procedure wr(const val; len: PtrInt);
    /// append some shortstring as binary to the internal buffer
    procedure wrss(const str: shortstring); {$ifdef HASINLINE}inline;{$endif}
    /// append some string as binary to the internal buffer
    procedure wrs(const str: RawByteString); {$ifdef HASINLINE}inline;{$endif}
    /// append some 8-bit value as binary to the internal buffer
    procedure wrb(b: byte);                 {$ifdef HASINLINE}inline;{$endif}
    /// append some 16-bit value as binary to the internal buffer
    procedure wrw(w: word);                 {$ifdef HASINLINE}inline;{$endif}
    /// append some 32-bit value as binary to the internal buffer
    procedure wrint(int: integer);          {$ifdef HASINLINE}inline;{$endif}
    /// append some 32-bit/64-bit pointer value as binary to the internal buffer
    procedure wrptr(ptr: pointer);          {$ifdef HASINLINE}inline;{$endif}
    /// append some 32-bit/64-bit integer as binary to the internal buffer
    procedure wrptrint(int: PtrInt);        {$ifdef HASINLINE}inline;{$endif}
    /// append some fixed-value bytes as binary to the internal buffer
    // - returns a pointer to the first byte of the added memory chunk
    function wrfillchar(count: integer; value: byte): PAnsiChar;
    /// returns the current offset position in the internal buffer
    function Position: PtrInt; {$ifdef HASINLINE}inline;{$endif}
    /// returns the buffer as a RawByteString instance
    function AsBinary: RawByteString;
    /// returns the buffer as a RawUTF8 instance
    procedure AsUTF8(var result: RawUTF8);
  protected
    tmp: TSynTempBuffer;
  end;

  /// available kind of integer array storage, corresponding to the data layout
  // - wkUInt32 will write the content as "plain" 4 bytes binary (this is the
  // prefered way if the integers can be negative)
  // - wkVarUInt32 will write the content using our 32-bit variable-length integer
  // encoding
  // - wkVarInt32 will write the content using our 32-bit variable-length integer
  // encoding and the by-two complement (0=0,1=1,2=-1,3=2,4=-2...)
  // - wkSorted will write an increasing array of integers, handling the special
  // case of a difference of similar value (e.g. 1) between two values - note
  // that this encoding is efficient only if the difference is main < 253
  // - wkOffsetU and wkOffsetI will write the difference between two successive
  // values, handling constant difference (Unsigned or Integer) in an optimized manner
  // - wkFakeMarker won't be used by WriteVarUInt32Array, but to notify a
  // custom encoding
  TFileBufferWriterKind = (wkUInt32, wkVarUInt32, wkVarInt32, wkSorted,
    wkOffsetU, wkOffsetI, wkFakeMarker);

  /// this class can be used to speed up writing to a file
  // - big speed up if data is written in small blocks
  // - also handle optimized storage of any dynamic array of Integer/Int64/RawUTF8
  // - use TFileBufferReader or TFastReader for decoding of the stored binary
  TFileBufferWriter = class
  private
    fPos: PtrInt;
    fBufLen: PtrInt;
    fStream: TStream;
    fTotalWritten: Int64;
    fInternalStream: boolean;
    fTag: PtrInt;
    fBuffer: PByteArray;
    fBufInternal: RawByteString;
    procedure InternalFlush;
  public
    /// initialize the buffer, and specify a file handle to use for writing
    // - use an internal buffer of the specified size
    constructor Create(aFile: THandle; BufLen: integer=65536); overload;
    /// initialize the buffer, and specify a TStream to use for writing
    // - use an internal buffer of the specified size
    constructor Create(aStream: TStream; BufLen: integer=65536); overload;
    /// initialize the buffer, and specify a file to use for writing
    // - use an internal buffer of the specified size
    // - would replace any existing file by default, unless Append is TRUE
    constructor Create(const aFileName: TFileName; BufLen: integer=65536;
      Append: boolean=false); overload;
    /// initialize the buffer, using an internal TStream instance
    // - parameter could be e.g. THeapMemoryStream or TRawByteStringStream
    // - use Flush then TMemoryStream(Stream) to retrieve its content, or
    // TRawByteStringStream(Stream).DataString
    constructor Create(aClass: TStreamClass; BufLen: integer=4096); overload;
    /// initialize with a specified buffer and TStream class
    // - use a specified external buffer (which may be allocated on stack),
    // to avoid a memory allocation
    constructor Create(aStream: TStream; aTempBuf: pointer; aTempLen: integer); overload;
    /// initialize with a specified buffer
    // - use a specified external buffer (which may be allocated on stack),
    // to avoid a memory allocation
    // - aStream parameter could be e.g. THeapMemoryStream or TRawByteStringStream
    constructor Create(aClass: TStreamClass; aTempBuf: pointer; aTempLen: integer); overload;
    /// release internal TStream (after AssignToHandle call)
    // - warning: an explicit call to Flush is needed to write the data pending
    // in internal buffer
    destructor Destroy; override;
    /// append some data at the current position
    procedure Write(Data: pointer; DataLen: PtrInt); overload;
    /// append 1 byte of data at the current position
    procedure Write1(Data: Byte); {$ifdef HASINLINE}inline;{$endif}
    /// append 2 bytes of data at the current position
    procedure Write2(Data: Word); {$ifdef HASINLINE}inline;{$endif}
    /// append 4 bytes of data at the current position
    procedure Write4(Data: integer); {$ifdef HASINLINE}inline;{$endif}
    /// append 4 bytes of data, encoded as BigEndian, at the current position
    procedure Write4BigEndian(Data: integer); {$ifdef HASINLINE}inline;{$endif}
    /// append 8 bytes of data at the current position
    procedure Write8(const Data8Bytes); {$ifdef HASINLINE}inline;{$endif}
    /// append the same byte a given number of occurences at the current position
    procedure WriteN(Data: Byte; Count: integer);
    /// append some UTF-8 encoded text at the current position
    // - will write the string length (as VarUInt32), then the string content, as expected
    // by the FromVarString() function
    procedure Write(const Text: RawByteString); overload; {$ifdef HASINLINE}inline;{$endif}
    /// append some UTF-8 encoded text at the current position
    // - will write the string length (as VarUInt32), then the string content
    procedure WriteShort(const Text: ShortString);
    /// append some content at the current position
    // - will write the binary data, without any length prefix
    procedure WriteBinary(const Data: RawByteString);
    {$ifndef NOVARIANTS}
    /// append some variant value at the current position
    // - matches FromVarVariant() and VariantSave/VariantLoad format
    procedure Write(const Value: variant); overload;
    /// append some TDocVariant value at the current position, as JSON string
    // - matches TFastReader.NextDocVariantData format
    procedure WriteDocVariantData(const Value: variant);
    {$endif}
    /// append some record at the current position, with binary serialization
    // - will use the binary serialization as for:
    // ! aWriter.WriteBinary(RecordSave(Rec,RecTypeInfo));
    // but writing directly into the buffer, if possible
    procedure WriteRecord(const Rec; RecTypeInfo: pointer);
    /// append some dynamic array at the current position
    // - will use the binary serialization as for:
    // ! aWriter.WriteBinary(DA.SaveTo);
    // but writing directly into the buffer, if possible
    procedure WriteDynArray(const DA: TDynArray);
    /// append "New[0..Len-1] xor Old[0..Len-1]" bytes
    // - as used e.g. by ZeroCompressXor/TSynBloomFilterDiff.SaveTo
    procedure WriteXor(New,Old: PAnsiChar; Len: PtrInt; crc: PCardinal=nil);
    /// append a cardinal value using 32-bit variable-length integer encoding
    procedure WriteVarUInt32(Value: PtrUInt);
    /// append an integer value using 32-bit variable-length integer encoding of
    // the by-two complement of the given value
    procedure WriteVarInt32(Value: PtrInt);
    /// append an integer value using 64-bit variable-length integer encoding of
    // the by-two complement of the given value
    procedure WriteVarInt64(Value: Int64);
    /// append an unsigned integer value using 64-bit variable-length encoding
    procedure WriteVarUInt64(Value: QWord);
    /// append cardinal values (NONE must be negative!) using 32-bit
    // variable-length integer encoding or other specialized algorithm,
    // depending on the data layout
    procedure WriteVarUInt32Array(const Values: TIntegerDynArray; ValuesCount: integer;
      DataLayout: TFileBufferWriterKind);
    /// append cardinal values (NONE must be negative!) using 32-bit
    // variable-length integer encoding or other specialized algorithm,
    // depending on the data layout
    procedure WriteVarUInt32Values(Values: PIntegerArray; ValuesCount: integer;
      DataLayout: TFileBufferWriterKind);
    /// append UInt64 values using 64-bit variable length integer encoding
    // - if Offset is TRUE, then it will store the difference between
    // two values using 64-bit variable-length integer encoding (in this case,
    // a fixed-sized record storage is also handled separately)
    // - could be decoded later on via TFileBufferReader.ReadVarUInt64Array
    procedure WriteVarUInt64DynArray(const Values: TInt64DynArray;
      ValuesCount: integer; Offset: Boolean);
    /// append the RawUTF8 dynamic array
    // - handled the fixed size strings array case in a very efficient way
    procedure WriteRawUTF8DynArray(const Values: TRawUTF8DynArray; ValuesCount: integer);
    /// append a RawUTF8 array of values, from its low-level memory pointer
    // - handled the fixed size strings array case in a very efficient way
    procedure WriteRawUTF8Array(Values: PPtrUIntArray; ValuesCount: integer);
    /// append the RawUTF8List content
    // - if StoreObjectsAsVarUInt32 is TRUE, all Objects[] properties will be
    // stored as VarUInt32
    procedure WriteRawUTF8List(List: TRawUTF8List; StoreObjectsAsVarUInt32: Boolean=false);
    /// append a TStream content
    // - is StreamSize is left as -1, the Stream.Size is used
    // - the size of the content is stored in the resulting stream
    procedure WriteStream(aStream: TCustomMemoryStream; aStreamSize: Integer=-1);
    /// allows to write directly to a memory buffer
    // - caller should specify the maximum possible number of bytes to be written
    // - then write the data to the returned pointer, and call DirectWriteFlush
    function DirectWritePrepare(len: PtrInt; out tmp: RawByteString): PAnsiChar;
    /// finalize a direct write to a memory buffer
    // - by specifying the number of bytes written to the buffer
    procedure DirectWriteFlush(len: PtrInt; const tmp: RawByteString);
    /// write any pending data in the internal buffer to the file
    // - after a Flush, it's possible to call FileSeek64(aFile,....)
    // - returns the number of bytes written between two FLush method calls
    function Flush: Int64;
    /// write any pending data, then call algo.Compress() on the buffer
    // - expect the instance to have been created via
    // ! TFileBufferWriter.Create(TRawByteStringStream)
    // - if algo is left to its default nil, will use global AlgoSynLZ
    // - features direct compression from internal buffer, if stream was not used
    // - BufferOffset could be set to reserve some bytes before the compressed buffer
    function FlushAndCompress(nocompression: boolean=false; algo: TAlgoCompress=nil;
      BufferOffset: integer=0): RawByteString;
    /// rewind the Stream to the position when Create() was called
    // - note that this does not clear the Stream content itself, just
    // move back its writing position to its initial place
    procedure CancelAll; virtual;
    /// the associated writing stream
    property Stream: TStream read fStream;
    /// get the byte count written since last Flush
    property TotalWritten: Int64 read fTotalWritten;
    /// simple property used to store some integer content
    property Tag: PtrInt read fTag write fTag;
  end;

  PFileBufferReader = ^TFileBufferReader;

  /// this structure can be used to speed up reading from a file
  // - use internaly memory mapped files for a file up to 2 GB (Windows has
  // problems with memory mapped files bigger than this size limit - at least
  // with 32-bit executables) - but sometimes, Windows fails to allocate
  // more than 512 MB for a memory map, because it does lack of contiguous
  // memory space: in this case, we fall back on direct file reading
  // - maximum handled file size has no limit (but will use slower direct
  // file reading)
  // - can handle sophisticated storage layout of TFileBufferWriter for
  // dynamic arrays of Integer/Int64/RawUTF8
  // - is defined as an object or as a record, due to a bug
  // in Delphi 2009/2010 compiler (at least): this structure is not initialized
  // if defined as an object on the stack, but will be as a record :(
  TFileBufferReader = object
  protected
    fCurrentPos: PtrUInt;
    fMap: TMemoryMap;
    /// get Isize + buffer from current memory map or fBufTemp into (P,PEnd)
    procedure ReadChunk(out P, PEnd: PByte; var BufTemp: RawByteString);
  public
    /// initialize the buffer, and specify a file to use for reading
    // - will try to map the whole file content in memory
    // - if memory mapping failed, or aFileNotMapped is true, methods
    // will use default slower file API
    procedure Open(aFile: THandle; aFileNotMapped: boolean=false);
    /// initialize the buffer from an already existing memory block
    // - may be e.g. a resource or a TMemoryStream
    procedure OpenFrom(aBuffer: pointer; aBufferSize: PtrUInt); overload;
    /// initialize the buffer from an already existing memory block
    procedure OpenFrom(const aBuffer: RawByteString); overload;
    /// initialize the buffer from an already existing Stream
    // - accept either TFileStream or TCustomMemoryStream kind of stream
    function OpenFrom(Stream: TStream): boolean; overload;
    /// close all internal mapped files
    // - call Open() again to use the Read() methods
    procedure Close;
    {$ifndef CPU64}
    /// change the current reading position, from the beginning of the file
    // - returns TRUE if success, or FALSE if Offset is out of range
    function Seek(Offset: Int64): boolean; overload;
    {$endif}
    /// change the current reading position, from the beginning of the file
    // - returns TRUE if success, or FALSE if Offset is out of range
    function Seek(Offset: PtrInt): boolean; overload;
    /// raise an exception in case of invalid content
    procedure ErrorInvalidContent;
    /// read some bytes from the given reading position
    // - returns the number of bytes which was read
    // - if Data is nil, it won't read content but will forward reading position
    function Read(Data: pointer; DataLen: PtrInt): integer; overload;
    /// read some UTF-8 encoded text at the current position
    // - returns the resulting text length, in bytes
    function Read(out Text: RawUTF8): integer; overload;
    /// read some buffer texgt at the current position
    // - returns the resulting text length, in bytes
    function Read(out Text: RawByteString): integer; overload;
    /// read some UTF-8 encoded text at the current position
    // - returns the resulting text
    function ReadRawUTF8: RawUTF8; {$ifdef HASINLINE}inline;{$endif}
    /// read one byte
    // - if reached end of file, don't raise any error, but returns 0
    function ReadByte: PtrUInt; {$ifdef HASINLINE}inline;{$endif}
    /// read one cardinal, which was written as fixed length
    // - if reached end of file, don't raise any error, but returns 0
    function ReadCardinal: cardinal;
    /// read one cardinal value encoded using our 32-bit variable-length integer
    function ReadVarUInt32: PtrUInt;
    /// read one integer value encoded using our 32-bit variable-length integer,
    // and the by-two complement
    function ReadVarInt32: PtrInt;
    /// read one UInt64 value encoded using our 64-bit variable-length integer
    function ReadVarUInt64: QWord;
    /// read one Int64 value encoded using our 64-bit variable-length integer
    function ReadVarInt64: Int64;
    /// retrieved cardinal values encoded with TFileBufferWriter.WriteVarUInt32Array
    // - returns the number of items read into Values[] (may differ from
    // length(Values), which will be resized, so could be void before calling)
    // - if the returned integer is negative, it is -Count, and testifies from
    // wkFakeMarker and the content should be retrieved by the caller
    function ReadVarUInt32Array(var Values: TIntegerDynArray): PtrInt;
    /// retrieved Int64 values encoded with TFileBufferWriter.WriteVarUInt64DynArray
    // - returns the number of items read into Values[] (may differ from length(Values))
    function ReadVarUInt64Array(var Values: TInt64DynArray): PtrInt;
    /// retrieved RawUTF8 values encoded with TFileBufferWriter.WriteRawUTF8DynArray
    // - returns the number of items read into Values[] (may differ from length(Values))
    function ReadVarRawUTF8DynArray(var Values: TRawUTF8DynArray): PtrInt;
    /// retrieve the RawUTF8List content encoded with TFileBufferWriter.WriteRawUTF8List
    // - if StoreObjectsAsVarUInt32 was TRUE, all Objects[] properties will be
    // retrieved as VarUInt32
    function ReadRawUTF8List(List: TRawUTF8List): boolean;
    /// retrieve a pointer to the current position, for a given data length
    // - if the data is available in the current memory mapped file, it
    // will just return a pointer to it
    // - otherwise (i.e. if the data is split between to 1GB memory map buffers),
    // data will be copied into the temporary aTempData buffer before retrieval
    function ReadPointer(DataLen: PtrUInt; var aTempData: RawByteString): pointer;
    /// create a TMemoryStream instance from the current position
    // - the content size is either specified by DataLen>=0, either available at
    // the current position, as saved by TFileBufferWriter.WriteStream method
    // - if this content fit in the current 1GB memory map buffer, a
    // TSynMemoryStream instance is returned, with no data copy (faster)
    // - if this content is not already mapped in memory, a separate memory map
    // will be created (the returned instance is a TSynMemoryStreamMapped)
    function ReadStream(DataLen: PtrInt=-1): TCustomMemoryStream;
    /// retrieve the current in-memory pointer
    // - if file was not memory-mapped, returns nil
    // - if DataLen>0, will increment the current in-memory position
    function CurrentMemory(DataLen: PtrUInt=0; PEnd: PPAnsiChar=nil): pointer;
    /// retrieve the current in-memory position
    // - if file was not memory-mapped, returns -1
    function CurrentPosition: integer; {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to the global file size
    function FileSize: Int64; {$ifdef HASINLINE}inline;{$endif}
    /// read-only access to the global mapped buffer binary
    function MappedBuffer: PAnsiChar; {$ifdef HASINLINE}inline;{$endif}
  end;

  /// implements a thread-safe Bloom Filter storage
  // - a "Bloom Filter" is a space-efficient probabilistic data structure,
  // that is used to test whether an element is a member of a set. False positive
  // matches are possible, but false negatives are not. Elements can be added to
  // the set, but not removed. Typical use cases are to avoid unecessary
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
  // used, with some random seed values, to simulate several hashing functions
  // - Insert/MayExist/Reset methods are thread-safe
  TSynBloomFilter = class(TSynPersistentLock)
  private
    fSize: cardinal;
    fFalsePositivePercent: double;
    fBits: cardinal;
    fHashFunctions: cardinal;
    fInserted: cardinal;
    fStore: RawByteString;
    function GetInserted: cardinal;
  public
    /// initialize the internal bits storage for a given number of items
    // - by default, internal bits array size will be guess from a 1 % false
    // positive rate - but you may specify another value, to reduce memory use
    // - this constructor would compute and initialize Bits and HashFunctions
    // corresponding to the expected false positive ratio
    constructor Create(aSize: integer; aFalsePositivePercent: double = 1); reintroduce; overload;
    /// initialize the internal bits storage from a SaveTo() binary buffer
    // - this constructor will initialize the internal bits array calling LoadFrom()
    constructor Create(const aSaved: RawByteString; aMagic: cardinal=$B1003F11); reintroduce; overload;
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
    // - this method is thread-safe
    function MayExist(const aValue: RawByteString): boolean; overload;
    /// returns TRUE if the supplied items was probably set via Insert()
    // - some false positive may occur, but not much than FalsePositivePercent
    // - this method is thread-safe
    function MayExist(aValue: pointer; aValueLen: integer): boolean; overload;
    /// store the internal bits array into a binary buffer
    // - may be used to transmit or store the state of a dataset, avoiding
    // to recompute all Insert() at program startup, or to synchronize
    // networks nodes information and reduce the number of remote requests
    function SaveTo(aMagic: cardinal=$B1003F11): RawByteString; overload;
    /// store the internal bits array into a binary buffer
    // - may be used to transmit or store the state of a dataset, avoiding
    // to recompute all Insert() at program startup, or to synchronize
    // networks nodes information and reduce the number of remote requests
    procedure SaveTo(aDest: TFileBufferWriter; aMagic: cardinal=$B1003F11); overload;
    /// read the internal bits array from a binary buffer
    // - as previously serialized by the SaveTo method
    // - may be used to transmit or store the state of a dataset
    function LoadFrom(const aSaved: RawByteString; aMagic: cardinal=$B1003F11): boolean; overload;
    /// read the internal bits array from a binary buffer
    // - as previously serialized by the SaveTo method
    // - may be used to transmit or store the state of a dataset
    function LoadFrom(P: PByte; PLen: integer; aMagic: cardinal=$B1003F11): boolean; overload; virtual;
  published
    /// maximum number of items which are expected to be inserted
    property Size: cardinal read fSize;
    /// expected percentage (1..100) of false positive results for MayExists()
    property FalsePositivePercent: double read fFalsePositivePercent;
    /// number of bits stored in the internal bits array
    property Bits: cardinal read fBits;
    /// how many hash functions would be applied for each Insert()
    property HashFunctions: cardinal read fHashFunctions;
    /// how many times the Insert() method has been called
    property Inserted: cardinal read GetInserted;
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
    property Revision: Int64 read fRevision;
    /// after how many Insert() the internal bits array storage should be
    // promoted as known revision
    // - equals Size div 32 by default
    property SnapshotAfterInsertCount: cardinal read fSnapshotAfterInsertCount
      write fSnapshotAfterInsertCount;
    /// after how many time the internal bits array storage should be
    // promoted as known revision
    // - equals 30 minutes by default
    property SnapShotAfterMinutes: cardinal read fSnapShotAfterMinutes
      write fSnapShotAfterMinutes;
  end;


/// RLE compression of a memory buffer containing mostly zeros
// - will store the number of consecutive zeros instead of plain zero bytes
// - used for spare bit sets, e.g. TSynBloomFilter serialization
// - will also compute the crc32c of the supplied content
// - use ZeroDecompress() to expand the compressed result
// - resulting content would be at most 14 bytes bigger than the input
// - you may use this function before SynLZ compression
procedure ZeroCompress(P: PAnsiChar; Len: integer; Dest: TFileBufferWriter);

/// RLE uncompression of a memory buffer containing mostly zeros
// - returns Dest='' if P^ is not a valid ZeroCompress() function result
// - used for spare bit sets, e.g. TSynBloomFilter serialization
// - will also check the crc32c of the supplied content
procedure ZeroDecompress(P: PByte; Len: integer; {$ifdef FPC}var{$else}out{$endif} Dest: RawByteString);

/// RLE compression of XORed memory buffers resulting in mostly zeros
// - will perform ZeroCompress(Dest^ := New^ xor Old^) without any temporary
// memory allocation
// - is used  e.g. by TSynBloomFilterDiff.SaveToDiff() in incremental mode
// - will also compute the crc32c of the supplied content
procedure ZeroCompressXor(New,Old: PAnsiChar; Len: cardinal; Dest: TFileBufferWriter);

/// RLE uncompression and ORing of a memory buffer containing mostly zeros
// - will perform Dest^ := Dest^ or ZeroDecompress(P^) without any temporary
// memory allocation
// - is used  e.g. by TSynBloomFilterDiff.LoadFromDiff() in incremental mode
// - returns false if P^ is not a valid ZeroCompress/ZeroCompressXor() result
// - will also check the crc32c of the supplied content
function ZeroDecompressOr(P,Dest: PAnsiChar; Len,DestLen: integer): boolean;


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
  Level: integer=DELTA_LEVEL_FAST; BufSize: integer=DELTA_BUF_DEFAULT): RawByteString; overload;

/// compute difference of two binary buffers
// - returns '=' for equal buffers, or an optimized binary delta
// - DeltaExtract() could be used later on to compute New from Old
function DeltaCompress(New, Old: PAnsiChar; NewSize, OldSize: integer;
  Level: integer=DELTA_LEVEL_FAST; BufSize: integer=DELTA_BUF_DEFAULT): RawByteString; overload;

/// compute difference of two binary buffers
// - returns '=' for equal buffers, or an optimized binary delta
// - DeltaExtract() could be used later on to compute New from Old + Delta
// - caller should call Freemem(Delta) once finished with the output buffer
function DeltaCompress(New, Old: PAnsiChar; NewSize, OldSize: integer;
  out Delta: PAnsiChar; Level: integer=DELTA_LEVEL_FAST; BufSize: integer=DELTA_BUF_DEFAULT): integer; overload;

type
  /// result of function DeltaExtract()
  TDeltaError = (
    dsSuccess, dsCrcCopy, dsCrcComp, dsCrcBegin, dsCrcEnd, dsCrcExtract, dsFlag, dsLen);

/// returns how many bytes a DeltaCompress() result will expand to
function DeltaExtractSize(const Delta: RawByteString): integer; overload;

/// returns how many bytes a DeltaCompress() result will expand to
function DeltaExtractSize(Delta: PAnsiChar): integer; overload;

/// apply the delta binary as computed by DeltaCompress()
// - decompression don't use any RAM, will perform crc32c check, and is very fast
// - return dsSuccess if was uncompressed to aOutUpd as expected
function DeltaExtract(const Delta,Old: RawByteString; out New: RawByteString): TDeltaError; overload;

/// low-level apply the delta binary as computed by DeltaCompress()
// - New should already be allocated with DeltaExtractSize(Delta) bytes
// - as such, expect Delta, Old and New to be <> nil, and Delta <> '='
// - return dsSuccess if was uncompressed to aOutUpd as expected
function DeltaExtract(Delta,Old,New: PAnsiChar): TDeltaError; overload;

function ToText(err: TDeltaError): PShortString; overload;


{ ************ high-level storage classes ************************* }

type
  /// implement a cache of some key/value pairs, e.g. to improve reading speed
  // - used e.g. by TSQLDataBase for caching the SELECT statements results in an
  // internal JSON format (which is faster than a query to the SQLite3 engine)
  // - internally make use of an efficient hashing algorithm for fast response
  // (i.e. TSynNameValue will use the TDynArrayHashed wrapper mechanism)
  // - this class is thread-safe if you use properly the associated Safe lock
  TSynCache = class(TSynPersistentLock)
  protected
    fFindLastKey: RawUTF8;
    fNameValue: TSynNameValue;
    fRamUsed: cardinal;
    fMaxRamUsed: cardinal;
    fTimeoutSeconds: cardinal;
    fTimeoutTix: cardinal;
    procedure ResetIfNeeded;
  public
    /// initialize the internal storage
    // - aMaxCacheRamUsed can set the maximum RAM to be used for values, in bytes
    // (default is 16 MB), after which the cache is flushed
    // - by default, key search is done case-insensitively, but you can specify
    // another option here
    // - by default, there is no timeout period, but you may specify a number of
    // seconds of inactivity (i.e. no Add call) after which the cache is flushed
    constructor Create(aMaxCacheRamUsed: cardinal=16 shl 20;
      aCaseSensitive: boolean=false; aTimeoutSeconds: cardinal=0); reintroduce;
    /// find a Key in the cache entries
    // - return '' if nothing found: you may call Add() just after to insert
    // the expected value in the cache
    // - return the associated Value otherwise, and the associated integer tag
    // if aResultTag address is supplied
    // - this method is not thread-safe, unless you call Safe.Lock before
    // calling Find(), and Safe.Unlock after calling Add()
    function Find(const aKey: RawUTF8; aResultTag: PPtrInt=nil): RawUTF8;
    /// add a Key and its associated value (and tag) to the cache entries
    // - you MUST always call Find() with the associated Key first
    // - this method is not thread-safe, unless you call Safe.Lock before
    // calling Find(), and Safe.Unlock after calling Add()
    procedure Add(const aValue: RawUTF8; aTag: PtrInt);
    /// add a Key/Value pair in the cache entries
    // - returns true if aKey was not existing yet, and aValue has been stored
    // - returns false if aKey did already exist in the internal cache, and
    // its entry has been updated with the supplied aValue/aTag
    // - this method is thread-safe, using the Safe locker of this instance
    function AddOrUpdate(const aKey, aValue: RawUTF8; aTag: PtrInt): boolean;
    /// called after a write access to the database to flush the cache
    // - set Count to 0
    // - release all cache memory
    // - returns TRUE if was flushed, i.e. if there was something in cache
    // - this method is thread-safe, using the Safe locker of this instance
    function Reset: boolean;
    /// number of entries in the cache
    function Count: integer;
    /// access to the internal locker, for thread-safe process
    // - Find/Add methods calls should be protected as such:
    // ! cache.Safe.Lock;
    // ! try
    // !   ... cache.Find/cache.Add ...
    // ! finally
    // !   cache.Safe.Unlock;
    // ! end;
    property Safe: PSynLocker read fSafe;
    /// the current global size of Values in RAM cache, in bytes
    property RamUsed: cardinal read fRamUsed;
    /// the maximum RAM to be used for values, in bytes
    // - the cache is flushed when ValueSize reaches this limit
    // - default is 16 MB (16 shl 20)
    property MaxRamUsed: cardinal read fMaxRamUsed;
    /// after how many seconds betwen Add() calls the cache should be flushed
    // - equals 0 by default, meaning no time out
    property TimeoutSeconds: cardinal read fTimeoutSeconds;
  end;

  /// thread-safe FIFO (First-In-First-Out) in-order queue of records
  // - uses internally a dynamic array storage, with a sliding algorithm
  // (more efficient than the FPC or Delphi TQueue)
  TSynQueue = class(TSynPersistentLock)
  protected
    fValues: TDynArray;
    fValueVar: pointer;
    fCount, fFirst, fLast: integer;
    fWaitPopFlags: set of (wpfDestroying);
    fWaitPopCounter: integer;
    procedure InternalGrow;
    function InternalDestroying(incPopCounter: integer): boolean;
    function InternalWaitDone(endtix: Int64; const idle: TThreadMethod): boolean;
  public
    /// initialize the queue storage
    // - aTypeInfo should be a dynamic array TypeInfo() RTTI pointer, which
    // would store the values within this TSynQueue instance
    constructor Create(aTypeInfo: pointer); reintroduce; virtual;
    /// finalize the storage
    // - would release all internal stored values, and call WaitPopFinalize
    destructor Destroy; override;
    /// store one item into the queue
    // - this method is thread-safe, since it will lock the instance
    procedure Push(const aValue);
    /// extract one item from the queue, as FIFO (First-In-First-Out)
    // - returns true if aValue has been filled with a pending item, which
    // is removed from the queue (use Peek if you don't want to remove it)
    // - returns false if the queue is empty
    // - this method is thread-safe, since it will lock the instance
    function Pop(out aValue): boolean;
    /// extract one matching item from the queue, as FIFO (First-In-First-Out)
    // - the current pending item is compared with aAnother value
    function PopEquals(aAnother: pointer; aCompare: TDynArraySortCompare; out aValue): boolean;
    /// lookup one item from the queue, as FIFO (First-In-First-Out)
    // - returns true if aValue has been filled with a pending item, without
    // removing it from the queue (as Pop method does)
    // - returns false if the queue is empty
    // - this method is thread-safe, since it will lock the instance
    function Peek(out aValue): boolean;
    /// waiting extract of one item from the queue, as FIFO (First-In-First-Out)
    // - returns true if aValue has been filled with a pending item within the
    // specified aTimeoutMS time
    // - returns false if nothing was pushed into the queue in time, or if
    // WaitPopFinalize has been called
    // - aWhenIdle could be assigned e.g. to VCL/LCL Application.ProcessMessages
    // - you can optionally compare the pending item before returning it (could
    // be used e.g. when several threads are putting items into the queue)
    // - this method is thread-safe, but will lock the instance only if needed
    function WaitPop(aTimeoutMS: integer; const aWhenIdle: TThreadMethod; out aValue;
      aCompared: pointer=nil; aCompare: TDynArraySortCompare=nil): boolean;
    /// waiting lookup of one item from the queue, as FIFO (First-In-First-Out)
    // - returns a pointer to a pending item within the specified aTimeoutMS
    // time - the Safe.Lock is still there, so that caller could check its content,
    // then call Pop() if it is the expected one, and eventually always call Safe.Unlock
    // - returns nil if nothing was pushed into the queue in time
    // - this method is thread-safe, but will lock the instance only if needed
    function WaitPeekLocked(aTimeoutMS: integer; const aWhenIdle: TThreadMethod): pointer;
    /// ensure any pending or future WaitPop() returns immediately as false
    // - is always called by Destroy destructor
    // - could be also called e.g. from an UI OnClose event to avoid any lock
    // - this method is thread-safe, but will lock the instance only if needed
    procedure WaitPopFinalize(aTimeoutMS: integer=100);
    /// delete all items currently stored in this queue, and void its capacity
    // - this method is thread-safe, since it will lock the instance
    procedure Clear;
    /// initialize a dynamic array with the stored queue items
    // - aDynArrayValues should be a variable defined as aTypeInfo from Create
    // - you can retrieve an optional TDynArray wrapper, e.g. for binary or JSON
    // persistence
    // - this method is thread-safe, and will make a copy of the queue data
    procedure Save(out aDynArrayValues; aDynArray: PDynArray=nil);
    /// returns how many items are currently stored in this queue
    // - this method is thread-safe
    function Count: Integer;
    /// returns how much slots is currently reserved in memory
    // - the queue has an optimized auto-sizing algorithm, you can use this
    // method to return its current capacity
    // - this method is thread-safe
    function Capacity: integer;
    /// returns true if there are some items currently pending in the queue
    // - slightly faster than checking Count=0, and much faster than Pop or Peek
    function Pending: boolean;
  end;

  /// maintain a thread-safe sorted list of TSynPersistentLock objects
  // - will use fast O(log(n)) binary search for efficient search - it is
  // a lighter alternative to TObjectListHashedAbstract/TObjectListPropertyHashed
  // if hashing has a performance cost (e.g. if there are a few items, or
  // deletion occurs regularly)
  // - in practice, insertion becomes slower after around 100,000 items stored
  // - expect to store only TSynPersistentLock inherited items, so that
  // the process is explicitly thread-safe
  // - inherited classes should override the Compare and NewItem abstract methods
  TObjectListSorted = class(TSynPersistentLock)
  protected
    fCount: integer;
    fObjArray: TSynPersistentLockDynArray;
    function FastLocate(const Value; out Index: Integer): boolean;
    procedure InsertNew(Item: TSynPersistentLock; Index: integer);
    // override those methods for actual implementation
    function Compare(Item: TSynPersistentLock; const Value): integer; virtual; abstract;
    function NewItem(const Value): TSynPersistentLock; virtual; abstract;
  public
    /// finalize the list
    destructor Destroy; override;
    /// search a given TSynPersistentLock instance from a value
    // - if returns not nil, caller should make result.Safe.UnLock once finished
    // - will use the TObjectListSortedCompare function for the search
    function FindLocked(const Value): pointer;
    /// search or add a given TSynPersistentLock instance from a value
    // - if returns not nil, caller should make result.Safe.UnLock once finished
    // - added is TRUE if a new void item has just been created
    // - will use the TObjectListSortedCompare function for the search
    function FindOrAddLocked(const Value; out added: boolean): pointer;
    /// remove a given TSynPersistentLock instance from a value
    function Delete(const Value): boolean;
    /// how many items are actually stored
    property Count: Integer read fCount;
    /// low-level access to the stored items
    // - warning: use should be protected by Lock.Enter/Lock.Leave
    property ObjArray: TSynPersistentLockDynArray read fObjArray;
  end;

  /// abstract high-level handling of (SynLZ-)compressed persisted storage
  // - LoadFromReader/SaveToWriter abstract methods should be overriden
  // with proper binary persistence implementation
  TSynPersistentStore = class(TSynPersistentLock)
  protected
    fName: RawUTF8;
    fReader: TFastReader;
    fReaderTemp: PRawByteString;
    fLoadFromLastUncompressed, fSaveToLastUncompressed: integer;
    fLoadFromLastAlgo: TAlgoCompress;
    /// low-level virtual methods implementing the persistence
    procedure LoadFromReader; virtual;
    procedure SaveToWriter(aWriter: TFileBufferWriter); virtual;
  public
    /// initialize a void storage with the supplied name
    constructor Create(const aName: RawUTF8); reintroduce; overload; virtual;
    /// initialize a storage from a SaveTo persisted buffer
    // - raise a EFastReader exception on decoding error
    constructor CreateFrom(const aBuffer: RawByteString;
      aLoad: TAlgoCompressLoad = aclNormal);
    /// initialize a storage from a SaveTo persisted buffer
    // - raise a EFastReader exception on decoding error
    constructor CreateFromBuffer(aBuffer: pointer; aBufferLen: integer;
      aLoad: TAlgoCompressLoad = aclNormal);
    /// initialize a storage from a SaveTo persisted buffer
    // - raise a EFastReader exception on decoding error
    constructor CreateFromFile(const aFileName: TFileName;
      aLoad: TAlgoCompressLoad = aclNormal);
    /// fill the storage from a SaveTo persisted buffer
    // - actually call the LoadFromReader() virtual method for persistence
    // - raise a EFastReader exception on decoding error
    procedure LoadFrom(const aBuffer: RawByteString;
      aLoad: TAlgoCompressLoad = aclNormal); overload;
    /// initialize the storage from a SaveTo persisted buffer
    // - actually call the LoadFromReader() virtual method for persistence
    // - raise a EFastReader exception on decoding error
    procedure LoadFrom(aBuffer: pointer; aBufferLen: integer;
       aLoad: TAlgoCompressLoad = aclNormal); overload; virtual;
    /// initialize the storage from a SaveToFile content
    // - actually call the LoadFromReader() virtual method for persistence
    // - returns false if the file is not found, true if the file was loaded
    // without any problem, or raise a EFastReader exception on decoding error
    function LoadFromFile(const aFileName: TFileName;
      aLoad: TAlgoCompressLoad = aclNormal): boolean;
    /// persist the content as a SynLZ-compressed binary blob
    // - to be retrieved later on via LoadFrom method
    // - actually call the SaveToWriter() protected virtual method for persistence
    // - you can specify ForcedAlgo if you want to override the default AlgoSynLZ
    // - BufferOffset could be set to reserve some bytes before the compressed buffer
    procedure SaveTo(out aBuffer: RawByteString; nocompression: boolean=false;
      BufLen: integer=65536; ForcedAlgo: TAlgoCompress=nil; BufferOffset: integer=0); overload; virtual;
    /// persist the content as a SynLZ-compressed binary blob
    // - just an overloaded wrapper
    function SaveTo(nocompression: boolean=false; BufLen: integer=65536;
      ForcedAlgo: TAlgoCompress=nil; BufferOffset: integer=0): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// persist the content as a SynLZ-compressed binary file
    // - to be retrieved later on via LoadFromFile method
    // - returns the number of bytes of the resulting file
    // - actually call the SaveTo method for persistence
    function SaveToFile(const aFileName: TFileName; nocompression: boolean=false;
      BufLen: integer=65536; ForcedAlgo: TAlgoCompress=nil): PtrUInt;
    /// one optional text associated with this storage
    // - you can define this field as published to serialize its value in log/JSON
    property Name: RawUTF8 read fName;
    /// after a LoadFrom(), contains the uncompressed data size read
    property LoadFromLastUncompressed: integer read fLoadFromLastUncompressed;
    /// after a SaveTo(), contains the uncompressed data size written
    property SaveToLastUncompressed: integer read fSaveToLastUncompressed;
  end;

  /// implement binary persistence and JSON serialization (not deserialization)
  TSynPersistentStoreJson = class(TSynPersistentStore)
  protected
    // append "name" -> inherited should add properties to the JSON object
    procedure AddJSON(W: TTextWriter); virtual;
  public
    /// serialize this instance as a JSON object
    function SaveToJSON(reformat: TTextWriterJSONFormat = jsonCompact): RawUTF8;
  end;


type
  /// item as stored in a TRawByteStringGroup instance
  TRawByteStringGroupValue = record
    Position: integer;
    Value: RawByteString;
  end;
  PRawByteStringGroupValue = ^TRawByteStringGroupValue;
  /// items as stored in a TRawByteStringGroup instance
  TRawByteStringGroupValueDynArray = array of TRawByteStringGroupValue;

  /// store several RawByteString content with optional concatenation
  {$ifdef USERECORDWITHMETHODS}TRawByteStringGroup = record
    {$else}TRawByteStringGroup = object{$endif}
  public
    /// actual list storing the data
    Values: TRawByteStringGroupValueDynArray;
    /// how many items are currently stored in Values[]
    Count: integer;
    /// the current size of data stored in Values[]
    Position: integer;
    /// naive but efficient cache for Find()
    LastFind: integer;
    /// add a new item to Values[]
    procedure Add(const aItem: RawByteString); overload;
    /// add a new item to Values[]
    procedure Add(aItem: pointer; aItemLen: integer); overload;
    {$ifndef DELPHI5OROLDER}
    /// add another TRawByteStringGroup to Values[]
    procedure Add(const aAnother: TRawByteStringGroup); overload;
    /// low-level method to abort the latest Add() call
    // - warning: will work only once, if an Add() has actually been just called:
    // otherwise, the behavior is unexpected, and may wrongly truncate data
    procedure RemoveLastAdd;
    /// compare two TRawByteStringGroup instance stored text
    function Equals(const aAnother: TRawByteStringGroup): boolean;
    {$endif DELPHI5OROLDER}
    /// clear any stored information
    procedure Clear;
    /// append stored information into another RawByteString, and clear content
    procedure AppendTextAndClear(var aDest: RawByteString);
    // compact the Values[] array into a single item
    // - is also used by AsText to compute a single RawByteString
    procedure Compact;
    /// return all content as a single RawByteString
    // - will also compact the Values[] array into a single item (which is returned)
    function AsText: RawByteString;
    /// return all content as a single TByteDynArray
    function AsBytes: TByteDynArray;
    /// save all content into a TTextWriter instance
    procedure Write(W: TTextWriter; Escape: TTextWriterKind=twJSONEscape); overload;
    /// save all content into a TFileBufferWriter instance
    procedure WriteBinary(W: TFileBufferWriter); overload;
    /// save all content as a string into a TFileBufferWriter instance
    // - storing the length as WriteVarUInt32() prefix
    procedure WriteString(W: TFileBufferWriter);
    /// add another TRawByteStringGroup previously serialized via WriteString()
    procedure AddFromReader(var aReader: TFastReader);
    /// returns a pointer to Values[] containing a given position
    // - returns nil if not found
    function Find(aPosition: integer): PRawByteStringGroupValue; overload;
    /// returns a pointer to Values[].Value containing a given position and length
    // - returns nil if not found
    function Find(aPosition, aLength: integer): pointer; overload;
    /// returns the text at a given position in Values[]
    // - text should be in a single Values[] entry
    procedure FindAsText(aPosition, aLength: integer; out aText: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the text at a given position in Values[]
    // - text should be in a single Values[] entry
    function FindAsText(aPosition, aLength: integer): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    {$ifndef NOVARIANTS}
    /// returns the text at a given position in Values[]
    // - text should be in a single Values[] entry
    // - explicitly returns null if the supplied text was not found
    procedure FindAsVariant(aPosition, aLength: integer; out aDest: variant);
      {$ifdef HASINLINE}inline;{$endif}
    {$endif}
    /// append the text at a given position in Values[], JSON escaped by default
    // - text should be in a single Values[] entry
    procedure FindWrite(aPosition, aLength: integer; W: TTextWriter;
      Escape: TTextWriterKind=twJSONEscape; TrailingCharsToIgnore: integer=0);
      {$ifdef HASINLINE}inline;{$endif}
    /// append the blob at a given position in Values[], base-64 encoded
    // - text should be in a single Values[] entry
    procedure FindWriteBase64(aPosition, aLength: integer; W: TTextWriter;
      withMagic: boolean); {$ifdef HASINLINE}inline;{$endif}
    /// copy the text at a given position in Values[]
    // - text should be in a single Values[] entry
    procedure FindMove(aPosition, aLength: integer; aDest: pointer);
  end;
  /// pointer reference to a TRawByteStringGroup
  PRawByteStringGroup = ^TRawByteStringGroup;

  /// simple stack-allocated type for handling a non-void type names list
  // - Delphi "object" is buggy on stack -> also defined as record with methods
  {$ifdef USERECORDWITHMETHODS}TPropNameList = record
    {$else}TPropNameList = object{$endif}
  public
    /// the actual names storage
    Values: TRawUTF8DynArray;
    /// how many items are currently in Values[]
    Count: Integer;
    /// initialize the list
    // - set Count := 0
    procedure Init; {$ifdef HASINLINE}inline;{$endif}
    /// search for a Value within Values[0..Count-1] using IdemPropNameU()
    function FindPropName(const Value: RawUTF8): Integer; {$ifdef HASINLINE}inline;{$endif}
    /// if Value is in Values[0..Count-1] using IdemPropNameU() returns FALSE
    // - otherwise, returns TRUE and add Value to Values[]
    // - any Value='' is rejected
    function AddPropName(const Value: RawUTF8): Boolean;
  end;


{ ************  Security and Identifier classes ************************** }

type
  /// 64-bit integer unique identifier, as computed by TSynUniqueIdentifierGenerator
  // - they are increasing over time (so are much easier to store/shard/balance
  // than UUID/GUID), and contain generation time and a 16-bit process ID
  // - mapped by TSynUniqueIdentifierBits memory structure
  // - may be used on client side for something similar to a MongoDB ObjectID,
  // but compatible with TSQLRecord.ID: TID properties
  TSynUniqueIdentifier = type Int64;

  /// 16-bit unique process identifier, used to compute TSynUniqueIdentifier
  // - each TSynUniqueIdentifierGenerator instance is expected to have
  // its own unique process identifier, stored as a 16 bit integer 1..65535 value
  TSynUniqueIdentifierProcess = type word;

  {$A-}
  /// map 64-bit integer unique identifier internal memory structure
  // - as stored in TSynUniqueIdentifier = Int64 values, and computed by
  // TSynUniqueIdentifierGenerator
  // - bits 0..14 map a 15-bit increasing counter (collision-free)
  // - bits 15..30 map a 16-bit process identifier
  // - bits 31..63 map a 33-bit UTC time, encoded as seconds since Unix epoch
  {$ifdef USERECORDWITHMETHODS}TSynUniqueIdentifierBits = record
    {$else}TSynUniqueIdentifierBits = object{$endif}
  public
    /// the actual 64-bit storage value
    // - in practice, only first 63 bits are used
    Value: TSynUniqueIdentifier;
    /// 15-bit counter (0..32767), starting with a random value
    function Counter: word;
      {$ifdef HASINLINE}inline;{$endif}
    /// 16-bit unique process identifier
    // - as specified to TSynUniqueIdentifierGenerator constructor
    function ProcessID: TSynUniqueIdentifierProcess;
      {$ifdef HASINLINE}inline;{$endif}
    /// low-endian 4-byte value representing the seconds since the Unix epoch
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    // - it uses in fact a 33-bit resolution, so is "Year 2038" bug-free
    function CreateTimeUnix: TUnixTime;
      {$ifdef HASINLINE}inline;{$endif}
    /// fill this unique identifier structure from its TSynUniqueIdentifier value
    // - is just a wrapper around PInt64(@self)^
    procedure From(const AID: TSynUniqueIdentifier);
      {$ifdef HASINLINE}inline;{$endif}
    {$ifndef NOVARIANTS}
    /// convert this identifier as an explicit TDocVariant JSON object
    // - returns e.g.
    // ! {"Created":"2016-04-19T15:27:58","Identifier":1,"Counter":1,
    // ! "Value":3137644716930138113,"Hex":"2B8B273F00008001"}
    function AsVariant: variant; {$ifdef HASINLINE}inline;{$endif}
    /// convert this identifier to an explicit TDocVariant JSON object
    // - returns e.g.
    // ! {"Created":"2016-04-19T15:27:58","Identifier":1,"Counter":1,
    // ! "Value":3137644716930138113,"Hex":"2B8B273F00008001"}
    procedure ToVariant(out result: variant);
    {$endif NOVARIANTS}
    /// extract the UTC generation timestamp from the identifier as TDateTime
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    function CreateDateTime: TDateTime;
      {$ifdef HASINLINE}inline;{$endif}
    /// extract the UTC generation timestamp from the identifier
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    function CreateTimeLog: TTimeLog;
    {$ifndef DELPHI5OROLDER}
    /// compare two Identifiers
    function Equal(const Another: TSynUniqueIdentifierBits): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    {$endif DELPHI5OROLDER}
    /// convert the identifier into a 16 chars hexadecimal string
    function ToHexa: RawUTF8;
      {$ifdef HASINLINE}inline;{$endif}
    /// fill this unique identifier back from a 16 chars hexadecimal string
    // - returns TRUE if the supplied hexadecimal is on the expected format
    // - returns FALSE if the supplied text is invalid
    function FromHexa(const hexa: RawUTF8): boolean;
    /// fill this unique identifier with a fake value corresponding to a given
    // timestamp
    // - may be used e.g. to limit database queries on a particular time range
    // - bits 0..30 would be 0, i.e. would set Counter = 0 and ProcessID = 0
    procedure FromDateTime(const aDateTime: TDateTime);
    /// fill this unique identifier with a fake value corresponding to a given
    // timestamp
    // - may be used e.g. to limit database queries on a particular time range
    // - bits 0..30 would be 0, i.e. would set Counter = 0 and ProcessID = 0
    procedure FromUnixTime(const aUnixTime: TUnixTime);
  end;
  {$A+}

  /// points to a 64-bit integer identifier, as computed by TSynUniqueIdentifierGenerator
  // - may be used to access the identifier internals, from its stored
  // Int64 or TSynUniqueIdentifier value
  PSynUniqueIdentifierBits = ^TSynUniqueIdentifierBits;

  /// a 24 chars cyphered hexadecimal string, mapping a TSynUniqueIdentifier
  // - has handled by TSynUniqueIdentifierGenerator.ToObfuscated/FromObfuscated
  TSynUniqueIdentifierObfuscated = type RawUTF8;

  /// thread-safe 64-bit integer unique identifier computation
  // - may be used on client side for something similar to a MongoDB ObjectID,
  // but compatible with TSQLRecord.ID: TID properties, since it will contain
  // a 63-bit unsigned integer, following our ORM expectations
  // - each identifier would contain a 16-bit process identifier, which is
  // supplied by the application, and should be unique for this process at a
  // given time
  // - identifiers may be obfuscated as hexadecimal text, using both encryption
  // and digital signature
  TSynUniqueIdentifierGenerator = class(TSynPersistent)
  protected
    fUnixCreateTime: cardinal;
    fLatestCounterOverflowUnixCreateTime: cardinal;
    fIdentifier: TSynUniqueIdentifierProcess;
    fIdentifierShifted: cardinal;
    fLastCounter: cardinal;
    fCrypto: array[0..7] of cardinal; // only fCrypto[6..7] are used in practice
    fCryptoCRC: cardinal;
    fSafe: TSynLocker;
    function GetComputedCount: Int64;
  public
    /// initialize the generator for the given 16-bit process identifier
    // - you can supply an obfuscation key, which should be shared for the
    // whole system, so that you may use FromObfuscated/ToObfuscated methods
    constructor Create(aIdentifier: TSynUniqueIdentifierProcess;
      const aSharedObfuscationKey: RawUTF8=''); reintroduce;
    /// finalize the generator structure
    destructor Destroy; override;
    /// return a new unique ID
    // - this method is very optimized, and would use very little CPU
    procedure ComputeNew(out result: TSynUniqueIdentifierBits); overload;
    /// return a new unique ID, type-casted to an Int64
    function ComputeNew: Int64; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// return an unique ID matching this generator pattern, at a given timestamp
    // - may be used e.g. to limit database queries on a particular time range
    procedure ComputeFromDateTime(const aDateTime: TDateTime; out result: TSynUniqueIdentifierBits);
    /// return an unique ID matching this generator pattern, at a given timestamp
    // - may be used e.g. to limit database queries on a particular time range
    procedure ComputeFromUnixTime(const aUnixTime: TUnixTime; out result: TSynUniqueIdentifierBits);
    /// map a TSynUniqueIdentifier as 24 chars cyphered hexadecimal text
    // - cyphering includes simple key-based encryption and a CRC-32 digital signature
    function ToObfuscated(const aIdentifier: TSynUniqueIdentifier): TSynUniqueIdentifierObfuscated;
    /// retrieve a TSynUniqueIdentifier from 24 chars cyphered hexadecimal text
    // - any file extension (e.g. '.jpeg') would be first deleted from the
    // supplied obfuscated text
    // - returns true if the supplied obfuscated text has the expected layout
    // and a valid digital signature
    // - returns false if the supplied obfuscated text is invalid
    function FromObfuscated(const aObfuscated: TSynUniqueIdentifierObfuscated;
      out aIdentifier: TSynUniqueIdentifier): boolean;
    /// some 32-bit value, derivated from aSharedObfuscationKey as supplied
    // to the class constructor
    // - FromObfuscated and ToObfuscated methods will validate their hexadecimal
    // content with this value to secure the associated CRC
    // - may be used e.g. as system-depending salt
    property CryptoCRC: cardinal read fCryptoCRC;
    /// direct access to the associated mutex
    property Safe: TSynLocker read fSafe;
  published
    /// the process identifier, associated with this generator
    property Identifier: TSynUniqueIdentifierProcess read fIdentifier;
    /// how many times ComputeNew method has been called
    property ComputedCount: Int64 read GetComputedCount;
  end;

type
  /// abstract TSynPersistent class allowing safe storage of a password
  // - the associated Password, e.g. for storage or transmission encryption
  // will be persisted encrypted with a private key (which can be customized)
  // - if default simple symmetric encryption is not enough, you may define
  // a custom TSynPersistentWithPasswordUserCrypt callback, e.g. to
  // SynCrypto's CryptDataForCurrentUser, for hardened password storage
  // - a published property should be defined as such in inherited class:
  // ! property PasswordPropertyName: RawUTF8 read fPassword write fPassword;
  // - use the PassWordPlain property to access to its uncyphered value
  TSynPersistentWithPassword = class(TSynPersistent)
  protected
    fPassWord: RawUTF8;
    fKey: cardinal;
    function GetKey: cardinal; {$ifdef HASINLINE}inline;{$endif}
    function GetPassWordPlain: RawUTF8;
    function GetPassWordPlainInternal(AppSecret: RawUTF8): RawUTF8;
    procedure SetPassWordPlain(const Value: RawUTF8);
  public
    /// finalize the instance
    destructor Destroy; override;
    /// this class method could be used to compute the encrypted password,
    // ready to be stored as JSON, according to a given private key
    class function ComputePassword(const PlainPassword: RawUTF8;
      CustomKey: cardinal=0): RawUTF8; overload;
    /// this class method could be used to compute the encrypted password from
    // a binary digest, ready to be stored as JSON, according to a given private key
    // - just a wrapper around ComputePassword(BinToBase64URI())
    class function ComputePassword(PlainPassword: pointer; PlainPasswordLen: integer;
      CustomKey: cardinal=0): RawUTF8; overload;
    /// this class method could be used to decrypt a password, stored as JSON,
    // according to a given private key
    // - may trigger a ESynException if the password was stored using a custom
    // TSynPersistentWithPasswordUserCrypt callback, and the current user
    // doesn't match the expected user stored in the field
    class function ComputePlainPassword(const CypheredPassword: RawUTF8;
      CustomKey: cardinal=0; const AppSecret: RawUTF8=''): RawUTF8;
    /// low-level function used to identify if a given field is a Password
    // - this method is used e.g. by TJSONSerializer.WriteObject to identify the
    // password field, since its published name is set by the inherited classes
    function GetPasswordFieldAddress: pointer; {$ifdef HASINLINE}inline;{$endif}
    /// the private key used to cypher the password storage on serialization
    // - application can override the default 0 value at runtime
    property Key: cardinal read GetKey write fKey;
    /// access to the associated unencrypted Password value
    // - read may trigger a ESynException if the password was stored using a
    // custom TSynPersistentWithPasswordUserCrypt callback, and the current user
    // doesn't match the expected user stored in the field
    property PasswordPlain: RawUTF8 read GetPassWordPlain write SetPassWordPlain;
  end;

var
  /// function prototype to customize TSynPersistent class password storage
  // - is called when 'user1:base64pass1,user2:base64pass2' layout is found,
  // and the current user logged on the system is user1 or user2
  // - you should not call this low-level method, but assign e.g. from SynCrypto:
  // $ TSynPersistentWithPasswordUserCrypt := CryptDataForCurrentUser;
  TSynPersistentWithPasswordUserCrypt:
    function(const Data,AppServer: RawByteString; Encrypt: boolean): RawByteString;

type
  /// could be used to store a credential pair, as user name and password
  // - password will be stored with TSynPersistentWithPassword encryption
  TSynUserPassword = class(TSynPersistentWithPassword)
  protected
    fUserName: RawUTF8;
  published
    /// the associated user name
    property UserName: RawUTF8 read FUserName write FUserName;
    /// the associated encrypted password
    // - use the PasswordPlain public property to access to the uncrypted password
    property Password: RawUTF8 read FPassword write FPassword;
  end;

  /// handle safe storage of any connection properties
  // - would be used by SynDB.pas to serialize TSQLDBConnectionProperties, or
  // by mORMot.pas to serialize TSQLRest instances
  // - the password will be stored as Base64, after a simple encryption as
  // defined by TSynPersistentWithPassword
  // - typical content could be:
  // $ {
  // $	"Kind": "TSQLDBSQLite3ConnectionProperties",
  // $	"ServerName": "server",
  // $	"DatabaseName": "",
  // $	"User": "",
  // $	"Password": "PtvlPA=="
  // $ }
  // - the "Kind" value will be used to let the corresponding TSQLRest or
  // TSQLDBConnectionProperties NewInstance*() class methods create the
  // actual instance, from its class name
  TSynConnectionDefinition = class(TSynPersistentWithPassword)
  protected
    fKind: string;
    fServerName: RawUTF8;
    fDatabaseName: RawUTF8;
    fUser: RawUTF8;
  public
    /// unserialize the database definition from JSON
    // - as previously serialized with the SaveToJSON method
    // - you can specify a custom Key used for password encryption, if the
    // default value is not safe enough for you
    // - this method won't use JSONToObject() so avoid any dependency to mORMot.pas
    constructor CreateFromJSON(const JSON: RawUTF8; Key: cardinal=0); virtual;
    /// serialize the database definition as JSON
    // - this method won't use ObjectToJSON() so avoid any dependency to mORMot.pas
    function SaveToJSON: RawUTF8; virtual;
  published
    /// the class name implementing the connection or TSQLRest instance
    // - will be used to instantiate the expected class type
    property Kind: string read fKind write fKind;
    /// the associated server name (or file, for SQLite3) to be connected to
    property ServerName: RawUTF8 read fServerName write fServerName;
    /// the associated database name (if any), or additional options
    property DatabaseName: RawUTF8 read fDatabaseName write fDatabaseName;
    /// the associated User Identifier (if any)
    property User: RawUTF8 read fUser write fUser;
    /// the associated Password, e.g. for storage or transmission encryption
    // - will be persisted encrypted with a private key
    // - use the PassWordPlain property to access to its uncyphered value
    property Password: RawUTF8 read fPassword write fPassword;
  end;


type
  /// class-reference type (metaclass) of an authentication class
  TSynAuthenticationClass = class of TSynAuthenticationAbstract;

  /// abstract authentication class, implementing safe token/challenge security
  // and a list of active sessions
  // - do not use this class, but plain TSynAuthentication
  TSynAuthenticationAbstract = class
  protected
    fSessions: TIntegerDynArray;
    fSessionsCount: Integer;
    fSessionGenerator: integer;
    fTokenSeed: Int64;
    fSafe: TSynLocker;
    function ComputeCredential(previous: boolean; const UserName,PassWord: RawUTF8): cardinal; virtual;
    function GetPassword(const UserName: RawUTF8; out Password: RawUTF8): boolean; virtual; abstract;
    function GetUsersCount: integer; virtual; abstract;
    // check the given Hash challenge, against stored credentials
    function CheckCredentials(const UserName: RaWUTF8; Hash: cardinal): boolean; virtual;
  public
    /// initialize the authentication scheme
    constructor Create;
    /// finalize the authentation
    destructor Destroy; override;
    /// register one credential for a given user
    // - this abstract method will raise an exception: inherited classes should
    // implement them as expected
    procedure AuthenticateUser(const aName, aPassword: RawUTF8); virtual;
    /// unregister one credential for a given user
    // - this abstract method will raise an exception: inherited classes should
    // implement them as expected
    procedure DisauthenticateUser(const aName: RawUTF8); virtual;
    /// create a new session
    // - should return 0 on authentication error, or an integer session ID
    // - this method will check the User name and password, and create a new session
    function CreateSession(const User: RawUTF8; Hash: cardinal): integer; virtual;
    /// check if the session exists in the internal list
    function SessionExists(aID: integer): boolean;
    /// delete a session
    procedure RemoveSession(aID: integer);
    /// returns the current identification token
    // - to be sent to the client for its authentication challenge
    function CurrentToken: Int64;
    /// the number of current opened sessions
    property SessionsCount: integer read fSessionsCount;
    /// the number of registered users
    property UsersCount: integer read GetUsersCount;
    /// to be used to compute a Hash on the client sude, for a given Token
    // - the token should have been retrieved from the server, and the client
    // should compute and return this hash value, to perform the authentication
    // challenge and create the session
    // - internal algorithm is not cryptographic secure, but fast and safe
    class function ComputeHash(Token: Int64; const UserName,PassWord: RawUTF8): cardinal; virtual;
  end;

  /// simple authentication class, implementing safe token/challenge security
  // - maintain a list of user / name credential pairs, and a list of sessions
  // - is not meant to handle authorization, just plain user access validation
  // - used e.g. by TSQLDBConnection.RemoteProcessMessage (on server side) and
  // TSQLDBProxyConnectionPropertiesAbstract (on client side) in SynDB.pas
  TSynAuthentication = class(TSynAuthenticationAbstract)
  protected
    fCredentials: TSynNameValue; // store user/password pairs
    function GetPassword(const UserName: RawUTF8; out Password: RawUTF8): boolean; override;
    function GetUsersCount: integer; override;
  public
    /// initialize the authentication scheme
    // - you can optionally register one user credential
    constructor Create(const aUserName: RawUTF8=''; const aPassword: RawUTF8=''); reintroduce;
    /// register one credential for a given user
    procedure AuthenticateUser(const aName, aPassword: RawUTF8); override;
    /// unregister one credential for a given user
    procedure DisauthenticateUser(const aName: RawUTF8); override;
  end;

type
  /// optimized thread-safe storage of a list of IP v4 adresses
  // - can be used e.g. as white-list or black-list of clients
  // - will maintain internally a sorted list of 32-bit integers for fast lookup
  // - with optional binary persistence
  TIPBan = class(TSynPersistentStore)
  protected
    fIP4: TIntegerDynArray;
    fCount: integer;
    procedure LoadFromReader; override;
    procedure SaveToWriter(aWriter: TFileBufferWriter); override;
  public
    /// register one IP to the list
    function Add(const aIP: RawUTF8): boolean;
    /// unregister one IP to the list
    function Delete(const aIP: RawUTF8): boolean;
    /// returns true if the IP is in the list
    function Exists(const aIP: RawUTF8): boolean;
    /// creates a TDynArray wrapper around the stored list of values
    // - could be used e.g. for binary persistence
    // - warning: caller should make Safe.Unlock when finished
    function DynArrayLocked: TDynArray;
    /// low-level access to the internal IPv4 list
    // - 32-bit unsigned values are sorted, for fast O(log(n)) binary search
    property IP4: TIntegerDynArray read fIP4;
  published
    /// how many IPs are currently banned
    property Count: integer read fCount;
  end;


{ ************ Expression Search Engine ************************** }

type
  /// exception type used by TExprParser
  EExprParser = class(ESynException);

  /// identify an expression search engine node type, as used by TExprParser
  TExprNodeType = (entWord, entNot, entOr, entAnd);

  /// results returned by TExprParserAbstract.Parse method
  TExprParserResult = (
    eprSuccess, eprNoExpression,
    eprMissingParenthesis, eprTooManyParenthesis, eprMissingFinalWord,
    eprInvalidExpression, eprUnknownVariable, eprUnsupportedOperator,
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
    property Next: TExprNode read fNext;
    /// what is actually stored in this node
    property NodeType: TExprNodeType read fNodeType;
  end;

  /// abstract class to handle word search, as used by TExprParser
  TExprNodeWordAbstract = class(TExprNode)
  protected
    fOwner: TParserAbstract;
    fWord: RawUTF8;
    /// should be set from actual data before TExprParser.Found is called
    fFound: boolean;
    function ParseWord: TExprParserResult; virtual; abstract;
  public
    /// you should override this virtual constructor for proper initialization
    constructor Create(aOwner: TParserAbstract; const aWord: RawUTF8); reintroduce; virtual;
  end;

  /// class-reference type (metaclass) for a TExprNode
  // - allow to customize the actual searching process for entWord
  TExprNodeWordClass = class of TExprNodeWordAbstract;

  /// parent class of TExprParserAbstract
  TParserAbstract = class(TSynPersistent)
  protected
    fExpression, fCurrentWord, fAndWord, fOrWord, fNotWord: RawUTF8;
    fCurrent: PUTF8Char;
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
    function Execute: boolean; {$ifdef HASINLINE}inline;{$endif}
  public
    /// initialize an expression parser
    constructor Create; override;
    /// finalize the expression parser
    destructor Destroy; override;
    /// initialize the parser from a given text expression
    function Parse(const aExpression: RawUTF8): TExprParserResult;
    /// try this parser class on a given text expression
    // - returns '' on success, or an explicit error message (e.g.
    // 'Missing parenthesis')
    class function ParseError(const aExpression: RawUTF8): RawUTF8;
    /// the associated text expression used to define the search
    property Expression: RawUTF8 read fExpression;
    /// how many words did appear in the search expression
    property WordCount: integer read fWordCount;
  end;

  /// abstract class to parse a text expression into nodes
  // - you should inherit this class to provide actual text search
  // - searched expressions can use parenthesis and &=AND -=WITHOUT +=OR operators,
  // e.g. '((w1 & w2) - w3) + w4' means ((w1 and w2) without w3) or w4
  // - no operator is handled like a AND, e.g. 'w1 w2' = 'w1 & w2'
  TExprParserAbstract = class(TParserAbstract)
  protected
    procedure ParseNextCurrentWord; override;
    // may be overriden to provide custom words escaping (e.g. handle quotes)
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
    function Search(aText: PUTF8Char; aTextLen: PtrInt): boolean; overload;
    /// returns TRUE if the expression is within the text buffer
    function Search(const aText: RawUTF8): boolean; overload; {$ifdef HASINLINE}inline;{$endif}
  end;

const
  /// may be used when overriding TExprParserAbstract.ParseNextWord method
  PARSER_STOPCHAR = ['&', '+', '-', '(', ')'];

function ToText(r: TExprParserResult): PShortString; overload;
function ToUTF8(r: TExprParserResult): RawUTF8; overload;


{ ************ Multi-Threading classes ************************** }

type
  /// internal item definition, used by TPendingTaskList storage
  TPendingTaskListItem = packed record
    /// the task should be executed when TPendingTaskList.GetTimestamp reaches
    // this value
    Timestamp: Int64;
    /// the associated task, stored by representation as raw binary
    Task: RawByteString;
  end;
  /// internal list definition, used by TPendingTaskList storage
  TPendingTaskListItemDynArray = array of TPendingTaskListItem;

  /// handle a list of tasks, stored as RawByteString, with a time stamp
  // - internal time stamps would be GetTickCount64 by default, so have a
  // resolution of about 16 ms under Windows
  // - you can add tasks to the internal list, to be executed after a given
  // delay, using a post/peek like algorithm
  // - execution delays are not expected to be accurate, but are best guess,
  // according to NextTask call
  // - this implementation is thread-safe, thanks to the Safe internal locker
  TPendingTaskList = class(TSynPersistentLock)
  protected
    fCount: Integer;
    fTask: TPendingTaskListItemDynArray;
    fTasks: TDynArray;
    function GetCount: integer;
    function GetTimestamp: Int64; virtual;
  public
    /// initialize the list memory and resources
    constructor Create; override;
    /// append a task, specifying a delay in milliseconds from current time
    procedure AddTask(aMilliSecondsDelayFromNow: integer; const aTask: RawByteString); virtual;
    /// append several tasks, specifying a delay in milliseconds between tasks
    // - first supplied delay would be computed from the current time, then
    // it would specify how much time to wait between the next supplied task
    procedure AddTasks(const aMilliSecondsDelays: array of integer;
      const aTasks: array of RawByteString);
    /// retrieve the next pending task
    // - returns '' if there is no scheduled task available at the current time
    // - returns the next stack as defined corresponding to its specified delay
    function NextPendingTask: RawByteString; virtual;
    /// flush all pending tasks
    procedure Clear; virtual;
    /// access to the internal TPendingTaskListItem.Timestamp stored value
    // - corresponding to the current time
    // - default implementation is to return GetTickCount64, with a 16 ms
    // typical resolution under Windows
    property Timestamp: Int64 read GetTimestamp;
    /// how many pending tasks are currently defined
    property Count: integer read GetCount;
    /// direct low-level access to the internal task list
    // - warning: this dynamic array length is the list capacity: use Count
    // property to retrieve the exact number of stored items
    // - use Safe.Lock/TryLock with a try ... finally Safe.Unlock block for
    // thread-safe access to this array
    // - items are stored in increasing Timestamp, i.e. the first item is
    // the next one which would be returned by the NextPendingTask method
    property Task: TPendingTaskListItemDynArray read fTask;
  end;

{$ifndef LVCL} // LVCL does not implement TEvent

type
  {$M+}
  TSynBackgroundThreadAbstract = class;
  TSynBackgroundThreadEvent = class;
  {$M-}

  /// idle method called by TSynBackgroundThreadAbstract in the caller thread
  // during remote blocking process in a background thread
  // - typical use is to run Application.ProcessMessages, e.g. for
  // TSQLRestClientURI.URI() to provide a responsive UI even in case of slow
  // blocking remote access
  // - provide the time elapsed (in milliseconds) from the request start (can be
  // used e.g. to popup a temporary message to wait)
  // - is call once with ElapsedMS=0 at request start
  // - is call once with ElapsedMS=-1 at request ending
  // - see TLoginForm.OnIdleProcess and OnIdleProcessForm in mORMotUILogin.pas
  TOnIdleSynBackgroundThread = procedure(Sender: TSynBackgroundThreadAbstract;
    ElapsedMS: Integer) of object;

  /// event prototype used e.g. by TSynBackgroundThreadAbstract callbacks
  // - a similar signature is defined in SynCrtSock and LVCL.Classes
  TNotifyThreadEvent = procedure(Sender: TThread) of object;

  /// abstract TThread with its own execution content
  // - you should not use this class directly, but use either
  // TSynBackgroundThreadMethodAbstract / TSynBackgroundThreadEvent /
  // TSynBackgroundThreadMethod and provide a much more convenient callback
  TSynBackgroundThreadAbstract = class(TThread)
  protected
    fProcessEvent: TEvent;
    fOnBeforeExecute: TNotifyThreadEvent;
    fOnAfterExecute: TNotifyThreadEvent;
    fThreadName: RawUTF8;
    fExecute: (exCreated,exRun,exFinished);
    fExecuteLoopPause: boolean;
    procedure SetExecuteLoopPause(dopause: boolean);
    /// where the main process takes place
    procedure Execute; override;
    procedure ExecuteLoop; virtual; abstract;
  public
    /// initialize the thread
    // - you could define some callbacks to nest the thread execution, e.g.
    // assigned to TSQLRestServer.BeginCurrentThread/EndCurrentThread, or
    // at least set OnAfterExecute to TSynLogFamily.OnThreadEnded
    constructor Create(const aThreadName: RawUTF8; OnBeforeExecute: TNotifyThreadEvent=nil;
      OnAfterExecute: TNotifyThreadEvent=nil; CreateSuspended: boolean=false); reintroduce;
    /// release used resources
    destructor Destroy; override;
    {$ifndef HASTTHREADSTART}
    /// method to be called to start the thread
    // - Resume is deprecated in the newest RTL, since some OS - e.g. Linux -
    // do not implement this pause/resume feature; we define here this method
    // for older versions of Delphi
    procedure Start;
    {$endif}
    {$ifdef HASTTHREADTERMINATESET}
    /// properly terminate the thread
    // - called by TThread.Terminate
    procedure TerminatedSet; override;
    {$else}
    /// properly terminate the thread
    // - called by reintroduced Terminate
    procedure TerminatedSet; virtual;
    /// reintroduced to call TeminatedSet
    procedure Terminate; reintroduce;
    {$endif}
    /// wait for Execute/ExecuteLoop to be ended (i.e. fExecute<>exRun)
    procedure WaitForNotExecuting(maxMS: integer=500);
    /// temporary stop the execution of ExecuteLoop, until set back to false
    // - may be used e.g. by TSynBackgroundTimer to delay the process of
    // background tasks
    property Pause: boolean read fExecuteLoopPause write SetExecuteLoopPause;
    /// access to the low-level associated event used to notify task execution
    // to the background thread
    // - you may call ProcessEvent.SetEvent to trigger the internal process loop
    property ProcessEvent: TEvent read fProcessEvent;
    /// defined as public since may be used to terminate the processing methods
    property Terminated;
  end;

  /// state machine status of the TSynBackgroundThreadAbstract process
  TSynBackgroundThreadProcessStep = (
    flagIdle, flagStarted, flagFinished, flagDestroying);

  /// state machine statuses of the TSynBackgroundThreadAbstract process
  TSynBackgroundThreadProcessSteps = set of TSynBackgroundThreadProcessStep;

  /// abstract TThread able to run a method in its own execution content
  // - typical use is a background thread for processing data or remote access,
  // while the UI will be still responsive by running OnIdle event in loop: see
  // e.g. how TSQLRestClientURI.OnIdle handle this in mORMot.pas unit
  // - you should not use this class directly, but inherit from it and override
  // the Process method, or use either TSynBackgroundThreadEvent /
  // TSynBackgroundThreadMethod and provide a much more convenient callback
  TSynBackgroundThreadMethodAbstract = class(TSynBackgroundThreadAbstract)
  protected
    fCallerEvent: TEvent;
    fParam: pointer;
    fCallerThreadID: TThreadID;
    fBackgroundException: Exception;
    fOnIdle: TOnIdleSynBackgroundThread;
    fOnBeforeProcess: TNotifyThreadEvent;
    fOnAfterProcess: TNotifyThreadEvent;
    fPendingProcessFlag: TSynBackgroundThreadProcessStep;
    fPendingProcessLock: TSynLocker;
    procedure ExecuteLoop; override;
    function OnIdleProcessNotify(start: Int64): integer;
    function GetOnIdleBackgroundThreadActive: boolean;
    function GetPendingProcess: TSynBackgroundThreadProcessStep;
    procedure SetPendingProcess(State: TSynBackgroundThreadProcessStep);
    // returns  flagIdle if acquired, flagDestroying if terminated
    function AcquireThread: TSynBackgroundThreadProcessStep;
    procedure WaitForFinished(start: Int64; const onmainthreadidle: TNotifyEvent);
    /// called by Execute method when fProcessParams<>nil and fEvent is notified
    procedure Process; virtual; abstract;
  public
    /// initialize the thread
    // - if aOnIdle is not set (i.e. equals nil), it will simply wait for
    // the background process to finish until RunAndWait() will return
    // - you could define some callbacks to nest the thread execution, e.g.
    // assigned to TSQLRestServer.BeginCurrentThread/EndCurrentThread
    constructor Create(aOnIdle: TOnIdleSynBackgroundThread;
      const aThreadName: RawUTF8; OnBeforeExecute: TNotifyThreadEvent=nil;
      OnAfterExecute: TNotifyThreadEvent=nil); reintroduce;
    /// finalize the thread
    destructor Destroy; override;
    /// launch Process abstract method asynchronously in the background thread
    // - wait until process is finished, calling OnIdle() callback in
    // the meanwhile
    // - any exception raised in background thread will be translated in the
    // caller thread
    // - returns false if self is not set, or if called from the same thread
    // as it is currently processing (to avoid race condition from OnIdle()
    // callback)
    // - returns true when the background process is finished
    // - OpaqueParam will be used to specify a thread-safe content for the
    // background process
    // - this method is thread-safe, that is it will wait for any started process
    // already launch by another thread: you may call this method from any
    // thread, even if its main purpose is to be called from the main UI thread
    function RunAndWait(OpaqueParam: pointer): boolean;
    /// set a callback event to be executed in loop during remote blocking
    // process, e.g. to refresh the UI during a somewhat long request
    // - you can assign a callback to this property, calling for instance
    // Application.ProcessMessages, to execute the remote request in a
    // background thread, but let the UI still be reactive: the
    // TLoginForm.OnIdleProcess and OnIdleProcessForm methods of
    // mORMotUILogin.pas will match this property expectations
    // - if OnIdle is not set (i.e. equals nil), it will simply wait for
    // the background process to finish until RunAndWait() will return
    property OnIdle: TOnIdleSynBackgroundThread read fOnIdle write fOnIdle;
    /// TRUE if the background thread is active, and OnIdle event is called
    // during process
    // - to be used e.g. to ensure no re-entrance from User Interface messages
    property OnIdleBackgroundThreadActive: Boolean read GetOnIdleBackgroundThreadActive;
    /// optional callback event triggered in Execute before each Process
    property OnBeforeProcess: TNotifyThreadEvent read fOnBeforeProcess write fOnBeforeProcess;
    /// optional callback event triggered in Execute after each Process
    property OnAfterProcess: TNotifyThreadEvent read fOnAfterProcess write fOnAfterProcess;
  end;

  /// background process method called by TSynBackgroundThreadEvent
  // - will supply the OpaqueParam parameter as provided to RunAndWait()
  // method when the Process virtual method will be executed
  TOnProcessSynBackgroundThread = procedure(Sender: TSynBackgroundThreadEvent;
    ProcessOpaqueParam: pointer) of object;

  /// allow background thread process of a method callback
  TSynBackgroundThreadEvent = class(TSynBackgroundThreadMethodAbstract)
  protected
    fOnProcess: TOnProcessSynBackgroundThread;
    /// just call the OnProcess handler
    procedure Process; override;
  public
    /// initialize the thread
    // - if aOnIdle is not set (i.e. equals nil), it will simply wait for
    // the background process to finish until RunAndWait() will return
    constructor Create(aOnProcess: TOnProcessSynBackgroundThread;
      aOnIdle: TOnIdleSynBackgroundThread; const aThreadName: RawUTF8); reintroduce;
    /// provide a method handler to be execute in the background thread
    // - triggered by RunAndWait() method - which will wait until finished
    // - the OpaqueParam as specified to RunAndWait() will be supplied here
    property OnProcess: TOnProcessSynBackgroundThread read fOnProcess write fOnProcess;
  end;

  /// allow background thread process of a variable TThreadMethod callback
  TSynBackgroundThreadMethod = class(TSynBackgroundThreadMethodAbstract)
  protected
    /// just call the TThreadMethod, as supplied to RunAndWait()
    procedure Process; override;
  public
    /// run once the supplied TThreadMethod callback
    // - use this method, and not the inherited RunAndWait()
    procedure RunAndWait(Method: TThreadMethod); reintroduce;
  end;

  /// background process procedure called by TSynBackgroundThreadProcedure
  // - will supply the OpaqueParam parameter as provided to RunAndWait()
  // method when the Process virtual method will be executed
  TOnProcessSynBackgroundThreadProc = procedure(ProcessOpaqueParam: pointer);

  /// allow background thread process of a procedure callback
  TSynBackgroundThreadProcedure = class(TSynBackgroundThreadMethodAbstract)
  protected
    fOnProcess: TOnProcessSynBackgroundThreadProc;
    /// just call the OnProcess handler
    procedure Process; override;
  public
    /// initialize the thread
    // - if aOnIdle is not set (i.e. equals nil), it will simply wait for
    // the background process to finish until RunAndWait() will return
    constructor Create(aOnProcess: TOnProcessSynBackgroundThreadProc;
      aOnIdle: TOnIdleSynBackgroundThread; const aThreadName: RawUTF8); reintroduce;
    /// provide a procedure handler to be execute in the background thread
    // - triggered by RunAndWait() method - which will wait until finished
    // - the OpaqueParam as specified to RunAndWait() will be supplied here
    property OnProcess: TOnProcessSynBackgroundThreadProc read fOnProcess write fOnProcess;
  end;

  /// an exception which would be raised by TSynParallelProcess
  ESynParallelProcess = class(ESynException);

  /// callback implementing some parallelized process for TSynParallelProcess
  // - if 0<=IndexStart<=IndexStop, it should execute some process
  TSynParallelProcessMethod = procedure(IndexStart, IndexStop: integer) of object;

  /// thread executing process for TSynParallelProcess
  TSynParallelProcessThread = class(TSynBackgroundThreadMethodAbstract)
  protected
    fMethod: TSynParallelProcessMethod;
    fIndexStart, fIndexStop: integer;
    procedure Start(Method: TSynParallelProcessMethod; IndexStart,IndexStop: integer);
    /// executes fMethod(fIndexStart,fIndexStop)
    procedure Process; override;
  public
  end;

  /// allow parallel execution of an index-based process in a thread pool
  // - will create its own thread pool, then execute any method by spliting the
  // work into each thread
  TSynParallelProcess = class(TSynPersistentLock)
  protected
    fThreadName: RawUTF8;
    fPool: array of TSynParallelProcessThread;
    fThreadPoolCount: integer;
    fParallelRunCount: integer;
  public
    /// initialize the thread pool
    // - you could define some callbacks to nest the thread execution, e.g.
    // assigned to TSQLRestServer.BeginCurrentThread/EndCurrentThread
    // - up to MaxThreadPoolCount=32 threads could be setup (you may allow a
    // bigger value, but interrest of this thread pool is to have its process
    // saturating each CPU core)
    // - if ThreadPoolCount is 0, no thread would be created, and process
    // would take place in the current thread
    constructor Create(ThreadPoolCount: integer; const ThreadName: RawUTF8;
      OnBeforeExecute: TNotifyThreadEvent=nil; OnAfterExecute: TNotifyThreadEvent=nil;
      MaxThreadPoolCount: integer = 32); reintroduce; virtual;
    /// finalize the thread pool
    destructor Destroy; override;
    /// run a method in parallel, and wait for the execution to finish
    // - will split Method[0..MethodCount-1] execution over the threads
    // - in case of any exception during process, an ESynParallelProcess
    // exception would be raised by this method
    // - if OnMainThreadIdle is set, the current thread (which is expected to be
    // e.g. the main UI thread) won't process anything, but call this event
    // during waiting for the background threads
    procedure ParallelRunAndWait(const Method: TSynParallelProcessMethod;
      MethodCount: integer; const OnMainThreadIdle: TNotifyEvent = nil);
  published
    /// how many threads have been activated
    property ParallelRunCount: integer read fParallelRunCount;
    /// how many threads are currently in this instance thread pool
    property ThreadPoolCount: integer read fThreadPoolCount;
    /// some text identifier, used to distinguish each owned thread
    property ThreadName: RawUTF8 read fThreadName;
  end;

  TSynBackgroundThreadProcess = class;

  /// event callback executed periodically by TSynBackgroundThreadProcess
  // - Event is wrTimeout after the OnProcessMS waiting period
  // - Event is wrSignaled if ProcessEvent.SetEvent has been called
  TOnSynBackgroundThreadProcess = procedure(Sender: TSynBackgroundThreadProcess;
    Event: TWaitResult) of object;

  /// TThread able to run a method at a given periodic pace
  TSynBackgroundThreadProcess = class(TSynBackgroundThreadAbstract)
  protected
    fOnProcess: TOnSynBackgroundThreadProcess;
    fOnException: TNotifyEvent;
    fOnProcessMS: cardinal;
    fStats: TSynMonitor;
    procedure ExecuteLoop; override;
  public
    /// initialize the thread for a periodic task processing
    // - aOnProcess would be called when ProcessEvent.SetEvent is called or
    // aOnProcessMS milliseconds period was elapse since last process
    // - if aOnProcessMS is 0, will wait until ProcessEvent.SetEvent is called
    // - you could define some callbacks to nest the thread execution, e.g.
    // assigned to TSQLRestServer.BeginCurrentThread/EndCurrentThread
    constructor Create(const aThreadName: RawUTF8;
      aOnProcess: TOnSynBackgroundThreadProcess; aOnProcessMS: cardinal;
      aOnBeforeExecute: TNotifyThreadEvent=nil;
      aOnAfterExecute: TNotifyThreadEvent=nil;
      aStats: TSynMonitorClass=nil; CreateSuspended: boolean=false); reintroduce; virtual;
    /// finalize the thread
    destructor Destroy; override;
    /// access to the implementation event of the periodic task
    property OnProcess: TOnSynBackgroundThreadProcess read fOnProcess;
    /// event callback executed when OnProcess did raise an exception
    // - supplied Sender parameter is the raised Exception instance
    property OnException: TNotifyEvent read fOnException write fOnException;
  published
    /// access to the delay, in milliseconds, of the periodic task processing
    property OnProcessMS: cardinal read fOnProcessMS write fOnProcessMS;
    /// processing statistics
    // - may be nil if aStats was nil in the class constructor
    property Stats: TSynMonitor read fStats;
  end;

  TSynBackgroundTimer = class;

  /// event callback executed periodically by TSynBackgroundThreadProcess
  // - Event is wrTimeout after the OnProcessMS waiting period
  // - Event is wrSignaled if ProcessEvent.SetEvent has been called
  // - Msg is '' if there is no pending message in this task FIFO
  // - Msg is set for each pending message in this task FIFO
  TOnSynBackgroundTimerProcess = procedure(Sender: TSynBackgroundTimer;
    Event: TWaitResult; const Msg: RawUTF8) of object;

  /// used by TSynBackgroundTimer internal registration list
  TSynBackgroundTimerTask = record
    OnProcess: TOnSynBackgroundTimerProcess;
    Secs: cardinal;
    NextTix: Int64;
    FIFO: TRawUTF8DynArray;
  end;
  /// stores TSynBackgroundTimer internal registration list
  TSynBackgroundTimerTaskDynArray = array of TSynBackgroundTimerTask;

  /// TThread able to run one or several tasks at a periodic pace in a
  // background thread
  // - as used e.g. by TSQLRest.TimerEnable/TimerDisable methods, via the
  // inherited TSQLRestBackgroundTimer
  // - each process can have its own FIFO of text messages
  // - if you expect to update some GUI, you should rather use a TTimer
  // component (with a period of e.g. 200ms), since TSynBackgroundTimer will
  // use its own separated thread
  TSynBackgroundTimer = class(TSynBackgroundThreadProcess)
  protected
    fTask: TSynBackgroundTimerTaskDynArray;
    fTasks: TDynArray;
    fTaskLock: TSynLocker;
    procedure EverySecond(Sender: TSynBackgroundThreadProcess; Event: TWaitResult);
    function Find(const aProcess: TMethod): integer;
    function Add(aOnProcess: TOnSynBackgroundTimerProcess;
      const aMsg: RawUTF8; aExecuteNow: boolean): boolean;
  public
    /// initialize the thread for a periodic task processing
    // - you could define some callbacks to nest the thread execution, e.g.
    // assigned to TSQLRestServer.BeginCurrentThread/EndCurrentThread, as
    // made by TSQLRestBackgroundTimer.Create
    constructor Create(const aThreadName: RawUTF8;
      aOnBeforeExecute: TNotifyThreadEvent=nil; aOnAfterExecute: TNotifyThreadEvent=nil;
      aStats: TSynMonitorClass=nil); reintroduce; virtual;
    /// finalize the thread
    destructor Destroy; override;
    /// define a process method for a task running on a periodic number of seconds
    // - for background process on a mORMot service, consider using TSQLRest
    // TimerEnable/TimerDisable methods, and its associated BackgroundTimer thread
    procedure Enable(aOnProcess: TOnSynBackgroundTimerProcess; aOnProcessSecs: cardinal);
    /// undefine a task running on a periodic number of seconds
    // - aOnProcess should have been registered by a previous call to Enable() method
    // - returns true on success, false if the supplied task was not registered
    // - for background process on a mORMot service, consider using TSQLRestServer
    // TimerEnable/TimerDisable methods, and their TSynBackgroundTimer thread
    function Disable(aOnProcess: TOnSynBackgroundTimerProcess): boolean;
    /// add a message to be processed during the next execution of a task
    // - supplied message will be added to the internal FIFO list associated
    // with aOnProcess, then supplied to as aMsg parameter for each call
    // - if aExecuteNow is true, won't wait for the next aOnProcessSecs occurence
    // - aOnProcess should have been registered by a previous call to Enable() method
    // - returns true on success, false if the supplied task was not registered
    function EnQueue(aOnProcess: TOnSynBackgroundTimerProcess;
      const aMsg: RawUTF8; aExecuteNow: boolean=false): boolean; overload;
    /// add a message to be processed during the next execution of a task
    // - supplied message will be added to the internal FIFO list associated
    // with aOnProcess, then supplied to as aMsg parameter for each call
    // - if aExecuteNow is true, won't wait for the next aOnProcessSecs occurence
    // - aOnProcess should have been registered by a previous call to Enable() method
    // - returns true on success, false if the supplied task was not registered
    function EnQueue(aOnProcess: TOnSynBackgroundTimerProcess;
      const aMsgFmt: RawUTF8; const Args: array of const; aExecuteNow: boolean=false): boolean; overload;
    /// remove a message from the processing list
    // - supplied message will be searched in the internal FIFO list associated
    // with aOnProcess, then removed from the list if found
    // - aOnProcess should have been registered by a previous call to Enable() method
    // - returns true on success, false if the supplied message was not registered
    function DeQueue(aOnProcess: TOnSynBackgroundTimerProcess; const aMsg: RawUTF8): boolean;
    /// execute a task without waiting for the next aOnProcessSecs occurence
    // - aOnProcess should have been registered by a previous call to Enable() method
    // - returns true on success, false if the supplied task was not registered
    function ExecuteNow(aOnProcess: TOnSynBackgroundTimerProcess): boolean;
    /// returns true if there is currenly one task processed
    function Processing: boolean;
    /// wait until no background task is processed
    procedure WaitUntilNotProcessing(timeoutsecs: integer=10);
    /// low-level access to the internal task list
    property Task: TSynBackgroundTimerTaskDynArray read fTask;
    /// low-level access to the internal task mutex
    property TaskLock: TSynLocker read fTaskLock;
  end;

  /// the current state of a TBlockingProcess instance
  TBlockingEvent = (evNone,evWaiting,evTimeOut,evRaised);

  {$M+}
  /// a semaphore used to wait for some process to be finished
  // - used e.g. by TBlockingCallback in mORMot.pas
  // - once created, process would block via a WaitFor call, which would be
  // released when NotifyFinished is called by the process background thread
  TBlockingProcess = class(TEvent)
  protected
    fTimeOutMs: integer;
    fEvent: TBlockingEvent;
    fSafe: PSynLocker;
    fOwnedSafe: boolean;
    procedure ResetInternal; virtual; // override to reset associated params
  public
    /// initialize the semaphore instance
    // - specify a time out millliseconds period after which blocking execution
    // should be handled as failure (if 0 is set, default 3000 would be used)
    // - an associated mutex shall be supplied
    constructor Create(aTimeOutMs: integer; aSafe: PSynLocker); reintroduce; overload; virtual;
    /// initialize the semaphore instance
    // - specify a time out millliseconds period after which blocking execution
    // should be handled as failure (if 0 is set, default 3000 would be used)
    // - an associated mutex would be created and owned by this instance
    constructor Create(aTimeOutMs: integer); reintroduce; overload; virtual;
    /// finalize the instance
    destructor Destroy; override;
    /// called to wait for NotifyFinished() to be called, or trigger timeout
    // - returns the final state of the process, i.e. evRaised or evTimeOut
    function WaitFor: TBlockingEvent; reintroduce; overload; virtual;
    /// called to wait for NotifyFinished() to be called, or trigger timeout
    // - returns the final state of the process, i.e. evRaised or evTimeOut
    function WaitFor(TimeOutMS: integer): TBlockingEvent; reintroduce; overload;
    /// should be called by the background process when it is finished
    // - the caller would then let its WaitFor method return
    // - returns TRUE on success (i.e. status was not evRaised or evTimeout)
    // - if the instance is already locked (e.g. when retrieved from
    // TBlockingProcessPool.FromCallLocked), you may set alreadyLocked=TRUE
    function NotifyFinished(alreadyLocked: boolean=false): boolean; virtual;
    /// just a wrapper to reset the internal Event state to evNone
    // - may be used to re-use the same TBlockingProcess instance, after
    // a successfull WaitFor/NotifyFinished process
    // - returns TRUE on success (i.e. status was not evWaiting), setting
    // the current state to evNone, and the Call property to 0
    // - if there is a WaitFor currently in progress, returns FALSE
    function Reset: boolean; virtual;
    /// just a wrapper around fSafe^.Lock
    procedure Lock;
    /// just a wrapper around fSafe^.Unlock
    procedure Unlock;
  published
    /// the current state of process
    // - use Reset method to re-use this instance after a WaitFor process
    property Event: TBlockingEvent read fEvent;
    /// the time out period, in ms, as defined at constructor level
    property TimeOutMs: integer read fTimeOutMS;
  end;
  {$M-}

  /// used to identify each TBlockingProcessPool call
  // - allow to match a given TBlockingProcessPoolItem semaphore
  TBlockingProcessPoolCall = type integer;

  /// a semaphore used in the TBlockingProcessPool
  // - such semaphore have a Call field to identify each execution
  TBlockingProcessPoolItem = class(TBlockingProcess)
  protected
    fCall: TBlockingProcessPoolCall;
    procedure ResetInternal; override;
  published
    /// an unique identifier, when owned by a TBlockingProcessPool
    // - Reset would restore this field to its 0 default value
    property Call: TBlockingProcessPoolCall read fCall;
  end;

  /// class-reference type (metaclass) of a TBlockingProcess
  TBlockingProcessPoolItemClass = class of TBlockingProcessPoolItem;

  /// manage a pool of TBlockingProcessPoolItem instances
  // - each call will be identified via a TBlockingProcessPoolCall unique value
  // - to be used to emulate e.g. blocking execution from an asynchronous
  // event-driven DDD process
  // - it would also allow to re-use TEvent system resources
  TBlockingProcessPool = class(TSynPersistent)
  protected
    fClass: TBlockingProcessPoolItemClass;
    fPool: TSynObjectListLocked;
    fCallCounter: TBlockingProcessPoolCall; // set TBlockingProcessPoolItem.Call
  public
    /// initialize the pool, for a given implementation class
    constructor Create(aClass: TBlockingProcessPoolItemClass=nil); reintroduce;
    /// finalize the pool
    // - would also force all pending WaitFor to trigger a evTimeOut
    destructor Destroy; override;
    /// book a TBlockingProcess from the internal pool
    // - returns nil on error (e.g. the instance is destroying)
    // - or returns the blocking process instance corresponding to this call;
    // its Call property would identify the call for the asynchronous callback,
    // then after WaitFor, the Reset method should be run to release the mutex
    // for the pool
    function NewProcess(aTimeOutMs: integer): TBlockingProcessPoolItem; virtual;
    /// retrieve a TBlockingProcess from its call identifier
    // - may be used e.g. from the callback of the asynchronous process
    // to set some additional parameters to the inherited TBlockingProcess,
    // then call NotifyFinished to release the caller WaitFor
    // - if leavelocked is TRUE, the returned instance would be locked: caller
    // should execute result.Unlock or NotifyFinished(true) after use
    function FromCall(call: TBlockingProcessPoolCall;
      locked: boolean=false): TBlockingProcessPoolItem; virtual;
  end;

/// allow to fix TEvent.WaitFor() method for Kylix
// - under Windows or with FPC, will call original TEvent.WaitFor() method
function FixedWaitFor(Event: TEvent; Timeout: LongWord): TWaitResult;

/// allow to fix TEvent.WaitFor(Event,INFINITE) method for Kylix
// - under Windows or with FPC, will call original TEvent.WaitFor() method
procedure FixedWaitForever(Event: TEvent);

{$endif LVCL} // LVCL does not implement TEvent


{ ************ Operating System types and classes ************************** }

type
  /// store CPU and RAM usage for a given process
  // - as used by TSystemUse class
  TSystemUseData = packed record
    /// when the data has been sampled
    Timestamp: TDateTime;
    /// percent of current Kernel-space CPU usage for this process
    Kernel: single;
    /// percent of current User-space CPU usage for this process
    User: single;
    /// how many KB of working memory are used by this process
    WorkKB: cardinal;
    /// how many KB of virtual memory are used by this process
    VirtualKB: cardinal;
  end;
  /// store CPU and RAM usage history for a given process
  // - as returned by TSystemUse.History
  TSystemUseDataDynArray = array of TSystemUseData;

  /// low-level structure used to compute process memory and CPU usage
  {$ifdef USERECORDWITHMETHODS}TProcessInfo = record
    {$else}TProcessInfo = object {$endif}
  private
    {$ifdef MSWINDOWS}
    fSysPrevIdle, fSysPrevKernel, fSysPrevUser,
    fDiffIdle, fDiffKernel, fDiffUser, fDiffTotal: Int64;
    {$endif}
  public
    /// initialize the system/process resource tracking
    function Init: boolean;
    /// to be called before PerSystem() or PerProcess() iteration
    function Start: boolean;
    /// percent of current Idle/Kernel/User CPU usage for all processes
    function PerSystem(out Idle,Kernel,User: currency): boolean;
    /// retrieve CPU and RAM usage for a given process
    function PerProcess(PID: cardinal; Now: PDateTime; out Data: TSystemUseData;
      var PrevKernel, PrevUser: Int64): boolean;
  end;

  /// event handler which may be executed by TSystemUse.BackgroundExecute
  // - called just after the measurement of each process CPU and RAM consumption
  // - run from the background thread, so should not directly make VCL calls,
  // unless BackgroundExecute is run from a VCL timer
  TOnSystemUseMeasured = procedure(ProcessID: integer; const Data: TSystemUseData) of object;

  /// internal storage of CPU and RAM usage for one process
  TSystemUseProcess = record
    ID: integer;
    Data: TSystemUseDataDynArray;
    PrevKernel: Int64;
    PrevUser: Int64;
  end;
  /// internal storage of CPU and RAM usage for a set of processes
  TSystemUseProcessDynArray = array of TSystemUseProcess;

  /// monitor CPU and RAM usage of one or several processes
  // - you should execute BackgroundExecute on a regular pace (e.g. every second)
  // to gather low-level CPU and RAM information for the given set of processes
  // - is able to keep an history of latest sample values
  // - use Current class function to access a process-wide instance
  TSystemUse = class(TSynPersistentLock)
  protected
    fProcess: TSystemUseProcessDynArray;
    fProcesses: TDynArray;
    fDataIndex: integer;
    fProcessInfo: TProcessInfo;
    fHistoryDepth: integer;
    fOnMeasured: TOnSystemUseMeasured;
    fTimer: TSynBackgroundTimer;
    fUnsubscribeProcessOnAccessError: boolean;
    function ProcessIndex(aProcessID: integer): integer;
  public
    /// a TSynBackgroundThreadProcess compatible event
    // - matches TOnSynBackgroundTimerProcess callback signature
    // - to be supplied e.g. to a TSynBackgroundTimer.Enable method so that it
    // will run every few seconds and retrieve the CPU and RAM use
    procedure BackgroundExecute(Sender: TSynBackgroundTimer;
      Event: TWaitResult; const Msg: RawUTF8);
    /// a VCL's TTimer.OnTimer compatible event
    // - to be run every few seconds and retrieve the CPU and RAM use:
    // ! tmrSystemUse.Interval := 10000; // every 10 seconds
    // ! tmrSystemUse.OnTimer := TSystemUse.Current.OnTimerExecute;
    procedure OnTimerExecute(Sender: TObject);
    /// track the CPU and RAM usage of the supplied set of Process ID
    // - any aProcessID[]=0 will be replaced by the current process ID
    // - you can specify the number of sample values for the History() method
    // - you should then execute the BackgroundExecute method of this instance
    // in a VCL timer or from a TSynBackgroundTimer.Enable() registration
    constructor Create(const aProcessID: array of integer;
      aHistoryDepth: integer=60); reintroduce; overload; virtual;
    /// track the CPU and RAM usage of the current process
    // - you can specify the number of sample values for the History() method
    // - you should then execute the BackgroundExecute method of this instance
    // in a VCL timer or from a TSynBackgroundTimer.Enable() registration
    constructor Create(aHistoryDepth: integer=60); reintroduce; overload; virtual;
    /// add a Process ID to the internal tracking list
    procedure Subscribe(aProcessID: integer);
    /// remove a Process ID from the internal tracking list
    function Unsubscribe(aProcessID: integer): boolean;
    /// returns the total (Kernel+User) CPU usage percent of the supplied process
    // - aProcessID=0 will return information from the current process
    // - returns -1 if the Process ID was not registered via Create/Subscribe
    function Percent(aProcessID: integer=0): single; overload;
    /// returns the Kernel-space CPU usage percent of the supplied process
    // - aProcessID=0 will return information from the current process
    // - returns -1 if the Process ID was not registered via Create/Subscribe
    function PercentKernel(aProcessID: integer=0): single; overload;
    /// returns the User-space CPU usage percent of the supplied process
    // - aProcessID=0 will return information from the current process
    // - returns -1 if the Process ID was not registered via Create/Subscribe
    function PercentUser(aProcessID: integer=0): single; overload;
    /// returns the total (Work+Paged) RAM use of the supplied process, in KB
    // - aProcessID=0 will return information from the current process
    // - returns 0 if the Process ID was not registered via Create/Subscribe
    function KB(aProcessID: integer=0): cardinal; overload;
    /// percent of current Idle/Kernel/User CPU usage for all processes
    function PercentSystem(out Idle,Kernel,User: currency): boolean;
    /// returns the detailed CPU and RAM usage percent of the supplied process
    // - aProcessID=0 will return information from the current process
    // - returns -1 if the Process ID was not registered via Create/Subscribe
    function Data(out aData: TSystemUseData; aProcessID: integer=0): boolean; overload;
    /// returns the detailed CPU and RAM usage percent of the supplied process
    // - aProcessID=0 will return information from the current process
    // - returns Timestamp=0 if the Process ID was not registered via Create/Subscribe
    function Data(aProcessID: integer=0): TSystemUseData; overload;
    /// returns total (Kernel+User) CPU usage percent history of the supplied process
    // - aProcessID=0 will return information from the current process
    // - returns nil if the Process ID was not registered via Create/Subscribe
    // - returns the sample values as an array, starting from the last to the oldest
    // - you can customize the maximum depth, with aDepth < HistoryDepth
    function History(aProcessID: integer=0; aDepth: integer=0): TSingleDynArray; overload;
    /// returns total (Kernel+User) CPU usage percent history of the supplied
    // process, as a string of two digits values
    // - aProcessID=0 will return information from the current process
    // - returns '' if the Process ID was not registered via Create/Subscribe
    // - you can customize the maximum depth, with aDepth < HistoryDepth
    // - the memory history (in MB) can be optionally returned in aDestMemoryMB
    function HistoryText(aProcessID: integer=0; aDepth: integer=0;
      aDestMemoryMB: PRawUTF8=nil): RawUTF8;
    {$ifndef NOVARIANTS}
    /// returns total (Kernel+User) CPU usage percent history of the supplied process
    // - aProcessID=0 will return information from the current process
    // - returns null if the Process ID was not registered via Create/Subscribe
    // - returns the sample values as a TDocVariant array, starting from the
    // last to the oldest, with two digits precision (as currency values)
    // - you can customize the maximum depth, with aDepth < HistoryDepth
    function HistoryVariant(aProcessID: integer=0; aDepth: integer=0): variant;
    {$endif}
    /// access to a global instance, corresponding to the current process
    // - its HistoryDepth will be of 60 items
    class function Current(aCreateIfNone: boolean=true): TSystemUse;
    /// returns detailed CPU and RAM usage history of the supplied process
    // - aProcessID=0 will return information from the current process
    // - returns nil if the Process ID was not registered via Create/Subscribe
    // - returns the sample values as an array, starting from the last to the oldest
    // - you can customize the maximum depth, with aDepth < HistoryDepth
    function HistoryData(aProcessID: integer=0; aDepth: integer=0): TSystemUseDataDynArray; overload;
    /// if any unexisting (e.g. closed/killed) process should be unregistered
    // - e.g. if OpenProcess() API call fails
    property UnsubscribeProcessOnAccessError: boolean
      read fUnsubscribeProcessOnAccessError write fUnsubscribeProcessOnAccessError;
    /// how many items are stored internally, and returned by the History() method
    property HistoryDepth: integer read fHistoryDepth;
    /// executed when TSystemUse.BackgroundExecute finished its measurement
    property OnMeasured: TOnSystemUseMeasured read fOnMeasured write fOnMeasured;
    /// low-level access to the associated timer running BackgroundExecute
    // - equals nil if has been associated to no timer
    property Timer: TSynBackgroundTimer read fTimer write fTimer;
  end;

  /// stores information about a disk partition
  TDiskPartition = packed record
    /// the name of this partition
    // - is the Volume name under Windows, or the Device name under POSIX
    name: RawUTF8;
    /// where this partition has been mounted
    // - e.g. 'C:' or '/home'
    // - you can use GetDiskInfo(mounted) to retrieve current space information
    mounted: TFileName;
    /// total size (in bytes) of this partition
    size: QWord;
  end;
  /// stores information about several disk partitions
  TDiskPartitions = array of TDiskPartition;

  /// value object able to gather information about the current system memory
  TSynMonitorMemory = class(TSynPersistent)
  protected
    FAllocatedUsed: TSynMonitorOneSize;
    FAllocatedReserved: TSynMonitorOneSize;
    FMemoryLoadPercent: integer;
    FPhysicalMemoryFree: TSynMonitorOneSize;
    FVirtualMemoryFree: TSynMonitorOneSize;
    FPagingFileTotal: TSynMonitorOneSize;
    FPhysicalMemoryTotal: TSynMonitorOneSize;
    FVirtualMemoryTotal: TSynMonitorOneSize;
    FPagingFileFree: TSynMonitorOneSize;
    fLastMemoryInfoRetrievedTix: cardinal;
    procedure RetrieveMemoryInfo; virtual;
    function GetAllocatedUsed: TSynMonitorOneSize;
    function GetAllocatedReserved: TSynMonitorOneSize;
    function GetMemoryLoadPercent: integer;
    function GetPagingFileFree: TSynMonitorOneSize;
    function GetPagingFileTotal: TSynMonitorOneSize;
    function GetPhysicalMemoryFree: TSynMonitorOneSize;
    function GetPhysicalMemoryTotal: TSynMonitorOneSize;
    function GetVirtualMemoryFree: TSynMonitorOneSize;
    function GetVirtualMemoryTotal: TSynMonitorOneSize;
  public
    /// initialize the class, and its nested TSynMonitorOneSize instances
    constructor Create(aTextNoSpace: boolean); reintroduce;
    /// finalize the class, and its nested TSynMonitorOneSize instances
    destructor Destroy; override;
    /// some text corresponding to current 'free/total' memory information
    // - returns e.g. '10.3 GB / 15.6 GB'
    class function FreeAsText(nospace: boolean=false): ShortString;
    /// how many physical memory is currently installed, as text (e.g. '32 GB');
    class function PhysicalAsText(nospace: boolean=false): TShort16;
    /// returns a JSON object with the current system memory information
    // - numbers would be given in KB (Bytes shl 10)
    class function ToJSON: RawUTF8;
    {$ifndef NOVARIANTS}
    /// fill a TDocVariant with the current system memory information
    // - numbers would be given in KB (Bytes shl 10)
    class function ToVariant: variant;
    {$endif}
  published
    /// Total of allocated memory used by the program
    property AllocatedUsed: TSynMonitorOneSize read GetAllocatedUsed;
    /// Total of allocated memory reserved by the program
    property AllocatedReserved: TSynMonitorOneSize read GetAllocatedReserved;
    /// Percent of memory in use for the system
    property MemoryLoadPercent: integer read GetMemoryLoadPercent;
    /// Total of physical memory for the system
    property PhysicalMemoryTotal: TSynMonitorOneSize read GetPhysicalMemoryTotal;
    /// Free of physical memory for the system
    property PhysicalMemoryFree: TSynMonitorOneSize read GetPhysicalMemoryFree;
    /// Total of paging file for the system
    property PagingFileTotal: TSynMonitorOneSize read GetPagingFileTotal;
    /// Free of paging file for the system
    property PagingFileFree: TSynMonitorOneSize read GetPagingFileFree;
    {$ifdef MSWINDOWS}
    /// Total of virtual memory for the system
    // - property not defined under Linux, since not applying to this OS
    property VirtualMemoryTotal: TSynMonitorOneSize read GetVirtualMemoryTotal;
    /// Free of virtual memory for the system
    // - property not defined under Linux, since not applying to this OS
    property VirtualMemoryFree: TSynMonitorOneSize read GetVirtualMemoryFree;
    {$endif}
  end;

  /// value object able to gather information about a system drive
  TSynMonitorDisk = class(TSynPersistent)
  protected
    fName: TFileName;
    {$ifdef MSWINDOWS}
    fVolumeName: TFileName;
    {$endif}
    fAvailableSize: TSynMonitorOneSize;
    fFreeSize: TSynMonitorOneSize;
    fTotalSize: TSynMonitorOneSize;
    fLastDiskInfoRetrievedTix: cardinal;
    procedure RetrieveDiskInfo; virtual;
    function GetName: TFileName;
    function GetAvailable: TSynMonitorOneSize;
    function GetFree: TSynMonitorOneSize;
    function GetTotal: TSynMonitorOneSize;
  public
    /// initialize the class, and its nested TSynMonitorOneSize instances
    constructor Create; override;
    /// finalize the class, and its nested TSynMonitorOneSize instances
    destructor Destroy; override;
    /// some text corresponding to current 'free/total' disk information
    // - could return e.g. 'D: 64.4 GB / 213.4 GB'
    class function FreeAsText: RawUTF8;
  published
    /// the disk name
    property Name: TFileName read GetName;
    {$ifdef MSWINDOWS}
    /// the volume name (only available on Windows)
    property VolumeName: TFileName read fVolumeName write fVolumeName;
    /// space currently available on this disk for the current user
    // - may be less then FreeSize, if user quotas are specified (only taken
    // into account under Windows)
    property AvailableSize: TSynMonitorOneSize read GetAvailable;
    {$endif MSWINDOWS}
    /// free space currently available on this disk
    property FreeSize: TSynMonitorOneSize read GetFree;
    /// total space
    property TotalSize: TSynMonitorOneSize read GetTotal;
  end;

  /// hold low-level information about current memory usage
  // - as filled by GetMemoryInfo()
  TMemoryInfo = record
    memtotal, memfree, filetotal, filefree, vmtotal, vmfree,
    allocreserved, allocused: QWord;
    percent: integer;
  end;

type
  {$A-}
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
  TTimeZoneID = type RawUTF8;

  /// used to store Time Zone information for a single area in TSynTimeZone
  // - Delphi "object" is buggy on stack -> also defined as record with methods
  {$ifdef USERECORDWITHMETHODS}TTimeZoneData = record
    {$else}TTimeZoneData = object{$endif}
  public
    id: TTimeZoneID;
    display: RawUTF8;
    tzi: TTimeZoneInfo;
    dyn: array of packed record
      year: integer;
      tzi: TTimeZoneInfo;
    end;
    function GetTziFor(year: integer): PTimeZoneInfo;
  end;
  /// used to store the Time Zone information of a TSynTimeZone class
  TTimeZoneDataDynArray = array of TTimeZoneData;
  {$A+}

  /// handle cross-platform time conversions, following Microsoft time zones
  // - is able to retrieve accurate information from the Windows registry,
  // or from a binary compressed file on other platforms (which should have been
  // saved from a Windows system first)
  // - each time zone will be idendified by its TzId string, as defined by
  // Microsoft for its Windows Operating system
  TSynTimeZone = class
  protected
    fZone: TTimeZoneDataDynArray;
    fZones: TDynArrayHashed;
    fLastZone: TTimeZoneID;
    fLastIndex: integer;
    fIds: TStringList;
    fDisplays: TStringList;
  public
    /// will retrieve the default shared TSynTimeZone instance
    // - locally created via the CreateDefault constructor
    // - this is the usual entry point for time zone process, calling e.g.
    // $ aLocalTime := TSynTimeZone.Default.NowToLocal(aTimeZoneID);
    class function Default: TSynTimeZone;
    /// initialize the internal storage
    // - but no data is available, until Load* methods are called
    constructor Create;
    /// retrieve the time zones from Windows registry, or from a local file
    // - under Linux, the file should be located with the executable, renamed
    // with a .tz extension - may have been created via SaveToFile(''), or
    // from a 'TSynTimeZone' bound resource
    // "dummy" parameter exists only to disambiguate constructors for C++
    constructor CreateDefault(dummy: integer=0);
    /// finalize the instance
    destructor Destroy; override;
    {$ifdef MSWINDOWS}
    /// read time zone information from the Windows registry
    procedure LoadFromRegistry;
    {$endif MSWINDOWS}
    /// read time zone information from a compressed file
    // - if no file name is supplied, a ExecutableName.tz file would be used
    procedure LoadFromFile(const FileName: TFileName='');
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
    procedure LoadFromResource(Instance: THandle=0);
    /// write then time zone information into a compressed file
    // - if no file name is supplied, a ExecutableName.tz file would be created
    procedure SaveToFile(const FileName: TFileName);
    /// write then time zone information into a compressed memory buffer
    function SaveToBuffer: RawByteString;
    /// retrieve the time bias (in minutes) for a given date/time on a TzId
    function GetBiasForDateTime(const Value: TDateTime; const TzId: TTimeZoneID;
      out Bias: integer; out HaveDaylight: boolean; DateIsUTC: boolean=false): boolean;
    /// retrieve the display text corresponding to a TzId
    // - returns '' if the supplied TzId is not recognized
    function GetDisplay(const TzId: TTimeZoneID): RawUTF8;
    /// compute the UTC date/time corrected for a given TzId
    function UtcToLocal(const UtcDateTime: TDateTime; const TzId: TTimeZoneID): TDateTime;
    /// compute the current date/time corrected for a given TzId
    function NowToLocal(const TzId: TTimeZoneID): TDateTime;
    /// compute the UTC date/time for a given local TzId value
    // - by definition, a local time may correspond to two UTC times, during the
    // time biais period, so the returned value is informative only, and any
    // stored value should be following UTC
    function LocalToUtc(const LocalDateTime: TDateTime; const TzID: TTimeZoneID): TDateTime;
    /// direct access to the low-level time zone information
    property Zone: TTimeZoneDataDynArray read fZone;
    /// direct access to the wrapper over the time zone information array
    property Zones: TDynArrayHashed read fZones;
    /// returns a TStringList of all TzID values
    // - could be used to fill any VCL component to select the time zone
    // - order in Ids[] array follows the Zone[].id information
    function Ids: TStrings;
    /// returns a TStringList of all Display text values
    // - could be used to fill any VCL component to select the time zone
    // - order in Displays[] array follows the Zone[].display information
    function Displays: TStrings;
  end;


/// retrieve low-level information about all mounted disk partitions of the system
// - returned partitions array is sorted by "mounted" ascending order
function GetDiskPartitions: TDiskPartitions;

/// retrieve low-level information about all mounted disk partitions as text
// - returns e.g. under Linux
// '/ /dev/sda3 (19 GB), /boot /dev/sda2 (486.8 MB), /home /dev/sda4 (0.9 TB)'
// or under Windows 'C:\ System (115 GB), D:\ Data (99.3 GB)'
// - uses internally a cache unless nocache is true
// - includes the free space if withfreespace is true - e.g. '(80 GB / 115 GB)'
function GetDiskPartitionsText(nocache: boolean=false;
  withfreespace: boolean=false; nospace: boolean=false): RawUTF8;

/// returns a JSON object containing basic information about the computer
// - including Host, User, CPU, OS, freemem, freedisk...
function SystemInfoJson: RawUTF8;

{$ifdef MSWINDOWS}

/// a wrapper around EnumProcesses() PsAPI call
function EnumAllProcesses(out Count: Cardinal): TCardinalDynArray;

/// a wrapper around QueryFullProcessImageNameW/GetModuleFileNameEx PsAPI call
function EnumProcessName(PID: Cardinal): RawUTF8;

{$endif MSWINDOWS}


/// retrieve low-level information about current memory usage
// - as used by TSynMonitorMemory
// - under BSD, only memtotal/memfree/percent are properly returned
// - allocreserved and allocused are set only if withalloc is TRUE
function GetMemoryInfo(out info: TMemoryInfo; withalloc: boolean): boolean;

/// retrieve low-level information about a given disk partition
// - as used by TSynMonitorDisk and GetDiskPartitionsText()
// - only under Windows the Quotas are applied separately to aAvailableBytes
// in respect to global aFreeBytes
function GetDiskInfo(var aDriveFolderOrFile: TFileName;
  out aAvailableBytes, aFreeBytes, aTotalBytes: QWord
  {$ifdef MSWINDOWS}; aVolumeName: PFileName = nil{$endif}): boolean;


{ ************ Markup (e.g. HTML or Emoji) process ******************** }

type
  /// tune AddHtmlEscapeWiki/AddHtmlEscapeMarkdown wrapper functions process
  // - heHtmlEscape will escape any HTML special chars, e.g. & into &amp;
  // - heEmojiToUTF8 will convert any Emoji text into UTF-8 Unicode character,
  // recognizing e.g. :joy: or :) in the text
  TTextWriterHTMLEscape = set of (
    heHtmlEscape, heEmojiToUTF8);

/// convert some wiki-like text into proper HTML
// - convert all #13#10 into <p>...</p>, *..* into <em>..</em>, +..+ into
// <strong>..</strong>, `..` into <code>..</code>, and http://... as
// <a href=http://...>
// - escape any HTML special chars, and Emoji tags as specified with esc
procedure AddHtmlEscapeWiki(W: TTextWriter; P: PUTF8Char;
  esc: TTextWriterHTMLEscape=[heHtmlEscape,heEmojiToUTF8]);

/// convert minimal Markdown text into proper HTML
// - see https://enterprise.github.com/downloads/en/markdown-cheatsheet.pdf
// - convert all #13#10 into <p>...</p>, *..* into <em>..</em>, **..** into
// <strong>..</strong>, `...` into <code>...</code>, backslash espaces \\
// \* \_ and so on, [title](http://...) and detect plain http:// as
// <a href=...>
// - create unordered lists from trailing * + - chars, blockquotes from
// trailing > char, and code line from 4 initial spaces
// - as with default Markdown, won't escape HTML special chars (i.e. you can
// write plain HTML in the supplied text) unless esc is set otherwise
// - only inline-style links and images are supported yet (not reference-style);
// tables aren't supported either
procedure AddHtmlEscapeMarkdown(W: TTextWriter; P: PUTF8Char;
  esc: TTextWriterHTMLEscape=[heEmojiToUTF8]);

/// escape some wiki-marked text into HTML
// - just a wrapper around AddHtmlEscapeWiki() process
function HtmlEscapeWiki(const wiki: RawUTF8; esc: TTextWriterHTMLEscape=[heHtmlEscape,heEmojiToUTF8]): RawUTF8;

/// escape some Markdown-marked text into HTML
// - just a wrapper around AddHtmlEscapeMarkdown() process
function HtmlEscapeMarkdown(const md: RawUTF8; esc: TTextWriterHTMLEscape=[heEmojiToUTF8]): RawUTF8;


type
  /// map the first Unicode page of Emojis, from U+1F600 to U+1F64F
  // - naming comes from github/Markdown :identifiers:
  TEmoji = (eNone,
     eGrinning, eGrin, eJoy, eSmiley, eSmile, eSweat_smile,
     eLaughing, eInnocent, eSmiling_imp, eWink, eBlush, eYum, eRelieved,
     eHeart_eyes, eSunglasses, eSmirk, eNeutral_face, eExpressionless,
     eUnamused, eSweat, ePensive,eConfused, eConfounded, eKissing,
     eKissing_heart, eKissing_smiling_eyes, eKissing_closed_eyes,
     eStuck_out_tongue, eStuck_out_tongue_winking_eye,
     eStuck_out_tongue_closed_eyes, eDisappointed, eWorried, eAngry,
     ePout, eCry, ePersevere, eTriumph, eDisappointed_relieved, eFrowning,
     eAnguished, eFearful, eWeary, eSleepy, eTired_face, eGrimacing, eSob,
     eOpen_mouth, eHushed, eCold_sweat, eScream, eAstonished, eFlushed,
     eSleeping, eDizzy_face, eNo_mouth, eMask, eSmile_cat, eJoy_cat, eSmiley_cat,
     eHeart_eyes_cat, eSmirk_cat, eKissing_cat, ePouting_cat, eCrying_cat_face,
     eScream_cat, eSlightly_frowning_face, eSlightly_smiling_face,
     eUpside_down_face, eRoll_eyes, eNo_good, oOk_woman, eBow, eSee_no_evil,
     eHear_no_evil, eSpeak_no_evil, eRaising_hand, eRaised_hands,
     eFrowning_woman, ePerson_with_pouting_face, ePray);

var
  /// github/Markdown compatible text of Emojis
  // - e.g. 'grinning' or 'person_with_pouting_face'
  EMOJI_TEXT: array[TEmoji] of RawUTF8;
  /// github/Markdown compatible tag of Emojis, including trailing and ending :
  // - e.g. ':grinning:' or ':person_with_pouting_face:'
  EMOJI_TAG: array[TEmoji] of RawUTF8;
  /// the Unicode character matching a given Emoji, after UTF-8 encoding
  EMOJI_UTF8: array[TEmoji] of RawUTF8;
  /// low-level access to TEmoji RTTI - used when inlining EmojiFromText()
  EMOJI_RTTI: PShortString;
  /// to recognize simple :) :( :| :/ :D :o :p :s characters as smilleys
  EMOJI_AFTERDOTS: array['('..'|'] of TEmoji;

/// recognize github/Markdown compatible text of Emojis
// - for instance 'sunglasses' text buffer will return eSunglasses
// - returns eNone if no case-insensitive match was found
function EmojiFromText(P: PUTF8Char; len: PtrInt): TEmoji;
  {$ifdef HASINLINE}inline;{$endif}

/// low-level parser of github/Markdown compatible text of Emojis
// - supplied P^ should point to ':'
// - will append the recognized UTF-8 Emoji if P contains e.g. :joy: or :)
// - will append ':' if no Emoji text is recognized, and return eNone
// - will try both EMOJI_AFTERDOTS[] and EMOJI_RTTI[] reference set
// - if W is nil, won't append anything, but just return the recognized TEmoji
function EmojiParseDots(var P: PUTF8Char; W: TTextWriter=nil): TEmoji;

/// low-level conversion of UTF-8 Emoji sequences into github/Markdown :identifiers:
procedure EmojiToDots(P: PUTF8Char; W: TTextWriter); overload;

/// conversion of UTF-8 Emoji sequences into github/Markdown :identifiers:
function EmojiToDots(const text: RawUTF8): RawUTF8; overload;

/// low-level conversion of github/Markdown :identifiers: into UTF-8 Emoji sequences
procedure EmojiFromDots(P: PUTF8Char; W: TTextWriter); overload;

/// conversion of github/Markdown :identifiers: into UTF-8 Emoji sequences
function EmojiFromDots(const text: RawUTF8): RawUTF8; overload;


{ ************ Command Line and Console process ************************** }

type
  /// available console colors (under Windows at least)
  TConsoleColor = (
    ccBlack, ccBlue, ccGreen, ccCyan, ccRed, ccMagenta, ccBrown, ccLightGray,
    ccDarkGray, ccLightBlue, ccLightGreen, ccLightCyan, ccLightRed, ccLightMagenta,
    ccYellow, ccWhite);

{$ifdef FPC}{$ifdef Linux}
var
  stdoutIsTTY: boolean;
{$endif}{$endif}

/// change the console text writing color
// - you should call this procedure to initialize StdOut global variable, if
// you manually initialized the Windows console, e.g. via the following code:
// ! AllocConsole;
// ! TextColor(ccLightGray); // initialize internal console context
procedure TextColor(Color: TConsoleColor);

/// write some text to the console using a given color
procedure ConsoleWrite(const Text: RawUTF8; Color: TConsoleColor=ccLightGray;
  NoLineFeed: boolean=false; NoColor: boolean=false); overload;

/// write some text to the console using a given color
procedure ConsoleWrite(const Fmt: RawUTF8; const Args: array of const;
  Color: TConsoleColor=ccLightGray; NoLineFeed: boolean=false); overload;

/// change the console text background color
procedure TextBackground(Color: TConsoleColor);

/// will wait for the ENTER key to be pressed, processing Synchronize() pending
// notifications, and the internal Windows Message loop (on this OS)
// - to be used e.g. for proper work of console applications with interface-based
// service implemented as optExecInMainThread
procedure ConsoleWaitForEnterKey;

/// read all available content from stdin
// - could be used to retrieve some file piped to the command line
// - the content is not converted, so will follow the encoding used for storage
function ConsoleReadBody: RawByteString;

{$ifdef MSWINDOWS}
/// low-level access to the keyboard state of a given key
function ConsoleKeyPressed(ExpectedKey: Word): Boolean;
{$endif}

/// direct conversion of a UTF-8 encoded string into a console OEM-encoded String
// - under Windows, will use the CP_OEMCP encoding
// - under Linux, will expect the console to be defined with UTF-8 encoding
function Utf8ToConsole(const S: RawUTF8): RawByteString;
  {$ifndef MSWINDOWS}{$ifdef HASINLINE}inline;{$endif}{$endif}

/// direct conversion of a VCL string into a console OEM-encoded String
// - under Windows, will use the CP_OEMCP encoding
// - under Linux, will expect the console to be defined with UTF-8 encoding
function StringToConsole(const S: string): RawByteString;
  {$ifndef MSWINDOWS}{$ifdef HASINLINE}inline;{$endif}{$endif}

/// could be used in the main program block of a console application to
// handle unexpected fatal exceptions
// - typical use may be:
// !begin
// !  try
// !    ... // main console process
// !  except
// !    on E: Exception do
// !      ConsoleShowFatalException(E);
// !  end;
// !end.
procedure ConsoleShowFatalException(E: Exception; WaitForEnterKey: boolean=true);

var
  /// low-level handle used for console writing
  // - may be overriden when console is redirected
  // - is initialized when TextColor() is called
  StdOut: THandle;


{$ifndef NOVARIANTS}
type
  /// an interface to process the command line switches over a console
  // - as implemented e.g. by TCommandLine class
  // - can implement any process, optionally with console interactivity
  ICommandLine = interface
    ['{77AB427C-1025-488B-8E04-3E62C8100E62}']
    /// returns a command line switch value as UTF-8 text
    // - you can specify a prompt text, when asking for any missing switch
    function AsUTF8(const Switch, Default: RawUTF8; const Prompt: string): RawUTF8;
    /// returns a command line switch value as VCL string text
    // - you can specify a prompt text, when asking for any missing switch
    function AsString(const Switch: RawUTF8; const Default, Prompt: string): string;
    /// returns a command line switch value as integer
    // - you can specify a prompt text, when asking for any missing switch
    function AsInt(const Switch: RawUTF8; Default: Int64; const Prompt: string): Int64;
    /// returns a command line switch ISO-8601 value as date value
    // - here dates are expected to be encoded with ISO-8601, i.e. YYYY-MM-DD
    // - you can specify a prompt text, when asking for any missing switch
    function AsDate(const Switch: RawUTF8; Default: TDateTime; const Prompt: string): TDateTime;
    /// returns a command line switch value as enumeration ordinal
    // - RTTI will be used to check for the enumeration text, or plain integer
    // value will be returned as ordinal value
    // - you can specify a prompt text, when asking for any missing switch
    function AsEnum(const Switch, Default: RawUTF8; TypeInfo: pointer;
      const Prompt: string): integer;
    /// returns all command line values as an array of UTF-8 text
    // - i.e. won't interpret the various switches in the input parameters
    // - as created e.g. by TCommandLine.CreateAsArray constructor
    function AsArray: TRawUTF8DynArray;
    /// serialize all recognized switches as UTF-8 JSON text
    function AsJSON(Format: TTextWriterJSONFormat): RawUTF8;
    /// equals TRUE if the -noprompt switch has been supplied
    // - may be used to force pure execution without console interaction,
    // e.g. when run from another process
    function NoPrompt: boolean;
    /// change the console text color
    // - do nothing if NoPrompt is TRUE
    procedure TextColor(Color: TConsoleColor);
    /// write some console text, with an optional color
    // - will output the text even if NoPrompt is TRUE
    procedure Text(const Fmt: RawUTF8; const Args: array of const;
      Color: TConsoleColor=ccLightGray);
  end;

  /// a class to process the command line switches, with console interactivity
  // - is able to redirect all Text() output to an internal UTF-8 storage,
  // in addition or instead of the console (to be used e.g. from a GUI)
  // - implements ICommandLine interface
  TCommandLine = class(TInterfacedObjectWithCustomCreate, ICommandLine)
  private
    fValues: TDocVariantData;
    fNoPrompt: boolean;
    fNoConsole: boolean;
    fLines: TRawUTF8DynArray;
    procedure SetNoConsole(value: boolean);
  public
    /// initialize the internal storage from the command line
    // - will parse "-switch1 value1 -switch2 value2" layout
    // - stand-alone "-switch1 -switch2 value2" will a create switch1=true value
    constructor Create; overload; override;
    /// initialize the internal storage from the command line
    // - will set paramstr(firstParam)..paramstr(paramcount) in fValues as array
    // - may be used e.g. for "val1 val2 val3" command line layout
    constructor CreateAsArray(firstParam: integer);
    /// initialize the internal storage with some ready-to-use switches
    // - will also set the NoPrompt option, and set the supplied NoConsole value
    // - may be used e.g. from a graphical interface instead of console mode
    constructor Create(const switches: variant;
      aNoConsole: boolean=true); reintroduce; overload;
    /// initialize the internal storage with some ready-to-use name/value pairs
    // - will also set the NoPrompt option, and set the supplied NoConsole value
    // - may be used e.g. from a graphical interface instead of console mode
    constructor Create(const NameValuePairs: array of const;
      aNoConsole: boolean=true); reintroduce; overload;
    /// returns a command line switch value as UTF-8 text
    // - you can specify a prompt text, when asking for any missing switch
    function AsUTF8(const Switch, Default: RawUTF8; const Prompt: string): RawUTF8;
    /// returns a command line switch value as VCL string text
    // - you can specify a prompt text, when asking for any missing switch
    function AsString(const Switch: RawUTF8; const Default, Prompt: string): string;
    /// returns a command line switch value as integer
    // - you can specify a prompt text, when asking for any missing switch
    function AsInt(const Switch: RawUTF8; Default: Int64; const Prompt: string): Int64;
    /// returns a command line switch ISO-8601 value as date value
    // - here dates are expected to be encoded with ISO-8601, i.e. YYYY-MM-DD
    // - you can specify a prompt text, when asking for any missing switch
    function AsDate(const Switch: RawUTF8; Default: TDateTime; const Prompt: string): TDateTime;
    /// returns a command line switch value as enumeration ordinal
    // - RTTI will be used to check for the enumeration text, or plain integer
    // value will be returned as ordinal value
    // - you can specify a prompt text, when asking for any missing switch
    function AsEnum(const Switch, Default: RawUTF8; TypeInfo: pointer;
      const Prompt: string): integer;
    /// returns all command line values as an array of UTF-8 text
    // - i.e. won't interpret the various switches in the input parameters
    // - as created e.g. by TCommandLine.CreateAsArray constructor
    function AsArray: TRawUTF8DynArray;
    /// serialize all recognized switches as UTF-8 JSON text
    function AsJSON(Format: TTextWriterJSONFormat): RawUTF8;
    /// equals TRUE if the -noprompt switch has been supplied
    // - may be used to force pure execution without console interaction,
    // e.g. when run from another process
    function NoPrompt: boolean;
    /// change the console text color
    // - do nothing if NoPrompt is TRUE
    procedure TextColor(Color: TConsoleColor);
    /// write some console text, with an optional color
    // - will output the text even if NoPrompt=TRUE, but not if NoConsole=TRUE
    // - will append the text to the internal storage, available from ConsoleText
    procedure Text(const Fmt: RawUTF8; const Args: array of const;
      Color: TConsoleColor=ccLightGray);
    /// low-level access to the internal switches storage
    property Values: TDocVariantData read fValues;
    /// if Text() should be redirected to ConsoleText internal storage
    // - and don't write anything to the console
    // - should be associated with NoProperty = TRUE property
    property NoConsole: boolean read fNoConsole write SetNoConsole;
    /// low-level access to the internal UTF-8 console lines storage
    property ConsoleLines: TRawUTF8DynArray read fLines;
    /// returns the UTF-8 text as inserted by Text() calls
    // - line feeds will be included to the ConsoleLines[] values
    function ConsoleText(const LineFeed: RawUTF8=sLineBreak): RawUTF8;
  end;
{$endif NOVARIANTS}

{ ************ TSynTable types and classes ************************** }

{$define SORTCOMPAREMETHOD}
{ if defined, the field content comparison will use a method instead of fixed
  functions - could be mandatory for tftArray field kind }

type
  /// exception raised by all TSynTable related code
  ETableDataException = class(ESynException);

  /// the available types for any TSynTable field property
  // - this is used in our so-called SBF compact binary format
  // (similar to BSON or Protocol Buffers)
  // - those types are used for both storage and JSON conversion
  // - basic types are similar to SQLite3, i.e. Int64/Double/UTF-8/Blob
  // - storage can be of fixed size, or of variable length
  // - you can specify to use WinAnsi encoding instead of UTF-8 for string storage
  // (it can use less space on disk than UTF-8 encoding)
  // - BLOB fields can be either internal (i.e. handled by TSynTable like a
  // RawByteString text storage), either external (i.e. must be stored in a dedicated
  // storage structure - e.g. another TSynBigTable instance)
  TSynTableFieldType =
    (// unknown or not defined field type
     tftUnknown,
     // some fixed-size field value
     tftBoolean, tftUInt8, tftUInt16, tftUInt24, tftInt32, tftInt64,
     tftCurrency, tftDouble,
     // some variable-size field value
     tftVarUInt32, tftVarInt32, tftVarUInt64,
     // text storage
     tftWinAnsi, tftUTF8,
     // BLOB fields
     tftBlobInternal, tftBlobExternal,
     // other variable-size field value
     tftVarInt64);

  /// set of available field types for TSynTable
  TSynTableFieldTypes = set of TSynTableFieldType;

  /// available option types for a field property
  // - tfoIndex is set if an index must be created for this field
  // - tfoUnique is set if field values must be unique (if set, the tfoIndex
  // will be always forced)
  // - tfoCaseInsensitive can be set to make no difference between 'a' and 'A'
  // (by default, comparison is case-sensitive) - this option has an effect
  // not only if tfoIndex or tfoUnique is set, but also for iterating search
  TSynTableFieldOption = (
    tfoIndex, tfoUnique, tfoCaseInsensitive);

  /// set of option types for a field
  TSynTableFieldOptions = set of TSynTableFieldOption;

  /// used to store bit set for all available fiels in a Table
  // - with current format, maximum field count is 64
  TSynTableFieldBits = set of 0..63;

  /// an custom RawByteString type used to store internaly a data in
  // our SBF compact binary format
  TSBFString = type RawByteString;

  /// function prototype used to retrieve the index of a specified property name
  // - 'ID' is handled separately: here must be available only the custom fields
  TSynTableFieldIndex = function(const PropName: RawUTF8): integer of object;

  /// the recognized operators for a TSynTableStatement where clause
  TSynTableStatementOperator = (
     opEqualTo,
     opNotEqualTo,
     opLessThan,
     opLessThanOrEqualTo,
     opGreaterThan,
     opGreaterThanOrEqualTo,
     opIn,
     opIsNull,
     opIsNotNull,
     opLike,
     opContains,
     opFunction);

  TSynTableFieldProperties = class;

  /// one recognized SELECT expression for TSynTableStatement
  TSynTableStatementSelect = record
    /// the column SELECTed for the SQL statement, in the expected order
    // - contains 0 for ID/RowID, or the RTTI field index + 1
    Field: integer;
    /// an optional integer to be added
    // - recognized from .. +123 .. -123 patterns in the select
    ToBeAdded: integer;
    /// the optional column alias, e.g. 'MaxID' for 'max(id) as MaxID'
    Alias: RawUTF8;
    /// the optional function applied to the SELECTed column
    // - e.g. Max(RowID) would store 'Max' and SelectField[0]=0
    // - but Count( * ) would store 'Count' and SelectField[0]=0, and
    // set FunctionIsCountStart = TRUE
    FunctionName: RawUTF8;
    /// if the function needs a special process
    // - e.g. funcCountStar for the special Count( * ) expression or
    // funcDistinct, funcMax for distinct(...)/max(...) aggregation
    FunctionKnown: (funcNone, funcCountStar, funcDistinct, funcMax);
    /// MongoDB-like sub field e.g. 'mainfield.subfield1.subfield2'
    // - still identifying 'mainfield' in Field index, and setting
    // SubField='.subfield1.subfield2'
    SubField: RawUTF8;
  end;

  /// the recognized SELECT expressions for TSynTableStatement
  TSynTableStatementSelectDynArray = array of TSynTableStatementSelect;

  /// one recognized WHERE expression for TSynTableStatement
  TSynTableStatementWhere = record
    /// any '(' before the actual expression
    ParenthesisBefore: RawUTF8;
    /// any ')' after the actual expression
    ParenthesisAfter: RawUTF8;
    /// expressions are evaluated as AND unless this field is set to TRUE
    JoinedOR: boolean;
    /// if this expression is preceded by a NOT modifier
    NotClause: boolean;
    /// the index of the field used for the WHERE expression
    // - WhereField=0 for ID, 1 for field # 0, 2 for field #1,
    // and so on... (i.e. WhereField = RTTI field index +1)
    Field: integer;
    /// MongoDB-like sub field e.g. 'mainfield.subfield1.subfield2'
    // - still identifying 'mainfield' in Field index, and setting
    // SubField='.subfield1.subfield2'
    SubField: RawUTF8;
    /// the operator of the WHERE expression
    Operator: TSynTableStatementOperator;
    /// the SQL function name associated to a Field and Value
    // - e.g. 'INTEGERDYNARRAYCONTAINS' and Field=0 for
    // IntegerDynArrayContains(RowID,10) and ValueInteger=10
    // - Value does not contain anything
    FunctionName: RawUTF8;
    /// the value used for the WHERE expression
    Value: RawUTF8;
    /// the raw value SQL buffer used for the WHERE expression
    ValueSQL: PUTF8Char;
    /// the raw value SQL buffer length used for the WHERE expression
    ValueSQLLen: integer;
    /// an integer representation of WhereValue (used for ID check e.g.)
    ValueInteger: integer;
    /// used to fast compare with SBF binary compact formatted data
    ValueSBF: TSBFString;
    {$ifndef NOVARIANTS}
    /// the value used for the WHERE expression, encoded as Variant
    // - may be a TDocVariant for the IN operator
    ValueVariant: variant;
    {$endif}
  end;

  /// the recognized WHERE expressions for TSynTableStatement
  TSynTableStatementWhereDynArray = array of TSynTableStatementWhere;

  /// used to parse a SELECT SQL statement, following the SQlite3 syntax
  // - handle basic REST commands, i.e. a SELECT over a single table (no JOIN)
  // with its WHERE clause, and result column aliases
  // - handle also aggregate functions like "SELECT Count( * ) FROM TableName"
  // - will also parse any LIMIT, OFFSET, ORDER BY, GROUP BY statement clause
  TSynTableStatement = class
  protected
    fSQLStatement: RawUTF8;
    fSelect: TSynTableStatementSelectDynArray;
    fSelectFunctionCount: integer;
    fTableName: RawUTF8;
    fWhere: TSynTableStatementWhereDynArray;
    fOrderByField: TSQLFieldIndexDynArray;
    fGroupByField: TSQLFieldIndexDynArray;
    fWhereHasParenthesis, fHasSelectSubFields, fWhereHasSubFields: boolean;
    fOrderByDesc: boolean;
    fLimit: integer;
    fOffset: integer;
    fWriter: TJSONWriter;
  public
    /// parse the given SELECT SQL statement and retrieve the corresponding
    // parameters into this class read-only properties
    // - the supplied GetFieldIndex() method is used to populate the
    // SelectedFields and Where[].Field properties
    // - SimpleFieldsBits is used for '*' field names
    // - SQLStatement is left '' if the SQL statement is not correct
    // - if SQLStatement is set, the caller must check for TableName to match
    // the expected value, then use the Where[] to retrieve the content
    // - if FieldProp is set, then the Where[].ValueSBF property is initialized
    // with the SBF equivalence of the Where[].Value
    constructor Create(const SQL: RawUTF8; GetFieldIndex: TSynTableFieldIndex;
      SimpleFieldsBits: TSQLFieldBits=[0..MAX_SQLFIELDS-1];
      FieldProp: TSynTableFieldProperties=nil);
    /// compute the SELECT column bits from the SelectFields array
    // - optionally set Select[].SubField into SubFields[Select[].Field]
    // (e.g. to include specific fields from MongoDB embedded document)
    procedure SelectFieldBits(var Fields: TSQLFieldBits; var withID: boolean;
      SubFields: PRawUTF8Array=nil);

    /// the SELECT SQL statement parsed
    // - equals '' if the parsing failed
    property SQLStatement: RawUTF8 read fSQLStatement;
    /// the column SELECTed for the SQL statement, in the expected order
    property Select: TSynTableStatementSelectDynArray read fSelect;
    /// if the SELECTed expression of this SQL statement have any function defined
    property SelectFunctionCount: integer read fSelectFunctionCount;
    /// the retrieved table name
    property TableName: RawUTF8 read fTableName;
    /// if any Select[].SubField was actually set
    property HasSelectSubFields: boolean read fHasSelectSubFields;
    /// the WHERE clause of this SQL statement
    property Where: TSynTableStatementWhereDynArray read fWhere;
    /// if the WHERE clause contains any ( ) parenthesis expression
    property WhereHasParenthesis: boolean read fWhereHasParenthesis;
    /// if the WHERE clause contains any Where[].SubField
    property WhereHasSubFields: boolean read fWhereHasSubFields;
    /// recognize an GROUP BY clause with one or several fields
    // - here 0 = ID, otherwise RTTI field index +1
    property GroupByField: TSQLFieldIndexDynArray read fGroupByField;
    /// recognize an ORDER BY clause with one or several fields
    // - here 0 = ID, otherwise RTTI field index +1
    property OrderByField: TSQLFieldIndexDynArray read fOrderByField;
    /// false for default ASC order, true for DESC attribute
    property OrderByDesc: boolean read fOrderByDesc;
    /// the number specified by the optional LIMIT ... clause
    // - set to 0 by default (meaning no LIMIT clause)
    property Limit: integer read fLimit;
    /// the number specified by the optional OFFSET ... clause
    // - set to 0 by default (meaning no OFFSET clause)
    property Offset: integer read fOffset;
    /// optional associated writer
    property Writer: TJSONWriter read fWriter write fWriter;
  end;

  /// function prototype used to retrieve the RECORD data of a specified Index
  // - the index is not the per-ID index, but the "physical" index, i.e. the
  // index value used to retrieve data from low-level (and faster) method
  // - should return nil if Index is out of range
  // - caller must provide a temporary storage buffer to be used optionally
  TSynTableGetRecordData = function(
    Index: integer; var aTempData: RawByteString): pointer of object;

  TSynTable = class;

  {$ifdef SORTCOMPAREMETHOD}
  /// internal value used by TSynTableFieldProperties.SortCompare() method to
  // avoid stack allocation
  TSortCompareTmp = record
    PB1, PB2: PByte;
    L1,L2: integer;
  end;
  {$endif}

  /// store the type properties of a given field / database column
  TSynTableFieldProperties = class
  protected
    /// used during OrderedIndexSort to prevent stack usage
    SortPivot: pointer;
    {$ifdef SORTCOMPAREMETHOD}
    /// internal value used by SortCompare() method to avoid stack allocation
    SortCompareTmp: TSortCompareTmp;
    {$endif}
    /// these two temporary buffers are used to call TSynTableGetRecordData
    DataTemp1, DataTemp2: RawByteString;
    /// the associated table which own this field property
    Owner: TSynTable;
    /// the global size of a default field value, as encoded
    // in our SBF compact binary format
    fDefaultFieldLength: integer;
    /// a default field data, as encoded in our SBF compact binary format
    fDefaultFieldData: TSBFString;
    /// last >=0 value returned by the last OrderedIndexFindAdd() call
    fOrderedIndexFindAdd: integer;
    /// used for internal QuickSort of OrderedIndex[]
    // - call SortCompare() for sorting the items
    procedure OrderedIndexSort(L,R: PtrInt);
    /// retrieve an index from OrderedIndex[] of the given value
    // - call SortCompare() to compare to the reference value
    function OrderedIndexFind(Value: pointer): PtrInt;
    /// retrieve an index where a Value must be added into OrderedIndex[]
    // - call SortCompare() to compare to the reference value
    // - returns -1 if Value is there, or the index where to insert
    // - the returned value (if >= 0) will be stored in fOrderedIndexFindAdd
    function OrderedIndexFindAdd(Value: pointer): PtrInt;
    /// set OrderedIndexReverse[OrderedIndex[aOrderedIndex]] := aOrderedIndex;
    procedure OrderedIndexReverseSet(aOrderedIndex: integer);
  public
    /// the field name
    Name: RawUTF8;
    /// kind of field (defines both value type and storage to be used)
    FieldType: TSynTableFieldType;
    /// the fixed-length size, or -1 for a varInt, -2 for a variable string
    FieldSize: integer;
    /// options of this field
    Options: TSynTableFieldOptions;
    /// contains the offset of this field, in case of fixed-length field
    // - normally, fixed-length fields are stored in the beginning of the record
    // storage: in this case, a value >= 0 will point to the position of the
    // field value of this field
    // - if the value is < 0, its absolute will be the field number to be counted
    // after TSynTable.fFieldVariableOffset (-1 for first item)
    Offset: integer;
    /// number of the field in the table (starting at 0)
    FieldNumber: integer;
    /// if allocated, contains the storage indexes of every item, in sorted order
    // - only available if tfoIndex is in Options
    // - the index is not the per-ID index, but the "physical" index, i.e. the
    // index value used to retrieve data from low-level (and faster) method
    OrderedIndex: TIntegerDynArray;
    /// if allocated, contains the reverse storage index of OrderedIndex
    // - i.e. OrderedIndexReverse[OrderedIndex[i]] := i;
    // - used to speed up the record update procedure with huge number of
    // records
    OrderedIndexReverse: TIntegerDynArray;
    /// number of items in OrderedIndex[]
    // - is set to 0 when the content has been modified (mark force recreate)
    OrderedIndexCount: integer;
    /// if set to TRUE after an OrderedIndex[] refresh but with not sorting
    // - OrderedIndexSort(0,OrderedIndexCount-1) must be called before using
    // the OrderedIndex[] array
    // - you should call OrderedIndexRefresh method to ensure it is sorted
    OrderedIndexNotSorted: boolean;
    /// all TSynValidate instances registered per each field
    Filters: TSynObjectList;
    /// all TSynValidate instances registered per each field
    Validates: TSynObjectList;
    /// low-level binary comparison used by IDSort and TSynTable.IterateJSONValues
    // - P1 and P2 must point to the values encoded in our SBF compact binary format
    {$ifdef SORTCOMPAREMETHOD}
    function SortCompare(P1,P2: PUTF8Char): PtrInt;
    {$else}
    SortCompare: TUTF8Compare;
    {$endif}

    /// read entry from a specified file reader
    constructor CreateFrom(var RD: TFileBufferReader);
    /// release associated memory and objects
    destructor Destroy; override;
    /// save entry to a specified file writer
    procedure SaveTo(WR: TFileBufferWriter);
    /// decode the value from our SBF compact binary format into UTF-8 JSON
    // - returns the next FieldBuffer value
    function GetJSON(FieldBuffer: pointer; W: TTextWriter): pointer;
    /// decode the value from our SBF compact binary format into UTF-8 text
    // - this method does not check for FieldBuffer to be not nil -> caller
    // should check this explicitely
    function GetValue(FieldBuffer: pointer): RawUTF8;
    /// decode the value from a record buffer into an Boolean
    // - will call Owner.GetData to retrieve then decode the field SBF content
    function GetBoolean(RecordBuffer: pointer): Boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// decode the value from a record buffer into an integer
    // - will call Owner.GetData to retrieve then decode the field SBF content
    function GetInteger(RecordBuffer: pointer): Integer;
    /// decode the value from a record buffer into an Int64
    // - will call Owner.GetData to retrieve then decode the field SBF content
    function GetInt64(RecordBuffer: pointer): Int64;
    /// decode the value from a record buffer into an floating-point value
    // - will call Owner.GetData to retrieve then decode the field SBF content
    function GetDouble(RecordBuffer: pointer): Double;
    /// decode the value from a record buffer into an currency value
    // - will call Owner.GetData to retrieve then decode the field SBF content
    function GetCurrency(RecordBuffer: pointer): Currency;
    /// decode the value from a record buffer into a RawUTF8 string
    // - will call Owner.GetData to retrieve then decode the field SBF content
    function GetRawUTF8(RecordBuffer: pointer): RawUTF8;
    {$ifndef NOVARIANTS}
    /// decode the value from our SBF compact binary format into a Variant
    function GetVariant(FieldBuffer: pointer): Variant; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// decode the value from our SBF compact binary format into a Variant
    procedure GetVariant(FieldBuffer: pointer; var result: Variant); overload;
    {$endif}
    /// retrieve the binary length (in bytes) of some SBF compact binary format
    function GetLength(FieldBuffer: pointer): Integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// create some SBF compact binary format from a Delphi binary value
    // - will return '' if the field type doesn't match a boolean
    function SBF(const Value: Boolean): TSBFString; overload;
    /// create some SBF compact binary format from a Delphi binary value
    // - will encode any byte, word, integer, cardinal, Int64 value
    // - will return '' if the field type doesn't match an integer
    function SBF(const Value: Int64): TSBFString; overload;
    /// create some SBF compact binary format from a Delphi binary value
    // - will encode any byte, word, integer, cardinal value
    // - will return '' if the field type doesn't match an integer
    function SBF(const Value: Integer): TSBFString; overload;
    /// create some SBF compact binary format from a Delphi binary value
    // - will return '' if the field type doesn't match a currency
    // - we can't use SBF() method name because of Currency/Double ambiguity
    function SBFCurr(const Value: Currency): TSBFString;
    /// create some SBF compact binary format from a Delphi binary value
    // - will return '' if the field type doesn't match a floating-point
    // - we can't use SBF() method name because of Currency/Double ambiguity
    function SBFFloat(const Value: Double): TSBFString;
    /// create some SBF compact binary format from a Delphi binary value
    // - expect a RawUTF8 string: will be converted to WinAnsiString
    // before storage, for tftWinAnsi
    // - will return '' if the field type doesn't match a string
    function SBF(const Value: RawUTF8): TSBFString; overload;
    /// create some SBF compact binary format from a BLOB memory buffer
    // - will return '' if the field type doesn't match tftBlobInternal
    function SBF(Value: pointer; ValueLen: integer): TSBFString; overload;
    /// convert any UTF-8 encoded value into our SBF compact binary format
    // - can be used e.g. from a WHERE clause, for fast comparison in
    // TSynTableStatement.WhereValue content using OrderedIndex[]
    // - is the reverse of GetValue/GetRawUTF8 methods above
    function SBFFromRawUTF8(const aValue: RawUTF8): TSBFString;
    {$ifndef NOVARIANTS}
    /// create some SBF compact binary format from a Variant value
    function SBF(const Value: Variant): TSBFString; overload;
    {$endif}

    /// will update then sort the array of indexes used for the field index
    // - the OrderedIndex[] array is first refreshed according to the
    // aOldIndex, aNewIndex parameters: aOldIndex=-1 for Add, aNewIndex=-1 for
    // Delete, or both >= 0 for update
    // - call with both indexes = -1 will sort the existing OrderedIndex[] array
    // - GetData property must have been set with a method returning a pointer
    // to the field data for a given index (this index is not the per-ID index,
    // but the "physical" index, i.e. the index value used to retrieve data
    // from low-level (and fast) GetData method)
    // - aOldRecordData and aNewRecordData can be specified in order to guess
    // if the field data has really been modified (speed up the update a lot
    // to only sort indexed fields if its content has been really modified)
    // - returns FALSE if any parameter is invalid
    function OrderedIndexUpdate(aOldIndex, aNewIndex: integer;
      aOldRecordData, aNewRecordData: pointer): boolean;
    /// retrieve one or more "physical" indexes matching a WHERE Statement
    // - is faster than O(1) GetIteraring(), because will use O(log(n)) binary
    // search using the OrderedIndex[] array
    // - returns the resulting indexes as a a sorted list in MatchIndex/MatchIndexCount
    // - if the indexes are already present in the list, won't duplicate them
    // - WhereSBFValue must be a valid SBF formated field buffer content
    // - the Limit parameter is similar to the SQL LIMIT clause: if greater than 0,
    // an upper bound on the number of rows returned is placed (e.g. set Limit=1
    // to only retrieve the first match)
    // - GetData property must have been set with a method returning a pointer
    // to the field data for a given index (this index is not the per-ID index,
    // but the "physical" index, i.e. the index value used to retrieve data
    // from low-level (and fast) GetData method)
    // - in this method, indexes are not the per-ID indexes, but the "physical"
    // indexes, i.e. each index value used to retrieve data from low-level
    // (and fast) GetData method
    function OrderedIndexMatch(WhereSBFValue: pointer;
      var MatchIndex: TIntegerDynArray; var MatchIndexCount: integer;
      Limit: Integer=0): Boolean;
    /// will force refresh the OrderedIndex[] array
    // - to be called e.g. if OrderedIndexNotSorted = TRUE, if you want to
    // access to the OrderedIndex[] array
    procedure OrderedIndexRefresh;
    /// register a custom filter or validation rule to the class for this field
    // - this will be used by Filter() and Validate() methods
    // - will return the specified associated TSynFilterOrValidate instance
    // - a TSynValidateTableUniqueField is always added by
    // TSynTable.AfterFieldModif if tfoUnique is set in Options
    function AddFilterOrValidate(aFilter: TSynFilterOrValidate): TSynFilterOrValidate;
    /// check the registered constraints
    // - returns '' on success
    // - returns an error message e.g. if a tftUnique constraint failed
    // - RecordIndex=-1 in case of adding, or the physical index of the updated record
    function Validate(RecordBuffer: pointer; RecordIndex: integer): string;
    /// some default SBF compact binary format content
    property SBFDefault: TSBFString read fDefaultFieldData;
  end;


{$ifndef DELPHI5OROLDER}

  /// a pointer to structure used to store a TSynTable record
  PSynTableData = ^TSynTableData;

  {$A-} { packet object not allowed since Delphi 2009 :( }
  /// used to store a TSynTable record using our SBF compact binary format
  // - this object can be created on the stack
  // - it is mapped into a variant TVarData, to be retrieved by the
  // TSynTable.Data method - but direct allocation of a TSynTableData on the
  // stack is faster (due to the Variant overhead)
  // - is defined either as an object either as a record, due to a bug
  // in Delphi 2009/2010 compiler (at least): this structure is not initialized
  // if defined as an object on the stack, but will be as a record :(
  {$ifdef USERECORDWITHMETHODS}TSynTableData = record
    {$else}TSynTableData = object {$endif UNICODE}
  private
    VType: cardinal; // defined as cardinal not as word for proper aligment
    VID: integer;
    VTable: TSynTable;
    VValue: TSBFString;
    {$ifndef NOVARIANTS}
    function GetFieldVarData(FieldName: PUTF8Char; FieldNameLen: PtrInt; var Value: TVarData): boolean;
    procedure GetFieldVariant(const FieldName: RawUTF8; var result: Variant);
    function GetField(const FieldName: RawUTF8): Variant;
    procedure SetField(const FieldName: RawUTF8; const Value: Variant);
    {$endif}
    /// raise an exception if VTable=nil
    procedure CheckVTableInitialized;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// initialize a record data content for a specified table
    // - a void content is set
    procedure Init(aTable: TSynTable; aID: Integer=0); overload; {$ifdef HASINLINE}inline;{$endif}
    /// initialize a record data content for a specified table
    // - the specified SBF content is store inside this TSynTableData
    procedure Init(aTable: TSynTable; aID: Integer; RecordBuffer: pointer;
      RecordBufferLen: integer); overload;
    /// the associated record ID
    property ID: integer read VID write VID;
    /// the associated TSynTable instance
    property Table: TSynTable read VTable write VTable;
    /// the record content, SBF compact binary format encoded
    property SBF: TSBFString read VValue;
    {$ifndef NOVARIANTS}
    /// set or retrieve a field value from a variant data
    property Field[const FieldName: RawUTF8]: Variant read GetField write SetField;
    /// get a field value for a specified field
    // - this method is faster than Field[], because it won't look for the field name
    function GetFieldValue(aField: TSynTableFieldProperties): Variant;
    /// set a field value for a specified field
    // - this method is faster than Field[], because it won't look for the field name
    procedure SetFieldValue(aField: TSynTableFieldProperties; const Value: Variant);
      {$ifdef HASINLINE}inline;{$endif}
    {$endif}
    /// set a field value for a specified field, from SBF-encoded data
    // - this method is faster than the other, because it won't look for the field
    // name nor make any variant conversion
    procedure SetFieldSBFValue(aField: TSynTableFieldProperties; const Value: TSBFString);
    /// get a field value for a specified field, into SBF-encoded data
    // - this method is faster than the other, because it won't look for the field
    // name nor make any variant conversion
    function GetFieldSBFValue(aField: TSynTableFieldProperties): TSBFString;
    /// filter the SBF buffer record content with all registered filters
    // - all field values are filtered in-place, following our SBF compact
    // binary format encoding for this record
    procedure FilterSBFValue; {$ifdef HASINLINE}inline;{$endif}
    /// check the registered constraints according to a record SBF buffer
    // - returns '' on success
    // - returns an error message e.g. if a tftUnique constraint failed
    // - RecordIndex=-1 in case of adding, or the physical index of the updated record
    function ValidateSBFValue(RecordIndex: integer): string;
  end;
  {$A+} { packet object not allowed since Delphi 2009 :( }
{$endif DELPHI5OROLDER}

  PUpdateFieldEvent = ^TUpdateFieldEvent;

  /// an opaque structure used for TSynTable.UpdateFieldEvent method
  TUpdateFieldEvent = record
    /// the number of record added
    Count: integer;
    /// the list of IDs added
    // - this list is already in increasing order, because GetIterating was
    // called with the ioID order
    IDs: TIntegerDynArray;
    /// the offset of every record added
    // - follows the IDs[] order
    Offsets64: TInt64DynArray;
    /// previous indexes: NewIndexs[oldIndex] := newIndex
    NewIndexs: TIntegerDynArray;
    /// the list of existing field in the previous data
    AvailableFields: TSQLFieldBits;
    /// where to write the updated data
    WR: TFileBufferWriter;
  end;

  /// will define a validation to be applied to a TSynTableFieldProperties field
  // - a typical usage is to validate a value to be unique in the table
  // (implemented in the TSynValidateTableUniqueField class)
  // - the optional associated parameters are to be supplied JSON-encoded
  // - ProcessField and ProcessRecordIndex properties will be filled before
  // Process method call by TSynTableFieldProperties.Validate()
  TSynValidateTable = class(TSynValidate)
  protected
    fProcessField: TSynTableFieldProperties;
    fProcessRecordIndex: integer;
  public
    /// the associated TSQLRest instance
    // - this value is filled by TSynTableFieldProperties.Validate with its
    // self value to be used for the validation
    // - it can be used in the overridden Process method
    property ProcessField: TSynTableFieldProperties read fProcessField write fProcessField;
    /// the associated record index (in case of update)
    // - is set to -1 in case of adding, or the physical index of the updated record
    // - this value is filled by TSynTableFieldProperties.Validate
    // - it can be used in the overridden Process method
    property ProcessRecordIndex: integer read fProcessRecordIndex write fProcessRecordIndex;
  end;

  /// will define a validation for a TSynTableFieldProperties Unique field
  // - implement constraints check e.g. if tfoUnique is set in Options
  // - it will check that the field value is not void
  // - it will check that the field value is not a duplicate
  TSynValidateTableUniqueField = class(TSynValidateTable)
  public
    /// perform the unique field validation action to the specified value
    // - duplication value check will use the ProcessField  and
    // ProcessRecordIndex properties, which will be filled before call by
    // TSynTableFieldProperties.Validate()
    // - aFieldIndex parameter is not used here, since we have already the
    // ProcessField property set
    // - here the Value is expected to be UTF-8 text, as converted from our SBF
    // compact binary format via e.g. TSynTableFieldProperties.GetValue /
    // GetRawUTF8: this is mandatory to have the validation rule fit with other
    // TSynValidateTable classes
    function Process(aFieldIndex: integer; const Value: RawUTF8; var ErrorMsg: string): boolean; override;
  end;

  /// store the description of a table with records, to implement a Database
  // - can be used with several storage engines, for instance TSynBigTableRecord
  // - each record can have up to 64 fields
  // - a mandatory ID field must be handled by the storage engine itself
  // - will handle the storage of records into our SBF compact binary format, in
  // which fixed-length fields are stored leftmost side, then variable-length
  // fields follow
  TSynTable = class
  protected
    fTableName: RawUTF8;
    /// list of TSynTableFieldProperties instances
    fField: TObjectList;
    /// offset of the first variable length value field
    fFieldVariableOffset: PtrUInt;
    /// index of the first variable length value field
    // - equals -1 if no variable length field exists
    fFieldVariableIndex: integer;
    /// bit is set for a tftWinAnsi, tftUTF8 or tftBlobInternal kind of field
    // - these kind of field are encoded as a VarInt length, then the data
    fFieldIsVarString: TSynTableFieldBits;
    /// bit is set for a tftBlobExternal kind of field e.g.
    fFieldIsExternal: TSynTableFieldBits;
    /// event used for proper data retrieval of a given record buffer
    fGetRecordData: TSynTableGetRecordData;
    /// the global size of a default value, as encoded in our SBF compact binary format
    fDefaultRecordLength: integer;
    /// a default record data, as encoded in our SBF compact binary format
    fDefaultRecordData: TSBFString;
    /// list of TSynTableFieldProperties added via all AddField() call
    fAddedField: TList;
    /// true if any field has a tfoUnique option set
    fFieldHasUniqueIndexes: boolean;
    function GetFieldType(Index: integer): TSynTableFieldProperties;
    function GetFieldCount: integer;
    function GetFieldFromName(const aName: RawUTF8): TSynTableFieldProperties;
    function GetFieldFromNameLen(aName: PUTF8Char; aNameLen: integer): TSynTableFieldProperties;
    /// following method matchs the TSynTableFieldIndex event type
    function GetFieldIndexFromName(const aName: RawUTF8): integer; overload;
    function GetFieldIndexFromNameLen(aName: PUTF8Char; aNameLen: integer): integer; overload;
    /// refresh Offset,FieldNumber,FieldSize and fFieldVariableIndex,fFieldVariableOffset
    procedure AfterFieldModif;
  public
    /// create a table definition instance
    constructor Create(const aTableName: RawUTF8);
    /// create a table definition instance from a specified file reader
    procedure LoadFrom(var RD: TFileBufferReader);
    /// release used memory
    destructor Destroy; override;
    /// save field properties to a specified file writer
    procedure SaveTo(WR: TFileBufferWriter);

    /// retrieve to the corresponding data address of a given field
    function GetData(RecordBuffer: PUTF8Char; Field: TSynTableFieldProperties): pointer;
    /// add a field description to the table
    // - warning: the class responsible of the storage itself must process the
    // data already stored when a field is created, e.g. in
    // TSynBigTableRecord.AddFieldUpdate method
    // - physical order does not necessary follow the AddField() call order:
    // for better performance, it will try to store fixed-sized record first,
    // multiple of 4 bytes first (access is faster if dat is 4 byte aligned),
    // then variable-length after fixed-sized fields; in all case, a field
    // indexed will be put first
    function AddField(const aName: RawUTF8; aType: TSynTableFieldType;
      aOptions: TSynTableFieldOptions=[]): TSynTableFieldProperties;
    /// update a record content
    // - return the updated record data, in our SBF compact binary format
    // - if NewFieldData is not specified, a default 0 or '' value is appended
    // - if NewFieldData is set, it must match the field value kind
    // - warning: this method will update result in-place, so RecordBuffer MUST
    // be <> pointer(result) or data corruption may occur
    procedure UpdateFieldData(RecordBuffer: PUTF8Char; RecordBufferLen,
      FieldIndex: integer; var result: TSBFString; const NewFieldData: TSBFString='');
    /// update a record content after any AddfieldUpdate, to refresh the data
    // - AvailableFields must contain the list of existing fields in the previous data
    function UpdateFieldRecord(RecordBuffer: PUTF8Char; var AvailableFields: TSQLFieldBits): TSBFString;
    /// this Event is to be called for all data records (via a GetIterating method)
    // after any AddfieldUpdate, to refresh the data
    // - Opaque is in fact a pointer to a TUpdateFieldEvent record, and will contain
    // all parameters set by TSynBigTableRecord.AddFieldUpdate, including a
    // TFileBufferWriter instance to use to write the recreated data
    // - it will work with either any newly added field, handly also field data
    // order change in SBF record (e.g. when a fixed-sized field has been added
    // on a record containing variable-length fields)
    function UpdateFieldEvent(Sender: TObject; Opaque: pointer; ID, Index: integer;
      Data: pointer; DataLen: integer): boolean;
    /// event which must be called by the storage engine when some values are modified
    // - if aOldIndex and aNewIndex are both >= 0, the corresponding aOldIndex
    // will be replaced by aNewIndex value (i.e. called in case of a data Update)
    // - if aOldIndex is -1 and aNewIndex is >= 0, aNewIndex refers to a just
    // created item (i.e. called in case of a data Add)
    // - if aOldIndex is >= 0 and aNewIndex is -1, aNewIndex refers to a just
    // deleted item (i.e. called in case of a data Delete)
    // - will update then sort all existing TSynTableFieldProperties.OrderedIndex
    // values
    // - the GetDataBuffer protected virtual method must have been overridden to
    // properly return the record data for a given "physical/stored" index
    // - aOldRecordData and aNewRecordData can be specified in order to guess
    // if the field data has really been modified (speed up the update a lot
    // to only sort indexed fields if its content has been really modified)
    procedure FieldIndexModify(aOldIndex, aNewIndex: integer;
      aOldRecordData, aNewRecordData: pointer);
    /// return the total length of the given record buffer, encoded in our SBF
    // compact binary format
    function DataLength(RecordBuffer: pointer): integer;
    {$ifndef NOVARIANTS}
    /// create a Variant able to access any field content via late binding
    // - i.e. you can use Var.Name to access the 'Name' field of record Var
    // - if you leave ID and RecordBuffer void, a void record is created
    function Data(aID: integer=0; RecordBuffer: pointer=nil;
      RecordBufferLen: Integer=0): Variant; overload;
    {$endif NOVARIANTS}
    /// return a default content for ALL record fields
    // - uses our SBF compact binary format
    property DefaultRecordData: TSBFString read fDefaultRecordData;
    /// list of TSynTableFieldProperties added via all AddField() call
    // - this list will allow TSynBigTableRecord.AddFieldUpdate to refresh
    // the data on disk according to the new field configuration
    property AddedField: TList read fAddedField write fAddedField;
    /// offset of the first variable length value field
    property FieldVariableOffset: PtrUInt read fFieldVariableOffset;
  public
    {$ifndef DELPHI5OROLDER}
    /// create a TJSONWriter, ready to be filled with GetJSONValues(W) below
    // - will initialize all TJSONWriter.ColNames[] values according to the
    // specified Fields index list, and initialize the JSON content
    function CreateJSONWriter(JSON: TStream; Expand, withID: boolean;
      const Fields: TSQLFieldIndexDynArray): TJSONWriter; overload;
    /// create a TJSONWriter, ready to be filled with GetJSONValues(W) below
    // - will initialize all TJSONWriter.ColNames[] values according to the
    // specified Fields bit set, and initialize the JSON content
    function CreateJSONWriter(JSON: TStream; Expand, withID: boolean;
      const Fields: TSQLFieldBits): TJSONWriter; overload;
    /// return the UTF-8 encoded JSON objects for the values contained
    // in the specified RecordBuffer encoded in our SBF compact binary format,
    // according to the Expand/WithID/Fields parameters of W
    // - if W.Expand is true, JSON data is an object, for direct use with any Ajax or .NET client:
    // ! {"col1":val11,"col2":"val12"}
    //- if W.Expand is false, JSON data is serialized (as used in TSQLTableJSON)
    // ! { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
    // - only fields with a bit set in W.Fields will be appended
    // - if W.WithID is true, then the first ID field value is included
    procedure GetJSONValues(aID: integer; RecordBuffer: PUTF8Char; W: TJSONWriter);
    /// can be used to retrieve all values matching a preparated TSynTableStatement
    // - this method matchs the TSynBigTableIterateEvent callback definition
    // - Sender will be the TSynBigTable instance, and Opaque will point to a
    // TSynTableStatement instance (with all fields initialized, including Writer)
    function IterateJSONValues(Sender: TObject; Opaque: pointer; ID: integer;
      Data: pointer; DataLen: integer): boolean;
    {$endif DELPHI5OROLDER}
    /// check the registered constraints according to a record SBF buffer
    // - returns '' on success
    // - returns an error message e.g. if a tftUnique constraint failed
    // - RecordIndex=-1 in case of adding, or the physical index of the updated record
    function Validate(RecordBuffer: pointer; RecordIndex: integer): string;
    /// filter the SBF buffer record content with all registered filters
    // - all field values are filtered in-place, following our SBF compact
    // binary format encoding for this record
    procedure Filter(var RecordBuffer: TSBFString);

    /// event used for proper data retrieval of a given record buffer, according
    // to the physical/storage index value (not per-ID index)
    // - if not set, field indexes won't work
    // - will be mapped e.g. to TSynBigTable.GetPointerFromPhysicalIndex
    property GetRecordData: TSynTableGetRecordData read fGetRecordData write fGetRecordData;
  public
    /// the internal Table name used to identify it (e.g. from JSON or SQL)
    // - similar to the SQL Table name
    property TableName: RawUTF8 read fTableName write fTableName;
    /// number of fields in this table
    property FieldCount: integer read GetFieldCount;
    /// retrieve the properties of a given field
    // - returns nil if the specified Index is out of range
    property Field[Index: integer]: TSynTableFieldProperties read GetFieldType;
    /// retrieve the properties of a given field
    // - returns nil if the specified Index is out of range
    property FieldFromName[const aName: RawUTF8]: TSynTableFieldProperties read GetFieldFromName; default;
    /// retrieve the index of a given field
    // - returns -1 if the specified Index is out of range
    property FieldIndexFromName[const aName: RawUTF8]: integer read GetFieldIndexFromName;
    /// read-only access to the Field list
    property FieldList: TObjectList read fField;
    /// true if any field has a tfoUnique option set
    property HasUniqueIndexes: boolean read fFieldHasUniqueIndexes;
  end;

{$ifndef NOVARIANTS}
  /// a custom variant type used to have direct access to a record content
  // - use TSynTable.Data method to retrieve such a Variant
  // - this variant will store internaly a SBF compact binary format
  // representation of the record content
  // - uses internally a TSynTableData object
  TSynTableVariantType = class(TSynInvokeableVariantType)
  protected
    function IntGet(var Dest: TVarData; const Instance: TVarData; Name: PAnsiChar; NameLen: PtrInt): boolean; override;
    function IntSet(const Instance, Value: TVarData; Name: PAnsiChar; NameLen: PtrInt): boolean; override;
  public
    /// retrieve the SBF compact binary format representation of a record content
    class function ToSBF(const V: Variant): TSBFString;
    /// retrieve the ID value associated to a record content
    class function ToID(const V: Variant): integer;
    /// retrieve the TSynTable instance associated to a record content
    class function ToTable(const V: Variant): TSynTable;
    /// clear the content
    procedure Clear(var V: TVarData); override;
    /// copy two record content
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
  end;

/// initialize TSynTableVariantType if needed, and return the correspongind VType
function SynTableVariantVarType: cardinal;

{$endif NOVARIANTS}

const
  /// used by TSynTableStatement.WhereField for "SELECT .. FROM TableName WHERE ID=?"
  SYNTABLESTATEMENTWHEREID = 0;


/// low-level integer comparison according to a specified operator
// - SBF must point to the values encoded in our SBF compact binary format
// - Value must contain the plain integer value
// - Value can be a Currency accessed via a PInt64
// - will work only for tftBoolean, tftUInt8, tftUInt16, tftUInt24,
// tftInt32, tftInt64 and tftCurrency field types
// - will handle only soEqualTo...soGreaterThanOrEqualTo operators
// - if SBFEnd is not nil, it will test for all values until SBF>=SBFEnd
// (can be used for tftArray)
// - returns true if both values match, or false otherwise
function CompareOperator(FieldType: TSynTableFieldType; SBF, SBFEnd: PUTF8Char;
  Value: Int64; Oper: TCompareOperator): boolean; overload;

/// low-level floating-point comparison according to a specified operator
// - SBF must point to the values encoded in our SBF compact binary format
// - Value must contain the plain floating-point value
// - will work only for tftDouble field type
// - will handle only soEqualTo...soGreaterThanOrEqualTo operators
// - if SBFEnd is not nil, it will test for all values until SBF>=SBFEnd
// (can be used for tftArray)
// - returns true if both values match, or false otherwise
function CompareOperator(SBF, SBFEnd: PUTF8Char;
  Value: double; Oper: TCompareOperator): boolean; overload;

/// low-level text comparison according to a specified operator
// - SBF must point to the values encoded in our SBF compact binary format
// - Value must contain the plain text value, in the same encoding (either
// WinAnsi either UTF-8, as FieldType defined for the SBF value)
// - will work only for tftWinAnsi and tftUTF8 field types
// - will handle all kind of operators - including soBeginWith, soContains and
// soSoundsLike* - but soSoundsLike* won't make use of the CaseSensitive parameter
// - for soSoundsLikeEnglish, soSoundsLikeFrench and soSoundsLikeSpanish
// operators, Value is not a real PUTF8Char but a prepared PSynSoundEx
// - if SBFEnd is not nil, it will test for all values until SBF>=SBFEnd
// (can be used for tftArray)
// - returns true if both values match, or false otherwise
function CompareOperator(FieldType: TSynTableFieldType; SBF, SBFEnd: PUTF8Char;
  Value: PUTF8Char; ValueLen: integer; Oper: TCompareOperator;
  CaseSensitive: boolean): boolean; overload;

/// convert any AnsiString content into our SBF compact binary format storage
procedure ToSBFStr(const Value: RawByteString; out Result: TSBFString);


implementation

{$ifdef WITH_FASTMM4STATS}
uses
  FastMM4; // override OS information by actual FastMM4 status
{$endif WITH_FASTMM4STATS}

{$ifdef FPCLINUX}
uses
  termio,
  {$ifdef BSD}
  ctypes,
  sysctl,
  {$else}
  Linux,
  {$endif BSD}
  SynFPCLinux;
{$endif FPCLINUX}


{ ************ TSynTable generic types and classes ************************** }

{$ifndef NOVARIANTS}

{ TSynTableVariantType }

var
  SynTableVariantType: TCustomVariantType = nil;

function SynTableVariantVarType: cardinal;
begin
  if SynTableVariantType=nil then
    SynTableVariantType := SynRegisterCustomVariantType(TSynTableVariantType);
  result := SynTableVariantType.VarType;
end;

procedure TSynTableVariantType.Clear(var V: TVarData);
begin
  //Assert(V.VType=SynTableVariantType.VarType);
  TSynTableData(V).VValue := ''; // clean memory release
  PPtrUInt(@V)^ := 0; // will set V.VType := varEmpty
end;

procedure TSynTableVariantType.Copy(var Dest: TVarData;
  const Source: TVarData; const Indirect: Boolean);
begin
  //Assert(Source.VType=SynTableVariantType.VarType);
  inherited Copy(Dest,Source,Indirect); // copy VType+VID+VTable
  if not Indirect then
    with TSynTableData(Dest) do begin
      PtrInt(VValue) := 0; // avoid GPF
      VValue := TSynTableData(Source).VValue; // copy by reference
    end;
end;

function TSynTableVariantType.IntGet(var Dest: TVarData; const Instance: TVarData;
  Name: PAnsiChar; NameLen: PtrInt): boolean;
begin
  result:= TSynTableData(Instance).GetFieldVarData(pointer(Name),NameLen,Dest);
end;

function TSynTableVariantType.IntSet(const Instance, Value: TVarData;
  Name: PAnsiChar; NameLen: PtrInt): boolean;
var aName: RawUTF8;
begin
  FastSetString(aName,Name,NameLen);
  TSynTableData(Instance).SetField(aName,Variant(Value));
  result := true;
end;

class function TSynTableVariantType.ToID(const V: Variant): integer;
var Data: TSynTableData absolute V;
begin
  if Data.VType<>SynTableVariantType.VarType then
    result := 0 else
    result := Data.VID;
end;

class function TSynTableVariantType.ToSBF(const V: Variant): TSBFString;
var Data: TSynTableData absolute V;
begin
  if Data.VType<>SynTableVariantType.VarType then
    result := '' else
    result := Data.VValue;
end;

class function TSynTableVariantType.ToTable(const V: Variant): TSynTable;
var Data: TSynTableData absolute V;
begin
  if Data.VType<>SynTableVariantType.VarType then
    result := nil else
    result := Data.VTable;
end;

{$endif NOVARIANTS}


{ TSynTable }

{$ifdef CPUX86}
function SortQWord(const A,B: QWord): integer; {$ifdef FPC} nostackframe; assembler; {$endif}
asm // Delphi x86 compiler is not efficient, and oldest even incorrect
        mov     ecx, [eax]
        mov     eax, [eax + 4]
        cmp     eax, [edx + 4]
        jnz     @nz
        cmp     ecx, [edx]
        jz      @0
@nz:    jnb     @p
        or      eax, -1
        ret
@0:     xor     eax, eax
        ret
@p:     mov     eax, 1
end;

function SortInt64(const A,B: Int64): integer; {$ifdef FPC} nostackframe; assembler; {$endif}
asm // Delphi x86 compiler is not efficient at compiling below code
        mov     ecx, [eax]
        mov     eax, [eax + 4]
        cmp     eax, [edx + 4]
        jnz     @nz
        cmp     ecx, [edx]
        jz      @0
        jnb     @p
@n:     or      eax, -1
        ret
@0:     xor     eax, eax
        ret
@nz:    jl      @n
@p:     mov     eax, 1
end;
{$endif}

{$ifndef SORTCOMPAREMETHOD}

function SortU8(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := PByte(P1)^-PByte(P2)^;
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortU16(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := PWord(P1)^-PWord(P2)^;
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortI32(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := PInteger(P1)^-PInteger(P2)^;
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortDouble(P1,P2: PUTF8Char): PtrInt;
var V: Double;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        V := unaligned(PDouble(P1)^)-unaligned(PDouble(P2)^);
        if V<0 then
          result := -1 else
        if V=0 then
          result := 0 else
          result := 1;
       end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortU24(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := PtrInt(PWord(P1)^)+PtrInt(P1[2])shl 16
          -PtrInt(PWord(P2)^)-PtrInt(P2[2]) shl 16;
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarUInt32(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := FromVarUInt32(PByte(P1))-FromVarUInt32(PByte(P2));
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarInt32(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := FromVarInt32(PByte(P1))-FromVarInt32(PByte(P2));
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

{$ifdef CPU64} // PtrInt = Int64 -> so direct substraction works

function SortI64(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
        result := PInt64(P1)^-PInt64(P2)^ else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarUInt64(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
        result := FromVarUInt64(PByte(P1))-FromVarUInt64(PByte(P2)) else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarInt64(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
        result := FromVarInt64(PByte(P1))-FromVarInt64(PByte(P2)) else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

{$else}

{$ifdef CPUX86} // circumvent comparison slowness (and QWord bug)

function SortI64(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
        result := SortInt64(PInt64(P1)^,PInt64(P2)^) else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarUInt64(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
        result := SortQWord(FromVarUInt64(PByte(P1)),FromVarUInt64(PByte(P2))) else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarInt64(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
        result := SortInt64(FromVarInt64(PByte(P1)),FromVarInt64(PByte(P2))) else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

{$else}

function SortI64(P1,P2: PUTF8Char): PtrInt;
var V: Int64;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        V := PInt64(P1)^-PInt64(P2)^;
        if V<0 then
          result := -1 else
        if V>0 then
          result := 1 else
          result := 0;
       end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarUInt64(P1,P2: PUTF8Char): PtrInt;
var V1,V2: QWord;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        V1 := FromVarUInt64(PByte(P1));
        V2 := FromVarUInt64(PByte(P2));
        if V1>V2 then
          result := 1 else
        if V1=V2 then
          result := 0 else
          result := -1;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarInt64(P1,P2: PUTF8Char): PtrInt;
var V1,V2: Int64;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        V1 := FromVarInt64(PByte(P1));
        V2 := FromVarInt64(PByte(P2));
        if V1>V2 then
          result := 1 else
        if V1=V2 then
          result := 0 else
          result := -1;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

{$endif CPUX86}

{$endif CPU64}

function SortStr(P1,P2: PUTF8Char): PtrInt;
var L1, L2, L, i: PtrInt;
    PB1, PB2: PByte;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        if PtrInt(P1^)<=$7F then begin
          L1 := PtrInt(P1^);
          inc(P1);
        end else begin
          PB1 := pointer(P1);
          L1 := FromVarUInt32High(PB1);
          P1 := pointer(PB1);
        end;
        if PtrInt(P2^)<=$7F then begin
          L2 := PtrInt(P2^);
          inc(P2);
        end else begin
          PB2 := pointer(P2);
          L2 := FromVarUInt32High(PB2);
          P2 := pointer(PB2);
        end;
        L := L1;
        if L2>L then
          L := L2;
        for i := 0 to L-1 do begin
          result := PtrInt(P1[i])-PtrInt(P2[i]);
          if Result<>0 then
            exit;
        end;
        result := L1-L2;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortIStr(P1,P2: PUTF8Char): PtrInt;
var L1, L2, L, i: PtrInt;
    PB1, PB2: PByte;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        if PtrInt(P1^)<=$7F then begin
          L1 := PtrInt(P1^);
          inc(P1);
        end else begin
          PB1 := pointer(P1);
          L1 := FromVarUInt32High(PB1);
          P1 := pointer(PB1);
        end;
        if PtrInt(P2^)<=$7F then begin
          L2 := PtrInt(P2^);
          inc(P2);
        end else begin
          PB2 := pointer(P2);
          L2 := FromVarUInt32High(PB2);
          P2 := pointer(PB2);
        end;
        if L2>L1 then
          L := L2 else
          L := L1;
        for i := 0 to L-1 do // NormToUpperAnsi7 works for both WinAnsi & UTF-8
          if NormToUpperAnsi7[P1[i]]<>NormToUpperAnsi7[P2[i]] then begin
            result := PtrInt(P1[i])-PtrInt(P2[i]);
            exit;
          end;
        result := L1-L2;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

const
  FIELD_SORT: array[TSynTableFieldType] of TUTF8Compare = (
    nil, // tftUnknown,
    SortU8,    SortU8,  SortU16,  SortU24,  SortI32, SortI64,
 // tftBoolean,tftUInt8,tftUInt16,tftUInt24,tftInt32,tftInt64,
    SortI64,  SortDouble, SortVarUInt32,SortVarInt32,SortVarUInt64,
 // tftCurrency,tftDouble, tftVarUInt32, tftVarInt32,tftVarUInt64,
    SortStr,   SortStr, SortStr,        nil,           SortVarInt64);
 // tftWinAnsi,tftUTF8, tftBlobInternal,tftBlobExternal,tftVarInt64);

{$endif SORTCOMPAREMETHOD}

const
  FIELD_FIXEDSIZE: array[TSynTableFieldType] of Integer = (
     0, // tftUnknown,
     1, 1, 2, 3, 4, 8, 8, 8,
     // tftBoolean, tftUInt8, tftUInt16, tftUInt24, tftInt32, tftInt64, tftCurrency, tftDouble
     -1, -1, -1, // tftVarUInt32, tftVarInt32, tftVarUInt64 have -1 as size
     -2, -2, -2, // tftWinAnsi, tftUTF8, tftBlobInternal have -2 as size
     -3,  // tftBlobExternal has -3 as size
     -1); //tftVarInt64

  // note: boolean is not in this set, because it can be 'true' or 'false'
  FIELD_INTEGER: TSynTableFieldTypes = [
    tftUInt8, tftUInt16, tftUInt24, tftInt32, tftInt64,
    tftVarUInt32, tftVarInt32, tftVarUInt64, tftVarInt64];

function TSynTable.AddField(const aName: RawUTF8;
  aType: TSynTableFieldType; aOptions: TSynTableFieldOptions): TSynTableFieldProperties;
var aSize: Integer;
begin
  result := nil;
  aSize := FIELD_FIXEDSIZE[aType];
  if (self=nil) or (aSize=0) or IsRowID(pointer(aName)) or
     not PropNameValid(pointer(aName)) or (GetFieldFromName(aName)<>nil) then
    exit;
  result := TSynTableFieldProperties.Create;
  if fAddedField=nil then
    fAddedField := TList.Create;
  fAddedField.Add(result);
  result.Name := aName;
  result.FieldType := aType;
  if tfoUnique in aOptions then
    Include(aOptions,tfoIndex); // create an index for faster Unique field
  if aSize=-3 then // external field has no index available
    aOptions := aOptions-[tfoIndex,tfoUnique];
  result.Options := aOptions;
  if aSize>0 then begin
    // fixed-size field should be inserted left-side of the stream
    if (tfoIndex in aOptions) or (aSize and 3=0) then begin
      // indexed field or size is alignment friendly: put left side
      if not ((tfoIndex in aOptions) and (aSize and 3=0)) then
        // indexed+aligned field -> set first, otherwise at variable or not indexed
        while result.FieldNumber<fField.Count do
          with TSynTableFieldProperties(fField.List[result.FieldNumber]) do
          if (Offset<0) or not (tfoIndex in Options) then
            break else
            inc(result.FieldNumber);
      end else
      // not indexed field: insert after previous fixed-sized fields
      if fFieldVariableIndex>=0 then
        result.FieldNumber := fFieldVariableIndex else
        result.FieldNumber := fField.Count;
    fField.Insert(result.FieldNumber,result);
  end else begin
    if (tfoIndex in aOptions) and (fFieldVariableIndex>=0) then begin
      // indexed field should be added left side (faster access for sort)
      result.FieldNumber := fFieldVariableIndex;
      while result.FieldNumber<fField.Count do
        with TSynTableFieldProperties(fField.List[result.FieldNumber]) do
        if not (tfoIndex in Options) then
          break else
          inc(result.FieldNumber);
      fField.Insert(result.FieldNumber,result);
    end else
      // not indexed field: just add at the end of the field list
      result.FieldNumber := fField.Add(result);
  end;
  if tfoUnique in aOptions then begin
    fFieldHasUniqueIndexes := true;
    result.AddFilterOrValidate(TSynValidateTableUniqueField.Create);
  end;
  AfterFieldModif; // set Offset,FieldNumber,FieldSize fFieldVariableIndex/Offset
end;

procedure TSynTable.UpdateFieldData(RecordBuffer: PUTF8Char; RecordBufferLen,
  FieldIndex: integer; var result: TSBFString; const NewFieldData: TSBFString);
var NewSize, DestOffset, OldSize: integer;
    F: TSynTableFieldProperties;
    NewData, Dest: PAnsiChar;
begin
  if (self<>nil) and ((RecordBuffer=nil) or (RecordBufferLen=0)) then begin
    // no data yet -> use default
    RecordBuffer := pointer(fDefaultRecordData);
    RecordBufferLen := fDefaultRecordLength;
  end;
  if RecordBuffer=pointer(result) then
    // update content code below will fail -> please correct calling code
    raise ETableDataException.CreateUTF8('In-place call of %.UpdateFieldData',[self]);
  if (self=nil) or (cardinal(FieldIndex)>=cardinal(fField.Count)) then begin
    SetString(result,PAnsiChar(RecordBuffer),RecordBufferLen);
    exit;
  end;
  F := TSynTableFieldProperties(fField.List[FieldIndex]);
  NewSize := length(NewFieldData);
  if NewSize=0 then begin
    // no NewFieldData specified -> use default field data to be inserted
    NewData := pointer(F.fDefaultFieldData);
    NewSize := F.fDefaultFieldLength;
  end else
    NewData := pointer(NewFieldData);
  Dest := GetData(RecordBuffer,F);
  DestOffset := Dest-RecordBuffer;
  // update content
  OldSize :=  F.GetLength(Dest);
  dec(RecordBufferLen,OldSize);
  SetString(Result,nil,RecordBufferLen+NewSize);
  MoveFast(RecordBuffer^,PByteArray(result)[0],DestOffset);
  MoveFast(NewData^,PByteArray(result)[DestOffset],NewSize);
  MoveFast(Dest[OldSize],PByteArray(result)[DestOffset+NewSize],RecordBufferLen-DestOffset);
end;

constructor TSynTable.Create(const aTableName: RawUTF8);
begin
  if not PropNameValid(pointer(aTableName)) then
    raise ETableDataException.CreateUTF8('Invalid %.Create(%)',[self,aTableName]);
  fTableName := aTableName;
  fField := TObjectList.Create;
  fFieldVariableIndex := -1;
end;

procedure TSynTable.LoadFrom(var RD: TFileBufferReader);
var n, i: integer;
    aTableName: RawUTF8;
begin
  fField.Clear;
  RD.Read(aTableName);
  if not PropNameValid(pointer(aTableName)) then
    RD.ErrorInvalidContent;
  fTableName := aTableName;
  n := RD.ReadVarUInt32;
  if cardinal(n)>=MAX_SQLFIELDS then
    RD.ErrorInvalidContent;
  for i := 0 to n-1 do
    fField.Add(TSynTableFieldProperties.CreateFrom(RD));
  AfterFieldModif;
end;

destructor TSynTable.Destroy;
begin
  fField.Free;
  fAddedField.Free;
  inherited;
end;

function TSynTable.GetFieldCount: integer;
begin
  if self=nil then
    result := 0 else
    result := fField.Count;
end;

function TSynTable.GetFieldFromName(const aName: RawUTF8): TSynTableFieldProperties;
begin
  result := GetFieldFromNameLen(pointer(aName),length(aName));
end;

function TSynTable.GetFieldFromNameLen(aName: PUTF8Char; aNameLen: integer): TSynTableFieldProperties;
var p: ^TSynTableFieldProperties;
    i: integer;
begin
  if self<>nil then begin
    p := pointer(fField.List);
    for i := 1 to fField.Count do
      if IdemPropNameU(p^.Name,aName,aNameLen) then begin
        result := p^;
        exit;
      end else
        inc(p);
  end;
  result := nil;
end;

function TSynTable.GetFieldIndexFromName(const aName: RawUTF8): integer;
begin
  result := GetFieldIndexFromNameLen(pointer(aName),length(aName));
end;

function TSynTable.GetFieldIndexFromNameLen(aName: PUTF8Char; aNameLen: integer): integer;
var p: ^TSynTableFieldProperties;
begin
  if self<>nil then begin
    p := pointer(fField.List);
    for result := 0 to fField.Count-1 do
      if IdemPropNameU(p^.Name,aName,aNameLen) then
        exit else
        inc(p);
  end;
  result := -1;
end;

function TSynTable.GetFieldType(Index: integer): TSynTableFieldProperties;
begin
  if (self=nil) or (cardinal(Index)>=cardinal(fField.Count)) then
    result := nil else // avoid GPF
    result := fField.List[Index];
end;

{$ifndef DELPHI5OROLDER}

function TSynTable.CreateJSONWriter(JSON: TStream; Expand, withID: boolean;
  const Fields: TSQLFieldBits): TJSONWriter;
begin
  result := CreateJSONWriter(JSON,Expand,withID,FieldBitsToIndex(Fields,fField.Count));
end;

function TSynTable.CreateJSONWriter(JSON: TStream; Expand, withID: boolean;
  const Fields: TSQLFieldIndexDynArray): TJSONWriter;
var i,nf,n: integer;
begin
  if (self=nil) or ((Fields=nil) and not withID) then begin
    result := nil; // no data to retrieve
    exit;
  end;
  result := TJSONWriter.Create(JSON,Expand,withID,Fields);
  // set col names
  if withID then
    n := 1 else
    n := 0;
  nf := length(Fields);
  SetLength(result.ColNames,nf+n);
  if withID then
    result.ColNames[0] := 'ID';
  for i := 0 to nf-1 do
    result.ColNames[i+n] := TSynTableFieldProperties(fField.List[Fields[i]]).Name;
  result.AddColumns; // write or init field names for appropriate JSON Expand
end;

procedure TSynTable.GetJSONValues(aID: integer; RecordBuffer: PUTF8Char;
  W: TJSONWriter);
var i,n: integer;
    buf: array[0..MAX_SQLFIELDS-1] of PUTF8Char;
begin
  if (self=nil) or (RecordBuffer=nil) or (W=nil) then
    exit; // avoid GPF
  if W.Expand then begin
    W.Add('{');
    if W.WithID then
      W.AddString(W.ColNames[0]);
  end;
  if W.WithID then begin
    W.Add(aID);
    W.Add(',');
    n := 1;
  end else
    n := 0;
  for i := 0 to fField.Count-1 do begin
    buf[i] := RecordBuffer;
    inc(RecordBuffer,TSynTableFieldProperties(fField.List[i]).GetLength(RecordBuffer));
  end;
  for i := 0 to length(W.Fields)-1 do begin
    if W.Expand then begin
      W.AddString(W.ColNames[n]); // '"'+ColNames[]+'":'
      inc(n);
    end;
    TSynTableFieldProperties(fField.List[W.Fields[i]]).GetJSON(buf[i],W);
    W.Add(',');
  end;
  W.CancelLastComma; // cancel last ','
  if W.Expand then
    W.Add('}');
end;

function TSynTable.IterateJSONValues(Sender: TObject; Opaque: pointer;
  ID: integer; Data: pointer; DataLen: integer): boolean;
var Statement: TSynTableStatement absolute Opaque;
    F: TSynTableFieldProperties;
    nWhere,fIndex: cardinal;
begin  // note: we should have handled -2 (=COUNT) case already
  nWhere := length(Statement.Where);
  if (self=nil) or (Statement=nil) or (Data=nil) or
     (Statement.Select=nil) or (nWhere>1) or
     ((nWhere=1)and(Statement.Where[0].ValueSBF='')) then begin
    result := false;
    exit;
  end;
  result := true;
  if nWhere=1 then begin // Where=nil -> all rows
    fIndex := Statement.Where[0].Field;
    if fIndex=SYNTABLESTATEMENTWHEREID then begin
      if ID<>Statement.Where[0].ValueInteger then
        exit;
    end else begin
      dec(fIndex); // 0 is ID, 1 for field # 0, 2 for field #1, and so on...
      if fIndex<cardinal(fField.Count) then begin
        F := TSynTableFieldProperties(fField.List[fIndex]);
        if F.SortCompare(GetData(Data,F),pointer(Statement.Where[0].ValueSBF))<>0 then
          exit;
      end;
    end;
  end;
  GetJSONValues(ID,Data,Statement.Writer);
end;

{$endif DELPHI5OROLDER}

function TSynTable.GetData(RecordBuffer: PUTF8Char; Field: TSynTableFieldProperties): pointer;
var i: integer;
    PB: PByte;
begin
  if Field.Offset>=0 then
    result := RecordBuffer+Field.Offset else begin
    result := RecordBuffer+fFieldVariableOffset;
    for i := fFieldVariableIndex to Field.FieldNumber-1 do
      if i in fFieldIsVarString then begin
        // inlined result := GotoNextVarString(result);
        if PByte(result)^<=$7f then
          inc(PByte(result),PByte(result)^+1) else begin
          PB := result;
          inc(PByte(result),FromVarUInt32High(PB)+PtrUInt(PB)-PtrUInt(result));
        end;
      end else
      if not (i in fFieldIsExternal) then begin
        // inlined result := GotoNextVarInt(result)
        while PByte(result)^>$7f do inc(PByte(result));
        inc(PByte(result));
      end;
  end;
end;

procedure TSynTable.SaveTo(WR: TFileBufferWriter);
var i: Integer;
begin
  WR.Write(fTableName);
  WR.WriteVarUInt32(fField.Count);
  for i := 0 to fField.Count-1 do
    TSynTableFieldProperties(fField.List[i]).SaveTo(WR);
end;

procedure TSynTable.AfterFieldModif;
var i, Offs: integer;
begin
  PInt64(@fFieldIsVarString)^ := 0;
  PInt64(@fFieldIsExternal)^ := 0;
  fFieldVariableIndex := -1;
  fDefaultRecordLength := 0;
  fFieldHasUniqueIndexes := false;
  Offs := 0;
  for i := 0 to fField.Count-1 do
  with TSynTableFieldProperties(fField.List[i]) do begin
    FieldNumber := i;
    {$ifndef SORTCOMPAREMETHOD}
    SortCompare := FIELD_SORT[FieldType];
    {$endif}
    Owner := self;
    FieldSize := FIELD_FIXEDSIZE[FieldType];
    if FieldSize>=0 then begin
      //assert(Offs>=0);
      Offset := Offs;
      inc(Offs,FieldSize);
      inc(fDefaultRecordLength,FieldSize);
      fDefaultFieldLength := FieldSize;
    end else begin
      if FieldSize=-3 then
        Include(fFieldIsExternal,i) else begin
        fDefaultFieldLength := 1;
        inc(fDefaultRecordLength);
        if FieldSize=-2 then
          Include(fFieldIsVarString,i);
        {$ifndef SORTCOMPAREMETHOD}
        if (FieldType in [tftWinAnsi,tftUTF8]) and
           (tfoCaseInsensitive in Options) then
          SortCompare := SortIStr; // works for both WinAnsi and UTF-8 encodings
        {$endif}
      end;
      // we need the Offset even for tftBlobExternal (FieldSize=-3)
      if fFieldVariableIndex<0 then begin
        fFieldVariableIndex := i;
        fFieldVariableOffset := Offs;
        Offs := -1;
      end;
      Offset := Offs;
      dec(Offs);
    end;
    SetLength(fDefaultFieldData,fDefaultFieldLength);
    FillcharFast(pointer(fDefaultFieldData)^,fDefaultFieldLength,0);
  end;
  SetLength(fDefaultRecordData,fDefaultRecordLength);
  FillcharFast(pointer(fDefaultRecordData)^,fDefaultRecordLength,0);
end;

procedure TSynTable.FieldIndexModify(aOldIndex, aNewIndex: integer;
  aOldRecordData, aNewRecordData: pointer);
var F: integer;
begin
  for F := 0 to fField.Count-1 do
    with TSynTableFieldProperties(fField.List[F]) do
      if tfoIndex in Options then
        OrderedIndexUpdate(aOldIndex,aNewIndex,aOldRecordData,aNewRecordData);
end;

procedure TSynTable.Filter(var RecordBuffer: TSBFString);
var Old, New: RawUTF8;
    NewRecord: TSBFString; // UpdateFieldData update result in-place
    F, i: integer;
begin
  for F := 0 to fField.Count-1 do
    with TSynTableFieldProperties(fField.List[F]) do
    if Filters<>nil then begin
      Old := GetRawUTF8(pointer(RecordBuffer));
      New := Old;
      for i := 0 to Filters.Count-1 do
        TSynFilter(Filters.List[i]).Process(F,New);
      if Old<>New then begin
        // value was changed -> store modified
        UpdateFieldData(pointer(RecordBuffer),length(RecordBuffer),F,
          NewRecord,SBFFromRawUTF8(New));
        RecordBuffer := NewRecord;
      end;
    end;
end;

{$ifndef NOVARIANTS}
function TSynTable.Data(aID: integer; RecordBuffer: pointer; RecordBufferLen: Integer): Variant;
var data: TSynTableData absolute result;
begin
  VarClear(result);
  data.VType := SynTableVariantVarType;
  data.VID := aID;
  data.VTable := self;
  pointer(data.VValue) := nil; // avoid GPF
  if RecordBuffer=nil then
    data.VValue := DefaultRecordData else begin
    if RecordBufferLen=0 then
      RecordBufferLen := DataLength(RecordBuffer);
    SetString(data.VValue,PAnsiChar(RecordBuffer),RecordBufferLen);
  end;
end;
{$endif NOVARIANTS}

function TSynTable.DataLength(RecordBuffer: pointer): integer;
var F: Integer;
    PC: PUTF8Char;
begin
  if (Self<>nil) and (RecordBuffer<>nil) then begin
    PC := RecordBuffer;
    for F := 0 to fField.Count-1 do
      inc(PC,TSynTableFieldProperties(fField.List[F]).GetLength(PC));
    result := PC-RecordBuffer;
  end else
    result := 0;
end;

function TSynTable.UpdateFieldEvent(Sender: TObject; Opaque: pointer;
  ID, Index: integer; Data: pointer; DataLen: integer): boolean;
var Added: PUpdateFieldEvent absolute Opaque;
    F, aSize: integer;
begin // in practice, this data processing is very fast (thanks to WR speed)
  with Added^ do begin
    result := Count<length(IDs);
    if not result then
      exit;
    for F := 0 to fField.Count-1 do
    with TSynTableFieldProperties(fField.List[F]) do
    if F in AvailableFields then begin
      // add previous field content: will handle any field offset change in record
      aSize := Getlength(Data);
      WR.Write(Data,aSize);
      inc(PByte(Data),aSize);
    end else
      // add default field content for a newly added field
      WR.Write(Pointer(fDefaultFieldData),fDefaultFieldLength);
    if WR.TotalWritten>1 shl 30 then
      raise ETableDataException.CreateUTF8('%: File size too big (>1GB)',[self]) else
      Offsets64[Count] := WR.TotalWritten;
    IDs[Count] := ID;
    NewIndexs[Index] := Count;
    inc(Count);
  end;
end;

function TSynTable.UpdateFieldRecord(RecordBuffer: PUTF8Char;
  var AvailableFields: TSQLFieldBits): TSBFString;
var Lens: array[0..MAX_SQLFIELDS-1] of Integer;
    F, Len, TotalLen: integer;
    P: PUTF8Char;
    Dest: PByte;
begin
  // retrieve all field buffer lengths, to speed up record content creation
  TotalLen := 0;
  P := RecordBuffer;
  for F := 0 to fField.Count-1 do
  with TSynTableFieldProperties(fField.List[F]) do
  if F in AvailableFields then begin
    Len := GetLength(P);
    inc(P,Len);
    inc(TotalLen,Len);
    Lens[F] := Len;
  end else
    inc(TotalLen,fDefaultFieldLength);
  // create new record content
  P := RecordBuffer;
  SetString(Result,nil,TotalLen);
  Dest := pointer(Result);
  for F := 0 to fField.Count-1 do
  with TSynTableFieldProperties(fField.List[F]) do
    if F in AvailableFields then begin
      Len := Lens[F];
      MoveFast(P^,Dest^,Len);
      inc(P,Len);
      inc(Dest,Len);
    end else begin
      FillcharFast(Dest^,fDefaultFieldLength,0);
      inc(Dest,fDefaultFieldLength);
    end;
  //Assert(PtrUInt(Dest)-PtrUInt(result)=PtrUInt(TotalLen));
end;

function TSynTable.Validate(RecordBuffer: pointer; RecordIndex: integer): string;
var F: integer;
begin
  result := '';
  for F := 0 to fField.Count-1 do
    with TSynTableFieldProperties(fField.List[F]) do
      if Validates<>nil then begin
        result := Validate(RecordBuffer,RecordIndex);
        if result<>'' then
          exit;
      end;
end;


{ TSynTableFieldProperties }

constructor TSynTableFieldProperties.CreateFrom(var RD: TFileBufferReader);
begin
  fOrderedIndexFindAdd := -1;
  RD.Read(Name);
  if not PropNameValid(pointer(Name)) then
    RD.ErrorInvalidContent;
  RD.Read(@FieldType,SizeOf(FieldType));
  RD.Read(@Options,SizeOf(Options));
  if (FieldType>high(FieldType)) then
    RD.ErrorInvalidContent;
  OrderedIndexCount := RD.ReadVarUInt32Array(OrderedIndex);
  if OrderedIndexCount>0 then begin
    if tfoIndex in Options then begin
      //assert(OrderedIndexReverse=nil);
      OrderedIndexReverseSet(-1); // compute whole OrderedIndexReverse[] array
    end else
      RD.ErrorInvalidContent;
  end;
  // we allow a void OrderedIndex[] array from disk
end;

destructor TSynTableFieldProperties.Destroy;
begin
  Filters.Free;
  Validates.Free;
  inherited;
end;

function TSynTableFieldProperties.GetJSON(FieldBuffer: pointer;
  W: TTextWriter): pointer;
var len: integer;
    tmp: RawUTF8;
begin
  case FieldType of
  // fixed-sized field value
  tftBoolean:
    W.Add(PBoolean(FieldBuffer)^);
  tftUInt8:
    W.Add(PByte(FieldBuffer)^);
  tftUInt16:
    W.Add(PWord(FieldBuffer)^);
  tftUInt24:
    // PInteger()^ and $ffffff -> possible GPF on Memory Mapped file
    W.Add(PWord(FieldBuffer)^+integer(PByteArray(FieldBuffer)^[2])shl 16);
  tftInt32:
    W.Add(PInteger(FieldBuffer)^);
  tftInt64:
    W.Add(PInt64(FieldBuffer)^);
  tftCurrency:
    W.AddCurr64(PInt64(FieldBuffer)^);
  tftDouble:
    W.AddDouble(unaligned(PDouble(FieldBuffer)^));
  // some variable-size field value
  tftVarUInt32:
    W.Add(FromVarUInt32(PByte(FieldBuffer)));
  tftVarInt32:
    W.Add(FromVarInt32(PByte(FieldBuffer)));
  tftVarUInt64:
    W.AddQ(FromVarUInt64(PByte(FieldBuffer)));
  tftVarInt64:
    W.Add(FromVarInt64(PByte(FieldBuffer)));
  // text storage - WinAnsi could use less space than UTF-8
  tftWinAnsi, tftUTF8: begin
    W.Add('"');
    len := FromVarUInt32(PByte(FieldBuffer));
    if len>0 then
      if FieldType=tftUTF8 then
        W.AddJSONEscape(PAnsiChar(FieldBuffer),len) else begin
        SetLength(tmp,len*3); // in-place decoding and appending
        W.AddJSONEscape(pointer(tmp),WinAnsiBufferToUtf8(pointer(tmp),PAnsiChar(FieldBuffer),len)-pointer(tmp));
      end;
    W.Add('"');
    result := PAnsiChar(FieldBuffer)+len;
    exit;
  end;
  tftBlobInternal: begin
    W.AddShort('"X''');
    len := FromVarUInt32(PByte(FieldBuffer));
    W.AddBinToHex(PByte(FieldBuffer),len);
    W.Add('''','"');
  end;
  tftBlobExternal:
    ; // BLOB fields are not handled here, but must be directly accessed
  end;
  result := PAnsiChar(FieldBuffer)+FieldSize; // // tftWinAnsi,tftUTF8 already done
end;

function TSynTableFieldProperties.GetLength(FieldBuffer: pointer): Integer;
var PB: PByte;
begin
  if FieldSize>=0 then
    result := FieldSize else
    case FieldSize of
    -1: begin // variable-length data
      result := 0;
      while PByteArray(FieldBuffer)^[result]>$7f do inc(result);
      inc(result);
    end;
    -2: begin // tftWinAnsi, tftUTF8, tftBlobInternal records
      result := PByte(FieldBuffer)^;
      if result<=$7F then
        inc(Result) else begin
        PB := FieldBuffer;
        result := FromVarUInt32High(PB)+PtrUInt(PB)-PtrUInt(FieldBuffer);
      end;
    end;
    else
      result := 0; // tftBlobExternal is not stored in FieldBuffer
    end;
end;

{$ifndef NOVARIANTS}
function TSynTableFieldProperties.GetVariant(FieldBuffer: pointer): Variant;
begin
  GetVariant(FieldBuffer,result);
end;

procedure TSynTableFieldProperties.GetVariant(FieldBuffer: pointer; var result: Variant);
var len: integer;
    PB: PByte absolute FieldBuffer;
    PA: PAnsiChar absolute FieldBuffer;
    PU: PUTF8Char absolute FieldBuffer;
    tmp: RawByteString;
    {$ifndef HASVARUSTRING}
    WS: SynUnicode;
    {$endif}
begin
  case FieldType of
  // fixed-sized field value
  tftBoolean:
    result := PBoolean(FieldBuffer)^;
  tftUInt8:
    result := PB^;
  tftUInt16:
    result := PWord(FieldBuffer)^;
  tftUInt24:
    // PInteger()^ and $ffffff -> possible GPF on Memory Mapped file
    result := PWord(FieldBuffer)^+integer(PByteArray(FieldBuffer)^[2])shl 16;
  tftInt32:
    result := PInteger(FieldBuffer)^;
  tftInt64:
    result := PInt64(FieldBuffer)^;
  tftCurrency:
    result := PCurrency(FieldBuffer)^;
  tftDouble:
    result := unaligned(PDouble(FieldBuffer)^);
  // some variable-size field value
  tftVarUInt32:
    result := FromVarUInt32(PB);
  tftVarInt32:
    result := FromVarInt32(PB);
  tftVarUInt64:
    result := FromVarUInt64(PB);
  tftVarInt64:
    result := FromVarInt64(PB);
  // text storage - WinAnsi could use less space than UTF-8
  tftWinAnsi: begin
    len := FromVarUInt32(PB);
    if len>0 then
      {$ifdef HASVARUSTRING}
      result := WinAnsiToUnicodeString(PA,len)
      {$else}
      result := CurrentAnsiConvert.AnsiToAnsi(WinAnsiConvert,PA,len)
      {$endif} else
      result := '';
  end;
  tftUTF8: begin
    len := FromVarUInt32(PB);
    if len>0 then
      {$ifdef HASVARUSTRING}
      result := UTF8DecodeToUnicodeString(PU,len)
      {$else} begin
        UTF8ToSynUnicode(PU,len,WS);
        result := WS;
      end
      {$endif} else
      result := '';
  end;
  tftBlobInternal: begin
    len := FromVarUInt32(PB);
    SetString(tmp,PA,len);
    result := tmp; // return internal BLOB content as string
  end
  else
    result := ''; // tftBlobExternal fields e.g. must be directly accessed
  end;
end;
{$endif}

{$ifdef ISDELPHI20062007}
  {$WARNINGS OFF} // circument Delphi 2007 false positive warning
{$endif}

function TSynTableFieldProperties.GetValue(FieldBuffer: pointer): RawUTF8;
var len: integer;
    PB: PByte absolute FieldBuffer;
    PC: PAnsiChar absolute FieldBuffer;
begin
  result := '';
  case FieldType of
  // fixed-sized field value
  tftBoolean:
    result := BOOL_UTF8[PBoolean(FieldBuffer)^];
  tftUInt8:
    UInt32ToUtf8(PB^,result);
  tftUInt16:
    UInt32ToUtf8(PWord(FieldBuffer)^,result);
  tftUInt24:
    // PInteger()^ and $ffffff -> possible GPF on Memory Mapped file
    UInt32ToUtf8(PWord(FieldBuffer)^+integer(PByteArray(FieldBuffer)^[2])shl 16,result);
  tftInt32:
    Int32ToUtf8(PInteger(FieldBuffer)^,result);
  tftInt64:
    Int64ToUtf8(PInt64(FieldBuffer)^,result);
  tftCurrency:
    Curr64ToStr(PInt64(FieldBuffer)^,result);
  tftDouble:
    DoubleToStr(unaligned(PDouble(FieldBuffer)^),result);
  // some variable-size field value
  tftVarUInt32:
    UInt32ToUtf8(FromVarUInt32(PB),result);
  tftVarInt32:
    Int32ToUtf8(FromVarInt32(PB),result);
  tftVarUInt64:
    UInt64ToUtf8(FromVarUInt64(PB),result);
  tftVarInt64:
    Int64ToUtf8(FromVarInt64(PB),result);
  // text storage - WinAnsi could use less space than UTF-8
  tftWinAnsi, tftUTF8, tftBlobInternal: begin
    len := FromVarUInt32(PB);
    if len>0 then
      if FieldType<>tftWinAnsi then
        SetString(result,PC,len) else
        result := WinAnsiConvert.AnsiBufferToRawUTF8(PC,len);
  end;
  // tftBlobExternal fields e.g. must be directly accessed
  end;
end;

{$ifdef ISDELPHI20062007}
  {$WARNINGS ON} // circument Delphi 2007 false positive warning
{$endif}

procedure TSynTableFieldProperties.OrderedIndexReverseSet(aOrderedIndex: integer);
var nrev, ndx, n: PtrInt;
begin
  n := length(OrderedIndex);
  nrev := length(OrderedIndexReverse);
  if nrev=0 then
    if n=0 then
      exit else begin
      // void OrderedIndexReverse[]
      nrev := MaxInteger(OrderedIndex,OrderedIndexCount,n)+1;
      SetLength(OrderedIndexReverse,nrev);
      FillcharFast(OrderedIndexReverse[0],nrev*4,255); // all to -1
      Reverse(OrderedIndex,OrderedIndexCount,pointer(OrderedIndexReverse));
    end;
  if PtrUInt(aOrderedIndex)>=PtrUInt(OrderedIndexCount) then
    exit; // e.g. CreateFrom() will call OrderedIndexReverseSet(-1)
  if nrev<n then begin
    SetLength(OrderedIndexReverse,n); // resize if needed
    nrev := n;
  end;
  ndx := OrderedIndex[aOrderedIndex];
  if ndx>=nrev then
    SetLength(OrderedIndexReverse,ndx+256) else
  OrderedIndexReverse[ndx] := aOrderedIndex;
end;

procedure TSynTableFieldProperties.OrderedIndexSort(L, R: PtrInt);
var I, J, P: PtrInt;
    TmpI, TmpJ: integer;
begin
  if (L<R) and Assigned(Owner.GetRecordData) then
  repeat
    I := L; J := R;
    P := (L + R) shr 1;
    repeat
      with Owner do begin
        SortPivot := GetData(GetRecordData(OrderedIndex[P],DataTemp1),self);
        while SortCompare(GetData(GetRecordData(OrderedIndex[I],DataTemp2),self),
          SortPivot)<0 do inc(I);
        while SortCompare(GetData(GetRecordData(OrderedIndex[J],DataTemp2),self),
          SortPivot)>0 do dec(J);
      end;
      if I <= J then begin
        if I < J then begin
          TmpJ := OrderedIndex[J];
          TmpI := OrderedIndex[I];
          OrderedIndex[J] := TmpI;
          OrderedIndex[I] := TmpJ;
          // keep OrderedIndexReverse[OrderedIndex[i]]=i
          OrderedIndexReverse[TmpJ] := I;
          OrderedIndexReverse[TmpI] := J;
        end;
        if P = I then P := J else if P = J then P := I;
        inc(I); dec(J);
      end;
    until I > J;
    if J - L < R - I then begin // use recursion only for smaller range
      if L < J then
        OrderedIndexSort(L, J);
      L := I;
    end else begin
      if I < R then
        OrderedIndexSort(I, R);
      R := J;
    end;
  until L >= R;
end;

procedure TSynTableFieldProperties.OrderedIndexRefresh;
begin
  if (self=nil) or not OrderedIndexNotSorted then
    exit; // already sorted
  OrderedIndexSort(0,OrderedIndexCount-1);
  OrderedIndexNotSorted := false;
end;

function TSynTableFieldProperties.OrderedIndexFind(Value: pointer): PtrInt;
var L,R: PtrInt;
    cmp: PtrInt;
begin
  if OrderedIndexNotSorted then
    OrderedIndexRefresh;
  L := 0;
  R := OrderedIndexCount-1;
  with Owner do
    if (R>=0) and Assigned(GetRecordData) then
    repeat
      result := (L + R) shr 1;
      cmp := SortCompare(GetData(GetRecordData(OrderedIndex[result],DataTemp1),self),Value);
      if cmp=0 then
        exit;
      if cmp<0 then
        L := result + 1 else
        R := result - 1;
    until (L > R);
  result := -1
end;

function TSynTableFieldProperties.OrderedIndexFindAdd(Value: pointer): PtrInt;
var L,R,i: PtrInt;
    cmp: PtrInt;
begin
  if OrderedIndexNotSorted then
    OrderedIndexRefresh;
  R := OrderedIndexCount-1;
  if R<0 then
    result := 0 else
    with Owner do begin
      fOrderedIndexFindAdd := -1;
      L := 0;
      result := -1; // return -1 if found
      repeat
        i := (L + R) shr 1;
        cmp := SortCompare(GetData(GetRecordData(OrderedIndex[i],DataTemp1),self),Value);
        if cmp=0 then
          exit;
        if cmp<0 then
          L := i + 1 else
          R := i - 1;
      until (L > R);
      while (i>=0) and
            (SortCompare(GetData(GetRecordData(OrderedIndex[i],DataTemp1),self),Value)>=0) do
        dec(i);
      result := i+1; // return the index where to insert
    end;
  fOrderedIndexFindAdd := result; // store inserting index for OrderedIndexUpdate
end;

function TSynTableFieldProperties.OrderedIndexMatch(WhereSBFValue: pointer;
  var MatchIndex: TIntegerDynArray; var MatchIndexCount: integer; Limit: Integer=0): Boolean;
var i, L,R: PtrInt;
begin
  result := false;
  if (self=nil) or (WhereSBFValue=nil) or not Assigned(Owner.GetRecordData) or
     (OrderedIndex=nil) or not (tfoIndex in Options) then
    exit;
  i := OrderedIndexFind(WhereSBFValue);
  if i<0 then
    exit; // WHERE value not found
  if (tfoUnique in Options) or (Limit=1) then begin
    // unique index: direct fastest O(log(n)) binary search
    AddSortedInteger(MatchIndex,MatchIndexCount,OrderedIndex[i]);
    // AddSortedInteger() will fail if OrderedIndex[i] already exists
  end else
  with Owner do begin
    // multiple index matches possible: add matching range
    L := i;
    repeat
      dec(L);
    until (L<0) or (SortCompare(GetData(GetRecordData(
      OrderedIndex[L],DataTemp1),self),WhereSBFValue)<>0);
    R := i;
    repeat
      inc(R);
    until (R>=OrderedIndexCount) or
      (SortCompare(GetData(GetRecordData(OrderedIndex[R],DataTemp1),self),WhereSBFValue)<>0);
    if Limit=0 then
      Limit := MaxInt; // no LIMIT set -> retrieve all rows
    for i := L+1 to R-1 do begin
      AddSortedInteger(MatchIndex,MatchIndexCount,OrderedIndex[i]);
      dec(Limit);
      if Limit=0 then
        Break; // reach LIMIT upperbound result count
    end;
  end;
  result := true;
end;

function TSynTableFieldProperties.OrderedIndexUpdate(aOldIndex, aNewIndex: integer;
  aOldRecordData, aNewRecordData: pointer): boolean;
var aOldIndexIndex: integer;
begin
  result := false;
  if (self=nil) or not Assigned(Owner.GetRecordData) then
    exit; // avoid GPF
  // update content
  if aOldIndex<0 then
    if aNewIndex<0 then begin
      // both indexes equal -1 -> force sort
      OrderedIndexSort(0,OrderedIndexCount-1);
      OrderedIndexNotSorted := false;
    end else begin
      // added record
      if tfoUnique in Options then begin
        if fOrderedIndexFindAdd<0 then
          raise ETableDataException.CreateUTF8(
            '%.CheckConstraint call needed before %.OrderedIndexUpdate',[self,Name]);
        OrderedIndexReverseSet(InsertInteger(OrderedIndex,OrderedIndexCount,
          aNewIndex,fOrderedIndexFindAdd));
      end else begin
        AddInteger(OrderedIndex,OrderedIndexCount,aNewIndex);
        OrderedIndexReverseSet(OrderedIndexCount-1);
        OrderedIndexNotSorted := true; // -> OrderedIndexSort() call on purpose
      end;
    end else begin
    // aOldIndex>=0: update a value
    // retrieve position in OrderedIndex[] to be deleted/updated
    if OrderedIndexReverse=nil then
      OrderedIndexReverseSet(0) else // do OrderedIndexReverse[OrderedIndex[i]] := i
      {assert(aOldIndex<length(OrderedIndexReverse))};
    //assert(IntegerScanIndex(Pointer(OrderedIndex),OrderedIndexCount,aOldIndex)=OrderedIndexReverse[aOldIndex]);
    aOldIndexIndex := OrderedIndexReverse[aOldIndex]; // use FAST reverse array
    if aOldIndexIndex<0 then
      exit; // invalid Old index
    if aNewIndex<0 then begin
      // deleted record
      DeleteInteger(OrderedIndex,OrderedIndexCount,aOldIndexIndex);
      Reverse(OrderedIndex,OrderedIndexCount,pointer(OrderedIndexReverse));
      // no need to refresh OrderedIndex[], since data will remain sorted
    end else begin
      // updated record
      OrderedIndex[aOldIndexIndex] := aNewIndex;
      OrderedIndexReverseSet(aOldIndexIndex);
      if (aOldRecordData<>nil) or (aOldIndex<>aNewIndex) then // not in-place update
        with Owner do begin
          if aOldRecordData=nil then
            aOldRecordData := GetRecordData(aOldIndex,DataTemp1);
          if aNewRecordData=nil then
            aNewRecordData := GetRecordData(aNewIndex,DataTemp2);
          if SortCompare(GetData(aOldRecordData,self),GetData(aNewRecordData,self))=0 then begin
            // only sort if field content was modified -> MUCH faster in most case
            result := true;
            exit;
          end;
        end;
      if tfoUnique in Options then begin
        if fOrderedIndexFindAdd>=0 then begin
          // we know which OrderedIndex[] has to be changed -> manual update
          // - this is still a bottleneck in the current implementation, but
          // I was not able to find out how to make it faster, and still
          // being able to check unique field constraints without changing the
          // OrderedIndex[] content from a simple list into e.g. a red-black
          // tree: such a structure performs better, but uses much more memory
          // and is to be implemented
          // - it's still fast, faster than any DB AFAIK, around 500 updates
          // per second with 1,000,000 records on a Core i7
          // - it's still faster to refresh OrderedIndex[] than iterating
          // through all items to validate the unique constraint
          DeleteInteger(OrderedIndex,OrderedIndexCount,aOldIndexIndex);
          if fOrderedIndexFindAdd>aOldIndexIndex then
            dec(fOrderedIndexFindAdd);
          InsertInteger(OrderedIndex,OrderedIndexCount,aNewIndex,fOrderedIndexFindAdd);
          Reverse(OrderedIndex,OrderedIndexCount,pointer(OrderedIndexReverse));
        end else
          // slow full sort - with 1,000,000 items it's about 100 times slower
          // (never called with common usage in SynBigTable unit)
          OrderedIndexSort(0,OrderedIndexCount-1);
      end else
        OrderedIndexNotSorted := true; // will call OrderedIndexSort() on purpose
    end;
  end;
  fOrderedIndexFindAdd := -1; // consume this value
  result := true;
end;

procedure TSynTableFieldProperties.SaveTo(WR: TFileBufferWriter);
begin
  WR.Write(Name);
  WR.Write(@FieldType,SizeOf(FieldType));
  WR.Write(@Options,SizeOf(Options));
  WR.WriteVarUInt32Array(OrderedIndex,OrderedIndexCount,wkVarUInt32);
end;

function TSynTableFieldProperties.SBF(const Value: Int64): TSBFString;
var tmp: array[0..15] of AnsiChar;
begin
  case FieldType of
    tftInt32: begin // special version for handling negative values
      PInteger(@tmp)^ := Value;
      SetString(Result,tmp,SizeOf(Integer));
    end;
    tftUInt8, tftUInt16, tftUInt24, tftInt64:
      SetString(Result,PAnsiChar(@Value),FieldSize);
    tftVarUInt32:
      SetString(Result,tmp,PAnsiChar(ToVarUInt32(Value,@tmp))-tmp);
    tftVarInt32:
      SetString(Result,tmp,PAnsiChar(ToVarInt32(Value,@tmp))-tmp);
    tftVarUInt64:
      SetString(Result,tmp,PAnsiChar(ToVarUInt64(Value,@tmp))-tmp);
    tftVarInt64:
      SetString(Result,tmp,PAnsiChar(ToVarInt64(Value,@tmp))-tmp);
    else
      result := '';
  end;
end;

function TSynTableFieldProperties.SBF(const Value: Integer): TSBFString;
var tmp: array[0..15] of AnsiChar;
begin
  case FieldType of
    tftUInt8, tftUInt16, tftUInt24, tftInt32:
      SetString(Result,PAnsiChar(@Value),FieldSize);
    tftInt64: begin // special version for handling negative values
      PInt64(@tmp)^ := Value;
      SetString(Result,tmp,SizeOf(Int64));
    end;
    tftVarUInt32:
      if Value<0 then // expect an unsigned integer
        result := '' else
        SetString(Result,tmp,PAnsiChar(ToVarUInt32(Value,@tmp))-tmp);
    tftVarInt32:
      SetString(Result,tmp,PAnsiChar(ToVarInt32(Value,@tmp))-tmp);
    tftVarUInt64:
      if cardinal(Value)>cardinal(maxInt) then
        result := '' else // expect a 32 bit integer
        SetString(Result,tmp,PAnsiChar(ToVarUInt64(Value,@tmp))-tmp);
    tftVarInt64:
      SetString(Result,tmp,PAnsiChar(ToVarInt64(Value,@tmp))-tmp);
    else
      result := '';
  end;
end;

const
  SBF_BOOL: array[boolean] of TSBFString =
   (#0,#1);

{$ifndef NOVARIANTS}
function TSynTableFieldProperties.SBF(const Value: Variant): TSBFString;
var V64: Int64;
    VC: Currency absolute V64;
    VD: Double absolute V64;
begin // VarIsOrdinal/VarIsFloat/VarIsStr are buggy -> use field type
  case FieldType of
    tftBoolean:
      result := SBF_BOOL[boolean(Value)];
    tftUInt8, tftUInt16, tftUInt24, tftInt32, tftInt64,
    tftVarUInt32, tftVarInt32, tftVarUInt64, tftVarInt64: begin
      if not VariantToInt64(Value,V64) then
        V64 := 0;
      result := SBF(V64);
    end;
    tftCurrency: begin
      VC := Value;
      SetString(result,PAnsiChar(@VC),SizeOf(VC));
    end;
    tftDouble: begin
      VD := Value;
      SetString(result,PAnsiChar(@VD),SizeOf(VD));
    end;
    tftWinAnsi:
      ToSBFStr(WinAnsiConvert.UTF8ToAnsi(VariantToUTF8(Value)),result);
    tftUTF8:
      ToSBFStr(VariantToUTF8(Value),result);
    else
      result := '';
  end;
  if result='' then
    result := SBFDefault;
end;
{$endif}

function TSynTableFieldProperties.SBF(const Value: Boolean): TSBFString;
begin
  if FieldType<>tftBoolean then
    result := '' else
    result := SBF_BOOL[Value];
end;

function TSynTableFieldProperties.SBFCurr(const Value: Currency): TSBFString;
begin
  if FieldType<>tftCurrency then
    result := '' else
    SetString(Result,PAnsiChar(@Value),SizeOf(Value));
end;

procedure ToSBFStr(const Value: RawByteString; out Result: TSBFString);
var tmp: array[0..15] of AnsiChar;
    Len, Head: integer;
begin
  if PtrUInt(Value)=0 then
    Result := #0 else begin
    Len := {$ifdef FPC}length(Value){$else}PInteger(PtrUInt(Value)-SizeOf(integer))^{$endif};
    Head := PAnsiChar(ToVarUInt32(Len,@tmp))-tmp;
    SetLength(Result,Len+Head);
    MoveFast(tmp,PByteArray(Result)[0],Head);
    MoveFast(pointer(Value)^,PByteArray(Result)[Head],Len);
  end;
end;

function TSynTableFieldProperties.SBF(const Value: RawUTF8): TSBFString;
begin
  case FieldType of
    tftUTF8:
      ToSBFStr(Value,Result);
    tftWinAnsi:
      ToSBFStr(Utf8ToWinAnsi(Value),Result);
    else
      result := '';
  end;
end;

function TSynTableFieldProperties.SBF(Value: pointer; ValueLen: integer): TSBFString;
var tmp: array[0..15] of AnsiChar;
    Head: integer;
begin
  if FieldType<>tftBlobInternal then
    result := '' else
  if (Value=nil) or (ValueLen=0) then
    result := #0 else begin // inlined ToSBFStr() code
    Head := PAnsiChar(ToVarUInt32(ValueLen,@tmp))-tmp;
    SetString(Result,nil,ValueLen+Head);
    MoveFast(tmp,PByteArray(Result)[0],Head);
    MoveFast(Value^,PByteArray(Result)[Head],ValueLen);
  end;
end;

function TSynTableFieldProperties.SBFFloat(const Value: Double): TSBFString;
begin
  if FieldType<>tftDouble then
    result := '' else
    SetString(Result,PAnsiChar(@Value),SizeOf(Value));
end;

function TSynTableFieldProperties.SBFFromRawUTF8(const aValue: RawUTF8): TSBFString;
var Curr: Currency;
begin
  case FieldType of
  tftBoolean:
    if (SynCommons.GetInteger(pointer(aValue))<>0) or IdemPropNameU(aValue,'true') then
      result := #1 else
      result := #0; // store false by default
  tftUInt8, tftUInt16, tftUInt24, tftInt32, tftVarInt32:
    result := SBF(SynCommons.GetInteger(pointer(aValue)));
  tftVarUInt32, tftInt64, tftVarUInt64, tftVarInt64:
    result := SBF(SynCommons.GetInt64(pointer(aValue)));
  tftCurrency: begin
    PInt64(@Curr)^ := StrToCurr64(pointer(aValue));
    result := SBFCurr(Curr);
  end;
  tftDouble:
    result := SBFFloat(GetExtended(pointer(aValue)));
  // text storage - WinAnsi could use less space than UTF-8
  tftUTF8, tftWinAnsi:
    result := SBF(aValue);
  else
    result := ''; // tftBlob* fields e.g. must be handled directly
  end;
end;

function TSynTableFieldProperties.GetInteger(RecordBuffer: pointer): Integer;
begin
  if (self=nil) or (RecordBuffer=nil) or (Owner=nil) then
    result := 0 else begin
    RecordBuffer := Owner.GetData(RecordBuffer,self);
    case FieldType of
    tftBoolean, tftUInt8:
      result := PByte(RecordBuffer)^;
    tftUInt16:
      result := PWord(RecordBuffer)^;
    tftUInt24:
      // PInteger()^ and $ffffff -> possible GPF on Memory Mapped file
      result := PWord(RecordBuffer)^+integer(PByteArray(RecordBuffer)^[2])shl 16;
    tftInt32:
      result := PInteger(RecordBuffer)^;
    tftInt64:
      result := PInt64(RecordBuffer)^;
    // some variable-size field value
    tftVarUInt32:
      result := FromVarUInt32(PByte(RecordBuffer));
    tftVarInt32:
      result := FromVarInt32(PByte(RecordBuffer));
    tftVarUInt64:
      result := FromVarUInt64(PByte(RecordBuffer));
    tftVarInt64:
      result := FromVarInt64(PByte(RecordBuffer));
    else
      result := 0;
    end;
  end;
end;

function TSynTableFieldProperties.GetInt64(RecordBuffer: pointer): Int64;
var PB: PByte;
begin
  if (self=nil) or (RecordBuffer=nil) or (Owner=nil) then
    result := 0 else begin
    PB := Owner.GetData(RecordBuffer,self);
    case FieldType of
    tftInt64:
      result := PInt64(PB)^;
    tftVarUInt64:
      result := FromVarUInt64(PB);
    tftVarInt64:
      result := FromVarInt64(PB);
    else
      result := GetInteger(RecordBuffer);
    end;
  end;
end;

function TSynTableFieldProperties.GetBoolean(RecordBuffer: pointer): Boolean;
begin
  result := boolean(GetInteger(RecordBuffer));
end;

function TSynTableFieldProperties.GetCurrency(RecordBuffer: pointer): Currency;
begin
  if (self=nil) or (RecordBuffer=nil) or (Owner=nil) then
    result := 0 else
    case FieldType of
    tftCurrency:
      result := PCurrency(Owner.GetData(RecordBuffer,self))^;
    else
      result := GetInt64(RecordBuffer);
    end;
end;

function TSynTableFieldProperties.GetDouble(RecordBuffer: pointer): Double;
begin
  if (self=nil) or (RecordBuffer=nil) or (Owner=nil) then
    result := 0 else
    case FieldType of
    tftDouble:
      result := unaligned(PDouble(Owner.GetData(RecordBuffer,self))^);
    else
      result := GetInt64(RecordBuffer);
    end;
end;

function TSynTableFieldProperties.GetRawUTF8(RecordBuffer: pointer): RawUTF8;
begin
  if (self=nil) or (RecordBuffer=nil) or (Owner=nil) then
    result := '' else begin
    RecordBuffer := Owner.GetData(RecordBuffer,self);
    if RecordBuffer<>nil then
      result := GetValue(RecordBuffer) else // will do conversion to text
      result := '';
  end;
end;

function TSynTableFieldProperties.AddFilterOrValidate(aFilter: TSynFilterOrValidate): TSynFilterOrValidate;
  procedure Add(var List: TSynObjectList);
  begin
    if List=nil then
      List := TSynObjectList.Create;
    List.Add(result);
  end;
begin
  result := aFilter;
  if (self=nil) or (result=nil) then
    result := nil else
  if aFilter.InheritsFrom(TSynFilter) then
    Add(Filters) else
  if aFilter.InheritsFrom(TSynValidate) then
    Add(Validates) else
    result := nil;
end;

function TSynTableFieldProperties.Validate(RecordBuffer: pointer;
  RecordIndex: integer): string;
var i: integer;
    Value: RawUTF8;
    aValidate: TSynValidate;
    aValidateTable: TSynValidateTable absolute aValidate;
begin
  result := '';
  if (self=nil) or (Validates=nil) then
    exit;
  Value := GetRawUTF8(RecordBuffer); // TSynTableValidate needs RawUTF8 text
  for i := 0 to Validates.Count-1 do begin
    aValidate := Validates.List[i];
    if aValidate.InheritsFrom(TSynValidateTable) then begin
      aValidateTable.ProcessField := self;
      aValidateTable.ProcessRecordIndex := RecordIndex;
    end;
    if not aValidate.Process(FieldNumber,Value,result) then begin
      if result='' then
        // no custom message -> show a default message
        result := format(sValidationFailed,[
          GetCaptionFromClass(aValidate.ClassType)]);
      break;
    end;
  end;
end;

{$ifdef SORTCOMPAREMETHOD}
function TSynTableFieldProperties.SortCompare(P1, P2: PUTF8Char): PtrInt;
var i, L: integer;
label minus,plus,zer;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
      case FieldType of
      tftBoolean, tftUInt8:
        result := PByte(P1)^-PByte(P2)^;
      tftUInt16:
        result := PWord(P1)^-PWord(P2)^;
      tftUInt24:
        result := PtrInt(PWord(P1)^)+PtrInt(P1[2])shl 16
          -PtrInt(PWord(P2)^)-PtrInt(P2[2]) shl 16;
      tftInt32:
        result := PInteger(P1)^-PInteger(P2)^;
      tftDouble: begin
        unaligned(PDouble(@SortCompareTmp)^) := unaligned(PDouble(P1)^)-unaligned(PDouble(P2)^);
        if unaligned(PDouble(@SortCompareTmp)^)<0 then
          goto minus else
        if unaligned(PDouble(@SortCompareTmp)^)>0 then
          goto plus else
          goto zer;
      end;
      tftVarUInt32:
      with SortCompareTmp do begin
        PB1 := Pointer(P1);
        PB2 := Pointer(P2);
        result := FromVarUInt32(PB1)-FromVarUInt32(PB2);
      end;
      tftVarInt32:
      with SortCompareTmp do begin
        PB1 := Pointer(P1);
        PB2 := Pointer(P2);
        result := FromVarInt32(PB1)-FromVarInt32(PB2);
      end;
      {$ifdef CPUX86} // circumvent comparison slowness (and QWord bug)
      tftInt64, tftCurrency:
        result := SortInt64(PInt64(P1)^,PInt64(P2)^);
      tftVarUInt64:
      with SortCompareTmp do begin
        PB1 := Pointer(P1);
        PB2 := Pointer(P2);
        result := SortQWord(FromVarUInt64(PB1),FromVarUInt64(PB2));
      end;
      tftVarInt64:
      with SortCompareTmp do begin
        PB1 := Pointer(P1);
        PB2 := Pointer(P2);
        result := SortInt64(FromVarInt64(PB1),FromVarInt64(PB2));
      end;
      {$else}
      {$ifdef CPU64} // PtrInt = Int64
      tftInt64, tftCurrency:
        result := PInt64(P1)^-PInt64(P2)^;
      tftVarUInt64:
      with SortCompareTmp do begin
        PB1 := Pointer(P1);
        PB2 := Pointer(P2);
        result := FromVarUInt64(PB1)-FromVarUInt64(PB2);
      end;
      tftVarInt64:
      with SortCompareTmp do begin
        PB1 := Pointer(P1);
        PB2 := Pointer(P2);
        result := FromVarInt64(PB1)-FromVarInt64(PB2);
      end;
      {$else}
      tftInt64, tftCurrency: begin
        PInt64(@SortCompareTmp)^ := PInt64(P1)^-PInt64(P2)^;
        if PInt64(@SortCompareTmp)^<0 then
          goto minus else
        if PInt64(@SortCompareTmp)^>0 then
          goto plus else
          goto zer;
      end;
      tftVarUInt64:
      with SortCompareTmp do begin
        PB1 := Pointer(P1);
        PB2 := Pointer(P2);
        PInt64(@SortCompareTmp)^ := FromVarUInt64(PB1)-FromVarUInt64(PB2);
        if PInt64(@SortCompareTmp)^<0 then
          goto minus else
        if PInt64(@SortCompareTmp)^>0 then
          goto plus else
          goto zer;
      end;
      tftVarInt64:
      with SortCompareTmp do begin
        PB1 := Pointer(P1);
        PB2 := Pointer(P2);
        PInt64(@SortCompareTmp)^ := FromVarInt64(PB1)-FromVarInt64(PB2);
        if PInt64(@SortCompareTmp)^<0 then
          goto minus else
        if PInt64(@SortCompareTmp)^>0 then
          goto plus else
          goto zer;
      end;
      {$endif}
      {$endif}
      tftWinAnsi, tftUTF8, tftBlobInternal:
      begin
        with SortCompareTmp do begin
          if PtrInt(P1^)<=$7F then begin
            L1 := PtrInt(P1^);
            inc(P1);
          end else begin
            PB1 := pointer(P1);
            L1 := FromVarUInt32High(PB1);
            P1 := pointer(PB1);
          end;
          if PtrInt(P2^)<=$7F then begin
            L2 := PtrInt(P2^);
            inc(P2);
          end else begin
            PB2 := pointer(P2);
            L2 := FromVarUInt32High(PB2);
            P2 := pointer(PB2);
          end;
        end;
        with SortCompareTmp do begin
          L := L1;
          if L2>L then
            L := L2;
        end;
        if tfoCaseInsensitive in Options then begin
          i := 0;
          while i<L do begin
            result := PtrInt(NormToUpperAnsi7[P1[i]])-PtrInt(NormToUpperAnsi7[P2[i]]);
            if result<>0 then
              exit else
              inc(i);
          end;
        end else begin
          i := 0;
          while i<L do begin
            result := PtrInt(P1[i])-PtrInt(P2[i]);
            if result<>0 then
              exit else
              inc(i);
          end;
        end;
        with SortCompareTmp do
          result := L1-L2;
      end;
      else
        goto zer;
      end else
plus:   result := 1 else  // P2=nil
minus:result := -1 else // P1=nil
zer:result := 0;      // P1=P2
end;
{$endif}

function CompareOperator(FieldType: TSynTableFieldType; SBF, SBFEnd: PUTF8Char;
  Value: Int64; Oper: TCompareOperator): boolean;
var V: Int64;
    PB: PByte absolute SBF;
begin
  result := true;
  if PB<>nil then
  repeat
    case FieldType of
      tftBoolean, tftUInt8:
        V := PB^;
      tftUInt16:
        V := PWord(PB)^;
      tftUInt24:
        // PInteger()^ and $ffffff -> possible GPF on Memory Mapped file
        V := PWord(PB)^+integer(PByteArray(PB)^[2])shl 16;
      tftInt32:
        V := PInteger(PB)^;
      tftInt64:
        V := PInt64(PB)^;
      // some variable-size field value
      tftVarUInt32:
        V := FromVarUInt32(PB);
      tftVarInt32:
        V := FromVarInt32(PB);
      tftVarUInt64:
        V := FromVarUInt64(PB);
      tftVarInt64:
        V := FromVarInt64(PB);
      else V := 0;  // makes compiler happy
    end;
    case Oper of
      soEqualTo:              if V=Value then exit;
      soNotEqualTo:           if V<>Value then exit;
      soLessThan:             if V<Value then exit;
      soLessThanOrEqualTo:    if V<=Value then exit;
      soGreaterThan:          if V>Value then exit;
      soGreaterThanOrEqualTo: if V>=Value then exit;
      else break;
    end;
    // not found: go to next value
    if SBFEnd=nil then
      break; // only one value to be checked
    if FIELD_FIXEDSIZE[FieldType]>0 then
      inc(SBF,FIELD_FIXEDSIZE[FieldType]); // FromVar*() already updated PB/SBF
  until SBF>=SBFEnd;
  result := false; // not found
end;

function CompareOperator(SBF, SBFEnd: PUTF8Char;
  Value: double; Oper: TCompareOperator): boolean;
begin
  result := true;
  if SBF<>nil then
  repeat
    case Oper of
      soEqualTo:              if unaligned(PDouble(SBF)^)=Value then exit;
      soNotEqualTo:           if unaligned(PDouble(SBF)^)<>Value then exit;
      soLessThan:             if unaligned(PDouble(SBF)^)<Value then exit;
      soLessThanOrEqualTo:    if unaligned(PDouble(SBF)^)<=Value then exit;
      soGreaterThan:          if unaligned(PDouble(SBF)^)>Value then exit;
      soGreaterThanOrEqualTo: if unaligned(PDouble(SBF)^)>=Value then exit;
      else break;
    end;
    // not found: go to next value
    if SBFEnd=nil then
      break; // only one value to be checked
    inc(SBF,SizeOf(Value));
  until SBF>=SBFEnd;
  result := false; // not found
end;

function CompareOperator(FieldType: TSynTableFieldType; SBF, SBFEnd: PUTF8Char;
  Value: PUTF8Char; ValueLen: integer; Oper: TCompareOperator;
  CaseSensitive: boolean): boolean; overload;
var L, Cmp: PtrInt;
    PB: PByte;
    tmp: array[byte] of AnsiChar;
begin
  result := true;
  if SBF<>nil then
  repeat
    // get length of text in the SBF encoded buffer
    if integer(SBF^)<=$7f then begin
      L := integer(SBF^);
      inc(SBF);
    end else begin
      PB := Pointer(SBF);
      L := FromVarUInt32(PB);
      SBF := pointer(PB);
    end;
    // perform comparison: returns nil on match
    case Oper of
      soEqualTo..soGreaterThanOrEqualTo: begin
        Cmp := L-ValueLen;
        if Cmp<0 then
          L := ValueLen;
        if CaseSensitive then
          Cmp := StrCompL(SBF,Value,L,Cmp) else
          Cmp := StrCompIL(SBF,Value,L,Cmp);
        case Oper of
          soEqualTo:              if Cmp=0 then exit;
          soNotEqualTo:           if Cmp<>0 then exit;
          soLessThan:             if Cmp<0 then exit;
          soLessThanOrEqualTo:    if Cmp<=0 then exit;
          soGreaterThan:          if Cmp>0 then exit;
          soGreaterThanOrEqualTo: if Cmp>=0 then exit;
        end;
      end;
      soBeginWith:
        if ValueLen>=L then
          if CaseSensitive then begin
            if StrCompL(SBF,Value,ValueLen,0)=0 then
              exit;
          end else
            if StrCompIL(SBF,Value,ValueLen,0)=0 then
              exit;
      soContains: begin
        dec(L,ValueLen);
        while L>=0 do begin
          while (L>=0) and not(tcWord in TEXT_CHARS[SBF^]) do begin
            dec(L);
            inc(SBF);
          end; // begin of next word reached
          if L<0 then
            Break; // not enough chars to contain the Value
          if CaseSensitive then begin
            if StrCompL(SBF,Value,ValueLen,0)=0 then
              exit;
          end else
            if StrCompIL(SBF,Value,ValueLen,0)=0 then
              exit;
          while (L>=0) and (tcWord in TEXT_CHARS[SBF^]) do begin
            dec(L);
            inc(SBF);
          end; // end of word reached
        end;
        if SBFEnd=nil then
          break; // only one value to be checked
        inc(SBF,ValueLen); // custom inc(SBF,L);
        if SBF<SBFEnd then
          continue else break;
      end;
      soSoundsLikeEnglish,
      soSoundsLikeFrench,
      soSoundsLikeSpanish: begin
        if L>high(tmp) then
          Cmp := high(tmp) else
          Cmp := L;
        tmp[Cmp] := #0; // TSynSoundEx expect the buffer to be #0 terminated
        MoveFast(SBF^,tmp,Cmp);
        case FieldType of
        tftWinAnsi:
          if PSynSoundEx(Value)^.Ansi(tmp) then
            exit;
        tftUTF8:
          if PSynSoundEx(Value)^.UTF8(tmp) then
            exit;
        else break;
        end;
      end;
      else break;
    end;
    // no match -> go to the end of the SBF buffer
    if SBFEnd=nil then
      exit; // only one value to be checked
    inc(SBF,L);
    if SBF>=SBFEnd then
      break;
  until false;
end;


{ TSynValidateTableUniqueField }

function TSynValidateTableUniqueField.Process(aFieldIndex: integer;
  const Value: RawUTF8; var ErrorMsg: string): boolean;
var S: TSBFString;
begin
  result := false;
  if (self=nil) or (Value='') or (ProcessField=nil) then
    exit; // void field can't be unique
  if not (tfoIndex in ProcessField.Options) then
    exit; // index should be always created by TSynTable.AfterFieldModif
  S := ProcessField.SBFFromRawUTF8(Value);
  if S='' then
    exit; // void field can't be unique
  if ProcessField.OrderedIndexFindAdd(Pointer(S))>=0 then
    // there is some place to insert the Value -> not existing yet -> OK
    result := true else begin
    // RecordIndex=-1 in case of adding, or the physical index of the updated record
    if (ProcessRecordIndex>=0) and
       (ProcessField.OrderedIndex[ProcessField.OrderedIndexFind(Pointer(S))]=
         ProcessRecordIndex) then
      // allow update of the record
      result := true else
      // found a dupplicated value
      ErrorMsg := sValidationFieldDuplicate;
  end;
end;


{ TSynTableStatement }

const
  NULL_UPP  = ord('N')+ord('U')shl 8+ord('L')shl 16+ord('L')shl 24;

constructor TSynTableStatement.Create(const SQL: RawUTF8;
  GetFieldIndex: TSynTableFieldIndex; SimpleFieldsBits: TSQLFieldBits;
  FieldProp: TSynTableFieldProperties);
var Prop, whereBefore: RawUTF8;
    P, B: PUTF8Char;
    ndx,err,len,selectCount,whereCount: integer;
    whereWithOR,whereNotClause: boolean;

function GetPropIndex: integer;
begin
  if not GetNextFieldProp(P,Prop) then
    result := -1 else
    if IsRowID(pointer(Prop)) then
      result := 0 else begin // 0 = ID field
      result := GetFieldIndex(Prop);
      if result>=0 then // -1 = no valid field name
        inc(result);  // otherwise: PropertyIndex+1
    end;
end;
function SetFields: boolean;
var select: TSynTableStatementSelect;
    B: PUTF8Char;
begin
  result := false;
  FillcharFast(select,SizeOf(select),0);
  select.Field := GetPropIndex; // 0 = ID, otherwise PropertyIndex+1
  if select.Field<0 then begin
    if P^<>'(' then // Field not found -> try function(field)
      exit;
    P := GotoNextNotSpace(P+1);
    select.FunctionName := Prop;
    inc(fSelectFunctionCount);
    if IdemPropNameU(Prop,'COUNT') and (P^='*') then begin
      select.Field := 0; // count( * ) -> count(ID)
      select.FunctionKnown := funcCountStar;
      P := GotoNextNotSpace(P+1);
    end else begin
      if IdemPropNameU(Prop,'DISTINCT') then
        select.FunctionKnown := funcDistinct else
      if IdemPropNameU(Prop,'MAX') then
        select.FunctionKnown := funcMax;
      select.Field := GetPropIndex;
      if select.Field<0 then
        exit;
    end;
    if P^<>')' then
      exit;
    P := GotoNextNotSpace(P+1);
  end else
    if P^='.' then begin // MongoDB-like field.subfield1.subfield2
      B := P;
      repeat
        inc(P);
      until not (jcJsonIdentifier in JSON_CHARS[P^]);
      FastSetString(select.SubField,B,P-B);
      fHasSelectSubFields := true;
    end;
  if P^ in ['+','-'] then begin
    select.ToBeAdded := GetNextItemInteger(P,' ');
    if select.ToBeAdded=0 then
      exit;
    P := GotoNextNotSpace(P);
  end;
  if IdemPChar(P,'AS ') then begin
    inc(P,3);
    if not GetNextFieldProp(P,select.Alias) then
      exit;
  end;
  SetLength(fSelect,selectCount+1);
  fSelect[selectCount] := select;
  inc(selectCount);
  result := true;
end;
function GetWhereValue(var Where: TSynTableStatementWhere): boolean;
var B: PUTF8Char;
begin
  result := false;
  P := GotoNextNotSpace(P);
  Where.ValueSQL := P;
  if PWord(P)^=ord(':')+ord('(') shl 8 then
    inc(P,2); // ignore :(...): parameter (no prepared statements here)
  if P^ in ['''','"'] then begin
    // SQL String statement
    P := UnQuoteSQLStringVar(P,Where.Value);
    if P=nil then
      exit; // end of string before end quote -> incorrect
    {$ifndef NOVARIANTS}
    RawUTF8ToVariant(Where.Value,Where.ValueVariant);
    {$endif}
    if FieldProp<>nil then
      // create a SBF formatted version of the WHERE value
      Where.ValueSBF := FieldProp.SBFFromRawUTF8(Where.Value);
  end else
  if (PInteger(P)^ and $DFDFDFDF=NULL_UPP) and (P[4] in [#0..' ',';']) then begin
    // NULL statement
    Where.Value := NULL_STR_VAR; // not void
    {$ifndef NOVARIANTS}
    SetVariantNull(Where.ValueVariant);
    {$endif}
    inc(P,4);
  end else begin
    // numeric statement or 'true' or 'false' (OK for NormalizeValue)
    B := P;
    repeat
      inc(P);
    until P^ in [#0..' ',';',')',','];
    SetString(Where.Value,B,P-B);
    {$ifndef NOVARIANTS}
    Where.ValueVariant := VariantLoadJSON(Where.Value);
    {$endif}
    Where.ValueInteger := GetInteger(pointer(Where.Value),err);
    if FieldProp<>nil then
      if Where.Value<>'?' then
        if (FieldProp.FieldType in FIELD_INTEGER) and (err<>0) then
          // we expect a true INTEGER value here
          Where.Value := '' else
          // create a SBF formatted version of the WHERE value
          Where.ValueSBF := FieldProp.SBFFromRawUTF8(Where.Value);
  end;
  if PWord(P)^=ord(')')+ord(':')shl 8 then
    inc(P,2); // ignore :(...): parameter
  Where.ValueSQLLen := P-Where.ValueSQL;
  P := GotoNextNotSpace(P);
  if (P^=')') and (Where.FunctionName='') then begin
    B := P;
    repeat
      inc(P);
    until not (P^ in [#1..' ',')']);
    while P[-1]=' ' do dec(P); // trim right space
    SetString(Where.ParenthesisAfter,B,P-B);
    P := GotoNextNotSpace(P);
  end;
  result := true;
end;
{$ifndef NOVARIANTS}
function GetWhereValues(var Where: TSynTableStatementWhere): boolean;
var v: TSynTableStatementWhereDynArray;
    n, w: integer;
    tmp: RawUTF8;
begin
  result := false;
  if Where.ValueSQLLen<=2 then
    exit;
  SetString(tmp,PAnsiChar(Where.ValueSQL)+1,Where.ValueSQLLen-2);
  P := pointer(tmp); // parse again the IN (...,...,... ) expression
  n := 0;
  try
    repeat
      if n=length(v) then
        SetLength(v,NextGrow(n));
      if not GetWhereValue(v[n]) then
        exit;
      inc(n);
      if P^=#0 then
        break;
      if P^<>',' then
        exit;
      inc(P);
    until false;
  finally
    P := Where.ValueSQL+Where.ValueSQLLen; // continue parsing as usual
  end;
  with TDocVariantData(Where.ValueVariant) do begin
    InitFast(n,dvArray);
    for w := 0 to n-1 do
      AddItem(v[w].ValueVariant);
    Where.Value := ToJSON;
  end;
  result := true;
end;
{$endif}
function GetWhereExpression(FieldIndex: integer; var Where: TSynTableStatementWhere): boolean;
var B: PUTF8Char;
begin
  result := false;
  Where.ParenthesisBefore := whereBefore;
  Where.JoinedOR := whereWithOR;
  Where.NotClause := whereNotClause;
  Where.Field := FieldIndex; // 0 = ID, otherwise PropertyIndex+1
  if P^='.' then begin // MongoDB-like field.subfield1.subfield2
    B := P;
    repeat
      inc(P);
    until not (jcJsonIdentifier in JSON_CHARS[P^]);
    FastSetString(Where.SubField,B,P-B);
    fWhereHasSubFields := true;
    P := GotoNextNotSpace(P);
  end;
  case P^ of
  '=': Where.Operator := opEqualTo;
  '>': if P[1]='=' then begin
         inc(P);
         Where.Operator := opGreaterThanOrEqualTo;
       end else
         Where.Operator := opGreaterThan;
  '<': case P[1] of
       '=': begin
         inc(P);
         Where.Operator := opLessThanOrEqualTo;
       end;
       '>': begin
         inc(P);
         Where.Operator := opNotEqualTo;
       end;
       else
         Where.Operator := opLessThan;
       end;
  'i','I':
    case P[1] of
    's','S': begin
      P := GotoNextNotSpace(P+2);
      if IdemPChar(P,'NULL') then begin
        Where.Value := NULL_STR_VAR;
        Where.Operator := opIsNull;
        Where.ValueSQL := P;
        Where.ValueSQLLen := 4;
        {$ifndef NOVARIANTS}
        TVarData(Where.ValueVariant).VType := varNull;
        {$endif}
        inc(P,4);
        result := true;
      end else
      if IdemPChar(P,'NOT NULL') then begin
        Where.Value := 'not null';
        Where.Operator := opIsNotNull;
        Where.ValueSQL := P;
        Where.ValueSQLLen := 8;
        {$ifndef NOVARIANTS}
        TVarData(Where.ValueVariant).VType := varNull;
        {$endif}
        inc(P,8);
        result := true; // leave ValueVariant=unassigned
      end;
      exit;
    end;
    {$ifndef NOVARIANTS}
    'n','N': begin
       Where.Operator := opIn;
       P := GotoNextNotSpace(P+2);
       if P^<>'(' then
         exit; // incorrect SQL statement
       B := P; // get the IN() clause as JSON
       inc(P);
       while (P^<>')') or (P[1]=':') do // handle :(...): within the clause
         if P^=#0 then
           exit else
           inc(P);
       inc(P);
       SetString(Where.Value,PAnsiChar(B),P-B);
       Where.ValueSQL := B;
       Where.ValueSQLLen := P-B;
       result := GetWhereValues(Where);
       exit;
    end;
    {$endif}
    end; // 'i','I':
  'l','L':
    if IdemPChar(P+1,'IKE') then begin
      inc(P,3);
      Where.Operator := opLike;
    end else
    exit;
  else exit; // unknown operator
  end;
  // we got 'WHERE FieldName operator ' -> handle value
  inc(P);
  result := GetWhereValue(Where);
end;

label lim,lim2;
begin
  P := pointer(SQL);
  if (P=nil) or (self=nil) then
    exit; // avoid GPF
  P := GotoNextNotSpace(P); // trim left
  if not IdemPChar(P,'SELECT ') then
    exit else // handle only SELECT statement
    inc(P,7);
  // 1. get SELECT clause: set bits in Fields from CSV field IDs in SQL
  selectCount := 0;
  P := GotoNextNotSpace(P); // trim left
  if P^=#0 then
    exit; // no SQL statement
  if P^='*' then begin // all simple (not TSQLRawBlob/TSQLRecordMany) fields
    inc(P);
    len := GetBitsCount(SimpleFieldsBits,MAX_SQLFIELDS)+1;
    SetLength(fSelect,len);
    selectCount := 1; // Select[0].Field := 0 -> ID
    for ndx := 0 to MAX_SQLFIELDS-1 do
      if ndx in SimpleFieldsBits then begin
        fSelect[selectCount].Field := ndx+1;
        inc(selectCount);
        if selectCount=len then
          break;
      end;
    GetNextFieldProp(P,Prop);
  end else
  if not SetFields then
    exit else // we need at least one field name
    if P^<>',' then
      GetNextFieldProp(P,Prop) else
      repeat
        while P^ in [',',#1..' '] do inc(P); // trim left
      until not SetFields; // add other CSV field names
  // 2. get FROM clause
  if not IdemPropNameU(Prop,'FROM') then exit; // incorrect SQL statement
  GetNextFieldProp(P,Prop);
  fTableName := Prop;
  // 3. get WHERE clause
  whereCount := 0;
  whereWithOR := false;
  whereNotClause := false;
  whereBefore := '';
  GetNextFieldProp(P,Prop);
  if IdemPropNameU(Prop,'WHERE') then begin
    repeat
      B := P;
      if P^='(' then begin
        fWhereHasParenthesis := true;
        repeat
          inc(P);
        until not (P^ in [#1..' ','(']);
        while P[-1]=' ' do dec(P); // trim right space
        SetString(whereBefore,B,P-B);
        B := P;
      end;
      ndx := GetPropIndex;
      if ndx<0 then begin
        if IdemPropNameU(Prop,'NOT') then begin
          whereNotClause := true;
          continue;
        end;
        if P^='(' then begin
          inc(P);
          SetLength(fWhere,whereCount+1);
          with fWhere[whereCount] do begin
            ParenthesisBefore := whereBefore;
            JoinedOR := whereWithOR;
            NotClause := whereNotClause;
            FunctionName := UpperCase(Prop);
            // Byte/Word/Integer/Cardinal/Int64/CurrencyDynArrayContains(BlobField,I64)
            len := length(Prop);
            if (len>16) and
               IdemPropName('DynArrayContains',PUTF8Char(@PByteArray(Prop)[len-16]),16) then
              Operator := opContains else
              Operator := opFunction;
            B := P;
            Field := GetPropIndex;
            if Field<0 then
              P := B else
              if P^<>',' then
                break else
                P := GotoNextNotSpace(P+1);
            if (P^=')') or
               (GetWhereValue(fWhere[whereCount]) and (P^=')')) then begin
              inc(P);
              break;
            end;
          end;
        end;
        P := B;
        break;
      end;
      SetLength(fWhere,whereCount+1);
      if not GetWhereExpression(ndx,fWhere[whereCount]) then
        exit; // invalid SQL statement
      inc(whereCount);
      GetNextFieldProp(P,Prop);
      if IdemPropNameU(Prop,'OR') then
        whereWithOR := true else
      if IdemPropNameU(Prop,'AND') then
        whereWithOR := false else
        goto lim2;
      whereNotClause := false;
      whereBefore := '';
    until false;
    // 4. get optional LIMIT/OFFSET/ORDER clause
lim:P := GotoNextNotSpace(P);
    while (P<>nil) and not(P^ in [#0,';']) do begin
      GetNextFieldProp(P,Prop);
lim2: if IdemPropNameU(Prop,'LIMIT') then
        fLimit := GetNextItemCardinal(P,' ') else
      if IdemPropNameU(Prop,'OFFSET') then
        fOffset := GetNextItemCardinal(P,' ') else
      if IdemPropNameU(Prop,'ORDER') then begin
        GetNextFieldProp(P,Prop);
        if IdemPropNameU(Prop,'BY') then begin
          repeat
            ndx := GetPropIndex; // 0 = ID, otherwise PropertyIndex+1
            if ndx<0 then
              exit; // incorrect SQL statement
            AddFieldIndex(fOrderByField,ndx);
            if P^<>',' then begin // check ORDER BY ... ASC/DESC
              B := P;
              if GetNextFieldProp(P,Prop) then
                if IdemPropNameU(Prop,'DESC') then
                  fOrderByDesc := true else
                if not IdemPropNameU(Prop,'ASC') then
                  P := B;
              break;
            end;
            P := GotoNextNotSpace(P+1);
          until P^ in [#0,';'];
        end else
        exit; // incorrect SQL statement
      end else
      if IdemPropNameU(Prop,'GROUP') then begin
        GetNextFieldProp(P,Prop);
        if IdemPropNameU(Prop,'BY') then begin
          repeat
            ndx := GetPropIndex; // 0 = ID, otherwise PropertyIndex+1
            if ndx<0 then
              exit; // incorrect SQL statement
            AddFieldIndex(fGroupByField,ndx);
            if P^<>',' then
              break;
            P := GotoNextNotSpace(P+1);
          until P^ in [#0,';'];
        end else
        exit; // incorrect SQL statement
      end else
      if (Prop<>'') or not(GotoNextNotSpace(P)^ in [#0, ';']) then
        exit else // incorrect SQL statement
        break; // reached the end of the statement
    end;
  end else
  if Prop<>'' then
    goto lim2; // handle LIMIT OFFSET ORDER
  fSQLStatement := SQL; // make a private copy e.g. for Where[].ValueSQL
end;

procedure TSynTableStatement.SelectFieldBits(var Fields: TSQLFieldBits;
  var withID: boolean; SubFields: PRawUTF8Array);
var i: integer;
    f: ^TSynTableStatementSelect;
begin
  FillcharFast(Fields,SizeOf(Fields),0);
  withID := false;
  f := pointer(Select);
  for i := 1 to Length(Select) do begin
    if f^.Field=0 then
      withID := true else
      include(Fields,f^.Field-1);
    if (SubFields<>nil) and fHasSelectSubFields then
      SubFields^[f^.Field] := f^.SubField;
    inc(f);
  end;
end;


{$ifndef DELPHI5OROLDER}

{ TSynTableData }

procedure TSynTableData.CheckVTableInitialized;
begin
  if VTable=nil then
    raise ETableDataException.Create('TSynTableData non initialized');
end;

{$ifndef NOVARIANTS}

function TSynTableData.GetField(const FieldName: RawUTF8): Variant;
begin
  GetFieldVariant(FieldName,result);
end;

function TSynTableData.GetFieldVarData(FieldName: PUTF8Char; FieldNameLen: PtrInt;
  var Value: TVarData): boolean;
var aField: TSynTableFieldProperties;
begin
  if IsRowID(FieldName,FieldNameLen) then begin
    PVariant(@Value)^ := VID;
    result := true;
  end else begin
    CheckVTableInitialized;
    aField := VTable.GetFieldFromNameLen(FieldName,FieldNameLen);
    if aField<>nil then begin
      aField.GetVariant(VTable.GetData(pointer(VValue),aField),PVariant(@Value)^);
      result := true;
    end else
      result := false;
  end;
end;

procedure TSynTableData.GetFieldVariant(const FieldName: RawUTF8; var result: Variant);
var aField: TSynTableFieldProperties;
begin
  if IsRowID(Pointer(FieldName)) then
    result := VID else begin
    CheckVTableInitialized;
    aField := VTable.FieldFromName[FieldName];
    if aField=nil then
      raise ETableDataException.CreateUTF8('Unknown % property',[FieldName]) else
    aField.GetVariant(VTable.GetData(pointer(VValue),aField),result);
  end;
end;

function TSynTableData.GetFieldValue(aField: TSynTableFieldProperties): Variant;
begin
  CheckVTableInitialized;
  aField.GetVariant(VTable.GetData(pointer(VValue),aField),result);
end;

{$endif NOVARIANTS}

procedure TSynTableData.FilterSBFValue;
begin
  CheckVTableInitialized;
  VTable.Filter(VValue);
end;

function TSynTableData.GetFieldSBFValue(aField: TSynTableFieldProperties): TSBFString;
var FieldBuffer: PAnsiChar;
begin
  CheckVTableInitialized;
  FieldBuffer := VTable.GetData(pointer(VValue),aField);
  SetString(Result,FieldBuffer,aField.GetLength(FieldBuffer));
end;

procedure TSynTableData.Init(aTable: TSynTable; aID: Integer);
begin
  VType := SynTableVariantVarType;
  VID := aID;
  VTable := aTable;
  VValue := VTable.DefaultRecordData;
end;

procedure TSynTableData.Init(aTable: TSynTable; aID: Integer;
  RecordBuffer: pointer; RecordBufferLen: integer);
begin
  VType := SynTableVariantVarType;
  VTable := aTable;
  if (RecordBufferLen=0) or (RecordBuffer=nil) then begin
    VID := 0;
    VValue := VTable.DefaultRecordData;
  end else begin
    VID := aID;
    SetString(VValue,PAnsiChar(RecordBuffer),RecordBufferLen);
  end;
end;

{$ifndef NOVARIANTS}
procedure TSynTableData.SetField(const FieldName: RawUTF8;
  const Value: Variant);
var F: TSynTableFieldProperties;
begin
  CheckVTableInitialized;
  if IsRowID(Pointer(FieldName)) then
    VID := Value else begin
    F := VTable.FieldFromName[FieldName];
    if F=nil then
      raise ETableDataException.CreateUTF8('Unknown % property',[FieldName]) else
      SetFieldValue(F,Value);
  end;
end;

procedure TSynTableData.SetFieldValue(aField: TSynTableFieldProperties; const Value: Variant);
begin
  SetFieldSBFValue(aField,aField.SBF(Value));
end;
{$endif}

procedure TSynTableData.SetFieldSBFValue(aField: TSynTableFieldProperties;
  const Value: TSBFString);
var NewValue: TSBFString;
begin
  CheckVTableInitialized;
  if (aField.FieldSize>0) and (VValue<>'') then begin
    // fixed size content: fast in-place update
    MoveFast(pointer(Value)^,VValue[aField.Offset+1],aField.FieldSize)
    // VValue[F.Offset+1] above will call UniqueString(VValue), even under FPC
  end else begin
    // variable-length update
    VTable.UpdateFieldData(pointer(VValue),length(VValue),
      aField.FieldNumber,NewValue,Value);
    VValue := NewValue;
  end;
end;

function TSynTableData.ValidateSBFValue(RecordIndex: integer): string;
begin
  CheckVTableInitialized;
  Result := VTable.Validate(Pointer(VValue),RecordIndex);
end;

{$endif DELPHI5OROLDER}


{ ************ text search and functions ****************** }

// code below adapted from ZMatchPattern.pas - http://www.zeoslib.sourceforge.net

procedure TMatch.MatchMain;
var RangeStart, RangeEnd: PtrInt;
    c: AnsiChar;
    flags: set of(Invert, MemberMatch);
begin
  while ((State = sNONE) and (P <= PMax)) do begin
    c := Upper[Pattern[P]];
    if T > TMax then begin
      if (c = '*') and (P + 1 > PMax) then
        State := sVALID else
        State := sABORT;
      exit;
    end else
    case c of
      '?': ;
      '*':
        MatchAfterStar;
      '[': begin
        inc(P);
        byte(flags) := 0;
        if Pattern[P] in ['!','^'] then begin
          include(flags, Invert);
          inc(P);
        end;
        if (Pattern[P] = ']') then begin
          State := sPATTERN;
          exit;
        end;
        c := Upper[Text[T]];
        while Pattern[P] <> ']' do begin
          RangeStart := P;
          RangeEnd := P;
          inc(P);
          if P > PMax then begin
            State := sPATTERN;
            exit;
          end;
          if Pattern[P] = '-' then begin
            inc(P);
            RangeEnd := P;
            if (P > PMax) or (Pattern[RangeEnd] = ']') then begin
              State := sPATTERN;
              exit;
            end;
            inc(P);
          end;
          if P > PMax then begin
            State := sPATTERN;
            exit;
          end;
          if RangeStart < RangeEnd then begin
            if (c >= Upper[Pattern[RangeStart]]) and (c <= Upper[Pattern[RangeEnd]]) then begin
              include(flags, MemberMatch);
              break;
            end;
          end
          else
            if (c >= Upper[Pattern[RangeEnd]]) and (c <= Upper[Pattern[RangeStart]]) then begin
              include(flags, MemberMatch);
              break;
            end;
        end;
        if ((Invert in flags) and (MemberMatch in flags)) or
           not ((Invert in flags) or (MemberMatch in flags)) then begin
          State := sRANGE;
          exit;
        end;
        if MemberMatch in flags then
          while (P <= PMax) and (Pattern[P] <> ']') do
            inc(P);
        if P > PMax then begin
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
      State := sEND else
      State := sVALID;
end;

procedure TMatch.MatchAfterStar;
var retryT, retryP: PtrInt;
begin
  if (TMax = 1) or (P = PMax) then begin
    State := sVALID;
    exit;
  end else
  if (PMax = 0) or (TMax = 0) then begin
    State := sABORT;
    exit;
  end;
  while ((T <= TMax) and (P < PMax)) and (Pattern[P] in ['?', '*']) do begin
    if Pattern[P] = '?' then
      inc(T);
    inc(P);
  end;
  if T >= TMax then begin
    State := sABORT;
    exit;
  end else
  if P >= PMax then begin
    State := sVALID;
    exit;
  end;
  repeat
    if (Upper[Pattern[P]] = Upper[Text[T]]) or (Pattern[P] = '[') then begin
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
    if (T > TMax) or (P > PMax) then begin
      State := sABORT;
      exit;
    end;
  until State <> sNONE;
end;

function SearchAny(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
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

function SearchNoRange(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
{$ifdef CPUX86}
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
    if pat <= aMatch.PMax then begin
      c := aMatch.Pattern[pat];
      case c of
        '?':
          if txt <= aMatch.TMax then begin
            inc(pat);
            inc(txt);
            continue;
          end;
        '*': begin
          aMatch.P := pat;
          aMatch.T := txt + 1;
          inc(pat);
          continue;
        end;
        else if (txt <= aMatch.TMax) and (c = aMatch.Text[txt]) then begin
          inc(pat);
          inc(txt);
          continue;
        end;
      end;
    end
    else if txt > aMatch.TMax then
      break;
    txt := aMatch.T;
    if (txt > 0) and (txt <= aMatch.TMax + 1) then begin
      inc(aMatch.T);
      pat := aMatch.P+1;
      continue;
    end;
    result := false;
    exit;
  until false;
  result := true;
end;
{$else} // optimized for x86_64/ARM with more registers
var
  c: AnsiChar;
  pat, patend, txtend, txtretry, patretry: PUTF8Char;
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
    if pat <= patend then begin
      c := pat^;
      if c <> '*' then
        if c <> '?' then begin
          if (aText <= txtend) and (c = aText^) then begin
            inc(pat);
            inc(aText);
            continue;
          end;
        end
        else begin // '?'
          if aText <= txtend then begin
            inc(pat);
            inc(aText);
            continue;
          end;
        end
        else begin // '*'
          inc(pat);
          txtretry := aText + 1;
          patretry := pat;
          continue;
        end;
    end
    else if aText > txtend then
      break;
    if (PtrInt(PtrUInt(txtretry)) > 0) and (txtretry <= txtend + 1) then begin
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

function SearchNoRangeU(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
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
    if pat <= aMatch.PMax then begin
      c := aMatch.Pattern[pat];
      case c of
        '?':
          if txt <= aMatch.TMax then begin
            inc(pat);
            inc(txt);
            continue;
          end;
        '*': begin
          aMatch.P := pat;
          aMatch.T := txt + 1;
          inc(pat);
          continue;
        end;
        else if (txt <= aMatch.TMax) and
           (aMatch.Upper[c] = aMatch.Upper[aMatch.Text[txt]]) then begin
          inc(pat);
          inc(txt);
          continue;
        end;
      end;
    end
    else if txt > aMatch.TMax then
      break;
    txt := aMatch.T;
    if (txt > 0) and (txt <= aMatch.TMax + 1) then begin
      inc(aMatch.T);
      pat := aMatch.P+1;
      continue;
    end;
    result := false;
    exit;
  until false;
  result := true;
end;

function SimpleContainsU(t, tend, p: PUTF8Char; pmax: PtrInt; up: PNormTable): boolean;
  {$ifdef HASINLINE}inline;{$endif}
// brute force case-insensitive search p[0..pmax] in t..tend-1
var first: AnsiChar;
    i: PtrInt;
label next;
begin
  first := up[p^];
  repeat
    if up[t^] <> first then begin
next: inc(t);
      if t < tend then
        continue else
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
function SimpleContains8(t, tend, p: PUTF8Char; pmax: PtrInt): boolean; inline;
label next;
var i, first: PtrInt;
begin
  first := PPtrInt(p)^;
  repeat
    if PPtrInt(t)^ <> first then begin
next: inc(t);
      if t < tend then
        continue else
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

function SimpleContains4(t, tend, p: PUTF8Char; pmax: PtrInt): boolean;
  {$ifdef HASINLINE}inline;{$endif}
label next;
var i: PtrInt;
{$ifdef CPUX86} // circumvent lack of registers for this CPU
begin
  repeat
    if PCardinal(t)^ <> PCardinal(p)^ then begin
{$else}
    first: cardinal;
begin
  first := PCardinal(p)^;
  repeat
    if PCardinal(t)^ <> first then begin
{$endif}
next: inc(t);
      if t < tend then
        continue else
        break;
    end;
    for i := 4 to pmax do
      if t[i] <> p[i] then
        goto next;
    result := true;
    exit;
  until false;
  result := false;
end;

function SimpleContains1(t, tend, p: PUTF8Char; pmax: PtrInt): boolean;
  {$ifdef HASINLINE}inline;{$endif}
label next;
var i: PtrInt;
{$ifdef CPUX86}
begin
  repeat
    if t^ <> p^ then begin
{$else}
    first: AnsiChar;
begin
  first := p^;
  repeat
    if t^ <> first then begin
{$endif}
next: inc(t);
      if t < tend then
        continue else
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

function CompareMemU(P1, P2: PUTF8Char; len: PtrInt; U: PNormTable): Boolean;
  {$ifdef FPC}inline;{$endif}
begin // here we know that len>0
  result := false;
  repeat
    dec(len);
    if U[P1[len]] <> U[P2[len]] then
      exit;
  until len = 0;
  result := true;
end;

function SearchVoid(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := aTextLen = 0;
end;

function SearchNoPattern(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := (aMatch.PMax + 1 = aTextlen) and CompareMem(aText, aMatch.Pattern, aTextLen);
end;

function SearchNoPatternU(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := (aMatch.PMax + 1 = aTextlen) and CompareMemU(aText, aMatch.Pattern, aTextLen, aMatch.Upper);
end;

function SearchContainsValid(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := true;
end;

function SearchContainsU(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  if aTextLen > 0 then
    result := SimpleContainsU(aText, aText + aTextLen, aMatch.Pattern, aMatch.PMax, aMatch.Upper)
  else
    result := false;
end;

function SearchContains1(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  if aTextLen > 0 then
    result := SimpleContains1(aText, aText + aTextLen, aMatch.Pattern, aMatch.PMax)
  else
    result := false;
end;

function SearchContains4(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  if aTextLen > 0 then
    result := SimpleContains4(aText, aText + aTextLen, aMatch.Pattern, aMatch.PMax)
  else
    result := false;
end;

{$ifdef CPU64}
function SearchContains8(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin // optimized e.g. to search an IP address as '*12.34.56.78*' in logs
  dec(aTextLen, aMatch.PMax);
  if aTextLen > 0 then
    result := SimpleContains8(aText, aText + aTextLen, aMatch.Pattern, aMatch.PMax)
  else
    result := false;
end;
{$endif CPU64}

function SearchStartWith(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := (aMatch.PMax < aTextlen) and CompareMem(aText, aMatch.Pattern, aMatch.PMax + 1);
end;

function SearchStartWithU(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  result := (aMatch.PMax < aTextlen) and CompareMemU(aText, aMatch.Pattern, aMatch.PMax + 1, aMatch.Upper);
end;

function SearchEndWith(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  result := (aTextlen >= 0) and CompareMem(aText + aTextLen, aMatch.Pattern, aMatch.PMax);
end;

function SearchEndWithU(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  dec(aTextLen, aMatch.PMax);
  result := (aTextlen >= 0) and CompareMemU(aText + aTextLen, aMatch.Pattern, aMatch.PMax, aMatch.Upper);
end;

procedure TMatch.Prepare(const aPattern: RawUTF8; aCaseInsensitive, aReuse: boolean);
begin
  Prepare(pointer(aPattern), length(aPattern), aCaseInsensitive, aReuse);
end;

procedure TMatch.Prepare(aPattern: PUTF8Char; aPatternLen: integer;
  aCaseInsensitive, aReuse: boolean);
const SPECIALS: PUTF8Char = '*?[';
begin
  Pattern := aPattern;
  PMax := aPatternLen - 1; // search in Pattern[0..PMax]
  if Pattern = nil then begin
    Search := SearchVoid;
    exit;
  end;
  if aCaseInsensitive and not IsCaseSensitive(aPattern,aPatternLen) then
    aCaseInsensitive := false; // don't slow down e.g. number or IP search
  if aCaseInsensitive then
    Upper := @NormToUpperAnsi7 else
    Upper := @NormToNorm;
  Search := nil;
  if aReuse then
    if strcspn(Pattern, SPECIALS) > PMax then
      if aCaseInsensitive then
        Search := SearchNoPatternU
      else
        Search := SearchNoPattern
    else if PMax > 0 then begin
      if Pattern[PMax] = '*' then begin
        if strcspn(Pattern + 1, SPECIALS) = PMax - 1 then
          case Pattern[0] of
            '*': begin // *something*
              inc(Pattern);
              dec(PMax, 2); // trim trailing and ending *
              if PMax < 0 then
                Search := SearchContainsValid
              else if aCaseInsensitive then
                Search := SearchContainsU
              {$ifdef CPU64}
              else if PMax >= 7 then
                Search := SearchContains8
              {$endif}
              else if PMax >= 3 then
                Search := SearchContains4
              else
                Search := SearchContains1;
            end;
            '?': // ?something*
              if aCaseInsensitive then
                Search := SearchNoRangeU
              else
                Search := SearchNoRange;
            '[':
              Search := SearchAny;
            else begin
              dec(PMax); // trim trailing *
              if aCaseInsensitive then
                Search := SearchStartWithU
              else
                Search := SearchStartWith;
            end;
          end;
      end
      else if (Pattern[0] = '*') and (strcspn(Pattern + 1, SPECIALS) >= PMax) then begin
        inc(Pattern); // jump leading *
        if aCaseInsensitive then
          Search := SearchEndWithU
        else
          Search := SearchEndWith;
      end;
    end else
      if Pattern[0] in ['*','?'] then
        Search := SearchContainsValid;
  if not Assigned(Search) then begin
    aPattern := PosChar(Pattern, '[');
    if (aPattern = nil) or (aPattern - Pattern > PMax) then
      if aCaseInsensitive then
        Search := SearchNoRangeU
      else
        Search := SearchNoRange
    else
      Search := SearchAny;
  end;
end;

type // Holub and Durian (2005) SBNDM2 algorithm
  // see http://www.cri.haifa.ac.il/events/2005/string/presentations/Holub.pdf
  TSBNDMQ2Mask = array[AnsiChar] of cardinal;
  PSBNDMQ2Mask = ^TSBNDMQ2Mask;

function SearchSBNDMQ2ComputeMask(const Pattern: RawUTF8; u: PNormTable): RawByteString;
var
  i: PtrInt;
  p: PAnsiChar absolute Pattern;
  m: PSBNDMQ2Mask absolute result;
  c: PCardinal;
begin
  SetString(result, nil, SizeOf(m^));
  FillCharFast(m^, SizeOf(m^), 0);
  for i := 0 to length(Pattern) - 1 do begin
    c := @m^[u[p[i]]]; // for FPC code generation
    c^ := c^ or (1 shl i);
  end;
end;

function SearchSBNDMQ2(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
var
  mask: PSBNDMQ2Mask;
  max, i, j: PtrInt;
  state: cardinal;
begin
  mask := pointer(aMatch^.Pattern); // was filled by SearchSBNDMQ2ComputeMask()
  max := aMatch^.PMax;
  i := max - 1;
  dec(aTextLen);
  if i < aTextLen then begin
    repeat
      state := mask[aText[i+1]] shr 1; // in two steps for better FPC codegen
      state := state and mask[aText[i]];
      if state = 0 then begin
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
      if i = j then begin
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

function SearchSBNDMQ2U(aMatch: PMatch; aText: PUTF8Char; aTextLen: PtrInt): boolean;
var
  u: PNormTable;
  mask: PSBNDMQ2Mask;
  max, i, j: PtrInt;
  state: cardinal;
begin
  mask := pointer(aMatch^.Pattern);
  max := aMatch^.PMax;
  u := aMatch^.Upper;
  i := max - 1;
  dec(aTextLen);
  if i < aTextLen then begin
    repeat
      state := mask[u[aText[i+1]]] shr 1;
      state := state and mask[u[aText[i]]];
      if state = 0 then begin
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
      if i = j then begin
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

procedure TMatch.PrepareContains(var aPattern: RawUTF8;
  aCaseInsensitive: boolean);
begin
  PMax := length(aPattern) - 1;
  if aCaseInsensitive and not IsCaseSensitive(pointer(aPattern), PMax + 1) then
    aCaseInsensitive := false;
  if aCaseInsensitive then
    Upper := @NormToUpperAnsi7
  else
    Upper := @NormToNorm;
  if PMax < 0 then
    Search := SearchContainsValid
  else if PMax > 30 then
    if aCaseInsensitive then
      Search := SearchContainsU
    else
      Search := {$ifdef CPU64}SearchContains8{$else}SearchContains4{$endif}
  else if PMax >= 1 then begin // len in [2..31] = PMax in [1..30]
    aPattern := SearchSBNDMQ2ComputeMask(aPattern, Upper); // lookup table
    if aCaseInsensitive then
      Search := SearchSBNDMQ2U
    else
      Search := SearchSBNDMQ2;
  end
  else if aCaseInsensitive then
    Search := SearchContainsU
  else
    Search := SearchContains1; // todo: use IndexByte() on FPC?
  Pattern := pointer(aPattern);
end;

procedure TMatch.PrepareRaw(aPattern: PUTF8Char; aPatternLen: integer;
  aSearch: TMatchSearchFunction);
begin
  Pattern := aPattern;
  PMax := aPatternLen - 1; // search in Pattern[0..PMax]
  Search := aSearch;
end;

function TMatch.Match(const aText: RawUTF8): boolean;
begin
  if aText <> '' then
    result := Search(@self, pointer(aText), length(aText))
  else
    result := PMax < 0;
end;

function TMatch.Match(aText: PUTF8Char; aTextLen: PtrInt): boolean;
begin
  if (aText <> nil) and (aTextLen > 0) then
    result := Search(@self, aText, aTextLen)
  else
    result := PMax < 0;
end;

function TMatch.MatchThreadSafe(const aText: RawUTF8): boolean;
var local: TMatch; // thread-safe with no lock!
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
  if aText = '' then begin
    result := PMax < 0;
    exit;
  end;
  len := length(aText);
  temp.Init(len * 3);
  {$ifdef UNICODE}
  len := RawUnicodeToUtf8(temp.buf, temp.len + 1, pointer(aText), len, [ccfNoTrailingZero]);
  {$else}
  len := CurrentAnsiConvert.AnsiBufferToUTF8(temp.buf, pointer(aText), len) - temp.buf;
  {$endif}
  local := self;
  result := local.Search(@local, temp.buf, len);
  temp.Done;
end;

function TMatch.Equals(const aAnother{$ifndef DELPHI5OROLDER}: TMatch{$endif}): boolean;
begin
  result := (PMax = TMatch(aAnother).PMax) and (Upper = TMatch(aAnother).Upper) and
    CompareMem(Pattern, TMatch(aAnother).Pattern, PMax + 1);
end;

function TMatch.PatternLength: integer;
begin
  result := PMax + 1;
end;

function TMatch.PatternText: PUTF8Char;
begin
  result := Pattern;
end;

function IsMatch(const Pattern, Text: RawUTF8; CaseInsensitive: boolean): boolean;
var match: TMatch;
begin
  match.Prepare(Pattern, CaseInsensitive, {reuse=}false);
  result := match.Match(Text);
end;

function IsMatchString(const Pattern, Text: string; CaseInsensitive: boolean): boolean;
var match: TMatch;
    pat, txt: RawUTF8;
begin
  StringToUTF8(Pattern, pat); // local variable is mandatory for FPC
  StringToUTF8(Text, txt);
  match.Prepare(pat, CaseInsensitive, {reuse=}false);
  result := match.Match(txt);
end;

function SetMatchs(const CSVPattern: RawUTF8; CaseInsensitive: boolean;
  out Match: TMatchDynArray): integer;
var P, S: PUTF8Char;
begin
  result := 0;
  P := pointer(CSVPattern);
  if P <> nil then
    repeat
      S := P;
      while not (P^ in [#0, ',']) do
        inc(P);
      if P <> S then begin
        SetLength(Match, result + 1);
        Match[result].Prepare(S, P - S, CaseInsensitive, {reuse=}true);
        inc(result);
      end;
      if P^ = #0 then
        break;
      inc(P);
    until false;
end;

function SetMatchs(CSVPattern: PUTF8Char; CaseInsensitive: boolean;
  Match: PMatch; MatchMax: integer): integer;
var S: PUTF8Char;
begin
  result := 0;
  if (CSVPattern <> nil) and (MatchMax >= 0) then
    repeat
      S := CSVPattern;
      while not (CSVPattern^ in [#0, ',']) do
        inc(CSVPattern);
      if CSVPattern <> S then begin
        Match^.Prepare(S, CSVPattern - S, CaseInsensitive, {reuse=}true);
        inc(result);
        if result > MatchMax then
          break;
        inc(Match);
      end;
      if CSVPattern^ = #0 then
        break;
      inc(CSVPattern);
    until false;
end;

function MatchExists(const One: TMatch; const Several: TMatchDynArray): boolean;
var
  i: integer;
begin
  result := true;
  for i := 0 to high(Several) do
    if Several[i].Equals(One) then
      exit;
  result := false;
end;

function MatchAdd(const One: TMatch; var Several: TMatchDynArray): boolean;
var
  n: integer;
begin
  result := not MatchExists(One, Several);
  if result then begin
    n := length(Several);
    SetLength(Several, n + 1);
    Several[n] := One;
  end;
end;

function MatchAny(const Match: TMatchDynArray; const Text: RawUTF8): boolean;
var
  m: PMatch;
  i: integer;
begin
  result := true;
  if Match = nil then
    exit;
  m := pointer(Match);
  for i := 1 to length(Match) do
    if m^.Match(Text) then
      exit
    else
      inc(m);
  result := false;
end;

procedure FilterMatchs(const CSVPattern: RawUTF8; CaseInsensitive: boolean;
  var Values: TRawUTF8DynArray);
var
  match: TMatchDynArray;
  m, n, i: integer;
begin
  if SetMatchs(CSVPattern, CaseInsensitive, match) = 0 then
    exit;
  n := 0;
  for i := 0 to high(Values) do
    for m := 0 to high(match) do
      if match[m].Match(Values[i]) then begin
        if i <> n then
          Values[n] := Values[i];
        inc(n);
        break;
      end;
  if n <> length(Values) then
    SetLength(Values, n);
end;


{ TMatchs }

constructor TMatchs.Create(const aPatterns: TRawUTF8DynArray; CaseInsensitive: Boolean);
begin
  inherited Create;
  Subscribe(aPatterns, CaseInsensitive);
end;

function TMatchs.Match(const aText: RawUTF8): integer;
begin
  result := Match(pointer(aText), length(aText));
end;

function TMatchs.Match(aText: PUTF8Char; aLen: integer): integer;
var
  one: ^TMatchStore;
  local: TMatch; // thread-safe with no lock!
begin
  if (self = nil) or (fMatch = nil) then
    result := -1 // no filter by name -> allow e.g. to process everything
  else begin
    one := pointer(fMatch);
    if aLen <> 0 then begin
      for result := 0 to fMatchCount - 1 do begin
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
  len := length(aText);
  temp.Init(len * 3);
  {$ifdef UNICODE}
  len := RawUnicodeToUtf8(temp.buf, temp.len + 1, pointer(aText), len, [ccfNoTrailingZero]);
  {$else}
  len := CurrentAnsiConvert.AnsiBufferToUTF8(temp.buf, pointer(aText), len, true) - temp.buf;
  {$endif}
  result := Match(temp.buf, len);
  temp.Done;
end;

procedure TMatchs.Subscribe(const aPatternsCSV: RawUTF8; CaseInsensitive: Boolean);
var
  patterns: TRawUTF8DynArray;
begin
  CSVToRawUTF8DynArray(pointer(aPatternsCSV), patterns);
  Subscribe(patterns, CaseInsensitive);
end;

procedure TMatchs.Subscribe(const aPatterns: TRawUTF8DynArray; CaseInsensitive: Boolean);
var
  i, j, m, n: integer;
  found: ^TMatchStore;
  pat: PRawUTF8;
begin
  m := length(aPatterns);
  if m = 0 then
    exit;
  n := fMatchCount;
  SetLength(fMatch, n + m);
  pat := pointer(aPatterns);
  for i := 1 to m do begin
    found := pointer(fMatch);
    for j := 1 to n do
      if StrComp(pointer(found^.PatternInstance), pointer(pat^)) = 0 then begin
        found := nil;
        break;
      end
      else
        inc(found);
    if found <> nil then
      with fMatch[n] do begin
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


procedure SoundExComputeAnsi(var p: PAnsiChar; var result: cardinal; Values: PSoundExValues);
var n,v,old: PtrUInt;
begin
  n := 0;
  old := 0;
  if Values<>nil then
  repeat
    {$ifdef USENORMTOUPPER}
    v := NormToUpperByte[ord(p^)]; // also handle 8 bit WinAnsi (1252 accents)
    {$else}
    v := NormToUpperAnsi7Byte[ord(p^)]; // 7 bit char uppercase
    {$endif}
    if not (tcWord in TEXT_BYTES[v]) then break;
    inc(p);
    dec(v,ord('B'));
    if v>high(TSoundExValues) then continue;
    v := Values[v]; // get soundex value
    if (v=0) or (v=old) then continue; // invalid or dopple value
    old := v;
    result := result shl SOUNDEX_BITS;
    inc(result,v);
    inc(n);
    if n=((32-8)div SOUNDEX_BITS) then // first char use up to 8 bits
      break; // result up to a cardinal size
  until false;
end;

function SoundExComputeFirstCharAnsi(var p: PAnsiChar): cardinal;
label Err;
begin
  if p=nil then begin
Err:result := 0;
    exit;
  end;
  repeat
    {$ifdef USENORMTOUPPER}
    result := NormToUpperByte[ord(p^)]; // also handle 8 bit WinAnsi (CP 1252)
    {$else}
    result := NormToUpperAnsi7Byte[ord(p^)]; // 7 bit char uppercase
    {$endif}
    if result=0 then
      goto Err; // end of input text, without a word
    inc(p);
    // trim initial spaces or 'H'
  until AnsiChar(result) in ['A'..'G','I'..'Z'];
end;

procedure SoundExComputeUTF8(var U: PUTF8Char; var result: cardinal; Values: PSoundExValues);
var n,v,old: cardinal;
begin
  n := 0;
  old := 0;
  if Values<>nil then
  repeat
    v := GetNextUTF8Upper(U);
    if not (tcWord in TEXT_BYTES[v]) then break;
    dec(v,ord('B'));
    if v>high(TSoundExValues) then continue;
    v := Values[v]; // get soundex value
    if (v=0) or (v=old) then continue; // invalid or dopple value
    old := v;
    result := result shl SOUNDEX_BITS;
    inc(result,v);
    inc(n);
    if n=((32-8)div SOUNDEX_BITS) then // first char use up to 8 bits
      break; // result up to a cardinal size
  until false;
end;

function SoundExComputeFirstCharUTF8(var U: PUTF8Char): cardinal;
label Err;
begin
  if U=nil then begin
Err:result := 0;
    exit;
  end;
  repeat
    result := GetNextUTF8Upper(U);
    if result=0 then
      goto Err; // end of input text, without a word
    // trim initial spaces or 'H'
  until AnsiChar(result) in ['A'..'G','I'..'Z'];
end;


{ TSynSoundEx }

const
  /// english Soundex pronunciation scores
  // - defines the default values used for the SoundEx() function below
  // (used if Values parameter is nil)
  ValueEnglish: TSoundExValues =
  // B C D E F G H I J K L M N O P Q R S T U V W X Y Z
    (1,2,3,0,1,2,0,0,2,2,4,5,5,0,1,2,6,2,3,0,1,0,2,0,2);

  /// french Soundex pronunciation scores
  // - can be used to override default values used for the SoundEx()
  // function below
  ValueFrench: TSoundExValues =
  // B C D E F G H I J K L M N O P Q R S T U V W X Y Z
    (1,2,3,0,9,7,0,0,7,2,4,5,5,0,1,2,6,8,3,0,9,0,8,0,8);

  /// spanish Soundex pronunciation scores
  // - can be used to override default values used for the SoundEx()
  // function below
  ValueSpanish: TSoundExValues =
  // B C D E F G H I J K L M N O P Q R S T U V W X Y Z
    (1,2,3,0,1,2,0,0,0,2,0,5,5,0,1,2,6,2,3,0,1,0,2,0,2);

  SOUNDEXVALUES: array[TSynSoundExPronunciation] of PSoundExValues =
    (@ValueEnglish,@ValueFrench,@ValueSpanish,@ValueEnglish);

function TSynSoundEx.Ansi(A: PAnsiChar): boolean;
var Value, c: cardinal;
begin
  result := false;
  if A=nil then exit;
  repeat
    // test beginning of word
    c := SoundExComputeFirstCharAnsi(A);
    if c=0 then exit else
    if c=FirstChar then begin
      // here we had the first char match -> check if word match UpperValue
      Value := c-(ord('A')-1);
      SoundExComputeAnsi(A,Value,fValues);
      if Value=search then begin
        result := true; // UpperValue found!
        exit;
      end;
    end else
    repeat
      if A^=#0 then exit else
{$ifdef USENORMTOUPPER}
        if not(tcWord in TEXT_CHARS[NormToUpper[A^]]) then break else inc(A);
{$else} if not(tcWord in TEXT_CHARS[A^]) then break else inc(A); {$endif}
    until false;
    // find beginning of next word
    repeat
      if A^=#0 then exit else
{$ifdef USENORMTOUPPER}
        if tcWord in TEXT_CHARS[NormToUpper[A^]] then break else inc(A);
{$else} if tcWord in TEXT_CHARS[A^] then break else inc(A); {$endif}
    until false;
  until false;
end;

function TSynSoundEx.UTF8(U: PUTF8Char): boolean;
var Value, c: cardinal;
    V: PUTF8Char;
begin
  result := false;
  if U=nil then exit;
  repeat
    // find beginning of word
    c := SoundExComputeFirstCharUTF8(U);
    if c=0 then exit else
    if c=FirstChar then begin
      // here we had the first char match -> check if word match UpperValue
      Value := c-(ord('A')-1);
      SoundExComputeUTF8(U,Value,fValues);
      if Value=search then begin
        result := true; // UpperValue found!
        exit;
      end;
    end else
    repeat
      c := GetNextUTF8Upper(U);
      if c=0 then
        exit;
    until not(tcWord in TEXT_BYTES[c]);
    // find beginning of next word
    repeat
      if U=nil then exit;
      V := U;
      c := GetNextUTF8Upper(U);
      if c=0 then
        exit;
    until tcWord in TEXT_BYTES[c];
    U := V;
  until U=nil;
end;

function TSynSoundEx.Prepare(UpperValue: PAnsiChar; Lang: PSoundExValues): boolean;
begin
  fValues := Lang;
  Search := SoundExAnsi(UpperValue,nil,Lang);
  if Search=0 then
    result := false else begin
    FirstChar := SoundExComputeFirstCharAnsi(UpperValue);
    result := true;
  end;
end;

function TSynSoundEx.Prepare(UpperValue: PAnsiChar; Lang: TSynSoundExPronunciation): boolean;
begin
  result := Prepare(UpperValue,SOUNDEXVALUES[Lang]);
end;

function SoundExAnsi(A: PAnsiChar; next: PPAnsiChar;
  Lang: PSoundExValues): cardinal;
begin
  result := SoundExComputeFirstCharAnsi(A);
  if result<>0 then begin
    dec(result,ord('A')-1);   // first Soundex char is first char
    SoundExComputeAnsi(A,result,Lang);
  end;
  if next<>nil then begin
    {$ifdef USENORMTOUPPER}
    while tcWord in TEXT_CHARS[NormToUpper[A^]] do inc(A); // go to end of word
    {$else}
    while tcWord in TEXT_CHARS[A^] do inc(A); // go to end of word
    {$endif}
    next^ := A;
  end;
end;

function SoundExAnsi(A: PAnsiChar; next: PPAnsiChar;
  Lang: TSynSoundExPronunciation): cardinal;
begin
  result := SoundExAnsi(A,next,SOUNDEXVALUES[Lang]);
end;

function SoundExUTF8(U: PUTF8Char; next: PPUTF8Char;
  Lang: TSynSoundExPronunciation): cardinal;
begin
  result := SoundExComputeFirstCharUTF8(U);
  if result<>0 then begin
    dec(result,ord('A')-1);   // first Soundex char is first char
    SoundExComputeUTF8(U,result,SOUNDEXVALUES[Lang]);
  end;
  if next<>nil then
    next^ := FindNextUTF8WordBegin(U);
end;

{$ifdef ASMX64AVX} // AVX2 ASM not available on Delphi yet
// adapted from https://github.com/simdjson/simdjson - Apache License 2.0
function IsValidUtf8LenAvx2(source: PUtf8Char; sourcelen: PtrInt): boolean;
  {$ifdef FPC}nostackframe; assembler; asm {$else} asm .noframe {$endif FPC}
        test    source, source
        jz      @ok
        test    sourcelen, sourcelen
        jle     @ok
        {$ifdef win64} // this ABI doesn't consider rsi/rdi as volatile
        push    rsi
        push    rdi
        {$endif}
        push    rbp
        mov     r8, source
        mov     rdx, sourcelen
        mov     rsi, r8
        mov     ecx, 64
        mov     rax, rsi
        mov     rdi, rdx
        mov     rbp, rsp
        and     rsp, 0FFFFFFFFFFFFFFE0H // align stack at 32 bytes
        sub     rsp, 160
        cmp     rdx, 64
        cmovnc  rcx, rdx
        sub     rcx, 64
        je      @small
        vpxor   xmm3, xmm3, xmm3
        vmovdqa ymm7,  ymmword ptr [rip + @0f]
        vmovdqa ymm15, ymmword ptr [rip + @_6]
        xor     esi, esi
        vmovdqa ymm14, ymmword ptr [rip + @_7]
        vmovdqa ymm13, ymmword ptr [rip + @_8]
        vmovdqa ymm5, ymm3
        vmovdqa ymm2, ymm3
        // main processing loop, 64 bytes per iteration
        align 16
@loop:  vmovdqu xmm6, xmmword ptr [rax + rsi]
        vinserti128 ymm0, ymm6, xmmword ptr [rax + rsi + 10H], 01H
        vmovdqu xmm6, xmmword ptr [rax + rsi + 20H]
        vinserti128 ymm1, ymm6, xmmword ptr [rax + rsi + 30H], 01H
        add     rsi, 64
        vpor    ymm4, ymm1, ymm0
        vpmovmskb rdx, ymm4 // check set MSB of each 64 bytes
        test    edx, edx
        jne     @check
        vpor    ymm2, ymm5, ymm2
        vmovdqa ymm4, ymm2
        cmp     rcx, rsi
        ja      @loop
        // process trailing 0..63 bytes
@trail: sub     rdi, rsi
        jz      @ended
        add     rsi, rax
        vmovdqa xmm0, xmmword ptr [rip + @20]
        lea     rdx, qword ptr [rsp + 60H] // copy on stack with space padding
        sub     rsi, rdx
        vmovdqa xmmword ptr [rdx], xmm0
        vmovdqa xmmword ptr [rdx + 10H], xmm0
        vmovdqa xmmword ptr [rdx + 20H], xmm0
        vmovdqa xmmword ptr [rdx + 30H], xmm0
@by8:   sub     rdi, 8
        jb      @by1
        mov     rax, qword ptr [rsi + rdx]
        mov     qword ptr [rdx], rax
        add     rdx, 8 // in-order copy to preserve UTF-8 encoding
        jmp     @by8
@by1:   add     rdi, 8
        jz      @0
@sml:   mov     al, byte ptr [rsi + rdx]
        mov     byte ptr [rdx], al
        add     rdx, 1
        sub     rdi, 1
        jnz     @sml
@0:     vmovdqa ymm1, ymmword ptr [rsp + 60H]
        vmovdqa ymm2, ymmword ptr [rsp + 80H]
        vpor    ymm0, ymm1, ymm2
        vpmovmskb rax, ymm0 // check any set MSB
        test    eax, eax
        jne     @last
@ended: vpor    ymm5, ymm5, ymm4
@final: vptest  ymm5, ymm5
        sete    al
        vzeroupper
        leave      // mov rsp,rbp + pop rbp
        {$ifdef win64}
        pop     rdi
        pop     rsi
        {$endif}
        ret
@ok:    mov     al, 1
        ret
@small: vpxor   xmm4, xmm4, xmm4
        xor     esi, esi
        vmovdqa ymm3, ymm4
        vmovdqa ymm5, ymm4
        jmp     @trail
        // validate UTF-8 extra bytes from main loop
        align 8
@check: vpsrlw  ymm9, ymm0, 4
        vpsrlw  ymm12, ymm1, 4
        vperm2i128 ymm3, ymm3, ymm0, 21H
        vpalignr ymm5, ymm0, ymm3, 0FH
        vpalignr ymm11, ymm0, ymm3, 0EH
        vpsubusb ymm11, ymm11, ymmword ptr [rip + @_9]
        vpalignr ymm3, ymm0, ymm3, 0DH
        vperm2i128 ymm0, ymm0, ymm1, 21H
        vpsubusb ymm3, ymm3, ymmword ptr [rip + @_10]
        vpalignr ymm8, ymm1, ymm0, 0FH
        vpsrlw  ymm10, ymm5, 4
        vpand   ymm5, ymm7, ymm5
        vpsrlw  ymm6, ymm8, 4
        vpalignr ymm4, ymm1, ymm0, 0EH
        vpsubusb ymm4, ymm4, ymmword ptr [rip + @_9]
        vpalignr ymm0, ymm1, ymm0, 0DH
        vpsubusb ymm0, ymm0, ymmword ptr [rip + @_10]
        vpand   ymm10, ymm10, ymm7
        vpand   ymm6, ymm6, ymm7
        vpand   ymm8, ymm7, ymm8
        vpor    ymm3, ymm3, ymm11
        vpor    ymm0, ymm4, ymm0
        vpxor   xmm11, xmm11, xmm11
        vpshufb ymm10, ymm15, ymm10
        vpshufb ymm5, ymm14, ymm5
        vpand   ymm9, ymm9, ymm7
        vpshufb ymm6, ymm15, ymm6
        vpshufb ymm8, ymm14, ymm8
        vpand   ymm12, ymm12, ymm7
        vpand   ymm5, ymm5, ymm10
        vpcmpgtb ymm3, ymm3, ymm11
        vpcmpgtb ymm0, ymm0, ymm11
        vpshufb ymm9, ymm13, ymm9
        vpand   ymm3, ymm3, ymmword ptr [rip + @_11]
        vpand   ymm0, ymm0, ymmword ptr [rip + @_11]
        vpshufb ymm12, ymm13, ymm12
        vpand   ymm6, ymm6, ymm8
        vpand   ymm9, ymm5, ymm9
        vpsubusb ymm5, ymm1, ymmword ptr [rip + @_12]
        vpand   ymm12, ymm6, ymm12
        vpxor   ymm9, ymm3, ymm9
        vmovdqa ymm3, ymm1
        vpxor   ymm12, ymm0, ymm12
        vpor    ymm9, ymm9, ymm12
        vpor    ymm2, ymm9, ymm2
        vmovdqa ymm4, ymm2
        cmp     rcx, rsi
        ja      @loop
        jmp     @trail
        // validate UTF-8 extra bytes from input ending
        align 8
@last:  vmovdqa ymm5, ymmword ptr [rip + @0f]
        vperm2i128 ymm3, ymm3, ymm1, 21H
        vmovdqa ymm9, ymmword ptr [rip + @_7]
        vpsrlw  ymm11, ymm1, 4
        vpalignr ymm0, ymm1, ymm3, 0FH
        vmovdqa ymm13, ymmword ptr [rip + @_10]
        vmovdqa ymm14, ymmword ptr [rip + @_9]
        vpsrlw  ymm6, ymm0, 4
        vpand   ymm0, ymm5, ymm0
        vpand   ymm11, ymm11, ymm5
        vmovdqa ymm7, ymmword ptr [rip + @_6]
        vpshufb ymm10, ymm9, ymm0
        vpalignr ymm0, ymm1, ymm3, 0EH
        vpand   ymm6, ymm6, ymm5
        vmovdqa ymm8, ymmword ptr [rip + @_8]
        vpalignr ymm3, ymm1, ymm3, 0DH
        vperm2i128 ymm1, ymm1, ymm2, 21H
        vpsubusb ymm0, ymm0, ymm14
        vpsubusb ymm12, ymm3, ymm13
        vpalignr ymm3, ymm2, ymm1, 0FH
        vpshufb ymm6, ymm7, ymm6
        vpsrlw  ymm15, ymm3, 4
        vpand   ymm3, ymm5, ymm3
        vpor    ymm0, ymm0, ymm12
        vpshufb ymm9, ymm9, ymm3
        vpsrlw  ymm3, ymm2, 4
        vpand   ymm15, ymm15, ymm5
        vpand   ymm5, ymm3, ymm5
        vpalignr ymm3, ymm2, ymm1, 0EH
        vpxor   xmm12, xmm12, xmm12
        vpalignr ymm1, ymm2, ymm1, 0DH
        vpsubusb ymm3, ymm3, ymm14
        vpshufb ymm11, ymm8, ymm11
        vpsubusb ymm1, ymm1, ymm13
        vpcmpgtb ymm0, ymm0, ymm12
        vpshufb ymm7, ymm7, ymm15
        vpor    ymm1, ymm3, ymm1
        vpshufb ymm8, ymm8, ymm5
        vpsubusb ymm5, ymm2, ymmword ptr [rip + @_12]
        vmovdqa ymm2, ymmword ptr [rip + @_11]
        vpcmpgtb ymm1, ymm1, ymm12
        vpand   ymm6, ymm6, ymm10
        vpand   ymm7, ymm7, ymm9
        vpand   ymm0, ymm0, ymm2
        vpand   ymm11, ymm6, ymm11
        vpand   ymm8, ymm7, ymm8
        vpxor   ymm0, ymm0, ymm11
        vpor    ymm5, ymm4, ymm5
        vpand   ymm1, ymm1, ymm2
        vpxor   ymm1, ymm1, ymm8
        vpor    ymm0, ymm0, ymm1
        vpor    ymm5, ymm0, ymm5
        jmp     @final
        align 16
@20:    dq 2020202020202020H
        dq 2020202020202020H
        align 32
@0f:    dq 0F0F0F0F0F0F0F0FH
        dq 0F0F0F0F0F0F0F0FH
        dq 0F0F0F0F0F0F0F0FH
        dq 0F0F0F0F0F0F0F0FH
@_6:    dq 0202020202020202H
        dq 4915012180808080H
        dq 0202020202020202H
        dq 4915012180808080H
@_7:    dq 0CBCBCB8B8383A3E7H
        dq 0CBCBDBCBCBCBCBCBH
        dq 0CBCBCB8B8383A3E7H
        dq 0CBCBDBCBCBCBCBCBH
@_8:    dq 0101010101010101H
        dq 01010101BABAAEE6H
        dq 0101010101010101H
        dq 01010101BABAAEE6H
@_9:    dq 0DFDFDFDFDFDFDFDFH
        dq 0DFDFDFDFDFDFDFDFH
        dq 0DFDFDFDFDFDFDFDFH
        dq 0DFDFDFDFDFDFDFDFH
@_10:   dq 0EFEFEFEFEFEFEFEFH
        dq 0EFEFEFEFEFEFEFEFH
        dq 0EFEFEFEFEFEFEFEFH
        dq 0EFEFEFEFEFEFEFEFH
@_11:   dq 8080808080808080H
        dq 8080808080808080H
        dq 8080808080808080H
        dq 8080808080808080H
@_12:   db 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH
        db 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH
        db 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0FFH
        db 0FFH, 0FFH, 0FFH, 0FFH, 0FFH, 0EFH, 0DFH, 0BFH
end;

function IsValidUTF8Avx2(source: PUTF8Char): Boolean;
begin
  result := IsValidUTF8LenAvx2(source,StrLen(source));
end;
{$endif ASMX64AVX}

function IsValidUTF8Pas(source: PUTF8Char): Boolean;
var extra, i: integer;
    c: cardinal;
begin
  result := false;
  if source<>nil then
  repeat
    c := byte(source^);
    inc(source);
    if c=0 then break else
    if c and $80<>0 then begin
      extra := UTF8_EXTRABYTES[c];
      if extra=0 then exit else // invalid leading byte
      for i := 1 to extra do
        if byte(source^) and $c0<>$80 then
          exit else
          inc(source); // check valid UTF-8 content
    end;
  until false;
  result := true;
end;

function IsValidUTF8LenPas(source: PUTF8Char; sourcelen: PtrInt): Boolean;
var extra, i: integer;
    c: cardinal;
begin
  result := false;
  inc(sourcelen,PtrInt(source));
  if source<>nil then
    while PtrInt(PtrUInt(source))<sourcelen do begin
      c := byte(source^);
      inc(source);
      if c=0 then exit else
      if c and $80<>0 then begin
        extra := UTF8_EXTRABYTES[c];
        if extra=0 then exit else // invalid leading byte
        for i := 1 to extra do
          if (PtrInt(PtrUInt(source))>=sourcelen) or (byte(source^) and $c0<>$80) then
            exit else
            inc(source); // check valid UTF-8 content
      end;
    end;
  result := true;
end;

function IsValidUTF8(source: PUTF8Char): Boolean;
begin
  result := DoIsValidUTF8(source);
end;

function IsValidUTF8(source: PUTF8Char; sourcelen: PtrInt): Boolean;
begin
  result := DoIsValidUTF8Len(source,sourcelen);
end;

function IsValidUTF8(const source: RawUTF8): Boolean;
begin
  result := DoIsValidUTF8Len(pointer(Source),length(Source));
end;


{ ************ filtering and validation classes and functions *************** }

function IPToCardinal(P: PUTF8Char; out aValue: cardinal): boolean;
var i,c: cardinal;
    b: array[0..3] of byte;
begin
  aValue := 0;
  result := false;
  if (P=nil) or (IdemPChar(P,'127.0.0.1') and (P[9]=#0)) then
    exit;
  for i := 0 to 3 do begin
    c := GetNextItemCardinal(P,'.');
    if (c>255) or ((P=nil) and (i<3)) then
      exit;
    b[i] := c;
  end;
  if PCardinal(@b)^<>$0100007f then begin
    aValue := PCardinal(@b)^;
    result := true;
  end;
end;

function IPToCardinal(const aIP: RawUTF8; out aValue: cardinal): boolean;
begin
  result := IPToCardinal(pointer(aIP),aValue);
end;

function IPToCardinal(const aIP: RawUTF8): cardinal;
begin
  IPToCardinal(pointer(aIP),result);
end;

function IsValidIP4Address(P: PUTF8Char): boolean;
var ndot: PtrInt;
    V: PtrUInt;
begin
  result := false;
  if (P=nil) or not (P^ in ['0'..'9']) then
    exit;
  V := 0;
  ndot := 0;
  repeat
    case P^ of
      #0: break;
      '.': if (P[-1]='.') or (V>255) then
        exit else begin
        inc(ndot);
        V := 0;
      end;
      '0'..'9': V := (V*10)+ord(P^)-48;
      else exit;
    end;
    inc(P);
  until false;
  if (ndot=3) and (V<=255) and (P[-1]<>'.') then
    result := true;
end;

function IsValidEmail(P: PUTF8Char): boolean;
// Initial Author: Ernesto D'Spirito - UTF-8 version by AB
// http://www.howtodothings.com/computers/a1169-validating-email-addresses-in-delphi.html
const
  // Valid characters in an "atom"
  atom_chars: TSynAnsicharSet = [#33..#255] -
     ['(', ')', '<', '>', '@', ',', ';', ':', '\', '/', '"', '.', '[', ']', #127];
  // Valid characters in a "quoted-string"
  quoted_string_chars: TSynAnsicharSet = [#0..#255] - ['"', #13, '\'];
  // Valid characters in a subdomain
  letters_digits: TSynAnsicharSet = ['0'..'9', 'A'..'Z', 'a'..'z'];
type
  States = (STATE_BEGIN, STATE_ATOM, STATE_QTEXT, STATE_QCHAR,
    STATE_QUOTE, STATE_LOCAL_PERIOD, STATE_EXPECTING_SUBDOMAIN,
    STATE_SUBDOMAIN, STATE_HYPHEN);
var
  State: States;
  subdomains: integer;
  c: AnsiChar;
  ch: PtrInt;
begin
  State := STATE_BEGIN;
  subdomains := 1;
  if P<>nil then
  repeat
    ch := ord(P^);
    if ch and $80=0 then
      inc(P) else
      ch := GetHighUTF8UCS4(P);
    if (ch<=255) and (WinAnsiConvert.AnsiToWide[ch]<=255) then
      // convert into WinAnsi char
      c := AnsiChar(ch) else
      // invalid char
      c := #127;
    case State of
    STATE_BEGIN:
      if c in atom_chars then
        State := STATE_ATOM else
      if c='"' then
        State := STATE_QTEXT else
        break;
    STATE_ATOM:
      if c='@' then
        State := STATE_EXPECTING_SUBDOMAIN else
      if c='.' then
        State := STATE_LOCAL_PERIOD else
      if not (c in atom_chars) then
        break;
    STATE_QTEXT:
      if c='\' then
        State := STATE_QCHAR else
      if c='"' then
        State := STATE_QUOTE else
      if not (c in quoted_string_chars) then
        break;
    STATE_QCHAR:
      State := STATE_QTEXT;
    STATE_QUOTE:
      if c='@' then
        State := STATE_EXPECTING_SUBDOMAIN else
      if c='.' then
        State := STATE_LOCAL_PERIOD else
        break;
    STATE_LOCAL_PERIOD:
      if c in atom_chars then
        State := STATE_ATOM else
      if c='"' then
        State := STATE_QTEXT else
        break;
    STATE_EXPECTING_SUBDOMAIN:
      if c in letters_digits then
        State := STATE_SUBDOMAIN else
        break;
    STATE_SUBDOMAIN:
      if c='.' then begin
        inc(subdomains);
        State := STATE_EXPECTING_SUBDOMAIN
      end else
      if c='-' then
        State := STATE_HYPHEN else
      if not (c in letters_digits) then
        break;
    STATE_HYPHEN:
      if c in letters_digits then
        State := STATE_SUBDOMAIN else
      if c<>'-' then
        break;
    end;
    if P^=#0 then begin
      P := nil;
      break;
    end;
  until false;
  result := (State = STATE_SUBDOMAIN) and (subdomains >= 2);
end;


{ TSynFilterOrValidate }

constructor TSynFilterOrValidate.Create(const aParameters: RawUTF8);
begin
  inherited Create;
  SetParameters(aParameters); // should parse the JSON-encoded parameters
end;

constructor TSynFilterOrValidate.CreateUTF8(const Format: RawUTF8;
  const Args, Params: array of const);
begin
  Create(FormatUTF8(Format,Args,Params,true));
end;

procedure TSynFilterOrValidate.SetParameters(const value: RawUTF8);
begin
  fParameters := value;
end;

function TSynFilterOrValidate.AddOnce(var aObjArray: TSynFilterOrValidateObjArray;
  aFreeIfAlreadyThere: boolean): TSynFilterOrValidate;
var i: integer;
begin
  if self<>nil then begin
    for i := 0 to length(aObjArray)-1 do
      if (PPointer(aObjArray[i])^=PPointer(self)^) and
         (aObjArray[i].fParameters=fParameters) then begin
        if aFreeIfAlreadyThere then
          Free;
        result := aObjArray[i];
        exit;
      end;
    ObjArrayAdd(aObjArray,self);
  end;
  result := self;
end;


{ TSynFilterUpperCase }

procedure TSynFilterUpperCase.Process(aFieldIndex: integer; var value: RawUTF8);
begin
  value := SynCommons.UpperCase(value);
end;


{ TSynFilterUpperCaseU }

procedure TSynFilterUpperCaseU.Process(aFieldIndex: integer; var value: RawUTF8);
begin
  value := UpperCaseU(value);
end;


{ TSynFilterLowerCase }

procedure TSynFilterLowerCase.Process(aFieldIndex: integer; var value: RawUTF8);
begin
  value := LowerCase(value);
end;


{ TSynFilterLowerCaseU }

procedure TSynFilterLowerCaseU.Process(aFieldIndex: integer; var value: RawUTF8);
begin
  value := LowerCaseU(value);
end;


{ TSynFilterTrim }

procedure TSynFilterTrim.Process(aFieldIndex: integer; var value: RawUTF8);
begin
  value := Trim(value);
end;


{ TSynFilterTruncate}

procedure TSynFilterTruncate.SetParameters(const value: RawUTF8);
var V: array[0..1] of TValuePUTF8Char;
    tmp: TSynTempBuffer;
begin
  tmp.Init(value);
  JSONDecode(tmp.buf,['MaxLength','UTF8Length'],@V);
  fMaxLength := GetCardinalDef(V[0].Value,0);
  fUTF8Length := V[1].Idem('1') or V[1].Idem('true');
  tmp.Done;
end;

procedure TSynFilterTruncate.Process(aFieldIndex: integer; var value: RawUTF8);
begin
  if fMaxLength-1<cardinal(maxInt) then
    if fUTF8Length then
      Utf8TruncateToLength(value,fMaxLength) else
      Utf8TruncateToUnicodeLength(value,fMaxLength);
end;


{ TSynValidateIPAddress }

function TSynValidateIPAddress.Process(aFieldIndex: integer; const value: RawUTF8;
  var ErrorMsg: string): boolean;
begin
  result := IsValidIP4Address(pointer(value));
  if not result then
    ErrorMsg := Format(sInvalidIPAddress,[UTF8ToString(value)]);
end;


{ TSynValidateEmail }

function TSynValidateEmail.Process(aFieldIndex: integer; const value: RawUTF8;
  var ErrorMsg: string): boolean;
var TLD,DOM: RawUTF8;
    i: integer;
const TopLevelTLD: array[0..19] of PUTF8Char = (
  // see http://en.wikipedia.org/wiki/List_of_Internet_top-level_domains
  'aero','asia','biz','cat','com','coop','edu','gov','info','int','jobs',
  'mil','mobi','museum','name','net','org','pro','tel','travel'); // no xxx !
begin
  if IsValidEmail(pointer(value)) then
  repeat
    DOM := lowercase(copy(value,PosExChar('@',value)+1,100));
    if length(DOM)>63 then
      break; // exceeded 63-character limit of a DNS name
    if (ForbiddenDomains<>'') and (FindCSVIndex(pointer(ForbiddenDomains),DOM)>=0) then
      break;
    i := length(value);
    while (i>0) and (value[i]<>'.') do dec(i);
    TLD := lowercase(copy(value,i+1,100));
    if (AllowedTLD<>'') and (FindCSVIndex(pointer(AllowedTLD),TLD)<0) then
      break;
    if (ForbiddenTLD<>'') and (FindCSVIndex(pointer(ForbiddenTLD),TLD)>=0) then
      break;
    if not fAnyTLD then
      if FastFindPUTF8CharSorted(@TopLevelTLD,high(TopLevelTLD),pointer(TLD))<0 then
        if length(TLD)<>2 then
          break; // assume a two chars string is a ISO 3166-1 alpha-2 code
    result := true;
    exit;
  until true;
  ErrorMsg := Format(sInvalidEmailAddress,[UTF8ToString(value)]);
  result := false;
end;

procedure TSynValidateEmail.SetParameters(const value: RawUTF8);
var V: array[0..3] of TValuePUTF8Char;
    tmp: TSynTempBuffer;
begin
  inherited;
  tmp.Init(value);
  JSONDecode(tmp.buf,['AllowedTLD','ForbiddenTLD','ForbiddenDomains','AnyTLD'],@V);
  LowerCaseCopy(V[0].Value,V[0].ValueLen,fAllowedTLD);
  LowerCaseCopy(V[1].Value,V[1].ValueLen,fForbiddenTLD);
  LowerCaseCopy(V[2].Value,V[2].ValueLen,fForbiddenDomains);
  AnyTLD := V[3].Idem('1') or V[3].Idem('true');
  tmp.Done;
end;


{ TSynValidatePattern }

procedure TSynValidatePattern.SetParameters(const Value: RawUTF8);
begin
  inherited SetParameters(Value);
  fMatch.Prepare(Value, ClassType=TSynValidatePatternI, {reuse=}true);
end;

function TSynValidatePattern.Process(aFieldIndex: integer; const value: RawUTF8;
  var ErrorMsg: string): boolean;
  procedure SetErrorMsg;
  begin
    ErrorMsg := Format(sInvalidPattern,[UTF8ToString(value)]);
  end;
begin
  result := fMatch.Match(value);
  if not result then
    SetErrorMsg;
end;


{ TSynValidateNonVoidText }

function Character01n(n: integer): string;
begin
  if n<0 then
    n := 0 else
  if n>1 then
    n := 2;
  result := GetCSVItemString(pointer(string(sCharacter01n)),n);
end;

procedure InvalidTextLengthMin(min: integer; var result: string);
begin
  result := Format(sInvalidTextLengthMin,[min,Character01n(min)]);
end;

function TSynValidateNonVoidText.Process(aFieldIndex: integer; const value: RawUTF8;
  var ErrorMsg: string): boolean;
begin
  if value='' then begin
    InvalidTextLengthMin(1,ErrorMsg);
    result := false;
  end else
    result := true;
end;


{ TSynValidateText }

procedure TSynValidateText.SetErrorMsg(fPropsIndex, InvalidTextIndex,
  MainIndex: integer; var result: string);
var P: PChar;
begin
  P := pointer(string(sInvalidTextChar));
  result := GetCSVItemString(P,MainIndex);
  if fPropsIndex>0 then
    result := Format(result,
      [fProps[fPropsIndex],GetCSVItemString(P,InvalidTextIndex),
       Character01n(fProps[fPropsIndex])]);
end;

function TSynValidateText.Process(aFieldIndex: integer; const value: RawUTF8;
  var ErrorMsg: string): boolean;
var i, L: cardinal;
    Min: array[2..7] of cardinal;
begin
  result := false;
  if fUTF8Length then
    L := length(value) else
    L := Utf8ToUnicodeLength(pointer(value));
  if L<MinLength then
    InvalidTextLengthMin(MinLength,ErrorMsg) else
  if L>MaxLength then
    ErrorMsg := Format(sInvalidTextLengthMax,[MaxLength,Character01n(MaxLength)]) else begin
    FillCharFast(Min,SizeOf(Min),0);
    L := length(value);
    for i := 1 to L do
    case value[i] of
      ' ':
        inc(Min[7]);
      'a'..'z': begin
        inc(Min[2]);
        inc(Min[5]);
      end;
      'A'..'Z': begin
        inc(Min[2]);
        inc(Min[6]);
      end;
      '0'..'9':
        inc(Min[3]);
      '_','!',';','.',',','/',':','?','%','$','=','"','#','@','(',')','{','}',
      '+','''','-','*':
        inc(Min[4]);
    end;
    for i := 2 to 7 do
      if Min[i]<fProps[i] then begin
        SetErrorMsg(i,i,0,ErrorMsg);
        exit;
      end else
      if Min[i]>fProps[i+8] then begin
        SetErrorMsg(i+8,i,1,ErrorMsg);
        exit;
      end;
    if value<>'' then begin
      if MaxLeftTrimCount<cardinal(maxInt) then begin
        // if MaxLeftTrimCount is set, check against Value
        i := 0;
        while (i<L) and (value[i+1]=' ') do inc(i);
        if i>MaxLeftTrimCount then begin
          SetErrorMsg(0,0,8,ErrorMsg);
          exit;
        end;
      end;
      if MaxRightTrimCount<cardinal(maxInt) then begin
        // if MaxRightTrimCount is set, check against Value
        i := 0;
        while (i<L) and (value[L-i]=' ') do dec(i);
        if i>MaxRightTrimCount then begin
          SetErrorMsg(0,0,9,ErrorMsg);
          exit;
        end;
      end;
    end;
    result := true;
  end;
end;

procedure TSynValidateText.SetParameters(const value: RawUTF8);
var V: array[0..high(TSynValidateTextProps)+1] of TValuePUTF8Char;
    i: integer;
    tmp: TSynTempBuffer;
const DEFAULT: TSynValidateTextProps = (
  1,maxInt,0,0,0,0,0,0,maxInt,maxInt,maxInt,maxInt,maxInt,maxInt,maxInt,maxInt);
begin
  if (MinLength=0) and (MaxLength=0) then  // if not previously set
    fProps := DEFAULT;
  inherited SetParameters(value);
  if value='' then
    exit;
  tmp.Init(value);
  try
    JSONDecode(tmp.buf,['MinLength','MaxLength',
      'MinAlphaCount','MinDigitCount','MinPunctCount',
      'MinLowerCount','MinUpperCount','MinSpaceCount',
      'MaxLeftTrimCount','MaxRightTrimCount',
      'MaxAlphaCount','MaxDigitCount','MaxPunctCount',
      'MaxLowerCount','MaxUpperCount','MaxSpaceCount',
      'UTF8Length'],@V);
    for i := 0 to high(fProps) do
      fProps[i] := GetCardinalDef(V[i].Value,fProps[i]);
    with V[high(V)] do
      fUTF8Length := Idem('1') or Idem('true');
  finally
    tmp.Done;
  end;
end;


{ TSynValidatePassWord }

procedure TSynValidatePassWord.SetParameters(const value: RawUTF8);
const DEFAULT: TSynValidateTextProps = (
  5,20,1,1,1,1,1,0,maxInt,maxInt,maxInt,maxInt,maxInt,maxInt,maxInt,0);
begin
  // set default values for validating a strong password
  fProps := DEFAULT;
  // read custom parameters
  inherited;
end;


{ ************ low-level buffer processing functions************************* }

{ TSynBloomFilter }

const
  BLOOM_VERSION = 0;
  BLOOM_MAXHASH = 32; // only 7 is needed for 1% false positive ratio

constructor TSynBloomFilter.Create(aSize: integer; aFalsePositivePercent: double);
const LN2 = 0.69314718056;
begin
  inherited Create;
  if aSize < 0 then
    fSize := 1000 else
    fSize := aSize;
  if aFalsePositivePercent<=0 then
    fFalsePositivePercent := 1 else
  if aFalsePositivePercent>100 then
    fFalsePositivePercent := 100 else
    fFalsePositivePercent := aFalsePositivePercent;
  // see http://stackoverflow.com/a/22467497
  fBits := Round(-ln(fFalsePositivePercent/100)*aSize/(LN2*LN2));
  fHashFunctions := Round(fBits/fSize*LN2);
  if fHashFunctions=0 then
    fHashFunctions := 1 else
  if fHashFunctions>BLOOM_MAXHASH then
    fHashFunctions := BLOOM_MAXHASH;
  Reset;
end;

constructor TSynBloomFilter.Create(const aSaved: RawByteString; aMagic: cardinal);
begin
  inherited Create;
  if not LoadFrom(aSaved,aMagic) then
    raise ESynException.CreateUTF8('%.Create with invalid aSaved content',[self]);
end;

procedure TSynBloomFilter.Insert(const aValue: RawByteString);
begin
  Insert(pointer(aValue),length(aValue));
end;

procedure TSynBloomFilter.Insert(aValue: pointer; aValueLen: integer);
var h: integer;
    h1,h2: cardinal; // https://goo.gl/Pls5wi
begin
  if (self=nil) or (aValueLen<=0) or (fBits=0) then
    exit;
  h1 := crc32c(0,aValue,aValueLen);
  if fHashFunctions=1 then
    h2 := 0 else
    h2 := crc32c(h1,aValue,aValueLen);
  Safe.Lock;
  try
    for h := 0 to fHashFunctions-1 do begin
      SetBitPtr(pointer(fStore),h1 mod fBits);
      inc(h1,h2);
    end;
    inc(fInserted);
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilter.GetInserted: cardinal;
begin
  Safe.Lock;
  try
    result := fInserted; // Delphi 5 does not support LockedInt64[]
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilter.MayExist(const aValue: RawByteString): boolean;
begin
  result := MayExist(pointer(aValue),length(aValue));
end;

function TSynBloomFilter.MayExist(aValue: pointer; aValueLen: integer): boolean;
var h: integer;
    h1,h2: cardinal; // https://goo.gl/Pls5wi
begin
  result := false;
  if (self=nil) or (aValueLen<=0) or (fBits=0) then
    exit;
  h1 := crc32c(0,aValue,aValueLen);
  if fHashFunctions=1 then
    h2 := 0 else
    h2 := crc32c(h1,aValue,aValueLen);
  Safe.Lock;
  try
    for h := 0 to fHashFunctions-1 do
      if GetBitPtr(pointer(fStore),h1 mod fBits) then
        inc(h1,h2) else
        exit;
  finally
    Safe.UnLock;
  end;
  result := true;
end;

procedure TSynBloomFilter.Reset;
begin
  Safe.Lock;
  try
    if fStore='' then
      SetLength(fStore,(fBits shr 3)+1);
    FillcharFast(pointer(fStore)^,length(fStore),0);
    fInserted := 0;
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilter.SaveTo(aMagic: cardinal): RawByteString;
var W: TFileBufferWriter;
    BufLen: integer;
    temp: array[word] of byte;
begin
  BufLen := length(fStore)+100;
  if BufLen<=SizeOf(temp) then
    W := TFileBufferWriter.Create(TRawByteStringStream,@temp,SizeOf(temp)) else
    W := TFileBufferWriter.Create(TRawByteStringStream,BufLen);
  try
    SaveTo(W,aMagic);
    W.Flush;
    result := TRawByteStringStream(W.Stream).DataString;
  finally
    W.Free;
  end;
end;

procedure TSynBloomFilter.SaveTo(aDest: TFileBufferWriter; aMagic: cardinal=$B1003F11);
begin
  aDest.Write4(aMagic);
  aDest.Write1(BLOOM_VERSION);
  Safe.Lock;
  try
    aDest.Write8(fFalsePositivePercent);
    aDest.Write4(fSize);
    aDest.Write4(fBits);
    aDest.Write1(fHashFunctions);
    aDest.Write4(fInserted);
    ZeroCompress(pointer(fStore),Length(fStore),aDest);
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilter.LoadFrom(const aSaved: RawByteString; aMagic: cardinal): boolean;
begin
  result := LoadFrom(pointer(aSaved),length(aSaved));
end;

function TSynBloomFilter.LoadFrom(P: PByte; PLen: integer; aMagic: cardinal): boolean;
var start: PByte;
    version: integer;
begin
  result := false;
  start := P;
  if (P=nil) or (PLen<32) or (PCardinal(P)^<>aMagic) then
    exit;
  inc(P,4);
  version := P^; inc(P);
  if version>BLOOM_VERSION then
    exit;
  Safe.Lock;
  try
    fFalsePositivePercent := unaligned(PDouble(P)^); inc(P,8);
    if (fFalsePositivePercent<=0) or (fFalsePositivePercent>100) then
      exit;
    fSize := PCardinal(P)^; inc(P,4);
    fBits := PCardinal(P)^; inc(P,4);
    if fBits<fSize then
      exit;
    fHashFunctions := P^; inc(P);
    if fHashFunctions-1>=BLOOM_MAXHASH then
      exit;
    Reset;
    fInserted := PCardinal(P)^; inc(P,4);
    ZeroDecompress(P,PLen-(PAnsiChar(P)-PAnsiChar(start)),fStore);
    result := length(fStore)=integer(fBits shr 3)+1;
  finally
    Safe.UnLock;
  end;
end;


{ TSynBloomFilterDiff }

type
  TBloomDiffHeader = packed record
    kind: (bdDiff,bdFull,bdUpToDate);
    size: cardinal;
    inserted: cardinal;
    revision: Int64;
    crc: cardinal;
  end;

procedure TSynBloomFilterDiff.Insert(aValue: pointer; aValueLen: integer);
begin
  Safe.Lock;
  try
    inherited Insert(aValue,aValueLen);
    inc(fRevision);
    inc(fSnapshotInsertCount);
  finally
    Safe.UnLock;
  end;
end;

procedure TSynBloomFilterDiff.Reset;
begin
  Safe.Lock;
  try
    inherited Reset;
    fSnapshotAfterInsertCount := fSize shr 5;
    fSnapShotAfterMinutes := 30;
    fSnapshotTimestamp := 0;
    fSnapshotInsertCount := 0;
    fRevision := UnixTimeUTC shl 31;
    fKnownRevision := 0;
    fKnownStore := '';
  finally
    Safe.UnLock;
  end;
end;

procedure TSynBloomFilterDiff.DiffSnapshot;
begin
  Safe.Lock;
  try
    fKnownRevision := fRevision;
    fSnapshotInsertCount := 0;
    SetString(fKnownStore,PAnsiChar(pointer(fStore)),length(fStore));
    if fSnapShotAfterMinutes=0 then
      fSnapshotTimestamp := 0 else
      fSnapshotTimestamp := GetTickCount64+fSnapShotAfterMinutes*60000;
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilterDiff.SaveToDiff(const aKnownRevision: Int64): RawByteString;
var head: TBloomDiffHeader;
    W: TFileBufferWriter;
    temp: array[word] of byte;
begin
  Safe.Lock;
  try
    if aKnownRevision=fRevision then
      head.kind := bdUpToDate else
    if (fKnownRevision=0) or
       (fSnapshotInsertCount>fSnapshotAfterInsertCount) or
       ((fSnapshotInsertCount>0) and (fSnapshotTimestamp<>0) and
        (GetTickCount64>fSnapshotTimestamp)) then begin
      DiffSnapshot;
      head.kind := bdFull;
    end else
    if (aKnownRevision<fKnownRevision) or (aKnownRevision>fRevision) then
      head.kind := bdFull else
      head.kind := bdDiff;
    head.size := length(fStore);
    head.inserted := fInserted;
    head.revision := fRevision;
    head.crc := crc32c(0,@head,SizeOf(head)-SizeOf(head.crc));
    if head.kind=bdUpToDate then begin
      SetString(result,PAnsiChar(@head),SizeOf(head));
      exit;
    end;
    if head.size+100<=SizeOf(temp) then
      W := TFileBufferWriter.Create(TRawByteStringStream,@temp,SizeOf(temp)) else
      W := TFileBufferWriter.Create(TRawByteStringStream,head.size+100);
    try
      W.Write(@head,SizeOf(head));
      case head.kind of
      bdFull:
        SaveTo(W);
      bdDiff:
        ZeroCompressXor(pointer(fStore),pointer(fKnownStore),head.size,W);
      end;
      W.Flush;
      result := TRawByteStringStream(W.Stream).DataString;
    finally
      W.Free;
    end;
  finally
    Safe.UnLock;
  end;
end;

function TSynBloomFilterDiff.DiffKnownRevision(const aDiff: RawByteString): Int64;
var head: ^TBloomDiffHeader absolute aDiff;
begin
  if (length(aDiff)<SizeOf(head^)) or (head.kind>high(head.kind)) or
     (head.size<>cardinal(length(fStore))) or
     (head.crc<>crc32c(0,pointer(head),SizeOf(head^)-SizeOf(head.crc))) then
    result := 0 else
    result := head.Revision;
end;

function TSynBloomFilterDiff.LoadFromDiff(const aDiff: RawByteString): boolean;
var head: ^TBloomDiffHeader absolute aDiff;
    P: PByte;
    PLen: integer;
begin
  result := false;
  P := pointer(aDiff);
  PLen := length(aDiff);
  if (PLen<SizeOf(head^)) or (head.kind>high(head.kind)) or
     (head.crc<>crc32c(0,pointer(head),SizeOf(head^)-SizeOf(head.crc))) then
    exit;
  if (fStore<>'') and (head.size<>cardinal(length(fStore))) then
    exit;
  inc(P,SizeOf(head^));
  dec(PLen,SizeOf(head^));
  Safe.Lock;
  try
    case head.kind of
    bdFull:
      result := LoadFrom(P,PLen);
    bdDiff:
      if fStore<>'' then
        result := ZeroDecompressOr(pointer(P),Pointer(fStore),PLen,head.size);
    bdUpToDate:
      result := true;
    end;
    if result then begin
      fRevision := head.revision;
      fInserted := head.inserted;
    end;
  finally
    Safe.UnLock;
  end;
end;

procedure ZeroCompress(P: PAnsiChar; Len: integer; Dest: TFileBufferWriter);
var PEnd,beg,zero: PAnsiChar;
    crc: cardinal;
begin
  Dest.WriteVarUInt32(Len);
  PEnd := P+Len;
  beg := P;
  crc := 0;
  while P<PEnd do begin
    while (P^<>#0) and (P<PEnd) do inc(P);
    zero := P;
    while (P^=#0) and (P<PEnd) do inc(P);
    if P-zero>3 then begin
      Len := zero-beg;
      crc := crc32c(crc,beg,Len);
      Dest.WriteVarUInt32(Len);
      Dest.Write(beg,Len);
      Len := P-zero;
      crc := crc32c(crc,@Len,SizeOf(Len));
      Dest.WriteVarUInt32(Len-3);
      beg := P;
    end;
  end;
  Len := P-beg;
  if Len>0 then begin
    crc := crc32c(crc,beg,Len);
    Dest.WriteVarUInt32(Len);
    Dest.Write(beg,Len);
  end;
  Dest.Write4(crc);
end;

procedure ZeroCompressXor(New,Old: PAnsiChar; Len: cardinal; Dest: TFileBufferWriter);
var beg,same,index,crc,L: cardinal;
begin
  Dest.WriteVarUInt32(Len);
  beg := 0;
  index := 0;
  crc := 0;
  while index<Len do begin
    while (New[index]<>Old[index]) and (index<Len) do inc(index);
    same := index;
    while (New[index]=Old[index]) and (index<Len) do inc(index);
    L := index-same;
    if L>3 then begin
      Dest.WriteVarUInt32(same-beg);
      Dest.WriteXor(New+beg,Old+beg,same-beg,@crc);
      crc := crc32c(crc,@L,SizeOf(L));
      Dest.WriteVarUInt32(L-3);
      beg := index;
    end;
  end;
  L := index-beg;
  if L>0 then begin
    Dest.WriteVarUInt32(L);
    Dest.WriteXor(New+beg,Old+beg,L,@crc);
  end;
  Dest.Write4(crc);
end;

procedure ZeroDecompress(P: PByte; Len: integer; {$ifdef FPC}var{$else}out{$endif} Dest: RawByteString);
var PEnd,D,DEnd: PAnsiChar;
    DestLen,crc: cardinal;
begin
  PEnd := PAnsiChar(P)+Len-4;
  DestLen := FromVarUInt32(P);
  SetString(Dest,nil,DestLen); // FPC uses var
  D := pointer(Dest);
  DEnd := D+DestLen;
  crc := 0;
  while PAnsiChar(P)<PEnd do begin
    Len := FromVarUInt32(P);
    if D+Len>DEnd then
      break;
    MoveFast(P^,D^,Len);
    crc := crc32c(crc,D,Len);
    inc(P,Len);
    inc(D,Len);
    if PAnsiChar(P)>=PEnd then
      break;
    Len := FromVarUInt32(P)+3;
    if D+Len>DEnd then
      break;
    FillCharFast(D^,Len,0);
    crc := crc32c(crc,@Len,SizeOf(Len));
    inc(D,Len);
  end;
  if crc<>PCardinal(P)^ then
    Dest := '';
end;

function ZeroDecompressOr(P,Dest: PAnsiChar; Len,DestLen: integer): boolean;
var PEnd,DEnd: PAnsiChar;
    crc: cardinal;
begin
  PEnd := P+Len-4;
  if cardinal(DestLen)<>FromVarUInt32(PByte(P)) then begin
    result := false;
    exit;
  end;
  DEnd := Dest+DestLen;
  crc := 0;
  while (P<PEnd) and (Dest<DEnd) do begin
    Len := FromVarUInt32(PByte(P));
    if Dest+Len>DEnd then
      break;
    crc := crc32c(crc,P,Len);
    OrMemory(pointer(Dest),pointer(P),Len);
    inc(P,Len);
    inc(Dest,Len);
    if P>=PEnd then
      break;
    Len := FromVarUInt32(PByte(P))+3;
    crc := crc32c(crc,@Len,SizeOf(Len));
    inc(Dest,Len);
  end;
  result := crc=PCardinal(P)^;
end;


function Max(a,b: PtrInt): PtrInt; {$ifdef HASINLINE}inline;{$endif}
begin
  if a > b then
    result := a else
    result := b;
end;

function Min(a,b: PtrInt): PtrInt; {$ifdef HASINLINE}inline;{$endif}
begin
  if a < b then
    result := a else
    result := b;
end;

function Comp(a,b: PAnsiChar; len: PtrInt): PtrInt;
{$ifdef HASINLINE} inline;
var lenptr: PtrInt;
begin
  result := 0;
  lenptr := len-SizeOf(PtrInt);
  if lenptr>=0 then
    repeat
      if PPtrInt(a+result)^<>PPtrInt(b+result)^ then
        break;
      inc(result,SizeOf(PtrInt));
    until result>lenptr;
  if result<len then
    repeat
      if a[result]<>b[result] then
        exit;
      inc(result);
    until result=len;
end;
{$else} // eax = a, edx = b, ecx = len
asm // the 'rep cmpsb' version is slower on Intel Core CPU (not AMD)
     or ecx,ecx
     push ebx
     push ecx
     jz @ok
@1:  mov bx,[eax]
     lea eax,[eax+2]
     cmp bl,[edx]
     jne @ok
     dec ecx
     jz @ok
     cmp bh,[edx+1]
     lea edx,[edx+2]
     jne @ok
     dec ecx
     jnz @1
@ok: pop eax
     sub eax,ecx
     pop ebx
end;
{$endif}

function CompReverse(a,b: pointer; len: PtrInt): PtrInt;
begin
  result := 0;
  if len>0 then
    repeat
      if PByteArray(a)[-result]<>PByteArray(b)[-result] then
        exit;
      inc(result);
    until result=len;
end;

procedure movechars(s,d: PAnsiChar; t: PtrUInt);
  {$ifdef HASINLINE}inline;{$endif}
// this code is sometimes used rather than MoveFast() for overlapping copy
begin
  dec(PtrUInt(s), PtrUInt(d));
  inc(t, PtrUInt(d));
  repeat
    d^ := s[PtrUInt(d)];
    inc(d);
  until PtrUInt(d)=t;
end;

function WriteCurOfs(curofs,curlen,curofssize: integer; sp: PAnsiChar): PAnsiChar;
begin
  if curlen=0 then begin
    sp^ := #0;
    inc(sp);
  end else begin
    sp := Pointer(ToVarUInt32(curlen,PByte(sp)));
    PInteger(sp)^ := curofs;
    inc(sp,curofssize);
  end;
  result := sp;
end;

{$ifdef CPUINTEL} // crc32c SSE4.2 hardware accellerated dword hash
function crc32csse42(buf: pointer): cardinal;
{$ifdef CPUX86} {$ifdef FPC} nostackframe; assembler; {$endif}
asm
        mov     edx, eax
        xor     eax, eax
        {$ifdef ISDELPHI2010}
        crc32   eax, dword ptr[edx]
        {$else}
        db $F2, $0F, $38, $F1, $02
        {$endif}
end;
{$else} {$ifdef FPC}nostackframe; assembler; asm {$else}
asm // ecx=buf (Linux: edi=buf)
        .noframe
{$endif FPC}
        xor     eax, eax
        crc32   eax, dword ptr[buf]
end;
{$endif CPUX86}
{$endif CPUINTEL}

function hash32prime(buf: pointer): cardinal;
begin // xxhash32-inspired - and won't pollute L1 cache with lookup tables
  result := PCardinal(buf)^;
  result := result xor (result shr 15);
  result := result * 2246822519;
  result := result xor (result shr 13);
  result := result * 3266489917;
  result := result xor (result shr 16);
end;

const
  HTabBits = 18; // fits well with DeltaCompress(..,BufSize=2MB)
  HTabMask = (1 shl HTabBits)-1; // =$3ffff
  HListMask = $ffffff; // HTab[]=($ff,$ff,$ff)

type
  PHTab = ^THTab; // HTabBits=18 -> SizeOf=767KB
  THTab = packed array[0..HTabMask] of array[0..2] of byte;

function DeltaCompute(NewBuf, OldBuf, OutBuf, WorkBuf: PAnsiChar;
  NewBufSize, OldBufSize, MaxLevel: PtrInt; HList, HTab: PHTab): PAnsiChar;
var i, curofs, curlen, curlevel, match, curofssize, h, oldh: PtrInt;
    sp, pInBuf, pOut: PAnsiChar;
    ofs: cardinal;
    spb: PByte absolute sp;
    hash: function(buf: pointer): cardinal;
begin
  // 1. fill HTab[] with hashes for all old data
  {$ifdef CPUINTEL}
  if cfSSE42 in CpuFeatures then
    hash := @crc32csse42 else
  {$endif}
    hash := @hash32prime;
  FillCharFast(HTab^,SizeOf(HTab^),$ff); // HTab[]=HListMask by default
  pInBuf := OldBuf;
  oldh := -1; // force calculate first hash
  sp := pointer(HList);
  for i := 0 to OldBufSize-3 do begin
    h := hash(pInBuf) and HTabMask;
    inc(pInBuf);
    if h=oldh then
      continue;
    oldh := h;
    h := PtrInt(@HTab^[h]); // fast 24-bit data process
    PCardinal(sp)^ := PCardinal(h)^;
    PCardinal(h)^ := cardinal(i) or (PCardinal(h)^ and $ff000000);
    inc(sp,3);
  end;
  // 2. compression init
  if OldBufSize<=$ffff then
    curofssize := 2 else
    curofssize := 3;
  curlen := -1;
  curofs := 0;
  pOut := OutBuf+7;
  sp := WorkBuf;
  // 3. handle identical leading bytes
  match := Comp(OldBuf,NewBuf,Min(OldBufSize,NewBufSize));
  if match>2 then begin
    sp := WriteCurOfs(0,match,curofssize,sp);
    sp^ := #0; inc(sp);
    inc(NewBuf,match);
    dec(NewBufSize,match);
  end;
  pInBuf := NewBuf;
  // 4. main loop: identify longest sequences using hash, and store reference
  if NewBufSize>=8 then
  repeat
    // hash 4 next bytes from NewBuf, and find longest match in OldBuf
    ofs := PCardinal(@HTab^[hash(NewBuf) and HTabMask])^ and HListMask;
    if ofs<>HListMask then begin // brute force search loop of best hash match
      curlevel := MaxLevel;
      repeat
        with PHash128Rec(OldBuf+ofs)^ do
        {$ifdef CPU64} // test 8 bytes
        if PHash128Rec(NewBuf)^.Lo=Lo then begin
        {$else}
        if (PHash128Rec(NewBuf)^.c0=c0) and (PHash128Rec(NewBuf)^.c1=c1) then begin
        {$endif}
          match := Comp(@PHash128Rec(NewBuf)^.c2,@c2,Min(PtrUInt(OldBufSize)-ofs,NewBufSize)-8);
          if match>curlen then begin // found a longer sequence
            curlen := match;
            curofs := ofs;
          end;
        end;
        dec(curlevel);
        ofs := PCardinal(@HList^[ofs])^ and HListMask;
      until (ofs=HListMask) or (curlevel=0);
    end;
    // curlen = longest sequence length
    if curlen<0 then begin // no sequence found -> copy one byte
      dec(NewBufSize);
      pOut^ := NewBuf^;
      inc(NewBuf);
      inc(pOut);
      if NewBufSize>8 then // >=8 may overflow
        continue else
        break;
    end;
    inc(curlen,8);
    sp := WriteCurOfs(curofs,curlen,curofssize,sp);
    spb := ToVarUInt32(NewBuf-pInBuf,spb);
    inc(NewBuf,curlen); // continue to search after the sequence
    dec(NewBufSize,curlen);
    curlen := -1;
    pInBuf := NewBuf;
    if NewBufSize>8 then // >=8 may overflow
      continue else
      break;
  until false;
  // 5. write remaining bytes
  if NewBufSize>0 then begin
    MoveFast(NewBuf^,pOut^,NewBufSize);
    inc(pOut,NewBufSize);
    inc(newBuf,NewBufSize);
  end;
  sp^ := #0; inc(sp);
  spb := ToVarUInt32(NewBuf-pInBuf,spb);
  // 6. write header
  PInteger(OutBuf)^ := pOut-OutBuf-7;
  h := sp-WorkBuf;
  PInteger(OutBuf+3)^ := h;
  OutBuf[6] := AnsiChar(curofssize);
  // 7. copy commands
  MoveFast(WorkBuf^,pOut^,h);
  result := pOut+h;
end;

function ExtractBuf(GoodCRC: cardinal; p: PAnsiChar; var aUpd, Delta: PAnsiChar;
  Old: PAnsiChar): TDeltaError;
var pEnd, buf, upd, src: PAnsiChar;
    bufsize, datasize, leading, srclen: PtrUInt;
    curofssize: byte;
begin
  // 1. decompression init
  upd := aUpd;
  bufsize :=  PCardinal(p)^ and $00ffffff; inc(p,3);
  datasize := PCardinal(p)^ and $00ffffff; inc(p,3);
  curofssize := ord(p^); inc(p);
  buf := p; inc(p,bufsize);
  pEnd := p+datasize;
  src := nil;
  // 2. main loop
  while p<pEnd do begin
    // src/srclen = sequence to be copied
    srclen := FromVarUInt32(PByte(P));
    if srclen>0 then
      if curofssize=2 then begin
        src := Old+PWord(p)^;
        inc(p,2);
      end else begin
        src := Old+PCardinal(p)^ and $00ffffff;
        inc(p,3);
      end;
    // copy leading bytes
    leading := FromVarUInt32(PByte(P));
    if leading<>0 then begin
      MoveFast(buf^,upd^,leading);
      inc(buf,leading);
      inc(upd,leading);
    end;
    // copy sequence
    if srclen<>0 then begin
      if PtrUInt(upd-src)<srclen then
        movechars(src,upd,srclen) else
        MoveFast(src^,upd^,srclen);
      inc(upd,srclen);
    end;
  end;
  // 3. result check
  Delta := p;
  if (p=pEnd) and (crc32c(0,aUpd,upd-aUpd)=GoodCRC) then // whole CRC is faster
    result := dsSuccess else
    result := dsCrcExtract;
  aUpd := upd;
end;

procedure WriteByte(var P: PAnsiChar; V: Byte); {$ifdef HASINLINE}inline;{$endif}
begin
  PByte(P)^ := V;
  inc(P);
end;

procedure WriteInt(var P: PAnsiChar; V: Cardinal); {$ifdef HASINLINE}inline;{$endif}
begin
  PCardinal(P)^ := V;
  inc(P,4);
end;

const
  FLAG_COPIED   = 0;
  FLAG_COMPRESS = 1;
  FLAG_BEGIN    = 2;
  FLAG_END      = 3;

function DeltaCompress(New, Old: PAnsiChar; NewSize, OldSize: integer;
  out Delta: PAnsiChar; Level, BufSize: integer): integer;
var HTab, HList: PHTab;
    d, workbuf: PAnsiChar;
    db: PByte absolute d;
    BufRead, OldRead, Trailing, NewSizeSave: PtrInt;
    bigfile: boolean;
  procedure CreateCopied;
  begin
    Getmem(Delta,NewSizeSave+17);  // 17 = 4*Integer + 1*Byte
    d := Delta;
    db := ToVarUInt32(0,ToVarUInt32(NewSizeSave,db));
    WriteByte(d,FLAG_COPIED); // block copied flag
    db := ToVarUInt32(NewSizeSave,db);
    WriteInt(d,crc32c(0,New,NewSizeSave));
    MoveFast(New^,d^,NewSizeSave);
    inc(d,NewSizeSave);
    result := d-Delta;
  end;
begin
  // 1. special cases
  if (NewSize=OldSize) and CompareMem(Old,New,NewSize) then begin
    Getmem(Delta,1);
    Delta^ := '=';
    result := 1;
    exit;
  end;
  NewSizeSave := NewSize;
  if OldSize=0 then begin // Delta from nothing -> direct copy of whole block
    CreateCopied;
    exit;
  end;
  // 2. compression init
  bigfile := OldSize>BufSize;
  if BufSize>NewSize then
    BufSize := NewSize;
  if BufSize>$ffffff then
    BufSize := $ffffff; // we store offsets with 2..3 bytes -> max 16MB chunk
  Trailing := 0;
  Getmem(workbuf,BufSize); // compression temporary buffers
  Getmem(HList,BufSize*SizeOf(HList[0]));
  Getmem(HTab,SizeOf(HTab^));
  Getmem(Delta,Max(NewSize,OldSize)+4096); // Delta size max evalulation
  try
    d := Delta;
    db := ToVarUInt32(NewSize,db); // Destination Size
    // 3. handle leading and trailing identical bytes (for biggest files)
    if bigfile then begin
      BufRead := Comp(New,Old,Min(NewSize,OldSize));   // test 1st same chars
      if BufRead>9 then begin // it happens very often
        db := ToVarUInt32(BufRead,db); // blockSize = Size BufIdem
        WriteByte(d,FLAG_BEGIN);
        WriteInt(d,crc32c(0,New,BufRead));
        inc(New,BufRead);
        dec(NewSize,BufRead);
        inc(Old,BufRead);
        dec(OldSize,BufRead);
      end;                                             // test last same chars
      BufRead := CompReverse(New+NewSize-1,Old+OldSize-1,Min(NewSize,OldSize));
      if BufRead>5 then begin
        if NewSize=BufRead then
          dec(BufRead); // avoid block overflow
        dec(OldSize,BufRead);
        dec(NewSize,BufRead);
        Trailing := BufRead;
      end;
    end;
    // 4. main loop
    repeat
      BufRead := Min(BufSize,NewSize);
      dec(NewSize,BufRead);
      if (BufRead=0) and (Trailing>0) then begin
        db := ToVarUInt32(Trailing,db);
        WriteByte(d,FLAG_END); // block idem end flag -> BufRead := 0 not necessary
        WriteInt(d,crc32c(0,New,Trailing));
        break;
      end;
      OldRead := Min(BufSize,OldSize);
      dec(OldSize,OldRead);
      db := ToVarUInt32(OldRead,db);
      If (BufRead<4) or (OldRead<4) or (BufRead div 4>OldRead) then begin
        WriteByte(d,FLAG_COPIED); // block copied flag
        db := ToVarUInt32(BufRead,db);
        if BufRead=0 then
          break;
        WriteInt(d,crc32c(0,New,BufRead));
        MoveFast(New^,d^,BufRead);
        inc(New,BufRead);
        inc(d,BufRead);
      end else begin
        WriteByte(d,FLAG_COMPRESS); // block compressed flag
        WriteInt(d,crc32c(0,New,BufRead));
        WriteInt(d,crc32c(0,Old,OldRead));
        d := DeltaCompute(New,Old,d,workbuf,BufRead,OldRead,Level,HList,HTab);
        inc(New,BufRead);
        inc(Old,OldRead);
      end;
    until false;
  // 5. release temp memory
  finally
    result := d-Delta;
    Freemem(HTab);
    Freemem(HList);
    Freemem(workbuf);
  end;
  if result>=NewSizeSave+17 then begin
    // Delta didn't compress well -> store it (with 17 bytes overhead)
    Freemem(Delta);
    CreateCopied;
  end;
end;

function DeltaCompress(const New, Old: RawByteString;
  Level, BufSize: integer): RawByteString;
begin
  result := DeltaCompress(pointer(New),pointer(Old),
    length(New),length(Old),Level,BufSize);
end;

function DeltaCompress(New, Old: PAnsiChar; NewSize, OldSize: integer;
  Level, BufSize: integer): RawByteString;
var Delta: PAnsiChar;
    DeltaLen: integer;
begin
  DeltaLen := DeltaCompress(New,Old,Newsize,OldSize,Delta,Level,BufSize);
  SetString(result,Delta,DeltaLen);
  Freemem(Delta);
end;

function DeltaExtract(Delta,Old,New: PAnsiChar): TDeltaError;
var BufCRC: Cardinal;
    Code: Byte;
    Len, BufRead, OldRead: PtrInt;
    db: PByte absolute Delta;
    Upd: PAnsiChar;
begin
  Result := dsSuccess;
  Len := FromVarUInt32(db);
  Upd := New;
  repeat
    OldRead := FromVarUInt32(db);
    Code := db^; inc(db);
    Case Code of
    FLAG_COPIED: begin // block copied flag - copy new from Delta
      BufRead := FromVarUInt32(db);
      If BufRead=0 then
        break;
      If crc32c(0,Delta+4,BufRead)<>PCardinal(Delta)^ then begin
        result := dsCrcCopy;
        exit;
      end;
      inc(Delta,4);
      MoveFast(Delta^,New^,BufRead);
      if BufRead>=Len then
        exit; // if Old=nil -> only copy new
      inc(Delta,BufRead);
      inc(New,BufRead);
    end;
    FLAG_COMPRESS: begin // block compressed flag - extract Delta from Old
      BufCRC := PCardinal(Delta)^; inc(Delta,4);
      if crc32c(0,Old,OldRead)<>PCardinal(Delta)^ then begin
        result := dsCrcComp;
        exit;
      end;
      inc(Delta,4);
      result := ExtractBuf(BufCRC,Delta,New,Delta,Old);
      if result<>dsSuccess then
        exit;
    end;
    FLAG_BEGIN: begin // block idem begin flag
      if crc32c(0,Old,OldRead)<>PCardinal(Delta)^ then begin
        result := dsCrcBegin;
        exit;
      end;
      inc(Delta,4);
      MoveFast(Old^,New^,OldRead);
      inc(New,OldRead);
    end;
    FLAG_END: begin // block idem end flag
      if crc32c(0,Old,OldRead)<>PCardinal(Delta)^ then
        Result := dsCrcEnd;
      MoveFast(Old^,New^,OldRead);
      inc(New,OldRead);
      break;
    end;
    else begin
      result := dsFlag;
      exit;
    end;
    end; // Case Code of
    inc(Old,OldRead);
  until false;
  if New-Upd<>Len then
    result := dsLen;
end;

function DeltaExtract(const Delta,Old: RawByteString; out New: RawByteString): TDeltaError;
begin
  if (Delta='') or (Delta='=') then begin
    New := Old;
    result := dsSuccess;
  end else begin
    SetLength(New,DeltaExtractSize(pointer(Delta)));
    result := DeltaExtract(pointer(Delta),pointer(Old),pointer(New));
  end;
end;

function DeltaExtractSize(const Delta: RawByteString): integer;
begin
  result := DeltaExtractSize(pointer(Delta));
end;

function DeltaExtractSize(Delta: PAnsiChar): integer;
begin
  if Delta=nil then
    result := 0 else
    result := FromVarUInt32(PByte(Delta));
end;

function ToText(err: TDeltaError): PShortString;
begin
  result := GetEnumName(TypeInfo(TDeltaError),ord(err));
end;


{ TFastReader }

procedure TFastReader.Init(Buffer: pointer; Len: integer);
begin
  P := Buffer;
  Last := P+Len;
  OnErrorOverflow := nil;
  OnErrorData := nil;
  Tag := 0;
end;

procedure TFastReader.Init(const Buffer: RawByteString);
begin
  Init(pointer(Buffer),length(Buffer));
end;

procedure TFastReader.ErrorOverflow;
begin
  if Assigned(OnErrorOverflow) then
    OnErrorOverflow else
    raise EFastReader.Create('Reached End of Input');
end;

procedure TFastReader.ErrorData(const fmt: RawUTF8; const args: array of const);
begin
  if Assigned(OnErrorData) then
    OnErrorData(fmt,args) else
    raise EFastReader.CreateUTF8('Incorrect Data: '+fmt,args);
end;

function TFastReader.EOF: boolean;
begin
  result := P>=Last;
end;

function TFastReader.RemainingLength: PtrUInt;
begin
  result := PtrUInt(Last)-PtrUInt(P);
end;

function TFastReader.NextByte: byte;
begin
  if P>=Last then
    ErrorOverflow;
  result := ord(P^);
  inc(P);
end;

function TFastReader.NextByteSafe(dest: pointer): boolean;
begin
  if P>=Last then
    result := false
  else begin
    PAnsiChar(dest)^ := P^;
    inc(P);
    result := true;
  end;
end;

function TFastReader.Next4: cardinal;
begin
  if P+3>=Last then
    ErrorOverflow;
  result := PCardinal(P)^;
  inc(P,4);
end;

function TFastReader.Next8: QWord;
begin
  if P+7>=Last then
    ErrorOverflow;
  result := PQWord(P)^;
  inc(P,8);
end;

function TFastReader.NextByteEquals(Value: byte): boolean;
begin
  if P>=Last then
    ErrorOverflow;
  if ord(P^) = Value then begin
    inc(P);
    result := true;
  end else
    result := false;
end;

function TFastReader.Next(DataLen: PtrInt): pointer;
begin
  if P+DataLen>Last then
    ErrorOverflow;
  result := P;
  inc(P,DataLen);
end;

function TFastReader.NextSafe(out Data: Pointer; DataLen: PtrInt): boolean;
begin
  if P+DataLen>Last then
    result := false else begin
    Data := P;
    inc(P,DataLen);
    result := true;
  end;
end;

procedure TFastReader.Copy(out Dest; DataLen: PtrInt);
begin
  if P+DataLen>Last then
    ErrorOverflow;
  MoveFast(P^,Dest,DataLen);
  inc(P,DataLen);
end;

function TFastReader.CopySafe(out Dest; DataLen: PtrInt): boolean;
begin
  if P+DataLen>Last then
    result := false else begin
    MoveFast(P^,Dest,DataLen);
    inc(P,DataLen);
    result := true;
  end;
end;

procedure TFastReader.VarBlob(out result: TValueResult);
begin
  result.Len := VarUInt32;
  if P+result.Len>Last then
    ErrorOverflow;
  result.Ptr := P;
  inc(P,result.Len);
end;

function TFastReader.VarBlob: TValueResult;
begin
  result.Len := VarUInt32;
  if P+result.Len>Last then
    ErrorOverflow;
  result.Ptr := P;
  inc(P,result.Len);
end;

{$ifndef NOVARIANTS}
procedure TFastReader.NextVariant(var Value: variant;
  CustomVariantOptions: PDocVariantOptions);
begin
  P := VariantLoad(Value,P,CustomVariantOptions,Last);
  if P=nil then
    ErrorData('VariantLoad=nil',[]) else
  if P>Last then
    ErrorOverFlow;
end;

procedure TFastReader.NextDocVariantData(out Value: variant;
  CustomVariantOptions: PDocVariantOptions);
var json: TValueResult;
    temp: TSynTempBuffer;
begin
  VarBlob(json);
  if json.Len<=0 then
    exit;
  temp.Init(json.Ptr,json.Len); // parsing will modify input buffer in-place
  try
    if CustomVariantOptions=nil then
      CustomVariantOptions := @JSON_OPTIONS[true];
    TDocVariantData(Value).InitJSONInPlace(temp.buf,CustomVariantOptions^);
  finally
    temp.Done;
  end;
end;
{$endif NOVARIANTS}

function TFastReader.VarInt32: integer;
begin
  result := VarUInt32;
  if result and 1<>0 then
    // 1->1, 3->2..
    result := result shr 1+1 else
    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
end;

function TFastReader.VarInt64: Int64;
begin
  result := VarUInt64;
  if result and 1<>0 then
    // 1->1, 3->2..
    result := result shr 1+1 else
    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
end;

function TFastReader.VarUInt32: cardinal;
var c: cardinal;
{$ifdef CPUX86} // not enough CPU registers
label err;
begin
  result := ord(P^);
  if P>=Last then
    goto err;
  inc(P);
  if result<=$7f then
    exit;
  if P>=Last then
    goto err;
  c := ord(P^) shl 7;
  inc(P);
  result := result and $7F or c;
  if c<=$7f shl 7 then
    exit; // Values between 128 and 16256
  if P>=Last then
    goto err;
  c := ord(P^) shl 14;
  inc(P);
  result := result and $3FFF or c;
  if c<=$7f shl 14 then
    exit; // Values between 16257 and 2080768
  if P>=Last then
    goto err;
  c := ord(P^) shl 21;
  inc(P);
  result := result and $1FFFFF or c;
  if c<=$7f shl 21 then
    exit; // Values between 2080769 and 266338304
  if P>=Last then
err:ErrorOverflow;
  c := ord(P^) shl 28;
  inc(P);
  result := result and $FFFFFFF or c;
{$else}
    s,l: PByte;
label err,fin;
begin
  s := pointer(P);
  l := pointer(Last);
  result := s^;
  if PAnsiChar(s)>=PAnsiChar(l) then
    goto err;
  inc(s);
  if result<=$7f then
    goto fin;
  if PAnsiChar(s)>=PAnsiChar(l) then
    goto err;
  c := s^ shl 7;
  inc(s);
  result := result and $7F or c;
  if c<=$7f shl 7 then
    goto fin; // Values between 128 and 16256
  if PAnsiChar(s)>=PAnsiChar(l) then
    goto err;
  c := s^ shl 14;
  inc(s);
  result := result and $3FFF or c;
  if c<=$7f shl 14 then
    goto fin; // Values between 16257 and 2080768
  if PAnsiChar(s)>=PAnsiChar(l) then
    goto err;
  c := s^ shl 21;
  inc(s);
  result := result and $1FFFFF or c;
  if c<=$7f shl 21 then
    goto fin; // Values between 2080769 and 266338304
  if PAnsiChar(s)>=PAnsiChar(l) then
err:ErrorOverflow;
  c := s^ shl 28;
  inc(s);
  result := result and $FFFFFFF or c;
fin:
  P := pointer(s);
{$endif}
end;

procedure TFastReader.VarNextInt;
{$ifdef CPUX86} // not enough CPU registers
begin
  repeat
    if P>=Last then
      break;  // reached end of input
    if P^<=#$7f then
      break; // reached end of VarUInt32/VarUInt64
    inc(P);
  until false;
  inc(P);
{$else}
var s: PAnsiChar;
begin
  s := P;
  repeat
    if s>=Last then
      break;  // reached end of input
    if s^<=#$7f then
      break; // reached end of VarUInt32/VarUInt64
    inc(s);
  until false;
  P := s+1;
{$endif CPUX86}
end;

procedure TFastReader.VarNextInt(count: integer);
{$ifdef CPUX86} // not enough CPU registers
begin
  if count=0 then
    exit;
  repeat
    if P>=Last then
      break;  // reached end of input
    if P^>#$7f then begin
      inc(P);
      continue; // didn't reach end of VarUInt32/VarUInt64
    end;
    inc(P);
    dec(count);
    if count=0 then
      break;
  until false;
{$else}
var s, max: PAnsiChar;
begin
  if count=0 then
    exit;
  s := P;
  max := Last;
  repeat
    if s>=max then
      break;  // reached end of input
    if s^>#$7f then begin
      inc(s);
      continue; // didn't reach end of VarUInt32/VarUInt64
    end;
    inc(s);
    dec(count);
    if count=0 then
      break;
  until false;
  P := s;
{$endif CPUX86}
end;

function TFastReader.PeekVarInt32(out value: PtrInt): boolean;
begin
  result := PeekVarUInt32(PtrUInt(value));
  if result then
    if value and 1<>0 then
      // 1->1, 3->2..
      value := value shr 1+1 else
      // 0->0, 2->-1, 4->-2..
      value := -(value shr 1);
end;

function TFastReader.PeekVarUInt32(out value: PtrUInt): boolean;
var s: PAnsiChar;
begin
  result := false;
  s := P;
  repeat
    if s>=Last then
      exit;  // reached end of input -> returns false
    if s^<=#$7f then
      break; // reached end of VarUInt32
    inc(s);
  until false;
  s := P;
  value := VarUInt32; // fast value decode
  P := s; // rewind
  result := true;
end;

function TFastReader.VarUInt32Safe(out Value: cardinal): boolean;
var c, n, v: cardinal;
begin
  result := false;
  if P>=Last then
    exit;
  v := ord(P^);
  inc(P);
  if v>$7f then begin
    n := 0;
    v := v and $7F;
    repeat
      if P>=Last then
        exit;
      c := ord(P^);
      inc(P);
      inc(n,7);
      if c<=$7f then break;
      v := v or ((c and $7f) shl n);
    until false;
    v := v or (c shl n);
  end;
  Value := v;
  result := true; // success
end;

function TFastReader.VarUInt64: QWord;
label err;
var c, n: PtrUInt;
begin
  if P>=Last then
err: ErrorOverflow;
  c := ord(P^);
  inc(P);
  if c>$7f then begin
    result := c and $7F;
    n := 0;
    repeat
      if P>=Last then
        goto err;
      c := ord(P^);
      inc(P);
      inc(n,7);
      if c<=$7f then break;
      result := result or (QWord(c and $7f) shl n);
    until false;
    result := result or (QWord(c) shl n);
  end else
    result := c;
end;

function TFastReader.VarString: RawByteString;
begin
  with VarBlob do
    SetString(result,Ptr,Len);
end;

procedure TFastReader.VarUTF8(out result: RawUTF8);
begin
  with VarBlob do
    FastSetString(result,Ptr,Len);
end;

function TFastReader.VarUTF8: RawUTF8;
begin
  with VarBlob do
    FastSetString(result,Ptr,Len);
end;

function TFastReader.VarShortString: shortstring;
begin
  with VarBlob do
    SetString(result,Ptr,Len);
end;

function TFastReader.VarUTF8Safe(out Value: RawUTF8): boolean;
var len: cardinal;
begin
  if VarUInt32Safe(len) then
    if len=0 then
      result := true else
      if P+len<=Last then begin
        FastSetString(Value,P,len);
        inc(P,len);
        result := true;
      end else
        result := false else
    result := false;
end;

procedure TFastReader.Read(var DA: TDynArray; NoCheckHash: boolean);
begin
  P := DA.LoadFrom(P,nil,NoCheckHash,Last);
  if P=nil then
    ErrorData('TDynArray.LoadFrom %',[DA.ArrayTypeShort^]);
end;

function TFastReader.ReadVarUInt32Array(var Values: TIntegerDynArray): PtrInt;
var i: integer;
    k: TFileBufferWriterKind;
begin
  result := VarUInt32;
  SetLength(Values,result);
  Copy(k,1);
  if k=wkUInt32 then begin
    Copy(Values[0],result*4);
    exit;
  end;
  Next(4); // format: Isize+varUInt32s
  case k of
  wkVarInt32:  for i := 0 to result-1 do Values[i] := VarInt32;
  wkVarUInt32: for i := 0 to result-1 do Values[i] := VarUInt32;
  else ErrorData('ReadVarUInt32Array: unhandled kind=%', [ord(k)]);
  end;
end;

function TFastReader.ReadCompressed(Load: TAlgoCompressLoad; BufferOffset: integer): RawByteString;
var comp: PAnsiChar;
    complen: PtrUInt;
begin
  complen := VarUInt32;
  comp := Next(complen);
  TAlgoCompress.Algo(comp,complen).Decompress(comp,complen,result,Load,BufferOffset);
end;


{ TSynTempWriter }

procedure TSynTempWriter.Init(maxsize: integer);
begin
  if maxsize<=0 then
    pos := tmp.InitOnStack else
    pos := tmp.Init(maxsize);
end;

procedure TSynTempWriter.Done;
begin
  tmp.Done;
end;

function TSynTempWriter.AsBinary: RawByteString;
begin
  FastSetStringCP(result,tmp.buf,pos-PAnsiChar(tmp.buf),CP_RAWBYTESTRING);
end;

procedure TSynTempWriter.AsUTF8(var result: RawUTF8);
begin
  FastSetString(result,tmp.buf,pos-PAnsiChar(tmp.buf));
end;

function TSynTempWriter.Position: PtrInt;
begin
  result := pos-PAnsiChar(tmp.buf);
end;

procedure TSynTempWriter.wr(const val; len: PtrInt);
begin
  if len<=0 then
    exit;
  if pos-PAnsiChar(tmp.buf)+len>tmp.len then
     raise ESynException.CreateUTF8('TSynTempWriter(%) overflow',[tmp.len]);
  MoveSmall(@val,pos,len);
  inc(pos,len);
end;

procedure TSynTempWriter.wrb(b: byte);
begin
  wr(b,1);
end;

procedure TSynTempWriter.wrint(int: integer);
begin
  wr(int,4);
end;

procedure TSynTempWriter.wrptrint(int: PtrInt);
begin
  wr(int,SizeOf(int));
end;

procedure TSynTempWriter.wrptr(ptr: pointer);
begin
  wr(ptr,SizeOf(ptr));
end;

procedure TSynTempWriter.wrss(const str: shortstring);
begin
  wr(str,ord(str[0])+1);
end;

procedure TSynTempWriter.wrs(const str: RawByteString);
begin
  if str<>'' then
    wr(pointer(str),length(str));
end;

procedure TSynTempWriter.wrw(w: word);
begin
  wr(w,2);
end;

function TSynTempWriter.wrfillchar(count: integer; value: byte): PAnsiChar;
begin
  if pos-PAnsiChar(tmp.buf)+count>tmp.len then
     raise ESynException.CreateUTF8('TSynTempWriter(%) overflow',[tmp.len]);
  FillCharFast(pos^,count,value);
  result := pos;
  inc(pos,count);
end;



{ TFileBufferWriter }

constructor TFileBufferWriter.Create(aFile: THandle; BufLen: integer);
begin
  Create(THandleStream.Create(aFile),BufLen);
  fInternalStream := true;
end;

constructor TFileBufferWriter.Create(const aFileName: TFileName; BufLen: integer;
  Append: boolean);
var s: TStream;
begin
  if Append and FileExists(aFileName) then begin
    s := TFileStream.Create(aFileName,fmOpenWrite);
    s.Seek(0,soEnd);
  end else
    s := TFileStream.Create(aFileName,fmCreate);
  Create(s,BufLen);
  fInternalStream := true;
end;

constructor TFileBufferWriter.Create(aStream: TStream; BufLen: integer);
begin
  if BufLen>1 shl 22 then
    fBufLen := 1 shl 22 else // 4 MB sounds right enough
  if BufLen<32 then
    fBufLen := 32;
    fBufLen := BufLen;
  fStream := aStream;
  SetLength(fBufInternal,fBufLen);
  fBuffer := pointer(fBufInternal);
end;

constructor TFileBufferWriter.Create(aClass: TStreamClass; BufLen: integer);
begin
  Create(aClass.Create,BufLen);
  fInternalStream := true;
end;

constructor TFileBufferWriter.Create(aStream: TStream; aTempBuf: pointer; aTempLen: integer);
begin
  fBufLen := aTempLen;
  fBuffer := aTempBuf;
  fStream := aStream;
end;

constructor TFileBufferWriter.Create(aClass: TStreamClass; aTempBuf: pointer; aTempLen: integer);
begin
  Create(aClass.Create,aTempBuf,aTempLen);
  fInternalStream := true;
end;

destructor TFileBufferWriter.Destroy;
begin
  if fInternalStream then
    fStream.Free;
  inherited;
end;

procedure TFileBufferWriter.InternalFlush;
begin
  fStream.WriteBuffer(fBuffer^,fPos);
  fPos := 0;
end;

function TFileBufferWriter.Flush: Int64;
begin
  if fPos>0 then
    InternalFlush;
  result := fTotalWritten;
  fTotalWritten := 0;
end;

procedure TFileBufferWriter.CancelAll;
begin
  fTotalWritten := 0;
  fPos := 0;
  if fStream.ClassType = TRawByteStringStream then
    TRawByteStringStream(fStream).Size := 0 else
    fStream.Seek(0,soBeginning);
end;

procedure TFileBufferWriter.Write(Data: pointer; DataLen: PtrInt);
begin
  if (DataLen<=0) or (Data=nil) then
    exit;
  inc(fTotalWritten,DataLen);
  if fPos+DataLen>fBufLen then begin
    if fPos>0 then
      InternalFlush;
    if DataLen>fBufLen then begin
      fStream.WriteBuffer(Data^,DataLen);
      exit;
    end;
  end;
  MoveFast(Data^,fBuffer^[fPos],DataLen);
  inc(fPos,DataLen);
end;

procedure TFileBufferWriter.WriteN(Data: Byte; Count: integer);
var len: integer;
begin
  inc(fTotalWritten,Count);
  while Count>0 do begin
    if Count>fBufLen then
      len := fBufLen else
      len := Count;
    if fPos+len>fBufLen then
      InternalFlush;
    FillCharFast(fBuffer^[fPos],len,Data);
    inc(fPos,len);
    dec(Count,len);
  end;
end;

procedure TFileBufferWriter.Write1(Data: Byte);
begin
  if fPos+1>fBufLen then
    InternalFlush;
  fBuffer^[fPos] := Data;
  inc(fPos);
  inc(fTotalWritten);
end;

procedure TFileBufferWriter.Write2(Data: Word);
begin
  if fPos+2>fBufLen then
    InternalFlush;
  PWord(@fBuffer^[fPos])^ := Data;
  inc(fPos,SizeOf(Word));
  inc(fTotalWritten,SizeOf(Word));
end;

procedure TFileBufferWriter.Write4(Data: integer);
begin
  if fPos+4>fBufLen then
    InternalFlush;
  PInteger(@fBuffer^[fPos])^ := Data;
  inc(fPos,SizeOf(integer));
  inc(fTotalWritten,SizeOf(integer));
end;

procedure TFileBufferWriter.Write4BigEndian(Data: integer);
begin
  Write4(bswap32(Data));
end;

procedure TFileBufferWriter.Write8(const Data8Bytes);
begin
  if fPos+8>fBufLen then
    InternalFlush;
  PInt64(@fBuffer^[fPos])^ := PInt64(@Data8Bytes)^;
  inc(fPos,SizeOf(Int64));
  inc(fTotalWritten,SizeOf(Int64));
end;

procedure TFileBufferWriter.Write(const Text: RawByteString);
var L: integer;
begin
  L := length(Text);
  if L=0 then
    Write1(0) else begin
    WriteVarUInt32(L);
    Write(pointer(Text),L);
  end;
end;

procedure TFileBufferWriter.WriteShort(const Text: ShortString);
var L: integer;
begin
  L := ord(Text[0]);
  if L<$80 then
    Write(@Text[0],L+1) else begin
    WriteVarUInt32(L);
    Write(@Text[1],L);
  end;
end;

procedure TFileBufferWriter.WriteBinary(const Data: RawByteString);
begin
  Write(pointer(Data),Length(Data));
end;

function TFileBufferWriter.DirectWritePrepare(len: PtrInt; out tmp: RawByteString): PAnsiChar;
begin
  if (len<=fBufLen) and (fPos+len>fBufLen) then
    InternalFlush;
  if fPos+len>fBufLen then begin
    SetLength(tmp,len);
    result := pointer(tmp);
  end else
    result := @fBuffer^[fPos]; // write directly into the buffer
end;

procedure TFileBufferWriter.DirectWriteFlush(len: PtrInt; const tmp: RawByteString);
begin
  if tmp='' then begin
    inc(fPos,len);
    inc(fTotalWritten,len);
  end else
    Write(pointer(tmp),len);
end;

procedure TFileBufferWriter.WriteRecord(const Rec; RecTypeInfo: pointer);
var len: integer;
    tmp: RawByteString;
    P: PAnsiChar;
begin
  len := RecordSaveLength(Rec,RecTypeInfo);
  P := DirectWritePrepare(len,tmp);
  if RecordSave(Rec,P,RecTypeInfo)-P<>len then
    raise ESynException.CreateUTF8('%.WriteRecord: RecordSave?',[self]);
  DirectWriteFlush(len,tmp);
end;

procedure TFileBufferWriter.WriteDynArray(const DA: TDynArray);
var len: integer;
    tmp: RawByteString;
    P: PAnsiChar;
begin
  len := DA.SaveToLength;
  P := DirectWritePrepare(len,tmp);
  if DA.SaveTo(P)-P<>len then
    raise ESynException.CreateUTF8('%.WriteDynArray: DA.SaveTo?',[self]);
  DirectWriteFlush(len,tmp);
end;

{$ifndef NOVARIANTS}
procedure TFileBufferWriter.Write(const Value: variant);
var buf: PAnsiChar;
    vt,len: integer;
    tmp: RawByteString;
begin
  vt := TVarData(Value).VType;
  if vt>varAny then begin // avoid VariantSaveLength() call
    Write(@TVarData(Value).VType,SizeOf(TVarData(Value).VType));
    tmp := VariantSaveJSON(Value);
    Write(tmp);
    exit;
  end;
  len := VariantSaveLength(Value);
  if len=0 then
    raise ESynException.CreateUTF8('%.Write(VType=%) VariantSaveLength=0',[self,vt]);
  buf := DirectWritePrepare(len,tmp);
  if VariantSave(Value,buf)=nil then
    raise ESynException.CreateUTF8('%.Write(VType=%) VariantSave=nil',[self,vt]);
  DirectWriteFlush(len,tmp);
end;

procedure TFileBufferWriter.WriteDocVariantData(const Value: variant);
begin
  with _Safe(Value)^ do
    if Count=0 then
      Write1(0) else
      Write(ToJSON);
end;

{$endif NOVARIANTS}

procedure TFileBufferWriter.WriteXor(New,Old: PAnsiChar; Len: PtrInt; crc: PCardinal);
var L: integer;
    Dest: PAnsiChar;
begin
  if (New=nil) or (Old=nil) then
    exit;
  inc(fTotalWritten,Len);
  while Len>0 do begin
    Dest := pointer(fBuffer);
    if fPos+Len>fBufLen then
      InternalFlush else
      inc(Dest,fPos);
    if Len>fBufLen then
      L := fBufLen else
      L := Len;
    XorMemory(pointer(Dest),pointer(New),pointer(Old),L);
    if crc<>nil then
      crc^ := crc32c(crc^,Dest,L);
    inc(Old,L);
    inc(New,L);
    dec(Len,L);
    inc(fPos,L);
  end;
end;

procedure TFileBufferWriter.WriteRawUTF8DynArray(const Values: TRawUTF8DynArray;
  ValuesCount: integer);
begin
  WriteRawUTF8Array(pointer(Values),ValuesCount);
end;

procedure TFileBufferWriter.WriteRawUTF8Array(Values: PPtrUIntArray; ValuesCount: integer);
var n, i: integer;
    fixedsize, len: PtrUInt;
    P, PEnd: PByte;
    PBeg: PAnsiChar;
begin
  WriteVarUInt32(ValuesCount);
  if ValuesCount=0 then
    exit;
  fixedsize := Values^[0];
  if fixedsize<>0 then begin
    fixedsize := PStrLen(fixedsize-_STRLEN)^;
    for i := 1 to ValuesCount-1 do
      if (Values^[i]=0) or (PStrLen(Values^[i]-_STRLEN)^<>TStrLen(fixedsize)) then begin
        fixedsize := 0;
        break;
      end;
  end;
  WriteVarUInt32(fixedsize);
  repeat
    P := @fBuffer^[fPos];
    PEnd := @fBuffer^[fBufLen-8];
    if PtrUInt(P)<PtrUInt(PEnd) then begin
      n := ValuesCount;
      PBeg := PAnsiChar(P); // leave space for chunk size
      inc(P,4);
      if fixedsize=0 then
        for i := 0 to ValuesCount-1 do
          if Values^[i]=0 then begin
            P^ := 0; // store length=0
            inc(P);
            if PtrUInt(P)>=PtrUInt(PEnd) then begin
              n := i+1;
              break; // avoid buffer overflow
            end;
          end else begin
            len := PStrLen(Values^[i]-_STRLEN)^;
            if PtrUInt(PEnd)-PtrUInt(P)<=len then begin
              n := i;
              break; // avoid buffer overflow
            end;
            P := ToVarUInt32(len,P);
            MoveFast(pointer(Values^[i])^,P^,len); // here len>0
            inc(P,len);
          end else // fixedsize<>0:
        for i := 0 to ValuesCount-1 do begin
          if PtrUInt(PEnd)-PtrUInt(P)<=fixedsize then begin
            n := i;
            break; // avoid buffer overflow
          end;
          MoveFast(pointer(Values^[i])^,P^,fixedsize);
          inc(P,fixedsize);
        end;
      len := PAnsiChar(P)-PBeg; // format: Isize+varUInt32s*strings
      PInteger(PBeg)^ := len-4;
      inc(fTotalWritten,len);
      inc(fPos,len);
      inc(PByte(Values),n*SizeOf(PtrInt));
      dec(ValuesCount,n);
      if ValuesCount=0 then
        break;
    end;
    InternalFlush;
  until false;
end;

procedure TFileBufferWriter.WriteRawUTF8List(List: TRawUTF8List;
  StoreObjectsAsVarUInt32: Boolean);
var i: integer;
    o: PPointerArray;
begin
  if List=nil then
    WriteVarUInt32(0) else begin
    List.Safe.Lock;
    try
      WriteRawUTF8Array(pointer(List.TextPtr),List.Count);
      o := List.ObjectPtr;
      if o=nil then
        StoreObjectsAsVarUInt32 := false; // no Objects[] values
      Write(@StoreObjectsAsVarUInt32,1);
      if StoreObjectsAsVarUInt32 then
        for i := 0 to List.Count-1 do
          WriteVarUInt32(PtrUInt(o[i]));
    finally
      List.Safe.UnLock;
    end;
  end;
end;

procedure TFileBufferWriter.WriteStream(aStream: TCustomMemoryStream;
  aStreamSize: Integer);
begin
  if aStreamSize<0 then
    if aStream=nil then
      aStreamSize := 0 else
      aStreamSize := aStream.Size;
  WriteVarUInt32(aStreamSize);
  if aStreamSize>0 then
    Write(aStream.Memory,aStreamSize);
end;

procedure TFileBufferWriter.WriteVarInt32(Value: PtrInt);
begin
  if Value<=0 then
    // 0->0, -1->2, -2->4..
    Value := (-Value) shl 1 else
    // 1->1, 2->3..
    Value := (Value shl 1)-1;
  WriteVarUInt32(Value);
end;

procedure TFileBufferWriter.WriteVarUInt32(Value: PtrUInt);
var pos: integer;
begin
  if fPos+16>fBufLen then
    InternalFlush;
  pos := fPos;
  fPos := PtrUInt(ToVarUInt32(Value,@fBuffer^[fPos]))-PtrUInt(fBuffer);
  inc(fTotalWritten,PtrUInt(fPos-Pos));
end;

procedure TFileBufferWriter.WriteVarInt64(Value: Int64);
var pos: integer;
begin
  if fPos+48>fBufLen then
    InternalFlush;
  pos := fPos;
  fPos := PtrUInt(ToVarInt64(Value,@fBuffer^[fPos]))-PtrUInt(fBuffer);
  inc(fTotalWritten,PtrUInt(fPos-Pos));
end;

procedure TFileBufferWriter.WriteVarUInt64(Value: QWord);
var pos: integer;
begin
  if fPos+48>fBufLen then
    InternalFlush;
  pos := fPos;
  fPos := PtrUInt(ToVarUInt64(Value,@fBuffer^[fPos]))-PtrUInt(fBuffer);
  inc(fTotalWritten,PtrUInt(fPos-Pos));
end;

function CleverStoreInteger(p: PInteger; V, VEnd: PAnsiChar; pCount: integer;
  var StoredCount: integer): PAnsiChar;
// Clever = store Values[i+1]-Values[i] (with special diff=1 count)
// format:  Integer: firstValue, then:
//          B:0 W:difference with previous
//          B:1..253 = difference with previous
//          B:254 W:byOne
//          B:255 B:byOne
var i, d, byOne: integer;
begin
  StoredCount := pCount;
  if pCount<=0 then begin
    result := V;
    exit;
  end;
  i := p^;
  PInteger(V)^ := p^;
  inc(V,4);
  dec(pCount);
  inc(p);
  byOne := 0;
  if pCount>0 then
  repeat
    d := p^-i;
    i := p^;
    inc(p);
    if d=1 then begin
      dec(pCount);
      inc(byOne);
      if pCount>0 then continue;
    end else
    if d<0 then begin
      result:= nil;
      exit;
    end;
    if byOne<>0 then begin
      case byOne of
      1: begin V^ := #1; inc(V); end; // B:1..253 = difference with previous
      2: begin PWord(V)^ := $0101; inc(V,2); end; // B:1..253 = difference
      else
      if byOne>255 then begin
        while byOne>65535 do begin
          PInteger(V)^ := $fffffe; inc(V,3); // store as many len=$ffff as necessary
          dec(byOne,$ffff);
        end;
        PInteger(V)^ := byOne shl 8+$fe; inc(V,3); // B:254 W:byOne
      end else begin
        PWord(V)^ := byOne shl 8+$ff; inc(V,2); // B:255 B:byOne
      end;
      end; // case byOne of
      if pCount=0 then break;
      byOne := 0;
    end;
    if (d=0) or (d>253) then begin
      while cardinal(d)>65535 do begin
        PInteger(V)^ := $ffff00; inc(V,3); // store as many len=$ffff as necessary
        dec(cardinal(d),$ffff);
      end;
      dec(pCount);
      PInteger(V)^ := d shl 8; inc(V,3); // B:0 W:difference with previous
      if (V<VEnd) and (pCount>0) then continue else break;
    end else begin
      dec(pCount);
      V^ := AnsiChar(d); inc(V); // B:1..253 = difference with previous
      if (V<VEnd) and (pCount>0) then continue else break;
    end;
    if V>=VEnd then
      break; // avoid GPF
  until false;
  dec(StoredCount,pCount);
  result := V;
end;

procedure TFileBufferWriter.WriteVarUInt32Array(const Values: TIntegerDynArray;
  ValuesCount: integer; DataLayout: TFileBufferWriterKind);
begin
  WriteVarUInt32Values(pointer(Values),ValuesCount,DataLayout);
end;

procedure TFileBufferWriter.WriteVarUInt32Values(Values: PIntegerArray;
  ValuesCount: integer; DataLayout: TFileBufferWriterKind);
var n, i, pos, diff: integer;
    P: PByte;
    PBeg, PEnd: PAnsiChar;
begin
  WriteVarUInt32(ValuesCount);
  if ValuesCount=0 then
    exit;
  fBuffer^[fPos] := ord(DataLayout);
  inc(fPos);
  inc(fTotalWritten);
  if DataLayout in [wkOffsetU, wkOffsetI] then begin
    pos := fPos;
    fPos := PtrUInt(ToVarUInt32(Values^[0],@fBuffer^[fPos]))-PtrUInt(fBuffer);
    diff := Values^[1]-Values^[0];
    inc(PInteger(Values));
    dec(ValuesCount);
    if ValuesCount=0 then begin
      inc(fTotalWritten,PtrUInt(fPos-pos));
      exit;
    end;
    if diff>0 then begin
      for i := 1 to ValuesCount-1 do
        if Values^[i]-Values^[i-1]<>diff then begin
          diff := 0; // not always the same offset
          break;
        end;
    end else
      diff := 0;
    fPos := PtrUInt(ToVarUInt32(diff,@fBuffer^[fPos]))-PtrUInt(fBuffer);
    inc(fTotalWritten,PtrUInt(fPos-pos));
    if diff<>0 then
      exit; // same offset for all items (fixed sized records) -> quit now
  end;
  repeat
    P := @fBuffer^[fPos];
    PEnd := @fBuffer^[fBufLen-32];
    if PtrUInt(P)<PtrUInt(PEnd) then begin
      pos := fPos;
      case DataLayout of
      wkUInt32: begin
        n := (fBufLen-fPos)shr 2;
        if ValuesCount<n then
          n := ValuesCount;
        MoveFast(Values^,P^,n*4);
        inc(P,n*4);
      end;
      wkVarInt32, wkVarUInt32, wkOffsetU, wkOffsetI: begin
        PBeg := PAnsiChar(P); // leave space for chunk size
        inc(P,4);
        n := ValuesCount;
        case DataLayout of
        wkVarInt32:
          for i := 0 to ValuesCount-1 do begin
            P := ToVarInt32(Values^[i],P);
            if PtrUInt(P)>=PtrUInt(PEnd) then begin
              n := i+1;
              break; // avoid buffer overflow
            end;
          end;
        wkVarUInt32:
          for i := 0 to ValuesCount-1 do begin
            P := ToVarUInt32(Values^[i],P);
            if PtrUInt(P)>=PtrUInt(PEnd) then begin
              n := i+1;
              break; // avoid buffer overflow
            end;
          end;
        wkOffsetU:
          for i := 0 to ValuesCount-1 do begin
            P := ToVarUInt32(Values^[i]-Values^[i-1],P);
            if PtrUInt(P)>=PtrUInt(PEnd) then begin
              n := i+1;
              break; // avoid buffer overflow
            end;
          end;
        wkOffsetI:
          for i := 0 to ValuesCount-1 do begin
            P := ToVarInt32(Values^[i]-Values^[i-1],P);
            if PtrUInt(P)>=PtrUInt(PEnd) then begin
              n := i+1;
              break; // avoid buffer overflow
            end;
          end;
        end;
        PInteger(PBeg)^ := PAnsiChar(P)-PBeg-4; // format: Isize+varUInt32s
      end;
      wkSorted: begin
        PBeg := PAnsiChar(P)+4; // leave space for chunk size
        P := PByte(CleverStoreInteger(pointer(Values),PBeg,PEnd,ValuesCount,n));
        if P=nil then
          raise ESynException.CreateUTF8('%.WriteVarUInt32Array: data not sorted',[self]);
        PInteger(PBeg-4)^ := PAnsiChar(P)-PBeg; // format: Isize+cleverStorage
      end;
      end;
      inc(PByte(Values),n*4);
      fPos := PtrUInt(P)-PtrUInt(fBuffer);
      inc(fTotalWritten,PtrUInt(fPos-pos));
      dec(ValuesCount,n);
      if ValuesCount=0 then
        break;
    end;
    InternalFlush;
  until false;
end;

procedure TFileBufferWriter.WriteVarUInt64DynArray(
  const Values: TInt64DynArray; ValuesCount: integer; Offset: Boolean);
var n, i, pos: integer;
    diff: Int64;
    P, PEnd: PByte;
    PI: PInt64Array;
    PBeg: PAnsiChar;
begin
  WriteVarUInt32(ValuesCount);
  if ValuesCount=0 then
    exit;
  PI := pointer(Values);
  pos := fPos;
  if Offset then begin
    fBuffer^[fPos] := 1;
    fPos := PtrUInt(ToVarUInt64(PI^[0],@fBuffer^[fPos+1]))-PtrUInt(fBuffer);
    diff := PI^[1]-PI^[0];
    inc(PByte(PI),8);
    dec(ValuesCount);
    if ValuesCount=0 then begin
      inc(fTotalWritten,PtrUInt(fPos-pos));
      exit;
    end;
    if (diff>0) and (diff<MaxInt) then begin
      for i := 1 to ValuesCount-1 do
        if PI^[i]-PI^[i-1]<>diff then begin
          diff := 0; // not always the same offset
          break;
        end;
    end else
      diff := 0;
    fPos := PtrUInt(ToVarUInt32(diff,@fBuffer^[fPos]))-PtrUInt(fBuffer);
    if diff<>0 then begin
      inc(fTotalWritten,PtrUInt(fPos-Pos));
      exit; // same offset for all items (fixed sized records) -> quit now
    end;
  end else begin
    fBuffer^[fPos] := 0;
    inc(fPos);
  end;
  inc(fTotalWritten,PtrUInt(fPos-Pos));
  repeat
    P := @fBuffer^[fPos];
    PEnd := @fBuffer^[fBufLen-32];
    if PtrUInt(P)<PtrUInt(PEnd) then begin
      pos := fPos;
      PBeg := PAnsiChar(P); // leave space for chunk size
      inc(P,4);
      n := ValuesCount;
      if Offset then
        for i := 0 to ValuesCount-1 do begin
          P := ToVarUInt64(PI^[i]-PI^[i-1],P); // store diffs
          if PtrUInt(P)>=PtrUInt(PEnd) then begin
            n := i+1;
            break; // avoid buffer overflow
          end;
        end
      else
        for i := 0 to ValuesCount-1 do begin
          P := ToVarUInt64(PI^[i],P);
          if PtrUInt(P)>=PtrUInt(PEnd) then begin
            n := i+1;
            break; // avoid buffer overflow
          end;
        end;
      PInteger(PBeg)^ := PAnsiChar(P)-PBeg-4; // format: Isize+varUInt32/64s
      inc(PByte(PI),n*8);
      fPos := PtrUInt(P)-PtrUInt(fBuffer);
      inc(fTotalWritten,PtrUInt(fPos-Pos));
      dec(ValuesCount,n);
      if ValuesCount=0 then
        break;
    end;
    InternalFlush;
  until false;
end;

function TFileBufferWriter.FlushAndCompress(nocompression: boolean; algo: TAlgoCompress;
  BufferOffset: integer): RawByteString;
var trig: integer;
begin
  if algo=nil then
    algo := AlgoSynLZ;
  trig := SYNLZTRIG[nocompression];
  if fStream.Position=0 then // direct compression from internal buffer
    result := algo.Compress(PAnsiChar(fBuffer),fPos,trig,false,BufferOffset) else begin
    Flush;
    result := algo.Compress((fStream as TRawByteStringStream).DataString,trig,false,BufferOffset);
  end;
end;


{ TFileBufferReader }

procedure TFileBufferReader.Close;
begin
  fMap.UnMap;
end;

procedure TFileBufferReader.ErrorInvalidContent;
begin
  raise ESynException.Create('TFileBufferReader: invalid content');
end;

procedure TFileBufferReader.OpenFrom(aBuffer: pointer; aBufferSize: PtrUInt);
begin
  fCurrentPos := 0;
  fMap.Map(aBuffer,aBufferSize);
end;

procedure TFileBufferReader.OpenFrom(const aBuffer: RawByteString);
begin
  OpenFrom(pointer(aBuffer),length(aBuffer));
end;

function TFileBufferReader.OpenFrom(Stream: TStream): boolean;
begin
  result := false;
  if Stream=nil then
    exit;
  if Stream.InheritsFrom(TFileStream) then
    Open(TFileStream(Stream).Handle) else
  if Stream.InheritsFrom(TCustomMemoryStream) then
    with TCustomMemoryStream(Stream) do
      OpenFrom(Memory,Size) else
      exit;
  result := true
end;

procedure TFileBufferReader.Open(aFile: THandle; aFileNotMapped: boolean);
begin
  fCurrentPos := 0;
  if aFileNotMapped then
    FillcharFast(fMap,SizeOf(fMap),0) else
    fMap.Map(aFile)
    // if Windows failed to find a contiguous VA space -> fall back on direct read
end;

function TFileBufferReader.Read(Data: pointer; DataLen: PtrInt): integer;
var len: PtrInt;
begin
  if DataLen>0 then
    if fMap.Buffer<>nil then begin
      // file up to 2 GB: use fast memory map
      len := fMap.Size-fCurrentPos;
      if len>DataLen then
        len := DataLen;
      if Data<>nil then
        MoveFast(fMap.Buffer[fCurrentPos],Data^,len);
      inc(fCurrentPos,len);
      result := len;
    end else
      // file bigger than 2 GB: slower but accurate reading from file
      if Data=nil then begin
        FileSeek64(fMap.FileHandle,DataLen,soFromCurrent);
        result := DataLen;
      end else
        result := FileRead(fMap.FileHandle,Data^,DataLen) else
      // DataLen=0
      result := 0;
end;

function TFileBufferReader.Read(out Text: RawByteString): integer;
begin
  result := ReadVarUInt32;
  if result=0 then
    exit;
  SetLength(Text,result);
  if Read(pointer(Text),result)<>result then
    ErrorInvalidContent;
end;

function TFileBufferReader.Read(out Text: RawUTF8): integer;
begin
  result := ReadVarUInt32;
  if result=0 then
    exit;
  SetLength(Text,result);
  if Read(pointer(Text),result)<>result then
    ErrorInvalidContent;
end;

function TFileBufferReader.ReadRawUTF8: RawUTF8;
begin
  Read(result);
end;

procedure TFileBufferReader.ReadChunk(out P, PEnd: PByte; var BufTemp: RawByteString);
var len: integer;
begin // read Isize + buffer in P,PEnd
  if (Read(@len,4)<>4) or (len<0) then
    ErrorInvalidContent;
  P := ReadPointer(len,BufTemp);
  if P=nil then
    ErrorInvalidContent;
  PEnd := pointer(PtrUInt(P)+PtrUInt(len));
end;

function TFileBufferReader.CurrentMemory(DataLen: PtrUInt; PEnd: PPAnsiChar): pointer;
begin
  if (fMap.Buffer=nil) or (fCurrentPos+DataLen>=fMap.Size) then
    result := nil else begin
    result := @fMap.Buffer[fCurrentPos];
    if PEnd<>nil then
      PEnd^ := @fMap.Buffer[fMap.Size];
    inc(fCurrentPos,DataLen);
  end;
end;

function TFileBufferReader.CurrentPosition: integer;
begin
  if (fMap.Buffer=nil) or (fCurrentPos>=fMap.Size) then
    result := -1 else
    result := fCurrentPos;
end;

function TFileBufferReader.FileSize: Int64;
begin
  result := fMap.FileSize;
end;

function TFileBufferReader.MappedBuffer: PAnsiChar;
begin
  result := fMap.Buffer;
end;

function TFileBufferReader.ReadPointer(DataLen: PtrUInt;
  var aTempData: RawByteString): pointer;
begin
  if fMap.Buffer=nil then begin
    // read from file
    if DataLen>PtrUInt(Length(aTempData)) then begin
      aTempData := ''; // so no move() call in SetLength() below
      SetLength(aTempData,DataLen);
    end;
    if PtrUInt(FileRead(fMap.FileHandle,pointer(aTempData)^,DataLen))<>DataLen then
      result := nil else // invalid content
      result := pointer(aTempData);
  end else
  if DataLen+fCurrentPos>fMap.Size then
    // invalid request
    result := nil else begin
    // get pointer to data from current memory map (no data copy)
    result := @fMap.Buffer[fCurrentPos];
    inc(fCurrentPos,DataLen);
  end;
end;

function TFileBufferReader.ReadStream(DataLen: PtrInt): TCustomMemoryStream;
var FileCurrentPos: Int64;
begin
  if DataLen<0 then
    DataLen := ReadVarUInt32;
  if DataLen<>0 then
    if fMap.Buffer=nil then begin
      FileCurrentPos := FileSeek64(fMap.FileHandle,0,soFromCurrent);
      if FileCurrentPos+DataLen>fMap.Size then
        // invalid content
        result := nil else begin
        // create a temporary memory map buffer stream
        result := TSynMemoryStreamMapped.Create(fMap.FileHandle,DataLen,FileCurrentPos);
        FileSeek64(fMap.FileHandle,DataLen,soFromCurrent);
      end;
    end else
    if PtrUInt(DataLen)+fCurrentPos>fMap.Size then
      // invalid content
      result := nil else begin
      // get pointer to data from current memory map (no data copy)
      result := TSynMemoryStream.Create(@fMap.Buffer[fCurrentPos],DataLen);
      inc(fCurrentPos,DataLen);
    end else
    // DataLen=0 -> invalid content
    result := nil;
end;

function TFileBufferReader.ReadByte: PtrUInt;
begin
  if fMap.Buffer<>nil then
    if fCurrentPos>=fMap.Size then
      // invalid request
      result := 0 else begin
      // read 8-bit from current memory map
      result := ord(fMap.Buffer[fCurrentPos]);
      inc(fCurrentPos);
    end else begin
    // read from file if >= 2 GB (slow, but works)
    result := 0;
    if FileRead(fMap.FileHandle,result,1)<>1 then
      result := 0;
  end;
end;

function TFileBufferReader.ReadCardinal: cardinal;
begin
  if fMap.Buffer<>nil then
    if fCurrentPos+3>=fMap.Size then
      // invalid request
      result := 0 else begin
      // read 32-bit from current memory map
      result := PCardinal(fMap.Buffer+fCurrentPos)^;
      inc(fCurrentPos,4);
    end else begin
      // read from file if >= 2 GB (slow, but works)
      result := 0;
      if FileRead(fMap.FileHandle,result,4)<>4 then
        result := 0;
    end;
end;

function TFileBufferReader.ReadVarUInt32: PtrUInt;
var c, n: PtrUInt;
begin
  result := ReadByte;
  if result>$7f then begin
    n := 0;
    result := result and $7F;
    repeat
      c := ReadByte;
      inc(n,7);
      if c<=$7f then break;
      result := result or ((c and $7f) shl n);
    until false;
    result := result or (c shl n);
  end;
end;

function TFileBufferReader.ReadVarInt32: PtrInt;
begin
  result := ReadVarUInt32;
  if result and 1<>0 then
    // 1->1, 3->2..
    result := result shr 1+1 else
    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
end;

function TFileBufferReader.ReadVarUInt64: QWord;
var c, n: PtrUInt;
begin
  result := ReadByte;
  if result>$7f then begin
    n := 0;
    result := result and $7F;
    repeat
      c := ReadByte;
      inc(n,7);
      if c<=$7f then break;
      result := result or (QWord(c and $7f) shl n);
    until false;
    result := result or (QWord(c) shl n);
  end;
end;

function TFileBufferReader.ReadVarInt64: Int64;
begin
  result := ReadVarUInt64;
  if result<>0 then
    if result and 1<>0 then
      // 1->1, 3->2..
      result := result shr 1+1 else
      // 0->0, 2->-1, 4->-2..
      result := -(result shr 1);
end;

function CleverReadInteger(p, pEnd: PAnsiChar; V: PInteger): PtrUInt;
// Clever = decode Values[i+1]-Values[i] storage (with special diff=1 count)
var i, n: PtrUInt;
begin
  result := PtrUInt(V);
  i := PInteger(p)^; inc(p,4); // Integer: firstValue
  V^ := i; inc(V);
  if PtrUInt(p)<PtrUInt(pEnd) then
  repeat
    case p^ of
      #0: begin // B:0 W:difference with previous
        inc(i,PWord(p+1)^); inc(p,3);
        V^ := i; inc(V);
        if PtrUInt(p)<PtrUInt(pEnd) then continue else break;
      end;
      #254: begin // B:254 W:byOne
        for n := 1 to PWord(p+1)^ do begin
          inc(i);
          V^ := i; inc(V);
        end;
        inc(p,3);
        if PtrUInt(p)<PtrUInt(pEnd) then continue else break;
      end;
      #255: begin // B:255 B:byOne
        for n := 1 to pByte(p+1)^ do begin
          inc(i);
          V^ := i; inc(V);
        end;
        inc(p,2);
        if PtrUInt(p)<PtrUInt(pEnd) then continue else break;
      end else begin // B:1..253 = difference with previous
        inc(i,ord(p^)); inc(p);
        V^ := i; inc(V);
        if PtrUInt(p)<PtrUInt(pEnd) then continue else break;
      end;
    end; // case p^ of
  until false;
  result := (PtrUInt(V)-result) shr 2; // returns count of stored integer
end;

function TFileBufferReader.ReadVarUInt32Array(var Values: TIntegerDynArray): PtrInt;
var count, n, i, diff: integer;
    DataLayout: TFileBufferWriterKind;
    P, PEnd: PByte;
    PI: PInteger;
    PIA: PIntegerArray absolute PI;
    BufTemp: RawByteString;
begin
  result := ReadVarUInt32;
  if result=0 then
    exit;
  DataLayout := TFileBufferWriterKind(ReadByte);
  if DataLayout=wkFakeMarker then begin
    result := -result;
    exit;
  end;
  count := result;
  if count>length(Values) then // only set length is not big enough
    SetLength(Values,count);
  PI := pointer(Values);
  if DataLayout in [wkOffsetU, wkOffsetI] then begin
    PI^ := ReadVarUInt32;
    dec(count);
    if count=0 then
      exit;
    diff := ReadVarUInt32;
    if diff<>0 then begin
      for i := 0 to count-1 do
        PIA^[i+1] := PIA^[i]+diff;
      exit;
    end;
  end;
  if DataLayout=wkUInt32 then
    Read(@Values[0],count*4) else begin
    repeat
      ReadChunk(P,PEnd,BufTemp); // raise ErrorInvalidContent on error
      case DataLayout of
      wkVarInt32:
        while (count>0) and (PtrUInt(P)<PtrUInt(PEnd)) do begin
          PI^ := FromVarInt32(P);
          dec(count);
          inc(PI);
        end;
      wkVarUInt32:
        while (count>0) and (PtrUInt(P)<PtrUInt(PEnd)) do begin
          PI^ := FromVarUInt32(P);
          dec(count);
          inc(PI);
        end;
      wkSorted: begin
        n := CleverReadInteger(pointer(P),pointer(PEnd),PI);
        dec(count,n);
        inc(PByte(PI),n*4);
      end;
      wkOffsetU: begin
        while (count>0) and (PtrUInt(P)<PtrUInt(PEnd)) do begin
          PIA^[1] := PIA^[0]+integer(FromVarUInt32(P));
          dec(count);
          inc(PI);
        end;
        if count<=0 then
          inc(PI); // make sure PI=@Values[result]
      end;
      wkOffsetI: begin
        while (count>0) and (PtrUInt(P)<PtrUInt(PEnd)) do begin
          PIA^[1] := PIA^[0]+FromVarInt32(P);
          dec(count);
          inc(PI);
        end;
        if count<=0 then
          inc(PI); // make sure PI=@Values[result]
      end;
      else
        ErrorInvalidContent;
      end;
    until count<=0;
    if PI<>@Values[result] then
      ErrorInvalidContent;
  end;
end;

function TFileBufferReader.ReadVarUInt64Array(var Values: TInt64DynArray): PtrInt;
var count, i: integer;
    P, PEnd: PByte;
    v: PQWordArray;
    diff: QWord;
    BufTemp: RawByteString;
begin
  result := ReadVarUInt32;
  if result=0 then
    exit;
  count := result;
  if count>length(Values) then // only set length if not big enough
    SetLength(Values,count);
  v := pointer(Values);
  if boolean(ReadByte) then begin // values were stored as offsets
    v^[0] := ReadVarUInt64; // read first value
    dec(count);
    diff := ReadVarUInt32;
    if diff=0 then begin
      // read all offsets, and compute (not fixed sized records)
      repeat
        ReadChunk(P,PEnd,BufTemp); // raise ErrorInvalidContent on error
        while (count>0) and (PtrUInt(P)<PtrUInt(PEnd)) do begin
          FromVarUInt64Safe(P,PEnd,diff);
          v^[1] := v^[0]+diff;
          v := @v^[1];
          dec(count);
        end;
      until count<=0;
    end else
      // same offset for all items (fixed sized records)
      for i := 0 to count-1 do
        v^[i+1] := v^[i]+diff;
  end else
    repeat // raw values were stored, not their offsets
      ReadChunk(P,PEnd,BufTemp); // raise ErrorInvalidContent on error
      while PtrUInt(P)<PtrUInt(PEnd) do begin
        FromVarUInt64Safe(P,PEnd,v^[0]);
        v := @v^[1];
        dec(count);
        if count=0 then
          exit;
      end;
    until false;
end;

function TFileBufferReader.ReadRawUTF8List(List: TRawUTF8List): boolean;
var i,n: integer;
    v: TRawUTF8DynArray;
    o: TObjectDynArray;
    StoreObjectsAsVarUInt32: Boolean;
begin
  if (fMap.Buffer<>nil) and (List<>nil) then begin
    List.BeginUpdate;
    try
      List.Clear;
      n := ReadVarRawUTF8DynArray(v);
      result := true;
      if n=0 then
        exit;
      Read(@StoreObjectsAsVarUInt32,1);
      if StoreObjectsAsVarUInt32 then begin
        List.Flags := List.Flags-[fObjectsOwned]; // Int32 here, not instances
        SetLength(o,length(v));
        for i := 0 to n-1 do
          o[i] := TObject(ReadVarUInt32);
      end;
      List.SetFrom(v,o); // fast assignment using refcnt
    finally
      List.EndUpdate;
    end;
  end else
    result := false;
end;

function TFileBufferReader.ReadVarRawUTF8DynArray(var Values: TRawUTF8DynArray): PtrInt;
var count, len, fixedsize: integer;
    P, PEnd: PByte;
    PI: PRawUTF8;
    BufTemp: RawByteString;
begin
  result := ReadVarUInt32;
  if result=0 then
    exit;
  count := result;
  if count>length(Values) then // change Values[] length only if not big enough
    SetLength(Values,count);
  PI := pointer(Values);
  fixedsize := ReadVarUInt32;
  repeat
    ReadChunk(P,PEnd,BufTemp); // raise ErrorInvalidContent on error
    if fixedsize=0 then
    while (count>0) and (PtrUInt(P)<PtrUInt(PEnd)) do begin
      len := FromVarUInt32(P);
      if len>0 then begin
        FastSetString(PI^,P,len);
        inc(P,len);
      end else
        if PI^<>'' then
          PI^ := '';
      dec(count);
      inc(PI);
    end else
    // fixed size strings case
    while (count>0) and (PtrUInt(P)<PtrUInt(PEnd)) do begin
      FastSetString(PI^,P,fixedsize);
      inc(P,fixedsize);
      dec(count);
      inc(PI);
    end;
  until count<=0;
  if PI<>@Values[result] then
    ErrorInvalidContent;
end;

{$ifndef CPU64}
function TFileBufferReader.Seek(Offset: Int64): boolean;
begin
  if (Offset<0) or (Offset>fMap.Size) then
    result := False else
  if fMap.Buffer=nil then
    result := FileSeek64(fMap.FileHandle,Offset,soFromBeginning)=Offset else begin
    fCurrentPos := PCardinal(@Offset)^;
    result := true;
  end;
end;
{$endif CPU64}

function TFileBufferReader.Seek(Offset: PtrInt): boolean;
begin
  // we don't need to handle fMap=0 here
  if fMap.Buffer=nil then
    Result := FileSeek(fMap.FileHandle,Offset,0)=Offset else
  if (fMap.Buffer<>nil) and (PtrUInt(Offset)<PPtrUInt(@fMap.Size)^) then begin
    fCurrentPos := Offset;
    result := true;
  end else
    result := false;
end;


{ ************ high-level storage classes ************************* }

{ TSynCache }

constructor TSynCache.Create(aMaxCacheRamUsed: cardinal; aCaseSensitive: boolean;
  aTimeoutSeconds: cardinal);
begin
  inherited Create;
  fNameValue.Init(aCaseSensitive);
  fMaxRamUsed := aMaxCacheRamUsed;
  fTimeoutSeconds := aTimeoutSeconds;
end;

procedure TSynCache.ResetIfNeeded;
var tix: cardinal;
begin
  if fRamUsed>fMaxRamUsed then
    Reset;
  if fTimeoutSeconds>0 then begin
    tix := {$ifdef FPCLINUX}SynFPCLinux.{$endif}GetTickCount64 shr 10;
    if fTimeoutTix>tix then
      Reset;
    fTimeoutTix := tix+fTimeoutSeconds;
  end;
end;

procedure TSynCache.Add(const aValue: RawUTF8; aTag: PtrInt);
begin
  if (self=nil) or (fFindLastKey='') then
    exit;
  ResetIfNeeded;
  inc(fRamUsed,length(aValue));
  fNameValue.Add(fFindLastKey,aValue,aTag);
  fFindLastKey := '';
end;

function TSynCache.Find(const aKey: RawUTF8; aResultTag: PPtrInt): RawUTF8;
var ndx: integer;
begin
  result := '';
  if self=nil then
    exit;
  fFindLastKey := aKey;
  if aKey='' then
    exit;
  ndx := fNameValue.Find(aKey);
  if ndx<0 then
    exit;
  with fNameValue.List[ndx] do begin
    result := Value;
    if aResultTag<>nil then
      aResultTag^ := Tag;
  end;
end;

function TSynCache.AddOrUpdate(const aKey, aValue: RawUTF8; aTag: PtrInt): boolean;
var ndx: integer;
begin
  result := false;
  if self=nil then
    exit; // avoid GPF
  fSafe.Lock;
  try
    ResetIfNeeded;
    ndx := fNameValue.DynArray.FindHashedForAdding(aKey,result);
    with fNameValue.List[ndx] do begin
      Name := aKey;
      dec(fRamUsed,length(Value));
      Value := aValue;
      inc(fRamUsed,length(Value));
      Tag := aTag;
    end;
  finally
    fSafe.Unlock;
  end;
end;

function TSynCache.Reset: boolean;
begin
  result := false;
  if self=nil then
    exit; // avoid GPF
  fSafe.Lock;
  try
    if Count<>0 then begin
      fNameValue.DynArray.Clear;
      fNameValue.DynArray.ReHash;
      result := true; // mark something was flushed
    end;
    fRamUsed := 0;
    fTimeoutTix := 0;
  finally
    fSafe.Unlock;
  end;
end;

function TSynCache.Count: integer;
begin
  if self=nil then begin
    result := 0;
    exit;
  end;
  fSafe.Lock;
  try
    result := fNameValue.Count;
  finally
    fSafe.Unlock;
  end;
end;


{ TSynQueue }

constructor TSynQueue.Create(aTypeInfo: pointer);
begin
  inherited Create;
  fFirst := -1;
  fLast := -2;
  fValues.Init(aTypeInfo,fValueVar,@fCount);
end;

destructor TSynQueue.Destroy;
begin
  WaitPopFinalize;
  fValues.Clear;
  inherited Destroy;
end;

procedure TSynQueue.Clear;
begin
  fSafe.Lock;
  try
    fValues.Clear;
    fFirst := -1;
    fLast := -2;
  finally
    fSafe.UnLock;
  end;
end;

function TSynQueue.Count: Integer;
begin
  if self=nil then
    result := 0 else begin
    fSafe.Lock;
    try
      if fFirst<0 then
        result := 0 else
        if fFirst<=fLast then
          result := fLast-fFirst+1 else
          result := fCount-fFirst+fLast+1;
    finally
      fSafe.UnLock;
    end;
  end;
end;

function TSynQueue.Capacity: integer;
begin
  if self=nil then
    result := 0 else begin
    fSafe.Lock;
    try
      result := fValues.Capacity;
    finally
      fSafe.UnLock;
    end;
  end;
end;

function TSynQueue.Pending: boolean;
begin // allow some false positive: fSafe.Lock not used here
  result := (self<>nil) and (fFirst>=0);
end;

procedure TSynQueue.Push(const aValue);
begin
  fSafe.Lock;
  try
    if fFirst<0 then begin
      fFirst := 0; // start from the bottom of the void queue
      fLast := 0;
      if fCount=0 then
        fValues.Count := 64;
    end else
      if fFirst<=fLast then begin // stored in-order
        inc(fLast);
        if fLast=fCount then
          InternalGrow;
      end else begin
        inc(fLast);
        if fLast=fFirst then begin // collision -> arrange
          fValues.AddArray(fValueVar,0,fLast); // move 0..fLast to the end
          fLast := fCount;
          InternalGrow;
        end;
      end;
    fValues.ElemCopyFrom(aValue,fLast);
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynQueue.InternalGrow;
var cap: integer;
begin
  cap := fValues.Capacity;
  if fFirst>cap-fCount then // use leading space if worth it
    fLast := 0 else         // append at the end
    if fCount=cap then      // reallocation needed
      fValues.Count := cap+cap shr 3+64 else
      fCount := cap;        // fill trailing memory as much as possible
end;

function TSynQueue.Peek(out aValue): boolean;
begin
  fSafe.Lock;
  try
    result := fFirst>=0;
    if result then
      fValues.ElemCopyAt(fFirst,aValue);
  finally
    fSafe.UnLock;
  end;
end;

function TSynQueue.Pop(out aValue): boolean;
begin
  fSafe.Lock;
  try
    result := fFirst>=0;
    if result then begin
      fValues.ElemMoveTo(fFirst,aValue);
      if fFirst=fLast then begin
        fFirst := -1; // reset whole store (keeping current capacity)
        fLast := -2;
      end else begin
        inc(fFirst);
        if fFirst=fCount then
          fFirst := 0; // will retrieve from leading items
      end;
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TSynQueue.PopEquals(aAnother: pointer; aCompare: TDynArraySortCompare;
  out aValue): boolean;
begin
  fSafe.Lock;
  try
    result := (fFirst>=0) and Assigned(aCompare) and Assigned(aAnother) and
      (aCompare(fValues.ElemPtr(fFirst)^,aAnother^)=0) and Pop(aValue);
  finally
    fSafe.UnLock;
  end;
end;

function TSynQueue.InternalDestroying(incPopCounter: integer): boolean;
begin
  fSafe.Lock;
  try
    result := wpfDestroying in fWaitPopFlags;
    inc(fWaitPopCounter,incPopCounter);
  finally
    fSafe.UnLock;
  end;
end;

function TSynQueue.InternalWaitDone(endtix: Int64; const idle: TThreadMethod): boolean;
begin
  SleepHiRes(1);
  if Assigned(idle) then
    idle; // e.g. Application.ProcessMessages
  result := InternalDestroying(0) or (GetTickCount64>endtix);
end;

function TSynQueue.WaitPop(aTimeoutMS: integer; const aWhenIdle: TThreadMethod;
  out aValue; aCompared: pointer; aCompare: TDynArraySortCompare): boolean;
var endtix: Int64;
begin
  result := false;
  if not InternalDestroying(+1) then
    try
      endtix := GetTickCount64+aTimeoutMS;
      repeat
        if Assigned(aCompared) and Assigned(aCompare) then
          result := PopEquals(aCompared,aCompare,aValue) else
          result := Pop(aValue);
      until result or InternalWaitDone(endtix,aWhenIdle);
    finally
      InternalDestroying(-1);
    end;
end;

function TSynQueue.WaitPeekLocked(aTimeoutMS: integer; const aWhenIdle: TThreadMethod): pointer;
var endtix: Int64;
begin
  result := nil;
  if not InternalDestroying(+1) then
    try
      endtix := GetTickCount64+aTimeoutMS;
      repeat
        fSafe.Lock;
        try
          if fFirst>=0 then
            result := fValues.ElemPtr(fFirst);
        finally
          if result=nil then
            fSafe.UnLock; // caller should always Unlock once done
        end;
      until (result<>nil) or InternalWaitDone(endtix,aWhenIdle);
    finally
      InternalDestroying(-1);
    end;
end;

procedure TSynQueue.WaitPopFinalize(aTimeoutMS: integer);
var endtix: Int64; // never wait forever
begin
  fSafe.Lock;
  try
    include(fWaitPopFlags,wpfDestroying);
    if fWaitPopCounter=0 then
      exit;
  finally
    fSafe.UnLock;
  end;
  endtix := GetTickCount64+aTimeoutMS;
  repeat
    SleepHiRes(1); // ensure WaitPos() is actually finished
  until (fWaitPopCounter=0) or (GetTickCount64>endtix);
end;

procedure TSynQueue.Save(out aDynArrayValues; aDynArray: PDynArray);
var n: integer;
    DA: TDynArray;
begin
  DA.Init(fValues.ArrayType,aDynArrayValues,@n);
  fSafe.Lock;
  try
    DA.Capacity := Count; // pre-allocate whole array, and set its length
    if fFirst>=0 then
      if fFirst<=fLast then
        DA.AddArray(fValueVar,fFirst,fLast-fFirst+1) else begin
        DA.AddArray(fValueVar,fFirst,fCount-fFirst);
        DA.AddArray(fValueVar,0,fLast+1);
      end;
  finally
    fSafe.UnLock;
  end;
  if aDynArray<>nil then
    aDynArray^.Init(fValues.ArrayType,aDynArrayValues);
end;


{ TObjectListSorted }

destructor TObjectListSorted.Destroy;
var i: integer;
begin
  for i := 0 to fCount-1 do
    fObjArray[i].Free;
  inherited;
end;

function TObjectListSorted.FastLocate(const Value; out Index: Integer): boolean;
var n, i, cmp: integer;
begin
  result := False;
  n := Count;
  if n=0 then // a void array is always sorted
    Index := 0 else begin
    dec(n);
    if Compare(fObjArray[n],Value)<0 then begin // already sorted
      Index := n+1; // returns false + last position index to insert
      exit;
    end;
    Index := 0;
    while Index<=n do begin // O(log(n)) binary search of the sorted position
      i := (Index+n) shr 1;
      cmp := Compare(fObjArray[i],Value);
      if cmp=0 then begin
        Index := i; // index of existing Elem
        result := True;
        exit;
      end else
        if cmp<0 then
          Index := i+1 else
          n := i-1;
    end;
    // Elem not found: returns false + the index where to insert
  end;
end;

procedure TObjectListSorted.InsertNew(Item: TSynPersistentLock;
  Index: integer);
begin
  if fCount=length(fObjArray) then
    SetLength(fObjArray,NextGrow(fCount));
  if cardinal(Index)<cardinal(fCount) then
    MoveFast(fObjArray[Index],fObjArray[Index+1],(fCount-Index)*SizeOf(TObject)) else
    Index := fCount;
  fObjArray[Index] := Item;
  inc(fCount);
end;

function TObjectListSorted.Delete(const Value): boolean;
var i: integer;
begin
  result := false;
  fSafe.Lock;
  try
    if FastLocate(Value,i) and (cardinal(i)<cardinal(fCount)) then begin
      fObjArray[i].Free;
      dec(fCount);
      if fCount>i then
        MoveFast(fObjArray[i+1],fObjArray[i],(fCount-i)*SizeOf(TObject));
      result := true;
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TObjectListSorted.FindLocked(const Value): pointer;
var i: integer;
begin
  result := nil;
  fSafe.Lock;
  try
    if FastLocate(Value,i) then begin
      result := fObjArray[i];
      TSynPersistentLock(result).Safe.Lock;
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TObjectListSorted.FindOrAddLocked(const Value; out added: boolean): pointer;
var i: integer;
begin
  added := false;
  fSafe.Lock;
  try
    if not FastLocate(Value,i) then begin
      InsertNew(NewItem(Value),i);
      added := true;
    end;
    result := fObjArray[i];
    TSynPersistentLock(result).Safe.Lock;
  finally
    fSafe.UnLock;
  end;
end;



{ TSynPersistentStore }

constructor TSynPersistentStore.Create(const aName: RawUTF8);
begin
  Create;
  fName := aName;
end;

constructor TSynPersistentStore.CreateFrom(const aBuffer: RawByteString;
  aLoad: TAlgoCompressLoad);
begin
  CreateFromBuffer(pointer(aBuffer),length(aBuffer),aLoad);
end;

constructor TSynPersistentStore.CreateFromBuffer(
  aBuffer: pointer; aBufferLen: integer; aLoad: TAlgoCompressLoad);
begin
  Create('');
  LoadFrom(aBuffer,aBufferLen,aLoad);
end;

constructor TSynPersistentStore.CreateFromFile(const aFileName: TFileName;
  aLoad: TAlgoCompressLoad);
begin
  Create('');
  LoadFromFile(aFileName,aLoad);
end;

procedure TSynPersistentStore.LoadFromReader;
begin
  fReader.VarUTF8(fName);
end;

procedure TSynPersistentStore.SaveToWriter(aWriter: TFileBufferWriter);
begin
  aWriter.Write(fName);
end;

procedure TSynPersistentStore.LoadFrom(const aBuffer: RawByteString;
  aLoad: TAlgoCompressLoad);
begin
  if aBuffer <> '' then
    LoadFrom(pointer(aBuffer),length(aBuffer),aLoad);
end;

procedure TSynPersistentStore.LoadFrom(aBuffer: pointer; aBufferLen: integer;
  aLoad: TAlgoCompressLoad);
var localtemp: RawByteString;
    p: pointer;
    temp: PRawByteString;
begin
  if (aBuffer=nil) or (aBufferLen<=0) then
    exit; // nothing to load
  fLoadFromLastAlgo := TAlgoCompress.Algo(aBuffer,aBufferLen);
  if fLoadFromLastAlgo = nil then
    fReader.ErrorData('%.LoadFrom unknown TAlgoCompress AlgoID=%',
      [self,PByteArray(aBuffer)[4]]);
  temp := fReaderTemp;
  if temp=nil then
    temp := @localtemp;
  p := fLoadFromLastAlgo.Decompress(aBuffer,aBufferLen,fLoadFromLastUncompressed,temp^,aLoad);
  if p=nil then
    fReader.ErrorData('%.LoadFrom %.Decompress failed',[self,fLoadFromLastAlgo]);
  fReader.Init(p,fLoadFromLastUncompressed);
  LoadFromReader;
end;

function TSynPersistentStore.LoadFromFile(const aFileName: TFileName;
  aLoad: TAlgoCompressLoad): boolean;
var temp: RawByteString;
begin
  temp := StringFromFile(aFileName);
  result := temp<>'';
  if result then
    LoadFrom(temp,aLoad);
end;

procedure TSynPersistentStore.SaveTo(out aBuffer: RawByteString; nocompression: boolean;
  BufLen: integer; ForcedAlgo: TAlgoCompress; BufferOffset: integer);
var writer: TFileBufferWriter;
    temp: array[word] of byte;
begin
  if BufLen<=SizeOf(temp) then
    writer := TFileBufferWriter.Create(TRawByteStringStream,@temp,SizeOf(temp)) else
    writer := TFileBufferWriter.Create(TRawByteStringStream,BufLen);
  try
    SaveToWriter(writer);
    fSaveToLastUncompressed := writer.TotalWritten;
    aBuffer := writer.FlushAndCompress(nocompression,ForcedAlgo,BufferOffset);
  finally
    writer.Free;
  end;
end;

function TSynPersistentStore.SaveTo(nocompression: boolean; BufLen: integer;
  ForcedAlgo: TAlgoCompress; BufferOffset: integer): RawByteString;
begin
  SaveTo(result,nocompression,BufLen,ForcedAlgo,BufferOffset);
end;

function TSynPersistentStore.SaveToFile(const aFileName: TFileName;
  nocompression: boolean; BufLen: integer; ForcedAlgo: TAlgoCompress): PtrUInt;
var temp: RawByteString;
begin
  SaveTo(temp,nocompression,BufLen,ForcedAlgo);
  if FileFromString(temp,aFileName) then
    result := length(temp) else
    result := 0;
end;


{ TSynPersistentStoreJson }

procedure TSynPersistentStoreJson.AddJSON(W: TTextWriter);
begin
  W.AddPropJSONString('name', fName);
end;

function TSynPersistentStoreJson.SaveToJSON(reformat: TTextWriterJSONFormat): RawUTF8;
var
  W: TTextWriter;
begin
  W := DefaultTextWriterSerializer.CreateOwnedStream(65536);
  try
    W.Add('{');
    AddJSON(W);
    W.CancelLastComma;
    W.Add('}');
    W.SetText(result, reformat);
  finally
    W.Free;
  end;
end;


{ TRawByteStringGroup }

procedure TRawByteStringGroup.Add(const aItem: RawByteString);
begin
  if Values=nil then
    Clear; // ensure all fields are initialized, even if on stack
  if Count=Length(Values) then
    SetLength(Values,NextGrow(Count));
  with Values[Count] do begin
    Position := self.Position;
    Value := aItem;
  end;
  LastFind := Count;
  inc(Count);
  inc(Position,Length(aItem));
end;

procedure TRawByteStringGroup.Add(aItem: pointer; aItemLen: integer);
var tmp: RawByteString;
begin
  SetString(tmp,PAnsiChar(aItem),aItemLen);
  Add(tmp);
end;

{$ifndef DELPHI5OROLDER} // circumvent Delphi 5 compiler bug

procedure TRawByteStringGroup.Add(const aAnother: TRawByteStringGroup);
var i: integer;
    s,d: PRawByteStringGroupValue;
begin
  if aAnother.Values=nil then
    exit;
  if Values=nil then
    Clear; // ensure all fields are initialized, even if on stack
  if Count+aAnother.Count>Length(Values) then
    SetLength(Values,Count+aAnother.Count);
  s := pointer(aAnother.Values);
  d := @Values[Count];
  for i := 1 to aAnother.Count do begin
    d^.Position := Position;
    d^.Value := s^.Value;
    inc(Position,length(s^.Value));
    inc(s);
    inc(d);
  end;
  inc(Count,aAnother.Count);
  LastFind := Count-1;
end;

procedure TRawByteStringGroup.RemoveLastAdd;
begin
  if Count>0 then begin
    dec(Count);
    dec(Position,Length(Values[Count].Value));
    Values[Count].Value := ''; // release memory
    LastFind := Count-1;
  end;
end;

function TRawByteStringGroup.Equals(const aAnother: TRawByteStringGroup): boolean;
begin
  if ((Values=nil) and (aAnother.Values<>nil)) or ((Values<>nil) and (aAnother.Values=nil)) or
     (Position<>aAnother.Position) then
    result := false else
    if (Count<>1) or (aAnother.Count<>1) or (Values[0].Value<>aAnother.Values[0].Value) then
      result := AsText=aAnother.AsText else
      result := true;
end;

{$endif DELPHI5OROLDER}

procedure TRawByteStringGroup.Clear;
begin
  Values := nil;
  Position := 0;
  Count := 0;
  LastFind := 0;
end;

procedure TRawByteStringGroup.AppendTextAndClear(var aDest: RawByteString);
var d,i: integer;
    v: PRawByteStringGroupValue;
begin
  d := length(aDest);
  SetLength(aDest,d+Position);
  v := pointer(Values);
  for i := 1 to Count do begin
    MoveFast(pointer(v^.Value)^,PByteArray(aDest)[d+v^.Position],length(v^.Value));
    inc(v);
  end;
  Clear;
end;

function TRawByteStringGroup.AsText: RawByteString;
begin
  if Values=nil then
    result := '' else begin
    if Count>1 then
      Compact;
    result := Values[0].Value;
  end;
end;

procedure TRawByteStringGroup.Compact;
var i: integer;
    v: PRawByteStringGroupValue;
    tmp: RawByteString;
begin
  if (Values<>nil) and (Count>1) then begin
    SetString(tmp,nil,Position);
    v := pointer(Values);
    for i := 1 to Count do begin
      MoveFast(pointer(v^.Value)^,PByteArray(tmp)[v^.Position],length(v^.Value));
      {$ifdef FPC}Finalize(v^.Value){$else}v^.Value := ''{$endif}; // free chunks
      inc(v);
    end;
    Values[0].Value := tmp; // use result for absolute compaction ;)
    if Count>128 then
      SetLength(Values,128);
    Count := 1;
    LastFind := 0;
  end;
end;

function TRawByteStringGroup.AsBytes: TByteDynArray;
var i: integer;
begin
  result := nil;
  if Values=nil then
    exit;
  SetLength(result,Position);
  for i := 0 to Count-1 do
  with Values[i] do
    MoveFast(pointer(Value)^,PByteArray(result)[Position],length(Value));
end;

procedure TRawByteStringGroup.Write(W: TTextWriter; Escape: TTextWriterKind);
var i: integer;
begin
  if Values<>nil then
    for i := 0 to Count-1 do
    with Values[i] do
      W.Add(PUTF8Char(pointer(Value)),length(Value),Escape);
end;

procedure TRawByteStringGroup.WriteBinary(W: TFileBufferWriter);
var i: integer;
begin
  if Values<>nil then
    for i := 0 to Count-1 do
      W.WriteBinary(Values[i].Value);
end;

procedure TRawByteStringGroup.WriteString(W: TFileBufferWriter);
begin
  if Values=nil then begin
    W.Write1(0);
    exit;
  end;
  W.WriteVarUInt32(Position);
  WriteBinary(W);
end;

procedure TRawByteStringGroup.AddFromReader(var aReader: TFastReader);
var complexsize: integer;
begin
  complexsize := aReader.VarUInt32;
  if complexsize>0 then // directly create a RawByteString from aReader buffer
    Add(aReader.Next(complexsize),complexsize);
end;

function TRawByteStringGroup.Find(aPosition: integer): PRawByteStringGroupValue;
var i: integer;
begin
  if (pointer(Values)<>nil) and (cardinal(aPosition)<cardinal(Position)) then begin
    result := @Values[LastFind]; // this cache is very efficient in practice
    if (aPosition>=result^.Position) and (aPosition<result^.Position+length(result^.Value)) then
      exit;
    result := @Values[1]; // seldom O(n) brute force search (in CPU L1 cache)
    for i := 0 to Count-2 do
      if result^.Position>aPosition then begin
        dec(result);
        LastFind := i;
        exit;
      end else
        inc(result);
    dec(result);
    LastFind := Count-1;
  end
  else
    result := nil;
end;

function TRawByteStringGroup.Find(aPosition, aLength: integer): pointer;
var P: PRawByteStringGroupValue;
    i: integer;
label found;
begin
  if (pointer(Values)<>nil) and (cardinal(aPosition)<cardinal(Position)) then begin
    P := @Values[LastFind]; // this cache is very efficient in practice
    i := aPosition-P^.Position;
    if (i>=0) and (i+aLength<length(P^.Value)) then begin
      result := @PByteArray(P^.Value)[i];
      exit;
    end;
    P := @Values[1]; // seldom O(n) brute force search (in CPU L1 cache)
    for i := 0 to Count-2 do
      if P^.Position>aPosition then begin
        LastFind := i;
found:  dec(P);
        dec(aPosition,P^.Position);
        if aLength-aPosition<=length(P^.Value) then
          result := @PByteArray(P^.Value)[aPosition] else
          result := nil;
        exit;
      end else
        inc(P);
    LastFind := Count-1;
    goto found;
  end
  else
    result := nil;
end;

procedure TRawByteStringGroup.FindAsText(aPosition, aLength: integer; out aText: RawByteString);
var P: PRawByteStringGroupValue;
begin
  P := Find(aPosition);
  if P=nil then
    exit;
  dec(aPosition,P^.Position);
  if (aPosition=0) and (length(P^.Value)=aLength) then
    aText := P^.Value else // direct return if not yet compacted
    if aLength-aPosition<=length(P^.Value) then
      SetString(aText,PAnsiChar(@PByteArray(P^.Value)[aPosition]),aLength);
end;

function TRawByteStringGroup.FindAsText(aPosition, aLength: integer): RawByteString;
begin
  FindAsText(aPosition,aLength,result);
end;

{$ifndef NOVARIANTS}
procedure TRawByteStringGroup.FindAsVariant(aPosition, aLength: integer; out aDest: variant);
var tmp: RawByteString;
begin
  tmp := FindAsText(aPosition,aLength);
  if tmp <> '' then
    RawUTF8ToVariant(tmp,aDest);
end;
{$endif NOVARIANTS}

procedure TRawByteStringGroup.FindWrite(aPosition, aLength: integer;
  W: TTextWriter; Escape: TTextWriterKind; TrailingCharsToIgnore: integer);
var P: pointer;
begin
  P := Find(aPosition,aLength);
  if P<>nil then
    W.Add(PUTF8Char(P)+TrailingCharsToIgnore,aLength-TrailingCharsToIgnore,Escape);
end;

procedure TRawByteStringGroup.FindWriteBase64(aPosition, aLength: integer;
  W: TTextWriter; withMagic: boolean);
var P: pointer;
begin
  P := Find(aPosition,aLength);
  if P<>nil then
    W.WrBase64(P,aLength,withMagic);
end;

procedure TRawByteStringGroup.FindMove(aPosition, aLength: integer; aDest: pointer);
var P: pointer;
begin
  P := Find(aPosition,aLength);
  if P<>nil then
    MoveFast(P^,aDest^,aLength);
end;


{ TPropNameList }

procedure TPropNameList.Init;
begin
  Count := 0;
end;

function TPropNameList.FindPropName(const Value: RawUTF8): Integer;
begin
  result := SynCommons.FindPropName(Pointer(Values),Value,Count);
end;

function TPropNameList.AddPropName(const Value: RawUTF8): Boolean;
begin
  if (Value<>'') and (SynCommons.FindPropName(pointer(Values),Value,Count)<0) then begin
    if Count=length(Values) then
      SetLength(Values,NextGrow(Count));
    Values[Count] := Value;
    inc(Count);
    result := true;
  end else
    result := false;
end;


{ ************  Security and Identifiers classes ************************** }

{ TSynUniqueIdentifierBits }

function TSynUniqueIdentifierBits.Counter: word;
begin
  result := PWord(@Value)^ and $7fff;
end;

function TSynUniqueIdentifierBits.ProcessID: TSynUniqueIdentifierProcess;
begin
  result := (PCardinal(@Value)^ shr 15) and $ffff;
end;

function TSynUniqueIdentifierBits.CreateTimeUnix: TUnixTime;
begin
  result := Value shr 31;
end;

{$ifndef NOVARIANTS}
function TSynUniqueIdentifierBits.AsVariant: variant;
begin
  ToVariant(result);
end;

procedure TSynUniqueIdentifierBits.ToVariant(out result: variant);
begin
  TDocVariantData(result).InitObject(['Created',DateTimeToIso8601Text(CreateDateTime),
    'Identifier',ProcessID,'Counter',Counter,'Value',Value,
    'Hex',Int64ToHex(Value)],JSON_OPTIONS_FAST);
end;
{$endif NOVARIANTS}

{$ifndef DELPHI5OROLDER}
function TSynUniqueIdentifierBits.Equal(const Another: TSynUniqueIdentifierBits): boolean;
begin
  result := Value=Another.Value;
end;
{$endif}

procedure TSynUniqueIdentifierBits.From(const AID: TSynUniqueIdentifier);
begin
  Value := AID;
end;

function TSynUniqueIdentifierBits.CreateTimeLog: TTimeLog;
begin
  PTimeLogBits(@result)^.From(UnixTimeToDateTime(Value shr 31));
end;

function TSynUniqueIdentifierBits.CreateDateTime: TDateTime;
begin
  result := UnixTimeToDateTime(Value shr 31);
end;

function TSynUniqueIdentifierBits.ToHexa: RawUTF8;
begin
  Int64ToHex(Value,result);
end;

function TSynUniqueIdentifierBits.FromHexa(const hexa: RawUTF8): boolean;
begin
  result := (Length(hexa)=16) and HexDisplayToBin(pointer(hexa),@Value,SizeOf(Value));
end;

procedure TSynUniqueIdentifierBits.FromDateTime(const aDateTime: TDateTime);
begin
  Value := DateTimeToUnixTime(aDateTime) shl 31;
end;

procedure TSynUniqueIdentifierBits.FromUnixTime(const aUnixTime: TUnixTime);
begin
  Value := aUnixTime shl 31;
end;


{ TSynUniqueIdentifierGenerator }

const // fSafe.Padding[] slots
  SYNUNIQUEGEN_COMPUTECOUNT = 0;

procedure TSynUniqueIdentifierGenerator.ComputeNew(
  out result: TSynUniqueIdentifierBits);
var currentTime: cardinal;
begin
  currentTime := UnixTimeUTC; // under Windows faster than GetTickCount64
  fSafe.Lock;
  try
    if currentTime>fUnixCreateTime then begin
      fUnixCreateTime := currentTime;
      fLastCounter := 0; // reset
    end;
    if fLastCounter=$7fff then begin // collision (unlikely) -> cheat on timestamp
      inc(fUnixCreateTime);
      fLastCounter := 0;
    end else
      inc(fLastCounter);
    result.Value := Int64(fLastCounter or fIdentifierShifted) or
                    (Int64(fUnixCreateTime) shl 31);
    inc(fSafe.Padding[SYNUNIQUEGEN_COMPUTECOUNT].VInt64);
  finally
    fSafe.UnLock;
  end;
end;

function TSynUniqueIdentifierGenerator.ComputeNew: Int64;
begin
  ComputeNew(PSynUniqueIdentifierBits(@result)^);
end;

function TSynUniqueIdentifierGenerator.GetComputedCount: Int64;
begin
  {$ifdef NOVARIANTS}
  fSafe.Lock;
  result := fSafe.Padding[SYNUNIQUEGEN_COMPUTECOUNT].VInt64;
  fSafe.Unlock;
  {$else}
  result := fSafe.LockedInt64[SYNUNIQUEGEN_COMPUTECOUNT];
  {$endif}
end;

procedure TSynUniqueIdentifierGenerator.ComputeFromDateTime(const aDateTime: TDateTime;
  out result: TSynUniqueIdentifierBits);
begin // assume fLastCounter=0
  ComputeFromUnixTime(DateTimeToUnixTime(aDateTime),result);
end;

procedure TSynUniqueIdentifierGenerator.ComputeFromUnixTime(const aUnixTime: TUnixTime;
  out result: TSynUniqueIdentifierBits);
begin // assume fLastCounter=0
  result.Value := aUnixTime shl 31;
  if self<>nil then
    result.Value := result.Value or fIdentifierShifted;
end;

constructor TSynUniqueIdentifierGenerator.Create(aIdentifier: TSynUniqueIdentifierProcess;
  const aSharedObfuscationKey: RawUTF8);
var i, len: integer;
    crc: cardinal;
begin
  fIdentifier := aIdentifier;
  fIdentifierShifted := aIdentifier shl 15;
  fSafe.Init;
  fSafe.Padding[SYNUNIQUEGEN_COMPUTECOUNT].VType := varInt64; // reset to 0
  // compute obfuscation key using hash diffusion of the supplied text
  len := length(aSharedObfuscationKey);
  crc := crc32ctab[0,len and 1023];
  for i := 0 to high(fCrypto)+1 do begin
    crc := crc32ctab[0,crc and 1023] xor crc32ctab[3,i] xor
           kr32(crc,pointer(aSharedObfuscationKey),len) xor
           crc32c(crc,pointer(aSharedObfuscationKey),len) xor
           fnv32(crc,pointer(aSharedObfuscationKey),len);
    // do not modify those hashes above or you will break obfuscation pattern!
    if i<=high(fCrypto) then
      fCrypto[i] := crc else
      fCryptoCRC := crc;
  end;
  // due to the weakness of the hash algorithms used, this approach is a bit
  // naive and would be broken easily with brute force - but point here is to
  // hide/obfuscate public values at end-user level (e.g. when publishing URIs),
  // not implement strong security, so it sounds good enough for our purpose
end;

destructor TSynUniqueIdentifierGenerator.Destroy;
begin
  fSafe.Done;
  FillCharFast(fCrypto,SizeOf(fCrypto),0);
  fCryptoCRC := 0;
  inherited Destroy;
end;

type // compute a 24 hexadecimal chars (96 bits) obfuscated pseudo file name
  TSynUniqueIdentifierObfuscatedBits = packed record
    crc: cardinal;
    id: TSynUniqueIdentifierBits;
  end;

function TSynUniqueIdentifierGenerator.ToObfuscated(
  const aIdentifier: TSynUniqueIdentifier): TSynUniqueIdentifierObfuscated;
var bits: TSynUniqueIdentifierObfuscatedBits;
    key: cardinal;
begin
  result := '';
  if aIdentifier=0 then
    exit;
  bits.id.Value := aIdentifier;
  if self=nil then
    key := 0 else
    key := crc32ctab[0,bits.id.ProcessID and 1023] xor fCryptoCRC;
  bits.crc := crc32c(bits.id.ProcessID,@bits.id,SizeOf(bits.id)) xor key;
  if self<>nil then
    bits.id.Value := bits.id.Value xor PInt64(@fCrypto[high(fCrypto)-1])^;
  result := BinToHex(@bits,SizeOf(bits));
end;

function TSynUniqueIdentifierGenerator.FromObfuscated(
  const aObfuscated: TSynUniqueIdentifierObfuscated;
  out aIdentifier: TSynUniqueIdentifier): boolean;
var bits: TSynUniqueIdentifierObfuscatedBits;
    len: integer;
    key: cardinal;
begin
  result := false;
  len := PosExChar('.',aObfuscated);
  if len=0 then
    len := Length(aObfuscated) else
    dec(len); // trim right '.jpg'
  if (len<>SizeOf(bits)*2) or
     not SynCommons.HexToBin(pointer(aObfuscated),@bits,SizeOf(bits)) then
    exit;
  if self=nil then
    key := 0 else begin
    bits.id.Value := bits.id.Value xor PInt64(@fCrypto[high(fCrypto)-1])^;
    key := crc32ctab[0,bits.id.ProcessID and 1023] xor fCryptoCRC;
  end;
  if crc32c(bits.id.ProcessID,@bits.id,SizeOf(bits.id)) xor key=bits.crc then begin
    aIdentifier := bits.id.Value;
    result := true;
  end;
end;


{ TSynPersistentWithPassword }

destructor TSynPersistentWithPassword.Destroy;
begin
  UniqueRawUTF8(fPassword);
  FillZero(fPassword);
  inherited Destroy;
end;

class function TSynPersistentWithPassword.ComputePassword(const PlainPassword: RawUTF8;
  CustomKey: cardinal): RawUTF8;
var instance: TSynPersistentWithPassword;
begin
  instance := TSynPersistentWithPassword.Create;
  try
    instance.Key := CustomKey;
    instance.SetPassWordPlain(PlainPassword);
    result := instance.fPassWord;
  finally
    instance.Free;
  end;
end;

class function TSynPersistentWithPassword.ComputePassword(PlainPassword: pointer;
  PlainPasswordLen: integer; CustomKey: cardinal): RawUTF8;
begin
  result := ComputePassword(BinToBase64uri(PlainPassword,PlainPasswordLen));
end;

class function TSynPersistentWithPassword.ComputePlainPassword(const CypheredPassword: RawUTF8;
  CustomKey: cardinal; const AppSecret: RawUTF8): RawUTF8;
var instance: TSynPersistentWithPassword;
begin
  instance := TSynPersistentWithPassword.Create;
  try
    instance.Key := CustomKey;
    instance.fPassWord := CypheredPassword;
    result := instance.GetPassWordPlainInternal(AppSecret);
  finally
    instance.Free;
  end;
end;

function TSynPersistentWithPassword.GetPasswordFieldAddress: pointer;
begin
  result := @fPassword;
end;

function TSynPersistentWithPassword.GetKey: cardinal;
begin
  if self=nil then
    result := 0 else
    result := fKey xor $A5abba5A;
end;

function TSynPersistentWithPassword.GetPassWordPlain: RawUTF8;
begin
  result := GetPassWordPlainInternal('');
end;

function TSynPersistentWithPassword.GetPassWordPlainInternal(AppSecret: RawUTF8): RawUTF8;
var value,pass: RawByteString;
    usr: RawUTF8;
    i,j: integer;
begin
  result := '';
  if (self=nil) or (fPassWord='') then
    exit;
  if Assigned(TSynPersistentWithPasswordUserCrypt) then begin
    if AppSecret='' then
      ToText(ClassType,AppSecret);
    usr := ExeVersion.User+':';
    i := PosEx(usr,fPassword);
    if (i=1) or ((i>0) and (fPassword[i-1]=',')) then begin
      inc(i,length(usr));
      j := PosEx(',',fPassword,i);
      if j=0 then
        j := length(fPassword)+1;
      Base64ToBin(@fPassword[i],j-i,pass);
      if pass<>'' then
        result := TSynPersistentWithPasswordUserCrypt(pass,AppSecret,false);
    end else begin
      i := PosExChar(':',fPassword);
      if i>0 then
        raise ESynException.CreateUTF8('%.GetPassWordPlain unable to retrieve the '+
          'stored value: current user is [%], but password in % was encoded for [%]',
          [self,ExeVersion.User,AppSecret,copy(fPassword,1,i-1)]);
    end;
  end;
  if result='' then begin
    value := Base64ToBin(fPassWord);
    SymmetricEncrypt(GetKey,value);
    result := value;
  end;
end;

procedure TSynPersistentWithPassword.SetPassWordPlain(const value: RawUTF8);
var tmp: RawByteString;
begin
  if self=nil then
    exit;
  if value='' then begin
    fPassWord := '';
    exit;
  end;
  SetString(tmp,PAnsiChar(pointer(value)),Length(value)); // private copy
  SymmetricEncrypt(GetKey,tmp);
  fPassWord := BinToBase64(tmp);
end;


{ TSynConnectionDefinition }

constructor TSynConnectionDefinition.CreateFromJSON(const JSON: RawUTF8;
  Key: cardinal);
var privateCopy: RawUTF8;
    values: array[0..4] of TValuePUTF8Char;
begin
  fKey := Key;
  privateCopy := JSON;
  JSONDecode(privateCopy,['Kind','ServerName','DatabaseName','User','Password'],@values);
  fKind := values[0].ToString;
  values[1].ToUTF8(fServerName);
  values[2].ToUTF8(fDatabaseName);
  values[3].ToUTF8(fUser);
  values[4].ToUTF8(fPassWord);
end;

function TSynConnectionDefinition.SaveToJSON: RawUTF8;
begin
  result := JSONEncode(['Kind',fKind,'ServerName',fServerName,
    'DatabaseName',fDatabaseName,'User',fUser,'Password',fPassword]);
end;


{ TSynAuthenticationAbstract }

constructor TSynAuthenticationAbstract.Create;
begin
  fSafe.Init;
  fTokenSeed := Random32gsl;
  fSessionGenerator := abs(fTokenSeed*PPtrInt(self)^);
  fTokenSeed := abs(fTokenSeed*Random32gsl);
end;

destructor TSynAuthenticationAbstract.Destroy;
begin
  fSafe.Done;
  inherited;
end;

class function TSynAuthenticationAbstract.ComputeHash(Token: Int64;
  const UserName,PassWord: RawUTF8): cardinal;
begin // rough authentication - xxHash32 is less reversible than crc32c
  result := xxHash32(xxHash32(xxHash32(Token,@Token,SizeOf(Token)),
    pointer(UserName),length(UserName)),pointer(Password),length(PassWord));
end;

function TSynAuthenticationAbstract.ComputeCredential(previous: boolean;
  const UserName,PassWord: RawUTF8): cardinal;
var tok: Int64;
begin
  tok := GetTickCount64 div 10000;
  if previous then
    dec(tok);
  result := ComputeHash(tok xor fTokenSeed,UserName,PassWord);
end;

function TSynAuthenticationAbstract.CurrentToken: Int64;
begin
  result := (GetTickCount64 div 10000) xor fTokenSeed;
end;

procedure TSynAuthenticationAbstract.AuthenticateUser(const aName, aPassword: RawUTF8);
begin
  raise ESynException.CreateUTF8('%.AuthenticateUser() is not implemented',[self]);
end;

procedure TSynAuthenticationAbstract.DisauthenticateUser(const aName: RawUTF8);
begin
  raise ESynException.CreateUTF8('%.DisauthenticateUser() is not implemented',[self]);
end;

function TSynAuthenticationAbstract.CheckCredentials(const UserName: RaWUTF8;
  Hash: cardinal): boolean;
var password: RawUTF8;
begin
  result := GetPassword(UserName,password) and
     ((ComputeCredential(false,UserName,password)=Hash) or
      (ComputeCredential(true,UserName,password)=Hash));
end;

function TSynAuthenticationAbstract.CreateSession(const User: RawUTF8;
  Hash: cardinal): integer;
begin
  result := 0;
  fSafe.Lock;
  try
    if not CheckCredentials(User,Hash) then
      exit;
    repeat
      result := fSessionGenerator;
      inc(fSessionGenerator);
    until result<>0;
    AddSortedInteger(fSessions,fSessionsCount,result);
  finally
    fSafe.UnLock;
  end;
end;

function TSynAuthenticationAbstract.SessionExists(aID: integer): boolean;
begin
  fSafe.Lock;
  try
    result := FastFindIntegerSorted(pointer(fSessions),fSessionsCount-1,aID)>=0;
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynAuthenticationAbstract.RemoveSession(aID: integer);
var i: integer;
begin
  fSafe.Lock;
  try
    i := FastFindIntegerSorted(pointer(fSessions),fSessionsCount-1,aID);
    if i>=0 then
      DeleteInteger(fSessions,fSessionsCount,i);
  finally
    fSafe.UnLock;
  end;
end;


{ TSynAuthentication }

constructor TSynAuthentication.Create(const aUserName,aPassword: RawUTF8);
begin
  inherited Create;
  fCredentials.Init(true);
  if aUserName<>'' then
    AuthenticateUser(aUserName,aPassword);
end;

function TSynAuthentication.GetPassword(const UserName: RawUTF8;
  out Password: RawUTF8): boolean;
var i: integer;
begin // caller did protect this method via fSafe.Lock
  i := fCredentials.Find(UserName);
  if i<0 then begin
    result := false;
    exit;
  end;
  password := fCredentials.List[i].Value;
  result := true;
end;

function TSynAuthentication.GetUsersCount: integer;
begin
  fSafe.Lock;
  try
    result := fCredentials.Count;
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynAuthentication.AuthenticateUser(const aName, aPassword: RawUTF8);
begin
  fSafe.Lock;
  try
    fCredentials.Add(aName,aPassword);
  finally
    fSafe.UnLock;
  end;
end;

procedure TSynAuthentication.DisauthenticateUser(const aName: RawUTF8);
begin
  fSafe.Lock;
  try
    fCredentials.Delete(aName);
  finally
    fSafe.UnLock;
  end;
end;


{ TIPBan }

procedure TIPBan.LoadFromReader;
begin
  inherited;
  fReader.ReadVarUInt32Array(fIP4);
  fCount := length(fIP4);
end;

procedure TIPBan.SaveToWriter(aWriter: TFileBufferWriter);
begin // wkSorted not efficient: too big diffs between IPs
  aWriter.WriteVarUInt32Array(fIP4, fCount, wkUInt32);
end;

function TIPBan.Add(const aIP: RawUTF8): boolean;
var ip4: cardinal;
begin
  result := false;
  if (self=nil) or not IPToCardinal(aIP,ip4) then
    exit;
  fSafe.Lock;
  try
    AddSortedInteger(fIP4,fCount,ip4);
    result := true;
  finally
    fSafe.UnLock;
  end;
end;

function TIPBan.Delete(const aIP: RawUTF8): boolean;
var ip4: cardinal;
    i: integer;
begin
  result := false;
  if (self=nil) or not IPToCardinal(aIP,ip4) then
    exit;
  fSafe.Lock;
  try
    i := FastFindIntegerSorted(pointer(fIP4),fCount-1,ip4);
    if i<0 then
      exit;
    DeleteInteger(fIP4,fCount,i);
    result := true;
  finally
    fSafe.UnLock;
  end;
end;

function TIPBan.Exists(const aIP: RawUTF8): boolean;
var ip4: cardinal;
begin
  result := false;
  if (self=nil) or (fCount=0) or not IPToCardinal(aIP,ip4) then
    exit;
  fSafe.Lock;
  try
    if FastFindIntegerSorted(pointer(fIP4),fCount-1,ip4)>=0 then
      result := true;
  finally
    fSafe.UnLock;
  end;
end;

function TIPBan.DynArrayLocked: TDynArray;
begin
  fSafe.Lock;
  result.InitSpecific(TypeInfo(TCardinalDynArray),fIP4,djCardinal,@fCount);
end;


{ ************ Database types and classes ************************** }

{$ifdef FPC}{$push}{$endif}
{$WARNINGS OFF} // yes, we know there will be dead code below: we rely on it ;)

function IsZero(const Fields: TSQLFieldBits): boolean;
var f: TPtrIntArray absolute Fields;
begin
  {$ifdef CPU64}
  if MAX_SQLFIELDS=64 then
    result := (f[0]=0) else
  if MAX_SQLFields=128 then
    result := (f[0]=0) and (f[1]=0) else
  if MAX_SQLFields=192 then
    result := (f[0]=0) and (f[1]=0) and (f[2]=0) else
  if MAX_SQLFields=256 then
    result := (f[0]=0) and (f[1]=0) and (f[2]=0) and (f[3]=0) else
  {$else}
  if MAX_SQLFIELDS=64 then
    result := (f[0]=0) and (f[1]=0) else
  if MAX_SQLFields=128 then
    result := (f[0]=0) and (f[1]=0) and (f[2]=0) and (f[3]=0) else
  if MAX_SQLFields=192 then
    result := (f[0]=0) and (f[1]=0) and (f[2]=0) and (f[3]=0)
          and (f[4]=0) and (f[5]=0) else
  if MAX_SQLFields=256 then
    result := (f[0]=0) and (f[1]=0) and (f[2]=0) and (f[3]=0)
          and (f[4]=0) and (f[5]=0) and (f[6]=0) and (f[7]=0) else
  {$endif}
    result := IsZero(@Fields,SizeOf(Fields))
end;

function IsEqual(const A,B: TSQLFieldBits): boolean;
var a_: TPtrIntArray absolute A;
    b_: TPtrIntArray absolute B;
begin
  {$ifdef CPU64}
  if MAX_SQLFIELDS=64 then
    result := (a_[0]=b_[0]) else
  if MAX_SQLFields=128 then
    result := (a_[0]=b_[0]) and (a_[1]=b_[1]) else
  if MAX_SQLFields=192 then
    result := (a_[0]=b_[0]) and (a_[1]=b_[1]) and (a_[2]=b_[2]) else
  if MAX_SQLFields=256 then
    result := (a_[0]=b_[0]) and (a_[1]=b_[1]) and (a_[2]=b_[2]) and (a_[3]=b_[3]) else
  {$else}
  if MAX_SQLFIELDS=64 then
    result := (a_[0]=b_[0]) and (a_[1]=b_[1]) else
  if MAX_SQLFields=128 then
    result := (a_[0]=b_[0]) and (a_[1]=b_[1]) and (a_[2]=b_[2]) and (a_[3]=b_[3]) else
  if MAX_SQLFields=192 then
    result := (a_[0]=b_[0]) and (a_[1]=b_[1]) and (a_[2]=b_[2]) and (a_[3]=b_[3])
          and (a_[4]=b_[4]) and (a_[5]=b_[5]) else
  if MAX_SQLFields=256 then
    result := (a_[0]=b_[0]) and (a_[1]=b_[1]) and (a_[2]=b_[2]) and (a_[3]=b_[3])
          and (a_[4]=b_[4]) and (a_[5]=b_[5]) and (a_[6]=b_[6]) and (a_[7]=b_[7]) else
  {$endif}
    result := CompareMemFixed(@A,@B,SizeOf(TSQLFieldBits))
end;

procedure FillZero(var Fields: TSQLFieldBits);
begin
  if MAX_SQLFIELDS=64 then
    PInt64(@Fields)^ := 0 else
  if MAX_SQLFields=128 then begin
    PInt64Array(@Fields)^[0] := 0;
    PInt64Array(@Fields)^[1] := 0;
  end else
  if MAX_SQLFields=192 then begin
    PInt64Array(@Fields)^[0] := 0;
    PInt64Array(@Fields)^[1] := 0;
    PInt64Array(@Fields)^[2] := 0;
  end else
  if MAX_SQLFields=256 then begin
    PInt64Array(@Fields)^[0] := 0;
    PInt64Array(@Fields)^[1] := 0;
    PInt64Array(@Fields)^[2] := 0;
    PInt64Array(@Fields)^[3] := 0;
  end else
    FillCharFast(Fields,SizeOf(Fields),0);
end;

{$ifdef FPC}{$pop}{$else}{$WARNINGS ON}{$endif}

procedure FieldBitsToIndex(const Fields: TSQLFieldBits; var Index: TSQLFieldIndexDynArray;
  MaxLength,IndexStart: integer);
var i,n: integer;
    sets: array[0..MAX_SQLFIELDS-1] of TSQLFieldIndex; // to avoid memory reallocation
begin
  n := 0;
  for i := 0 to MaxLength-1 do
    if i in Fields then begin
      sets[n] := i;
      inc(n);
    end;
  SetLength(Index,IndexStart+n);
  for i := 0 to n-1 do
    Index[IndexStart+i] := sets[i];
end;

function FieldBitsToIndex(const Fields: TSQLFieldBits;
  MaxLength: integer): TSQLFieldIndexDynArray;
begin
  FieldBitsToIndex(Fields,result,MaxLength);
end;

function AddFieldIndex(var Indexes: TSQLFieldIndexDynArray; Field: integer): integer;
begin
  result := length(Indexes);
  SetLength(Indexes,result+1);
  Indexes[result] := Field;
end;

function SearchFieldIndex(var Indexes: TSQLFieldIndexDynArray; Field: integer): integer;
begin
  for result := 0 to length(Indexes)-1 do
    if Indexes[result]=Field then
      exit;
  result := -1;
end;

procedure FieldIndexToBits(const Index: TSQLFieldIndexDynArray; var Fields: TSQLFieldBits);
var i: integer;
begin
  FillCharFast(Fields,SizeOf(Fields),0);
  for i := 0 to Length(Index)-1 do
    if Index[i]>=0 then
      include(Fields,Index[i]);
end;

function FieldIndexToBits(const Index: TSQLFieldIndexDynArray): TSQLFieldBits;
begin
  FieldIndexToBits(Index,result);
end;

function DateToSQL(Date: TDateTime): RawUTF8;
begin
  result := '';
  if Date<=0 then
     exit;
  FastSetString(result,nil,13);
  PCardinal(pointer(result))^ := JSON_SQLDATE_MAGIC;
  DateToIso8601PChar(Date,PUTF8Char(pointer(result))+3,True);
end;

function DateToSQL(Year,Month,Day: Cardinal): RawUTF8;
begin
  result := '';
  if (Year=0) or (Month-1>11) or (Day-1>30) then
    exit;
  FastSetString(result,nil,13);
  PCardinal(pointer(result))^ := JSON_SQLDATE_MAGIC;
  DateToIso8601PChar(PUTF8Char(pointer(result))+3,True,Year,Month,Day);
end;

var
  JSON_SQLDATE_MAGIC_TEXT: RawUTF8;

function DateTimeToSQL(DT: TDateTime; WithMS: boolean): RawUTF8;
begin
  if DT<=0 then
    result := '' else begin
    if frac(DT)=0 then
      result := JSON_SQLDATE_MAGIC_TEXT+DateToIso8601(DT,true) else
    if trunc(DT)=0 then
      result := JSON_SQLDATE_MAGIC_TEXT+TimeToIso8601(DT,true,'T',WithMS) else
      result := JSON_SQLDATE_MAGIC_TEXT+DateTimeToIso8601(DT,true,'T',WithMS);
  end;
end;

function TimeLogToSQL(const Timestamp: TTimeLog): RawUTF8;
begin
  if Timestamp=0 then
    result := '' else
    result := JSON_SQLDATE_MAGIC_TEXT+PTimeLogBits(@Timestamp)^.Text(true);
end;

function Iso8601ToSQL(const S: RawByteString): RawUTF8;
begin
  if IsIso8601(pointer(S),length(S)) then
    result := JSON_SQLDATE_MAGIC_TEXT+S else
    result := '';
end;

function SQLToDateTime(const ParamValueWithMagic: RawUTF8): TDateTime;
begin
  result := Iso8601ToDateTimePUTF8Char(PUTF8Char(pointer(ParamValueWithMagic))+3,
    length(ParamValueWithMagic)-3);
end;

const
  NULL_LOW  = ord('n')+ord('u')shl 8+ord('l')shl 16+ord('l')shl 24;

function SQLParamContent(P: PUTF8Char; out ParamType: TSQLParamType; out ParamValue: RawUTF8;
  out wasNull: boolean): PUTF8Char;
var PBeg: PAnsiChar;
    L: integer;
    c: cardinal;
begin
  ParamType := sptUnknown;
  wasNull := false;
  result := nil;
  if P=nil then
    exit;
  while (P^<=' ') and (P^<>#0) do inc(P);
  case P^ of
  '''','"': begin
    P := UnQuoteSQLStringVar(P,ParamValue);
    if P=nil then
      exit; // not a valid quoted string (e.g. unexpected end in middle of it)
    ParamType := sptText;
    L := length(ParamValue)-3;
    if L>0 then begin
      c := PInteger(ParamValue)^ and $00ffffff;
      if c=JSON_BASE64_MAGIC then begin
        // ':("\uFFF0base64encodedbinary"):' format -> decode
        Base64MagicDecode(ParamValue); // wrapper function to avoid temp. string
        ParamType := sptBlob;
      end else
      if (c=JSON_SQLDATE_MAGIC) and // handle ':("\uFFF112012-05-04"):' format
         IsIso8601(PUTF8Char(pointer(ParamValue))+3,L) then begin
        Delete(ParamValue,1,3);   // return only ISO-8601 text
        ParamType := sptDateTime; // identified as Date/Time
      end;
    end;
  end;
  '-','+','0'..'9': begin // allow 0 or + in SQL
    // check if P^ is a true numerical value
    PBeg := pointer(P);
    ParamType := sptInteger;
    repeat inc(P) until not (P^ in ['0'..'9']); // check digits
    if P^='.' then begin
      inc(P);
      if P^ in ['0'..'9'] then begin
        ParamType := sptFloat;
        repeat inc(P) until not (P^ in ['0'..'9']); // check fractional digits
      end else begin
        ParamType := sptUnknown; // invalid '23023.' value
        exit;
      end;
    end;
    if byte(P^) and $DF=ord('E') then begin
      ParamType := sptFloat;
      inc(P);
      if P^='+' then inc(P) else
      if P^='-' then inc(P);
      while P^ in ['0'..'9'] do inc(P);
    end;
    FastSetString(ParamValue,PBeg,P-PBeg);
  end;
  'n':
  if PInteger(P)^=NULL_LOW then begin
    inc(P,4);
    wasNull := true;
  end else
    exit; // invalid content (only :(null): expected)
  else
    exit; // invalid content
  end;
  while (P^<=' ') and (P^<>#0) do inc(P);
  if PWord(P)^<>Ord(')')+Ord(':')shl 8 then
    // we expect finishing with P^ pointing at '):'
    ParamType := sptUnknown else
    // result<>nil only if value content in P^
    result := P+2;
end;

function ExtractInlineParameters(const SQL: RawUTF8;
  var Types: TSQLParamTypeDynArray; var Values: TRawUTF8DynArray;
  var maxParam: integer; var Nulls: TSQLFieldBits): RawUTF8;
var ppBeg: integer;
    P, Gen: PUTF8Char;
    wasNull: boolean;
begin
  maxParam := 0;
  FillCharFast(Nulls,SizeOf(Nulls),0);
  ppBeg := PosEx(RawUTF8(':('),SQL,1);
  if (ppBeg=0) or (PosEx(RawUTF8('):'),SQL,ppBeg+2)=0) then begin
    // SQL code with no valid :(...): internal parameters -> leave maxParam=0
    result := SQL;
    exit;
  end;
  // compute GenericSQL from SQL, converting :(...): into ?
  FastSetString(result,pointer(SQL),length(SQL)); // private copy for unescape
  P := pointer(result); // in-place string unescape (keep SQL untouched)
  Gen := P+ppBeg-1; // Gen^ just before :(
  inc(P,ppBeg+1);   // P^ just after :(
  repeat
    Gen^ := '?'; // replace :(...): by ?
    inc(Gen);
    if length(Values)<=maxParam then
      SetLength(Values,maxParam+16);
    if length(Types)<=maxParam then
      SetLength(Types,maxParam+64);
    P := SQLParamContent(P,Types[maxParam],Values[maxParam],wasNull);
    if P=nil then begin
      maxParam := 0;
      result := SQL;
      exit; // any invalid parameter -> try direct SQL
    end;
    if wasNull then
      include(Nulls,maxParam);
    while (P^<>#0) and (PWord(P)^<>Ord(':')+Ord('(')shl 8) do begin
      Gen^ := P^;
      inc(Gen);
      inc(P);
    end;
    if P^=#0 then
      Break;
    inc(P,2);
    inc(maxParam);
  until false;
  // return generic SQL statement, with ? place-holders and params in Values[]
  SetLength(result,Gen-pointer(result));
  inc(maxParam);
end;

function InlineParameter(ID: Int64): shortstring;
begin
  FormatShort(':(%):',[ID],result);
end;

function InlineParameter(const value: RawUTF8): RawUTF8;
begin
  QuotedStrJSON(value,result,':(','):');
end;

function SQLVarLength(const Value: TSQLVar): integer;
begin
  case Value.VType of
    ftBlob:
      result := Value.VBlobLen;
    ftUTF8:
      result := StrLen(Value.VText); // fast enough for our purpose
    else
      result := 0; // simple/ordinal values, or ftNull
  end;
end;

{$ifndef NOVARIANTS}

procedure VariantToSQLVar(const Input: variant; var temp: RawByteString;
  var Output: TSQLVar);
var wasString: boolean;
begin
  Output.Options := [];
  with TVarData(Input) do
  if VType=varVariant or varByRef then
    VariantToSQLVar(PVariant(VPointer)^,temp,Output) else
  case VType of
  varEmpty, varNull:
    Output.VType := ftNull;
  varByte: begin
    Output.VType := ftInt64;
    Output.VInt64 := VByte;
  end;
  varInteger: begin
    Output.VType := ftInt64;
    Output.VInt64 := VInteger;
  end;
  {$ifndef DELPHI5OROLDER}
  varLongWord: begin
    Output.VType := ftInt64;
    Output.VInt64 := VLongWord;
  end;
  {$endif}
  varWord64, varInt64: begin
    Output.VType := ftInt64;
    Output.VInt64 := VInt64;
  end;
  varSingle: begin
    Output.VType := ftDouble;
    Output.VDouble := VSingle;
  end;
  varDouble: begin // varDate would be converted into ISO8601 by VariantToUTF8()
    Output.VType := ftDouble;
    Output.VDouble := VDouble;
  end;
  varCurrency: begin
    Output.VType := ftCurrency;
    Output.VInt64 := VInt64;
  end;
  varString: begin // assume RawUTF8
    Output.VType := ftUTF8;
    Output.VText := VPointer;
  end;
  else // handle less current cases
    if VariantToInt64(Input,Output.VInt64) then
      Output.VType := ftInt64 else begin
      VariantToUTF8(Input,RawUTF8(temp),wasString);
      if wasString then begin
        Output.VType := ftUTF8;
        Output.VText := pointer(temp);
      end else
        Output.VType := ftNull;
    end;
  end;
end;

function VariantVTypeToSQLDBFieldType(VType: cardinal): TSQLDBFieldType;
begin
  case VType of
  varNull:
    result := ftNull;
  {$ifndef DELPHI5OROLDER}varShortInt, varWord, varLongWord,{$endif}
  varSmallInt, varByte, varBoolean, varInteger, varInt64, varWord64:
    result := ftInt64;
  varSingle,varDouble:
    result := ftDouble;
  varDate:
    result := ftDate;
  varCurrency:
    result := ftCurrency;
  varString:
    result := ftUTF8;
  else
    result := ftUnknown; // includes varEmpty
  end;
end;

function VariantTypeToSQLDBFieldType(const V: Variant): TSQLDBFieldType;
var VD: TVarData absolute V;
    tmp: TVarData;
begin
  result := VariantVTypeToSQLDBFieldType(VD.VType);
  case result of
    ftUnknown:
      if VD.VType=varEmpty then
        result := ftUnknown else
      if SetVariantUnRefSimpleValue(V,tmp) then
        result := VariantTypeToSQLDBFieldType(variant(tmp)) else
        result := ftUTF8;
    ftUTF8:
      if (VD.VString<>nil) and (PCardinal(VD.VString)^ and $ffffff=JSON_BASE64_MAGIC) then
        result := ftBlob;
  end;
end;

function TextToSQLDBFieldType(json: PUTF8Char): TSQLDBFieldType;
begin
  if json=nil then
    result := ftNull else
    result := VariantVTypeToSQLDBFieldType(TextToVariantNumberType(json));
end;

function NullableInteger(const Value: Int64): TNullableInteger;
begin
  PVariant(@result)^ := Value;
end;

function NullableIntegerIsEmptyOrNull(const V: TNullableInteger): Boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableIntegerToValue(const V: TNullableInteger; out Value: Int64): Boolean;
begin
  Value := 0;
  result := not VarDataIsEmptyOrNull(@V) and VariantToInt64(PVariant(@V)^,Value);
end;

function NullableIntegerToValue(const V: TNullableInteger): Int64;
begin
  VariantToInt64(PVariant(@V)^,result);
end;

function NullableBoolean(Value: boolean): TNullableBoolean;
begin
  PVariant(@result)^ := Value;
end;

function NullableBooleanIsEmptyOrNull(const V: TNullableBoolean): Boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableBooleanToValue(const V: TNullableBoolean; out Value: Boolean): Boolean;
begin
  Value := false;
  result := not VarDataIsEmptyOrNull(@V) and VariantToBoolean(PVariant(@V)^,Value);
end;

function NullableBooleanToValue(const V: TNullableBoolean): Boolean;
begin
  VariantToBoolean(PVariant(@V)^,result);
end;


function NullableFloat(const Value: double): TNullableFloat;
begin
  PVariant(@result)^ := Value;
end;

function NullableFloatIsEmptyOrNull(const V: TNullableFloat): Boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableFloatToValue(const V: TNullableFloat; out Value: Double): Boolean;
begin
  Value := 0;
  result := not VarDataIsEmptyOrNull(@V) and VariantToDouble(PVariant(@V)^,Value);
end;

function NullableFloatToValue(const V: TNullableFloat): Double;
begin
  VariantToDouble(PVariant(@V)^,result);
end;


function NullableCurrency(const Value: currency): TNullableCurrency;
begin
  PVariant(@result)^ := Value;
end;

function NullableCurrencyIsEmptyOrNull(const V: TNullableCurrency): Boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableCurrencyToValue(const V: TNullableCurrency; out Value: currency): Boolean;
begin
  Value := 0;
  result := not VarDataIsEmptyOrNull(@V) and VariantToCurrency(PVariant(@V)^,Value);
end;

function NullableCurrencyToValue(const V: TNullableCurrency): currency;
begin
  VariantToCurrency(PVariant(@V)^,result);
end;


function NullableDateTime(const Value: TDateTime): TNullableDateTime;
begin
  PVariant(@result)^ := Value;
end;

function NullableDateTimeIsEmptyOrNull(const V: TNullableDateTime): Boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableDateTimeToValue(const V: TNullableDateTime; out Value: TDateTime): Boolean;
begin
  Value := 0;
  result := not VarDataIsEmptyOrNull(@V) and VariantToDouble(PVariant(@V)^,Double(Value));
end;

function NullableDateTimeToValue(const V: TNullableDateTime): TDateTime;
begin
  VariantToDouble(PVariant(@V)^,Double(result));
end;


function NullableTimeLog(const Value: TTimeLog): TNullableTimeLog;
begin
  PVariant(@result)^ := Value;
end;

function NullableTimeLogIsEmptyOrNull(const V: TNullableTimeLog): Boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableTimeLogToValue(const V: TNullableTimeLog; out Value: TTimeLog): Boolean;
begin
  Value := 0;
  result := not VarDataIsEmptyOrNull(@V) and VariantToInt64(PVariant(@V)^,PInt64(@Value)^);
end;

function NullableTimeLogToValue(const V: TNullableTimeLog): TTimeLog;
begin
  VariantToInt64(PVariant(@V)^,PInt64(@result)^);
end;


function NullableUTF8Text(const Value: RawUTF8): TNullableUTF8Text;
begin
  VarClear(PVariant(@result)^);
  TVarData(result).VType := varString;
  TVarData(result).VAny := nil; // avoid GPF below
  RawUTF8(TVarData(result).VAny) := Value;
end;

function NullableUTF8TextIsEmptyOrNull(const V: TNullableUTF8Text): Boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableUTF8TextToValue(const V: TNullableUTF8Text; out Value: RawUTF8): boolean;
begin
  result := not VarDataIsEmptyOrNull(@V) and VariantToUTF8(PVariant(@V)^,Value);
end;

function NullableUTF8TextToValue(const V: TNullableUTF8Text): RawUTF8;
var dummy: boolean;
begin
  if VarDataIsEmptyOrNull(@V) then // VariantToUTF8() will return 'null'
    result := '' else
    VariantToUTF8(PVariant(@V)^,result,dummy);
end;


{$endif NOVARIANTS}

{ TJSONWriter }

procedure TJSONWriter.CancelAllVoid;
const VOIDARRAY: PAnsiChar = '[]'#10;
      VOIDFIELD: PAnsiChar = '{"FieldCount":0}';
begin
  CancelAll; // rewind JSON
  if fExpand then // same as sqlite3_get_table()
    inc(fTotalFileSize,fStream.Write(VOIDARRAY^,3)) else
    inc(fTotalFileSize,fStream.Write(VOIDFIELD^,16));
end;

constructor TJSONWriter.Create(aStream: TStream; Expand, withID: boolean;
  const Fields: TSQLFieldBits; aBufSize: integer);
begin
  Create(aStream,Expand,withID,FieldBitsToIndex(Fields),aBufSize);
end;

constructor TJSONWriter.Create(aStream: TStream; Expand, withID: boolean;
  const Fields: TSQLFieldIndexDynArray; aBufSize: integer;
  aStackBuffer: PTextWriterStackBuffer);
begin
  if aStream=nil then
    if aStackBuffer<>nil then
      CreateOwnedStream(aStackBuffer^) else
      CreateOwnedStream(aBufSize) else
    if aStackBuffer<>nil then
      inherited Create(aStream,aStackBuffer,SizeOf(aStackBuffer^)) else
      inherited Create(aStream,aBufSize);
  fExpand := Expand;
  fWithID := withID;
  fFields := Fields;
end;

procedure TJSONWriter.AddColumns(aKnownRowsCount: integer);
var i: integer;
begin
  if fExpand then begin
    if twoForceJSONExtended in CustomOptions then
      for i := 0 to High(ColNames) do
        ColNames[i] := ColNames[i]+':' else
      for i := 0 to High(ColNames) do
        ColNames[i] := '"'+ColNames[i]+'":';
  end else begin
    AddShort('{"fieldCount":');
    Add(length(ColNames));
    if aKnownRowsCount>0 then begin
      AddShort(',"rowCount":');
      Add(aKnownRowsCount);
    end;
    AddShort(',"values":["');
    // first row is FieldNames
    for i := 0 to High(ColNames) do begin
      AddString(ColNames[i]);
      AddNoJSONEscape(PAnsiChar('","'),3);
    end;
    CancelLastChar('"');
    fStartDataPosition := fStream.Position+(B-fTempBuf);
     // B := buf-1 at startup -> need ',val11' position in
     // "values":["col1","col2",val11,' i.e. current pos without the ','
  end;
end;

procedure TJSONWriter.ChangeExpandedFields(aWithID: boolean;
  const aFields: TSQLFieldIndexDynArray);
begin
  if not Expand then
    raise ESynException.CreateUTF8(
      '%.ChangeExpandedFields() called with Expanded=false',[self]);
  fWithID := aWithID;
  fFields := aFields;
end;

procedure TJSONWriter.EndJSONObject(aKnownRowsCount,aRowsCount: integer;
  aFlushFinal: boolean);
begin
  CancelLastComma; // cancel last ','
  Add(']');
  if not fExpand then begin
    if aKnownRowsCount=0 then begin
      AddShort(',"rowCount":');
      Add(aRowsCount);
    end;
    Add('}');
  end;
  Add(#10);
  if aFlushFinal then
    FlushFinal;
end;

procedure TJSONWriter.TrimFirstRow;
var P, PBegin, PEnd: PUTF8Char;
begin
  if (self=nil) or not fStream.InheritsFrom(TMemoryStream) or
     fExpand or (fStartDataPosition=0) then
    exit;
  // go to begin of first row
  FlushToStream; // we need the data to be in fStream memory
  // PBegin^=val11 in { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
  PBegin := TMemoryStream(fStream).Memory;
  PEnd := PBegin+fStream.Position;
  PEnd^ := #0; // mark end of current values
  inc(PBegin,fStartDataPosition+1); // +1 to include ',' of ',val11'
  // jump to end of first row
  P := GotoNextJSONItem(PBegin,length(ColNames));
  if P=nil then exit; // unexpected end
  // trim first row data
  if P^<>#0 then
    MoveFast(P^,PBegin^,PEnd-P); // erase content
  fStream.Seek(PBegin-P,soCurrent); // adjust current stream position
end;


{ ************ Expression Search Engine ************************** }

function ToText(r: TExprParserResult): PShortString;
begin
  result := GetEnumName(TypeInfo(TExprParserResult), ord(r));
end;

function ToUTF8(r: TExprParserResult): RawUTF8;
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
  inherited Create;
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
  inherited Create;
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
  if (fCurrentWord = '') or (fCurrentWord = ')') then
    exit;
  if IdemPropNameU(fCurrentWord, fAndWord) then begin // w1 & w2 = w1 AND w2
    ParseNextCurrentWord;
    if result.Append(ParseExpr) then
      result.Append(TExprNode.Create(entAnd));
    exit;
  end
  else if IdemPropNameU(fCurrentWord, fOrWord) then begin // w1 + w2 = w1 OR w2
    ParseNextCurrentWord;
    if result.Append(ParseExpr) then
      result.Append(TExprNode.Create(entOr));
    exit;
  end
  else if fNoWordIsAnd and result.Append(ParseExpr) then // 'w1 w2' = 'w1 & w2'
    result.Append(TExprNode.Create(entAnd));
end;

function TParserAbstract.ParseFactor: TExprNode;
begin
  if fCurrentError <> eprSuccess then
    result := nil
  else if IdemPropNameU(fCurrentWord, fNotWord) then begin
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
  if fCurrentWord = '(' then begin
    ParseNextCurrentWord;
    result := ParseExpr;
    if fCurrentError <> eprSuccess then
      exit;
    if fCurrentWord <> ')' then begin
      FreeAndNil(result);
      fCurrentError := eprMissingParenthesis;
    end;
  end
  else if fCurrentWord = '' then begin
    result := nil;
    fCurrentError := eprMissingFinalWord;
  end
  else
    try // calls meta-class overriden constructor
      result := fWordClass.Create(self, fCurrentWord);
      fCurrentError := TExprNodeWordAbstract(result).ParseWord;
      if fCurrentError <> eprSuccess then begin
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

function TParserAbstract.Parse(const aExpression: RawUTF8): TExprParserResult;
var
  depth: integer;
  n: TExprNode;
begin
  Clear;
  fCurrentError := eprSuccess;
  fCurrent := pointer(aExpression);
  ParseNextCurrentWord;
  if fCurrentWord = '' then begin
    result := eprNoExpression;
    exit;
  end;
  fFirstNode := ParseExpr;
  result := fCurrentError;
  if result = eprSuccess then begin
    depth := 0;
    n := fFirstNode;
    while n <> nil do begin
      case n.NodeType of
        entWord: begin
          inc(depth);
          if depth > high(fFoundStack) then begin
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

class function TParserAbstract.ParseError(const aExpression: RawUTF8): RawUTF8;
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
      result := ToUTF8(res);
  finally
    parser.Free;
  end;
end;

function TParserAbstract.Execute: boolean;
var
  n: TExprNode;
  st: PBoolean;
begin // code below compiles very efficiently on FPC/x86-64
  st := @fFoundStack;
  n := fFirstNode;
  repeat
    case n.NodeType of
      entWord: begin
        st^ := TExprNodeWordAbstract(n).fFound;
        inc(st); // see eprTooManyParenthesis above to avoid buffer overflow
      end;
      entNot:
        PAnsiChar(st)[-1] := AnsiChar(ord(PAnsiChar(st)[-1]) xor 1);
      entOr: begin
        dec(st);
        PAnsiChar(st)[-1] := AnsiChar(st^ or boolean(PAnsiChar(st)[-1]));
      end; { TODO : optimize TExprParser OR when left member is already TRUE }
      entAnd: begin
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
  P: PUTF8Char;
begin
  fCurrentWord := '';
  P := fCurrent;
  if P = nil then
    exit;
  while P^ in [#1..' '] do
    inc(P);
  if P^ = #0 then
    exit;
  if P^ in PARSER_STOPCHAR then begin
    FastSetString(fCurrentWord, P, 1);
    fCurrent := P + 1;
  end
  else begin
    fCurrent := P;
    ParseNextWord;
  end;
end;

procedure TExprParserAbstract.ParseNextWord;
const
  STOPCHAR = PARSER_STOPCHAR + [#0, ' '];
var
  P: PUTF8Char;
begin
  P := fCurrent;
  while not(P^ in STOPCHAR) do
    inc(P);
  FastSetString(fCurrentWord, fCurrent, P - fCurrent);
  fCurrent := P;
end;


{ TExprNodeWordAbstract }

constructor TExprNodeWordAbstract.Create(aOwner: TParserAbstract; const aWord: RawUTF8);
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
  PExprParserMatchNode = ^TExprParserMatchNode;

function TExprParserMatchNode.ParseWord: TExprParserResult;
begin
  fMatch.Prepare(fWord, (fOwner as TExprParserMatch).fCaseSensitive, {reuse=}true);
  result := eprSuccess;
end;


{ TExprParserMatch }

constructor TExprParserMatch.Create(aCaseSensitive: boolean);
begin
  inherited Create;
  fCaseSensitive := aCaseSensitive;
end;

procedure TExprParserMatch.Initialize;
begin
  inherited Initialize;
  fWordClass := TExprParserMatchNode;
end;

function TExprParserMatch.Search(const aText: RawUTF8): boolean;
begin
  result := Search(pointer(aText), length(aText));
end;

function TExprParserMatch.Search(aText: PUTF8Char; aTextLen: PtrInt): boolean;
const // rough estimation of UTF-8 characters
  IS_UTF8_WORD = ['0' .. '9', 'A' .. 'Z', 'a' .. 'z', #$80 ..#$ff];
var
  P, PEnd: PUTF8Char;
  n: PtrInt;
begin
  P := aText;
  if (P = nil) or (fWords = nil) then begin
    result := false;
    exit;
  end;
  if fMatchedLastSet > 0 then begin
    n := fWordCount;
    repeat
      dec(n);
      fWords[n].fFound := false;
    until n = 0;
    fMatchedLastSet := 0;
  end;
  PEnd := P + aTextLen;
  while (P < PEnd) and (fMatchedLastSet < fWordCount) do begin
    while not(P^ in IS_UTF8_WORD) do begin
      inc(P);
      if P = PEnd then 
        break;
    end;
    if P = PEnd then
      break;
    aText := P;
    repeat
      inc(P);
    until (P = PEnd) or not(P^ in IS_UTF8_WORD);
    aTextLen := P - aText; // now aText/aTextLen point to a word
    n := fWordCount;
    repeat
      dec(n);
      with TExprParserMatchNode(fWords[n]) do
        if not fFound and fMatch.Match(aText, aTextLen) then begin
          fFound := true;
          inc(fMatchedLastSet);
        end;
    until n = 0;
  end;
  result := Execute;
end;

{ ************ Multi-Threading classes ************************** }

{ TPendingTaskList }

constructor TPendingTaskList.Create;
begin
  inherited Create;
  fTasks.InitSpecific(TypeInfo(TPendingTaskListItemDynArray),fTask,djInt64,@fCount);
end;

function TPendingTaskList.GetTimestamp: Int64;
begin
  result := {$ifdef FPCLINUX}SynFPCLinux.{$endif}GetTickCount64;
end;

procedure TPendingTaskList.AddTask(aMilliSecondsDelayFromNow: integer;
  const aTask: RawByteString);
var item: TPendingTaskListItem;
    ndx: integer;
begin
  item.Timestamp := GetTimestamp+aMilliSecondsDelayFromNow;
  item.Task := aTask;
  fSafe.Lock;
  try
    if fTasks.FastLocateSorted(item,ndx) then
      inc(ndx); // always insert just after any existing timestamp
    fTasks.FastAddSorted(ndx,item);
  finally
    fSafe.UnLock;
  end;
end;

procedure TPendingTaskList.AddTasks(
  const aMilliSecondsDelays: array of integer;
  const aTasks: array of RawByteString);
var item: TPendingTaskListItem;
    i,ndx: integer;
begin
  if length(aTasks)<>length(aMilliSecondsDelays) then
    exit;
  item.Timestamp := GetTimestamp;
  fSafe.Lock;
  try
    for i := 0 to High(aTasks) do begin
      inc(item.Timestamp,aMilliSecondsDelays[i]);
      item.Task := aTasks[i];
      if fTasks.FastLocateSorted(item,ndx) then
        inc(ndx); // always insert just after any existing timestamp
      fTasks.FastAddSorted(ndx,item);
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TPendingTaskList.GetCount: integer;
begin
  if self=nil then
    result := 0 else begin
    fSafe.Lock;
    try
      result := fCount;
    finally
      fSafe.UnLock;
    end;
  end;
end;

function TPendingTaskList.NextPendingTask: RawByteString;
begin
  result := '';
  if (self=nil) or (fCount=0) then
    exit;
  fSafe.Lock;
  try
    if fCount>0 then
      if GetTimestamp>=fTask[0].Timestamp then begin
        result := fTask[0].Task;
        fTasks.FastDeleteSorted(0);
      end;
  finally
    fSafe.UnLock;
  end;
end;

procedure TPendingTaskList.Clear;
begin
  if (self=nil) or (fCount=0) then
    exit;
  fSafe.Lock;
  try
    fTasks.Clear;
  finally
    fSafe.UnLock;
  end;
end;


{$ifndef LVCL} // LVCL does not implement TEvent

{ TSynBackgroundThreadAbstract }

constructor TSynBackgroundThreadAbstract.Create(const aThreadName: RawUTF8;
  OnBeforeExecute,OnAfterExecute: TNotifyThreadEvent; CreateSuspended: boolean);
begin
  fProcessEvent := TEvent.Create(nil,false,false,'');
  fThreadName := aThreadName;
  fOnBeforeExecute := OnBeforeExecute;
  fOnAfterExecute := OnAfterExecute;
  inherited Create(CreateSuspended{$ifdef FPC},512*1024{$endif}); // DefaultStackSize=512KB
end;

{$ifndef HASTTHREADSTART}
procedure TSynBackgroundThreadAbstract.Start;
begin
  Resume;
end;
{$endif}

{$ifndef HASTTHREADTERMINATESET}
procedure TSynBackgroundThreadAbstract.Terminate;
begin
  inherited Terminate; // FTerminated := True
  TerminatedSet;
end;
{$endif}

procedure TSynBackgroundThreadAbstract.TerminatedSet;
begin
  fProcessEvent.SetEvent; // ExecuteLoop should handle Terminated flag
end;

procedure TSynBackgroundThreadAbstract.WaitForNotExecuting(maxMS: integer);
var endtix: Int64;
begin
  if fExecute=exRun then begin
    endtix := SynCommons.GetTickCount64+maxMS;
    repeat
      SleepHiRes(1); // wait for Execute to finish
    until (fExecute<>exRun) or (SynCommons.GetTickCount64>=endtix);
  end;
end;

destructor TSynBackgroundThreadAbstract.Destroy;
begin
  if fExecute=exRun then begin
    Terminate;
    WaitForNotExecuting(100);
  end;
  inherited Destroy;
  FreeAndNil(fProcessEvent);
end;

procedure TSynBackgroundThreadAbstract.SetExecuteLoopPause(dopause: boolean);
begin
  if Terminated or (dopause=fExecuteLoopPause) or (fExecute=exFinished) then
    exit;
  fExecuteLoopPause := dopause;
  fProcessEvent.SetEvent; // notify Execute main loop
end;

procedure TSynBackgroundThreadAbstract.Execute;
begin
  try
    if fThreadName='' then
      SetCurrentThreadName('%(%)',[self,pointer(self)]) else
      SetCurrentThreadName('%',[fThreadName]);
    if Assigned(fOnBeforeExecute) then
      fOnBeforeExecute(self);
    try
      fExecute := exRun;
      while not Terminated do
        if fExecuteLoopPause then
          FixedWaitFor(fProcessEvent,100) else
          ExecuteLoop;
    finally
      if Assigned(fOnAfterExecute) then
        fOnAfterExecute(self);
    end;
  finally
    fExecute := exFinished;
  end;
end;

{ TSynBackgroundThreadMethodAbstract }

constructor TSynBackgroundThreadMethodAbstract.Create(aOnIdle: TOnIdleSynBackgroundThread;
  const aThreadName: RawUTF8; OnBeforeExecute,OnAfterExecute: TNotifyThreadEvent);
begin
  fOnIdle := aOnIdle; // cross-platform may run Execute as soon as Create is called
  fCallerEvent := TEvent.Create(nil,false,false,'');
  fPendingProcessLock.Init;
  inherited Create(aThreadName,OnBeforeExecute,OnAfterExecute);
end;

destructor TSynBackgroundThreadMethodAbstract.Destroy;
begin
  SetPendingProcess(flagDestroying);
  fProcessEvent.SetEvent;  // notify terminated
  FixedWaitForever(fCallerEvent); // wait for actual termination
  FreeAndNil(fCallerEvent);
  inherited Destroy;
  fPendingProcessLock.Done;
end;

function TSynBackgroundThreadMethodAbstract.GetPendingProcess: TSynBackgroundThreadProcessStep;
begin
  fPendingProcessLock.Lock;
  result := fPendingProcessFlag;
  fPendingProcessLock.UnLock;
end;

procedure TSynBackgroundThreadMethodAbstract.SetPendingProcess(State: TSynBackgroundThreadProcessStep);
begin
  fPendingProcessLock.Lock;
  fPendingProcessFlag := State;
  fPendingProcessLock.UnLock;
end;

procedure TSynBackgroundThreadMethodAbstract.ExecuteLoop;
{$ifndef DELPHI5OROLDER}
var E: TObject;
{$endif}
begin
  case FixedWaitFor(fProcessEvent,INFINITE) of
    wrSignaled:
      case GetPendingProcess of
      flagDestroying: begin
        fCallerEvent.SetEvent; // abort caller thread process
        Terminate; // forces Execute loop ending
        exit;
      end;
      flagStarted:
        if not Terminated then
          if fExecuteLoopPause then // pause -> try again later
            fProcessEvent.SetEvent else
            try
              fBackgroundException := nil;
              try
                if Assigned(fOnBeforeProcess) then
                  fOnBeforeProcess(self);
                try
                  Process;
                finally
                  if Assigned(fOnAfterProcess) then
                    fOnAfterProcess(self);
                end;
              except
                {$ifdef DELPHI5OROLDER}
                on E: Exception do
                  fBackgroundException := ESynException.CreateUTF8(
                    'Redirected % [%]',[E,E.Message]);
                {$else}
                E := AcquireExceptionObject;
                if E.InheritsFrom(Exception) then
                  fBackgroundException := Exception(E);
                {$endif}
              end;
            finally
              SetPendingProcess(flagFinished);
              fCallerEvent.SetEvent;
            end;
     end;
  end;
end;

function TSynBackgroundThreadMethodAbstract.AcquireThread: TSynBackgroundThreadProcessStep;
begin
  fPendingProcessLock.Lock;
  try
    result := fPendingProcessFlag;
    if result=flagIdle then begin // we just acquired the thread! congrats!
      fPendingProcessFlag := flagStarted; // atomic set "started" flag
      fCallerThreadID := ThreadID;
    end;
  finally
    fPendingProcessLock.UnLock;
  end;
end;

function TSynBackgroundThreadMethodAbstract.OnIdleProcessNotify(start: Int64): integer;
begin
  result := {$ifdef FPCLINUX}SynFPCLinux.{$else}SynCommons.{$endif}GetTickCount64-start;
  if result<0 then
    result := MaxInt; // should happen only under XP -> ignore
  if Assigned(fOnIdle) then
    fOnIdle(self,result) ;
end;

procedure TSynBackgroundThreadMethodAbstract.WaitForFinished(start: Int64;
  const onmainthreadidle: TNotifyEvent);
var E: Exception;
begin
  if (self=nil) or not(fPendingProcessFlag in [flagStarted, flagFinished]) then
    exit; // nothing to wait for
  try
    if Assigned(onmainthreadidle) then begin
      while FixedWaitFor(fCallerEvent,100)=wrTimeout do
        onmainthreadidle(self);
    end else
    {$ifdef MSWINDOWS} // do process the OnIdle only if UI
    if Assigned(fOnIdle) then begin
      while FixedWaitFor(fCallerEvent,100)=wrTimeout do
        OnIdleProcessNotify(start);
    end else
    {$endif}
      FixedWaitForever(fCallerEvent);
    if fPendingProcessFlag<>flagFinished then
      ESynException.CreateUTF8('%.WaitForFinished: flagFinished?',[self]);
    if fBackgroundException<>nil then begin
      E := fBackgroundException;
      fBackgroundException := nil;
      raise E; // raise background exception in the calling scope
    end;
  finally
    fParam := nil;
    fCallerThreadID := 0;
    FreeAndNil(fBackgroundException);
    SetPendingProcess(flagIdle);
    if Assigned(fOnIdle) then
      fOnIdle(self,-1); // notify finished
  end;
end;

function TSynBackgroundThreadMethodAbstract.RunAndWait(OpaqueParam: pointer): boolean;
var start: Int64;
    ThreadID: TThreadID;
begin
  result := false;
  ThreadID := GetCurrentThreadId;
  if (self=nil) or (ThreadID=fCallerThreadID) then
    // avoid endless loop when waiting in same thread (e.g. UI + OnIdle)
    exit;
  // 1. wait for any previous request to be finished (should not happen often)
  if Assigned(fOnIdle) then
    fOnIdle(self,0); // notify started
  start := {$ifdef FPCLINUX}SynFPCLinux.{$else}SynCommons.{$endif}GetTickCount64;
  repeat
    case AcquireThread of
    flagDestroying:
      exit;
    flagIdle:
      break; // we acquired the background thread
    end;
    case OnIdleProcessNotify(start) of // Windows.GetTickCount64 res is 10-16 ms
    0..20:    SleepHiRes(0); // 10 microsec delay on POSIX
    21..100:  SleepHiRes(1);
    101..900: SleepHiRes(5);
    else      SleepHiRes(50);
    end;
  until false;
  // 2. process execution in the background thread
  fParam := OpaqueParam;
  fProcessEvent.SetEvent; // notify background thread for Call pending process
  WaitForFinished(start,nil); // wait for flagFinished, then set flagIdle
  result := true;
end;

function TSynBackgroundThreadMethodAbstract.GetOnIdleBackgroundThreadActive: boolean;
begin
  result := (self<>nil) and Assigned(fOnIdle) and (GetPendingProcess<>flagIdle);
end;


{ TSynBackgroundThreadEvent }

constructor TSynBackgroundThreadEvent.Create(aOnProcess: TOnProcessSynBackgroundThread;
  aOnIdle: TOnIdleSynBackgroundThread; const aThreadName: RawUTF8);
begin
  inherited Create(aOnIdle,aThreadName);
  fOnProcess := aOnProcess;
end;

procedure TSynBackgroundThreadEvent.Process;
begin
  if not Assigned(fOnProcess) then
    raise ESynException.CreateUTF8('Invalid %.RunAndWait() call',[self]);
  fOnProcess(self,fParam);
end;


{ TSynBackgroundThreadMethod }

procedure TSynBackgroundThreadMethod.Process;
var Method: ^TThreadMethod;
begin
  if fParam=nil then
    raise ESynException.CreateUTF8('Invalid %.RunAndWait() call',[self]);
  Method := fParam;
  Method^();
end;

procedure TSynBackgroundThreadMethod.RunAndWait(Method: TThreadMethod);
var Met: TMethod absolute Method;
begin
  inherited RunAndWait(@Met);
end;


{ TSynBackgroundThreadProcedure }

constructor TSynBackgroundThreadProcedure.Create(aOnProcess: TOnProcessSynBackgroundThreadProc;
  aOnIdle: TOnIdleSynBackgroundThread; const aThreadName: RawUTF8);
begin
  inherited Create(aOnIdle,aThreadName);
  fOnProcess := aOnProcess;
end;

procedure TSynBackgroundThreadProcedure.Process;
begin
  if not Assigned(fOnProcess) then
    raise ESynException.CreateUTF8('Invalid %.RunAndWait() call',[self]);
  fOnProcess(fParam);
end;


{ TSynParallelProcessThread }

procedure TSynParallelProcessThread.Process;
begin
  if not Assigned(fMethod) then
    exit;
  fMethod(fIndexStart,fIndexStop);
  fMethod := nil;
end;

procedure TSynParallelProcessThread.Start(
  Method: TSynParallelProcessMethod; IndexStart, IndexStop: integer);
begin
  fMethod := Method;
  fIndexStart := IndexStart;
  fIndexStop := IndexStop;
  fProcessEvent.SetEvent; // notify execution
end;


{ TSynBackgroundThreadProcess }

constructor TSynBackgroundThreadProcess.Create(const aThreadName: RawUTF8;
  aOnProcess: TOnSynBackgroundThreadProcess; aOnProcessMS: cardinal;
  aOnBeforeExecute, aOnAfterExecute: TNotifyThreadEvent;
  aStats: TSynMonitorClass; CreateSuspended: boolean);
begin
  if not Assigned(aOnProcess) then
    raise ESynException.CreateUTF8('%.Create(aOnProcess=nil)',[self]);
  if aStats<>nil then
    fStats := aStats.Create(aThreadName);
  fOnProcess := aOnProcess;
  fOnProcessMS := aOnProcessMS;
  if fOnProcessMS=0 then
    fOnProcessMS := INFINITE; // wait until ProcessEvent.SetEvent or Terminated
  inherited Create(aThreadName,aOnBeforeExecute,aOnAfterExecute,CreateSuspended);
end;

destructor TSynBackgroundThreadProcess.Destroy;
begin
  if fExecute=exRun then begin
    Terminate;
    WaitForNotExecuting(10000); // expect the background task to be finished
  end;
  inherited Destroy;
  fStats.Free;
end;

procedure TSynBackgroundThreadProcess.ExecuteLoop;
var wait: TWaitResult;
begin
  wait := FixedWaitFor(fProcessEvent,fOnProcessMS);
  if not Terminated and (wait in [wrSignaled,wrTimeout]) then
    if fExecuteLoopPause then // pause -> try again later
      fProcessEvent.SetEvent else
      try
        if fStats<>nil then
          fStats.ProcessStartTask;
        try
          fOnProcess(self,wait);
        finally
          if fStats<>nil then
            fStats.ProcessEnd;
        end;
      except
        on E: Exception do begin
          if fStats<>nil then
            fStats.ProcessErrorRaised(E);
          if Assigned(fOnException) then
            fOnException(E);
        end;
      end;
end;


{ TSynBackgroundTimer }

var
  ProcessSystemUse: TSystemUse;

constructor TSynBackgroundTimer.Create(const aThreadName: RawUTF8;
  aOnBeforeExecute, aOnAfterExecute: TNotifyThreadEvent; aStats: TSynMonitorClass);
begin
  fTasks.Init(TypeInfo(TSynBackgroundTimerTaskDynArray),fTask);
  fTaskLock.Init;
  {$ifndef NOVARIANTS}
  fTaskLock.LockedBool[0] := false;
  {$endif}
  inherited Create(aThreadName,EverySecond,1000,aOnBeforeExecute,aOnAfterExecute,aStats);
end;

destructor TSynBackgroundTimer.Destroy;
begin
  if (ProcessSystemUse<>nil) and (ProcessSystemUse.fTimer=self) then
    ProcessSystemUse.fTimer := nil; // allows processing by another background timer
  inherited Destroy;
  fTaskLock.Done;
end;

const
  TIXPRECISION = 32; // GetTickCount64 resolution (for aOnProcessSecs=1)

procedure TSynBackgroundTimer.EverySecond(
  Sender: TSynBackgroundThreadProcess; Event: TWaitResult);
var tix: Int64;
    i,f,n: integer;
    t: ^TSynBackgroundTimerTask;
    todo: TSynBackgroundTimerTaskDynArray; // avoid lock contention
begin
  if (fTask=nil) or Terminated then
    exit;
  tix := {$ifdef FPCLINUX}SynFPCLinux.{$else}SynCommons.{$endif}GetTickCount64;
  n := 0;
  fTaskLock.Lock;
  try
    variant(fTaskLock.Padding[0]) := true; // = fTaskLock.LockedBool[0]
    try
      for i := 0 to length(fTask)-1 do begin
        t := @fTask[i];
        if tix>=t^.NextTix then begin
          SetLength(todo,n+1);
          todo[n] := t^;
          inc(n);
          t^.FIFO := nil; // now owned by todo[n].FIFO
          t^.NextTix := tix+((t^.Secs*1000)-TIXPRECISION);
        end;
      end;
    finally
      fTaskLock.UnLock;
    end;
    for i := 0 to n-1 do
      with todo[i] do
        if FIFO<>nil then
          for f := 0 to length(FIFO)-1 do
          try
            OnProcess(self,Event,FIFO[f]);
          except
          end
        else
          try
            OnProcess(self,Event,'');
          except
          end;
  finally
    {$ifdef NOVARIANTS}
    fTaskLock.Lock;
    variant(fTaskLock.Padding[0]) := false;
    fTaskLock.UnLock;
    {$else}
    fTaskLock.LockedBool[0] := false;
    {$endif}
  end;
end;

function TSynBackgroundTimer.Find(const aProcess: TMethod): integer;
begin // caller should have made fTaskLock.Lock;
  for result := length(fTask)-1 downto 0 do
    with TMethod(fTask[result].OnProcess) do
      if (Code=aProcess.Code) and (Data=aProcess.Data) then
        exit;
  result := -1;
end;

procedure TSynBackgroundTimer.Enable(
  aOnProcess: TOnSynBackgroundTimerProcess; aOnProcessSecs: cardinal);
var task: TSynBackgroundTimerTask;
    found: integer;
begin
  if (self=nil) or Terminated or not Assigned(aOnProcess) then
    exit;
  if aOnProcessSecs=0 then begin
    Disable(aOnProcess);
    exit;
  end;
  task.OnProcess := aOnProcess;
  task.Secs := aOnProcessSecs;
  task.NextTix := {$ifdef FPCLINUX}SynFPCLinux.{$else}SynCommons.{$endif}GetTickCount64+
    (aOnProcessSecs*1000-TIXPRECISION);
  fTaskLock.Lock;
  try
    found := Find(TMethod(aOnProcess));
    if found>=0 then
      fTask[found] := task else
      fTasks.Add(task);
  finally
    fTaskLock.UnLock;
  end;
end;

function TSynBackgroundTimer.Processing: boolean;
begin
  {$ifdef NOVARIANTS}
  with fTaskLock.Padding[0] do
    result := (VType=varBoolean) and VBoolean;
  {$else}
  result := fTaskLock.LockedBool[0];
  {$endif}
end;

procedure TSynBackgroundTimer.WaitUntilNotProcessing(timeoutsecs: integer);
var timeout: Int64;
begin
  if not Processing then
    exit;
  timeout := {$ifdef FPCLINUX}SynFPCLinux.{$else}SynCommons.{$endif}GetTickCount64+timeoutsecs*1000;
  repeat
    SleepHiRes(1);
  until not Processing or
    ({$ifdef FPCLINUX}SynFPCLinux.{$else}SynCommons.{$endif}GetTickcount64>timeout);
end;

function TSynBackgroundTimer.ExecuteNow(aOnProcess: TOnSynBackgroundTimerProcess): boolean;
begin
  result := Add(aOnProcess,#0,true);
end;

function TSynBackgroundTimer.EnQueue(aOnProcess: TOnSynBackgroundTimerProcess;
  const aMsg: RawUTF8; aExecuteNow: boolean): boolean;
begin
  result := Add(aOnProcess,aMsg,aExecuteNow);
end;

function TSynBackgroundTimer.EnQueue(aOnProcess: TOnSynBackgroundTimerProcess;
  const aMsgFmt: RawUTF8; const Args: array of const; aExecuteNow: boolean): boolean;
var msg: RawUTF8;
begin
  FormatUTF8(aMsgFmt,Args,msg);
  result := Add(aOnProcess,msg,aExecuteNow);
end;

function TSynBackgroundTimer.Add(aOnProcess: TOnSynBackgroundTimerProcess;
  const aMsg: RawUTF8; aExecuteNow: boolean): boolean;
var found: integer;
begin
  result := false;
  if (self=nil) or Terminated or not Assigned(aOnProcess) then
    exit;
  fTaskLock.Lock;
  try
    found := Find(TMethod(aOnProcess));
    if found>=0 then begin
      with fTask[found] do begin
        if aExecuteNow then
          NextTix := 0;
        if aMsg<>#0 then
          AddRawUTF8(FIFO,aMsg);
      end;
      if aExecuteNow then
        ProcessEvent.SetEvent;
      result := true;
    end;
  finally
    fTaskLock.UnLock;
  end;
end;

function TSynBackgroundTimer.DeQueue(aOnProcess: TOnSynBackgroundTimerProcess;
  const aMsg: RawUTF8): boolean;
var found: integer;
begin
  result := false;
  if (self=nil) or Terminated or not Assigned(aOnProcess) then
    exit;
  fTaskLock.Lock;
  try
    found := Find(TMethod(aOnProcess));
    if found>=0 then
      with fTask[found] do
        result := DeleteRawUTF8(FIFO,FindRawUTF8(FIFO,aMsg));
  finally
    fTaskLock.UnLock;
  end;
end;

function TSynBackgroundTimer.Disable(aOnProcess: TOnSynBackgroundTimerProcess): boolean;
var found: integer;
begin
  result := false;
  if (self=nil) or Terminated or not Assigned(aOnProcess) then
    exit;
  fTaskLock.Lock;
  try
    found := Find(TMethod(aOnProcess));
    if found>=0 then begin
      fTasks.Delete(found);
      result := true;
    end;
  finally
    fTaskLock.UnLock;
  end;
end;

{ TSynParallelProcess }

constructor TSynParallelProcess.Create(ThreadPoolCount: integer; const ThreadName: RawUTF8;
  OnBeforeExecute, OnAfterExecute: TNotifyThreadEvent;
  MaxThreadPoolCount: integer);
var i: integer;
begin
  inherited Create;
  if ThreadPoolCount<0 then
    raise ESynParallelProcess.CreateUTF8('%.Create(%,%)',[Self,ThreadPoolCount,ThreadName]);
  if ThreadPoolCount>MaxThreadPoolCount then
    ThreadPoolCount := MaxThreadPoolCount;
  fThreadPoolCount := ThreadPoolCount;
  fThreadName := ThreadName;
  SetLength(fPool,fThreadPoolCount);
  for i := 0 to fThreadPoolCount-1 do
    fPool[i] := TSynParallelProcessThread.Create(nil,FormatUTF8('%#%/%',
      [fThreadName,i+1,fThreadPoolCount]),OnBeforeExecute,OnAfterExecute);
end;

destructor TSynParallelProcess.Destroy;
begin
  ObjArrayClear(fPool);
  inherited;
end;

procedure TSynParallelProcess.ParallelRunAndWait(const Method: TSynParallelProcessMethod;
  MethodCount: integer; const OnMainThreadIdle: TNotifyEvent);
var use,t,n,perthread: integer;
    error: RawUTF8;
begin
  if (MethodCount<=0) or not Assigned(Method) then
    exit;
  if not Assigned(OnMainThreadIdle) then
    if (self=nil) or (MethodCount=1) or (fThreadPoolCount=0) then begin
      Method(0,MethodCount-1); // no need (or impossible) to use background thread
      exit;
    end;
  use := MethodCount;
  t := fThreadPoolCount;
  if not Assigned(OnMainThreadIdle) then
    inc(t); // include current thread
  if use>t then
    use := t;
  try
    // start secondary threads
    perthread := MethodCount div use;
    if perthread=0 then
      use := 1;
    n := 0;
    for t := 0 to use-2 do begin
      repeat
        case fPool[t].AcquireThread of
        flagDestroying: // should not happen
          raise ESynParallelProcess.CreateUTF8(
            '%.ParallelRunAndWait [%] destroying',[self,fPool[t].fThreadName]);
        flagIdle:
          break; // acquired (should always be the case)
        end;
        SleepHiRes(1);
        if Assigned(OnMainThreadIdle) then
          OnMainThreadIdle(self);
      until false;
      fPool[t].Start(Method,n,n+perthread-1);
      inc(n,perthread);
      inc(fParallelRunCount);
    end;
    // run remaining items in the current/last thread
    if n<MethodCount then begin
      if Assigned(OnMainThreadIdle) then begin
        fPool[use-1].Start(Method,n,MethodCount-1);
        inc(use); // also wait for the last thread
      end else
        Method(n,MethodCount-1);
      inc(fParallelRunCount);
    end;
  finally
    // wait for the process to finish
    for t := 0 to use-2 do
    try
      fPool[t].WaitForFinished(0,OnMainThreadIdle);
    except
      on E: Exception do
        error := FormatUTF8('% % on thread % [%]',[error,E,fPool[t].fThreadName,E.Message]);
    end;
    if error<>'' then
      raise ESynParallelProcess.CreateUTF8('%.ParallelRunAndWait: %',[self,error]);
  end;
end;


{ TBlockingProcess }

constructor TBlockingProcess.Create(aTimeOutMs: integer; aSafe: PSynLocker);
begin
  inherited Create(nil,false,false,'');
  if aTimeOutMs<=0 then
    fTimeOutMs := 3000 else // never wait for ever
    fTimeOutMs := aTimeOutMs;
  fSafe := aSafe;
end;

constructor TBlockingProcess.Create(aTimeOutMs: integer);
begin
  fOwnedSafe := true;
  Create(aTimeOutMS,NewSynLocker);
end;

destructor TBlockingProcess.Destroy;
begin
  inherited Destroy;
  if fOwnedSafe then
    fSafe^.DoneAndFreeMem;
end;

function TBlockingProcess.WaitFor: TBlockingEvent;
begin
  fSafe^.Lock;
  try
    result := fEvent;
    if fEvent in [evRaised,evTimeOut] then
      exit;
    fEvent := evWaiting;
  finally
    fSafe^.UnLock;
  end;
  FixedWaitFor(self,fTimeOutMs);
  fSafe^.Lock;
  try
    if fEvent<>evRaised then
      fEvent := evTimeOut;
    result := fEvent;
  finally
    fSafe^.UnLock;
  end;
end;

function TBlockingProcess.WaitFor(TimeOutMS: integer): TBlockingEvent;
begin
  if TimeOutMS <= 0 then
    fTimeOutMs := 3000 // never wait for ever
  else
    fTimeOutMs := TimeOutMS;
  result := WaitFor;
end;

function TBlockingProcess.NotifyFinished(alreadyLocked: boolean): boolean;
begin
  result := false;
  if not alreadyLocked then
    fSafe^.Lock;
  try
    if fEvent in [evRaised,evTimeOut] then
      exit; // ignore if already notified
    fEvent := evRaised;
    SetEvent; // notify caller to unlock "WaitFor" method
    result := true;
  finally
    fSafe^.UnLock;
  end;
end;

procedure TBlockingProcess.ResetInternal;
begin
  ResetEvent;
  fEvent := evNone;
end;

function TBlockingProcess.Reset: boolean;
begin
  fSafe^.Lock;
  try
    result := fEvent<>evWaiting;
    if result then
      ResetInternal;
  finally
    fSafe^.UnLock;
  end;
end;

procedure TBlockingProcess.Lock;
begin
  fSafe^.Lock;
end;

procedure TBlockingProcess.Unlock;
begin
  fSafe^.Unlock;
end;


{ TBlockingProcessPoolItem }

procedure TBlockingProcessPoolItem.ResetInternal;
begin
  inherited ResetInternal; // set fEvent := evNone
  fCall := 0;
end;


{ TBlockingProcessPool }

constructor TBlockingProcessPool.Create(aClass: TBlockingProcessPoolItemClass);
begin
  inherited Create;
  if aClass=nil then
    fClass := TBlockingProcessPoolItem else
    fClass := aClass;
  fPool := TSynObjectListLocked.Create;
end;

const
  CALL_DESTROYING = -1;

destructor TBlockingProcessPool.Destroy;
var i: integer;
    someWaiting: boolean;
begin
  fCallCounter := CALL_DESTROYING;
  someWaiting := false;
  for i := 0 to fPool.Count-1 do
    with TBlockingProcessPoolItem(fPool.List[i]) do
    if Event=evWaiting then begin
      SetEvent; // release WaitFor (with evTimeOut)
      someWaiting := true;
    end;
  if someWaiting then
    SleepHiRes(10); // propagate the pending evTimeOut to the WaitFor threads
  fPool.Free;
  inherited;
end;

function TBlockingProcessPool.NewProcess(aTimeOutMs: integer): TBlockingProcessPoolItem;
var i: integer;
    p: ^TBlockingProcessPoolItem;
begin
  result := nil;
  if fCallCounter=CALL_DESTROYING then
    exit;
  if aTimeOutMs<=0 then
    aTimeOutMs := 3000; // never wait for ever
  fPool.Safe.Lock;
  try
    p := pointer(fPool.List);
    for i := 1 to fPool.Count do
      if p^.Call=0 then begin
        result := p^; // found a non-used entry
        result.fTimeOutMs := aTimeOutMS;
        break;
      end else
        inc(p);
    if result=nil then begin
      result := fClass.Create(aTimeOutMS);
      fPool.Add(result);
    end;
    inc(fCallCounter); // 1,2,3,...
    result.fCall := fCallCounter;
  finally
    fPool.Safe.UnLock;
  end;
end;

function TBlockingProcessPool.FromCall(call: TBlockingProcessPoolCall;
  locked: boolean): TBlockingProcessPoolItem;
var i: integer;
    p: ^TBlockingProcessPoolItem;
begin
  result := nil;
  if (fCallCounter=CALL_DESTROYING) or (call<=0) then
    exit;
  fPool.Safe.Lock;
  try
    p := pointer(fPool.List);
    for i := 1 to fPool.Count do
      if p^.Call=call then begin
        result := p^;
        if locked then
          result.Lock;
        exit;
      end else
        inc(p);
  finally
    fPool.Safe.UnLock;
  end;
end;

{$ifdef KYLIX3}
type
  // see http://stackoverflow.com/a/3085509 about this known Kylix bug
  TEventHack = class(THandleObject) // should match EXACTLY SyncObjs.pas source!
  private
    FEvent: TSemaphore;
    FManualReset: Boolean;
  end;

function FixedWaitFor(Event: TEvent; Timeout: LongWord): TWaitResult;
var E: TEventHack absolute Event;
  procedure SetResult(res: integer);
  begin
    if res=0 then
      result := wrSignaled else
    if errno in [EAGAIN,ETIMEDOUT] then
      result := wrTimeOut else begin
      write(TimeOut,':',errno,' ');
      result := wrError;
    end;
  end;
{.$define USESEMTRYWAIT}
// sem_timedwait() is slower than sem_trywait(), but consuming much less CPU
{$ifdef USESEMTRYWAIT}
var time: timespec;
{$else}
var start,current: Int64;
    elapsed: LongWord;
{$endif}
begin
  if Timeout=INFINITE then begin
    SetResult(sem_wait(E.FEvent));
    exit;
  end;
  if TimeOut=0 then begin
    SetResult(sem_trywait(E.FEvent));
    exit;
  end;
  {$ifdef USESEMTRYWAIT}
  clock_gettime(CLOCK_REALTIME,time);
  inc(time.tv_sec,TimeOut div 1000);
  inc(time.tv_nsec,(TimeOut mod 1000)*1000000);
  while time.tv_nsec>1000000000 do begin
    inc(time.tv_sec);
    dec(time.tv_nsec,1000000000);
  end;
  SetResult(sem_timedwait(E.FEvent,time));
  {$else}
  start := GetTickCount64;
  repeat
     if sem_trywait(E.FEvent)=0 then begin
       result := wrSignaled;
       break;
     end;
     current := GetTickCount64;
     elapsed := current-start;
     if elapsed=0 then
       usleep(1) else
     if elapsed>TimeOut then begin
       result := wrTimeOut;
       break;
     end else
     if elapsed<5 then
       usleep(50) else
       usleep(1000);
  until false;
  {$endif}
  if E.FManualReset then begin
    repeat until sem_trywait(E.FEvent)<>0; // reset semaphore state
    sem_post(E.FEvent);
  end;
end;

{$else KYLIX3} // original FPC or Windows implementation is OK

function FixedWaitFor(Event: TEvent; Timeout: LongWord): TWaitResult;
begin
  result := Event.WaitFor(TimeOut);
end;

{$endif KYLIX3}

procedure FixedWaitForever(Event: TEvent);
begin
  FixedWaitFor(Event,INFINITE);
end;

{$endif LVCL} // LVCL does not implement TEvent


{ ************ System Analysis types and classes ************************** }


function SystemInfoJson: RawUTF8;
var cpu,mem: RawUTF8;
begin
  cpu := TSystemUse.Current(false).HistoryText(0,15,@mem);
  with SystemInfo do
    result := JSONEncode([
      'host',ExeVersion.Host,'user',ExeVersion.User,'os',OSVersionText,
      'cpu',CpuInfoText,'bios',BiosInfoText,
      {$ifdef MSWINDOWS}{$ifndef CPU64}'wow64',IsWow64,{$endif}{$endif MSWINDOWS}
      {$ifdef CPUINTEL}'cpufeatures', LowerCase(ToText(CpuFeatures, ' ')),{$endif}
      'processcpu',cpu,'processmem',mem,
      'freemem',TSynMonitorMemory.FreeAsText,
      'disk',GetDiskPartitionsText(false,true)]);
end;


{ TProcessInfo }

{$ifdef MSWINDOWS}
type
  TProcessMemoryCounters = record
    cb: DWORD;
    PageFaultCount: DWORD;
    PeakWorkingSetSize: PtrUInt;
    WorkingSetSize: PtrUInt;
    QuotaPeakPagedPoolUsage: PtrUInt;
    QuotaPagedPoolUsage: PtrUInt;
    QuotaPeakNonPagedPoolUsage: PtrUInt;
    QuotaNonPagedPoolUsage: PtrUInt;
    PagefileUsage: PtrUInt;
    PeakPagefileUsage: PtrUInt;
  end;
const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
var
  // PROCESS_QUERY_INFORMATION or PROCESS_QUERY_LIMITED_INFORMATION
  OpenProcessAccess: DWORD;
  // late-binding of Windows version specific API entries
  GetSystemTimes: function(var lpIdleTime, lpKernelTime, lpUserTime: TFileTime): BOOL; stdcall;
  GetProcessTimes: function(hProcess: THandle;
    var lpCreationTime, lpExitTime, lpKernelTime, lpUserTime: TFileTime): BOOL; stdcall;
  GetProcessMemoryInfo: function(Process: THandle;
    var ppsmemCounters: TProcessMemoryCounters; cb: DWORD): BOOL; stdcall;
  EnumProcessModules: function (hProcess: THandle; var lphModule: HMODULE; cb: DWORD;
    var lpcbNeeded: DWORD): BOOL; stdcall;
  EnumProcesses: function(lpidProcess: PDWORD; cb: DWORD; var cbNeeded: DWORD): BOOL; stdcall;
  GetModuleFileNameExW: function(hProcess: THandle; hModule: HMODULE;
    lpBaseName: PWideChar; nSize: DWORD): DWORD; stdcall;
  // Vista+/WS2008+ (use GetModuleFileNameEx on XP)
  QueryFullProcessImageNameW: function(hProcess: THandle; dwFlags: DWORD;
    lpExeName: PWideChar; lpdwSize: PDWORD): BOOL; stdcall;

procedure InitWindowsAPI;
var Kernel, Psapi: THandle;
begin
  if OSVersion>=wVista then
    OpenProcessAccess := PROCESS_QUERY_LIMITED_INFORMATION else
    OpenProcessAccess := PROCESS_QUERY_INFORMATION or PROCESS_VM_READ;
  Kernel := GetModuleHandle(kernel32);
  @GetSystemTimes := GetProcAddress(Kernel,'GetSystemTimes');
  @GetProcessTimes := GetProcAddress(Kernel,'GetProcessTimes');
  @QueryFullProcessImageNameW := GetProcAddress(Kernel,'QueryFullProcessImageNameW');
  Psapi := LoadLibrary('Psapi.dll');
  if Psapi>=32 then begin
    @EnumProcesses := GetProcAddress(Psapi,'EnumProcesses');
    @GetModuleFileNameExW := GetProcAddress(Psapi,'GetModuleFileNameExW');
    @EnumProcessModules := GetProcAddress(Psapi, 'EnumProcessModules');
    @GetProcessMemoryInfo := GetProcAddress(Psapi,'GetProcessMemoryInfo');
  end;
end;

function EnumAllProcesses(out Count: Cardinal): TCardinalDynArray;
var n: cardinal;
begin
  result := nil;
  n := 2048;
  repeat
    SetLength(result, n);
    if EnumProcesses(pointer(result), n * 4, Count) then
      Count := Count shr 2 else
      Count := 0;
    if Count < n then begin
      if Count = 0 then
        result := nil;
      exit;
    end;
    inc(n, 1024); // (very unlikely) too small buffer
  until n>8192;
end;

function EnumProcessName(PID: Cardinal): RawUTF8;
var h: THandle;
    len: DWORD;
    name: array[0..4095] of WideChar;
begin
  result := '';
  if PID = 0 then
    exit;
  h := OpenProcess(OpenProcessAccess, false, PID);
  if h <> 0 then
    try
      if Assigned(QueryFullProcessImageNameW) then begin
        len := high(name);
        if QueryFullProcessImageNameW(h, 0, name, @len) then
          RawUnicodeToUtf8(name, len, result);
      end else
        if GetModuleFileNameExW(h,0,name,high(name))<>0 then
          RawUnicodeToUtf8(name, StrLenW(name), result);
    finally
      CloseHandle(h);
    end;
end;

function TProcessInfo.Init: boolean;
begin
  FillCharFast(self,SizeOf(self),0);
  result := Assigned(GetSystemTimes) and Assigned(GetProcessTimes) and
    Assigned(GetProcessMemoryInfo); // no monitoring API under oldest Windows
end;

function TProcessInfo.Start: boolean;
var ftidl,ftkrn,ftusr: TFileTime;
    sidl,skrn,susr: Int64;
begin
  result := Assigned(GetSystemTimes) and GetSystemTimes(ftidl,ftkrn,ftusr);
  if not result then
    exit;
  FileTimeToInt64(ftidl,sidl);
  FileTimeToInt64(ftkrn,skrn);
  FileTimeToInt64(ftusr,susr);
  fDiffIdle := sidl-fSysPrevIdle;
  fDiffKernel := skrn-fSysPrevKernel;
  fDiffUser := susr-fSysPrevUser;
  fDiffTotal := fDiffKernel+fDiffUser; // kernel time also includes idle time
  dec(fDiffKernel, fDiffIdle);
  fSysPrevIdle := sidl;
  fSysPrevKernel := skrn;
  fSysPrevUser := susr;
end;

function TProcessInfo.PerProcess(PID: cardinal; Now: PDateTime;
  out Data: TSystemUseData; var PrevKernel, PrevUser: Int64): boolean;
var
  h: THandle;
  ftkrn,ftusr,ftp,fte: TFileTime;
  pkrn,pusr: Int64;
  mem: TProcessMemoryCounters;
begin
  result := false;
  FillCharFast(Data,SizeOf(Data),0);
  h := OpenProcess(OpenProcessAccess,false,PID);
  if h<>0 then
    try
      if GetProcessTimes(h,ftp,fte,ftkrn,ftusr) then begin
        if Now<>nil then
          Data.Timestamp := Now^;
        FileTimeToInt64(ftkrn,pkrn);
        FileTimeToInt64(ftusr,pusr);
        if (PrevKernel<>0) and (fDiffTotal>0) then begin
          Data.Kernel := ((pkrn-PrevKernel)*100)/fDiffTotal;
          Data.User := ((pusr-PrevUser)*100)/fDiffTotal;
        end;
        PrevKernel := pkrn;
        PrevUser := pusr;
        FillCharFast(mem,SizeOf(mem),0);
        mem.cb := SizeOf(mem);
        if GetProcessMemoryInfo(h,mem,SizeOf(mem)) then begin
          Data.WorkKB := mem.WorkingSetSize shr 10;
          Data.VirtualKB := mem.PagefileUsage shr 10;
        end;
        result := true;
      end;
    finally
      CloseHandle(h);
    end;
end;

function TProcessInfo.PerSystem(out Idle,Kernel,User: currency): boolean;
begin
  if fDiffTotal<=0 then begin
    Idle := 0;
    Kernel := 0;
    User := 0;
    result := false;
  end else begin
    Kernel := SimpleRoundTo2Digits((fDiffKernel*100)/fDiffTotal);
    User := SimpleRoundTo2Digits((fDiffUser*100)/fDiffTotal);
    Idle := 100-Kernel-User; // ensure sum is always 100%
    result := true;
  end;
end;
{$else} // not implemented yet (use /proc ?)
function TProcessInfo.Init: boolean;
begin
  FillCharFast(self,SizeOf(self),0);
  result := false;
end;

function TProcessInfo.Start: boolean;
begin
  result := false;
end;

function TProcessInfo.PerProcess(PID: cardinal; Now: PDateTime;
  out Data: TSystemUseData; var PrevKernel, PrevUser: Int64): boolean;
begin
  result := false;
end;

function TProcessInfo.PerSystem(out Idle,Kernel,User: currency): boolean;
var P: PUTF8Char;
    U, K, I, S: cardinal;
begin // see http://www.linuxhowtos.org/System/procstat.htm
  result := false;
  P := pointer(StringFromFile('/proc/stat', {nosize=}true));
  if P=nil then
    exit;
  U := GetNextItemCardinal(P,' '){=user}+GetNextItemCardinal(P,' '){=nice};
  K := GetNextItemCardinal(P,' '){=system};
  I := GetNextItemCardinal(P,' '){=idle};
  S := U+K+I;
  Kernel := SimpleRoundTo2Digits((K*100)/S);
  User := SimpleRoundTo2Digits((U*100)/S);
  Idle := 100-Kernel-User; // ensure sum is always 100%
  result := S<>0;
end; { TODO : use a diff approach for TProcessInfo.PerSystem on Linux }
{$endif MSWINDOWS}


{ TSystemUse }

procedure TSystemUse.BackgroundExecute(Sender: TSynBackgroundTimer;
  Event: TWaitResult; const Msg: RawUTF8);
var i: integer;
    now: TDateTime;
begin
  if (fProcess=nil) or (fHistoryDepth=0) or not fProcessInfo.Start then
    exit;
  fTimer := Sender;
  now := NowUTC;
  fSafe.Lock;
  try
    inc(fDataIndex);
    if fDataIndex>=fHistoryDepth then
      fDataIndex := 0;
    for i := high(fProcess) downto 0 do // backwards for fProcesses.Delete(i)
      with fProcess[i] do
      if fProcessInfo.PerProcess(ID,@now,Data[fDataIndex],PrevKernel,PrevUser) then begin
        if Assigned(fOnMeasured) then
          fOnMeasured(ID,Data[fDataIndex]);
      end else
      if UnsubscribeProcessOnAccessError then
        // if GetLastError=ERROR_INVALID_PARAMETER then
        fProcesses.Delete(i);
  finally
    fSafe.UnLock;
  end;
end;

procedure TSystemUse.OnTimerExecute(Sender: TObject);
begin
  BackgroundExecute(nil,wrSignaled,'');
end;

constructor TSystemUse.Create(const aProcessID: array of integer;
  aHistoryDepth: integer);
var i: integer;
begin
  inherited Create;
  fProcesses.Init(TypeInfo(TSystemUseProcessDynArray),fProcess);
  {$ifdef MSWINDOWS}
  if not Assigned(GetSystemTimes) or not Assigned(GetProcessTimes) or
     not Assigned(GetProcessMemoryInfo) then
    exit; // no system monitoring API under oldest Windows
  {$else}
  exit; // not implemented yet
  {$endif}
  if aHistoryDepth<=0 then
    aHistoryDepth := 1;
  fHistoryDepth := aHistoryDepth;
  SetLength(fProcess,length(aProcessID));
  for i := 0 to high(aProcessID) do begin
    {$ifdef MSWINDOWS}
    if aProcessID[i]=0 then
      fProcess[i].ID := GetCurrentProcessID else
    {$endif}
      fProcess[i].ID := aProcessID[i];
    SetLength(fProcess[i].Data,fHistoryDepth);
  end;
end;

constructor TSystemUse.Create(aHistoryDepth: integer);
begin
  Create([0],aHistoryDepth);
end;

procedure TSystemUse.Subscribe(aProcessID: integer);
var i,n: integer;
begin
  if self=nil then
    exit;
  {$ifdef MSWINDOWS}
  if aProcessID=0 then
    aProcessID := GetCurrentProcessID;
  {$endif}
  fSafe.Lock;
  try
    n := length(fProcess);
    for i := 0 to n-1 do
      if fProcess[i].ID=aProcessID then
        exit; // already subscribed
    SetLength(fProcess,n+1);
    fProcess[n].ID := aProcessID;
    SetLength(fProcess[n].Data,fHistoryDepth);
  finally
    fSafe.UnLock;
  end;
end;

function TSystemUse.Unsubscribe(aProcessID: integer): boolean;
var i: integer;
begin
  result := false;
  if self=nil then
    exit;
  fSafe.Lock;
  try
    i := ProcessIndex(aProcessID);
    if i>=0 then begin
      fProcesses.Delete(i);
      result := true;
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TSystemUse.ProcessIndex(aProcessID: integer): integer;
begin // caller should have made fSafe.Enter
  {$ifdef MSWINDOWS}
  if aProcessID=0 then
    aProcessID := GetCurrentProcessID;
  {$endif}
  if self<>nil then
    for result := 0 to high(fProcess) do
      if fProcess[result].ID=aProcessID then
        exit;
  result := -1;
end;

function TSystemUse.Data(out aData: TSystemUseData; aProcessID: integer=0): boolean;
var i: integer;
begin
  result := false;
  if self<>nil then begin
    fSafe.Lock;
    try
      i := ProcessIndex(aProcessID);
      if i>=0 then begin
        with fProcess[i] do
          aData := Data[fDataIndex];
        result := aData.Timestamp<>0;
        if result then
          exit;
      end;
    finally
      fSafe.UnLock;
    end;
  end;
  FillCharFast(aData,SizeOf(aData),0);
end;

function TSystemUse.Data(aProcessID: integer): TSystemUseData;
begin
  Data(result,aProcessID);
end;

function TSystemUse.KB(aProcessID: integer=0): cardinal;
begin
  with Data(aProcessID) do
    result := WorkKB+VirtualKB;
end;

function TSystemUse.Percent(aProcessID: integer): single;
begin
  with Data(aProcessID) do
    result := Kernel+User;
end;

function TSystemUse.PercentKernel(aProcessID: integer): single;
begin
  result := Data(aProcessID).Kernel;
end;

function TSystemUse.PercentUser(aProcessID: integer): single;
begin
  result := Data(aProcessID).User;
end;

function TSystemUse.PercentSystem(out Idle,Kernel,User: currency): boolean;
begin
  result := fProcessInfo.PerSystem(Idle,Kernel,User);
end;

function TSystemUse.HistoryData(aProcessID,aDepth: integer): TSystemUseDataDynArray;
var i,n,last: integer;
begin
  result := nil;
  if self=nil then
    exit;
  fSafe.Lock;
  try
    i := ProcessIndex(aProcessID);
    if i>=0 then
      with fProcess[i] do begin
        n := length(Data);
        last := n-1;
        if (aDepth>0) and (n>aDepth) then
          n := aDepth;
        SetLength(result,n); // make ordered copy
        for i := 0 to n-1 do begin
          if i<=fDataIndex then
            result[i] := Data[fDataIndex-i] else begin
            result[i] := Data[last];
            dec(last);
          end;
          if PInt64(@result[i].Timestamp)^=0 then begin
            SetLength(result,i); // truncate to latest available sample
            break;
          end;
        end;
      end;
  finally
    fSafe.UnLock;
  end;
end;

function TSystemUse.History(aProcessID,aDepth: integer): TSingleDynArray;
var i,n: integer;
    data: TSystemUseDataDynArray;
begin
  result := nil;
  data := HistoryData(aProcessID,aDepth);
  n := length(data);
  SetLength(result,n);
  for i := 0 to n-1 do
    result[i] := data[i].Kernel+data[i].User;
end;

class function TSystemUse.Current(aCreateIfNone: boolean): TSystemUse;
begin
  if (ProcessSystemUse=nil) and aCreateIfNone then
    GarbageCollectorFreeAndNil(ProcessSystemUse,TSystemUse.Create(60));
  result := ProcessSystemUse;
end;

function TSystemUse.HistoryText(aProcessID,aDepth: integer;
  aDestMemoryMB: PRawUTF8): RawUTF8;
var data: TSystemUseDataDynArray;
    mem: RawUTF8;
    i: integer;
begin
  result := '';
  data := HistoryData(aProcessID,aDepth);
  {$ifdef LINUXNOTBSD} // bsd: see VM_LOADAVG
  // https://www.retro11.de/ouxr/211bsd/usr/src/lib/libc/gen/getloadavg.c.html
  if data = nil then
    result := StringFromFile('/proc/loadavg',{HasNoSize=}true) else
  {$endif LINUXNOTBSD}
  for i := 0 to high(data) do
  with data[i] do begin
    result := FormatUTF8('%% ',[result,TruncTo2Digits(Kernel+User)]);
    if aDestMemoryMB<>nil then
      mem := FormatUTF8('%% ',[mem,TruncTo2Digits(WorkKB/1024)]);
  end;
  result := trim(result);
  if aDestMemoryMB<>nil then
    aDestMemoryMB^ := trim(mem);
end;

{$ifndef NOVARIANTS}

function TSystemUse.HistoryVariant(aProcessID,aDepth: integer): variant;
var res: TDocVariantData absolute result;
    data: TSystemUseDataDynArray;
    i: integer;
begin
  VarClear(result);
  data := HistoryData(aProcessID,aDepth);
  res.InitFast(length(data),dvArray);
  for i := 0 to high(data) do
    res.AddItem(TruncTo2Digits(data[i].Kernel+data[i].User));
end;

{$endif NOVARIANTS}

function SortDynArrayDiskPartitions(const A,B): integer;
begin
  result := SortDynArrayString(TDiskPartition(A).mounted,TDiskPartition(B).mounted);
end;

function GetDiskPartitions: TDiskPartitions;
{$ifdef MSWINDOWS} // DeviceIoControl(IOCTL_DISK_GET_PARTITION_INFO) requires root
var drives, drive, m, n: integer;
    fn, volume: TFileName;
{$else}
var mounts, fs, mnt, typ: RawUTF8;
    p: PUTF8Char;
    fn: TFileName;
    n: integer;
{$endif}
    av, fr, tot: QWord;
begin
  result := nil;
  n := 0;
{$ifdef MSWINDOWS}
   fn := '#:\';
   drives := GetLogicalDrives;
   m := 1 shl 2;
   for drive := 3 to 26 do begin // retrieve partitions mounted as C..Z drives
     if drives and m <> 0 then begin
       fn[1] := char(64+drive);
       if GetDiskInfo(fn,av,fr,tot,@volume) then begin
         SetLength(result,n+1);
         StringToUTF8(volume,result[n].name);
         volume := '';
         result[n].mounted := fn;
         result[n].size := tot;
         inc(n);
       end;
     end;
     m := m shl 1;
   end;
{$else} // see https://github.com/gagern/gnulib/blob/master/lib/mountlist.c
  mounts := StringFromFile({$ifdef BSD}'/etc/mtab'{$else}'/proc/self/mounts'{$endif},
    {hasnosize=}true);
  p := pointer(mounts);
  repeat
    fs := '';
    mnt := '';
    typ := '';
    ScanUTF8(GetNextLine(p,p),'%S %S %S',[@fs,@mnt,@typ]);
    if (fs<>'') and (fs<>'rootfs') and (IdemPCharArray(pointer(fs),['/DEV/LOOP'])<0) and
       (mnt<>'') and (mnt<>'/mnt') and (typ<>'') and
       (IdemPCharArray(pointer(mnt),['/PROC/','/SYS/','/RUN/'])<0) and
       (FindPropName(['autofs','proc','subfs','debugfs','devpts','fusectl','mqueue',
        'rpc-pipefs','sysfs','devfs','kernfs','ignore','none','tmpfs','securityfs',
        'ramfs','rootfs','devtmpfs','hugetlbfs','iso9660'],typ)<0) then begin
      fn := UTF8ToString(mnt);
      if GetDiskInfo(fn,av,fr,tot) and (tot>1 shl 20) then begin
        //writeln('fs=',fs,' mnt=',mnt,' typ=',typ, ' av=',KB(av),' fr=',KB(fr),' tot=',KB(tot));
        SetLength(result,n+1);
        result[n].name := fs;
        result[n].mounted := fn;
        result[n].size := tot;
        inc(n);
      end;
    end;
  until p=nil;
  DynArray(TypeInfo(TDiskPartitions),result).Sort(SortDynArrayDiskPartitions);
  {$endif}
end;

var
  _DiskPartitions: TDiskPartitions;

function GetDiskPartitionsText(nocache, withfreespace, nospace: boolean): RawUTF8;
const F: array[boolean] of RawUTF8 = ({$ifdef MSWINDOWS}'%: % (% / %)', '%: % (%/%)'
         {$else}'% % (% / %)', '% % (%/%)'{$endif});
var i: integer;
    parts: TDiskPartitions;
   function GetInfo(var p: TDiskPartition): shortstring;
   var av, fr, tot: QWord;
   begin
     if not withfreespace or not GetDiskInfo(p.mounted,av,fr,tot) then
       {$ifdef MSWINDOWS}
       FormatShort('%: % (%)',[p.mounted[1],p.name,KB(p.size,nospace)],result) else
       FormatShort(F[nospace],[p.mounted[1],p.name,KB(fr,nospace),KB(tot,nospace)],result);
       {$else}
       FormatShort('% % (%)',[p.mounted,p.name,KB(p.size,nospace)],result) else
       FormatShort(F[nospace],[p.mounted,p.name,KB(fr,nospace),KB(tot,nospace)],result);
       {$endif}
   end;
begin
  if (_DiskPartitions=nil) or nocache then
    _DiskPartitions := GetDiskPartitions;
  parts := _DiskPartitions;
  if parts=nil then
    result := '' else
    ShortStringToAnsi7String(GetInfo(parts[0]),result);
  for i := 1 to high(parts) do
    result := FormatUTF8('%, %',[result,GetInfo(parts[i])]);
end;

{ TSynMonitorMemory }

constructor TSynMonitorMemory.Create(aTextNoSpace: boolean);
begin
  FAllocatedUsed := TSynMonitorOneSize.Create(aTextNoSpace);
  FAllocatedReserved := TSynMonitorOneSize.Create(aTextNoSpace);
  FPhysicalMemoryFree := TSynMonitorOneSize.Create(aTextNoSpace);
  FVirtualMemoryFree := TSynMonitorOneSize.Create(aTextNoSpace);
  FPagingFileTotal := TSynMonitorOneSize.Create(aTextNoSpace);
  FPhysicalMemoryTotal := TSynMonitorOneSize.Create(aTextNoSpace);
  FVirtualMemoryTotal := TSynMonitorOneSize.Create(aTextNoSpace);
  FPagingFileFree := TSynMonitorOneSize.Create(aTextNoSpace);
end;

destructor TSynMonitorMemory.Destroy;
begin
  FAllocatedReserved.Free;
  FAllocatedUsed.Free;
  FPhysicalMemoryFree.Free;
  FVirtualMemoryFree.Free;
  FPagingFileTotal.Free;
  FPhysicalMemoryTotal.Free;
  FVirtualMemoryTotal.Free;
  FPagingFileFree.Free;
  inherited Destroy;
end;

class function TSynMonitorMemory.FreeAsText(nospace: boolean): ShortString;
const F: array[boolean] of RawUTF8 = ('% / %', '%/%');
begin
  with TSynMonitorMemory.Create(nospace) do
  try
    RetrieveMemoryInfo;
    FormatShort(F[nospace],[fPhysicalMemoryFree.Text,fPhysicalMemoryTotal.Text],result);
  finally
    Free;
  end;
end;

var
  PhysicalAsTextCache: TShort16; // this value doesn't change usually

class function TSynMonitorMemory.PhysicalAsText(nospace: boolean): TShort16;
begin
  if PhysicalAsTextCache='' then
    with TSynMonitorMemory.Create(nospace) do
    try
      PhysicalAsTextCache := PhysicalMemoryTotal.Text;
    finally
      Free;
    end;
  result := PhysicalAsTextCache;
end;

class function TSynMonitorMemory.ToJSON: RawUTF8;
begin
  with TSynMonitorMemory.Create(false) do
  try
    RetrieveMemoryInfo;
    FormatUTF8('{Allocated:{reserved:%,used:%},Physical:{total:%,free:%,percent:%},'+
      {$ifdef MSWINDOWS}'Virtual:{total:%,free:%},'+{$endif}'Paged:{total:%,free:%}}',
      [fAllocatedReserved.Bytes shr 10,fAllocatedUsed.Bytes shr 10,
       fPhysicalMemoryTotal.Bytes shr 10,fPhysicalMemoryFree.Bytes shr 10, fMemoryLoadPercent,
       {$ifdef MSWINDOWS}fVirtualMemoryTotal.Bytes shr 10,fVirtualMemoryFree.Bytes shr 10,{$endif}
       fPagingFileTotal.Bytes shr 10,fPagingFileFree.Bytes shr 10],result);
  finally
    Free;
  end;
end;

{$ifndef NOVARIANTS}
class function TSynMonitorMemory.ToVariant: variant;
begin
  result := _JsonFast(ToJSON);
end;
{$endif}

function TSynMonitorMemory.GetAllocatedUsed: TSynMonitorOneSize;
begin
  RetrieveMemoryInfo;
  result := FAllocatedUsed;
end;

function TSynMonitorMemory.GetAllocatedReserved: TSynMonitorOneSize;
begin
  RetrieveMemoryInfo;
  result := FAllocatedReserved;
end;

function TSynMonitorMemory.GetMemoryLoadPercent: integer;
begin
  RetrieveMemoryInfo;
  result := FMemoryLoadPercent;
end;

function TSynMonitorMemory.GetPagingFileFree: TSynMonitorOneSize;
begin
  RetrieveMemoryInfo;
  result := FPagingFileFree;
end;

function TSynMonitorMemory.GetPagingFileTotal: TSynMonitorOneSize;
begin
  RetrieveMemoryInfo;
  result := FPagingFileTotal;
end;

function TSynMonitorMemory.GetPhysicalMemoryFree: TSynMonitorOneSize;
begin
  RetrieveMemoryInfo;
  result := FPhysicalMemoryFree;
end;

function TSynMonitorMemory.GetPhysicalMemoryTotal: TSynMonitorOneSize;
begin
  RetrieveMemoryInfo;
  result := FPhysicalMemoryTotal;
end;

function TSynMonitorMemory.GetVirtualMemoryFree: TSynMonitorOneSize;
begin
  RetrieveMemoryInfo;
  result := FVirtualMemoryFree;
end;

function TSynMonitorMemory.GetVirtualMemoryTotal: TSynMonitorOneSize;
begin
  RetrieveMemoryInfo;
  result := FVirtualMemoryTotal;
end;

{$ifdef MSWINDOWS}
{$ifndef UNICODE} // missing API for oldest Delphi
type
  DWORDLONG = Int64;
  TMemoryStatusEx = record
    dwLength: DWORD;
    dwMemoryLoad: DWORD;
    ullTotalPhys: DWORDLONG;
    ullAvailPhys: DWORDLONG;
    ullTotalPageFile: DWORDLONG;
    ullAvailPageFile: DWORDLONG;
    ullTotalVirtual: DWORDLONG;
    ullAvailVirtual: DWORDLONG;
    ullAvailExtendedVirtual: DWORDLONG;
  end;

// information about the system's current usage of both physical and virtual memory
function GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx): BOOL;
  stdcall; external kernel32;
{$endif}
{$endif}

function GetMemoryInfo(out info: TMemoryInfo; withalloc: boolean): boolean;
{$ifdef WITH_FASTMM4STATS}
var Heap: TMemoryManagerState;
    sb: integer;
{$endif}
{$ifdef MSWINDOWS}
var global: TMemoryStatusEx;
    {$ifdef FPC}mem: TProcessMemoryCounters;{$endif}
begin
  FillCharFast(global,SizeOf(global),0);
  global.dwLength := SizeOf(global);
  result := GlobalMemoryStatusEx(global);
  info.percent := global.dwMemoryLoad;
  info.memtotal := global.ullTotalPhys;
  info.memfree := global.ullAvailPhys;
  info.filetotal := global.ullTotalPageFile;
  info.filefree := global.ullAvailPageFile;
  info.vmtotal := global.ullTotalVirtual;
  info.vmfree := global.ullAvailVirtual;
  {$ifdef FPC} // GetHeapStatus is only about current thread -> use WinAPI
  if withalloc and Assigned(GetProcessMemoryInfo) then begin
    FillcharFast(mem,SizeOf(mem),0);
    mem.cb := SizeOf(mem);
    GetProcessMemoryInfo(GetCurrentProcess,mem,SizeOf(mem));
    info.allocreserved := mem.PeakWorkingSetSize;
    info.allocused := mem.WorkingSetSize;
  end;
  {$endif FPC}
{$else}
{$ifdef BSD}
begin
  FillCharFast(info,SizeOf(info),0);
  info.memtotal := fpsysctlhwint({$ifdef DARWIN}HW_MEMSIZE{$else}HW_PHYSMEM{$endif});
  info.memfree := info.memtotal-fpsysctlhwint(HW_USERMEM);
  if info.memtotal<>0 then // avoid div per 0 exception
    info.percent := ((info.memtotal-info.memfree)*100)div info.memtotal;
{$else}
var si: TSysInfo; // Linuxism
    P: PUTF8Char;
    {$ifdef FPC}mu: cardinal{$else}const mu=1{$endif};
begin
  FillCharFast(info,SizeOf(info),0);
  {$ifdef FPC}
  result := SysInfo(@si)=0;
  mu := si.mem_unit;
  {$else}
  result := SysInfo(si)=0; // some missing fields in Kylix' Libc
  {$endif}
  if si.totalram<>0 then // avoid div per 0 exception
    info.percent := ((si.totalram-si.freeram)*100)div si.totalram;
  info.memtotal := si.totalram*mu;
  info.memfree := si.freeram*mu;
  info.filetotal := si.totalswap*mu;
  info.filefree := si.freeswap*mu;
  if withalloc then begin
    // virtual memory information is not available under Linux
    P := pointer(StringFromFile('/proc/self/statm',{hasnosize=}true));
    info.allocreserved := GetNextItemCardinal(P,' ')*SystemInfo.dwPageSize; // VmSize
    info.allocused := GetNextItemCardinal(P,' ')*SystemInfo.dwPageSize;     // VmRSS
  end;
  // GetHeapStatus is only about current thread -> use /proc/[pid]/statm
{$endif BSD}
{$endif MSWINDOWS}
{$ifdef WITH_FASTMM4STATS} // override OS information by actual FastMM4
  if withalloc then begin
    GetMemoryManagerState(Heap); // direct raw FastMM4 access
    info.allocused := Heap.TotalAllocatedMediumBlockSize+Heap.TotalAllocatedLargeBlockSize;
    info.allocreserved := Heap.ReservedMediumBlockAddressSpace+Heap.ReservedLargeBlockAddressSpace;
    for sb := 0 to high(Heap.SmallBlockTypeStates) do
      with Heap.SmallBlockTypeStates[sb] do begin
        inc(info.allocused,UseableBlockSize*AllocatedBlockCount);
        inc(info.allocreserved,ReservedAddressSpace);
      end;
  end;
{$endif WITH_FASTMM4STATS}
end;

procedure TSynMonitorMemory.RetrieveMemoryInfo;
var tix: cardinal;
    info: TMemoryInfo;
begin
  tix := GetTickCount64 shr 7; // allow 128 ms resolution for updates
  if fLastMemoryInfoRetrievedTix<>tix then begin
    fLastMemoryInfoRetrievedTix := tix;
    if not GetMemoryInfo(info,{withalloc=}true) then
      exit;
    FMemoryLoadPercent := info.percent;
    FPhysicalMemoryTotal.Bytes := info.memtotal;
    FPhysicalMemoryFree.Bytes := info.memfree;
    FPagingFileTotal.Bytes := info.filetotal;
    FPagingFileFree.Bytes  := info.filefree;
    FVirtualMemoryTotal.Bytes := info.vmtotal;
    FVirtualMemoryFree.Bytes := info.vmfree;
    FAllocatedReserved.Bytes := info.allocreserved;
    FAllocatedUsed.Bytes := info.allocused;
  end;
end;


{ TSynMonitorDisk }

constructor TSynMonitorDisk.Create;
begin
  fAvailableSize := TSynMonitorOneSize.Create({nospace=}false);
  fFreeSize := TSynMonitorOneSize.Create({nospace=}false);
  fTotalSize := TSynMonitorOneSize.Create({nospace=}false);
end;

destructor TSynMonitorDisk.Destroy;
begin
  fAvailableSize.Free;
  fFreeSize.Free;
  fTotalSize.Free;
  inherited;
end;

function TSynMonitorDisk.GetName: TFileName;
begin
  RetrieveDiskInfo;
  result := fName;
end;

function TSynMonitorDisk.GetAvailable: TSynMonitorOneSize;
begin
  RetrieveDiskInfo;
  result := fAvailableSize;
end;

function TSynMonitorDisk.GetFree: TSynMonitorOneSize;
begin
  RetrieveDiskInfo;
  result := fFreeSize;
end;

function TSynMonitorDisk.GetTotal: TSynMonitorOneSize;
begin
  RetrieveDiskInfo;
  result := fTotalSize;
end;

class function TSynMonitorDisk.FreeAsText: RawUTF8;
var name: TFileName;
    avail,free,total: QWord;
begin
  GetDiskInfo(name,avail,free,total);
  FormatUTF8('% % / %',[name, KB(free),KB(total)],result);
end;

{$ifdef MSWINDOWS}
function GetDiskFreeSpaceExA(lpDirectoryName: PAnsiChar;
  var lpFreeBytesAvailableToCaller, lpTotalNumberOfBytes,
      lpTotalNumberOfFreeBytes: QWord): LongBool; stdcall; external kernel32;
function GetDiskFreeSpaceExW(lpDirectoryName: PWideChar;
  var lpFreeBytesAvailableToCaller, lpTotalNumberOfBytes,
      lpTotalNumberOfFreeBytes: QWord): LongBool; stdcall; external kernel32;
function DeviceIoControl(hDevice: THandle; dwIoControlCode: DWORD; lpInBuffer: Pointer;
  nInBufferSize: DWORD; lpOutBuffer: Pointer; nOutBufferSize: DWORD;
  var lpBytesReturned: DWORD; lpOverlapped: POverlapped): BOOL; stdcall; external kernel32;
{$endif}

function GetDiskInfo(var aDriveFolderOrFile: TFileName;
  out aAvailableBytes, aFreeBytes, aTotalBytes: QWord
  {$ifdef MSWINDOWS}; aVolumeName: PFileName = nil{$endif}): boolean;
{$ifdef MSWINDOWS}
var tmp: array[0..MAX_PATH-1] of Char;
    dummy,flags: DWORD;
    dn: TFileName;
begin
  if aDriveFolderOrFile='' then
    aDriveFolderOrFile := SysUtils.UpperCase(ExtractFileDrive(ExeVersion.ProgramFilePath));
  dn := aDriveFolderOrFile;
  if (dn<>'') and (dn[2]=':') and (dn[3]=#0) then
    dn := dn+'\';
  if (aVolumeName<>nil) and (aVolumeName^='') then begin
    tmp[0] := #0;
    GetVolumeInformation(pointer(dn),tmp,MAX_PATH,nil,dummy,flags,nil,0);
    aVolumeName^ := tmp;
  end;
  result := {$ifdef UNICODE}GetDiskFreeSpaceExW{$else}GetDiskFreeSpaceExA{$endif}(
    pointer(dn),aAvailableBytes,aTotalBytes,aFreeBytes);
{$else}
{$ifdef KYLIX3}
var fs: TStatFs64;
    h: THandle;
begin
  if aDriveFolderOrFile='' then
    aDriveFolderOrFile := '.';
  h := FileOpen(aDriveFolderOrFile,fmShareDenyNone);
  result := fstatfs64(h,fs)=0;
  FileClose(h);
  aAvailableBytes := fs.f_bavail*fs.f_bsize;
  aFreeBytes := aAvailableBytes;
  aTotalBytes := fs.f_blocks*fs.f_bsize;
{$endif}
{$ifdef FPC}
var fs: tstatfs;
begin
  if aDriveFolderOrFile='' then
    aDriveFolderOrFile := '.';
  result := fpStatFS(aDriveFolderOrFile,@fs)=0;
  aAvailableBytes := QWord(fs.bavail)*QWord(fs.bsize);
  aFreeBytes := aAvailableBytes; // no user Quota involved here
  aTotalBytes := QWord(fs.blocks)*QWord(fs.bsize);
{$endif FPC}
{$endif MSWINDOWS}
end;

procedure TSynMonitorDisk.RetrieveDiskInfo;
var tix: cardinal;
begin
  tix := GetTickCount64 shr 7; // allow 128 ms resolution for updates
  if fLastDiskInfoRetrievedTix<>tix then begin
    fLastDiskInfoRetrievedTix := tix;
    GetDiskInfo(fName,PQWord(@fAvailableSize.Bytes)^,PQWord(@fFreeSize.Bytes)^,
      PQWord(@fTotalSize.Bytes)^{$ifdef MSWINDOWS},@fVolumeName{$endif});
  end;
end;


{ TTimeZoneData }

function TTimeZoneData.GetTziFor(year: integer): PTimeZoneInfo;
var i,last: PtrInt;
begin
  if dyn=nil then
    result := @tzi else
    if year<=dyn[0].year then
      result := @dyn[0].tzi else begin
      last := high(dyn);
      if year>=dyn[last].year then
        result := @dyn[last].tzi else begin
        for i := 1 to last do
          if year<dyn[i].year then begin
            result := @dyn[i-1].tzi;
            exit;
          end;
        result := @tzi; // should never happen, but makes compiler happy
      end;
    end;
end;


{ TTimeZoneInformation }

constructor TSynTimeZone.Create;
begin
  fZones.InitSpecific(TypeInfo(TTimeZoneDataDynArray),fZone,djRawUTF8);
end;

constructor TSynTimeZone.CreateDefault;
begin
  Create;
  {$ifdef LVCL}
  LoadFromFile;
  {$else}
  {$ifdef MSWINDOWS}
  LoadFromRegistry;
  {$else}
  LoadFromFile;
  if fZones.Count=0 then
    LoadFromResource; // if no .tz file is available, try from registry
  {$endif}
  {$endif}
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
  if SharedSynTimeZone=nil then
    GarbageCollectorFreeAndNil(SharedSynTimeZone,TSynTimeZone.CreateDefault);
  result := SharedSynTimeZone;
end;

function TSynTimeZone.SaveToBuffer: RawByteString;
begin
  result := SynLZCompress(fZones.SaveTo);
end;

procedure TSynTimeZone.SaveToFile(const FileName: TFileName);
var FN: TFileName;
begin
  if FileName='' then
    FN := ChangeFileExt(ExeVersion.ProgramFileName,'.tz') else
    FN := FileName;
  FileFromString(SaveToBuffer,FN);
end;

procedure TSynTimeZone.LoadFromBuffer(const Buffer: RawByteString);
begin
  fZones.LoadFromBinary(AlgoSynLZ.Decompress(Buffer),{nohash=}true);
  fZones.ReHash;
  FreeAndNil(fIds);
  FreeAndNil(fDisplays);
end;

procedure TSynTimeZone.LoadFromFile(const FileName: TFileName);
var FN: TFileName;
begin
  if FileName='' then
    FN := ChangeFileExt(ExeVersion.ProgramFileName,'.tz') else
    FN := FileName;
  LoadFromBuffer(StringFromFile(FN));
end;

procedure TSynTimeZone.LoadFromResource(Instance: THandle);
var buf: RawByteString;
begin
  ResourceToRawByteString(ClassName,PChar(10),buf,Instance);
  if buf<>'' then
    LoadFromBuffer(buf);
end;

{$ifdef MSWINDOWS}
procedure TSynTimeZone.LoadFromRegistry;
const REGKEY = 'Software\Microsoft\Windows NT\CurrentVersion\Time Zones\';
var reg: TWinRegistry;
    keys: TRawUTF8DynArray;
    i,first,last,year,n: integer;
    itemsize: DWORD;
    item: TTimeZoneData;
begin
  fZones.Clear;
  if reg.ReadOpen(HKEY_LOCAL_MACHINE,REGKEY) then
    keys := reg.ReadEnumEntries else
    keys := nil; // make Delphi 6 happy
  n := length(keys);
  fZones.Capacity := n;
  for i := 0 to n-1 do begin
    Finalize(item);
    FillcharFast(item.tzi,SizeOf(item.tzi),0);
    if reg.ReadOpen(HKEY_LOCAL_MACHINE,REGKEY+keys[i],true) then begin
      item.id := keys[i];
      item.Display := reg.ReadString('Display');
      itemsize := SizeOf(item.tzi);
      RegQueryValueExW(reg.key,'TZI',nil,nil,@item.tzi,@itemsize);
      if reg.ReadOpen(HKEY_LOCAL_MACHINE,REGKEY+keys[i]+'\Dynamic DST',true) then begin
        // warning: never defined on XP/2003, and not for all entries
        first := reg.ReadDword('FirstEntry');
        last := reg.ReadDword('LastEntry');
        if (first>0) and (last>=first) then begin
          n := 0;
          SetLength(item.dyn,last-first+1);
          for year := first to last do begin
            itemsize := SizeOf(TTimeZoneInfo);
            if RegQueryValueExA(reg.key,pointer(UInt32ToUTF8(year)),nil,nil,
               @item.dyn[n].tzi,@itemsize)=0 then begin
              item.dyn[n].year := year;
              inc(n);
            end;
          end;
          SetLength(item.dyn,n);
        end;
      end;
      fZones.Add(item);
    end;
  end;
  reg.Close;
  fZones.ReHash;
  FreeAndNil(fIds);
  FreeAndNil(fDisplays);
end;
{$endif MSWINDOWS}

function TSynTimeZone.GetDisplay(const TzId: TTimeZoneID): RawUTF8;
var ndx: integer;
begin
  if self=nil then
    ndx := -1 else
    ndx := fZones.FindHashed(TzID);
  if ndx<0 then
    if TzID='UTC' then // e.g. on XP
      result := TzID else
      result := '' else
    result := fZone[ndx].display;
end;

function TSynTimeZone.GetBiasForDateTime(const Value: TDateTime;
  const TzId: TTimeZoneID; out Bias: integer; out HaveDaylight: boolean;
  DateIsUTC: boolean): boolean;
var ndx: integer;
    d: TSynSystemTime;
    tzi: PTimeZoneInfo;
    std,dlt: TDateTime;
begin
  if (self=nil) or (TzId='') then
    ndx := -1 else
    if TzID=fLastZone then
      ndx := fLastIndex else begin
      ndx := fZones.FindHashed(TzID);
      fLastZone := TzID;
      flastIndex := ndx;
    end;
  if ndx<0 then begin
    Bias := 0;
    HaveDayLight := false;
    result := TzID='UTC'; // e.g. on XP
    exit;
  end;
  d.FromDate(Value); // faster than DecodeDate
  tzi := fZone[ndx].GetTziFor(d.Year);
  if tzi.change_time_std.IsZero then begin
    HaveDaylight := false;
    Bias := tzi.Bias+tzi.bias_std;
  end else begin
    HaveDaylight := true;
    std := tzi.change_time_std.EncodeForTimeChange(d.Year);
    dlt := tzi.change_time_dlt.EncodeForTimeChange(d.Year);
    if DateIsUTC then begin // std shifts by the DST bias, dst by STD
      std := ((std*MinsPerDay)+tzi.Bias+tzi.bias_dlt)/MinsPerDay;
      dlt := ((dlt*MinsPerDay)+tzi.Bias+tzi.bias_std)/MinsPerDay;
    end;
    if std<dlt then
      if (std<=Value) and (Value<dlt) then
        Bias := tzi.Bias+tzi.bias_std else
        Bias := tzi.Bias+tzi.bias_dlt else
      if (dlt<=Value) and (Value<std) then
        Bias := tzi.Bias+tzi.bias_dlt else
        Bias := tzi.Bias+tzi.bias_std;
  end;
  result := true;
end;

function TSynTimeZone.UtcToLocal(const UtcDateTime: TDateTime;
  const TzId: TTimeZoneID): TDateTime;
var Bias: integer;
    HaveDaylight: boolean;
begin
  if (self=nil) or (TzId='') then
    result := UtcDateTime else begin
    GetBiasForDateTime(UtcDateTime,TzId,Bias,HaveDaylight,{DateIsUTC=}true);
    result := ((UtcDateTime*MinsPerDay)-Bias)/MinsPerDay;
  end;
end;

function TSynTimeZone.NowToLocal(const TzId: TTimeZoneID): TDateTime;
begin
  result := UtcToLocal(NowUtc,TzId);
end;

function TSynTimeZone.LocalToUtc(const LocalDateTime: TDateTime; const TzID: TTimeZoneID): TDateTime;
var Bias: integer;
    HaveDaylight: boolean;
begin
  if (self=nil) or (TzId='') then
    result := LocalDateTime else begin
    GetBiasForDateTime(LocalDateTime,TzId,Bias,HaveDaylight);
    result := ((LocalDateTime*MinsPerDay)+Bias)/MinsPerDay;
  end;
end;

function TSynTimeZone.Ids: TStrings;
var i: integer;
begin
  if fIDs=nil then begin
    fIDs := TStringList.Create;
    for i := 0 to length(fZone)-1 do
      fIDs.Add(UTF8ToString(fZone[i].id));
  end;
  result := fIDs;
end;

function TSynTimeZone.Displays: TStrings;
var i: integer;
begin
  if fDisplays=nil then begin
    fDisplays := TStringList.Create;
    for i := 0 to length(fZone)-1 do
      fDisplays.Add(UTF8ToString(fZone[i].Display));
  end;
  result := fDisplays;
end;


{ ************ Markup (e.g. Emoji) process  ************************** }

type
  TTextWriterEscapeStyle = (tweBold,tweItalic,tweCode);
  TTextWriterEscapeLineStyle = (
    twlNone,twlParagraph,twlOrderedList,twlUnorderedList,twlBlockquote,twlCode4,twlCode3);
  TTextWriterEscape = object
    P,B,P2,B2: PUTF8Char;
    W: TTextWriter;
    st: set of TTextWriterEscapeStyle;
    fmt: TTextWriterHTMLFormat;
    esc: TTextWriterHTMLEscape;
    lst: TTextWriterEscapeLineStyle;
    procedure Start(dest: TTextWriter; src: PUTF8Char; escape: TTextWriterHTMLEscape);
    function ProcessText(const stopchars: TSynByteSet): AnsiChar;
    procedure ProcessHRef;
    function ProcessLink: boolean;
    procedure ProcessEmoji; {$ifdef HASINLINE}inline;{$endif}
    procedure Toggle(style: TTextWriterEscapeStyle);
    procedure SetLine(style: TTextWriterEscapeLineStyle);
    procedure EndOfParagraph;
    procedure NewMarkdownLine;
    procedure AddHtmlEscapeWiki(dest: TTextWriter; src: PUTF8Char; escape: TTextWriterHTMLEscape);
    procedure AddHtmlEscapeMarkdown(dest: TTextWriter; src: PUTF8Char; escape: TTextWriterHTMLEscape);
  end;

procedure TTextWriterEscape.Start(dest: TTextWriter; src: PUTF8Char; escape: TTextWriterHTMLEscape);
begin
  P := src;
  W := dest;
  st := [];
  if heHtmlEscape in escape then
    fmt := hfOutsideAttributes else
    fmt := hfNone;
  esc := escape;
  lst := twlNone;
end;

function IsHttpOrHttps(P: PUTF8Char): boolean; {$ifdef HASINLINE}inline;{$endif}
begin
  result := (PCardinal(P)^=ord('h')+ord('t')shl 8+ord('t') shl 16+ord('p')shl 24) and
   ((PCardinal(P+4)^ and $ffffff=ord(':')+ord('/')shl 8+ord('/') shl 16) or
    (PCardinal(P+4)^=ord('s')+ord(':')shl 8+ord('/') shl 16+ord('/')shl 24));
end;

function TTextWriterEscape.ProcessText(const stopchars: TSynByteSet): AnsiChar;
begin
  if P=nil then begin
    result := #0;
    exit;
  end;
  B := P;
  while not(ord(P^) in stopchars) and not IsHttpOrHttps(P) do
    inc(P);
  W.AddHtmlEscape(B,P-B,fmt);
  result := P^;
end;

procedure TTextWriterEscape.ProcessHRef;
begin
  B := P;
  while P^>' ' do inc(P);
  W.AddShort('<a href="');
  W.AddHtmlEscape(B,P-B,hfWithinAttributes);
  W.AddShort('" rel="nofollow">');
  W.AddHtmlEscape(B,P-B);
  W.AddShort('</a>');
end;

function TTextWriterEscape.ProcessLink: boolean;
begin
  inc(P);
  B2 := P;
  while not (P^ in [#0,']']) do inc(P);
  P2 := P;
  if PWord(P)^=ord(']')+ord('(')shl 8 then begin
    inc(P,2);
    B := P;
    while not (P^ in [#0,')']) do inc(P);
    if P^=')' then begin // [GitHub](https://github.com)
      result := true;
      exit;
    end;
  end;
  P := B2; // rollback
  result := false;
end;

procedure TTextWriterEscape.ProcessEmoji;
begin
  if heEmojiToUTF8 in esc then
    EmojiParseDots(P,W) else begin
    W.Add(':');
    inc(P);
  end;
end;

procedure TTextWriterEscape.Toggle(style: TTextWriterEscapeStyle);
const HTML: array[tweBold..tweCode] of string[7] = ('strong>','em>','code>');
begin
  W.Add('<');
  if style in st then begin
    W.Add('/');
    exclude(st,style);
  end else
    include(st,style);
  W.AddShort(HTML[style]);
end;

procedure TTextWriterEscape.EndOfParagraph;
begin
  if tweBold in st then
    Toggle(tweBold);
  if tweItalic in st then
    Toggle(tweItalic);
  if P<>nil then
    if PWord(P)^=$0a0d then
      inc(P,2) else
      inc(P);
end;

procedure TTextWriterEscape.SetLine(style: TTextWriterEscapeLineStyle);
const HTML: array[twlParagraph..twlCode3] of string[5] = ('p>','li>','li>','p>','code>','code>');
      HTML2: array[twlOrderedList..twlCode3] of string[11] = ('ol>','ul>','blockquote>','pre>','pre>');
begin
  if lst>=low(HTML) then begin
    if (lst<twlCode4) or (lst<>style) then begin
      W.Add('<','/');
      W.AddShort(HTML[lst]);
    end;
    if (lst>=low(HTML2)) and (lst<>style) then begin
      W.Add('<','/');
      W.AddShort(HTML2[lst]);
    end;
  end;
  if style>=low(HTML) then begin
    if (style>=low(HTML2)) and (lst<>style) then begin
      W.Add('<');
      W.AddShort(HTML2[style]);
    end;
    if (style<twlCode4) or (lst<>style) then begin
      W.Add('<');
      W.AddShort(HTML[style]);
    end;
  end;
  lst := style;
end;

procedure TTextWriterEscape.NewMarkdownLine;
label none;
var c: cardinal;
begin
  if P=nil then
    exit;
  c := PCardinal(P)^;
  if c and $ffffff=ord('`')+ord('`')shl 8+ord('`')shl 16 then begin
    inc(P,3);
    if lst=twlCode3 then begin
      lst := twlCode4; // to close </code></pre>
      NewMarkdownLine;
      exit;
    end;
    SetLine(twlCode3);
  end;
  if lst=twlCode3 then
    exit; // no prefix process within ``` code blocks
  if c=$20202020 then begin
    SetLine(twlCode4);
    inc(P,4);
    exit;
  end;
  P := GotoNextNotSpaceSameLine(P); // don't implement nested levels yet
  case P^ of
    '*','+','-':
      if P[1]=' ' then
        SetLine(twlUnorderedList) else
        goto none;
    '1'..'9': begin // first should be 1. then any ##. number to continue
      B := P;
      repeat inc(P) until not (P^ in ['0'..'9']);
      if (P^='.') and ((lst=twlOrderedList) or (PWord(B)^=ord('1')+ord('.')shl 8)) then
        SetLine(twlOrderedList) else begin
        P := B;
none:   if lst=twlParagraph then begin
          c := PWord(P)^; // detect blank line to separate paragraphs
          if c=$0a0d then
            inc(P,2) else
          if c and $ff=$0a then
            inc(P) else begin
            W.AddOnce(' ');
            exit;
          end;
        end;
        SetLine(twlParagraph);
        exit;
      end;
    end;
    '>':
      if P[1]=' ' then
        SetLine(twlBlockquote) else
        goto none;
    else
      goto none;
  end;
  P := GotoNextNotSpaceSameLine(P+1);
end;

procedure TTextWriterEscape.AddHtmlEscapeWiki(dest: TTextWriter; src: PUTF8Char;
  escape: TTextWriterHTMLEscape);
begin
  Start(dest,src,escape);
  SetLine(twlParagraph);
  repeat
    case ProcessText([0,10,13,ord('*'),ord('+'),ord('`'),ord('\'),ord(':')]) of
    #0: break;
    #10,#13: begin
      EndOfParagraph;
      SetLine(twlParagraph);
      continue;
    end;
    '\': if P[1] in ['\','`','*','+'] then begin
        inc(P);
        W.Add(P^);
      end else
        W.Add('\');
    '*':
      Toggle(tweItalic);
    '+':
      Toggle(tweBold);
    '`':
      Toggle(tweCode);
    'h': begin
      ProcessHRef;
      continue;
    end;
    ':': begin
      ProcessEmoji;
      continue;
    end;
    end;
    inc(P);
  until false;
  EndOfParagraph;
  SetLine(twlNone);
end;

procedure TTextWriterEscape.AddHtmlEscapeMarkdown(dest: TTextWriter;
  src: PUTF8Char; escape: TTextWriterHTMLEscape);
begin
  Start(dest,src,escape);
  NewMarkDownLine;
  repeat
    if lst>=twlCode4 then // no Markdown tags within code blocks
      if ProcessText([0,10,13])=#0 then
        break else begin
        if PWord(P)^=$0a0d then
          inc(P,2) else
          inc(P);
        W.AddCR; // keep LF within <pre>
        NewMarkdownLine;
        continue;
      end else
    case ProcessText([0,10,13,ord('*'),ord('_'),ord('`'),ord('\'),ord('['),ord('!'),ord(':')]) of
    #0: break;
    #10,#13: begin
      EndOfParagraph;
      NewMarkdownLine;
      continue;
    end;
    '\': if P[1] in ['\','`','*','_','[',']','{','}','(',')','#','+','-','.','!'] then begin
      inc(P);
      W.Add(P^); // backslash escape
    end else
      W.Add('\');
    '*','_':
      if P[1]=P[0] then begin
        inc(P); // **This text will be bold** or __This text will be bold__
        Toggle(tweBold);
      end else  // *This text will be italic* or _This text will be italic_
        Toggle(tweItalic);
    '`':
      Toggle(tweCode);  // `This text will be code`
    '[':
       if ProcessLink then begin // [GitHub](https://github.com)
         W.AddShort('<a href="');
         W.AddHtmlEscape(B,P-B,hfWithinAttributes);
         if IsHttpOrHttps(B) then
           W.AddShort('" rel="nofollow">') else
           W.Add('"','>');
         W.AddHtmlEscape(B2,P2-B2,fmt);
         W.AddShort('</a>'); // no continune -> need inc(P) over ending )
       end else
         W.Add('['); // not a true link -> just append
    '!': begin
      if P[1]='[' then begin
       inc(P);
       if ProcessLink then begin
         W.AddShort('<img alt="');
         W.AddHtmlEscape(B2,P2-B2,hfWithinAttributes);
         W.AddShort('" src="');
         W.AddNoJSONEscape(B,P-B);
         W.AddShort('">');
         inc(P);
         continue;
       end;
       dec(P);
      end;
      W.Add('!'); // not a true image
    end;
    'h': begin
      ProcessHRef;
      continue;
    end;
    ':': begin
      ProcessEmoji;
      continue;
    end;
    end;
    inc(P);
  until false;
  EndOfParagraph;
  SetLine(twlNone);
end;

function HtmlEscapeWiki(const wiki: RawUTF8; esc: TTextWriterHTMLEscape): RawUTF8;
var temp: TTextWriterStackBuffer;
    W: TTextWriter;
begin
  W := TTextWriter.CreateOwnedStream(temp);
  try
    AddHtmlEscapeWiki(W,pointer(wiki),esc);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function HtmlEscapeMarkdown(const md: RawUTF8; esc: TTextWriterHTMLEscape): RawUTF8;
var temp: TTextWriterStackBuffer;
    W: TTextWriter;
begin
  W := TTextWriter.CreateOwnedStream(temp);
  try
    AddHtmlEscapeMarkdown(W,pointer(md),esc);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure AddHtmlEscapeWiki(W: TTextWriter; P: PUTF8Char; esc: TTextWriterHTMLEscape);
var doesc: TTextWriterEscape;
begin
  doesc.AddHtmlEscapeWiki(W,P,esc);
end;

procedure AddHtmlEscapeMarkdown(W: TTextWriter; P: PUTF8Char; esc: TTextWriterHTMLEscape);
var doesc: TTextWriterEscape;
begin
  doesc.AddHtmlEscapeMarkdown(W,P,esc);
end;


function EmojiFromText(P: PUTF8Char; len: PtrInt): TEmoji;
begin // RTTI has shortstrings in adjacent L1 cache lines -> faster than EMOJI_TEXT[]
  result := TEmoji(FindShortStringListTrimLowerCase(EMOJI_RTTI,ord(high(TEmoji))-1,P,len)+1);
end;

function EmojiParseDots(var P: PUTF8Char; W: TTextWriter): TEmoji;
var c: PUTF8Char;
begin
  result := eNone;
  inc(P); // ignore trailing ':'
  c := P;
  if c[-2]<=' ' then begin
    if (c[1]<=' ') and (c^ in ['('..'|']) then
      result := EMOJI_AFTERDOTS[c^]; // e.g. :)
    if result=eNone then begin
      while c^ in ['a'..'z','A'..'Z','_'] do
        inc(c);
      if (c^=':') and (c[1]<=' ') then // try e.g. :joy_cat:
        result := EmojiFromText(P,c-P);
    end;
    if result<>eNone then begin
      P := c+1; // continue parsing after the Emoji text
      if W<>nil then
        W.AddNoJSONEscape(pointer(EMOJI_UTF8[result]),4);
      exit;
    end;
  end;
  if W<>nil then
    W.Add(':');
end;

procedure EmojiToDots(P: PUTF8Char; W: TTextWriter);
var B: PUTF8Char;
    c: cardinal;
begin
  if (P<>nil) and (W<>nil) then
    repeat
      B := P;
      while (P^<>#0) and (PWord(P)^<>$9ff0) do
        inc(P);
      W.AddNoJSONEscape(B,P-B);
      if P^=#0 then
        break;
      B := P;
      c := NextUTF8UCS4(P)-$1f5ff;
      if c<=cardinal(high(TEmoji)) then
        W.AddNoJSONEscapeUTF8(EMOJI_TAG[TEmoji(c)]) else
        W.AddNoJSONEscape(B,P-B);
    until P^=#0;
end;

function EmojiToDots(const text: RawUTF8): RawUTF8;
var W: TTextWriter;
    tmp: TTextWriterStackBuffer;
begin
  if PosExChar(#$f0,text)=0 then begin
    result := text; // no UTF-8 smiley for sure
    exit;
  end;
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    EmojiToDots(pointer(text),W);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure EmojiFromDots(P: PUTF8Char; W: TTextWriter);
var B: PUTF8Char;
begin
  if (P<>nil) and (W<>nil) then
    repeat
      B := P;
      while not(P^ in [#0,':']) do
        inc(P);
      W.AddNoJSONEscape(B,P-B);
      if P^=#0 then
        break;
      EmojiParseDots(P,W);
    until P^=#0;
end;

function EmojiFromDots(const text: RawUTF8): RawUTF8;
var W: TTextWriter;
    tmp: TTextWriterStackBuffer;
begin
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    EmojiFromDots(pointer(text),W);
    W.SetText(result);
  finally
    W.Free;
  end;
end;


{ ************ Command Line and Console process ************************** }

var
  TextAttr: integer = ord(ccDarkGray);

{$I-}
{$ifdef MSWINDOWS}

procedure InitConsole;
begin
 StdOut := GetStdHandle(STD_OUTPUT_HANDLE);
 if StdOut=INVALID_HANDLE_VALUE then
   StdOut := 0;
end;

procedure TextColor(Color: TConsoleColor);
var oldAttr: integer;
begin
  if StdOut=0 then
    InitConsole;
  oldAttr := TextAttr;
  TextAttr := (TextAttr and $F0) or ord(Color);
  if TextAttr<>oldAttr then
    SetConsoleTextAttribute(StdOut,TextAttr);
end;

procedure TextBackground(Color: TConsoleColor);
var oldAttr: integer;
begin
  if StdOut=0 then
    InitConsole;
  oldAttr := TextAttr;
  TextAttr := (TextAttr and $0F) or (ord(Color) shl 4);
  if TextAttr<>oldAttr then
    SetConsoleTextAttribute(StdOut,TextAttr);
end;

function ConsoleKeyPressed(ExpectedKey: Word): Boolean;
var lpNumberOfEvents: DWORD;
    lpBuffer: TInputRecord;
    lpNumberOfEventsRead : DWORD;
    nStdHandle: THandle;
begin
  result := false;
  nStdHandle := GetStdHandle(STD_INPUT_HANDLE);
  lpNumberOfEvents := 0;
  GetNumberOfConsoleInputEvents(nStdHandle,lpNumberOfEvents);
  if lpNumberOfEvents<>0 then begin
    PeekConsoleInput(nStdHandle,lpBuffer,1,lpNumberOfEventsRead);
    if lpNumberOfEventsRead<>0 then
      if lpBuffer.EventType=KEY_EVENT then
        if lpBuffer.Event.KeyEvent.bKeyDown and
           ((ExpectedKey=0) or (lpBuffer.Event.KeyEvent.wVirtualKeyCode=ExpectedKey)) then
          result := true else
          FlushConsoleInputBuffer(nStdHandle) else
        FlushConsoleInputBuffer(nStdHandle);
  end;
end;

procedure ConsoleWaitForEnterKey;
{$ifdef DELPHI5OROLDER}
begin
  readln;
end;
{$else}
var msg: TMsg;
begin
  while not ConsoleKeyPressed(VK_RETURN) do begin
    {$ifndef LVCL}
    if GetCurrentThreadID=MainThreadID then
      CheckSynchronize{$ifdef WITHUXTHEME}(1000){$endif}  else
    {$endif}
      WaitMessage;
    while PeekMessage(msg,0,0,0,PM_REMOVE) do
      if Msg.Message=WM_QUIT then
        exit else begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
  end;
end;
{$endif DELPHI5OROLDER}

function Utf8ToConsole(const S: RawUTF8): RawByteString;
begin
  result := TSynAnsiConvert.Engine(CP_OEMCP).UTF8ToAnsi(S);
end;

{$else MSWINDOWS}

// we bypass crt.pp since this unit cancels the SIGINT signal

procedure TextColor(Color: TConsoleColor);
const AnsiTbl : string[8]='04261537';
begin
{$ifdef FPC}{$ifdef Linux}
  if not stdoutIsTTY then
    exit;
{$endif}{$endif}
  if ord(color)=TextAttr then
    exit;
  TextAttr := ord(color);
  if ord(color)>=8 then
    write(#27'[1;3',AnsiTbl[(ord(color) and 7)+1],'m') else
    write(#27'[0;3',AnsiTbl[(ord(color) and 7)+1],'m');
  ioresult;
end;

procedure TextBackground(Color: TConsoleColor);
begin // not implemented yet - but not needed either
end;

procedure ConsoleWaitForEnterKey;
var c: AnsiChar;
begin
  {$ifdef FPC}
  if IsMultiThread and (GetCurrentThreadID=MainThreadID) then
    repeat
      CheckSynchronize(100);
      if UnixKeyPending then
        repeat
          c := #0;
          if FpRead(StdInputHandle,c,1)<>1 then
            break;
          if c in [#10,#13] then
            exit;
        until false;
    until false else
  {$endif FPC}
    ReadLn;
end;

function Utf8ToConsole(const S: RawUTF8): RawByteString;
begin
  result := S; // expect a UTF-8 console under Linux/BSD
end;

{$endif MSWINDOWS}

function ConsoleReadBody: RawByteString;
var len, n: integer;
    P: PByte;
    {$ifndef FPC}StdInputHandle: THandle;{$endif}
begin
  result := '';
  {$ifdef MSWINDOWS}
  {$ifndef FPC}StdInputHandle := GetStdHandle(STD_INPUT_HANDLE);{$endif}
  if not PeekNamedPipe(StdInputHandle,nil,0,nil,@len,nil) then
  {$else}
  if fpioctl(StdInputHandle,FIONREAD,@len)<0 then
  {$endif}
    len := 0;
  SetLength(result,len);
  P := pointer(result);
  while len>0 do begin
    n := FileRead(StdInputHandle,P^,len);
    if n<=0 then begin
      result := ''; // read error
      break;
    end;
    dec(len,n);
    inc(P,n);
  end;
end;

function StringToConsole(const S: string): RawByteString;
begin
  result := Utf8ToConsole(StringToUTF8(S));
end;

procedure ConsoleWrite(const Text: RawUTF8; Color: TConsoleColor; NoLineFeed, NoColor: boolean);
begin
  if not NoColor then
    TextColor(Color);
  write(Utf8ToConsole(Text));
  if not NoLineFeed then
    writeln;
  ioresult;
end;

procedure ConsoleWrite(const Fmt: RawUTF8; const Args: array of const;
  Color: TConsoleColor; NoLineFeed: boolean);
var tmp: RawUTF8;
begin
  FormatUTF8(Fmt,Args,tmp);
  ConsoleWrite(tmp,Color,NoLineFeed);
end;

procedure ConsoleShowFatalException(E: Exception; WaitForEnterKey: boolean);
begin
  ConsoleWrite(#13#10'Fatal exception ',cclightRed,true);
  ConsoleWrite('%',[E.ClassName],ccWhite,true);
  ConsoleWrite(' raised with message ',ccLightRed,true);
  ConsoleWrite('%',[E.Message],ccLightMagenta);
  TextColor(ccLightGray);
  if WaitForEnterKey then begin
    writeln(#13#10'Program will now abort');
    {$ifndef LINUX}
    writeln('Press [Enter] to quit');
    if ioresult=0 then
      Readln;
    {$endif}
  end;
  ioresult;
end;
{$I+}


{$ifndef NOVARIANTS}

{ TCommandLine }

constructor TCommandLine.Create;
var i: integer;
    p, sw: RawUTF8;
begin
  inherited Create;
  fValues.InitFast(ParamCount shr 1,dvObject);
  for i := 1 to ParamCount do begin
    p := StringToUTF8(ParamStr(i));
    if p<>'' then
      if p[1] in ['-','/'] then begin
        if sw<>'' then
          fValues.AddValue(sw,true); // -flag -switch value -> flag=true
        sw := LowerCase(copy(p,2,100));
        if sw='noprompt' then begin
          fNoPrompt := true;
          sw := '';
        end;
      end else
        if sw<>'' then begin
          fValues.AddValueFromText(sw,p,true);
          sw := '';
        end;
  end;
  if sw<>'' then
    fValues.AddValue(sw,true); // trailing -flag
end;

constructor TCommandLine.Create(const switches: variant; aNoConsole: boolean);
begin
  inherited Create;
  fValues.InitCopy(switches,JSON_OPTIONS_FAST);
  fNoPrompt := true;
  fNoConsole := aNoConsole;
end;

constructor TCommandLine.Create(const NameValuePairs: array of const; aNoConsole: boolean);
begin
  inherited Create;
  fValues.InitObject(NameValuePairs,JSON_OPTIONS_FAST);
  fNoPrompt := true;
  fNoConsole := aNoConsole;
end;

constructor TCommandLine.CreateAsArray(firstParam: integer);
var i: integer;
begin
  inherited Create;
  fValues.InitFast(ParamCount,dvArray);
  for i := firstParam to ParamCount do
    fValues.AddItem(ParamStr(i));
end;

function TCommandLine.NoPrompt: boolean;
begin
  result := fNoPrompt;
end;

function TCommandLine.ConsoleText(const LineFeed: RawUTF8): RawUTF8;
begin
  result := RawUTF8ArrayToCSV(fLines,LineFeed);
end;

procedure TCommandLine.SetNoConsole(value: boolean);
begin
  if value=fNoConsole then
    exit;
  if value then
    fNoPrompt := true;
  fNoConsole := false;
end;

procedure TCommandLine.TextColor(Color: TConsoleColor);
begin
  if not fNoPrompt then
    SynTable.TextColor(Color);
end;

procedure TCommandLine.Text(const Fmt: RawUTF8; const Args: array of const;
  Color: TConsoleColor);
var msg: RawUTF8;
begin
  FormatUTF8(Fmt,Args,msg);
  {$I-}
  if msg<>'' then begin
    TextColor(Color);
    AddRawUTF8(fLines,msg);
    if not fNoConsole then
      write(Utf8ToConsole(msg));
  end;
  if not fNoConsole then begin
    writeln;
    ioresult;
  end;
  {$I+}
end;

function TCommandLine.AsUTF8(const Switch, Default: RawUTF8;
  const Prompt: string): RawUTF8;
var i: integer;
begin
  i := fValues.GetValueIndex(Switch);
  if i>=0 then begin // found
    VariantToUTF8(fValues.Values[i],result);
    fValues.Delete(i);
    exit;
  end;
  result := Default;
  if fNoPrompt or (Prompt='') then
    exit;
  TextColor(ccLightGray);
  {$I-}
  writeln(Prompt);
  if ioresult<>0 then
    exit; // no console -> no prompt
  TextColor(ccCyan);
  write(Switch);
  if Default<>'' then
    write(' [',Default,'] ');
  write(': ');
  TextColor(ccWhite);
  readln(result);
  writeln;
  ioresult;
  {$I+}
  TextColor(ccLightGray);
  result := trim(result);
  if result='' then
    result := Default;
end;

function TCommandLine.AsInt(const Switch: RawUTF8; Default: Int64;
  const Prompt: string): Int64;
var res: RawUTF8;
begin
  res := AsUTF8(Switch, Int64ToUtf8(Default), Prompt);
  result := GetInt64Def(pointer(res),Default);
end;

function TCommandLine.AsDate(const Switch: RawUTF8; Default: TDateTime;
  const Prompt: string): TDateTime;
var res: RawUTF8;
begin
  res := AsUTF8(Switch, DateTimeToIso8601Text(Default), Prompt);
  if res='0' then begin
    result := 0;
    exit;
  end;
  result := Iso8601ToDateTime(res);
  if result=0 then
    result := Default;
end;

function TCommandLine.AsEnum(const Switch, Default: RawUTF8; TypeInfo: pointer;
  const Prompt: string): integer;
var res: RawUTF8;
begin
  res := AsUTF8(Switch, Default, Prompt);
  if not ToInteger(res,result) then
    result := GetEnumNameValue(TypeInfo,pointer(res),length(res),true);
end;

function TCommandLine.AsArray: TRawUTF8DynArray;
begin
  fValues.ToRawUTF8DynArray(result);
end;

function TCommandLine.AsJSON(Format: TTextWriterJSONFormat): RawUTF8;
begin
  result := fValues.ToJSON('','',Format);
end;

function TCommandLine.AsString(const Switch: RawUTF8; const Default, Prompt: string): string;
begin
  result := UTF8ToString(AsUTF8(Switch,StringToUTF8(Default),Prompt));
end;

{$endif NOVARIANTS}

procedure InitInternalTables;
var e: TEmoji;
begin
  {$ifdef MSWINDOWS}
  InitWindowsAPI;
  {$else}
  stdoutIsTTY := IsATTY(StdOutputHandle)=1;
  {$endif MSWINDOWS}
  SetLength(JSON_SQLDATE_MAGIC_TEXT,3);
  PCardinal(pointer(JSON_SQLDATE_MAGIC_TEXT))^ := JSON_SQLDATE_MAGIC;
  Assert(ord(high(TEmoji))=$4f+1);
  EMOJI_RTTI := GetEnumName(TypeInfo(TEmoji),1); // ignore eNone=0
  GetEnumTrimmedNames(TypeInfo(TEmoji),@EMOJI_TEXT);
  EMOJI_TEXT[eNone] := '';
  for e := succ(low(e)) to high(e) do begin
    LowerCaseSelf(EMOJI_TEXT[e]);
    EMOJI_TAG[e] := ':'+EMOJI_TEXT[e]+':';
    SetLength(EMOJI_UTF8[e],4);
    UCS4ToUTF8(ord(e)+$1f5ff,pointer(EMOJI_UTF8[e]));
  end;
  EMOJI_AFTERDOTS[')'] := eSmiley;
  EMOJI_AFTERDOTS['('] := eFrowning;
  EMOJI_AFTERDOTS['|'] := eExpressionless;
  EMOJI_AFTERDOTS['/'] := eConfused;
  EMOJI_AFTERDOTS['D'] := eLaughing;
  EMOJI_AFTERDOTS['o'] := eOpen_mouth;
  EMOJI_AFTERDOTS['O'] := eOpen_mouth;
  EMOJI_AFTERDOTS['p'] := eYum;
  EMOJI_AFTERDOTS['P'] := eYum;
  EMOJI_AFTERDOTS['s'] := eScream;
  EMOJI_AFTERDOTS['S'] := eScream;
  DoIsValidUTF8 := IsValidUTF8Pas;
  DoIsValidUTF8Len := IsValidUTF8LenPas;
  {$ifdef ASMX64AVX}
  if CpuFeatures * [cfAVX2, cfSSE42, cfBMI1, cfBMI2, cfCLMUL] =
                   [cfAVX2, cfSSE42, cfBMI1, cfBMI2, cfCLMUL] then begin
    // Haswell CPUs can use simdjson AVX2 asm for IsValidUtf8()
    DoIsValidUTF8 := IsValidUTF8Avx2;
    DoIsValidUTF8Len := IsValidUTF8LenAvx2;
  end;
  {$endif ASMX64AVX}
end;


initialization
  Assert(SizeOf(TSynTableFieldType)=1); // as expected by TSynTableFieldProperties
  Assert(SizeOf(TSynTableFieldOptions)=1);
  {$ifndef NOVARIANTS}
  Assert(SizeOf(TSynTableData)=SizeOf(TVarData));
  {$endif NOVARIANTS}
  Assert(SizeOf(THTab)=$40000*3); // 786,432 bytes
  Assert(SizeOf(TSynUniqueIdentifierBits)=SizeOf(TSynUniqueIdentifier));
  InitInternalTables;
  TTextWriter.RegisterCustomJSONSerializerFromText([
    TypeInfo(TDiskPartitions),
     'name:RawUTF8 mounted:string size:QWord',
    TypeInfo(TSystemUseDataDynArray),
     'Timestamp:TDateTime Kernel,User:single WorkDB,VirtualKB:cardinal']);
end.

