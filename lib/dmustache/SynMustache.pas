/// Logic-less {{mustache}} template rendering
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynMustache;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2022 Arnaud Bouchez
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

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2022
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - shura1990

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

}


{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef HASINLINENOTX86}
  {$ifdef MSWINDOWS}Windows,{$endif} // for Lock/UnLock inlining
  {$endif}
  Variants,
  SysUtils,
  SynCommons,
  SynTable;


type
  /// exception raised during process of a {{mustache}} template
  ESynMustache = class(ESynException);

  /// identify the {{mustache}} tag kind
  // - mtVariable if the tag is a variable - e.g. {{myValue}} - or an Expression
  // Helper - e.g. {{helperName valueName}}
  // - mtVariableUnescape, mtVariableUnescapeAmp to unescape the variable HTML - e.g.
  // {{{myRawValue}}} or {{& name}}
  // - mtSection and mtInvertedSection for sections beginning - e.g.
  // {{#person}} or {{^person}}
  // - mtSectionEnd for sections ending - e.g. {{/person}}
  // - mtComment for comments - e.g. {{! ignore me}}
  // - mtPartial for partials - e.g. {{> next_more}}
  // - mtSetPartial for setting an internal partial - e.g.
  // {{<foo}}This is the foo partial {{myValue}} template{{/foo}}
  // - mtSetDelimiter for setting custom delimeter symbols - e.g. {{=<% %>=}} -
  // Warning: current implementation only supports two character delimiters
  // - mtTranslate for content i18n via a callback - e.g. {{"English text}}
  // - mtText for all text that appears outside a symbol
  TSynMustacheTagKind = (
    mtVariable, mtVariableUnescape, mtVariableUnescapeAmp,
    mtSection, mtInvertedSection, mtSectionEnd,
    mtComment, mtPartial, mtSetPartial, mtSetDelimiter, mtTranslate, mtText);

  /// store a {{mustache}} tag
  TSynMustacheTag = record
    /// the kind of the tag
    Kind: TSynMustacheTagKind;
    /// points to the mtText buffer start
    // - main template's text is not allocated as a separate string during
    // parsing, but will rather be copied directly from the template memory
    TextStart: PUTF8Char;
    /// stores the mtText buffer length
    TextLen: integer;
    /// the index in Tags[] of the other end of this section
    // - either the index of mtSectionEnd for mtSection/mtInvertedSection
    // - or the index of mtSection/mtInvertedSection for mtSectionEnd
    SectionOppositeIndex: integer;
    /// the tag content, excluding trailing {{ }} and corresponding symbol
    // - is not set for mtText nor mtSetDelimiter
    Value: RawUTF8;
  end;

  /// store all {{mustache}} tags of a given template
  TSynMustacheTagDynArray = array of TSynMustacheTag;

  /// states the section content according to a given value
  // - msNothing for false values or empty lists
  // - msSingle for non-false values but not a list
  // - msSinglePseudo is for *-first *-last *-odd and helper values
  // - msList for non-empty lists
  TSynMustacheSectionType = (msNothing,msSingle,msSinglePseudo,msList);

  TSynMustache = class;

  /// callback signature used to process an Expression Helper variable
  // - i.e. {{helperName value}} tags
  // - returned value will be used to process as replacement of a single {{tag}}
  TSynMustacheHelperEvent = procedure(const Value: variant; out result: variant) of object;

  /// used to store a registered Expression Helper implementation
  TSynMustacheHelper = record
    /// the Expression Helper name
    Name: RawUTF8;
    /// the corresponding callback to process the tag
    Event: TSynMustacheHelperEvent;
  end;

  /// used to store all registered Expression Helpers
  // - i.e. {{helperName value}} tags
  // - use TSynMustache.HelperAdd/HelperDelete class methods to manage the list
  // or retrieve standard helpers via TSynMustache.HelpersGetStandardList
  TSynMustacheHelpers = array of TSynMustacheHelper;

  /// handle {{mustache}} template rendering context, i.e. all values
  // - this abstract class should not be used directly, but rather any
  // other overridden class
  TSynMustacheContext = class
  protected
    fContextCount: integer;
    fWriter: TTextWriter;
    fOwner: TSynMustache;
    fEscapeInvert: boolean;
    fHelpers: TSynMustacheHelpers;
    fOnStringTranslate: TOnStringTranslate;
    procedure TranslateBlock(Text: PUTF8Char; TextLen: Integer); virtual;
    procedure PopContext; virtual; abstract;
    procedure AppendValue(const ValueName: RawUTF8; UnEscape: boolean);
      virtual; abstract;
    function AppendSection(const ValueName: RawUTF8): TSynMustacheSectionType;
      virtual; abstract;
    function GotoNextListItem: boolean;
      virtual; abstract;
  public
    /// initialize the rendering context for the given text writer
    constructor Create(Owner: TSynMustache; WR: TTextWriter);
    /// the registered Expression Helpers, to handle {{helperName value}} tags
    // - use TSynMustache.HelperAdd/HelperDelete class methods to manage the list
    // or retrieve standard helpers via TSynMustache.HelpersGetStandardList
    property Helpers: TSynMustacheHelpers read fHelpers write fHelpers;
    /// access to the {{"English text}} translation callback
    property OnStringTranslate: TOnStringTranslate
      read fOnStringTranslate write fOnStringTranslate;
    /// read-only access to the associated text writer instance
    property Writer: TTextWriter read fWriter;
    /// invert the HTML characters escaping process
    // - by default, {{value}} will escape value chars, and {{{value}} won't
    // - set this property to true to force {{value}} NOT to escape HTML chars
    // and {{{value}} escaping chars (may be useful e.g. for code generation)
    property EscapeInvert: boolean read fEscapeInvert write fEscapeInvert;
  end;

  /// handle {{mustache}} template rendering context from a custom variant
  // - the context is given via a custom variant type implementing
  // TSynInvokeableVariantType.Lookup, e.g. TDocVariant or TSMVariant
  TSynMustacheContextVariant = class(TSynMustacheContext)
  protected
    fContext: array of record
      Document: TVarData;
      DocumentType: TSynInvokeableVariantType;
      ListCount: integer;
      ListCurrent: integer;
      ListCurrentDocument: TVarData;
      ListCurrentDocumentType: TSynInvokeableVariantType;
    end;
    fTempGetValueFromContextHelper: TVariantDynArray;
    procedure PushContext(aDoc: TVarData);
    procedure PopContext; override;
    procedure AppendValue(const ValueName: RawUTF8; UnEscape: boolean); override;
    function AppendSection(const ValueName: RawUTF8): TSynMustacheSectionType; override;
    function GotoNextListItem: boolean; override;
    function GetDocumentType(const aDoc: TVarData): TSynInvokeableVariantType;
    function GetValueFromContext(const ValueName: RawUTF8; var Value: TVarData): TSynMustacheSectionType;
    function GetValueCopyFromContext(const ValueName: RawUTF8): variant;
    procedure AppendVariant(const Value: variant; UnEscape: boolean);
  public
    /// initialize the context from a custom variant document
    // - note that the aDocument instance shall be available during all
    // lifetime of this TSynMustacheContextVariant instance
    // - you should not use this constructor directly, but the
    // corresponding TSynMustache.Render*() methods
    constructor Create(Owner: TSynMustache; WR: TTextWriter; SectionMaxCount: integer;
       const aDocument: variant);
  end;

  /// maintain a list of {{mustache}} partials
  // - this list of partials template could be supplied to TSynMustache.Render()
  // method, to render {{>partials}} as expected
  // - using a dedicated class allows to share the partials between execution
  // context, without recurring to non SOLID global variables
  // - you may also define "internal" partials, e.g. {{<foo}}This is foo{{/foo}}
  TSynMustachePartials = class
  protected
    fList: TRawUTF8List;
    fOwned: boolean;
    function GetPartial(const PartialName: RawUTF8): TSynMustache;
  public
    /// initialize the template partials storage
    // - after creation, the partials should be registered via the Add() method
    // - you shall manage this instance life time with a try..finally Free block
    constructor Create; overload;
    /// initialize a template partials storage with the supplied templates
    // - partials list is expected to be supplied in Name / Template pairs
    // - this instance can be supplied as parameter to the TSynMustache.Render()
    // method, which will free the instances as soon as it finishes
    constructor CreateOwned(const NameTemplatePairs: array of RawUTF8); overload;
    /// initialize a template partials storage with the supplied templates
    // - partials list is expected to be supplied as a dvObject TDocVariant,
    // each member being the name/template string pairs
    // - if the supplied variant is not a matching TDocVariant, will return nil
    // - this instance can be supplied as parameter to the TSynMustache.Render()
    // method, which will free the instances as soon as it finishes
    class function CreateOwned(const Partials: variant): TSynMustachePartials; overload;
    /// register a {{>partialName}} template
    // - returns the parsed template
    function Add(const aName,aTemplate: RawUTF8): TSynMustache; overload;
    /// register a {{>partialName}} template
    // - returns the parsed template
    function Add(const aName: RawUTF8; aTemplateStart,aTemplateEnd: PUTF8Char): TSynMustache; overload;
    /// search some text withing the {{mustache}} partial
    function FoundInTemplate(const text: RawUTF8): PtrInt;
    /// delete the partials
    destructor Destroy; override;
    /// low-level access to the internal partials list
    property List: TRawUTF8List read fList;
  end;

  /// stores one {{mustache}} pre-rendered template
  // - once parsed, a template will be stored in this class instance, to be
  // rendered lated via the Render() method
  // - you can use the Parse() class function to maintain a shared cache of
  // parsed templates
  // - implements all official mustache specifications, and some extensions
  // - handles {{.}} pseudo-variable for the current context object (very
  // handy when looping through a simple list, for instance)
  // - handles {{-index}} pseudo-variable for the current context array index
  // (1-based value) so that e.g.
  // "My favorite things:\n{{#things}}{{-index}}. {{.}}\n{{/things}}"
  // over {things:["Peanut butter", "Pen spinning", "Handstands"]} renders as
  // "My favorite things:\n1. Peanut butter\n2. Pen spinning\n3. Handstands\n"
  // - you could use {{-index0}} for 0-based index value
  // - handles -first  -last  and  -odd  pseudo-section keys, e.g.
  // "{{#things}}{{^-first}}, {{/-first}}{{.}}{{/things}}"
  // over {things:["one", "two", "three"]} renders as 'one, two, three'
  // - allows inlined partial templates , to be defined e.g. as
  // {{<foo}}This is the foo partial {{myValue}} template{{/foo}}
  // - features {{"English text}} translation, via a custom callback
  // - this implementation is thread-safe and re-entrant (i.e. the same
  // TSynMustache instance can be used by several threads at once)
  TSynMustache = class
  protected
    fTemplate: RawUTF8;
    fTags: TSynMustacheTagDynArray;
    fInternalPartials: TSynMustachePartials;
    fSectionMaxCount: Integer;
    class procedure DateTimeToText(const Value: variant; out result: variant);
    class procedure DateToText(const Value: variant; out result: variant);
    class procedure DateFmt(const Value: variant; out result: variant);
    class procedure TimeLogToText(const Value: variant; out result: variant);
    class procedure BlobToBase64(const Value: variant; out result: variant);
    class procedure ToJSON(const Value: variant; out result: variant);
    class procedure JSONQuote(const Value: variant; out result: variant);
    class procedure JSONQuoteURI(const Value: variant; out result: variant);
    class procedure WikiToHtml(const Value: variant; out result: variant);
    class procedure MarkdownToHtml(const Value: variant; out result: variant);
    class procedure SimpleToHtml(const Value: variant; out result: variant);
    class procedure Lower(const Value: variant; out result: variant);
    class procedure Upper(const Value: variant; out result: variant);
    class procedure EnumTrim(const Value: variant; out result: variant);
    class procedure EnumTrimRight(const Value: variant; out result: variant);
    class procedure PowerOfTwo(const Value: variant; out result: variant);
    class procedure Equals_(const Value: variant; out result: variant);
    class procedure If_(const Value: variant; out result: variant);
    class procedure NewGUID(const Value: variant; out result: variant);
    class procedure ExtractFileName(const Value: variant; out result: variant);
  public
    /// parse a {{mustache}} template, and returns the corresponding
    // TSynMustache instance
    // - an internal cache is maintained by this class function
    // - this implementation is thread-safe and re-entrant: i.e. the same
    // TSynMustache returned instance can be used by several threads at once
    // - will raise an ESynMustache exception on error
    class function Parse(const aTemplate: RawUTF8): TSynMustache;
    /// remove the specified {{mustache}} template from the internal cache
    // - returns TRUE on success, or FALSE if the template was not cached
    // by a previous call to Parse() class function
    class function UnParse(const aTemplate: RawUTF8): boolean;
    /// parse and render a {{mustache}} template over the supplied JSON
    // - an internal templates cache is maintained by this class function
    // - returns TRUE and set aContent the rendered content on success
    // - returns FALSE if the template is not correct
    class function TryRenderJson(const aTemplate,aJSON: RawUTF8;
      out aContent: RawUTF8): boolean;
  public
    /// initialize and parse a pre-rendered {{mustache}} template
    // - you should better use the Parse() class function instead, which
    // features an internal thread-safe cache
    constructor Create(const aTemplate: RawUTF8); overload;
    /// initialize and parse a pre-rendered {{mustache}} template
    // - you should better use the Parse() class function instead, which
    // features an internal thread-safe cache
    constructor Create(aTemplate: PUTF8Char; aTemplateLen: integer); overload; virtual;
    /// finalize internal memory
    destructor Destroy; override;
    /// register one Expression Helper callback for a given list of helpers
    // - i.e. to let aEvent process {{aName value}} tags
    // - the supplied name will be checked against the current list, and replace
    // any existing entry
    class procedure HelperAdd(var Helpers: TSynMustacheHelpers;
      const aName: RawUTF8; aEvent: TSynMustacheHelperEvent); overload;
    /// register several Expression Helper callbacks for a given list of helpers
    // - the supplied names will be checked against the current list, and replace
    // any existing entry
    class procedure HelperAdd(var Helpers: TSynMustacheHelpers;
      const aNames: array of RawUTF8; const aEvents: array of TSynMustacheHelperEvent); overload;
    /// unregister one Expression Helper callback for a given list of helpers
    class procedure HelperDelete(var Helpers: TSynMustacheHelpers;
      const aName: RawUTF8);
    /// search for one Expression Helper event by name
    class function HelperFind(const Helpers: TSynMustacheHelpers;
      aName: PUTF8Char; aNameLen: integer): integer;
    /// returns a list of most used static Expression Helpers
    // - registered helpers are DateTimeToText, DateToText, DateFmt, TimeLogToText,
    // BlobToBase64, JSONQuote, JSONQuoteURI, ToJSON, EnumTrim, EnumTrimRight,
    // Lower, Upper, PowerOfTwo, Equals (expecting two parameters), MarkdownToHtml,
    // SimpleToHtml (Markdown with no HTML pass-through) and WikiToHtml
    // (following TTextWriter.AddHtmlEscapeWiki syntax)
    // - an additional #if helper is also registered, which would allow runtime
    // view logic, via = < > <= >= <> operators over two values:
    // $ {{#if .,"=",123}}  {{#if Total,">",1000}}  {{#if info,"<>",""}}
    // which may be shortened as such:
    // $ {{#if .=123}}  {{#if Total>1000}}  {{#if info<>""}}
    class function HelpersGetStandardList: TSynMustacheHelpers; overload;
    /// returns a list of most used static Expression Helpers, adding some
    // custom callbacks
    // - is just a wrapper around HelpersGetStandardList and HelperAdd()
    class function HelpersGetStandardList(const aNames: array of RawUTF8;
      const aEvents: array of TSynMustacheHelperEvent): TSynMustacheHelpers; overload;

    /// renders the {{mustache}} template into a destination text buffer
    // - the context is given via our abstract TSynMustacheContext wrapper
    // - the rendering extended in fTags[] is supplied as parameters
    // - you can specify a list of partials via TSynMustachePartials.CreateOwned
    procedure RenderContext(Context: TSynMustacheContext; TagStart,TagEnd: integer;
      Partials: TSynMustachePartials; NeverFreePartials: boolean);
    /// renders the {{mustache}} template from a variant defined context
    // - the context is given via a custom variant type implementing
    // TSynInvokeableVariantType.Lookup, e.g. TDocVariant or TSMVariant
    // - you can specify a list of partials via TSynMustachePartials.CreateOwned,
    // a list of Expression Helpers, or a custom {{"English text}} callback
    // - can be used e.g. via a TDocVariant:
    // !var mustache := TSynMustache;
    // !    doc: variant;
    // !    html: RawUTF8;
    // !begin
    // !  mustache := TSynMustache.Parse(
    // !    'Hello {{name}}'#13#10'You have just won {{value}} dollars!');
    // !  TDocVariant.New(doc);
    // !  doc.name := 'Chris';
    // !  doc.value := 10000;
    // !  html := mustache.Render(doc);
    // !  // here html='Hello Chris'#13#10'You have just won 10000 dollars!'
    // - you can also retrieve the context from an ORM query of mORMot.pas:
    // ! dummy := TSynMustache.Parse(
    // !   '{{#items}}'#13#10'{{Int}}={{Test}}'#13#10'{{/items}}').Render(
    // !   aClient.RetrieveDocVariantArray(TSQLRecordTest,'items','Int,Test'));
    // - set EscapeInvert = true to force {{value}} NOT to escape HTML chars
    // and {{{value}} escaping chars (may be useful e.g. for code generation)
    function Render(const Context: variant; Partials: TSynMustachePartials=nil;
      Helpers: TSynMustacheHelpers=nil; OnTranslate: TOnStringTranslate=nil;
      EscapeInvert: boolean=false): RawUTF8;
    /// renders the {{mustache}} template from JSON defined context
    // - the context is given via a JSON object, defined from UTF-8 buffer
    // - you can specify a list of partials via TSynMustachePartials.CreateOwned,
    // a list of Expression Helpers, or a custom {{"English text}} callback
    // - is just a wrapper around Render(_JsonFast())
    // - you can write e.g. with the extended JSON syntax:
    // ! html := mustache.RenderJSON('{things:["one", "two", "three"]}');
    // - set EscapeInvert = true to force {{value}} NOT to escape HTML chars
    // and {{{value}} escaping chars (may be useful e.g. for code generation)
    function RenderJSON(const JSON: RawUTF8; Partials: TSynMustachePartials=nil;
      Helpers: TSynMustacheHelpers=nil; OnTranslate: TOnStringTranslate=nil;
      EscapeInvert: boolean=false): RawUTF8; overload;
    /// renders the {{mustache}} template from JSON defined context
    // - the context is given via a JSON object, defined with parameters
    // - you can specify a list of partials via TSynMustachePartials.CreateOwned,
    // a list of Expression Helpers, or a custom {{"English text}} callback
    // - is just a wrapper around Render(_JsonFastFmt())
    // - you can write e.g. with the extended JSON syntax:
    // !   html := mustache.RenderJSON('{name:?,value:?}',[],['Chris',10000]);
    // - set EscapeInvert = true to force {{value}} NOT to escape HTML chars
    // and {{{value}} escaping chars (may be useful e.g. for code generation)
    function RenderJSON(const JSON: RawUTF8; const Args,Params: array of const;
      Partials: TSynMustachePartials=nil; Helpers: TSynMustacheHelpers=nil;
      OnTranslate: TOnStringTranslate=nil;
      EscapeInvert: boolean=false): RawUTF8; overload;
    /// search some text within the {{mustache}} template text
    function FoundInTemplate(const text: RawUTF8): boolean;

    /// read-only access to the raw {{mustache}} template content
    property Template: RawUTF8 read fTemplate;
    /// the maximum possible number of nested contexts
    property SectionMaxCount: Integer read fSectionMaxCount;
  end;


const
  /// this constant can be used to define as JSON a tag value
  NULL_OR_TRUE: array[boolean] of RawUTF8 = ('null','true');

  /// this constant can be used to define as JSON a tag value as separator
  NULL_OR_COMMA: array[boolean] of RawUTF8 = ('null','","');

implementation

function KindToText(Kind: TSynMustacheTagKind): PShortString;
begin
  result := GetEnumName(TypeInfo(TSynMustacheTagKind),ord(Kind));
end;

type
  TSynMustacheParser = class
  protected
    fTagStart, fTagStop: word;
    fPos, fPosMin, fPosMax, fPosTagStart: PUTF8Char;
    fTagCount: integer;
    fTemplate: TSynMustache;
    fScanStart, fScanEnd: PUTF8Char;
    function Scan(ExpectedTag: Word): boolean;
    procedure AddTag(aKind: TSynMustacheTagKind;
      aStart: PUTF8Char=nil; aEnd: PUTF8Char=nil);
  public
    constructor Create(Template: TSynMustache; const DelimiterStart, DelimiterStop: RawUTF8);
    procedure Parse(P,PEnd: PUTF8Char);
  end;

  TSynMustacheCache = class(TRawUTF8List)
  public
    function Parse(const aTemplate: RawUTF8): TSynMustache;
    function UnParse(const aTemplate: RawUTF8): boolean;
  end;

var
  SynMustacheCache: TSynMustacheCache = nil;



{ TSynMustacheParser }

procedure TSynMustacheParser.AddTag(aKind: TSynMustacheTagKind;
  aStart, aEnd: PUTF8Char);
var
  P: PUTF8Char;
begin
  if (aStart=nil) or (aEnd=nil) then begin
    aStart := fScanStart;
    aEnd := fScanEnd;
    case aKind of
    mtComment, mtSection, mtSectionEnd, mtInvertedSection, mtSetDelimiter, mtPartial: begin
      // (indented) standalone lines should be removed from the template
      if aKind<>mtPartial then
        while (fPosTagStart>fPosMin) and (fPosTagStart[-1] in [' ',#9]) do
          dec(fPosTagStart); // ignore any indentation chars
      if (fPosTagStart=fPosMin) or (fPosTagStart[-1]=#$0A) then
        // tag starts on a new line -> check if ends on the same line
        if (fPos>fPosMax) or (fPos^=#$0A) or (PWord(fPos)^=$0A0D) then begin
          if fPos<=fPosMax then
            if fPos^=#$0A then
              inc(fPos) else
            if PWord(fPos)^=$0A0D then
              inc(fPos,2);
          if fTagCount>0 then // remove any indentation chars from previous text
            with fTemplate.fTags[fTagCount-1] do
              if Kind=mtText then
                while (TextLen>0) and (TextStart[TextLen-1] in [' ',#9]) do
                  dec(TextLen);
        end;
    end;
    mtVariable, mtVariableUnescape, mtVariableUnescapeAmp: begin
      // handle JSON object/array with nested } e.g. as {{helper [{a:{a:1,b:2}}]}}
      P := PosChar(aStart,' ');
      if (P<>nil) and (P<aEnd) then begin
        P := GotoNextNotSpaceSameLine(P+1);
        if P^ in ['{','['] then begin
          P := GotoNextJSONObjectOrArray(P);
          if P<>nil then begin
            aEnd := P;
            fPos := P;
            if not Scan(fTagStop) then
              raise ESynMustache.CreateUTF8('Unfinished {{%',[aStart]);
            if (aKind=mtVariableUnescape) and (fTagStop=$7d7d) and (PWord(fPos-1)^=$7d7d) then
              inc(fPos); // {{{name}}} -> point after }}}
          end;
        end;
      end;
    end;
    end;
  end;
  if aEnd<=aStart then
    exit;
  if fTagCount>=length(fTemplate.fTags) then
    SetLength(fTemplate.fTags,NextGrow(fTagCount));
  with fTemplate.fTags[fTagCount] do begin
    Kind := aKind;
    SectionOppositeIndex := -1;
    case aKind of
    mtText, mtComment, mtTranslate: begin
      TextStart := aStart;
      TextLen := aEnd-aStart;
    end;
    else begin
      TextStart := fPosTagStart;
      TextLen := aEnd-fPosTagStart;
      // superfluous in-tag whitespace should be ignored
      while (aStart<aEnd) and (aStart^<=' ') do inc(aStart);
      while (aEnd>aStart) and (aEnd[-1]<=' ') do dec(aEnd);
      if aEnd=aStart then
        raise ESynMustache.CreateUTF8('Void % identifier',[KindToText(aKind)^]);
      FastSetString(Value,aStart,aEnd-aStart);
    end;
    end;
  end;
  inc(fTagCount);
end;

constructor TSynMustacheParser.Create(Template: TSynMustache;
  const DelimiterStart, DelimiterStop: RawUTF8);
begin
  fTemplate := Template;
  if length(DelimiterStart)<>2 then
    raise ESynMustache.CreateUTF8('DelimiterStart="%"',[DelimiterStart]);
  if length(DelimiterStop)<>2 then
    raise ESynMustache.CreateUTF8('DelimiterStop="%"',[DelimiterStop]);
  fTagStart := PWord(DelimiterStart)^;
  fTagStop := PWord(DelimiterStop)^;
end;

function GotoNextTag(P,PMax: PUTF8Char; ExpectedTag: Word): PUTF8Char;
begin
  if P<PMax then
    repeat
      if PWord(P)^<>ExpectedTag then begin
        inc(P);
        if P<PMax then continue;
        break;
      end;
      result := P;
      exit;
    until false;
  result := nil;
end;

function TSynMustacheParser.Scan(ExpectedTag: Word): boolean;
var P: PUTF8Char;
begin
  P := GotoNextTag(fPos,fPosMax,ExpectedTag);
  if P=nil then
    result := false else begin
    fScanEnd := P;
    fScanStart := fPos;
    fPos := P+2;
    result := true;
  end;
end;

function SectionNameMatch(const start,finish: RawUTF8): boolean;
var i: integer;
begin
  if start=finish then
    result := true else begin
    i := PosExChar(' ',start);
    result := (i>0) and IdemPropNameU(finish,pointer(start),i-1);
  end;
end;

procedure TSynMustacheParser.Parse(P, PEnd: PUTF8Char);
var Kind: TSynMustacheTagKind;
    Symbol: AnsiChar;
    i,j,secCount,secLevel: integer;
begin
  secCount := 0;
  if P=nil then
    exit;
  fPos := P;
  fPosMin := P;
  fPosMax := PEnd-1;
  repeat
    if not Scan(fTagStart) then
      break;
    fPosTagStart := fScanEnd;
    AddTag(mtText);
    if fPos>=fPosMax then
      break;
    Symbol := fPos^;
    case Symbol of
    '=': Kind := mtSetDelimiter;
    '{': Kind := mtVariableUnescape;
    '&': Kind := mtVariableUnescapeAmp;
    '#': Kind := mtSection;
    '^': Kind := mtInvertedSection;
    '/': Kind := mtSectionEnd;
    '!': Kind := mtComment;
    '>': Kind := mtPartial;
    '<': Kind := mtSetPartial;
    '"': Kind := mtTranslate;
    else Kind := mtVariable;
    end;
    if Kind<>mtVariable then
      inc(fPos);
    if not Scan(fTagStop) then
      raise ESynMustache.CreateUTF8('Unfinished {{tag [%]',[fPos]);
    case Kind of
    mtSetDelimiter: begin
      if (fScanEnd-fScanStart<>6) or (fScanEnd[-1]<>'=') then
        raise ESynMustache.Create('mtSetDelimiter syntax is e.g. {{=<% %>=}}');
      fTagStart := PWord(fScanStart)^;
      fTagStop := PWord(fScanStart+3)^;
      continue; // do not call AddTag(mtSetDelimiter)
    end;
    mtVariableUnescape:
      if (Symbol='{') and (fTagStop=$7d7d) and (PWord(fPos-1)^=$7d7d) then
        inc(fPos); // {{{name}}} -> point after }}}
    end;
    AddTag(Kind);
  until false;
  AddTag(mtText,fPos,fPosMax+1);
  for i := 0 to fTagCount-1 do
  with fTemplate.fTags[i] do
  case Kind of
    mtSection, mtInvertedSection, mtSetPartial: begin
      inc(secCount);
      if secCount>fTemplate.fSectionMaxCount then
        fTemplate.fSectionMaxCount := secCount;
      secLevel := 1;
      for j := i+1 to fTagCount-1 do
        case fTemplate.fTags[j].Kind of
        mtSection, mtInvertedSection, mtSetPartial:
          inc(secLevel);
        mtSectionEnd: begin
          dec(secLevel);
          if secLevel=0 then
            if SectionNameMatch(Value,fTemplate.fTags[j].Value) then begin
              fTemplate.fTags[j].SectionOppositeIndex := i;
              SectionOppositeIndex := j;
              if Kind=mtSetPartial then begin
                if fTemplate.fInternalPartials=nil then
                  fTemplate.fInternalPartials := TSynMustachePartials.Create;
                fTemplate.fInternalPartials.Add(Value,
                  TextStart+TextLen+2,fTemplate.fTags[j].TextStart);
              end;
              break;
            end else
              raise ESynMustache.CreateUTF8('Got {{/%}}, expected {{/%}}',
                [Value,fTemplate.fTags[j].Value]);
        end;
        end;
      if SectionOppositeIndex<0 then
        raise ESynMustache.CreateUTF8('Missing section end {{/%}}',[Value]);
    end;
    mtSectionEnd: begin
      dec(secCount);
      if SectionOppositeIndex<0 then
        raise ESynMustache.CreateUTF8('Unexpected section end {{/%}}',[Value]);
    end;
  end;
  SetLength(fTemplate.fTags,fTagCount);
end;


{ TSynMustacheCache }

function TSynMustacheCache.Parse(const aTemplate: RawUTF8): TSynMustache;
begin
  result := GetObjectFrom(aTemplate);
  if result=nil then begin
    result := TSynMustache.Create(aTemplate);
    AddObjectUnique(aTemplate,@result);
  end;
end;

function TSynMustacheCache.UnParse(const aTemplate: RawUTF8): boolean;
begin
  result := Delete(aTemplate)>=0;
end;


{ TSynMustache }

class function TSynMustache.Parse(const aTemplate: RawUTF8): TSynMustache;
begin
  if SynMustacheCache=nil then
    GarbageCollectorFreeAndNil(SynMustacheCache,
      TSynMustacheCache.Create([fObjectsOwned,fNoDuplicate,fCaseSensitive]));
  result := SynMustacheCache.Parse(aTemplate);
end;

class function TSynMustache.UnParse(const aTemplate: RawUTF8): boolean;
begin
  result := SynMustacheCache.UnParse(aTemplate);
end;

class function TSynMustache.TryRenderJson(const aTemplate, aJSON: RawUTF8;
  out aContent: RawUTF8): boolean;
var mus: TSynMustache;
begin
  if aTemplate<>'' then
  try
    mus := Parse(aTemplate);
    aContent := mus.RenderJSON(aJSON);
    result := true;
  except
    result := false;
  end else
    result := false;
end;

constructor TSynMustache.Create(const aTemplate: RawUTF8);
begin
  Create(pointer(aTemplate),length(aTemplate));
end;

constructor TSynMustache.Create(aTemplate: PUTF8Char; aTemplateLen: integer);
begin
  inherited Create;
  fTemplate := aTemplate;
  with TSynMustacheParser.Create(self,'{{','}}') do
  try
    Parse(aTemplate,aTemplate+aTemplateLen);
  finally
    Free;
  end;
end;

type
  TSynMustacheProcessSection = procedure of object;

procedure TSynMustache.RenderContext(Context: TSynMustacheContext;
  TagStart,TagEnd: integer; Partials: TSynMustachePartials; NeverFreePartials: boolean);
var partial: TSynMustache;
begin
  try
    while TagStart<=TagEnd do begin
      with fTags[TagStart] do
      case Kind of
      mtText:
        if TextLen<>0 then // may be 0 e.g. for standalone without previous Line
         Context.fWriter.AddNoJSONEscape(TextStart,TextLen);
      mtVariable:
        Context.AppendValue(Value,false);
      mtVariableUnescape, mtVariableUnescapeAmp:
        Context.AppendValue(Value,true);
      mtSection:
        case Context.AppendSection(Value) of
        msNothing: begin // e.g. for no key, false value, or empty list
          TagStart := SectionOppositeIndex;
          continue; // ignore whole section
        end;
        msList: begin
          while Context.GotoNextListItem do
            RenderContext(Context,TagStart+1,SectionOppositeIndex-1,Partials,true);
          TagStart := SectionOppositeIndex;
          continue; // ignore whole section since we just rendered it as a list
        end;
        // msSingle,msSinglePseudo: process the section once with current context
        end;
      mtInvertedSection: // display section for no key, false value, or empty list
        if Context.AppendSection(Value)<>msNothing then begin
          TagStart := SectionOppositeIndex;
          continue; // ignore whole section
        end;
      mtSectionEnd:
        if (fTags[SectionOppositeIndex].Kind in [mtSection,mtInvertedSection]) and
           (Value[1]<>'-') and (PosExChar(' ',fTags[SectionOppositeIndex].Value)=0) then
          Context.PopContext;
      mtComment:
        ; // just ignored
      mtPartial: begin
        partial := fInternalPartials.GetPartial(Value);
        if (partial=nil) and (Context.fOwner<>self) then // recursive call
          partial := Context.fOwner.fInternalPartials.GetPartial(Value);
        if (partial=nil) and (Partials<>nil) then
          partial := Partials.GetPartial(Value);
        if partial<>nil then
          partial.RenderContext(Context,0,high(partial.fTags),Partials,true);
      end;
      mtSetPartial:
        TagStart := SectionOppositeIndex; // ignore whole internal {{<partial}}
      mtTranslate:
        if TextLen<>0 then
          Context.TranslateBlock(TextStart,TextLen);
      else
        raise ESynMustache.CreateUTF8('Kind=% not implemented yet',
          [KindToText(fTags[TagStart].Kind)^]);
      end;
      inc(TagStart);
    end;
  finally
    if (Partials<>nil) and (Partials.fOwned) and not NeverFreePartials then
      Partials.Free;
  end;
end;

function TSynMustache.Render(const Context: variant;
  Partials: TSynMustachePartials; Helpers: TSynMustacheHelpers;
  OnTranslate: TOnStringTranslate; EscapeInvert: boolean): RawUTF8;
var W: TTextWriter;
    Ctxt: TSynMustacheContext;
    tmp: TTextWriterStackBuffer;
begin
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    Ctxt := TSynMustacheContextVariant.Create(self,W,SectionMaxCount,Context);
    try
      Ctxt.Helpers := Helpers;
      Ctxt.OnStringTranslate := OnTranslate;
      Ctxt.EscapeInvert := EscapeInvert;
      RenderContext(Ctxt,0,high(fTags),Partials,false);
      W.SetText(result);
    finally
      Ctxt.Free;
    end;
  finally
    W.Free;
  end;
end;

function TSynMustache.RenderJSON(const JSON: RawUTF8;
  Partials: TSynMustachePartials; Helpers: TSynMustacheHelpers;
  OnTranslate: TOnStringTranslate; EscapeInvert: boolean): RawUTF8;
var context: variant;
begin
  _Json(JSON,context,JSON_OPTIONS[true]);
  result := Render(context,Partials,Helpers,OnTranslate,EscapeInvert);
end;

function TSynMustache.RenderJSON(const JSON: RawUTF8; const Args,
  Params: array of const; Partials: TSynMustachePartials;
  Helpers: TSynMustacheHelpers; OnTranslate: TOnStringTranslate;
  EscapeInvert: boolean): RawUTF8;
var context: variant;
begin
  _Json(FormatUTF8(JSON,Args,Params,true),context,JSON_OPTIONS[true]);
  result := Render(context,Partials,Helpers,OnTranslate,EscapeInvert);
end;

destructor TSynMustache.Destroy;
begin
  FreeAndNil(fInternalPartials);
  inherited;
end;

function TSynMustache.FoundInTemplate(const text: RawUTF8): boolean;
begin // internal partials are part of fTemplate
  result := (self<>nil) and (text<>'') and (PosEx(text,fTemplate)>0);
end;

class procedure TSynMustache.HelperAdd(var Helpers: TSynMustacheHelpers;
  const aName: RawUTF8; aEvent: TSynMustacheHelperEvent);
var n,i: PtrInt;
begin
  n := length(Helpers);
  for i := 0 to n-1 do
    if IdemPropNameU(Helpers[i].Name,aName) then begin
      Helpers[i].Event := aEvent;
      exit;
    end;
  SetLength(Helpers,n+1);
  Helpers[n].Name := aName;
  Helpers[n].Event := aEvent;
end;

class procedure TSynMustache.HelperAdd(var Helpers: TSynMustacheHelpers;
  const aNames: array of RawUTF8; const aEvents: array of TSynMustacheHelperEvent);
var n,i: PtrInt;
begin
  n := length(aNames);
  if n=length(aEvents) then
    for i := 0 to n-1 do
      HelperAdd(Helpers,aNames[i],aEvents[i]);
end;

class procedure TSynMustache.HelperDelete(var Helpers: TSynMustacheHelpers;
  const aName: RawUTF8);
var n,i,j: PtrInt;
begin
  n := length(Helpers);
  for i := 0 to n-1 do
    if IdemPropNameU(Helpers[i].Name,aName) then begin
      for j := i to n-2 do
        Helpers[j] := Helpers[j+1];
      SetLength(Helpers,n-1);
      exit;
    end;
end;

class function TSynMustache.HelperFind(const Helpers: TSynMustacheHelpers;
  aName: PUTF8Char; aNameLen: integer): integer;
begin
  for result := 0 to length(Helpers)-1 do
    if IdemPropNameU(Helpers[result].Name,aName,aNameLen) then
      exit;
  result := -1;
end;

var
  HelpersStandardList: TSynMustacheHelpers;

class function TSynMustache.HelpersGetStandardList: TSynMustacheHelpers;
begin
  if HelpersStandardList=nil then
    HelperAdd(HelpersStandardList,
      ['DateTimeToText','DateToText','DateFmt','TimeLogToText','JSONQuote','JSONQuoteURI',
       'ToJSON','MarkdownToHtml','SimpleToHtml','WikiToHtml','BlobToBase64','EnumTrim',
       'EnumTrimRight','PowerOfTwo','Equals','If','NewGUID','ExtractFileName','Lower','Upper'],
      [DateTimeToText,DateToText,DateFmt,TimeLogToText,JSONQuote,JSONQuoteURI,
       ToJSON,MarkdownToHtml,SimpleToHtml,WikiToHtml,BlobToBase64,EnumTrim,EnumTrimRight,
       PowerOfTwo,Equals_,If_,NewGUID,ExtractFileName,Lower,Upper]);
  result := HelpersStandardList;
end;

class function TSynMustache.HelpersGetStandardList(const aNames: array of RawUTF8;
  const aEvents: array of TSynMustacheHelperEvent): TSynMustacheHelpers;
begin
  result := copy(HelpersGetStandardList); // don't affect global HelpersStandardList
  HelperAdd(result,aNames,aEvents);
end;

class procedure TSynMustache.DateTimeToText(const Value: variant; out result: variant);
var Time: TTimeLogBits;
    dt: TDateTime;
begin
  if VariantToDateTime(Value,dt) then begin
    Time.From(dt,false);
    result := Time.i18nText;
  end else
    SetVariantNull(result);
end;

class procedure TSynMustache.DateToText(const Value: variant; out result: variant);
var Time: TTimeLogBits;
    dt: TDateTime;
begin
  if VariantToDateTime(Value,dt) then begin
    Time.From(dt,true);
    result := Time.i18nText;
  end else
    SetVariantNull(result);
end;

class procedure TSynMustache.DateFmt(const Value: variant; out result: variant);
var dt: TDateTime;
begin // {{DateFmt DateValue,"dd/mm/yyy"}}
  with _Safe(Value)^ do
    if (Kind=dvArray) and (Count=2) and VariantToDateTime(Values[0],dt) then
      result := FormatDateTime(Values[1],dt) else
      SetVariantNull(result);
end;

class procedure TSynMustache.TimeLogToText(const Value: variant; out result: variant);
var Time: TTimeLogBits;
begin
  if VariantToInt64(Value,Time.Value) then
    result := Time.i18nText else
    SetVariantNull(result);
end;

class procedure TSynMustache.ToJSON(const Value: variant; out result: variant);
begin
  if not VarIsEmptyOrNull(Value) then
    RawUTF8ToVariant(JSONReformat(VariantToUTF8(Value)),result);
end;

class procedure TSynMustache.JSONQuote(const Value: variant; out result: variant);
var json: RawUTF8;
begin
  if not VarIsEmptyOrNull(Value) then // avoid to return "null"
    VariantToUTF8(Value,json);
  RawUTF8ToVariant(QuotedStrJSON(json),result);
end;

class procedure TSynMustache.JSONQuoteURI(const Value: variant; out result: variant);
var json: RawUTF8;
begin
  if not VarIsEmptyOrNull(Value) then // avoid to return "null"
    VariantToUTF8(Value,json);
  RawUTF8ToVariant(UrlEncode(QuotedStrJSON(json)),result);
end;

procedure ToHtml(const Value: variant; var result: variant; fmt: TTextWriterHTMLEscape;
  wiki: boolean=false);
var txt: RawUTF8;
    d: PDocVariantData;
begin
  d := _Safe(Value); // {{{SimpleToHtml content,browserhasnoemoji,nohtmlescape}}}
  if (dvoIsArray in d^.Options) and (d^.Count>=2) then begin
    if VarIsEmptyOrNull(d^.Values[0]) then
      exit; // don't append 'null' text
    VariantToUTF8(d^.Values[0],txt);
    if not VarIsVoid(d^.Values[1]) then
      exclude(fmt,heEmojiToUTF8);
    if (d^.Count>=3) and not VarIsVoid(d^.Values[2]) then
      exclude(fmt,heHtmlEscape);
  end else // {{{MarkdownToHtml content}}}
    if VarIsEmptyOrNull(Value) then
      exit else
      VariantToUTF8(Value,txt);
  if txt<>'' then
    if wiki then
      txt := HtmlEscapeWiki(txt,fmt) else
      txt := HtmlEscapeMarkdown(txt,fmt);
  RawUTF8ToVariant(txt,result);
end;

class procedure TSynMustache.WikiToHtml(const Value: variant; out result: variant);
begin
  ToHtml(Value,result,[heHtmlEscape,heEmojiToUTF8],{wiki=}true);
end;

class procedure TSynMustache.MarkdownToHtml(const Value: variant; out result: variant);
begin
  ToHtml(Value,result,[heEmojiToUTF8]); // default Markdown is to allow HTML tags
end;

class procedure TSynMustache.SimpleToHtml(const Value: variant; out result: variant);
begin
  ToHtml(Value,result,[heHtmlEscape,heEmojiToUTF8]);
end;

class procedure TSynMustache.BlobToBase64(const Value: variant; out result: variant);
var tmp: RawUTF8;
    wasString: boolean;
begin
  VariantToUTF8(Value,tmp,wasString);
  if wasString and (pointer(tmp)<>nil) then begin
    if PInteger(tmp)^ and $00ffffff=JSON_BASE64_MAGIC then
      delete(tmp,1,3);
    RawUTF8ToVariant(tmp,result);
  end else
    result := Value;
end;

class procedure TSynMustache.EnumTrim(const Value: variant; out result: variant);
var tmp: RawUTF8;
    wasString: boolean;
    short: PUTF8Char;
begin
  VariantToUTF8(Value,tmp,wasString);
  if not wasString then
    exit;
  short := TrimLeftLowerCase(tmp);
  RawUTF8ToVariant(short,StrLen(short),result);
end;

class procedure TSynMustache.EnumTrimRight(const Value: variant; out result: variant);
var tmp: RawUTF8;
    wasString: boolean;
    i,L: integer;
begin
  VariantToUTF8(Value,tmp,wasString);
  if not wasString then
    exit;
  L := length(tmp);
  for i := 1 to L do
    if not (tmp[i] in ['a'..'z']) then begin
      L := i-1;
      break;
    end;
  RawUTF8ToVariant(Pointer(tmp),L,result);
end;

class procedure TSynMustache.PowerOfTwo(const Value: variant; out result: variant);
var V: Int64;
begin
  if TVarData(Value).VType>varNull then
    if VariantToInt64(Value,V) then
      result := Int64(1) shl V;
end;

class procedure TSynMustache.Equals_(const Value: variant; out result: variant);
begin // {{#Equals .,12}}
  with _Safe(Value)^ do
    if (Kind=dvArray) and (Count=2) and
       (SortDynArrayVariantComp(TVarData(Values[0]),TVarData(Values[1]),false)=0) then
      result := true else
      SetVariantNull(result);
end;

class procedure TSynMustache.If_(const Value: variant; out result: variant);
var cmp: integer;
    oper: RawUTF8;
    wasString: boolean;
begin // {{#if .<>""}} or {{#if .,"=",123}}
  SetVariantNull(result);
  with _Safe(Value)^ do
    if (Kind=dvArray) and (Count=3) then begin
      VariantToUTF8(Values[1],oper,wasString);
      if wasString and (oper<>'') then begin
        cmp := SortDynArrayVariantComp(TVarData(Values[0]),TVarData(Values[2]),false);
        case PWord(oper)^ of
        ord('='): if cmp=0 then result := True;
        ord('>'): if cmp>0 then result := True;
        ord('<'): if cmp<0 then result := True;
        ord('>')+ord('=')shl 8: if cmp>=0 then result := True;
        ord('<')+ord('=')shl 8: if cmp<=0 then result := True;
        ord('<')+ord('>')shl 8: if cmp<>0 then result := True;
        end;
      end;
    end;
end;

class procedure TSynMustache.NewGUID(const Value: variant; out result: variant);
var g: TGUID;
begin
  CreateGUID(g);
  RawUTF8ToVariant(GUIDToRawUTF8(g),result);
end;

class procedure TSynMustache.ExtractFileName(const Value: variant; out result: variant);
begin
  result := SysUtils.ExtractFileName(Value);
end;

class procedure TSynMustache.Lower(const Value: variant; out result: variant);
begin
  result := SysUtils.LowerCase(Value);
end;

class procedure TSynMustache.Upper(const Value: variant; out result: variant);
begin
  result := SysUtils.UpperCase(Value);
end;


{ TSynMustacheContext }

constructor TSynMustacheContext.Create(Owner: TSynMustache; WR: TTextWriter);
begin
  fOwner := Owner;
  fWriter := WR;
end;

procedure TSynMustacheContext.TranslateBlock(Text: PUTF8Char; TextLen: Integer);
var s: string;
begin
  if Assigned(OnStringTranslate) then begin
    UTF8DecodeToString(Text,TextLen,s);
    OnStringTranslate(s);
    fWriter.AddNoJSONEscapeString(s);
  end else
    fWriter.AddNoJSONEscape(Text,TextLen);
end;


{ TSynMustacheContextVariant }

constructor TSynMustacheContextVariant.Create(Owner: TSynMustache; WR: TTextWriter;
  SectionMaxCount: integer; const aDocument: variant);
begin
  inherited Create(Owner,WR);
  SetLength(fContext,SectionMaxCount+1);
  PushContext(TVarData(aDocument)); // weak copy
end;

function TSynMustacheContextVariant.GetDocumentType(
  const aDoc: TVarData): TSynInvokeableVariantType;
begin
  result := nil;
  if aDoc.VType<=varAny then
    exit;
  if (fContextCount>0) and (fContext[0].DocumentType<>nil) and
     (aDoc.VType=fContext[0].DocumentType.VarType) then
    result := fContext[0].DocumentType else
    if not (FindCustomVariantType(aDoc.VType,TCustomVariantType(result)) and
       result.InheritsFrom(TSynInvokeableVariantType)) then
      result := nil;
end;

procedure TSynMustacheContextVariant.PushContext(aDoc: TVarData);
begin
  if fContextCount>=length(fContext) then
    SetLength(fContext,fContextCount+32); // roughtly set by SectionMaxCount
  with fContext[fContextCount] do begin
    Document := aDoc;
    DocumentType := GetDocumentType(aDoc);
    ListCurrent := -1;
    if DocumentType=nil then
      ListCount := -1 else
      ListCount := DocumentType.IterateCount(aDoc);
  end;
  inc(fContextCount);
end;

procedure TSynMustacheContextVariant.PopContext;
begin
  if fContextCount>1 then
    dec(fContextCount);
end;

function TSynMustacheContextVariant.GetValueCopyFromContext(
  const ValueName: RawUTF8): variant;
var tmp: TVarData;
begin
  if (ValueName='') or (ValueName[1] in ['0'..'9','"','{','[']) or
     (ValueName='true') or (ValueName='false') or (ValueName='null') then
    VariantLoadJSON(result,ValueName,@JSON_OPTIONS[true]) else begin
    GetValueFromContext(ValueName,tmp);
    SetVariantByValue(variant(tmp),result); // copy value
  end;
end;

function TSynMustacheContextVariant.GetValueFromContext(
  const ValueName: RawUTF8; var Value: TVarData): TSynMustacheSectionType;
var i,space,helper: Integer;

  procedure ProcessHelper;
  var valnam: RawUTF8;
      val: TVarData;
      valArr: TDocVariantData absolute val;
      valFree: boolean;
      names: TRawUTF8DynArray;
      res: PVarData;
      j,k,n: integer;
  begin
    valnam := Copy(ValueName,space+1,maxInt);
    valFree := false;
    if valnam<>'' then begin
      if valnam='.' then
        GetValueFromContext(valnam,val) else
      if ((valnam<>'') and (valnam[1] in ['1'..'9','"','{','['])) or
         (valnam='true') or (valnam='false') or (valnam='null') then begin
        // {{helper 123}} or {{helper "constant"}} or {{helper [1,2,3]}}
        val.VType := varEmpty;
        VariantLoadJson(variant(val),pointer(valnam),nil,@JSON_OPTIONS[true]);
        valFree := true;
      end else begin
        for j := 1 to length(valnam) do
          case valnam[j] of
          ' ': break; // allows {{helper1 helper2 value}} recursive calls
          ',': begin  // {{helper value,123,"constant"}}
            CSVToRawUTF8DynArray(Pointer(valnam),names,',',true); // TODO: handle 123,"a,b,c"
            valArr.InitFast;
            for k := 0 to High(names) do
              valArr.AddItem(GetValueCopyFromContext(names[k]));
            valFree := true;
            break;
          end;
          '<','>','=': begin // {{#if .=123}} -> {{#if .,"=",123}}
            k := j+1;
            if valnam[k] in ['=','>'] then
              inc(k);
            valArr.InitArray([GetValueCopyFromContext(Copy(valnam,1,j-1)),
              Copy(valnam,j,k-j),GetValueCopyFromContext(Copy(valnam,k,maxInt))],JSON_OPTIONS[true]);
            valFree := true;
            break;
          end;
          end;
        if not valFree then
          GetValueFromContext(valnam,val);
      end;
    end;
    n := fContextCount+4;
    if length(fTempGetValueFromContextHelper)<n then
      SetLength(fTempGetValueFromContextHelper,n);
    res := @fTempGetValueFromContextHelper[fContextCount-1];
    Helpers[helper].Event(variant(val),variant(res^));
    Value := res^;
    if valFree then
      VarClear(variant(val));
    result := msSinglePseudo;
  end;

begin
  result := msNothing;
  if ValueName='.' then
    with fContext[fContextCount-1] do begin
      if ListCount>0 then
        Value := ListCurrentDocument else
        Value := Document;
      exit;
    end;
  space := PosExChar(' ',ValueName);
  if space>1 then begin // {{helper value}}
    helper := TSynMustache.HelperFind(Helpers,pointer(ValueName),space-1);
    if helper>=0 then begin
      ProcessHelper;
      exit;
    end; // if helper not found, will return the unprocessed value
  end;
  for i := fContextCount-1 downto 0 do // recursive search of {{value}}
    with fContext[i] do
      if DocumentType<>nil then
        if ListCount<0 then begin // single item context
          DocumentType.Lookup(Value,Document,pointer(ValueName));
          if Value.VType>=varNull then
            exit;
        end else
        if IdemPChar(pointer(ValueName),'-INDEX') then begin // {{-index}}
          Value.VType := varInteger;
          if ValueName[7]='0' then
            Value.VInteger := ListCurrent else
            Value.VInteger := ListCurrent+1;
          exit;
        end else
        if (ListCurrent<ListCount) and (ListCurrentDocumentType<>nil) then begin
          ListCurrentDocumentType.Lookup(Value,ListCurrentDocument,pointer(ValueName));
          if Value.VType>=varNull then
            exit;
        end;
  if space=0 then begin
    space := length(ValueName); // {{helper}}
    helper := TSynMustache.HelperFind(Helpers,pointer(ValueName),space);
    if helper>=0 then
      ProcessHelper;
  end;
end;

procedure TSynMustacheContextVariant.AppendValue(const ValueName: RawUTF8;
  UnEscape: boolean);
var Value: TVarData;
begin
  GetValueFromContext(ValueName,Value);
  AppendVariant(variant(Value),UnEscape);
end;

procedure TSynMustacheContextVariant.AppendVariant(const Value: variant;
  UnEscape: boolean);
var ValueText: RawUTF8;
    wasString: boolean;
begin
  if TVarData(Value).VType>varNull then
    if VarIsNumeric(Value) then // avoid RawUTF8 conversion for plain numbers
      fWriter.AddVariant(Value,twNone) else begin
      if fEscapeInvert then
        UnEscape := not UnEscape;
      VariantToUTF8(Value,ValueText,wasString);
      if UnEscape then
        fWriter.AddNoJSONEscape(pointer(ValueText),length(ValueText)) else
        fWriter.AddHtmlEscape(pointer(ValueText));
    end;
end;

function TSynMustacheContextVariant.AppendSection(
  const ValueName: RawUTF8): TSynMustacheSectionType;
var Value: TVarData;
begin
  result := msNothing;
  if (fContextCount>0) and (ValueName[1]='-') then
    with fContext[fContextCount-1] do
      if ListCount>=0 then begin
        if ((ValueName='-first') and (ListCurrent=0)) or
           ((ValueName='-last') and (ListCurrent=ListCount-1)) or
           ((ValueName='-odd') and (ListCurrent and 1=0)) then
          result := msSinglePseudo;
        exit;
      end;
  result := GetValueFromContext(ValueName,Value);
  if result<>msNothing then begin
    if (Value.VType<=varNull) or
       ((Value.VType=varBoolean) and not Value.VBoolean) then
      result := msNothing;
    exit;
  end;
  PushContext(Value);
  if (Value.VType<=varNull) or
     ((Value.VType=varBoolean) and not Value.VBoolean) then
    exit; // null or false value will not display the section
  with fContext[fContextCount-1] do
      if ListCount<0 then
        result := msSingle else // single item
        if ListCount=0 then     // empty list will not display the section
          exit else
          result := msList;     // non-empty list
end;

function TSynMustacheContextVariant.GotoNextListItem: boolean;
begin
  result := false;
  if fContextCount>0 then
    with fContext[fContextCount-1] do begin
      ListCurrentDocument.VType := varEmpty;
      ListCurrentDocumentType := nil;
      inc(ListCurrent);
      if ListCurrent>=ListCount then
        exit;
      DocumentType.Iterate(ListCurrentDocument,Document,ListCurrent);
      ListCurrentDocumentType := GetDocumentType(ListCurrentDocument);
      result := true;
    end;
end;


{ TSynMustachePartials }

constructor TSynMustachePartials.Create;
begin
  fList := TRawUTF8List.Create([fNoDuplicate,fCaseSensitive]);
end;

constructor TSynMustachePartials.CreateOwned(const NameTemplatePairs: array of RawUTF8);
var A: integer;
begin
  Create;
  fOwned := true;
  for A := 0 to high(NameTemplatePairs) div 2 do
    Add(NameTemplatePairs[A*2],NameTemplatePairs[A*2+1]);
end;

function TSynMustachePartials.Add(const aName, aTemplate: RawUTF8): TSynMustache;
begin
  result := TSynMustache.Parse(aTemplate);
  if (result<>nil) and (fList.AddObject(aName,result)<0) then
    raise ESynMustache.CreateUTF8('%.Add(%) duplicated name',[self,aName]);
end;

function TSynMustachePartials.Add(const aName: RawUTF8;
  aTemplateStart, aTemplateEnd: PUTF8Char): TSynMustache;
var aTemplate: RawUTF8;
begin
  FastSetString(aTemplate,aTemplateStart,aTemplateEnd-aTemplateStart);
  result := Add(aName,aTemplate);
end;

function TSynMustachePartials.FoundInTemplate(const text: RawUTF8): PtrInt;
begin
  if self<>nil then
    result := fList.Contains(text) else
    result := -1;
end;

class function TSynMustachePartials.CreateOwned(const Partials: variant): TSynMustachePartials;
var p: integer;
begin
  result := nil;
  if DocVariantType.IsOfType(Partials) then
  with TDocVariantData(partials) do
    if (Kind=dvObject) and (Count>0) then begin
      result := TSynMustachePartials.Create;
      result.fOwned := true;
      for p := 0 to Count-1 do
        result.Add(Names[p],VariantToUTF8(Values[p]));
    end;
end;

destructor TSynMustachePartials.Destroy;
begin
  FreeAndNil(fList);
  inherited;
end;

function TSynMustachePartials.GetPartial(const PartialName: RawUTF8): TSynMustache;
var i: integer;
begin
  if self=nil then begin
    result := nil;
    exit;
  end;
  i := fList.IndexOf(PartialName); // using O(1) hashing
  if i<0 then
    result := nil else
    result := TSynMustache(fList.Objects[i]);
end;

end.
