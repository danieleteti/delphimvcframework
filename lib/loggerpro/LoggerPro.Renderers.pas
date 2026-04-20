// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2026 Daniele Teti
//
// https://github.com/danieleteti/loggerpro
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

unit LoggerPro.Renderers;

interface

uses
  LoggerPro, LoggerPro.AnsiColors, System.SysUtils;

type
  TLogItemRendererDefault = class(TLogItemRenderer)
  private
    fInternalLogFormat: string;
    fInternalFormatSettings: TFormatSettings;
    fInitialized: Boolean;
  protected
    property InternalLogFormat: String read fInternalLogFormat;
    property InternalFormatSettings: TFormatSettings read fInternalFormatSettings;
    function GetLogLayoutWithPlaceHolders: String; virtual;
    function RenderContext(const aLogItem: TLogItem): String; virtual;
    // ILogLayoutRenderer
    procedure Setup; override;
    procedure TearDown; override;
    function RenderLogItem(const aLogItem: TLogItem): String; override;
  end;

  TLogItemRendererNoTag = class(TLogItemRendererDefault)
  protected
    function GetLogLayoutWithPlaceHolders: String; override;
    function RenderLogItem(const aLogItem: TLogItem): String; override;
  end;

  TLogItemRendererNoThreadID = class(TLogItemRendererDefault)
  protected
    function GetLogLayoutWithPlaceHolders: String; override;
    function RenderLogItem(const aLogItem: TLogItem): String; override;
  end;

  TLogItemRendererNoTagNoThreadID = class(TLogItemRendererDefault)
  protected
    function GetLogLayoutWithPlaceHolders: String; override;
    function RenderLogItem(const aLogItem: TLogItem): String; override;
  end;

  TLogItemRendererLogFmt = class(TLogItemRenderer)
  private
    fFormatSettings: TFormatSettings;
  protected
    // ILogLayoutRenderer
    procedure Setup; override;
    procedure TearDown; override;
    function RenderLogItem(const aLogItem: TLogItem): String; override;
  end;

  // Rich-colored renderer for console output.
  // Fixed layout: <timestamp> [TID <threadid>] <level> <message> [<tag>] <context>.
  // Every field is wrapped in the scheme's escape codes so a single WriteLn
  // lands a multi-color log line in the terminal. On pipe/redirect, pass
  // LogColorSchemes.Monochrome at construction time to emit plain text.
  TLogItemRendererRichColored = class(TLogItemRenderer)
  private
    fFormatSettings: TFormatSettings;
    fScheme: TLogColorScheme;
  protected
    procedure Setup; override;
    procedure TearDown; override;
    function RenderLogItem(const aLogItem: TLogItem): String; override;
    function RenderColoredContext(const aLogItem: TLogItem): string;
  public
    constructor Create; overload;
    constructor Create(const aScheme: TLogColorScheme); overload;
    property Scheme: TLogColorScheme read fScheme write fScheme;
  end;

  // Gin-inspired renderer. Layout mimics Gin's access log aesthetic:
  //
  //   [LPRO] 2026/04/18 - 17:33:42 | INFO  | connection established | DB | key=val
  //
  // Bracketed prefix, slash-separated date with an em-dash before time,
  // pipe separators between fields, padded level word (badge-friendly),
  // and inline key=val context. Pairs well with LogColorSchemes.GinBadge for
  // colored-background level badges that match Gin's status-code look.
  TLogItemRendererGinStyle = class(TLogItemRenderer)
  private
    fFormatSettings: TFormatSettings;
    fScheme: TLogColorScheme;
    fPrefix: string;
  protected
    procedure Setup; override;
    procedure TearDown; override;
    function RenderLogItem(const aLogItem: TLogItem): String; override;
    function RenderContext(const aLogItem: TLogItem): string;
  public
    { Default is empty - no prefix is emitted and the line starts with the
      date. Set to a short app/module name ("MYAPP", "BILLING") when the
      same stream aggregates logs from multiple processes and you need a
      visual source marker (Gin's [GIN] pattern). }
    const DEFAULT_PREFIX = '';
    constructor Create; overload;
    constructor Create(const aScheme: TLogColorScheme); overload;
    constructor Create(const aScheme: TLogColorScheme; const aPrefix: string); overload;
    property Scheme: TLogColorScheme read fScheme write fScheme;
    property Prefix: string read fPrefix write fPrefix;
  end;


function GetDefaultLogItemRenderer: ILogItemRenderer;

var
  gDefaultLogItemRenderer: TLogItemRendererClass = TLogItemRendererDefault;

implementation

uses
  System.DateUtils,
  System.Rtti,
  System.TypInfo,
  LoggerPro.RendererRegistry;

function GetDefaultLogItemRenderer: ILogItemRenderer;
begin
  Result := gDefaultLogItemRenderer.Create;
end;

{ TLogItemRendererDefault }

procedure TLogItemRendererDefault.TearDown;
begin
  // do nothing
end;

procedure TLogItemRendererDefault.Setup;
begin
  if not fInitialized then
  begin
    fInternalFormatSettings := GetDefaultFormatSettings;
    fInternalLogFormat := LogLayoutByPlaceHoldersToLogLayoutByIndexes(GetLogLayoutWithPlaceHolders, True);
    fInitialized := True;
  end;
end;

function TLogItemRendererDefault.GetLogLayoutWithPlaceHolders: String;
begin
  Result := '{timestamp}[TID {threadid}][{loglevel}] {message} [{tag}]';
end;

function TLogItemRendererDefault.RenderContext(const aLogItem: TLogItem): String;
var
  I: Integer;
  lParam: LogParam;
  lValueStr: string;
begin
  // Use pre-rendered context if available (optimization for fixed context)
  if aLogItem.PreRenderedContext <> '' then
  begin
    Result := aLogItem.PreRenderedContext;
    Exit;
  end;

  Result := '';
  if not aLogItem.HasContext then
    Exit;

  for I := 0 to High(aLogItem.Context) do
  begin
    lParam := aLogItem.Context[I];
    case lParam.Value.Kind of
      tkInteger, tkInt64:
        lValueStr := lParam.Value.AsInt64.ToString;
      tkFloat:
        if lParam.Value.TypeInfo = TypeInfo(TDateTime) then
          lValueStr := DateToISO8601(lParam.Value.AsType<TDateTime>, False)
        else
          lValueStr := FloatToStr(lParam.Value.AsExtended, InternalFormatSettings);
      tkEnumeration:
        if lParam.Value.TypeInfo = TypeInfo(Boolean) then
          lValueStr := BoolToStr(lParam.Value.AsBoolean, True).ToLower
        else
          lValueStr := lParam.Value.ToString;
    else
      lValueStr := lParam.Value.ToString.QuotedString('"');
    end;
    if I = 0 then
      Result := lParam.Key + '=' + lValueStr
    else
      Result := Result + ', ' + lParam.Key + '=' + lValueStr;
  end;
  Result := ' {' + Result + '}';
end;

function TLogItemRendererDefault.RenderLogItem(const aLogItem: TLogItem): String;
begin
  Result := Format(InternalLogFormat, [
    DateTimeToStr(ALogItem.TimeStamp, InternalFormatSettings),
    ALogItem.ThreadID.ToString,
    ALogItem.LogTypeAsString,
    ALogItem.LogMessage,
    ALogItem.LogTag
  ]) + RenderContext(aLogItem);
end;

{ TLogItemRendererNoTag }

function TLogItemRendererNoTag.GetLogLayoutWithPlaceHolders: String;
begin
  Result := '{timestamp}[TID {threadid}][{loglevel}] {message}';
end;

function TLogItemRendererNoTag.RenderLogItem(const aLogItem: TLogItem): String;
begin
  Result := Format(InternalLogFormat, [
    DateTimeToStr(ALogItem.TimeStamp, InternalFormatSettings),
    ALogItem.ThreadID.ToString,
    ALogItem.LogTypeAsString,
    ALogItem.LogMessage
  ]) + RenderContext(aLogItem);
end;

{ TLogItemRendererNoThreadID }

function TLogItemRendererNoThreadID.GetLogLayoutWithPlaceHolders: String;
begin
  Result := '{timestamp}[{loglevel}] {message} [{tag}]';
end;

function TLogItemRendererNoThreadID.RenderLogItem(const aLogItem: TLogItem): String;
begin
  Result := Format(InternalLogFormat, [
    DateTimeToStr(ALogItem.TimeStamp, InternalFormatSettings),
    ALogItem.LogTypeAsString,
    ALogItem.LogMessage,
    aLogItem.LogTag
  ]) + RenderContext(aLogItem);
end;

{ TLogItemRendererNoTagNoThreadID }

function TLogItemRendererNoTagNoThreadID.GetLogLayoutWithPlaceHolders: String;
begin
  Result := '{timestamp}[{loglevel}] {message}';
end;

function TLogItemRendererNoTagNoThreadID.RenderLogItem(const aLogItem: TLogItem): String;
begin
  Result := Format(InternalLogFormat, [
    DateTimeToStr(ALogItem.TimeStamp, InternalFormatSettings),
    ALogItem.LogTypeAsString,
    ALogItem.LogMessage
  ]) + RenderContext(aLogItem);
end;



{ TLogItemRendererLogFmt }

// LogFmt spec (Brandur/Heroku): key=value pairs separated by single spaces.
// Keys: [a-zA-Z0-9_.-]. Values: bare if no space/quote/= and not empty,
// otherwise "..." with \" and \\ escapes; control chars become \n \r \t.
function LogFmtSanitizeKey(const aKey: string): string;
var
  I: Integer;
  C: Char;
begin
  if aKey = '' then
    Exit('_');
  SetLength(Result, Length(aKey));
  for I := 1 to Length(aKey) do
  begin
    C := aKey[I];
    if CharInSet(C, ['a'..'z', 'A'..'Z', '0'..'9', '_', '.', '-']) then
      Result[I] := C
    else
      Result[I] := '_';
  end;
end;

function LogFmtNeedsQuoting(const aValue: string): Boolean;
var
  I: Integer;
  C: Char;
begin
  if aValue = '' then
    Exit(True);
  for I := 1 to Length(aValue) do
  begin
    C := aValue[I];
    if (C = ' ') or (C = '"') or (C = '=') or (C = '\') or (Ord(C) < 32) then
      Exit(True);
  end;
  Result := False;
end;

function LogFmtEscapeValue(const aValue: string): string;
var
  I: Integer;
  C: Char;
  lSB: TStringBuilder;
begin
  if not LogFmtNeedsQuoting(aValue) then
    Exit(aValue);
  lSB := TStringBuilder.Create(Length(aValue) + 8);
  try
    lSB.Append('"');
    for I := 1 to Length(aValue) do
    begin
      C := aValue[I];
      case C of
        '"': lSB.Append('\"');
        '\': lSB.Append('\\');
        #10: lSB.Append('\n');
        #13: lSB.Append('\r');
        #9:  lSB.Append('\t');
      else
        if Ord(C) < 32 then
          lSB.Append('\u').Append(IntToHex(Ord(C), 4))
        else
          lSB.Append(C);
      end;
    end;
    lSB.Append('"');
    Result := lSB.ToString;
  finally
    lSB.Free;
  end;
end;

function LogFmtRenderValue(const aValue: TValue;
  const aFormatSettings: TFormatSettings): string;
begin
  case aValue.Kind of
    tkInteger, tkInt64:
      Result := aValue.AsInt64.ToString;
    tkFloat:
      if aValue.TypeInfo = TypeInfo(TDateTime) then
        Result := DateToISO8601(aValue.AsType<TDateTime>, False)
      else
        Result := FloatToStr(aValue.AsExtended, aFormatSettings);
    tkEnumeration:
      if aValue.TypeInfo = TypeInfo(Boolean) then
        Result := BoolToStr(aValue.AsBoolean, True).ToLower
      else
        Result := LogFmtEscapeValue(aValue.ToString);
  else
    Result := LogFmtEscapeValue(aValue.ToString);
  end;
end;

function TLogItemRendererLogFmt.RenderLogItem(const aLogItem: TLogItem): String;
var
  I: Integer;
  lParam: LogParam;
  lSB: TStringBuilder;
begin
  lSB := TStringBuilder.Create;
  try
    lSB.Append('time=').Append(DateToISO8601(aLogItem.TimeStamp, False));
    lSB.Append(' threadid=').Append(aLogItem.ThreadID);
    lSB.Append(' type=').Append(aLogItem.LogTypeAsString);
    lSB.Append(' msg=').Append(LogFmtEscapeValue(aLogItem.LogMessage));
    lSB.Append(' tag=').Append(LogFmtEscapeValue(aLogItem.LogTag));

    if aLogItem.HasContext then
    begin
      for I := 0 to High(aLogItem.Context) do
      begin
        lParam := aLogItem.Context[I];
        lSB.Append(' ');
        lSB.Append(LogFmtSanitizeKey(lParam.Key));
        lSB.Append('=');
        lSB.Append(LogFmtRenderValue(lParam.Value, fFormatSettings));
      end;
    end;
    Result := lSB.ToString;
  finally
    lSB.Free;
  end;
end;

procedure TLogItemRendererLogFmt.Setup;
begin
  inherited;
  // Use invariant settings so floats always render with '.' decimal separator,
  // independent of OS locale. This is required by the logfmt contract.
  fFormatSettings := TFormatSettings.Invariant;
end;

procedure TLogItemRendererLogFmt.TearDown;
begin
  inherited;

end;

{ TLogItemRendererRichColored }

constructor TLogItemRendererRichColored.Create;
begin
  inherited Create;
  fScheme := LogColorSchemes.Default;
end;

constructor TLogItemRendererRichColored.Create(const aScheme: TLogColorScheme);
begin
  inherited Create;
  fScheme := aScheme;
end;

procedure TLogItemRendererRichColored.Setup;
begin
  inherited;
  fFormatSettings := GetDefaultFormatSettings;
end;

procedure TLogItemRendererRichColored.TearDown;
begin
  inherited;
end;

function TLogItemRendererRichColored.RenderColoredContext(const aLogItem: TLogItem): string;
var
  I: Integer;
  lParam: LogParam;
  lValueStr: string;
  lPairs: string;
begin
  Result := '';
  if not aLogItem.HasContext then
  begin
    if aLogItem.PreRenderedContext <> '' then
      Result := ' ' + aLogItem.PreRenderedContext;
    Exit;
  end;

  lPairs := '';
  for I := 0 to High(aLogItem.Context) do
  begin
    lParam := aLogItem.Context[I];
    case lParam.Value.Kind of
      tkInteger, tkInt64:
        lValueStr := lParam.Value.AsInt64.ToString;
      tkFloat:
        if lParam.Value.TypeInfo = TypeInfo(TDateTime) then
          lValueStr := DateToISO8601(lParam.Value.AsType<TDateTime>, False)
        else
          lValueStr := FloatToStr(lParam.Value.AsExtended, fFormatSettings);
      tkEnumeration:
        if lParam.Value.TypeInfo = TypeInfo(Boolean) then
          lValueStr := BoolToStr(lParam.Value.AsBoolean, True).ToLower
        else
          lValueStr := lParam.Value.ToString;
    else
      lValueStr := lParam.Value.ToString;
    end;

    if I > 0 then
      lPairs := lPairs + ', ';
    lPairs := lPairs +
      Colorize(lParam.Key, fScheme.ContextKeyColor) +
      '=' +
      Colorize(lValueStr, fScheme.ContextValueColor);
  end;
  Result := ' {' + lPairs + '}';
end;

function TLogItemRendererRichColored.RenderLogItem(const aLogItem: TLogItem): String;
var
  lTimestamp, lThreadID, lLevel, lMessage, lTag, lEndGuard: string;
begin
  lTimestamp := Colorize(
    FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', aLogItem.TimeStamp, fFormatSettings),
    fScheme.TimestampColor);

  lThreadID := Colorize('[TID ' + aLogItem.ThreadID.ToString + ']', fScheme.ThreadIDColor);

  lLevel := Colorize(Format('%-7s', [aLogItem.LogTypeAsString]),
    fScheme.LevelColor[aLogItem.LogType]);

  lMessage := Colorize(aLogItem.LogMessage, fScheme.MessageColor);

  if aLogItem.LogTag <> '' then
    lTag := ' ' + Colorize('[' + aLogItem.LogTag + ']', fScheme.TagColor)
  else
    lTag := '';

  // Colorama-style end-of-line autoreset: even if any individual field's
  // color string ended up open, this guarantees the terminal returns to
  // default state before the next log line (or any subsequent Writeln).
  // Skipped when the scheme is monochrome to keep piped output ANSI-free.
  if (fScheme.TimestampColor <> '') or (fScheme.ThreadIDColor <> '') or
     (fScheme.TagColor <> '') or (fScheme.MessageColor <> '') or
     (fScheme.ContextKeyColor <> '') or (fScheme.ContextValueColor <> '') or
     (fScheme.LevelColor[aLogItem.LogType] <> '') then
    lEndGuard := STYLE_RESETALL
  else
    lEndGuard := '';

  Result := lTimestamp + ' ' + lThreadID + ' ' + lLevel + ' ' + lMessage + lTag +
    RenderColoredContext(aLogItem) + lEndGuard;
end;

{ TLogItemRendererGinStyle }

constructor TLogItemRendererGinStyle.Create;
begin
  inherited Create;
  fScheme := LogColorSchemes.Default;
  fPrefix := DEFAULT_PREFIX;
end;

constructor TLogItemRendererGinStyle.Create(const aScheme: TLogColorScheme);
begin
  inherited Create;
  fScheme := aScheme;
  fPrefix := DEFAULT_PREFIX;
end;

constructor TLogItemRendererGinStyle.Create(const aScheme: TLogColorScheme;
  const aPrefix: string);
begin
  inherited Create;
  fScheme := aScheme;
  // aPrefix = '' means "no prefix" (line starts with the date).
  fPrefix := aPrefix;
end;

procedure TLogItemRendererGinStyle.Setup;
begin
  inherited;
  fFormatSettings := GetDefaultFormatSettings;
  // Gin uses a slash-separated date regardless of OS locale. Force it so
  // the output looks consistent across Windows/Linux/Italian/English
  // systems where the OS default date separator may be '-' or '.'.
  fFormatSettings.DateSeparator := '/';
end;

procedure TLogItemRendererGinStyle.TearDown;
begin
  inherited;
end;

function TLogItemRendererGinStyle.RenderContext(const aLogItem: TLogItem): string;
var
  I: Integer;
  lParam: LogParam;
  lValueStr: string;
  lPairs: string;
begin
  Result := '';
  if not aLogItem.HasContext then
    Exit;

  lPairs := '';
  for I := 0 to High(aLogItem.Context) do
  begin
    lParam := aLogItem.Context[I];
    case lParam.Value.Kind of
      tkInteger, tkInt64:
        lValueStr := lParam.Value.AsInt64.ToString;
      tkFloat:
        if lParam.Value.TypeInfo = TypeInfo(TDateTime) then
          lValueStr := DateToISO8601(lParam.Value.AsType<TDateTime>, False)
        else
          lValueStr := FloatToStr(lParam.Value.AsExtended, fFormatSettings);
      tkEnumeration:
        if lParam.Value.TypeInfo = TypeInfo(Boolean) then
          lValueStr := BoolToStr(lParam.Value.AsBoolean, True).ToLower
        else
          lValueStr := lParam.Value.ToString;
    else
      lValueStr := lParam.Value.ToString;
    end;
    if I > 0 then
      lPairs := lPairs + ' ';
    lPairs := lPairs +
      Colorize(lParam.Key, fScheme.ContextKeyColor) +
      '=' +
      Colorize(lValueStr, fScheme.ContextValueColor);
  end;
  Result := lPairs;
end;

function TLogItemRendererGinStyle.RenderLogItem(const aLogItem: TLogItem): String;
var
  lPrefix, lDate, lSep, lLevel, lMessage, lTag, lContext, lEndGuard: string;
begin
  // Empty prefix -> line starts with the date. Keeps common layout
  // compact and avoids redundant [LPRO] on every row.
  if fPrefix <> '' then
    lPrefix := Colorize('[' + fPrefix + ']', fScheme.PrefixColor) + ' '
  else
    lPrefix := '';

  lDate := Colorize(
    FormatDateTime('yyyy/mm/dd - hh:nn:ss', aLogItem.TimeStamp, fFormatSettings),
    fScheme.TimestampColor);

  lSep := Colorize('|', fScheme.SeparatorColor);

  // Pad with spaces on both sides so the level slot becomes a fixed-width
  // badge - gives colored-background schemes a proper "pill" look.
  lLevel := Colorize(' ' + Format('%-7s', [aLogItem.LogTypeAsString]) + ' ',
    fScheme.LevelColor[aLogItem.LogType]);

  lMessage := Colorize(aLogItem.LogMessage, fScheme.MessageColor);

  if aLogItem.LogTag <> '' then
    lTag := ' ' + lSep + ' ' + Colorize(aLogItem.LogTag, fScheme.TagColor)
  else
    lTag := '';

  lContext := RenderContext(aLogItem);
  if lContext <> '' then
    lContext := ' ' + lSep + ' ' + lContext;

  // Colorama-style line-end autoreset. Skipped when the scheme is
  // monochrome so piped output remains ANSI-free.
  if (fScheme.PrefixColor <> '') or (fScheme.TimestampColor <> '') or
     (fScheme.ThreadIDColor <> '') or (fScheme.TagColor <> '') or
     (fScheme.MessageColor <> '') or (fScheme.SeparatorColor <> '') or
     (fScheme.ContextKeyColor <> '') or (fScheme.ContextValueColor <> '') or
     (fScheme.LevelColor[aLogItem.LogType] <> '') then
    lEndGuard := STYLE_RESETALL
  else
    lEndGuard := '';

  Result := lPrefix + lDate + ' ' + lSep + lLevel + lSep + ' ' +
            lMessage + lTag + lContext + lEndGuard;
end;

initialization
  // Self-register the built-in renderers so they can be selected from JSON
  // configuration files via the "renderer" field on the Console appender.
  // Applications or libraries shipping additional renderers follow the same
  // pattern in their own unit's initialization section.
  RegisterRenderer('Default',         TLogItemRendererDefault);
  RegisterRenderer('NoTag',           TLogItemRendererNoTag);
  RegisterRenderer('NoThreadID',      TLogItemRendererNoThreadID);
  RegisterRenderer('NoTagNoThreadID', TLogItemRendererNoTagNoThreadID);
  RegisterRenderer('LogFmt',          TLogItemRendererLogFmt);
  RegisterRenderer('RichColored',     TLogItemRendererRichColored);
  RegisterRenderer('GinStyle',        TLogItemRendererGinStyle);

end.
