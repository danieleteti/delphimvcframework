// ***************************************************************************
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

unit LoggerPro.Config;

(*
  File-based LoggerPro configuration.

  Build a fully-configured ILogWriter from a JSON file or JSON string.
  Zero external dependencies: uses System.JSON shipped with Delphi.

  Usage:
    Log := TLoggerProConfig.FromJSONFile('loggerpro.json');

  Example JSON:
    [
      configVersion: 1,
      minimumLevel:  "Info",
      defaultTag:    "myapp",
      appenders: [
        { type: "Console",  colors: true, colorScheme: "Midnight" },
        { type: "File",     logsFolder: "logs", maxBackupFiles: 5 },
        { type: "HTMLFile", fileName: "logs/app.html", title: "My App" }
      ]
    ]
  (Actual JSON uses curly braces at root - this doc sketch uses square
  brackets to avoid clashing with Pascal's nested-comment rules.)

  Versioning:
    "configVersion" is an integer at the root identifying the schema
    version the file was written against. Current: 1.
    - Missing    -> assume latest (LOGGERPRO_CONFIG_VERSION).
    - Higher     -> raise ELoggerProConfigError (newer than supported).
    - Lower/eq   -> parse with backward-compatible logic.
    Bump only on BREAKING schema changes; additive fields keep v1.

  Extensibility: register custom appender types with
    TLoggerProConfig.RegisterAppenderType('MyType', MyFactoryProc);
*)

interface

uses
  System.JSON,
  System.SysUtils,
  LoggerPro,
  LoggerPro.Builder;

const
  /// <summary>Schema version understood by this LoggerPro build.
  /// Bumped whenever the JSON schema gains a backward-incompatible
  /// field name, required field, or enum value. Minor additions that
  /// remain backward-compatible do NOT bump this version.
  ///
  /// A config file may carry a top-level
  ///   "configVersion": 1
  /// field. When missing, the parser assumes this constant (latest known).
  /// When present and higher than this constant, the parser raises
  /// ELoggerProConfigError so the user knows their file was written for
  /// a newer LoggerPro than the one loading it.</summary>
  LOGGERPRO_CONFIG_VERSION = 1;

type
  // ELoggerProConfigError is declared in LoggerPro.pas (re-exposed here
  // through the `uses LoggerPro` clause above) so that callers can stay
  // on a single `uses LoggerPro;` import. See its definition for docs.

  /// <summary>Factory signature for a single appender type. Receives the
  /// builder and the JSON object for this appender entry, must call the
  /// appropriate builder.WriteToXxx(...), configure it from JSON fields,
  /// and call .Done.</summary>
  TLoggerProAppenderFactory = reference to procedure(
    const aBuilder: ILoggerProBuilder;
    const aConfig: TJSONObject);

  TLoggerProConfig = class
  public
    /// <summary>Read the JSON file and return a fully-configured logger.
    /// File is read as UTF-8. Parent directory must exist; the file itself
    /// must exist (use defaults in code, then override via file as needed).</summary>
    class function FromJSONFile(const aFileName: string): ILogWriter; static;

    /// <summary>Parse the JSON text and return a fully-configured logger.
    /// Useful when the config comes from an embedded resource or a
    /// hand-assembled string.</summary>
    class function FromJSONString(const aJSON: string): ILogWriter; static;

    /// <summary>Read the JSON file and return the underlying builder
    /// without finalizing it. Use this when you need to add appenders
    /// that cannot be expressed in JSON (Callback / Strings / VCL components,
    /// or any pre-existing ILogAppender instance) on top of a file-driven
    /// configuration. Caller is responsible for calling <c>.Build</c>.</summary>
    class function BuilderFromJSONFile(const aFileName: string): ILoggerProBuilder; static;

    /// <summary>Parse the JSON text and return the underlying builder
    /// without finalizing it. See <c>BuilderFromJSONFile</c> for usage.</summary>
    class function BuilderFromJSONString(const aJSON: string): ILoggerProBuilder; static;

    /// <summary>Register a factory for a custom appender type. Type name
    /// is matched case-insensitively in JSON. Registering an existing name
    /// replaces the previous factory.
    /// <para>
    /// <c>aAllowedFields</c> is the closed set of JSON field names this
    /// factory accepts (excluding <c>"type"</c> which is always implicit).
    /// Any unknown field in the JSON raises <c>ELoggerProConfigError</c>
    /// with the typo and the list of valid fields. This catches the common
    /// "silent typo" trap (e.g. <c>"colour"</c> instead of <c>"color"</c>).
    /// </para></summary>
    class procedure RegisterAppenderType(
      const aType: string;
      const aFactory: TLoggerProAppenderFactory;
      const aAllowedFields: TArray<string>); static;

    /// <summary>Parse a log-level string (case-insensitive). Accepted:
    /// Debug, Info, Warn / Warning, Error, Fatal. Raises
    /// <c>ELoggerProConfigError</c> on unknown values. Exposed so
    /// out-of-core appender factories (see
    /// <c>LoggerPro.ExeWatchAppender</c>, <c>LoggerPro.ElasticSearchAppender</c>,
    /// <c>LoggerPro.WindowsEventLogAppender</c>) can share the same
    /// parsing rules instead of hand-rolling their own.</summary>
    class function ParseLogLevel(const aValue: string): TLogType; static;

    /// <summary>Read a string-valued <c>"minimumLevel"</c> field off a JSON
    /// object and parse it. Returns False if the field is missing; raises
    /// <c>ELoggerProConfigError</c> if present but not a recognised
    /// level. See <c>ParseLogLevel</c>.</summary>
    class function TryGetJSONLogLevel(const aObj: TJSONObject; const aName: string; out aLevel: TLogType): Boolean; static;
  end;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  LoggerPro.AnsiColors,
  LoggerPro.ConsoleAppender,
  LoggerPro.FileAppender,
  LoggerPro.HTMLFileAppender,
  LoggerPro.JSONLFileAppender,
  LoggerPro.TimeRotatingFileAppender,
  LoggerPro.WebhookAppender,
  LoggerPro.UDPSyslogAppender,
  LoggerPro.OutputDebugStringAppender,
  LoggerPro.MemoryAppender,
  LoggerPro.FileBySourceAppender,
  LoggerPro.RendererRegistry;

type
  TAppenderTypeInfo = record
    Factory: TLoggerProAppenderFactory;
    AllowedFields: TArray<string>;
  end;

const
  ROOT_FIELDS: array [0 .. 4] of string = (
    'configVersion', 'minimumLevel', 'defaultMinimumLevel', 'defaultTag', 'appenders');

var
  gFactories: TDictionary<string, TAppenderTypeInfo> = nil;

{ ===== small helpers ===== }

function JoinStrings(const aArray: array of string; const aSeparator: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(aArray) do
  begin
    if i > 0 then
      Result := Result + aSeparator;
    Result := Result + aArray[i];
  end;
end;

function ContainsCI(const aArray: array of string; const aValue: string): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(aArray) do
    if SameText(aArray[i], aValue) then
      Exit(True);
  Result := False;
end;

function TryGetString(const aObj: TJSONObject; const aName: string; out aValue: string): Boolean;
var
  lValue: TJSONValue;
begin
  Result := False;
  if aObj = nil then Exit;
  lValue := aObj.GetValue(aName);
  if lValue = nil then Exit;
  if lValue is TJSONString then
  begin
    aValue := TJSONString(lValue).Value;
    Exit(True);
  end;
  if lValue is TJSONNumber then
  begin
    aValue := TJSONNumber(lValue).ToString;
    Exit(True);
  end;
  raise ELoggerProConfigError.CreateFmt(
    'Field "%s" must be a string, got %s.', [aName, lValue.ClassName]);
end;

function TryGetInt(const aObj: TJSONObject; const aName: string; out aValue: Integer): Boolean;
var
  lValue: TJSONValue;
begin
  Result := False;
  if aObj = nil then Exit;
  lValue := aObj.GetValue(aName);
  if lValue = nil then Exit;
  if lValue is TJSONNumber then
  begin
    aValue := TJSONNumber(lValue).AsInt;
    Exit(True);
  end;
  raise ELoggerProConfigError.CreateFmt(
    'Field "%s" must be a number, got %s.', [aName, lValue.ClassName]);
end;

function TryGetBool(const aObj: TJSONObject; const aName: string; out aValue: Boolean): Boolean;
var
  lValue: TJSONValue;
begin
  Result := False;
  if aObj = nil then Exit;
  lValue := aObj.GetValue(aName);
  if lValue = nil then Exit;
  if lValue is TJSONBool then
  begin
    aValue := TJSONBool(lValue).AsBoolean;
    Exit(True);
  end;
  raise ELoggerProConfigError.CreateFmt(
    'Field "%s" must be a boolean, got %s.', [aName, lValue.ClassName]);
end;

const
  VALID_LOG_LEVELS: array [0 .. 4] of string =
    ('Debug', 'Info', 'Warning', 'Error', 'Fatal');

function ParseLogType(const aValue: string): TLogType;
var
  lNorm: string;
begin
  lNorm := Trim(aValue).ToLower;
  if (lNorm = 'debug') then Exit(TLogType.Debug);
  if (lNorm = 'info') or (lNorm = 'normal') then Exit(TLogType.Info);
  if (lNorm = 'warning') or (lNorm = 'warn') then Exit(TLogType.Warning);
  if (lNorm = 'error') then Exit(TLogType.Error);
  if (lNorm = 'fatal') then Exit(TLogType.Fatal);
  raise ELoggerProConfigError.CreateFmt(
    'Invalid log level "%s". Valid values: %s.',
    [aValue, JoinStrings(VALID_LOG_LEVELS, ', ')]);
end;

function TryGetLogLevel(const aObj: TJSONObject; const aName: string; out aLevel: TLogType): Boolean;
var
  lStr: string;
begin
  Result := TryGetString(aObj, aName, lStr);
  if Result then
    aLevel := ParseLogType(lStr);
end;

const
  VALID_COLOR_SCHEMES: array [0 .. 10] of string = (
    'Default', 'Monochrome', 'GinBadge', 'GinMinimal', 'GinVibrant',
    'Midnight', 'Nord', 'Matrix', 'Amber', 'Ocean', 'Cyberpunk');

function ParseColorScheme(const aName: string): TLogColorScheme;
var
  lNorm: string;
begin
  lNorm := Trim(aName).ToLower;
  if (lNorm = '') or (lNorm = 'default') then Exit(LogColorSchemes.Default);
  if (lNorm = 'monochrome') or (lNorm = 'none') then Exit(LogColorSchemes.Monochrome);
  if (lNorm = 'ginbadge') or (lNorm = 'gin-badge') or (lNorm = 'badge') then Exit(LogColorSchemes.GinBadge);
  if (lNorm = 'ginminimal') or (lNorm = 'gin-minimal') or (lNorm = 'minimal') then Exit(LogColorSchemes.GinMinimal);
  if (lNorm = 'ginvibrant') or (lNorm = 'gin-vibrant') or (lNorm = 'vibrant') then Exit(LogColorSchemes.GinVibrant);
  if (lNorm = 'midnight') then Exit(LogColorSchemes.Midnight);
  if (lNorm = 'nord') then Exit(LogColorSchemes.Nord);
  if (lNorm = 'matrix') then Exit(LogColorSchemes.Matrix);
  if (lNorm = 'amber') then Exit(LogColorSchemes.Amber);
  if (lNorm = 'ocean') then Exit(LogColorSchemes.Ocean);
  if (lNorm = 'cyberpunk') then Exit(LogColorSchemes.Cyberpunk);
  raise ELoggerProConfigError.CreateFmt(
    'Unknown color scheme "%s". Valid values: %s.',
    [aName, JoinStrings(VALID_COLOR_SCHEMES, ', ')]);
end;

const
  VALID_ROTATION_INTERVALS: array [0 .. 4] of string =
    ('None', 'Hourly', 'Daily', 'Weekly', 'Monthly');

function ParseRotationInterval(const aValue: string): TTimeRotationInterval;
var
  lNorm: string;
begin
  lNorm := Trim(aValue).ToLower;
  if (lNorm = '') or (lNorm = 'none') then Exit(TTimeRotationInterval.None);
  if (lNorm = 'hourly') then Exit(TTimeRotationInterval.Hourly);
  if (lNorm = 'daily') then Exit(TTimeRotationInterval.Daily);
  if (lNorm = 'weekly') then Exit(TTimeRotationInterval.Weekly);
  if (lNorm = 'monthly') then Exit(TTimeRotationInterval.Monthly);
  raise ELoggerProConfigError.CreateFmt(
    'Invalid rotation interval "%s". Valid values: %s.',
    [aValue, JoinStrings(VALID_ROTATION_INTERVALS, ', ')]);
end;

const
  VALID_API_KEY_LOCATIONS: array [0 .. 1] of string = ('Header', 'QueryString');

function ParseAPIKeyLocation(const aValue: string): TWebhookAPIKeyLocation;
var
  lNorm: string;
begin
  lNorm := Trim(aValue).ToLower;
  if (lNorm = '') or (lNorm = 'header') then Exit(TWebhookAPIKeyLocation.Header);
  if (lNorm = 'querystring') or (lNorm = 'query') or (lNorm = 'querystringparam') then
    Exit(TWebhookAPIKeyLocation.QueryString);
  raise ELoggerProConfigError.CreateFmt(
    'Invalid API key location "%s". Valid values: %s.',
    [aValue, JoinStrings(VALID_API_KEY_LOCATIONS, ', ')]);
end;

const
  VALID_HTTP_CONTENT_TYPES: array [0 .. 1] of string = ('JSON', 'PlainText');

function ParseHTTPContentType(const aValue: string): TWebhookContentType;
var
  lNorm: string;
begin
  lNorm := Trim(aValue).ToLower;
  if (lNorm = '') or (lNorm = 'json') or (lNorm = 'application/json') then Exit(TWebhookContentType.JSON);
  if (lNorm = 'plaintext') or (lNorm = 'plain') or (lNorm = 'text') then Exit(TWebhookContentType.PlainText);
  raise ELoggerProConfigError.CreateFmt(
    'Invalid HTTP content type "%s". Valid values: %s.',
    [aValue, JoinStrings(VALID_HTTP_CONTENT_TYPES, ', ')]);
end;

{ ===== built-in factories ===== }

procedure ConsoleFactory(const aBuilder: ILoggerProBuilder; const aConfig: TJSONObject);
var
  lCfg: IConsoleAppenderConfigurator;
  lLogLevel: TLogType;
  lStr: string;
  lBool: Boolean;
  lColorsField: Boolean;
  lColorsExplicit: Boolean;
  lSchemeValue: TJSONValue;
  lColorsEnabled: Boolean;
  lSchemeName: string;
  lHasSchemeField: Boolean;
  lRendererName: string;
  lRenderer: ILogItemRenderer;
begin
  lCfg := aBuilder.WriteToConsole;
  if TryGetLogLevel(aConfig, 'minimumLevel', lLogLevel) then
    lCfg := lCfg.WithMinimumLevel(lLogLevel);

  // Optional "renderer" field: resolved against the process-wide renderer
  // registry. When set, the named renderer replaces the Console appender's
  // built-in rendering pipeline - color/scheme/prefix fields below are still
  // applied on top but the chosen renderer is what formats each TLogItem.
  // Unknown names raise: silent fallback would mask typos in config files.
  if TryGetString(aConfig, 'renderer', lRendererName) and (lRendererName <> '') then
  begin
    if not TryCreateRenderer(lRendererName, lRenderer) then
      raise ELoggerProConfigError.CreateFmt(
        'Unknown renderer "%s". Registered renderers: %s. ' +
        'Register custom renderers with LoggerPro.RendererRegistry.RegisterRenderer ' +
        'from a unit that is reachable from the program''s uses clause.',
        [lRendererName, JoinStrings(RegisteredRendererNames, ', ')]);
    lCfg := lCfg.WithRenderer(lRenderer);
  end;

  // Resolve the color story. Rules:
  //   - colors ON by default (the "Midnight" scheme); the renderer
  //     auto-degrades to plain text when stdout is piped/redirected
  //     so log files never contain ANSI escapes.
  //   - "colors": false                       -> plain text, no ANSI.
  //   - "colorScheme": null | ""              -> plain text, no ANSI.
  //   - "colorScheme": "Nord" / "GinBadge"..  -> that scheme.
  //   - "colors": true, no scheme             -> default scheme.
  lColorsExplicit := TryGetBool(aConfig, 'colors', lColorsField);

  lHasSchemeField := False;
  lSchemeName := '';
  lSchemeValue := aConfig.GetValue('colorScheme');
  if lSchemeValue <> nil then
  begin
    lHasSchemeField := True;
    if lSchemeValue is TJSONString then
      lSchemeName := TJSONString(lSchemeValue).Value
    else if not (lSchemeValue is TJSONNull) then
      raise ELoggerProConfigError.Create(
        'Field "colorScheme" must be a string or null.');
  end;

  // Reject conflicting "colors: false" together with a non-empty
  // "colorScheme": one says no, the other says yes. Silently picking
  // one would mask a typo / stale field in the JSON.
  if lColorsExplicit and (not lColorsField) and
     lHasSchemeField and (lSchemeName <> '') then
    raise ELoggerProConfigError.CreateFmt(
      'Conflicting JSON: "colors": false together with "colorScheme": "%s". ' +
      'Use "colors": false (no colors) OR a colorScheme (with colors), not both.',
      [lSchemeName]);

  // Decide whether colors are on and which scheme wins.
  if lColorsExplicit and (not lColorsField) then
    lColorsEnabled := False
  else if lHasSchemeField and (lSchemeName = '') then
    // explicit "" or null -> opt out
    lColorsEnabled := False
  else
    lColorsEnabled := True;

  if lColorsEnabled then
  begin
    if lSchemeName <> '' then
      lCfg := lCfg.WithColorScheme(ParseColorScheme(lSchemeName))
    else
      // default professional scheme
      lCfg := lCfg.WithColorScheme(LogColorSchemes.Midnight);
  end;

  if TryGetString(aConfig, 'prefix', lStr) then
    lCfg := lCfg.WithPrefix(lStr);
  if TryGetBool(aConfig, 'utf8Output', lBool) and lBool then
    lCfg := lCfg.WithUTF8Output;
  lCfg.Done;
end;

procedure SimpleConsoleFactory(const aBuilder: ILoggerProBuilder; const aConfig: TJSONObject);
var
  lCfg: ISimpleConsoleAppenderConfigurator;
  lLogLevel: TLogType;
  lBool: Boolean;
begin
  lCfg := aBuilder.WriteToSimpleConsole;
  if TryGetLogLevel(aConfig, 'minimumLevel', lLogLevel) then
    lCfg := lCfg.WithMinimumLevel(lLogLevel);
  if TryGetBool(aConfig, 'utf8Output', lBool) and lBool then
    lCfg := lCfg.WithUTF8Output;
  lCfg.Done;
end;

procedure FileFactory(const aBuilder: ILoggerProBuilder; const aConfig: TJSONObject);
var
  lCfg: IFileAppenderConfigurator;
  lLogLevel: TLogType;
  lStr: string;
  lInt: Integer;
begin
  lCfg := aBuilder.WriteToFile;
  if TryGetLogLevel(aConfig, 'minimumLevel', lLogLevel) then
    lCfg := lCfg.WithMinimumLevel(lLogLevel);
  if TryGetString(aConfig, 'logsFolder', lStr) then
    lCfg := lCfg.WithLogsFolder(lStr);
  if TryGetString(aConfig, 'fileBaseName', lStr) then
    lCfg := lCfg.WithFileBaseName(lStr);
  if TryGetString(aConfig, 'fileFormat', lStr) then
    lCfg := lCfg.WithFileFormat(lStr);
  if TryGetInt(aConfig, 'maxBackupFiles', lInt) then
    lCfg := lCfg.WithMaxBackupFiles(lInt);
  if TryGetInt(aConfig, 'maxFileSizeInKB', lInt) then
    lCfg := lCfg.WithMaxFileSizeInKB(lInt);
  if TryGetInt(aConfig, 'maxRetainedFiles', lInt) then
    lCfg := lCfg.WithMaxRetainedFiles(lInt);
  if TryGetString(aConfig, 'interval', lStr) then
    lCfg := lCfg.WithInterval(ParseRotationInterval(lStr));
  lCfg.Done;
end;

procedure JSONLFileFactory(const aBuilder: ILoggerProBuilder; const aConfig: TJSONObject);
var
  lCfg: IJSONLFileAppenderConfigurator;
  lLogLevel: TLogType;
  lStr: string;
  lInt: Integer;
begin
  lCfg := aBuilder.WriteToJSONLFile;
  if TryGetLogLevel(aConfig, 'minimumLevel', lLogLevel) then
    lCfg := lCfg.WithMinimumLevel(lLogLevel);
  if TryGetString(aConfig, 'logsFolder', lStr) then
    lCfg := lCfg.WithLogsFolder(lStr);
  if TryGetString(aConfig, 'fileBaseName', lStr) then
    lCfg := lCfg.WithFileBaseName(lStr);
  if TryGetInt(aConfig, 'maxBackupFiles', lInt) then
    lCfg := lCfg.WithMaxBackupFiles(lInt);
  if TryGetInt(aConfig, 'maxFileSizeInKB', lInt) then
    lCfg := lCfg.WithMaxFileSizeInKB(lInt);
  lCfg.Done;
end;

procedure TimeRotatingFileFactory(const aBuilder: ILoggerProBuilder; const aConfig: TJSONObject);
var
  lCfg: ITimeRotatingFileAppenderConfigurator;
  lLogLevel: TLogType;
  lStr: string;
  lInt: Integer;
begin
  lCfg := aBuilder.WriteToTimeRotatingFile;
  if TryGetLogLevel(aConfig, 'minimumLevel', lLogLevel) then
    lCfg := lCfg.WithMinimumLevel(lLogLevel);
  if TryGetString(aConfig, 'logsFolder', lStr) then
    lCfg := lCfg.WithLogsFolder(lStr);
  if TryGetString(aConfig, 'fileBaseName', lStr) then
    lCfg := lCfg.WithFileBaseName(lStr);
  if TryGetString(aConfig, 'interval', lStr) then
    lCfg := lCfg.WithInterval(ParseRotationInterval(lStr));
  if TryGetInt(aConfig, 'maxBackupFiles', lInt) then
    lCfg := lCfg.WithMaxBackupFiles(lInt);
  lCfg.Done;
end;

procedure FileBySourceFactory(const aBuilder: ILoggerProBuilder; const aConfig: TJSONObject);
var
  lCfg: IFileBySourceAppenderConfigurator;
  lLogLevel: TLogType;
  lStr: string;
  lInt: Integer;
begin
  lCfg := aBuilder.WriteToFileBySource;
  if TryGetLogLevel(aConfig, 'minimumLevel', lLogLevel) then
    lCfg := lCfg.WithMinimumLevel(lLogLevel);
  if TryGetString(aConfig, 'logsFolder', lStr) then
    lCfg := lCfg.WithLogsFolder(lStr);
  if TryGetString(aConfig, 'defaultSource', lStr) then
    lCfg := lCfg.WithDefaultSource(lStr);
  if TryGetInt(aConfig, 'maxFileSizeInKB', lInt) then
    lCfg := lCfg.WithMaxFileSizeInKB(lInt);
  if TryGetInt(aConfig, 'retainDays', lInt) then
    lCfg := lCfg.WithRetainDays(lInt);
  lCfg.Done;
end;

procedure HTMLFileFactory(const aBuilder: ILoggerProBuilder; const aConfig: TJSONObject);
var
  lCfg: IHTMLFileAppenderConfigurator;
  lLogLevel: TLogType;
  lStr: string;
  lInt: Integer;
begin
  lCfg := aBuilder.WriteToHTMLFile;
  if TryGetLogLevel(aConfig, 'minimumLevel', lLogLevel) then
    lCfg := lCfg.WithMinimumLevel(lLogLevel);
  if TryGetString(aConfig, 'logsFolder', lStr) then
    lCfg := lCfg.WithLogsFolder(lStr);
  if TryGetString(aConfig, 'fileBaseName', lStr) then
    lCfg := lCfg.WithFileBaseName(lStr);
  if TryGetString(aConfig, 'title', lStr) then
    lCfg := lCfg.WithTitle(lStr);
  if TryGetInt(aConfig, 'maxBackupFiles', lInt) then
    lCfg := lCfg.WithMaxBackupFiles(lInt);
  if TryGetInt(aConfig, 'maxFileSizeInKB', lInt) then
    lCfg := lCfg.WithMaxFileSizeInKB(lInt);
  if TryGetInt(aConfig, 'maxRetainedFiles', lInt) then
    lCfg := lCfg.WithMaxRetainedFiles(lInt);
  if TryGetString(aConfig, 'interval', lStr) then
    lCfg := lCfg.WithInterval(ParseRotationInterval(lStr));
  lCfg.Done;
end;

procedure WebhookFactory(const aBuilder: ILoggerProBuilder; const aConfig: TJSONObject);
var
  lCfg: IWebhookAppenderConfigurator;
  lLogLevel: TLogType;
  lStr: string;
  lInt: Integer;
  lHeaders: TJSONObject;
  lPair: TJSONPair;
  lKey, lKeyName: string;
  lLocation: TWebhookAPIKeyLocation;
  lHasKey: Boolean;
begin
  lCfg := aBuilder.WriteToWebhook;
  if TryGetLogLevel(aConfig, 'minimumLevel', lLogLevel) then
    lCfg := lCfg.WithMinimumLevel(lLogLevel);
  if TryGetString(aConfig, 'url', lStr) then
    lCfg := lCfg.WithURL(lStr);
  if TryGetString(aConfig, 'contentType', lStr) then
    lCfg := lCfg.WithContentType(ParseHTTPContentType(lStr));
  if TryGetInt(aConfig, 'timeout', lInt) then
    lCfg := lCfg.WithTimeout(lInt);
  if TryGetInt(aConfig, 'retryCount', lInt) then
    lCfg := lCfg.WithRetryCount(lInt);
  // Optional "headers": { "X-Foo": "bar", ... }
  lHeaders := aConfig.GetValue('headers') as TJSONObject;
  if lHeaders <> nil then
    for lPair in lHeaders do
      lCfg := lCfg.WithHeader(lPair.JsonString.Value, (lPair.JsonValue as TJSONString).Value);
  // API key: apiKey is the secret; apiKeyLocation ("Header"/"QueryString",
  // default Header) and apiKeyName (default depends on location) are optional.
  lHasKey := TryGetString(aConfig, 'apiKey', lKey);
  if lHasKey then
  begin
    lLocation := TWebhookAPIKeyLocation.Header;
    if TryGetString(aConfig, 'apiKeyLocation', lStr) then
      lLocation := ParseAPIKeyLocation(lStr);
    lKeyName := '';
    TryGetString(aConfig, 'apiKeyName', lKeyName);
    lCfg := lCfg.WithAPIKey(lKey, lLocation, lKeyName);
  end;
  lCfg.Done;
end;

procedure UDPSyslogFactory(const aBuilder: ILoggerProBuilder; const aConfig: TJSONObject);
var
  lCfg: IUDPSyslogAppenderConfigurator;
  lLogLevel: TLogType;
  lStr: string;
  lInt: Integer;
  lBool: Boolean;
begin
  lCfg := aBuilder.WriteToUDPSyslog;
  if TryGetLogLevel(aConfig, 'minimumLevel', lLogLevel) then
    lCfg := lCfg.WithMinimumLevel(lLogLevel);
  if TryGetString(aConfig, 'host', lStr) then
    lCfg := lCfg.WithHost(lStr);
  if TryGetInt(aConfig, 'port', lInt) then
    lCfg := lCfg.WithPort(lInt);
  if TryGetString(aConfig, 'hostName', lStr) then
    lCfg := lCfg.WithHostName(lStr);
  if TryGetString(aConfig, 'userName', lStr) then
    lCfg := lCfg.WithUserName(lStr);
  if TryGetString(aConfig, 'application', lStr) then
    lCfg := lCfg.WithApplication(lStr);
  if TryGetString(aConfig, 'version', lStr) then
    lCfg := lCfg.WithVersion(lStr);
  if TryGetString(aConfig, 'procID', lStr) then
    lCfg := lCfg.WithProcID(lStr);
  if TryGetBool(aConfig, 'useLocalTime', lBool) then
    lCfg := lCfg.WithUseLocalTime(lBool);
  lCfg.Done;
end;

procedure OutputDebugStringFactory(const aBuilder: ILoggerProBuilder; const aConfig: TJSONObject);
var
  lCfg: IOutputDebugStringAppenderConfigurator;
  lLogLevel: TLogType;
begin
  lCfg := aBuilder.WriteToOutputDebugString;
  if TryGetLogLevel(aConfig, 'minimumLevel', lLogLevel) then
    lCfg := lCfg.WithMinimumLevel(lLogLevel);
  lCfg.Done;
end;

procedure MemoryFactory(const aBuilder: ILoggerProBuilder; const aConfig: TJSONObject);
var
  lCfg: IMemoryAppenderConfigurator;
  lLogLevel: TLogType;
  lInt: Integer;
begin
  lCfg := aBuilder.WriteToMemory;
  if TryGetLogLevel(aConfig, 'minimumLevel', lLogLevel) then
    lCfg := lCfg.WithMinimumLevel(lLogLevel);
  if TryGetInt(aConfig, 'maxSize', lInt) then
    lCfg := lCfg.WithMaxSize(lInt);
  lCfg.Done;
end;

{ Built-in appender registration.

  An appender qualifies as "built-in" (registered here, no extra `uses`
  required to enable the JSON name) when it satisfies ALL of:

    1) Fully describable in JSON. Every constructor / configuration
       parameter must be expressible as a string, number or boolean.
       Appenders that need a runtime object (TStrings, a VCL component,
       a TFDConnection, a TIdSMTP) or a callback (TFunc / TProc) are
       structurally incompatible with file-based config and stay out
       of this list - they remain available via the imperative /
       fluent builder API.

    2) Generally useful AND lightweight. The entries below cover the
       routine observability needs (console / files / network syslog /
       webhook / OS-level outputs). Backends that target a specific
       external system (ElasticSearch, ExeWatch, WindowsEventLog,
       DBAppender, NSQ, EMail, FireDAC...) ship in their own units and
       self-register from their own initialization section - including
       the unit in `uses` is enough to enable its JSON name.

    3) Lives in the core package. Optional / pluggable units (see
       LoggerPro.ExeWatchAppender, LoggerPro.ElasticSearchAppender,
       LoggerPro.WindowsEventLogAppender for the canonical pattern)
       register themselves so the core has no dependency on optional
       code.

  Adding a new built-in: append a RegisterAppenderType call below AND
  add the unit to the implementation `uses` clause of this unit. }
procedure RegisterBuiltInFactories;
begin
  TLoggerProConfig.RegisterAppenderType('Console', ConsoleFactory,
    ['minimumLevel', 'colors', 'colorScheme', 'prefix', 'utf8Output', 'renderer']);

  TLoggerProConfig.RegisterAppenderType('SimpleConsole', SimpleConsoleFactory,
    ['minimumLevel', 'utf8Output']);

  TLoggerProConfig.RegisterAppenderType('File', FileFactory,
    ['minimumLevel', 'logsFolder', 'fileBaseName', 'fileFormat',
     'maxBackupFiles', 'maxFileSizeInKB', 'maxRetainedFiles', 'interval']);

  TLoggerProConfig.RegisterAppenderType('JSONLFile', JSONLFileFactory,
    ['minimumLevel', 'logsFolder', 'fileBaseName',
     'maxBackupFiles', 'maxFileSizeInKB']);

  TLoggerProConfig.RegisterAppenderType('TimeRotatingFile', TimeRotatingFileFactory,
    ['minimumLevel', 'logsFolder', 'fileBaseName', 'interval', 'maxBackupFiles']);

  TLoggerProConfig.RegisterAppenderType('FileBySource', FileBySourceFactory,
    ['minimumLevel', 'logsFolder', 'defaultSource',
     'maxFileSizeInKB', 'retainDays']);

  TLoggerProConfig.RegisterAppenderType('HTMLFile', HTMLFileFactory,
    ['minimumLevel', 'logsFolder', 'fileBaseName', 'title',
     'maxBackupFiles', 'maxFileSizeInKB', 'maxRetainedFiles', 'interval']);

  TLoggerProConfig.RegisterAppenderType('Webhook', WebhookFactory,
    ['minimumLevel', 'url', 'contentType', 'timeout', 'retryCount', 'headers',
     'apiKey', 'apiKeyLocation', 'apiKeyName']);

  TLoggerProConfig.RegisterAppenderType('UDPSyslog', UDPSyslogFactory,
    ['minimumLevel', 'host', 'port', 'hostName', 'userName', 'application',
     'version', 'procID', 'useLocalTime']);

  TLoggerProConfig.RegisterAppenderType('OutputDebugString', OutputDebugStringFactory,
    ['minimumLevel']);

  TLoggerProConfig.RegisterAppenderType('Memory', MemoryFactory,
    ['minimumLevel', 'maxSize']);
end;

{ ===== TLoggerProConfig ===== }

class function TLoggerProConfig.ParseLogLevel(const aValue: string): TLogType;
begin
  Result := ParseLogType(aValue);
end;

class function TLoggerProConfig.TryGetJSONLogLevel(const aObj: TJSONObject;
  const aName: string; out aLevel: TLogType): Boolean;
begin
  Result := TryGetLogLevel(aObj, aName, aLevel);
end;

class procedure TLoggerProConfig.RegisterAppenderType(const aType: string;
  const aFactory: TLoggerProAppenderFactory;
  const aAllowedFields: TArray<string>);
var
  lInfo: TAppenderTypeInfo;
begin
  // Lazy init of the factory dictionary: an appender unit may initialize
  // BEFORE LoggerPro.Config when the `uses` graph has a cycle (e.g. an
  // appender whose impl section uses LoggerPro.Config, while Config's impl
  // transitively pulls that appender back through LoggerPro.Builder). In
  // such cases Delphi picks one of the two units for first-init; if the
  // appender wins, gFactories would be nil here - just materialize it.
  if gFactories = nil then
    gFactories := TDictionary<string, TAppenderTypeInfo>.Create;
  lInfo.Factory := aFactory;
  lInfo.AllowedFields := aAllowedFields;
  gFactories.AddOrSetValue(LowerCase(aType), lInfo);
end;

{ Build a helpful "unknown appender type" diagnostic. The user typed
  a type name that isn't in the registry, which is almost always one
  of two things: a typo, or a forgotten `uses` clause for an optional
  appender unit (the unit's initialization is what calls
  RegisterAppenderType). We surface BOTH possibilities in the message:
  the full list of currently-registered types (catches typos), and a
  reminder that pluggable appenders need their unit included. }
function BuildUnknownTypeMessage(const aIndex: Integer; const aType: string): string;
var
  lTypes: TArray<string>;
  lKey: string;
begin
  lTypes := nil;
  for lKey in gFactories.Keys do
    lTypes := lTypes + [lKey];
  TArray.Sort<string>(lTypes);
  Result := Format(
    'appenders[%d]: unknown type "%s".'#10 +
    'Currently registered types: %s.'#10 +
    'If you want "%s", make sure the unit that registers it is in your `uses` clause '
    + '(typically LoggerPro.%sAppender) - optional appenders self-register from their '
    + 'initialization section, so simply including the unit is enough. '
    + 'For custom factories use TLoggerProConfig.RegisterAppenderType.',
    [aIndex, aType, JoinStrings(lTypes, ', '), aType, aType]);
end;

procedure ValidateRootFields(const aRoot: TJSONObject);
var
  lPair: TJSONPair;
begin
  for lPair in aRoot do
    if not ContainsCI(ROOT_FIELDS, lPair.JsonString.Value) then
      raise ELoggerProConfigError.CreateFmt(
        'Unknown root field "%s". Valid root fields: %s.',
        [lPair.JsonString.Value, JoinStrings(ROOT_FIELDS, ', ')]);
end;

procedure ValidateAppenderFields(const aAppender: TJSONObject;
  const aType: string; const aIndex: Integer;
  const aAllowedFields: TArray<string>);
var
  lPair: TJSONPair;
  lKey: string;
begin
  for lPair in aAppender do
  begin
    lKey := lPair.JsonString.Value;
    if SameText(lKey, 'type') then Continue;
    if not ContainsCI(aAllowedFields, lKey) then
      raise ELoggerProConfigError.CreateFmt(
        'appenders[%d] (type=%s): unknown field "%s". Valid fields: %s.',
        [aIndex, aType, lKey, JoinStrings(aAllowedFields, ', ')]);
  end;
end;

class function TLoggerProConfig.BuilderFromJSONString(const aJSON: string): ILoggerProBuilder;
var
  lRoot: TJSONValue;
  lObj: TJSONObject;
  lBuilder: ILoggerProBuilder;
  lAppenders: TJSONArray;
  lAppender: TJSONValue;
  lAppenderObj: TJSONObject;
  lInfo: TAppenderTypeInfo;
  lType, lLevelStr, lTag: string;
  lLevel: TLogType;
  lVersion: Integer;
  i: Integer;
begin
  lRoot := TJSONObject.ParseJSONValue(aJSON);
  if lRoot = nil then
    raise ELoggerProConfigError.Create('Malformed JSON: parser returned nil.');
  try
    if not (lRoot is TJSONObject) then
      raise ELoggerProConfigError.Create('Root JSON value must be an object.');
    lObj := TJSONObject(lRoot);

    // Strict: every root field must be in the known set (catches typos
    // such as "defaultag" instead of "defaultTag").
    ValidateRootFields(lObj);

    // Schema version check. Missing = assume latest (forward-compatible
    // for files written before versioning was added). Present-and-future =
    // hard error so the user doesn't get silent partial parsing.
    if TryGetInt(lObj, 'configVersion', lVersion) then
    begin
      if lVersion > LOGGERPRO_CONFIG_VERSION then
        raise ELoggerProConfigError.CreateFmt(
          'configVersion %d is newer than supported (max %d). Update LoggerPro.',
          [lVersion, LOGGERPRO_CONFIG_VERSION]);
      if lVersion < 1 then
        raise ELoggerProConfigError.CreateFmt(
          'configVersion must be >= 1 (got %d).', [lVersion]);
    end;

    lBuilder := LoggerProBuilder;

    // Global options
    if TryGetString(lObj, 'minimumLevel', lLevelStr) then
    begin
      lLevel := ParseLogType(lLevelStr);
      lBuilder.WithMinimumLevel(lLevel);
    end;
    if TryGetString(lObj, 'defaultMinimumLevel', lLevelStr) then
    begin
      lLevel := ParseLogType(lLevelStr);
      lBuilder.WithDefaultMinimumLevel(lLevel);
    end;
    if TryGetString(lObj, 'defaultTag', lTag) then
      lBuilder.WithDefaultTag(lTag);

    // Appenders
    lAppenders := lObj.GetValue('appenders') as TJSONArray;
    if lAppenders = nil then
      raise ELoggerProConfigError.Create(
        'Missing required "appenders" array at root.');

    for i := 0 to lAppenders.Count - 1 do
    begin
      lAppender := lAppenders.Items[i];
      if not (lAppender is TJSONObject) then
        raise ELoggerProConfigError.CreateFmt(
          'appenders[%d] must be an object.', [i]);
      lAppenderObj := TJSONObject(lAppender);

      if not TryGetString(lAppenderObj, 'type', lType) then
        raise ELoggerProConfigError.CreateFmt(
          'appenders[%d] is missing required "type" field.', [i]);

      if not gFactories.TryGetValue(LowerCase(lType), lInfo) then
        raise ELoggerProConfigError.Create(
          BuildUnknownTypeMessage(i, lType));

      // Strict: reject any field the factory does not know. This catches
      // typos like "colour" vs "color" - the user gets an exact error with
      // the full list of valid field names for this appender type.
      ValidateAppenderFields(lAppenderObj, lType, i, lInfo.AllowedFields);

      try
        lInfo.Factory(lBuilder, lAppenderObj);
      except
        on E: ELoggerProConfigError do
          raise ELoggerProConfigError.CreateFmt('appenders[%d] (type=%s): %s',
            [i, lType, E.Message]);
        on E: Exception do
          raise ELoggerProConfigError.CreateFmt(
            'appenders[%d] (type=%s): %s - %s',
            [i, lType, E.ClassName, E.Message]);
      end;
    end;

    Result := lBuilder;
  finally
    lRoot.Free;
  end;
end;

class function TLoggerProConfig.BuilderFromJSONFile(const aFileName: string): ILoggerProBuilder;
var
  lJSON: string;
begin
  if not TFile.Exists(aFileName) then
    raise ELoggerProConfigError.CreateFmt(
      'Config file not found: %s', [aFileName]);
  lJSON := TFile.ReadAllText(aFileName, TEncoding.UTF8);
  Result := BuilderFromJSONString(lJSON);
end;

class function TLoggerProConfig.FromJSONString(const aJSON: string): ILogWriter;
begin
  Result := BuilderFromJSONString(aJSON).Build;
end;

class function TLoggerProConfig.FromJSONFile(const aFileName: string): ILogWriter;
begin
  Result := BuilderFromJSONFile(aFileName).Build;
end;

initialization
  // gFactories is lazy-created by RegisterAppenderType to avoid a nil deref
  // if an appender unit's initialization runs before Config's (cyclic-deps).
  if gFactories = nil then
    gFactories := TDictionary<string, TAppenderTypeInfo>.Create;
  RegisterBuiltInFactories;

finalization
  FreeAndNil(gFactories);

end.
