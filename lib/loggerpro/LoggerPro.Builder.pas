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

unit LoggerPro.Builder;

{ Fluent interface builder for LoggerPro configuration }

interface

uses
  LoggerPro,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

{ Helper function for fluent builder creation }
function LoggerProBuilder: ILoggerProBuilder;

implementation

uses
  LoggerPro.AnsiColors,
  LoggerPro.CallbackAppender,
  LoggerPro.FileAppender,
  LoggerPro.TimeRotatingFileAppender,
  LoggerPro.WebhookAppender,
  LoggerPro.Proxy,
  LoggerPro.ConsoleAppender,
  LoggerPro.Renderers,
  LoggerPro.RendererRegistry,
  LoggerPro.JSONLFileAppender,
  LoggerPro.ElasticSearchAppender,
  LoggerPro.MemoryAppender,
  LoggerPro.OutputDebugStringAppender,
  LoggerPro.UDPSyslogAppender,
  LoggerPro.StringsAppender,
  LoggerPro.FileBySourceAppender,
  LoggerPro.HTMLFileAppender,
  LoggerPro.DBAppender.FireDAC
{$IF Defined(MSWINDOWS)}
  , LoggerPro.VCLMemoAppender
  , LoggerPro.VCLListBoxAppender
  , LoggerPro.VCLListViewAppender
  , LoggerPro.WindowsEventLogAppender
  , Vcl.StdCtrls
  , Vcl.ComCtrls
  , Vcl.SvcMgr
{$ENDIF}
  ;

type
  { Forward declaration }
  TLoggerProBuilder = class;

  { Base configurator class }
  TBaseAppenderConfigurator = class(TInterfacedObject)
  protected
    FBuilder: TLoggerProBuilder;
    FLogLevel: TLogType;
    FLogLevelSet: Boolean;
    FRenderer: ILogItemRenderer;
    procedure ApplyLogLevel(aAppender: ILogAppender);
    function GetRenderer: ILogItemRenderer;
  public
    constructor Create(aBuilder: TLoggerProBuilder);
  end;

  { Console appender configurator }
  TConsoleAppenderConfigurator = class(TBaseAppenderConfigurator, IConsoleAppenderConfigurator)
  private
    FUTF8Output: Boolean;
    FUseColors: Boolean;
    FColorScheme: TLogColorScheme;
    FPrefix: string;
  public
    function WithMinimumLevel(aLogLevel: TLogType): IConsoleAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): IConsoleAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IConsoleAppenderConfigurator; overload;
    function WithUTF8Output: IConsoleAppenderConfigurator;
    function WithColors: IConsoleAppenderConfigurator;
    function WithColorScheme(const aScheme: TLogColorScheme): IConsoleAppenderConfigurator;
    function WithPrefix(const aPrefix: string): IConsoleAppenderConfigurator;
    function Done: ILoggerProBuilder;
  end;

  { Simple console appender configurator }
  TSimpleConsoleAppenderConfigurator = class(TBaseAppenderConfigurator, ISimpleConsoleAppenderConfigurator)
  private
    FUTF8Output: Boolean;
  public
    function WithMinimumLevel(aLogLevel: TLogType): ISimpleConsoleAppenderConfigurator;
    function WithUTF8Output: ISimpleConsoleAppenderConfigurator;
    function Done: ILoggerProBuilder;
  end;

  { File appender configurator }
  TFileAppenderConfigurator = class(TBaseAppenderConfigurator, IFileAppenderConfigurator)
  private
    FLogsFolder: string;
    FFileBaseName: string;
    FMaxBackupFiles: Integer;
    FMaxFileSizeInKB: Integer;
    FEncoding: TEncoding;
    FRotationInterval: TTimeRotationInterval;
    FMaxRetainedFiles: Integer;
    FFileFormat: string;
    FOnAfterRotate: TFileRotateCallback;
  public
    constructor Create(aBuilder: TLoggerProBuilder);
    function WithLogsFolder(const aLogsFolder: string): IFileAppenderConfigurator;
    function WithFileBaseName(const aFileBaseName: string): IFileAppenderConfigurator;
    function WithMaxBackupFiles(aMaxBackupFiles: Integer): IFileAppenderConfigurator;
    function WithMaxFileSizeInKB(aMaxFileSizeInKB: Integer): IFileAppenderConfigurator;
    function WithInterval(aInterval: TTimeRotationInterval): IFileAppenderConfigurator;
    function WithFileFormat(const aFileFormat: string): IFileAppenderConfigurator;
    function WithMaxRetainedFiles(aMaxFiles: Integer): IFileAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IFileAppenderConfigurator;
    function WithEncoding(aEncoding: TEncoding): IFileAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): IFileAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IFileAppenderConfigurator; overload;
    function WithOnAfterRotate(aCallback: TFileRotateCallback): IFileAppenderConfigurator;
    function Done: ILoggerProBuilder;
  end;

  { JSONL file appender configurator }
  TJSONLFileAppenderConfigurator = class(TBaseAppenderConfigurator, IJSONLFileAppenderConfigurator)
  private
    FLogsFolder: string;
    FFileBaseName: string;
    FMaxBackupFiles: Integer;
    FMaxFileSizeInKB: Integer;
  public
    constructor Create(aBuilder: TLoggerProBuilder);
    function WithLogsFolder(const aLogsFolder: string): IJSONLFileAppenderConfigurator;
    function WithFileBaseName(const aFileBaseName: string): IJSONLFileAppenderConfigurator;
    function WithMaxBackupFiles(aMaxBackupFiles: Integer): IJSONLFileAppenderConfigurator;
    function WithMaxFileSizeInKB(aMaxFileSizeInKB: Integer): IJSONLFileAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IJSONLFileAppenderConfigurator;
    function Done: ILoggerProBuilder;
  end;

  { Time rotating file appender configurator }
  TTimeRotatingFileAppenderConfigurator = class(TBaseAppenderConfigurator, ITimeRotatingFileAppenderConfigurator)
  private
    FInterval: TTimeRotationInterval;
    FMaxBackupFiles: Integer;
    FLogsFolder: string;
    FFileBaseName: string;
  public
    constructor Create(aBuilder: TLoggerProBuilder);
    function WithInterval(aInterval: TTimeRotationInterval): ITimeRotatingFileAppenderConfigurator;
    function WithMaxBackupFiles(aMaxBackupFiles: Integer): ITimeRotatingFileAppenderConfigurator;
    function WithLogsFolder(const aLogsFolder: string): ITimeRotatingFileAppenderConfigurator;
    function WithFileBaseName(const aFileBaseName: string): ITimeRotatingFileAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): ITimeRotatingFileAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): ITimeRotatingFileAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): ITimeRotatingFileAppenderConfigurator; overload;
    function Done: ILoggerProBuilder;
  end;

  { HTTP appender configurator }
  TWebhookAppenderConfigurator = class(TBaseAppenderConfigurator, IWebhookAppenderConfigurator)
  private
    FURL: string;
    FContentType: TWebhookContentType;
    FTimeoutSeconds: Integer;
    FRetryCount: Integer;
    FHeaders: TDictionary<string, string>;
    FAPIKey: string;
    FAPIKeyName: string;
    FAPIKeyLocation: TWebhookAPIKeyLocation;
    FAPIKeySet: Boolean;
  public
    constructor Create(aBuilder: TLoggerProBuilder);
    destructor Destroy; override;
    function WithURL(const aURL: string): IWebhookAppenderConfigurator;
    function WithContentType(aContentType: TWebhookContentType): IWebhookAppenderConfigurator;
    function WithTimeout(aTimeoutSeconds: Integer): IWebhookAppenderConfigurator;
    function WithRetryCount(aRetryCount: Integer): IWebhookAppenderConfigurator;
    function WithHeader(const aName, aValue: string): IWebhookAppenderConfigurator;
    function WithAPIKey(const aValue: string;
      aLocation: TWebhookAPIKeyLocation = TWebhookAPIKeyLocation.Header;
      const aName: string = ''): IWebhookAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IWebhookAppenderConfigurator;
    function Done: ILoggerProBuilder;
  end;

  { ElasticSearch appender configurator }
  TElasticSearchAppenderConfigurator = class(TBaseAppenderConfigurator, IElasticSearchAppenderConfigurator)
  private
    FURL: string;
    FHost: string;
    FPort: Integer;
    FIndex: string;
    FTimeoutSeconds: Integer;
    FUseHostPortIndex: Boolean;
    FUseBasicAuth: Boolean;
    FBasicAuthUsername: string;
    FBasicAuthPassword: string;
    FUseAPIKey: Boolean;
    FAPIKey: string;
    FUseBearerToken: Boolean;
    FBearerToken: string;
  public
    constructor Create(aBuilder: TLoggerProBuilder);
    function WithURL(const aURL: string): IElasticSearchAppenderConfigurator;
    function WithHost(const aHost: string): IElasticSearchAppenderConfigurator;
    function WithPort(aPort: Integer): IElasticSearchAppenderConfigurator;
    function WithIndex(const aIndex: string): IElasticSearchAppenderConfigurator;
    function WithTimeout(aTimeoutSeconds: Integer): IElasticSearchAppenderConfigurator;
    function WithBasicAuth(const aUsername, aPassword: string): IElasticSearchAppenderConfigurator;
    function WithAPIKey(const aAPIKey: string): IElasticSearchAppenderConfigurator;
    function WithBearerToken(const aToken: string): IElasticSearchAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IElasticSearchAppenderConfigurator;
    function Done: ILoggerProBuilder;
  end;

  { Memory appender configurator }
  TMemoryAppenderConfigurator = class(TBaseAppenderConfigurator, IMemoryAppenderConfigurator)
  private
    FMaxSize: Integer;
  public
    constructor Create(aBuilder: TLoggerProBuilder);
    function WithMaxSize(aMaxSize: Integer): IMemoryAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IMemoryAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): IMemoryAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IMemoryAppenderConfigurator; overload;
    function Done: ILoggerProBuilder;
  end;

  { Callback appender configurator }
  TCallbackAppenderConfigurator = class(TBaseAppenderConfigurator, ICallbackAppenderConfigurator)
  private
    FCallback: TLogItemCallback;
    FSynchronizeToMainThread: Boolean;
  public
    constructor Create(aBuilder: TLoggerProBuilder);
    function WithCallback(aCallback: TLogItemCallback): ICallbackAppenderConfigurator;
    function WithSynchronizeToMainThread(aValue: Boolean): ICallbackAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): ICallbackAppenderConfigurator;
    function Done: ILoggerProBuilder;
  end;

  { OutputDebugString appender configurator }
  TOutputDebugStringAppenderConfigurator = class(TBaseAppenderConfigurator, IOutputDebugStringAppenderConfigurator)
  public
    function WithMinimumLevel(aLogLevel: TLogType): IOutputDebugStringAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): IOutputDebugStringAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IOutputDebugStringAppenderConfigurator; overload;
    function Done: ILoggerProBuilder;
  end;

  { UDP Syslog appender configurator }
  TUDPSyslogAppenderConfigurator = class(TBaseAppenderConfigurator, IUDPSyslogAppenderConfigurator)
  private
    FHost: string;
    FPort: Integer;
    FHostName: string;
    FUserName: string;
    FApplication: string;
    FVersion: string;
    FProcID: string;
    FUseLocalTime: Boolean;
  public
    constructor Create(aBuilder: TLoggerProBuilder);
    function WithHost(const aHost: string): IUDPSyslogAppenderConfigurator;
    function WithPort(aPort: Integer): IUDPSyslogAppenderConfigurator;
    function WithHostName(const aHostName: string): IUDPSyslogAppenderConfigurator;
    function WithUserName(const aUserName: string): IUDPSyslogAppenderConfigurator;
    function WithApplication(const aApplication: string): IUDPSyslogAppenderConfigurator;
    function WithVersion(const aVersion: string): IUDPSyslogAppenderConfigurator;
    function WithProcID(const aProcID: string): IUDPSyslogAppenderConfigurator;
    function WithUseLocalTime(aUseLocalTime: Boolean): IUDPSyslogAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IUDPSyslogAppenderConfigurator;
    function Done: ILoggerProBuilder;
  end;

  { Strings appender configurator }
  TStringsAppenderConfigurator = class(TBaseAppenderConfigurator, IStringsAppenderConfigurator)
  private
    FStrings: TStrings;
    FMaxLogLines: Word;
    FClearOnStartup: Boolean;
  public
    constructor Create(aBuilder: TLoggerProBuilder; aStrings: TStrings);
    function WithMaxLogLines(aMaxLogLines: Word): IStringsAppenderConfigurator;
    function WithClearOnStartup(aValue: Boolean): IStringsAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IStringsAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): IStringsAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IStringsAppenderConfigurator; overload;
    function Done: ILoggerProBuilder;
  end;

{$IF Defined(MSWINDOWS)}
  { VCL Memo appender configurator }
  TVCLMemoAppenderConfigurator = class(TBaseAppenderConfigurator, IVCLMemoAppenderConfigurator)
  private
    FMemo: TMemo;
    FMaxLogLines: Word;
    FClearOnStartup: Boolean;
  public
    constructor Create(aBuilder: TLoggerProBuilder; aMemo: TMemo);
    function WithMaxLogLines(aMaxLogLines: Word): IVCLMemoAppenderConfigurator;
    function WithClearOnStartup(aValue: Boolean): IVCLMemoAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IVCLMemoAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): IVCLMemoAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IVCLMemoAppenderConfigurator; overload;
    function Done: ILoggerProBuilder;
  end;

  { VCL ListBox appender configurator }
  TVCLListBoxAppenderConfigurator = class(TBaseAppenderConfigurator, IVCLListBoxAppenderConfigurator)
  private
    FListBox: TListBox;
    FMaxLogLines: Word;
  public
    constructor Create(aBuilder: TLoggerProBuilder; aListBox: TListBox);
    function WithMaxLogLines(aMaxLogLines: Word): IVCLListBoxAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IVCLListBoxAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): IVCLListBoxAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IVCLListBoxAppenderConfigurator; overload;
    function Done: ILoggerProBuilder;
  end;

  { VCL ListView appender configurator }
  TVCLListViewAppenderConfigurator = class(TBaseAppenderConfigurator, IVCLListViewAppenderConfigurator)
  private
    FListView: TListView;
    FMaxLogLines: Word;
  public
    constructor Create(aBuilder: TLoggerProBuilder; aListView: TListView);
    function WithMaxLogLines(aMaxLogLines: Word): IVCLListViewAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IVCLListViewAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): IVCLListViewAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IVCLListViewAppenderConfigurator; overload;
    function Done: ILoggerProBuilder;
  end;

  { Windows Event Log appender configurator (Windows only) }
  TWindowsEventLogAppenderConfigurator = class(TBaseAppenderConfigurator, IWindowsEventLogAppenderConfigurator)
  private
    FService: TService;
    FSourceName: string;
    FUseService: Boolean;
  public
    constructor Create(aBuilder: TLoggerProBuilder); overload;
    constructor Create(aBuilder: TLoggerProBuilder; aService: TService); overload;
    function WithMinimumLevel(aLogLevel: TLogType): IWindowsEventLogAppenderConfigurator;
    function WithSourceName(const aSourceName: string): IWindowsEventLogAppenderConfigurator;
    function Done: ILoggerProBuilder;
  end;

{$ENDIF}

  { FireDAC DB appender configurator (cross-platform) }
  TFireDACAppenderConfigurator = class(TBaseAppenderConfigurator, IFireDACAppenderConfigurator)
  private
    FConnectionDefName: string;
    FStoredProcName: string;
  public
    function WithConnectionDefName(const aConnectionDefName: string): IFireDACAppenderConfigurator;
    function WithStoredProcName(const aStoredProcName: string): IFireDACAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IFireDACAppenderConfigurator;
    function Done: ILoggerProBuilder;
  end;

  { File-by-source appender configurator }
  TFileBySourceAppenderConfigurator = class(TBaseAppenderConfigurator, IFileBySourceAppenderConfigurator)
  private
    FLogsFolder: string;
    FMaxFileSizeInKB: Integer;
    FRetainDays: Integer;
    FDefaultSource: string;
  public
    constructor Create(aBuilder: TLoggerProBuilder);
    function WithLogsFolder(const aLogsFolder: string): IFileBySourceAppenderConfigurator;
    function WithMaxFileSizeInKB(aMaxFileSizeInKB: Integer): IFileBySourceAppenderConfigurator;
    function WithRetainDays(aRetainDays: Integer): IFileBySourceAppenderConfigurator;
    function WithDefaultSource(const aDefaultSource: string): IFileBySourceAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IFileBySourceAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): IFileBySourceAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IFileBySourceAppenderConfigurator; overload;
    function Done: ILoggerProBuilder;
  end;

  THTMLFileAppenderConfigurator = class(TBaseAppenderConfigurator, IHTMLFileAppenderConfigurator)
  private
    FLogsFolder: string;
    FFileBaseName: string;
    FTitle: string;
    FMaxBackupFiles: Integer;
    FMaxFileSizeInKB: Integer;
    FInterval: TTimeRotationInterval;
    FMaxRetainedFiles: Integer;
  public
    constructor Create(aBuilder: TLoggerProBuilder);
    function WithLogsFolder(const aLogsFolder: string): IHTMLFileAppenderConfigurator;
    function WithFileBaseName(const aFileBaseName: string): IHTMLFileAppenderConfigurator;
    function WithTitle(const aTitle: string): IHTMLFileAppenderConfigurator;
    function WithMaxBackupFiles(aMaxBackupFiles: Integer): IHTMLFileAppenderConfigurator;
    function WithMaxFileSizeInKB(aMaxFileSizeInKB: Integer): IHTMLFileAppenderConfigurator;
    function WithInterval(aInterval: TTimeRotationInterval): IHTMLFileAppenderConfigurator;
    function WithMaxRetainedFiles(aMaxFiles: Integer): IHTMLFileAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IHTMLFileAppenderConfigurator;
    function Done: ILoggerProBuilder;
  end;

  { Filtered appender configurator - generic filter for any appender }
  TFilteredAppenderConfigurator = class(TBaseAppenderConfigurator, IFilteredAppenderConfigurator)
  private
    FInnerAppender: ILogAppender;
    FFilter: TLogItemFilterFunc;
  public
    constructor Create(aBuilder: TLoggerProBuilder; aAppender: ILogAppender);
    function WithFilter(aFilter: TLogItemFilterFunc): IFilteredAppenderConfigurator;
    function Done: ILoggerProBuilder;
  end;

  { Builder implementation - hidden from interface }
  TLoggerProBuilder = class(TInterfacedObject, ILoggerProBuilder)
  private
    FAppenders: TList<ILogAppender>;
    FDefaultLogLevel: TLogType;
    FMinimumLevel: TLogType;
    FMinimumLevelSet: Boolean;
    FDefaultRenderer: ILogItemRenderer;
    FDefaultTag: string;
    FStackTraceFormatter: TStackTraceFormatter;
    FPendingConfiguratorName: string;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: ILoggerProBuilder;
    // WriteTo appender methods
    function WriteToConsole: IConsoleAppenderConfigurator;
    function WriteToSimpleConsole: ISimpleConsoleAppenderConfigurator;
    function WriteToFile: IFileAppenderConfigurator;
    function WriteToJSONLFile: IJSONLFileAppenderConfigurator;
    function WriteToTimeRotatingFile: ITimeRotatingFileAppenderConfigurator;
    function WriteToWebhook: IWebhookAppenderConfigurator;
    function WriteToElasticSearch: IElasticSearchAppenderConfigurator;
    function WriteToMemory: IMemoryAppenderConfigurator;
    function WriteToCallback: ICallbackAppenderConfigurator;
    function WriteToOutputDebugString: IOutputDebugStringAppenderConfigurator;
    function WriteToUDPSyslog: IUDPSyslogAppenderConfigurator;
    function WriteToStrings(aStrings: TStrings): IStringsAppenderConfigurator;
{$IF Defined(MSWINDOWS)}
    // VCL appenders (Windows only)
    function WriteToVCLMemo(aMemo: TObject): IVCLMemoAppenderConfigurator;
    function WriteToVCLListBox(aListBox: TObject): IVCLListBoxAppenderConfigurator;
    function WriteToVCLListView(aListView: TObject): IVCLListViewAppenderConfigurator;
    // Windows Event Log appender (Windows only)
    function WriteToWindowsEventLog: IWindowsEventLogAppenderConfigurator;
    function WriteToWindowsEventLogForService(aService: TObject): IWindowsEventLogAppenderConfigurator;
{$ENDIF}
    // FireDAC appender (cross-platform)
    function WriteToFireDAC: IFireDACAppenderConfigurator;
    // Filtered appender - wraps any appender with a filter
    function WriteToFilteredAppender(aAppender: ILogAppender): IFilteredAppenderConfigurator;
    // File-by-source appender
    function WriteToFileBySource: IFileBySourceAppenderConfigurator;
    // Self-contained HTML file appender
    function WriteToHTMLFile: IHTMLFileAppenderConfigurator;
    // Generic method for adding pre-configured appenders
    function WriteToAppender(aAppender: ILogAppender): ILoggerProBuilder;
    // Global configuration
    function WithDefaultMinimumLevel(aLogLevel: TLogType): ILoggerProBuilder;
    function WithMinimumLevel(aLevel: TLogType): ILoggerProBuilder;
    function WithDefaultRenderer(aRenderer: ILogItemRenderer): ILoggerProBuilder;
    function WithDefaultTag(const aTag: string): ILoggerProBuilder;
    function WithStackTraceFormatter(aFormatter: TStackTraceFormatter): ILoggerProBuilder;
    // Build the logger
    function Build: ILogWriter;
    // Used by configurators
    procedure InternalAddAppender(aAppender: ILogAppender);
    function GetDefaultRenderer: ILogItemRenderer;
    procedure SetPendingConfigurator(const aName: string);
    procedure ClearPendingConfigurator;
    // Exposed so TBaseAppenderConfigurator.ApplyLogLevel can read the
    // builder-wide fallback (set via WithDefaultMinimumLevel / the JSON
    // "defaultMinimumLevel" root field).
    property DefaultMinimumLevel: TLogType read FDefaultLogLevel;
  end;

{ TBaseAppenderConfigurator }

constructor TBaseAppenderConfigurator.Create(aBuilder: TLoggerProBuilder);
begin
  inherited Create;
  FBuilder := aBuilder;
  FLogLevel := TLogType.Debug;
  FLogLevelSet := False;
end;

procedure TBaseAppenderConfigurator.ApplyLogLevel(aAppender: ILogAppender);
begin
  if FLogLevelSet then
    // explicit .WithMinimumLevel(...) wins
    aAppender.SetMinimumLevel(FLogLevel)
  else if Assigned(FBuilder) then
    // fall back to the builder-wide default (set via WithDefaultMinimumLevel
    // or the JSON "defaultMinimumLevel" root field)
    aAppender.SetMinimumLevel(FBuilder.DefaultMinimumLevel);
end;

function TBaseAppenderConfigurator.GetRenderer: ILogItemRenderer;
begin
  if Assigned(FRenderer) then
    Result := FRenderer
  else
    Result := FBuilder.GetDefaultRenderer;
end;

{ TLoggerProBuilder }

constructor TLoggerProBuilder.Create;
begin
  inherited Create;
  FAppenders := TList<ILogAppender>.Create;
  FDefaultLogLevel := TLogType.Debug;
  FMinimumLevelSet := False;
end;

destructor TLoggerProBuilder.Destroy;
begin
  FAppenders.Free;
  inherited;
end;

class function TLoggerProBuilder.New: ILoggerProBuilder;
begin
  Result := TLoggerProBuilder.Create;
end;

procedure TLoggerProBuilder.InternalAddAppender(aAppender: ILogAppender);
begin
  FAppenders.Add(aAppender);
  ClearPendingConfigurator;
end;

procedure TLoggerProBuilder.SetPendingConfigurator(const aName: string);
begin
  FPendingConfiguratorName := aName;
end;

procedure TLoggerProBuilder.ClearPendingConfigurator;
begin
  FPendingConfiguratorName := '';
end;

function TLoggerProBuilder.WriteToAppender(aAppender: ILogAppender): ILoggerProBuilder;
begin
  FAppenders.Add(aAppender);
  Result := Self;
end;

function TLoggerProBuilder.WriteToConsole: IConsoleAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToConsole');
  Result := TConsoleAppenderConfigurator.Create(Self);
end;

function TLoggerProBuilder.WriteToSimpleConsole: ISimpleConsoleAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToSimpleConsole');
  Result := TSimpleConsoleAppenderConfigurator.Create(Self);
end;

function TLoggerProBuilder.WriteToFile: IFileAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToFile');
  Result := TFileAppenderConfigurator.Create(Self);
end;

function TLoggerProBuilder.WriteToJSONLFile: IJSONLFileAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToJSONLFile');
  Result := TJSONLFileAppenderConfigurator.Create(Self);
end;

function TLoggerProBuilder.WriteToTimeRotatingFile: ITimeRotatingFileAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToTimeRotatingFile');
  Result := TTimeRotatingFileAppenderConfigurator.Create(Self);
end;

function TLoggerProBuilder.WriteToWebhook: IWebhookAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToWebhook');
  Result := TWebhookAppenderConfigurator.Create(Self);
end;

function TLoggerProBuilder.WriteToElasticSearch: IElasticSearchAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToElasticSearch');
  Result := TElasticSearchAppenderConfigurator.Create(Self);
end;

function TLoggerProBuilder.WriteToMemory: IMemoryAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToMemory');
  Result := TMemoryAppenderConfigurator.Create(Self);
end;

function TLoggerProBuilder.WriteToCallback: ICallbackAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToCallback');
  Result := TCallbackAppenderConfigurator.Create(Self);
end;

function TLoggerProBuilder.WriteToOutputDebugString: IOutputDebugStringAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToOutputDebugString');
  Result := TOutputDebugStringAppenderConfigurator.Create(Self);
end;

function TLoggerProBuilder.WriteToUDPSyslog: IUDPSyslogAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToUDPSyslog');
  Result := TUDPSyslogAppenderConfigurator.Create(Self);
end;

function TLoggerProBuilder.WriteToStrings(aStrings: TStrings): IStringsAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToStrings');
  Result := TStringsAppenderConfigurator.Create(Self, aStrings);
end;

{$IF Defined(MSWINDOWS)}
function TLoggerProBuilder.WriteToVCLMemo(aMemo: TObject): IVCLMemoAppenderConfigurator;
begin
  if not (aMemo is TMemo) then
    raise ELoggerPro.Create('WriteToVCLMemo requires a TMemo instance');
  SetPendingConfigurator('WriteToVCLMemo');
  Result := TVCLMemoAppenderConfigurator.Create(Self, TMemo(aMemo));
end;

function TLoggerProBuilder.WriteToVCLListBox(aListBox: TObject): IVCLListBoxAppenderConfigurator;
begin
  if not (aListBox is TListBox) then
    raise ELoggerPro.Create('WriteToVCLListBox requires a TListBox instance');
  SetPendingConfigurator('WriteToVCLListBox');
  Result := TVCLListBoxAppenderConfigurator.Create(Self, TListBox(aListBox));
end;

function TLoggerProBuilder.WriteToVCLListView(aListView: TObject): IVCLListViewAppenderConfigurator;
begin
  if not (aListView is TListView) then
    raise ELoggerPro.Create('WriteToVCLListView requires a TListView instance');
  SetPendingConfigurator('WriteToVCLListView');
  Result := TVCLListViewAppenderConfigurator.Create(Self, TListView(aListView));
end;

function TLoggerProBuilder.WriteToWindowsEventLog: IWindowsEventLogAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToWindowsEventLog');
  Result := TWindowsEventLogAppenderConfigurator.Create(Self);
end;

function TLoggerProBuilder.WriteToWindowsEventLogForService(aService: TObject): IWindowsEventLogAppenderConfigurator;
begin
  if not (aService is TService) then
    raise ELoggerPro.Create('WriteToWindowsEventLogForService requires a TService instance');
  SetPendingConfigurator('WriteToWindowsEventLogForService');
  Result := TWindowsEventLogAppenderConfigurator.Create(Self, TService(aService));
end;

{$ENDIF}

function TLoggerProBuilder.WriteToFireDAC: IFireDACAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToFireDAC');
  Result := TFireDACAppenderConfigurator.Create(Self);
end;

function TLoggerProBuilder.WriteToFileBySource: IFileBySourceAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToFileBySource');
  Result := TFileBySourceAppenderConfigurator.Create(Self);
end;

function TLoggerProBuilder.WriteToHTMLFile: IHTMLFileAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToHTMLFile');
  Result := THTMLFileAppenderConfigurator.Create(Self);
end;

function TLoggerProBuilder.WriteToFilteredAppender(aAppender: ILogAppender): IFilteredAppenderConfigurator;
begin
  SetPendingConfigurator('WriteToFilteredAppender');
  Result := TFilteredAppenderConfigurator.Create(Self, aAppender);
end;

function TLoggerProBuilder.WithDefaultMinimumLevel(aLogLevel: TLogType): ILoggerProBuilder;
begin
  FDefaultLogLevel := aLogLevel;
  Result := Self;
end;

function TLoggerProBuilder.WithMinimumLevel(aLevel: TLogType): ILoggerProBuilder;
begin
  FMinimumLevel := aLevel;
  FMinimumLevelSet := True;
  Result := Self;
end;

function TLoggerProBuilder.WithDefaultRenderer(aRenderer: ILogItemRenderer): ILoggerProBuilder;
begin
  FDefaultRenderer := aRenderer;
  Result := Self;
end;

function TLoggerProBuilder.WithDefaultTag(const aTag: string): ILoggerProBuilder;
begin
  FDefaultTag := aTag;
  Result := Self;
end;

function TLoggerProBuilder.WithStackTraceFormatter(aFormatter: TStackTraceFormatter): ILoggerProBuilder;
begin
  FStackTraceFormatter := aFormatter;
  Result := Self;
end;

function TLoggerProBuilder.GetDefaultRenderer: ILogItemRenderer;
begin
  Result := FDefaultRenderer;
end;

function TLoggerProBuilder.Build: ILogWriter;
var
  lAppendersArray: TArray<ILogAppender>;
  lLogLevelsArray: TArray<TLogType>;
  lLogWriter: TCustomLogWriter;
  I: Integer;
begin
  if not FPendingConfiguratorName.IsEmpty then
    raise ELoggerPro.Create('Appender configurator "' + FPendingConfiguratorName +
      '" was not finalized. Call .Done before calling .Build');

  if FAppenders.Count = 0 then
    raise ELoggerPro.Create('No appenders configured. Add at least one appender before calling Build.');

  // Collect the log levels the configurators already set on each appender.
  // BuildLogWriter(aAppenders) defaults every per-appender level to Debug,
  // which would clobber the levels we honored in ApplyLogLevel (and every
  // "minimumLevel" field in a JSON config). Pass the levels through explicitly.
  SetLength(lAppendersArray, FAppenders.Count);
  SetLength(lLogLevelsArray, FAppenders.Count);
  for I := 0 to FAppenders.Count - 1 do
  begin
    lAppendersArray[I] := FAppenders[I];
    lLogLevelsArray[I] := FAppenders[I].GetMinimumLevel;
  end;

  {$WARN SYMBOL_DEPRECATED OFF}
  Result := BuildLogWriter(lAppendersArray, nil, lLogLevelsArray);
  {$WARN SYMBOL_DEPRECATED ON}
  lLogWriter := Result as TCustomLogWriter;

  // Set minimum log level if configured
  if FMinimumLevelSet then
    lLogWriter.MinimumLevel := FMinimumLevel;

  // Set stack trace formatter if configured
  if Assigned(FStackTraceFormatter) then
    lLogWriter.StackTraceFormatter := FStackTraceFormatter;

  // Wrap with default tag if configured
  if not FDefaultTag.IsEmpty then
    Result := Result.WithDefaultTag(FDefaultTag);
end;

{ TConsoleAppenderConfigurator }

function TConsoleAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): IConsoleAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TConsoleAppenderConfigurator.WithRenderer(aRenderer: ILogItemRenderer): IConsoleAppenderConfigurator;
begin
  FRenderer := aRenderer;
  Result := Self;
end;

function TConsoleAppenderConfigurator.WithRenderer(const aRendererName: string): IConsoleAppenderConfigurator;
var
  lRenderer: ILogItemRenderer;
begin
  if not TryCreateRenderer(aRendererName, lRenderer) then
    raise ELoggerPro.CreateFmt(
      'Unknown renderer "%s". Currently registered: %s. ' +
      'To fix: call LoggerPro.RendererRegistry.RegisterRenderer(''%s'', TYourRenderer) ' +
      'from a unit on the program''s uses clause, or add the renderer''s unit to uses ' +
      '(renderers from optional units self-register in their initialization section).',
      [aRendererName, string.Join(', ', RegisteredRendererNames), aRendererName]);
  Result := WithRenderer(lRenderer);
end;
function TConsoleAppenderConfigurator.WithUTF8Output: IConsoleAppenderConfigurator;
begin
  FUTF8Output := True;
  Result := Self;
end;

function TConsoleAppenderConfigurator.WithColors: IConsoleAppenderConfigurator;
begin
  FUseColors := True;
  FColorScheme := LogColorSchemes.Default;
  Result := Self;
end;

function TConsoleAppenderConfigurator.WithColorScheme(
  const aScheme: TLogColorScheme): IConsoleAppenderConfigurator;
begin
  FUseColors := True;
  FColorScheme := aScheme;
  Result := Self;
end;

function TConsoleAppenderConfigurator.WithPrefix(
  const aPrefix: string): IConsoleAppenderConfigurator;
begin
  FPrefix := aPrefix;
  Result := Self;
end;

function TConsoleAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
  lScheme: TLogColorScheme;
  lGinRenderer: TLogItemRendererGinStyle;
begin
  if FUseColors then
  begin
    // Auto-degrade to plain when stdout is piped/redirected - avoids
    // polluting log files with ANSI escape codes.
    if IsStdoutTerminal then
      lScheme := FColorScheme
    else
      lScheme := LogColorSchemes.Monochrome;
    lGinRenderer := TLogItemRendererGinStyle.Create(lScheme, FPrefix);
    lAppender := TLoggerProConsoleAppender.Create(lGinRenderer);
    (lAppender as TLoggerProConsoleAppender).RendererHandlesColors := True;
  end
  else
  begin
    lAppender := TLoggerProConsoleAppender.Create(GetRenderer);
  end;
  (lAppender as TLoggerProConsoleAppender).UTF8Output := FUTF8Output;
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{ TSimpleConsoleAppenderConfigurator }

function TSimpleConsoleAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): ISimpleConsoleAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TSimpleConsoleAppenderConfigurator.WithUTF8Output: ISimpleConsoleAppenderConfigurator;
begin
  FUTF8Output := True;
  Result := Self;
end;

function TSimpleConsoleAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
begin
  lAppender := TLoggerProSimpleConsoleAppender.Create;
  (lAppender as TLoggerProSimpleConsoleAppender).UTF8Output := FUTF8Output;
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{ TFileAppenderConfigurator }

constructor TFileAppenderConfigurator.Create(aBuilder: TLoggerProBuilder);
begin
  inherited Create(aBuilder);
  FLogsFolder := '';
  FFileBaseName := '';
  FMaxBackupFiles := TLoggerProFileAppender.DEFAULT_MAX_BACKUP_FILE_COUNT;
  FMaxFileSizeInKB := TLoggerProFileAppender.DEFAULT_MAX_FILE_SIZE_KB;
  FEncoding := nil;
  FRotationInterval := TTimeRotationInterval.None;
  FMaxRetainedFiles := 0;
  FFileFormat := '';
end;

function TFileAppenderConfigurator.WithLogsFolder(const aLogsFolder: string): IFileAppenderConfigurator;
begin
  FLogsFolder := aLogsFolder;
  Result := Self;
end;

function TFileAppenderConfigurator.WithFileBaseName(const aFileBaseName: string): IFileAppenderConfigurator;
begin
  FFileBaseName := aFileBaseName;
  Result := Self;
end;

function TFileAppenderConfigurator.WithMaxBackupFiles(aMaxBackupFiles: Integer): IFileAppenderConfigurator;
begin
  FMaxBackupFiles := aMaxBackupFiles;
  Result := Self;
end;

function TFileAppenderConfigurator.WithMaxFileSizeInKB(aMaxFileSizeInKB: Integer): IFileAppenderConfigurator;
begin
  FMaxFileSizeInKB := aMaxFileSizeInKB;
  Result := Self;
end;

function TFileAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): IFileAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TFileAppenderConfigurator.WithEncoding(aEncoding: TEncoding): IFileAppenderConfigurator;
begin
  FEncoding := aEncoding;
  Result := Self;
end;

function TFileAppenderConfigurator.WithInterval(aInterval: TTimeRotationInterval): IFileAppenderConfigurator;
begin
  FRotationInterval := aInterval;
  Result := Self;
end;

function TFileAppenderConfigurator.WithFileFormat(const aFileFormat: string): IFileAppenderConfigurator;
begin
  FFileFormat := aFileFormat;
  Result := Self;
end;

function TFileAppenderConfigurator.WithMaxRetainedFiles(aMaxFiles: Integer): IFileAppenderConfigurator;
begin
  FMaxRetainedFiles := aMaxFiles;
  Result := Self;
end;

function TFileAppenderConfigurator.WithRenderer(aRenderer: ILogItemRenderer): IFileAppenderConfigurator;
begin
  FRenderer := aRenderer;
  Result := Self;
end;

function TFileAppenderConfigurator.WithRenderer(const aRendererName: string): IFileAppenderConfigurator;
var
  lRenderer: ILogItemRenderer;
begin
  if not TryCreateRenderer(aRendererName, lRenderer) then
    raise ELoggerPro.CreateFmt(
      'Unknown renderer "%s". Currently registered: %s. ' +
      'To fix: call LoggerPro.RendererRegistry.RegisterRenderer(''%s'', TYourRenderer) ' +
      'from a unit on the program''s uses clause, or add the renderer''s unit to uses ' +
      '(renderers from optional units self-register in their initialization section).',
      [aRendererName, string.Join(', ', RegisteredRendererNames), aRendererName]);
  Result := WithRenderer(lRenderer);
end;
function TFileAppenderConfigurator.WithOnAfterRotate(aCallback: TFileRotateCallback): IFileAppenderConfigurator;
begin
  FOnAfterRotate := aCallback;
  Result := Self;
end;

function TFileAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
  lFileNameFormat: string;
  lBaseName: string;
begin
  // Determine the file name format
  if not FFileFormat.IsEmpty then
  begin
    // User-provided explicit format
    lFileNameFormat := FFileFormat;
  end
  else if FRotationInterval <> TTimeRotationInterval.None then
  begin
    // Auto-generate format with {date} placeholder
    if FFileBaseName.IsEmpty then
      lBaseName := '{module}'
    else
      lBaseName := FFileBaseName;

    if FMaxFileSizeInKB > 0 then
      lFileNameFormat := lBaseName + '.{date}.{number}.{tag}.log'
    else
      lFileNameFormat := lBaseName + '.{date}.{tag}.log';
  end
  else
  begin
    // Original behavior (no time rotation)
    if FFileBaseName.IsEmpty then
      lFileNameFormat := TLoggerProFileAppenderBase.DEFAULT_FILENAME_FORMAT
    else
      lFileNameFormat := FFileBaseName + '.{number}.{tag}.log';
  end;

  lAppender := TLoggerProFileAppender.Create(
    FMaxBackupFiles,
    FMaxFileSizeInKB,
    FLogsFolder,
    lFileNameFormat,
    GetRenderer,
    FEncoding,
    FRotationInterval,
    FMaxRetainedFiles);
  if Assigned(FOnAfterRotate) then
    (lAppender as TLoggerProFileAppender).OnAfterRotate := FOnAfterRotate;
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{ TJSONLFileAppenderConfigurator }

constructor TJSONLFileAppenderConfigurator.Create(aBuilder: TLoggerProBuilder);
begin
  inherited Create(aBuilder);
  FLogsFolder := '';
  FFileBaseName := '';
  FMaxBackupFiles := TLoggerProJSONLFileAppender.DEFAULT_MAX_BACKUP_FILE_COUNT;
  FMaxFileSizeInKB := TLoggerProJSONLFileAppender.DEFAULT_MAX_FILE_SIZE_KB;
end;

function TJSONLFileAppenderConfigurator.WithLogsFolder(const aLogsFolder: string): IJSONLFileAppenderConfigurator;
begin
  FLogsFolder := aLogsFolder;
  Result := Self;
end;

function TJSONLFileAppenderConfigurator.WithFileBaseName(const aFileBaseName: string): IJSONLFileAppenderConfigurator;
begin
  FFileBaseName := aFileBaseName;
  Result := Self;
end;

function TJSONLFileAppenderConfigurator.WithMaxBackupFiles(aMaxBackupFiles: Integer): IJSONLFileAppenderConfigurator;
begin
  FMaxBackupFiles := aMaxBackupFiles;
  Result := Self;
end;

function TJSONLFileAppenderConfigurator.WithMaxFileSizeInKB(aMaxFileSizeInKB: Integer): IJSONLFileAppenderConfigurator;
begin
  FMaxFileSizeInKB := aMaxFileSizeInKB;
  Result := Self;
end;

function TJSONLFileAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): IJSONLFileAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TJSONLFileAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
  lFileNameFormat: string;
begin
  if FFileBaseName.IsEmpty then
    lFileNameFormat := TLoggerProSimpleFileAppender.DEFAULT_FILENAME_FORMAT
  else
    lFileNameFormat := FFileBaseName + '.{number}.log';
  lAppender := TLoggerProJSONLFileAppender.Create(
    FMaxBackupFiles,
    FMaxFileSizeInKB,
    FLogsFolder,
    lFileNameFormat);
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{ TTimeRotatingFileAppenderConfigurator }

constructor TTimeRotatingFileAppenderConfigurator.Create(aBuilder: TLoggerProBuilder);
begin
  inherited Create(aBuilder);
  FInterval := TTimeRotationInterval.Daily;
  FMaxBackupFiles := TLoggerProTimeRotatingFileAppender.DEFAULT_MAX_BACKUP_FILES;
  FLogsFolder := '';
  FFileBaseName := '';
end;

function TTimeRotatingFileAppenderConfigurator.WithInterval(aInterval: TTimeRotationInterval): ITimeRotatingFileAppenderConfigurator;
begin
  FInterval := aInterval;
  Result := Self;
end;

function TTimeRotatingFileAppenderConfigurator.WithMaxBackupFiles(aMaxBackupFiles: Integer): ITimeRotatingFileAppenderConfigurator;
begin
  FMaxBackupFiles := aMaxBackupFiles;
  Result := Self;
end;

function TTimeRotatingFileAppenderConfigurator.WithLogsFolder(const aLogsFolder: string): ITimeRotatingFileAppenderConfigurator;
begin
  FLogsFolder := aLogsFolder;
  Result := Self;
end;

function TTimeRotatingFileAppenderConfigurator.WithFileBaseName(const aFileBaseName: string): ITimeRotatingFileAppenderConfigurator;
begin
  FFileBaseName := aFileBaseName;
  Result := Self;
end;

function TTimeRotatingFileAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): ITimeRotatingFileAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TTimeRotatingFileAppenderConfigurator.WithRenderer(aRenderer: ILogItemRenderer): ITimeRotatingFileAppenderConfigurator;
begin
  FRenderer := aRenderer;
  Result := Self;
end;

function TTimeRotatingFileAppenderConfigurator.WithRenderer(const aRendererName: string): ITimeRotatingFileAppenderConfigurator;
var
  lRenderer: ILogItemRenderer;
begin
  if not TryCreateRenderer(aRendererName, lRenderer) then
    raise ELoggerPro.CreateFmt(
      'Unknown renderer "%s". Currently registered: %s. ' +
      'To fix: call LoggerPro.RendererRegistry.RegisterRenderer(''%s'', TYourRenderer) ' +
      'from a unit on the program''s uses clause, or add the renderer''s unit to uses ' +
      '(renderers from optional units self-register in their initialization section).',
      [aRendererName, string.Join(', ', RegisteredRendererNames), aRendererName]);
  Result := WithRenderer(lRenderer);
end;
function TTimeRotatingFileAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
begin
  lAppender := TLoggerProTimeRotatingFileAppender.Create(
    FInterval,
    FMaxBackupFiles,
    FLogsFolder,
    FFileBaseName,
    GetRenderer);
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{ TWebhookAppenderConfigurator }

constructor TWebhookAppenderConfigurator.Create(aBuilder: TLoggerProBuilder);
begin
  inherited Create(aBuilder);
  FURL := '';
  FContentType := TWebhookContentType.JSON;
  FTimeoutSeconds := TLoggerProWebhookAppender.DEFAULT_TIMEOUT_SECONDS;
  FRetryCount := TLoggerProWebhookAppender.DEFAULT_MAX_RETRY_COUNT;
  FHeaders := TDictionary<string, string>.Create;
end;

destructor TWebhookAppenderConfigurator.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

function TWebhookAppenderConfigurator.WithURL(const aURL: string): IWebhookAppenderConfigurator;
begin
  FURL := aURL;
  Result := Self;
end;

function TWebhookAppenderConfigurator.WithContentType(aContentType: TWebhookContentType): IWebhookAppenderConfigurator;
begin
  FContentType := aContentType;
  Result := Self;
end;

function TWebhookAppenderConfigurator.WithTimeout(aTimeoutSeconds: Integer): IWebhookAppenderConfigurator;
begin
  FTimeoutSeconds := aTimeoutSeconds;
  Result := Self;
end;

function TWebhookAppenderConfigurator.WithRetryCount(aRetryCount: Integer): IWebhookAppenderConfigurator;
begin
  FRetryCount := aRetryCount;
  Result := Self;
end;

function TWebhookAppenderConfigurator.WithHeader(const aName, aValue: string): IWebhookAppenderConfigurator;
begin
  FHeaders.AddOrSetValue(aName, aValue);
  Result := Self;
end;

function TWebhookAppenderConfigurator.WithAPIKey(const aValue: string;
  aLocation: TWebhookAPIKeyLocation; const aName: string): IWebhookAppenderConfigurator;
begin
  FAPIKey := aValue;
  FAPIKeyLocation := aLocation;
  FAPIKeyName := aName;
  FAPIKeySet := True;
  Result := Self;
end;

function TWebhookAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): IWebhookAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TWebhookAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;  // interface ref pins refcount at >=1 through config
  lConcrete: TLoggerProWebhookAppender;
  lPair: TPair<string, string>;
begin
  if FURL.Trim.IsEmpty then
    raise ELoggerPro.Create('Webhook appender requires a URL. Use WithURL to set it.');

  // Assigning the freshly-created object into an ILogAppender var bumps
  // the refcount from 0 to 1 immediately. Without this pin, the first
  // implicit class->interface conversion inside ApplyLogLevel would create
  // the ONLY interface reference; when that call's temporary is released
  // at the end of the statement, refcount would drop to 0 and the object
  // would be destroyed mid-configuration.
  lAppender := TLoggerProWebhookAppender.Create(FURL, FContentType, FTimeoutSeconds);
  lConcrete := lAppender as TLoggerProWebhookAppender;
  lConcrete.MaxRetryCount := FRetryCount;
  for lPair in FHeaders do
    lConcrete.AddHeader(lPair.Key, lPair.Value);
  if FAPIKeySet then
    lConcrete.SetAPIKey(FAPIKey, FAPIKeyLocation, FAPIKeyName);
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{ TElasticSearchAppenderConfigurator }

constructor TElasticSearchAppenderConfigurator.Create(aBuilder: TLoggerProBuilder);
begin
  inherited Create(aBuilder);
  FURL := '';
  FHost := 'http://localhost';
  FPort := 9200;
  FIndex := 'logs';
  FTimeoutSeconds := TLoggerProElasticSearchAppender.DEFAULT_TIMEOUT_SECONDS;
  FUseHostPortIndex := False;
  FUseBasicAuth := False;
  FUseAPIKey := False;
  FUseBearerToken := False;
end;

function TElasticSearchAppenderConfigurator.WithURL(const aURL: string): IElasticSearchAppenderConfigurator;
begin
  FURL := aURL;
  FUseHostPortIndex := False;
  Result := Self;
end;

function TElasticSearchAppenderConfigurator.WithHost(const aHost: string): IElasticSearchAppenderConfigurator;
begin
  FHost := aHost;
  FUseHostPortIndex := True;
  Result := Self;
end;

function TElasticSearchAppenderConfigurator.WithPort(aPort: Integer): IElasticSearchAppenderConfigurator;
begin
  FPort := aPort;
  FUseHostPortIndex := True;
  Result := Self;
end;

function TElasticSearchAppenderConfigurator.WithIndex(const aIndex: string): IElasticSearchAppenderConfigurator;
begin
  FIndex := aIndex;
  FUseHostPortIndex := True;
  Result := Self;
end;

function TElasticSearchAppenderConfigurator.WithTimeout(aTimeoutSeconds: Integer): IElasticSearchAppenderConfigurator;
begin
  FTimeoutSeconds := aTimeoutSeconds;
  Result := Self;
end;

function TElasticSearchAppenderConfigurator.WithBasicAuth(const aUsername, aPassword: string): IElasticSearchAppenderConfigurator;
begin
  FUseBasicAuth := True;
  FBasicAuthUsername := aUsername;
  FBasicAuthPassword := aPassword;
  FUseAPIKey := False;
  FUseBearerToken := False;
  Result := Self;
end;

function TElasticSearchAppenderConfigurator.WithAPIKey(const aAPIKey: string): IElasticSearchAppenderConfigurator;
begin
  FUseAPIKey := True;
  FAPIKey := aAPIKey;
  FUseBasicAuth := False;
  FUseBearerToken := False;
  Result := Self;
end;

function TElasticSearchAppenderConfigurator.WithBearerToken(const aToken: string): IElasticSearchAppenderConfigurator;
begin
  FUseBearerToken := True;
  FBearerToken := aToken;
  FUseBasicAuth := False;
  FUseAPIKey := False;
  Result := Self;
end;

function TElasticSearchAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): IElasticSearchAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TElasticSearchAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
  lESAppender: TLoggerProElasticSearchAppender;
begin
  if FUseHostPortIndex then
    lAppender := TLoggerProElasticSearchAppender.Create(FHost, FPort, FIndex, FTimeoutSeconds)
  else if not FURL.Trim.IsEmpty then
    lAppender := TLoggerProElasticSearchAppender.Create(FURL, FTimeoutSeconds)
  else
    raise ELoggerPro.Create('ElasticSearch appender requires either a URL or Host/Port/Index configuration.');

  // Configure authentication if provided
  lESAppender := lAppender as TLoggerProElasticSearchAppender;
  if FUseBasicAuth then
    lESAppender.SetBasicAuth(FBasicAuthUsername, FBasicAuthPassword)
  else if FUseAPIKey then
    lESAppender.SetAPIKey(FAPIKey)
  else if FUseBearerToken then
    lESAppender.SetBearerToken(FBearerToken);

  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{ TMemoryAppenderConfigurator }

constructor TMemoryAppenderConfigurator.Create(aBuilder: TLoggerProBuilder);
begin
  inherited Create(aBuilder);
  FMaxSize := TLoggerProMemoryRingBufferAppender.DEFAULT_MAX_SIZE;
end;

function TMemoryAppenderConfigurator.WithMaxSize(aMaxSize: Integer): IMemoryAppenderConfigurator;
begin
  FMaxSize := aMaxSize;
  Result := Self;
end;

function TMemoryAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): IMemoryAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TMemoryAppenderConfigurator.WithRenderer(aRenderer: ILogItemRenderer): IMemoryAppenderConfigurator;
begin
  FRenderer := aRenderer;
  Result := Self;
end;

function TMemoryAppenderConfigurator.WithRenderer(const aRendererName: string): IMemoryAppenderConfigurator;
var
  lRenderer: ILogItemRenderer;
begin
  if not TryCreateRenderer(aRendererName, lRenderer) then
    raise ELoggerPro.CreateFmt(
      'Unknown renderer "%s". Currently registered: %s. ' +
      'To fix: call LoggerPro.RendererRegistry.RegisterRenderer(''%s'', TYourRenderer) ' +
      'from a unit on the program''s uses clause, or add the renderer''s unit to uses ' +
      '(renderers from optional units self-register in their initialization section).',
      [aRendererName, string.Join(', ', RegisteredRendererNames), aRendererName]);
  Result := WithRenderer(lRenderer);
end;
function TMemoryAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
begin
  lAppender := TLoggerProMemoryRingBufferAppender.Create(FMaxSize, GetRenderer);
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{ TCallbackAppenderConfigurator }

function TCallbackAppenderConfigurator.WithCallback(aCallback: TLogItemCallback): ICallbackAppenderConfigurator;
begin
  FCallback := aCallback;
  Result := Self;
end;

function TCallbackAppenderConfigurator.WithSynchronizeToMainThread(aValue: Boolean): ICallbackAppenderConfigurator;
begin
  FSynchronizeToMainThread := aValue;
  Result := Self;
end;

function TCallbackAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): ICallbackAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

constructor TCallbackAppenderConfigurator.Create(aBuilder: TLoggerProBuilder);
begin
  inherited;
  FSynchronizeToMainThread := False;
end;

function TCallbackAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
begin
  if not Assigned(FCallback) then
    raise ELoggerPro.Create('Callback appender requires a callback. Use WithCallback to set it.');
  lAppender := TLoggerProCallbackAppender.Create(FCallback, FSynchronizeToMainThread);
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;


{ TOutputDebugStringAppenderConfigurator }

function TOutputDebugStringAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): IOutputDebugStringAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TOutputDebugStringAppenderConfigurator.WithRenderer(aRenderer: ILogItemRenderer): IOutputDebugStringAppenderConfigurator;
begin
  FRenderer := aRenderer;
  Result := Self;
end;

function TOutputDebugStringAppenderConfigurator.WithRenderer(const aRendererName: string): IOutputDebugStringAppenderConfigurator;
var
  lRenderer: ILogItemRenderer;
begin
  if not TryCreateRenderer(aRendererName, lRenderer) then
    raise ELoggerPro.CreateFmt(
      'Unknown renderer "%s". Currently registered: %s. ' +
      'To fix: call LoggerPro.RendererRegistry.RegisterRenderer(''%s'', TYourRenderer) ' +
      'from a unit on the program''s uses clause, or add the renderer''s unit to uses ' +
      '(renderers from optional units self-register in their initialization section).',
      [aRendererName, string.Join(', ', RegisteredRendererNames), aRendererName]);
  Result := WithRenderer(lRenderer);
end;
function TOutputDebugStringAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
begin
  lAppender := TLoggerProOutputDebugStringAppender.Create(GetRenderer);
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{ TUDPSyslogAppenderConfigurator }

constructor TUDPSyslogAppenderConfigurator.Create(aBuilder: TLoggerProBuilder);
begin
  inherited Create(aBuilder);
  FHost := 'localhost';
  FPort := 514;
  FHostName := '';
  FUserName := '';
  FApplication := '';
  FVersion := '1.0';
  FProcID := '';
  FUseLocalTime := False;
end;

function TUDPSyslogAppenderConfigurator.WithHost(const aHost: string): IUDPSyslogAppenderConfigurator;
begin
  FHost := aHost;
  Result := Self;
end;

function TUDPSyslogAppenderConfigurator.WithPort(aPort: Integer): IUDPSyslogAppenderConfigurator;
begin
  FPort := aPort;
  Result := Self;
end;

function TUDPSyslogAppenderConfigurator.WithHostName(const aHostName: string): IUDPSyslogAppenderConfigurator;
begin
  FHostName := aHostName;
  Result := Self;
end;

function TUDPSyslogAppenderConfigurator.WithUserName(const aUserName: string): IUDPSyslogAppenderConfigurator;
begin
  FUserName := aUserName;
  Result := Self;
end;

function TUDPSyslogAppenderConfigurator.WithApplication(const aApplication: string): IUDPSyslogAppenderConfigurator;
begin
  FApplication := aApplication;
  Result := Self;
end;

function TUDPSyslogAppenderConfigurator.WithVersion(const aVersion: string): IUDPSyslogAppenderConfigurator;
begin
  FVersion := aVersion;
  Result := Self;
end;

function TUDPSyslogAppenderConfigurator.WithProcID(const aProcID: string): IUDPSyslogAppenderConfigurator;
begin
  FProcID := aProcID;
  Result := Self;
end;

function TUDPSyslogAppenderConfigurator.WithUseLocalTime(aUseLocalTime: Boolean): IUDPSyslogAppenderConfigurator;
begin
  FUseLocalTime := aUseLocalTime;
  Result := Self;
end;

function TUDPSyslogAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): IUDPSyslogAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TUDPSyslogAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
begin
  lAppender := TLoggerProUDPSyslogAppender.Create(FHost, FPort, FHostName, FUserName, FApplication, FVersion, FProcID, False, False, FUseLocalTime);
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{ TStringsAppenderConfigurator }

constructor TStringsAppenderConfigurator.Create(aBuilder: TLoggerProBuilder; aStrings: TStrings);
begin
  inherited Create(aBuilder);
  FStrings := aStrings;
  FMaxLogLines := TStringsLogAppender.DEFAULT_MAX_LOG_LINES;
  FClearOnStartup := False;
end;

function TStringsAppenderConfigurator.WithMaxLogLines(aMaxLogLines: Word): IStringsAppenderConfigurator;
begin
  if aMaxLogLines = 0 then
    raise ELoggerPro.Create('MaxLogLines must be greater than zero');
  FMaxLogLines := aMaxLogLines;
  Result := Self;
end;

function TStringsAppenderConfigurator.WithClearOnStartup(aValue: Boolean): IStringsAppenderConfigurator;
begin
  FClearOnStartup := aValue;
  Result := Self;
end;

function TStringsAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): IStringsAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TStringsAppenderConfigurator.WithRenderer(aRenderer: ILogItemRenderer): IStringsAppenderConfigurator;
begin
  FRenderer := aRenderer;
  Result := Self;
end;

function TStringsAppenderConfigurator.WithRenderer(const aRendererName: string): IStringsAppenderConfigurator;
var
  lRenderer: ILogItemRenderer;
begin
  if not TryCreateRenderer(aRendererName, lRenderer) then
    raise ELoggerPro.CreateFmt(
      'Unknown renderer "%s". Currently registered: %s. ' +
      'To fix: call LoggerPro.RendererRegistry.RegisterRenderer(''%s'', TYourRenderer) ' +
      'from a unit on the program''s uses clause, or add the renderer''s unit to uses ' +
      '(renderers from optional units self-register in their initialization section).',
      [aRendererName, string.Join(', ', RegisteredRendererNames), aRendererName]);
  Result := WithRenderer(lRenderer);
end;
function TStringsAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
begin
  lAppender := TStringsLogAppender.Create(FStrings, FMaxLogLines, FClearOnStartup, GetRenderer);
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{$IF Defined(MSWINDOWS)}

{ TVCLMemoAppenderConfigurator }

constructor TVCLMemoAppenderConfigurator.Create(aBuilder: TLoggerProBuilder; aMemo: TMemo);
begin
  inherited Create(aBuilder);
  FMemo := aMemo;
  FMaxLogLines := 100;
  FClearOnStartup := False;
end;

function TVCLMemoAppenderConfigurator.WithMaxLogLines(aMaxLogLines: Word): IVCLMemoAppenderConfigurator;
begin
  FMaxLogLines := aMaxLogLines;
  Result := Self;
end;

function TVCLMemoAppenderConfigurator.WithClearOnStartup(aValue: Boolean): IVCLMemoAppenderConfigurator;
begin
  FClearOnStartup := aValue;
  Result := Self;
end;

function TVCLMemoAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): IVCLMemoAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TVCLMemoAppenderConfigurator.WithRenderer(aRenderer: ILogItemRenderer): IVCLMemoAppenderConfigurator;
begin
  FRenderer := aRenderer;
  Result := Self;
end;

function TVCLMemoAppenderConfigurator.WithRenderer(const aRendererName: string): IVCLMemoAppenderConfigurator;
var
  lRenderer: ILogItemRenderer;
begin
  if not TryCreateRenderer(aRendererName, lRenderer) then
    raise ELoggerPro.CreateFmt(
      'Unknown renderer "%s". Currently registered: %s. ' +
      'To fix: call LoggerPro.RendererRegistry.RegisterRenderer(''%s'', TYourRenderer) ' +
      'from a unit on the program''s uses clause, or add the renderer''s unit to uses ' +
      '(renderers from optional units self-register in their initialization section).',
      [aRendererName, string.Join(', ', RegisteredRendererNames), aRendererName]);
  Result := WithRenderer(lRenderer);
end;
function TVCLMemoAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
begin
  lAppender := TVCLMemoLogAppender.Create(FMemo, FMaxLogLines, FClearOnStartup, GetRenderer);
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{ TVCLListBoxAppenderConfigurator }

constructor TVCLListBoxAppenderConfigurator.Create(aBuilder: TLoggerProBuilder; aListBox: TListBox);
begin
  inherited Create(aBuilder);
  FListBox := aListBox;
  FMaxLogLines := 500;
end;

function TVCLListBoxAppenderConfigurator.WithMaxLogLines(aMaxLogLines: Word): IVCLListBoxAppenderConfigurator;
begin
  FMaxLogLines := aMaxLogLines;
  Result := Self;
end;

function TVCLListBoxAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): IVCLListBoxAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TVCLListBoxAppenderConfigurator.WithRenderer(aRenderer: ILogItemRenderer): IVCLListBoxAppenderConfigurator;
begin
  FRenderer := aRenderer;
  Result := Self;
end;

function TVCLListBoxAppenderConfigurator.WithRenderer(const aRendererName: string): IVCLListBoxAppenderConfigurator;
var
  lRenderer: ILogItemRenderer;
begin
  if not TryCreateRenderer(aRendererName, lRenderer) then
    raise ELoggerPro.CreateFmt(
      'Unknown renderer "%s". Currently registered: %s. ' +
      'To fix: call LoggerPro.RendererRegistry.RegisterRenderer(''%s'', TYourRenderer) ' +
      'from a unit on the program''s uses clause, or add the renderer''s unit to uses ' +
      '(renderers from optional units self-register in their initialization section).',
      [aRendererName, string.Join(', ', RegisteredRendererNames), aRendererName]);
  Result := WithRenderer(lRenderer);
end;
function TVCLListBoxAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
begin
  lAppender := TVCLListBoxAppender.Create(FListBox, FMaxLogLines, GetRenderer);
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{ TVCLListViewAppenderConfigurator }

constructor TVCLListViewAppenderConfigurator.Create(aBuilder: TLoggerProBuilder; aListView: TListView);
begin
  inherited Create(aBuilder);
  FListView := aListView;
  FMaxLogLines := 500;
end;

function TVCLListViewAppenderConfigurator.WithMaxLogLines(aMaxLogLines: Word): IVCLListViewAppenderConfigurator;
begin
  FMaxLogLines := aMaxLogLines;
  Result := Self;
end;

function TVCLListViewAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): IVCLListViewAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TVCLListViewAppenderConfigurator.WithRenderer(aRenderer: ILogItemRenderer): IVCLListViewAppenderConfigurator;
begin
  FRenderer := aRenderer;
  Result := Self;
end;

function TVCLListViewAppenderConfigurator.WithRenderer(const aRendererName: string): IVCLListViewAppenderConfigurator;
var
  lRenderer: ILogItemRenderer;
begin
  if not TryCreateRenderer(aRendererName, lRenderer) then
    raise ELoggerPro.CreateFmt(
      'Unknown renderer "%s". Currently registered: %s. ' +
      'To fix: call LoggerPro.RendererRegistry.RegisterRenderer(''%s'', TYourRenderer) ' +
      'from a unit on the program''s uses clause, or add the renderer''s unit to uses ' +
      '(renderers from optional units self-register in their initialization section).',
      [aRendererName, string.Join(', ', RegisteredRendererNames), aRendererName]);
  Result := WithRenderer(lRenderer);
end;
function TVCLListViewAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
begin
  lAppender := TVCLListViewAppender.Create(FListView, FMaxLogLines, GetRenderer);
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{ TWindowsEventLogAppenderConfigurator }

constructor TWindowsEventLogAppenderConfigurator.Create(aBuilder: TLoggerProBuilder);
begin
  inherited Create(aBuilder);
  FService := nil;
  FSourceName := '';
  FUseService := False;
end;

constructor TWindowsEventLogAppenderConfigurator.Create(aBuilder: TLoggerProBuilder; aService: TService);
begin
  inherited Create(aBuilder);
  FService := aService;
  FSourceName := '';
  FUseService := True;
end;

function TWindowsEventLogAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): IWindowsEventLogAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TWindowsEventLogAppenderConfigurator.WithSourceName(const aSourceName: string): IWindowsEventLogAppenderConfigurator;
begin
  FSourceName := aSourceName;
  Result := Self;
end;

function TWindowsEventLogAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
begin
  if FUseService then
    lAppender := TLoggerProWindowsEventLogAppender.Create(FService)
  else
    lAppender := TLoggerProWindowsEventLogAppender.Create(FSourceName);
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{$ENDIF}

{ TFireDACAppenderConfigurator }

function TFireDACAppenderConfigurator.WithConnectionDefName(const aConnectionDefName: string): IFireDACAppenderConfigurator;
begin
  FConnectionDefName := aConnectionDefName;
  Result := Self;
end;

function TFireDACAppenderConfigurator.WithStoredProcName(const aStoredProcName: string): IFireDACAppenderConfigurator;
begin
  FStoredProcName := aStoredProcName;
  Result := Self;
end;

function TFireDACAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): IFireDACAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TFireDACAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
begin
  if FConnectionDefName.Trim.IsEmpty then
    raise ELoggerPro.Create('FireDAC appender requires a ConnectionDefName. Use WithConnectionDefName to set it.');
  if FStoredProcName.Trim.IsEmpty then
    raise ELoggerPro.Create('FireDAC appender requires a StoredProcName. Use WithStoredProcName to set it.');

  lAppender := TLoggerProDBAppenderFireDAC.Create(FConnectionDefName, FStoredProcName, nil);
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{ TFileBySourceAppenderConfigurator }

constructor TFileBySourceAppenderConfigurator.Create(aBuilder: TLoggerProBuilder);
begin
  inherited Create(aBuilder);
  FLogsFolder := '';
  FMaxFileSizeInKB := TLoggerProFileBySourceAppender.DEFAULT_MAX_FILE_SIZE_KB;
  FRetainDays := TLoggerProFileBySourceAppender.DEFAULT_RETAIN_DAYS;
  FDefaultSource := TLoggerProFileBySourceAppender.DEFAULT_SOURCE;
end;

function TFileBySourceAppenderConfigurator.WithLogsFolder(
  const aLogsFolder: string): IFileBySourceAppenderConfigurator;
begin
  FLogsFolder := aLogsFolder;
  Result := Self;
end;

function TFileBySourceAppenderConfigurator.WithMaxFileSizeInKB(
  aMaxFileSizeInKB: Integer): IFileBySourceAppenderConfigurator;
begin
  FMaxFileSizeInKB := aMaxFileSizeInKB;
  Result := Self;
end;

function TFileBySourceAppenderConfigurator.WithRetainDays(
  aRetainDays: Integer): IFileBySourceAppenderConfigurator;
begin
  FRetainDays := aRetainDays;
  Result := Self;
end;

function TFileBySourceAppenderConfigurator.WithDefaultSource(
  const aDefaultSource: string): IFileBySourceAppenderConfigurator;
begin
  FDefaultSource := aDefaultSource;
  Result := Self;
end;

function TFileBySourceAppenderConfigurator.WithMinimumLevel(
  aLogLevel: TLogType): IFileBySourceAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function TFileBySourceAppenderConfigurator.WithRenderer(
  aRenderer: ILogItemRenderer): IFileBySourceAppenderConfigurator;
begin
  FRenderer := aRenderer;
  Result := Self;
end;

function TFileBySourceAppenderConfigurator.WithRenderer(const aRendererName: string): IFileBySourceAppenderConfigurator;
var
  lRenderer: ILogItemRenderer;
begin
  if not TryCreateRenderer(aRendererName, lRenderer) then
    raise ELoggerPro.CreateFmt(
      'Unknown renderer "%s". Currently registered: %s. ' +
      'To fix: call LoggerPro.RendererRegistry.RegisterRenderer(''%s'', TYourRenderer) ' +
      'from a unit on the program''s uses clause, or add the renderer''s unit to uses ' +
      '(renderers from optional units self-register in their initialization section).',
      [aRendererName, string.Join(', ', RegisteredRendererNames), aRendererName]);
  Result := WithRenderer(lRenderer);
end;

function TFileBySourceAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
begin
  lAppender := TLoggerProFileBySourceAppender.Create(
    FLogsFolder, FMaxFileSizeInKB, FRetainDays, FDefaultSource,
    GetRenderer);
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{ THTMLFileAppenderConfigurator }

constructor THTMLFileAppenderConfigurator.Create(aBuilder: TLoggerProBuilder);
begin
  inherited Create(aBuilder);
  FLogsFolder := '';
  FFileBaseName := '';
  FTitle := 'LoggerPro';
  FMaxBackupFiles := TLoggerProFileAppenderBase.DEFAULT_MAX_BACKUP_FILE_COUNT;
  FMaxFileSizeInKB := TLoggerProFileAppenderBase.DEFAULT_MAX_FILE_SIZE_KB;
  FInterval := TTimeRotationInterval.None;
  FMaxRetainedFiles := 0;
end;

function THTMLFileAppenderConfigurator.WithLogsFolder(const aLogsFolder: string): IHTMLFileAppenderConfigurator;
begin
  FLogsFolder := aLogsFolder;
  Result := Self;
end;

function THTMLFileAppenderConfigurator.WithFileBaseName(const aFileBaseName: string): IHTMLFileAppenderConfigurator;
begin
  FFileBaseName := aFileBaseName;
  Result := Self;
end;

function THTMLFileAppenderConfigurator.WithTitle(const aTitle: string): IHTMLFileAppenderConfigurator;
begin
  FTitle := aTitle;
  Result := Self;
end;

function THTMLFileAppenderConfigurator.WithMaxBackupFiles(aMaxBackupFiles: Integer): IHTMLFileAppenderConfigurator;
begin
  FMaxBackupFiles := aMaxBackupFiles;
  Result := Self;
end;

function THTMLFileAppenderConfigurator.WithMaxFileSizeInKB(aMaxFileSizeInKB: Integer): IHTMLFileAppenderConfigurator;
begin
  FMaxFileSizeInKB := aMaxFileSizeInKB;
  Result := Self;
end;

function THTMLFileAppenderConfigurator.WithInterval(aInterval: TTimeRotationInterval): IHTMLFileAppenderConfigurator;
begin
  FInterval := aInterval;
  Result := Self;
end;

function THTMLFileAppenderConfigurator.WithMaxRetainedFiles(aMaxFiles: Integer): IHTMLFileAppenderConfigurator;
begin
  FMaxRetainedFiles := aMaxFiles;
  Result := Self;
end;

function THTMLFileAppenderConfigurator.WithMinimumLevel(aLogLevel: TLogType): IHTMLFileAppenderConfigurator;
begin
  FLogLevel := aLogLevel;
  FLogLevelSet := True;
  Result := Self;
end;

function THTMLFileAppenderConfigurator.Done: ILoggerProBuilder;
var
  lAppender: ILogAppender;
  lFormat: string;
begin
  if FFileBaseName.IsEmpty then
    lFormat := TLoggerProHTMLFileAppender.DEFAULT_FILENAME_FORMAT
  else
    lFormat := FFileBaseName + '.{number}.html';
  lAppender := TLoggerProHTMLFileAppender.Create(
    FTitle, FMaxBackupFiles, FMaxFileSizeInKB,
    FLogsFolder, lFormat, FInterval, FMaxRetainedFiles);
  ApplyLogLevel(lAppender);
  FBuilder.InternalAddAppender(lAppender);
  Result := FBuilder;
end;

{ TFilteredAppenderConfigurator }

constructor TFilteredAppenderConfigurator.Create(aBuilder: TLoggerProBuilder; aAppender: ILogAppender);
begin
  inherited Create(aBuilder);
  FInnerAppender := aAppender;
end;

function TFilteredAppenderConfigurator.WithFilter(aFilter: TLogItemFilterFunc): IFilteredAppenderConfigurator;
begin
  FFilter := aFilter;
  Result := Self;
end;

function TFilteredAppenderConfigurator.Done: ILoggerProBuilder;
var
  lFilteredAppender: ILogAppender;
begin
  if not Assigned(FFilter) then
    raise ELoggerPro.Create('Filtered appender requires a filter function. Use WithFilter to set it.');

  lFilteredAppender := TLoggerProFilter.Build(FInnerAppender, FFilter);
  ApplyLogLevel(lFilteredAppender);
  FBuilder.InternalAddAppender(lFilteredAppender);
  Result := FBuilder;
end;

{ Helper function }

function LoggerProBuilder: ILoggerProBuilder;
begin
  Result := TLoggerProBuilder.Create;
end;

end.
