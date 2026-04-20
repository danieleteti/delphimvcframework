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

unit LoggerPro;

{$SCOPEDENUMS ON}

interface

uses
  System.Generics.Collections,
  System.Classes,
  System.Rtti,
  System.SyncObjs,
  ThreadSafeQueueU, System.SysUtils;

const
  DEFAULT_LOG_TAG = 'main';

{$I loggerprobuildconsts.inc}  

var
  DefaultLoggerProMainQueueSize: Cardinal = 50000;
  DefaultLoggerProAppenderQueueSize: Cardinal = 50000;

type
  TLogType = (Debug = 0, Info, Warning, Error, Fatal);
  TLogErrorReason = (QueueFull);
  TLogErrorAction = (SkipNewest, DiscardOlder);
  TLogExtendedInfo = (EIUserName, EIComputerName, EIProcessName, EIProcessID, EIDeviceID { mobile });
  TLoggerProExtendedInfo = set of TLogExtendedInfo;

  { @abstract(Time rotation intervals for file appenders)
    Used by WriteToFile.WithInterval() and WriteToTimeRotatingFile. }
  TTimeRotationInterval = (
    None,       // No time-based rotation (size only)
    Hourly,     // Rotate every hour: app.2025120312.log
    Daily,      // Rotate every day: app.20251203.log
    Weekly,     // Rotate every week: app.2025W49.log
    Monthly     // Rotate every month: app.202512.log
  );

  { @abstract(Represents a key-value pair for structured logging context) }
  LogParam = record
    Key: string;
    Value: TValue;
    // NOT inline on purpose: inline-expanding these at the call site would
    // require every caller (sample projects, user applications, DMVC, etc.)
    // to add System.Rtti to their own uses clause - just because TValue is
    // declared there. Keeping them as regular class functions shields users
    // from that implementation leak. The cost (one extra call) is noise
    // compared to the TValue construction happening inside.
    class function S(const AKey: string; const AValue: string): LogParam; static;
    class function I(const AKey: string; const AValue: Integer): LogParam; static;
    class function B(const AKey: string; const AValue: Boolean): LogParam; static;
    class function F(const AKey: string; const AValue: Double): LogParam; static;
    class function D(const AKey: string; const AValue: TDateTime): LogParam; static;
    class function FmtS(const AKey: string; const AFormat: string; const AArgs: array of const): LogParam; static;
    class function V(const AKey: string; const AValue: TValue): LogParam; static;
  end;
  LogParams = TArray<LogParam>;

  { @abstract(Function type for pluggable stack trace formatters)
    Assign a function to extract stack traces using JCL, madExcept, EurekaLog, or
    any other library. If no formatter is set, exception logging will only
    include the exception class name and message.

    Example with JCL:
    @longcode(#
    Log := LoggerProBuilder
      .WithStackTraceFormatter(
        function(E: Exception): string
        begin
          Result := JclLastExceptStackListToString;
        end)
      .WriteToConsole.Done
      .Build;
    #) }
  TStackTraceFormatter = TFunc<Exception, string>;

  { @abstract(Represent the single log item)
    Each call to some kind of log method is wrapped in a @link(TLogItem)
    instance and passed down the layour of LoggerPro. }
  TLogItem = class sealed
  private
    FType: TLogType;
    FMessage: string;
    FTag: string;
    FTimeStamp: TDateTime;
    FThreadID: TThreadID;
    FContext: LogParams;
    FPreRenderedContext: string;
    function GetLogTypeAsString: string;
  public
    constructor Create(const aType: TLogType; const aMessage: string; const aTag: string); overload;
    constructor Create(const aType: TLogType; const aMessage: string; const aTag: string; const aTimeStamp: TDateTime;
      const aThreadID: TThreadID); overload;
    constructor Create(const aType: TLogType; const aMessage: string; const aTag: string;
      const aContext: LogParams); overload;
    constructor Create(const aType: TLogType; const aMessage: string; const aTag: string; const aTimeStamp: TDateTime;
      const aThreadID: TThreadID; const aContext: LogParams); overload;

    function Clone: TLogItem;
    { @abstract(The type of the log)
      Log can be one of the following types:
      @unorderedlist(
      @item(DEBUG)
      @item(INFO)
      @item(WARNING)
      @item(ERROR)
      @item(FATAL)
      ) }
    property LogType: TLogType read FType;
    { @abstract(The text of the log message) }
    property LogMessage: string read FMessage;
    { @abstract(The tag of the log message) }
    property LogTag: string read FTag;
    { @abstract(The timestamp when the @link(TLogItem) is generated) }
    property TimeStamp: TDateTime read FTimeStamp;
    { @abstract(The IDof the thread which generated the log item) }
    property ThreadID: TThreadID read FThreadID;
    { @abstract(The type of the log converted in string) }
    property LogTypeAsString: string read GetLogTypeAsString;
    { @abstract(The structured context key-value pairs) }
    property Context: LogParams read FContext;
    { @abstract(Pre-rendered context string for fixed context optimization) }
    property PreRenderedContext: string read FPreRenderedContext write FPreRenderedContext;
    { @abstract(Returns true if the log item has context data) }
    function HasContext: Boolean; inline;
  end;

  ILogItemRenderer = interface
    ['{CBD22D22-C387-4A97-AD5D-4945B812FDB0}']
    procedure Setup;
    procedure TearDown;
    function RenderLogItem(const aLogItem: TLogItem): String;
  end;

  TLoggerProAppenderErrorEvent = reference to procedure(const AppenderClassName: string; const aFailedLogItem: TLogItem;
    const Reason: TLogErrorReason; var Action: TLogErrorAction);

  TLoggerProEventsHandler = class sealed
  public
    OnAppenderError: TLoggerProAppenderErrorEvent;
  end;

  { @abstract(Interface implemented by all the classes used as appenders) }
  ILogAppender = interface
    ['{58AFB557-C594-4A4B-8DC9-0F13B37F60CB}']
    { @abstract(This method is internally called by LoggerPro to initialize the appender) }
    procedure Setup;
    { @abstract(This method is called at each log item represented by @link(TLogItem))
      The appender should be as-fast-as-it-can to handle the message, however
      each appender runs in a separated thread. }
    procedure WriteLog(const aLogItem: TLogItem);
    { @abstract(This method is internally called by LoggerPro to deinitialize the appender) }
    procedure TearDown;
    { @abstract(Enable or disable the log appender) }
    procedure SetEnabled(const Value: Boolean);
    { @abstract(Returns if the logappender is currently enabled or not) }
    function IsEnabled: Boolean;
    { @abstract(Set the minimum severity level this appender will emit.
      Messages whose severity is below this threshold are dropped at this
      appender (but may still reach other appenders). Setting a value
      below the global LogWriter <c>MinimumLevel</c> is a no-op, because
      the global gate filters messages before they reach any appender. }
    procedure SetMinimumLevel(const Value: TLogType);
    { @abstract(Get the minimum severity level this appender emits. }
    function GetMinimumLevel: TLogType;
    { @abstract(If the appender is disabled, this method is called at each new
      logitem. This method should not raise exceptions and should try to restart the appender
      at specified time and only if some appropriate seconds/miutes are elapsed between the
      LastErrorTimestamp. }
    procedure TryToRestart(var Restarted: Boolean);

    procedure SetLastErrorTimeStamp(const LastErrorTimeStamp: TDateTime);
    function GetLastErrorTimeStamp: TDateTime;
    property LastErrorTimeStamp: TDateTime read GetLastErrorTimeStamp write SetLastErrorTimeStamp;
  end;

  ELoggerPro = class(Exception)

  end;

  /// <summary>Raised by the JSON config facade (LoggerProFromJSONFile,
  /// LoggerProFromJSONString and the underlying TLoggerProConfig).
  /// Declared here so callers using only `uses LoggerPro;` can catch
  /// it without pulling in LoggerPro.Config.</summary>
  ELoggerProConfigError = class(ELoggerPro);

  TAppenderQueue = class(TThreadSafeQueue<TLogItem>)
  end;

  ICustomLogWriter = interface
    ['{8BC2F462-E2A3-4117-BE1F-03EB31997BB4}']
    function GetAppendersClassNames: TArray<string>;
    function GetAppenders(const aIndex: Integer): ILogAppender;
    property Appenders[const aIndex: Integer]: ILogAppender read GetAppenders;
    procedure AddAppender(const aAppenders: ILogAppender);
    procedure DelAppender(const aAppenders: ILogAppender);
    function AppendersCount(): Integer;

    // Base logging method
    procedure Log(const aType: TLogType; const aMessage: string; const aTag: string); overload;
    // Enqueue a pre-built log item (for internal use with pre-rendered context)
    procedure EnqueueLogItem(const aLogItem: TLogItem);

    // Global gate level. Reading: lowest level that will pass the gate.
    // Writing: changes the gate at runtime - useful for bridges (e.g. the
    // DMVC MVCFramework.Logger.UseLoggerVerbosityLevel setter) that need
    // to honor a verbosity change without rebuilding the writer.
    function GetMinimumLevel: TLogType;
    procedure SetMinimumLevel(const aLevel: TLogType);
    property MinimumLevel: TLogType read GetMinimumLevel write SetMinimumLevel;
  end;

  ILogWriter = interface(ICustomLogWriter)
    ['{A717A040-4493-458F-91B2-6F6E2AFB496F}']
    { Logging methods without tag - uses default tag }
    procedure Debug(const aMessage: string); overload;
    procedure Info(const aMessage: string); overload;
    procedure Warn(const aMessage: string); overload;
    procedure Error(const aMessage: string); overload;
    procedure Fatal(const aMessage: string); overload;

    { Logging methods with explicit tag }
    procedure Debug(const aMessage: string; const aTag: string); overload;
    procedure Debug(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Debug(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Info(const aMessage: string; const aTag: string); overload;
    procedure Info(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Info(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Warn(const aMessage: string; const aTag: string); overload;
    procedure Warn(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Warn(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Error(const aMessage: string; const aTag: string); overload;
    procedure Error(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Error(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Fatal(const aMessage: string; const aTag: string); overload;
    procedure Fatal(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Fatal(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    { Exception logging - logs exception with optional stack trace if provider is set }
    procedure LogException(const E: Exception); overload;
    procedure LogException(const E: Exception; const aMessage: string); overload;
    procedure LogException(const E: Exception; const aMessage: string; const aTag: string); overload;

    procedure Log(const aType: TLogType; const aMessage: string; const aTag: string); overload;
    procedure Log(const aType: TLogType; const aMessage: string; const aParams: array of const; const aTag: string); overload;
    procedure Log(const aType: TLogType; const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    { @abstract(Returns a new ILogWriter with bound context. Thread-safe - creates a new instance.) }
    function WithProperty(const aKey: string; const aValue: string): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: Integer): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: Boolean): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: Double): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: TDateTime): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: TValue): ILogWriter; overload;
    function WithPropertyFmt(const aKey: string; const aFormat: string; const aArgs: array of const): ILogWriter;

    { @abstract(Returns a new ILogWriter with a custom default tag. Thread-safe - creates a new instance.) }
    function WithDefaultTag(const aTag: string): ILogWriter;

    { @abstract(Returns a new ILogWriter with a default context. All log calls will include this context.) }
    function WithDefaultContext(const aContext: array of LogParam): ILogWriter;

    procedure Disable;
    procedure Enable;

    { @abstract(Returns True if a message at this level would actually be
      dispatched. Useful to guard expensive log-argument construction:
      @longcode(#
      if Log.IsDebugEnabled then
        Log.Debug('Trace: %s', [ExpensiveToString]);
      #)
      Returns False while shutting down, while disabled, or when the level
      is below the configured minimum. All wrapper writers (WithProperty,
      WithDefaultTag, WithDefaultContext) delegate to the root writer so
      the result reflects the true dispatch decision.) }
    function IsEnabled(aLevel: TLogType): Boolean;
    function IsDebugEnabled: Boolean;
    function IsInfoEnabled: Boolean;
    function IsWarningEnabled: Boolean;
    function IsErrorEnabled: Boolean;
    function IsFatalEnabled: Boolean;

    { @abstract(Forces flush and shutdown of the logger thread, regardless of reference count.
      After calling Shutdown, any logging attempt will be silently ignored (Release) or Assert (Debug).
      Use this in finalization to ensure all logs are written before the application exits.) }
    procedure Shutdown;
  end;

  TLogAppenderList = TList<ILogAppender>;

  TAppenderThread = class(TThread)
  private
    FLogAppender: ILogAppender;
    FAppenderQueue: TAppenderQueue;
    FFailing: Boolean;
    FReadyEvent: TEvent;
    procedure SetFailing(const Value: Boolean);
  protected
    procedure Execute; override;

    type
      TAppenderStatus = (BeforeSetup, Running, WaitAfterFail, ToRestart, BeforeTearDown);
  public
    constructor Create(aLogAppender: ILogAppender; aAppenderQueue: TAppenderQueue);
    destructor Destroy; override;
    { Wait until the thread has attempted its first Setup. Guarantees that
      when the caller proceeds, the appender is either Running (Setup OK)
      or in WaitAfterFail (Setup failed repeatedly). Either way the thread
      is scheduled and observing its queue - no log item enqueued after
      this point can be lost to "thread not scheduled yet" races. }
    function WaitUntilReady(aTimeoutMs: Cardinal): Boolean;
    property Failing: Boolean read FFailing write SetFailing;
  end;

  TLoggerThread = class(TThread)
  private type
      TAppenderAdapter = class
      private
        FAppenderQueue: TAppenderQueue;
        FAppenderThread: TAppenderThread;
        FLogAppender: ILogAppender;
        FFailsCount: Cardinal;
      public
        constructor Create(aAppender: ILogAppender); virtual;
        destructor Destroy; override;
        function EnqueueLog(const aLogItem: TLogItem): Boolean;
        property Queue: TAppenderQueue read FAppenderQueue;
        property FailsCount: Cardinal read FFailsCount;
        function GetMinimumLevel: TLogType;
        procedure Terminate;
        procedure SignalTerminate;
        procedure WaitForExit;
      end;

      TAppenderAdapterList = class(TObjectList<TAppenderAdapter>)
      public
        constructor Create;
      end;

  private
    FQueue: TThreadSafeQueue<TLogItem>;
    FAppenders: TLogAppenderList;
    FEventsHandlers: TLoggerProEventsHandler;
    FAppendersDecorators: TObjectList<TAppenderAdapter>;
    function BuildAppenderAdapters: TAppenderAdapterList;
    procedure DispatchLogItem(const aLogItem: TLogItem);
    procedure DoOnAppenderError(const FailAppenderClassName: string; const aFailedLogItem: TLogItem; const aReason: TLogErrorReason;
      var aAction: TLogErrorAction);
    procedure SetEventsHandlers(const Value: TLoggerProEventsHandler);
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create(aAppenders: TLogAppenderList);
    destructor Destroy; override;

    property EventsHandlers: TLoggerProEventsHandler read FEventsHandlers write SetEventsHandlers;
    property LogWriterQueue: TThreadSafeQueue < TLogItem > read FQueue;
  end;

  TLoggerProInterfacedObject = class(TInterfacedObject)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  TCustomLogWriter = class(TLoggerProInterfacedObject, ICustomLogWriter)
  strict private
    FLoggerThread: TLoggerThread;
    FLogAppenders: TLogAppenderList;
    FFreeAllowed: Boolean;
    FLock: TObject;
    FStackTraceFormatter: TStackTraceFormatter;
    function GetAppendersClassNames: TArray<string>;
  protected
    FEnabled: Boolean;
    FLogLevel: TLogType;
    FShuttingDown: Boolean;
    procedure Initialize(const aEventsHandler: TLoggerProEventsHandler);
    function FormatExceptionMessage(const E: Exception; const aMessage: string): string;
  public
    constructor Create(const aLogLevel: TLogType = TLogType.Debug); overload;
    constructor Create(const aLogAppenders: TLogAppenderList; const aLogLevel: TLogType = TLogType.Debug); overload;
    destructor Destroy; override;

    function GetAppenders(const aIndex: Integer): ILogAppender;
    procedure AddAppender(const aAppender: ILogAppender);
    procedure DelAppender(const aAppender: ILogAppender);
    function AppendersCount(): Integer;

    procedure Log(const aType: TLogType; const aMessage: string; const aTag: string); overload;
    procedure Log(const aType: TLogType; const aMessage: string; const aTag: string; const aContext: LogParams); overload;
    procedure EnqueueLogItem(const aLogItem: TLogItem);

    procedure Disable; virtual;
    procedure Enable; virtual;
    procedure Shutdown; virtual;

    function GetMinimumLevel: TLogType;
    procedure SetMinimumLevel(const aLevel: TLogType);

    property StackTraceFormatter: TStackTraceFormatter read FStackTraceFormatter write FStackTraceFormatter;
    property MinimumLevel: TLogType read GetMinimumLevel write SetMinimumLevel;
  end;

  TLogWriter = class(TCustomLogWriter, ILogWriter)
  public
    { Logging methods without tag - uses DEFAULT_LOG_TAG }
    procedure Debug(const aMessage: string); overload;
    procedure Info(const aMessage: string); overload;
    procedure Warn(const aMessage: string); overload;
    procedure Error(const aMessage: string); overload;
    procedure Fatal(const aMessage: string); overload;

    { Logging methods with explicit tag }
    procedure Debug(const aMessage: string; const aTag: string); overload;
    procedure Debug(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Debug(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Info(const aMessage: string; const aTag: string); overload;
    procedure Info(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Info(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Warn(const aMessage: string; const aTag: string); overload;
    procedure Warn(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Warn(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Error(const aMessage: string; const aTag: string); overload;
    procedure Error(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Error(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Fatal(const aMessage: string; const aTag: string); overload;
    procedure Fatal(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Fatal(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure LogException(const E: Exception); overload;
    procedure LogException(const E: Exception; const aMessage: string); overload;
    procedure LogException(const E: Exception; const aMessage: string; const aTag: string); overload;

    procedure Log(const aType: TLogType; const aMessage: string; const aParams: array of const; const aTag: string); overload;
    procedure Log(const aType: TLogType; const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    function WithProperty(const aKey: string; const aValue: string): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: Integer): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: Boolean): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: Double): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: TDateTime): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: TValue): ILogWriter; overload;
    function WithPropertyFmt(const aKey: string; const aFormat: string; const aArgs: array of const): ILogWriter;

    function WithDefaultTag(const aTag: string): ILogWriter;
    function WithDefaultContext(const aContext: array of LogParam): ILogWriter;

    function IsEnabled(aLevel: TLogType): Boolean;
    function IsDebugEnabled: Boolean;
    function IsInfoEnabled: Boolean;
    function IsWarningEnabled: Boolean;
    function IsErrorEnabled: Boolean;
    function IsFatalEnabled: Boolean;
  end;

  { @abstract(Wrapper for ILogWriter with bound context) }
  TLogWriterWithContext = class(TInterfacedObject, ILogWriter)
  private
    FInner: ILogWriter;
    FContext: LogParams;
    FPreRenderedContext: string;
    function MergeContext(const aContext: array of LogParam): LogParams;
    class function RenderContextToString(const AContext: LogParams): string; static;
  public
    constructor Create(const AInner: ILogWriter; const AContext: LogParams);

    // ICustomLogWriter
    function GetAppendersClassNames: TArray<string>;
    function GetAppenders(const aIndex: Integer): ILogAppender;
    procedure AddAppender(const aAppenders: ILogAppender);
    procedure DelAppender(const aAppenders: ILogAppender);
    function AppendersCount(): Integer;
    procedure Log(const aType: TLogType; const aMessage: string; const aTag: string); overload;
    procedure EnqueueLogItem(const aLogItem: TLogItem);
    function GetMinimumLevel: TLogType;
    procedure SetMinimumLevel(const aLevel: TLogType);

    // ILogWriter - without tag (uses DEFAULT_LOG_TAG)
    procedure Debug(const aMessage: string); overload;
    procedure Info(const aMessage: string); overload;
    procedure Warn(const aMessage: string); overload;
    procedure Error(const aMessage: string); overload;
    procedure Fatal(const aMessage: string); overload;

    // ILogWriter - with explicit tag
    procedure Debug(const aMessage: string; const aTag: string); overload;
    procedure Debug(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Debug(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Info(const aMessage: string; const aTag: string); overload;
    procedure Info(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Info(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Warn(const aMessage: string; const aTag: string); overload;
    procedure Warn(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Warn(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Error(const aMessage: string; const aTag: string); overload;
    procedure Error(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Error(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Fatal(const aMessage: string; const aTag: string); overload;
    procedure Fatal(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Fatal(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure LogException(const E: Exception); overload;
    procedure LogException(const E: Exception; const aMessage: string); overload;
    procedure LogException(const E: Exception; const aMessage: string; const aTag: string); overload;

    procedure Log(const aType: TLogType; const aMessage: string; const aParams: array of const; const aTag: string); overload;
    procedure Log(const aType: TLogType; const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    function WithProperty(const aKey: string; const aValue: string): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: Integer): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: Boolean): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: Double): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: TDateTime): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: TValue): ILogWriter; overload;
    function WithPropertyFmt(const aKey: string; const aFormat: string; const aArgs: array of const): ILogWriter;

    function WithDefaultTag(const aTag: string): ILogWriter;
    function WithDefaultContext(const aContext: array of LogParam): ILogWriter;

    function IsEnabled(aLevel: TLogType): Boolean;
    function IsDebugEnabled: Boolean;
    function IsInfoEnabled: Boolean;
    function IsWarningEnabled: Boolean;
    function IsErrorEnabled: Boolean;
    function IsFatalEnabled: Boolean;

    procedure Disable;
    procedure Enable;
    procedure Shutdown;
  end;

  { @abstract(Wrapper for ILogWriter with custom default tag) }
  TLogWriterWithDefaultTag = class(TInterfacedObject, ILogWriter)
  private
    FInner: ILogWriter;
    FDefaultTag: string;
  public
    constructor Create(const AInner: ILogWriter; const ADefaultTag: string);

    // ICustomLogWriter
    function GetAppendersClassNames: TArray<string>;
    function GetAppenders(const aIndex: Integer): ILogAppender;
    procedure AddAppender(const aAppenders: ILogAppender);
    procedure DelAppender(const aAppenders: ILogAppender);
    function AppendersCount(): Integer;
    procedure Log(const aType: TLogType; const aMessage: string; const aTag: string); overload;
    procedure EnqueueLogItem(const aLogItem: TLogItem);
    function GetMinimumLevel: TLogType;
    procedure SetMinimumLevel(const aLevel: TLogType);

    // ILogWriter - without tag (uses FDefaultTag)
    procedure Debug(const aMessage: string); overload;
    procedure Info(const aMessage: string); overload;
    procedure Warn(const aMessage: string); overload;
    procedure Error(const aMessage: string); overload;
    procedure Fatal(const aMessage: string); overload;

    // ILogWriter - with explicit tag
    procedure Debug(const aMessage: string; const aTag: string); overload;
    procedure Debug(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Debug(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Info(const aMessage: string; const aTag: string); overload;
    procedure Info(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Info(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Warn(const aMessage: string; const aTag: string); overload;
    procedure Warn(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Warn(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Error(const aMessage: string; const aTag: string); overload;
    procedure Error(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Error(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Fatal(const aMessage: string; const aTag: string); overload;
    procedure Fatal(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Fatal(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure LogException(const E: Exception); overload;
    procedure LogException(const E: Exception; const aMessage: string); overload;
    procedure LogException(const E: Exception; const aMessage: string; const aTag: string); overload;

    procedure Log(const aType: TLogType; const aMessage: string; const aParams: array of const; const aTag: string); overload;
    procedure Log(const aType: TLogType; const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    function WithProperty(const aKey: string; const aValue: string): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: Integer): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: Boolean): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: Double): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: TDateTime): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: TValue): ILogWriter; overload;
    function WithPropertyFmt(const aKey: string; const aFormat: string; const aArgs: array of const): ILogWriter;

    function WithDefaultTag(const aTag: string): ILogWriter;
    function WithDefaultContext(const aContext: array of LogParam): ILogWriter;

    function IsEnabled(aLevel: TLogType): Boolean;
    function IsDebugEnabled: Boolean;
    function IsInfoEnabled: Boolean;
    function IsWarningEnabled: Boolean;
    function IsErrorEnabled: Boolean;
    function IsFatalEnabled: Boolean;

    procedure Disable;
    procedure Enable;
    procedure Shutdown;
  end;

  { @abstract(Wrapper for ILogWriter with default context) }
  TLogWriterWithDefaultContext = class(TInterfacedObject, ILogWriter)
  private
    FInner: ILogWriter;
    FDefaultContext: LogParams;
    function MergeContext(const aContext: array of LogParam): LogParams;
  public
    constructor Create(const AInner: ILogWriter; const ADefaultContext: array of LogParam);

    // ICustomLogWriter
    function GetAppendersClassNames: TArray<string>;
    function GetAppenders(const aIndex: Integer): ILogAppender;
    procedure AddAppender(const aAppenders: ILogAppender);
    procedure DelAppender(const aAppenders: ILogAppender);
    function AppendersCount(): Integer;
    procedure Log(const aType: TLogType; const aMessage: string; const aTag: string); overload;
    procedure EnqueueLogItem(const aLogItem: TLogItem);
    function GetMinimumLevel: TLogType;
    procedure SetMinimumLevel(const aLevel: TLogType);

    // Senza tag (usa tag predefinito 'main')
    procedure Debug(const aMessage: string); overload;
    procedure Info(const aMessage: string); overload;
    procedure Warn(const aMessage: string); overload;
    procedure Error(const aMessage: string); overload;
    procedure Fatal(const aMessage: string); overload;

    // Con tag esplicito
    procedure Debug(const aMessage: string; const aTag: string); overload;
    procedure Debug(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Debug(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Info(const aMessage: string; const aTag: string); overload;
    procedure Info(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Info(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Warn(const aMessage: string; const aTag: string); overload;
    procedure Warn(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Warn(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Error(const aMessage: string; const aTag: string); overload;
    procedure Error(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Error(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure Fatal(const aMessage: string; const aTag: string); overload;
    procedure Fatal(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure Fatal(const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    procedure LogException(const E: Exception); overload;
    procedure LogException(const E: Exception; const aMessage: string); overload;
    procedure LogException(const E: Exception; const aMessage: string; const aTag: string); overload;

    procedure Log(const aType: TLogType; const aMessage: string; const aParams: array of const; const aTag: string); overload;
    procedure Log(const aType: TLogType; const aMessage: string; const aTag: string; const aContext: array of LogParam); overload;

    function WithProperty(const aKey: string; const aValue: string): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: Integer): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: Boolean): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: Double): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: TDateTime): ILogWriter; overload;
    function WithProperty(const aKey: string; const aValue: TValue): ILogWriter; overload;
    function WithPropertyFmt(const aKey: string; const aFormat: string; const aArgs: array of const): ILogWriter;

    function WithDefaultTag(const aTag: string): ILogWriter;
    function WithDefaultContext(const aContext: array of LogParam): ILogWriter;

    function IsEnabled(aLevel: TLogType): Boolean;
    function IsDebugEnabled: Boolean;
    function IsInfoEnabled: Boolean;
    function IsWarningEnabled: Boolean;
    function IsErrorEnabled: Boolean;
    function IsFatalEnabled: Boolean;

    procedure Disable;
    procedure Enable;
    procedure Shutdown;
  end;

  TOnAppenderLogRow = reference to procedure(const LogItem: TLogItem; out LogRow: string);

  TLoggerProAppenderBase = class abstract(TInterfacedObject, ILogAppender)
  private
    FLogLevel: TLogType;
    FEnabled: Boolean;
    FLastErrorTimeStamp: TDateTime;
    FFormatSettings: TFormatSettings;
  protected
    FOnLogRow: TOnAppenderLogRow;
    FLogItemRenderer: ILogItemRenderer;
    property FormatSettings: TFormatSettings read FFormatSettings;
    property LogItemRenderer: ILogItemRenderer read FLogItemRenderer;
  public
    constructor Create(ALogItemRenderer: ILogItemRenderer); overload; virtual;
    constructor Create; overload; virtual;
    procedure Setup; virtual;
    function FormatLog(const ALogItem: TLogItem): string; virtual;
    procedure WriteLog(const aLogItem: TLogItem); virtual; abstract;
    procedure TearDown; virtual;
    procedure TryToRestart(var Restarted: Boolean); virtual;
    procedure SetMinimumLevel(const Value: TLogType);
    function GetMinimumLevel: TLogType; inline;
    procedure SetLastErrorTimeStamp(const Value: TDateTime);
    function GetLastErrorTimeStamp: TDateTime;
    procedure SetEnabled(const Value: Boolean);
    function IsEnabled: Boolean;
    property MinimumLevel: TLogType read GetMinimumLevel write SetMinimumLevel;
    property Enabled: Boolean read IsEnabled write SetEnabled;
    property OnLogRow: TOnAppenderLogRow read FOnLogRow write FOnLogRow;
  end;

  { @abstract(Builds a new ILogWriter instance. Call this global function to start logging like a pro.)
    Here's a sample unit that you can use in your code
    @longcode(#
    unit LoggerProConfig;

    interface

    uses
    LoggerPro;

    function Log: ILogWriter;

    implementation

    uses
    LoggerPro.FileAppender;

    var
    _Log: ILogWriter;

    function Log: ILogWriter;
    begin
    Result := _Log;
    end;

    initialization

    //If you need other appenders, feel free to add them here in the array
    _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5)]);

    end.
    #)

    Add this unit to your project, then when you need to use the logger, include
    the unit and call one of the followings:
    @unorderedlist(
    @item(Log.Debug('This is a debug message', 'tag1'))
    @item(Log.Info('This is an information message', 'tag1'))
    @item(Log.Warn('This is a warning message', 'tag1'))
    @item(Log.Error('This is an error message', 'tag1'))
	@item(Log.Fatal('This is a fatal message', 'tag1'))	
    )
  }


  TLogItemRenderer = class abstract(TInterfacedObject, ILogItemRenderer)
  protected
    procedure Setup; virtual;
    procedure TearDown; virtual;
    function RenderLogItem(const aLogItem: TLogItem): String; virtual;abstract;
  public
    class function GetDefaultLogItemRenderer: ILogItemRenderer;
  end;
  TLogItemRendererClass = class of TLogItemRenderer;

  { @abstract(Callback type invoked after a log file is rotated.)
    The callback receives the full path of the rotated file.
    @bold(Important:) This callback runs on the logger thread.
    Do not perform long-running operations directly; instead,
    kick off async work (e.g., compress, upload, archive).
  }
  TFileRotateCallback = reference to procedure(const aRotatedFileName: string);

  { Callback signature for receiving log items }
  TLogItemCallback = reference to procedure(const aLogItem: TLogItem; const aFormattedMessage: string);

  { Webhook appender body content type }
  TWebhookContentType = (JSON, PlainText);

  /// <summary>Where the API key value is carried on every POST.</summary>
  TWebhookAPIKeyLocation = (
    /// <summary>As an HTTP request header (default; header name configurable,
    /// defaults to <c>X-API-Key</c>). Works with API Gateway / Azure APIM /
    /// Kong / custom REST endpoints that read keys from headers.</summary>
    Header,
    /// <summary>As a query-string parameter (parameter name configurable,
    /// defaults to <c>api_key</c>). Useful for services that accept auth
    /// via URL (some cloud logging webhooks, legacy endpoints).</summary>
    QueryString);

  { Per-element color / style scheme (colorama-style: only colors, no
    suffixes; the renderer auto-appends STYLE_RESETALL after every colored
    token and at end of line, so color bleed is impossible).
    Each field holds a ready-made ANSI prefix string - bright, dim,
    background, 256-color, true color, or empty for no effect. }
  TLogColorScheme = record
    PrefixColor: string;
    SeparatorColor: string;
    TimestampColor: string;
    ThreadIDColor: string;
    TagColor: string;
    MessageColor: string;
    LevelColor: array [TLogType] of string;
    ContextKeyColor: string;
    ContextValueColor: string;
  end;

  /// <summary>
  /// Built-in color-scheme presets for the Gin-style console renderer.
  /// Namespaced like Fore / Back / Style - use LogColorSchemes.GinBadge,
  /// LogColorSchemes.Monochrome, etc. Pass to .WithColorScheme(LogColorSchemes.X) in the
  /// Builder.
  /// </summary>
  LogColorSchemes = record
  public
    /// <summary>Gin-inspired classic: foreground-only level colors,
    /// gray timestamp, cyan tag. Default when .WithColors is used.</summary>
    class function Default: TLogColorScheme; static;
    /// <summary>All fields empty - renders pure plain text, no ANSI.
    /// Used automatically when stdout is piped / redirected.</summary>
    class function Monochrome: TLogColorScheme; static;
    /// <summary>Colored-background level badges (INFO=green bg,
    /// ERROR=red bg, etc.) - looks like Gin's HTTP status-code badges.</summary>
    class function GinBadge: TLogColorScheme; static;
    /// <summary>All metadata dim gray, only the level word retains
    /// color. Best signal-to-noise ratio for busy production logs.</summary>
    class function GinMinimal: TLogColorScheme; static;
    /// <summary>Saturated rainbow - one loud color per field. For demos
    /// or screenshots; not recommended for long reading sessions.</summary>
    class function GinVibrant: TLogColorScheme; static;
    /// <summary>Midnight dark palette: purple prefix, magenta tag, green
    /// context, muted gray metadata. Easy on the eyes for long sessions on
    /// a dark terminal.</summary>
    class function Midnight: TLogColorScheme; static;
    /// <summary>Nord theme: cool arctic blues and cyans, muted warnings.
    /// Low-contrast, calm; good for ambient logging on a dark background.</summary>
    class function Nord: TLogColorScheme; static;
    /// <summary>The Matrix: every field bright green, levels distinguished
    /// by background badges. Pure nostalgia / ASCII-art demos.</summary>
    class function Matrix: TLogColorScheme; static;
    /// <summary>Amber CRT: warm 80s terminal - orange/yellow metadata,
    /// red errors. Looks great on a true black terminal background.</summary>
    class function Amber: TLogColorScheme; static;
    /// <summary>Ocean: layered blues and cyans, teal tag, bright cyan
    /// values. Cool and high-contrast without being loud.</summary>
    class function Ocean: TLogColorScheme; static;
    /// <summary>Cyberpunk neon: hot magenta prefix, cyan tag, yellow values,
    /// level badges on vivid backgrounds. Loud, saturated, nightlife.</summary>
    class function Cyberpunk: TLogColorScheme; static;
  end;

  { ============ Builder interfaces ============ }

  ILoggerProBuilder = interface;

  { Base interface for all appender configurators }
  IAppenderConfigurator = interface
    ['{A1B2C3D4-E5F6-4A5B-8C9D-0E1F2A3B4C5D}']
    function Done: ILoggerProBuilder;
  end;

  { Console appender configurator }
  IConsoleAppenderConfigurator = interface(IAppenderConfigurator)
    ['{B2C3D4E5-F6A7-5B6C-9D0E-1F2A3B4C5D6E}']
    function WithMinimumLevel(aLogLevel: TLogType): IConsoleAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): IConsoleAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IConsoleAppenderConfigurator; overload;
    function WithUTF8Output: IConsoleAppenderConfigurator;
    /// <summary>Enable per-field rich coloring (dim timestamp / thread ID,
    /// level in level-specific color, cyan tag, green/yellow key=value).
    /// Replaces the appender's whole-line coloring with the renderer's
    /// in-line ANSI codes. Auto-degrades to monochrome when stdout is
    /// redirected to a file or pipe.</summary>
    function WithColors: IConsoleAppenderConfigurator;
    /// <summary>Like WithColors but with a user-supplied scheme. Useful to
    /// customize per-level colors, tag color, key/value colors, or
    /// background highlights.</summary>
    function WithColorScheme(const aScheme: TLogColorScheme): IConsoleAppenderConfigurator;
    /// <summary>Prepend a bracketed source marker to every colored log
    /// line (Gin-style [GIN] pattern): [MYAPP] 2026/04/18 - 17:33:42 | ...
    /// Default is empty string = no prefix (line starts at the date).
    /// Useful when multiple processes or modules aggregate into the same
    /// console/pipe stream and you need to distinguish them visually.</summary>
    function WithPrefix(const aPrefix: string): IConsoleAppenderConfigurator;
  end;

  { Simple console appender configurator }
  ISimpleConsoleAppenderConfigurator = interface(IAppenderConfigurator)
    ['{B2C3D4E5-F6A7-5B6C-9D0E-1F2A3B4C5D6F}']
    function WithMinimumLevel(aLogLevel: TLogType): ISimpleConsoleAppenderConfigurator;
    function WithUTF8Output: ISimpleConsoleAppenderConfigurator;
  end;

  { File appender configurator }
  IFileAppenderConfigurator = interface(IAppenderConfigurator)
    ['{C3D4E5F6-A7B8-6C7D-0E1F-2A3B4C5D6E7F}']
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
  end;

  { JSONL file appender configurator }
  IJSONLFileAppenderConfigurator = interface(IAppenderConfigurator)
    ['{D4E5F6A7-B8C9-7D8E-1F2A-3B4C5D6E7F8A}']
    function WithLogsFolder(const aLogsFolder: string): IJSONLFileAppenderConfigurator;
    function WithFileBaseName(const aFileBaseName: string): IJSONLFileAppenderConfigurator;
    function WithMaxBackupFiles(aMaxBackupFiles: Integer): IJSONLFileAppenderConfigurator;
    function WithMaxFileSizeInKB(aMaxFileSizeInKB: Integer): IJSONLFileAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IJSONLFileAppenderConfigurator;
  end;

  /// <summary>
  /// Configurator for the time-rotating file appender.
  /// Creates a new log file each time the configured time interval rolls over
  /// (e.g. every hour, day, week, or month).
  /// <code>
  /// Log := LoggerProBuilder
  ///   .WriteToTimeRotatingFile
  ///     .WithInterval(TTimeRotationInterval.Hourly)
  ///     .WithLogsFolder('logs')
  ///     .WithFileBaseName('myapp')
  ///     .WithMaxBackupFiles(48)
  ///     .Done
  ///   .Build;
  /// </code>
  /// </summary>
  ITimeRotatingFileAppenderConfigurator = interface(IAppenderConfigurator)
    ['{E5F6A7B8-C9D0-8E9F-2A3B-4C5D6E7F8A9B}']
    /// <summary>Sets the rotation interval. Determines how often a new log file
    /// is created. Values: Hourly, Daily (default), Weekly, Monthly.
    /// File name includes a timestamp suffix matching the interval:
    ///   Hourly  -> myapp.2026032614.log
    ///   Daily   -> myapp.20260326.log
    ///   Weekly  -> myapp.2026W13.log
    ///   Monthly -> myapp.202603.log</summary>
    function WithInterval(aInterval: TTimeRotationInterval): ITimeRotatingFileAppenderConfigurator;
    /// <summary>Maximum number of old rotated log files to keep.
    /// When exceeded, the oldest file is deleted. Default: 30.</summary>
    function WithMaxBackupFiles(aMaxBackupFiles: Integer): ITimeRotatingFileAppenderConfigurator;
    /// <summary>Folder where log files are written.
    /// Default: application directory.</summary>
    function WithLogsFolder(const aLogsFolder: string): ITimeRotatingFileAppenderConfigurator;
    /// <summary>Base name for the log file (without extension or timestamp).
    /// Default: application executable name without extension.</summary>
    function WithFileBaseName(const aFileBaseName: string): ITimeRotatingFileAppenderConfigurator;
    /// <summary>Sets the minimum log level for this appender.
    /// Messages below this level are ignored by this appender.</summary>
    function WithMinimumLevel(aLogLevel: TLogType): ITimeRotatingFileAppenderConfigurator;
    /// <summary>Sets a custom renderer for formatting log entries.
    /// Default: standard text renderer.</summary>
    function WithRenderer(aRenderer: ILogItemRenderer): ITimeRotatingFileAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): ITimeRotatingFileAppenderConfigurator; overload;
  end;

  { HTTP appender configurator }
  IWebhookAppenderConfigurator = interface(IAppenderConfigurator)
    ['{F6A7B8C9-D0E1-9F0A-3B4C-5D6E7F8A9B0C}']
    function WithURL(const aURL: string): IWebhookAppenderConfigurator;
    function WithContentType(aContentType: TWebhookContentType): IWebhookAppenderConfigurator;
    function WithTimeout(aTimeoutSeconds: Integer): IWebhookAppenderConfigurator;
    function WithRetryCount(aRetryCount: Integer): IWebhookAppenderConfigurator;
    function WithHeader(const aName, aValue: string): IWebhookAppenderConfigurator;
    /// <summary>Send an API key on every request. <c>aLocation</c> picks
    /// header (default) or query-string; <c>aName</c> overrides the
    /// standard name (<c>X-API-Key</c> for header, <c>api_key</c> for
    /// query string) - pass empty string to use the default.</summary>
    function WithAPIKey(const aValue: string;
      aLocation: TWebhookAPIKeyLocation = TWebhookAPIKeyLocation.Header;
      const aName: string = ''): IWebhookAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IWebhookAppenderConfigurator;
  end;

  { ElasticSearch appender configurator }
  IElasticSearchAppenderConfigurator = interface(IAppenderConfigurator)
    ['{A7B8C9D0-E1F2-0A1B-4C5D-6E7F8A9B0C1D}']
    function WithURL(const aURL: string): IElasticSearchAppenderConfigurator;
    function WithHost(const aHost: string): IElasticSearchAppenderConfigurator;
    function WithPort(aPort: Integer): IElasticSearchAppenderConfigurator;
    function WithIndex(const aIndex: string): IElasticSearchAppenderConfigurator;
    function WithTimeout(aTimeoutSeconds: Integer): IElasticSearchAppenderConfigurator;
    function WithBasicAuth(const aUsername, aPassword: string): IElasticSearchAppenderConfigurator;
    function WithAPIKey(const aAPIKey: string): IElasticSearchAppenderConfigurator;
    function WithBearerToken(const aToken: string): IElasticSearchAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IElasticSearchAppenderConfigurator;
  end;

  { Memory appender configurator }
  IMemoryAppenderConfigurator = interface(IAppenderConfigurator)
    ['{B8C9D0E1-F2A3-1B2C-5D6E-7F8A9B0C1D2E}']
    function WithMaxSize(aMaxSize: Integer): IMemoryAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IMemoryAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): IMemoryAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IMemoryAppenderConfigurator; overload;
  end;

  { Callback appender configurator }
  ICallbackAppenderConfigurator = interface(IAppenderConfigurator)
    ['{C9D0E1F2-A3B4-2C3D-6E7F-8A9B0C1D2E3F}']
    function WithCallback(aCallback: TLogItemCallback): ICallbackAppenderConfigurator;
    function WithSynchronizeToMainThread(aValue: Boolean): ICallbackAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): ICallbackAppenderConfigurator;
  end;

  { Strings appender configurator (cross-platform, works with any TStrings).
    Default: MaxLogLines = 100, ClearOnStartup = False.
    The TStrings instance is NOT owned by the appender. The caller is
    responsible for its lifetime and must ensure it outlives the ILogWriter. }
  IStringsAppenderConfigurator = interface(IAppenderConfigurator)
    ['{A3B4C5D6-E7F8-9A0B-1C2D-3E4F5A6B7C8D}']
    { Sets the maximum number of lines retained. Oldest lines are removed first.
      Must be > 0. Default: 100. }
    function WithMaxLogLines(aMaxLogLines: Word): IStringsAppenderConfigurator;
    { If True, the TStrings instance is cleared when the logger starts. Default: False. }
    function WithClearOnStartup(aValue: Boolean): IStringsAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IStringsAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): IStringsAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IStringsAppenderConfigurator; overload;
  end;

  { OutputDebugString appender configurator }
  IOutputDebugStringAppenderConfigurator = interface(IAppenderConfigurator)
    ['{E1F2A3B4-C5D6-4E5F-8A9B-0C1D2E3F4A5B}']
    function WithMinimumLevel(aLogLevel: TLogType): IOutputDebugStringAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): IOutputDebugStringAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IOutputDebugStringAppenderConfigurator; overload;
  end;

  { UDP Syslog appender configurator }
  IUDPSyslogAppenderConfigurator = interface(IAppenderConfigurator)
    ['{F2A3B4C5-D6E7-5F6A-9B0C-1D2E3F4A5B6C}']
    function WithHost(const aHost: string): IUDPSyslogAppenderConfigurator;
    function WithPort(aPort: Integer): IUDPSyslogAppenderConfigurator;
    function WithHostName(const aHostName: string): IUDPSyslogAppenderConfigurator;
    function WithUserName(const aUserName: string): IUDPSyslogAppenderConfigurator;
    function WithApplication(const aApplication: string): IUDPSyslogAppenderConfigurator;
    function WithVersion(const aVersion: string): IUDPSyslogAppenderConfigurator;
    function WithProcID(const aProcID: string): IUDPSyslogAppenderConfigurator;
    function WithUseLocalTime(aUseLocalTime: Boolean): IUDPSyslogAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IUDPSyslogAppenderConfigurator;
  end;


{$IF Defined(MSWINDOWS)}
  { VCL Memo appender configurator (requires VCL, Windows only) }
  IVCLMemoAppenderConfigurator = interface(IAppenderConfigurator)
    ['{B4C5D6E7-F8A9-7B8C-1D2E-3F4A5B6C7D8E}']
    function WithMaxLogLines(aMaxLogLines: Word): IVCLMemoAppenderConfigurator;
    function WithClearOnStartup(aValue: Boolean): IVCLMemoAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IVCLMemoAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): IVCLMemoAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IVCLMemoAppenderConfigurator; overload;
  end;

  { VCL ListBox appender configurator (requires VCL, Windows only) }
  IVCLListBoxAppenderConfigurator = interface(IAppenderConfigurator)
    ['{C5D6E7F8-A9B0-8C9D-2E3F-4A5B6C7D8E9F}']
    function WithMaxLogLines(aMaxLogLines: Word): IVCLListBoxAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IVCLListBoxAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): IVCLListBoxAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IVCLListBoxAppenderConfigurator; overload;
  end;

  { VCL ListView appender configurator (requires VCL, Windows only) }
  IVCLListViewAppenderConfigurator = interface(IAppenderConfigurator)
    ['{D6E7F8A9-B0C1-9D0E-3F4A-5B6C7D8E9F0A}']
    function WithMaxLogLines(aMaxLogLines: Word): IVCLListViewAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IVCLListViewAppenderConfigurator;
    function WithRenderer(aRenderer: ILogItemRenderer): IVCLListViewAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IVCLListViewAppenderConfigurator; overload;
  end;

  { Windows Event Log appender configurator (Windows only) }
  IWindowsEventLogAppenderConfigurator = interface(IAppenderConfigurator)
    ['{A1B2C3D4-E5F6-7A8B-9C0D-E1F2A3B4C5D6}']
    function WithMinimumLevel(aLogLevel: TLogType): IWindowsEventLogAppenderConfigurator;
    function WithSourceName(const aSourceName: string): IWindowsEventLogAppenderConfigurator;
  end;
{$ENDIF}

  { FireDAC DB appender configurator (cross-platform) }
  IFireDACAppenderConfigurator = interface(IAppenderConfigurator)
    ['{E7F8A9B0-C1D2-0E1F-4A5B-6C7D8E9F0A1B}']
    function WithConnectionDefName(const aConnectionDefName: string): IFireDACAppenderConfigurator;
    function WithStoredProcName(const aStoredProcName: string): IFireDACAppenderConfigurator;
    function WithMinimumLevel(aLogLevel: TLogType): IFireDACAppenderConfigurator;
  end;

  { Filter appender configurator - wraps another appender with a filter }
  TLogItemFilterFunc = TFunc<TLogItem, Boolean>;

  { Filtered appender configurator - generic filter that wraps any appender }
  IFilteredAppenderConfigurator = interface(IAppenderConfigurator)
    ['{F8A9B0C1-D2E3-1F2A-5B6C-7D8E9F0A1B2C}']
    function WithFilter(aFilter: TLogItemFilterFunc): IFilteredAppenderConfigurator;
  end;

  /// <summary>
  /// Configurator for the file-by-source appender.
  /// Organizes logs into per-source subfolders. The source is extracted from
  /// the log context using LogParam.S('source', 'ClientA') or set via
  /// WithDefaultContext. Files are named Source.Tag.YYYYMMDD.NN.log with
  /// day-change and size-based rotation. Retention is by number of days.
  /// <code>
  /// Log := LoggerProBuilder
  ///   .WriteToFileBySource
  ///     .WithLogsFolder('logs')
  ///     .WithMaxFileSizeInKB(5000)
  ///     .WithRetainDays(30)
  ///     .Done
  ///   .Build;
  /// </code>
  /// </summary>
  /// <summary>
  /// Configurator for <c>WriteToHTMLFile</c>. Produces a self-contained
  /// .html file with inline CSS + JS (sticky filter bar, level-based row
  /// coloring, client-side text search). Open the file in any browser to
  /// analyze logs - no HTTP server, no external assets.
  ///
  /// Example:
  /// <code>
  ///   .WriteToHTMLFile
  ///     .WithFile('logs/app.html')
  ///     .WithTitle('My Application')
  ///     .WithMinimumLevel(TLogType.Info)
  ///     .Done
  /// </code>
  /// </summary>
  IHTMLFileAppenderConfigurator = interface(IAppenderConfigurator)
    ['{E4F5A6B7-C8D9-40EA-B1C2-D3E4F5A6B7C8}']
    /// <summary>Folder where HTML files are written. Created if missing.
    /// Default: application directory.</summary>
    function WithLogsFolder(const aLogsFolder: string): IHTMLFileAppenderConfigurator;
    /// <summary>Base name for the log file. Extension and {number} suffix
    /// are added by the format. Default: executable name.</summary>
    function WithFileBaseName(const aFileBaseName: string): IHTMLFileAppenderConfigurator;
    /// <summary>Page title shown in browser tab and H1 header.</summary>
    function WithTitle(const aTitle: string): IHTMLFileAppenderConfigurator;
    /// <summary>Maximum backup file count kept on disk. Default: 5.</summary>
    function WithMaxBackupFiles(aMaxBackupFiles: Integer): IHTMLFileAppenderConfigurator;
    /// <summary>Rotate when the active file exceeds this many KB. Default: 1000.</summary>
    function WithMaxFileSizeInKB(aMaxFileSizeInKB: Integer): IHTMLFileAppenderConfigurator;
    /// <summary>Time-based rotation interval. Default: None.</summary>
    function WithInterval(aInterval: TTimeRotationInterval): IHTMLFileAppenderConfigurator;
    /// <summary>Number of days to keep rotated files. 0 = unlimited.</summary>
    function WithMaxRetainedFiles(aMaxFiles: Integer): IHTMLFileAppenderConfigurator;
    /// <summary>Minimum log level for this appender.</summary>
    function WithMinimumLevel(aLogLevel: TLogType): IHTMLFileAppenderConfigurator;
  end;

  IFileBySourceAppenderConfigurator = interface(IAppenderConfigurator)
    ['{D1E2F3A4-B5C6-7D8E-9F0A-1B2C3D4E5F6A}']
    /// <summary>Folder where source subfolders are created.
    /// Default: application directory.</summary>
    function WithLogsFolder(const aLogsFolder: string): IFileBySourceAppenderConfigurator;
    /// <summary>Maximum file size in KB before rotating to a new sequence.
    /// Default: 1000 (1 MB).</summary>
    function WithMaxFileSizeInKB(aMaxFileSizeInKB: Integer): IFileBySourceAppenderConfigurator;
    /// <summary>Number of days to retain log files. Files older than this
    /// are deleted on startup and on each day change. Default: 30.</summary>
    function WithRetainDays(aRetainDays: Integer): IFileBySourceAppenderConfigurator;
    /// <summary>Source name used when no 'source' key is found in the log
    /// context. Default: 'default'.</summary>
    function WithDefaultSource(const aDefaultSource: string): IFileBySourceAppenderConfigurator;
    /// <summary>Sets the minimum log level for this appender.</summary>
    function WithMinimumLevel(aLogLevel: TLogType): IFileBySourceAppenderConfigurator;
    /// <summary>Sets a custom renderer for formatting log entries.</summary>
    function WithRenderer(aRenderer: ILogItemRenderer): IFileBySourceAppenderConfigurator; overload;
    function WithRenderer(const aRendererName: string): IFileBySourceAppenderConfigurator; overload;
  end;

  { Main builder interface }
  ILoggerProBuilder = interface
    ['{1A2B3C4D-5E6F-7A8B-9C0D-1E2F3A4B5C6D}']
    // WriteTo appender methods - all return configurators
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
    { Strings appender - logs to any TStrings instance (cross-platform) }
    function WriteToStrings(aStrings: TStrings): IStringsAppenderConfigurator;
{$IF Defined(MSWINDOWS)}
    { VCL appenders - require VCL units (Windows only) }
    function WriteToVCLMemo(aMemo: TObject): IVCLMemoAppenderConfigurator;
    function WriteToVCLListBox(aListBox: TObject): IVCLListBoxAppenderConfigurator;
    function WriteToVCLListView(aListView: TObject): IVCLListViewAppenderConfigurator;
    { Windows Event Log appender }
    function WriteToWindowsEventLog: IWindowsEventLogAppenderConfigurator;
    { Windows Event Log appender for Windows Services (uses TService.LogMessage) }
    function WriteToWindowsEventLogForService(aService: TObject): IWindowsEventLogAppenderConfigurator;
{$ENDIF}
    { FireDAC appender (cross-platform) }
    function WriteToFireDAC: IFireDACAppenderConfigurator;

    { Filter appender - wraps another appender with a filter function }
    function WriteToFilteredAppender(aAppender: ILogAppender): IFilteredAppenderConfigurator;

    { File-by-source appender - per-source folders with date+size rotation }
    function WriteToFileBySource: IFileBySourceAppenderConfigurator;

    { Standalone HTML file appender - produces a self-contained browsable
      .html with filter bar and level coloring. }
    function WriteToHTMLFile: IHTMLFileAppenderConfigurator;

    { Generic method for adding pre-configured appenders }
    function WriteToAppender(aAppender: ILogAppender): ILoggerProBuilder;

    // Global configuration
    function WithDefaultMinimumLevel(aLogLevel: TLogType): ILoggerProBuilder;
    function WithMinimumLevel(aLevel: TLogType): ILoggerProBuilder;
    function WithDefaultRenderer(aRenderer: ILogItemRenderer): ILoggerProBuilder;
    function WithDefaultTag(const aTag: string): ILoggerProBuilder;
    function WithStackTraceFormatter(aFormatter: TStackTraceFormatter): ILoggerProBuilder;

    // Build the logger
    function Build: ILogWriter;
  end;


function GetDefaultFormatSettings: TFormatSettings;
function StringToLogType(const aLogType: string): TLogType;
/// <summary>Legacy factory - prefer <c>LoggerProBuilder</c> (the fluent
/// Builder API) or <c>LoggerProFromJSONFile</c> / <c>LoggerProFromJSONString</c>
/// (file-driven config). Kept for backward compatibility only.</summary>
function BuildLogWriter(aAppenders: array of ILogAppender; aEventsHandlers: TLoggerProEventsHandler = nil;
  aLogLevel: TLogType = TLogType.Debug): ILogWriter; overload; deprecated 'Use LoggerProBuilder or LoggerProFromJSONFile';
function BuildLogWriter(aAppenders: array of ILogAppender; aEventsHandlers: TLoggerProEventsHandler;
  aLogLevels: TArray<TLogType>): ILogWriter; overload; deprecated 'Use LoggerProBuilder or LoggerProFromJSONFile';
function LogLayoutByPlaceHoldersToLogLayoutByIndexes(const LogLayoutByPlaceHolders: String; const UseZeroBasedIncrementalIndexes: Boolean): String;

/// <summary>Builds an ILogWriter from a JSON configuration file.
/// Convenience facade over TLoggerProConfig.FromJSONFile so callers can
/// stay on a single `uses LoggerPro;` clause. See LoggerPro.Config for
/// the full schema and TLoggerProConfig.RegisterAppenderType to plug in
/// custom appender types.</summary>
function LoggerProFromJSONFile(const aFileName: string): ILogWriter;

/// <summary>Builds an ILogWriter from an in-memory JSON string. Same
/// facade as LoggerProFromJSONFile, useful for embedded config or tests.</summary>
function LoggerProFromJSONString(const aJSON: string): ILogWriter;

/// <summary>Read JSON config file and return the underlying ILoggerProBuilder
/// without finalizing (no .Build called). Use to chain extra appenders that
/// cannot be expressed in JSON (Callback, Strings, VCL components, any
/// pre-existing ILogAppender) on top of a file-driven config. Caller is
/// responsible for calling .Build.</summary>
function LoggerProBuilderFromJSONFile(const aFileName: string): ILoggerProBuilder;

/// <summary>Parse an in-memory JSON string and return the underlying
/// ILoggerProBuilder without finalizing. See <c>LoggerProBuilderFromJSONFile</c>
/// for usage.</summary>
function LoggerProBuilderFromJSONString(const aJSON: string): ILoggerProBuilder;

implementation

uses
  System.Types,
  System.TypInfo,
  LoggerPro.FileAppender,
  System.DateUtils,
  System.IOUtils,
  LoggerPro.AnsiColors,
  LoggerPro.Renderers,
  // Pulled in only by the implementation: LoggerPro.Config uses LoggerPro
  // back, so listing it here avoids the interface-section circular ref
  // while still letting the JSON facade live on `uses LoggerPro;`.
  LoggerPro.Config;

function GetDefaultFormatSettings: TFormatSettings;
begin
  Result.DateSeparator := '-';
  Result.TimeSeparator := ':';
  Result.DecimalSeparator := '.';
  Result.ShortDateFormat := 'YYYY-MM-DD HH:NN:SS:ZZZ';
  Result.ShortTimeFormat := 'HH:NN:SS';
end;

{ LogParam }

class function LogParam.S(const AKey: string; const AValue: string): LogParam;
begin
  Result.Key := AKey;
  Result.Value := TValue.From<string>(AValue);
end;

class function LogParam.I(const AKey: string; const AValue: Integer): LogParam;
begin
  Result.Key := AKey;
  Result.Value := TValue.From<Integer>(AValue);
end;

class function LogParam.B(const AKey: string; const AValue: Boolean): LogParam;
begin
  Result.Key := AKey;
  Result.Value := TValue.From<Boolean>(AValue);
end;

class function LogParam.F(const AKey: string; const AValue: Double): LogParam;
begin
  Result.Key := AKey;
  Result.Value := TValue.From<Double>(AValue);
end;

class function LogParam.D(const AKey: string; const AValue: TDateTime): LogParam;
begin
  Result.Key := AKey;
  Result.Value := TValue.From<TDateTime>(AValue);
end;

class function LogParam.FmtS(const AKey: string; const AFormat: string; const AArgs: array of const): LogParam;
begin
  Result.Key := AKey;
  Result.Value := TValue.From<string>(Format(AFormat, AArgs));
end;

class function LogParam.V(const AKey: string; const AValue: TValue): LogParam;
begin
  Result.Key := AKey;
  Result.Value := AValue;
end;

function LogLayoutByPlaceHoldersToLogLayoutByIndexes(const LogLayoutByPlaceHolders: String; const UseZeroBasedIncrementalIndexes: Boolean): String;
var
  PlaceHolders, PlaceHolderWidthsAndPaddings: TArray<string>;
  I: Integer;
  lIdx: Integer;
begin
  if LogLayoutByPlaceHolders.Contains('%s') or LogLayoutByPlaceHolders.Contains('%d') then
  begin
    Exit(LogLayoutByPlaceHolders); //there aren't placeholders
  end;

  // Format specifiers have the following form:
  // "%" [index ":"] ["-"] [width] ["." proc] type

  //Format params by index are:
  // 0: timestamp
  // 1: threadid
  // 2: loglevel
  // 3: message
  // 4: tag

  SetLength(PlaceHolders, 5);
  PlaceHolders[0] := 'timestamp';
  PlaceHolders[1] := 'threadid';
  PlaceHolders[2] := 'loglevel';
  PlaceHolders[3] := 'message';
  PlaceHolders[4] := 'tag';

  //DEFAULT_LOG_FORMAT = '%0:s [TID %1:-8d][%2:-10s] %3:s [%4:s]';

  SetLength(PlaceHolderWidthsAndPaddings, Length(PlaceHolders));
  PlaceHolderWidthsAndPaddings[0] := '';
  PlaceHolderWidthsAndPaddings[1] := '8';
  PlaceHolderWidthsAndPaddings[2] := '-7';
  PlaceHolderWidthsAndPaddings[3] := '';
  PlaceHolderWidthsAndPaddings[4] := '';

  Result := LogLayoutByPlaceHolders;


  if UseZeroBasedIncrementalIndexes then
  begin
    lIdx := 0;
    for I := 0 to High(PlaceHolders) do
    begin
      if Result.Contains('{' + PlaceHolders[I] + '}') then
      begin
        Result := Result.Replace(
          '{' + PlaceHolders[I] + '}',
          '%' + IntToStr(lIdx) + ':' + PlaceHolderWidthsAndPaddings[I] + 's');
        Inc(lIdx);
      end;
    end;
  end
  else
  begin
    for I := 0 to High(PlaceHolders) do
    begin
      Result := Result.Replace(
        '{' + PlaceHolders[I] + '}',
        '%' + IntToStr(I) + ':' + PlaceHolderWidthsAndPaddings[I] + 's');
    end;
  end;
end;

function StringToLogType(const aLogType: string): TLogType;
var
  lLogType: string;
begin
  lLogType := LowerCase(Trim(aLogType));
  if lLogType = 'debug' then
    Exit(TLogType.Debug);
  if lLogType = 'info' then
    Exit(TLogType.Info);
  if lLogType = 'warning' then
    Exit(TLogType.Warning);
  if lLogType = 'error' then
    Exit(TLogType.Error);
  if lLogType = 'fatal' then
    Exit(TLogType.Fatal);
  raise ELoggerPro.CreateFmt('Invalid LogType: %s', [aLogType]);
end;

function BuildLogWriter(aAppenders: array of ILogAppender; aEventsHandlers: TLoggerProEventsHandler; aLogLevel: TLogType): ILogWriter;
var
  lLogLevelsArray: TArray<TLogType>;
  I: Integer;
begin
  SetLength(lLogLevelsArray, length(aAppenders));
  for I := 0 to Length(lLogLevelsArray) - 1 do
  begin
    lLogLevelsArray[I] := aLogLevel;
  end;
  Result := BuildLogWriter(aAppenders, aEventsHandlers, lLogLevelsArray);
end;

function BuildLogWriter(aAppenders: array of ILogAppender; aEventsHandlers: TLoggerProEventsHandler; aLogLevels: TArray<TLogType>): ILogWriter;
var
  lLogAppenders: TLogAppenderList;
  lLowestLogLevel: TLogType;
  I: Integer;
begin
  lLowestLogLevel := TLogType.Fatal;
  if Length(aAppenders) <> Length(aLogLevels) then
  begin
    raise ELoggerPro.Create('LogLevels.Count <> Appenders.Count');
  end;
  lLogAppenders := TLogAppenderList.Create;
  for I := 0 to Length(aAppenders) - 1 do
  begin
    lLogAppenders.Add(aAppenders[I]);
    aAppenders[I].SetMinimumLevel(aLogLevels[I]);
    if aLogLevels[I] < lLowestLogLevel then
    begin
      lLowestLogLevel := aLogLevels[I];
    end;
  end;
  Result := TLogWriter.Create(lLogAppenders, lLowestLogLevel);
  TLogWriter(Result).Initialize(aEventsHandlers);
end;

function LoggerProFromJSONFile(const aFileName: string): ILogWriter;
begin
  Result := TLoggerProConfig.FromJSONFile(aFileName);
end;

function LoggerProFromJSONString(const aJSON: string): ILogWriter;
begin
  Result := TLoggerProConfig.FromJSONString(aJSON);
end;

function LoggerProBuilderFromJSONFile(const aFileName: string): ILoggerProBuilder;
begin
  Result := TLoggerProConfig.BuilderFromJSONFile(aFileName);
end;

function LoggerProBuilderFromJSONString(const aJSON: string): ILoggerProBuilder;
begin
  Result := TLoggerProConfig.BuilderFromJSONString(aJSON);
end;

{ LogColorSchemes }

class function LogColorSchemes.Default: TLogColorScheme;
begin
  Result := System.Default(TLogColorScheme);

  // Gin-inspired palette. [LPRO] prefix bright green + bold (matches
  // Gin's signature [GIN] prefix). Pipe separators dim gray so they
  // recede. Date dim, level colored per-severity, tag cyan.
  Result.PrefixColor    := STYLE_BRIGHT + FORE_GREEN;
  Result.SeparatorColor := STYLE_DIM + FORE_GRAY;

  Result.TimestampColor := STYLE_DIM + FORE_GRAY;
  Result.ThreadIDColor  := STYLE_DIM + FORE_GRAY;

  Result.TagColor := FORE_CYAN;

  Result.MessageColor := '';

  Result.LevelColor[TLogType.Debug]   := STYLE_DIM + FORE_GREEN;
  Result.LevelColor[TLogType.Info]    := FORE_WHITE;
  Result.LevelColor[TLogType.Warning] := FORE_YELLOW;
  Result.LevelColor[TLogType.Error]   := STYLE_BRIGHT + FORE_RED;
  Result.LevelColor[TLogType.Fatal]   := STYLE_BRIGHT + FORE_MAGENTA;

  Result.ContextKeyColor   := STYLE_DIM + FORE_GREEN;
  Result.ContextValueColor := FORE_YELLOW;
end;

class function LogColorSchemes.Monochrome: TLogColorScheme;
begin
  Result := System.Default(TLogColorScheme);
  // All color strings stay empty -> Colorize emits plain text.
end;

class function LogColorSchemes.GinBadge: TLogColorScheme;
begin
  Result := LogColorSchemes.Default;
  // Replace foreground-only level colors with colored-background "badges"
  // - the level looks like a status-code badge from Gin's access log.
  Result.LevelColor[TLogType.Debug]   := STYLE_BRIGHT + FORE_WHITE + BACK_DARKBLUE;
  Result.LevelColor[TLogType.Info]    := STYLE_BRIGHT + FORE_WHITE + BACK_DARKGREEN;
  Result.LevelColor[TLogType.Warning] := STYLE_BRIGHT + FORE_BLACK + BACK_YELLOW;
  Result.LevelColor[TLogType.Error]   := STYLE_BRIGHT + FORE_WHITE + BACK_DARKRED;
  Result.LevelColor[TLogType.Fatal]   := STYLE_BRIGHT + FORE_WHITE + BACK_MAGENTA;
end;

class function LogColorSchemes.GinMinimal: TLogColorScheme;
begin
  Result := System.Default(TLogColorScheme);
  // All metadata dim gray. Only the level retains identity.
  Result.PrefixColor    := STYLE_DIM + FORE_GRAY;
  Result.SeparatorColor := STYLE_DIM + FORE_GRAY;
  Result.TimestampColor := STYLE_DIM + FORE_GRAY;
  Result.ThreadIDColor  := STYLE_DIM + FORE_GRAY;
  Result.TagColor       := STYLE_DIM + FORE_GRAY;
  Result.MessageColor   := '';
  Result.ContextKeyColor   := STYLE_DIM + FORE_GRAY;
  Result.ContextValueColor := STYLE_DIM + FORE_GRAY;
  Result.LevelColor[TLogType.Debug]   := FORE_GRAY;
  Result.LevelColor[TLogType.Info]    := FORE_WHITE;
  Result.LevelColor[TLogType.Warning] := FORE_YELLOW;
  Result.LevelColor[TLogType.Error]   := STYLE_BRIGHT + FORE_RED;
  Result.LevelColor[TLogType.Fatal]   := STYLE_BRIGHT + FORE_MAGENTA;
end;

class function LogColorSchemes.GinVibrant: TLogColorScheme;
begin
  Result := System.Default(TLogColorScheme);
  // Saturated rainbow - every field a distinct loud color.
  Result.PrefixColor    := STYLE_BRIGHT + FORE_GREEN;
  Result.SeparatorColor := FORE_BLUE;
  Result.TimestampColor := FORE_CYAN;
  Result.ThreadIDColor  := FORE_MAGENTA;
  Result.TagColor       := STYLE_BRIGHT + FORE_CYAN;
  Result.MessageColor   := FORE_WHITE;
  Result.ContextKeyColor   := FORE_GREEN;
  Result.ContextValueColor := STYLE_BRIGHT + FORE_YELLOW;
  Result.LevelColor[TLogType.Debug]   := STYLE_BRIGHT + FORE_BLUE;
  Result.LevelColor[TLogType.Info]    := STYLE_BRIGHT + FORE_WHITE;
  Result.LevelColor[TLogType.Warning] := STYLE_BRIGHT + FORE_YELLOW;
  Result.LevelColor[TLogType.Error]   := STYLE_BRIGHT + FORE_WHITE + BACK_RED;
  Result.LevelColor[TLogType.Fatal]   := STYLE_BRIGHT + FORE_WHITE + BACK_MAGENTA;
end;

class function LogColorSchemes.Midnight: TLogColorScheme;
begin
  Result := System.Default(TLogColorScheme);
  // Midnight palette: purple/magenta prefix and tag, green context keys,
  // bright cyan values, muted gray metadata. Dark-terminal friendly,
  // high-contrast but not loud.
  Result.PrefixColor    := STYLE_BRIGHT + FORE_MAGENTA;
  Result.SeparatorColor := STYLE_DIM + FORE_GRAY;
  Result.TimestampColor := STYLE_DIM + FORE_GRAY;
  Result.ThreadIDColor  := STYLE_DIM + FORE_DARKMAGENTA;
  Result.TagColor       := STYLE_BRIGHT + FORE_MAGENTA;
  Result.MessageColor   := '';
  Result.ContextKeyColor   := FORE_GREEN;
  Result.ContextValueColor := STYLE_BRIGHT + FORE_CYAN;
  Result.LevelColor[TLogType.Debug]   := STYLE_DIM + FORE_CYAN;
  Result.LevelColor[TLogType.Info]    := STYLE_BRIGHT + FORE_GREEN;
  Result.LevelColor[TLogType.Warning] := STYLE_BRIGHT + FORE_YELLOW;
  Result.LevelColor[TLogType.Error]   := STYLE_BRIGHT + FORE_RED;
  Result.LevelColor[TLogType.Fatal]   := STYLE_BRIGHT + FORE_WHITE + BACK_MAGENTA;
end;

class function LogColorSchemes.Nord: TLogColorScheme;
begin
  Result := System.Default(TLogColorScheme);
  // Nord theme (nordtheme.com): frost blues/cyans with restrained warmth.
  // nord7 cyan / nord8 bright cyan / nord9 blue / nord13 yellow / nord11 red.
  Result.PrefixColor    := STYLE_BRIGHT + FORE_CYAN;
  Result.SeparatorColor := STYLE_DIM + FORE_GRAY;
  Result.TimestampColor := STYLE_DIM + FORE_BLUE;
  Result.ThreadIDColor  := STYLE_DIM + FORE_GRAY;
  Result.TagColor       := FORE_CYAN;
  Result.MessageColor   := '';
  Result.ContextKeyColor   := STYLE_DIM + FORE_CYAN;
  Result.ContextValueColor := FORE_WHITE;
  Result.LevelColor[TLogType.Debug]   := STYLE_DIM + FORE_CYAN;
  Result.LevelColor[TLogType.Info]    := FORE_BLUE;
  Result.LevelColor[TLogType.Warning] := FORE_YELLOW;
  Result.LevelColor[TLogType.Error]   := STYLE_BRIGHT + FORE_RED;
  Result.LevelColor[TLogType.Fatal]   := STYLE_BRIGHT + FORE_WHITE + BACK_DARKRED;
end;

class function LogColorSchemes.Matrix: TLogColorScheme;
begin
  Result := System.Default(TLogColorScheme);
  // Everything in the green family. Levels become background badges so
  // they remain distinguishable while keeping the all-green aesthetic.
  Result.PrefixColor    := STYLE_BRIGHT + FORE_GREEN;
  Result.SeparatorColor := STYLE_DIM + FORE_DARKGREEN;
  Result.TimestampColor := STYLE_DIM + FORE_GREEN;
  Result.ThreadIDColor  := STYLE_DIM + FORE_DARKGREEN;
  Result.TagColor       := FORE_GREEN;
  Result.MessageColor   := FORE_DARKGREEN;
  Result.ContextKeyColor   := STYLE_DIM + FORE_GREEN;
  Result.ContextValueColor := STYLE_BRIGHT + FORE_GREEN;
  Result.LevelColor[TLogType.Debug]   := STYLE_DIM + FORE_GREEN;
  Result.LevelColor[TLogType.Info]    := STYLE_BRIGHT + FORE_GREEN;
  Result.LevelColor[TLogType.Warning] := STYLE_BRIGHT + FORE_BLACK + BACK_GREEN;
  Result.LevelColor[TLogType.Error]   := STYLE_BRIGHT + FORE_GREEN + BACK_DARKRED;
  Result.LevelColor[TLogType.Fatal]   := STYLE_BRIGHT + FORE_GREEN + BACK_BLACK;
end;

class function LogColorSchemes.Amber: TLogColorScheme;
begin
  Result := System.Default(TLogColorScheme);
  // 80s amber CRT monitor. Yellow/orange family; red only for errors.
  Result.PrefixColor    := STYLE_BRIGHT + FORE_YELLOW;
  Result.SeparatorColor := STYLE_DIM + FORE_DARKYELLOW;
  Result.TimestampColor := STYLE_DIM + FORE_DARKYELLOW;
  Result.ThreadIDColor  := STYLE_DIM + FORE_DARKYELLOW;
  Result.TagColor       := FORE_YELLOW;
  Result.MessageColor   := FORE_DARKYELLOW;
  Result.ContextKeyColor   := STYLE_DIM + FORE_YELLOW;
  Result.ContextValueColor := STYLE_BRIGHT + FORE_YELLOW;
  Result.LevelColor[TLogType.Debug]   := STYLE_DIM + FORE_YELLOW;
  Result.LevelColor[TLogType.Info]    := STYLE_BRIGHT + FORE_YELLOW;
  Result.LevelColor[TLogType.Warning] := STYLE_BRIGHT + FORE_WHITE + BACK_DARKYELLOW;
  Result.LevelColor[TLogType.Error]   := STYLE_BRIGHT + FORE_WHITE + BACK_DARKRED;
  Result.LevelColor[TLogType.Fatal]   := STYLE_BRIGHT + FORE_YELLOW + BACK_DARKRED;
end;

class function LogColorSchemes.Ocean: TLogColorScheme;
begin
  Result := System.Default(TLogColorScheme);
  // Layered blues and cyans. Calm, cool, readable on dark terminals.
  Result.PrefixColor    := STYLE_BRIGHT + FORE_BLUE;
  Result.SeparatorColor := STYLE_DIM + FORE_DARKBLUE;
  Result.TimestampColor := STYLE_DIM + FORE_CYAN;
  Result.ThreadIDColor  := STYLE_DIM + FORE_BLUE;
  Result.TagColor       := FORE_CYAN;
  Result.MessageColor   := '';
  Result.ContextKeyColor   := STYLE_DIM + FORE_CYAN;
  Result.ContextValueColor := STYLE_BRIGHT + FORE_CYAN;
  Result.LevelColor[TLogType.Debug]   := STYLE_DIM + FORE_BLUE;
  Result.LevelColor[TLogType.Info]    := STYLE_BRIGHT + FORE_CYAN;
  Result.LevelColor[TLogType.Warning] := STYLE_BRIGHT + FORE_YELLOW;
  Result.LevelColor[TLogType.Error]   := STYLE_BRIGHT + FORE_WHITE + BACK_DARKBLUE;
  Result.LevelColor[TLogType.Fatal]   := STYLE_BRIGHT + FORE_WHITE + BACK_DARKRED;
end;

class function LogColorSchemes.Cyberpunk: TLogColorScheme;
begin
  Result := System.Default(TLogColorScheme);
  // Neon magenta + cyan, saturated badges. Built for demos and
  // screenshots that need to POP. Loud on purpose.
  Result.PrefixColor    := STYLE_BRIGHT + FORE_MAGENTA;
  Result.SeparatorColor := STYLE_DIM + FORE_MAGENTA;
  Result.TimestampColor := STYLE_DIM + FORE_CYAN;
  Result.ThreadIDColor  := STYLE_DIM + FORE_MAGENTA;
  Result.TagColor       := STYLE_BRIGHT + FORE_CYAN;
  Result.MessageColor   := FORE_WHITE;
  Result.ContextKeyColor   := STYLE_BRIGHT + FORE_MAGENTA;
  Result.ContextValueColor := STYLE_BRIGHT + FORE_YELLOW;
  Result.LevelColor[TLogType.Debug]   := STYLE_BRIGHT + FORE_BLACK + BACK_CYAN;
  Result.LevelColor[TLogType.Info]    := STYLE_BRIGHT + FORE_BLACK + BACK_MAGENTA;
  Result.LevelColor[TLogType.Warning] := STYLE_BRIGHT + FORE_BLACK + BACK_YELLOW;
  Result.LevelColor[TLogType.Error]   := STYLE_BRIGHT + FORE_WHITE + BACK_RED;
  Result.LevelColor[TLogType.Fatal]   := STYLE_BRIGHT + FORE_YELLOW + BACK_MAGENTA;
end;

{ TLogger.TCustomLogWriter }

function TCustomLogWriter.AppendersCount: Integer;
begin
  TMonitor.Enter(FLock);
  try
    Result := Self.FLogAppenders.Count;
  finally
    TMonitor.Exit(FLock);
  end;
end;

constructor TCustomLogWriter.Create(const aLogAppenders: TLogAppenderList; const aLogLevel: TLogType = TLogType.Debug);
begin
  inherited Create;
  FLock := TObject.Create;
  FFreeAllowed := False;
  FShuttingDown := False;
  FLogAppenders := aLogAppenders;
  FLogLevel := aLogLevel;
  FEnabled := True;
end;

constructor TCustomLogWriter.Create(const aLogLevel: TLogType = TLogType.Debug);
begin
  Create(TLogAppenderList.Create, aLogLevel);
end;

destructor TCustomLogWriter.Destroy;
begin
  // Call Shutdown if not already called (idempotent)
  Shutdown;

  // Free resources
  FLoggerThread.Free;
  FLogAppenders.Free;
  FLock.Free;
  inherited Destroy;
end;

function TCustomLogWriter.GetAppenders(const aIndex: Integer): ILogAppender;
begin
  TMonitor.Enter(FLock);
  try
    Result := Self.FLogAppenders[aIndex];
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TCustomLogWriter.AddAppender(const aAppender: ILogAppender);
begin
  TMonitor.Enter(FLock);
  try
    Self.FLogAppenders.Add(aAppender);
    if Assigned(Self.FLoggerThread.FAppendersDecorators) then
      Self.FLoggerThread.FAppendersDecorators.Add(TLoggerThread.TAppenderAdapter.Create(aAppender));
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TCustomLogWriter.DelAppender(const aAppender: ILogAppender);
var
  i: Integer;
begin
  TMonitor.Enter(FLock);
  try
    i := Self.FLoggerThread.FAppenders.IndexOf(aAppender);
    if i >= 0 then
      Self.FLoggerThread.FAppenders.Delete(i);

    i := Self.FLogAppenders.IndexOf(aAppender);
    if i >= 0 then
      Self.FLogAppenders.Delete(i);

    if Assigned(Self.FLoggerThread.FAppendersDecorators) then
      for i := Self.FLoggerThread.FAppendersDecorators.Count - 1 downto 0 do
        if Self.FLoggerThread.FAppendersDecorators[i].FLogAppender = aAppender then
          Self.FLoggerThread.FAppendersDecorators.Delete(i);
  finally
    TMonitor.Exit(FLock);
  end;
end;

function TCustomLogWriter.GetAppendersClassNames: TArray<string>;
var
  I: Cardinal;
begin
  TMonitor.Enter(FLock);
  try
    SetLength(Result, FLogAppenders.Count);
    for I := 0 to FLogAppenders.Count - 1 do
    begin
      Result[I] := TObject(FLogAppenders[I]).ClassName;
    end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TCustomLogWriter.Initialize(const aEventsHandler: TLoggerProEventsHandler);
begin
  FLoggerThread := TLoggerThread.Create(FLogAppenders);
  FLoggerThread.EventsHandlers := aEventsHandler;
  FLoggerThread.Start;
  if not System.IsLibrary then
  begin
    while not FLoggerThread.Started do
      Sleep(1);
  end;
  // When running in a DLL, we skip the spin-wait to avoid a deadlock
  // caused by the Windows Loader Lock. The queue is already created
  // in TLoggerThread.Create, so log items can be safely enqueued
  // before the thread begins executing.
end;

procedure TCustomLogWriter.Log(const aType: TLogType; const aMessage, aTag: string);
var
  lLogItem: TLogItem;
begin
  Assert(not FShuttingDown, 'Cannot log: logger is shutting down');
  if FShuttingDown then Exit;
  if FEnabled and (aType >= FLogLevel) then
  begin
    lLogItem := TLogItem.Create(aType, aMessage, aTag);
    try
      if not FLoggerThread.LogWriterQueue.Enqueue(lLogItem) then
      begin
        raise ELoggerPro.Create
          ('Main logs queue is full. Hints: Are there appenders? Are these appenders fast enough considering the log item production?');
      end;
    except
      FreeAndNil(lLogItem);
      raise;
    end;
  end;
end;

procedure TCustomLogWriter.Log(const aType: TLogType; const aMessage, aTag: string; const aContext: LogParams);
var
  lLogItem: TLogItem;
begin
  // Check FShuttingDown - if shutdown completed, thread is gone, can't log
  Assert(not FShuttingDown, 'Cannot log: logger has been shut down');
  if FShuttingDown then Exit;

  if FEnabled and (aType >= FLogLevel) then
  begin
    lLogItem := TLogItem.Create(aType, aMessage, aTag, aContext);
    try
      if not FLoggerThread.LogWriterQueue.Enqueue(lLogItem) then
      begin
        raise ELoggerPro.Create
          ('Main logs queue is full. Hints: Are there appenders? Are these appenders fast enough considering the log item production?');
      end;
    except
      FreeAndNil(lLogItem);
      raise;
    end;
  end;
end;

procedure TCustomLogWriter.EnqueueLogItem(const aLogItem: TLogItem);
begin
  // Check FShuttingDown - if shutdown completed, thread is gone, can't log
  Assert(not FShuttingDown, 'Cannot log: logger has been shut down');
  if FShuttingDown then
  begin
    aLogItem.Free;
    Exit;
  end;

  if FEnabled and (aLogItem.LogType >= FLogLevel) then
  begin
    if not FLoggerThread.LogWriterQueue.Enqueue(aLogItem) then
    begin
      aLogItem.Free;
      raise ELoggerPro.Create
        ('Main logs queue is full. Hints: Are there appenders? Are these appenders fast enough considering the log item production?');
    end;
  end
  else
    aLogItem.Free;
end;

{ TLogger.TLogWriter }

procedure TLogWriter.Debug(const aMessage: string);
begin
  Debug(aMessage, DEFAULT_LOG_TAG);
end;

procedure TLogWriter.Debug(const aMessage, aTag: string);
begin
  Log(TLogType.Debug, aMessage, aTag);
end;

procedure TLogWriter.Debug(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  Log(TLogType.Debug, aMessage, aParams, aTag);
end;

procedure TCustomLogWriter.Disable;
begin
  fEnabled := False;
end;

function TCustomLogWriter.GetMinimumLevel: TLogType;
begin
  // Full-fence read for ARM / weakly-ordered platforms. FLogLevel is a
  // 1-byte enum (TLogType) so x86 already gets an atomic load for free;
  // the memory barrier here guarantees the same contract on ARM.
  MemoryBarrier;
  Result := FLogLevel;
end;

procedure TCustomLogWriter.SetMinimumLevel(const aLevel: TLogType);
begin
  FLogLevel := aLevel;
  MemoryBarrier;
end;

procedure TLogWriter.Error(const aMessage: string);
begin
  Error(aMessage, DEFAULT_LOG_TAG);
end;

procedure TLogWriter.Error(const aMessage, aTag: string);
begin
  Log(TLogType.Error, aMessage, aTag);
end;

procedure TCustomLogWriter.Enable;
begin
  fEnabled := True;
end;

procedure TCustomLogWriter.Shutdown;
begin
  if FShuttingDown then
    Exit;  // Already shut down, idempotent

  // Close the faucet first: set FShuttingDown so EnqueueLogItem rejects
  // any further log attempts from other threads. Only then signal the
  // logger thread to drain. Without this ordering, a concurrent Log.Info
  // could slip an item into the queue AFTER the thread sees "queue empty
  // and Terminated" and exits - that item would be silently lost.
  FShuttingDown := True;

  if FLoggerThread <> nil then
  begin
    FLoggerThread.Terminate;
    FLoggerThread.LogWriterQueue.SetEvent;  // Wake up thread if blocked
    FLoggerThread.WaitFor;
  end;

  Disable;
end;

function TCustomLogWriter.FormatExceptionMessage(const E: Exception; const aMessage: string): string;
var
  lStackTrace: string;
  lInner: Exception;
begin
  if aMessage <> '' then
    Result := aMessage + ' - '
  else
    Result := '';

  Result := Result + E.ClassName + ': ' + E.Message;

  // Unwind chained exceptions (InnerException chain)
  lInner := E.InnerException;
  while lInner <> nil do
  begin
    Result := Result + sLineBreak + '  Caused by: ' + lInner.ClassName + ': ' + lInner.Message;
    lInner := lInner.InnerException;
  end;

  if Assigned(FStackTraceFormatter) then
  begin
    lStackTrace := FStackTraceFormatter(E);
    if lStackTrace <> '' then
      Result := Result + sLineBreak + lStackTrace;
  end;
end;

procedure TLogWriter.Error(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  Log(TLogType.Error, aMessage, aParams, aTag);
end;

procedure TLogWriter.Fatal(const aMessage: string);
begin
  Fatal(aMessage, DEFAULT_LOG_TAG);
end;

procedure TLogWriter.Fatal(const aMessage, aTag: string);
begin
  Log(TLogType.Fatal, aMessage, aTag);
end;

procedure TLogWriter.Fatal(const aMessage: string;
  const aParams: array of TVarRec; const aTag: string);
begin
  Log(TLogType.Fatal, aMessage, aParams, aTag);
end;

procedure TLogWriter.Info(const aMessage: string);
begin
  Info(aMessage, DEFAULT_LOG_TAG);
end;

procedure TLogWriter.Info(const aMessage, aTag: string);
begin
  Log(TLogType.Info, aMessage, aTag);
end;

procedure TLogWriter.Info(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  Log(TLogType.Info, aMessage, aParams, aTag);
end;

procedure TLogWriter.Log(const aType: TLogType; const aMessage: string; const aParams: array of const; const aTag: string);
begin
  Log(aType, Format(aMessage, aParams), aTag);
end;

procedure TLogWriter.Warn(const aMessage: string);
begin
  Warn(aMessage, DEFAULT_LOG_TAG);
end;

procedure TLogWriter.Warn(const aMessage, aTag: string);
begin
  Log(TLogType.Warning, aMessage, aTag);
end;

procedure TLogWriter.Warn(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  Log(TLogType.Warning, aMessage, aParams, aTag);
end;

procedure TLogWriter.Debug(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  Log(TLogType.Debug, aMessage, aTag, aContext);
end;

procedure TLogWriter.Info(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  Log(TLogType.Info, aMessage, aTag, aContext);
end;

procedure TLogWriter.Warn(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  Log(TLogType.Warning, aMessage, aTag, aContext);
end;

procedure TLogWriter.Error(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  Log(TLogType.Error, aMessage, aTag, aContext);
end;

procedure TLogWriter.Fatal(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  Log(TLogType.Fatal, aMessage, aTag, aContext);
end;

procedure TLogWriter.LogException(const E: Exception);
begin
  LogException(E, '', DEFAULT_LOG_TAG);
end;

procedure TLogWriter.LogException(const E: Exception; const aMessage: string);
begin
  LogException(E, aMessage, DEFAULT_LOG_TAG);
end;

procedure TLogWriter.LogException(const E: Exception; const aMessage: string; const aTag: string);
begin
  Log(TLogType.Error, FormatExceptionMessage(E, aMessage), aTag);
end;

procedure TLogWriter.Log(const aType: TLogType; const aMessage: string; const aTag: string; const aContext: array of LogParam);
var
  lContext: LogParams;
  I: Integer;
begin
  SetLength(lContext, Length(aContext));
  for I := 0 to High(aContext) do
    lContext[I] := aContext[I];
  inherited Log(aType, aMessage, aTag, lContext);
end;

function TLogWriter.WithProperty(const aKey: string; const aValue: string): ILogWriter;
begin
  Result := TLogWriterWithContext.Create(Self, [LogParam.S(aKey, aValue)]);
end;

function TLogWriter.WithProperty(const aKey: string; const aValue: Integer): ILogWriter;
begin
  Result := TLogWriterWithContext.Create(Self, [LogParam.I(aKey, aValue)]);
end;

function TLogWriter.WithProperty(const aKey: string; const aValue: Boolean): ILogWriter;
begin
  Result := TLogWriterWithContext.Create(Self, [LogParam.B(aKey, aValue)]);
end;

function TLogWriter.WithProperty(const aKey: string; const aValue: Double): ILogWriter;
begin
  Result := TLogWriterWithContext.Create(Self, [LogParam.F(aKey, aValue)]);
end;

function TLogWriter.WithProperty(const aKey: string; const aValue: TDateTime): ILogWriter;
begin
  Result := TLogWriterWithContext.Create(Self, [LogParam.D(aKey, aValue)]);
end;

function TLogWriter.WithProperty(const aKey: string; const aValue: TValue): ILogWriter;
begin
  Result := TLogWriterWithContext.Create(Self, [LogParam.V(aKey, aValue)]);
end;

function TLogWriter.WithPropertyFmt(const aKey: string; const aFormat: string; const aArgs: array of const): ILogWriter;
begin
  Result := TLogWriterWithContext.Create(Self, [LogParam.FmtS(aKey, aFormat, aArgs)]);
end;

function TLogWriter.WithDefaultTag(const aTag: string): ILogWriter;
begin
  Result := TLogWriterWithDefaultTag.Create(Self, aTag);
end;

function TLogWriter.WithDefaultContext(const aContext: array of LogParam): ILogWriter;
begin
  Result := TLogWriterWithDefaultContext.Create(Self, aContext);
end;

function TLogWriter.IsEnabled(aLevel: TLogType): Boolean;
begin
  Result := FEnabled and (not FShuttingDown) and (aLevel >= FLogLevel);
end;

function TLogWriter.IsDebugEnabled: Boolean;
begin
  Result := IsEnabled(TLogType.Debug);
end;

function TLogWriter.IsInfoEnabled: Boolean;
begin
  Result := IsEnabled(TLogType.Info);
end;

function TLogWriter.IsWarningEnabled: Boolean;
begin
  Result := IsEnabled(TLogType.Warning);
end;

function TLogWriter.IsErrorEnabled: Boolean;
begin
  Result := IsEnabled(TLogType.Error);
end;

function TLogWriter.IsFatalEnabled: Boolean;
begin
  Result := IsEnabled(TLogType.Fatal);
end;

{ TLogger.TLogItem }

function TLogItem.Clone: TLogItem;
begin
  Result := TLogItem.Create(FType, FMessage, FTag, FTimeStamp, FThreadID, FContext);
end;

constructor TLogItem.Create(const aType: TLogType; const aMessage, aTag: string);
begin
  Create(aType, aMessage, aTag, now, TThread.CurrentThread.ThreadID, nil);
end;

constructor TLogItem.Create(const aType: TLogType; const aMessage, aTag: string;
  const aContext: LogParams);
begin
  Create(aType, aMessage, aTag, now, TThread.CurrentThread.ThreadID, aContext);
end;

{ TLogger.TLoggerThread }

constructor TLoggerThread.Create(aAppenders: TLogAppenderList);
begin
  FQueue := TThreadSafeQueue<TLogItem>.Create(DefaultLoggerProMainQueueSize, 500);
  FAppenders := aAppenders;
  inherited Create(true);
  FreeOnTerminate := False;
end;

destructor TLoggerThread.Destroy;
begin
  FQueue.Free;
  inherited;
end;

procedure TLoggerThread.DoOnAppenderError(const FailAppenderClassName: string; const aFailedLogItem: TLogItem;
  const aReason: TLogErrorReason; var aAction: TLogErrorAction);
begin
  if Assigned(FEventsHandlers) and (Assigned(FEventsHandlers.OnAppenderError)) then
  begin
    FEventsHandlers.OnAppenderError(FailAppenderClassName, aFailedLogItem, aReason, aAction);
  end;
end;

procedure TLoggerThread.DoTerminate;
begin

  inherited;
end;

procedure TLoggerThread.DispatchLogItem(const aLogItem: TLogItem);
var
  I: Integer;
  lAction: TLogErrorAction;
begin
  for I := 0 to FAppendersDecorators.Count - 1 do
  begin
    if aLogItem.LogType >= FAppendersDecorators[I].GetMinimumLevel then
    begin
      if not FAppendersDecorators[I].EnqueueLog(aLogItem) then
      begin
        lAction := TLogErrorAction.SkipNewest; // default
        DoOnAppenderError(TObject(FAppendersDecorators[I].FLogAppender).ClassName, aLogItem,
          TLogErrorReason.QueueFull, lAction);
        case lAction of
          TLogErrorAction.SkipNewest:
            begin
              // just skip the new message
            end;
          TLogErrorAction.DiscardOlder:
            begin
              // just remove the oldest log message
              FAppendersDecorators[I].Queue.Dequeue.Free;
            end;
        end;
      end;
    end;
  end;
end;

var
  gAdapterLifecycleLock: TCriticalSection = nil;

procedure TLoggerThread.Execute;
var
  lQSize: UInt64;
  lLogItem: TLogItem;
  I: Integer;
  lWaitResult: TWaitResult;
begin
  // Serialize adapter lifecycle across ALL TLoggerThread instances in the
  // process. Without this CS, rapid create-tear-down-create sequences
  // (e.g. several loggers built back-to-back) let a new logger start
  // building its adapters before the previous one's adapter threads have
  // fully released OS-level resources. Under the IDE debugger the race
  // is visible as entire missing sections of log output: the new adapter
  // thread never gets scheduled for Setup. Holding the lock only around
  // the creation + destruction phases keeps normal runtime concurrency
  // intact.
  gAdapterLifecycleLock.Enter;
  try
    FAppendersDecorators := BuildAppenderAdapters;
  finally
    gAdapterLifecycleLock.Leave;
  end;
  try
    while (not Terminated) or (FQueue.QueueSize > 0) do
    begin
      lWaitResult := FQueue.Dequeue(lQSize, lLogItem);
      case lWaitResult of
        wrSignaled:
          begin
            if lLogItem <> nil then
            begin
              try
                DispatchLogItem(lLogItem);
              finally
                lLogItem.Free;
              end;
            end;
          end;
        wrTimeout, wrAbandoned, wrError:
          begin
            // If we're terminating and there are items in queue, wake up immediately
            // to process them instead of waiting for next timeout
            if Terminated and (FQueue.QueueSize > 0) then
              FQueue.SetEvent;
            // Continue loop; will exit when Terminated and queue is empty
          end;
        wrIOCompletion:
          begin
            raise ELoggerPro.Create('Unhandled WaitResult: wrIOCompletition');
          end;
      end;
    end;
    // Deterministic shutdown in two phases:
    //   1) broadcast the Terminate signal to ALL appender threads so they
    //      all start draining their queues in parallel;
    //   2) wait for EACH to actually finish (which includes TearDown
    //      releasing stdout / file handles / etc.).
    // The previous one-by-one Terminate+WaitFor serialised the drains -
    // a slow appender (e.g. console under debugger) delayed every later
    // appender. That caused handle-release races when the next section
    // re-opened the same resources before the previous one had let go.
    for I := 0 to FAppendersDecorators.Count - 1 do
      FAppendersDecorators[I].SignalTerminate;
    for I := 0 to FAppendersDecorators.Count - 1 do
      FAppendersDecorators[I].WaitForExit;
  finally
    gAdapterLifecycleLock.Enter;
    try
      FAppendersDecorators.Free;
    finally
      gAdapterLifecycleLock.Leave;
    end;
  end;
end;

procedure TLoggerThread.SetEventsHandlers(const Value: TLoggerProEventsHandler);
begin
  FEventsHandlers := Value;
end;

function TLoggerThread.BuildAppenderAdapters: TAppenderAdapterList;
var
  I: Integer;
begin
  Result := TAppenderAdapterList.Create;
  try
    for I := 0 to FAppenders.Count - 1 do
    begin
      Result.Add(TAppenderAdapter.Create(FAppenders[I]));
    end;
  except
    Result.Free;
    raise;
  end;
end;

constructor TLogItem.Create(const aType: TLogType; const aMessage, aTag: string; const aTimeStamp: TDateTime; const aThreadID: TThreadID);
begin
  Create(aType, aMessage, aTag, aTimeStamp, aThreadID, nil);
end;

constructor TLogItem.Create(const aType: TLogType; const aMessage, aTag: string;
  const aTimeStamp: TDateTime; const aThreadID: TThreadID; const aContext: LogParams);
begin
  inherited Create;
  FType := aType;
  FMessage := aMessage;
  FTag := aTag;
  FTimeStamp := aTimeStamp;
  FThreadID := aThreadID;
  FContext := aContext;
end;

function TLogItem.HasContext: Boolean;
begin
  Result := Length(FContext) > 0;
end;

function TLogItem.GetLogTypeAsString: string;
begin
  case FType of
    TLogType.Debug:
      Exit('DEBUG');
    TLogType.Info:
      Exit('INFO');
    TLogType.Warning:
      Exit('WARNING');
    TLogType.Error:
      Exit('ERROR');
    TLogType.Fatal:
      Exit('FATAL');
  else
    raise ELoggerPro.Create('Invalid LogType');
  end;
end;

{ TLoggerThread.TAppenderDecorator }

constructor TLoggerThread.TAppenderAdapter.Create(aAppender: ILogAppender);
const
  READY_TIMEOUT_MS = 15000; // plenty even under an attached debugger
begin
  inherited Create;
  FFailsCount := 0;
  FLogAppender := aAppender;
  FAppenderQueue := TAppenderQueue.Create(DefaultLoggerProAppenderQueueSize, 10);
  FAppenderThread := TAppenderThread.Create(FLogAppender, FAppenderQueue);
  { Block until the appender thread has reached a known state (Running or
    WaitAfterFail after the first Setup attempt). This closes the race
    where the caller returns from Create, the main TLoggerThread starts
    dispatching items to this adapter, but the OS has not yet scheduled
    the appender thread - amplified under the IDE debugger where thread
    creation is gated on the debugger processing CREATE_THREAD events. }
  FAppenderThread.WaitUntilReady(READY_TIMEOUT_MS);
end;

destructor TLoggerThread.TAppenderAdapter.Destroy;
begin
  Terminate;
  FAppenderQueue.Free;
  inherited;
end;

function TLoggerThread.TAppenderAdapter.GetMinimumLevel: TLogType;
begin
  Result := FLogAppender.GetMinimumLevel;
end;

procedure TLoggerThread.TAppenderAdapter.Terminate;
begin
  if FAppenderThread <> nil then
  begin
    FAppenderThread.Terminate;
    FAppenderQueue.SetEvent; // Wake up the thread if it's waiting in Dequeue
    FAppenderThread.WaitFor;
    FreeAndNil(FAppenderThread);
  end;
end;

procedure TLoggerThread.TAppenderAdapter.SignalTerminate;
begin
  if FAppenderThread <> nil then
  begin
    FAppenderThread.Terminate;
    FAppenderQueue.SetEvent; // Wake up the thread if it's waiting in Dequeue
  end;
end;

procedure TLoggerThread.TAppenderAdapter.WaitForExit;
begin
  if FAppenderThread <> nil then
  begin
    FAppenderThread.WaitFor;
    FreeAndNil(FAppenderThread);
  end;
end;

function TLoggerThread.TAppenderAdapter.EnqueueLog(const aLogItem: TLogItem): Boolean;
var
  lLogItem: TLogItem;
begin
  lLogItem := aLogItem.Clone;
  Result := FAppenderQueue.Enqueue(lLogItem); // = TWaitResult.wrSignaled;
  if not Result then
  begin
    lLogItem.Free;
    FFailsCount := FFailsCount + 1
  end
  else
    FFailsCount := 0;
end;

{ TLoggerProAppenderBase }

constructor TLoggerProAppenderBase.Create(aLogItemRenderer: ILogItemRenderer);
begin
  inherited Create;
  Self.FEnabled := True;
  Self.FLogLevel := TLogType.Debug;
  if Assigned(aLogItemRenderer) then
  begin
    Self.FLogItemRenderer := aLogItemRenderer;
  end
  else
  begin
    Self.FLogItemRenderer := GetDefaultLogItemRenderer;
  end;
  Self.FOnLogRow := nil;
end;

constructor TLoggerProAppenderBase.Create;
begin
  Create(nil);
end;

function TLoggerProAppenderBase.FormatLog(const ALogItem: TLogItem): string;
begin
  if Assigned(FOnLogRow) then
  begin
    FOnLogRow(ALogItem, Result);
  end
  else
  begin
    Result := FLogItemRenderer.RenderLogItem(ALogItem);
  end;
end;

function TLoggerProAppenderBase.GetLastErrorTimeStamp: TDateTime;
begin
  Result := FLastErrorTimeStamp;
end;

function TLoggerProAppenderBase.GetMinimumLevel: TLogType;
begin
  Result := FLogLevel;
end;

procedure TLoggerProAppenderBase.SetLastErrorTimeStamp(const Value: TDateTime);
begin
  FLastErrorTimeStamp := Value;
end;

procedure TLoggerProAppenderBase.SetMinimumLevel(const Value: TLogType);
begin
  FLogLevel := Value;
end;

procedure TLoggerProAppenderBase.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

function TLoggerProAppenderBase.IsEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TLoggerProAppenderBase.Setup;
begin
  FFormatSettings := GetDefaultFormatSettings;
  FLogItemRenderer.Setup;
end;

procedure TLoggerProAppenderBase.TearDown;
begin
  FLogItemRenderer.TearDown;
end;

procedure TLoggerProAppenderBase.TryToRestart(var Restarted: Boolean);
begin
  Restarted := False;
  // do nothing "smart" here... descendant must implement specific "restart" strategies
end;

{ TAppenderThread }

constructor TAppenderThread.Create(aLogAppender: ILogAppender; aAppenderQueue: TAppenderQueue);
begin
  FLogAppender := aLogAppender;
  FAppenderQueue := aAppenderQueue;
  FReadyEvent := TEvent.Create(nil, True, False, '');
  inherited Create(False);
end;

destructor TAppenderThread.Destroy;
begin
  FReadyEvent.Free;
  inherited;
end;

function TAppenderThread.WaitUntilReady(aTimeoutMs: Cardinal): Boolean;
begin
  Result := FReadyEvent.WaitFor(aTimeoutMs) = TWaitResult.wrSignaled;
end;

procedure TAppenderThread.Execute;
const
  // Cap on the WaitAfterFail<->ToRestart bounce while shutting down.
  // Each iteration is one TryToRestart call (no Sleep on shutdown), so
  // 5 attempts is plenty for an appender that legitimately recovers.
  MAX_SHUTDOWN_RESTART_ATTEMPTS = 5;
var
  lLogItem: TLogItem;
  lRestarted: Boolean;
  lStatus: TAppenderStatus;
  lSetupFailCount: Integer;
  lReadySignaled: Boolean;
  lShutdownRestartAttempts: Integer;
begin
  lSetupFailCount := 0;
  lStatus := TAppenderStatus.BeforeSetup;
  lReadySignaled := False;
  lShutdownRestartAttempts := 0;
  try
    { Drain until the queue is empty. When Terminated is set we still
      MUST flush every pending item before exiting, otherwise a failed
      WriteLog that flips the state to WaitAfterFail would silently
      discard the rest of the queue. }
    while (not Terminated) or (FAppenderQueue.QueueSize > 0) do
    begin
      try
        { this state machine handles the status of the appender }
        case lStatus of
          TAppenderStatus.BeforeTearDown:
            begin
              Break;
            end;

          TAppenderStatus.BeforeSetup:
            begin
              try
                FLogAppender.Setup;
                lStatus := TAppenderStatus.Running;
              except
                if lSetupFailCount = 10 then
                begin
                  lStatus := TAppenderStatus.WaitAfterFail;
                end
                else
                begin
                  Inc(lSetupFailCount);
                  // During shutdown we cannot afford the 1s retry wait -
                  // items are already in the queue and TLoggerThread is
                  // blocked waiting for us. Skip the sleep so the next
                  // Setup retry happens immediately.
                  if not Terminated then
                    Sleep(1000);
                end;
              end;
              { Signal "ready" after the first Setup attempt, success OR
                final failure. The caller (TAppenderAdapter.Create) may
                be blocked waiting for this signal to guarantee that the
                appender thread is actually scheduled and observing its
                queue before any log item is dispatched to it. }
              if not lReadySignaled and
                 ((lStatus = TAppenderStatus.Running) or
                  (lStatus = TAppenderStatus.WaitAfterFail)) then
              begin
                FReadyEvent.SetEvent;
                lReadySignaled := True;
              end;
            end;

          TAppenderStatus.ToRestart:
            begin
              try
                lRestarted := False;
                FLogAppender.TryToRestart(lRestarted);
                if lRestarted then
                begin
                  lStatus := TAppenderStatus.Running;
                  FLogAppender.LastErrorTimeStamp := 0;
                end
                else
                begin
                  lRestarted := False;
                  FLogAppender.LastErrorTimeStamp := now;
                  lStatus := TAppenderStatus.WaitAfterFail;
                end;
              except
                lRestarted := False;
              end;
              Failing := not lRestarted;
            end;

          TAppenderStatus.WaitAfterFail:
            begin
              // On shutdown, force immediate restart attempt so we can
              // flush the remaining queue. Otherwise back off the normal
              // 500ms and reconsider every 5s.
              if Terminated then
              begin
                // Bound the shutdown retry loop. Default TryToRestart
                // returns False forever, so without this guard we'd
                // burn a CPU spinning between WaitAfterFail and
                // ToRestart while still holding queued items - and
                // TLoggerThread.Shutdown's WaitFor would hang.
                Inc(lShutdownRestartAttempts);
                if lShutdownRestartAttempts > MAX_SHUTDOWN_RESTART_ATTEMPTS then
                begin
                  // Give up on this appender. Drop the rest of its
                  // queue so the outer drain loop can exit. Drain
                  // by result (not QueueSize) so we bail on the first
                  // wrTimeout / wrAbandoned instead of spinning on a
                  // size check that races the dequeue.
                  while FAppenderQueue.Dequeue(lLogItem) = TWaitResult.wrSignaled do
                    lLogItem.Free;
                  Break;
                end;
                lStatus := TAppenderStatus.ToRestart;
              end
              else
              begin
                Sleep(500);
                if SecondsBetween(now, FLogAppender.LastErrorTimeStamp) >= 5 then
                  lStatus := TAppenderStatus.ToRestart;
              end;
            end;

          TAppenderStatus.Running:
            begin
              if FAppenderQueue.Dequeue(lLogItem) = TWaitResult.wrSignaled then
              begin
                if lLogItem <> nil then
                begin
                  try
                    try
                      if FLogAppender.IsEnabled then
                        FLogAppender.WriteLog(lLogItem);
                    except
                      Failing := true;
                      FLogAppender.LastErrorTimeStamp := now;
                      lStatus := TAppenderStatus.WaitAfterFail;
                      Continue;
                    end;
                  finally
                    lLogItem.Free;
                  end;
                end;
              end;
            end;
        end;
      except
        // something wrong... but we cannot stop the thread. Let's retry.
      end;
    end;
  finally
    { Release anyone still blocked on WaitUntilReady, even if we never
      reached a Setup outcome (e.g. immediate Terminate on an empty queue). }
    if not lReadySignaled then
      FReadyEvent.SetEvent;
    FLogAppender.TearDown;
  end;
end;

procedure TAppenderThread.SetFailing(const Value: Boolean);
begin
  FFailing := Value;
end;

{ TLoggerThread.TAppenderAdaptersList }

constructor TLoggerThread.TAppenderAdapterList.Create;
begin
  inherited Create(true);
end;

{ TLoggerProInterfacedObject }

function TLoggerProInterfacedObject._AddRef: Integer;
begin
  Result := inherited;
end;

function TLoggerProInterfacedObject._Release: Integer;
begin
  Result := inherited;
end;


{ TLogItemRenderer }

class function TLogItemRenderer.GetDefaultLogItemRenderer: ILogItemRenderer;
begin
  Result := LoggerPro.Renderers.GetDefaultLogItemRenderer;
end;

procedure TLogItemRenderer.Setup;
begin
  // do nothing
end;

procedure TLogItemRenderer.TearDown;
begin
  // do nothing
end;

{ TLogWriterWithContext }

class function TLogWriterWithContext.RenderContextToString(const AContext: LogParams): string;
var
  I: Integer;
  lParam: LogParam;
  lValueStr: string;
  lFormatSettings: TFormatSettings;
begin
  Result := '';
  if Length(AContext) = 0 then
    Exit;

  lFormatSettings := TFormatSettings.Create;
  for I := 0 to High(AContext) do
  begin
    lParam := AContext[I];
    case lParam.Value.Kind of
      tkInteger, tkInt64:
        lValueStr := lParam.Value.AsInt64.ToString;
      tkFloat:
        if lParam.Value.TypeInfo = TypeInfo(TDateTime) then
          lValueStr := DateToISO8601(lParam.Value.AsType<TDateTime>, False)
        else
          lValueStr := FloatToStr(lParam.Value.AsExtended, lFormatSettings);
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

constructor TLogWriterWithContext.Create(const AInner: ILogWriter; const AContext: LogParams);
begin
  inherited Create;
  FInner := AInner;
  FContext := AContext;
  FPreRenderedContext := RenderContextToString(AContext);
end;

function TLogWriterWithContext.MergeContext(const aContext: array of LogParam): LogParams;
var
  I: Integer;
begin
  SetLength(Result, Length(FContext) + Length(aContext));
  for I := 0 to High(FContext) do
    Result[I] := FContext[I];
  for I := 0 to High(aContext) do
    Result[Length(FContext) + I] := aContext[I];
end;

function TLogWriterWithContext.GetAppendersClassNames: TArray<string>;
begin
  Result := FInner.GetAppendersClassNames;
end;

function TLogWriterWithContext.GetAppenders(const aIndex: Integer): ILogAppender;
begin
  Result := FInner.GetAppenders(aIndex);
end;

procedure TLogWriterWithContext.AddAppender(const aAppenders: ILogAppender);
begin
  FInner.AddAppender(aAppenders);
end;

procedure TLogWriterWithContext.DelAppender(const aAppenders: ILogAppender);
begin
  FInner.DelAppender(aAppenders);
end;

function TLogWriterWithContext.AppendersCount: Integer;
begin
  Result := FInner.AppendersCount;
end;

function TLogWriterWithContext.GetMinimumLevel: TLogType;
begin
  Result := FInner.GetMinimumLevel;
end;

procedure TLogWriterWithContext.SetMinimumLevel(const aLevel: TLogType);
begin
  FInner.SetMinimumLevel(aLevel);
end;

procedure TLogWriterWithContext.EnqueueLogItem(const aLogItem: TLogItem);
begin
  FInner.EnqueueLogItem(aLogItem);
end;

procedure TLogWriterWithContext.Log(const aType: TLogType; const aMessage, aTag: string);
var
  lLogItem: TLogItem;
begin
  lLogItem := TLogItem.Create(aType, aMessage, aTag, FContext);
  lLogItem.PreRenderedContext := FPreRenderedContext;
  FInner.EnqueueLogItem(lLogItem);
end;

procedure TLogWriterWithContext.Debug(const aMessage: string);
begin
  Debug(aMessage, DEFAULT_LOG_TAG);
end;

procedure TLogWriterWithContext.Info(const aMessage: string);
begin
  Info(aMessage, DEFAULT_LOG_TAG);
end;

procedure TLogWriterWithContext.Warn(const aMessage: string);
begin
  Warn(aMessage, DEFAULT_LOG_TAG);
end;

procedure TLogWriterWithContext.Error(const aMessage: string);
begin
  Error(aMessage, DEFAULT_LOG_TAG);
end;

procedure TLogWriterWithContext.Fatal(const aMessage: string);
begin
  Fatal(aMessage, DEFAULT_LOG_TAG);
end;

procedure TLogWriterWithContext.Debug(const aMessage, aTag: string);
begin
  Log(TLogType.Debug, aMessage, aTag);
end;

procedure TLogWriterWithContext.Debug(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  FInner.Log(TLogType.Debug, Format(aMessage, aParams), aTag, FContext);
end;

procedure TLogWriterWithContext.Debug(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Log(TLogType.Debug, aMessage, aTag, MergeContext(aContext));
end;

procedure TLogWriterWithContext.Info(const aMessage, aTag: string);
begin
  Log(TLogType.Info, aMessage, aTag);
end;

procedure TLogWriterWithContext.Info(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  FInner.Log(TLogType.Info, Format(aMessage, aParams), aTag, FContext);
end;

procedure TLogWriterWithContext.Info(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Log(TLogType.Info, aMessage, aTag, MergeContext(aContext));
end;

procedure TLogWriterWithContext.Warn(const aMessage, aTag: string);
begin
  Log(TLogType.Warning, aMessage, aTag);
end;

procedure TLogWriterWithContext.Warn(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  FInner.Log(TLogType.Warning, Format(aMessage, aParams), aTag, FContext);
end;

procedure TLogWriterWithContext.Warn(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Log(TLogType.Warning, aMessage, aTag, MergeContext(aContext));
end;

procedure TLogWriterWithContext.Error(const aMessage, aTag: string);
begin
  Log(TLogType.Error, aMessage, aTag);
end;

procedure TLogWriterWithContext.Error(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  FInner.Log(TLogType.Error, Format(aMessage, aParams), aTag, FContext);
end;

procedure TLogWriterWithContext.Error(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Log(TLogType.Error, aMessage, aTag, MergeContext(aContext));
end;

procedure TLogWriterWithContext.Fatal(const aMessage, aTag: string);
begin
  Log(TLogType.Fatal, aMessage, aTag);
end;

procedure TLogWriterWithContext.Fatal(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  FInner.Log(TLogType.Fatal, Format(aMessage, aParams), aTag, FContext);
end;

procedure TLogWriterWithContext.Fatal(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Log(TLogType.Fatal, aMessage, aTag, MergeContext(aContext));
end;

procedure TLogWriterWithContext.LogException(const E: Exception);
begin
  FInner.LogException(E);
end;

procedure TLogWriterWithContext.LogException(const E: Exception; const aMessage: string);
begin
  FInner.LogException(E, aMessage);
end;

procedure TLogWriterWithContext.LogException(const E: Exception; const aMessage: string; const aTag: string);
begin
  FInner.LogException(E, aMessage, aTag);
end;

procedure TLogWriterWithContext.Log(const aType: TLogType; const aMessage: string;
  const aParams: array of const; const aTag: string);
begin
  FInner.Log(aType, Format(aMessage, aParams), aTag, FContext);
end;

procedure TLogWriterWithContext.Log(const aType: TLogType; const aMessage: string;
  const aTag: string; const aContext: array of LogParam);
begin
  FInner.Log(aType, aMessage, aTag, MergeContext(aContext));
end;

function TLogWriterWithContext.WithProperty(const aKey: string; const aValue: string): ILogWriter;
begin
  Result := TLogWriterWithContext.Create(FInner, FContext + [LogParam.S(aKey, aValue)]);
end;

function TLogWriterWithContext.WithProperty(const aKey: string; const aValue: Integer): ILogWriter;
begin
  Result := TLogWriterWithContext.Create(FInner, FContext + [LogParam.I(aKey, aValue)]);
end;

function TLogWriterWithContext.WithProperty(const aKey: string; const aValue: Boolean): ILogWriter;
begin
  Result := TLogWriterWithContext.Create(FInner, FContext + [LogParam.B(aKey, aValue)]);
end;

function TLogWriterWithContext.WithProperty(const aKey: string; const aValue: Double): ILogWriter;
begin
  Result := TLogWriterWithContext.Create(FInner, FContext + [LogParam.F(aKey, aValue)]);
end;

function TLogWriterWithContext.WithProperty(const aKey: string; const aValue: TDateTime): ILogWriter;
begin
  Result := TLogWriterWithContext.Create(FInner, FContext + [LogParam.D(aKey, aValue)]);
end;

function TLogWriterWithContext.WithProperty(const aKey: string; const aValue: TValue): ILogWriter;
begin
  Result := TLogWriterWithContext.Create(FInner, FContext + [LogParam.V(aKey, aValue)]);
end;

function TLogWriterWithContext.WithPropertyFmt(const aKey: string; const aFormat: string; const aArgs: array of const): ILogWriter;
begin
  Result := TLogWriterWithContext.Create(FInner, FContext + [LogParam.FmtS(aKey, aFormat, aArgs)]);
end;

function TLogWriterWithContext.WithDefaultTag(const aTag: string): ILogWriter;
begin
  Result := TLogWriterWithDefaultTag.Create(Self, aTag);
end;

function TLogWriterWithContext.WithDefaultContext(const aContext: array of LogParam): ILogWriter;
begin
  Result := TLogWriterWithDefaultContext.Create(Self, aContext);
end;

procedure TLogWriterWithContext.Disable;
begin
  FInner.Disable;
end;

procedure TLogWriterWithContext.Enable;
begin
  FInner.Enable;
end;

procedure TLogWriterWithContext.Shutdown;
begin
  FInner.Shutdown;
end;

function TLogWriterWithContext.IsEnabled(aLevel: TLogType): Boolean;
begin
  Result := FInner.IsEnabled(aLevel);
end;

function TLogWriterWithContext.IsDebugEnabled: Boolean;   begin Result := FInner.IsDebugEnabled;   end;
function TLogWriterWithContext.IsInfoEnabled: Boolean;    begin Result := FInner.IsInfoEnabled;    end;
function TLogWriterWithContext.IsWarningEnabled: Boolean; begin Result := FInner.IsWarningEnabled; end;
function TLogWriterWithContext.IsErrorEnabled: Boolean;   begin Result := FInner.IsErrorEnabled;   end;
function TLogWriterWithContext.IsFatalEnabled: Boolean;   begin Result := FInner.IsFatalEnabled;   end;

{ TLogWriterWithDefaultTag }

constructor TLogWriterWithDefaultTag.Create(const AInner: ILogWriter; const ADefaultTag: string);
begin
  inherited Create;
  FInner := AInner;
  FDefaultTag := ADefaultTag;
end;

function TLogWriterWithDefaultTag.GetAppendersClassNames: TArray<string>;
begin
  Result := FInner.GetAppendersClassNames;
end;

function TLogWriterWithDefaultTag.GetAppenders(const aIndex: Integer): ILogAppender;
begin
  Result := FInner.GetAppenders(aIndex);
end;

procedure TLogWriterWithDefaultTag.AddAppender(const aAppenders: ILogAppender);
begin
  FInner.AddAppender(aAppenders);
end;

procedure TLogWriterWithDefaultTag.DelAppender(const aAppenders: ILogAppender);
begin
  FInner.DelAppender(aAppenders);
end;

function TLogWriterWithDefaultTag.AppendersCount: Integer;
begin
  Result := FInner.AppendersCount;
end;

function TLogWriterWithDefaultTag.GetMinimumLevel: TLogType;
begin
  Result := FInner.GetMinimumLevel;
end;

procedure TLogWriterWithDefaultTag.SetMinimumLevel(const aLevel: TLogType);
begin
  FInner.SetMinimumLevel(aLevel);
end;

procedure TLogWriterWithDefaultTag.EnqueueLogItem(const aLogItem: TLogItem);
begin
  FInner.EnqueueLogItem(aLogItem);
end;

procedure TLogWriterWithDefaultTag.Log(const aType: TLogType; const aMessage, aTag: string);
begin
  FInner.Log(aType, aMessage, aTag);
end;

procedure TLogWriterWithDefaultTag.Debug(const aMessage: string);
begin
  FInner.Debug(aMessage, FDefaultTag);
end;

procedure TLogWriterWithDefaultTag.Info(const aMessage: string);
begin
  FInner.Info(aMessage, FDefaultTag);
end;

procedure TLogWriterWithDefaultTag.Warn(const aMessage: string);
begin
  FInner.Warn(aMessage, FDefaultTag);
end;

procedure TLogWriterWithDefaultTag.Error(const aMessage: string);
begin
  FInner.Error(aMessage, FDefaultTag);
end;

procedure TLogWriterWithDefaultTag.Fatal(const aMessage: string);
begin
  FInner.Fatal(aMessage, FDefaultTag);
end;

procedure TLogWriterWithDefaultTag.Debug(const aMessage, aTag: string);
begin
  FInner.Debug(aMessage, aTag);
end;

procedure TLogWriterWithDefaultTag.Debug(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  FInner.Debug(aMessage, aParams, aTag);
end;

procedure TLogWriterWithDefaultTag.Debug(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Debug(aMessage, aTag, aContext);
end;

procedure TLogWriterWithDefaultTag.Info(const aMessage, aTag: string);
begin
  FInner.Info(aMessage, aTag);
end;

procedure TLogWriterWithDefaultTag.Info(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  FInner.Info(aMessage, aParams, aTag);
end;

procedure TLogWriterWithDefaultTag.Info(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Info(aMessage, aTag, aContext);
end;

procedure TLogWriterWithDefaultTag.Warn(const aMessage, aTag: string);
begin
  FInner.Warn(aMessage, aTag);
end;

procedure TLogWriterWithDefaultTag.Warn(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  FInner.Warn(aMessage, aParams, aTag);
end;

procedure TLogWriterWithDefaultTag.Warn(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Warn(aMessage, aTag, aContext);
end;

procedure TLogWriterWithDefaultTag.Error(const aMessage, aTag: string);
begin
  FInner.Error(aMessage, aTag);
end;

procedure TLogWriterWithDefaultTag.Error(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  FInner.Error(aMessage, aParams, aTag);
end;

procedure TLogWriterWithDefaultTag.Error(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Error(aMessage, aTag, aContext);
end;

procedure TLogWriterWithDefaultTag.Fatal(const aMessage, aTag: string);
begin
  FInner.Fatal(aMessage, aTag);
end;

procedure TLogWriterWithDefaultTag.Fatal(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  FInner.Fatal(aMessage, aParams, aTag);
end;

procedure TLogWriterWithDefaultTag.Fatal(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Fatal(aMessage, aTag, aContext);
end;

procedure TLogWriterWithDefaultTag.LogException(const E: Exception);
begin
  FInner.LogException(E, '', FDefaultTag);
end;

procedure TLogWriterWithDefaultTag.LogException(const E: Exception; const aMessage: string);
begin
  FInner.LogException(E, aMessage, FDefaultTag);
end;

procedure TLogWriterWithDefaultTag.LogException(const E: Exception; const aMessage: string; const aTag: string);
begin
  FInner.LogException(E, aMessage, aTag);
end;

procedure TLogWriterWithDefaultTag.Log(const aType: TLogType; const aMessage: string; const aParams: array of const; const aTag: string);
begin
  FInner.Log(aType, aMessage, aParams, aTag);
end;

procedure TLogWriterWithDefaultTag.Log(const aType: TLogType; const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Log(aType, aMessage, aTag, aContext);
end;

function TLogWriterWithDefaultTag.WithProperty(const aKey: string; const aValue: string): ILogWriter;
begin
  Result := TLogWriterWithDefaultTag.Create(FInner.WithProperty(aKey, aValue), FDefaultTag);
end;

function TLogWriterWithDefaultTag.WithProperty(const aKey: string; const aValue: Integer): ILogWriter;
begin
  Result := TLogWriterWithDefaultTag.Create(FInner.WithProperty(aKey, aValue), FDefaultTag);
end;

function TLogWriterWithDefaultTag.WithProperty(const aKey: string; const aValue: Boolean): ILogWriter;
begin
  Result := TLogWriterWithDefaultTag.Create(FInner.WithProperty(aKey, aValue), FDefaultTag);
end;

function TLogWriterWithDefaultTag.WithProperty(const aKey: string; const aValue: Double): ILogWriter;
begin
  Result := TLogWriterWithDefaultTag.Create(FInner.WithProperty(aKey, aValue), FDefaultTag);
end;

function TLogWriterWithDefaultTag.WithProperty(const aKey: string; const aValue: TDateTime): ILogWriter;
begin
  Result := TLogWriterWithDefaultTag.Create(FInner.WithProperty(aKey, aValue), FDefaultTag);
end;

function TLogWriterWithDefaultTag.WithProperty(const aKey: string; const aValue: TValue): ILogWriter;
begin
  Result := TLogWriterWithDefaultTag.Create(FInner.WithProperty(aKey, aValue), FDefaultTag);
end;

function TLogWriterWithDefaultTag.WithPropertyFmt(const aKey: string; const aFormat: string; const aArgs: array of const): ILogWriter;
begin
  Result := TLogWriterWithDefaultTag.Create(FInner.WithPropertyFmt(aKey, aFormat, aArgs), FDefaultTag);
end;

function TLogWriterWithDefaultTag.WithDefaultTag(const aTag: string): ILogWriter;
begin
  Result := TLogWriterWithDefaultTag.Create(FInner, aTag);
end;

function TLogWriterWithDefaultTag.WithDefaultContext(const aContext: array of LogParam): ILogWriter;
begin
  Result := TLogWriterWithDefaultContext.Create(Self, aContext);
end;

procedure TLogWriterWithDefaultTag.Disable;
begin
  FInner.Disable;
end;

procedure TLogWriterWithDefaultTag.Enable;
begin
  FInner.Enable;
end;

procedure TLogWriterWithDefaultTag.Shutdown;
begin
  FInner.Shutdown;
end;

function TLogWriterWithDefaultTag.IsEnabled(aLevel: TLogType): Boolean;
begin
  Result := FInner.IsEnabled(aLevel);
end;

function TLogWriterWithDefaultTag.IsDebugEnabled: Boolean;   begin Result := FInner.IsDebugEnabled;   end;
function TLogWriterWithDefaultTag.IsInfoEnabled: Boolean;    begin Result := FInner.IsInfoEnabled;    end;
function TLogWriterWithDefaultTag.IsWarningEnabled: Boolean; begin Result := FInner.IsWarningEnabled; end;
function TLogWriterWithDefaultTag.IsErrorEnabled: Boolean;   begin Result := FInner.IsErrorEnabled;   end;
function TLogWriterWithDefaultTag.IsFatalEnabled: Boolean;   begin Result := FInner.IsFatalEnabled;   end;

{ TLogWriterWithDefaultContext }

constructor TLogWriterWithDefaultContext.Create(const AInner: ILogWriter; const ADefaultContext: array of LogParam);
var
  I: Integer;
begin
  inherited Create;
  FInner := AInner;
  SetLength(FDefaultContext, Length(ADefaultContext));
  for I := 0 to High(ADefaultContext) do
    FDefaultContext[I] := ADefaultContext[I];
end;

function TLogWriterWithDefaultContext.MergeContext(const aContext: array of LogParam): LogParams;
var
  I: Integer;
begin
  SetLength(Result, Length(FDefaultContext) + Length(aContext));
  for I := 0 to High(FDefaultContext) do
    Result[I] := FDefaultContext[I];
  for I := 0 to High(aContext) do
    Result[Length(FDefaultContext) + I] := aContext[I];
end;

function TLogWriterWithDefaultContext.GetAppendersClassNames: TArray<string>;
begin
  Result := FInner.GetAppendersClassNames;
end;

function TLogWriterWithDefaultContext.GetAppenders(const aIndex: Integer): ILogAppender;
begin
  Result := FInner.GetAppenders(aIndex);
end;

procedure TLogWriterWithDefaultContext.AddAppender(const aAppenders: ILogAppender);
begin
  FInner.AddAppender(aAppenders);
end;

procedure TLogWriterWithDefaultContext.DelAppender(const aAppenders: ILogAppender);
begin
  FInner.DelAppender(aAppenders);
end;

function TLogWriterWithDefaultContext.AppendersCount: Integer;
begin
  Result := FInner.AppendersCount;
end;

function TLogWriterWithDefaultContext.GetMinimumLevel: TLogType;
begin
  Result := FInner.GetMinimumLevel;
end;

procedure TLogWriterWithDefaultContext.SetMinimumLevel(const aLevel: TLogType);
begin
  FInner.SetMinimumLevel(aLevel);
end;

procedure TLogWriterWithDefaultContext.EnqueueLogItem(const aLogItem: TLogItem);
begin
  FInner.EnqueueLogItem(aLogItem);
end;

procedure TLogWriterWithDefaultContext.Log(const aType: TLogType; const aMessage, aTag: string);
begin
  FInner.Log(aType, aMessage, aTag, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Debug(const aMessage: string);
begin
  FInner.Debug(aMessage, DEFAULT_LOG_TAG, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Info(const aMessage: string);
begin
  FInner.Info(aMessage, DEFAULT_LOG_TAG, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Warn(const aMessage: string);
begin
  FInner.Warn(aMessage, DEFAULT_LOG_TAG, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Error(const aMessage: string);
begin
  FInner.Error(aMessage, DEFAULT_LOG_TAG, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Fatal(const aMessage: string);
begin
  FInner.Fatal(aMessage, DEFAULT_LOG_TAG, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Debug(const aMessage, aTag: string);
begin
  FInner.Debug(aMessage, aTag, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Debug(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  FInner.Debug(Format(aMessage, aParams), aTag, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Debug(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Debug(aMessage, aTag, MergeContext(aContext));
end;

procedure TLogWriterWithDefaultContext.Info(const aMessage, aTag: string);
begin
  FInner.Info(aMessage, aTag, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Info(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  FInner.Info(Format(aMessage, aParams), aTag, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Info(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Info(aMessage, aTag, MergeContext(aContext));
end;

procedure TLogWriterWithDefaultContext.Warn(const aMessage, aTag: string);
begin
  FInner.Warn(aMessage, aTag, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Warn(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  FInner.Warn(Format(aMessage, aParams), aTag, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Warn(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Warn(aMessage, aTag, MergeContext(aContext));
end;

procedure TLogWriterWithDefaultContext.Error(const aMessage, aTag: string);
begin
  FInner.Error(aMessage, aTag, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Error(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  FInner.Error(Format(aMessage, aParams), aTag, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Error(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Error(aMessage, aTag, MergeContext(aContext));
end;

procedure TLogWriterWithDefaultContext.Fatal(const aMessage, aTag: string);
begin
  FInner.Fatal(aMessage, aTag, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Fatal(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  FInner.Fatal(Format(aMessage, aParams), aTag, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Fatal(const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Fatal(aMessage, aTag, MergeContext(aContext));
end;

procedure TLogWriterWithDefaultContext.LogException(const E: Exception);
begin
  FInner.Error(E.ClassName + ': ' + E.Message, DEFAULT_LOG_TAG, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.LogException(const E: Exception; const aMessage: string);
begin
  FInner.Error(E.ClassName + ': ' + E.Message + ' - ' + aMessage, DEFAULT_LOG_TAG, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.LogException(const E: Exception; const aMessage: string; const aTag: string);
begin
  FInner.Error(E.ClassName + ': ' + E.Message + ' - ' + aMessage, aTag, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Log(const aType: TLogType; const aMessage: string; const aParams: array of const; const aTag: string);
begin
  FInner.Log(aType, Format(aMessage, aParams), aTag, FDefaultContext);
end;

procedure TLogWriterWithDefaultContext.Log(const aType: TLogType; const aMessage: string; const aTag: string; const aContext: array of LogParam);
begin
  FInner.Log(aType, aMessage, aTag, MergeContext(aContext));
end;

function TLogWriterWithDefaultContext.WithProperty(const aKey: string; const aValue: string): ILogWriter;
begin
  Result := TLogWriterWithDefaultContext.Create(FInner, FDefaultContext + [LogParam.S(aKey, aValue)]);
end;

function TLogWriterWithDefaultContext.WithProperty(const aKey: string; const aValue: Integer): ILogWriter;
begin
  Result := TLogWriterWithDefaultContext.Create(FInner, FDefaultContext + [LogParam.I(aKey, aValue)]);
end;

function TLogWriterWithDefaultContext.WithProperty(const aKey: string; const aValue: Boolean): ILogWriter;
begin
  Result := TLogWriterWithDefaultContext.Create(FInner, FDefaultContext + [LogParam.B(aKey, aValue)]);
end;

function TLogWriterWithDefaultContext.WithProperty(const aKey: string; const aValue: Double): ILogWriter;
begin
  Result := TLogWriterWithDefaultContext.Create(FInner, FDefaultContext + [LogParam.F(aKey, aValue)]);
end;

function TLogWriterWithDefaultContext.WithProperty(const aKey: string; const aValue: TDateTime): ILogWriter;
begin
  Result := TLogWriterWithDefaultContext.Create(FInner, FDefaultContext + [LogParam.D(aKey, aValue)]);
end;

function TLogWriterWithDefaultContext.WithProperty(const aKey: string; const aValue: TValue): ILogWriter;
begin
  Result := TLogWriterWithDefaultContext.Create(FInner, FDefaultContext + [LogParam.V(aKey, aValue)]);
end;

function TLogWriterWithDefaultContext.WithPropertyFmt(const aKey: string; const aFormat: string; const aArgs: array of const): ILogWriter;
begin
  Result := TLogWriterWithDefaultContext.Create(FInner, FDefaultContext + [LogParam.FmtS(aKey, aFormat, aArgs)]);
end;

function TLogWriterWithDefaultContext.WithDefaultTag(const aTag: string): ILogWriter;
begin
  Result := TLogWriterWithDefaultTag.Create(Self, aTag);
end;

function TLogWriterWithDefaultContext.WithDefaultContext(const aContext: array of LogParam): ILogWriter;
begin
  Result := TLogWriterWithDefaultContext.Create(FInner, MergeContext(aContext));
end;

procedure TLogWriterWithDefaultContext.Disable;
begin
  FInner.Disable;
end;

procedure TLogWriterWithDefaultContext.Enable;
begin
  FInner.Enable;
end;

procedure TLogWriterWithDefaultContext.Shutdown;
begin
  FInner.Shutdown;
end;

function TLogWriterWithDefaultContext.IsEnabled(aLevel: TLogType): Boolean;
begin
  Result := FInner.IsEnabled(aLevel);
end;

function TLogWriterWithDefaultContext.IsDebugEnabled: Boolean;   begin Result := FInner.IsDebugEnabled;   end;
function TLogWriterWithDefaultContext.IsInfoEnabled: Boolean;    begin Result := FInner.IsInfoEnabled;    end;
function TLogWriterWithDefaultContext.IsWarningEnabled: Boolean; begin Result := FInner.IsWarningEnabled; end;
function TLogWriterWithDefaultContext.IsErrorEnabled: Boolean;   begin Result := FInner.IsErrorEnabled;   end;
function TLogWriterWithDefaultContext.IsFatalEnabled: Boolean;   begin Result := FInner.IsFatalEnabled;   end;

initialization
  gAdapterLifecycleLock := TCriticalSection.Create;

finalization
  FreeAndNil(gAdapterLifecycleLock);

end.

