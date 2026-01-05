// *************************************************************************** }
//
// LoggerPro
//
// Copyright (c) 2010-2025 Daniele Teti
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
  ThreadSafeQueueU, System.SysUtils;

const
  DEFAULT_LOG_TAG = 'main';

var
  DefaultLoggerProMainQueueSize: Cardinal = 50000;
  DefaultLoggerProAppenderQueueSize: Cardinal = 50000;

type
  TLogType = (Debug = 0, Info, Warning, Error, Fatal);
  TLogErrorReason = (QueueFull);
  TLogErrorAction = (SkipNewest, DiscardOlder);
  TLogExtendedInfo = (EIUserName, EIComputerName, EIProcessName, EIProcessID, EIDeviceID { mobile });
  TLoggerProExtendedInfo = set of TLogExtendedInfo;

  { @abstract(Represents a key-value pair for structured logging context) }
  LogParam = record
    Key: string;
    Value: TValue;
    class function S(const AKey: string; const AValue: string): LogParam; static; inline;
    class function I(const AKey: string; const AValue: Integer): LogParam; static; inline;
    class function B(const AKey: string; const AValue: Boolean): LogParam; static; inline;
    class function F(const AKey: string; const AValue: Double): LogParam; static; inline;
    class function D(const AKey: string; const AValue: TDateTime): LogParam; static; inline;
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
    { @abstract(Set a custom log level for this appender. This value must be lower than the global LogWriter log level. }
    procedure SetLogLevel(const Value: TLogType);
    { @abstract(Get the loglevel for the appender. }
    function GetLogLevel: TLogType;
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

    procedure Disable;
    procedure Enable;
  end;

  TLogAppenderList = TList<ILogAppender>;

  TAppenderThread = class(TThread)
  private
    FLogAppender: ILogAppender;
    FAppenderQueue: TAppenderQueue;
    FFailing: Boolean;
    procedure SetFailing(const Value: Boolean);
  protected
    procedure Execute; override;

    type
      TAppenderStatus = (BeforeSetup, Running, WaitAfterFail, ToRestart, BeforeTearDown);
  public
    constructor Create(aLogAppender: ILogAppender; aAppenderQueue: TAppenderQueue);
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
        function GetLogLevel: TLogType;
        procedure Terminate;
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
    FLogLevel: TLogType;
    FLock: TObject;
    FShuttingDown: Boolean;
    FStackTraceFormatter: TStackTraceFormatter;
    function GetAppendersClassNames: TArray<string>;
  protected
    FEnabled: Boolean;
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

    property StackTraceFormatter: TStackTraceFormatter read FStackTraceFormatter write FStackTraceFormatter;
    property MinimumLevel: TLogType read FLogLevel write FLogLevel;
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

    procedure Disable;
    procedure Enable;
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

    procedure Disable;
    procedure Enable;
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
    procedure SetLogLevel(const Value: TLogType);
    function GetLogLevel: TLogType; inline;
    procedure SetLastErrorTimeStamp(const Value: TDateTime);
    function GetLastErrorTimeStamp: TDateTime;
    procedure SetEnabled(const Value: Boolean);
    function IsEnabled: Boolean;
    property LogLevel: TLogType read GetLogLevel write SetLogLevel;
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


function GetDefaultFormatSettings: TFormatSettings;
function StringToLogType(const aLogType: string): TLogType;
function BuildLogWriter(aAppenders: array of ILogAppender; aEventsHandlers: TLoggerProEventsHandler = nil;
  aLogLevel: TLogType = TLogType.Debug): ILogWriter; overload;
function BuildLogWriter(aAppenders: array of ILogAppender; aEventsHandlers: TLoggerProEventsHandler;
  aLogLevels: TArray<TLogType>): ILogWriter; overload;
function LogLayoutByPlaceHoldersToLogLayoutByIndexes(const LogLayoutByPlaceHolders: String; const UseZeroBasedIncrementalIndexes: Boolean): String;

implementation

uses
  System.Types,
  System.TypInfo,
  LoggerPro.FileAppender,
  System.SyncObjs,
  System.DateUtils,
  System.IOUtils,
  LoggerPro.Renderers;

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
    aAppenders[I].SetLogLevel(aLogLevels[I]);
    if aLogLevels[I] < lLowestLogLevel then
    begin
      lLowestLogLevel := aLogLevels[I];
    end;
  end;
  Result := TLogWriter.Create(lLogAppenders, lLowestLogLevel);
  TLogWriter(Result).Initialize(aEventsHandlers);
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
  FShuttingDown := True;  // Signal shutdown before anything else
  Disable;
  FLoggerThread.Terminate;
  FLoggerThread.LogWriterQueue.SetEvent; // Wake up thread if blocked in Dequeue
  FLoggerThread.WaitFor;
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
  while not FLoggerThread.Started do
    Sleep(1); // Wait for thread to actually start
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
  Assert(not FShuttingDown, 'Cannot log: logger is shutting down');
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
  Assert(not FShuttingDown, 'Cannot log: logger is shutting down');
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

function TCustomLogWriter.FormatExceptionMessage(const E: Exception; const aMessage: string): string;
var
  lStackTrace: string;
begin
  if aMessage <> '' then
    Result := aMessage + ' - '
  else
    Result := '';

  Result := Result + E.ClassName + ': ' + E.Message;

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
    if aLogItem.LogType >= FAppendersDecorators[I].GetLogLevel then
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

procedure TLoggerThread.Execute;
var
  lQSize: UInt64;
  lLogItem: TLogItem;
  I: Integer;
  lWaitResult: TWaitResult;
begin
  FAppendersDecorators := BuildAppenderAdapters;
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
            // Continue loop; will exit when Terminated and queue is empty
          end;
        wrIOCompletion:
          begin
            raise ELoggerPro.Create('Unhandled WaitResult: wrIOCompletition');
          end;
      end;
    end;
    // Terminate all appenders (they will flush their own queues)
    for I := 0 to FAppendersDecorators.Count - 1 do
    begin
      FAppendersDecorators[I].Terminate;
    end;
  finally
    FAppendersDecorators.Free;
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
begin
  inherited Create;
  FFailsCount := 0;
  FLogAppender := aAppender;
  FAppenderQueue := TAppenderQueue.Create(DefaultLoggerProAppenderQueueSize, 10);
  FAppenderThread := TAppenderThread.Create(FLogAppender, FAppenderQueue);
end;

destructor TLoggerThread.TAppenderAdapter.Destroy;
begin
  Terminate;
  FAppenderQueue.Free;
  inherited;
end;

function TLoggerThread.TAppenderAdapter.GetLogLevel: TLogType;
begin
  Result := FLogAppender.GetLogLevel;
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

function TLoggerProAppenderBase.GetLogLevel: TLogType;
begin
  Result := FLogLevel;
end;

procedure TLoggerProAppenderBase.SetLastErrorTimeStamp(const Value: TDateTime);
begin
  FLastErrorTimeStamp := Value;
end;

procedure TLoggerProAppenderBase.SetLogLevel(const Value: TLogType);
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
  inherited Create(False);
end;

procedure TAppenderThread.Execute;
var
  lLogItem: TLogItem;
  lRestarted: Boolean;
  lStatus: TAppenderStatus;
  lSetupFailCount: Integer;
begin
  lSetupFailCount := 0;
  lStatus := TAppenderStatus.BeforeSetup;
  try
    { the appender tries to log all the messages before terminate... }
    //dt
    while (not Terminated) or (FAppenderQueue.QueueSize > 0) do
    begin
      { ...but if the thread should be terminated, and the appender is failing,
        its messages will be lost }
      if Terminated and (lStatus = TAppenderStatus.WaitAfterFail) then
        Break;

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
                  Sleep(1000); // wait before next setup call
                end;
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
              Sleep(500);
              if SecondsBetween(now, FLogAppender.LastErrorTimeStamp) >= 5 then
                lStatus := TAppenderStatus.ToRestart;
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
    Result := Result + ' ' + lParam.Key + '=' + lValueStr;
  end;
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

procedure TLogWriterWithContext.Disable;
begin
  FInner.Disable;
end;

procedure TLogWriterWithContext.Enable;
begin
  FInner.Enable;
end;

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

procedure TLogWriterWithDefaultTag.Disable;
begin
  FInner.Disable;
end;

procedure TLogWriterWithDefaultTag.Enable;
begin
  FInner.Enable;
end;

end.

