// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
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
// *************************************************************************** }

unit MVCFramework;

{$I dmvcframework.inc}

{$IFDEF ANDROID OR IOS}
{$MESSAGE Fatal 'This unit is not compilable on mobile platforms'}
{$ENDIF}

{$RTTI EXPLICIT
METHODS([vcPublic, vcPublished, vcProtected])
FIELDS(DefaultFieldRttiVisibility)
PROPERTIES(DefaultPropertyRttiVisibility)}
{$WARNINGS OFF}

interface

uses
  System.Generics.Collections,
  MVCFramework.Logger,
  Web.HTTPApp,
  System.RTTI,
  System.Classes,
  Data.DB,
  System.SysUtils,
  MVCFramework.Commons,
  MVCFramework.View.Cache,
  IdHeaderList,
  MVCFramework.ApplicationSession,
  MVCFramework.Session,
  StompClient,
  ObjectsMappers
{$IFDEF SYSTEMJSON}
    , System.JSON
{$ELSE}
    , Data.DBXJSON
{$ENDIF}
{$IFDEF WEBAPACHEHTTP}
    , Web.ApacheHTTP
  // Apache Support since XE6 http://docwiki.embarcadero.com/Libraries/XE6/de/Web.ApacheHTTP
{$ENDIF}
    , ReqMulti {Delphi XE4 (all update) and XE5 (with no update) dont contains this unit. Look for the bug in QC}
    , LoggerPro
    , MVCFramework.DuckTyping
    , MVCFramework.Patches;

type
  TDMVCSerializationType = TSerializationType;
  TMVCCustomData = TDictionary<string, string>;
  TSessionData = TMVCCustomData;

  // RTTI ATTRIBUTES

  MVCHTTPMethodAttribute = class(TCustomAttribute)
  private
    FMVCHTTPMethods: TMVCHTTPMethods;
    function GetMVCHTTPMethodsAsString: string;

  public
    constructor Create(AMVCHTTPMethods: TMVCHTTPMethods);
    property MVCHTTPMethods: TMVCHTTPMethods read FMVCHTTPMethods;
    property MVCHTTPMethodsAsString: string read GetMVCHTTPMethodsAsString;

  end;

  MVCHTTPMethodsAttribute = MVCHTTPMethodAttribute; // just an alias

  MVCBaseAttribute = class(TCustomAttribute)

  end;

  MVCStringAttribute = class(MVCBaseAttribute)
  private
    FValue: string;

  public
    constructor Create(const Value: string);
    property Value: string read FValue;
  end;

  MVCConsumesAttribute = class(MVCStringAttribute)

  end;

  MVCDocAttribute = class(MVCStringAttribute)

  end;

  MVCProducesAttribute = class(MVCStringAttribute)
  private
    FProduceEncoding: string;
    procedure SetProduceEncoding(const Value: string);
  public
    constructor Create(const Value: string); overload;
    constructor Create(const Value: string;
      const ProduceEncoding: string); overload;
    property ProduceEncoding: string read FProduceEncoding
      write SetProduceEncoding;
  end;

  MVCPathAttribute = class(MVCBaseAttribute)
  private
    FPath: string;

  public
    constructor Create(const Value: string); overload;
    constructor Create; overload;
    property Path: string read FPath;
  end;

  TMVCWebRequest = class
  public
    constructor Create(AWebRequest: TWebRequest); virtual;
  private
    FBody: string;
    FWebRequest: TWebRequest;
    FParamsTable: TMVCRequestParamsTable;
    FContentType: string;
    FCharset: string;
    FContentCharset: string;
    function GetHeader(const Name: string): string;
    // function GetHeaderValue(const Name: string): string;
    function GetPathInfo: string;
    function GetParamAll(const ParamName: string): string;
    function GetSegmentParam(const ParamName: string; out Value: string): Boolean;
    function GetSegmentParamsCount: Integer;
    function GetIsAjax: Boolean;
    function GetHTTPMethod: TMVCHTTPMethodType;
    function GetHTTPMethodAsString: string;
    function GetParamAllAsInteger(const ParamName: string): Integer;
    function GetParamAllAsInt64(const ParamName: string): Int64;
    function GetClientPreferHTML: Boolean;
    function GetFiles: TAbstractWebRequestFiles;

  strict protected
    FBodyAsJSONValue: TJSONValue;
    FParamNames: TArray<string>;
  public
    destructor Destroy; override;
    procedure SetParamsTable(AParamsTable: TMVCRequestParamsTable);
    function GetParamNames: TArray<string>;
    function ClientIP: string; virtual;
    function ClientPrefer(MimeType: string): Boolean;
    function ThereIsRequestBody: Boolean;
    function Accept: string;
    function QueryStringParam(Name: string): string; virtual;
    function QueryStringParamExists(Name: string): Boolean; virtual;
    function QueryStringParams: TStrings;
    procedure EnsureQueryParamExists(const Name: string);
    function ContentParam(Name: string): string; virtual;
    function Cookie(Name: string): string; virtual;
    property PathInfo: string read GetPathInfo;
    function Body: string;
    function BodyAs<T: class, constructor>(const RootProperty: string = ''): T;
    function BodyAsListOf<T: class, constructor>(const RootProperty
      : string = ''): TObjectList<T>;
    function BodyAsJSONObject: TJSONObject;
    function BodyAsJSONValue: TJSONValue;
    property Headers[const HeaderName: string]: string read GetHeader;
    property ParamsAsInteger[const ParamName: string]: Integer
      read GetParamAllAsInteger;
    property ParamsAsInt64[const ParamName: string]: Int64
      read GetParamAllAsInt64;
    property Params[const ParamName: string]: string read GetParamAll;
    property IsAjax: Boolean read GetIsAjax;
    property HTTPMethod: TMVCHTTPMethodType read GetHTTPMethod;
    property HTTPMethodAsString: string read GetHTTPMethodAsString;
    property RawWebRequest: TWebRequest read FWebRequest;
    property ClientPreferHTML: Boolean read GetClientPreferHTML;
    property Files: TAbstractWebRequestFiles read GetFiles;
    property ContentType: string read FContentType;
    property ContentCharset: string read FContentCharset;
    property Charset: string read FCharset;
  end;

{$IFDEF WEBAPACHEHTTP}

  TMVCApacheWebRequest = class(TMVCWebRequest)
  public
    constructor Create(AWebRequest: TWebRequest); override;
  end;
{$ENDIF}

  TMVCISAPIWebRequest = class(TMVCWebRequest)
  public
    constructor Create(AWebRequest: TWebRequest); override;
  end;

  TMVCINDYWebRequest = class(TMVCWebRequest)
  public
    constructor Create(AWebRequest: TWebRequest); override;
  end;

  TMVCWebResponse = class
  strict private
    function GetCustomHeaders: TStrings;

  private
    FStreamOutputDone: Boolean;
    FFlushOnDestroy: Boolean; // tristan
    procedure SetStatusCode(const Value: Integer);
    function GetStatusCode: Integer;
    procedure SetReasonString(const Value: string);
    function GetCookies: TCookieCollection;
    procedure SetContentType(const Value: string);
    function GetContentType: string;
    procedure SetContent(const Value: string);
    function GetContent: string;
    function GetLocation: string;
    procedure SetLocation(const Value: string);
    function GetReasonString: string;
    property Content: string read GetContent write SetContent;

  protected // do not put this as "strict"
    FWebResponse: TWebResponse;

  public
    constructor Create(AWebResponse: TWebResponse); virtual;
    destructor Destroy; override;
    procedure Flush;
    procedure SetCustomHeader(const Name, Value: string);
    procedure SetContentStream(AStream: TStream; AContentType: string);
    procedure SendHeaders;
    property CustomHeaders: TStrings read GetCustomHeaders;
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    property ReasonString: string read GetReasonString write SetReasonString;
    property Cookies: TCookieCollection read GetCookies;
    property ContentType: string read GetContentType write SetContentType;
    property Location: string read GetLocation write SetLocation;
    property RawWebResponse: TWebResponse read FWebResponse;
    property FlushOnDestroy: Boolean read FFlushOnDestroy write FFlushOnDestroy;
    // tristan
  end;

  TMVCEngine = class;

  TUser = class
  private
    FRoles: TList<string>;
    FUserName: string;
    FLoggedSince: TDateTime;
    FRealm: string;
    FCustomData: TMVCCustomData;
    procedure SetUserName(const Value: string);
    procedure SetLoggedSince(const Value: TDateTime);
    function GetIsValidLoggedUser: Boolean;
    procedure SetRealm(const Value: string);
    procedure SetCustomData(const Value: TMVCCustomData);

  public
    procedure SaveToSession(AWebSession: TWebSession);
    function LoadFromSession(AWebSession: TWebSession): Boolean;
    procedure Clear;
    property Roles: TList<string> read FRoles;
    property UserName: string read FUserName write SetUserName;
    property LoggedSince: TDateTime read FLoggedSince write SetLoggedSince;
    property IsValid: Boolean read GetIsValidLoggedUser;
    property Realm: string read FRealm write SetRealm;
    property CustomData: TMVCCustomData read FCustomData write SetCustomData;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TWebContext = class
  private
    FRequest: TMVCWebRequest;
    FResponse: TMVCWebResponse;
    FConfig: TMVCConfig;
    FParamsTable: TMVCRequestParamsTable;
    FData: TDictionary<string, string>;
    FLoggedUser: TUser;
    FWebSession: TWebSession;
    FIsSessionStarted: Boolean;
    FSessionMustBeClose: Boolean;
    function GetData: TDictionary<string, string>;
    function GetWebSession: TWebSession;
  protected
    function SessionMustBeClose: Boolean;
    constructor Create(ARequest: TWebRequest; AResponse: TWebResponse;
      AConfig: TMVCConfig); virtual;
    procedure SetParams(AParamsTable: TMVCRequestParamsTable);
    procedure Flush;
    function GetLoggedUser: TUser;
    // Session
    function IsSessionStarted: Boolean;
    procedure SessionStart; virtual;
    procedure BindToSession(SessionID: string);
    function SendSessionCookie(AContext: TWebContext): string;

  public
    destructor Destroy; override;
    procedure SessionStop(ARaiseExceptionIfExpired: Boolean = true); virtual;
    function SessionStarted: Boolean;
    function SessionID: string;
    property LoggedUser: TUser read GetLoggedUser;
    property Request: TMVCWebRequest read FRequest;
    property Response: TMVCWebResponse read FResponse;
    property Session: TWebSession read GetWebSession;
    property Config: TMVCConfig read FConfig;
    property Data: TDictionary<string, string> read GetData;
  end;

  TMVCActionProc = reference to procedure(Context: TWebContext);

  TMVCBase = class(TObject)
  private
    FMVCEngine: TMVCEngine;
    FMVCConfig: TMVCConfig;
    FApplicationSession: TWebApplicationSession;

  protected
    class function GetApplicationFileName: string;
    class function GetApplicationFileNamePath: string;
    procedure SetApplicationSession(const Value: TWebApplicationSession);

  public
    procedure SetMVCConfig(const Value: TMVCConfig);
    function GetMVCConfig: TMVCConfig;
    procedure SetMVCEngine(const Value: TMVCEngine);
    function GetMVCEngine: TMVCEngine;
    property ApplicationSession: TWebApplicationSession read FApplicationSession
      write SetApplicationSession;
  end;

  TMVCController = class(TMVCBase)
  private
    FViewModel: TMVCDataObjects;
    FViewDataSets: TObjectDictionary<string, TDataSet>;
    FContext: TWebContext;
    FResponseStream: TStringBuilder;
    FContentCharset: string;
    procedure SetContext(const Value: TWebContext);
    procedure SetWebSession(const Value: TWebSession);
    procedure SetContentType(const Value: string);
    function GetContentType: string;
    function GetWebSession: TWebSession;
    function GetContentCharset: string;
    procedure SetContentCharset(const Value: string);
    // procedure Render<T: class>(ACollection: TObjectList<T>; AInstanceOwner: boolean;
    // AJSONObjectActionProc: TJSONObjectActionProc; ASerializationType: TSerializationType); overload;
    // procedure Render<T: class>(ACollection: TObjectList<T>; AInstanceOwner: boolean;
    // AJSONObjectActionProc: TJSONObjectActionProc; ASerializationType: TSerializationType);

  protected const
    CLIENTID_KEY = '__clientid';
  protected
    function GetClientID: string;
    procedure RaiseSessionExpired; virtual;
    function GetCurrentWebModule: TWebModule;
    function ResponseStream: TStringBuilder;
    function GetNewStompClient(ClientID: string = ''): IStompClient;
    /// <summary>
    /// Load mustache view located in TMVCConfigKey.ViewsPath
    /// returns the rendered views and generates output using
    /// models pushed using Push* methods
    /// </summary>
    function LoadView(const ViewNames: TArray<string>): string; virtual;

    /// <summary>
    /// Load a view fragment in the output render stream. The view fragment is appended to the
    /// ResponseStream verbatim. No processing happens.
    /// Useful when used with cache.
    /// It is equivalent to <code>ResponseStream.Append(ViewFragment);</code>
    /// </summary>
    procedure LoadViewFragment(const ViewFragment: string);

    /// <summary>
    /// Load mustache view located in TMVCConfigKey.ViewsPath and
    /// returns output using models pushed using Push* methods
    /// </summary>
    function GetRenderedView(const ViewNames: TArray<string>): string; virtual;
    function SessionAs<T: TWebSession>: T;
    property Context: TWebContext read FContext write SetContext;
    property Session: TWebSession read GetWebSession write SetWebSession;
    procedure MVCControllerAfterCreate; virtual;
    procedure MVCControllerBeforeDestroy; virtual;
    property ContentType: string read GetContentType write SetContentType;
    property ContentCharset: string read GetContentCharset
      write SetContentCharset;
    // Renderers
    procedure Render(const Content: string); overload; virtual;
    procedure Render; overload; virtual; deprecated 'Use RenderResponseStream()';
    procedure RenderResponseStream; virtual;
    procedure RenderWrappedList(aList: IWrappedList;
      aJSONObjectActionProc: TJSONObjectActionProc = nil;
      aSerializationType: TDMVCSerializationType = TDMVCSerializationType.
      Properties);
    procedure Render<T: class>(aCollection: TObjectList<T>;
      aInstanceOwner: Boolean = true;
      aJSONObjectActionProc: TJSONObjectActionProc = nil;
      aSerializationType: TDMVCSerializationType = TDMVCSerializationType.
      Properties); overload;
    procedure Render(aObject: TObject; aInstanceOwner: Boolean = true;
      aSerializationType: TDMVCSerializationType = TDMVCSerializationType.
      Properties); overload; virtual;
    procedure Render(aDataSet: TDataSet; aInstanceOwner: Boolean = false;
      aOnlySingleRecord: Boolean = false;
      aJSONObjectActionProc: TJSONObjectActionProc = nil); overload; virtual;
    procedure Render(aJSONValue: TJSONValue; aInstanceOwner: Boolean = true);
      overload; virtual;
    procedure Render(aTextWriter: TTextWriter; aInstanceOwner: Boolean = true); overload;
    procedure RenderListAsProperty<T: class>(const aPropertyName: string;
      aObjectList: TObjectList<T>; aOwnsInstance: Boolean = true;
      aJSONObjectActionProc: TJSONObjectActionProc = nil);
    procedure RenderJSONArrayAsProperty(const aPropertyName: string;
      AJSONArray: TJSONArray);
    procedure Render(E: Exception; ErrorItems: TList<string> = nil);
      overload; virtual;
    procedure Render(const aErrorCode: UInt16; const aErrorMessage: string;
      const AErrorClassName: string = ''); overload;
    procedure Render(const aErrorCode: UInt16; aJSONValue: TJSONValue;
      aInstanceOwner: Boolean = true); overload;
    procedure Render(const aErrorCode: UInt16; aObject: TObject;
      aInstanceOwner: Boolean = true); overload;
    procedure RenderStreamAndFree(const AStream: TStream);
      deprecated 'Use Render(TStream,Boolean)';
    procedure Render(const AStream: TStream;
      aInstanceOwner: Boolean = true); overload;
    // messaging
    procedure EnqueueMessageOnTopicOrQueue(const IsQueue: Boolean;
      const ATopic: string; AJSONObject: TJSONObject;
      aOwnsInstance: Boolean = true);
    function ReceiveMessageFromTopic(const ATopic: string; ATimeout: Int64;
      var JSONObject: TJSONObject): Boolean;
    // redirects
    procedure Redirect(const URL: string);
    // http return code
    procedure ResponseStatusCode(const AStatusCode: UInt16;
      AStatusText: string = '');
    // streams and files
    procedure SendStream(AStream: TStream; AOwnStream: Boolean = true;
      ARewindStream: Boolean = false); virtual;
    procedure SendFile(AFileName: string); virtual;
    // filters before, after
    procedure OnBeforeAction(Context: TWebContext; const aActionName: string;
      var Handled: Boolean); virtual;
    procedure OnAfterAction(Context: TWebContext;
      const aActionName: string); virtual;

    procedure SetStatusCode(const Value: UInt16);

    function GetStatusCode: UInt16;

    property Config: TMVCConfig read GetMVCConfig;

    property StatusCode: UInt16 read GetStatusCode write SetStatusCode;

  public
    // property ViewCache: TViewCache read FViewCache write SetViewCache;
    procedure PushJSONToView(const AModelName: string; AModel: TJSONValue);
    procedure PushObjectToView(const AModelName: string; AModel: TObject);
    procedure PushDataSetToView(const AModelName: string; aDataSet: TDataSet);
    constructor Create;
    destructor Destroy; override;
  end;

  TMVCControllerClass = class of TMVCController;

  TMVCControllerDelegate = reference to function: TMVCController;

  TMVCControllerRoutable = class
  strict private
    FClass: TMVCControllerClass;
    FDelegate: TMVCControllerDelegate;
  public
    constructor Create(AClass: TMVCControllerClass;
      ADelegate: TMVCControllerDelegate);

    property &Class: TMVCControllerClass read FClass;
    property Delegate: TMVCControllerDelegate read FDelegate;
  end;

  /// <summary>
  /// Basis Interface for DMVC Middleware.
  /// </summary>
  IMVCMiddleware = interface
    ['{3278183A-124A-4214-AB4E-94CA4C22450D}']
    /// <summary>
    /// Procedure is called before the MVCEngine routes the request to a specific controller/method.
    /// </summary>
    /// <param name="Context">Webcontext which contains the complete request and response of the actual call.</param>
    /// <param name="Handled">If set to True the Request would finished. Response must be set by the implementor. Default value is False.</param>
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    /// <summary>
    /// Procedure is called before the specific controller method is called.
    /// </summary>
    /// <param name="Context">Webcontext which contains the complete request and response of the actual call.</param>
    /// <param name="AControllerQualifiedClassName">Qualified classname of the matching controller.</param>
    /// <param name="AActionNAme">Method name of the matching controller method.</param>
    /// <param name="Handled">If set to True the Request would finished. Response must be set by the implementor. Default value is False.</param>
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const aActionName: string;
      var Handled: Boolean);
    /// <summary>
    /// Procedure is called after the specific controller method was called.
    /// It is still possible to cancel or to completly modifiy the request.
    /// </summary>
    /// <param name="Context">Webcontext which contains the complete request and response of the actual call.</param>
    /// <param name="AActionNAme">Method name of the matching controller method.</param>
    /// <param name="Handled">If set to True the Request would finished. Response must be set by the implementor. Default value is False.</param>
    procedure OnAfterControllerAction(Context: TWebContext;
      const aActionName: string; const Handled: Boolean);
  end;

  TMVCEngine = class(TComponent)
  strict private
    FApplicationSession: TWebApplicationSession;

  private
    FWebModule: TWebModule;
    FSavedOnBeforeDispatch: THTTPMethodEvent;
    FMVCConfig: TMVCConfig;
    // FViewCache            : TViewCache;
    FMimeTypes: TDictionary<string, string>;
    procedure SetApplicationSession(const Value: TWebApplicationSession);
    procedure SetDefaultReponseHeaders(AContext: TWebContext);
  protected
    FConfiguredSessionTimeout: Int64;
    FControllers: TObjectList<TMVCControllerRoutable>;
    FMiddleware: TList<IMVCMiddleware>;
    procedure ExecuteBeforeRoutingMiddleware(Context: TWebContext;
      var Handled: Boolean);
    procedure ExecuteBeforeControllerActionMiddleware(MVCEngine: TMVCEngine;
      Context: TWebContext; const AControllerQualifiedClassName: string;
      const aActionName: string; var Handled: Boolean);
    procedure ExecuteAfterControllerActionMiddleware(Context: TWebContext;
      const aActionName: string; const Handled: Boolean);
    procedure ConfigDefaultValues; virtual;
    procedure FixUpWebModule;
    procedure OnBeforeDispatch(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean); virtual;
    function ExecuteAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse): Boolean; virtual;
    procedure LoadSystemControllers; virtual;
    procedure ResponseErrorPage(E: Exception; Request: TWebRequest;
      Response: TWebResponse); virtual;
    class procedure ClearSessionCookiesAlreadySet(aCookies: TCookieCollection);
  public
    class function GetCurrentSession(ASessionTimeout: UInt64;
      const ASessionID: string; ARaiseExceptionIfExpired: Boolean = true)
      : TWebSession;
    class function ExtractSessionIDFromWebRequest
      (AWebRequest: TWebRequest): string;
    constructor Create(WebModule: TWebModule;
      ConfigProc: TProc<TMVCConfig> = nil; CustomLogger: ILogWriter = nil); reintroduce;
    destructor Destroy; override;
    class function SendSessionCookie(AContext: TWebContext): string; overload;
    class function SendSessionCookie(AContext: TWebContext; const ASessionID: string)
      : string; overload;
    function GetSessionBySessionID(const ASessionID: string): TWebSession;
    function AddController(AControllerClass: TMVCControllerClass)
      : TMVCEngine; overload;
    function AddController(AControllerClass: TMVCControllerClass;
      ADelegate: TMVCControllerDelegate): TMVCEngine; overload;
    function AddMiddleware(AMiddleware: IMVCMiddleware): TMVCEngine;
    // internal methods
    function RegisteredControllers: TObjectList<TMVCControllerRoutable>;
    // http return codes
    procedure Http404(AWebContext: TWebContext);
    procedure Http500(AWebContext: TWebContext; const AReasonText: string = '');
    property Config: TMVCConfig read FMVCConfig; // allow a simple client code
    property ApplicationSession: TWebApplicationSession read FApplicationSession
      write SetApplicationSession;
  end;

  TMVCStaticContents = class(TMVCController)
  public
    // [MVCPath('/static/($filename)')]
    class procedure SendFile(AFileName, AMimeType: string;
      Context: TWebContext);
    class function IsStaticFile(AViewPath, AWebRequestPath: string;
      out ARealFileName: string): Boolean;
    class function IsScriptableFile(StaticFileName: string;
      Config: TMVCConfig): Boolean;
  end;

type
  TMVCConfigKey = class
  public const
    SessionTimeout = 'sessiontimeout';
    DocumentRoot = 'document_root';
    ViewPath = 'view_path';
    DefaultContentType = 'default_content_type';
    DefaultContentCharset = 'default_content_charset';
    DefaultViewFileExtension = 'default_view_file_extension';
    ISAPIPath = 'isapi_path';
    StompServer = 'stompserver';
    StompServerPort = 'stompserverport';
    StompUsername = 'stompusername';
    StompPassword = 'stomppassword';
    Messaging = 'messaging';
    AllowUnhandledAction = 'allow_unhandled_action'; // tristan
    ServerName = 'server_name'; // tristan
    ExposeServerSignature = 'server_signature';
    IndexDocument = 'index_document';
    SessionType = 'session_type';
    /// <summary>
    /// Define a default URL for requests that don't map to a route or a file
    /// </summary>
    FallbackResource = 'fallback_resource';
  end;

function IsShuttingDown: Boolean;
procedure EnterInShutdownState;

procedure InternalRender(const Content: string;
  ContentType, ContentEncoding: string; Context: TWebContext); overload;
procedure InternalRenderText(const AContent: string;
  ContentType, ContentEncoding: string; Context: TWebContext);
procedure InternalRender(aJSONValue: TJSONValue;
  ContentType, ContentEncoding: string; Context: TWebContext;
  aInstanceOwner: Boolean = true); overload;

implementation

uses
  System.SyncObjs,
  idglobal,
  IdGlobalProtocols,
  System.DateUtils,
  System.RegularExpressions,
  WinApi.Windows,
  System.TypInfo,
  System.ioutils,
  System.StrUtils,
  Web.Win.IsapiHTTP,
  MVCFramework.Router,
  MVCFramework.View,
  IdURI,
  IdStack,
  IdHTTPWebBrokerBridge,
  MVCFramework.MessagingController,
  Web.WebReq,
  MVCFramework.SysControllers;

const
  ALLOWED_TYPED_ACTION_PARAMETERS_TYPES =
    'Integer, Int64, Single, Double, Extended, Boolean, TDate, TTime, TDateTime and String';

type
  TIdHTTPAppRequestHack = class(TIdHTTPAppRequest)

  end;

threadvar ctx: TRTTIContext;

var
  _IsShuttingDown: Int64 = 0;
  // this variable is used by TInterlocked functions to handlòe the "shuttingdown" mode

  { TMVCEngine }

function TMVCEngine.AddController(AControllerClass: TMVCControllerClass)
  : TMVCEngine;
begin
  Result := AddController(AControllerClass, nil);
end;

function TMVCEngine.AddController(AControllerClass: TMVCControllerClass;
  ADelegate: TMVCControllerDelegate): TMVCEngine;
begin
  FControllers.Add(TMVCControllerRoutable.Create(AControllerClass, ADelegate));
  Result := Self;
end;

function TMVCEngine.AddMiddleware(AMiddleware: IMVCMiddleware): TMVCEngine;
begin
  FMiddleware.Add(AMiddleware);
  Result := Self;
end;

function AddSessionToTheSessionList(const aSessionType, ASessionID: string;
  ASessionTimeout: UInt64): TWebSession;
var
  LSess: TWebSession;
begin
  if Trim(aSessionType) = '' then
  begin
    raise EMVCException.Create('Empty Session Type');
  end;

  TMonitor.Enter(SessionList);
  try
    LSess := TMVCSessionFactory.GetInstance.CreateNewByType(
      aSessionType,
      ASessionID, ASessionTimeout);
    SessionList.Add(ASessionID, LSess);
    Result := LSess;
    LSess.MarkAsUsed;
  finally
    TMonitor.Exit(SessionList);
  end;
end;

class procedure TMVCEngine.ClearSessionCookiesAlreadySet(
  aCookies: TCookieCollection);
var
  I: Integer;
  lSessCookieName: string;
  lCookie: TCookie;
begin
  lSessCookieName := TMVCConstants.SESSION_TOKEN_NAME.ToLower;
  I := 0;
  while true do
  begin
    if I = aCookies.Count then
      Break;
    lCookie := aCookies[I];
    if LowerCase(lCookie.Name) = lSessCookieName then
    begin
      aCookies.Delete(I);
    end
    else
      Inc(I);
  end;
end;

procedure TMVCEngine.ConfigDefaultValues;
begin
  Log.Info('ENTER: Config default values', LOGGERPRO_TAG);
  Config[TMVCConfigKey.SessionTimeout] := '30'; // 30 minutes
  Config[TMVCConfigKey.DocumentRoot] := '.\www';
  Config[TMVCConfigKey.FallbackResource] := '';
  Config[TMVCConfigKey.DefaultContentType] :=
    TMVCConstants.DEFAULT_CONTENT_TYPE;
  Config[TMVCConfigKey.DefaultContentCharset] :=
    TMVCConstants.DEFAULT_CONTENT_CHARSET;

  Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
  Config[TMVCConfigKey.ViewPath] := 'templates';
  Config[TMVCConfigKey.ISAPIPath] := '';

  Config[TMVCConfigKey.StompServer] := 'localhost';
  Config[TMVCConfigKey.StompServerPort] := '61613';
  Config[TMVCConfigKey.StompUsername] := 'guest';
  Config[TMVCConfigKey.StompPassword] := 'guest';
  Config[TMVCConfigKey.Messaging] := 'false';

  Config[TMVCConfigKey.AllowUnhandledAction] := 'false'; // tristan
  Config[TMVCConfigKey.ServerName] := 'DelphiMVCFramework'; // tristan
  Config[TMVCConfigKey.ExposeServerSignature] := 'true';
  Config[TMVCConfigKey.SessionType] := 'memory';

  Config[TMVCConfigKey.IndexDocument] := 'index.html';

  FMimeTypes.Add('.html', TMVCMimeType.TEXT_HTML);
  FMimeTypes.Add('.htm', TMVCMimeType.TEXT_HTML);
  FMimeTypes.Add('.txt', TMVCMimeType.TEXT_PLAIN);
  FMimeTypes.Add('.css', TMVCMimeType.TEXT_CSS);
  FMimeTypes.Add('.js', TMVCMimeType.TEXT_JAVASCRIPT);
  FMimeTypes.Add('.jpg', TMVCMimeType.IMAGE_JPEG);
  FMimeTypes.Add('.jpeg', TMVCMimeType.IMAGE_JPEG);
  FMimeTypes.Add('.png', TMVCMimeType.IMAGE_PNG);
  FMimeTypes.Add('.appcache', TMVCMimeType.TEXT_CACHEMANIFEST);

  Log.Info('EXIT: Config default values', LOGGERPRO_TAG);
end;

constructor TMVCEngine.Create(WebModule: TWebModule;
  ConfigProc: TProc<TMVCConfig>; CustomLogger: ILogWriter);
begin
  inherited Create(WebModule);
  WebRequestHandler.CacheConnections := true;
  WebRequestHandler.MaxConnections := 4096;
  FMimeTypes := TDictionary<string, string>.Create;
  FMVCConfig := TMVCConfig.Create;
  FWebModule := WebModule;
  FControllers := TObjectList<TMVCControllerRoutable>.Create(true);
  FMiddleware := TList<IMVCMiddleware>.Create;
  // FViewCache := TViewCache.Create;
  FixUpWebModule;
  MVCFramework.Logger.SetDefaultLogger(CustomLogger);
  // WARNING!! from now on, the logger subsystem is available
  ConfigDefaultValues;

  if Assigned(ConfigProc) then
  begin
    LogEnterMethod('Custom configuration proc');
    ConfigProc(FMVCConfig);
    LogExitMethod('Custom configuration proc');
  end;

  LoadSystemControllers;
end;

destructor TMVCEngine.Destroy;
begin
  FMimeTypes.Free;
  FMVCConfig.Free;
  FControllers.Free;
  FMiddleware.Free;
  // FViewCache.Free;
  inherited;
end;

procedure TMVCEngine.SetDefaultReponseHeaders(AContext: TWebContext);
begin
  if Config[TMVCConfigKey.ExposeServerSignature] = 'true' then
  begin
    AContext.Response.CustomHeaders.Values['Server'] :=
      Config[TMVCConfigKey.ServerName];
  end;
  AContext.Response.RawWebResponse.Date := Now;
end;

function TMVCEngine.ExecuteAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse): Boolean;
var
  lSelectedController: TMVCController;
  lContext: TWebContext;
  lParamsTable: TMVCRequestParamsTable;
  lRouter: TMVCRouter;
  lStaticFileName: string;
  lContentType: string;
  lHandled: Boolean;
  lResponseContentType, lResponseContentCharset: string;
  lActionFormalParams: TArray<TRttiParameter>;
  lActualParams: TArray<TValue>;

  function SendFileIfPresent(const AFileName: String): Boolean;
  begin
    lStaticFileName := TPath.Combine(Config[TMVCConfigKey.DocumentRoot], AFileName);
    if TFile.Exists(lStaticFileName) then
    begin
      if FMimeTypes.TryGetValue(LowerCase(ExtractFileExt(lStaticFileName)), lContentType) then
      begin
        lContentType := lContentType + ';charset=' + FMVCConfig
          [TMVCConfigKey.DefaultContentCharset];
      end
      else
      begin
        lContentType := TMVCMimeType.APPLICATION_OCTETSTREAM;
      end;
      TMVCStaticContents.SendFile(lStaticFileName, lContentType, lContext);
      Result := true;
    end
    else
      Result := false;
  end;

  function SendDocumentIndexIfPresent: Boolean;
  begin
    Result := SendFileIfPresent(Config[TMVCConfigKey.IndexDocument]);
    // lStaticFileName := TPath.Combine(Config[TMVCConfigKey.DocumentRoot],
    // Config[TMVCConfigKey.IndexDocument]);
    // if TFile.Exists(lStaticFileName) then
    // begin
    // if FMimeTypes.TryGetValue(LowerCase(ExtractFileExt(lStaticFileName)), lContentType) then
    // begin
    // lContentType := lContentType + ';charset=' + FMVCConfig
    // [TMVCConfigKey.DefaultContentCharset];
    // end
    // else
    // begin
    // lContentType := TMVCMimeType.APPLICATION_OCTETSTREAM;
    // end;
    // TMVCStaticContents.SendFile(lStaticFileName, lContentType, lContext);
    // Result := true;
    // end
    // else
    // Result := false;
  end;

  procedure FillActualParamsForAction(const AContext: TWebContext;
    const aActionFormalParams: TArray<TRttiParameter>; const aActionName: string;
    var aActualParams: TArray<TValue>);
  var
    lParamName: string;
    I: Integer;
    lStrValue: string;
    lFormatSettings: TFormatSettings;
    lWasDateTime: Boolean;
  begin
    if AContext.Request.GetSegmentParamsCount <> Length(aActionFormalParams) then
      raise EMVCException.CreateFmt
        ('Paramaters count mismatch (expected %d actual %d) for action "%s"',
        [Length(aActionFormalParams), AContext.Request.GetSegmentParamsCount, aActionName]);
    SetLength(aActualParams, Length(aActionFormalParams));
    for I := 0 to Length(aActionFormalParams) - 1 do
    begin
      lParamName := aActionFormalParams[I].Name;
      if not AContext.Request.GetSegmentParam(lParamName, lStrValue) then
        raise EMVCException.CreateFmt
          ('Invalid paramater %s for action %s (Hint: Here parameters names are case-sensitive)',
          [lParamName, aActionName]);
      case aActionFormalParams[I].ParamType.TypeKind of
        tkInteger, tkInt64:
          begin
            aActualParams[I] := StrToInt(lStrValue);
          end;
        tkUString:
          begin
            aActualParams[I] := lStrValue;
          end;
        tkFloat:
          begin
            lWasDateTime := false;
            if aActionFormalParams[I].ParamType.QualifiedName = 'System.TDate' then
            begin
              try
                lWasDateTime := true;
                aActualParams[I] := ISOStrToDate(lStrValue);
              except
                raise EMVCException.CreateFmt('Invalid TDate value for param [%s]',
                  [aActionFormalParams[I].Name]);
              end;
            end
            else if aActionFormalParams[I].ParamType.QualifiedName = 'System.TDateTime' then
            begin
              try
                lWasDateTime := true;
                aActualParams[I] := ISOStrToDateTime(lStrValue);
              except
                raise EMVCException.CreateFmt('Invalid TDateTime value for param [%s]',
                  [aActionFormalParams[I].Name]);
              end;
            end
            else if aActionFormalParams[I].ParamType.QualifiedName = 'System.TTime' then
            begin
              try
                lWasDateTime := true;
                aActualParams[I] := ISOStrToTime(lStrValue);
              except
                raise EMVCException.CreateFmt('Invalid TTime value for param [%s]',
                  [aActionFormalParams[I].Name]);
              end;
            end;

            if not lWasDateTime then
            begin
              lFormatSettings.DecimalSeparator := '.';
              aActualParams[I] := StrToFloat(lStrValue, lFormatSettings);
            end;
          end;
        tkEnumeration:
          begin
            if aActionFormalParams[I].ParamType.QualifiedName = 'System.Boolean' then
            begin
              if SameText(lStrValue, 'true') or SameText(lStrValue, '1') then
                aActualParams[I] := true
              else if SameText(lStrValue, 'false') or SameText(lStrValue, '0') then
                aActualParams[I] := false
              else
                raise EMVCException.CreateFmt
                  ('Invalid boolean value for parameter %s. Boolean parameters accepts only "true"/"false" or "1"/"0".',
                  [lParamName]);
            end
            else
              raise EMVCException.CreateFmt
                ('Invalid type for parameter %s. Allowed types are ' +
                ALLOWED_TYPED_ACTION_PARAMETERS_TYPES,
                [lParamName]);
          end;
      else
        begin
          raise EMVCException.CreateFmt
            ('Invalid type for parameter %s. Allowed types are ' +
            ALLOWED_TYPED_ACTION_PARAMETERS_TYPES,
            [lParamName]);
        end;

        {
          tkChar, tkEnumeration, ,
          tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
          tkVariant, tkArray, tkRecord, tkInterface, , tkDynArray, tkUString,
          tkClassRef, tkPointer, tkProcedure }
      end;

    end;
  end;

begin
  // LogEnterMethod(Request.PathInfo);
  // try
  Result := false;
  lParamsTable := TMVCRequestParamsTable.Create;
  try
    lContext := TWebContext.Create(Request, Response, FMVCConfig);
    try
      SetDefaultReponseHeaders(lContext); // tristan
      // Static file handling
      if (not FMVCConfig[TMVCConfigKey.DocumentRoot].IsEmpty) and
      // dt: if document_root is empty, no static file are served
        (TMVCStaticContents.IsStaticFile(TPath.Combine(AppPath,
        FMVCConfig[TMVCConfigKey.DocumentRoot]), Request.PathInfo,
        lStaticFileName)) then
      begin
        // if TMVCStaticContents.IsScriptableFile(StaticFileName, FMVCConfig) then
        // // execute the file
        // begin
        // ExecuteFile(StaticFileName, Context);
        // end
        // else // serve the file
        // begin
        if FMimeTypes.TryGetValue(LowerCase(ExtractFileExt(lStaticFileName)), lContentType) then
        begin
          lContentType := lContentType + ';charset=' + FMVCConfig
            [TMVCConfigKey.DefaultContentCharset];
        end
        else
        begin
          lContentType := TMVCMimeType.APPLICATION_OCTETSTREAM;
        end;
        TMVCStaticContents.SendFile(lStaticFileName, lContentType, lContext);
        Result := true;
        // end;
      end
      else
      begin
        lRouter := TMVCRouter.Create(Config);
        try
          ExecuteBeforeRoutingMiddleware(lContext, lHandled);
          if not lHandled then
          begin
            if lRouter.ExecuteRouting(Request.PathInfo,
              TMVCRouter.StringMethodToHTTPMetod(Request.Method),
              Request.ContentType, Request.Accept, FControllers,
              FMVCConfig[TMVCConfigKey.DefaultContentType],
              FMVCConfig[TMVCConfigKey.DefaultContentCharset], lParamsTable,
              lResponseContentType, lResponseContentCharset) then
            begin
              if Assigned(lRouter.MVCControllerDelegate) then
                lSelectedController := lRouter.MVCControllerDelegate()
              else
                lSelectedController := lRouter.MVCControllerClass.Create;
              try
                lSelectedController.SetMVCConfig(Config);
                lSelectedController.ApplicationSession := FApplicationSession;
                lContext.SetParams(lParamsTable);
                lSelectedController.SetContext(lContext);
                lSelectedController.SetMVCEngine(Self);

                // exception?
                try
                  { middlewares before controller action }
                  ExecuteBeforeControllerActionMiddleware(Self, lContext,
                    lRouter.MVCControllerClass.QualifiedClassName,
                    lRouter.MethodToCall.Name, lHandled);
                  if lHandled then
                    Exit(true);

                  lSelectedController.MVCControllerAfterCreate;
                  try
                    lHandled := false;
                    // gets response contentype from MVCProduces attribute
                    lSelectedController.ContentType := lResponseContentType;
                    lSelectedController.ContentCharset :=
                      lResponseContentCharset;
                    if not lHandled then
                    begin

                      lActionFormalParams := lRouter.MethodToCall.GetParameters;

                      // case1: check for parameterless action
                      if Length(lActionFormalParams) = 0 then
                      begin
                        SetLength(lActualParams, 0);
                      end
                      // case2: check for action with only TWebContext
                      else if (Length(lActionFormalParams) = 1) and
                        (SameText(lActionFormalParams[0].ParamType.QualifiedName,
                        'MVCFramework.TWebContext')) then
                      begin
                        SetLength(lActualParams, 1);
                        lActualParams[0] := lContext;
                      end
                      // case3: strongly typed declaration... injection parameters
                      else
                      begin
                        FillActualParamsForAction(lContext, lActionFormalParams,
                          lRouter.MethodToCall.Name, lActualParams);
                      end;

                      /// ///////////////////////////////////////////////////////
                      lSelectedController.OnBeforeAction(lContext,
                        lRouter.MethodToCall.Name, lHandled);
                      { WARNING!!! Is the BeforeAction filter set lHandled = true,
                        the AfterAction is never called }
                      if not lHandled then
                      begin
                        try
                          lRouter.MethodToCall.Invoke(lSelectedController, lActualParams);
                        finally
                          lSelectedController.OnAfterAction(lContext,
                            lRouter.MethodToCall.Name);
                        end;
                      end;
                      /// ///////////////////////////////////////////////////////

                    end;
                  finally
                    lSelectedController.MVCControllerBeforeDestroy;
                  end;
                  ExecuteAfterControllerActionMiddleware(lContext,
                    lRouter.MethodToCall.Name, lHandled);
                except
                  on E: EMVCSessionExpiredException do
                  begin
                    LogException(E, E.DetailedMessage);
                    lContext.SessionStop(false);
                    lSelectedController.ResponseStatusCode(E.HTTPErrorCode);
                    lSelectedController.Render(E);
                  end;
                  on E: EMVCException do
                  begin
                    LogException(E, E.DetailedMessage);
                    lSelectedController.ResponseStatusCode(E.HTTPErrorCode);
                    lSelectedController.Render(E);
                  end;
                  on E: EInvalidOp do
                  begin
                    LogException(E, 'Invalid OP');
                    lSelectedController.Render(E);
                  end;
                  on E: Exception do
                  begin
                    LogException(E, 'Global Action Exception Handler');
                    lSelectedController.ResponseStatusCode(HTTP_STATUS.InternalServerError);
                    lSelectedController.Render(E);
                  end;
                end;
                lContext.Response.ContentType :=
                  lSelectedController.ContentType;

                Log(TLogLevel.levNormal, Request.Method + ':' +
                  Request.RawPathInfo + ' -> ' +
                  lRouter.MVCControllerClass.QualifiedClassName + ' - ' +
                  IntToStr(Response.StatusCode) + ' ' + Response.ReasonString)
              finally
                lSelectedController.Free;
              end;
            end
            else
            begin
              if Config[TMVCConfigKey.AllowUnhandledAction] = 'false' then
              begin
                Result := false;
                if not Config[TMVCConfigKey.FallbackResource].IsEmpty then
                  Result := SendFileIfPresent(Config[TMVCConfigKey.FallbackResource]);
                if not Result then
                begin
                  Http404(lContext);
                  Log(TLogLevel.levNormal, Request.Method + ':' +
                    Request.RawPathInfo + ' -> NO ACTION ' + ' - ' +
                    IntToStr(Response.StatusCode) + ' ' +
                    Response.ReasonString);
                end;
              end
              else
              begin
                Result := false;
                lContext.Response.FlushOnDestroy := false; // tristan
              end;
            end;
          end;
        finally
          lRouter.Free;
        end;
      end; // end if IS_STATIC
    finally

      lContext.Free;
    end;
  finally
    lParamsTable.Free;
  end;
  // finally
  // LogExitMethod(Request.PathInfo + ' [' + IntToStr(Response.StatusCode) + ' ' +
  // Response.ReasonString + ']');
  // end;
end;

procedure TMVCEngine.ExecuteAfterControllerActionMiddleware
  (Context: TWebContext; const aActionName: string; const Handled: Boolean);
var
  I: Integer;
begin
  for I := FMiddleware.Count - 1 downto 0 do
  begin
    FMiddleware[I].OnAfterControllerAction(Context, aActionName, Handled);
  end;
end;

procedure TMVCEngine.ExecuteBeforeControllerActionMiddleware
  (MVCEngine: TMVCEngine; Context: TWebContext;
  const AControllerQualifiedClassName: string; const aActionName: string;
  var Handled: Boolean);
var
  LMiddleware: IMVCMiddleware;
begin
  if not Handled then
    for LMiddleware in FMiddleware do
    begin
      LMiddleware.OnBeforeControllerAction(Context,
        AControllerQualifiedClassName, aActionName, Handled);
      if Handled then
        Break;
    end;
end;

procedure TMVCEngine.ExecuteBeforeRoutingMiddleware(Context: TWebContext;
  var Handled: Boolean);
var
  middleware: IMVCMiddleware;
begin
  if not Handled then
    for middleware in FMiddleware do
    begin
      middleware.OnBeforeRouting(Context, Handled);
      if Handled then
        Break;
    end;
end;

class
  function TMVCEngine.ExtractSessionIDFromWebRequest
  (AWebRequest: TWebRequest): string;
begin
  Result := AWebRequest.CookieFields.Values[TMVCConstants.SESSION_TOKEN_NAME];
  if not Result.IsEmpty then
    Result := TIdURI.URLDecode(Result);
end;

procedure TMVCEngine.FixUpWebModule;
begin
  FSavedOnBeforeDispatch := FWebModule.BeforeDispatch;
  FWebModule.BeforeDispatch := OnBeforeDispatch;
end;

class
  function TMVCEngine.GetCurrentSession(ASessionTimeout: UInt64;
  const ASessionID: string; ARaiseExceptionIfExpired: Boolean): TWebSession;
var
  // SessionID: string;
  List: TObjectDictionary<string, TWebSession>;
  IsExpired: Boolean;
begin
  List := SessionList;
  TMonitor.Enter(List);
  try
    Result := nil;

    // if ASessionID.IsEmpty then
    // raise EMVCException.Create('Empty SessionID');

    { SESSION IS NOT AUTOCREATED BY DEFAULT }
    if not ASessionID.IsEmpty then
    begin
      IsExpired := true;
      if List.TryGetValue(ASessionID, Result) then
      begin
        // spinettaro sessiontimeout -- if a session cookie has been choosed the inactivity time is 60 minutes
        if ASessionTimeout = 0 then
          IsExpired := MinutesBetween(Now, Result.LastAccess) > DEFAULT_SESSION_INACTIVITY
        else
          IsExpired := MinutesBetween(Now, Result.LastAccess) > ASessionTimeout;
        // StrToInt(Config.Value['sessiontimeout']);
      end;

      if Assigned(Result) then
      begin
        if IsExpired then
        begin
          List.Remove(ASessionID); // remove expired session from session list
          if ARaiseExceptionIfExpired then
            raise EMVCSessionExpiredException.Create('Session expired')
          else
            Result := nil;
        end
        else
        begin
          Result.MarkAsUsed;
        end;
      end;
    end;
  finally
    TMonitor.Exit(List);
  end;
end;

function TMVCEngine.GetSessionBySessionID(const ASessionID: string)
  : TWebSession;
begin
  Result := TMVCEngine.GetCurrentSession
    (StrToInt64(Config[TMVCConfigKey.SessionTimeout]), ASessionID, false);
  if Assigned(Result) then
  begin
    Result.MarkAsUsed;
    // TMVCEngine.SendSessionCookie(FContext, SessionID);
  end;
end;

procedure TMVCEngine.Http404(AWebContext: TWebContext);
begin
  AWebContext.Response.StatusCode := 404;
  AWebContext.Response.ReasonString := 'Not Found';
  AWebContext.Response.Content := 'Not Found';
end;

procedure TMVCEngine.Http500(AWebContext: TWebContext; const AReasonText: string);
begin
  AWebContext.Response.StatusCode := 500;
  AWebContext.Response.ReasonString := 'Internal server error: ' + AReasonText;
  AWebContext.Response.Content := 'Internal server error: ' + AReasonText;
end;

procedure TMVCEngine.LoadSystemControllers;
begin
  Log(TLogLevel.levNormal, 'ENTER: LoadSystemControllers');
  AddController(TMVCSystemController);
  if Config[TMVCConfigKey.Messaging].ToLower.Equals('true') then
  begin
    AddController(TMVCBUSController);
    Log(TLogLevel.levNormal, 'Loaded system controller ' +
      TMVCBUSController.QualifiedClassName);
  end;
  Log(TLogLevel.levNormal, 'EXIT: LoadSystemControllers');
end;

procedure TMVCEngine.OnBeforeDispatch(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Handled := false;
  if Assigned(FSavedOnBeforeDispatch) then
    FSavedOnBeforeDispatch(Sender, Request, Response, Handled);
  // _Request := Request as TIdHTTPAppRequest;
  if not Handled then
  begin
    try
      // "X-Requested-With", "XMLHttpRequest"
      Handled := ExecuteAction(Sender, Request, Response); // tristan
    except
      on E: Exception do
      begin
        LogException(E);
        // Response.ContentStream.Size := 0;
        Response.Content := E.Message;
        Response.SendResponse;
        Handled := true;
      end;
    end;
    // Handled := true;
  end;
end;

function TMVCEngine.RegisteredControllers: TObjectList<TMVCControllerRoutable>;
begin
  Result := FControllers;
end;

procedure TMVCEngine.ResponseErrorPage(E: Exception; Request: TWebRequest;
  Response: TWebResponse);
begin
  Response.SetCustomHeader('x-mvc-error', E.ClassName + ': ' + E.Message);
  Response.StatusCode := 200;
  // if Pos('text/html', LowerCase(Request.Accept)) = 1 then
  // begin
  // Response.ContentType := 'text/plain';
  // Response.Content := Config[TMVCConfigKey.ServerName] + ' ERROR:' +
  // sLineBreak + 'Exception raised of class: ' + E.ClassName + sLineBreak +
  // '***********************************************' + sLineBreak + E.Message
  // + sLineBreak + '***********************************************';
  // end
  // else
  // Same code in if and else section
  begin
    Response.ContentType := 'text/plain';
    Response.Content := Config[TMVCConfigKey.ServerName] + ' ERROR:' +
      sLineBreak + 'Exception raised of class: ' + E.ClassName + sLineBreak +
      '***********************************************' + sLineBreak + E.Message
      + sLineBreak + '***********************************************';
  end;
end;

class
  function TMVCEngine.SendSessionCookie(AContext: TWebContext): string;
var
  LSessionID: string;
begin
  LSessionID := StringReplace
    (StringReplace(StringReplace(GUIDToString(TGUID.NewGuid), '}', '', []), '{',
    '', []), '-', '', [rfReplaceAll]);
  Result := SendSessionCookie(AContext, LSessionID);
end;

class
  function TMVCEngine.SendSessionCookie(AContext: TWebContext;
  const ASessionID: string): string;
var
  Cookie: TCookie;
  LSessTimeout: Integer;
begin
  ClearSessionCookiesAlreadySet(AContext.Response.Cookies);
  Cookie := AContext.Response.Cookies.Add;
  Cookie.Name := TMVCConstants.SESSION_TOKEN_NAME;
  Cookie.Value := ASessionID;
  LSessTimeout := StrToIntDef(AContext.Config[TMVCConfigKey.SessionTimeout], 0);
  if LSessTimeout = 0 then
    Cookie.Expires := -1
  else
    Cookie.Expires := Now + OneMinute * LSessTimeout;
  Cookie.Path := '/';
  Result := ASessionID;
end;

procedure TMVCEngine.SetApplicationSession(const Value: TWebApplicationSession);
begin
  FApplicationSession := Value;
end;

{ TWebContext }

constructor TWebContext.Create(ARequest: TWebRequest; AResponse: TWebResponse;
  AConfig: TMVCConfig);
begin
  inherited Create;
  FIsSessionStarted := false;
  FSessionMustBeClose := false;

  if IsLibrary then
  begin
{$IFDEF WEBAPACHEHTTP}
    if ARequest is TApacheRequest then
      FRequest := TMVCApacheWebRequest.Create(ARequest)
    else if ARequest is TISAPIRequest then
      FRequest := TMVCISAPIWebRequest.Create(ARequest)
    else
      raise EMVCException.Create('Unknown request type ' + ARequest.ClassName);
{$ELSE}
    FRequest := TMVCISAPIWebRequest.Create(ARequest)
{$ENDIF}
  end
  else
  begin
    FRequest := TMVCINDYWebRequest.Create(ARequest);
  end;
  FResponse := TMVCWebResponse.Create(AResponse);
  FConfig := AConfig;
  FData := TDictionary<string, string>.Create;
  FLoggedUser := TUser.Create;
end;

destructor TWebContext.Destroy;
begin
  FreeAndNil(FResponse);
  FreeAndNil(FRequest);
  FreeAndNil(FData);
  FreeAndNil(FLoggedUser);
  // do not destroy session here... it is stored in the session list
  inherited;
end;

procedure TWebContext.Flush;
begin
  FResponse.Flush;
end;

function TWebContext.GetData: TDictionary<string, string>;
begin
  Result := FData;
end;

function TWebContext.GetLoggedUser: TUser;
begin
  if not Assigned(FLoggedUser) then
  begin
    FLoggedUser := TUser.Create;
  end;
  Result := FLoggedUser;
end;

function TWebContext.GetWebSession: TWebSession;
begin
  if not Assigned(FWebSession) then
  begin
    FWebSession := TMVCEngine.GetCurrentSession
      (StrToInt64(FConfig[TMVCConfigKey.SessionTimeout]),
      TMVCEngine.ExtractSessionIDFromWebRequest(FRequest.RawWebRequest), false);
    if not Assigned(FWebSession) then
      SessionStart
    else
    begin
      TMVCEngine.SendSessionCookie(Self, FWebSession.SessionID);
      // daniele
    end;
  end;
  Result := FWebSession;
  Result.MarkAsUsed;
  {
    LSessionIDFromWebRequest := TMVCEngine.ExtractSessionIDFromWebRequest
    (Context.Request.RawWebRequest);
    LWebSession := TMVCEngine.GetCurrentSession
    (Context.Config.AsInt64[TMVCConfigKey.SessionTimeout],
    LSessionIDFromWebRequest, False);

  }
end;

function TWebContext.IsSessionStarted: Boolean;
begin
  Result := FIsSessionStarted;
end;

procedure TWebContext.SetParams(AParamsTable: TMVCRequestParamsTable);
begin
  FParamsTable := AParamsTable;
  FRequest.FParamsTable := AParamsTable;
end;

{ TMVCWebResponse }

constructor TMVCWebResponse.Create(AWebResponse: TWebResponse);
begin
  FStreamOutputDone := false;
  FFlushOnDestroy := true;
  inherited Create;
  FWebResponse := AWebResponse;
end;

destructor TMVCWebResponse.Destroy;
begin
  if FFlushOnDestroy then // tristan
    Flush;
  inherited;
end;

procedure TMVCWebResponse.Flush;
begin
  try
    FWebResponse.SendResponse; // daniele
  except
  end;
end;

function TMVCWebResponse.GetContent: string;
begin
  Result := FWebResponse.Content;
end;

function TMVCWebResponse.GetContentType: string;
begin
  Result := FWebResponse.ContentType;
end;

function TMVCWebResponse.GetCookies: TCookieCollection;
begin
  Result := Self.FWebResponse.Cookies;
end;

function TMVCWebResponse.GetCustomHeaders: TStrings;
begin
  Result := FWebResponse.CustomHeaders;
end;

function TMVCWebResponse.GetLocation: string;
begin
  Result := CustomHeaders.Values['location'];
end;

function TMVCWebResponse.GetReasonString: string;
begin
  Result := FWebResponse.ReasonString;
end;

function TMVCWebResponse.GetStatusCode: Integer;
begin
  Result := FWebResponse.StatusCode;
end;

procedure TMVCWebResponse.SendHeaders;
begin
  FWebResponse.SendResponse
end;

procedure TMVCWebResponse.SetContent(const Value: string);
begin
  FWebResponse.Content := Value;
end;

procedure TMVCWebResponse.SetContentStream(AStream: TStream;
  AContentType: string);
begin
  FWebResponse.ContentType := AContentType;
  FWebResponse.ContentStream := AStream;
end;

procedure TMVCWebResponse.SetContentType(const Value: string);
begin
  FWebResponse.ContentType := Value;
end;

procedure TMVCWebResponse.SetCustomHeader(const Name, Value: string);
begin
  Self.FWebResponse.SetCustomHeader(name, Value);
end;

procedure TMVCWebResponse.SetLocation(const Value: string);
begin
  CustomHeaders.Values['location'] := Value;
end;

procedure TMVCWebResponse.SetReasonString(const Value: string);
begin
  FWebResponse.ReasonString := Value;
end;

procedure TMVCWebResponse.SetStatusCode(const Value: Integer);
begin
  FWebResponse.StatusCode := Value;
end;

{ TMVCWebRequest }

function TMVCWebRequest.Accept: string;
begin
  Result := Self.FWebRequest.Accept;
end;

function TMVCWebRequest.Body: string;
var
  Encoding: TEncoding;
  Buffer: TArray<Byte>;
  I: Integer;
{$IFNDEF BERLINORBETTER}
  TestBuffer: TArray<Byte>;
{$ENDIF}
begin
  Encoding := nil;
  if (FBody = '') then
  begin
    try
{$IFDEF BERLINORBETTER}
      if (FCharset = '') then
      begin
        SetLength(Buffer, 10);
        for I := 0 to 9 do
          Buffer[I] := FWebRequest.RawContent[I];
        TEncoding.GetBufferEncoding(Buffer, Encoding, TEncoding.Default);
        SetLength(Buffer, 0);
      end
      else
      begin
        Encoding := TEncoding.GetEncoding(FCharset);
      end;
      FBody := Encoding.GetString(FWebRequest.RawContent);
{$ELSE}
      SetLength(Buffer, FWebRequest.ContentLength);
      FWebRequest.ReadClient(Buffer[0], FWebRequest.ContentLength);
      if (FCharset = '') then
      begin
        SetLength(TestBuffer, 10);
        for I := 0 to 9 do
        begin
          TestBuffer[I] := Buffer[I];
        end;
        TEncoding.GetBufferEncoding(TestBuffer, Encoding, TEncoding.Default);
        SetLength(TestBuffer, 0);
      end
      else
      begin
        Encoding := TEncoding.GetEncoding(FCharset);
      end;
      FBody := Encoding.GetString(Buffer);
{$ENDIF}
    finally
      Encoding.Free;
    end;
  end;
  Result := FBody;
end;

function TMVCWebRequest.BodyAs<T>(const RootProperty: string): T;
var
  S: string;
  JObj: TJSONObject;
begin
  if ContentType.Equals(TMVCMimeType.APPLICATION_JSON) then
  begin
    if RootProperty = '' then
    begin
      JObj := BodyAsJSONObject;
      if not Assigned(JObj) then
        raise EMVCException.Create('Invalid or not present JSON body');
      Result := Mapper.JSONObjectToObject<T>(JObj);
    end
    else
    begin
      S := Mapper.GetStringDef(BodyAsJSONObject, RootProperty, '');
      if not S.IsEmpty then
        Result := Mapper.JSONObjectToObject<T>(BodyAsJSONObject.Get(S)
          .JsonValue as TJSONObject)
      else
        raise EMVCException.CreateFmt('Body property %s not valid',
          [RootProperty]);
    end;
  end
  else
    raise EMVCException.CreateFmt('Body ContentType %s not supported',
      [ContentType]);
end;

function TMVCWebRequest.BodyAsJSONObject: TJSONObject;
begin
  Result := BodyAsJSONValue as TJSONObject;
end;

function TMVCWebRequest.BodyAsJSONValue: TJSONValue;
begin
  if not Assigned(FBodyAsJSONValue) then
    try
      FBodyAsJSONValue := TJSONObject.ParseJSONValue(Body);
    except
      FBodyAsJSONValue := nil;
    end;
  Result := FBodyAsJSONValue;
end;

function TMVCWebRequest.BodyAsListOf<T>(const RootProperty: string)
  : TObjectList<T>;
var
  S: string;
begin
  if ContentType.Equals(TMVCMimeType.APPLICATION_JSON) then
  begin
    if RootProperty = '' then
      Result := Mapper.JSONArrayToObjectList<T>((BodyAsJSONValue as TJSONArray),
        false, true)
      // Ezequiel J. Müller (bug fix)
    else
    begin
      S := Mapper.GetStringDef(BodyAsJSONObject, RootProperty, '');
      if not S.IsEmpty then
        Result := Mapper.JSONArrayToObjectList<T>(BodyAsJSONObject.Get(S)
          .JsonValue as TJSONArray, false, true) // thank you Ezequiel J. Müller
      else
        raise EMVCException.CreateFmt('Body property %s not valid',
          [RootProperty]);
    end;
  end
  else
    raise EMVCException.CreateFmt('Body ContentType %s not supported',
      [ContentType]);
end;

function TMVCWebRequest.ClientIP: string;
{
  This code has been converted to Delphi from a PHP code
  http://www.grantburton.com/2008/11/30/fix-for-incorrect-ip-addresses-in-wordpress-comments/
}
var
  S: string;
begin
  if FWebRequest.GetFieldByName('HTTP_CLIENT_IP') <> '' then
    Exit(FWebRequest.GetFieldByName('HTTP_CLIENT_IP'));

  for S in string(FWebRequest.GetFieldByName('HTTP_X_FORWARDED_FOR'))
    .Split([',']) do
  begin
    if not S.Trim.IsEmpty then
      Exit(S.Trim);
  end;

  if FWebRequest.GetFieldByName('HTTP_X_FORWARDED') <> '' then
    Exit(FWebRequest.GetFieldByName('HTTP_X_FORWARDED'));

  if FWebRequest.GetFieldByName('HTTP_X_CLUSTER_CLIENT_IP') <> '' then
    Exit(FWebRequest.GetFieldByName('HTTP_X_CLUSTER_CLIENT_IP'));

  if FWebRequest.GetFieldByName('HTTP_FORWARDED_FOR') <> '' then
    Exit(FWebRequest.GetFieldByName('HTTP_FORWARDED_FOR'));

  if FWebRequest.GetFieldByName('HTTP_FORWARDED') <> '' then
    Exit(FWebRequest.GetFieldByName('HTTP_FORWARDED'));

  if FWebRequest.GetFieldByName('REMOTE_ADDR') <> '' then
    Exit(FWebRequest.GetFieldByName('REMOTE_ADDR'));

  if FWebRequest.RemoteIP <> '' then
    Exit(FWebRequest.RemoteIP);

  if FWebRequest.RemoteAddr <> '' then
    Exit(FWebRequest.RemoteAddr);

  if FWebRequest.RemoteHost <> '' then
    Exit(FWebRequest.RemoteHost);

  if FWebRequest.RemoteAddr <> '' then
    Exit(FWebRequest.RemoteAddr);

  if FWebRequest.RemoteIP <> '' then
    Exit(FWebRequest.RemoteIP);

  if FWebRequest.RemoteHost <> '' then
    Exit(FWebRequest.RemoteHost);

  Result := '';
end;

function TMVCWebRequest.ClientPrefer(MimeType: string): Boolean;
begin
  Result := AnsiPos(MimeType, LowerCase(RawWebRequest.Accept)) = 1;
end;

function TMVCWebRequest.ContentParam(Name: string): string;
begin
  Result := FWebRequest.ContentFields.Values[name];
end;

function TMVCWebRequest.Cookie(Name: string): string;
begin
  Result := FWebRequest.CookieFields.Values[name];
end;

constructor TMVCWebRequest.Create(AWebRequest: TWebRequest);
var
  CT: TArray<string>;
  c: string;
begin
  inherited Create;
  FBody := '';
  c := AWebRequest.GetFieldByName('Content-Type');
  if not c.IsEmpty then
  begin
    CT := c.Split([';']);
    FContentType := Trim(CT[0]);
    FCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET; // default charset
    if Length(CT) > 1 then
    begin
      if CT[1].Trim.StartsWith('charset', true) then
      begin
        FCharset := CT[1].Trim.Split(['='])[1].Trim;
      end;
    end;
  end;

  // c := GetHeaderValue('content-encoding');
  // if c.IsEmpty then
  // FContentEncoding := c;
end;

destructor TMVCWebRequest.Destroy;
begin
  FreeAndNil(FBodyAsJSONValue);
  inherited;
end;

{ TMVCAction }

procedure TWebContext.BindToSession(SessionID: string);
begin
  if not Assigned(FWebSession) then
  begin
    FWebSession := TMVCEngine.GetCurrentSession
      (StrToInt64(FConfig[TMVCConfigKey.SessionTimeout]), SessionID, false);
    if not Assigned(FWebSession) then
      raise EMVCException.Create('Invalid SessionID');
    FWebSession.MarkAsUsed;
    TMVCEngine.SendSessionCookie(Self, SessionID);
  end
  else
    raise EMVCException.Create('Session already bounded for this request');
end;

constructor TMVCController.Create;
begin
  inherited Create;
  FContentCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET;
end;

destructor TMVCController.Destroy;
begin
  FreeAndNil(FResponseStream);
  FreeAndNil(FViewDataSets);
  FreeAndNil(FViewModel);
  inherited;
end;

procedure TMVCController.EnqueueMessageOnTopicOrQueue(const IsQueue: Boolean;
  const ATopic: string; AJSONObject: TJSONObject; aOwnsInstance: Boolean);
var
  Stomp: IStompClient;
  H: IStompHeaders;
  msg: TJSONObject;
begin
  msg := TJSONObject.Create;
  try
    if aOwnsInstance then
      msg.AddPair('message', AJSONObject)
    else
      msg.AddPair('message', AJSONObject.Clone as TJSONObject);

    if IsQueue then
      msg.AddPair('_queue', ATopic)
    else
      msg.AddPair('_topic', ATopic);

    msg.AddPair('_username', GetClientID).AddPair('_timestamp',
      FormatDateTime('YYYY-MM-DD HH:NN:SS', Now));

    Stomp := GetNewStompClient(GetClientID);
    H := StompUtils.NewHeaders.Add(StompUtils.NewPersistentHeader(true));
    Stomp.Send(ATopic, msg.ToJSON);
    TThread.Sleep(100);
    // single user cannot enqueue more than 10 message in noe second...
    // it is noot too much elegant, but it works as DoS protection
  finally
    msg.Free;
  end;
end;

function TMVCController.GetClientID: string;
begin
  Result := Session[CLIENTID_KEY];
  if Result.IsEmpty then
    // if Result.IsEmpty then
    raise EMVCException.Create('Invalid ClientID' + sLineBreak +
      'Hint: Messaging extensions require a valid clientid. Did you call /messages/clients/YOUR_CLIENT_ID ?');
end;

function TMVCController.GetContentCharset: string;
begin
  Result := FContentCharset;
end;

function TMVCController.GetContentType: string;
begin
  Result := FContext.Response.ContentType;
end;

function TMVCController.GetCurrentWebModule: TWebModule;
begin
  Result := GetMVCEngine.Owner as TWebModule;
end;

function TMVCController.GetNewStompClient(ClientID: string): IStompClient;
begin
  raise EMVCException.Create('Not Implemented');
  // Result := StompUtils.NewStomp(Config[TMVCConfigKey.StompServer],
  // StrToInt(Config[TMVCConfigKey.StompServerPort]), GetClientID,
  // Config[TMVCConfigKey.StompUsername], Config[TMVCConfigKey.StompPassword]);
end;

function TMVCController.SessionAs<T>: T;
begin
  Result := Session as T;
end;

function TMVCController.GetRenderedView(const ViewNames
  : TArray<string>): string;
var
  View: TMVCMustacheView;
  LViewName: string;
  LSBuilder: TStringBuilder;
begin
  LSBuilder := TStringBuilder.Create;
  try
    try
      for LViewName in ViewNames do
      begin

        View := TMVCMustacheView.Create(LViewName, GetMVCEngine, FContext,
          FViewModel, FViewDataSets, ContentType);
        try
          View.SetMVCConfig(GetMVCConfig);
          View.Execute;
          LSBuilder.Append(View.GetOutput);
        finally
          View.Free;
        end;
      end;
      Result := LSBuilder.ToString;
    except
      on E: Exception do
      begin
        ContentType := 'text/plain';
        Render(E);
      end;
    end;
  finally
    LSBuilder.Free;
  end;
end;

function TMVCController.GetWebSession: TWebSession;
begin
  Result := FContext.Session;
end;

function TMVCController.LoadView(const ViewNames: TArray<string>): string;
begin
  try
    Result := GetRenderedView(ViewNames);
    ResponseStream.Append(Result);
  except
    on E: Exception do
    begin
      LogException(E);
      ContentType := 'text/plain';
      Render(E);
    end;
  end;
end;

procedure TMVCController.LoadViewFragment(const ViewFragment: string);
begin
  ResponseStream.Append(ViewFragment);
end;

procedure TMVCController.MVCControllerAfterCreate;
begin
  inherited;
end;

procedure TMVCController.MVCControllerBeforeDestroy;
begin
  inherited;
end;

procedure TMVCController.OnAfterAction(Context: TWebContext;
  const aActionName: string);
begin
  // do nothing
end;

procedure TMVCController.OnBeforeAction(Context: TWebContext;
  const aActionName: string; var Handled: Boolean);
begin
  Handled := false;
  if ContentType.IsEmpty then
    ContentType := GetMVCConfig[TMVCConfigKey.DefaultContentType];
end;

procedure TMVCController.PushDataSetToView(const AModelName: string;
  aDataSet: TDataSet);
var
  LJArr: TJSONArray;
begin
  LJArr := TJSONArray.Create;
  try
    Mapper.DataSetToJSONArray(aDataSet, LJArr, true);
  except
    LJArr.Free;
    raise;
  end;
  PushJSONToView(AModelName, LJArr);
end;

procedure TMVCController.PushJSONToView(const AModelName: string;
  AModel: TJSONValue);
begin
  if not Assigned(FViewModel) then
    FViewModel := TMVCDataObjects.Create;
  FViewModel.Add(AModelName, AModel);
end;

procedure TMVCController.PushObjectToView(const AModelName: string;
  AModel: TObject);
begin
  PushJSONToView(AModelName, Mapper.ObjectToJSONObject(AModel));
end;

procedure InternalRenderText(const AContent: string;
  ContentType, ContentEncoding: string; Context: TWebContext);
var
  OutEncoding: TEncoding;
  lContentType: String;
begin
  lContentType := ContentType + '; charset=' + ContentEncoding;
  OutEncoding := TEncoding.GetEncoding(ContentEncoding);
  try
    // Context.Response.RawWebResponse.ContentStream := TStringStream.Create(UTF8Encode(AContent));
    if SameText('UTF-8', ContentEncoding) then
    begin
      Context.Response.SetContentStream(
        // TStringStream.Create(UTF8Encode(AContent), TEncoding.UTF8),
        TStringStream.Create( { UTF8Encode( } AContent { ) } , TEncoding.UTF8),
        lContentType);
      // Context.Response.RawWebResponse.Content := '';
      // Context.Response.RawWebResponse.ContentStream :=
      // TStringStream.Create(UTF8Encode(AContent));
    end
    else
    begin
      Context.Response.SetContentStream(
        TBytesStream.Create(
        TEncoding.Convert(TEncoding.Default, OutEncoding, TEncoding.Default.GetBytes(AContent))),
        lContentType
        );
      // Context.Response.RawWebResponse.Content :=
      // OutEncoding.GetString(TEncoding.Convert(TEncoding.UTF8, OutEncoding,
      // TEncoding.Default.GetBytes(AContent)));
    end;
  finally
    OutEncoding.Free;
  end;
  // Context.Response.RawWebResponse.ContentType := TMVCMimeType.APPLICATION_JSON;
  // Context.Response.RawWebResponse.ContentEncoding := ContentEncoding;
  // OutEncoding := TEncoding.GetEncoding(ContentEncoding);
  // InEncoding := TEncoding.Default; // GetEncoding(S);
  // Context.Response.Content := OutEncoding.GetString
  // (TEncoding.Convert(InEncoding, OutEncoding, InEncoding.GetBytes(AContent)));
  // OutEncoding.Free;
end;

procedure InternalRender(aJSONValue: TJSONValue;
  ContentType, ContentEncoding: string; Context: TWebContext;
  aInstanceOwner: Boolean);
var
  OutEncoding: TEncoding;
  lContentType, lJString: string;
begin
  lJString := aJSONValue.ToJSON;
  // first set the ContentType; because of this bug:
  // http://qc.embarcadero.com/wc/qcmain.aspx?d=67350
  Context.Response.RawWebResponse.ContentType := ContentType + '; charset=' +
    ContentEncoding;
  lContentType := ContentType + '; charset=' +
    ContentEncoding;
  OutEncoding := TEncoding.GetEncoding(ContentEncoding);
  try
    Context.Response.SetContentStream(
      TBytesStream.Create(
      TEncoding.Convert(TEncoding.Default, OutEncoding,
      TEncoding.Default.GetBytes(lJString))
      ), lContentType);

    // Context.Response.SetContent(
    // OutEncoding.GetString(
    // TEncoding.Convert(TEncoding.Default, OutEncoding,
    // TEncoding.Default.GetBytes(lJString))
    // ));

    // Context.Response.RawWebResponse.Content :=
    // OutEncoding.GetString(TEncoding.Convert(TEncoding.Default, OutEncoding,
    // TEncoding.Default.GetBytes(JString)));
  finally
    OutEncoding.Free;
  end;

  if aInstanceOwner then
    FreeAndNil(aJSONValue)
end;

procedure InternalRender(const Content: string;
  ContentType, ContentEncoding: string; Context: TWebContext);
begin
  if ContentType = TMVCMimeType.APPLICATION_JSON then
  begin
    InternalRender(TJSONString.Create(Content), ContentType, ContentEncoding,
      Context, true);
  end
  else if ContentType = TMVCMimeType.TEXT_XML then
  begin
    raise EMVCException.Create('Format still not supported - ' + ContentType);
  end
  else
  begin
    if ContentType.IsEmpty then
      InternalRenderText(Content, 'text/plain', ContentEncoding, Context)
    else
      InternalRenderText(Content, ContentType, ContentEncoding, Context);
  end;
end;

procedure TMVCController.RenderResponseStream;
begin
  InternalRenderText(ResponseStream.ToString, ContentType, ContentCharset, Context);
end;

procedure TMVCController.Render(const Content: string);
begin
  InternalRender(Content, ContentType, ContentCharset, Context);
end;

procedure TMVCController.Render(aObject: TObject; aInstanceOwner: Boolean;
  aSerializationType: TDMVCSerializationType);
var
  JSON: TJSONObject;
begin
  if aSerializationType = TDMVCSerializationType.Properties then
    JSON := Mapper.ObjectToJSONObject(aObject)
  else
    JSON := Mapper.ObjectToJSONObjectFields(aObject, []);
  Render(JSON, true);
  if aInstanceOwner then
    FreeAndNil(aObject);
end;

procedure TMVCController.SendFile(AFileName: string);
begin
  TMVCStaticContents.SendFile(AFileName, ContentType, Context);
end;

function TWebContext.SendSessionCookie(AContext: TWebContext): string;
begin
  Result := TMVCEngine.SendSessionCookie(Self);
end;

procedure TMVCController.SendStream(AStream: TStream; AOwnStream: Boolean;
  ARewindStream: Boolean);
var
  lStream: TStream;
begin
  if ARewindStream then
    AStream.Position := 0;

  if not AOwnStream then
  begin
    lStream := TMemoryStream.Create;
    lStream.CopyFrom(AStream, 0);
    lStream.Position := 0;
  end
  else
  begin
    lStream := AStream;
  end;

  FContext.Response.FWebResponse.Content := '';
  FContext.Response.FWebResponse.ContentType := ContentType;
  FContext.Response.FWebResponse.ContentStream := lStream;
  FContext.Response.FWebResponse.FreeContentStream := true;
end;

function TWebContext.SessionID: string;
begin
  if Assigned(FWebSession) then
    Exit(FWebSession.SessionID);
  Result := FRequest.Cookie(TMVCConstants.SESSION_TOKEN_NAME);
end;

function TWebContext.SessionMustBeClose: Boolean;
begin
  Result := FSessionMustBeClose;
end;

procedure TWebContext.SessionStart;
var
  LSessionID: string;
begin
  if not Assigned(FWebSession) then
  begin
    LSessionID := TMVCEngine.SendSessionCookie(Self);
    FWebSession := AddSessionToTheSessionList(
      Config[TMVCConfigKey.SessionType],
      LSessionID,
      StrToInt64(Config[TMVCConfigKey.SessionTimeout]));
    FIsSessionStarted := true;
    FSessionMustBeClose := false;
  end;
end;

function TWebContext.SessionStarted: Boolean;
var
  LSessionID: string;
begin
  LSessionID := SessionID;
  if LSessionID.IsEmpty then
    Exit(false);
  TMonitor.Enter(SessionList);
  try
    Result := SessionList.ContainsKey(LSessionID);
  finally
    TMonitor.Exit(SessionList);
  end;
end;

procedure TWebContext.SessionStop(ARaiseExceptionIfExpired: Boolean);
var
  Cookie: TCookie;
  LSessionID: string;
begin
  // Set-Cookie: token=deleted; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT
  FResponse.Cookies.Clear; // daniele ... remove all previous cookies
  Cookie := FResponse.Cookies.Add;
  Cookie.Name := TMVCConstants.SESSION_TOKEN_NAME;

  // rubbish... invalid the cookie value
  Cookie.Value := GUIDToString(TGUID.NewGuid) + 'invalid' +
    GUIDToString(TGUID.NewGuid);
  Cookie.Expires := EncodeDate(1970, 1, 1);
  Cookie.Path := '/';

  TMonitor.Enter(SessionList);
  try
    LSessionID := TMVCEngine.ExtractSessionIDFromWebRequest
      (FRequest.RawWebRequest);
    // if not Assigned(FWebSession) then
    // FWebSession := TMVCEngine.GetCurrentSession
    // (StrToInt64(FConfig[TMVCConfigKey.SessionTimeout]), '',
    // ARaiseExceptionIfExpired);
    // if Assigned(FWebSession) then
    SessionList.Remove(LSessionID);
  finally
    TMonitor.Exit(SessionList);
  end;
  FIsSessionStarted := false;
  FSessionMustBeClose := true;
end;

procedure TMVCController.SetContentCharset(const Value: string);
begin
  FContentCharset := Value;
end;

procedure TMVCController.SetContentType(const Value: string);
begin
  FContext.Response.ContentType := Value;
end;

procedure TMVCController.SetContext(const Value: TWebContext);
begin
  if FContext = nil then
    FContext := Value
  else
    raise EMVCException.Create('Context already set');
end;

// procedure TMVCController.SetViewCache(const Value: TViewCache);
// begin
// FViewCache := Value;
// end;

procedure TMVCController.SetWebSession(const Value: TWebSession);
begin
  raise Exception.Create('Qualcuno mi usa...');
  // if Assigned(FContext.FWebSession) then
  // raise EMVCException.Create('Web Session already set for controller ' +
  // ClassName);
  // FContext.FWebSession := Value;
  // FIsSessionStarted := Assigned(FContext.FWebSession);
end;

{ TMVCPathAttribute }

constructor MVCPathAttribute.Create(const Value: string);
begin
  inherited Create;
  FPath := Value;
end;

function TMVCWebRequest.QueryStringParams: TStrings;
begin
  Result := FWebRequest.QueryFields;
end;

function TMVCWebRequest.QueryStringParam(Name: string): string;
begin
  Result := FWebRequest.QueryFields.Values[name];
end;

procedure TMVCWebRequest.EnsureQueryParamExists(const Name: string);
begin
  if GetParamAll(name).IsEmpty then
    raise EMVCException.CreateFmt('Parameter "%s" required', [name]);
end;

function TMVCWebRequest.QueryStringParamExists(Name: string): Boolean;
begin
  Result := not QueryStringParam(name).IsEmpty;
end;

function TMVCWebRequest.GetClientPreferHTML: Boolean;
begin
  Result := ClientPrefer(TMVCMimeType.TEXT_HTML);
end;

function TMVCWebRequest.GetFiles: TAbstractWebRequestFiles;
begin
  Result := FWebRequest.Files;
end;

function TMVCWebRequest.GetHeader(const Name: string): string;
begin
  if Assigned(FWebRequest) then
    Result := FWebRequest.GetFieldByName(name)
  else
    Result := '';
end;

// function TMVCWebRequest.GetHeaderValue(const Name: string): string;
// var
// S: string;
// begin
// S := GetHeader(name);
// if S.IsEmpty then
// Result := ''
// else
// Result := S.Split([':'])[1].trim;
// end;

// function TMVCWebRequest.GetHeaderAll(const HeaderName: string): string;
// begin
// Result := Self.FWebRequest.GetFieldByName(HeaderName);
// end;

function TMVCWebRequest.GetHTTPMethod: TMVCHTTPMethodType;
begin
  Result := TMVCRouter.StringMethodToHTTPMetod(FWebRequest.Method);
end;

function TMVCWebRequest.GetHTTPMethodAsString: string;
begin
  Result := FWebRequest.Method;
end;

function TMVCWebRequest.GetIsAjax: Boolean;
begin
  Result := LowerCase(FWebRequest.GetFieldByName('X-Requested-With'))
    = 'xmlhttprequest';
end;

function TMVCWebRequest.GetSegmentParam(const ParamName: string;
  out Value: string): Boolean;
begin
  if (not Assigned(FParamsTable)) then
    Exit(false);
  Result := FParamsTable.TryGetValue(ParamName, Value);
end;

function TMVCWebRequest.GetSegmentParamsCount: Integer;
begin
  if Assigned(FParamsTable) then
    Result := FParamsTable.Count
  else
    Result := 0;
end;

function TMVCWebRequest.GetParamAll(const ParamName: string): string;
begin
  if (not Assigned(FParamsTable)) or
    (not FParamsTable.TryGetValue(ParamName, Result)) then
  begin
    Result := FWebRequest.QueryFields.Values[ParamName];
    if Result = EmptyStr then
      Result := FWebRequest.ContentFields.Values[ParamName];
    if Result = EmptyStr then
      Result := FWebRequest.CookieFields.Values[ParamName];
  end;
end;

function TMVCWebRequest.GetParamAllAsInt64(const ParamName: string): Int64;
begin
  Result := StrToInt64(GetParamAll(ParamName));
end;

function TMVCWebRequest.GetParamAllAsInteger(const ParamName: string): Integer;
begin
  Result := StrToInt(GetParamAll(ParamName));
end;

function TMVCWebRequest.GetParamNames: TArray<string>;
var
  I: Integer;
  Names: TList<string>;
  n: string;
begin
  if Length(FParamNames) > 0 then
    Exit(FParamNames);

  Names := TList<string>.Create;
  try
    if Assigned(FParamsTable) and (Length(FParamsTable.Keys.ToArray) > 0) then
      for n in FParamsTable.Keys.ToArray do
        Names.Add(n);

    if FWebRequest.QueryFields.Count > 0 then
      for I := 0 to FWebRequest.QueryFields.Count - 1 do
        Names.Add(FWebRequest.QueryFields.Names[I]);

    if FWebRequest.ContentFields.Count > 0 then
      for I := 0 to FWebRequest.ContentFields.Count - 1 do
        Names.Add(FWebRequest.ContentFields.Names[I]);

    if FWebRequest.CookieFields.Count > 0 then
      for I := 0 to FWebRequest.CookieFields.Count - 1 do
        Names.Add(FWebRequest.CookieFields.Names[I]);
    Result := Names.ToArray;
  finally
    Names.Free;
  end;
end;

function TMVCWebRequest.GetPathInfo: string;
begin
  Result := FWebRequest.PathInfo;
end;

procedure TMVCWebRequest.SetParamsTable(AParamsTable: TMVCRequestParamsTable);
begin
  FParamsTable := AParamsTable;
end;

function TMVCWebRequest.ThereIsRequestBody: Boolean;
begin
  Result := FWebRequest.Content <> '';
end;

{ MVCHTTPMethodAttribute }

constructor MVCHTTPMethodAttribute.Create(AMVCHTTPMethods: TMVCHTTPMethods);
begin
  inherited Create;
  FMVCHTTPMethods := AMVCHTTPMethods;
end;

function MVCHTTPMethodAttribute.GetMVCHTTPMethodsAsString: string;
var
  I: TMVCHTTPMethodType;
begin
  Result := '';
  for I := low(TMVCHTTPMethodType) to high(TMVCHTTPMethodType) do
  begin
    if I in FMVCHTTPMethods then
    begin
      Result := Result + ',' + GetEnumName
        (TypeInfo(TMVCHTTPMethodType), Ord(I));
    end;
  end;

  if not Result.IsEmpty then
    Result := Result.Remove(0, 1)
  else
    Result := 'any';
end;

{ TMVCStaticContents }
class
  procedure TMVCStaticContents.SendFile(AFileName, AMimeType: string;
  Context: TWebContext);
var
  LFileDate: TDateTime;
  LReqDate: TDateTime;
  S: TFileStream;
begin
  LFileDate := IndyFileAge(AFileName);
  if (LFileDate = 0.0) and (not FileExists(AFileName)) then
  begin
    Context.Response.StatusCode := 404;
  end
  else
  begin
    LReqDate := GMTToLocalDateTime(Context.Request.Headers
      ['If-Modified-Since']);
    if (LReqDate <> 0) and (abs(LReqDate - LFileDate) < 2 * (1 / (24 * 60 * 60)))
    then
    begin
      Context.Response.ContentType := AMimeType;
      Context.Response.StatusCode := 304;
    end
    else
    begin
      S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
      // Content-Length is set in (%DELPHI%)\source\internet\Web.Win.IsapiHTTP.pas
      // procedure TISAPIResponse.SendResponse;
      // if set twice it could be a problem under IIS (ISAPI)
      // the header is available 1x but the value are doubled
      // sometimes some images are not shown
      // How to unittest this behavior?
      // Context.Response.SetCustomHeader('Content-Length', IntToStr(S.Size));
      Context.Response.SetCustomHeader('Last-Modified',
        LocalDateTimeToHttpStr(LFileDate));
      Context.Response.SetContentStream(S, AMimeType);
    end;
  end;
end;

class
  function TMVCStaticContents.IsScriptableFile(StaticFileName: string;
  Config: TMVCConfig): Boolean;
begin
  Result := TPath.GetExtension(StaticFileName).ToLower = '.' +
    Config[TMVCConfigKey.DefaultViewFileExtension].ToLower;
end;

class
  function TMVCStaticContents.IsStaticFile(AViewPath, AWebRequestPath
  : string; out ARealFileName: string): Boolean;
var
  FileName: string;
begin
  if TDirectory.Exists(AViewPath) then // absolute path
    FileName := AViewPath + AWebRequestPath.Replace('/',
      TPath.DirectorySeparatorChar)
  else
    FileName := GetApplicationFileNamePath + AViewPath +
    // relative path
      AWebRequestPath.Replace('/', TPath.DirectorySeparatorChar);
  Result := TFile.Exists(FileName);
  ARealFileName := FileName;
end;

procedure TMVCBase.SetApplicationSession(const Value: TWebApplicationSession);
begin
  if Assigned(FApplicationSession) then
    raise EMVCException.Create('Application Session already set');
  FApplicationSession := Value;
end;

procedure TMVCBase.SetMVCConfig(const Value: TMVCConfig);
begin
  FMVCConfig := Value;
end;

procedure TMVCBase.SetMVCEngine(const Value: TMVCEngine);
begin
  FMVCEngine := Value;
end;

class
  function TMVCBase.GetApplicationFileName: string;
var
  fname: PChar;
  Size: Integer;
begin
  Result := '';
  fname := GetMemory(2048);
  try
    Size := GetModuleFileName(0, fname, 2048);
    if Size > 0 then
      Result := fname;
  finally
    FreeMem(fname, 2048);
  end;
end;

class
  function TMVCBase.GetApplicationFileNamePath: string;
begin
  Result := IncludeTrailingPathDelimiter
    (ExtractFilePath(GetApplicationFileName));
end;

function TMVCBase.GetMVCConfig: TMVCConfig;
begin
  if not Assigned(FMVCConfig) then
    EMVCConfigException.Create('MVCConfig not assigned to this ' + ClassName +
      ' instances');
  Result := FMVCConfig;

end;

function TMVCBase.GetMVCEngine: TMVCEngine;
begin
  Result := FMVCEngine;
end;

{ TMVCISAPIWebRequest }

constructor TMVCISAPIWebRequest.Create(AWebRequest: TWebRequest);
begin
  inherited;
  FWebRequest := AWebRequest as TISAPIRequest;
end;

{ TMVCApacheWebRequest }
{$IF CompilerVersion >= 27}


constructor TMVCApacheWebRequest.Create(AWebRequest: TWebRequest);
begin
  inherited;
  FWebRequest := AWebRequest as TApacheRequest;
end;
{$ENDIF}
{ TMVCINDYWebRequest }

constructor TMVCINDYWebRequest.Create(AWebRequest: TWebRequest);
begin
  inherited;
  FWebRequest := AWebRequest; // as TIdHTTPAppRequest;
end;
{ TWebSession }

procedure TMVCController.RaiseSessionExpired;
begin
  raise EMVCSessionExpiredException.Create('Session expired');
end;

function TMVCController.ReceiveMessageFromTopic(const ATopic: string;
  ATimeout: Int64; var JSONObject: TJSONObject): Boolean;
var
  Stomp: IStompClient;
  frame: IStompFrame;
  o: TJSONValue;
begin
  Result := false;
  Stomp := GetNewStompClient(GetClientID);
  if not Stomp.Receive(frame, ATimeout) then
    JSONObject := nil
  else
  begin
    o := TJSONObject.ParseJSONValue(frame.GetBody);
    if not Assigned(o) then
      raise EMVCException.Create('Message is not a valid JSONObject')
    else
    begin
      if not(o is TJSONObject) then
      begin
        FreeAndNil(o);
        raise EMVCException.Create
          ('Message is a JSONValue but not a JSONObject')
      end
      else
        JSONObject := TJSONObject(o);
    end;
  end;
end;

procedure TMVCController.Redirect(const URL: string);
begin
  FContext.Response.FWebResponse.SendRedirect(URL);
end;

procedure TMVCController.Render(E: Exception; ErrorItems: TList<string>);
var
  j: TJSONObject;
  S: string;
  jarr: TJSONArray;
begin
  if E is EMVCException then
    ResponseStatusCode(EMVCException(E).HTTPErrorCode,
      E.Message + ' [' + E.ClassName + ']')
  else
  begin
    if Context.Response.StatusCode = 200 then
      ResponseStatusCode(500, E.Message + ' [' + E.ClassName + ']');
  end;

  if (not Context.Request.IsAjax) and (Context.Request.ClientPreferHTML) then
  begin
    ContentType := TMVCMimeType.TEXT_HTML;
    ContentCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET;
    ResponseStream.Clear;

    ResponseStream.Append
      ('<html><head><style>pre { color: #000000; background-color: #d0d0d0; }</style></head><body>')
      .Append('<h1>' + Config[TMVCConfigKey.ServerName] + ': Error Raised</h1>')
      .AppendFormat('<pre>HTTP Return Code: %d' + sLineBreak,
      [Context.Response.StatusCode])
      .AppendFormat('HTTP Reason Text: "%s"</pre>',
      [Context.Response.ReasonString]).Append('<h3><pre>')
      .AppendFormat('Exception Class Name : %s' + sLineBreak, [E.ClassName])
      .AppendFormat('Exception Message    : %s' + sLineBreak, [E.Message])
      .Append('</pre></h3>');
    if Assigned(ErrorItems) and (ErrorItems.Count > 0) then
    begin
      ResponseStream.Append('<h2><pre>');
      for S in ErrorItems do
        ResponseStream.AppendLine('- ' + S);
      ResponseStream.Append('</pre><h2>');
    end
    else
    begin
      ResponseStream.AppendLine('<pre>No other informations available</pre>');
    end;
    ResponseStream.Append('</body></html>');
    Render;
  end
  else if Context.Request.IsAjax or (ContentType = 'application/json') then
  begin
    j := TJSONObject.Create;
    j.AddPair('status', 'error');
    j.AddPair('classname', E.ClassName);
    j.AddPair('message', E.Message);
    j.AddPair('http_error', TJSONNumber.Create(Context.Response.StatusCode));
    if Assigned(ErrorItems) then
    begin
      jarr := TJSONArray.Create;
      j.AddPair('erroritems', jarr);
      for S in ErrorItems do
      begin
        jarr.AddElement(TJSONString.Create(S));
      end;
    end;
    Render(j);
  end
  else
  begin
    Render(Format('Exception: [%s] %s', [E.ClassName, E.Message]));
  end;
end;

procedure TMVCController.Render(const aErrorCode: UInt16;
  const aErrorMessage: string; const AErrorClassName: string = '');
var
  j: TJSONObject;
  status: string;
begin
  ResponseStatusCode(aErrorCode, aErrorMessage);
  if Context.Request.IsAjax or (ContentType = 'application/json') then
  begin
    status := 'error';
    if (aErrorCode div 100) = 2 then
      status := 'ok';
    j := TJSONObject.Create;
    j.AddPair('status', status);
    if AErrorClassName = '' then
      j.AddPair('classname', TJSONNull.Create)
    else
      j.AddPair('classname', AErrorClassName);
    j.AddPair('message', aErrorMessage);
    Render(j);
  end
  else
  begin
    Render(Format('Error: [%d] %s', [aErrorCode, aErrorMessage]));
  end;
end;

procedure TMVCController.Render(aDataSet: TDataSet; aInstanceOwner: Boolean;
  aOnlySingleRecord: Boolean; aJSONObjectActionProc: TJSONObjectActionProc);
var
  arr: TJSONArray;
  JObj: TJSONObject;
begin
  if ContentType = TMVCMimeType.APPLICATION_JSON then
  begin
    if not aOnlySingleRecord then
    begin
      aDataSet.First;
      arr := TJSONArray.Create;
      Mapper.DataSetToJSONArray(aDataSet, arr, aInstanceOwner,
        aJSONObjectActionProc);
      Render(arr);
    end
    else
    begin
      JObj := TJSONObject.Create;
      Mapper.DataSetToJSONObject(aDataSet, JObj, aInstanceOwner,
        aJSONObjectActionProc);
      Render(JObj);
    end;
  end
  else
    raise Exception.Create('ContentType not supported for this render [' +
      ContentType + ']');
  // if ContentType = TMVCMimeType.TEXT_XML then
  // begin
  // Mapper.DataSetToXML(ADataSet, S, AInstanceOwner);
  // Render(S);
  // end;
end;

procedure TMVCController.Render(const AStream: TStream;
  aInstanceOwner: Boolean);
begin
  SendStream(AStream, aInstanceOwner);
end;

procedure TMVCController.RenderWrappedList(aList: IWrappedList;
  aJSONObjectActionProc: TJSONObjectActionProc = nil;
  aSerializationType: TDMVCSerializationType = TDMVCSerializationType.
  Properties);
var
  JSON: TJSONArray;
begin
  if aSerializationType = TSerializationType.Properties then
    JSON := Mapper.ObjectListToJSONArray(aList, true,
      aJSONObjectActionProc)
  else
    JSON := Mapper.ObjectListToJSONArrayFields(aList, true,
      aJSONObjectActionProc);
  Render(JSON, true);
end;

procedure TMVCController.Render<T>(aCollection: TObjectList<T>;
  aInstanceOwner: Boolean; aJSONObjectActionProc: TJSONObjectActionProc;
  aSerializationType: TSerializationType);
var
  JSON: TJSONArray;
begin
  if aSerializationType = TSerializationType.Properties then
    JSON := Mapper.ObjectListToJSONArray<T>(aCollection, false,
      aJSONObjectActionProc)
  else
    JSON := Mapper.ObjectListToJSONArrayFields<T>(aCollection, false,
      aJSONObjectActionProc);
  Render(JSON, true);
  if aInstanceOwner then
    FreeAndNil(aCollection);
end;

procedure TMVCController.RenderJSONArrayAsProperty(const aPropertyName: string;
  AJSONArray: TJSONArray);
begin
  Render(TJSONObject.Create(TJSONPair.Create(aPropertyName,
    AJSONArray)));
end;

procedure TMVCController.RenderListAsProperty<T>(const aPropertyName: string;
  aObjectList: TObjectList<T>; aOwnsInstance: Boolean;
  aJSONObjectActionProc: TJSONObjectActionProc);
begin
  Render(TJSONObject.Create(TJSONPair.Create(aPropertyName,
    Mapper.ObjectListToJSONArray<T>(aObjectList, aOwnsInstance,
    aJSONObjectActionProc))));
end;

procedure TMVCController.RenderStreamAndFree(const AStream: TStream);
begin
  SendStream(AStream);
end;

procedure TMVCController.Render(aTextWriter: TTextWriter; aInstanceOwner: Boolean);
begin
  InternalRenderText(aTextWriter.ToString, ContentType, ContentCharset, Context);
end;

procedure TMVCController.Render(aJSONValue: TJSONValue;
  aInstanceOwner: Boolean);
begin
  InternalRender(aJSONValue, ContentType, ContentCharset, Context,
    aInstanceOwner);
end;

procedure TMVCController.ResponseStatusCode(const AStatusCode: UInt16;
  AStatusText: string);
begin
  StatusCode := AStatusCode;
  Context.Response.ReasonString := AStatusText;
end;

function TMVCController.GetStatusCode: UInt16;
begin
  Result := Context.Response.StatusCode;
end;

procedure TMVCController.SetStatusCode(const Value: UInt16);
begin
  Context.Response.StatusCode := Value;
end;

function TMVCController.ResponseStream: TStringBuilder;
begin
  if not Assigned(FResponseStream) then
    FResponseStream := TStringBuilder.Create;
  Result := FResponseStream;
end;

constructor MVCPathAttribute.Create;
begin
  Create('');
end;

procedure TMVCController.Render(const aErrorCode: UInt16;
  aJSONValue: TJSONValue; aInstanceOwner: Boolean);
begin
  ResponseStatusCode(aErrorCode);
  if ContentType = 'application/json' then
  begin
    Render(aJSONValue, aInstanceOwner);
  end
  else
  begin
    raise EMVCException.Create
      ('Cannot render a JSONValue if ContentType is not application/json');
  end;

end;

procedure TMVCController.Render(const aErrorCode: UInt16; aObject: TObject;
  aInstanceOwner: Boolean);
begin
  Render(aErrorCode, Mapper.ObjectToJSONObject(aObject), true);
  if aInstanceOwner then
    aObject.Free;
end;

procedure TMVCController.Render;
begin
  RenderResponseStream;
end;

{ MVCStringAttribute }

constructor MVCStringAttribute.Create(const Value: string);
begin
  inherited Create;
  FValue := Value;
end;

function IsShuttingDown: Boolean;
begin
  Result := TInterlocked.Read(_IsShuttingDown) = 1
end;

procedure EnterInShutdownState;
begin
  TInterlocked.Add(_IsShuttingDown, 1);
end;

{ MVCProduceAttribute }

constructor MVCProducesAttribute.Create(const Value, ProduceEncoding: string);
begin
  Create(Value);
  FProduceEncoding := ProduceEncoding;
end;

constructor MVCProducesAttribute.Create(const Value: string);
begin
  inherited;
  FProduceEncoding := 'UTF-8';
end;

procedure MVCProducesAttribute.SetProduceEncoding(const Value: string);
begin
  FProduceEncoding := Value;
end;

{ TUser }

procedure TUser.Clear;
begin
  FUserName := '';
  FLoggedSince := 0;
  FRealm := '';
  FRoles.Clear;
end;

constructor TUser.Create;
begin
  inherited;
  FCustomData := nil;
  FRoles := TList<string>.Create;
  Clear;
end;

destructor TUser.Destroy;
begin
  FRoles.Free;
  FreeAndNil(FCustomData);
  inherited;
end;

function TUser.GetIsValidLoggedUser: Boolean;
begin
  Result := (not UserName.IsEmpty) and (LoggedSince > 0);
end;

function TUser.LoadFromSession(AWebSession: TWebSession): Boolean;
var
  LSerObj: string;
  LPieces: TArray<string>;
  I: Integer;
begin
  if not Assigned(AWebSession) then
    Exit(false);
  LSerObj := AWebSession[TMVCConstants.CURRENT_USER_SESSION_KEY];
  Result := not LSerObj.IsEmpty;
  if Result then
  begin
    Clear;
    LPieces := LSerObj.Split(['$$'], TStringSplitOptions.None);
    UserName := LPieces[0];
    LoggedSince := ISOStrToDateTime(LPieces[1]);
    Realm := LPieces[2];
    Roles.Clear;
    for I := 2 to Length(LPieces) - 1 do
    begin
      Roles.Add(LPieces[I]);
    end;
  end;
end;

procedure TUser.SaveToSession(AWebSession: TWebSession);
var
  LRoles: string;
begin
  if FRoles.Count > 0 then // bug in string.Join
    LRoles := string.Join('$$', FRoles.ToArray)
  else
    LRoles := '';
  AWebSession[TMVCConstants.CURRENT_USER_SESSION_KEY] := FUserName + '$$' +
    ISODateTimeToString(FLoggedSince) + '$$' + FRealm + '$$' + LRoles;
end;

procedure TUser.SetCustomData(const Value: TMVCCustomData);
begin
  FCustomData := Value;
end;

procedure TUser.SetLoggedSince(const Value: TDateTime);
begin
  if FLoggedSince = 0 then
    FLoggedSince := Value
  else
    raise EMVCException.Create('User.LoggedSince already set');
end;

procedure TUser.SetRealm(const Value: string);
begin
  FRealm := Value;
end;

procedure TUser.SetUserName(const Value: string);
begin
  FUserName := Value;
end;

{ TMVCControllerRoutable }

constructor TMVCControllerRoutable.Create(AClass: TMVCControllerClass;
  ADelegate: TMVCControllerDelegate);
begin
  FClass := AClass;
  FDelegate := ADelegate;
end;

initialization

_IsShuttingDown := 0;

end.