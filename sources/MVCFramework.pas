// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
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
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.IOUtils,
  System.SyncObjs,
  System.DateUtils,
  System.Generics.Collections,
  System.Rtti,
  // WinApi.Windows,
  MVCFramework.Commons,
  Data.DB,
  MVCFramework.Session,
  MVCFramework.DuckTyping,
  MVCFramework.Logger,
  MVCFramework.ApplicationSession,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.JSON,

  {$IFDEF WEBAPACHEHTTP}

  Web.ApacheHTTP, // Apache Support since XE6 http://docwiki.embarcadero.com/Libraries/XE6/de/Web.ApacheHTTP

  {$ENDIF}

  // Delphi XE4 (all update) and XE5 (with no update) dont contains this unit. Look for the bug in QC
  // https://quality.embarcadero.com/browse/RSP-17216

  {$IFNDEF VER320}

  Web.ReqMulti,

  {$ENDIF}

  Web.HTTPApp,

  {$IFNDEF LINUX}

  Web.Win.IsapiHTTP,

  {$ENDIF}

  Web.WebReq,
  LoggerPro,
  IdGlobal,
  IdGlobalProtocols,
  IdURI;

type

  TSessionData = TDictionary<string, string>;
  TMVCBaseViewEngine = class;
  TMVCViewEngineClass = class of TMVCBaseViewEngine;

  MVCBaseAttribute = class(TCustomAttribute)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  MVCHTTPMethodsAttribute = class(MVCBaseAttribute)
  private
    FMVCHTTPMethods: TMVCHTTPMethods;
    function GetMVCHTTPMethodsAsString: string;
  protected
    { protected declarations }
  public
    constructor Create(const AMVCHTTPMethods: TMVCHTTPMethods);
    property MVCHTTPMethods: TMVCHTTPMethods read FMVCHTTPMethods;
    property MVCHTTPMethodsAsString: string read GetMVCHTTPMethodsAsString;
  end;

  MVCHTTPMethodAttribute = MVCHTTPMethodsAttribute;

  MVCStringAttribute = class(MVCBaseAttribute)
  private
    FValue: string;
  protected
    { protected declarations }
  public
    constructor Create(const AValue: string);
    property Value: string read FValue;
  end;

  MVCConsumesAttribute = class(MVCStringAttribute)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  MVCProducesAttribute = class(MVCStringAttribute)
  private
    FEncoding: string;
  protected
    { protected declarations }
  public
    constructor Create(const AValue: string); overload;
    constructor Create(const AValue: string; const AEncoding: string); overload;
    property Encoding: string read FEncoding;
  end;

  MVCDocAttribute = class(MVCStringAttribute)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  MVCPathAttribute = class(MVCBaseAttribute)
  private
    FPath: string;
  protected
    { protected declarations }
  public
    constructor Create(const APath: string); overload;
    property Path: string read FPath;
  end;

  TMVCWebRequest = class
  private
    FWebRequest: TWebRequest;
    FSerializers: TDictionary<string, IMVCSerializer>;
    FBody: string;
    FContentType: string;
    FCharset: string;
    FParamsTable: TMVCRequestParamsTable;
    procedure DefineContentTypeAndCharset;
    function GetHeader(const AName: string): string;
    function GetPathInfo: string;
    function GetParams(const AParamName: string): string;
    function GetIsAjax: Boolean;
    function GetHTTPMethod: TMVCHTTPMethodType;
    function GetHTTPMethodAsString: string;
    function GetParamAsInteger(const AParamName: string): Integer;
    function GetParamAsInt64(const AParamName: string): Int64;
    function GetFiles: TAbstractWebRequestFiles;
    function GetParamNames: TArray<string>;
    function GetParamsMulti(const AParamName: string): TArray<string>;
  protected
    { protected declarations }
  public
    constructor Create(const AWebRequest: TWebRequest; const ASerializers: TDictionary<string, IMVCSerializer>);
    destructor Destroy; override;

    function ClientIp: string;
    function ClientPrefer(const AMediaType: string): Boolean;
    function ClientPreferHTML: Boolean;

    function SegmentParam(const AParamName: string; out AValue: string): Boolean;
    function SegmentParamsCount: Integer;
    function ThereIsRequestBody: Boolean;

    procedure EnsureQueryParamExists(const AName: string);
    function QueryStringParam(const AName: string): string;
    function QueryStringParamExists(const AName: string): Boolean;
    function QueryStringParams: TStrings;

    function Accept: string;
    function ContentParam(const AName: string): string;
    function Cookie(const AName: string): string;
    function Body: string;
    function BodyAs<T: class, constructor>: T;
    function BodyAsListOf<T: class, constructor>: TObjectList<T>;
    procedure BodyFor<T: class, constructor>(const AObject: T);
    procedure BodyForListOf<T: class, constructor>(const AObjectList: TObjectList<T>);

    property RawWebRequest: TWebRequest read FWebRequest;
    property ContentType: string read FContentType;
    property Charset: string read FCharset;
    property Headers[const AHeaderName: string]: string read GetHeader;
    property PathInfo: string read GetPathInfo;
    property ParamsTable: TMVCRequestParamsTable read FParamsTable write FParamsTable;
    property ParamNames: TArray<string> read GetParamNames;
    property Params[const AParamName: string]: string read GetParams;
    property ParamsMulti[const AParamName: string]: TArray<string> read GetParamsMulti;
    property ParamsAsInteger[const AParamName: string]: Integer read GetParamAsInteger;
    property ParamsAsInt64[const AParamName: string]: Int64 read GetParamAsInt64;
    property IsAjax: Boolean read GetIsAjax;
    property HTTPMethod: TMVCHTTPMethodType read GetHTTPMethod;
    property HTTPMethodAsString: string read GetHTTPMethodAsString;
    property Files: TAbstractWebRequestFiles read GetFiles;
  end;

  {$IFDEF WEBAPACHEHTTP}

  TMVCApacheWebRequest = class(TMVCWebRequest)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  {$ENDIF}

  TMVCISAPIWebRequest = class(TMVCWebRequest)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  TMVCIndyWebRequest = class(TMVCWebRequest)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  TMVCWebResponse = class
  private
    FWebResponse: TWebResponse;
    FFlushOnDestroy: Boolean;
    function GetCustomHeaders: TStrings;
    function GetReasonString: string;
    function GetStatusCode: Integer;
    function GetCookies: TCookieCollection;
    function GetContentType: string;
    function GetLocation: string;
    function GetContent: string;
    procedure SetReasonString(const AValue: string);
    procedure SetStatusCode(const AValue: Integer);
    procedure SetContentType(const AValue: string);
    procedure SetLocation(const AValue: string);
    procedure SetContent(const AValue: string);
  protected
    { protected declarations }
  public
    constructor Create(const AWebResponse: TWebResponse);
    destructor Destroy; override;

    procedure Flush;
    procedure SendHeaders;
    procedure SetCustomHeader(const AName, AValue: string);
    procedure SetContentStream(const AStream: TStream; const AContentType: string);

    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    property ReasonString: string read GetReasonString write SetReasonString;
    property ContentType: string read GetContentType write SetContentType;
    property CustomHeaders: TStrings read GetCustomHeaders;
    property Cookies: TCookieCollection read GetCookies;
    property Location: string read GetLocation write SetLocation;
    property RawWebResponse: TWebResponse read FWebResponse;
    property Content: string read GetContent write SetContent;
    property FlushOnDestroy: Boolean read FFlushOnDestroy write FFlushOnDestroy;
  end;

  TUser = class
  private
    FUserName: string;
    FRoles: TList<string>;
    FLoggedSince: TDateTime;
    FRealm: string;
    procedure SetLoggedSince(const AValue: TDateTime);
  protected
    { protected declarations }
  public
    constructor Create;
    destructor Destroy; override;

    function IsValid: Boolean;
    procedure Clear;

    procedure SaveToSession(const AWebSession: TWebSession);
    function LoadFromSession(const AWebSession: TWebSession): Boolean;

    property UserName: string read FUserName write FUserName;
    property Roles: TList<string> read FRoles;
    property LoggedSince: TDateTime read FLoggedSince write SetLoggedSince;
    property Realm: string read FRealm write FRealm;
  end;

  TWebContext = class
  private
    FRequest: TMVCWebRequest;
    FResponse: TMVCWebResponse;
    FConfig: TMVCConfig;
    FSerializers: TDictionary<string, IMVCSerializer>;
    FIsSessionStarted: Boolean;
    FSessionMustBeClose: Boolean;
    FLoggedUser: TUser;
    FData: TDictionary<string, string>;
    FWebSession: TWebSession;
    function GetWebSession: TWebSession;
    function GetLoggedUser: TUser;
    function GetParamsTable: TMVCRequestParamsTable;
    procedure SetParamsTable(const AValue: TMVCRequestParamsTable);
  protected
    procedure Flush; virtual;
    procedure BindToSession(const ASessionId: string);
    function SendSessionCookie(const AContext: TWebContext): string;
    function AddSessionToTheSessionList(const ASessionType, ASessionId: string; const ASessionTimeout: Integer): TWebSession;
  public
    constructor Create(const ARequest: TWebRequest; const AResponse: TWebResponse; const AConfig: TMVCConfig; const ASerializers: TDictionary<string, IMVCSerializer>);
    destructor Destroy; override;

    procedure SessionStart; virtual;
    procedure SessionStop(const ARaiseExceptionIfExpired: Boolean = True); virtual;

    function SessionStarted: Boolean;
    function SessionId: string;
    function IsSessionStarted: Boolean;
    function SessionMustBeClose: Boolean;

    property LoggedUser: TUser read GetLoggedUser;
    property Request: TMVCWebRequest read FRequest;
    property Response: TMVCWebResponse read FResponse;
    property Session: TWebSession read GetWebSession;
    property Config: TMVCConfig read FConfig;
    property Data: TDictionary<string, string> read FData;
    property ParamsTable: TMVCRequestParamsTable read GetParamsTable write SetParamsTable;
  end;

  TMVCEngine = class;

  TMVCBase = class
  private
    FEngine: TMVCEngine;
    FApplicationSession: TWebApplicationSession;
    function GetEngine: TMVCEngine;
    function GetConfig: TMVCConfig;
    function GetApplicationSession: TWebApplicationSession;
    procedure SetApplicationSession(const AValue: TWebApplicationSession);
    procedure SetEngine(const AValue: TMVCEngine);
  protected
    class function GetApplicationFileName: string; static;
    class function GetApplicationFileNamePath: string; static;
  public
    property Engine: TMVCEngine read GetEngine write SetEngine;
    property Config: TMVCConfig read GetConfig;
    property ApplicationSession: TWebApplicationSession read GetApplicationSession write SetApplicationSession;
  end;

  TMVCStompMessage = class;
  TMVCErrorResponse = class;

  TMVCController = class(TMVCBase)
  private
    FContext: TWebContext;
    FContentCharset: string;
    FResponseStream: TStringBuilder;
    FViewModel: TMVCViewDataObject;
    FViewDataSets: TObjectDictionary<string, TDataSet>;
    function GetContext: TWebContext;
    function GetSession: TWebSession;
    function GetContentType: string;
    function GetStatusCode: Integer;
    procedure SetContentType(const AValue: string);
    procedure SetStatusCode(const AValue: Integer);
  protected const
    CLIENTID_KEY = '__clientid';
  protected
    procedure MVCControllerAfterCreate; virtual;
    procedure MVCControllerBeforeDestroy; virtual;

    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string; var AHandled: Boolean); virtual;
    procedure OnAfterAction(AContext: TWebContext; const AActionName: string); virtual;

    function GetClientId: string;
    function GetCurrentWebModule: TWebModule;
    function GetViewModel: TMVCViewDataObject;
    function GetViewDataSets: TObjectDictionary<string, TDataSet>;
    function GetRenderedView(const AViewNames: TArray<string>): string; virtual;

    /// <summary>
    /// Load mustache view located in TMVCConfigKey.ViewsPath
    /// returns the rendered views and generates output using
    /// models pushed using Push* methods
    /// </summary>
    function LoadView(const AViewNames: TArray<string>): string; virtual;

    /// <summary>
    /// Load a view fragment in the output render stream. The view fragment is appended to the
    /// ResponseStream verbatim. No processing happens.
    /// Useful when used with cache.
    /// It is equivalent to <code>ResponseStream.Append(AViewFragment);</code>
    /// </summary>
    procedure LoadViewFragment(const AViewFragment: string);

    function ResponseStream: TStringBuilder;
    function SessionAs<T: TWebSession>: T;

    procedure SendStream(const AStream: TStream; const AOwns: Boolean = True; const ARewind: Boolean = False); virtual;
    procedure SendFile(const AFileName: string); virtual;
    procedure RenderResponseStream; virtual;

    procedure RaiseSessionExpired; virtual;
    procedure Redirect(const AUrl: string); virtual;
    procedure ResponseStatus(const AStatusCode: Integer; const AReasonString: string = ''); virtual;

    function Serializer: IMVCSerializer; overload;
    function Serializer(const AContentType: string): IMVCSerializer; overload;

    procedure Render(const AContent: string); overload;
    procedure Render(const AObject: TObject); overload;
    procedure Render(const AObject: TObject; const AOwns: Boolean); overload;
    procedure Render(const AObject: TObject; const AOwns: Boolean; const AType: TMVCSerializationType); overload;
    procedure Render<T: class>(const ACollection: TObjectList<T>); overload;
    procedure Render<T: class>(const ACollection: TObjectList<T>; const AOwns: Boolean); overload;
    procedure Render<T: class>(const ACollection: TObjectList<T>; const AOwns: Boolean; const AType: TMVCSerializationType); overload;
    procedure Render(const ACollection: IMVCList); overload;
    procedure Render(const ACollection: IMVCList; const AType: TMVCSerializationType); overload;
    procedure Render(const ADataSet: TDataSet); overload;
    procedure Render(const ADataSet: TDataSet; const AOwns: boolean); overload;
    procedure Render(const ADataSet: TDataSet; const AOwns: boolean; const ASerializationType: TMVCDatasetSerializationType); overload;
    procedure Render(const ADataSet: TDataSet; const AOwns: boolean; const AIgnoredFields: TMVCIgnoredList; const ASerializationType: TMVCDatasetSerializationType); overload;
    procedure Render(const ADataSet: TDataSet; const AOwns: boolean; const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase; const ASerializationType: TMVCDatasetSerializationType); overload;
    procedure Render(const ATextWriter: TTextWriter; const AOwns: Boolean = True); overload;
    procedure Render(const AStream: TStream; const AOwns: Boolean = True); overload;
    procedure Render(const AErrorCode: Integer; const AErrorMessage: string; const AErrorClassName: string = ''); overload;
    procedure Render(const AException: Exception; AExceptionItems: TList<string> = nil; const AOwns: Boolean = True); overload;
    procedure Render(const AError: TMVCErrorResponse; const AOwns: Boolean = True); overload;

    property Context: TWebContext read GetContext write FContext;
    property Session: TWebSession read GetSession;
    property ContentType: string read GetContentType write SetContentType;
    property ContentCharset: string read FContentCharset write FContentCharset;
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    property ViewModel: TMVCViewDataObject read GetViewModel;
    property ViewDataSets: TObjectDictionary<string, TDataSet> read GetViewDataSets;
  public
    constructor Create;
    destructor Destroy; override;

    // procedure PushToView(const AModelName: string; const AModel: string);
    procedure PushObjectToView(const AModelName: string; const AModel: TObject);
    procedure PushDataSetToView(const AModelName: string; const ADataSet: TDataSet);
  end;

  TMVCControllerClazz = class of TMVCController;

  TMVCControllerCreateAction = reference to function: TMVCController;

  TMVCControllerDelegate = class
  private
    FClazz: TMVCControllerClazz;
    FCreateAction: TMVCControllerCreateAction;
  protected
    { protected declarations }
  public
    constructor Create(const AClazz: TMVCControllerClazz; const ACreateAction: TMVCControllerCreateAction);

    property Clazz: TMVCControllerClazz read FClazz;
    property CreateAction: TMVCControllerCreateAction read FCreateAction;
  end;

  TMVCStaticContents = class(TMVCController)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    class procedure SendFile(const AFileName, AMediaType: string; AContext: TWebContext);
    class function IsStaticFile(const AViewPath, AWebRequestPath: string; out ARealFileName: string): Boolean;
    class function IsScriptableFile(const AStaticFileName: string; const AConfig: TMVCConfig): Boolean;
  end;

  /// <summary>
  /// Basis Interface for DMVC Middleware.
  /// </summary>
  IMVCMiddleware = interface
    ['{3278183A-124A-4214-AB4E-94CA4C22450D}']
    /// <summary>
    /// Procedure is called before the MVCEngine routes the request to a specific controller/method.
    /// </summary>
    /// <param name="AContext">Webcontext which contains the complete request and response of the actual call.</param>
    /// <param name="AHandled">If set to True the Request would finished. Response must be set by the implementor. Default value is False.</param>
    procedure OnBeforeRouting(
      AContext: TWebContext;
      var AHandled: Boolean
      );
    /// <summary>
    /// Procedure is called before the specific controller method is called.
    /// </summary>
    /// <param name="AContext">Webcontext which contains the complete request and response of the actual call.</param>
    /// <param name="AControllerQualifiedClassName">Qualified classname of the matching controller.</param>
    /// <param name="AActionName">Method name of the matching controller method.</param>
    /// <param name="AHandled">If set to True the Request would finished. Response must be set by the implementor. Default value is False.</param>
    procedure OnBeforeControllerAction(
      AContext: TWebContext;
      const AControllerQualifiedClassName: string;
      const AActionName: string;
      var AHandled: Boolean
      );
    /// <summary>
    /// Procedure is called after the specific controller method was called.
    /// It is still possible to cancel or to completly modifiy the request.
    /// </summary>
    /// <param name="AContext">Webcontext which contains the complete request and response of the actual call.</param>
    /// <param name="AActionName">Method name of the matching controller method.</param>
    /// <param name="AHandled">If set to True the Request would finished. Response must be set by the implementor. Default value is False.</param>
    procedure OnAfterControllerAction(
      AContext: TWebContext;
      const AActionName: string;
      const AHandled: Boolean
      );
  end;

  TMVCEngine = class(TComponent)
  private const
    ALLOWED_TYPED_ACTION_PARAMETERS_TYPES = 'Integer, Int64, Single, Double, Extended, Boolean, TDate, TTime, TDateTime and String';
  private
    FViewEngineClass: TMVCViewEngineClass;
    FWebModule: TWebModule;
    FConfig: TMVCConfig;
    FSerializers: TDictionary<string, IMVCSerializer>;
    FMiddlewares: TList<IMVCMiddleware>;
    FControllers: TObjectList<TMVCControllerDelegate>;
    FMediaTypes: TDictionary<string, string>;
    FApplicationSession: TWebApplicationSession;
    FSavedOnBeforeDispatch: THTTPMethodEvent;
    function IsStaticFileRequest(const ARequest: TWebRequest; out AFileName: string): Boolean;
    function SendStaticFileIfPresent(const AContext: TWebContext; const AFileName: string): Boolean;
    procedure FillActualParamsForAction(
      const AContext: TWebContext;
      const AActionFormalParams: TArray<TRttiParameter>;
      const AActionName: string;
      var AActualParams: TArray<TValue>);
    procedure RegisterDefaultsSerializers;
    function GetViewEngineClass: TMVCViewEngineClass;
  protected
    procedure ConfigDefaultValues; virtual;
    procedure LoadSystemControllers; virtual;
    procedure FixUpWebModule;

    procedure ExecuteBeforeRoutingMiddleware(const AContext: TWebContext; var AHandled: Boolean);
    procedure ExecuteBeforeControllerActionMiddleware(
      const AContext: TWebContext;
      const AControllerQualifiedClassName: string;
      const AActionName: string;
      var AHandled: Boolean);
    procedure ExecuteAfterControllerActionMiddleware(const AContext: TWebContext; const AActionName: string; const AHandled: Boolean);

    procedure DefineDefaultReponseHeaders(const AContext: TWebContext);
    procedure OnBeforeDispatch(ASender: TObject; ARequest: TWebRequest; AResponse: TWebResponse; var AHandled: Boolean); virtual;
    procedure ResponseErrorPage(const AException: Exception; const ARequest: TWebRequest; const AResponse: TWebResponse); virtual;
    function ExecuteAction(const ASender: TObject; const ARequest: TWebRequest; const AResponse: TWebResponse): Boolean; virtual;
  public
    class function GetCurrentSession(const ASessionTimeout: Integer; const ASessionId: string; const ARaiseExceptionIfExpired: Boolean = True): TWebSession; static;
    class function ExtractSessionIdFromWebRequest(const AWebRequest: TWebRequest): string; static;
    class function SendSessionCookie(const AContext: TWebContext): string; overload; static;
    class function SendSessionCookie(const AContext: TWebContext; const ASessionId: string): string; overload; static;
    class procedure ClearSessionCookiesAlreadySet(const ACookies: TCookieCollection); static;
  public
    constructor Create(const AWebModule: TWebModule; const AConfigAction: TProc<TMVCConfig> = nil; const ACustomLogger: ILogWriter = nil); reintroduce;
    destructor Destroy; override;

    function GetSessionBySessionId(const ASessionId: string): TWebSession;

    function AddSerializer(const AContentType: string; const ASerializer: IMVCSerializer): TMVCEngine;
    function AddMiddleware(const AMiddleware: IMVCMiddleware): TMVCEngine;
    function AddController(const AControllerClazz: TMVCControllerClazz): TMVCEngine; overload;
    function AddController(const AControllerClazz: TMVCControllerClazz; const ACreateAction: TMVCControllerCreateAction): TMVCEngine; overload;
    function SetViewEngine(const AViewEngineClass: TMVCViewEngineClass): TMVCEngine;

    procedure HTTP404(const AContext: TWebContext);
    procedure HTTP500(const AContext: TWebContext; const AReasonString: string = '');

    property ViewEngineClass: TMVCViewEngineClass read GetViewEngineClass;
    property WebModule: TWebModule read FWebModule;
    property Config: TMVCConfig read FConfig;
    property Serializers: TDictionary<string, IMVCSerializer> read FSerializers;
    property Middlewares: TList<IMVCMiddleware> read FMiddlewares;
    property Controllers: TObjectList<TMVCControllerDelegate> read FControllers;
    property ApplicationSession: TWebApplicationSession read FApplicationSession write FApplicationSession;
  end;

  TMVCStompMessage = class
  private
    FSmTimestamp: TDateTime;
    FSmQueue: string;
    FSmUsername: string;
    FSmTopic: string;
    FSmMessage: string;
  protected
    { protected declarations }
  public
    [MVCNameAs('message')]
    property SmMessage: string read FSmMessage write FSmMessage;

    [MVCNameAs('_queue')]
    property SmQueue: string read FSmQueue write FSmQueue;

    [MVCNameAs('_topic')]
    property SmTopic: string read FSmTopic write FSmTopic;

    [MVCNameAs('_username')]
    property SmUsername: string read FSmUsername write FSmUsername;

    [MVCNameAs('_timestamp')]
    property SmTimestamp: TDateTime read FSmTimestamp write FSmTimestamp;
  end;

  [MVCNameCase(ncLowerCase)]
  TMVCErrorResponseItem = class
  private
    FMessage: string;
  protected
    { protected declarations }
  public
    property message: string read FMessage write FMessage;
  end;

  [MVCNameCase(ncLowerCase)]
  TMVCErrorResponse = class
  private
    FStatusCode: Integer;
    FReasonString: string;
    FMessage: string;
    FClassname: string;
    FItems: TObjectList<TMVCErrorResponseItem>;
  protected
    { protected declarations }
  public
    constructor Create; overload;
    constructor Create(AStatusCode: Integer; AReasonString: string; AMessage: string); overload;
    destructor Destroy; override;

    property StatusCode: Integer read FStatusCode write FStatusCode;
    property ReasonString: string read FReasonString write fReasonString;
    property message: string read FMessage write FMessage;
    property Classname: string read FClassname write FClassname;

    [MVCListOf(TMVCErrorResponseItem)]
    property Items: TObjectList<TMVCErrorResponseItem> read FItems;
  end;

  TMVCBaseViewEngine = class(TMVCBase)
  private
    FViewName: string;
    FWebContext: TWebContext;
    FViewModel: TMVCViewDataObject;
    FViewDataSets: TObjectDictionary<string, TDataSet>;
    FContentType: string;
    FOutput: string;
  protected
    function GetRealFileName(const AViewName: string): string; virtual;
    function IsCompiledVersionUpToDate(const AFileName, ACompiledFileName: string): Boolean; virtual; abstract;
    procedure SetOutput(const AOutput: string);
  public
    constructor Create(
      const AEngine: TMVCEngine;
      const AWebContext: TWebContext;
      const AViewModel: TMVCViewDataObject;
      const AViewDataSets: TObjectDictionary<string, TDataSet>;
      const AContentType: string); virtual;
    destructor Destroy; override;

    procedure Execute(const ViewName: String); virtual; abstract;

    property ViewName: string read FViewName;
    property WebContext: TWebContext read FWebContext;
    property ViewModel: TMVCViewDataObject read FViewModel;
    property ViewDataSets: TObjectDictionary<string, TDataSet> read FViewDataSets;
    property ContentType: string read FContentType;
    property Output: string read FOutput;
  end;

function IsShuttingDown: Boolean;
procedure EnterInShutdownState;

implementation

uses
  MVCFramework.Router,
  MVCFramework.SysControllers,
  MVCFramework.Serializer.JsonDataObjects;

var
  _IsShuttingDown: Int64 = 0;

function IsShuttingDown: Boolean;
begin
  Result := TInterlocked.Read(_IsShuttingDown) = 1
end;

procedure EnterInShutdownState;
begin
  TInterlocked.Add(_IsShuttingDown, 1);
end;

{ MVCHTTPMethodsAttribute }

constructor MVCHTTPMethodsAttribute.Create(const AMVCHTTPMethods: TMVCHTTPMethods);
begin
  inherited Create;
  FMVCHTTPMethods := AMVCHTTPMethods;
end;

function MVCHTTPMethodsAttribute.GetMVCHTTPMethodsAsString: string;
var
  I: TMVCHTTPMethodType;
begin
  Result := '';

  for I := low(TMVCHTTPMethodType) to high(TMVCHTTPMethodType) do
    if I in FMVCHTTPMethods then
      Result := Result + ',' + GetEnumName(TypeInfo(TMVCHTTPMethodType), Ord(I));

  if Result <> EmptyStr then
    Result := Result.Remove(0, 1)
  else
    Result := 'any';
end;

{ MVCStringAttribute }

constructor MVCStringAttribute.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

{ MVCProducesAttribute }

constructor MVCProducesAttribute.Create(const AValue, AEncoding: string);
begin
  Create(AValue);
  FEncoding := AEncoding;
end;

constructor MVCProducesAttribute.Create(const AValue: string);
begin
  inherited Create(AValue);
  FEncoding := TMVCCharset.UTF_8;
end;

{ MVCPathAttribute }

constructor MVCPathAttribute.Create(const APath: string);
begin
  inherited Create;
  FPath := APath;
end;

{ TMVCWebRequest }

function TMVCWebRequest.Accept: string;
begin
  Result := FWebRequest.Accept;
end;

function TMVCWebRequest.Body: string;
var
  Encoding: TEncoding;
  Buffer: TArray<Byte>;
  I: Integer;

  {$IFNDEF BERLINORBETTER}

  BufferOut: TArray<Byte>;

  {$ENDIF}

begin
  { TODO -oEzequiel -cRefactoring : Refactoring the method TMVCWebRequest.Body }
  if (FBody = EmptyStr) then
  begin
    Encoding := nil;
    try

      {$IFDEF BERLINORBETTER}

      if (FCharset = EmptyStr) then
      begin
        SetLength(Buffer, 10);
        for I := 0 to 9 do
          Buffer[I] := FWebRequest.RawContent[I];
        TEncoding.GetBufferEncoding(Buffer, Encoding, TEncoding.Default);
        SetLength(Buffer, 0);
      end
      else
        Encoding := TEncoding.GetEncoding(FCharset);
      FBody := Encoding.GetString(FWebRequest.RawContent);

      {$ELSE}

      SetLength(Buffer, FWebRequest.ContentLength);
      FWebRequest.ReadClient(Buffer[0], FWebRequest.ContentLength);
      if (FCharset = EmptyStr) then
      begin
        SetLength(BufferOut, 10);
        for I := 0 to 9 do
        begin
          BufferOut[I] := Buffer[I];
        end;
        TEncoding.GetBufferEncoding(BufferOut, Encoding, TEncoding.Default);
        SetLength(BufferOut, 0);
      end
      else
        Encoding := TEncoding.GetEncoding(FCharset);
      FBody := Encoding.GetString(Buffer);

      {$ENDIF}

    finally
      if Assigned(Encoding) then
        Encoding.Free;
    end;
  end;
  Result := FBody;
end;

function TMVCWebRequest.BodyAs<T>: T;
var
  Obj: TObject;
begin
  Result := nil;
  if FSerializers.ContainsKey(ContentType) then
  begin
    Obj := TMVCSerializerHelpful.CreateObject(TClass(T).QualifiedClassName);
    FSerializers.Items[ContentType].DeserializeObject(Body, Obj);
    Result := Obj as T;
  end
  else
    raise EMVCException.CreateFmt('Body ContentType %s not supported', [ContentType]);
end;

function TMVCWebRequest.BodyAsListOf<T>: TObjectList<T>;
var
  List: TObjectList<T>;
begin
  Result := nil;
  if FSerializers.ContainsKey(ContentType) then
  begin
    List := TObjectList<T>.Create;
    FSerializers.Items[ContentType].DeserializeCollection(Body, List, T);
    Result := List;
  end
  else
    raise EMVCException.CreateFmt('Body ContentType %s not supported', [ContentType]);
end;

procedure TMVCWebRequest.BodyFor<T>(const AObject: T);
begin
  if Assigned(AObject) then
    if FSerializers.ContainsKey(ContentType) then
      FSerializers.Items[ContentType].DeserializeObject(Body, AObject)
    else
      raise EMVCException.CreateFmt('Body ContentType %s not supported', [ContentType]);
end;

procedure TMVCWebRequest.BodyForListOf<T>(const AObjectList: TObjectList<T>);
begin
  if Assigned(AObjectList) then
    if FSerializers.ContainsKey(ContentType) then
      FSerializers.Items[ContentType].DeserializeCollection(Body, AObjectList, T)
    else
      raise EMVCException.CreateFmt('Body ContentType %s not supported', [ContentType]);
end;

function TMVCWebRequest.ClientIp: string;
var
  S: string;
begin
  Result := EmptyStr;

  if FWebRequest.GetFieldByName('HTTP_CLIENT_IP') <> EmptyStr then
    Exit(FWebRequest.GetFieldByName('HTTP_CLIENT_IP'));

  for S in string(FWebRequest.GetFieldByName('HTTP_X_FORWARDED_FOR')).Split([',']) do
    if not S.Trim.IsEmpty then
      Exit(S.Trim);

  if FWebRequest.GetFieldByName('HTTP_X_FORWARDED') <> EmptyStr then
    Exit(FWebRequest.GetFieldByName('HTTP_X_FORWARDED'));

  if FWebRequest.GetFieldByName('HTTP_X_CLUSTER_CLIENT_IP') <> EmptyStr then
    Exit(FWebRequest.GetFieldByName('HTTP_X_CLUSTER_CLIENT_IP'));

  if FWebRequest.GetFieldByName('HTTP_FORWARDED_FOR') <> EmptyStr then
    Exit(FWebRequest.GetFieldByName('HTTP_FORWARDED_FOR'));

  if FWebRequest.GetFieldByName('HTTP_FORWARDED') <> EmptyStr then
    Exit(FWebRequest.GetFieldByName('HTTP_FORWARDED'));

  if FWebRequest.GetFieldByName('REMOTE_ADDR') <> EmptyStr then
    Exit(FWebRequest.GetFieldByName('REMOTE_ADDR'));

  if FWebRequest.RemoteIP <> EmptyStr then
    Exit(FWebRequest.RemoteIP);

  if FWebRequest.RemoteAddr <> EmptyStr then
    Exit(FWebRequest.RemoteAddr);

  if FWebRequest.RemoteHost <> EmptyStr then
    Exit(FWebRequest.RemoteHost);

  if FWebRequest.RemoteAddr <> EmptyStr then
    Exit(FWebRequest.RemoteAddr);

  if FWebRequest.RemoteIP <> EmptyStr then
    Exit(FWebRequest.RemoteIP);

  if FWebRequest.RemoteHost <> EmptyStr then
    Exit(FWebRequest.RemoteHost);
end;

function TMVCWebRequest.ClientPrefer(const AMediaType: string): Boolean;
begin
  Result := AnsiPos(AMediaType, LowerCase(RawWebRequest.Accept)) = 1;
end;

function TMVCWebRequest.ClientPreferHTML: Boolean;
begin
  Result := ClientPrefer(TMVCMediaType.TEXT_HTML);
end;

function TMVCWebRequest.ContentParam(const AName: string): string;
begin
  Result := FWebRequest.ContentFields.Values[AName];
end;

function TMVCWebRequest.Cookie(const AName: string): string;
begin
  Result := FWebRequest.CookieFields.Values[AName];
end;

constructor TMVCWebRequest.Create(const AWebRequest: TWebRequest; const ASerializers: TDictionary<string, IMVCSerializer>);
begin
  inherited Create;
  FBody := EmptyStr;
  FContentType := TMVCConstants.DEFAULT_CONTENT_TYPE;
  FCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET;
  FWebRequest := AWebRequest;
  FSerializers := ASerializers;
  FParamsTable := nil;
  DefineContentTypeAndCharset;
end;

procedure TMVCWebRequest.DefineContentTypeAndCharset;
var
  RequestContentType: string;
  ContentTypeValues: TArray<string>;
begin
  RequestContentType := FWebRequest.GetFieldByName('Content-Type');
  if not RequestContentType.IsEmpty then
  begin
    ContentTypeValues := RequestContentType.Split([';']);
    FContentType := Trim(ContentTypeValues[0]);
    if Length(ContentTypeValues) > 1 then
      if ContentTypeValues[1].Trim.StartsWith('charset', True) then
        FCharset := ContentTypeValues[1].Trim.Split(['='])[1].Trim;
  end;
end;

destructor TMVCWebRequest.Destroy;
begin
  inherited Destroy;
end;

procedure TMVCWebRequest.EnsureQueryParamExists(const AName: string);
begin
  if GetParams(AName).IsEmpty then
    raise EMVCException.CreateFmt('Parameter "%s" required', [AName]);
end;

function TMVCWebRequest.GetFiles: TAbstractWebRequestFiles;
begin
  Result := FWebRequest.Files;
end;

function TMVCWebRequest.GetHeader(const AName: string): string;
begin
  Result := FWebRequest.GetFieldByName(AName);
end;

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
  Result := LowerCase(FWebRequest.GetFieldByName('X-Requested-With')) = 'xmlhttprequest';
end;

function TMVCWebRequest.GetParamAsInt64(const AParamName: string): Int64;
begin
  Result := StrToInt64(GetParams(AParamName));
end;

function TMVCWebRequest.GetParamAsInteger(const AParamName: string): Integer;
begin
  Result := StrToInt(GetParams(AParamName));
end;

function TMVCWebRequest.GetParamNames: TArray<string>;
var
  I: Integer;
  Names: TList<string>;
  N: string;
begin
  Names := TList<string>.Create;
  try
    if Assigned(FParamsTable) and (Length(FParamsTable.Keys.ToArray) > 0) then
      for N in FParamsTable.Keys.ToArray do
        Names.Add(N);

    if (FWebRequest.QueryFields.Count > 0) then
      for I := 0 to FWebRequest.QueryFields.Count - 1 do
        Names.Add(FWebRequest.QueryFields.Names[I]);

    if (FWebRequest.ContentFields.Count > 0) then
      for I := 0 to FWebRequest.ContentFields.Count - 1 do
        Names.Add(FWebRequest.ContentFields.Names[I]);

    if (FWebRequest.CookieFields.Count > 0) then
      for I := 0 to FWebRequest.CookieFields.Count - 1 do
        Names.Add(FWebRequest.CookieFields.Names[I]);

    Result := Names.ToArray;
  finally
    Names.Free;
  end;
end;

function TMVCWebRequest.GetParams(const AParamName: string): string;
begin
  if (not Assigned(FParamsTable)) or (not FParamsTable.TryGetValue(AParamName, Result)) then
  begin
    Result := FWebRequest.ContentFields.Values[AParamName];
    if Result.IsEmpty then
      Result := FWebRequest.QueryFields.Values[AParamName];
    if Result.IsEmpty then
      Result := FWebRequest.CookieFields.Values[AParamName];
  end;
end;

function TMVCWebRequest.GetParamsMulti(
  const AParamName: string): TArray<string>;
var
  lList: TList<string>;
  procedure AddParamsToList(const AStrings: TStrings; const AList: TList<string>);
  var
    I: Integer;
  begin
    for I := 0 to AStrings.Count - 1 do
      if SameText(AStrings.Names[i], AParamName) then
        AList.Add(AStrings.ValueFromIndex[i]);
  end;

begin
  lList := TList<string>.Create;
  try
    AddParamsToList(FWebRequest.ContentFields, lList);
    AddParamsToList(FWebRequest.QueryFields, lList);
    AddParamsToList(FWebRequest.CookieFields, lList);
    Result := lList.ToArray;
  finally
    lList.Free;
  end;
end;

function TMVCWebRequest.GetPathInfo: string;
begin
  Result := FWebRequest.PathInfo;
end;

function TMVCWebRequest.QueryStringParam(const AName: string): string;
begin
  Result := FWebRequest.QueryFields.Values[AName];
end;

function TMVCWebRequest.QueryStringParamExists(const AName: string): Boolean;
begin
  Result := QueryStringParam(AName) <> EmptyStr;
end;

function TMVCWebRequest.QueryStringParams: TStrings;
begin
  Result := FWebRequest.QueryFields;
end;

function TMVCWebRequest.SegmentParam(const AParamName: string; out AValue: string): Boolean;
begin
  Result := False;
  if Assigned(FParamsTable) then
    Result := FParamsTable.TryGetValue(AParamName, AValue);
end;

function TMVCWebRequest.SegmentParamsCount: Integer;
begin
  Result := 0;
  if Assigned(FParamsTable) then
    Result := FParamsTable.Count;
end;

function TMVCWebRequest.ThereIsRequestBody: Boolean;
begin
  Result := (FWebRequest.Content <> EmptyStr);
end;

{ TMVCWebResponse }

constructor TMVCWebResponse.Create(const AWebResponse: TWebResponse);
begin
  inherited Create;
  FWebResponse := AWebResponse;
  FFlushOnDestroy := True;
end;

destructor TMVCWebResponse.Destroy;
begin
  if FFlushOnDestroy then
    Flush;
  inherited Destroy;
end;

procedure TMVCWebResponse.Flush;
begin
  try
    FWebResponse.SendResponse;
  except
    { TODO -oEzequiel -cException : Check why this exception is being eaten }
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
  Result := FWebResponse.Cookies;
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
  FWebResponse.SendResponse;
end;

procedure TMVCWebResponse.SetContent(const AValue: string);
begin
  FWebResponse.Content := AValue;
end;

procedure TMVCWebResponse.SetContentStream(const AStream: TStream; const AContentType: string);
begin
  FWebResponse.ContentType := AContentType;
  FWebResponse.ContentStream := AStream;
end;

procedure TMVCWebResponse.SetContentType(const AValue: string);
begin
  FWebResponse.ContentType := AValue;
end;

procedure TMVCWebResponse.SetCustomHeader(const AName, AValue: string);
begin
  FWebResponse.SetCustomHeader(AName, AValue);
end;

procedure TMVCWebResponse.SetLocation(const AValue: string);
begin
  CustomHeaders.Values['location'] := AValue;
end;

procedure TMVCWebResponse.SetReasonString(const AValue: string);
begin
  FWebResponse.ReasonString := AValue;
end;

procedure TMVCWebResponse.SetStatusCode(const AValue: Integer);
begin
  FWebResponse.StatusCode := AValue;
end;

{ TUser }

procedure TUser.Clear;
begin
  FUserName := EmptyStr;
  FLoggedSince := 0;
  FRealm := EmptyStr;
  FRoles.Clear;
end;

constructor TUser.Create;
begin
  inherited Create;
  FRoles := TList<string>.Create;
end;

destructor TUser.Destroy;
begin
  FRoles.Free;
  inherited Destroy;
end;

function TUser.IsValid: Boolean;
begin
  Result := (not UserName.IsEmpty) and (LoggedSince > 0);
end;

function TUser.LoadFromSession(const AWebSession: TWebSession): Boolean;
var
  SerObj: string;
  Pieces: TArray<string>;
  I: Integer;
begin
  if not Assigned(AWebSession) then
    Exit(False);
  SerObj := AWebSession[TMVCConstants.CURRENT_USER_SESSION_KEY];
  Result := not SerObj.IsEmpty;
  if Result then
  begin
    Clear;
    Pieces := SerObj.Split(['$$'], TStringSplitOptions.None);
    UserName := Pieces[0];
    LoggedSince := ISOTimeStampToDateTime(Pieces[1]);
    Realm := Pieces[2];
    Roles.Clear;
    for I := 2 to Length(Pieces) - 1 do
      Roles.Add(Pieces[I]);
  end;
end;

procedure TUser.SaveToSession(const AWebSession: TWebSession);
var
  LRoles: string;
begin
  if (FRoles.Count > 0) then
    LRoles := string.Join('$$', FRoles.ToArray)
  else
    LRoles := '';
  AWebSession[TMVCConstants.CURRENT_USER_SESSION_KEY] := FUserName + '$$' + DateTimeToISOTimeStamp(FLoggedSince) + '$$' + FRealm + '$$' + LRoles;
end;

procedure TUser.SetLoggedSince(const AValue: TDateTime);
begin
  if (FLoggedSince = 0) then
    FLoggedSince := AValue
  else
    raise EMVCException.Create('TUser.LoggedSince already set.');
end;

{ TWebContext }

function TWebContext.AddSessionToTheSessionList(const ASessionType, ASessionId: string; const ASessionTimeout: Integer): TWebSession;
var
  Session: TWebSession;
begin
  if (Trim(ASessionType) = EmptyStr) then
    raise EMVCException.Create('Empty Session Type');

  TMonitor.Enter(GlobalSessionList);
  try
    Session := TMVCSessionFactory.GetInstance.CreateNewByType(ASessionType, ASessionId, ASessionTimeout);
    GlobalSessionList.Add(ASessionId, Session);
    Result := Session;
    Session.MarkAsUsed;
  finally
    TMonitor.Exit(GlobalSessionList);
  end;
end;

procedure TWebContext.BindToSession(const ASessionId: string);
begin
  if not Assigned(FWebSession) then
  begin
    FWebSession := TMVCEngine.GetCurrentSession(StrToInt64(FConfig[TMVCConfigKey.SessionTimeout]), ASessionId, False);
    if not Assigned(FWebSession) then
      raise EMVCException.Create('Invalid SessionID');
    FWebSession.MarkAsUsed;
    TMVCEngine.SendSessionCookie(Self, ASessionId);
  end
  else
    raise EMVCException.Create('Session already bounded for this request');
end;

constructor TWebContext.Create(
  const ARequest: TWebRequest;
  const AResponse: TWebResponse;
  const AConfig: TMVCConfig;
  const ASerializers: TDictionary<string, IMVCSerializer>);
begin
  inherited Create;
  FIsSessionStarted := False;
  FSessionMustBeClose := False;
  FWebSession := nil;

  if IsLibrary then
  begin

    {$IFDEF WEBAPACHEHTTP}

    if ARequest is TApacheRequest then
      FRequest := TMVCApacheWebRequest.Create(ARequest, ASerializers)

      {$IFNDEF LINUX}

    else if ARequest is TISAPIRequest then
      FRequest := TMVCISAPIWebRequest.Create(ARequest, ASerializers)

      {$ENDIF}

    else
      raise EMVCException.Create('Unknown request type ' + ARequest.ClassName);

    {$ELSE}

      FRequest := TMVCISAPIWebRequest.Create(ARequest, ASerializers)

    {$ENDIF}

  end
  else
    FRequest := TMVCINDYWebRequest.Create(ARequest, ASerializers);

  FResponse := TMVCWebResponse.Create(AResponse);
  FConfig := AConfig;
  FSerializers := ASerializers;
  FData := TDictionary<string, string>.Create;
  FLoggedUser := nil;
end;

destructor TWebContext.Destroy;
begin
  FResponse.Free;
  FRequest.Free;
  FData.Free;
  if Assigned(FLoggedUser) then
    FLoggedUser.Free;
  inherited Destroy;
end;

procedure TWebContext.Flush;
begin
  FResponse.Flush;
end;

function TWebContext.GetLoggedUser: TUser;
begin
  if not Assigned(FLoggedUser) then
    FLoggedUser := TUser.Create;
  Result := FLoggedUser;
end;

function TWebContext.GetParamsTable: TMVCRequestParamsTable;
begin
  Result := FRequest.ParamsTable;
end;

function TWebContext.GetWebSession: TWebSession;
begin
  if not Assigned(FWebSession) then
  begin
    FWebSession := TMVCEngine.GetCurrentSession(StrToInt64(FConfig[TMVCConfigKey.SessionTimeout]), TMVCEngine.ExtractSessionIdFromWebRequest(FRequest.RawWebRequest), False);
    if not Assigned(FWebSession) then
      SessionStart
    else
      TMVCEngine.SendSessionCookie(Self, FWebSession.SessionId);
  end;
  Result := FWebSession;
  Result.MarkAsUsed;
end;

function TWebContext.IsSessionStarted: Boolean;
begin
  Result := FIsSessionStarted;
end;

function TWebContext.SendSessionCookie(const AContext: TWebContext): string;
begin
  Result := TMVCEngine.SendSessionCookie(Self);
end;

function TWebContext.SessionId: string;
begin
  if Assigned(FWebSession) then
    Exit(FWebSession.SessionId);
  Result := FRequest.Cookie(TMVCConstants.SESSION_TOKEN_NAME);
end;

function TWebContext.SessionMustBeClose: Boolean;
begin
  Result := FSessionMustBeClose;
end;

procedure TWebContext.SessionStart;
var
  Id: string;
begin
  if not Assigned(FWebSession) then
  begin
    Id := TMVCEngine.SendSessionCookie(Self);
    FWebSession := AddSessionToTheSessionList(Config[TMVCConfigKey.SessionType], Id, StrToInt64(Config[TMVCConfigKey.SessionTimeout]));
    FIsSessionStarted := True;
    FSessionMustBeClose := False;
  end;
end;

function TWebContext.SessionStarted: Boolean;
var
  SId: string;
begin
  SId := SessionId;
  if SId.IsEmpty then
    Exit(False);
  TMonitor.Enter(GlobalSessionList);
  try
    Result := GlobalSessionList.ContainsKey(SId);
  finally
    TMonitor.Exit(GlobalSessionList);
  end;
end;

procedure TWebContext.SessionStop(const ARaiseExceptionIfExpired: Boolean);
var
  Cookie: TCookie;
  SId: string;
begin
  FResponse.Cookies.Clear;

  Cookie := FResponse.Cookies.Add;
  Cookie.Name := TMVCConstants.SESSION_TOKEN_NAME;

  Cookie.Value := GUIDToString(TGUID.NewGuid) + 'invalid' + GUIDToString(TGUID.NewGuid);
  Cookie.Expires := EncodeDate(1970, 1, 1);
  Cookie.Path := '/';

  TMonitor.Enter(GlobalSessionList);
  try
    SId := TMVCEngine.ExtractSessionIdFromWebRequest(FRequest.RawWebRequest);
    GlobalSessionList.Remove(SId);
  finally
    TMonitor.Exit(GlobalSessionList);
  end;

  FIsSessionStarted := False;
  FSessionMustBeClose := True;
end;

procedure TWebContext.SetParamsTable(const AValue: TMVCRequestParamsTable);
begin
  FRequest.ParamsTable := AValue;
end;

{ TMVCEngine }

function TMVCEngine.AddController(const AControllerClazz: TMVCControllerClazz): TMVCEngine;
begin
  Result := AddController(AControllerClazz, nil);
end;

function TMVCEngine.AddController(const AControllerClazz: TMVCControllerClazz; const ACreateAction: TMVCControllerCreateAction): TMVCEngine;
begin
  FControllers.Add(TMVCControllerDelegate.Create(AControllerClazz, ACreateAction));
  Result := Self;
end;

function TMVCEngine.AddMiddleware(const AMiddleware: IMVCMiddleware): TMVCEngine;
begin
  FMiddlewares.Add(AMiddleware);
  Result := Self;
end;

function TMVCEngine.AddSerializer(const AContentType: string; const ASerializer: IMVCSerializer): TMVCEngine;
begin
  FSerializers.AddOrSetValue(AContentType, ASerializer);
  Result := Self;
end;

class procedure TMVCEngine.ClearSessionCookiesAlreadySet(const ACookies: TCookieCollection);
var
  I: Integer;
  SessionCookieName: string;
  Cookie: TCookie;
begin
  SessionCookieName := TMVCConstants.SESSION_TOKEN_NAME.ToLower;
  I := 0;
  while true do
  begin
    if I = ACookies.Count then
      Break;
    Cookie := ACookies[I];
    if LowerCase(Cookie.Name) = SessionCookieName then
      ACookies.Delete(I)
    else
      Inc(I);
  end;
end;

procedure TMVCEngine.ConfigDefaultValues;
begin
  Log.Info('ENTER: Config default values', LOGGERPRO_TAG);

  Config[TMVCConfigKey.SessionTimeout] := '30' { 30 minutes };
  Config[TMVCConfigKey.DocumentRoot] := '.\www';
  Config[TMVCConfigKey.FallbackResource] := '';
  Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
  Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
  Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
  Config[TMVCConfigKey.ViewPath] := 'templates';
  Config[TMVCConfigKey.ISAPIPath] := '';
  Config[TMVCConfigKey.StompServer] := 'localhost';
  Config[TMVCConfigKey.StompServerPort] := '61613';
  Config[TMVCConfigKey.StompUsername] := 'guest';
  Config[TMVCConfigKey.StompPassword] := 'guest';
  Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
  Config[TMVCConfigKey.ServerName] := 'DelphiMVCFramework';
  Config[TMVCConfigKey.ExposeServerSignature] := 'true';
  Config[TMVCConfigKey.SessionType] := 'memory';
  Config[TMVCConfigKey.IndexDocument] := 'index.html';

  FMediaTypes.Add('.html', TMVCMediaType.TEXT_HTML);
  FMediaTypes.Add('.htm', TMVCMediaType.TEXT_HTML);
  FMediaTypes.Add('.txt', TMVCMediaType.TEXT_PLAIN);
  FMediaTypes.Add('.css', TMVCMediaType.TEXT_CSS);
  FMediaTypes.Add('.js', TMVCMediaType.TEXT_JAVASCRIPT);
  FMediaTypes.Add('.jpg', TMVCMediaType.IMAGE_JPEG);
  FMediaTypes.Add('.jpeg', TMVCMediaType.IMAGE_JPEG);
  FMediaTypes.Add('.png', TMVCMediaType.IMAGE_PNG);
  FMediaTypes.Add('.appcache', TMVCMediaType.TEXT_CACHEMANIFEST);

  Log.Info('EXIT: Config default values', LOGGERPRO_TAG);
end;

constructor TMVCEngine.Create(
  const AWebModule: TWebModule;
  const AConfigAction: TProc<TMVCConfig>;
  const ACustomLogger: ILogWriter);
begin
  inherited Create(AWebModule);
  FWebModule := AWebModule;
  FConfig := TMVCConfig.Create;
  FSerializers := TDictionary<string, IMVCSerializer>.Create;
  FMiddlewares := TList<IMVCMiddleware>.Create;
  FControllers := TObjectList<TMVCControllerDelegate>.Create(True);
  FMediaTypes := TDictionary<string, string>.Create;
  FApplicationSession := nil;
  FSavedOnBeforeDispatch := nil;

  WebRequestHandler.CacheConnections := True;
  WebRequestHandler.MaxConnections := 4096;

  FixUpWebModule;
  MVCFramework.Logger.SetDefaultLogger(ACustomLogger);
  ConfigDefaultValues;

  if Assigned(AConfigAction) then
  begin
    LogEnterMethod('Custom configuration method');
    AConfigAction(FConfig);
    LogExitMethod('Custom configuration method');
  end;

  RegisterDefaultsSerializers;
  LoadSystemControllers;
end;

procedure TMVCEngine.DefineDefaultReponseHeaders(const AContext: TWebContext);
begin
  if Config[TMVCConfigKey.ExposeServerSignature] = 'true' then
    AContext.Response.CustomHeaders.Values['Server'] := Config[TMVCConfigKey.ServerName];
  AContext.Response.RawWebResponse.Date := Now;
end;

destructor TMVCEngine.Destroy;
begin
  FConfig.Free;
  FSerializers.Free;
  FMiddlewares.Free;
  FControllers.Free;
  FMediaTypes.Free;
  inherited Destroy;
end;

function TMVCEngine.ExecuteAction(const ASender: TObject; const ARequest: TWebRequest; const AResponse: TWebResponse): Boolean;
var
  LParamsTable: TMVCRequestParamsTable;
  LContext: TWebContext;
  LFileName: string;
  LRouter: TMVCRouter;
  LHandled: Boolean;
  LResponseContentType: string;
  LResponseContentCharset: string;
  LSelectedController: TMVCController;
  LActionFormalParams: TArray<TRttiParameter>;
  LActualParams: TArray<TValue>;
begin
  Result := False;

  LParamsTable := TMVCRequestParamsTable.Create;
  try
    LContext := TWebContext.Create(ARequest, AResponse, FConfig, FSerializers);
    try
      DefineDefaultReponseHeaders(LContext);
      if IsStaticFileRequest(ARequest, LFileName) then
        Result := SendStaticFileIfPresent(LContext, LFileName)
      else
      begin
        LHandled := False;
        LRouter := TMVCRouter.Create(FConfig);
        try
          ExecuteBeforeRoutingMiddleware(LContext, LHandled);
          if not LHandled then
          begin
            if LRouter.ExecuteRouting(
              ARequest.PathInfo,
              TMVCRouter.StringMethodToHTTPMetod(ARequest.Method),
              ARequest.ContentType,
              ARequest.Accept,
              FControllers,
              FConfig[TMVCConfigKey.DefaultContentType],
              FConfig[TMVCConfigKey.DefaultContentCharset],
              LParamsTable,
              LResponseContentType,
              LResponseContentCharset) then
            begin
              if Assigned(LRouter.ControllerCreateAction) then
                LSelectedController := LRouter.ControllerCreateAction()
              else
                LSelectedController := LRouter.ControllerClazz.Create;
              try
                LSelectedController.Engine := Self;
                LSelectedController.Context := LContext;
                LSelectedController.ApplicationSession := FApplicationSession;
                LContext.ParamsTable := LParamsTable;

                try
                  ExecuteBeforeControllerActionMiddleware(LContext, LRouter.ControllerClazz.QualifiedClassName, LRouter.MethodToCall.Name, LHandled);
                  if LHandled then
                    Exit(True);

                  LSelectedController.MVCControllerAfterCreate;
                  try
                    LHandled := False;
                    LSelectedController.ContentType := LResponseContentType;
                    LSelectedController.ContentCharset := LResponseContentCharset;
                    if not LHandled then
                    begin
                      LActionFormalParams := LRouter.MethodToCall.GetParameters;
                      if (Length(LActionFormalParams) = 0) then
                        SetLength(LActualParams, 0)
                      else if (Length(LActionFormalParams) = 1) and (SameText(LActionFormalParams[0].ParamType.QualifiedName, 'MVCFramework.TWebContext')) then
                      begin
                        SetLength(LActualParams, 1);
                        LActualParams[0] := LContext;
                      end
                      else
                        FillActualParamsForAction(LContext, LActionFormalParams, LRouter.MethodToCall.Name, LActualParams);

                      LSelectedController.OnBeforeAction(LContext, LRouter.MethodToCall.Name, LHandled);
                      if not LHandled then
                        try
                          LRouter.MethodToCall.Invoke(LSelectedController, LActualParams);
                        finally
                          LSelectedController.OnAfterAction(LContext, LRouter.MethodToCall.Name);
                        end;
                    end;
                  finally
                    LSelectedController.MVCControllerBeforeDestroy;
                  end;
                  ExecuteAfterControllerActionMiddleware(LContext, LRouter.MethodToCall.Name, LHandled);
                except
                  on E: EMVCSessionExpiredException do
                  begin
                    LogException(E, E.DetailedMessage);
                    LContext.SessionStop(false);
                    LSelectedController.ResponseStatus(E.HTTPErrorCode);
                    LSelectedController.Render(E);
                  end;
                  on E: EMVCException do
                  begin
                    LogException(E, E.DetailedMessage);
                    LSelectedController.ResponseStatus(E.HTTPErrorCode);
                    LSelectedController.Render(E);
                  end;
                  on E: EInvalidOp do
                  begin
                    LogException(E, 'Invalid OP');
                    LSelectedController.ResponseStatus(HTTP_STATUS.InternalServerError);
                    LSelectedController.Render(E);
                  end;
                  on E: Exception do
                  begin
                    LogException(E, 'Global Action Exception Handler');
                    LSelectedController.ResponseStatus(HTTP_STATUS.InternalServerError);
                    LSelectedController.Render(E);
                  end;
                end;

                LContext.Response.ContentType := LSelectedController.ContentType;
                Log(TLogLevel.levNormal, ARequest.Method + ':' +
                  ARequest.RawPathInfo + ' -> ' +
                  LRouter.ControllerClazz.QualifiedClassName + ' - ' +
                  IntToStr(AResponse.StatusCode) + ' ' + AResponse.ReasonString)
              finally
                LSelectedController.Free;
              end;
            end
            else
            begin
              if Config[TMVCConfigKey.AllowUnhandledAction] = 'false' then
              begin
                if not Config[TMVCConfigKey.FallbackResource].IsEmpty then
                  Result := SendStaticFileIfPresent(LContext, TPath.Combine(Config[TMVCConfigKey.DocumentRoot], Config[TMVCConfigKey.FallbackResource]));
                if not Result then
                begin
                  HTTP404(LContext);
                  Log(TLogLevel.levNormal, ARequest.Method + ':' +
                    ARequest.RawPathInfo + ' -> NO ACTION ' + ' - ' +
                    IntToStr(AResponse.StatusCode) + ' ' +
                    AResponse.ReasonString);
                end;
              end
              else
                LContext.Response.FlushOnDestroy := False;
            end;
          end;
        finally
          LRouter.Free;
        end;
      end;
    finally
      LContext.Free;
    end;
  finally
    LParamsTable.Free;
  end;
end;

procedure TMVCEngine.ExecuteAfterControllerActionMiddleware(
  const AContext: TWebContext;
  const AActionName: string;
  const AHandled: Boolean);
var
  I: Integer;
begin
  for I := FMiddlewares.Count - 1 downto 0 do
    FMiddlewares[I].OnAfterControllerAction(AContext, AActionName, AHandled);
end;

procedure TMVCEngine.ExecuteBeforeControllerActionMiddleware(
  const AContext: TWebContext;
  const AControllerQualifiedClassName: string;
  const AActionName: string;
  var AHandled: Boolean);
var
  Middleware: IMVCMiddleware;
begin
  if not AHandled then
    for Middleware in FMiddlewares do
    begin
      Middleware.OnBeforeControllerAction(AContext, AControllerQualifiedClassName, AActionName, AHandled);
      if AHandled then
        Break;
    end;
end;

procedure TMVCEngine.ExecuteBeforeRoutingMiddleware(const AContext: TWebContext; var AHandled: Boolean);
var
  Middleware: IMVCMiddleware;
begin
  if not AHandled then
    for Middleware in FMiddlewares do
    begin
      Middleware.OnBeforeRouting(AContext, AHandled);
      if AHandled then
        Break;
    end;
end;

class function TMVCEngine.ExtractSessionIdFromWebRequest(const AWebRequest: TWebRequest): string;
begin
  Result := AWebRequest.CookieFields.Values[TMVCConstants.SESSION_TOKEN_NAME];
  if not Result.IsEmpty then
    Result := TIdURI.URLDecode(Result);
end;

procedure TMVCEngine.FillActualParamsForAction(
  const AContext: TWebContext;
  const AActionFormalParams: TArray<TRttiParameter>;
  const AActionName: string;
  var AActualParams: TArray<TValue>);
var
  ParamName: string;
  I: Integer;
  StrValue: string;
  FormatSettings: TFormatSettings;
  WasDateTime: Boolean;
begin
  if AContext.Request.SegmentParamsCount <> Length(AActionFormalParams) then
    raise EMVCException.CreateFmt('Paramaters count mismatch (expected %d actual %d) for action "%s"', [Length(AActionFormalParams), AContext.Request.SegmentParamsCount, AActionName]);

  SetLength(AActualParams, Length(AActionFormalParams));
  for I := 0 to Length(AActionFormalParams) - 1 do
  begin
    ParamName := AActionFormalParams[I].Name;

    if not AContext.Request.SegmentParam(ParamName, StrValue) then
      raise EMVCException.CreateFmt('Invalid paramater %s for action %s (Hint: Here parameters names are case-sensitive)', [ParamName, AActionName]);

    case AActionFormalParams[I].ParamType.TypeKind of
      tkInteger, tkInt64:
        begin
          AActualParams[I] := StrToInt(StrValue);
        end;
      tkUString:
        begin
          AActualParams[I] := StrValue;
        end;
      tkFloat:
        begin
          WasDateTime := False;
          if AActionFormalParams[I].ParamType.QualifiedName = 'System.TDate' then
          begin
            try
              WasDateTime := True;
              AActualParams[I] := ISODateToDate(StrValue);
            except
              raise EMVCException.CreateFmt('Invalid TDate value for param [%s]', [AActionFormalParams[I].Name]);
            end;
          end
          else if AActionFormalParams[I].ParamType.QualifiedName = 'System.TDateTime' then
          begin
            try
              WasDateTime := True;
              AActualParams[I] := ISOTimeStampToDateTime(StrValue);
            except
              raise EMVCException.CreateFmt('Invalid TDateTime value for param [%s]', [AActionFormalParams[I].Name]);
            end;
          end
          else if AActionFormalParams[I].ParamType.QualifiedName = 'System.TTime' then
          begin
            try
              WasDateTime := True;
              AActualParams[I] := ISOTimeToTime(StrValue);
            except
              raise EMVCException.CreateFmt('Invalid TTime value for param [%s]', [AActionFormalParams[I].Name]);
            end;
          end;
          if not WasDateTime then
          begin
            FormatSettings.DecimalSeparator := '.';
            AActualParams[I] := StrToFloat(StrValue, FormatSettings);
          end;
        end;
      tkEnumeration:
        begin
          if AActionFormalParams[I].ParamType.QualifiedName = 'System.Boolean' then
          begin
            if SameText(StrValue, 'true') or SameText(StrValue, '1') then
              AActualParams[I] := True
            else if SameText(StrValue, 'false') or SameText(StrValue, '0') then
              AActualParams[I] := False
            else
              raise EMVCException.CreateFmt
                ('Invalid boolean value for parameter %s. Boolean parameters accepts only "true"/"false" or "1"/"0".', [ParamName]);
          end
          else
            raise EMVCException.CreateFmt
              ('Invalid type for parameter %s. Allowed types are ' + ALLOWED_TYPED_ACTION_PARAMETERS_TYPES, [ParamName]);
        end;
    else
      begin
        raise EMVCException.CreateFmt
          ('Invalid type for parameter %s. Allowed types are ' + ALLOWED_TYPED_ACTION_PARAMETERS_TYPES, [ParamName]);
      end;
    end;
  end;
end;

procedure TMVCEngine.FixUpWebModule;
begin
  FSavedOnBeforeDispatch := FWebModule.BeforeDispatch;
  FWebModule.BeforeDispatch := OnBeforeDispatch;
end;

class function TMVCEngine.GetCurrentSession(
  const ASessionTimeout: Integer;
  const ASessionId: string;
  const ARaiseExceptionIfExpired: Boolean): TWebSession;
var
  List: TObjectDictionary<string, TWebSession>;
  IsExpired: Boolean;
begin
  Result := nil;

  List := GlobalSessionList;
  TMonitor.Enter(List);
  try
    if not ASessionId.IsEmpty then
    begin
      IsExpired := True;
      if List.TryGetValue(ASessionId, Result) then
        if (ASessionTimeout = 0) then
          IsExpired := MinutesBetween(Now, Result.LastAccess) > DEFAULT_SESSION_INACTIVITY
        else
          IsExpired := MinutesBetween(Now, Result.LastAccess) > ASessionTimeout;

      if Assigned(Result) then
        if IsExpired then
        begin
          List.Remove(ASessionId);
          if ARaiseExceptionIfExpired then
            raise EMVCSessionExpiredException.Create('Session expired.')
          else
            Result := nil;
        end
        else
          Result.MarkAsUsed;
    end;
  finally
    TMonitor.Exit(List);
  end;
end;

function TMVCEngine.GetSessionBySessionId(const ASessionId: string): TWebSession;
begin
  Result := TMVCEngine.GetCurrentSession(StrToInt64(Config[TMVCConfigKey.SessionTimeout]), ASessionId, False);
  if Assigned(Result) then
    Result.MarkAsUsed;
end;

function TMVCEngine.GetViewEngineClass: TMVCViewEngineClass;
begin
  if FViewEngineClass = nil then
    raise EMVCConfigException.Create('No View Engine configured. [HINT: Use TMVCEngine.SetViewEngine() to set a valid view engine]');
  Result := FViewEngineClass;
end;

procedure TMVCEngine.HTTP404(const AContext: TWebContext);
begin
  AContext.Response.StatusCode := HTTP_STATUS.NotFound;
  AContext.Response.ReasonString := 'Not Found';
  AContext.Response.Content := 'Not Found';
end;

procedure TMVCEngine.HTTP500(const AContext: TWebContext; const AReasonString: string);
begin
  AContext.Response.StatusCode := HTTP_STATUS.InternalServerError;;
  AContext.Response.ReasonString := 'Internal server error: ' + AReasonString;
  AContext.Response.Content := 'Internal server error: ' + AReasonString;
end;

function TMVCEngine.IsStaticFileRequest(const ARequest: TWebRequest; out AFileName: string): Boolean;
begin
  Result := (not FConfig[TMVCConfigKey.DocumentRoot].IsEmpty) and (TMVCStaticContents.IsStaticFile(TPath.Combine(AppPath, FConfig[TMVCConfigKey.DocumentRoot]), ARequest.PathInfo, AFileName));
end;

procedure TMVCEngine.LoadSystemControllers;
begin
  Log(TLogLevel.levNormal, 'ENTER: LoadSystemControllers');
  AddController(TMVCSystemController);
  Log(TLogLevel.levNormal, 'EXIT: LoadSystemControllers');
end;

procedure TMVCEngine.OnBeforeDispatch(ASender: TObject; ARequest: TWebRequest; AResponse: TWebResponse; var AHandled: Boolean);
begin
  AHandled := False;
  if Assigned(FSavedOnBeforeDispatch) then
    FSavedOnBeforeDispatch(ASender, ARequest, AResponse, AHandled);
  if not AHandled then
  begin
    try
      AHandled := ExecuteAction(ASender, ARequest, AResponse);
    except
      on E: Exception do
      begin
        LogException(E);
        AResponse.Content := E.Message;
        AResponse.SendResponse;
        AHandled := True;
      end;
    end;
  end;
end;

procedure TMVCEngine.RegisterDefaultsSerializers;
begin
  if not FSerializers.ContainsKey(TMVCMediaType.APPLICATION_JSON) then
  begin
    FSerializers.Add(TMVCMediaType.APPLICATION_JSON, TMVCJSONDataObjectsSerializer.Create);
    // FSerializers.Add(TMVCMediaType.APPLICATION_JSON, TMVCJSONSerializer.Create);
  end;
end;

procedure TMVCEngine.ResponseErrorPage(const AException: Exception; const ARequest: TWebRequest; const AResponse: TWebResponse);
begin
  AResponse.SetCustomHeader('x-mvc-error', AException.ClassName + ': ' + AException.Message);
  AResponse.StatusCode := HTTP_STATUS.OK;
  begin
    AResponse.ContentType := TMVCMediaType.TEXT_PLAIN;
    AResponse.Content := Config[TMVCConfigKey.ServerName] + ' ERROR:' +
      sLineBreak + 'Exception raised of class: ' + AException.ClassName + sLineBreak +
      '***********************************************' + sLineBreak + AException.Message
      + sLineBreak + '***********************************************';
  end;
end;

class function TMVCEngine.SendSessionCookie(const AContext: TWebContext): string;
var
  SId: string;
begin
  SId := StringReplace(StringReplace(StringReplace(GUIDToString(TGUID.NewGuid), '}', '', []), '{', '', []), '-', '', [rfReplaceAll]);
  Result := SendSessionCookie(AContext, SId);
end;

class function TMVCEngine.SendSessionCookie(const AContext: TWebContext; const ASessionId: string): string;
var
  Cookie: TCookie;
  SessionTimeout: Integer;
begin
  ClearSessionCookiesAlreadySet(AContext.Response.Cookies);
  Cookie := AContext.Response.Cookies.Add;
  Cookie.Name := TMVCConstants.SESSION_TOKEN_NAME;
  Cookie.Value := ASessionId;
  SessionTimeout := StrToIntDef(AContext.Config[TMVCConfigKey.SessionTimeout], 0);
  if SessionTimeout = 0 then
    Cookie.Expires := 0
  else
    Cookie.Expires := Now + OneMinute * SessionTimeout;
  Cookie.Path := '/';
  Result := ASessionId;
end;

function TMVCEngine.SendStaticFileIfPresent(const AContext: TWebContext; const AFileName: string): Boolean;
var
  LContentType: string;
begin
  Result := False;
  if TFile.Exists(AFileName) then
  begin
    if FMediaTypes.TryGetValue(LowerCase(ExtractFileExt(AFileName)), LContentType) then
      LContentType := lContentType + ';charset=' + FConfig[TMVCConfigKey.DefaultContentCharset]
    else
      LContentType := TMVCMediaType.APPLICATION_OCTETSTREAM;
    TMVCStaticContents.SendFile(AFileName, LContentType, AContext);
    Result := True;
  end;
end;

function TMVCEngine.SetViewEngine(
  const AViewEngineClass: TMVCViewEngineClass): TMVCEngine;
begin
  FViewEngineClass := AViewEngineClass;
  Result := Self;
end;

{ TMVCBase }

class function TMVCBase.GetApplicationFileName: string;
// var
// Name: PChar;
// Size: Integer;
begin
  Result := GetModuleName(HInstance);
  // Result := EmptyStr;
  // Name := GetMemory(2048);
  // try
  // GetModuleName()
  // Size := GetModuleFileName(0, Name, 2048);
  // if Size > 0 then
  // Result := Name;
  // finally
  // FreeMem(Name, 2048);
  // end;
end;

class function TMVCBase.GetApplicationFileNamePath: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(GetApplicationFileName));
end;

function TMVCBase.GetApplicationSession: TWebApplicationSession;
begin
  if not Assigned(FApplicationSession) then
    raise EMVCException.CreateFmt('ApplicationSession not assigned to this %s instance.', [ClassName]);
  Result := FApplicationSession;
end;

function TMVCBase.GetConfig: TMVCConfig;
begin
  Result := Engine.Config;
end;

function TMVCBase.GetEngine: TMVCEngine;
begin
  if not Assigned(FEngine) then
    raise EMVCException.CreateFmt('MVCEngine not assigned to this %s instance.', [ClassName]);
  Result := FEngine;
end;

procedure TMVCBase.SetApplicationSession(const AValue: TWebApplicationSession);
begin
  FApplicationSession := AValue;
end;

procedure TMVCBase.SetEngine(const AValue: TMVCEngine);
begin
  FEngine := AValue;
end;

{ TMVCControllerDelegate }

constructor TMVCControllerDelegate.Create(const AClazz: TMVCControllerClazz; const ACreateAction: TMVCControllerCreateAction);
begin
  inherited Create;
  FClazz := AClazz;
  FCreateAction := ACreateAction;
end;

{ TMVCStaticContents }

class function TMVCStaticContents.IsScriptableFile(const AStaticFileName: string; const AConfig: TMVCConfig): Boolean;
begin
  Result := TPath.GetExtension(AStaticFileName).ToLower = '.' + AConfig[TMVCConfigKey.DefaultViewFileExtension].ToLower;
end;

class function TMVCStaticContents.IsStaticFile(const AViewPath, AWebRequestPath: string; out ARealFileName: string): Boolean;
var
  FileName: string;
begin
  if TDirectory.Exists(AViewPath) then
    FileName := AViewPath + AWebRequestPath.Replace('/', TPath.DirectorySeparatorChar)
  else
    FileName := GetApplicationFileNamePath + AViewPath + AWebRequestPath.Replace('/', TPath.DirectorySeparatorChar);
  ARealFileName := FileName;
  Result := TFile.Exists(ARealFileName);
end;

class procedure TMVCStaticContents.SendFile(const AFileName, AMediaType: string; AContext: TWebContext);
var
  FileDate: TDateTime;
  ReqDate: TDateTime;
  S: TFileStream;
begin
  FileDate := IndyFileAge(AFileName);
  if (FileDate = 0.0) and (not FileExists(AFileName)) then
  begin
    AContext.Response.StatusCode := 404;
  end
  else
  begin
    ReqDate := GMTToLocalDateTime(AContext.Request.Headers['If-Modified-Since']);
    if (ReqDate <> 0) and (abs(ReqDate - FileDate) < 2 * (1 / (24 * 60 * 60)))
    then
    begin
      AContext.Response.ContentType := AMediaType;
      AContext.Response.StatusCode := 304;
    end
    else
    begin
      S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
      AContext.Response.SetCustomHeader('Last-Modified', LocalDateTimeToHttpStr(FileDate));
      AContext.Response.SetContentStream(S, AMediaType);
    end;
  end;
end;

{ TMVCController }

constructor TMVCController.Create;
begin
  inherited Create;
  FContext := nil;
  FContentCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET;
  FResponseStream := nil;
  FViewModel := nil;
  FViewDataSets := nil;
end;

destructor TMVCController.Destroy;
begin
  if Assigned(FResponseStream) then
    FResponseStream.Free;

  if Assigned(FViewModel) then
    FViewModel.Free;

  if Assigned(FViewDataSets) then
    FViewDataSets.Free;
  inherited Destroy;
end;

function TMVCController.GetClientId: string;
begin
  Result := Session[CLIENTID_KEY];
  if Result.IsEmpty then
    raise EMVCException.Create('Invalid ClientID' + sLineBreak +
      'Hint: Messaging extensions require a valid clientid. Did you call /messages/clients/YOUR_CLIENT_ID ?');
end;

function TMVCController.GetContentType: string;
begin
  Result := GetContext.Response.ContentType;
end;

function TMVCController.GetContext: TWebContext;
begin
  if not Assigned(FContext) then
    raise EMVCException.CreateFmt('Context already set on %s.', [ClassName]);
  Result := FContext;
end;

function TMVCController.GetCurrentWebModule: TWebModule;
begin
  Result := Engine.WebModule;
end;

function TMVCController.GetSession: TWebSession;
begin
  Result := GetContext.Session;
end;

function TMVCController.GetStatusCode: Integer;
begin
  Result := GetContext.Response.StatusCode;
end;

function TMVCController.GetViewDataSets: TObjectDictionary<string, TDataSet>;
begin
  if not Assigned(FViewDataSets) then
    FViewDataSets := TObjectDictionary<string, TDataSet>.Create;
  Result := FViewDataSets;
end;

function TMVCController.GetViewModel: TMVCViewDataObject;
begin
  if not Assigned(FViewModel) then
    FViewModel := TMVCViewDataObject.Create;
  Result := FViewModel;
end;

function TMVCController.LoadView(const AViewNames: TArray<string>): string;
begin
  try
    Result := GetRenderedView(AViewNames);
    ResponseStream.Append(Result);
  except
    on E: Exception do
    begin
      LogException(E);
      ContentType := TMVCMediaType.TEXT_PLAIN;
      Render(E);
    end;
  end;
end;

procedure TMVCController.LoadViewFragment(const AViewFragment: string);
begin
  ResponseStream.Append(AViewFragment);
end;

procedure TMVCController.MVCControllerAfterCreate;
begin
  { Implement if need be. }
end;

procedure TMVCController.MVCControllerBeforeDestroy;
begin
  { Implement if need be. }
end;

procedure TMVCController.OnAfterAction(AContext: TWebContext; const AActionName: string);
begin
  { Implement if need be. }
end;

procedure TMVCController.OnBeforeAction(AContext: TWebContext; const AActionName: string; var AHandled: Boolean);
begin
  AHandled := False;
  if ContentType.IsEmpty then
    ContentType := Config[TMVCConfigKey.DefaultContentType];
  { Implement if need be. }
end;

procedure TMVCController.PushDataSetToView(const AModelName: string; const ADataSet: TDataSet);
begin
  GetViewDataSets.Add(AModelName, ADataSet);
end;

procedure TMVCController.PushObjectToView(const AModelName: string; const AModel: TObject);
begin
  GetViewModel.Add(AModelName, AModel);
end;

// procedure TMVCController.PushToView(const AModelName: string; const AModel: string);
// begin
// GetViewModel.Add(AModelName, AModel);
// end;

procedure TMVCController.RaiseSessionExpired;
begin
  raise EMVCSessionExpiredException.Create('Session expired.');
end;

procedure TMVCController.Redirect(const AUrl: string);
begin
  GetContext.Response.RawWebResponse.SendRedirect(AUrl);
end;

procedure TMVCController.Render(const AObject: TObject; const AOwns: Boolean);
begin
  Render(AObject, AOwns, stDefault);
end;

procedure TMVCController.Render(const AContent: string);
var
  LContentType: string;
  OutEncoding: TEncoding;
begin
  LContentType := ContentType + '; charset=' + ContentCharset;
  GetContext.Response.RawWebResponse.ContentType := LContentType;
  OutEncoding := TEncoding.GetEncoding(ContentCharset);
  try
    if SameText('UTF-8', UpperCase(ContentCharset)) then
      GetContext.Response.SetContentStream(TStringStream.Create(AContent, TEncoding.UTF8), LContentType)
    else
    begin
      GetContext.Response.SetContentStream(
        TBytesStream.Create(
        TEncoding.Convert(TEncoding.Default, OutEncoding, TEncoding.Default.GetBytes(AContent))),
        LContentType
        );
    end;
  finally
    OutEncoding.Free;
  end;
end;

procedure TMVCController.Render<T>(const ACollection: TObjectList<T>; const AOwns: Boolean);
begin
  Self.Render<T>(ACollection, AOwns, stDefault);
end;

procedure TMVCController.ResponseStatus(const AStatusCode: Integer; const AReasonString: string);
begin
  StatusCode := AStatusCode;
  GetContext.Response.ReasonString := AReasonString;
end;

function TMVCController.ResponseStream: TStringBuilder;
begin
  if not Assigned(FResponseStream) then
    FResponseStream := TStringBuilder.Create;
  Result := FResponseStream;
end;

function TMVCController.Serializer: IMVCSerializer;
begin
  Result := Serializer(ContentType);
end;

procedure TMVCController.SendFile(const AFileName: string);
begin
  TMVCStaticContents.SendFile(AFileName, ContentType, Context);
end;

procedure TMVCController.SendStream(
  const AStream: TStream;
  const AOwns: Boolean;
  const ARewind: Boolean);
var
  S: TStream;
begin
  if ARewind then
    AStream.Position := 0;

  if not AOwns then
  begin
    S := TMemoryStream.Create;
    S.CopyFrom(AStream, 0);
    S.Position := 0;
  end
  else
    S := AStream;

  GetContext.Response.RawWebResponse.Content := EmptyStr;
  GetContext.Response.RawWebResponse.ContentType := ContentType;
  GetContext.Response.RawWebResponse.ContentStream := S;
  GetContext.Response.RawWebResponse.FreeContentStream := True;
end;

function TMVCController.Serializer(const AContentType: string): IMVCSerializer;
begin
  if not Engine.Serializers.ContainsKey(AContentType) then
    raise EMVCException.CreateFmt('The serializer for %s could not be found.', [AContentType]);
  Result := Engine.Serializers.Items[AContentType];
end;

function TMVCController.SessionAs<T>: T;
begin
  Result := Session as T;
end;

procedure TMVCController.SetContentType(const AValue: string);
begin
  GetContext.Response.ContentType := AValue;
end;

procedure TMVCController.SetStatusCode(const AValue: Integer);
begin
  GetContext.Response.StatusCode := AValue;
end;

procedure TMVCController.Render(const AObject: TObject; const AOwns: Boolean; const AType: TMVCSerializationType);
begin
  try
    Render(Serializer(ContentType).SerializeObject(AObject, AType));
  finally
    if AOwns then
      AObject.Free;
  end;
end;

procedure TMVCController.Render(const AStream: TStream; const AOwns: Boolean);
begin
  SendStream(AStream, AOwns);
end;

procedure TMVCController.Render(const AErrorCode: Integer; const AErrorMessage, AErrorClassName: string);
var
  R: TMVCErrorResponse;
begin
  ResponseStatus(AErrorCode, AErrorMessage);
  R := TMVCErrorResponse.Create;
  try
    R.StatusCode := AErrorCode;
    if ((R.StatusCode div 100) = 2) then
      R.ReasonString := 'ok'
    else
      R.ReasonString := 'error';
    R.Message := AErrorMessage;
    R.Classname := AErrorClassName;
    Render(R, False, stProperties);
  finally
    R.Free;
  end;
end;

procedure TMVCController.Render(const ADataSet: TDataSet;
  const AOwns: boolean;
  const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase;
  const ASerializationType: TMVCDatasetSerializationType);
begin
  if Assigned(ADataSet) then
  begin
    try
      if ASerializationType = dstSingleRecord then
        Render(Serializer(ContentType).SerializeDataSetRecord(ADataSet, AIgnoredFields, ANameCase))
      else
        Render(Serializer(ContentType).SerializeDataSet(ADataSet, AIgnoredFields, ANameCase))
    finally
      if AOwns then
        ADataSet.Free;
    end;
  end
  else
    raise EMVCException.Create('Can not render an empty dataset.');
end;

procedure TMVCController.Render<T>(const ACollection: TObjectList<T>;
  const AOwns: Boolean; const AType: TMVCSerializationType);
begin
  if Assigned(ACollection) then
  begin
    try
      Render(Serializer(ContentType).SerializeCollection(ACollection, AType));
    finally
      if AOwns then
        ACollection.Free;
    end;
  end
  else
    raise EMVCException.Create('Can not render an empty collection.');
end;

function TMVCController.GetRenderedView(const AViewNames: TArray<string>): string;

{$IFNDEF LINUX}

var
  View: TMVCBaseViewEngine;
  ViewName: string;
  SBuilder: TStringBuilder;

  {$ENDIF}

begin

  {$IFNDEF LINUX}

  SBuilder := TStringBuilder.Create;
  try
    try
      View := FEngine.ViewEngineClass.Create(
        Engine,
        Context,
        ViewModel,
        ViewDataSets,
        ContentType);
      try

        for ViewName in AViewNames do
        begin
          View.Execute(ViewName);
          SBuilder.Append(View.Output);
        end;
      finally
        View.Free;
      end;
      Result := SBuilder.ToString;
    except
      on E: Exception do
      begin
        ContentType := TMVCMediaType.TEXT_PLAIN;
        Render(E);
      end;
    end;
  finally
    SBuilder.Free;
  end;

  {$ELSE}

  raise EMVCException.Create('Server Side Views are not supported on Linux');

  {$ENDIF}

end;

procedure TMVCController.Render<T>(const ACollection: TObjectList<T>);
begin
  Self.Render<T>(ACollection, True);
end;

procedure TMVCController.RenderResponseStream;
begin
  Render(ResponseStream.ToString);
end;

procedure TMVCController.Render(const ACollection: IMVCList);
begin
  Render(ACollection, stDefault);
end;

procedure TMVCController.Render(const ACollection: IMVCList;
  const AType: TMVCSerializationType);
begin
  if Assigned(ACollection) then
    Render(Serializer(ContentType).SerializeCollection(TObject(ACollection), AType))
  else
    raise EMVCException.Create('Can not render an empty collection.');
end;

procedure TMVCController.Render(const ATextWriter: TTextWriter; const AOwns: Boolean);
begin
  if Assigned(ATextWriter) then
  begin
    try
      Render(ATextWriter.ToString);
    finally
      if AOwns then
        ATextWriter.Free;
    end;
  end
  else
    raise EMVCException.Create('Can not render an empty textwriter.');
end;

procedure TMVCController.Render(const AException: Exception; AExceptionItems: TList<string>; const AOwns: Boolean);
var
  S: string;
  R: TMVCErrorResponse;
  I: TMVCErrorResponseItem;
begin
  try
    if AException is EMVCException then
      ResponseStatus(EMVCException(AException).HTTPErrorCode, AException.Message + ' [' + AException.ClassName + ']');

    if (GetContext.Response.StatusCode = HTTP_STATUS.OK) then
      ResponseStatus(HTTP_STATUS.InternalServerError, AException.Message + ' [' + AException.ClassName + ']');

    if (not GetContext.Request.IsAjax) and (GetContext.Request.ClientPrefer(TMVCMediaType.TEXT_HTML)) then
    begin
      ContentType := TMVCMediaType.TEXT_HTML;
      ContentCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      ResponseStream.Clear;
      ResponseStream.Append
        ('<html><head><style>pre { color: #000000; background-color: #d0d0d0; }</style></head><body>')
        .Append('<h1>' + Config[TMVCConfigKey.ServerName] + ': Error Raised</h1>')
        .AppendFormat('<pre>HTTP Return Code: %d' + sLineBreak, [GetContext.Response.StatusCode])
        .AppendFormat('HTTP Reason Text: "%s"</pre>', [GetContext.Response.ReasonString]).Append('<h3><pre>')
        .AppendFormat('Exception Class Name : %s' + sLineBreak, [AException.ClassName])
        .AppendFormat('Exception Message    : %s' + sLineBreak, [AException.Message])
        .Append('</pre></h3>');
      if Assigned(AExceptionItems) and (AExceptionItems.Count > 0) then
      begin
        ResponseStream.Append('<h2><pre>');
        for S in AExceptionItems do
          ResponseStream.AppendLine('- ' + S);
        ResponseStream.Append('</pre><h2>');
      end
      else
        ResponseStream.AppendLine('<pre>No other informations available</pre>');
      ResponseStream.Append('</body></html>');
      RenderResponseStream;
    end
    else
    begin
      R := TMVCErrorResponse.Create;
      try
        R.StatusCode := GetContext.Response.StatusCode;
        R.ReasonString := 'error';
        R.Message := AException.Message;
        R.Classname := AException.ClassName;
        if Assigned(AExceptionItems) and (AExceptionItems.Count > 0) then
        begin
          for S in AExceptionItems do
          begin
            I := TMVCErrorResponseItem.Create;
            I.Message := S;
            R.Items.Add(I);
          end;
        end;
        Render(R, False);
      finally
        R.Free;
      end;
    end;
  finally
    if AOwns then
      AExceptionItems.Free;
  end;
end;

procedure TMVCController.Render(const AError: TMVCErrorResponse; const AOwns: Boolean);
begin
  if Assigned(AError) then
  begin
    try
      Render(AError, False, stProperties);
    finally
      if AOwns then
        AError.Free;
    end;
  end
  else
    raise EMVCException.Create('Cannot render an empty error object.');
end;

procedure TMVCController.Render(const ADataSet: TDataSet);
begin
  Render(ADataSet, True);
end;

procedure TMVCController.Render(const ADataSet: TDataSet; const AOwns: boolean);
begin
  Render(ADataSet, AOwns, dstAllRecords);
end;

procedure TMVCController.Render(const AObject: TObject);
begin
  Render(AObject, True);
end;

procedure TMVCController.Render(const ADataSet: TDataSet; const AOwns: Boolean; const AIgnoredFields: TMVCIgnoredList; const ASerializationType: TMVCDatasetSerializationType);
begin
  Render(ADataSet, AOwns, [], ncLowerCase, ASerializationType);
end;

procedure TMVCController.Render(const ADataSet: TDataSet; const AOwns: boolean; const ASerializationType: TMVCDatasetSerializationType);
begin
  Render(ADataSet, AOwns, [], ASerializationType);
end;

{ TMVCErrorResponse }

constructor TMVCErrorResponse.Create;
begin
  inherited Create;
  FItems := TObjectList<TMVCErrorResponseItem>.Create;
end;

constructor TMVCErrorResponse.Create(AStatusCode: Integer; AReasonString,
  AMessage: string);
begin
  Create;
  StatusCode := AStatusCode;
  ReasonString := AReasonString;
  message := AMessage;
end;

destructor TMVCErrorResponse.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

{ TMVCBaseView }

constructor TMVCBaseViewEngine.Create(
  const AEngine: TMVCEngine;
  const AWebContext: TWebContext;
  const AViewModel: TMVCViewDataObject;
  const AViewDataSets: TObjectDictionary<string, TDataSet>;
  const AContentType: string);
begin
  inherited Create;
  Engine := AEngine;
  FWebContext := AWebContext;
  FViewModel := AViewModel;
  FViewDataSets := AViewDataSets;
  FContentType := AContentType;
  FOutput := EmptyStr;
end;

destructor TMVCBaseViewEngine.Destroy;
begin
  inherited Destroy;
end;

function TMVCBaseViewEngine.GetRealFileName(const AViewName: string): string;
var
  FileName: string;
  F: string;
  DefaultViewFileExtension: string;
begin
  DefaultViewFileExtension := Config[TMVCConfigKey.DefaultViewFileExtension];
  FileName := stringReplace(AViewName, '/', '\', [rfReplaceAll]);

  if (FileName = '\') then
    FileName := '\index.' + DefaultViewFileExtension
  else
    FileName := FileName + '.' + DefaultViewFileExtension;

  if DirectoryExists(Config[TMVCConfigKey.ViewPath]) then
    F := ExpandFileName(IncludeTrailingPathDelimiter(Config.Value[TMVCConfigKey.ViewPath]) + FileName)
  else
    F := ExpandFileName(IncludeTrailingPathDelimiter(GetApplicationFileNamePath + Config.Value[TMVCConfigKey.ViewPath]) + FileName);

  if not TFile.Exists(F) then
    FileName := ExpandFileName(IncludeTrailingPathDelimiter(GetApplicationFileNamePath + Config.Value[TMVCConfigKey.DocumentRoot]) + FileName)
  else
    FileName := F;

  if FileExists(FileName) then
    Result := FileName
  else
    Result := EmptyStr;
end;

procedure TMVCBaseViewEngine.SetOutput(const AOutput: string);
begin
  FOutput := AOutput;
end;

initialization

_IsShuttingDown := 0;

end.
