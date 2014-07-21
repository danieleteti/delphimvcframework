unit MVCFramework;

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
  StompTypes,
  ObjectsMappers
{$IF Defined(VER270)}
    , System.JSON
    , Web.ApacheHTTP
{$ELSE}
    , Data.DBXJSON
{$IFEND}
, ReqMulti {Delphi XE4 (all update) and XE5 (with no update) dont contains this unit. Look for the bug in QC};

type
  TMVCHTTPMethodType = (httpGET, httpPOST, httpPUT, httpDELETE, httpHEAD, httpOPTIONS, httpPATCH, httpTRACE);
  TMVCHTTPMethods = set of TMVCHTTPMethodType;
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

  MVCProducesAttribute = class(MVCStringAttribute)
  private
    FProduceEncoding: string;
    procedure SetProduceEncoding(const Value: string);
  public
    constructor Create(const Value: string); overload;
    constructor Create(const Value: string; const ProduceEncoding: string); overload;
    property ProduceEncoding: string read FProduceEncoding write SetProduceEncoding;
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
    FWebRequest: TWebRequest;
    FPathInfo: string;
    FParamsTable: TMVCRequestParamsTable;
    FContentType: string;
    FCharset: string;
    FContentCharset: string;
    function GetHeader(const Name: string): string;
    function GetHeaderValue(const Name: string): string;
    function GetPathInfo: string;
    procedure SetPathInfo(const Value: string);
    function Param(Name: string): string;
    function GetParamAll(const ParamName: string): string;
    function GetIsAjax: boolean;
    function GetHTTPMethod: TMVCHTTPMethodType;
    function GetHTTPMethodAsString: string;
    function GetParamAllAsInteger(const ParamName: string): Integer;
    function GetClientPreferHTML: boolean;
    function GetFiles: TAbstractWebRequestFiles;

  strict protected
    FBodyAsJSONValue: TJSONValue;
    FParamNames: TArray<string>;
  public
    destructor Destroy; override;
    procedure SetParamsTable(AParamsTable: TMVCRequestParamsTable);
    function GetParamNames: TArray<string>;
    function ClientIP: string; virtual; abstract;
    function ClientPrefer(MimeType: string): boolean;
    function ThereIsRequestBody: boolean;
    function Accept: string;
    function QueryStringParam(Name: string): string; virtual;
    function QueryStringParamExists(Name: string): boolean; virtual;
    function ContentParam(Name: string): string; virtual;
    function Cookie(Name: string): string; virtual;
    property PathInfo: string read GetPathInfo;
    function Body: string;
    function BodyAs<T: class, constructor>(const RootProperty: string = ''): T;
    function BodyAsListOf<T: class, constructor>(const RootProperty: string = ''): TObjectList<T>;
    function BodyAsJSONObject: TJSONObject;
    function BodyAsJSONValue: TJSONValue;
    property Headers[const HeaderName: string]: string read GetHeader;
    property ParamsAsInteger[const ParamName: string]: Integer read GetParamAllAsInteger;
    property Params[const ParamName: string]: string read GetParamAll;
    property IsAjax: boolean read GetIsAjax;
    property HTTPMethod: TMVCHTTPMethodType read GetHTTPMethod;
    property HTTPMethodAsString: string read GetHTTPMethodAsString;
    property RawWebRequest: TWebRequest read FWebRequest;
    property ClientPreferHTML: boolean read GetClientPreferHTML;
    property Files: TAbstractWebRequestFiles read GetFiles;
    property ContentType: string read FContentType;
    property ContentCharset: string read FContentCharset;
    property Charset: string read FCharset;
  end;

{$IF Defined(VER270)}

  TMVCApacheWebRequest = class(TMVCWebRequest)
  public
    constructor Create(AWebRequest: TWebRequest); override;
    function ClientIP: string; override;
  end;
{$ENDIF}

  TMVCISAPIWebRequest = class(TMVCWebRequest)
  public
    constructor Create(AWebRequest: TWebRequest); override;
    function ClientIP: string; override;
  end;

  TMVCINDYWebRequest = class(TMVCWebRequest)
  public
    constructor Create(AWebRequest: TWebRequest); override;
    function ClientIP: string; override;

  end;

{$IFDEF IOCP}

  TMVCIOCPWebRequest = class(TMVCWebRequest)
  public
    constructor Create(AWebRequest: TWebRequest); override;
  end;

{$ENDIF}

  TMVCWebResponse = class
  strict private
    function GetCustomHeaders: TStrings;

  private
    FStreamOutputDone: boolean;
    FReasonString: string;
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
    property ReasonString: string read FReasonString write SetReasonString;
    property Cookies: TCookieCollection read GetCookies;
    property ContentType: string read GetContentType write SetContentType;
    property Location: string read GetLocation write SetLocation;
    property RawWebResponse: TWebResponse read FWebResponse;
  end;

  TMVCEngine = class;

  TWebContext = class
  private
    FRequest: TMVCWebRequest;
    FResponse: TMVCWebResponse;
    FParamsTable: TMVCRequestParamsTable;
    FData: TDictionary<string, string>;
    function GetData: TDictionary<string, string>;

  protected
    constructor Create(ARequest: TWebRequest; AResponse: TWebResponse); virtual;
    procedure SetParams(AParamsTable: TMVCRequestParamsTable);
    procedure Flush;

  public
    ReservedData: TObject;
    destructor Destroy; override;
    property Request: TMVCWebRequest read FRequest;
    property Response: TMVCWebResponse read FResponse;
    property Data: TDictionary<string, string> read GetData;
  end;

  TMVCActionProc = reference to procedure(Context: TWebContext);

  TMVCBase = class(TObject)
  private
    FMVCEngine: TMVCEngine;
    FMVCConfig: TMVCConfig;
    FApplicationSession: TWebApplicationSession;
    procedure SetApplicationSession(const Value: TWebApplicationSession);

  protected
    class function GetApplicationFileName: string;
    class function GetApplicationFileNamePath: string;

  public
    procedure SetMVCConfig(const Value: TMVCConfig);
    function GetMVCConfig: TMVCConfig;
    procedure SetMVCEngine(const Value: TMVCEngine);
    function GetMVCEngine: TMVCEngine;
    property ApplicationSession: TWebApplicationSession read FApplicationSession write SetApplicationSession;
  end;

  TMVCController = class(TMVCBase)
  public
    IsSessionStarted: boolean;
    SessionMustBeClose: boolean;

  private
    FViewModel: TMVCDataObjects;
    FViewDataSets: TObjectDictionary<string, TDataSet>;
    FContext: TWebContext;
    FWebSession: TWebSession;
    FResponseStream: TStringBuilder;
    FContentCharset: string;
    procedure SetContext(const Value: TWebContext);
    procedure SetWebSession(const Value: TWebSession);
    procedure SetContentType(const Value: string);
    function GetContentType: string;
    function GetWebSession: TWebSession;
    function GetContentCharset: string;
    procedure SetContentCharset(const Value: string);

  protected
    procedure RaiseSessionExpired; virtual;
    function GetCurrentWebModule: TWebModule;
    function ResponseStream: TStringBuilder;
    function GetNewStompClient(ClientID: string = ''): IStompClient;
    function GetClientID: string;
    procedure LoadView(const ViewName: string); virtual;
    property Context: TWebContext read FContext write SetContext;
    property Session: TWebSession read GetWebSession write SetWebSession;
    procedure MVCControllerAfterCreate; virtual;
    procedure MVCControllerBeforeDestroy; virtual;
    property ContentType: string read GetContentType write SetContentType;
    property ContentCharset: string read GetContentCharset write SetContentCharset;
    // Session
    procedure SessionStart; virtual;
    procedure SessionStop(ARaiseExceptionIfExpired: boolean = true); virtual;
    procedure BindToSession(SessionID: string);
    procedure SendSessionCookie(const SessionID: string);
    // Renderers
    procedure Render(const Content: string); overload; virtual;
    procedure Render; overload; virtual;
    procedure Render<T: class>(ACollection: TObjectList<T>; AInstanceOwner: boolean = true;
      AJSONObjectActionProc: TJSONObjectActionProc = nil); overload;
    procedure Render(AObject: TObject; AInstanceOwner: boolean = true); overload; virtual;
    procedure Render(ADataSet: TDataSet; AInstanceOwner: boolean = false; AOnlySingleRecord: boolean = false);
      overload; virtual;
    procedure Render(AJSONValue: TJSONValue; AInstanceOwner: boolean = true); overload; virtual;
    procedure RenderListAsProperty<T: class>(const APropertyName: string; AObjectList: TObjectList<T>;
      AOwnsInstance: boolean = true; AJSONObjectActionProc: TJSONObjectActionProc = nil);
    procedure Render(E: Exception; ErrorItems: TList<string> = nil); overload; virtual;
    procedure Render(const AErrorCode: UInt16; const AErrorMessage: string); overload;
    procedure Render(const AErrorCode: UInt16; AJSONValue: TJSONValue;
      AInstanceOwner: boolean = true); overload;
    procedure Render(const AErrorCode: UInt16; AObject: TObject; AInstanceOwner: boolean = true); overload;
    procedure RenderStreamAndFree(const AStream: TStream);
    // messaging
    procedure EnqueueMessageOnTopic(const ATopic: string; AJSONObject: TJSONObject;
      AOwnsInstance: boolean = true);
    function ReceiveMessageFromTopic(const ATopic: string; ATimeout: Int64;
      var JSONObject: TJSONObject): boolean;
    // redirects
    procedure Redirect(const URL: string);
    // http return code
    procedure ResponseStatusCode(const ErrorCode: UInt16);
    // streams and files
    procedure SendStream(AStream: TStream); virtual;
    procedure SendFile(AFileName: string); virtual;
    // filters before, after
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string; var Handled: boolean); virtual;
    procedure OnAfterAction(Context: TWebContext; const AActionNAme: string); virtual;

    property Config: TMVCConfig read GetMVCConfig;

  public
    // property ViewCache: TViewCache read FViewCache write SetViewCache;
    procedure PushJSONToView(const AModelName: string; AModel: TJSONValue);
    procedure PushModelToView(const AModelName: string; AModel: TObject);
    procedure PushDataSetToView(const AModelName: string; ADataSet: TDataSet);
    constructor Create;
    destructor Destroy; override;
  end;

  TMVCControllerClass = class of TMVCController;

  IMVCMiddleware = interface
    ['{3278183A-124A-4214-AB4E-94CA4C22450D}']
    procedure OnBeforeRouting(Context: TWebContext; var Handled: boolean);
    procedure OnAfterControllerAction(Context: TWebContext; const AActionNAme: string;
      const Handled: boolean);
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
    function GetBinVersion(): string;
    function GetUpTime: string;

  protected
    FConfiguredSessionTimeout: Int64;
    FControllers: TList<TMVCControllerClass>;
    FMiddleware: TList<IMVCMiddleware>;
    procedure ExecuteBeforeRoutingMiddleware(Context: TWebContext; var Handled: boolean);
    procedure ExecuteAfterMiddleware(Context: TWebContext; const AActionNAme: string; const Handled: boolean);
    procedure ConfigDefaultValues; virtual;
    procedure FixUpWebModule;
    procedure OnBeforeDispatch(Sender: TObject; Request: TWebRequest; Response: TWebResponse;
      var Handled: boolean); virtual;
    function ExecuteAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse): boolean; virtual;
    procedure LoadSystemControllers; virtual;
    procedure ResponseErrorPage(E: Exception; Request: TWebRequest; Response: TWebResponse); virtual;
    function IsBuiltInMethod(const AWebRequest: TWebRequest; const AWebResponse: TWebResponse): boolean;
    procedure HandleBuiltInMethods(const AWebRequest: TWebRequest; const AWebResponse: TWebResponse);
    procedure ExecuteFile(const AFileName: string; AContext: TWebContext); virtual;

  public
    class function GetCurrentSession(Config: TMVCConfig; const AWebRequest: TWebRequest;
      const AWebResponse: TWebResponse; const BindToThisSessionID: string = '';
      ARaiseExceptionIfExpired: boolean = true): TWebSession;
    constructor Create(WebModule: TWebModule); reintroduce;
    destructor Destroy; override;
    function AddController(AControllerClass: TMVCControllerClass): TMVCEngine; overload;
    function AddMiddleware(AMiddleware: IMVCMiddleware): TMVCEngine;
    // http return codes
    procedure Http404(AWebContext: TWebContext);
    procedure Http500(AWebContext: TWebContext; AReasonText: string = '');
    property Config: TMVCConfig read FMVCConfig; // allow a simple client code
    property ApplicationSession: TWebApplicationSession read FApplicationSession write SetApplicationSession;
  end;

  TMVCStaticContents = class(TMVCController)
  public
    // [MVCPath('/static/($filename)')]
    class procedure SendFile(AFileName, AMimeType: string; Context: TWebContext);
    class function IsStaticFile(AViewPath, AWebRequestPath: string; out ARealFileName: string): boolean;
    class function IsScriptableFile(StaticFileName: string; Config: TMVCConfig): boolean;
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
  end;

function IsShuttingDown: boolean;
procedure EnterInShutdownState;

procedure InternalRender(const Content: string; ContentType, ContentEncoding: string;
  Context: TWebContext); overload;
procedure InternalRenderText(const AContent: string; ContentType, ContentEncoding: string;
  Context: TWebContext);
procedure InternalRender(AJSONValue: TJSONValue; ContentType, ContentEncoding: string; Context: TWebContext;
  AInstanceOwner: boolean = true); overload;

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
  DuckListU,
  IdStack

{$IFDEF IOCP},
  Iocp.DSHTTPWebBroker

{$ELSE},
  IdHTTPWebBrokerBridge

{$ENDIF},
  LuaBind,
  MVCFramework.BUSController, Web.WebReq;

type
  TIdHTTPAppRequestHack = class({$IFDEF IOCP}TIocpWebRequest
{$ELSE}TIdHTTPAppRequest{$ENDIF})

  end;

threadvar ctx: TRTTIContext;

var
  _IsShuttingDown: Int64 = 0;
  // this variable is used by TInterlocked functions to handlòe the "shuttingdown" mode

  { TMVCEngine }

function TMVCEngine.AddController(AControllerClass: TMVCControllerClass): TMVCEngine;
begin
  FControllers.Add(AControllerClass);
  Result := Self;
end;

function TMVCEngine.AddMiddleware(AMiddleware: IMVCMiddleware): TMVCEngine;
begin
  FMiddleware.Add(AMiddleware);
  Result := Self;
end;

procedure TMVCEngine.ConfigDefaultValues;
begin
  Config[TMVCConfigKey.SessionTimeout] := '30'; // 30 minutes
  Config[TMVCConfigKey.DocumentRoot] := '.\www';
  Config[TMVCConfigKey.ViewPath] := 'eLua';
  Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
  Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;

  Config[TMVCConfigKey.DefaultViewFileExtension] := 'elua';
  Config[TMVCConfigKey.ISAPIPath] := '';

  Config[TMVCConfigKey.StompServer] := 'localhost';
  Config[TMVCConfigKey.StompServerPort] := '61613';
  Config[TMVCConfigKey.StompUsername] := 'guest';
  Config[TMVCConfigKey.StompPassword] := 'guest';
  Config[TMVCConfigKey.Messaging] := 'false';

  // Config['sessiontimeout'] := '30'; // 30 minutes
  // Config['document_root'] := '..\..\..\www';
  // Config['view_path'] := 'eLua';
  // Config['default_content_type'] := TMVCMimeType.APPLICATION_JSON;
  // Config['default_view_file_extension'] := 'elua';
  // Config['isapi_path'] := '';
  //
  // Config['stompserver'] := 'localhost';
  // Config['stompserverport'] := '61613';
  // Config['stompusername'] := 'guest';
  // Config['stomppassword'] := 'guest';
  // Config['messaging'] := 'true';
  /// ///////
  FMimeTypes.Add('.html', TMVCMimeType.TEXT_HTML);
  FMimeTypes.Add('.htm', TMVCMimeType.TEXT_HTML);
  FMimeTypes.Add('.txt', TMVCMimeType.TEXT_PLAIN);
  FMimeTypes.Add('.css', TMVCMimeType.TEXT_CSS);
  FMimeTypes.Add('.js', TMVCMimeType.TEXT_JAVASCRIPT);
  FMimeTypes.Add('.jpg', TMVCMimeType.IMAGE_JPEG);
  FMimeTypes.Add('.jpeg', TMVCMimeType.IMAGE_JPEG);
  FMimeTypes.Add('.png', TMVCMimeType.IMAGE_PNG);
  FMimeTypes.Add('.appcache', TMVCMimeType.TEXT_CACHEMANIFEST);
end;

constructor TMVCEngine.Create(WebModule: TWebModule);
begin
  inherited Create(WebModule);
  WebRequestHandler.CacheConnections := true;
  WebRequestHandler.MaxConnections := 1024;
  FMimeTypes := TDictionary<string, string>.Create;
  FMVCConfig := TMVCConfig.Create;
  FWebModule := WebModule;
  FControllers := TList<TMVCControllerClass>.Create;
  FMiddleware := TList<IMVCMiddleware>.Create;
  // FViewCache := TViewCache.Create;
  FixUpWebModule;
  ConfigDefaultValues;
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

function TMVCEngine.ExecuteAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse): boolean;
var
  SelectedController: TMVCController;
  Context: TWebContext;
  ParamsTable: TMVCRequestParamsTable;
  Router: TMVCRouter;
  StaticFileName: string;
  ContentType: string;
  Handled: boolean;
  ResponseContentType, ResponseContentCharset: string;
begin
  LogEnterMethod(Request.PathInfo);
  try
    Result := false;
    ParamsTable := TMVCRequestParamsTable.Create;
    try
      Context := TWebContext.Create(Request, Response);
      try
        // Static file handling
        if TMVCStaticContents.IsStaticFile(TPath.Combine(AppPath, FMVCConfig[TMVCConfigKey.DocumentRoot]),
          Request.PathInfo, StaticFileName) then
        begin
          if TMVCStaticContents.IsScriptableFile(StaticFileName, FMVCConfig) then
          // execute the file
          begin
            ExecuteFile(StaticFileName, Context);
          end
          else // serve the file
          begin
            if not FMimeTypes.TryGetValue(LowerCase(ExtractFileExt(StaticFileName)), ContentType) then
              ContentType := TMVCMimeType.APPLICATION_OCTETSTREAM;
            TMVCStaticContents.SendFile(StaticFileName, ContentType, Context);
          end;
        end
        else
        begin
          Router := TMVCRouter.Create(Config);
          try
            ExecuteBeforeRoutingMiddleware(Context, Handled);
            if not Handled then
            begin
              if Router.ExecuteRouting(
                Request.PathInfo,
                TMVCRouter.StringMethodToHTTPMetod(Request.Method),
                Request.ContentType,
                Request.Accept,
                FControllers,
                FMVCConfig[TMVCConfigKey.DefaultContentType],
                FMVCConfig[TMVCConfigKey.DefaultContentCharset],
                ParamsTable,
                ResponseContentType,
                ResponseContentCharset) then
              begin
                SelectedController := Router.MVCControllerClass.Create;
                try
                  SelectedController.SetMVCConfig(Config);
                  SelectedController.ApplicationSession := FApplicationSession;
                  Context.SetParams(ParamsTable);
                  SelectedController.SetContext(Context);
                  SelectedController.SetMVCEngine(Self);
                  Log(TLogLevel.levNormal, Context.Request.HTTPMethodAsString + ':' + Request.RawPathInfo +
                    ' -> ' +
                    Router.MVCControllerClass.QualifiedClassName);

                  // exception?
                  try
                    SelectedController.MVCControllerAfterCreate;
                    try
                      Handled := false;
                      // gets response contentype from MVCProduces attribute
                      SelectedController.ContentType := ResponseContentType;
                      SelectedController.ContentCharset := ResponseContentCharset;
                      SelectedController.OnBeforeAction(Context, Router.MethodToCall.Name, Handled);
                      if not Handled then
                      begin
                        try
                          Router.MethodToCall.Invoke(SelectedController, [Context]);
                        finally
                          SelectedController.OnAfterAction(Context, Router.MethodToCall.Name);
                        end;
                      end;

                      if SelectedController.SessionMustBeClose then
                      begin
                        // SessionList.Remove(SelectedController.Session.SessionID);
                      end
                      else
                      begin

                      end;

                    finally
                      SelectedController.MVCControllerBeforeDestroy;
                    end;
                    ExecuteAfterMiddleware(Context, Router.MethodToCall.Name, Handled);
                  except
                    on E: EMVCSessionExpiredException do
                    begin
                      LogException(E, E.DetailedMessage);
                      SelectedController.SessionStop(false);
                      SelectedController.ResponseStatusCode(E.HTTPErrorCode);
                      SelectedController.Render(E);
                    end;
                    on E: EMVCException do
                    begin
                      LogException(E, E.DetailedMessage);
                      SelectedController.ResponseStatusCode(E.HTTPErrorCode);
                      SelectedController.Render(E);
                    end;
                    on E: EInvalidOp do
                    begin
                      LogException(E, 'Invalid OP');
                      SelectedController.Render(E);
                    end;
                    on E: Exception do
                    begin
                      LogException(E, 'Global Action Exception Handler');
                      SelectedController.Render(E);
                    end;
                  end;
                  Context.Response.ContentType := SelectedController.ContentType;
                finally
                  SelectedController.Free;
                end;
              end
              else if IsBuiltInMethod(Request, Response) then
              begin
                HandleBuiltInMethods(Request, Response);
              end
              else
              begin
                Http404(Context);
              end;
            end;
          finally
            Router.Free;
          end;
        end; // end if IS_STATIC
      finally
        Context.Free;
      end;
    finally
      ParamsTable.Free;
    end;
  finally
    LogExitMethod(Request.PathInfo + ' [' + IntToStr(Response.StatusCode) + ' ' +
      Response.ReasonString + ']');
  end;
end;

procedure TMVCEngine.ExecuteAfterMiddleware(Context: TWebContext; const AActionNAme: string;
  const Handled: boolean);
var
  middleware: IMVCMiddleware;
begin
  for middleware in FMiddleware do
  begin
    middleware.OnAfterControllerAction(Context, AActionNAme, Handled);
  end;
end;

procedure TMVCEngine.ExecuteBeforeRoutingMiddleware(Context: TWebContext; var Handled: boolean);
var
  middleware: IMVCMiddleware;
begin
  if not Handled then
    for middleware in FMiddleware do
    begin
      middleware.OnBeforeRouting(Context, Handled);
      if Handled then
        break;
    end;
end;

procedure TMVCEngine.ExecuteFile(const AFileName: string; AContext: TWebContext);
var
  View: TMVCEmbeddedLuaView;
begin
  try
    View := TMVCEmbeddedLuaView.Create('', Self, AContext, nil, nil, '');
    try
      View.FileName := AFileName;
      View.SetMVCConfig(FMVCConfig);
      View.Execute;
      AContext.Response.Content := View.Output;
    finally
      View.Free;
    end;
  except
    on E: Exception do
    begin
      AContext.Response.ContentType := TMVCMimeType.TEXT_PLAIN;
      AContext.Response.Content := E.ClassName + ' ' + E.Message;
      AContext.Response.StatusCode := 500;
    end;
  end;
end;

procedure TMVCEngine.FixUpWebModule;
begin
  FSavedOnBeforeDispatch := FWebModule.BeforeDispatch;
  FWebModule.BeforeDispatch := OnBeforeDispatch;
end;

function TMVCEngine.GetBinVersion: string;
begin
  raise Exception.Create('Not implemented');
end;

class function TMVCEngine.GetCurrentSession(Config: TMVCConfig; const AWebRequest: TWebRequest;
  const AWebResponse: TWebResponse; const BindToThisSessionID: string; ARaiseExceptionIfExpired: boolean)
  : TWebSession;
var
  SessionID: string;
  List: TObjectDictionary<string, TWebSession>;
  IsExpired: boolean;
begin
  List := SessionList;
  TMonitor.Enter(List);
  try
    Result := nil;

    if BindToThisSessionID.IsEmpty then
    begin
      SessionID := AWebRequest.CookieFields.Values[TMVCConstants.SESSION_TOKEN_NAME];
      if not SessionID.IsEmpty then
        SessionID := TIdURI.URLDecode(SessionID);
    end
    else
    begin
      SessionID := BindToThisSessionID;
    end;

    { SESSION IS NOT AUTOCREATED BY DEFAULT }
    if not SessionID.IsEmpty then
    begin

      IsExpired := true;
      if List.TryGetValue(SessionID, Result) then
      begin
        IsExpired := MinutesBetween(now, Result.LastAccess) > StrToInt(Config.Value['sessiontimeout']);
      end;

      if Assigned(Result) then
      begin
        if IsExpired then
        begin
          List.Remove(SessionID); // remove expired session from session list
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

function MSecToTime(mSec: Int64): string;
const
  secondTicks = 1000;
  minuteTicks = 1000 * 60;
  hourTicks = 1000 * 60 * 60;
  dayTicks = 1000 * 60 * 60 * 24;
var
  D, H, M, S: string;
  ZD, ZH, ZM, ZS: Integer;
begin
  ZD := mSec div dayTicks;
  Dec(mSec, ZD * dayTicks);
  ZH := mSec div hourTicks;
  Dec(mSec, ZH * hourTicks);
  ZM := mSec div minuteTicks;
  Dec(mSec, ZM * minuteTicks);
  ZS := mSec div secondTicks;
  D := IntToStr(ZD);
  H := IntToStr(ZH);
  M := IntToStr(ZM);
  S := IntToStr(ZS);
  Result := D + '.' + H + ':' + M + ':' + S;
end;

function TMVCEngine.GetUpTime: string;
begin
  Result := MSecToTime(GetTickCount);
end;

procedure TMVCEngine.HandleBuiltInMethods(const AWebRequest: TWebRequest; const AWebResponse: TWebResponse);
var
  j: TJSONObject;
  c: TMVCControllerClass;
  _type: TRttiInstanceType;
  _method: TRttiMethod;
  _methods: TArray<TRttiMethod>;
  ControllerInfo: TJSONObject;
  jmethod: TJSONObject;
  _a: TCustomAttribute;
  methods: TJSONArray;
  FoundAttrib: boolean;
  StrRelativePath: string;
  StrHTTPMethods: string;
  StrConsumes: string;
  StrProduces: string;
begin
  { TODO -oDaniele -cGeneral : Please! Use a register here!! }
  if LowerCase(string(AWebRequest.PathInfo)) = '/describeserver.info' then
  begin
    j := TJSONObject.Create;
    try
      for c in FControllers do
      begin
        ControllerInfo := TJSONObject.Create;
        j.AddPair(c.QualifiedClassName, ControllerInfo);

        _type := ctx.GetType(c) as TRttiInstanceType;
        for _a in _type.GetAttributes do
        begin
          if _a is MVCPathAttribute then
            ControllerInfo.AddPair('ResourcePath', MVCPathAttribute(_a).Path)
        end;

        methods := TJSONArray.Create;
        ControllerInfo.AddPair('Actions', methods);
        _methods := _type.GetDeclaredMethods;
        for _method in _methods do
        begin
          FoundAttrib := false;
          StrRelativePath := '';
          StrHTTPMethods := '';
          StrConsumes := '';
          StrProduces := '';
          for _a in _method.GetAttributes do
          begin
            if _a is MVCPathAttribute then
            begin
              StrRelativePath := MVCPathAttribute(_a).Path;
              FoundAttrib := true;
            end;
            if _a is MVCHTTPMethodAttribute then
            begin
              StrHTTPMethods := MVCHTTPMethodAttribute(_a).MVCHTTPMethodsAsString;
              FoundAttrib := true;
            end;
            if _a is MVCConsumesAttribute then
            begin
              StrConsumes := MVCConsumesAttribute(_a).Value;
              FoundAttrib := true;
            end;
            if _a is MVCProducesAttribute then
            begin
              StrProduces := MVCProducesAttribute(_a).Value;
              FoundAttrib := true;
            end;
          end;

          if FoundAttrib then
          begin
            jmethod := TJSONObject.Create;
            jmethod.AddPair('ActionName', _method.Name);
            jmethod.AddPair('RelativePath', StrRelativePath);
            jmethod.AddPair('Consumes', StrConsumes);
            jmethod.AddPair('Produces', StrProduces);
            jmethod.AddPair('HTTPMethods', StrHTTPMethods);
            methods.AddElement(jmethod);
          end;
        end;
      end;
      AWebResponse.ContentType := TMVCMimeType.APPLICATION_JSON;
      AWebResponse.Content := j.ToString;
      AWebResponse.StatusCode := 200;
    finally
      j.Free;
    end;
  end
  else if LowerCase(string(AWebRequest.PathInfo)) = '/describeplatform.info' then
  begin
    j := TJSONObject.Create;
    try
      j.AddPair('os', TOSVersion.ToString);
      // j.AddPair('binversion', GetBinVersion());
      j.AddPair('CPUs', TJSONNumber.Create(TThread.ProcessorCount));
      j.AddPair('CPU_architecture', IntToStr(Ord(TOSVersion.Architecture)) +
        ' /*(0=Intelx86; 1=Intelx64 2=ARM32)*/');
      j.AddPair('uptime', GetUpTime);

      AWebResponse.ContentType := TMVCMimeType.APPLICATION_JSON;
      AWebResponse.Content := j.ToString;
      AWebResponse.StatusCode := 200;
    finally
      j.Free;
    end;
  end
  else if LowerCase(string(AWebRequest.PathInfo)) = '/serverconfig.info' then
  begin
    AWebResponse.ContentType := TMVCMimeType.APPLICATION_JSON;
    AWebResponse.Content := Config.ToString.Replace('\', '\\', [rfReplaceAll]);
    AWebResponse.StatusCode := 200;
  end;
end;

procedure TMVCEngine.Http404(AWebContext: TWebContext);
begin
  AWebContext.Response.StatusCode := 404;
  AWebContext.Response.ReasonString := 'Not Found';
  AWebContext.Response.Content := 'Not Found';
end;

procedure TMVCEngine.Http500(AWebContext: TWebContext; AReasonText: string);
begin
  AWebContext.Response.StatusCode := 500;
  AWebContext.Response.ReasonString := 'Internal server error: ' + AReasonText;
  AWebContext.Response.Content := 'Internal server error: ' + AReasonText;
end;

function TMVCEngine.IsBuiltInMethod(const AWebRequest: TWebRequest; const AWebResponse: TWebResponse)
  : boolean;
begin
  Result := (LowerCase(AWebRequest.PathInfo) = '/describeserver.info') or
    (LowerCase(AWebRequest.PathInfo) = '/describeplatform.info') or
    (LowerCase(AWebRequest.PathInfo) = '/serverconfig.info');
end;

procedure TMVCEngine.LoadSystemControllers;
begin
  // AddController(TMVCStaticContents); //--daniele Static files are not handled directly by the router
  AddController(TMVCBUSController);
end;

procedure TMVCEngine.OnBeforeDispatch(Sender: TObject; Request: TWebRequest; Response: TWebResponse;
  var Handled: boolean);
begin
  Handled := false;
  if Assigned(FSavedOnBeforeDispatch) then
    FSavedOnBeforeDispatch(Sender, Request, Response, Handled);
  // _Request := Request as TIdHTTPAppRequest;
  if not Handled then
  begin
    try
      // "X-Requested-With", "XMLHttpRequest"
      ExecuteAction(Sender, Request, Response);
    except
      on E: Exception do
      begin
        LogException(E);
        // Response.ContentStream.Size := 0;
        Response.Content := E.Message;
        Response.SendResponse;
      end;
    end;
    Handled := true;
  end;
end;

procedure TMVCEngine.ResponseErrorPage(E: Exception; Request: TWebRequest; Response: TWebResponse);
begin
  Response.SetCustomHeader('x-mvc-error', E.ClassName + ': ' + E.Message);
  Response.StatusCode := 200;
  if Pos('text/html', LowerCase(Request.Accept)) = 1 then
  begin
    Response.ContentType := 'text/plain';
    Response.Content := 'DelphiMVCFramework ERROR:' + sLineBreak + 'Exception raised of class: ' + E.ClassName +
      sLineBreak + '***********************************************' + sLineBreak + E.Message + sLineBreak +
      '***********************************************';
  end
  else
  begin
    Response.ContentType := 'text/plain';
    Response.Content := 'DelphiMVCFramework ERROR:' + sLineBreak + 'Exception raised of class: ' + E.ClassName +
      sLineBreak + '***********************************************' + sLineBreak + E.Message + sLineBreak +
      '***********************************************';
  end;
end;

procedure TMVCEngine.SetApplicationSession(const Value: TWebApplicationSession);
begin
  FApplicationSession := Value;
end;

{ TWebContext }

constructor TWebContext.Create(ARequest: TWebRequest; AResponse: TWebResponse);
begin
  inherited Create;

  if IsLibrary then
  begin
{$IF Defined(VER270)}
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

{$IFDEF IOCP}
    FRequest := TMVCIOCPWebRequest.Create(ARequest);

{$ELSE}
    FRequest := TMVCINDYWebRequest.Create(ARequest);
{$ENDIF}
  end;
  FResponse := TMVCWebResponse.Create(AResponse);
  FData := TDictionary<string, string>.Create;
end;

destructor TWebContext.Destroy;
begin
  FreeAndNil(FResponse);
  FreeAndNil(FRequest);
  FreeAndNil(FData);
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

procedure TWebContext.SetParams(AParamsTable: TMVCRequestParamsTable);
begin
  FParamsTable := AParamsTable;
  FRequest.FParamsTable := AParamsTable;
end;

{ TMVCWebResponse }

constructor TMVCWebResponse.Create(AWebResponse: TWebResponse);
begin
  FStreamOutputDone := false;
  inherited Create;
  FWebResponse := AWebResponse;
end;

destructor TMVCWebResponse.Destroy;
begin
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

procedure TMVCWebResponse.SetContentStream(AStream: TStream; AContentType: string);
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
  FReasonString := Value;
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
  InEnc: TEncoding;
  Buffer: TArray<Byte>;
  I: Integer;
begin
  // Property FWebRequest.Content is broken. It doesn't correctly decode the response body
  // considering the content charser. So, here's the fix

  // check http://msdn.microsoft.com/en-us/library/dd317756(VS.85).aspx
  if FCharset.IsEmpty then
  begin
    SetLength(Buffer, 10);
    for I := 0 to 9 do
    begin
      Buffer[I] := Byte(FWebRequest.RawContent[I]);
    end;
    TEncoding.GetBufferEncoding(Buffer, InEnc, TEncoding.Default);
    SetLength(Buffer, 0);
  end
  else
    InEnc := TEncoding.GetEncoding(FCharset);
  try
    Buffer := TEncoding.Convert(InEnc, TEncoding.Default, TBytes(FWebRequest.RawContent));
    Result := TEncoding.Default.GetString(Buffer);
  finally
    InEnc.Free;
  end
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
        Result := Mapper.JSONObjectToObject<T>(BodyAsJSONObject.Get(S).JsonValue as TJSONObject)
      else
        raise EMVCException.CreateFmt('Body property %s not valid', [RootProperty]);
    end;
  end
  else
    raise EMVCException.CreateFmt('Body ContentType %s not supported', [ContentType]);
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

function TMVCWebRequest.BodyAsListOf<T>(const RootProperty: string): TObjectList<T>;
var
  S: string;
begin
  if ContentType.Equals(TMVCMimeType.APPLICATION_JSON) then
  begin
    if RootProperty = '' then
      Result := Mapper.JSONArrayToObjectList<T>(BodyAsJSONValue as TJSONArray)
    else
    begin
      S := Mapper.GetStringDef(BodyAsJSONObject, RootProperty, '');
      if not S.IsEmpty then
        Result := Mapper.JSONArrayToObjectList<T>(BodyAsJSONObject.Get(S).JsonValue as TJSONArray)
      else
        raise EMVCException.CreateFmt('Body property %s not valid', [RootProperty]);
    end;
  end
  else
    raise EMVCException.CreateFmt('Body ContentType %s not supported', [ContentType]);
end;

function TMVCWebRequest.ClientPrefer(MimeType: string): boolean;
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
  c := AWebRequest.GetFieldByName('Content-Type');
  if not c.IsEmpty then
  begin
    CT := c.Split([';']);
    FContentType := trim(CT[0]);
    FCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET; // default charset
    if Length(CT) > 1 then
    begin
      if CT[1].trim.StartsWith('charset', true) then
      begin
        FCharset := CT[1].trim.Split(['='])[1].trim;
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

procedure TMVCController.BindToSession(SessionID: string);
begin
  if not Assigned(FWebSession) then
  begin
    FWebSession := TMVCEngine.GetCurrentSession(GetMVCConfig, Context.Request.FWebRequest,
      Context.Response.FWebResponse, SessionID, false);
    if not Assigned(FWebSession) then
      raise EMVCException.Create('Invalid SessionID');
    FWebSession.MarkAsUsed;
    SendSessionCookie(SessionID);
  end
  else
    raise EMVCException.Create('Session already bounded for this request');
end;

constructor TMVCController.Create;
begin
  inherited Create;
  IsSessionStarted := false;
  SessionMustBeClose := false;
  FContentCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET;
end;

destructor TMVCController.Destroy;
begin
  FreeAndNil(FResponseStream);
  FreeAndNil(FViewDataSets);
  FreeAndNil(FViewModel);
  inherited;
end;

function TMVCController.GetClientID: string;
begin
  if Session['username'].IsEmpty then
    raise EMVCException.Create('Messaging extensions require a valid "username" key in session');
  Result := Session['username'];
  // + IntToStr(GetTickCount);
end;

procedure TMVCController.EnqueueMessageOnTopic(const ATopic: string; AJSONObject: TJSONObject;
  AOwnsInstance: boolean);
var
  Stomp: IStompClient;
  H: IStompHeaders;
  msg: TJSONObject;
begin
  msg := TJSONObject.Create;
  try
    if AOwnsInstance then
      msg.AddPair('message', AJSONObject)
    else
      msg.AddPair('message', AJSONObject.Clone as TJSONObject);

    msg.AddPair('_topic', ATopic).AddPair('_username', GetClientID)
      .AddPair('_timestamp', FormatDateTime('YYYY-MM-DD HH:NN:SS', now));

    Stomp := GetNewStompClient(GetClientID);
    H := StompUtils.NewHeaders.Add(TStompHeaders.NewPersistentHeader(true));
    Stomp.Send(ATopic, msg.ToString, H);
    TThread.Sleep(100);
    // single user cannot enqueue more than 10 message in noe second...
    // it is noot too much elegant, but it works as DoS protection
  finally
    msg.Free;
  end;
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
  Result := StompUtils.NewStomp(Config[TMVCConfigKey.StompServer],
    StrToInt(Config[TMVCConfigKey.StompServerPort]),
    GetClientID, Config[TMVCConfigKey.StompUsername], Config[TMVCConfigKey.StompPassword]);
end;

function TMVCController.GetWebSession: TWebSession;
begin
  if not Assigned(FWebSession) then
  begin
    FWebSession := TMVCEngine.GetCurrentSession(GetMVCConfig, Context.Request.FWebRequest,
      Context.Response.FWebResponse, '', false);
    if not Assigned(FWebSession) then
      SessionStart
      // else
      // SendSessionCookie(FWebSession.SessionID); //daniele
  end;
  Result := FWebSession;
  Result.MarkAsUsed;
end;

procedure TMVCController.LoadView(const ViewName: string);
var
  View: TMVCEmbeddedLuaView;
begin
  try
    View := TMVCEmbeddedLuaView.Create(ViewName, GetMVCEngine, FContext, FViewModel, FViewDataSets,
      ContentType);
    try
      View.SetMVCConfig(GetMVCConfig);
      View.Execute;
      Render(View.Output);
    finally
      View.Free;
    end;
  except
    on E: Exception do
    begin
      ContentType := 'text/plain';
      Render(E);
    end;
  end;
end;

procedure TMVCController.MVCControllerAfterCreate;
begin
  inherited;
end;

procedure TMVCController.MVCControllerBeforeDestroy;
begin
  inherited;
end;

procedure TMVCController.OnAfterAction(Context: TWebContext; const AActionNAme: string);
begin
  // do nothing
end;

procedure TMVCController.OnBeforeAction(Context: TWebContext; const AActionNAme: string;
  var Handled: boolean);
begin
  Handled := false;
  if ContentType.IsEmpty then
    ContentType := GetMVCConfig[TMVCConfigKey.DefaultContentType];
end;

procedure TMVCController.PushDataSetToView(const AModelName: string; ADataSet: TDataSet);
begin
  if not Assigned(FViewDataSets) then
    FViewDataSets := TObjectDictionary<string, TDataSet>.Create;
  FViewDataSets.Add(AModelName, ADataSet);
end;

procedure TMVCController.PushJSONToView(const AModelName: string; AModel: TJSONValue);
begin
  if not Assigned(FViewModel) then
    FViewModel := TMVCDataObjects.Create;
  FViewModel.Add(AModelName, AModel);
end;

procedure TMVCController.PushModelToView(const AModelName: string; AModel: TObject);
begin
  if not Assigned(FViewModel) then
    FViewModel := TMVCDataObjects.Create;
  FViewModel.Add(AModelName, AModel);
end;

procedure InternalRenderText(const AContent: string; ContentType, ContentEncoding: string;
  Context: TWebContext);
var
  OutEncoding: TEncoding;
begin
  OutEncoding := TEncoding.GetEncoding(ContentEncoding);
  try
    // Context.Response.RawWebResponse.ContentStream := TStringStream.Create(UTF8Encode(AContent));
    if SameText('UTF-8', ContentEncoding) then
    begin
      Context.Response.RawWebResponse.Content := '';
      Context.Response.RawWebResponse.ContentStream := TStringStream.Create(UTF8Encode(AContent));
    end
    else
    begin
      Context.Response.RawWebResponse.Content :=
        OutEncoding.GetString(
          TEncoding.Convert(
            TEncoding.UTF8,
            OutEncoding,
            TEncoding.Default.GetBytes(AContent)
          )
        );
    end;
  finally
    OutEncoding.Free;
  end;
  Context.Response.RawWebResponse.ContentType := ContentType + '; charset=' + ContentEncoding;
  // Context.Response.RawWebResponse.ContentType := TMVCMimeType.APPLICATION_JSON;
  // Context.Response.RawWebResponse.ContentEncoding := ContentEncoding;
  // OutEncoding := TEncoding.GetEncoding(ContentEncoding);
  // InEncoding := TEncoding.Default; // GetEncoding(S);
  // Context.Response.Content := OutEncoding.GetString
  // (TEncoding.Convert(InEncoding, OutEncoding, InEncoding.GetBytes(AContent)));
  // OutEncoding.Free;
end;

procedure InternalRender(AJSONValue: TJSONValue; ContentType, ContentEncoding: string;
  Context: TWebContext;
  AInstanceOwner: boolean);
var
  OutEncoding: TEncoding;
begin
  OutEncoding := TEncoding.GetEncoding(ContentEncoding);
  try
    Context.Response.RawWebResponse.Content :=
      OutEncoding.GetString(TEncoding.Convert(TEncoding.Default, OutEncoding,
      TEncoding.Default.GetBytes(AJSONValue.ToString)));
  finally
    OutEncoding.Free;
  end;
  Context.Response.RawWebResponse.ContentType := ContentType + '; charset=' + ContentEncoding;

  // Context.Response.RawWebResponse.StatusCode := 200;

  {
    Context.Response.RawWebResponse.ContentType := TMVCMimeType.APPLICATION_JSON;
    //Context.Response.RawWebResponse.ContentEncoding := ContentEncoding;
    S := AJSONValue.ToString;
    OutEncoding := TEncoding.GetEncoding(ContentEncoding);
    InEncoding := TEncoding.Default;
    Context.Response.RawWebResponse.Content := OutEncoding.GetString
    (TEncoding.Convert(InEncoding, OutEncoding, InEncoding.GetBytes(S)));
    OutEncoding.Free;
    Context.Response.RawWebResponse.Content := s;
  }
  if AInstanceOwner then
    FreeAndNil(AJSONValue)
end;

procedure InternalRender(const Content: string; ContentType, ContentEncoding: string;
  Context: TWebContext);
begin
  if ContentType = TMVCMimeType.APPLICATION_JSON then
  begin
    InternalRender(TJSONString.Create(Content), ContentType, ContentEncoding, Context, true);
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

procedure TMVCController.Render(const Content: string);
begin
  InternalRender(Content, ContentType, ContentCharset, Context);
end;

procedure TMVCController.Render(AObject: TObject; AInstanceOwner: boolean);
var
  JSON: TJSONObject;
begin
  JSON := Mapper.ObjectToJSONObject(AObject);
  Render(JSON, true);
  if AInstanceOwner then
    FreeAndNil(AObject);
end;

procedure TMVCController.SendFile(AFileName: string);

begin
  TMVCStaticContents.SendFile(AFileName, ContentType, Context);
  /// ///daniele
  // S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  // FContext.Response.SetContentStream(S, ContentType);

end;

procedure TMVCController.SendSessionCookie(const SessionID: string);
var
  Cookie: TCookie;
begin
  Cookie := FContext.FResponse.Cookies.Add;
  Cookie.Name := TMVCConstants.SESSION_TOKEN_NAME;
  Cookie.Value := SessionID;
  Cookie.Expires := now + OneHour * 24 * 365;
  // OneMinute * strtoint(GetMVCConfig['sessiontimeout']);
  Cookie.Path := '/';
end;

procedure TMVCController.SendStream(AStream: TStream);
begin
  FContext.Response.FWebResponse.Content := '';
  // FContext.Response.SetContentStream(AStream, ContentType);
  FContext.Response.FWebResponse.ContentType := ContentType;
  FContext.Response.FWebResponse.ContentStream := AStream;
  FContext.Response.FWebResponse.FreeContentStream := true;
end;

procedure TMVCController.SessionStart;
var
  Sess: TWebSession;
  SessionID: string;
begin
  if not Assigned(FWebSession) then
  begin
    SessionID := StringReplace(StringReplace(StringReplace(GUIDToString(TGUID.NewGuid), '}', '', []), '{',
      '',
      []), '-',
      '', [rfReplaceAll]);

    SendSessionCookie(SessionID);

    TMonitor.Enter(SessionList);
    try
      Sess := TMVCSessionFactory.GetInstance.CreateNewByType('memory', SessionID,
        StrToInt64(Config[TMVCConfigKey.SessionTimeout]));
      SessionList.Add(SessionID, Sess);
      FWebSession := Sess;
      Sess.MarkAsUsed;
    finally
      TMonitor.Exit(SessionList);
    end;
    IsSessionStarted := true;
    SessionMustBeClose := false;
  end;
end;

procedure TMVCController.SessionStop(ARaiseExceptionIfExpired: boolean);
var
  Cookie: TCookie;
begin
  // Set-Cookie: token=deleted; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT
  FContext.FResponse.Cookies.Clear; // daniele ... remove all previous cookies
  Cookie := FContext.FResponse.Cookies.Add;
  Cookie.Name := TMVCConstants.SESSION_TOKEN_NAME;

  // rubbish... invalid the cookie value
  Cookie.Value := GUIDToString(TGUID.NewGuid) + 'invalid' + GUIDToString(TGUID.NewGuid);
  Cookie.Expires := EncodeDate(1970, 1, 1);
  Cookie.Path := '/';

  TMonitor.Enter(SessionList);
  try
    if not Assigned(FWebSession) then
      FWebSession := TMVCEngine.GetCurrentSession(GetMVCConfig, Context.Request.FWebRequest,
        Context.Response.FWebResponse, '', ARaiseExceptionIfExpired);
    if Assigned(FWebSession) then
      SessionList.Remove(Session.SessionID);
  finally
    TMonitor.Exit(SessionList);
  end;

  IsSessionStarted := false;
  SessionMustBeClose := true;
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
  if Assigned(FWebSession) then
    raise EMVCException.Create('Web Session already set for controller ' + ClassName);
  FWebSession := Value;
  IsSessionStarted := Assigned(FWebSession);
end;

{ TMVCPathAttribute }

constructor MVCPathAttribute.Create(const Value: string);
begin
  inherited Create;
  FPath := Value;
end;

function TMVCWebRequest.Param(Name: string): string;
begin
  Result := FWebRequest.QueryFields.Values[name];
end;

function TMVCWebRequest.QueryStringParam(Name: string): string;
begin
  Result := FWebRequest.QueryFields.Values[name];
end;

function TMVCWebRequest.QueryStringParamExists(Name: string): boolean;
begin
  Result := not QueryStringParam(name).IsEmpty;
end;

function TMVCWebRequest.GetClientPreferHTML: boolean;
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

function TMVCWebRequest.GetHeaderValue(const Name: string): string;
var
  S: string;
begin
  S := GetHeader(name);
  if S.IsEmpty then
    Result := ''
  else
    Result := S.Split([':'])[1].trim;
end;

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

function TMVCWebRequest.GetIsAjax: boolean;
begin
  Result := LowerCase(FWebRequest.GetFieldByName('X-Requested-With')) = 'xmlhttprequest';
end;

function TMVCWebRequest.GetParamAll(const ParamName: string): string;
begin
  if (not Assigned(FParamsTable)) or (not FParamsTable.TryGetValue(ParamName, Result)) then
  begin
    Result := FWebRequest.QueryFields.Values[ParamName];
    if Result = EmptyStr then
      Result := FWebRequest.ContentFields.Values[ParamName];
    if Result = EmptyStr then
      Result := FWebRequest.CookieFields.Values[ParamName];
  end;
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

procedure TMVCWebRequest.SetPathInfo(const Value: string);
begin
  FPathInfo := Value;
end;

function TMVCWebRequest.ThereIsRequestBody: boolean;
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
      Result := Result + ',' + GetEnumName(TypeInfo(TMVCHTTPMethodType), Ord(I));
    end;
  end;

  if not Result.IsEmpty then
    Result := Result.Remove(0, 1)
  else
    Result := 'any';
end;

{ TMVCStaticContents }
class
  procedure TMVCStaticContents.SendFile(AFileName, AMimeType: string; Context: TWebContext);
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
    LReqDate := GMTToLocalDateTime(Context.Request.Headers['If-Modified-Since']);
    if (LReqDate <> 0) and (abs(LReqDate - LFileDate) < 2 * (1 / (24 * 60 * 60))) then
    begin
      Context.Response.ContentType := AMimeType;
      Context.Response.StatusCode := 304;
    end
    else
    begin
      S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
      Context.Response.SetCustomHeader('Content-Length', IntToStr(S.Size));
      Context.Response.SetCustomHeader('Last-Modified', LocalDateTimeToHttpStr(LFileDate));
      Context.Response.SetContentStream(S, AMimeType);
    end;
  end;
end;

class
  function TMVCStaticContents.IsScriptableFile(StaticFileName: string; Config: TMVCConfig): boolean;
begin
  Result := TPath.GetExtension(StaticFileName).ToLower = '.' +
    Config[TMVCConfigKey.DefaultViewFileExtension].ToLower;
end;

class
  function TMVCStaticContents.IsStaticFile(AViewPath, AWebRequestPath: string;
  out ARealFileName: string): boolean;
var
  FileName: string;
begin
  if TDirectory.Exists(AViewPath) then // absolute path
    FileName := AViewPath + AWebRequestPath.Replace('/', TPath.DirectorySeparatorChar)
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
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(GetApplicationFileName));
end;

function TMVCBase.GetMVCConfig: TMVCConfig;
begin
  if not Assigned(FMVCConfig) then
    EMVCConfigException.Create('MVCConfig not assigned to this ' + ClassName + ' instances');
  Result := FMVCConfig;

end;

function TMVCBase.GetMVCEngine: TMVCEngine;
begin
  Result := FMVCEngine;
end;

{ TMVCISAPIWebRequest }

function TMVCISAPIWebRequest.ClientIP: string;
begin
  raise EMVCException.Create('<TMVCISAPIWebRequest.ClientIP> Not implemented');
end;

constructor TMVCISAPIWebRequest.Create(AWebRequest: TWebRequest);
begin
  inherited;
  FWebRequest := AWebRequest as TISAPIRequest;
end;

{ TMVCApacheWebRequest }
{$IF Defined(VER270)}


function TMVCApacheWebRequest.ClientIP: string;
begin
  raise EMVCException.Create('<TMVCApacheWebRequest.ClientIP> Not implemented');
end;

constructor TMVCApacheWebRequest.Create(AWebRequest: TWebRequest);
begin
  inherited;
  FWebRequest := AWebRequest as TApacheRequest;
end;
{$ENDIF}
{ TMVCINDYWebRequest }

function TMVCINDYWebRequest.ClientIP: string;
{
  This code has been converted to Delphi from a PHP code
  http://www.grantburton.com/2008/11/30/fix-for-incorrect-ip-addresses-in-wordpress-comments/
}
  function CheckIP(IP: string): boolean;
  begin
    Result := (not IP.IsEmpty) and { (IP2Long(IP) <> -1) and }
      (IP2Long(IP) > 0);
  end;

var
  S: string;
  req: TIdHTTPAppRequestHack;

{$IFDEF IOCP}
  Headers: TStringList;

{$ELSE}
  Headers: TIdHeaderList;

{$ENDIF}
begin
  req := TIdHTTPAppRequestHack(FWebRequest);

{$IFDEF IOCP}
  Headers := req.FHttpConnection.RequestHeader;

{$ELSE}
  Headers := req.FRequestInfo.RawHeaders;

{$ENDIF}
  if CheckIP(Headers.Values['HTTP_CLIENT_IP']) then
    Exit(Headers.Values['HTTP_CLIENT_IP']);

  for S in Headers.Values['HTTP_X_FORWARDED_FOR'].Split([',']) do
  begin
    if CheckIP(S.trim) then
      Exit(S.trim);
  end;

  if CheckIP(Headers.Values['HTTP_X_FORWARDED']) then
    Exit(Headers.Values['HTTP_X_FORWARDED']);

  if CheckIP(Headers.Values['HTTP_X_CLUSTER_CLIENT_IP']) then
    Exit(Headers.Values['HTTP_X_CLUSTER_CLIENT_IP']);

  if CheckIP(Headers.Values['HTTP_FORWARDED_FOR']) then
    Exit(Headers.Values['HTTP_FORWARDED_FOR']);

  if CheckIP(Headers.Values['HTTP_FORWARDED']) then
    Exit(Headers.Values['HTTP_FORWARDED']);

  if CheckIP(Headers.Values['REMOTE_ADDR']) then
    Exit(Headers.Values['REMOTE_ADDR']);

  if CheckIP(FWebRequest.RemoteIP) then
    Exit(FWebRequest.RemoteIP);

  if CheckIP(FWebRequest.RemoteAddr) then
    Exit(FWebRequest.RemoteAddr);

  if CheckIP(FWebRequest.RemoteHost) then
    Exit(FWebRequest.RemoteHost);

  Result := '';
end;

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

function TMVCController.ReceiveMessageFromTopic(const ATopic: string; ATimeout: Int64;
  var JSONObject: TJSONObject): boolean;
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
        raise EMVCException.Create('Message is a JSONValue but not a JSONObject')
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
    ResponseStatusCode(EMVCException(E).HTTPErrorCode)
  else
  begin
    if Context.Response.StatusCode = 200 then
      ResponseStatusCode(500);
  end;

  if (not Context.Request.IsAjax) and (Context.Request.ClientPreferHTML) then
  begin
    ContentType := TMVCMimeType.TEXT_HTML;
    ContentCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET;
    ResponseStream.Clear;

    ResponseStream.Append
      ('<html><head><style>pre { color: #000000; background-color: #d0d0d0; }</style></head><body>')
      .Append('<h1>DMVCFramework: Error Raised</h1>').AppendFormat('<pre>HTTP Return Code: %d' + sLineBreak,
      [Context.Response.StatusCode]).AppendFormat('HTTP Reason Text: "%s"</pre>',
      [Context.Response.ReasonString])
      .Append('<h3><pre>').AppendFormat('Exception Class Name : %s' + sLineBreak, [E.ClassName])
      .AppendFormat('Exception Message    : %s' + sLineBreak, [E.Message]).Append('</pre></h3>');
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

procedure TMVCController.Render(const AErrorCode: UInt16; const AErrorMessage: string);
var
  j: TJSONObject;
  status: string;
begin
  ResponseStatusCode(AErrorCode);
  if Context.Request.IsAjax or (ContentType = 'application/json') then
  begin
    status := 'error';
    if (AErrorCode div 100) = 2 then
      status := 'ok';
    j := TJSONObject.Create;
    j.AddPair('status', status);
    j.AddPair('classname', TJSONNull.Create);
    j.AddPair('message', AErrorMessage);
    Render(j);
  end
  else
  begin
    Render(Format('Error: [%d] %s', [AErrorCode, AErrorMessage]));
  end;
end;

procedure TMVCController.Render(ADataSet: TDataSet; AInstanceOwner: boolean; AOnlySingleRecord: boolean);
var
  arr: TJSONArray;
  JObj: TJSONObject;
begin
  if ContentType = TMVCMimeType.APPLICATION_JSON then
  begin
    if not AOnlySingleRecord then
    begin
      ADataSet.First;
      arr := TJSONArray.Create;
      Mapper.DataSetToJSONArray(ADataSet, arr, AInstanceOwner);
      Render(arr);
    end
    else
    begin
      JObj := TJSONObject.Create;
      Mapper.DataSetToJSONObject(ADataSet, JObj, AInstanceOwner);
      Render(JObj);
    end;
  end
  else
    raise Exception.Create('ContentType not supported for this render [' + ContentType + ']');
  // if ContentType = TMVCMimeType.TEXT_XML then
  // begin
  // Mapper.DataSetToXML(ADataSet, S, AInstanceOwner);
  // Render(S);
  // end;
end;

procedure TMVCController.Render<T>(ACollection: TObjectList<T>; AInstanceOwner: boolean;
  AJSONObjectActionProc: TJSONObjectActionProc);
var
  JSON: TJSONArray;
begin
  JSON := Mapper.ObjectListToJSONArray<T>(ACollection, false, AJSONObjectActionProc);
  Render(JSON, true);
  if AInstanceOwner then
    FreeAndNil(ACollection);
end;

procedure TMVCController.RenderListAsProperty<T>(const APropertyName: string;
  AObjectList: TObjectList<T>;
  AOwnsInstance: boolean; AJSONObjectActionProc: TJSONObjectActionProc);
begin
  Render(TJSONObject.Create(TJSONPair.Create(APropertyName, Mapper.ObjectListToJSONArray<T>(AObjectList,
    AOwnsInstance,
    AJSONObjectActionProc))));
end;

procedure TMVCController.RenderStreamAndFree(const AStream: TStream);
begin
  SendStream(AStream);
end;

procedure TMVCController.Render(AJSONValue: TJSONValue; AInstanceOwner: boolean);
begin
  InternalRender(AJSONValue, ContentType, ContentCharset, Context, AInstanceOwner);
end;

procedure TMVCController.ResponseStatusCode(const ErrorCode: UInt16);
begin
  Context.Response.StatusCode := ErrorCode;
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

procedure TMVCController.Render(const AErrorCode: UInt16; AJSONValue: TJSONValue;
  AInstanceOwner: boolean);
begin
  ResponseStatusCode(AErrorCode);
  if ContentType = 'application/json' then
  begin
    Render(AJSONValue, AInstanceOwner);
  end
  else
  begin
    raise EMVCException.Create('Cannot render a JSONValue if ContentType is not application/json');
  end;

end;

procedure TMVCController.Render(const AErrorCode: UInt16; AObject: TObject; AInstanceOwner: boolean);
begin
  Render(AErrorCode, Mapper.ObjectToJSONObject(AObject), true);
  if AInstanceOwner then
    AObject.Free;
end;

procedure TMVCController.Render;
begin
  Render(ResponseStream.ToString);
end;

{$IFDEF IOCP}


constructor TMVCIOCPWebRequest.Create(AWebRequest: TWebRequest);
begin
  inherited;
  FWebRequest := AWebRequest as TIocpWebRequest;
end;

{$ENDIF}
{ MVCStringAttribute }

constructor MVCStringAttribute.Create(const Value: string);
begin
  inherited Create;
  FValue := Value;
end;

function IsShuttingDown: boolean;
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

initialization

_IsShuttingDown := 0;

end.
