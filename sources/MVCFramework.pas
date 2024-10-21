// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file:
// Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
// João Antônio Duarte (https://github.com/joaoduarte19)
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
{$IF IOS}
{$MESSAGE Fatal 'This unit is not compilable on iOS'}
{$ENDIF}
{$RTTI EXPLICIT
  METHODS(DefaultMethodRttiVisibility)
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
  JSONDataObjects,
  Data.DB,
  MVCFramework.Session,
  MVCFramework.DuckTyping,
  MVCFramework.Logger,
  MVCFramework.Container,
  MVCFramework.ApplicationSession,
  MVCFramework.Serializer.Intf,

{$IF Defined(WEBAPACHEHTTP)}
  Web.ApacheHTTP,
  // Apache Support since XE6 http://docwiki.embarcadero.com/Libraries/XE6/de/Web.ApacheHTTP

{$ENDIF}

  // Delphi XE4 (all update) and XE5 (with no update) don't contains this unit. Look for the bug in QC
  // https://quality.embarcadero.com/browse/RSP-17216

{$IF NOT Defined(MOBILE)} // file upload is not supported on mobile
{$IF Defined(SeattleOrBetter)}
  Web.ReqMulti,
{$ELSE}
  ReqMulti,
{$ENDIF}
{$ENDIF}
  Web.HTTPApp,

{$IF Defined(MSWINDOWS)}
  Web.Win.IsapiHTTP,
{$ENDIF}
  Web.WebReq,
  LoggerPro,
  IdGlobal,
  IdGlobalProtocols,
  Swag.Doc,
  Swag.Common.Types,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons;

type

  TSessionData = TDictionary<string, string>;
  TMVCCustomData = TSessionData;
  TMVCBaseViewEngine = class;
  TMVCViewEngineClass = class of TMVCBaseViewEngine;

  MVCBaseAttribute = class(TCustomAttribute)

  end;

  MVCRequiresAuthenticationAttribute = class(MVCBaseAttribute)

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

  MVCIntegerAttribute = class(MVCBaseAttribute)
  private
    FValue: Int64;
  protected
    { protected declarations }
  public
    constructor Create(const AValue: Int64);
    property Value: Int64 read FValue;
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
    FCharset: string;
  protected
    { protected declarations }
  public
    constructor Create(const AValue: string); overload;
    constructor Create(const AValue: string; const ACharset: string); overload;
    property Charset: string read FCharset;
  end;

  MVCDocAttribute = class(MVCStringAttribute)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  MVCFormatAttribute = class(MVCStringAttribute)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  MVCMaxLengthAttribute = class(MVCIntegerAttribute)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  MVCMinimumAttribute = class(MVCIntegerAttribute)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  MVCMaximumAttribute = class(MVCIntegerAttribute)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  MVCInheritableAttribute = class(MVCBaseAttribute)
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

  MVCResponseAttribute = class(MVCBaseAttribute)
  private
    FStatusCode: Integer;
    FDescription: string;
    FResponseClass: TClass;
  protected
    { protected declarations }
  public
    constructor Create(inStatusCode: Integer; const inDescription: string;
      inResponseClass: TClass = nil); overload;
    property StatusCode: Integer read FStatusCode;
    property Description: string read FDescription;
    property ResponseClass: TClass read FResponseClass;
  end;

  MVCResponseListAttribute = class(MVCBaseAttribute)
  private
    FStatusCode: Integer;
    FDescription: string;
    FResponseClass: TClass;
  protected
    { protected declarations }
  public
    constructor Create(inStatusCode: Integer; const inDescription: string;
      inResponseClass: TClass = nil); overload;
    property StatusCode: Integer read FStatusCode;
    property Description: string read FDescription;
    property ResponseClass: TClass read FResponseClass;
  end;

  MVCPathParamAttribute = class(MVCBaseAttribute)
  private
    FType: TSwagTypeParameter;
    FFormat: string;
    FValue: string;
  public
    constructor Create(AType: TSwagTypeParameter; APattern: string = ''; AFormat: string = '');
    property ParamType: TSwagTypeParameter read FType;
    property Format: string read FFormat;
    property Pattern: string read FValue;
  end;

  MVCParamAttribute = class(MVCStringAttribute)
  private
    FName: string;
    FLocation: TSwagRequestParameterInLocation;
    FType: TSwagTypeParameter;
    FClassType: TClass;
    FPattern: string;
    FFormat: string;
  public
    property name: string read FName write FName;
    property Location: TSwagRequestParameterInLocation read FLocation write FLocation;
    property ParamType: TSwagTypeParameter read FType write FType;
    property ClassType: TClass read FClassType write FClassType;
    property Pattern: string read FPattern write FPattern;
    property Format: string read FFormat write FFormat;

    constructor Create(name: string; Location: TSwagRequestParameterInLocation;
      AType: TSwagTypeParameter; APattern: string = ''; AFormat: string = ''); overload;
    constructor Create(name: string; Location: TSwagRequestParameterInLocation; AType: TClass;
      APattern: string = ''; AFormat: string = ''); overload;
  end;

  MVCPatternAttribute = class(MVCStringAttribute)

  end;

  MVCStringEnumAttribute = class(MVCBaseAttribute)
  private
    fValues: string;
  public
    constructor Create(const enumValue: string);
    property Values: string read fValues write fValues;
  end;

  MVCFromBodyAttribute = class(MVCBaseAttribute)
  private
    fRootNode: string;
    fDataType: TMVCDataType;
  public
    constructor Create(const RootNode: string = ''; const DataType: TMVCDataType = TMVCDataType.dtObject);
    function DataType: TMVCDataType;
    function RootNode: String;
  end;

  MVCInjectableParamAttribute = class(MVCBaseAttribute)
  private
    FParamName: string;
    FDefaultValueAsString: string;
    FCanBeUsedADefaultValue: Boolean;
  public
    constructor Create(const AParamName: string); overload;
    constructor Create(const AParamName: string; const DefaultAsString: string); overload;
    constructor Create(const AParamName: string; const DefaultAsInteger: Int64); overload;
    constructor Create(const AParamName: string; const DefaultAsBoolean: Boolean); overload;
    property ParamName: string read FParamName;
    property DefaultValueAsString: string read FDefaultValueAsString;
    property CanBeUsedADefaultValue: Boolean read FCanBeUsedADefaultValue;
  end;

  MVCFromQueryStringAttribute = class(MVCInjectableParamAttribute)

  end;

  MVCFromHeaderAttribute = class(MVCInjectableParamAttribute)

  end;

  MVCFromCookieAttribute = class(MVCInjectableParamAttribute)

  end;

  MVCFromContentFieldAttribute = class(MVCInjectableParamAttribute)

  end;

  MVCInjectAttribute = class(TCustomAttribute)
  private
    fServiceName: String;
  public
    constructor Create(ServiceName: String = '');
    property ServiceName: String read fServiceName;
  end;

  // test
  // TMVCHackHTTPAppRequest = class(TIdHTTPAppRequest)
  // private
  // function GetHeaders: TStringList;
  // public
  // property Headers: TStringList read GetHeaders;
  // end;
  // test-end

  TMVCWebRequest = class
  private
    FQueryParams: TDictionary<string, string>;
    FContentFields: TDictionary<string, string>;
    FWebRequest: TWebRequest;
    FSerializers: TDictionary<string, IMVCSerializer>;
    FBody: string;
    FContentType: string;
    FCharset: string;
    FParamsTable: TMVCRequestParamsTable;
    FContentMediaType: string;
    procedure DefineContentType;
    function GetContentFields: TDictionary<string, string>;
    function GetQueryParams: TDictionary<string, string>;
    function GetQueryParamsMulti(const AParamName: string): TArray<string>;
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
    function GetContentParamsMulti(const AParamName: string): TArray<string>;
    function GetMultiParamsAsArray(const AParamName: String; const AStrings: TStrings): TArray<String>;
  protected
    { protected declarations }
    procedure EnsureINDY;
  public
    constructor Create(const AWebRequest: TWebRequest;
      const ASerializers: TDictionary<string, IMVCSerializer>);
    destructor Destroy; override;
    function ClientIp: string;
    function ClientPrefer(const AMediaType: string): Boolean;
    function ClientPreferHTML: Boolean;
    function ClientPreferredLanguage(): String;
    function GetOverwrittenHTTPMethod: TMVCHTTPMethodType;

    function SegmentParam(const AParamName: string; out AValue: string): Boolean;
    function SegmentParamsCount: Integer;
    function HasBody: Boolean;

    procedure EnsureQueryParamExists(const AName: string);
    function QueryString: string;
    function QueryStringParam(const AName: string): string;
    function QueryStringParamExists(const AName: string): Boolean;
    function QueryStringParams: TStrings;

    function Accept: string;
    function BestAccept: string;
    function AcceptHTML: boolean;
    function CanAcceptMediaType(const MediaType: String): boolean;

    function ContentParam(const AName: string): string;
    function Cookie(const AName: string): string;
    function Body: string;
    function BodyAs<T: class, constructor>(const RootNode: string = ''): T;
    function BodyAsListOf<T: class, constructor>(const RootNode: string = ''): TObjectList<T>;
    procedure BodyFor<T: class, constructor>(const AObject: T; const RootNode: string = '');
    procedure BodyForListOf<T: class, constructor>(const AObjectList: TObjectList<T>; const RootNode: string = '');
    // function HeaderNames: TArray<String>;
    property RawWebRequest: TWebRequest read FWebRequest;
    property ContentMediaType: string read FContentMediaType;
    property ContentType: string read FContentType;
    property ContentCharset: string read FCharset;
    property ContentFields: TDictionary<string, string> read GetContentFields;
    property QueryParams: TDictionary<string, string> read GetQueryParams;
    property Headers[const AHeaderName: string]: string read GetHeader;
    property PathInfo: string read GetPathInfo;
    property ParamsTable: TMVCRequestParamsTable read FParamsTable write FParamsTable;
    property ParamNames: TArray<string> read GetParamNames;
    property Params[const AParamName: string]: string read GetParams;
    property ContentParamsMulti[const AParamName: string]: TArray<string> read GetContentParamsMulti;
    property ParamsAsInteger[const AParamName: string]: Integer read GetParamAsInteger;
    property ParamsAsInt64[const AParamName: string]: Int64 read GetParamAsInt64;
    property QueryParamsMulti[const AParamName: string]: TArray<string> read GetQueryParamsMulti;
    property IsAjax: Boolean read GetIsAjax;
    property HTTPMethod: TMVCHTTPMethodType read GetHTTPMethod;
    property HTTPMethodAsString: string read GetHTTPMethodAsString;
    property Files: TAbstractWebRequestFiles read GetFiles;
  end;

{$IF Defined(WEBAPACHEHTTP)}

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
    // function RawHeaders: TStrings; override;
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
  public
    constructor Create(const AWebResponse: TWebResponse);
    destructor Destroy; override;

    procedure Flush;
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
    FCustomData: TMVCCustomData;
    procedure SetLoggedSince(const AValue: TDateTime);
    procedure SetCustomData(const Value: TMVCCustomData);
  public
    constructor Create;
    destructor Destroy; override;

    function IsValid: Boolean;
    procedure Clear;

    procedure SaveToSession(const AWebSession: TMVCWebSession);
    function LoadFromSession(const AWebSession: TMVCWebSession): Boolean;

    property UserName: string read FUserName write FUserName;
    property Roles: TList<string> read FRoles;
    property LoggedSince: TDateTime read FLoggedSince write SetLoggedSince;
    property Realm: string read FRealm write FRealm;
    property CustomData: TMVCCustomData read FCustomData write SetCustomData;
  end;

  TWebContext = class
  private
    fRequest: TMVCWebRequest;
    fResponse: TMVCWebResponse;
    fConfig: TMVCConfig;
    fSerializers: TDictionary<string, IMVCSerializer>;
    fIsSessionStarted: Boolean;
    fSessionMustBeClose: Boolean;
    fLoggedUser: TUser;
    fWebSession: TMVCWebSession;
    fData: TMVCStringDictionary;
    fIntfObject: IInterface;
    fServiceContainerResolver: IMVCServiceContainerResolver;
    function GetWebSession: TMVCWebSession;
    function GetLoggedUser: TUser;
    function GetParamsTable: TMVCRequestParamsTable;
    procedure SetParamsTable(const AValue: TMVCRequestParamsTable);
    function GetHostingFrameworkType: TMVCHostingFrameworkType;
    function GetIntfObject: IInterface;
    procedure SetIntfObject(const Value: IInterface);
  protected
    fActionQualifiedName: String;
    procedure Flush; virtual;
    procedure BindToSession(const ASessionId: string);
    function SendSessionCookie(const AContext: TWebContext): string;
    function AddSessionToTheSessionList(const ASessionType, ASessionId: string;
      const ASessionTimeout: Integer): TMVCWebSession;
    function GetData: TMVCStringDictionary;
  public
    constructor Create(const AServiceContainerResolver: IMVCServiceContainerResolver; const ARequest: TWebRequest; const AResponse: TWebResponse;
      const AConfig: TMVCConfig; const ASerializers: TDictionary<string, IMVCSerializer>);
    destructor Destroy; override;

    procedure SessionStart(const SessionType: String); virtual;
    procedure SessionStop(const ARaiseExceptionIfExpired: Boolean = True); virtual;

    function SessionStarted: Boolean;
    function SessionId: string;
    function IsSessionStarted: Boolean;
    function SessionMustBeClose: Boolean;

    property HostingFrameworkType: TMVCHostingFrameworkType read GetHostingFrameworkType;
    property LoggedUser: TUser read GetLoggedUser;
    property Request: TMVCWebRequest read FRequest;
    property Response: TMVCWebResponse read FResponse;
    property Session: TMVCWebSession read GetWebSession;
    property Config: TMVCConfig read FConfig;
    property Data: TMVCStringDictionary read GetData;
    property CustomIntfObject: IInterface read GetIntfObject write SetIntfObject;
    property ParamsTable: TMVCRequestParamsTable read GetParamsTable write SetParamsTable;
    property ActionQualifiedName: String read fActionQualifiedName;
    property ServiceContainerResolver: IMVCServiceContainerResolver read fServiceContainerResolver;
  end;

  TMVCJSONRPCExceptionErrorInfo = record
    Code: Integer;
    Msg: string;
    Data: TValue;
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
    property ApplicationSession: TWebApplicationSession read GetApplicationSession
      write SetApplicationSession;
  end;

  TMVCResponse = class;
  TMVCErrorResponse = class;

  IMVCRenderer = interface
    ['{2FF6DAC8-2F19-4C78-B9EC-A86296847D39}']
    procedure Render(const AContent: string); overload;
    procedure Render(const AObject: TObject); overload;
    procedure Render(const AObject: TObject; const AOwns: Boolean); overload;
    procedure Render(const AObject: TObject; const AOwns: Boolean;
      const AType: TMVCSerializationType); overload;
    procedure Render(const AStatusCode: Integer; AObject: TObject; const AOwns: Boolean;
      const ASerializationAction: TMVCSerializationAction = nil); overload;
    procedure Render(const ACollection: IMVCList); overload;
    procedure Render(const ACollection: IMVCList; const AType: TMVCSerializationType); overload;
    procedure Render(const ADataSet: TDataSet;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    procedure Render(const ADataSet: TDataSet; const AOwns: Boolean;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    procedure Render(const ADataSet: TDataSet; const AOwns: Boolean;
      const ASerializationType: TMVCDatasetSerializationType;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    procedure Render(const ADataSet: TDataSet; const AOwns: Boolean;
      const AIgnoredFields: TMVCIgnoredList; const ASerializationType: TMVCDatasetSerializationType;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    procedure Render(const ADataSet: TDataSet; const AOwns: Boolean;
      const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase;
      const ASerializationType: TMVCDatasetSerializationType;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    procedure Render(const ATextWriter: TTextWriter; const AOwns: Boolean = True); overload;
    procedure Render(const AStream: TStream; const AOwns: Boolean = True); overload;
    procedure Render(const AErrorCode: Integer; const AErrorMessage: string;
      const AErrorClassName: string = ''); overload;
    procedure Render(const AException: Exception; AExceptionItems: TList<string> = nil;
      const AOwns: Boolean = True); overload;
    procedure Render(const AResponse: TMVCResponse; const AOwns: Boolean = True); overload;
    // SSE Support
    procedure RenderSSE(const EventID: string; const EventData: string; EventName: string = '';
      const Retry: Integer = TMVCConstants.SSE_RETRY_DEFAULT);

    procedure SendStream(const AStream: TStream; const AOwns: Boolean = True;
      const ARewind: Boolean = False);
    procedure SendFile(const AFileName: string);
    procedure RenderResponseStream;
    function ResponseStream: TStringBuilder;
    procedure Redirect(const AUrl: string);
    procedure ResponseStatus(const AStatusCode: Integer; const AReasonString: string = '');
    procedure Render201Created(const Location: string = '');
    // Serializers access
    function Serializer: IMVCSerializer; overload;
    function Serializer(const AContentType: string; const ARaiseExcpIfNotExists: Boolean = True)
      : IMVCSerializer; overload;
  end;

  IMVCAuthenticationHandler = interface
    ['{19B580EA-8A47-4364-A302-EEF3C6207A9F}']
    procedure OnRequest(const AContext: TWebContext; const AControllerQualifiedClassName,
      AActionName: string; var AAuthenticationRequired: Boolean);
    procedure OnAuthentication(const AContext: TWebContext; const AUserName, APassword: string;
      AUserRoles: TList<string>; var AIsValid: Boolean;
      const ASessionData: TDictionary<string, string>);
    procedure OnAuthorization(const AContext: TWebContext; AUserRoles: TList<string>;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var AIsAuthorized: Boolean);
  end;

  // std responses

  IMVCResponse = interface
    ['{9DFEC741-EE38-4AC9-9C2C-9EA0D15D08D5}']
    function GetData: TObject;
    function GetMessage: string;
    function GetStatusCode: Integer;
    function GetHeaders: TStringList;
    procedure SetData(const Value: TObject);
    procedure SetMessage(const Value: string);
    procedure SetHeaders(const Headers: TStringList);
    procedure SetObjectDictionary(const Value: IMVCObjectDictionary);
    function GetObjectDictionary: IMVCObjectDictionary;
    procedure SetStatusCode(const Value: Integer);
    function GetIgnoredList: TMVCIgnoredList;
    function HasHeaders: Boolean;
    function HasBody: Boolean;
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    property Message: string read GetMessage write SetMessage;
    property Data: TObject read GetData write SetData;
    property ObjectDictionary: IMVCObjectDictionary read GetObjectDictionary write SetObjectDictionary;
    property Headers: TStringList read GetHeaders write SetHeaders;
  end;

  TMVCRenderer = class(TMVCBase)
  strict private
    FContext: TWebContext;
    FContentCharset: string;
    FContentMediaType: String;
    FResponseStream: TStringBuilder;
  protected
    function ToMVCList(const AObject: TObject; AOwnsObject: Boolean = False): IMVCList;
    function InternalStatusCodeResponse(const StatusCode: Word; const Body: TObject; const Message: String = ''): IMVCResponse;
  public { this must be public because of entity processors }
    constructor Create; virtual;
    destructor Destroy; override;
    function GetContentType: string;
    property ContentMediaType: String read FContentMediaType;
    property ContentCharset: String read FContentCharset;
    function GetStatusCode: Integer;
    procedure SetContentType(const AValue: string);
    procedure SetStatusCode(const AValue: Integer);
    function GetContext: TWebContext;
    procedure SetContext(const Context: TWebContext);
    procedure Redirect(const AUrl: string); virtual;
    procedure ResponseStatus(const AStatusCode: Integer; const AReasonString: string = ''); virtual;
    class procedure InternalRenderMVCResponse(const Controller: TMVCRenderer; const MVCResponse: TMVCResponse);

    ////////////////////////////////////////////////////////////////////////////
    ///
    /// <summary>
    /// HTTP Status 201 indicates that as a result of HTTP POST request, one or more new resources have been successfully created on server.
    /// The response may contain URI in Location header field in HTTP headers list, which can have reference to the newly created resource. Also, response payload also may include an entity containing a list of resource characteristics and location(s) from which the user or user agent can choose the one most appropriate.
    /// WARNING: The origin server MUST create the resource before returning the 201 status code. If the action cannot be carried out immediately, the server SHOULD respond with 202 (Accepted) response instead.
    /// </summary>
    /// <remarks>
    /// https://restfulapi.net/http-status-201-created/
    /// </remarks>
    procedure Render201Created(const Location: string = '';
      const Reason: string = ''); virtual;


    //Response Result
    {
      BadRequestResult
      ConflictResult
      NoContentResult
      NotFoundResult
      OkResult
      UnauthorizedResult
      UnprocessableEntityResult
      UnsupportedMediaTypeResult
      ConflictResult
      InternalServerErrorResult
      RedirectResult
      StatusResponse --> Generic Status
    }

    function StatusResponse(const StatusCode: Word; const Body: TObject): IMVCResponse; overload;
    function StatusResponse(const StatusCode: Word; const Message: String): IMVCResponse; overload;
    function StatusResponse(const StatusCode: Word): IMVCResponse; overload;

    function OKResponse(const Body: TObject): IMVCResponse; overload;
    function OKResponse(const Body: IMVCObjectDictionary): IMVCResponse; overload;
    function OKResponse(const Message: String): IMVCResponse; overload;
    function OKResponse: IMVCResponse; overload;

    function NotFoundResponse(const Body: TObject): IMVCResponse; overload;
    function NotFoundResponse(const Message: String): IMVCResponse; overload;
    function NotFoundResponse: IMVCResponse; overload;

    function NotModifiedResponse: IMVCResponse;

    function NoContentResponse: IMVCResponse;

    function UnauthorizedResponse: IMVCResponse;

    function BadRequestResponse(const Error: TObject): IMVCResponse; overload;
    function BadRequestResponse: IMVCResponse; overload;
    function BadRequestResponse(const Message: String): IMVCResponse; overload;

    function UnprocessableContentResponse(const Error: TObject): IMVCResponse; overload;
    function UnprocessableContentResponse: IMVCResponse; overload;
    function UnprocessableContentResponse(const Message: String): IMVCResponse; overload;

    function CreatedResponse(const Location: string = ''; const Body: TObject = nil): IMVCResponse; overload;
    function CreatedResponse(const Location: string; const Message: String): IMVCResponse; overload;

    function AcceptedResponse(const Location: string = ''; const Body: TObject = nil): IMVCResponse;

    function ConflictResponse: IMVCResponse;

    function RedirectResponse(Location: String; Permanent: Boolean = False; PreserveMethod: Boolean = False): IMVCResponse;

    function InternalServerErrorResponse: IMVCResponse; overload;
    function InternalServerErrorResponse(const Error: TObject): IMVCResponse; overload;
    function InternalServerErrorResponse(const Message: String): IMVCResponse; overload;

    /// <summary>
    /// Allow a server to accept a request for some other process (perhaps a batch-oriented process that is only run once per day) without requiring that the user agents connection to the server persist until the process is completed.
    /// The entity returned with this response SHOULD describe the requests current status and point to (or embed) a status monitor that can provide the user with (or without) an estimate of when the request will be fulfilled.
    /// </summary>
    /// <remarks>
    /// https://restfulapi.net/http-status-202-accepted/
    /// </remarks>
    procedure Render202Accepted(const HREF: string; const ID: string;
      const Reason: string = 'Accepted'); virtual;
    /// <summary>
    /// HTTP Status 204 (No Content) indicates that the server has successfully fulfilled the request and that there is no content to send in the response payload body. The server might want to return updated meta information in the form of entity-headers, which if present SHOULD be applied to current documents active view if any.
    /// The 204 response MUST NOT include a message-body and thus is always terminated by the first empty line after the header fields.
    /// </summary>
    procedure Render204NoContent(const Location: string = '';
      const Reason: string = ''); virtual;




    ////////////////////////////////////////////////////////////////////////////
    function Serializer: IMVCSerializer; overload;
    function Serializer(const AContentType: string;
      const ARaiseExceptionIfNotExists: Boolean = True): IMVCSerializer; overload;
    procedure SendStream(const AStream: TStream; const AOwns: Boolean = True;
      const ARewind: Boolean = False); virtual;
    procedure RenderStream(const AStream: TStream; const AOwns: Boolean = True;
      const ARewind: Boolean = False); virtual;
    procedure SendFile(const AFileName: string); virtual;
    procedure RenderFile(const AFileName: string); virtual;
    procedure RenderResponseStream; virtual;
    function ResponseStream: TStringBuilder;
    procedure Render(const AContent: string); overload;
    procedure Render(const AStatusCode: Integer; const AContent: string); overload;
    procedure Render(const AStatusCode: Integer); overload;
    // PODO renders
    procedure Render(const AStatusCode: Integer; const AObject: TObject;
      const ASerializationAction: TMVCSerializationAction = nil;
      const AIgnoredFields: TMVCIgnoredList = nil); overload;
    procedure Render(const AObject: TObject;
      const ASerializationAction: TMVCSerializationAction = nil;
      const AIgnoredFields: TMVCIgnoredList = nil); overload;
    procedure Render(const AObject: TObject; const AOwns: Boolean;
      const ASerializationAction: TMVCSerializationAction = nil;
      const AIgnoredFields: TMVCIgnoredList = nil); overload;
    procedure Render(const AObject: TObject; const AOwns: Boolean;
      const AType: TMVCSerializationType;
      const ASerializationAction: TMVCSerializationAction = nil;
      const AIgnoredFields: TMVCIgnoredList = nil); overload;
    procedure Render(const AStatusCode: Integer; AObject: TObject; const AOwns: Boolean;
      const ASerializationAction: TMVCSerializationAction = nil; const AIgnoredFields: TMVCIgnoredList = nil); overload;
    procedure Render(const AObject: IInterface;
      const ASerializationAction: TMVCSerializationAction = nil;
      const AIgnoredFields: TMVCIgnoredList = nil); overload;
    procedure Render(const AStatusCode: Integer; const AObject: IInterface;
      const ASerializationAction: TMVCSerializationAction = nil); overload;
    procedure Render<T: record>(const AStatusCode: Integer; var ARecord: T); overload;
    // PODOs Collection render
    procedure Render<T: class>(const ACollection: TObjectList<T>;
      const ASerializationAction: TMVCSerializationAction<T> = nil); overload;
    procedure Render<T: class>(const ACollection: TObjectList<T>; const AOwns: Boolean;
      const ASerializationAction: TMVCSerializationAction<T> = nil); overload;
    procedure Render<T: class>(const AStatusCode: Integer; const ACollection: TObjectList<T>;
      const AOwns: Boolean; const ASerializationAction: TMVCSerializationAction<T> = nil); overload;
    procedure Render<T: class>(const ACollection: TObjectList<T>; const AOwns: Boolean;
      const AType: TMVCSerializationType;
      const ASerializationAction: TMVCSerializationAction<T> = nil); overload;
    procedure Render(const ACollection: IMVCList); overload;
    procedure Render(const ACollection: IMVCList; const AType: TMVCSerializationType); overload;
    procedure Render(const ATextWriter: TTextWriter; const AOwns: Boolean = True); overload;
    procedure Render(const AStream: TStream; const AOwns: Boolean = True); overload;
    procedure RenderStatusMessage(const AStatusCode: Integer; const AReasonMessage: string = '';
      const AErrorClassName: string = ''; const ADataObject: TObject = nil; const AOwns: Boolean = True); overload;
    procedure Render(const AException: Exception; AExceptionItems: TList<string> = nil;
      const AOwns: Boolean = True); overload;
    procedure Render(const AResponse: TMVCResponse; const AOwns: Boolean = True); overload;
    // Dataset support
    procedure Render(const ADataSet: TDataSet;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    procedure Render(const ADataSet: TDataSet; const AOwns: Boolean;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    procedure Render(const ADataSet: TDataSet; const AOwns: Boolean;
      const ASerializationType: TMVCDatasetSerializationType;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    procedure Render(const ADataSet: TDataSet; const AOwns: Boolean;
      const AIgnoredFields: TMVCIgnoredList; const ASerializationType: TMVCDatasetSerializationType;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    procedure Render(const ADataSet: TDataSet; const AOwns: Boolean;
      const AIgnoredFields: TMVCIgnoredList; const ANameCase: TMVCNameCase;
      const ASerializationType: TMVCDatasetSerializationType;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    // SSE Support
    procedure RenderSSE(const EventID: string; const EventData: string; EventName: string = '';
      const Retry: Integer = TMVCConstants.SSE_RETRY_DEFAULT);
  end;

  TMVCSSVBeforeRenderCallback = reference to procedure(const TemplateRenderInstance: TObject);

  TMVCController = class(TMVCRenderer)
  private
    FViewModel: TMVCViewDataObject;
    fPageHeaders: TArray<String>;
    fPageFooters: TArray<String>;
    function GetSession: TMVCWebSession;
    function GetViewData(const aModelName: string): TValue;
    procedure SetViewData(const aModelName: string; const Value: TValue);
  protected const
    CLIENTID_KEY = '__clientid';
  protected
    procedure MVCControllerAfterCreate; virtual;
    procedure MVCControllerBeforeDestroy; virtual;

    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string;
      var AHandled: Boolean); virtual;
    procedure OnAfterAction(AContext: TWebContext; const AActionName: string); virtual;

    function GetClientId: string;
    function GetCurrentWebModule: TWebModule;
    function GetViewModel: TMVCViewDataObject;
    function GetRenderedView(const AViewNames: TArray<string>; const OnBeforeRenderCallback: TMVCSSVBeforeRenderCallback = nil): string; overload; virtual;
    function GetRenderedView(const AViewNames: TArray<string>; const JSONModel: TJSONObject; const OnBeforeRenderCallback: TMVCSSVBeforeRenderCallback = nil): string; overload; virtual;

    /// <summary>
    ///   Normally used in OnBeforeControllerAction to define view headers automatically used by the Page method.
    /// </summary>
    procedure SetPagesCommonHeaders(const AViewNames: TArray<string>);

    /// <summary>
    ///   Normally used in OnBeforeControllerAction to define view footers automatically used by the Page method.
    /// </summary>
    procedure SetPagesCommonFooters(const AViewNames: TArray<string>);

    /// <summary>
    ///   Page calls GetRenderedView with sensible defaults.
    ///   Page method just concatenate -> commonheader_header_views + views + commonfooter_views
    ///   PageFragment ignore header and footer views
    /// </summary>
    function Page(const AViewNames: TArray<string>; const UseCommonHeadersAndFooters: Boolean = True; const OnBeforeRenderCallback: TMVCSSVBeforeRenderCallback = nil): string; overload; inline;
    function Page(const AViewName: string; const UseCommonHeadersAndFooters: Boolean = True; const OnBeforeRenderCallback: TMVCSSVBeforeRenderCallback = nil): string; overload; inline;

    /// <summary>
    ///   Page calls GetRenderedView with sensible defaults.
    ///   Page method with UseCommonHeadersAndFooters = True (default) concatenates
    //    commonheader_header_views + views + commonfooter_views
    /// </summary>
    function Page(const AViewNames: TArray<string>; const JSONModel: TJSONObject;
      const UseCommonHeadersAndFooters: Boolean = True; const OnBeforeRenderCallback: TMVCSSVBeforeRenderCallback = nil): string; overload; inline;

    /// <summary>
    ///   PageFragment calls GetRenderedView.
    ///   PageFragment ignore header and footer views.
    /// </summary>
    function PageFragment(const AViewNames: TArray<string>; const OnBeforeRenderCallback: TMVCSSVBeforeRenderCallback = nil): string; overload; inline;

    /// <summary>
    ///   PageFragment calls GetRenderedView.
    ///   PageFragment ignore header and footer views.
    /// </summary>
    function PageFragment(const AViewNames: TArray<string>; const JSONModel: TJSONObject; const OnBeforeRenderCallback: TMVCSSVBeforeRenderCallback = nil): string; overload; inline;

    /// <summary>
    /// Load mustache view located in TMVCConfigKey.ViewsPath
    /// returns the rendered views and generates output using
    /// models pushed using Push* methods.
    /// Do not use thie method directly. Use Page and PageFragment, instead.
    /// </summary>
    function LoadView(const AViewNames: TArray<string>; const JSONModel: TJSONObject = nil): string; virtual;

    function SessionAs<T: TMVCWebSession>: T;
    procedure RaiseSessionExpired; virtual;

    // Avoiding mid-air collisions - support
    procedure SetETag(const Data: String; const NeedsToBeHashed: Boolean = True);
    function ETagFromString(const Data: String): String;
    function GetIfMatch(): String;
    function GetIfNoneMatch(): String;
    procedure CheckIfMatch(const Data: String);
    // END - Avoiding mid-air collisions - support

    // Properties
    property Context: TWebContext read GetContext write SetContext;
    property Session: TMVCWebSession read GetSession;
    property ContentType: string read GetContentType write SetContentType;
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    procedure PushObjectToView(const aModelName: string; const AModel: TObject);
      deprecated 'Use "ViewData"';

    property ViewData[const aModelName: string]: TValue read GetViewData write SetViewData;
  public
    constructor Create; virtual;
    destructor Destroy; override;

  end;

  TMVCControllerClazz = class of TMVCController;

  TMVCControllerCreateAction = reference to function: TMVCController;
  TMVCObjectCreatorDelegate = reference to function: TObject;

  TMVCControllerDelegate = class
  private
    FClazz: TMVCControllerClazz;
    FCreateAction: TMVCControllerCreateAction;
    FURLSegment: string;
  protected
    { protected declarations }
  public
    constructor Create(const AClazz: TMVCControllerClazz;
      const ACreateAction: TMVCControllerCreateAction; const AURLSegment: string = '');
    property Clazz: TMVCControllerClazz read FClazz;
    property CreateAction: TMVCControllerCreateAction read FCreateAction;
    property URLSegment: string read FURLSegment;
  end;

  TMVCStaticContents = class(TMVCController)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    class procedure SendFile(const AFileName, AMediaType: string; AContext: TWebContext);
    class function IsStaticFile(const AViewPath, AWebRequestPath: string; out ARealFileName: string;
      out AIsDirectoryTraversalAttack: Boolean): Boolean;
    class function IsScriptableFile(const AStaticFileName: string;
      const AConfig: TMVCConfig): Boolean;
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
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    /// <summary>
    /// Procedure is called before the specific controller method is called.
    /// </summary>
    /// <param name="AContext">Webcontext which contains the complete request and response of the actual call.</param>
    /// <param name="AControllerQualifiedClassName">Qualified classname of the matching controller.</param>
    /// <param name="AActionName">Method name of the matching controller method.</param>
    /// <param name="AHandled">If set to True the Request would finished. Response must be set by the implementor. Default value is False.</param>
    procedure OnBeforeControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var AHandled: Boolean);
    /// <summary>
    /// Procedure is called after the specific controller method was called.
    /// It is still possible to cancel or to completly modifiy the request.
    /// </summary>
    /// <param name="AContext">Webcontext which contains the complete request and response of the actual call.</param>
    /// <param name="AActionName">Method name of the matching controller method.</param>
    /// <param name="AHandled">If set to True the Request would finished. Response must be set by the implementor. Default value is False.</param>
    procedure OnAfterControllerAction(AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);

    /// <summary>
    /// Procedure is called after the MVCEngine routes the request to a specific controller/method.
    /// </summary>
    /// <param name="AContext">Webcontext which contains the complete request and response of the actual call.</param>
    /// <param name="AHandled">If set to True the Request would finished. Response must be set by the implementor. Default value is False.</param>
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  end;

  TMVCExceptionHandlerProc = reference to procedure(E: Exception;
    SelectedController: TMVCController; WebContext: TWebContext; var ExceptionHandled: Boolean);
  TMVCRouterLogState = (rlsRouteFound, rlsRouteNotFound);
  TMVCRouterLogHandlerProc = reference to procedure(const Router: TMVCCustomRouter;
    const RouterLogState: TMVCRouterLogState; const WebContext: TWebContext);
  TMVCJSONRPCExceptionHandlerProc = reference to procedure(E: Exception;
    { SelectedController: TMVCController; //YAGNI }
    WebContext: TWebContext;
    var ErrorInfo: TMVCJSONRPCExceptionErrorInfo;
    var ExceptionHandled: Boolean);
  TWebContextCreateEvent = reference to procedure(const AContext: TWebContext);
  TWebContextDestroyEvent = reference to procedure(const AContext: TWebContext);

  TMVCEngine = class(TComponent)
  private const
    ALLOWED_TYPED_ACTION_PARAMETERS_TYPES =
      'Integer, Int64, Single, Double, Extended, Boolean, TDate, TTime, TDateTime, String and TGUID';
  private
    fViewEngineClass: TMVCViewEngineClass;
    fWebModule: TWebModule;
    fConfig: TMVCConfig;
    fConfigCache_MaxRequestSize: Int64;
    fConfigCache_ExposeServerSignature: Boolean;
    fConfigCache_ServerSignature: string;
    fConfigCache_ExposeXPoweredBy: Boolean;
    fConfigCache_DefaultContentType: String;
    fConfigCache_DefaultContentCharset: String;
    fConfigCache_PathPrefix: String;
    fConfigCache_UseViewCache: Boolean;
    fSerializers: TDictionary<string, IMVCSerializer>;
    fMiddlewares: TList<IMVCMiddleware>;
    fControllers: TObjectList<TMVCControllerDelegate>;
    fApplicationSession: TWebApplicationSession;
    fSavedOnBeforeDispatch: THTTPMethodEvent;
    fOnException: TMVCExceptionHandlerProc;
    fOnRouterLog: TMVCRouterLogHandlerProc;
    fWebContextCreateEvent: TWebContextCreateEvent;
    fWebContextDestroyEvent: TWebContextDestroyEvent;
    procedure FillActualParamsForAction(const ASelectedController: TMVCController;
      const AContext: TWebContext; const AActionFormalParams: TArray<TRttiParameter>;
      const AActionName: string; var AActualParams: TArray<TValue>; out ABodyParameter: TObject);
    procedure RegisterDefaultsSerializers;
    function GetViewEngineClass: TMVCViewEngineClass;
    procedure HandleDefaultValueForInjectedParameter(var InjectedParamValue: String;
      const InjectableParamAttribute: MVCInjectableParamAttribute);
//    procedure FillActualParamsForConstructor(
//      const AActionFormalParams: TArray<TRttiParameter>;
//      var AActualParams: TArray<TValue>);
  protected
    procedure DoWebContextCreateEvent(const AContext: TWebContext); inline;
    procedure DoWebContextDestroyEvent(const AContext: TWebContext); inline;
    function GetActualParam(const AFormalParam: TRttiParameter; const AStringValue: String): TValue;
    function GetActualParamMulti(const AFormalParam: TRttiParameter; const AStringMultiValue: TArray<String>): TValue;
    function CustomExceptionHandling(const Ex: Exception; const ASelectedController: TMVCController;
      const AContext: TWebContext): Boolean;
    procedure ConfigDefaultValues; virtual;
    procedure SaveCacheConfigValues;
    procedure LoadSystemControllers; virtual;
    procedure FixUpWebModule;
    procedure ExecuteBeforeRoutingMiddleware(const AContext: TWebContext; var AHandled: Boolean);
    procedure ExecuteBeforeControllerActionMiddleware(const AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      var AHandled: Boolean);
    procedure ExecuteAfterControllerActionMiddleware(const AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string;
      const AHandled: Boolean);
    procedure ExecuteAfterRoutingMiddleware(const AContext: TWebContext; const AHandled: Boolean);
    procedure DefineDefaultResponseHeaders(const AContext: TWebContext);
    procedure OnBeforeDispatch(ASender: TObject; ARequest: TWebRequest; AResponse: TWebResponse;
      var AHandled: Boolean); virtual;
    procedure ResponseErrorPage(const AException: Exception; const ARequest: TWebRequest;
      const AResponse: TWebResponse); virtual;
    function ExecuteAction(const ASender: TObject; const ARequest: TWebRequest;
      const AResponse: TWebResponse): Boolean; virtual;
    function CreateControllerWithDependencies(
      const Context: TWebContext;
      const ControllerClass: TMVCControllerClazz;
      const ConstructorMethod: TRttiMethod): TMVCController;
  public
    class function GetCurrentSession(const ASessionId: string;
      const ARaiseExceptionIfExpired: Boolean = True): TMVCWebSession; static;
    class function ExtractSessionIdFromWebRequest(const AWebRequest: TWebRequest): string; static;
    class function SendSessionCookie(const AContext: TWebContext): string; overload; static;
    class function SendSessionCookie(const AContext: TWebContext; const ASessionId: string): string;
      overload; static;
    class procedure ClearSessionCookiesAlreadySet(const ACookies: TCookieCollection); static;
  public
    constructor Create(const AWebModule: TWebModule; const AConfigAction: TProc<TMVCConfig> = nil); reintroduce;
    destructor Destroy; override;

    function GetSessionBySessionId(const ASessionId: string): TMVCWebSession;

    { webcontext events}
    procedure OnWebContextCreate(const WebContextCreateEvent: TWebContextCreateEvent);
    procedure OnWebContextDestroy(const WebContextDestroyEvent: TWebContextDestroyEvent);
    { end - webcontext events}

    function Serializer(const AContentType: string; const ARaiseExceptionIfNotExists: Boolean = True): IMVCSerializer;
    function AddSerializer(const AContentType: string; const ASerializer: IMVCSerializer)
      : TMVCEngine;
    function AddMiddleware(const AMiddleware: IMVCMiddleware): TMVCEngine;
    function AddController(const AControllerClazz: TMVCControllerClazz;
      const AURLSegment: string = ''): TMVCEngine; overload;
    function AddController(const AControllerClazz: TMVCControllerClazz;
      const ACreateAction: TMVCControllerCreateAction; const AURLSegment: string = '')
      : TMVCEngine; overload;
    function PublishObject(const AObjectCreatorDelegate: TMVCObjectCreatorDelegate;
      const AURLSegment: string; ExceptionHandler: TMVCJSONRPCExceptionHandlerProc = nil): TMVCEngine;
    function SetViewEngine(const AViewEngineClass: TMVCViewEngineClass): TMVCEngine;
    function SetExceptionHandler(const AExceptionHandlerProc: TMVCExceptionHandlerProc): TMVCEngine;

    procedure SendHTTPStatus(const AContext: TWebContext; const HTTPStatusCode: Integer;
      const AReasonString: string = ''; const AClassName: string = '');

    property ViewEngineClass: TMVCViewEngineClass read GetViewEngineClass;
    property WebModule: TWebModule read FWebModule;
    property Config: TMVCConfig read FConfig;
    property Middlewares: TList<IMVCMiddleware> read FMiddlewares;
    property Controllers: TObjectList<TMVCControllerDelegate> read FControllers;
    property ApplicationSession: TWebApplicationSession read FApplicationSession
      write FApplicationSession;
    property OnRouterLog: TMVCRouterLogHandlerProc read fOnRouterLog write fOnRouterLog;
  end;

  TMVCErrorResponseItem = class
  private
    FMessage: string;
  protected
    { protected declarations }
  public
    constructor Create(const AMessage: string = '');
    property message: string read FMessage write FMessage;
  end;

  TMVCBaseResponse = class abstract (TInterfacedObject, IMVCResponse)
  protected
    function GetMessage: string;virtual; abstract;
    function GetData: TObject; virtual; abstract;
    function GetObjectDictionary: IMVCObjectDictionary; virtual; abstract;
    function GetHeaders: TStringList; virtual; abstract;
    procedure SetMessage(const Value: string); virtual; abstract;
    procedure SetData(const Value: TObject); virtual; abstract;
    procedure SetObjectDictionary(const Value: IMVCObjectDictionary); virtual; abstract;
    procedure SetHeaders(const Headers: TStringList); virtual; abstract;
    function GetStatusCode: Integer; virtual; abstract;
    procedure SetStatusCode(const Value: Integer); virtual; abstract;
    function GetReasonString: string; virtual; abstract;
    procedure SetReasonString(const Value: string); virtual; abstract;
    function GetIgnoredList: TMVCIgnoredList; virtual; abstract;
    function HasHeaders: Boolean; virtual; abstract;
    function HasBody: Boolean; virtual; abstract;
  public
    constructor Create; virtual;
  end;

  TMVCResponse = class(TMVCBaseResponse)
  private
    fStatusCode: Integer;
    fMessage: string;
    fData: TObject;
    fObjectDictionary: IMVCObjectDictionary;
    fHeaders: TStringList;
    fReasonString: String;
    fOwnsData: Boolean;
    procedure SetOwnsData(const Value: Boolean);
  protected
    function GetData: TObject; override;
    function GetMessage: string; override;
    function GetStatusCode: Integer; override;
    function GetObjectDictionary: IMVCObjectDictionary; override;
    procedure SetData(const Value: TObject); override;
    procedure SetMessage(const Value: string); override;
    procedure SetStatusCode(const Value: Integer); override;
    procedure SetObjectDictionary(const Value: IMVCObjectDictionary); override;
    function GetReasonString: string; override;
    procedure SetReasonString(const Value: string); override;
    procedure SetHeaders(const Value: TStringList); override;
    function GetHeaders: TStringList; override;
    //do not expose this property through interface
    property OwnsData: Boolean read fOwnsData write SetOwnsData;
  protected
    function HasHeaders: Boolean; override;
    function HasBody: Boolean; override;
  public
    constructor Create; overload; virtual;
    constructor Create(const StatusCode: Integer; const Message: String); overload;
    destructor Destroy; override;
    function GetIgnoredList: TMVCIgnoredList; override;
    [MVCDoNotSerialize]
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    [MVCDoNotSerialize]
    property Headers: TStringList read GetHeaders write SetHeaders;
    property Message: string read GetMessage write SetMessage;
    property Data: TObject read GetData write SetData;
    property ObjectDictionary: IMVCObjectDictionary read GetObjectDictionary write SetObjectDictionary;
  end;

  TMVCErrorResponse = class(TMVCResponse)
  private
    fClassname: string;
    fItems: TObjectList<TMVCErrorResponseItem>;
    fAppErrorCode: Integer;
    fDetailedMessage: string;
    procedure SetAppErrorCode(const Value: Integer);
  public
    constructor Create; overload; override;
    constructor Create(const AStatusCode: Integer; const AMessage: String); overload;
    destructor Destroy; override;
    function GetIgnoredList: TMVCIgnoredList; override;
    property Classname: string read fClassname write fClassname;
    property DetailedMessage: string read fDetailedMessage write fDetailedMessage;
    property AppErrorCode: Integer read fAppErrorCode write SetAppErrorCode;
    [MVCListOf(TMVCErrorResponseItem)]
    property Items: TObjectList<TMVCErrorResponseItem> read fItems;
  end;

  // end - std responses


  TMVCBaseViewEngine = class(TMVCBase)
  private
    FViewName: string;
    FWebContext: TWebContext;
    FViewModel: TMVCViewDataObject;
    FContentType: string;
    FOutput: string;
    FController: TMVCController;
  protected
    FUseViewCache: Boolean;
    FJSONModel: TJSONObject;
    FBeforeRenderCallback: TMVCSSVBeforeRenderCallback;
    function GetRealFileName(const AViewName: string): string; virtual;
    function IsCompiledVersionUpToDate(const AFileName, ACompiledFileName: string): Boolean; virtual; abstract;
  public
    constructor Create(
      const AEngine: TMVCEngine;
      const AWebContext: TWebContext;
      const AController: TMVCController;
      const AViewModel: TMVCViewDataObject;
      const AContentType: string); overload; virtual;
    constructor Create(
      const AEngine: TMVCEngine;
      const AWebContext: TWebContext;
      const AController: TMVCController;
      const AViewModel: TMVCViewDataObject;
      const AJSONModel: TJSONObject;
      const AContentType: string); overload; virtual;
    destructor Destroy; override;

    procedure Execute(const ViewName: string; const Builder: TStringBuilder); virtual; abstract;

    property ViewName: string read FViewName;
    property WebContext: TWebContext read FWebContext;
    property ViewModel: TMVCViewDataObject read FViewModel;
    property ContentType: string read FContentType;
    property Output: string read FOutput;
  end;

function IsShuttingDown: Boolean;
procedure EnterInShutdownState;

type
  IMVCResponseBuilder = interface
    ['{10210D72-AFAE-4919-936D-EB08AA16C01C}']
    function StatusCode(const StatusCode: Integer): IMVCResponseBuilder;
    function Header(const Name: String; const Value: String): IMVCResponseBuilder;
    function Body(const Data: TObject; const Owns: Boolean = True): IMVCResponseBuilder; overload;
    function Body(const Message: String): IMVCResponseBuilder; overload;
    function Body(const ObjDictionary: IMVCObjectDictionary): IMVCResponseBuilder; overload;
    function Build: IMVCResponse;
  end;

function MVCResponseBuilder: IMVCResponseBuilder;

implementation

uses
  IdURI,
  System.StrUtils,
  sqids,
  MVCFramework.SysControllers,
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.JSONRPC,
  MVCFramework.Router,
  MVCFramework.Rtti.Utils,
  MVCFramework.Serializer.HTML,
  MVCFramework.Serializer.Abstract,
  MVCFramework.Utils, MVCFramework.Serializer.Text;

var
  gIsShuttingDown: Boolean = False;
  gMVCGlobalActionParamsCache: TMVCStringObjectDictionary<TMVCActionParamCacheItem> = nil;
  gHostingFramework: TMVCHostingFrameworkType = hftUnknown;
  gEncodingUTF8: TEncoding;


type
  EMVCResponseBuilderException = class(EMVCException)

  end;

  TMVCResponseBuilder = class sealed(TInterfacedObject, IMVCResponseBuilder)
  private
    fBuilt: Boolean;
    fHeaders: TStringList;
    fOwnsData: Boolean;
  protected
    fStatusCode: Integer;
    fMessage: String;
    fData: TObject;
    fObjectDict: IMVCObjectDictionary;
    function HasHeaders: Boolean;
    function StatusCode(const StatusCode: Integer): IMVCResponseBuilder;
    function Message(const Message: String): IMVCResponseBuilder;
    function Body(const Data: TObject; const Owns: Boolean): IMVCResponseBuilder; overload;
    function Body(const MessageText: String): IMVCResponseBuilder; overload;
    function Body(const ObjDictionary: IMVCObjectDictionary): IMVCResponseBuilder; overload;
    function Header(const Name: String; const Value: String): IMVCResponseBuilder;
    function Build: IMVCResponse;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;


function IsShuttingDown: Boolean;
begin
  Result := gIsShuttingDown;
end;

procedure EnterInShutdownState;
begin
  gIsShuttingDown := True;
end;

function GetRequestShortDescription(const AWebRequest: TWebRequest): String;
begin
  Result := Format('%s %s%s', [AWebRequest.Method, AWebRequest.PathInfo,
    IfThen(AWebRequest.Query = '', '', '?' + AWebRequest.Query)]);
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

{ MVCInjectableParamAttribute }

constructor MVCInjectableParamAttribute.Create(const AParamName: string;
  const DefaultAsString: string);
begin
  Create(AParamName);
  FCanBeUsedADefaultValue := True;
  FDefaultValueAsString := DefaultAsString;
end;

constructor MVCInjectableParamAttribute.Create(const AParamName: string);
begin
  inherited Create;
  FParamName := AParamName;
  FCanBeUsedADefaultValue := False;
end;

constructor MVCInjectableParamAttribute.Create(const AParamName: string;
  const DefaultAsInteger: Int64);
begin
  Create(AParamName, DefaultAsInteger.ToString);
end;

constructor MVCInjectableParamAttribute.Create(const AParamName: string;
  const DefaultAsBoolean: Boolean);
begin
  Create(AParamName, iif(DefaultAsBoolean,'true','false'));
end;

{ MVCProducesAttribute }

constructor MVCProducesAttribute.Create(const AValue, ACharset: string);
begin
  Create(AValue);
  FCharset := ACharset;
end;

constructor MVCProducesAttribute.Create(const AValue: string);
begin
  inherited Create(AValue);
  FCharset := TMVCCharset.UTF_8;
end;

{ MVCPathAttribute }

constructor MVCPathAttribute.Create(const APath: string);
begin
  inherited Create;
  FPath := APath;
end;

constructor MVCResponseAttribute.Create(inStatusCode: Integer; const inDescription: string;
  inResponseClass: TClass);
begin
  FStatusCode := inStatusCode;
  FDescription := inDescription;
  FResponseClass := inResponseClass;
end;

{ MVCResponseListAttribute }

constructor MVCResponseListAttribute.Create(inStatusCode: Integer; const inDescription: string;
  inResponseClass: TClass);
begin
  FStatusCode := inStatusCode;
  FDescription := inDescription;
  FResponseClass := inResponseClass;
end;

{ MVCStringEnumAttribute }

constructor MVCStringEnumAttribute.Create(const enumValue: string);
begin
  fValues := enumValue;
end;

{ TMVCWebRequest }

function TMVCWebRequest.Accept: string;
begin
  Result := FWebRequest.Accept;
end;

function TMVCWebRequest.AcceptHTML: boolean;
begin
  Result := CanAcceptMediaType(TMVCMediaType.TEXT_HTML);
end;

function TMVCWebRequest.BestAccept: string;
begin
  if Accept.Contains(',') then
  begin
    Result := Accept.Split([','])[0];
  end
  else
  begin
    Result := Accept;
  end;
end;

function TMVCWebRequest.Body: string;
var
  lEncoding: TEncoding;
{$IF not Defined(BERLINORBETTER)}
  lBuffer: TArray<Byte>;
{$ENDIF}
  lFreeEncoding: Boolean;
begin
  if (FBody = EmptyStr) then
  begin
    if (FCharset = EmptyStr) or (SameText(FCharset, TMVCCharSet.UTF_8)) then
    begin
      lFreeEncoding := False;
      lEncoding := gEncodingUTF8; //utf-8 is the most used encoding, we have a global instance
    end
    else
    begin
      lFreeEncoding := True;
      lEncoding := TEncoding.GetEncoding(FCharset);
    end;
    try
{$IF Defined(BERLINORBETTER)}
      FWebRequest.ReadTotalContent; // Otherwise ISAPI Raises "Empty BODY"
      FBody := lEncoding.GetString(FWebRequest.RawContent);
{$ELSE}
      SetLength(lBuffer, FWebRequest.ContentLength);
      FWebRequest.ReadClient(lBuffer[0], FWebRequest.ContentLength);
      FBody := lEncoding.GetString(lBuffer);
{$ENDIF}
    finally
      if lFreeEncoding then
      begin
        lEncoding.Free;
      end;
    end;
  end;
  Result := FBody;
end;

function TMVCWebRequest.BodyAs<T>(const RootNode: string): T;
var
  Obj: TObject;
  lSerializer: IMVCSerializer;
begin
  if FSerializers.TryGetValue(ContentMediaType, lSerializer) then
  begin
    Obj := TMVCSerializerHelper.CreateObject(TClass(T).QualifiedClassName);
    try
      lSerializer.DeserializeObject(Body, Obj, TMVCSerializationType.stDefault, nil, RootNode);
      Result := Obj as T;
    except
      on E: Exception do
      begin
        FreeAndNil(Obj);
        raise;
      end;
    end;
  end
  else
    raise EMVCDeserializationException.CreateFmt('Body ContentType "%s" not supported',
      [ContentType]);
end;

function TMVCWebRequest.BodyAsListOf<T>(const RootNode: string): TObjectList<T>;
var
  List: TObjectList<T>;
  lSerializer: IMVCSerializer;
begin
  if FSerializers.TryGetValue(ContentMediaType, lSerializer) then
  begin
    List := TObjectList<T>.Create(True);
    try
      lSerializer.DeserializeCollection(Body, List, T, TMVCSerializationType.stDefault, nil,
        RootNode);
      Result := List;
    except
      FreeAndNil(List);
      raise;
    end;
  end
  else
    raise EMVCException.CreateFmt('Body ContentType "%s" not supported', [ContentType]);
end;

procedure TMVCWebRequest.BodyFor<T>(const AObject: T; const RootNode: string);
var
  lSerializer: IMVCSerializer;
begin
  if Assigned(AObject) then
  begin
    if FSerializers.TryGetValue(ContentMediaType, lSerializer) then
    begin
      lSerializer.DeserializeObject(Body, AObject,
        TMVCSerializationType.stDefault, [], RootNode)
    end
    else
    begin
      raise EMVCException.CreateFmt('Body ContentType "%s" not supported', [ContentType]);
    end
  end;
end;

procedure TMVCWebRequest.BodyForListOf<T>(const AObjectList: TObjectList<T>; const RootNode: string);
var
  lSerializer: IMVCSerializer;
begin
  if Assigned(AObjectList) then
  begin
    if FSerializers.TryGetValue(ContentMediaType, lSerializer) then
    begin
      lSerializer.DeserializeCollection(Body, AObjectList, T,
        TMVCSerializationType.stDefault, [], RootNode)
    end
    else
    begin
      raise EMVCException.CreateFmt('Body ContentType "%s" not supported', [ContentType]);
    end;
  end;
end;

function TMVCWebRequest.CanAcceptMediaType(const MediaType: String): boolean;
begin
  Result := Accept.Contains(MediaType);
end;

function TMVCWebRequest.ClientIp: string;
var
  lValue: string;
  function GetFirst(const Value: String): String; inline;
  begin
    Result := Value.Split([',',';'])[0].Trim();
  end;
begin
  // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Forwarded-For
  lValue := String(FWebRequest.GetFieldByName('X-Forwarded-For'));
  if not lValue.IsEmpty then
  begin
    Exit(GetFirst(lValue));
  end;

  lValue := String(FWebRequest.GetFieldByName('X-Real-IP'));
  if not lValue.IsEmpty then
  begin
    Exit(GetFirst(lValue));
  end;

  Result := FWebRequest.RemoteAddr;
end;

function TMVCWebRequest.ClientPrefer(const AMediaType: string): Boolean;
begin
  Result := (RawWebRequest.Accept = '*/*') or (AnsiPos(AMediaType, LowerCase(RawWebRequest.Accept)) = 1);
end;

function TMVCWebRequest.ClientPreferHTML: Boolean;
begin
  Result := ClientPrefer(TMVCMediaType.TEXT_HTML);
end;

function TMVCWebRequest.ClientPreferredLanguage: String;
begin
  Result := FWebRequest.GetFieldByName('Accept-Language');
  if Result.Contains(',') then
  begin
    Result := Result.Split([','])[0];
  end;
end;

function TMVCWebRequest.ContentParam(const AName: string): string;
begin
  Result := FWebRequest.ContentFields.Values[AName];
end;

function TMVCWebRequest.Cookie(const AName: string): string;
begin
  Result := FWebRequest.CookieFields.Values[AName];
end;

constructor TMVCWebRequest.Create(const AWebRequest: TWebRequest;
  const ASerializers: TDictionary<string, IMVCSerializer>);
begin
  inherited Create;
  FBody := EmptyStr;
  FCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET;
  FWebRequest := AWebRequest;
  FSerializers := ASerializers;
  FParamsTable := nil;
  DefineContentType;
end;

procedure TMVCWebRequest.DefineContentType;
begin
{
  While not strictly required nor defined, DMVCFramework supports
  sending body data for all HTTP VERBS
  https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/GET
}
  SplitContentMediaTypeAndCharset(
    FWebRequest.GetFieldByName('Content-Type'),
    FContentMediaType,
    FCharset);
  if FContentMediaType.IsEmpty then
    FContentMediaType := TMVCConstants.DEFAULT_CONTENT_TYPE;
  if FCharset.IsEmpty then
    FCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET;
  FContentType := BuildContentType(FContentMediaType, FCharset);
end;

destructor TMVCWebRequest.Destroy;
begin
  if Assigned(FContentFields) then
  begin
    FContentFields.Free;
  end;
  if Assigned(FQueryParams) then
  begin
    FQueryParams.Free;
  end;
  inherited Destroy;
end;

procedure TMVCWebRequest.EnsureINDY;
begin
  if not(Self is TMVCIndyWebRequest) then
  begin
    raise EMVCException.Create(http_status.InternalServerError,
      'Method available only in INDY implementation');
  end;
end;

procedure TMVCWebRequest.EnsureQueryParamExists(const AName: string);
begin
  if GetParams(AName).IsEmpty then
    raise EMVCException.CreateFmt('Parameter "%s" required', [AName]);
end;

function TMVCWebRequest.GetContentFields: TDictionary<string, string>;
var
  I: Integer;
begin
  if not Assigned(FContentFields) then
  begin
    FContentFields := TDictionary<string, string>.Create;
    for I := 0 to Pred(FWebRequest.ContentFields.Count) do
    begin
      FContentFields.AddOrSetValue(LowerCase(FWebRequest.ContentFields.Names[I]),
        FWebRequest.ContentFields.ValueFromIndex[I]);
    end;
  end;
  Result := FContentFields;
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

function TMVCWebRequest.GetMultiParamsAsArray(const AParamName: String;
  const AStrings: TStrings): TArray<String>;
var
  I,J: Integer;
begin
  SetLength(Result, AStrings.Count);
  J := 0;
  for I := 0 to AStrings.Count - 1 do
  begin
    if SameText(AStrings.Names[I], AParamName) then
    begin
      Result[J] := AStrings.ValueFromIndex[I];
      Inc(J);
    end;
  end;
  SetLength(Result, J);
end;

function TMVCWebRequest.GetOverwrittenHTTPMethod: TMVCHTTPMethodType;
var
  lOverriddenMethod: string;
begin
  lOverriddenMethod := Headers[TMVCConstants.X_HTTP_Method_Override];
  if lOverriddenMethod.IsEmpty then
  begin
    Exit(HTTPMethod);
  end
  else
  begin
    Result := TMVCRouter.StringMethodToHTTPMetod(FWebRequest.Method);
  end;
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
    if Assigned(FParamsTable) and (FParamsTable.Keys.Count > 0) then
    begin
      for N in FParamsTable.Keys.ToArray do
      begin
        Names.Add(N);
      end;
    end;

    if (FWebRequest.QueryFields.Count > 0) then
    begin
      for I := 0 to FWebRequest.QueryFields.Count - 1 do
      begin
        Names.Add(FWebRequest.QueryFields.Names[I]);
      end;
    end;

    if (FWebRequest.ContentFields.Count > 0) then
    begin
      for I := 0 to FWebRequest.ContentFields.Count - 1 do
      begin
        if Names.IndexOf(FWebRequest.ContentFields.Names[I]) = -1 then
        begin
          Names.Add(FWebRequest.ContentFields.Names[I]);
        end;
      end;
    end;

    if (FWebRequest.CookieFields.Count > 0) then
    begin
      for I := 0 to FWebRequest.CookieFields.Count - 1 do
      begin
        Names.Add(FWebRequest.CookieFields.Names[I]);
      end;
    end;

    Result := Names.ToArray;
  finally
    Names.Free;
  end;
end;

function TMVCWebRequest.GetParams(const AParamName: string): string;
begin
  if (not Assigned(FParamsTable)) or (not FParamsTable.TryGetValue(AParamName, Result)) then
  begin
    Result := '';
    if string(FWebRequest.ContentType).StartsWith(TMVCMediaType.APPLICATION_FORM_URLENCODED, True) or
      string(FWebRequest.ContentType).StartsWith(TMVCMediaType.MULTIPART_FORM_DATA, True) then
      Result := FWebRequest.ContentFields.Values[AParamName];
    if Result.IsEmpty then
      Result := FWebRequest.QueryFields.Values[AParamName];
  end;
end;

function TMVCWebRequest.GetContentParamsMulti(const AParamName: string): TArray<string>;
begin
  Result := GetMultiParamsAsArray(AParamName, FWebRequest.ContentFields);
end;

function TMVCWebRequest.GetPathInfo: string;
begin
  Result := FWebRequest.PathInfo;
end;

function TMVCWebRequest.GetQueryParams: TDictionary<string, string>;
var
  I: Integer;
  lRow: String;
begin
  if not Assigned(FQueryParams) then
  begin
    FQueryParams := TDictionary<string, string>.Create;
    for I := 0 to Pred(FWebRequest.QueryFields.Count) do
    begin
      lRow := FWebRequest.QueryFields[i];
      if lRow.Contains('=') then
      begin
        FQueryParams.Add(
          LowerCase(Trim(FWebRequest.QueryFields.Names[I])),
          FWebRequest.QueryFields.ValueFromIndex[I]);
      end
      else
      begin
        FQueryParams.AddOrSetValue(LowerCase(lRow), '');
      end;
    end;
  end;
  Result := FQueryParams;
end;

function TMVCWebRequest.GetQueryParamsMulti(
  const AParamName: string): TArray<string>;
begin
  Result := GetMultiParamsAsArray(AParamName, FWebRequest.QueryFields);
end;

function TMVCWebRequest.QueryString: string;
begin
  Result := FWebRequest.Query;
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

function TMVCWebRequest.HasBody: Boolean;
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
  begin
    try
      Flush;
    except
      on E: Exception do
      begin
        LogException(E, '');
      end;
    end;
  end;
  inherited Destroy;
end;

procedure TMVCWebResponse.Flush;
begin
  if not FWebResponse.Sent then
    FWebResponse.SendResponse;
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

procedure TMVCWebResponse.SetContent(const AValue: string);
begin
  FWebResponse.Content := AValue;
end;

procedure TMVCWebResponse.SetContentStream(const AStream: TStream; const AContentType: string);
begin
  ContentType := AContentType;
  FWebResponse.ContentStream := AStream;
end;

procedure TMVCWebResponse.SetContentType(const AValue: string);
begin
  FWebResponse.ContentType := '';
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
  FCustomData := nil;
end;

destructor TUser.Destroy;
begin
  FRoles.Free;
  FreeAndNil(FCustomData);
  inherited Destroy;
end;

function TUser.IsValid: Boolean;
begin
  Result := (not UserName.IsEmpty) and (LoggedSince > 0);
end;

function TUser.LoadFromSession(const AWebSession: TMVCWebSession): Boolean;
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
    for I := 3 to Length(Pieces) - 1 do
      // https://github.com/danieleteti/delphimvcframework/issues/225
      Roles.Add(Pieces[I]);
  end;
end;

procedure TUser.SaveToSession(const AWebSession: TMVCWebSession);
var
  LRoles: string;
begin
  if (FRoles.Count > 0) then
    LRoles := string.Join('$$', FRoles.ToArray)
  else
    LRoles := '';
  AWebSession[TMVCConstants.CURRENT_USER_SESSION_KEY] := FUserName + '$$' +
    DateTimeToISOTimeStamp(FLoggedSince) + '$$' + FRealm + '$$' + LRoles;
end;

procedure TUser.SetCustomData(const Value: TMVCCustomData);
begin
  FCustomData := Value;
end;

procedure TUser.SetLoggedSince(const AValue: TDateTime);
begin
  if (FLoggedSince = 0) then
    FLoggedSince := AValue
  else
    raise EMVCException.Create('TUser.LoggedSince already set.');
end;

{ TWebContext }

function TWebContext.AddSessionToTheSessionList(const ASessionType, ASessionId: string;
  const ASessionTimeout: Integer): TMVCWebSession;
var
  Session: TMVCWebSession;
begin
  if (Trim(ASessionType) = EmptyStr) then
    raise EMVCException.Create('Empty Session Type');

  TMonitor.Enter(GlobalSessionList);
  try
    Session := TMVCSessionFactory.GetInstance.CreateNewByType(ASessionType, ASessionId,
      ASessionTimeout);
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
    FWebSession := TMVCEngine.GetCurrentSession(ASessionId, False);
    if not Assigned(FWebSession) then
      raise EMVCException.Create('Invalid SessionID');
    FWebSession.MarkAsUsed;
    TMVCEngine.SendSessionCookie(Self, ASessionId);
  end
  else
    raise EMVCException.Create('Session already bounded for this request');
end;

constructor TWebContext.Create(const AServiceContainerResolver: IMVCServiceContainerResolver; const ARequest: TWebRequest; const AResponse: TWebResponse;
  const AConfig: TMVCConfig; const ASerializers: TDictionary<string, IMVCSerializer>);
begin
  inherited Create;
  FServiceContainerResolver := AServiceContainerResolver;
  FIsSessionStarted := False;
  FSessionMustBeClose := False;
  FWebSession := nil;
  FRequest := nil;

  if not IsLibrary then
  begin
    FRequest := TMVCIndyWebRequest.Create(ARequest, ASerializers);
  end
  else
  begin
{$IF Defined(WEBAPACHEHTTP)}
    if ARequest.ClassType = TApacheRequest then
    begin
      FRequest := TMVCApacheWebRequest.Create(ARequest, ASerializers)
    end
    else
{$IF Defined(MSWINDOWS)}
      if ARequest.ClassType = TISAPIRequest then
      begin
        FRequest := TMVCISAPIWebRequest.Create(ARequest, ASerializers)
      end
      else
{$ENDIF} //MSWINDOWS
{$ENDIF} //WEBAPACHEHTTP
      begin
        FRequest := TMVCIndyWebRequest.Create(ARequest, ASerializers);
      end;
  end;

  FResponse := TMVCWebResponse.Create(AResponse);
  FConfig := AConfig;
  FSerializers := ASerializers;
  FData := nil;
  FLoggedUser := nil;
  fIntfObject := nil;
end;

destructor TWebContext.Destroy;
begin
  try
    FResponse.Free;
  except
  end;
  try
    FRequest.Free;
  except
  end;
  try
    FData.Free;
  except
  end;

  fIntfObject := nil;

  try
    FLoggedUser.Free;
  except
  end;
  inherited Destroy;
end;

procedure TWebContext.Flush;
begin
  FResponse.Flush;
end;

function TWebContext.GetData: TMVCStringDictionary;
begin
  if fData = nil then
  begin
    fData := TMVCStringDictionary.Create;
  end;
  Result := fData;
end;

function TWebContext.GetHostingFrameworkType: TMVCHostingFrameworkType;
begin
{$IF Defined(WEBAPACHEHTTP)}
  if FRequest.ClassType = TApacheRequest then
  begin
    Exit(hftApache);
  end;
{$ENDIF}
{$IF Defined(MSWINDOWS)}
    if FRequest.ClassType = TISAPIRequest then
    begin
      Exit(hftISAPI);
  end;
{$ENDIF}
      Exit(hftIndy);
    end;

function TWebContext.GetIntfObject: IInterface;
begin
  Result := fIntfObject;
end;

{ MVCFromBodyAttribute }

constructor MVCFromBodyAttribute.Create(const RootNode: string; const DataType: TMVCDataType);
begin
  inherited Create;
  fRootNode := '';
  fDataType := DataType;
end;

function MVCFromBodyAttribute.DataType: TMVCDataType;
begin
  Result := fDataType;
end;

function MVCFromBodyAttribute.RootNode: String;
begin
  Result := fRootNode;
end;

{ TMVCErrorResponseItem }

constructor TMVCErrorResponseItem.Create(const AMessage: string);
begin
  inherited Create;
  FMessage := AMessage;
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

function TWebContext.GetWebSession: TMVCWebSession;
var
  lSessionIDFromRequest: string;
  lSessionType: String;
begin
  if not Assigned(FWebSession) then
  begin
    lSessionIDFromRequest := TMVCEngine.ExtractSessionIdFromWebRequest(FRequest.RawWebRequest);
    FWebSession := TMVCEngine.GetCurrentSession(lSessionIDFromRequest, False);
    if not Assigned(FWebSession) then
    begin
      lSessionType := Config[TMVCConfigKey.SessionType];
      if not TMVCSessionFactory.GetInstance.TryFindSessionID(lSessionType, lSessionIDFromRequest) then
      begin
        SessionStart(lSessionType);
      end
      else
      begin
        FWebSession := AddSessionToTheSessionList(
          lSessionType,
          lSessionIDFromRequest,
          StrToInt(Config[TMVCConfigKey.SessionTimeout]));
        TMVCEngine.SendSessionCookie(Self, FWebSession.SessionId);
      end;
    end
    else
    begin
      TMVCEngine.SendSessionCookie(Self, FWebSession.SessionId);
    end;
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

procedure TWebContext.SessionStart(const SessionType: String);
var
  ID: string;
begin
  if not Assigned(FWebSession) then
  begin
    ID := TMVCEngine.SendSessionCookie(Self);
    FWebSession := AddSessionToTheSessionList(SessionType, ID,
      StrToInt64(Config[TMVCConfigKey.SessionTimeout]));
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
  Cookie.name := TMVCConstants.SESSION_TOKEN_NAME;

  Cookie.Value := GUIDToString(TGUID.NewGuid) + 'invalid' + GUIDToString(TGUID.NewGuid);
  Cookie.Expires := EncodeDate(1970, 1, 1);
  Cookie.Path := '/';

  TMonitor.Enter(GlobalSessionList);
  try
    SID := SessionId;
    if (SId = '') and (ARaiseExceptionIfExpired) then
    begin
      raise EMVCSessionExpiredException.Create('Session not started');
    end;

    GlobalSessionList.Remove(SId);

    if SId <> '' then
    begin
      FWebSession := nil;
      try
        TMVCSessionFactory.GetInstance.TryDeleteSessionID(Config[TMVCConfigKey.SessionType], SId);
      except
        on E: Exception do
        begin
          LogException(E, 'Cannot delete session file for sessionid: ' + SId);
        end;
      end;
    end;
  finally
    TMonitor.Exit(GlobalSessionList);
  end;

  FIsSessionStarted := False;
  FSessionMustBeClose := True;
end;

procedure TWebContext.SetIntfObject(const Value: IInterface);
begin
  fIntfObject := Value;
end;

procedure TWebContext.SetParamsTable(const AValue: TMVCRequestParamsTable);
begin
  FRequest.ParamsTable := AValue;
end;

{ TMVCEngine }

function TMVCEngine.AddController(const AControllerClazz: TMVCControllerClazz;
  const AURLSegment: string): TMVCEngine;
begin
  Result := AddController(AControllerClazz, nil, AURLSegment);
end;

function TMVCEngine.AddController(const AControllerClazz: TMVCControllerClazz;
  const ACreateAction: TMVCControllerCreateAction; const AURLSegment: string): TMVCEngine;
begin
  FControllers.Add(TMVCControllerDelegate.Create(AControllerClazz, ACreateAction, AURLSegment));
  Result := Self;
end;

function TMVCEngine.AddMiddleware(const AMiddleware: IMVCMiddleware): TMVCEngine;
begin
  FMiddlewares.Add(AMiddleware);
  Result := Self;
end;

function TMVCEngine.AddSerializer(const AContentType: string; const ASerializer: IMVCSerializer)
  : TMVCEngine;
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
  while True do
  begin
    if I = ACookies.Count then
      Break;
    Cookie := ACookies[I];
    if LowerCase(Cookie.name) = SessionCookieName then
      ACookies.Delete(I)
    else
      Inc(I);
  end;
end;

procedure TMVCEngine.ConfigDefaultValues;
begin
  LogI('Loading Config default values');

  Config[TMVCConfigKey.SessionTimeout] := '30' { 30 minutes };
  Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
  Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
  Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
  Config[TMVCConfigKey.ViewPath] := 'templates';
  Config[TMVCConfigKey.PathPrefix] := '';
  Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
  Config[TMVCConfigKey.ServerName] := 'DelphiMVCFramework';
  Config[TMVCConfigKey.ExposeServerSignature] := 'true';
  Config[TMVCConfigKey.ExposeXPoweredBy] := 'true';
  Config[TMVCConfigKey.SessionType] := 'memory';
  Config[TMVCConfigKey.MaxEntitiesRecordCount] := '20';
  Config[TMVCConfigKey.MaxRequestSize] := IntToStr(TMVCConstants.DEFAULT_MAX_REQUEST_SIZE);
  Config[TMVCConfigKey.HATEOSPropertyName] := '_links';
  Config[TMVCConfigKey.LoadSystemControllers] := 'true';

  fOnRouterLog :=
      procedure(
        const Sender: TMVCCustomRouter;
        const RouterLogState: TMVCRouterLogState;
        const Context: TWebContext)
    begin
      case RouterLogState of
        rlsRouteFound:
          begin
            LogI(Context.Request.HTTPMethodAsString + ':' +
              Context.Request.PathInfo + ' [' + Context.Request.ClientIp + '] -> ' +
              Sender.GetQualifiedActionName + ' - ' + IntToStr(Context.Response.StatusCode) + ' ' +
              Context.Response.ReasonString);
          end;
        rlsRouteNotFound:
          begin
            LogW(Context.Request.HTTPMethodAsString + ':' +
              Context.Request.PathInfo + ' [' + Context.Request.ClientIp + '] -> {ROUTE NOT FOUND} - ' +
              IntToStr(Context.Response.StatusCode) + ' ' + Context.Response.ReasonString);
          end;
      else
        raise EMVCException.Create('Invalid RouterLogState');
      end;
    end;
end;

constructor TMVCEngine.Create(const AWebModule: TWebModule; const AConfigAction: TProc<TMVCConfig>);
begin
  inherited Create(AWebModule);
  FWebModule := AWebModule;
  FixUpWebModule;
  FConfig := TMVCConfig.Create;
  FSerializers := TDictionary<string, IMVCSerializer>.Create;
  FMiddlewares := TList<IMVCMiddleware>.Create;
  FControllers := TObjectList<TMVCControllerDelegate>.Create(True);
  FApplicationSession := nil;
  FSavedOnBeforeDispatch := nil;
  WebRequestHandler.CacheConnections := True;
  WebRequestHandler.MaxConnections := 4096;

  ConfigDefaultValues;

  if Assigned(AConfigAction) then
  begin
    fConfig.dotEnv := dotEnv;
    AConfigAction(FConfig);
  end;
  FConfig.Freeze;
  SaveCacheConfigValues;
  RegisterDefaultsSerializers;
  LoadSystemControllers;
end;


//procedure TMVCEngine.FillActualParamsForConstructor(
//  const AActionFormalParams: TArray<TRttiParameter>;
//  var AActualParams: TArray<TValue>);
//var
//  lParamName: string;
//  I: Integer;
//  lIntf, lOutIntf: IInterface;
//begin
//  SetLength(AActualParams, Length(AActionFormalParams));
//  for I := 0 to Length(AActionFormalParams) - 1 do
//  begin
//    lParamName := AActionFormalParams[I].name;
//    lIntf := fServiceContainer.Resolve(AActionFormalParams[I].ParamType.Handle);
//    if not Supports(lIntf, AActionFormalParams[I].ParamType.Handle.TypeData.GUID, lOutIntf) then
//    begin
//      raise EMVCException.Create('Cannot inject ' + AActionFormalParams[I].Name + ' into constructor ' + );
//    end;
//    TValue.Make(@lOutIntf, AActionFormalParams[I].ParamType.Handle, AActualParams[I]);
//  end;
//end;
//

function TMVCEngine.CreateControllerWithDependencies(
  const Context: TWebContext;
  const ControllerClass: TMVCControllerClazz;
  const ConstructorMethod: TRttiMethod): TMVCController;
var
  lActionFormalParams: TArray<TRttiParameter>;
  lActualParams: TArray<TValue>;
  I: Integer;
  lIntf, lOutIntf: IInterface;
  lInjectAttribute: MVCInjectAttribute;
  lServiceName: string;
begin
  lActionFormalParams := ConstructorMethod.GetParameters;
  SetLength(lActualParams, Length(lActionFormalParams));
  if Length(lActionFormalParams) > 0 then
  begin
    for I := 0 to Length(lActionFormalParams) - 1 do
    begin
      lServiceName := '';
      {$IF Defined(ALEXANDRIAORBETTER)}
      lInjectAttribute := lActionFormalParams[I].GetAttribute<MVCInjectAttribute>;
      {$ELSE}
      lInjectAttribute := TRttiUtils.GetAttribute<MVCInjectAttribute>(lActionFormalParams[I]);
      {$ENDIF}

      if lInjectAttribute <> nil then
      begin
        lServiceName := lInjectAttribute.ServiceName;
      end;
      if (lActionFormalParams[I].ParamType.TypeKind <> tkInterface) then
      begin
        raise EMVCException.CreateFmt('Parameter "%s" is not an interface type', [lActionFormalParams[i].ToString]);
      end;
      lIntf := Context.ServiceContainerResolver.Resolve(lActionFormalParams[I].ParamType.Handle, lServiceName);
      Supports(lIntf, lActionFormalParams[I].ParamType.Handle.TypeData.GUID, lOutIntf);
      TValue.Make(@lOutIntf, lActionFormalParams[I].ParamType.Handle, lActualParams[I]);
    end;
  end;
  Result := TMVCController(ConstructorMethod.Invoke(ControllerClass, lActualParams).AsObject);
end;

function TMVCEngine.CustomExceptionHandling(const Ex: Exception;
  const ASelectedController: TMVCController; const AContext: TWebContext): Boolean;
begin
  Result := False;
  if Assigned(FOnException) then
  begin
    FOnException(Ex, ASelectedController, AContext, Result);
  end;
end;

procedure TMVCEngine.DefineDefaultResponseHeaders(const AContext: TWebContext);
begin
  if FConfigCache_ExposeServerSignature and (not IsLibrary) then
    AContext.Response.CustomHeaders.Values['Server'] := FConfigCache_ServerSignature;
  if FConfigCache_ExposeXPoweredBy then
    AContext.Response.CustomHeaders.Values['X-Powered-By'] := 'DMVCFramework ' +
      DMVCFRAMEWORK_VERSION;
  AContext.Response.RawWebResponse.Date := Now;
end;

destructor TMVCEngine.Destroy;
begin
  fConfig.Free;
  fSerializers.Free;
  fMiddlewares.Free;
  fControllers.Free;
  inherited Destroy;
end;

procedure TMVCEngine.DoWebContextCreateEvent(const AContext: TWebContext);
begin
  if Assigned(fWebContextCreateEvent) then
  begin
    fWebContextCreateEvent(AContext);
  end;
end;

procedure TMVCEngine.DoWebContextDestroyEvent(const AContext: TWebContext);
begin
  if Assigned(fWebContextDestroyEvent) then
  begin
    fWebContextDestroyEvent(AContext);
  end;
end;

function TMVCEngine.ExecuteAction(const ASender: TObject; const ARequest: TWebRequest;
  const AResponse: TWebResponse): Boolean;
var
  lParamsTable: TMVCRequestParamsTable;
  lContext: TWebContext;
  lRouter: TMVCRouter;
  lHandled: Boolean;
  lResponseContentMediaType: string;
  lResponseContentCharset: string;
  lRouterMethodToCallName: string;
  lRouterControllerClazzQualifiedClassName: string;
  lSelectedController: TMVCController;
  lActionFormalParams: TArray<TRttiParameter>;
  lActualParams: TArray<TValue>;
  lBodyParameter, lResponseObject: TObject;
  lInvokeResult: TValue;
  lObjList: IMVCList;
  lRespStatus: Integer;
begin
  Result := False;

  if ARequest.ContentLength > FConfigCache_MaxRequestSize then
  begin
    raise EMVCException.CreateFmt(http_status.RequestEntityTooLarge,
      'Request size exceeded the max allowed size [%d KiB] (1)',
      [(FConfigCache_MaxRequestSize div 1024)]);
  end;

{$IF Defined(BERLINORBETTER)}
  ARequest.ReadTotalContent;

  // Double check for malicious content-length header
  if ARequest.ContentLength > FConfigCache_MaxRequestSize then
  begin
    raise EMVCException.CreateFmt(http_status.RequestEntityTooLarge,
      'Request size exceeded the max allowed size [%d KiB] (2)',
      [(FConfigCache_MaxRequestSize div 1024)]);
  end;
{$ENDIF}
  lParamsTable := TMVCRequestParamsTable.Create;
  try
    lContext := TWebContext.Create(NewServiceContainerResolver, ARequest, AResponse, FConfig, FSerializers);
    try
      DefineDefaultResponseHeaders(lContext);
      DoWebContextCreateEvent(lContext);
      lHandled := False;
      lRouter := TMVCRouter.Create(FConfig, gMVCGlobalActionParamsCache);
      try // finally
        lSelectedController := nil;
        try // only for lSelectedController
          try // global exception handler
            ExecuteBeforeRoutingMiddleware(lContext, lHandled);
            if not lHandled then
            begin
              if lRouter.ExecuteRouting(ARequest.PathInfo,
                lContext.Request.GetOverwrittenHTTPMethod { lContext.Request.HTTPMethod } ,
                ARequest.ContentType, ARequest.Accept, FControllers,
                FConfigCache_DefaultContentType, FConfigCache_DefaultContentCharset,
                FConfigCache_PathPrefix, lParamsTable, lResponseContentMediaType,
                lResponseContentCharset) then
              begin
                try
                  if lRouter.ControllerCreateAction <> nil then
                  begin
                    lSelectedController := lRouter.ControllerCreateAction();
                  end
                  else if lRouter.ControllerInjectableConstructor <> nil then
                  begin
                    lSelectedController := CreateControllerWithDependencies(
                      lContext,
                      lRouter.ControllerClazz,
                      lRouter.ControllerInjectableConstructor);
                  end
                  else
                  begin
                    lSelectedController := lRouter.ControllerClazz.Create;
                  end;
                except
                  on Ex: Exception do
                  begin
                    Log.Error('[%s] %s [PathInfo "%s"] (Custom message: "%s")',
                      [Ex.Classname, Ex.Message, GetRequestShortDescription(ARequest), 'Cannot create controller'], LOGGERPRO_TAG);
                    raise EMVCException.Create(http_status.InternalServerError,
                      'Cannot create controller (see log for more info)');
                  end;
                end;
                lRouterMethodToCallName := lRouter.MethodToCall.Name;
                lRouterControllerClazzQualifiedClassName := lRouter.ControllerClazz.QualifiedClassName;

                MVCFramework.Logger.InitThreadVars;

                lContext.fActionQualifiedName := lRouterControllerClazzQualifiedClassName + '.'+ lRouterMethodToCallName;
                lSelectedController.Engine := Self;
                lSelectedController.Context := lContext;
                lSelectedController.ApplicationSession := FApplicationSession;
                lContext.ParamsTable := lParamsTable;
                ExecuteBeforeControllerActionMiddleware(
                  lContext,
                  lRouterControllerClazzQualifiedClassName,
                  lRouterMethodToCallName,
                  lHandled);
                if not lHandled then
                begin
                  lBodyParameter := nil;
                  lSelectedController.MVCControllerAfterCreate;
                  try
                    lHandled := False;
                    lSelectedController.ContentType := BuildContentType(lResponseContentMediaType,
                      lResponseContentCharset);
                    lActionFormalParams := lRouter.MethodToCall.GetParameters;
                    if (Length(lActionFormalParams) = 0) then
                      SetLength(lActualParams, 0)
                    else if (Length(lActionFormalParams) = 1) and
                      (SameText(lActionFormalParams[0].ParamType.QualifiedName,
                      'MVCFramework.TWebContext')) then
                    begin
                      SetLength(lActualParams, 1);
                      lActualParams[0] := lContext;
                    end
                    else
                    begin
                      FillActualParamsForAction(lSelectedController, lContext, lActionFormalParams,
                        lRouterMethodToCallName, lActualParams, lBodyParameter);
                    end;
                    lSelectedController.OnBeforeAction(lContext, lRouterMethodToCallName, lHandled);
                    if not lHandled then
                    begin
                      try
                        if lRouter.MethodToCall.MethodKind = mkProcedure then
                        begin
                          lRouter.MethodToCall.Invoke(lSelectedController, lActualParams);
                        end
                        else
                        begin
                          lInvokeResult := lRouter.MethodToCall.Invoke(lSelectedController, lActualParams);
                          case lInvokeResult.Kind of
                            tkInterface:
                            begin
                              if Supports(lInvokeResult.AsInterface, IMVCResponse) then
                              begin
                                TMVCRenderer.InternalRenderMVCResponse(lSelectedController, TMVCResponse(lInvokeResult.AsInterface));
                              end
                              else
                              begin
                                lSelectedController.Render(lInvokeResult.AsInterface);
                              end;
                            end;
                            tkClass:
                            begin
                              lResponseObject := lInvokeResult.AsObject;
                              try
                                if lResponseObject <> nil then
                                begin
                                  // https://learn.microsoft.com/en-us/aspnet/core/web-api/action-return-types?view=aspnetcore-7.0
                                  if lResponseObject is TDataSet then
                                  begin
                                    lSelectedController.Render(TDataSet(lResponseObject), False);
                                  end
                                  else if lResponseObject is TStream then
                                  begin
                                    lContext.Response.RawWebResponse.Content := EmptyStr;
                                    lContext.Response.RawWebResponse.ContentType := lContext.Response.ContentType;
                                    lContext.Response.RawWebResponse.ContentStream := TStream(lResponseObject);
                                    lContext.Response.RawWebResponse.FreeContentStream := True;
                                    lResponseObject := nil; //do not free it!!
                                  end
                                  else if lResponseObject is TMVCResponse then
                                  begin
                                    TMVCRenderer.InternalRenderMVCResponse(lSelectedController, TMVCResponse(lResponseObject));
                                  end
                                  else if (not lResponseObject.InheritsFrom(TJsonBaseObject)) and TDuckTypedList.CanBeWrappedAsList(lResponseObject, lObjList) then
                                  begin
                                    lSelectedController.Render(lObjList);
                                  end
                                  else
                                  begin
                                    lSelectedController.Render(lResponseObject, False);
                                  end;
                                end
                                else
                                begin
                                  lSelectedController.Render(TObject(nil));
                                end;
                              finally
                                lResponseObject.Free;
                              end
                            end;
                            tkRecord:
                            begin
                              lSelectedController.Render(
                                lSelectedController.Serializer(lSelectedController.GetContentType)
                                  .SerializeRecord(lInvokeResult.GetReferenceToRawData,
                                  lInvokeResult.TypeInfo,
                                  TMVCSerializationType.stFields,nil,nil));
                            end;
                            tkArray, tkDynArray:
                            begin
                              lSelectedController.Render(
                                lSelectedController.Serializer(lSelectedController.GetContentType)
                                  .SerializeArrayOfRecord(lInvokeResult,
                                    TMVCSerializationType.stFields,nil,nil));
                            end;
                            tkUString, tkString:
                            begin
                              lSelectedController.Render(lInvokeResult.AsString);
                            end;
                            tkEnumeration:
                            begin
                              lSelectedController.Render(GetEnumName(lInvokeResult.TypeInfo, lInvokeResult.AsOrdinal));
                            end;
                            tkFloat:
                            begin
                              lSelectedController.Render(FloatToStr(lInvokeResult.AsExtended, GetDefaultFormatSettings));
                            end;
                            tkInteger:
                            begin
                              lSelectedController.Render(IntToStr(lInvokeResult.AsInteger));
                            end;
                            tkInt64:
                            begin
                              lSelectedController.Render(IntToStr(lInvokeResult.AsInt64));
                            end
                            else
                            begin
                              RaiseSerializationError('Cannot serialize type ' + lInvokeResult.TypeInfo.Name);
                            end;
                          end;
                        end;
                      finally
                        lSelectedController.OnAfterAction(lContext, lRouterMethodToCallName);
                      end;
                    end;
                  finally
                    try
                      lBodyParameter.Free;
                    except
                      on E: Exception do
                      begin
                        LogE(Format('Cannot free Body object: [CLS: %s][MSG: %s]',
                          [E.Classname, E.Message]));
                      end;
                    end;
                    lSelectedController.MVCControllerBeforeDestroy;
                  end;
                  lContext.Response.ContentType := lSelectedController.ContentType;
                  Result := True; //handled
                end; //if not handled by OnBeforeControllerActionMiddleware
                ExecuteAfterControllerActionMiddleware(lContext,
                  lRouterControllerClazzQualifiedClassName,
                  lRouterMethodToCallName,
                  lHandled);
                fOnRouterLog(lRouter, rlsRouteFound, lContext);
              end
              else // execute-routing
              begin
                if Config[TMVCConfigKey.AllowUnhandledAction] = 'false' then
                begin
                  lContext.Response.StatusCode := http_status.NotFound;
                  lContext.Response.ReasonString := 'Not Found';
                  SendHTTPStatus(lContext, HTTP_STATUS.NotFound);
                  fOnRouterLog(lRouter, rlsRouteNotFound, lContext);
                end
                else
                begin
                  lContext.Response.FlushOnDestroy := False;
                end;
              end; // end-execute-routing
            end; // if not handled by beforerouting
          except
            on ESess: EMVCSessionExpiredException do
            begin
              if not CustomExceptionHandling(ESess, lSelectedController, lContext) then
              begin
                Log.Error('[%s] %s [PathInfo "%s"] - %d %s (Custom message: "%s")',
                  [
                    ESess.Classname,
                    ESess.Message,
                    GetRequestShortDescription(ARequest),
                    ESess.HTTPStatusCode,
                    HTTP_STATUS.ReasonStringFor(ESess.HTTPStatusCode),
                    ESess.DetailedMessage
                  ], LOGGERPRO_TAG);
                lContext.SessionStop;
                lSelectedController.ResponseStatus(ESess.HTTPStatusCode);
                lSelectedController.Render(ESess);
              end;
            end;
            on E: EMVCException do
            begin
              if not CustomExceptionHandling(E, lSelectedController, lContext) then
              begin
                Log.Error('[%s] %s [PathInfo "%s"] - %d %s (Custom message: "%s")',
                  [
                    E.Classname,
                    E.Message,
                    GetRequestShortDescription(ARequest),
                    E.HTTPStatusCode,
                    HTTP_STATUS.ReasonStringFor(E.HTTPStatusCode),
                    E.DetailedMessage
                  ], LOGGERPRO_TAG);
                if Assigned(lSelectedController) then
                begin
                  lSelectedController.ResponseStatus(E.HTTPStatusCode);
                  lSelectedController.Render(E);
                end
                else
                begin
                  SendHTTPStatus(lContext, E.HTTPStatusCode, E.Message, E.Classname);
                end;
              end;
            end;
            on EIO: EInvalidOp do
            begin
              if not CustomExceptionHandling(EIO, lSelectedController, lContext) then
              begin
                Log.Error('[%s] %s [PathInfo "%s"] - %d %s (Custom message: "%s")',
                  [
                    EIO.Classname,
                    EIO.Message,
                    GetRequestShortDescription(ARequest),
                    HTTP_STATUS.InternalServerError,
                    HTTP_STATUS.ReasonStringFor(HTTP_STATUS.InternalServerError),
                    'Invalid Op'], LOGGERPRO_TAG);
                if Assigned(lSelectedController) then
                begin
                  lSelectedController.ResponseStatus(HTTP_STATUS.InternalServerError);
                  lSelectedController.Render(EIO);
                end
                else
                begin
                  SendHTTPStatus(lContext, HTTP_STATUS.InternalServerError,
                    Format('[%s] %s', [EIO.Classname, EIO.Message]), EIO.Classname);
                end;
              end;
            end;
            on Ex: Exception do
            begin
              if Ex is ESqidsException then
              begin
                lRespStatus := HTTP_STATUS.BadRequest;
              end
              else
              begin
                lRespStatus := HTTP_STATUS.InternalServerError;
              end;

              if not CustomExceptionHandling(Ex, lSelectedController, lContext) then
              begin
                Log.Error('[%s] %s [PathInfo "%s"] - %d %s (Custom message: "%s")',
                  [
                    Ex.Classname,
                    Ex.Message,
                    GetRequestShortDescription(ARequest),
                    lRespStatus,
                    HTTP_STATUS.ReasonStringFor(lRespStatus),
                    'Global Action Exception Handler'
                  ], LOGGERPRO_TAG);
                if Assigned(lSelectedController) then
                begin
                  lSelectedController.ResponseStatus(lRespStatus);
                  lSelectedController.Render(Ex);
                end
                else
                begin
                  SendHTTPStatus(lContext, lRespStatus,
                    Format('[%s] %s', [Ex.Classname, Ex.Message]), Ex.Classname);
                end;
              end;
            end;
          end;
          try
            ExecuteAfterRoutingMiddleware(lContext, lHandled);
          except
            on Ex: Exception do
            begin
              if not CustomExceptionHandling(Ex, lSelectedController, lContext) then
              begin
                Log.Error('[%s] %s [PathInfo "%s"] - %d %s (Custom message: "%s")',
                  [
                    Ex.Classname,
                    Ex.Message,
                    GetRequestShortDescription(ARequest),
                    HTTP_STATUS.InternalServerError,
                    HTTP_STATUS.ReasonStringFor(HTTP_STATUS.InternalServerError),
                    'After Routing Exception Handler'
                  ], LOGGERPRO_TAG);
                if Assigned(lSelectedController) then
                begin
                  { middlewares *must* not raise unhandled exceptions }
                  lSelectedController.ResponseStatus(HTTP_STATUS.InternalServerError);
                  lSelectedController.Render(Ex);
                end
                else
                begin
                  SendHTTPStatus(lContext, http_status.InternalServerError,
                    Format('[%s] %s', [Ex.Classname, Ex.Message]), Ex.Classname);
                end;
              end;
            end;
          end;
        finally
          FreeAndNil(lSelectedController);
        end;
      finally
        lRouter.Free;
      end;
    finally
      DoWebContextDestroyEvent(lContext);
      lContext.Free;
    end;
  finally
    lParamsTable.Free;
  end;
end;

procedure TMVCEngine.ExecuteAfterControllerActionMiddleware(const AContext: TWebContext;
  const AControllerQualifiedClassName: string; const AActionName: string;
  const AHandled: Boolean);
var
  I: Integer;
begin
  for I := 0 to FMiddlewares.Count - 1 do
  begin
    FMiddlewares[I].OnAfterControllerAction(AContext, AControllerQualifiedClassName, AActionName, AHandled);
  end;
end;

procedure TMVCEngine.ExecuteAfterRoutingMiddleware(const AContext: TWebContext;
  const AHandled: Boolean);
var
  I: Integer;
begin
  for I := 0 to FMiddlewares.Count - 1 do
  begin
    FMiddlewares[I].OnAfterRouting(AContext, AHandled);
  end;
end;

procedure TMVCEngine.ExecuteBeforeControllerActionMiddleware(const AContext: TWebContext;
  const AControllerQualifiedClassName: string; const AActionName: string; var AHandled: Boolean);
var
  Middleware: IMVCMiddleware;
begin
  if not AHandled then
  begin
    for Middleware in FMiddlewares do
    begin
      Middleware.OnBeforeControllerAction(AContext, AControllerQualifiedClassName, AActionName,
        AHandled);
      if AHandled then
      begin
        Break;
      end;
    end;
  end;
end;

procedure TMVCEngine.ExecuteBeforeRoutingMiddleware(const AContext: TWebContext;
  var AHandled: Boolean);
var
  Middleware: IMVCMiddleware;
begin
  if not AHandled then
  begin
    for Middleware in FMiddlewares do
    begin
      Middleware.OnBeforeRouting(AContext, AHandled);
      if AHandled then
      begin
        Break;
      end;
    end;
  end;
end;

class function TMVCEngine.ExtractSessionIdFromWebRequest(const AWebRequest: TWebRequest): string;
begin
  Result := AWebRequest.CookieFields.Values[TMVCConstants.SESSION_TOKEN_NAME];
  if not Result.IsEmpty then
    Result := TIdURI.URLDecode(Result);
end;

procedure TMVCEngine.FillActualParamsForAction(const ASelectedController: TMVCController;
  const AContext: TWebContext; const AActionFormalParams: TArray<TRttiParameter>;
  const AActionName: string; var AActualParams: TArray<TValue>; out ABodyParameter: TObject);
var
  lParamName: string;
  I: Integer;
  lStrValue: string;
  lFromBodyAttribute: MVCFromBodyAttribute;
  lFromQueryStringAttribute: MVCFromQueryStringAttribute;
  lFromContentFieldAttribute: MVCFromContentFieldAttribute;
  lFromHeaderAttribute: MVCFromHeaderAttribute;
  lFromCookieAttribute: MVCFromCookieAttribute;
  lInjectAttribute: MVCInjectAttribute;
  lAttributeInjectedParamCount: Integer;
  lInjectedParamValue: string;
  lInjectedMultiParamValue: TArray<String>;
  lList: IMVCList;
  lItemClass: TClass;
  lIntf, lOutIntf: IInterface;
begin
  ABodyParameter := nil;
  lAttributeInjectedParamCount := 0;
  SetLength(AActualParams, Length(AActionFormalParams));
  for I := 0 to Length(AActionFormalParams) - 1 do
  begin
    lParamName := AActionFormalParams[I].name;
    if Length(AActionFormalParams[I].GetAttributes) > 0 then
    begin
      // Let's check how to inject this parameter
      if TRttiUtils.HasAttribute<MVCFromBodyAttribute>(AActionFormalParams[I], lFromBodyAttribute)
      then
      begin
        Inc(lAttributeInjectedParamCount, 1);
        if AActionFormalParams[I].ParamType.QualifiedName <> 'System.string' then
        begin
          ABodyParameter := TRttiUtils.CreateObject(AActionFormalParams[I].ParamType.QualifiedName);
          if TDuckTypedList.CanBeWrappedAsList(ABodyParameter, lList) then
          begin
            lItemClass := TMVCAbstractSerializer(ASelectedController.Serializer).GetObjectTypeOfGenericList(ABodyParameter.ClassInfo);
            ASelectedController.Serializer.DeserializeCollection(ASelectedController.Context.Request.Body,
              ABodyParameter, lItemClass, stDefault, [], lFromBodyAttribute.RootNode);
          end
          else
          begin
            ASelectedController.Serializer(AContext.Request.ContentType, True).DeserializeObject(ASelectedController.Context.Request.Body,
              ABodyParameter, stDefault, [], lFromBodyAttribute.RootNode);
          end;
          AActualParams[I] := ABodyParameter;
        end
        else
        begin
          AActualParams[I] := ASelectedController.Context.Request.Body;
          Continue;
        end;
      end
      else if TRttiUtils.HasAttribute<MVCFromQueryStringAttribute>(AActionFormalParams[I],
        lFromQueryStringAttribute) then
      begin
        Inc(lAttributeInjectedParamCount, 1);
        lInjectedParamValue := URLDecode(AContext.Request.QueryStringParam
          (lFromQueryStringAttribute.ParamName));
        HandleDefaultValueForInjectedParameter(lInjectedParamValue, lFromQueryStringAttribute);
        AActualParams[I] := GetActualParam(AActionFormalParams[I], lInjectedParamValue);
      end
      else if TRttiUtils.HasAttribute<MVCFromContentFieldAttribute>(AActionFormalParams[I],
        lFromContentFieldAttribute) then
      begin
        Inc(lAttributeInjectedParamCount, 1);
        if AActionFormalParams[I].ParamType.QualifiedName.StartsWith('System.TArray<System.', True) then
        begin
          lInjectedMultiParamValue := AContext.Request.ContentParamsMulti[lFromContentFieldAttribute.ParamName];
          AActualParams[I] := GetActualParamMulti(AActionFormalParams[I], lInjectedMultiParamValue);
        end
        else
        begin
          lInjectedParamValue := AContext.Request.ContentParam(lFromContentFieldAttribute.ParamName);
          HandleDefaultValueForInjectedParameter(lInjectedParamValue, lFromContentFieldAttribute);
          AActualParams[I] := GetActualParam(AActionFormalParams[I], lInjectedParamValue);
        end;
      end
      else if TRttiUtils.HasAttribute<MVCFromHeaderAttribute>(AActionFormalParams[I],
        lFromHeaderAttribute) then
      begin
        Inc(lAttributeInjectedParamCount, 1);
        lInjectedParamValue := AContext.Request.GetHeader(lFromHeaderAttribute.ParamName);
        HandleDefaultValueForInjectedParameter(lInjectedParamValue, lFromHeaderAttribute);
        AActualParams[I] := GetActualParam(AActionFormalParams[I], lInjectedParamValue);
      end
      else if TRttiUtils.HasAttribute<MVCFromCookieAttribute>(AActionFormalParams[I],
        lFromCookieAttribute) then
      begin
        Inc(lAttributeInjectedParamCount, 1);
        lInjectedParamValue := AContext.Request.Cookie(lFromCookieAttribute.ParamName);
        HandleDefaultValueForInjectedParameter(lInjectedParamValue, lFromCookieAttribute);
        AActualParams[I] := GetActualParam(AActionFormalParams[I], lInjectedParamValue);
      end
      else if TRttiUtils.HasAttribute<MVCInjectAttribute>(AActionFormalParams[I],
        lInjectAttribute) then
      begin
        Inc(lAttributeInjectedParamCount, 1);
        lIntf := AContext.ServiceContainerResolver.Resolve(AActionFormalParams[I].ParamType.Handle, lInjectAttribute.ServiceName);
        Supports(lIntf, AActionFormalParams[I].ParamType.Handle.TypeData.GUID, lOutIntf);
        TValue.Make(@lOutIntf, AActionFormalParams[I].ParamType.Handle, AActualParams[I]);
      end
      else
      begin
        raise EMVCException.Create(http_status.InternalServerError,
          'Unknown custom attribute on action parameter: ' + AActionFormalParams[I].name +
          '. [HINT: Allowed attributes are MVCFromBody, MVCFromQueryString, MVCFromHeader, MVCFromCookie, MVCInject]');
      end;
      Continue;
    end;

    // From now on we'll check for url mapped parameters
    if not AContext.Request.SegmentParam(lParamName, lStrValue) then
    begin
      raise EMVCException.CreateFmt(http_status.BadRequest,
        'Invalid parameter %s for action %s (Hint: Here parameters names are case-sensitive)',
        [lParamName, AActionName]);
    end;
    AActualParams[I] := GetActualParam(AActionFormalParams[I], lStrValue);
  end;

  if (AContext.Request.SegmentParamsCount + lAttributeInjectedParamCount) <>
    Length(AActionFormalParams) then
  begin
    raise EMVCException.CreateFmt(http_status.BadRequest,
      'Parameters count mismatch (expected %d actual %d) for action "%s"',
      [Length(AActionFormalParams), AContext.Request.SegmentParamsCount, AActionName]);
  end;
end;

procedure TMVCEngine.FixUpWebModule;
begin
  FSavedOnBeforeDispatch := FWebModule.BeforeDispatch;
  FWebModule.BeforeDispatch := OnBeforeDispatch;
end;

function TMVCEngine.GetActualParam(const AFormalParam: TRttiParameter;
  const AStringValue: String): TValue;
var lWasDateTime: Boolean; lQualifiedName: String;
  lFormatSettings: TFormatSettings;
begin
  case AFormalParam.ParamType.TypeKind of
    tkInteger:
      try
        Result := StrToInt(AStringValue);
      except
        on E: Exception do
        begin
          raise EMVCException.CreateFmt(http_status.BadRequest,
            'Invalid Integer value for param [%s] - [CLASS: %s][MSG: %s]',
            [AFormalParam.name, E.Classname, E.Message]);
        end;
      end;
    tkInt64:
      try
        Result := StrToInt64(AStringValue);
      except
        on E: Exception do
        begin
          raise EMVCException.CreateFmt(http_status.BadRequest,
            'Invalid Int64 value for param [%s] - [CLASS: %s][MSG: %s]',
            [AFormalParam.name, E.Classname, E.Message]);
        end;
      end;
    tkUString:
      begin
        Result := AStringValue;
      end;
    tkFloat:
      begin
        lWasDateTime := False;
        lQualifiedName := AFormalParam.ParamType.QualifiedName;
        if lQualifiedName = 'System.TDate' then
        begin
          try
            lWasDateTime := True;
            Result := ISODateToDate(AStringValue);
          except
            on E: Exception do
            begin
              raise EMVCException.CreateFmt(http_status.BadRequest,
                'Invalid TDate value for param [%s] - [CLASS: %s][MSG: %s]',
                [AFormalParam.name, E.Classname, E.Message]);
            end;
          end;
        end
        else if lQualifiedName = 'System.TDateTime' then
        begin
          try
            lWasDateTime := True;
            Result := ISOTimeStampToDateTime(AStringValue);
          except
            on E: Exception do
            begin
              raise EMVCException.CreateFmt(http_status.BadRequest,
                'Invalid TDateTime value for param [%s] - [CLASS: %s][MSG: %s]',
                [AFormalParam.name, E.Classname, E.Message]);
            end;
          end;
        end
        else if lQualifiedName = 'System.TTime' then
        begin
          try
            lWasDateTime := True;
            Result := ISOTimeToTime(AStringValue);
          except
            on E: Exception do
            begin
              raise EMVCException.CreateFmt(http_status.BadRequest,
                'Invalid TTime value for param [%s] - [CLASS: %s][MSG: %s]',
                [AFormalParam.name, E.Classname, E.Message]);
            end;
          end;
        end;
        if not lWasDateTime then
          try
            lFormatSettings.DecimalSeparator := '.';
            Result := StrToFloat(AStringValue, lFormatSettings);
          except
            on E: Exception do
            begin
              raise EMVCException.CreateFmt(http_status.BadRequest,
                'Invalid Float value for param [%s] - [CLASS: %s][MSG: %s]',
                [AFormalParam.name, E.Classname, E.Message]);
            end;
          end;
      end;
    tkEnumeration:
      begin
        if AFormalParam.ParamType.QualifiedName = 'System.Boolean' then
        begin
          if SameText(AStringValue, 'true') or SameText(AStringValue, '1') or SameText(AStringValue, 'yes') then
          begin
            Result := True;
          end
          else if SameText(AStringValue, 'false') or SameText(AStringValue, '0') or SameText(AStringValue, 'no') then
          begin
            Result := False;
          end
          else
          begin
            raise EMVCException.CreateFmt(http_status.BadRequest,
              'Invalid boolean value for parameter %s. Boolean parameters accepts only "true"/"false", "yes"/"no" or "1"/"0".',
              [AFormalParam.name]);
          end;
        end
        else
        begin
          raise EMVCException.CreateFmt(http_status.BadRequest,
            'Invalid type for parameter %s. Allowed types are ' +
            ALLOWED_TYPED_ACTION_PARAMETERS_TYPES, [AFormalParam.name]);
        end;
      end;
    tkRecord:
      begin
        if AFormalParam.ParamType.QualifiedName = 'System.TGUID' then
        begin
          try
            Result := TValue.From<TGUID>(TMVCGuidHelper.StringToGUIDEx(AStringValue));
          except
            raise EMVCException.CreateFmt('Invalid Guid value for param [%s]', [AFormalParam.name]);
          end;
        end
        else
          raise EMVCException.CreateFmt('Invalid type for parameter %s. Allowed types are ' +
            ALLOWED_TYPED_ACTION_PARAMETERS_TYPES, [AFormalParam.name]);
      end
  else
    begin
      raise EMVCException.CreateFmt(http_status.BadRequest,
        'Invalid type for parameter %s. Allowed types are ' + ALLOWED_TYPED_ACTION_PARAMETERS_TYPES,
        [AFormalParam.name]);
    end;
  end;
end;

function TMVCEngine.GetActualParamMulti(const AFormalParam: TRttiParameter;
  const AStringMultiValue: TArray<String>): TValue;
var
  lTValueArray: TArray<TValue>;
  I: Integer;
begin
  case AFormalParam.ParamType.TypeKind of
    tkDynArray:
      begin
        SetLength(lTValueArray, Length(AStringMultiValue));
        if SameText(AFormalParam.ParamType.QualifiedName, 'System.TArray<System.string>') then
        begin
          for I := 0 to High(lTValueArray) do
            lTValueArray[I] := AStringMultiValue[I];
        end else if SameText(AFormalParam.ParamType.QualifiedName, 'System.TArray<System.Int64>') then
        begin
          for I := 0 to High(lTValueArray) do
            lTValueArray[I] := StrToInt64(AStringMultiValue[I]);
        end else if AFormalParam.ParamType.QualifiedName.StartsWith('System.TArray<System.Int', True) then
        begin
          for I := 0 to High(lTValueArray) do
            lTValueArray[I] := StrToInt(AStringMultiValue[I]);
        end else if AFormalParam.ParamType.QualifiedName.StartsWith('System.TArray<System.boolean', True) then
        begin
          for I := 0 to High(lTValueArray) do
            lTValueArray[I] := StrToBool(AStringMultiValue[I]);
        end
        else
        begin
          raise EMVCException.CreateFmt('Invalid type for dynamic array parameter "%s". Allowed types are ' +
            'TArray<String>, TArray<Integer>, TArray<Int64>', [AFormalParam.name]);
        end;
        Result := TValue.FromArray(AFormalParam.ParamType.Handle, lTValueArray);
      end
  else
    begin
      raise EMVCException.CreateFmt(http_status.BadRequest,
        'Invalid type for parameter %s. Allowed types are ' + ALLOWED_TYPED_ACTION_PARAMETERS_TYPES,
        [AFormalParam.name]);
    end
  end;
end;

class function TMVCEngine.GetCurrentSession(const ASessionId: string; const ARaiseExceptionIfExpired: Boolean): TMVCWebSession;
var lSessionList: TObjectDictionary<string, TMVCWebSession>;
begin
  Result := nil;
  lSessionList := GlobalSessionList;
  TMonitor.Enter(lSessionList);
  try
    if not ASessionId.IsEmpty then
    begin
      if lSessionList.TryGetValue(ASessionId, Result) then
      begin
        { https://github.com/danieleteti/delphimvcframework/issues/355 }
        if Result.IsExpired then
        begin
          lSessionList.Remove(ASessionId);
          if ARaiseExceptionIfExpired then
          begin
            raise EMVCSessionExpiredException.Create('Session expired.')
          end
          else
          begin
            Result := nil;
          end;
        end
        else
        begin
          Result.MarkAsUsed;
        end;
      end;
    end;
  finally
    TMonitor.Exit(lSessionList);
  end;
end;

function TMVCEngine.GetSessionBySessionId(const ASessionId: string): TMVCWebSession;
begin
  Result := TMVCEngine.GetCurrentSession(ASessionId, False);
  if Assigned(Result) then
    Result.MarkAsUsed;
end;

function TMVCEngine.GetViewEngineClass: TMVCViewEngineClass;
begin
  if FViewEngineClass = nil then
    raise EMVCConfigException.Create
      ('No View Engine configured. [HINT: Use TMVCEngine.SetViewEngine() to set a valid view engine]');
  Result := FViewEngineClass;
end;

procedure TMVCEngine.HandleDefaultValueForInjectedParameter(var InjectedParamValue: String;
  const InjectableParamAttribute: MVCInjectableParamAttribute);
begin
  if InjectedParamValue.IsEmpty then
  begin
    if InjectableParamAttribute.CanBeUsedADefaultValue then
    begin
      InjectedParamValue := InjectableParamAttribute.DefaultValueAsString;
    end
    else
    begin
      raise EMVCException.CreateFmt
        ('Required parameter "%s" injected using "%s" has not provided and cannot be used a default value for it',
        [InjectableParamAttribute.ParamName, InjectableParamAttribute.Classname]);
    end;
  end;
end;

procedure TMVCEngine.SendHTTPStatus(const AContext: TWebContext; const HTTPStatusCode: Integer;
  const AReasonString: string; const AClassName: string);
var
  lSer: IMVCSerializer; lError: TMVCErrorResponse;
  lIgnored: TMVCIgnoredList;
  lContentType, lItem: String;
  lPreferredAcceptContentType: TArray<string>;
begin
  lPreferredAcceptContentType := [
    AContext.Request.BestAccept,
    FConfigCache_DefaultContentType,
    TMVCMediaType.TEXT_HTML,
    TMVCMediaType.TEXT_PLAIN];

  lError := TMVCErrorResponse.Create;
  try
    lError.Classname := AClassName;
    lError.StatusCode := HTTPStatusCode;
    lError.Message := IfThen(not AReasonString.IsEmpty, AReasonString, HTTP_STATUS.ReasonStringFor(HTTPStatusCode));

    lIgnored := ['ObjectDictionary'];
    if lError.fAppErrorCode = 0 then
      lIgnored := lIgnored + ['AppErrorCode'];
    if lError.Data = nil then
      lIgnored := lIgnored + ['Data'];
    if lError.Classname.IsEmpty then
      lIgnored := lIgnored + ['ClassName'];
    if lError.DetailedMessage.IsEmpty then
      lIgnored := lIgnored + ['DetailedMessage'];
    if lError.Items.Count = 0 then
    begin
      lIgnored := lIgnored + ['Items'];
    end;

    for lItem in lPreferredAcceptContentType do
    begin
      lSer := Serializer(lItem, False);
      if lSer <> nil then
      begin
        lContentType := lItem;
        Break;
      end;
    end;
    if lSer = nil then
    begin
      raise EMVCConfigException.Create('Cannot find a proper serializer among ' + string.Join(',', lPreferredAcceptContentType));
    end;

    AContext.Response.SetContentType(
      BuildContentType(lItem, AContext.Config[TMVCConfigKey.DefaultContentCharset]));
    AContext.Response.SetContent(lSer.SerializeObject(lError, stDefault, lIgnored));
    AContext.Response.SetStatusCode(HTTPStatusCode);
    AContext.Response.SetReasonString(lError.Message);
  finally
    lError.Free;
  end;
end;

procedure TMVCEngine.LoadSystemControllers;
begin
  if FConfig[TMVCConfigKey.LoadSystemControllers] = 'true' then
  begin
    LogI('Loading System Controllers');
    AddController(TMVCSystemController);
  end;
end;

procedure TMVCEngine.OnBeforeDispatch(ASender: TObject; ARequest: TWebRequest;
  AResponse: TWebResponse; var AHandled: Boolean);
begin
  AHandled := False;
  { there is a bug in WebBroker Linux on 10.2.1 tokyo }
  // if Assigned(FSavedOnBeforeDispatch) then
  // begin
  // FSavedOnBeforeDispatch(ASender, ARequest, AResponse, AHandled);
  // end;

  if IsShuttingDown then
  begin
    AResponse.StatusCode := http_status.ServiceUnavailable;
    AResponse.ContentType := TMVCMediaType.TEXT_PLAIN;
    AResponse.Content := 'Server is shutting down';
    AHandled := True;
  end;

  if not AHandled then
  begin
    try
      AHandled := ExecuteAction(ASender, ARequest, AResponse);
      if not AHandled then
      begin
        AResponse.ContentStream := nil;
      end;
    except
      on E: Exception do
      begin
        Log.Error('[%s] %s', [E.Classname, E.Message], LOGGERPRO_TAG);

        AResponse.StatusCode := http_status.InternalServerError; // default is Internal Server Error
        if E is EMVCException then
        begin
          AResponse.StatusCode := (E as EMVCException).HTTPStatusCode;
        end;

        AResponse.Content := E.Message;
        AResponse.SendResponse;
        AHandled := True;
      end;
    end;
  end;
end;

procedure TMVCEngine.OnWebContextCreate(
  const WebContextCreateEvent: TWebContextCreateEvent);
begin
  fWebContextCreateEvent := WebContextCreateEvent;
end;

procedure TMVCEngine.OnWebContextDestroy(
  const WebContextDestroyEvent: TWebContextDestroyEvent);
begin
  fWebContextDestroyEvent := WebContextDestroyEvent;
end;

function TMVCEngine.PublishObject(const AObjectCreatorDelegate: TMVCObjectCreatorDelegate;
      const AURLSegment: string; ExceptionHandler: TMVCJSONRPCExceptionHandlerProc = nil): TMVCEngine;
begin
  Result := AddController(TMVCJSONRPCPublisher,
    function: TMVCController
    begin
      Result := TMVCJSONRPCPublisher.Create(AObjectCreatorDelegate(),
        True, ExceptionHandler);
    end, AURLSegment);
end;

procedure TMVCEngine.RegisterDefaultsSerializers;
var lDefaultSerializerContentType: string;
begin
  // lDefaultSerializerContentType := BuildContentType(TMVCMediaType.APPLICATION_JSON, TMVCCharset.UTF_8);
  // if not FSerializers.ContainsKey(lDefaultSerializerContentType) then
  // begin
  // FSerializers.Add(lDefaultSerializerContentType, TMVCJSONDataObjectsSerializer.Create);
  // end;

  // register the same serializer without the charset in the contenttype
  lDefaultSerializerContentType := BuildContentType(TMVCMediaType.APPLICATION_JSON, '');
  if not FSerializers.ContainsKey(lDefaultSerializerContentType) then
  begin
    FSerializers.Add(lDefaultSerializerContentType, TMVCJSONDataObjectsSerializer.Create(Config));
  end;

  // this is used only for exceptions (dt 2020-09-16)
  lDefaultSerializerContentType := BuildContentType(TMVCMediaType.TEXT_HTML, '');
  if not FSerializers.ContainsKey(lDefaultSerializerContentType) then
  begin
    FSerializers.Add(lDefaultSerializerContentType, TMVCHTMLSerializer.Create(Config));
  end;

  // this is used only for TMVCError (dt 2024-02-22)
  lDefaultSerializerContentType := BuildContentType(TMVCMediaType.TEXT_PLAIN, '');
  if not FSerializers.ContainsKey(lDefaultSerializerContentType) then
  begin
    FSerializers.Add(lDefaultSerializerContentType, TMVCTextSerializer.Create(Config));
  end;
end;

procedure TMVCEngine.ResponseErrorPage(const AException: Exception; const ARequest: TWebRequest;
const AResponse: TWebResponse);
begin
  AResponse.SetCustomHeader('x-mvc-error', AException.Classname + ': ' + AException.Message);
  AResponse.StatusCode := http_status.OK;

  begin
    AResponse.ContentType := TMVCMediaType.TEXT_PLAIN;
    AResponse.Content := Config[TMVCConfigKey.ServerName] + ' ERROR:' + sLineBreak +
      'Exception raised of class: ' + AException.Classname + sLineBreak +
      '***********************************************' + sLineBreak + AException.Message +
      sLineBreak + '***********************************************';
  end;
end;

class function TMVCEngine.SendSessionCookie(const AContext: TWebContext): string;
var
  SId: string;
begin
  SId := StringReplace(StringReplace(StringReplace(
    'DT' + GUIDToString(TGUID.NewGuid) + GUIDToString(TGUID.NewGuid),
    '}', '', [rfReplaceAll]),
    '{', '', [rfReplaceAll]),
    '-', '', [rfReplaceAll]);
  Result := SendSessionCookie(AContext, SId);
end;

procedure TMVCEngine.SaveCacheConfigValues;
begin
  FConfigCache_MaxRequestSize := StrToInt64Def(Config[TMVCConfigKey.MaxRequestSize],
    TMVCConstants.DEFAULT_MAX_REQUEST_SIZE);
  FConfigCache_ExposeServerSignature := Config[TMVCConfigKey.ExposeServerSignature] = 'true';
  FConfigCache_ServerSignature := Config[TMVCConfigKey.ServerName];
  FConfigCache_ExposeXPoweredBy := Config[TMVCConfigKey.ExposeXPoweredBy] = 'true';
  FConfigCache_DefaultContentType := Config[TMVCConfigKey.DefaultContentType];
  FConfigCache_DefaultContentCharset := Config[TMVCConfigKey.DefaultContentCharset];
  FConfigCache_PathPrefix := Config[TMVCConfigKey.PathPrefix];
  FConfigCache_UseViewCache := Config[TMVCConfigKey.ViewCache] = 'true';
end;

class function TMVCEngine.SendSessionCookie(const AContext: TWebContext;
const ASessionId: string): string;
var Cookie: TCookie; SessionTimeout: Integer;
begin
  ClearSessionCookiesAlreadySet(AContext.Response.Cookies);
  Cookie := AContext.Response.Cookies.Add;
  Cookie.name := TMVCConstants.SESSION_TOKEN_NAME;
  Cookie.Value := ASessionId;
  if not TryStrToInt(AContext.Config[TMVCConfigKey.SessionTimeout], SessionTimeout) then
    raise EMVCException.Create('[Config::Session Timeout] is not a valid integer');

  if SessionTimeout = 0 then
    Cookie.Expires := 0 // session cookie
  else
    Cookie.Expires := Now + OneMinute * SessionTimeout;

  Cookie.Path := '/';
  Result := ASessionId;
end;

function TMVCEngine.Serializer(const AContentType: string; const ARaiseExceptionIfNotExists: Boolean): IMVCSerializer;
var
  lContentMediaType: string;
  lContentCharSet: string;
begin
  SplitContentMediaTypeAndCharset(AContentType.ToLower, lContentMediaType, lContentCharSet);
  if FSerializers.ContainsKey(lContentMediaType) then
  begin
    Result := FSerializers.Items[lContentMediaType];
  end
  else
  begin
    if ARaiseExceptionIfNotExists then
    begin
      raise EMVCException.CreateFmt('The serializer for %s could not be found. [HINT] Register on TMVCEngine instance using "AddSerializer" method.',
        [lContentMediaType]);
    end
    else
    begin
      Result := nil;
    end;
  end;
end;

function TMVCEngine.SetExceptionHandler(const AExceptionHandlerProc: TMVCExceptionHandlerProc)
  : TMVCEngine;
begin
  FOnException := AExceptionHandlerProc;
  Result := Self;
end;

function TMVCEngine.SetViewEngine(const AViewEngineClass: TMVCViewEngineClass): TMVCEngine;
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
    raise EMVCException.CreateFmt('ApplicationSession not assigned to this %s instance.',
      [Classname]);
  Result := FApplicationSession;
end;

function TMVCBase.GetConfig: TMVCConfig;
begin
  Result := Engine.Config;
end;

function TMVCBase.GetEngine: TMVCEngine;
begin
  if not Assigned(FEngine) then
    raise EMVCException.CreateFmt('MVCEngine not assigned to this %s instance.', [Classname]);
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

constructor TMVCControllerDelegate.Create(const AClazz: TMVCControllerClazz;
const ACreateAction: TMVCControllerCreateAction; const AURLSegment: string = '');
begin
  inherited Create;
  FClazz := AClazz;
  FCreateAction := ACreateAction;
  FURLSegment := AURLSegment;
end;

{ TMVCStaticContents }

class function TMVCStaticContents.IsScriptableFile(const AStaticFileName: string;
const AConfig: TMVCConfig): Boolean;
begin
  Result := TPath.GetExtension(AStaticFileName).ToLower = '.' +
    AConfig[TMVCConfigKey.DefaultViewFileExtension].ToLower;
end;

class function TMVCStaticContents.IsStaticFile(const AViewPath, AWebRequestPath: string;
out ARealFileName: string; out AIsDirectoryTraversalAttack: Boolean): Boolean;
var lFileName: string; lWebRoot: string;
begin
  AIsDirectoryTraversalAttack := False;
  if TDirectory.Exists(AViewPath) then
  begin
    lWebRoot := TPath.GetFullPath(AViewPath);
  end
  else
  begin
    lWebRoot := TPath.GetFullPath(GetApplicationFileNamePath + AViewPath);
  end;

  lFileName := TPath.Combine(lWebRoot, AWebRequestPath.Replace('/', TPath.DirectorySeparatorChar));
  if not TPath.HasValidPathChars(lFileName, True) then
  begin
    Exit(False);
  end;

  lFileName := TPath.GetFullPath(lFileName);
  if not lFileName.StartsWith(lWebRoot) then
  // AVOID PATH TRAVERSAL
  begin
    AIsDirectoryTraversalAttack := True;
    Exit(False);
  end;
  ARealFileName := lFileName;
  Result := TFile.Exists(ARealFileName);
end;

class procedure TMVCStaticContents.SendFile(
  const AFileName, AMediaType: string; AContext: TWebContext);
var
  FileDate: TDateTime;
  IfModifiedSinceDate: TDateTime;
  lIfModifiedSince: string;
begin
  if not FileExists(AFileName) then
  begin
    AContext.Response.StatusCode := 404;
  end
  else
  begin
    //https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-Modified-Since
    if AContext.Request.HTTPMethod in [httpGET, httpHEAD] then
    begin
      lIfModifiedSince := AContext.Request.Headers['If-Modified-Since'];
      FileDate := IndyFileAge(AFileName);
      if lIfModifiedSince <> '' then
      begin
        IfModifiedSinceDate := GMTToLocalDateTime(lIfModifiedSince);
        if (IfModifiedSinceDate <> 0) and (abs(IfModifiedSinceDate - FileDate) < 2 * (1 / (24 * 60 * 60))) then
        begin
          AContext.Response.ContentType := AMediaType;
          AContext.Response.StatusCode := 304;
          Exit;
        end
      end;
      AContext.Response.SetCustomHeader('Last-Modified', LocalDateTimeToHttpStr(FileDate));
      AContext.Response.SetContentStream(
        TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone), AMediaType);
    end
    else
    begin
      AContext.Response.SetContentStream(
        TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone), AMediaType);
    end;
  end;
end;

{ TMVCController }

procedure TMVCController.CheckIfMatch(const Data: String);
var
  lReqETag: String;
begin
  if Data.IsEmpty then
  begin
    raise EMVCException.Create(HTTP_STATUS.InternalServerError, 'Cannot calculate ETag using empty value');
  end;

  // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/ETag#avoiding_mid-air_collisions
  lReqETag := GetIfMatch;
  if lReqETag.IsEmpty then
  begin
    raise EMVCException.Create(HTTP_STATUS.PreconditionFailed, 'If-Match header is empty');
  end;

  if lReqETag <> GetSHA1HashFromString(Data) then
  begin
    raise EMVCException.Create(HTTP_STATUS.PreconditionFailed, 'mid-air collisions detected (invalid "if-match" value), cannot update or delete resource.');
  end;
end;

constructor TMVCController.Create;
begin
  inherited Create;
  fViewModel := nil;
  fPageHeaders := nil;
  fPageFooters := nil;
end;

destructor TMVCController.Destroy;
begin
  fViewModel.Free;
  inherited Destroy;
end;

function TMVCController.GetClientId: string;
begin
  Result := Session[CLIENTID_KEY];
  if Result.IsEmpty then
    raise EMVCException.Create('Invalid ClientID' + sLineBreak +
      'Hint: Messaging extensions require a valid clientid. Did you call /messages/clients/YOUR_CLIENT_ID ?');
end;

function TMVCRenderer.AcceptedResponse(const Location: string;
  const Body: TObject): IMVCResponse;
var
  lRespBuilder: IMVCResponseBuilder;
begin
  lRespBuilder := MVCResponseBuilder;
  if not Location.IsEmpty then
  begin
    lRespBuilder.Header('location', Location)
  end;
  if Assigned(Body) then
  begin
    lRespBuilder.Body(Body, True);
  end;
  Result := lRespBuilder.StatusCode(HTTP_STATUS.Accepted).Build;
end;

function TMVCRenderer.BadRequestResponse: IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.BadRequest, nil);
end;

function TMVCRenderer.BadRequestResponse(const Error: TObject): IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.BadRequest, Error);
end;

function TMVCRenderer.BadRequestResponse(const Message: String): IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.BadRequest, nil, Message);
end;

function TMVCRenderer.UnprocessableContentResponse: IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.UnprocessableEntity, nil);
end;

function TMVCRenderer.UnprocessableContentResponse(const Error: TObject): IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.UnprocessableEntity, Error);
end;

function TMVCRenderer.UnprocessableContentResponse(const Message: String): IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.UnprocessableEntity, nil, Message);
end;


function TMVCRenderer.ConflictResponse: IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.Conflict, nil);
end;

constructor TMVCRenderer.Create;
begin
  inherited;
  FContext := nil;
  FContentCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET;
  FResponseStream := nil;
end;

function TMVCRenderer.CreatedResponse(const Location: string ; const Message: String): IMVCResponse;
var
  lRespBuilder: IMVCResponseBuilder;
begin
  lRespBuilder := MVCResponseBuilder;
  if not Location.IsEmpty then
  begin
    lRespBuilder.Header('location', Location)
  end;
  lRespBuilder.Body(Message);
  Result := lRespBuilder.StatusCode(HTTP_STATUS.Created).Build;
end;

destructor TMVCRenderer.Destroy;
begin
  FResponseStream.Free;
  inherited;
end;

function TMVCRenderer.CreatedResponse(const Location: string;
  const Body: TObject): IMVCResponse;
var
  lRespBuilder: IMVCResponseBuilder;
begin
  lRespBuilder := MVCResponseBuilder;
  if not Location.IsEmpty then
  begin
    lRespBuilder.Header('location', Location)
  end;
  if Assigned(Body) then
  begin
    lRespBuilder.Body(Body, True);
  end;
  Result := lRespBuilder.StatusCode(HTTP_STATUS.Created).Build;
end;

function TMVCRenderer.GetContentType: string;
begin
  Result := GetContext.Response.ContentType;
  if Result.IsEmpty or FContentCharset.IsEmpty then
  begin
    Result := FContext.FConfig[MVCFramework.Commons.TMVCConfigKey.DefaultContentType];
    GetContext.Response.ContentType := Result;
    SplitContentMediaTypeAndCharset(Result, FContentMediaType, FContentCharset); //update FContentMediaType, FContentCharset

//    lRebuildContentType := False;
//    SplitContentMediaTypeAndCharset(Result, FContentMediaType, FContentCharset);
//    if FContentCharset.IsEmpty then
//    begin
//      lRebuildContentType := True;
//      FContentCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET;
//    end;
//    if FContentMediaType.IsEmpty then
//    begin
//      lRebuildContentType := True;
//      FContentMediaType := TMVCConstants.DEFAULT_CONTENT_TYPE;
//    end;
//    if lRebuildContentType then
//    begin
//      Result := BuildContentType(FContentMediaType, FContentCharset);
//      GetContext.Response.ContentType := Result;
//    end;
  end;
end;

function TMVCRenderer.GetContext: TWebContext;
begin
  if not Assigned(FContext) then
    raise EMVCException.CreateFmt('Context is not set %s.', [Classname]);
  Result := FContext;
end;

function TMVCController.GetCurrentWebModule: TWebModule;
begin
  Result := Engine.WebModule;
end;

function TMVCController.ETagFromString(const Data: String): String;
begin
  Result := GetSHA1HashFromString(Data);
end;

function TMVCController.GetIfMatch: String;
begin
  Result := Context.Request.GetHeader('If-Match');
end;

function TMVCController.GetIfNoneMatch: String;
begin
  Result := Context.Request.GetHeader('If-None-Match');
end;

function TMVCController.GetRenderedView(const AViewNames: TArray<string>; const JSONModel: TJSONObject; const OnBeforeRenderCallback: TMVCSSVBeforeRenderCallback): string;
var
  lView: TMVCBaseViewEngine; lViewName: string; lStrStream: TStringBuilder;
begin
  lStrStream := TStringBuilder.Create;
  try
    lView := FEngine.ViewEngineClass.Create(Engine, Context, Self, FViewModel, JSONModel, ContentType);
    try
      lView.FBeforeRenderCallback := OnBeforeRenderCallback;
      for lViewName in AViewNames do
      begin
        lView.Execute(lViewName, lStrStream);
      end;
    finally
      lView.Free;
    end;
    Result := lStrStream.ToString;
  finally
    lStrStream.Free;
  end;
end;

function TMVCController.GetSession: TMVCWebSession;
begin
  Result := GetContext.Session;
end;

function TMVCRenderer.GetStatusCode: Integer;
begin
  Result := GetContext.Response.StatusCode;
end;

class procedure TMVCRenderer.InternalRenderMVCResponse(
  const Controller: TMVCRenderer; const MVCResponse: TMVCResponse);
begin
begin
  if MVCResponse.HasHeaders then
  begin
    Controller.FContext.Response.CustomHeaders.AddStrings(MVCResponse.fHeaders);
  end;
  if MVCResponse.HasBody then
  begin
    Controller.Render(MVCResponse.StatusCode, MVCResponse, False, nil, MVCResponse.GetIgnoredList);
  end
  else
  begin
    {
      This useless call is required because otherwise
      the IdCustomHTTPServer.pas at line 2211 (11 alexandria)
      emits some useless HTML. Call Render('') cause
      the internal contentstream to be created so the empty
      stream is rendered and not the useless HTML.
    }
    Controller.Render(''); {required}
    Controller.ResponseStatus(MVCResponse.StatusCode);
  end;
end;

end;

function TMVCRenderer.RedirectResponse(Location: String; Permanent: Boolean = False; PreserveMethod: Boolean = False): IMVCResponse;
var
  lBuilder: IMVCResponseBuilder;
begin
  lBuilder := MVCResponseBuilder.Header('location', Location);
  if Permanent then
  begin
    if PreserveMethod then
    begin
      Result := lBuilder.StatusCode(HTTP_STATUS.TemporaryRedirect).Build;
    end
    else
    begin
      Result := lBuilder.StatusCode(HTTP_STATUS.MovedPermanently).Build;
    end;
  end
  else
  begin
    if PreserveMethod then
    begin
      Result := lBuilder.StatusCode(HTTP_STATUS.PermanentRedirect).Build;
    end
    else
    begin
      Result := lBuilder.StatusCode(HTTP_STATUS.Found).Build;
    end;
  end;
end;

function TMVCRenderer.InternalServerErrorResponse: IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.InternalServerError, nil);
end;


function TMVCRenderer.InternalServerErrorResponse(const Error: TObject): IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.InternalServerError, Error);
end;

function TMVCRenderer.InternalServerErrorResponse(const Message: String): IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.InternalServerError, nil, Message);
end;


function TMVCRenderer.NoContentResponse: IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.NoContent, nil);
end;

function TMVCRenderer.NotFoundResponse: IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.NotFound, nil);
end;

function TMVCRenderer.NotFoundResponse(const Body: TObject): IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.NotFound, Body);
end;

function TMVCRenderer.NotFoundResponse(const Message: String): IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.NotFound, nil, Message);
end;

function TMVCRenderer.NotModifiedResponse: IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.NotModified, nil);
end;

function TMVCRenderer.OKResponse: IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.OK, nil);
end;

function TMVCRenderer.OKResponse(const Message: String): IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.OK, nil, Message);
end;

function TMVCRenderer.OKResponse(const Body: TObject): IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.OK, Body);
end;

function TMVCRenderer.OKResponse(const Body: IMVCObjectDictionary): IMVCResponse;
begin
  Result := MVCResponseBuilder
    .StatusCode(HTTP_STATUS.OK)
    .Body(Body)
    .Build;
end;

function TMVCRenderer.StatusResponse(const StatusCode: Word): IMVCResponse;
begin
  Result := InternalStatusCodeResponse(StatusCode, nil);
end;

function TMVCRenderer.StatusResponse(const StatusCode: Word; const Message: String): IMVCResponse;
begin
  Result := InternalStatusCodeResponse(StatusCode, nil, Message);
end;

function TMVCRenderer.StatusResponse(const StatusCode: Word; const Body: TObject): IMVCResponse;
begin
  Result := InternalStatusCodeResponse(StatusCode, Body);
end;

function TMVCController.GetViewData(const aModelName: string): TValue;
begin
  if not FViewModel.TryGetValue(aModelName, Result) then
    Result := nil;
end;

function TMVCController.GetViewModel: TMVCViewDataObject;
begin
  if not Assigned(FViewModel) then
    FViewModel := TMVCViewDataObject.Create;
  Result := FViewModel;
end;

function TMVCController.LoadView(const AViewNames: TArray<string>; const JSONModel: TJSONObject = nil): string;
begin
  try
    Result := GetRenderedView(AViewNames, JSONModel);
    ResponseStream.Append(Result);
  except
    on E: Exception do
    begin
      Log.Error('[%s] %s', [E.Classname, E.Message], LOGGERPRO_TAG);
      raise;
    end;
  end;
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

procedure TMVCController.OnBeforeAction(AContext: TWebContext; const AActionName: string;
var AHandled: Boolean);
begin
  AHandled := False;
  if ContentType.IsEmpty then
    ContentType := Config[TMVCConfigKey.DefaultContentType];
  { Implement if need be. }
end;

function TMVCController.Page(const AViewNames: TArray<string>;
  const JSONModel: TJSONObject; const UseCommonHeadersAndFooters: Boolean; const OnBeforeRenderCallback: TMVCSSVBeforeRenderCallback): string;
begin
  if UseCommonHeadersAndFooters then
    Result := GetRenderedView(fPageHeaders + AViewNames + fPageFooters, JSONModel, OnBeforeRenderCallback)
  else
    Result := GetRenderedView(AViewNames, JSONModel, OnBeforeRenderCallback)
end;

function TMVCController.Page(const AViewName: string; const UseCommonHeadersAndFooters: Boolean; const OnBeforeRenderCallback: TMVCSSVBeforeRenderCallback): string;
begin
  Result := Page([AViewName], UseCommonHeadersAndFooters, OnBeforeRenderCallback);
end;

function TMVCController.PageFragment(const AViewNames: TArray<string>;
  const JSONModel: TJSONObject; const OnBeforeRenderCallback: TMVCSSVBeforeRenderCallback): string;
begin
  Result := Page(AViewNames, JSONModel, False, OnBeforeRenderCallback);
end;

function TMVCController.PageFragment(const AViewNames: TArray<string>; const OnBeforeRenderCallback: TMVCSSVBeforeRenderCallback): string;
begin
  Result := Page(AViewNames, nil, False);
end;

function TMVCController.Page(const AViewNames: TArray<string>; const UseCommonHeadersAndFooters: Boolean; const OnBeforeRenderCallback: TMVCSSVBeforeRenderCallback): string;
begin
  if UseCommonHeadersAndFooters then
    Result := GetRenderedView(fPageHeaders + AViewNames + fPageFooters, OnBeforeRenderCallback)
  else
    Result := GetRenderedView(AViewNames, OnBeforeRenderCallback);
end;

procedure TMVCController.PushObjectToView(const aModelName: string; const AModel: TObject);
begin
  GetViewModel.Add(aModelName, AModel);
end;

procedure TMVCController.RaiseSessionExpired;
begin
  raise EMVCSessionExpiredException.Create('Session expired.');
end;

procedure TMVCRenderer.Redirect(const AUrl: string);
begin
  GetContext.Response.RawWebResponse.SendRedirect(AUrl);
end;

procedure TMVCRenderer.Render(
  const AObject: TObject;
  const AOwns: Boolean;
  const ASerializationAction: TMVCSerializationAction = nil;
  const AIgnoredFields: TMVCIgnoredList = nil);
begin
  Render(AObject, AOwns, stDefault, ASerializationAction, AIgnoredFields);
end;

procedure TMVCRenderer.Render(const AStatusCode: Integer; const AContent: string);
begin
  SetStatusCode(AStatusCode);
  Render(AContent);
end;

procedure TMVCRenderer.Render(const AStatusCode: Integer);
begin
  RenderStatusMessage(AStatusCode, HTTP_STATUS.ReasonStringFor(AStatusCode));
end;

procedure TMVCRenderer.Render(const AContent: string);
var
//  lContentType: string;
  lOutEncoding: TEncoding;
//  lCharset: string;
begin
//  SplitContentMediaTypeAndCharset(GetContentType, lContentType, lCharset);
//  if lCharset.IsEmpty then
//    lCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET;
//  if lContentType.IsEmpty then
//    lContentType := TMVCConstants.DEFAULT_CONTENT_TYPE;
//  lContentType := BuildContentType(lContentType, lCharset);

  if SameText('UTF-8', FContentCharset) then
  begin
    GetContext
      .Response
      .SetContentStream(TStringStream.Create(AContent, gEncodingUTF8, False), GetContentType)
  end
  else
  begin
    lOutEncoding := TEncoding.GetEncoding(FContentCharset);
    try
      GetContext
        .Response
        .SetContentStream(
          TBytesStream.Create(
            TEncoding.Convert(
              TEncoding.Default,
              lOutEncoding,
              TEncoding.Default.GetBytes(AContent))), GetContentType);
    finally
      lOutEncoding.Free;
    end;
  end;
end;

procedure TMVCRenderer.Render<T>(const ACollection: TObjectList<T>; const AOwns: Boolean;
const ASerializationAction: TMVCSerializationAction<T>);
begin
  Self.Render<T>(ACollection, AOwns, stDefault, ASerializationAction);
end;

procedure TMVCRenderer.Render202Accepted(const HREF: string; const ID: string;
const Reason: string);
begin
  if HREF.IsEmpty then
  begin
    raise EMVCException.Create('Cannot send 202 without provide an HREF');
  end;
  ResponseStatus(http_status.Accepted, Reason);
  Render(TMVCAcceptedResponse.Create(HREF, ID));
end;

procedure TMVCRenderer.Render201Created(const Location, Reason: string);
begin
  if not Location.IsEmpty then
  begin
{$IF defined(BERLINORBETTER)}
    FContext.Response.CustomHeaders.AddPair('location', Location);
{$ELSE}
    FContext.Response.CustomHeaders.Add('location' + FContext.Response.CustomHeaders.NameValueSeparator + Location);
{$ENDIF}
  end;
  ResponseStatus(http_status.Created, Reason);
{$IF CompilerVersion >= 34}
  Render(''); // in 10.4 INDY requires something on the content
{$ENDIF}
end;

procedure TMVCRenderer.Render204NoContent(const Location, Reason: string);
begin
  if not Location.IsEmpty then
  begin
{$IF defined(BERLINORBETTER)}
    FContext.Response.CustomHeaders.AddPair('location', Location);
{$ELSE}
    FContext.Response.CustomHeaders.Add('location' + FContext.Response.CustomHeaders.NameValueSeparator + Location);
{$ENDIF}
  end;
  ResponseStatus(http_status.NoContent, Reason);
end;

procedure TMVCRenderer.ResponseStatus(const AStatusCode: Integer; const AReasonString: string);
begin
  SetStatusCode(AStatusCode);
  if AReasonString = '' then
    GetContext.Response.ReasonString := HTTP_STATUS.ReasonStringFor(AStatusCode)
  else
    GetContext.Response.ReasonString := AReasonString;
end;

function TMVCRenderer.ResponseStream: TStringBuilder;
begin
  if not Assigned(FResponseStream) then
    FResponseStream := TStringBuilder.Create;
  Result := FResponseStream;
end;

function TMVCRenderer.Serializer: IMVCSerializer;
begin
  Result := Serializer(GetContentType);
end;

procedure TMVCRenderer.SendFile(const AFileName: string);
begin
  TMVCStaticContents.SendFile(AFileName, GetContentType, GetContext);
end;

procedure TMVCRenderer.SendStream(const AStream: TStream; const AOwns: Boolean;
const ARewind: Boolean);
var lTemp: TStream;
begin
  if ARewind then
  begin
    AStream.Position := 0;
  end;

  lTemp := TMemoryStream.Create;
  try
    lTemp.CopyFrom(AStream, 0);
    lTemp.Position := 0;
  except
    lTemp.Free;
    raise;
  end;

  if AOwns then
  begin
    AStream.Free;
  end;

  GetContext.Response.RawWebResponse.Content := EmptyStr;
  GetContext.Response.RawWebResponse.ContentType := GetContentType;
  GetContext.Response.RawWebResponse.ContentStream := lTemp;
  GetContext.Response.RawWebResponse.FreeContentStream := True;
end;

function TMVCRenderer.Serializer(
  const AContentType: string;
  const ARaiseExceptionIfNotExists: Boolean): IMVCSerializer;
begin
  Result := Engine.Serializer(AContentType, ARaiseExceptionIfNotExists);
end;

function TMVCController.SessionAs<T>: T;
begin
  Result := Session as T;
end;

procedure TMVCRenderer.SetContentType(const AValue: string);
begin
  GetContext.Response.ContentType := AValue.Trim;
end;

procedure TMVCRenderer.SetContext(const Context: TWebContext);
begin
  FContext := Context;
end;

procedure TMVCRenderer.SetStatusCode(const AValue: Integer);
begin
  GetContext.Response.StatusCode := AValue;
end;

function TMVCRenderer.InternalStatusCodeResponse(const StatusCode: Word; const Body: TObject; const Message: String = ''): IMVCResponse;
begin
  if Body = nil then
  begin
    if Message.IsEmpty then
      Result := MVCResponseBuilder.StatusCode(StatusCode).Build
    else
      Result := MVCResponseBuilder.StatusCode(StatusCode).Body(Message).Build;
  end
  else
  begin
    if Message.IsEmpty then
      Result := MVCResponseBuilder.StatusCode(StatusCode).Body(Body, True).Build
    else
      Result := MVCResponseBuilder
        .StatusCode(StatusCode)
        .Body(Body)
        .Body(Message)
        .Build;
  end;
end;

function TMVCRenderer.ToMVCList(const AObject: TObject; AOwnsObject: Boolean): IMVCList;
begin
  Result := MVCFramework.DuckTyping.WrapAsList(AObject, AOwnsObject);
end;

function TMVCRenderer.UnauthorizedResponse: IMVCResponse;
begin
  Result := InternalStatusCodeResponse(HTTP_STATUS.Unauthorized, nil);
end;

procedure TMVCController.SetETag(const Data: String; const NeedsToBeHashed: Boolean);
begin
  if NeedsToBeHashed then
    Context.Response.SetCustomHeader('ETag', GetSHA1HashFromString(Data))
  else
    Context.Response.SetCustomHeader('ETag', Data);
end;

procedure TMVCController.SetPagesCommonFooters(const AViewNames: TArray<string>);
begin
  fPageFooters := AViewNames;
end;

procedure TMVCController.SetPagesCommonHeaders(const AViewNames: TArray<string>);
begin
  fPageHeaders := AViewNames;
end;

procedure TMVCController.SetViewData(const aModelName: string; const Value: TValue);
begin
  GetViewModel.AddOrSetValue(aModelName, Value);
end;

procedure TMVCRenderer.Render(
  const AObject: TObject;
  const AOwns: Boolean;
  const AType: TMVCSerializationType;
  const ASerializationAction: TMVCSerializationAction = nil;
  const AIgnoredFields: TMVCIgnoredList = nil);
begin
  try
    Render(Serializer(GetContentType).SerializeObject(AObject, AType, AIgnoredFields, ASerializationAction));
  finally
    if AOwns then
      AObject.Free;
  end;
end;

procedure TMVCRenderer.Render(const AStream: TStream; const AOwns: Boolean);
begin
  SendStream(AStream, AOwns);
end;

procedure TMVCRenderer.RenderStatusMessage(
  const AStatusCode: Integer;
  const AReasonMessage, AErrorClassName: string;
  const ADataObject: TObject;
  const AOwns: Boolean);
var
  lResponse: IMVCResponse;
begin
  lResponse := MVCResponseBuilder
      .StatusCode(AStatusCode)
      .Body(AReasonMessage)
      .Body(ADataObject, AOwns)
      .Build;
  TMVCRenderer.InternalRenderMVCResponse(Self, TMVCResponse(lResponse));
end;

procedure TMVCRenderer.RenderStream(const AStream: TStream; const AOwns,
  ARewind: Boolean);
begin
  SendStream(AStream, AOwns, ARewind);
end;

procedure TMVCRenderer.Render(
  const ADataSet: TDataSet;
  const AOwns: Boolean;
  const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase;
  const ASerializationType: TMVCDatasetSerializationType;
  const ASerializationAction: TMVCDatasetSerializationAction);
begin
  if Assigned(ADataSet) then
  begin
    try
      case ASerializationType of
        dstSingleRecord:
          begin
            Render(Serializer(GetContentType).SerializeDataSetRecord(ADataSet, AIgnoredFields,
              ANameCase, ASerializationAction))

          end;
        dstAllRecords:
          begin
            Render(Serializer(GetContentType).SerializeDataSet(ADataSet, AIgnoredFields, ANameCase,
              ASerializationAction))
          end
      else
        begin
          raise EMVCSerializationException.Create('Invalid dataset serialization type');
        end;
      end;
    finally
      if AOwns then
        ADataSet.Free;
    end;
  end
  else
    raise EMVCException.Create('Can not render an empty dataset.');
end;

procedure TMVCRenderer.Render(
  const AStatusCode: Integer;
  const AObject: IInterface;
  const ASerializationAction: TMVCSerializationAction);
begin
  SetStatusCode(AStatusCode);
  Render(AObject, ASerializationAction);
end;

procedure TMVCRenderer.Render(
  const AObject: IInterface;
  const ASerializationAction: TMVCSerializationAction;
  const AIgnoredFields: TMVCIgnoredList);
begin
  {TODO -oDanieleT -cGeneral : Handle StatusCode}
  Render(TObject(AObject), False, ASerializationAction, AIgnoredFields);
end;

procedure TMVCRenderer.Render(const AStatusCode: Integer; AObject: TObject; const AOwns: Boolean;
const ASerializationAction: TMVCSerializationAction; const AIgnoredFields: TMVCIgnoredList);
begin
  ResponseStatus(AStatusCode);
  Render(AObject, AOwns, ASerializationAction, AIgnoredFields);
end;

procedure TMVCRenderer.Render(
  const AStatusCode: Integer;
  const AObject: TObject;
  const ASerializationAction: TMVCSerializationAction;
  const AIgnoredFields: TMVCIgnoredList);
begin
  ResponseStatus(AStatusCode);
  Render(AObject, True, stDefault, ASerializationAction, AIgnoredFields);
end;

procedure TMVCRenderer.Render<T>(const ACollection: TObjectList<T>; const AOwns: Boolean;
const AType: TMVCSerializationType; const ASerializationAction: TMVCSerializationAction<T>);
var lSerializationAction: TMVCSerializationAction;
begin
  if Assigned(ACollection) then
  begin
    try
      if Assigned(ASerializationAction) then
      begin
        lSerializationAction := procedure(const AObject: TObject; const Dict: IMVCLinks)
          begin
            ASerializationAction(T(AObject), Dict);
          end;
      end
      else
      begin
        lSerializationAction := nil;
      end;
      Render(Serializer(GetContentType).SerializeCollection(ACollection, AType, [],
        lSerializationAction));
    finally
      if AOwns then
        ACollection.Free;
    end;
  end
  else
    raise EMVCException.Create('Can not render an empty collection.');
end;

procedure TMVCRenderer.RenderFile(const AFileName: string);
begin
  SendFile(AFileName);
end;

procedure TMVCRenderer.Render<T>(const AStatusCode: Integer; var ARecord: T);
begin
  SetStatusCode(AStatusCode);
  Render(
    Serializer(GetContentType)
      .SerializeRecord(@ARecord, TypeInfo(T), TMVCSerializationType.stFields,nil,nil));
end;

procedure TMVCRenderer.Render<T>(const AStatusCode: Integer; const ACollection: TObjectList<T>;
const AOwns: Boolean; const ASerializationAction: TMVCSerializationAction<T>);
begin
  SetStatusCode(AStatusCode);
  Render<T>(ACollection, AOwns, ASerializationAction);
end;

function TMVCController.GetRenderedView(const AViewNames: TArray<string>; const OnBeforeRenderCallback: TMVCSSVBeforeRenderCallback): string;
var
  lView: TMVCBaseViewEngine;
  lViewName: string;
  lStrStream: TStringBuilder;
begin
  lStrStream := TStringBuilder.Create;
  try
    lView := FEngine.ViewEngineClass.Create(
      Engine,
      Context,
      Self,
      FViewModel,
      ContentType);
    try
      lView.FBeforeRenderCallback := OnBeforeRenderCallback;
      for lViewName in AViewNames do
      begin
        lView.Execute(lViewName, lStrStream);
      end;
    finally
      lView.Free;
    end;
    Result := lStrStream.ToString;
  finally
    lStrStream.Free;
  end;
end;

procedure TMVCRenderer.Render<T>(const ACollection: TObjectList<T>;
const ASerializationAction: TMVCSerializationAction<T>);
begin
  Self.Render<T>(ACollection, True, ASerializationAction);
end;

procedure TMVCRenderer.RenderResponseStream;
begin
  Render(ResponseStream.ToString);
end;

procedure TMVCRenderer.RenderSSE(const EventID, EventData: string; EventName: string;
const Retry: Integer);
begin
  // setting up the correct SSE headers
  SetContentType(BuildContentType(TMVCMediaType.TEXT_EVENTSTREAM, TMVCCharSet.UTF_8));
  GetContext.Response.SetCustomHeader('Cache-Control', 'no-cache');
  GetContext.Response.StatusCode := HTTP_STATUS.OK;

  // render the response using SSE compliant data format

  // current event id (the client will resend this number at the next request)
  ResponseStream.Append(Format('id:%s'#13, [EventID]));

  // The browser attempts to reconnect to the source roughly 3 seconds after
  // each connection is closed. You can change that timeout by including a line
  // beginning with "retry:", followed by the number of milliseconds to wait
  // before trying to reconnect.

  if Retry > -1 then
  begin
    ResponseStream.Append(Format('retry:%d'#13, [Retry]));
  end;

  if not EventName.IsEmpty then
  begin
    ResponseStream.Append(Format('event:%s'#13, [EventName]));
  end;

  // actual message
  ResponseStream.Append('data:' + EventData.Replace(sLineBreak, '', [rfReplaceAll]) + #13#13);

  // render all the stuff
  RenderResponseStream;
end;

procedure TMVCRenderer.Render(const ACollection: IMVCList);
begin
  Render(ACollection, stDefault);
end;

procedure TMVCRenderer.Render(const ACollection: IMVCList; const AType: TMVCSerializationType);
begin
  if Assigned(ACollection) then
    Render(Serializer(GetContentType).SerializeCollection(TObject(ACollection), AType))
  else
    raise EMVCException.Create('Can not render an empty collection.');
end;

procedure TMVCRenderer.Render(const ATextWriter: TTextWriter; const AOwns: Boolean);
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

procedure TMVCRenderer.Render(const AException: Exception; AExceptionItems: TList<string>;
const AOwns: Boolean);
var S: string; R: TMVCErrorResponse; I: TMVCErrorResponseItem;
begin
  try
    if AException is EMVCException then
      ResponseStatus(EMVCException(AException).HTTPStatusCode)
    else
      ResponseStatus(http_status.InternalServerError);

    R := TMVCErrorResponse.Create;
    try
      R.StatusCode := GetContext.Response.StatusCode;
      R.Message := AException.Message;
      R.Classname := AException.Classname;
      if AException is EMVCException then
      begin
        R.AppErrorCode := EMVCException(AException).ApplicationErrorCode;
        R.DetailedMessage := EMVCException(AException).DetailedMessage;
        for S in EMVCException(AException).ErrorItems do
        begin
          R.Items.Add(TMVCErrorResponseItem.Create(S));
        end;
      end;
      if Assigned(AExceptionItems) and (AExceptionItems.Count > 0) then
      begin
        for S in AExceptionItems do
        begin
          I := TMVCErrorResponseItem.Create;
          I.Message := S;
          R.Items.Add(I);
        end;
      end;

      if Serializer(FContext.Request.BestAccept, False) <> nil then
        GetContext.Response.ContentType := FContext.Request.BestAccept
      else
        GetContext.Response.ContentType := Engine.FConfigCache_DefaultContentType;

      Render(R, False, nil, R.GetIgnoredList);
    finally
      R.Free;
    end;
  finally
    if AOwns then
      AExceptionItems.Free;
  end;
end;

procedure TMVCRenderer.Render(const AResponse: TMVCResponse; const AOwns: Boolean);
begin
  if Assigned(AResponse) then
  begin
    try
      GetContext.Response.StatusCode := AResponse.StatusCode;
      Render(AResponse, False, stProperties, nil, AResponse.GetIgnoredList);
    finally
      if AOwns then
        AResponse.Free;
    end;
  end
  else
    raise EMVCException.Create('Cannot render an empty response object.');
end;

procedure TMVCRenderer.Render(const ADataSet: TDataSet;
const ASerializationAction: TMVCDatasetSerializationAction);
begin
  Render(ADataSet, True, ASerializationAction);
end;

procedure TMVCRenderer.Render(
  const ADataSet: TDataSet;
  const AOwns: Boolean;
  const ASerializationAction: TMVCDatasetSerializationAction);
begin
  Render(ADataSet, AOwns, dstAllRecords, ASerializationAction);
end;

procedure TMVCRenderer.Render(
      const AObject: TObject;
      const ASerializationAction: TMVCSerializationAction = nil;
      const AIgnoredFields: TMVCIgnoredList = nil);
begin
  Render(AObject, True, stDefault, ASerializationAction, AIgnoredFields);
end;

procedure TMVCRenderer.Render(const ADataSet: TDataSet; const AOwns: Boolean;
const AIgnoredFields: TMVCIgnoredList; const ASerializationType: TMVCDatasetSerializationType;
const ASerializationAction: TMVCDatasetSerializationAction);
begin
  Render(ADataSet, AOwns, AIgnoredFields, TMVCNameCase.ncLowerCase, ASerializationType, ASerializationAction);
end;

procedure TMVCRenderer.Render(const ADataSet: TDataSet; const AOwns: Boolean;
const ASerializationType: TMVCDatasetSerializationType;
const ASerializationAction: TMVCDatasetSerializationAction);
begin
  Render(ADataSet, AOwns, [], ASerializationType, ASerializationAction);
end;

constructor TMVCResponse.Create;
begin
  inherited Create;
  fOwnsData := True;
  fData := nil;
  fMessage := '';
  fObjectDictionary := nil;
end;

constructor TMVCErrorResponse.Create;
begin
  inherited Create;
  FItems := TObjectList<TMVCErrorResponseItem>.Create(True);
end;

constructor TMVCResponse.Create(const StatusCode: Integer;
  const Message: String);
begin
  Create;
  fStatusCode := StatusCode;
  fMessage := Message;
end;

destructor TMVCResponse.Destroy;
begin
  if FOwnsData then
  begin
    fData.Free;
  end;
  inherited;
end;

function TMVCResponse.GetData: TObject;
begin
  Result := fData;
end;

function TMVCResponse.GetHeaders: TStringList;
begin
  Result := fHeaders;
end;

function TMVCResponse.GetIgnoredList: TMVCIgnoredList;
var
  lCount: Integer;
begin
  SetLength(Result, 3);
  lCount := 0;
  if fData = nil then
  begin
    Result[lCount] := 'Data';
    Inc(lCount);
  end;

  if fMessage.IsEmpty then
  begin
    Result[lCount] := 'Message';
    Inc(lCount);
  end;

  if fObjectDictionary = nil then
  begin
    Result[lCount] := 'ObjectDictionary';
    Inc(lCount);
  end;
  SetLength(Result, lCount);
end;

function TMVCResponse.GetMessage: string;
begin
  Result := fMessage;
end;

function TMVCResponse.GetObjectDictionary: IMVCObjectDictionary;
begin
  Result := fObjectDictionary;
end;

function TMVCResponse.GetReasonString: string;
begin
  if fReasonString.IsEmpty then
    Result := HTTP_STATUS.ReasonStringFor(fStatusCode)
  else
    Result := fReasonString;
end;

function TMVCResponse.GetStatusCode: Integer;
begin
  Result := fStatusCode;
end;

function TMVCResponse.HasBody: Boolean;
begin
  Result := (not fMessage.IsEmpty) or (fData <> nil) or (fObjectDictionary <> nil);
end;

function TMVCResponse.HasHeaders: Boolean;
begin
  Result := fHeaders <> nil;
end;

procedure TMVCResponse.SetData(const Value: TObject);
begin
  fData := Value;
end;

procedure TMVCResponse.SetHeaders(const Value: TStringList);
begin
  FreeAndNil(fHeaders);
  fHeaders := Value;
end;

procedure TMVCResponse.SetMessage(const Value: string);
begin
  fMessage := Value;
end;

procedure TMVCResponse.SetObjectDictionary(const Value: IMVCObjectDictionary);
begin
  fObjectDictionary := Value;
end;

procedure TMVCResponse.SetOwnsData(const Value: Boolean);
begin
  fOwnsData := Value;
end;

procedure TMVCResponse.SetReasonString(const Value: string);
begin
  fReasonString := Value;
end;

procedure TMVCResponse.SetStatusCode(const Value: Integer);
begin
  fStatusCode := Value;
end;

constructor TMVCErrorResponse.Create(const AStatusCode: Integer;
  const AMessage: String);
begin
  Create;
  fStatusCode := AStatusCode;
  fMessage := AMessage;
end;

destructor TMVCErrorResponse.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TMVCErrorResponse.GetIgnoredList: TMVCIgnoredList;
var
  lCount: Integer;
begin
  SetLength(Result, 3);
  lCount := 0;
  if Items.Count = 0 then
  begin
    Result[lCount] := 'Items';
    Inc(lCount);
  end;

  if AppErrorCode = 0 then
  begin
    Result[lCount] := 'AppErrorCode';
    Inc(lCount);
  end;

  if DetailedMessage.IsEmpty then
  begin
    Result[lCount] := 'DetailedMessage';
    Inc(lCount);
  end;
  SetLength(Result, lCount);
  Result := (inherited GetIgnoredList) + Result;
end;

procedure TMVCErrorResponse.SetAppErrorCode(const Value: Integer);
begin
  FAppErrorCode := Value;
end;

{ TMVCBaseView }

constructor TMVCBaseViewEngine.Create(
      const AEngine: TMVCEngine;
      const AWebContext: TWebContext;
      const AController: TMVCController;
      const AViewModel: TMVCViewDataObject;
      const AContentType: string);
begin
  inherited Create;
  Engine := AEngine;
  FWebContext := AWebContext;
  FController := AController;
  FViewModel := AViewModel;
  FContentType := AContentType;
  FOutput := EmptyStr;
  FUseViewCache := Engine.fConfigCache_UseViewCache;
end;

constructor TMVCBaseViewEngine.Create(
  const AEngine: TMVCEngine;
  const AWebContext: TWebContext;
  const AController: TMVCController;
  const AViewModel: TMVCViewDataObject;
  const AJSONModel: TJSONObject;
  const AContentType: string);
begin
  Create(AEngine, AWebContext, AController, AViewModel, AContentType);
  fJSONModel := AJSONModel;
end;

destructor TMVCBaseViewEngine.Destroy;
begin
  inherited Destroy;
end;

function TMVCBaseViewEngine.GetRealFileName(const AViewName: string): string;
var
  lFileName: string;
  lDefaultViewFileExtension: string;
  lTemplateFolder: string;
begin
  lDefaultViewFileExtension := Config[TMVCConfigKey.DefaultViewFileExtension];
  lFileName := StringReplace(AViewName, '/', PathDelim, [rfReplaceAll]);

  if (lFileName = PathDelim) then
  begin
    lFileName := PathDelim + 'index.' + lDefaultViewFileExtension
  end
  else
  begin
    lFileName := lFileName + '.' + lDefaultViewFileExtension;
  end;

  lTemplateFolder := Config[TMVCConfigKey.ViewPath];
  if DirectoryExists(lTemplateFolder) then
  begin
    lFileName := ExpandFileName(IncludeTrailingPathDelimiter(lTemplateFolder) + lFileName)
  end
  else
  begin
    lFileName := ExpandFileName(IncludeTrailingPathDelimiter(
      GetApplicationFileNamePath + lTemplateFolder) + lFileName);
  end;

  if FileExists(lFileName) then
    Result := lFileName
  else
    Result := EmptyStr;
end;

{ MVCIntegerAttribute }

constructor MVCIntegerAttribute.Create(const AValue: Int64);
begin
  FValue := AValue;
end;

{ MVCPathParamAttribute }

constructor MVCPathParamAttribute.Create(AType: TSwagTypeParameter; APattern, AFormat: string);
begin
  inherited Create;
  FType := AType;
  FValue := APattern;
  FFormat := AFormat;
end;

{ MVCParamAttribute }

constructor MVCParamAttribute.Create(
  Name: string;
  Location: TSwagRequestParameterInLocation;
  AType: TSwagTypeParameter; APattern, AFormat: string);
begin
  FName := name;
  FLocation := Location;
  FType := AType;
  FPattern := APattern;
  FFormat := AFormat;
end;

constructor MVCParamAttribute.Create(
  Name: string;
  Location: TSwagRequestParameterInLocation;
  AType: TClass; APattern, AFormat: string);
begin
  FName := name;
  FLocation := Location;
  FClassType := AType;
  FPattern := APattern;
  FFormat := AFormat;
end;

function MVCResponseBuilder: IMVCResponseBuilder;
begin
  Result := TMVCResponseBuilder.Create;
end;

function TMVCResponseBuilder.Body(const MessageText: String): IMVCResponseBuilder;
begin
  Result := Self.Message(MessageText);
end;

function TMVCResponseBuilder.Build: IMVCResponse;
begin
  Result := TMVCResponse.Create;
  Result.Data := fData;
  TMVCResponse(Result).OwnsData := fOwnsData;
  Result.Message := fMessage;
  Result.ObjectDictionary := fObjectDict;
  Result.StatusCode := fStatusCode;
  Result.Headers := fHeaders;
  fBuilt := True;
end;

constructor TMVCResponseBuilder.Create;
begin
  inherited;
  fOwnsData := True;
  fBuilt := False;
  fStatusCode := HTTP_STATUS.OK;
  fHeaders := nil;
end;

function TMVCResponseBuilder.Body(const Data: TObject; const Owns: Boolean): IMVCResponseBuilder;
begin
  if fData <> nil then
  begin
    raise EMVCResponseBuilderException.Create('Body already contains a "Data" node - To add two or more "Data" nodes use "ObjectDict"');
  end;
  fData := Data;
  fOwnsData := Owns;
  Result := Self;
end;

destructor TMVCResponseBuilder.Destroy;
begin
  if not fBuilt then
  begin
    FreeAndNil(fData);
    FreeAndNil(fHeaders);
  end;
  inherited;
end;

function TMVCResponseBuilder.HasHeaders: Boolean;
begin
  Result := fHeaders <> nil;
end;

function TMVCResponseBuilder.Header(const Name: String; const Value: String): IMVCResponseBuilder;
begin
  if not HasHeaders then
  begin
    fHeaders := TStringList.Create;
  end;
  fHeaders.Values[Name] := Value;
  Result := Self;
end;

function TMVCResponseBuilder.Message(
  const Message: String): IMVCResponseBuilder;
begin
  if not fMessage.IsEmpty then
  begin
    raise EMVCResponseBuilderException.Create('Body already contains a "Message" node');
  end;
  fMessage := Message;
  Result := Self;
end;

function TMVCResponseBuilder.Body(
  const ObjDictionary: IMVCObjectDictionary): IMVCResponseBuilder;
begin
  if fObjectDict <> nil then
  begin
    raise EMVCResponseBuilderException.Create('Body already contains an ObjectDict');
  end;
  fObjectDict := ObjDictionary;
  Result := Self;
end;

function TMVCResponseBuilder.StatusCode(
  const StatusCode: Integer): IMVCResponseBuilder;
begin
  fStatusCode := StatusCode;
  Result := Self;
end;

{ TMVCBaseResponse }

constructor TMVCBaseResponse.Create;
begin
  inherited;
end;

{ MVCInjectAttribute }

{ MVCInjectAttribute }

constructor MVCInjectAttribute.Create(ServiceName: String);
begin
  inherited Create;
  fServiceName := ServiceName;
end;

initialization

// https://quality.embarcadero.com/browse/RSP-38281
TRttiContext.KeepContext;

gIsShuttingDown := False;

gMVCGlobalActionParamsCache := TMVCStringObjectDictionary<TMVCActionParamCacheItem>.Create;
gEncodingUTF8 := TEncoding.GetEncoding(TMVCCharSet.UTF_8);

finalization

FreeAndNil(gEncodingUTF8);
FreeAndNil(gMVCGlobalActionParamsCache);


// https://quality.embarcadero.com/browse/RSP-38281
TRttiContext.DropContext;

end.
