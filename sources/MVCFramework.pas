﻿// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
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
  Data.DB,
  MVCFramework.Session,
  MVCFramework.DuckTyping,
  MVCFramework.Logger,
  MVCFramework.ApplicationSession,
  MVCFramework.Serializer.Intf,

{$IFDEF WEBAPACHEHTTP}
  Web.ApacheHTTP,
  // Apache Support since XE6 http://docwiki.embarcadero.com/Libraries/XE6/de/Web.ApacheHTTP

{$ENDIF}

  // Delphi XE4 (all update) and XE5 (with no update) don't contains this unit. Look for the bug in QC
  // https://quality.embarcadero.com/browse/RSP-17216

{$IFNDEF MOBILE} // file upload is not supported on mobile
{$IF Defined(SeattleOrBetter)}
  Web.ReqMulti,
{$ELSE}
  ReqMulti,
{$ENDIF}
{$ENDIF}
  Web.HTTPApp,

{$IFDEF MSWINDOWS}
  Web.Win.IsapiHTTP,
{$ENDIF}
  Web.WebReq,
  LoggerPro,
  IdGlobal,
  IdGlobalProtocols,
  IdURI,
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
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
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
    constructor Create(inStatusCode: Integer; const inDescription: string; inResponseClass: TClass = nil); overload;
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
    constructor Create(inStatusCode: Integer; const inDescription: string; inResponseClass: TClass = nil); overload;
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

    constructor Create(name: string; Location: TSwagRequestParameterInLocation; AType: TSwagTypeParameter;
      APattern: string = ''; AFormat: string = ''); overload;
    constructor Create(name: string; Location: TSwagRequestParameterInLocation; AType: TClass; APattern: string = '';
      AFormat: string = ''); overload;
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
    function GetOverwrittenHTTPMethod: TMVCHTTPMethodType;

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

    procedure SaveToSession(const AWebSession: TWebSession);
    function LoadFromSession(const AWebSession: TWebSession): Boolean;

    property UserName: string read FUserName write FUserName;
    property Roles: TList<string> read FRoles;
    property LoggedSince: TDateTime read FLoggedSince write SetLoggedSince;
    property Realm: string read FRealm write FRealm;
    property CustomData: TMVCCustomData read FCustomData write SetCustomData;
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
    function AddSessionToTheSessionList(const ASessionType, ASessionId: string; const ASessionTimeout: Integer)
      : TWebSession;
  public
    constructor Create(const ARequest: TWebRequest; const AResponse: TWebResponse; const AConfig: TMVCConfig;
      const ASerializers: TDictionary<string, IMVCSerializer>);
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

  TMVCResponse = class;
  TMVCErrorResponse = class;

  IMVCRenderer = interface
    ['{2FF6DAC8-2F19-4C78-B9EC-A86296847D39}']
    procedure Render(const AContent: string); overload;
    procedure Render(const AObject: TObject); overload;
    procedure Render(const AObject: TObject; const AOwns: Boolean); overload;
    procedure Render(const AObject: TObject; const AOwns: Boolean; const AType: TMVCSerializationType); overload;
    procedure Render(const AStatusCode: Integer; AObject: TObject; const AOwns: Boolean;
      const ASerializationAction: TMVCSerializationAction = nil); overload;
    procedure Render(const ACollection: IMVCList); overload;
    procedure Render(const ACollection: IMVCList; const AType: TMVCSerializationType); overload;
    procedure Render(
      const ADataSet: TDataSet;
      const ASerializationAction: TMVCDatasetSerializationAction = nil
      ); overload;
    procedure Render(
      const ADataSet: TDataSet;
      const AOwns: Boolean;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    procedure Render(
      const ADataSet: TDataSet;
      const AOwns: Boolean;
      const ASerializationType: TMVCDatasetSerializationType;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    procedure Render(
      const ADataSet: TDataSet;
      const AOwns: Boolean;
      const AIgnoredFields: TMVCIgnoredList;
      const ASerializationType: TMVCDatasetSerializationType;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    procedure Render(
      const ADataSet: TDataSet;
      const AOwns: Boolean;
      const AIgnoredFields: TMVCIgnoredList;
      const ANameCase: TMVCNameCase;
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

    procedure SendStream(const AStream: TStream; const AOwns: Boolean = True; const ARewind: Boolean = False);
    procedure SendFile(const AFileName: string);
    procedure RenderResponseStream;
    function ResponseStream: TStringBuilder;
    procedure Redirect(const AUrl: string);
    procedure ResponseStatus(const AStatusCode: Integer; const AReasonString: string = '');
    procedure Render201Created(const Location: string = '');
    // Serializer access
    function Serializer: IMVCSerializer; overload;
    function Serializer(const AContentType: string; const ARaiseExcpIfNotExists: Boolean = True)
      : IMVCSerializer; overload;
  end;

  IMVCAuthenticationHandler = interface
    ['{19B580EA-8A47-4364-A302-EEF3C6207A9F}']
    procedure OnRequest(const AContext: TWebContext; const AControllerQualifiedClassName, AActionName: string;
      var AAuthenticationRequired: Boolean);
    procedure OnAuthentication(const AContext: TWebContext; const AUserName, APassword: string;
      AUserRoles: TList<string>; var AIsValid: Boolean; const ASessionData: TDictionary<string, string>);
    procedure OnAuthorization(const AContext: TWebContext; AUserRoles: TList<string>;
      const AControllerQualifiedClassName: string; const AActionName: string; var AIsAuthorized: Boolean);
  end;

  TMVCRenderer = class(TMVCBase)
  protected
    FContext: TWebContext;
    FContentCharset: string;
    FResponseStream: TStringBuilder;
    function ToMVCList(const AObject: TObject; AOwnsObject: Boolean = False): IMVCList;
  public
    function GetContentType: string;
    function GetStatusCode: Integer;
    procedure SetContentType(const AValue: string);
    procedure SetStatusCode(const AValue: Integer);
    function GetContext: TWebContext;
    procedure Redirect(const AUrl: string); virtual;
    procedure ResponseStatus(const AStatusCode: Integer; const AReasonString: string = ''); virtual;
    /// <summary>
    /// HTTP Status 201 indicates that as a result of HTTP POST request, one or more new resources have been successfully created on server.
    /// The response may contain URI in Location header field in HTTP headers list, which can have reference to the newly created resource. Also, response payload also may include an entity containing a list of resource characteristics and location(s) from which the user or user agent can choose the one most appropriate.
    /// WARNING: The origin server MUST create the resource before returning the 201 status code. If the action cannot be carried out immediately, the server SHOULD respond with 202 (Accepted) response instead.
    /// </summary>
    /// <remarks>
    /// https://restfulapi.net/http-status-201-created/
    /// </remarks>
    procedure Render201Created(const Location: string = ''; const Reason: string = 'Created'); virtual;
    /// <summary>
    /// Allow a server to accept a request for some other process (perhaps a batch-oriented process that is only run once per day) without requiring that the user agents connection to the server persist until the process is completed.
    /// The entity returned with this response SHOULD describe the requests current status and point to (or embed) a status monitor that can provide the user with (or without) an estimate of when the request will be fulfilled.
    /// </summary>
    /// <remarks>
    /// https://restfulapi.net/http-status-202-accepted/
    /// </remarks>
    procedure Render202Accepted(const HREF: string; const ID: string; const Reason: string = 'Accepted'); virtual;
    /// <summary>
    /// HTTP Status 204 (No Content) indicates that the server has successfully fulfilled the request and that there is no content to send in the response payload body. The server might want to return updated meta information in the form of entity-headers, which if present SHOULD be applied to current documents active view if any.
    /// The 204 response MUST NOT include a message-body and thus is always terminated by the first empty line after the header fields.
    /// </summary>
    procedure Render204NoContent(const Location: string = ''; const Reason: string = 'No Content'); virtual;
    function Serializer: IMVCSerializer; overload;
    function Serializer(const AContentType: string; const ARaiseExceptionIfNotExists: Boolean = True)
      : IMVCSerializer; overload;
    procedure SendStream(const AStream: TStream; const AOwns: Boolean = True; const ARewind: Boolean = False); virtual;
    procedure SendFile(const AFileName: string); virtual;
    procedure RenderResponseStream; virtual;
    function ResponseStream: TStringBuilder;
    procedure Render(const AContent: string); overload;
    // PODO renders
    procedure Render(const AStatusCode: Integer; const AObject: TObject;
      const ASerializationAction: TMVCSerializationAction = nil); overload;
    procedure Render(const AObject: TObject; const ASerializationAction: TMVCSerializationAction = nil); overload;
    procedure Render(const AObject: TObject; const AOwns: Boolean;
      const ASerializationAction: TMVCSerializationAction = nil); overload;
    procedure Render(const AObject: TObject; const AOwns: Boolean; const AType: TMVCSerializationType;
      const ASerializationAction: TMVCSerializationAction = nil); overload;
    procedure Render(const AStatusCode: Integer; AObject: TObject; const AOwns: Boolean;
      const ASerializationAction: TMVCSerializationAction = nil); overload;
    procedure Render(const AObject: IInterface; const ASerializationAction: TMVCSerializationAction = nil); overload;
    // PODOs Collection render
    procedure Render<T: class>(const ACollection: TObjectList<T>;
      const ASerializationAction: TMVCSerializationAction<T> = nil); overload;
    procedure Render<T: class>(const ACollection: TObjectList<T>; const AOwns: Boolean;
      const ASerializationAction: TMVCSerializationAction<T> = nil); overload;
    procedure Render<T: class>(const AStatusCode: Integer; const ACollection: TObjectList<T>; const AOwns: Boolean;
      const ASerializationAction: TMVCSerializationAction<T> = nil); overload;
    procedure Render<T: class>(const ACollection: TObjectList<T>; const AOwns: Boolean;
      const AType: TMVCSerializationType; const ASerializationAction: TMVCSerializationAction<T> = nil); overload;
    procedure Render(const ACollection: IMVCList); overload;
    procedure Render(const ACollection: IMVCList; const AType: TMVCSerializationType); overload;
    procedure Render(const ATextWriter: TTextWriter; const AOwns: Boolean = True); overload;
    procedure Render(const AStream: TStream; const AOwns: Boolean = True); overload;
    procedure Render(const AErrorCode: Integer; const AErrorMessage: string = ''; const AErrorClassName: string = '';
      const ADataObject: TObject = nil); overload;
    procedure Render(const AException: Exception; AExceptionItems: TList<string> = nil;
      const AOwns: Boolean = True); overload;
    procedure Render(const AResponse: TMVCResponse; const AOwns: Boolean = True); overload;
    // Dataset support
    procedure Render(
      const ADataSet: TDataSet;
      const ASerializationAction: TMVCDatasetSerializationAction = nil
      ); overload;
    procedure Render(
      const ADataSet: TDataSet;
      const AOwns: Boolean;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    procedure Render(
      const ADataSet: TDataSet;
      const AOwns: Boolean;
      const ASerializationType: TMVCDatasetSerializationType;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    procedure Render(
      const ADataSet: TDataSet;
      const AOwns: Boolean;
      const AIgnoredFields: TMVCIgnoredList;
      const ASerializationType: TMVCDatasetSerializationType;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    procedure Render(
      const ADataSet: TDataSet;
      const AOwns: Boolean;
      const AIgnoredFields: TMVCIgnoredList;
      const ANameCase: TMVCNameCase;
      const ASerializationType: TMVCDatasetSerializationType;
      const ASerializationAction: TMVCDatasetSerializationAction = nil); overload;
    // SSE Support
    procedure RenderSSE(const EventID: string; const EventData: string; EventName: string = '';
      const Retry: Integer = TMVCConstants.SSE_RETRY_DEFAULT);
  end;

  TMVCController = class(TMVCRenderer)
  private
    FViewModel: TMVCViewDataObject;
    FViewDataSets: TMVCViewDataSet;
    function GetSession: TWebSession;
    function GetViewData(const aModelName: string): TObject;
    function GetViewDataset(const aDataSetName: string): TDataSet;
    procedure SetViewData(const aModelName: string; const Value: TObject);
    procedure SetViewDataset(const aDataSetName: string; const Value: TDataSet);
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
    function GetViewDataSets: TMVCViewDataSet;
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

    function SessionAs<T: TWebSession>: T;
    procedure RaiseSessionExpired; virtual;

    // Properties
    property Context: TWebContext read GetContext write FContext;
    property Session: TWebSession read GetSession;
    property ContentType: string read GetContentType write SetContentType;
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    property ViewModelList: TMVCViewDataObject read GetViewModel;
    property ViewDataSetList: TMVCViewDataSet read GetViewDataSets;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // procedure PushToView(const AModelName: string; const AModel: string);
    procedure PushObjectToView(const aModelName: string; const AModel: TObject); deprecated 'Use "ViewData"';
    procedure PushDataSetToView(const aModelName: string; const ADataSet: TDataSet); deprecated 'Use "ViewDataSet"';

    property ViewData[const aModelName: string]: TObject read GetViewData write SetViewData;
    property ViewDataset[const aDataSetName: string]: TDataSet read GetViewDataset write SetViewDataset;
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
    constructor Create(const AClazz: TMVCControllerClazz; const ACreateAction: TMVCControllerCreateAction;
      const AURLSegment: string = '');
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
    procedure OnBeforeRouting(AContext: TWebContext; var AHandled: Boolean);
    /// <summary>
    /// Procedure is called before the specific controller method is called.
    /// </summary>
    /// <param name="AContext">Webcontext which contains the complete request and response of the actual call.</param>
    /// <param name="AControllerQualifiedClassName">Qualified classname of the matching controller.</param>
    /// <param name="AActionName">Method name of the matching controller method.</param>
    /// <param name="AHandled">If set to True the Request would finished. Response must be set by the implementor. Default value is False.</param>
    procedure OnBeforeControllerAction(AContext: TWebContext; const AControllerQualifiedClassName: string;
      const AActionName: string; var AHandled: Boolean);
    /// <summary>
    /// Procedure is called after the specific controller method was called.
    /// It is still possible to cancel or to completly modifiy the request.
    /// </summary>
    /// <param name="AContext">Webcontext which contains the complete request and response of the actual call.</param>
    /// <param name="AActionName">Method name of the matching controller method.</param>
    /// <param name="AHandled">If set to True the Request would finished. Response must be set by the implementor. Default value is False.</param>
    procedure OnAfterControllerAction(AContext: TWebContext; const AActionName: string; const AHandled: Boolean);
  end;

  TMVCExceptionHandlerProc = reference to procedure(E: Exception; SelectedController: TMVCController;
    WebContext: TWebContext; var ExceptionHandled: Boolean);
  TMVCRouterLogState = (rlsRouteFound, rlsRouteNotFound);
  TMVCRouterLogHandlerProc = reference to procedure(
    const Router: TMVCCustomRouter;
    const RouterLogState: TMVCRouterLogState;
    const WebContext: TWebContext);

  TMVCEngine = class(TComponent)
  private const
    ALLOWED_TYPED_ACTION_PARAMETERS_TYPES =
      'Integer, Int64, Single, Double, Extended, Boolean, TDate, TTime, TDateTime, String and TGUID';
  private
    FViewEngineClass: TMVCViewEngineClass;
    FWebModule: TWebModule;
    FConfig: TMVCConfig;
    FConfigCache_MaxRequestSize: Int64;
    FSerializers: TDictionary<string, IMVCSerializer>;
    FMiddlewares: TList<IMVCMiddleware>;
    FControllers: TObjectList<TMVCControllerDelegate>;
    FMediaTypes: TDictionary<string, string>;
    FApplicationSession: TWebApplicationSession;
    FSavedOnBeforeDispatch: THTTPMethodEvent;
    FOnException: TMVCExceptionHandlerProc;
    fOnRouterLog: TMVCRouterLogHandlerProc;
    function IsStaticFileRequest(const ARequest: TWebRequest; out AFileName: string): Boolean;
    function SendStaticFileIfPresent(const AContext: TWebContext; const AFileName: string): Boolean;
    procedure FillActualParamsForAction(const AContext: TWebContext; const AActionFormalParams: TArray<TRttiParameter>;
      const AActionName: string; var AActualParams: TArray<TValue>);
    procedure RegisterDefaultsSerializers;
    function GetViewEngineClass: TMVCViewEngineClass;
  protected
    function CustomExceptionHandling(const Ex: Exception; const ASelectedController: TMVCController;
      const AContext: TWebContext): Boolean;
    procedure ConfigDefaultValues; virtual;
    procedure SaveCacheConfigValues;
    procedure LoadSystemControllers; virtual;
    procedure FixUpWebModule;
    procedure ExecuteBeforeRoutingMiddleware(const AContext: TWebContext; var AHandled: Boolean);
    procedure ExecuteBeforeControllerActionMiddleware(const AContext: TWebContext;
      const AControllerQualifiedClassName: string; const AActionName: string; var AHandled: Boolean);
    procedure ExecuteAfterControllerActionMiddleware(const AContext: TWebContext; const AActionName: string;
      const AHandled: Boolean);
    procedure DefineDefaultResponseHeaders(const AContext: TWebContext);
    procedure OnBeforeDispatch(ASender: TObject; ARequest: TWebRequest; AResponse: TWebResponse;
      var AHandled: Boolean); virtual;
    procedure ResponseErrorPage(const AException: Exception; const ARequest: TWebRequest;
      const AResponse: TWebResponse); virtual;
    function ExecuteAction(const ASender: TObject; const ARequest: TWebRequest; const AResponse: TWebResponse)
      : Boolean; virtual;
  public
    class function GetCurrentSession(const ASessionTimeout: Integer; const ASessionId: string;
      const ARaiseExceptionIfExpired: Boolean = True): TWebSession; static;
    class function ExtractSessionIdFromWebRequest(const AWebRequest: TWebRequest): string; static;
    class function SendSessionCookie(const AContext: TWebContext): string; overload; static;
    class function SendSessionCookie(const AContext: TWebContext; const ASessionId: string): string; overload; static;
    class procedure ClearSessionCookiesAlreadySet(const ACookies: TCookieCollection); static;
  public
    constructor Create(const AWebModule: TWebModule; const AConfigAction: TProc<TMVCConfig> = nil;
      const ACustomLogger: ILogWriter = nil); reintroduce;
    destructor Destroy; override;

    function GetSessionBySessionId(const ASessionId: string): TWebSession;

    function AddSerializer(const AContentType: string; const ASerializer: IMVCSerializer): TMVCEngine;
    function AddMiddleware(const AMiddleware: IMVCMiddleware): TMVCEngine;
    function AddController(const AControllerClazz: TMVCControllerClazz; const AURLSegment: string = '')
      : TMVCEngine; overload;
    function AddController(const AControllerClazz: TMVCControllerClazz; const ACreateAction: TMVCControllerCreateAction;
      const AURLSegment: string = ''): TMVCEngine; overload;
    function PublishObject(const AObjectCreatorDelegate: TMVCObjectCreatorDelegate; const AURLSegment: string)
      : TMVCEngine;
    function SetViewEngine(const AViewEngineClass: TMVCViewEngineClass): TMVCEngine;
    function SetExceptionHandler(const AExceptionHandlerProc: TMVCExceptionHandlerProc): TMVCEngine;

    function GetServerSignature(const AContext: TWebContext): string;
    procedure HTTP404(const AContext: TWebContext);
    procedure HTTP500(const AContext: TWebContext; const AReasonString: string = '');
    procedure SendRawHTTPStatus(const AContext: TWebContext; const HTTPStatusCode: Integer;
      const AReasonString: string; const AClassName: string = '');

    property ViewEngineClass: TMVCViewEngineClass read GetViewEngineClass;
    property WebModule: TWebModule read FWebModule;
    property Config: TMVCConfig read FConfig;
    property Serializers: TDictionary<string, IMVCSerializer> read FSerializers;
    property Middlewares: TList<IMVCMiddleware> read FMiddlewares;
    property Controllers: TObjectList<TMVCControllerDelegate> read FControllers;
    property ApplicationSession: TWebApplicationSession read FApplicationSession write FApplicationSession;
    property OnRouterLog: TMVCRouterLogHandlerProc read fOnRouterLog write fOnRouterLog;
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
  TMVCResponse = class
  private
    FStatusCode: Integer;
    FReasonString: string;
    FMessage: string;
    fDataObject: TObject;
  protected
    { protected declarations }
  public
    constructor Create; overload; virtual;
    constructor Create(AStatusCode: Integer; AReasonString: string; AMessage: string); overload;
    property StatusCode: Integer read FStatusCode write FStatusCode;
    property ReasonString: string read FReasonString write FReasonString;
    property message: string read FMessage write FMessage;
    property Data: TObject read fDataObject write fDataObject;
  end;

  [MVCNameCase(ncLowerCase)]
  TMVCErrorResponse = class(TMVCResponse)
  private
    FClassname: string;
    FItems: TObjectList<TMVCErrorResponseItem>;
  public
    constructor Create; override;
    destructor Destroy; override;
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
  public
    constructor Create(const AEngine: TMVCEngine; const AWebContext: TWebContext; const AViewModel: TMVCViewDataObject;
      const AViewDataSets: TObjectDictionary<string, TDataSet>; const AContentType: string); virtual;
    destructor Destroy; override;

    procedure Execute(const ViewName: string; const OutputStream: TStream); virtual; abstract;

    property ViewName: string read FViewName;
    property WebContext: TWebContext read FWebContext;
    property ViewModel: TMVCViewDataObject read FViewModel;
    property ViewDataSets: TObjectDictionary<string, TDataSet> read FViewDataSets;
    property ContentType: string read FContentType;
    property Output: string read FOutput;
  end;

function IsShuttingDown: Boolean;
procedure EnterInShutdownState;
function CreateResponse(const StatusCode: UInt16; const ReasonString: string; const Message: string = ''): TMVCResponse;

implementation

uses
  MVCFramework.SysControllers,
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.JSONRPC, MVCFramework.Router;

var
  _IsShuttingDown: Int64 = 0;
  _MVCGlobalActionParamsCache: TMVCStringObjectDictionary<TMVCActionParamCacheItem> = nil;

function IsShuttingDown: Boolean;
begin
  Result := TInterlocked.Read(_IsShuttingDown) = 1
end;

procedure EnterInShutdownState;
begin
  TInterlocked.Add(_IsShuttingDown, 1);
end;

function CreateResponse(const StatusCode: UInt16; const ReasonString: string; const Message: string = ''): TMVCResponse;
begin
  Result := TMVCResponse.Create(StatusCode, ReasonString, message);
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

constructor MVCResponseAttribute.Create(inStatusCode: Integer; const inDescription: string; inResponseClass: TClass);
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

function TMVCWebRequest.Body: string;
var
  lEncoding: TEncoding;
  lCurrCharset: string;

{$IFNDEF BERLINORBETTER}
  lBuffer: TArray<Byte>;

{$ENDIF}
begin
  { TODO -oEzequiel -cRefactoring : Refactoring the method TMVCWebRequest.Body }
  if (FBody = EmptyStr) then
  begin
    lCurrCharset := FCharset;
    if (lCurrCharset = EmptyStr) then
      lCurrCharset := 'UTF-8';
    lEncoding := TEncoding.GetEncoding(lCurrCharset);
    try

{$IFDEF BERLINORBETTER}
      FWebRequest.ReadTotalContent; // Otherwise ISAPI Raises "Empty BODY"
      FBody := lEncoding.GetString(FWebRequest.RawContent);
{$ELSE}
      SetLength(lBuffer, FWebRequest.ContentLength);
      FWebRequest.ReadClient(lBuffer[0], FWebRequest.ContentLength);
      FBody := lEncoding.GetString(lBuffer);
{$ENDIF}
    finally
      lEncoding.Free;
    end;
  end;
  Result := FBody;
end;

function TMVCWebRequest.BodyAs<T>: T;
var
  Obj: TObject;
  lSerializer: IMVCSerializer;
begin
  Result := nil;
  if FSerializers.TryGetValue(ContentMediaType, lSerializer) then
  begin
    Obj := TMVCSerializerHelper.CreateObject(TClass(T).QualifiedClassName);
    try
      lSerializer.DeserializeObject(Body, Obj);
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
    raise EMVCDeserializationException.CreateFmt('Body ContentType "%s" not supported', [ContentType]);
end;

function TMVCWebRequest.BodyAsListOf<T>: TObjectList<T>;
var
  List: TObjectList<T>;
  lSerializer: IMVCSerializer;
begin
  Result := nil;
  if FSerializers.TryGetValue(ContentMediaType, lSerializer) then
  begin
    List := TObjectList<T>.Create(True);
    try
      lSerializer.DeserializeCollection(Body, List, T);
      Result := List;
    except
      FreeAndNil(List);
      raise;
    end;
  end
  else
    raise EMVCException.CreateFmt('Body ContentType "%s" not supported', [ContentType]);
end;

procedure TMVCWebRequest.BodyFor<T>(const AObject: T);
var
  lSerializer: IMVCSerializer;
begin
  if Assigned(AObject) then
    if FSerializers.TryGetValue(ContentMediaType, lSerializer) then
      lSerializer.DeserializeObject(Body, AObject)
    else
    begin
      if ContentType.Trim.IsEmpty then
      begin
        raise EMVCException.Create('Request ContentType header is empty, cannot deserialize body');
      end
      else
      begin
        raise EMVCException.CreateFmt('Body ContentType "%s" not supported', [ContentType]);
      end;
    end;
end;

procedure TMVCWebRequest.BodyForListOf<T>(const AObjectList: TObjectList<T>);
var
  lSerializer: IMVCSerializer;
begin
  if Assigned(AObjectList) then
    if FSerializers.TryGetValue(ContentMediaType, lSerializer) then
      lSerializer.DeserializeCollection(Body, AObjectList, T)
    else
      raise EMVCException.CreateFmt('Body ContentType "%s" not supported', [ContentType]);
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

constructor TMVCWebRequest.Create(const AWebRequest: TWebRequest;
  const ASerializers: TDictionary<string, IMVCSerializer>);
begin
  inherited Create;
  FBody := EmptyStr;
  // FContentType := TMVCConstants.DEFAULT_CONTENT_TYPE;
  FCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET;
  FWebRequest := AWebRequest;
  FSerializers := ASerializers;
  FParamsTable := nil;
  DefineContentType;
end;

procedure TMVCWebRequest.DefineContentType;
begin
  SplitContentMediaTypeAndCharset(FWebRequest.GetFieldByName('Content-Type'), FContentMediaType, FCharset);
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

function TMVCWebRequest.GetParamsMulti(const AParamName: string): TArray<string>;
var
  lList: TList<string>;
  procedure AddParamsToList(const AStrings: TStrings; const AList: TList<string>);
  var
    I: Integer;
  begin
    for I := 0 to AStrings.Count - 1 do
      if SameText(AStrings.Names[I], AParamName) then
        AList.Add(AStrings.ValueFromIndex[I]);
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

function TMVCWebRequest.GetQueryParams: TDictionary<string, string>;
var
  I: Integer;
begin
  if not Assigned(FQueryParams) then
  begin
    FQueryParams := TDictionary<string, string>.Create;
    for I := 0 to Pred(FWebRequest.QueryFields.Count) do
    begin
      FQueryParams.Add(LowerCase(FWebRequest.QueryFields.Names[I]), FWebRequest.QueryFields.ValueFromIndex[I]);
    end;
  end;
  Result := FQueryParams;
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
  begin
    try
      Flush;
    except
      // do nothing
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
    for I := 3 to Length(Pieces) - 1 do // https://github.com/danieleteti/delphimvcframework/issues/225
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
  AWebSession[TMVCConstants.CURRENT_USER_SESSION_KEY] := FUserName + '$$' + DateTimeToISOTimeStamp(FLoggedSince) + '$$'
    + FRealm + '$$' + LRoles;
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

function TWebContext.AddSessionToTheSessionList(const ASessionType, ASessionId: string; const ASessionTimeout: Integer)
  : TWebSession;
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

constructor TWebContext.Create(const ARequest: TWebRequest; const AResponse: TWebResponse; const AConfig: TMVCConfig;
  const ASerializers: TDictionary<string, IMVCSerializer>);
begin
  inherited Create;
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
{$IFDEF WEBAPACHEHTTP}
    if ARequest.ClassType = TApacheRequest then
    begin
      FRequest := TMVCApacheWebRequest.Create(ARequest, ASerializers)
    end
    else
{$IFNDEF LINUX}
      if ARequest.ClassType = TISAPIRequest then
      begin
        FRequest := TMVCISAPIWebRequest.Create(ARequest, ASerializers)
      end
      else
{$ENDIF}
{$ENDIF}
      begin
        FRequest := TMVCIndyWebRequest.Create(ARequest, ASerializers);
      end;
  end;

  FResponse := TMVCWebResponse.Create(AResponse);
  FConfig := AConfig;
  FSerializers := ASerializers;
  FData := TDictionary<string, string>.Create;
  FLoggedUser := nil;
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
  try
    if Assigned(FLoggedUser) then
      FLoggedUser.Free;
  except
  end;
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
    FWebSession := TMVCEngine.GetCurrentSession(StrToInt64(FConfig[TMVCConfigKey.SessionTimeout]),
      TMVCEngine.ExtractSessionIdFromWebRequest(FRequest.RawWebRequest), False);
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
  ID: string;
begin
  if not Assigned(FWebSession) then
  begin
    ID := TMVCEngine.SendSessionCookie(Self);
    FWebSession := AddSessionToTheSessionList(Config[TMVCConfigKey.SessionType], ID,
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

function TMVCEngine.AddController(const AControllerClazz: TMVCControllerClazz; const AURLSegment: string): TMVCEngine;
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
  Log.Info('ENTER: Config default values', LOGGERPRO_TAG);

  Config[TMVCConfigKey.SessionTimeout] := '30' { 30 minutes };
  Config[TMVCConfigKey.DocumentRoot] := '.\www';
  Config[TMVCConfigKey.FallbackResource] := '';
  Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
  Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
  Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
  Config[TMVCConfigKey.ViewPath] := 'templates';
  Config[TMVCConfigKey.PathPrefix] := '';
  Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
  Config[TMVCConfigKey.ServerName] := 'DelphiMVCFramework';
  Config[TMVCConfigKey.ExposeServerSignature] := 'true';
  Config[TMVCConfigKey.SessionType] := 'memory';
  Config[TMVCConfigKey.IndexDocument] := 'index.html';
  Config[TMVCConfigKey.MaxEntitiesRecordCount] := '20';
  Config[TMVCConfigKey.MaxRequestSize] := IntToStr(TMVCConstants.DEFAULT_MAX_REQUEST_SIZE);
  Config[TMVCConfigKey.HATEOSPropertyName] := '_links';

  FMediaTypes.Add('.html', TMVCMediaType.TEXT_HTML);
  FMediaTypes.Add('.htm', TMVCMediaType.TEXT_HTML);
  FMediaTypes.Add('.txt', TMVCMediaType.TEXT_PLAIN);
  FMediaTypes.Add('.css', TMVCMediaType.TEXT_CSS);
  FMediaTypes.Add('.js', TMVCMediaType.TEXT_JAVASCRIPT);
  FMediaTypes.Add('.jpg', TMVCMediaType.IMAGE_JPEG);
  FMediaTypes.Add('.jpeg', TMVCMediaType.IMAGE_JPEG);
  FMediaTypes.Add('.png', TMVCMediaType.IMAGE_PNG);
  FMediaTypes.Add('.ico', TMVCMediaType.IMAGE_X_ICON);
  FMediaTypes.Add('.appcache', TMVCMediaType.TEXT_CACHEMANIFEST);

  Log.Info('EXIT: Config default values', LOGGERPRO_TAG);

  fOnRouterLog :=
      procedure(const Sender: TMVCCustomRouter; const RouterLogState: TMVCRouterLogState; const Context: TWebContext)
    begin
      case RouterLogState of
        rlsRouteFound:
          begin
            Log(TLogLevel.levNormal, Context.Request.HTTPMethodAsString + ':' + Context.Request.PathInfo + ' [' +
              Context.Request.ClientIp + '] -> ' +
              Sender.GetQualifiedActionName + ' - ' + IntToStr(Context.Response.StatusCode) + ' ' +
              Context.Response.ReasonString);
          end;
        rlsRouteNotFound:
          begin
            Log(TLogLevel.levNormal, Context.Request.HTTPMethodAsString + ':' + Context.Request.PathInfo + ' [' +
              Context.Request.ClientIp + '] -> {NOT FOUND} - ' + IntToStr(Context.Response.StatusCode) + ' ' +
              Context.Response.ReasonString);
          end;
      else
        raise EMVCException.Create('Invalid RouterLogState');
      end;
    end;
end;

constructor TMVCEngine.Create(const AWebModule: TWebModule; const AConfigAction: TProc<TMVCConfig>;
  const ACustomLogger: ILogWriter);
begin
  inherited Create(AWebModule);
  FWebModule := AWebModule;
  FixUpWebModule;
  FConfig := TMVCConfig.Create;
  FSerializers := TDictionary<string, IMVCSerializer>.Create;
  FMiddlewares := TList<IMVCMiddleware>.Create;
  FControllers := TObjectList<TMVCControllerDelegate>.Create(True);
  FMediaTypes := TDictionary<string, string>.Create;
  FApplicationSession := nil;
  FSavedOnBeforeDispatch := nil;

  WebRequestHandler.CacheConnections := True;
  WebRequestHandler.MaxConnections := 4096;

  MVCFramework.Logger.SetDefaultLogger(ACustomLogger);
  ConfigDefaultValues;

  if Assigned(AConfigAction) then
  begin
    LogEnterMethod('Custom configuration method');
    AConfigAction(FConfig);
    LogExitMethod('Custom configuration method');
  end;
  SaveCacheConfigValues;
  RegisterDefaultsSerializers;
  LoadSystemControllers;
end;

function TMVCEngine.CustomExceptionHandling(const Ex: Exception;
  const ASelectedController: TMVCController;
  const AContext: TWebContext): Boolean;
begin
  Result := False;
  if Assigned(FOnException) then
  begin
    Log.ErrorFmt('[%s] %s',
      [Ex.Classname, Ex.Message], LOGGERPRO_TAG);
    FOnException(Ex, ASelectedController, AContext, Result);
  end;
end;

procedure TMVCEngine.DefineDefaultResponseHeaders(const AContext: TWebContext);
begin
  if Config[TMVCConfigKey.ExposeServerSignature] = 'true' then
    AContext.Response.CustomHeaders.Values['Server'] := GetServerSignature(AContext);
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

function TMVCEngine.ExecuteAction(const ASender: TObject; const ARequest: TWebRequest;
  const AResponse: TWebResponse): Boolean;
var
  LParamsTable: TMVCRequestParamsTable;
  LContext: TWebContext;
  LFileName: string;
  LRouter: TMVCRouter;
  LHandled: Boolean;
  LResponseContentMediaType: string;
  LResponseContentCharset: string;
  LSelectedController: TMVCController;
  LActionFormalParams: TArray<TRttiParameter>;
  LActualParams: TArray<TValue>;
begin
  Result := False;

  if ARequest.ContentLength > FConfigCache_MaxRequestSize then
  begin
    raise EMVCException.CreateFmt('Request size exceeded the max allowed size [%d KiB] (1)',
      [(FConfigCache_MaxRequestSize div 1024)]);
  end;

{$IFDEF BERLINORBETTER}
  ARequest.ReadTotalContent;

  // Double check for malicious content-length header
  if ARequest.ContentLength > FConfigCache_MaxRequestSize then
  begin
    raise EMVCException.CreateFmt('Request size exceeded the max allowed size [%d KiB] (2)',
      [(FConfigCache_MaxRequestSize div 1024)]);
  end;
{$ENDIF}
  LParamsTable := TMVCRequestParamsTable.Create;
  try
    LContext := TWebContext.Create(ARequest, AResponse, FConfig, FSerializers);
    try
      DefineDefaultResponseHeaders(LContext);
      LHandled := False;
      LRouter := TMVCRouter.Create(FConfig, _MVCGlobalActionParamsCache);
      try // finally
        LSelectedController := nil;
        try // only for lselectedcontroller
          try // global exception handler
            ExecuteBeforeRoutingMiddleware(LContext, LHandled);
            if not LHandled then
            begin
              if LRouter.ExecuteRouting(ARequest.PathInfo,
                LContext.Request.GetOverwrittenHTTPMethod { LContext.Request.HTTPMethod } ,
                ARequest.ContentType, ARequest.Accept, FControllers, FConfig[TMVCConfigKey.DefaultContentType],
                FConfig[TMVCConfigKey.DefaultContentCharset], LParamsTable, LResponseContentMediaType,
                LResponseContentCharset) then
              begin
                try
                  if Assigned(LRouter.ControllerCreateAction) then
                    LSelectedController := LRouter.ControllerCreateAction()
                  else
                    LSelectedController := LRouter.ControllerClazz.Create;
                except
                  on Ex: Exception do
                  begin
                    Log.ErrorFmt('[%s] %s (Custom message: "%s")',
                      [Ex.Classname, Ex.Message, 'Cannot create controller'], LOGGERPRO_TAG);
                    raise EMVCException.Create(HTTP_STATUS.InternalServerError, 'Cannot create controller');
                    // HTTP500(LContext, 'Cannot create controller');
                    // Result := False;
                    // Exit;
                  end;
                end;
                LSelectedController.Engine := Self;
                LSelectedController.Context := LContext;
                LSelectedController.ApplicationSession := FApplicationSession;
                LContext.ParamsTable := LParamsTable;
                ExecuteBeforeControllerActionMiddleware(LContext, LRouter.ControllerClazz.QualifiedClassName,
                  LRouter.MethodToCall.name, LHandled);
                if LHandled then
                  Exit(True);

                LSelectedController.MVCControllerAfterCreate;
                try
                  LHandled := False;
                  LSelectedController.ContentType := BuildContentType(LResponseContentMediaType,
                    LResponseContentCharset);
                  // LSelectedController.ContentCharset := LResponseContentCharset;
                  LActionFormalParams := LRouter.MethodToCall.GetParameters;
                  if (Length(LActionFormalParams) = 0) then
                    SetLength(LActualParams, 0)
                  else if (Length(LActionFormalParams) = 1) and
                    (SameText(LActionFormalParams[0].ParamType.QualifiedName, 'MVCFramework.TWebContext')) then
                  begin
                    SetLength(LActualParams, 1);
                    LActualParams[0] := LContext;
                  end
                  else
                  begin
                    FillActualParamsForAction(LContext, LActionFormalParams, LRouter.MethodToCall.name,
                      LActualParams);
                  end;

                  LSelectedController.OnBeforeAction(LContext, LRouter.MethodToCall.name, LHandled);

                  if not LHandled then
                  begin
                    try
                      LRouter.MethodToCall.Invoke(LSelectedController, LActualParams);
                    finally
                      LSelectedController.OnAfterAction(LContext, LRouter.MethodToCall.name);
                    end;
                  end;
                finally
                  LSelectedController.MVCControllerBeforeDestroy;
                end;
                ExecuteAfterControllerActionMiddleware(LContext, LRouter.MethodToCall.name, LHandled);
                LContext.Response.ContentType := LSelectedController.ContentType;
                fOnRouterLog(LRouter, rlsRouteFound, LContext);
              end
              else // execute-routing
              begin
                if Config[TMVCConfigKey.AllowUnhandledAction] = 'false' then
                begin
                  if not Config[TMVCConfigKey.FallbackResource].IsEmpty then
                  begin
                    if (LContext.Request.PathInfo = '/') or (LContext.Request.PathInfo = '') then // useful for SPA
                    begin
                      LFileName := TPath.GetFullPath(TPath.Combine(Config[TMVCConfigKey.DocumentRoot],
                        Config[TMVCConfigKey.FallbackResource]));
                      Result := SendStaticFileIfPresent(LContext, LFileName);
                    end;
                  end;
                  if (not Result) and (IsStaticFileRequest(ARequest, LFileName)) then
                  begin
                    Result := SendStaticFileIfPresent(LContext, LFileName);
                  end;
                  if not Result then
                  begin
                    LContext.Response.StatusCode := HTTP_STATUS.NotFound;
                    LContext.Response.ReasonString := 'Not Found';
                    fOnRouterLog(LRouter, rlsRouteNotFound, LContext);
                    raise EMVCException.Create(
                      LContext.Response.ReasonString,
                      LContext.Request.HTTPMethodAsString + ' ' + LContext.Request.PathInfo,
                      0,
                      HTTP_STATUS.NotFound
                      );
                  end;
                end
                else
                  LContext.Response.FlushOnDestroy := False;
              end; // end-execute-routing
            end; // if not handled by beforerouting
          except
            on ESess: EMVCSessionExpiredException do
            begin
              if not CustomExceptionHandling(ESess, LSelectedController, LContext) then
              begin
                Log.ErrorFmt('[%s] %s (Custom message: "%s")', [ESess.Classname, ESess.Message, ESess.DetailedMessage],
                  LOGGERPRO_TAG);
                LContext.SessionStop(False);
                LSelectedController.ResponseStatus(ESess.HTTPErrorCode);
                LSelectedController.Render(ESess);
              end;
            end;
            on E: EMVCException do
            begin
              if not CustomExceptionHandling(E, LSelectedController, LContext) then
              begin
                Log.ErrorFmt('[%s] %s (Custom message: "%s")', [E.Classname, E.Message, E.DetailedMessage],
                  LOGGERPRO_TAG);
                if Assigned(LSelectedController) then
                begin
                  LSelectedController.ResponseStatus(E.HTTPErrorCode);
                  LSelectedController.Render(E);
                end
                else
                begin
                  SendRawHTTPStatus(LContext, E.HTTPErrorCode, Format('[%s] %s', [E.Classname, E.Message]),
                    E.Classname);
                end;
              end;
            end;
            on EIO: EInvalidOp do
            begin
              if not CustomExceptionHandling(EIO, LSelectedController, LContext) then
              begin
                Log.ErrorFmt('[%s] %s (Custom message: "%s")', [EIO.Classname, EIO.Message, 'Invalid Op'],
                  LOGGERPRO_TAG);
                if Assigned(LSelectedController) then
                begin
                  LSelectedController.ResponseStatus(HTTP_STATUS.InternalServerError);
                  LSelectedController.Render(EIO);
                end
                else
                begin
                  SendRawHTTPStatus(LContext, HTTP_STATUS.InternalServerError,
                    Format('[%s] %s', [EIO.Classname, EIO.Message]), EIO.Classname);
                end;
              end;
            end;
            on Ex: Exception do
            begin
              if not CustomExceptionHandling(Ex, LSelectedController, LContext) then
              begin
                Log.ErrorFmt('[%s] %s (Custom message: "%s")',
                  [Ex.Classname, Ex.Message, 'Global Action Exception Handler'], LOGGERPRO_TAG);
                if Assigned(LSelectedController) then
                begin
                  LSelectedController.ResponseStatus(HTTP_STATUS.InternalServerError);
                  LSelectedController.Render(Ex);
                end
                else
                begin
                  SendRawHTTPStatus(LContext, HTTP_STATUS.InternalServerError,
                    Format('[%s] %s', [Ex.Classname, Ex.Message]), Ex.Classname);
                end;
              end;
            end;
          end;
        finally
          FreeAndNil(LSelectedController);
        end;
      finally
        LRouter.Free;
      end;
    finally
      LContext.Free;
    end;
  finally
    LParamsTable.Free;
  end;
end;

procedure TMVCEngine.ExecuteAfterControllerActionMiddleware(const AContext: TWebContext; const AActionName: string;
  const AHandled: Boolean);
var
  I: Integer;
begin
  // for I := FMiddlewares.Count - 1 downto 0 do
  for I := 0 to FMiddlewares.Count - 1 do
    FMiddlewares[I].OnAfterControllerAction(AContext, AActionName, AHandled);
end;

procedure TMVCEngine.ExecuteBeforeControllerActionMiddleware(const AContext: TWebContext;
  const AControllerQualifiedClassName: string; const AActionName: string; var AHandled: Boolean);
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

procedure TMVCEngine.FillActualParamsForAction(const AContext: TWebContext;
  const AActionFormalParams: TArray<TRttiParameter>; const AActionName: string; var AActualParams: TArray<TValue>);
var
  ParamName: string;
  I: Integer;
  StrValue: string;
  FormatSettings: TFormatSettings;
  WasDateTime: Boolean;
  lQualifiedName: string;
begin
  if AContext.Request.SegmentParamsCount <> Length(AActionFormalParams) then
    raise EMVCException.CreateFmt(HTTP_STATUS.BadRequest,
      'Parameters count mismatch (expected %d actual %d) for action "%s"',
      [Length(AActionFormalParams), AContext.Request.SegmentParamsCount, AActionName]);

  SetLength(AActualParams, Length(AActionFormalParams));
  for I := 0 to Length(AActionFormalParams) - 1 do
  begin
    ParamName := AActionFormalParams[I].name;

    if not AContext.Request.SegmentParam(ParamName, StrValue) then
      raise EMVCException.CreateFmt
        (HTTP_STATUS.BadRequest, 'Invalid parameter %s for action %s (Hint: Here parameters names are case-sensitive)',
        [ParamName, AActionName]);

    case AActionFormalParams[I].ParamType.TypeKind of
      tkInteger:
        try
          AActualParams[I] := StrToInt(StrValue);
        except
          on E: Exception do
          begin
            raise EMVCException.CreateFmt(HTTP_STATUS.BadRequest,
              'Invalid Integer value for param [%s] - [CLASS: %s][MSG: %s]',
              [AActionFormalParams[I].name, E.Classname, E.Message]);
          end;
        end;
      tkInt64:
        try
          AActualParams[I] := StrToInt64(StrValue);
        except
          on E: Exception do
          begin
            raise EMVCException.CreateFmt(HTTP_STATUS.BadRequest,
              'Invalid Int64 value for param [%s] - [CLASS: %s][MSG: %s]',
              [AActionFormalParams[I].name, E.Classname, E.Message]);
          end;
        end;
      tkUString:
        begin
          AActualParams[I] := StrValue;
        end;
      tkFloat:
        begin
          WasDateTime := False;
          lQualifiedName := AActionFormalParams[I].ParamType.QualifiedName;
          if lQualifiedName = 'System.TDate' then
          begin
            try
              WasDateTime := True;
              AActualParams[I] := ISODateToDate(StrValue);
            except
              on E: Exception do
              begin
                raise EMVCException.CreateFmt(HTTP_STATUS.BadRequest,
                  'Invalid TDate value for param [%s] - [CLASS: %s][MSG: %s]',
                  [AActionFormalParams[I].name, E.Classname, E.Message]);
              end;
            end;
          end
          else
            if lQualifiedName = 'System.TDateTime' then
          begin
            try
              WasDateTime := True;
              AActualParams[I] := ISOTimeStampToDateTime(StrValue);
            except
              on E: Exception do
              begin
                raise EMVCException.CreateFmt(HTTP_STATUS.BadRequest,
                  'Invalid TDateTime value for param [%s] - [CLASS: %s][MSG: %s]',
                  [AActionFormalParams[I].name, E.Classname, E.Message]);
              end;
            end;
          end
          else
            if lQualifiedName = 'System.TTime' then
          begin
            try
              WasDateTime := True;
              AActualParams[I] := ISOTimeToTime(StrValue);
            except
              on E: Exception do
              begin
                raise EMVCException.CreateFmt(HTTP_STATUS.BadRequest,
                  'Invalid TTime value for param [%s] - [CLASS: %s][MSG: %s]',
                  [AActionFormalParams[I].name, E.Classname, E.Message]);
              end;
            end;
          end;
          if not WasDateTime then
            try
              FormatSettings.DecimalSeparator := '.';
              AActualParams[I] := StrToFloat(StrValue, FormatSettings);
            except
              on E: Exception do
              begin
                raise EMVCException.CreateFmt(HTTP_STATUS.BadRequest,
                  'Invalid Float value for param [%s] - [CLASS: %s][MSG: %s]',
                  [AActionFormalParams[I].name, E.Classname, E.Message]);
              end;
            end;
        end;
      tkEnumeration:
        begin
          if AActionFormalParams[I].ParamType.QualifiedName = 'System.Boolean' then
          begin
            if SameText(StrValue, 'true') or SameText(StrValue, '1') then
              AActualParams[I] := True
            else
              if SameText(StrValue, 'false') or SameText(StrValue, '0') then
              AActualParams[I] := False
            else
            begin
              raise EMVCException.CreateFmt
                (HTTP_STATUS.BadRequest,
                'Invalid boolean value for parameter %s. Boolean parameters accepts only "true"/"false" or "1"/"0".',
                [ParamName]);
            end;
          end
          else
          begin
            raise EMVCException.CreateFmt(HTTP_STATUS.BadRequest, 'Invalid type for parameter %s. Allowed types are ' +
              ALLOWED_TYPED_ACTION_PARAMETERS_TYPES, [ParamName]);
          end;
        end;
      tkRecord:
        begin
          if AActionFormalParams[I].ParamType.QualifiedName = 'System.TGUID' then
          begin
            try
              AActualParams[I] := TValue.From<TGUID>(TMVCGuidHelper.GuidFromString(StrValue));
            except
              raise EMVCException.CreateFmt('Invalid Guid value for param [%s]', [AActionFormalParams[I].name]);
            end;
          end
          else
            raise EMVCException.CreateFmt('Invalid type for parameter %s. Allowed types are ' +
              ALLOWED_TYPED_ACTION_PARAMETERS_TYPES, [ParamName]);
        end
    else
      begin
        raise EMVCException.CreateFmt(HTTP_STATUS.BadRequest, 'Invalid type for parameter %s. Allowed types are ' +
          ALLOWED_TYPED_ACTION_PARAMETERS_TYPES, [ParamName]);
      end;
    end;
  end;
end;

procedure TMVCEngine.FixUpWebModule;
begin
  FSavedOnBeforeDispatch := FWebModule.BeforeDispatch;
  FWebModule.BeforeDispatch := OnBeforeDispatch;
end;

class function TMVCEngine.GetCurrentSession(const ASessionTimeout: Integer; const ASessionId: string;
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

function TMVCEngine.GetServerSignature(const AContext: TWebContext): string;
begin
  if AContext.Config.Value[TMVCConfigKey.ExposeServerSignature] = 'true' then
  begin
    Result := 'DelphiMVCFramework ' + DMVCFRAMEWORK_VERSION;
  end
  else
  begin
    Result := '';
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
    raise EMVCConfigException.Create
      ('No View Engine configured. [HINT: Use TMVCEngine.SetViewEngine() to set a valid view engine]');
  Result := FViewEngineClass;
end;

procedure TMVCEngine.HTTP404(const AContext: TWebContext);
begin
  AContext.Response.SetStatusCode(HTTP_STATUS.NotFound);
  AContext.Response.SetContentType(BuildContentType(TMVCMediaType.TEXT_PLAIN,
    AContext.Config[TMVCConfigKey.DefaultContentCharset]));
  AContext.Response.SetReasonString('Not Found');
  AContext.Response.SetContent('Not Found' + sLineBreak + GetServerSignature(AContext));
end;

procedure TMVCEngine.HTTP500(const AContext: TWebContext; const AReasonString: string);
begin
  AContext.Response.SetStatusCode(HTTP_STATUS.InternalServerError);
  AContext.Response.SetContentType(BuildContentType(TMVCMediaType.TEXT_PLAIN,
    AContext.Config[TMVCConfigKey.DefaultContentCharset]));
  AContext.Response.SetReasonString('Internal server error');
  AContext.Response.SetContent('Internal server error' + sLineBreak + GetServerSignature(AContext) + ': ' +
    AReasonString);
end;

procedure TMVCEngine.SendRawHTTPStatus(const AContext: TWebContext; const HTTPStatusCode: Integer;
  const AReasonString: string; const AClassName: string);
var
  lSer: IMVCSerializer;
  lError: TMVCErrorResponse;
begin
  if Serializers.TryGetValue(AContext.Config[TMVCConfigKey.DefaultContentType], lSer) then
  begin
    lError := TMVCErrorResponse.Create;
    try
      lError.Classname := AClassName;
      lError.StatusCode := HTTPStatusCode;
      lError.Message := AReasonString;
      AContext.Response.SetContent(lSer.SerializeObject(lError));
    finally
      lError.Free;
    end;
    AContext.Response.SetContentType(BuildContentType(AContext.Config[TMVCConfigKey.DefaultContentType],
      AContext.Config[TMVCConfigKey.DefaultContentCharset]));
  end
  else
  begin
    AContext.Response.SetContentType(BuildContentType(TMVCMediaType.TEXT_PLAIN,
      AContext.Config[TMVCConfigKey.DefaultContentCharset]));
    AContext.Response.SetContent(GetServerSignature(AContext) + sLineBreak + 'HTTP ' + HTTPStatusCode.ToString + ': ' +
      AReasonString);
  end;
  AContext.Response.SetStatusCode(HTTPStatusCode);
  AContext.Response.SetReasonString(AReasonString);
end;

function TMVCEngine.IsStaticFileRequest(const ARequest: TWebRequest; out AFileName: string): Boolean;
begin
  Result := (not FConfig[TMVCConfigKey.DocumentRoot].IsEmpty) and
    (TMVCStaticContents.IsStaticFile(FConfig[TMVCConfigKey.DocumentRoot], ARequest.PathInfo, AFileName));
end;

procedure TMVCEngine.LoadSystemControllers;
begin
  Log(TLogLevel.levNormal, 'ENTER: LoadSystemControllers');
  AddController(TMVCSystemController);
  Log(TLogLevel.levNormal, 'EXIT: LoadSystemControllers');
end;

procedure TMVCEngine.OnBeforeDispatch(ASender: TObject; ARequest: TWebRequest; AResponse: TWebResponse;
  var AHandled: Boolean);
begin
  AHandled := False;
  { there is a bug in WebBroker Linux on 10.2.1 tokyo }
  // if Assigned(FSavedOnBeforeDispatch) then
  // begin
  // FSavedOnBeforeDispatch(ASender, ARequest, AResponse, AHandled);
  // end;

  if IsShuttingDown then
  begin
    AResponse.StatusCode := HTTP_STATUS.ServiceUnavailable;
    AResponse.ContentType := TMVCMediaType.TEXT_PLAIN;
    AResponse.Content := 'Server is shutting down';
    AHandled := True;
  end;

  if not AHandled then
  begin
    try
      AHandled := ExecuteAction(ASender, ARequest, AResponse);
    except
      on E: Exception do
      begin
        Log.ErrorFmt('[%s] %s', [E.Classname, E.Message], LOGGERPRO_TAG);
        AResponse.Content := E.Message;
        AResponse.SendResponse;
        AHandled := True;
      end;
    end;
  end;
end;

function TMVCEngine.PublishObject(const AObjectCreatorDelegate: TMVCObjectCreatorDelegate; const AURLSegment: string)
  : TMVCEngine;
begin
  Result := AddController(TMVCJSONRPCPublisher,
    function: TMVCController
    begin
      Result := TMVCJSONRPCPublisher.Create(AObjectCreatorDelegate(), True);
    end, AURLSegment);
end;

procedure TMVCEngine.RegisterDefaultsSerializers;
var
  lDefaultSerializerContentType: string;
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
    FSerializers.Add(lDefaultSerializerContentType, TMVCJSONDataObjectsSerializer.Create);
  end;
end;

procedure TMVCEngine.ResponseErrorPage(const AException: Exception; const ARequest: TWebRequest;
const AResponse: TWebResponse);
begin
  AResponse.SetCustomHeader('x-mvc-error', AException.Classname + ': ' + AException.Message);
  AResponse.StatusCode := HTTP_STATUS.OK;

  begin
    AResponse.ContentType := TMVCMediaType.TEXT_PLAIN;
    AResponse.Content := Config[TMVCConfigKey.ServerName] + ' ERROR:' + sLineBreak + 'Exception raised of class: ' +
      AException.Classname + sLineBreak + '***********************************************' + sLineBreak +
      AException.Message + sLineBreak + '***********************************************';
  end;
end;

class function TMVCEngine.SendSessionCookie(const AContext: TWebContext): string;
var
  SId: string;
begin
  SId := StringReplace(StringReplace(StringReplace(GUIDToString(TGUID.NewGuid), '}', '', []), '{', '', []), '-', '',
    [rfReplaceAll]);
  Result := SendSessionCookie(AContext, SId);
end;

procedure TMVCEngine.SaveCacheConfigValues;
begin
  FConfigCache_MaxRequestSize := StrToInt64Def(Config[TMVCConfigKey.MaxRequestSize],
    TMVCConstants.DEFAULT_MAX_REQUEST_SIZE);
end;

class function TMVCEngine.SendSessionCookie(const AContext: TWebContext; const ASessionId: string): string;
var
  Cookie: TCookie;
  SessionTimeout: Integer;
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

function TMVCEngine.SendStaticFileIfPresent(const AContext: TWebContext; const AFileName: string): Boolean;
var
  lContentType: string;
begin
  Result := False;
  if TFile.Exists(AFileName) then
  begin
    if FMediaTypes.TryGetValue(LowerCase(ExtractFileExt(AFileName)), lContentType) then
      lContentType := BuildContentType(lContentType, FConfig[TMVCConfigKey.DefaultContentCharset])
    else
      lContentType := BuildContentType(TMVCMediaType.APPLICATION_OCTETSTREAM, '');
    TMVCStaticContents.SendFile(AFileName, lContentType, AContext);
    Result := True;
  end;
end;

function TMVCEngine.SetExceptionHandler(
  const AExceptionHandlerProc: TMVCExceptionHandlerProc): TMVCEngine;
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
    raise EMVCException.CreateFmt('ApplicationSession not assigned to this %s instance.', [Classname]);
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

class function TMVCStaticContents.IsScriptableFile(const AStaticFileName: string; const AConfig: TMVCConfig): Boolean;
begin
  Result := TPath.GetExtension(AStaticFileName).ToLower = '.' + AConfig[TMVCConfigKey.DefaultViewFileExtension].ToLower;
end;

class function TMVCStaticContents.IsStaticFile(const AViewPath, AWebRequestPath: string;
out ARealFileName: string): Boolean;
var
  LFileName, lWebRoot: string;
begin
  if TDirectory.Exists(AViewPath) then
  begin
    lWebRoot := TPath.GetFullPath(AViewPath);
  end
  else
  begin
    lWebRoot := TPath.GetFullPath(GetApplicationFileNamePath + AViewPath);
  end;
  LFileName := TPath.GetFullPath(lWebRoot + AWebRequestPath.Replace('/', TPath.DirectorySeparatorChar));
  if not LFileName.StartsWith(lWebRoot) then
  begin
    Exit(False);
  end;
  ARealFileName := LFileName;
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
    if (ReqDate <> 0) and (abs(ReqDate - FileDate) < 2 * (1 / (24 * 60 * 60))) then
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

function TMVCRenderer.GetContentType: string;
begin
  Result := GetContext.Response.ContentType;
  if Result.Trim.IsEmpty then
  begin
    GetContext.Response.ContentType := FContext.FConfig[MVCFramework.Commons.TMVCConfigKey.DefaultContentType];
    Result := GetContentType;
  end;
end;

function TMVCRenderer.GetContext: TWebContext;
begin
  if not Assigned(FContext) then
    raise EMVCException.CreateFmt('Context already set on %s.', [Classname]);
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

function TMVCRenderer.GetStatusCode: Integer;
begin
  Result := GetContext.Response.StatusCode;
end;

function TMVCController.GetViewData(const aModelName: string): TObject;
begin
  if not FViewModel.TryGetValue(aModelName, Result) then
    Result := nil;
end;

function TMVCController.GetViewDataset(const aDataSetName: string): TDataSet;
begin
  if not FViewDataSets.TryGetValue(aDataSetName, Result) then
    Result := nil;
end;

function TMVCController.GetViewDataSets: TMVCViewDataSet;
begin
  if not Assigned(FViewDataSets) then
    FViewDataSets := TMVCViewDataSet.Create;
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
      Log.ErrorFmt('[%s] %s', [E.Classname, E.Message], LOGGERPRO_TAG);
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

procedure TMVCController.PushDataSetToView(const aModelName: string; const ADataSet: TDataSet);
begin
  GetViewDataSets.Add(aModelName, ADataSet);
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

procedure TMVCRenderer.Render(const AObject: TObject; const AOwns: Boolean;
const ASerializationAction: TMVCSerializationAction = nil);
begin
  Render(AObject, AOwns, stDefault, ASerializationAction);
end;

procedure TMVCRenderer.Render(const AContent: string);
var
  lContentType: string;
  lOutEncoding: TEncoding;
  lCharset: string;
begin
  SplitContentMediaTypeAndCharset(GetContentType, lContentType, lCharset);
  if lCharset.IsEmpty then
    lCharset := TMVCConstants.DEFAULT_CONTENT_CHARSET;
  if lContentType.IsEmpty then
    lContentType := TMVCConstants.DEFAULT_CONTENT_TYPE;
  lContentType := BuildContentType(lContentType, lCharset);

  lOutEncoding := TEncoding.GetEncoding(lCharset);
  try
    if SameText('UTF-8', UpperCase(lCharset)) then
      GetContext.Response.SetContentStream(TStringStream.Create(AContent, TEncoding.UTF8), lContentType)
    else
    begin
      GetContext.Response.SetContentStream(TBytesStream.Create(TEncoding.Convert(TEncoding.Default, lOutEncoding,
        TEncoding.Default.GetBytes(AContent))), lContentType);
    end;
  finally
    lOutEncoding.Free;
  end;
end;

procedure TMVCRenderer.Render<T>(const ACollection: TObjectList<T>; const AOwns: Boolean;
const ASerializationAction: TMVCSerializationAction<T>);
begin
  Self.Render<T>(ACollection, AOwns, stDefault, ASerializationAction);
end;

procedure TMVCRenderer.Render202Accepted(const HREF: string; const ID: string; const Reason: string);
begin
  if HREF.IsEmpty then
  begin
    raise EMVCException.Create('Cannot send 202 without provide an HREF');
  end;
  ResponseStatus(HTTP_STATUS.Accepted, Reason);
  Render(TMVCAcceptedResponse.Create(HREF, ID));
end;

procedure TMVCRenderer.Render201Created(const Location, Reason: string);
begin
  if not Location.IsEmpty then
  begin
    FContext.Response.CustomHeaders.AddPair('location', Location);
  end;
  ResponseStatus(HTTP_STATUS.Created, Reason);
end;

procedure TMVCRenderer.Render204NoContent(const Location, Reason: string);
begin
  if not Location.IsEmpty then
  begin
    FContext.Response.CustomHeaders.AddPair('location', Location);
  end;
  ResponseStatus(HTTP_STATUS.NoContent, Reason);
end;

procedure TMVCRenderer.ResponseStatus(const AStatusCode: Integer; const AReasonString: string);
begin
  SetStatusCode(AStatusCode);
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

procedure TMVCRenderer.SendStream(const AStream: TStream; const AOwns: Boolean; const ARewind: Boolean);
var
  lTemp: TStream;
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

function TMVCRenderer.Serializer(const AContentType: string; const ARaiseExceptionIfNotExists: Boolean): IMVCSerializer;
var
  lContentMediaType: string;
  lContentCharSet: string;
begin
  SplitContentMediaTypeAndCharset(AContentType.ToLower, lContentMediaType, lContentCharSet);
  if Engine.Serializers.ContainsKey(lContentMediaType) then
  begin
    Result := Engine.Serializers.Items[lContentMediaType];
  end
  else
  begin
    if ARaiseExceptionIfNotExists then
    begin
      raise EMVCException.CreateFmt('The serializer for %s could not be found.', [lContentMediaType]);
    end
    else
    begin
      Result := nil;
    end;
  end;
end;

function TMVCController.SessionAs<T>: T;
begin
  Result := Session as T;
end;

procedure TMVCRenderer.SetContentType(const AValue: string);
begin
  GetContext.Response.ContentType := AValue;
end;

procedure TMVCRenderer.SetStatusCode(const AValue: Integer);
begin
  GetContext.Response.StatusCode := AValue;
end;

function TMVCRenderer.ToMVCList(const AObject: TObject; AOwnsObject: Boolean): IMVCList;
begin
  Result := MVCFramework.DuckTyping.WrapAsList(AObject, AOwnsObject);
end;

procedure TMVCController.SetViewData(const aModelName: string; const Value: TObject);
begin
  GetViewModel.Add(aModelName, Value);
end;

procedure TMVCController.SetViewDataset(const aDataSetName: string; const Value: TDataSet);
begin
  GetViewDataSets.Add(aDataSetName, Value);
end;

procedure TMVCRenderer.Render(const AObject: TObject; const AOwns: Boolean; const AType: TMVCSerializationType;
const ASerializationAction: TMVCSerializationAction = nil);
begin
  try
    Render(Serializer(GetContentType).SerializeObject(AObject, AType, [], ASerializationAction));
  finally
    if AOwns then
      AObject.Free;
  end;
end;

procedure TMVCRenderer.Render(const AStream: TStream; const AOwns: Boolean);
begin
  SendStream(AStream, AOwns);
end;

procedure TMVCRenderer.Render(const AErrorCode: Integer; const AErrorMessage, AErrorClassName: string;
const ADataObject: TObject);
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
    R.Data := ADataObject;
    Render(R, False, stProperties);
  finally
    R.Free;
  end;
end;

procedure TMVCRenderer.Render(const ADataSet: TDataSet; const AOwns: Boolean; const AIgnoredFields: TMVCIgnoredList;
const ANameCase: TMVCNameCase; const ASerializationType: TMVCDatasetSerializationType;
const ASerializationAction: TMVCDatasetSerializationAction);
begin
  if Assigned(ADataSet) then
  begin
    try
      case ASerializationType of
        dstSingleRecord:
          begin
            Render(Serializer(GetContentType).SerializeDataSetRecord(ADataSet, AIgnoredFields, ANameCase,
              ASerializationAction))

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
      // if ASerializationType = dstSingleRecord then
      // Render(Serializer(GetContentType).SerializeDataSetRecord(ADataSet, AIgnoredFields, ANameCase,
      // ASerializationAction))
      // else
      // Render(Serializer(GetContentType).SerializeDataSet(ADataSet, AIgnoredFields, ANameCase, ASerializationAction))
    finally
      if AOwns then
        ADataSet.Free;
    end;
  end
  else
    raise EMVCException.Create('Can not render an empty dataset.');
end;

procedure TMVCRenderer.Render(const AObject: IInterface; const ASerializationAction: TMVCSerializationAction);
begin
  Render(TObject(AObject), False, ASerializationAction);
end;

procedure TMVCRenderer.Render(const AStatusCode: Integer; AObject: TObject;
const AOwns: Boolean; const ASerializationAction: TMVCSerializationAction);
begin
  ResponseStatus(AStatusCode);
  Render(AObject, AOwns, ASerializationAction);
end;

procedure TMVCRenderer.Render(const AStatusCode: Integer; const AObject: TObject;
const ASerializationAction: TMVCSerializationAction);
begin
  ResponseStatus(AStatusCode);
  Render(AObject, True, ASerializationAction);
end;

procedure TMVCRenderer.Render<T>(const ACollection: TObjectList<T>; const AOwns: Boolean;
const AType: TMVCSerializationType; const ASerializationAction: TMVCSerializationAction<T>);
var
  lSerializationAction: TMVCSerializationAction;
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
      Render(Serializer(GetContentType).SerializeCollection(ACollection, AType, [], lSerializationAction));
    finally
      if AOwns then
        ACollection.Free;
    end;
  end
  else
    raise EMVCException.Create('Can not render an empty collection.');
end;

procedure TMVCRenderer.Render<T>(const AStatusCode: Integer;
const ACollection: TObjectList<T>; const AOwns: Boolean;
const ASerializationAction: TMVCSerializationAction<T>);
begin
  SetStatusCode(AStatusCode);
  Render<T>(ACollection, AOwns, ASerializationAction);
end;

function TMVCController.GetRenderedView(const AViewNames: TArray<string>): string;
var
  lView: TMVCBaseViewEngine;
  lViewName: string;
  lStrStream: TStringStream;
begin
  lStrStream := TStringStream.Create('', TEncoding.UTF8);
  try
    lView := FEngine.ViewEngineClass.Create(Engine, Context, ViewModelList, ViewDataSetList, ContentType);
    try
      for lViewName in AViewNames do
      begin
        lView.Execute(lViewName, lStrStream);
      end;
    finally
      lView.Free;
    end;
    lStrStream.Position := 0;
    Result := lStrStream.DataString;
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

procedure TMVCRenderer.RenderSSE(const EventID, EventData: string; EventName: string; const Retry: Integer);
begin
  // setting up the correct SSE headers
  SetContentType('text/event-stream');
  GetContext.Response.SetCustomHeader('Cache-Control', 'no-cache');
  GetContext.Response.StatusCode := HTTP_STATUS.OK;

  // render the response using SSE compliant data format

  // current event id (the client will resend this number at the next request)
  ResponseStream.Append(Format('id: %s'#13, [EventID]));

  // The browser attempts to reconnect to the source roughly 3 seconds after
  // each connection is closed. You can change that timeout by including a line
  // beginning with "retry:", followed by the number of milliseconds to wait
  // before trying to reconnect.

  if Retry > -1 then
  begin
    ResponseStream.Append(Format('retry: %d'#13, [Retry]));
  end;

  if not EventName.IsEmpty then
  begin
    ResponseStream.Append(Format('event: %s'#13, [EventName]));
  end;

  // actual message
  ResponseStream.Append('data: ' + EventData + #13#13);

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

procedure TMVCRenderer.Render(const AException: Exception; AExceptionItems: TList<string>; const AOwns: Boolean);
var
  S: string;
  R: TMVCErrorResponse;
  I: TMVCErrorResponseItem;
begin
  try
    if AException is EMVCException then
      ResponseStatus(EMVCException(AException).HTTPErrorCode, AException.Message + ' [' + AException.Classname + ']');

    if (GetContext.Response.StatusCode = HTTP_STATUS.OK) then
      ResponseStatus(HTTP_STATUS.InternalServerError, AException.Message + ' [' + AException.Classname + ']');

    if (not GetContext.Request.IsAjax) and (GetContext.Request.ClientPrefer(TMVCMediaType.TEXT_HTML)) then
    begin
      SetContentType(TMVCMediaType.TEXT_HTML);
      ResponseStream.Clear;
      ResponseStream.Append
        ('<html><head><style>pre { padding: 15px; color: #000000; background-color: #e0e0e0; }</style></head><body>')
        .Append('<h1>' + Config[TMVCConfigKey.ServerName] + ': Error Raised</h1>')
        .AppendFormat('<pre>HTTP Return Code: %d' + sLineBreak, [GetContext.Response.StatusCode])
        .AppendFormat('HTTP Reason Text: "%s"</pre>', [GetContext.Response.ReasonString]).Append('<h3><pre>')
        .AppendFormat('Exception Class Name : %s' + sLineBreak, [AException.Classname])
        .AppendFormat('Exception Message    : %s' + sLineBreak, [AException.Message]).Append('</pre></h3>');
      if Assigned(AExceptionItems) and (AExceptionItems.Count > 0) then
      begin
        ResponseStream.Append('<h2><pre>');
        for S in AExceptionItems do
          ResponseStream.AppendLine('- ' + S);
        ResponseStream.Append('</pre><h2>');
      end
      else
        ResponseStream.AppendLine('<pre>No other information available</pre>');
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
        R.Classname := AException.Classname;
        if Assigned(AExceptionItems) and (AExceptionItems.Count > 0) then
        begin
          for S in AExceptionItems do
          begin
            I := TMVCErrorResponseItem.Create;
            I.Message := S;
            R.Items.Add(I);
          end;
        end;
        if Serializer(GetContentType, False) = nil then
        begin
          GetContext.Response.ContentType := GetConfig[TMVCConfigKey.DefaultContentType];
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

procedure TMVCRenderer.Render(const AResponse: TMVCResponse; const AOwns: Boolean);
begin
  if Assigned(AResponse) then
  begin
    try
      GetContext.Response.StatusCode := AResponse.StatusCode;
      Render(AResponse, False, stProperties);
    finally
      if AOwns then
        AResponse.Free;
    end;
  end
  else
    raise EMVCException.Create('Cannot render an empty response object.');
end;

procedure TMVCRenderer.Render(const ADataSet: TDataSet; const ASerializationAction: TMVCDatasetSerializationAction);
begin
  Render(ADataSet, True, ASerializationAction);
end;

procedure TMVCRenderer.Render(const ADataSet: TDataSet; const AOwns: Boolean;
const ASerializationAction: TMVCDatasetSerializationAction);
begin
  Render(ADataSet, AOwns, dstAllRecords, ASerializationAction);
end;

procedure TMVCRenderer.Render(const AObject: TObject; const ASerializationAction: TMVCSerializationAction = nil);
begin
  Render(AObject, True, ASerializationAction);
end;

procedure TMVCRenderer.Render(
  const ADataSet: TDataSet;
const AOwns: Boolean;
const AIgnoredFields: TMVCIgnoredList;
const ASerializationType: TMVCDatasetSerializationType;
const ASerializationAction: TMVCDatasetSerializationAction);
begin
  Render(ADataSet, AOwns, AIgnoredFields, ncLowerCase, ASerializationType, ASerializationAction);
end;

procedure TMVCRenderer.Render(
  const ADataSet: TDataSet;
const AOwns: Boolean;
const ASerializationType: TMVCDatasetSerializationType;
const ASerializationAction: TMVCDatasetSerializationAction);
begin
  Render(ADataSet, AOwns, [], ASerializationType, ASerializationAction);
end;

constructor TMVCResponse.Create;
begin
  inherited Create;
end;

constructor TMVCErrorResponse.Create;
begin
  inherited Create;
  FItems := TObjectList<TMVCErrorResponseItem>.Create(True);
end;

constructor TMVCResponse.Create(AStatusCode: Integer; AReasonString, AMessage: string);
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

constructor TMVCBaseViewEngine.Create(const AEngine: TMVCEngine; const AWebContext: TWebContext;
const AViewModel: TMVCViewDataObject; const AViewDataSets: TObjectDictionary<string, TDataSet>;
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
  FileName := StringReplace(AViewName, '/', '\', [rfReplaceAll]);

  if (FileName = '\') then
    FileName := '\index.' + DefaultViewFileExtension
  else
    FileName := FileName + '.' + DefaultViewFileExtension;

  if DirectoryExists(Config[TMVCConfigKey.ViewPath]) then
    F := ExpandFileName(IncludeTrailingPathDelimiter(Config.Value[TMVCConfigKey.ViewPath]) + FileName)
  else
    F := ExpandFileName(IncludeTrailingPathDelimiter(GetApplicationFileNamePath + Config.Value[TMVCConfigKey.ViewPath])
      + FileName);

  if not TFile.Exists(F) then
    FileName := ExpandFileName(IncludeTrailingPathDelimiter(GetApplicationFileNamePath +
      Config.Value[TMVCConfigKey.DocumentRoot]) + FileName)
  else
    FileName := F;

  if FileExists(FileName) then
    Result := FileName
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
  FType := AType;
  FValue := APattern;
  FFormat := AFormat;
end;

{ MVCParamAttribute }

constructor MVCParamAttribute.Create(name: string;
Location: TSwagRequestParameterInLocation; AType: TSwagTypeParameter;
APattern, AFormat: string);
begin
  FName := name;
  FLocation := Location;
  FType := AType;
  FPattern := APattern;
  FFormat := AFormat;
end;

constructor MVCParamAttribute.Create(name: string;
Location: TSwagRequestParameterInLocation; AType: TClass; APattern,
  AFormat: string);
begin
  FName := name;
  FLocation := Location;
  FClassType := AType;
  FPattern := APattern;
  FFormat := AFormat;
end;

initialization

_IsShuttingDown := 0;

_MVCGlobalActionParamsCache := TMVCStringObjectDictionary<TMVCActionParamCacheItem>.Create;

finalization

FreeAndNil(_MVCGlobalActionParamsCache);

end.
