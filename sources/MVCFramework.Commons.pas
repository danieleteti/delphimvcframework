// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2022 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Commons;

{$I dmvcframework.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.IOUtils,
  Data.DB,
  IdGlobal,
  IdCoderMIME,
  IdContext,
  System.Generics.Collections,
  MVCFramework.DuckTyping,
  JsonDataObjects;

{$I dmvcframeworkbuildconsts.inc}


type

  TMVCHTTPMethodType = (httpGET, httpPOST, httpPUT, httpDELETE, httpPATCH, httpHEAD, httpOPTIONS,
    httpTRACE);

  TMVCHTTPMethods = set of TMVCHTTPMethodType;

  TMVCMediaType = record
  public const
    APPLICATION_ATOM_XML = 'application/atom+xml';
    APPLICATION_JSON = 'application/json';
    APPLICATION_OCTET_STREAM = 'application/octet-stream';
    APPLICATION_SVG_XML = 'application/svg+xml';
    APPLICATION_XHTML_XML = 'application/xhtml+xml';
    APPLICATION_XML = 'application/xml';
    APPLICATION_OCTETSTREAM = 'application/octet-stream';
    MEDIA_TYPE_WILDCARD = '*';
    MULTIPART_FORM_DATA = 'multipart/form-data';
    APPLICATION_FORM_URLENCODED = 'application/x-www-form-urlencoded';
    TEXT_HTML = 'text/html';
    TEXT_PLAIN = 'text/plain';
    TEXT_XML = 'text/xml';
    TEXT_CSS = 'text/css';
    TEXT_JAVASCRIPT = 'text/javascript';
    TEXT_CACHEMANIFEST = 'text/cache-manifest';
    TEXT_EVENTSTREAM = 'text/event-stream';
    TEXT_CSV = 'text/csv';
    IMAGE_JPEG = 'image/jpeg';
    IMAGE_X_PNG = 'image/x-png';
    IMAGE_X_ICON = 'image/x-icon';
    IMAGE_PNG = 'image/png';
    IMAGE_SVG_XML = 'image/svg+xml';
    IMAGE_GIF = 'image/gif';
    APPLICATION_PDF = 'application/pdf';
    APPLICATION_X_PDF = 'application/x-pdf';
    WILDCARD = '*/*';
  end;

  TMVCCharSet = record
  public const
    US_ASCII = 'US-ASCII';
    WINDOWS_1250 = 'windows-1250';
    WINDOWS_1251 = 'windows-1251';
    WINDOWS_1252 = 'windows-1252';
    WINDOWS_1253 = 'windows-1253';
    WINDOWS_1254 = 'windows-1254';
    WINDOWS_1257 = 'windows-1257';
    ISO88591 = 'ISO-8859-1';
    ISO88592 = 'ISO-8859-2';
    ISO88593 = 'ISO-8859-3';
    ISO88594 = 'ISO-8859-4';
    ISO88595 = 'ISO-8859-5';
    ISO88596 = 'ISO-8859-6';
    ISO88597 = 'ISO-8859-7';
    ISO88598 = 'ISO-8859-8';
    ISO885915 = 'ISO-8859-15';
    UTF_8 = 'UTF-8';
    UTF_16 = 'UTF-16';
    UTF_16BE = 'UTF-16BE';
    UTF_16LE = 'UTF-16LE';
  end;

  TMVCConstants = record
  public const
    SESSION_TOKEN_NAME = 'dtsessionid';
    DEFAULT_CONTENT_CHARSET = 'UTF-8';
    DEFAULT_CONTENT_TYPE = TMVCMediaType.APPLICATION_JSON;
    CURRENT_USER_SESSION_KEY = '__DMVC_CURRENT_USER__';
    LAST_AUTHORIZATION_HEADER_VALUE = '__DMVC_LAST_AUTHORIZATION_HEADER_VALUE_';
    SSE_RETRY_DEFAULT = 100;
    SSE_LAST_EVENT_ID = 'Last-Event-ID';
    URL_MAPPED_PARAMS_ALLOWED_CHARS = ' àèéùòì''"@\[\]\{\}\(\)\=;&#\.:!\_,%\w\d\x2D\x3A\$';
    OneMiB = 1048576;
    OneKiB = 1024;
    DEFAULT_MAX_REQUEST_SIZE = OneMiB * 5; // 5 MiB
    HATEOAS_PROP_NAME = 'links';
    X_HTTP_Method_Override = 'X-HTTP-Method-Override';
    MAX_RECORD_COUNT = 100;
  end;

  HATEOAS = record
  public const
    /// <summary>
    /// Target URI: It indicates the target resource URI. This is represented by the href attribute.
    /// </summary>
    HREF = 'href';
    /// <summary>
    /// Link relation: The link relation type describes how the current context is related to the target resource. This is represented by the rel attribute.
    /// </summary>
    REL = 'rel';
    /// <summary>
    /// Type: This indicates the expected resource media type. This is represented by the type attribute.
    /// </summary>
    _TYPE = 'type';
  end;

  TMVCConfigKey = record
  public const
    SessionTimeout = 'sessiontimeout';
    ViewPath = 'view_path';
    DefaultContentType = 'default_content_type';
    DefaultContentCharset = 'default_content_charset';
    DefaultViewFileExtension = 'default_view_file_extension';
    PathPrefix = 'pathprefix';
    AllowUnhandledAction = 'allow_unhandled_action';
    ServerName = 'server_name';
    ExposeServerSignature = 'server_signature';
    ExposeXPoweredBy = 'xpoweredby';
    SessionType = 'session_type';
    MaxEntitiesRecordCount = 'max_entities_record_count';
    MaxRequestSize = 'max_request_size'; // bytes
    HATEOSPropertyName = 'hateos';
    LoadSystemControllers = 'load_system_controllers';
  end;

  TMVCHostingFrameworkType = (hftUnknown, hftIndy, hftApache, hftISAPI);

  // http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
  HTTP_STATUS = record
  const
    // Informational 1xx
    Continue = 100;
    SwitchingProtocols = 101;
    // Successful 2xx
    /// <summary>
    /// 200 OK
    /// The request has succeeded. The information returned with the response is dependent on the method used in the request, for example:
    /// GET an entity corresponding to the requested resource is sent in the response;
    /// HEAD the entity-header fields corresponding to the requested resource are sent in the response without any message-body;
    /// POST an entity describing or containing the result of the action;
    /// TRACE an entity containing the request message as received by the end server.
    /// </summary>
    OK = 200;
    /// <summary>
    /// 201 Created
    /// The request has been fulfilled and resulted in a new resource being created. The newly created resource can be referenced by the URI(s) returned in the entity of the response, with the most specific URI for the resource given by a Location header field. The response SHOULD include an entity containing a list of resource characteristics and location(s) from which the user or user agent can choose the one most appropriate. The entity format is specified by the media type given in the Content-Type header field. The origin server MUST create the resource before returning the 201 status code. If the action cannot be carried out immediately, the server SHOULD respond with 202 (Accepted) response instead.
    /// A 201 response MAY contain an ETag response header field indicating the current value of the entity tag for the requested variant just created
    /// </summary>
    Created = 201;
    /// <summary>
    /// 202 Accepted
    /// The request has been accepted for processing, but the processing has not been completed. The request might or might not eventually be acted upon, as it might be disallowed when processing actually takes place. There is no facility for re-sending a status code from an asynchronous operation such as this.
    /// The 202 response is intentionally non-committal. Its purpose is to allow a server to accept a request for some other process (perhaps a batch-oriented process that is only run once per day) without requiring that the user agent's connection to the server persist until the process is completed. The entity returned with this response SHOULD include an indication of the request's current status and either a pointer to a status monitor or some estimate of when the user can expect the request to be fulfilled.
    /// </summary>
    Accepted = 202;
    ///
    NonAuthoritativeInformation = 203;
    /// <summary>
    /// 204 No Content
    /// The server has fulfilled the request but does not need to return an entity-body, and might want to return updated metainformation. The response MAY include new or updated metainformation in the form of entity-headers, which if present SHOULD be associated with the requested variant.
    /// If the client is a user agent, it SHOULD NOT change its document view from that which caused the request to be sent. This response is primarily intended to allow input for actions to take place without causing a change to the user agent's active document view, although any new or updated metainformation SHOULD be applied to the document currently in the user agent's active view.
    /// The 204 response MUST NOT include a message-body, and thus is always terminated by the first empty line after the header fields.
    /// </summary>
    NoContent = 204;
    ResetContent = 205;
    PartialContent = 206;
    // Redirection 3xx
    MultipleChoices = 300;
    /// <summary>
    /// 301 Moved Permanently
    /// The requested resource has been assigned a new permanent URI and any future references to this resource SHOULD use one of the returned URIs. Clients with link editing capabilities ought to automatically re-link references to the Request-URI to one or more of the new references returned by the server, where possible. This response is cacheable unless indicated otherwise.
    /// The new permanent URI SHOULD be given by the Location field in the response. Unless the request method was HEAD, the entity of the response SHOULD contain a short hypertext note with a hyperlink to the new URI(s).
    /// If the 301 status code is received in response to a request other than GET or HEAD, the user agent MUST NOT automatically redirect the request unless it can be confirmed by the user, since this might change the conditions under which the request was issued.
    /// Note: When automatically redirecting a POST request after
    /// receiving a 301 status code, some existing HTTP/1.0 user agents
    /// will erroneously change it into a GET request.
    /// </summary>
    MovedPermanently = 301;
    /// <summary>
    /// 302 Found
    /// The requested resource resides temporarily under a different URI. Since the redirection might be altered on occasion, the client SHOULD continue to use the Request-URI for future requests. This response is only cacheable if indicated by a Cache-Control or Expires header field.
    /// The temporary URI SHOULD be given by the Location field in the response. Unless the request method was HEAD, the entity of the response SHOULD contain a short hypertext note with a hyperlink to the new URI(s).
    /// If the 302 status code is received in response to a request other than GET or HEAD, the user agent MUST NOT automatically redirect the request unless it can be confirmed by the user, since this might change the conditions under which the request was issued.
    /// </summary>
    Found = 302;
    /// <summary>
    /// 303 See Other
    /// The response to the request can be found under a different URI and SHOULD be retrieved using a GET method on that resource. This method exists primarily to allow the output of a POST-activated script to redirect the user agent to a selected resource. The new URI is not a substitute reference for the originally requested resource. The 303 response MUST NOT be cached, but the response to the second (redirected) request might be cacheable.
    /// The different URI SHOULD be given by the Location field in the response. Unless the request method was HEAD, the entity of the response SHOULD contain a short hypertext note with a hyperlink to the new URI(s).
    /// </summary>
    SeeOther = 303;
    /// <summary>
    /// 304 Not Modified
    /// If the client has performed a conditional GET request and access is allowed, but the document has not been modified, the server SHOULD respond with this status code. The 304 response MUST NOT contain a message-body, and thus is always terminated by the first empty line after the header fields.
    /// The response MUST include the following header fields:
    /// - Date, unless its omission is required by section 14.18.1
    /// If a clockless origin server obeys these rules, and proxies and clients add their own Date to any response received without one (as already specified by [RFC 2068], section 14.19), caches will operate correctly.
    /// - ETag and/or Content-Location, if the header would have been sent in a 200 response to the same request
    /// - Expires, Cache-Control, and/or Vary, if the field-value might differ from that sent in any previous response for the same variant
    /// If the conditional GET used a strong cache validator (see section 13.3.3), the response SHOULD NOT include other entity-headers. Otherwise (i.e., the conditional GET used a weak validator), the response MUST NOT include other entity-headers; this prevents inconsistencies between cached entity-bodies and updated headers.
    /// If a 304 response indicates an entity not currently cached, then the cache MUST disregard the response and repeat the request without the conditional.
    /// If a cache uses a received 304 response to update a cache entry, the cache MUST update the entry to reflect any new field values given in the response.
    /// </summary>
    NotModified = 304;
    UseProxy = 305;
    TemporaryRedirect = 307;
    // Client Error 4xx
    /// <summary>
    /// The request could not be understood by the server due to malformed syntax. The client SHOULD NOT repeat the request without modifications.
    /// </summary>
    BadRequest = 400;
    /// <summary>
    /// 401 Unauthorized
    /// The request requires user authentication. The response MUST include a WWW-Authenticate header field (section 14.47) containing a challenge applicable to the requested resource. The client MAY repeat the request with a suitable Authorization header field (section 14.8). If the request already included Authorization credentials, then the 401 response indicates that authorization has been refused for those credentials. If the 401 response contains the same challenge as the prior response, and the user agent has already attempted authentication at least once, then the user SHOULD be presented the entity that was given in the response, since that entity might include relevant diagnostic information. HTTP access authentication is explained in "HTTP Authentication: Basic and Digest Access Authentication".
    /// </summary>
    Unauthorized = 401;
    PaymentRequired = 402;
    /// <summary>
    /// 403 Forbidden
    /// The server understood the request, but is refusing to fulfill it. Authorization will not help and the request SHOULD NOT be repeated. If the request method was not HEAD and the server wishes to make public why the request has not been fulfilled, it SHOULD describe the reason for the refusal in the entity. If the server does not wish to make this information available to the client, the status code 404 (Not Found) can be used instead.
    /// </summary>
    Forbidden = 403;
    /// <summary>
    /// 404 Not Found
    /// The server has not found anything matching the Request-URI. No indication is given of whether the condition is temporary or permanent. The 410 (Gone) status code SHOULD be used if the server knows, through some internally configurable mechanism, that an old resource is permanently unavailable and has no forwarding address. This status code is commonly used when the server does not wish to reveal exactly why the request has been refused, or when no other response is applicable.
    /// </summary>
    NotFound = 404;
    /// <summary>
    /// 405 Method Not Allowed
    /// The method specified in the Request-Line is not allowed for the resource identified by the Request-URI. The response MUST include an Allow header containing a list of valid methods for the requested resource.
    /// </summary>
    MethodNotAllowed = 405;
    NotAcceptable = 406;
    ProxyAuthenticationRequired = 407;
    RequestTimeout = 408;
    Conflict = 409;
    Gone = 410;
    LengthRequired = 411;
    /// <summary>
    /// 412 Precondition Failed
    /// Any request can contain a conditional header defined in HTTP (If-
    /// Match, If-Modified-Since, etc.) or the "If" or "Overwrite"
    /// conditional headers defined in this specification.  If the server
    /// evaluates a conditional header, and if that condition fails to hold,
    /// then this error code MUST be returned.  On the other hand, if the
    /// client did not include a conditional header in the request, then the
    /// server MUST NOT use this status code.
    /// </summary>
    PreconditionFailed = 412;
    RequestEntityTooLarge = 413;
    RequestURITooLong = 414;
    UnsupportedMediaType = 415;
    RequestedRangeNotSatisfiable = 416;
    ExpectationFailed = 417;
    /// <summary>
    /// The 422 (Unprocessable Entity) status code means the server
    /// understands the content type of the request entity (hence a
    /// 415(Unsupported Media Type) status code is inappropriate), and the
    /// syntax of the request entity is correct (thus a 400 (Bad Request)
    /// status code is inappropriate) but was unable to process the contained
    /// instructions.  For example, this error condition may occur if an XML
    /// request body contains well-formed (i.e., syntactically correct), but
    /// semantically erroneous, XML instructions.
    /// </summary>
    UnprocessableEntity = 422;
    /// <summary>
    /// The 423 (Locked) status code means the source or destination resource
    /// of a method is locked.  This response SHOULD contain an appropriate
    /// precondition or postcondition code, such as 'lock-token-submitted' or 'no-conflicting-lock
    /// </summary>
    Locked = 423;
    /// <summary>
    /// The 424 (Failed Dependency) status code means that the method could
    /// not be performed on the resource because the requested action
    /// depended on another action and that action failed.  For example, if a
    /// command in a PROPPATCH method fails, then, at minimum, the rest of
    /// the commands will also fail with 424 (Failed Dependency).
    /// </summary>
    FailedDependency = 424;
    /// <summary>
    /// The 429 (Too Many Requests) status code indicates the user has sent too many requests
    /// in a given amount of time ("rate limiting").
    /// </summary>
    TooManyRequests = 429;
    // Server Error 5xx
    /// <summary>
    /// 500 Internal Server Error
    /// The server encountered an unexpected condition which prevented it from fulfilling the request.
    /// </summary>
    InternalServerError = 500;
    /// <summary>
    /// 501 Not Implemented
    /// The server does not support the functionality required to fulfill the request. This is the appropriate response when the server does not recognize the request method and is not capable of supporting it for any resource.
    /// </summary>
    NotImplemented = 501;
    BadGateway = 502;
    /// <summary>
    /// 503 Service Unavailable
    /// The server is currently unable to handle the request due to a temporary overloading or maintenance of the server. The implication is that this is a temporary condition which will be alleviated after some delay. If known, the length of the delay MAY be indicated in a Retry-After header. If no Retry-After is given, the client SHOULD handle the response as it would for a 500 response.
    /// Note: The existence of the 503 status code does not imply that a
    /// server must use it when becoming overloaded. Some servers may wish
    /// to simply refuse the connection.
    /// </summary>
    ServiceUnavailable = 503;
    GatewayTimeout = 504;
    HTTPVersionNotSupported = 505;

    /// <summary>
    /// The 507 (Insufficient Storage) status code means the method could not
    /// be performed on the resource because the server is unable to store
    /// the representation needed to successfully complete the request.
    /// This condition is considered to be temporary.  If the request that
    /// received this status code was the result of a user action, the
    /// request MUST NOT be repeated until it is requested by a separate user action.
    /// </summary>
    InsufficientStorage = 507;
  end;

  EMVCException = class(Exception)
  protected
    FHttpErrorCode: UInt16;
    FAppErrorCode: UInt16;
    FDetailedMessage: string;
    FErrorItems: TArray<String>;
    procedure CheckHTTPErrorCode(const AHTTPErrorCode: UInt16);
  public
    constructor Create(const AMsg: string); overload; virtual;
    constructor Create(const AMsg: string; const ADetailedMessage: string;
      const AAppErrorCode: UInt16 = 0;
      const AHTTPErrorCode: UInt16 = HTTP_STATUS.InternalServerError;
      const AErrorItems: TArray<String> = nil); overload; virtual;
    constructor Create(const AHTTPErrorCode: UInt16; const AMsg: string); overload; virtual;
    constructor Create(const AHTTPErrorCode: UInt16; const AAppErrorCode: Integer; const AMsg: string);
      overload; virtual;
    constructor CreateFmt(const AMsg: string; const AArgs: array of const); reintroduce; overload;
    constructor CreateFmt(const AHTTPErrorCode: UInt16; const AMsg: string; const AArgs: array of const); overload;
    property HttpErrorCode: UInt16 read FHttpErrorCode;
    property DetailedMessage: string read FDetailedMessage write FDetailedMessage;
    property ApplicationErrorCode: UInt16 read FAppErrorCode write FAppErrorCode;
    property ErrorItems: TArray<String> read FErrorItems;
  end;

  EMVCSessionExpiredException = class(EMVCException)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  EMVCConfigException = class(EMVCException)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  EMVCFrameworkViewException = class(EMVCException)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  EMVCJWTException = class(EMVCException)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  EMVCViewError = class(EMVCException)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  TMVCStringDictionaryList = class;

  TMVCRequestParamsTable = class(TDictionary<string, string>)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  IMVCLinkItem = interface
    ['{8BC70061-0DD0-4D0A-B135-F83A5C86629B}']
    function Add(const PropName: string; const PropValue: string): IMVCLinkItem;
  end;

  IMVCLinks = interface
    ['{8A116BED-9A10-4885-AD4B-DF38A7F0D7DF}']
    function AddRefLink: IMVCLinkItem;
    function Clear: IMVCLinks;
    function LinksData: TMVCStringDictionaryList;
  end;

  TMVCLinks = class(TInterfacedObject, IMVCLinks)
  private
    fData: TMVCStringDictionaryList;
  protected
    function AddRefLink: IMVCLinkItem;
    function Clear: IMVCLinks;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function LinksData: TMVCStringDictionaryList;
  end;

  // IMVCStringDictionary = interface
  // ['{164117AD-8DDD-47F7-877C-453979707D10}']
  // function GetItems(const Key: string): string;
  // procedure SetItems(const Key, Value: string);
  // procedure Clear;
  /// /    function Add(const Name, Value: string): IMVCStringDictionary;
  // function TryGetValue(const Name: string; out Value: string): Boolean; overload;
  // function TryGetValue(const Name: string; out Value: Integer): Boolean; overload;
  // function Count: Integer;
  // function GetEnumerator: TDictionary<string, string>.TPairEnumerator;
  // function ContainsKey(const Key: string): Boolean;
  // function Keys: TArray<string>;
  // property Items[const Key: string]: string read GetItems; default;
  // end;

  TMVCStringDictionary = class // (TInterfacedObject, IMVCStringDictionary)
  strict private
    function GetItems(const Key: string): string;
    procedure SetItems(const Key, Value: string);
  protected
    fDict: TDictionary<string, string>;
  public
    constructor Create; overload; virtual;
    constructor Create(const aKey, aValue: string); overload; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Add(const Name, Value: string): TMVCStringDictionary;
    function AddStrings(const Strings: TStrings): TMVCStringDictionary;
    function TryGetValue(const Name: string; out Value: string): Boolean; overload;
    function TryGetValue(const Name: string; out Value: Integer): Boolean; overload;
    function Count: Integer;
    function GetEnumerator: TDictionary<string, string>.TPairEnumerator;
    function ContainsKey(const Key: string): Boolean;
    function Keys: TArray<string>;
    function ToString: String; override;
    property Items[const Key: string]: string read GetItems write SetItems; default;
  end;

  TMVCDecoratorObject = class(TInterfacedObject, IMVCLinkItem)
  private
    fData: TMVCStringDictionary;
  public
    constructor Create(const aData: TMVCStringDictionary);
    function Add(const PropName: string; const PropValue: string): IMVCLinkItem;
  end;

  TMVCStringDictionaryList = class(TObjectList<TMVCStringDictionary>)
  public
    constructor Create;
  end;

  { This type is thread safe }
  TMVCStringObjectDictionary<T: class> = class
  private
    FMREWS: TMultiReadExclusiveWriteSynchronizer;
  protected
    fDict: TObjectDictionary<string, T>;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function TryGetValue(const Name: string; out Value: T): Boolean;
    procedure Add(const Name: string; Value: T);
  end;

  TMVCViewDataObject = class(TObjectDictionary<string, TObject>)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create;
  end;

  TMVCViewDataSet = class(TObjectDictionary<string, TDataset>)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create;
  end;

  TMVCCriticalSectionHelper = class helper for TCriticalSection
  public
    procedure DoWithLock(const AAction: TProc);
    function DoWithLockTimeout(const AAction: TProc; const ATimeOut: UInt32): TWaitResult;
  end;

  TMultiReadExclusiveWriteSynchronizerHelper = class helper for TMultiReadExclusiveWriteSynchronizer
  public
    procedure DoWithWriteLock(const AAction: TProc);
    procedure DoWithReadLock(const AAction: TProc);
  end;

  TMVCConfig = class sealed
  private
    FConfig: TMVCStringDictionary;
    FFreezed: Boolean;
    function GetValue(const AIndex: string): string;
    function GetValueAsInt64(const AIndex: string): Int64;
    procedure SetValue(const AIndex: string; const aValue: string);
    procedure CheckNotFreezed; inline;
  protected
    { protected declarations }
  public
    constructor Create;
    destructor Destroy; override;
    procedure Freeze;
    function Keys: TArray<string>;
    function ToString: string; override;
    procedure SaveToFile(const AFileName: string);
    procedure LoadFromFile(const AFileName: string);

    property Value[const AIndex: string]: string read GetValue write SetValue; default;
    property AsInt64[const AIndex: string]: Int64 read GetValueAsInt64;
  end;

  TMVCStreamHelper = class helper for TStream
  public
    procedure WriteUTF8(const AString: string);
  end;

  TMVCFieldMap = record
    InstanceFieldName: string;
    DatabaseFieldName: string;
    Alias: String; // allows to use "MVCNameAs" attribute in RQL queries
  end;

  TMVCCustomRouter = class abstract
  public
    function GetQualifiedActionName(): string; virtual; abstract;
  end;

  TMVCGuidHelper = record
  public
    class function StringToGUIDEx(const aGuidStr: string): TGUID; static; inline;
    class function GUIDToStringEx(const aGuid: TGUID): string; static; inline;
  end;

  TMVCStringHelper = record
  public
    class function StartsText(const ASubText, AText: string): Boolean; static;
    class function StartsWith(const ASubText, AText: string; AIgnoreCase: Boolean): Boolean; static;
  end;

  TMVCFieldsMapping = TArray<TMVCFieldMap>;

{$SCOPEDENUMS ON}
  TMVCCompressionType = (ctNone, ctDeflate, ctGZIP);


{ GENERIC TYPE ALIASES }
TMVCListOfString = TList<string>;
TMVCListOfInteger =  TList<Integer>;
TMVCListOfBoolean = TList<Boolean>;
TMVCListOfDouble =  TList<Double>;
{ GENERIC TYPE ALIASES // END}

function AppPath: string;
function IsReservedOrPrivateIP(const AIP: string): Boolean; inline;
function IP2Long(const AIP: string): UInt32; inline;

function B64Encode(const aValue: string): string; overload;
function B64Encode(const aValue: TBytes): string; overload;
function B64Decode(const aValue: string): string;

function URLSafeB64encode(const Value: string; IncludePadding: Boolean; AByteEncoding: IIdTextEncoding = nil)
  : string; overload;
function URLSafeB64encode(const Value: TBytes; IncludePadding: Boolean): string; overload;
function URLSafeB64Decode(const Value: string; AByteEncoding: IIdTextEncoding = nil): string;

function URLEncode(const Value: string): string; overload;
function URLDecode(const Value: string): string;


function ByteToHex(AInByte: Byte): string;
function BytesToHex(ABytes: TBytes): string;
procedure Base64StringToFile(const aBase64String, AFileName: string; const aOverwrite: Boolean = False);
function FileToBase64String(const FileName: string): string;

procedure SplitContentMediaTypeAndCharset(const aContentType: string; var aContentMediaType: string;
  var aContentCharSet: string);
function BuildContentType(const aContentMediaType: string; const aContentCharSet: string): string;

function StrToJSONObject(const aString: String; ARaiseExceptionOnError: Boolean = False): TJsonObject;
function StrToJSONArray(const aString: String; ARaiseExceptionOnError: Boolean = False): TJsonArray;

function WrapAsList(const AObject: TObject; AOwnsObject: Boolean = False): IMVCList;

{ changing case }
function CamelCase(const Value: string; const MakeFirstUpperToo: Boolean = False): string;
function SnakeCase(const Value: string): string;

const
  MVC_HTTP_METHODS_WITHOUT_CONTENT: TMVCHTTPMethods = [httpGET, httpDELETE, httpHEAD, httpOPTIONS];
  MVC_HTTP_METHODS_WITH_CONTENT: TMVCHTTPMethods = [httpPOST, httpPUT, httpPATCH];

const
  MVC_COMPRESSION_TYPE_AS_STRING: array [TMVCCompressionType] of string = ('none', 'deflate', 'gzip');
  MVC_COMPRESSION_ZLIB_WINDOW_BITS: array [TMVCCompressionType] of Integer = (0, -15, 31);
  // WindowBits: http://zlib.net/manual.html#Advanced

var
  gLock: TObject;

const
  RESERVED_IPv4: array [1 .. 11] of array [1 .. 2] of string = (
    ('0.0.0.0', '0.255.255.255'),
    ('10.0.0.0', '10.255.255.255'),
    ('127.0.0.0', '127.255.255.255'),
    ('169.254.0.0', '169.254.255.255'),
    ('172.16.0.0', '172.31.255.255'),
    ('192.0.2.0', '192.0.2.255'),
    ('192.88.99.0', '192.88.99.255'),
    ('192.168.0.0', '192.168.255.255'),
    ('198.18.0.0', '198.19.255.255'),
    ('224.0.0.0', '239.255.255.255'),
    ('240.0.0.0', '255.255.255.255'));

type
  TMVCParseAuthentication = class
  public
    class procedure OnParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: string; var VUsername,
      VPassword: string; var VHandled: Boolean);
  end;


implementation

uses

  IdCoder3to4,
  System.NetEncoding,
  System.Character,
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.Serializer.Commons,
  MVCFramework.Utils,
  System.RegularExpressions;

var
  GlobalAppName, GlobalAppPath, GlobalAppExe: string;


function URLEncode(const Value: string): string; overload;
begin
  Result := TNetEncoding.URL.Encode(Value);
end;

function URLDecode(const Value: string): string;
begin
  Result := TNetEncoding.URL.Decode(Value);
end;

function AppPath: string;
begin
  Result := GlobalAppPath;
end;

function IP2Long(const AIP: string): Cardinal;
var
  lPieces: TArray<string>;
begin
  if AIP.IsEmpty then
    Exit(0);
  lPieces := AIP.Split(['.']);
  Result := (StrToInt(lPieces[0]) * 16777216) + (StrToInt(lPieces[1]) * 65536) +
    (StrToInt(lPieces[2]) * 256) +
    StrToInt(lPieces[3]);
end;

function IsReservedOrPrivateIP(const AIP: string): Boolean;
var
  I: Integer;
  IntIP: Cardinal;
begin
  Result := False;
  if Pos(':', AIP) > 0 then
  begin
    {TODO -oDanieleT -cGeneral : Support for IPv6 Reserved IP}
    //https://www.iana.org/assignments/iana-ipv6-special-registry/iana-ipv6-special-registry.xhtml
    Exit;
  end;
  IntIP := IP2Long(AIP);
  for I := low(RESERVED_IPv4) to high(RESERVED_IPv4) do
    if (IntIP >= IP2Long(RESERVED_IPv4[I][1])) and (IntIP <= IP2Long(RESERVED_IPv4[I][2])) then
      Exit(True);
end;

// function IP2Long(const AIP: string): UInt32;
// begin
// Result := IdGlobal.IPv4ToUInt32(AIP);
// end;

function B64Encode(const aValue: string): string; overload;
begin
  // Do not use TNetEncoding
  Result := TIdEncoderMIME.EncodeString(aValue);
end;

function B64Encode(const aValue: TBytes): string; overload;
begin
  // Do not use TNetEncoding
  Result := TIdEncoderMIME.EncodeBytes(TIdBytes(aValue));
end;

function B64Decode(const aValue: string): string;
begin
  // Do not use TNetEncoding
  Result := TIdDecoderMIME.DecodeString(aValue);
end;

function ByteToHex(AInByte: Byte): string;
const
  DIGITS: array [0 .. 15] of Char = '0123456789abcdef';
begin
  Result := DIGITS[AInByte shr 4] + DIGITS[AInByte and $0F];
end;

function BytesToHex(ABytes: TBytes): string;
var
  B: Byte;
begin
  Result := EmptyStr;
  for B in ABytes do
    Result := Result + ByteToHex(B);
end;

function BuildContentType(const aContentMediaType: string; const aContentCharSet: string): string;
var
  lContentMediaType: string;
begin
  lContentMediaType := aContentMediaType.ToLower.Trim.Replace(' ', '', [rfReplaceAll]);

  if lContentMediaType = '' then
  begin
    Result := '';
  end
  else
  begin
    if aContentCharSet.IsEmpty then
    begin
      Result := lContentMediaType;
    end
    else
      if lContentMediaType.StartsWith('text/') or lContentMediaType.StartsWith('application/')
    then
    begin
      Result := lContentMediaType + ';charset=' + aContentCharSet.ToLower;
    end
    else
    begin
      Result := lContentMediaType;
    end;
  end;
end;

procedure SplitContentMediaTypeAndCharset(const aContentType: string;
  var aContentMediaType: string; var aContentCharSet: string);
var
  lContentTypeValues: TArray<string>;
  I,J: Integer;
begin
  if not aContentType.IsEmpty then
  begin
    lContentTypeValues := aContentType.Split([';']);
    aContentCharSet := '';
    for I := low(lContentTypeValues) to high(lContentTypeValues) do
    begin
      if lContentTypeValues[I].Trim.StartsWith('charset', True) then
      begin
        aContentCharSet := lContentTypeValues[I].Trim.Split(['='])[1].Trim;
        for J := I + 1 to high(lContentTypeValues) do
        begin
          lContentTypeValues[J - 1] := lContentTypeValues[J];
        end;
        SetLength(lContentTypeValues, Length(lContentTypeValues) - 1);
        Break;
      end;
    end;
    aContentMediaType := string.Join(';', lContentTypeValues);
  end
  else
  begin
    aContentMediaType := '';
    aContentCharSet := '';
  end;
end;

{ EMVCException }

constructor EMVCException.Create(const AMsg: string);
begin
  inherited Create(AMsg);
  FHttpErrorCode := HTTP_STATUS.InternalServerError;
  FDetailedMessage := EmptyStr;
  FAppErrorCode := 0;
  SetLength(FErrorItems, 0);
end;

constructor EMVCException.Create(const AMsg, ADetailedMessage: string;
  const AAppErrorCode, AHTTPErrorCode: UInt16; const AErrorItems: TArray<String>);
begin
  Create(AMsg);
  CheckHTTPErrorCode(AHTTPErrorCode);
  FHttpErrorCode := AHTTPErrorCode;
  FAppErrorCode := AAppErrorCode;
  FDetailedMessage := ADetailedMessage;
  if AErrorItems <> nil then
  begin
    FErrorItems := AErrorItems;
  end;
end;

constructor EMVCException.Create(const AHTTPErrorCode: UInt16; const AMsg: string);
begin
  CheckHTTPErrorCode(AHTTPErrorCode);
  Create(AMsg);
  FHttpErrorCode := AHTTPErrorCode;
end;

procedure EMVCException.CheckHTTPErrorCode(const AHTTPErrorCode: UInt16);
begin
  if (AHTTPErrorCode div 100 = 0) or (AHTTPErrorCode div 100 > 5) then
  begin
    raise EMVCException.CreateFmt('Invalid HTTP_STATUS [%d]', [AHTTPErrorCode]);
  end;
end;

constructor EMVCException.Create(const AHTTPErrorCode: UInt16;
  const AAppErrorCode: Integer; const AMsg: string);
begin
  CheckHTTPErrorCode(AHTTPErrorCode);
  Create(AMsg);
  FHttpErrorCode := AHTTPErrorCode;
  FAppErrorCode := AAppErrorCode;
end;

constructor EMVCException.CreateFmt(const AHTTPErrorCode: UInt16;
  const AMsg: string; const AArgs: array of const);
begin
  inherited CreateFmt(AMsg, AArgs);
  FHttpErrorCode := AHTTPErrorCode;
  FDetailedMessage := EmptyStr;
  FAppErrorCode := 0;
end;

constructor EMVCException.CreateFmt(const AMsg: string; const AArgs: array of const);
begin
  CreateFmt(HTTP_STATUS.InternalServerError, AMsg, AArgs);
end;

{ TMVCViewDataObject }

constructor TMVCViewDataObject.Create;
begin
  inherited Create([]);
end;

{ TMVCCriticalSectionHelper }

procedure TMVCCriticalSectionHelper.DoWithLock(const AAction: TProc);
begin
  Self.Enter;
  try
    AAction();
  finally
    Self.Leave;
  end;
end;

function TMVCCriticalSectionHelper.DoWithLockTimeout(const AAction: TProc; const ATimeOut: UInt32)
  : TWaitResult;
begin
  Result := Self.WaitFor(ATimeOut);
  if (Result = TWaitResult.wrSignaled) then
    try
      AAction();
    finally
      Self.Leave;
    end;
end;

{ TMVCConfig }

procedure TMVCConfig.CheckNotFreezed;
begin
  if FFreezed then
  begin
    raise EMVCException.Create('Configuration in freezed - no more changes allowed') at ReturnAddress;
  end;
end;

constructor TMVCConfig.Create;
begin
  inherited Create;
  FConfig := TMVCStringDictionary.Create;
  FFreezed := False;
end;

destructor TMVCConfig.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
end;

procedure TMVCConfig.Freeze;
begin
  FFreezed := True;
end;

function TMVCConfig.GetValue(const AIndex: string): string;
begin
  if FConfig.ContainsKey(AIndex) then
    Result := FConfig.Items[AIndex]
  else
    raise EMVCConfigException.CreateFmt('Invalid config key [%s]', [AIndex]);
end;

function TMVCConfig.GetValueAsInt64(const AIndex: string): Int64;
begin
  Result := StrToInt64(Value[AIndex]);
end;

function TMVCConfig.Keys: TArray<string>;
begin
  Result := FConfig.Keys;
end;

procedure TMVCConfig.LoadFromFile(const AFileName: string);
var
  lConfigString: string;
  lStreamReader: TStreamReader;
  lSer: TMVCJsonDataObjectsSerializer;
begin
  lStreamReader := TStreamReader.Create(TFileStream.Create(AFileName,
    fmOpenRead or fmShareDenyWrite), TEncoding.ASCII);
  try
    lStreamReader.OwnStream;
    lConfigString := lStreamReader.ReadToEnd;
  finally
    lStreamReader.Free;
  end;

  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    FConfig.Clear;
    lSer.DeserializeObject(lConfigString, FConfig);
  finally
    lSer.Free;
  end;
end;

procedure TMVCConfig.SaveToFile(const AFileName: string);
begin
  TFile.WriteAllText(AFileName, ToString, TEncoding.ASCII);
end;

procedure TMVCConfig.SetValue(const AIndex, aValue: string);
begin
  CheckNotFreezed;
  FConfig.Add(AIndex, aValue);
end;

function TMVCConfig.ToString: string;
var
  lSer: TMVCJsonDataObjectsSerializer;
begin
  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    Result := lSer.SerializeObject(FConfig);
  finally
    lSer.Free;
  end;
end;

{ TMVCStringDictionary }

function TMVCStringDictionary.AddStrings(const Strings: TStrings): TMVCStringDictionary;
var
  I: Integer;
  lName: string;
begin
  for I := 0 to Strings.Count-1 do
  begin
    lName := Strings.Names[I];
    Add(lName, Strings.Values[lName]);
  end;
  Result := Self;
end;

function TMVCStringDictionary.Add(const Name, Value: string): TMVCStringDictionary;
begin
  fDict.AddOrSetValue(name, Value);
  Result := Self;
end;

procedure TMVCStringDictionary.Clear;
begin
  fDict.Clear;
end;

function TMVCStringDictionary.ContainsKey(const Key: string): Boolean;
begin
  Result := fDict.ContainsKey(Key);
end;

function TMVCStringDictionary.Count: Integer;
begin
  Result := fDict.Count;
end;

constructor TMVCStringDictionary.Create(const aKey, aValue: string);
begin
  Create;
  Add(aKey, aValue);
end;

constructor TMVCStringDictionary.Create;
begin
  inherited;
  fDict := TDictionary<string, string>.Create;
end;

destructor TMVCStringDictionary.Destroy;
begin
  fDict.Free;
  inherited;
end;

function TMVCStringDictionary.GetEnumerator: TDictionary<string, string>.TPairEnumerator;
begin
  Result := fDict.GetEnumerator;
end;

function TMVCStringDictionary.GetItems(const Key: string): string;
begin
  Result := '';
  fDict.TryGetValue(Key, Result);
end;

function TMVCStringDictionary.ToString: String;
var
  I: Integer;
  lValues: TArray<String>;
  lKey: string;
begin
  SetLength(lValues, Length(Keys));
  for I := 0 to Count - 1 do
  begin
    lKey := Keys[I];
    lValues[I] := lKey + '=' + Items[lKey];
  end;
  Result := String.Join(';', lValues);
end;

function TMVCStringDictionary.Keys: TArray<string>;
begin
  Result := fDict.Keys.ToArray;
end;

procedure TMVCStringDictionary.SetItems(const Key, Value: string);
begin
  fDict.AddOrSetValue(Key, Value);
end;

function TMVCStringDictionary.TryGetValue(const Name: string; out Value: Integer): Boolean;
var
  lTmp: string;
begin
  Result := TryGetValue(name, lTmp) and TryStrToInt(lTmp, Value);
end;

function TMVCStringDictionary.TryGetValue(const Name: string; out Value: string): Boolean;
begin
  Result := fDict.TryGetValue(name, Value);
end;

{ TMVCStringObjectDictionary }

procedure TMVCStringObjectDictionary<T>.Add(const Name: string; Value: T);
begin
  FMREWS.BeginWrite;
  try
    if not fDict.ContainsKey(name) then
      fDict.Add(name, Value);
  finally
    FMREWS.EndWrite;
  end;
end;

constructor TMVCStringObjectDictionary<T>.Create;
begin
  inherited;
  fDict := TObjectDictionary<string, T>.Create([doOwnsValues]);
  FMREWS := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TMVCStringObjectDictionary<T>.Destroy;
begin
  fDict.Free;
  FMREWS.Free;
  inherited;
end;

function TMVCStringObjectDictionary<T>.TryGetValue(const Name: string; out Value: T): Boolean;
begin
  FMREWS.BeginRead;
  try
    Result := fDict.TryGetValue(name, Value);
  finally
    FMREWS.EndRead;
  end;
end;

type
  TURLSafeEncode = class(TIdEncoder3to4)
  protected
    procedure InitComponent; override;
  public

  end;

  TURLSafeDecode = class(TIdDecoder4to3)
  protected
    class var GSafeBaseBase64DecodeTable: TIdDecodeTable;
    procedure InitComponent; override;
  public

  end;

const
  GURLSafeBase64CodeTable
    : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';
  { Do not Localize }

procedure TURLSafeEncode.InitComponent;
begin
  inherited;
  FCodingTable := ToBytes(GURLSafeBase64CodeTable);
  FFillChar := '='; { Do not Localize }
end;

procedure TURLSafeDecode.InitComponent;
begin
  inherited;
  FDecodeTable := GSafeBaseBase64DecodeTable;
  FCodingTable := ToBytes(GURLSafeBase64CodeTable);
  FFillChar := '='; { Do not Localize }
end;

function URLSafeB64encode(const Value: string; IncludePadding: Boolean; AByteEncoding: IIdTextEncoding = nil)
  : string; overload;
begin
  if IncludePadding then
    Result := TURLSafeEncode.EncodeString(Value, AByteEncoding)
  else
    Result := TURLSafeEncode.EncodeString(Value, AByteEncoding).Replace('=', '', [rfReplaceAll]);
end;

/// <summary>
/// Remove "trimmed" character from the end of the string passed as parameter
/// </summary>
/// <param name="Value">Original string</param>
/// <param name="TrimmedChar">Character to remove</param>
/// <returns>Resulting string</returns>
function RTrim(const Value: string; TrimmedChar: Char): string;
var
  Strlen: Integer;
begin
  Strlen := Length(Value);
  while (Strlen > 0) and (Value[Strlen] = TrimmedChar) do
    dec(Strlen);
  Result := copy(Value, 1, Strlen)
end;

function URLSafeB64encode(const Value: TBytes; IncludePadding: Boolean): string; overload;
begin

  if IncludePadding then
    Result := TURLSafeEncode.EncodeBytes(TIdBytes(Value))
  else
    Result := RTrim(TURLSafeEncode.EncodeBytes(TIdBytes(Value)), '=');
end;

function URLSafeB64Decode(const Value: string; AByteEncoding: IIdTextEncoding = nil): string;
begin
  // SGR 2017-07-03 : b64url might not include padding. Need to add it before decoding
  case Length(Value) mod 4 of
    0:
      begin
        Result := TURLSafeDecode.DecodeString(Value, AByteEncoding);
      end;
    2:
      Result := TURLSafeDecode.DecodeString(Value + '==', AByteEncoding);
    3:
      Result := TURLSafeDecode.DecodeString(Value + '=', AByteEncoding);
  else
    raise EExternalException.Create('Illegal base64url length');
  end;
end;

{ TMultiReadExclusiveWriteSynchronizerHelper }

procedure TMultiReadExclusiveWriteSynchronizerHelper.DoWithReadLock(const AAction: TProc);
begin
  Self.BeginRead;
  try
    AAction();
  finally
    Self.EndRead;
  end;
end;

procedure TMultiReadExclusiveWriteSynchronizerHelper.DoWithWriteLock(const AAction: TProc);
begin
  Self.BeginWrite;
  try
    AAction();
  finally
    Self.EndWrite;
  end;
end;

{ TMVCViewDataSet }

constructor TMVCViewDataSet.Create;
begin
  inherited Create([]);
end;

{ TMVCStreamHelper }

procedure TMVCStreamHelper.WriteUTF8(const AString: string);
var
  UFTStr: UTF8String;
begin
  UFTStr := UTF8String(AString);
  if UFTStr <> '' then
  begin
    Self.WriteBuffer(UFTStr[low(UFTStr)], Length(UFTStr));
  end;
end;

{ TMVCDecorator }

function TMVCLinks.AddRefLink: IMVCLinkItem;
begin
  if not Assigned(fData) then
  begin
    fData := TMVCStringDictionaryList.Create;
  end;

  Result := TMVCDecoratorObject.Create(fData[fData.Add(TMVCStringDictionary.Create)]);
end;

function TMVCLinks.Clear: IMVCLinks;
begin
  if Assigned(fData) then
  begin
    fData.Clear;
  end;
  Result := Self;
end;

constructor TMVCLinks.Create;
begin
  inherited Create;
  fData := nil;
end;

function TMVCLinks.LinksData: TMVCStringDictionaryList;
begin
  Result := fData;
end;

destructor TMVCLinks.Destroy;
begin
  FreeAndNil(fData);
  inherited;
end;

{ TMVCDecoratorObject }

function TMVCDecoratorObject.Add(const PropName,
  PropValue: string): IMVCLinkItem;
begin
  fData.Items[PropName] := PropValue;
  Result := Self;
end;

constructor TMVCDecoratorObject.Create(const aData: TMVCStringDictionary);
begin
  inherited Create;
  fData := aData;
end;

{ TMVCNamedPairList }

constructor TMVCStringDictionaryList.Create;
begin
  inherited Create(True);
end;

procedure Base64StringToFile(const aBase64String, AFileName: string; const aOverwrite: Boolean = False);
var
  lSS: TStringStream;
  lFile: TFileStream;
begin
  lSS := TStringStream.Create;
  try
    lSS.WriteString(aBase64String);
    lSS.Position := 0;
    if aOverwrite then
    begin
      if TFile.Exists(AFileName) then
      begin
        TFile.Delete(AFileName);
      end;
    end;

    lFile := TFileStream.Create(AFileName, fmCreate);
    try
      TMVCSerializerHelper.DecodeStream(lSS, lFile);
    finally
      lFile.Free;
    end;
  finally
    lSS.Free;
  end;
end;

function FileToBase64String(const FileName: string): string;
var
  lTemplateFileB64: TStringStream;
  lTemplateFile: TFileStream;
begin
  lTemplateFileB64 := TStringStream.Create;
  try
    lTemplateFile := TFileStream.Create(FileName, fmOpenRead);
    try
      TMVCSerializerHelper.EncodeStream(lTemplateFile, lTemplateFileB64);
    finally
      lTemplateFile.Free;
    end;
    Result := lTemplateFileB64.DataString;
  finally
    lTemplateFileB64.Free;
  end;
end;

class procedure TMVCParseAuthentication.OnParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: string;
  var VUsername,
  VPassword: string; var VHandled: Boolean);
begin
  VHandled := SameText(LowerCase(AAuthType), 'bearer');
end;

{ TMVCGuidHelper }

class function TMVCGuidHelper.GUIDToStringEx(const aGuid: TGUID): string;
begin
  Result := aGuid.ToString.Substring(1, 36).ToLower; { UUID specification RFC 4122 - https://www.ietf.org/rfc/rfc4122.txt }
end;

class function TMVCGuidHelper.StringToGUIDEx(const aGuidStr: string): TGUID;
var
  lGuidStr: string;
begin
  case aGuidStr.Length of
    32: { string uuid without braces and dashes: ae502abe430bb23a28782d18d6a6e465 }
      begin
        lGuidStr := Format('{%s-%s-%s-%s-%s}', [aGuidStr.Substring(0, 8), aGuidStr.Substring(8, 4),
          aGuidStr.Substring(12, 4), aGuidStr.Substring(16, 4), aGuidStr.Substring(20, 12)]);
      end;
    36: { string uuid without braces: ae502abe-430b-b23a-2878-2d18d6a6e465 }
      begin
        lGuidStr := Format('{%s}', [aGuidStr])
      end
  else
    begin
      lGuidStr := aGuidStr;
    end;
  end;

  Result := StringToGUID(lGuidStr);
end;

function CamelCase(const Value: string; const MakeFirstUpperToo: Boolean): string;
var
  I: Integer;
  lNextUpCase: Boolean;
  lSB: TStringBuilder;
  C: Char;
  lIsLowerCase: Boolean;
  lIsUpperCase, lPreviousWasUpperCase: Boolean;
  lIsAlpha: Boolean;
  lIsNumber: Boolean;
begin
  {TODO -oDanieleT -cGeneral : Make this function faster!}
  lNextUpCase := MakeFirstUpperToo;
  lPreviousWasUpperCase := True;
  lSB := TStringBuilder.Create;
  try
    for I := 0 to Length(Value) - 1 do
    begin
      C := Value.Chars[I];
      lIsLowerCase := CharInSet(C, ['a' .. 'z']);
      lIsUpperCase := CharInSet(C, ['A' .. 'Z']);
      lIsNumber := CharInSet(C, ['0' .. '9']);
      lIsAlpha := lIsLowerCase or lIsUpperCase;
      if not (lIsAlpha or lIsNumber) then
      begin
        lNextUpCase := True;
        lPreviousWasUpperCase := False;
        Continue;
      end
      else
      begin
        if lNextUpCase then
        begin
          lNextUpCase := False;
          lSB.Append(UpCase(C));
        end
        else
        begin
          if lPreviousWasUpperCase then
          begin
            lSB.Append(LowerCase(C));
          end
          else
          begin
            lSB.Append(C);
          end;
        end;
      end;
      lPreviousWasUpperCase := lIsUpperCase;
      if lIsNumber then
      begin
        lNextUpCase := True;
      end;
    end;
    Result := lSB.ToString;
  finally
    lSB.Free;
  end;
end;

function SnakeCase(const Value: string): string;
var
  I: Integer;
  lSB: TStringBuilder;
  C: Char;
  lIsUpperCase: Boolean;
  lIsLowerCase: Boolean;
  lLastWasLowercase: Boolean;
  lIsNumber: Boolean;
  lLastWasUnderscore: Boolean;
  lIsUnderscore: Boolean;
  lLastWasNumber: Boolean;
  lNextUnderscore: Boolean;
  lLengthValue: Integer;
begin
  lLastWasLowercase := False;
  lLastWasUnderscore := False;
  lLastWasNumber := False;
  lNextUnderscore := False;
  lLengthValue := Length(Value);
  lSB := TStringBuilder.Create;
  try
    for I := 0 to lLengthValue - 1 do
    begin
      C := Value.Chars[I];
      lIsUpperCase := CharInSet(C, ['A' .. 'Z']);
      lIsLowerCase := CharInSet(C, ['a' .. 'z']);
      lIsNumber := CharInSet(C, ['0' .. '9']);
      lIsUnderscore := C = '_';

      if not (lIsUpperCase or lIsLowerCase or lIsNumber or lIsUnderscore) then
      begin
        lNextUnderscore := True;
        Continue;
      end
      else
      begin
        if (I > 0) and (not lLastWasUnderscore) and
          (lNextUnderscore or
          (lIsUpperCase and (lLastWasLowercase or lLastWasNumber)) or
          (lIsLowerCase and lLastWasNumber) or
          (lIsNumber and (not lLastWasNumber)) or
          (lIsUpperCase and (not lLastWasLowercase) and ((I + 1) <= (lLengthValue - 1)) and
          CharInSet(Value.Chars[I + 1], ['a' .. 'z']))) then
        begin
          lSB.Append('_');
        end;

        if not (lLastWasUnderscore and lIsUnderscore) then
        begin
          lSB.Append(LowerCase(C));
        end;
        lLastWasUnderscore := lIsUnderscore or lNextUnderscore;
        lLastWasLowercase := lIsLowerCase;
        lLastWasNumber := lIsNumber;
        lNextUnderscore := False;
      end;
    end;
    Result := lSB.ToString;
  finally
    lSB.Free;
  end;
end;

function StrToJSONObject(const aString: String; ARaiseExceptionOnError: Boolean = False): TJsonObject;
begin
  Result := MVCFramework.Utils.StrToJSONObject(aString, ARaiseExceptionOnError);
end;

function StrToJSONArray(const aString: String; ARaiseExceptionOnError: Boolean = False): TJsonArray;
begin
  Result := MVCFramework.Utils.StrToJSONArray(aString, ARaiseExceptionOnError);
end;

function WrapAsList(const AObject: TObject; AOwnsObject: Boolean = False): IMVCList;
begin
  Result := MVCFramework.Utils.WrapAsList(AObject, AOwnsObject);
end;

{ TMVCStringHelper }

class function TMVCStringHelper.StartsText(const ASubText, AText: string): Boolean;
begin
  if ASubText = EmptyStr then
    Result := True
  else
  begin
    if (AText.Length >= ASubText.Length) then
      Result := AnsiStrLIComp(PChar(ASubText), PChar(AText), ASubText.Length) = 0
    else
      Result := False;
  end;
end;

class function TMVCStringHelper.StartsWith(const ASubText, AText: string; AIgnoreCase: Boolean): Boolean;
begin
  if AIgnoreCase then
    Result := StartsText(ASubText, AText)
  else
  if ASubText = EmptyStr then
    Result := True
  else
  begin
    if (AText.Length >= ASubText.Length) then
      Result := CompareStr(AText.Substring(0, ASubText.Length), ASubText) = 0
    else
      Result := False;
  end;
end;


initialization

gLock := TObject.Create;

// SGR 2017-07-03 : Initialize decoding table for URLSafe Gb64 encoding
TURLSafeDecode.ConstructDecodeTable(GURLSafeBase64CodeTable,
  TURLSafeDecode.GSafeBaseBase64DecodeTable);

GlobalAppExe := ExtractFileName(GetModuleName(HInstance));
GlobalAppName := ChangeFileExt(GlobalAppExe, EmptyStr);
GlobalAppPath := IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleName(HInstance)));

finalization

FreeAndNil(gLock);

end.
