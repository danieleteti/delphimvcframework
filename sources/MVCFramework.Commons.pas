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

unit MVCFramework.Commons;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.SyncObjs,
  System.IOUtils,
  System.Generics.Collections,
  MVCFramework.TypesAliases,
  IdGlobal,
  IdCoderMIME;

{$I dmvcframeworkbuildconsts.inc}

type

  TMVCHTTPMethodType = (httpGET, httpPOST, httpPUT, httpDELETE, httpHEAD, httpOPTIONS, httpPATCH, httpTRACE);

  TMVCHTTPMethods = set of TMVCHTTPMethodType;

  TMVCMediaType = record
  public const
    APPLICATION_ATOM_XML = 'application/atom+xml';
    APPLICATION_FORM_URLENCODED = 'application/x-www-form-urlencoded';
    APPLICATION_JSON = 'application/json';
    APPLICATION_OCTET_STREAM = 'application/octet-stream';
    APPLICATION_SVG_XML = 'application/svg+xml';
    APPLICATION_XHTML_XML = 'application/xhtml+xml';
    APPLICATION_XML = 'application/xml';
    APPLICATION_OCTETSTREAM = 'application/octet-stream';
    MEDIA_TYPE_WILDCARD = '*';
    MULTIPART_FORM_DATA = 'multipart/form-data';
    TEXT_HTML = 'text/html';
    TEXT_PLAIN = 'text/plain';
    TEXT_XML = 'text/xml';
    TEXT_CSS = 'text/css';
    TEXT_JAVASCRIPT = 'text/javascript';
    TEXT_CACHEMANIFEST = 'text/cache-manifest';
    TEXT_EVENTSTREAM = 'text/event-stream';
    TEXT_CSV = 'text/csv';
    IMAGE_JPEG = 'image/jpeg';
    IMAGE_PNG = 'image/x-png';
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
  end;

  TMVCConfigKey = record
  public const
    SessionTimeout = 'sessiontimeout';
    DocumentRoot = 'document_root';
    ViewPath = 'view_path';
    DefaultContentType = 'default_content_type';
    DefaultContentCharset = 'default_content_charset';
    DefaultViewFileExtension = 'default_view_file_extension';
    //ISAPIPath = 'isapi_path';
    PathPrefix = 'pathprefix';
    StompServer = 'stompserver';
    StompServerPort = 'stompserverport';
    StompUsername = 'stompusername';
    StompPassword = 'stomppassword';
    AllowUnhandledAction = 'allow_unhandled_action';
    ServerName = 'server_name';
    ExposeServerSignature = 'server_signature';
    IndexDocument = 'index_document';
    SessionType = 'session_type';
    FallbackResource = 'fallback_resource';
  end;

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
    PreconditionFailed = 412;
    RequestEntityTooLarge = 413;
    RequestURITooLong = 414;
    UnsupportedMediaType = 415;
    RequestedRangeNotSatisfiable = 416;
    ExpectationFailed = 417;
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
  end;

  EMVCException = class(Exception)
  private
    FHttpErrorCode: UInt16;
    FAppErrorCode: UInt16;
    FDetailedMessage: string;
  protected
    { protected declarations }
  public
    constructor Create(const AMsg: string); overload; virtual;
    constructor Create(const AMsg: string; const ADetailedMessage: string; const AAppErrorCode: UInt16; const AHttpErrorCode: UInt16 = HTTP_STATUS.InternalServerError); overload; virtual;
    constructor Create(const AHttpErrorCode: UInt16; const AMsg: string); overload; virtual;
    constructor CreateFmt(const AMsg: string; const AArgs: array of const); reintroduce;

    property HttpErrorCode: UInt16 read FHttpErrorCode;
    property DetailedMessage: string read FDetailedMessage write FDetailedMessage;
    property ApplicationErrorCode: UInt16 read FAppErrorCode write FAppErrorCode;
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

  TMVCRequestParamsTable = class(TDictionary<string, string>)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  end;

  TMVCStringDictionary = class
  strict protected
    FDict: TDictionary<string, string>;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function AddProperty(const Name, Value: string): TMVCStringDictionary;
    function TryGetValue(const Name: string; out Value: string): Boolean;
    function Count: Integer;
    function GetEnumerator: TDictionary<string, string>.TPairEnumerator;
  end;

  { This type is thread safe }
  TMVCStringObjectDictionary<T: class> = class
  private
    FMREWS: TMultiReadExclusiveWriteSynchronizer;
  protected
    FDict: TObjectDictionary<string, T>;
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

  TMVCCriticalSectionHelper = class helper
    for TCriticalSection
  public
    procedure DoWithLock(const AAction: TProc);
    function DoWithLockTimeout(const AAction: TProc; const ATimeOut: UInt32): TWaitResult;
  end;

  TMVCConfig = class sealed
  private
    FConfig: TDictionary<string, string>;
    function GetValue(const AIndex: string): string;
    function GetValueAsInt64(const AIndex: string): Int64;
    procedure SetValue(const AIndex: string; const AValue: string);
  protected
    { protected declarations }
  public
    constructor Create;
    destructor Destroy; override;

    function Keys: TArray<string>;
    function ToString: string; override;
    procedure SaveToFile(const AFileName: string);
    procedure LoadFromFile(const AFileName: string);

    property Value[const AIndex: string]: string read GetValue write SetValue; default;
    property AsInt64[const AIndex: string]: Int64 read GetValueAsInt64;
  end;

  IMVCAuthenticationHandler = interface
    ['{19B580EA-8A47-4364-A302-EEF3C6207A9F}']
    procedure OnRequest(const AControllerQualifiedClassName, AActionName: string; var AAuthenticationRequired: Boolean);
    procedure OnAuthentication(const AUserName, APassword: string; AUserRoles: TList<string>; var AIsValid: Boolean; const ASessionData: TDictionary<string, string>);
    procedure OnAuthorization(AUserRoles: TList<string>; const AControllerQualifiedClassName: string; const AActionName: string; var AIsAuthorized: Boolean);
  end;

  {$SCOPEDENUMS ON}

function AppPath: string;
function IsReservedOrPrivateIP(const AIP: string): Boolean;
function IP2Long(const AIP: string): UInt32;

function B64Encode(const AValue: string): string; overload;
function B64Encode(const AValue: TBytes): string; overload;
function B64Decode(const AValue: string): string;

function URLSafeB64encode(const Value: string; IncludePadding: Boolean): String;  overload;
function URLSafeB64encode(const Value: TBytes; IncludePadding: Boolean): String;  overload;
function URLSafeB64Decode(const Value: string): String;

function ByteToHex(AInByte: Byte): string;
function BytesToHex(ABytes: TBytes): string;

var
  Lock: TObject;

implementation

uses
  IdCoder3to4;

const
  RESERVED_IPS: array [1 .. 11] of array [1 .. 2] of string =
    (('0.0.0.0', '0.255.255.255'), ('10.0.0.0', '10.255.255.255'),
    ('127.0.0.0', '127.255.255.255'), ('169.254.0.0', '169.254.255.255'),
    ('172.16.0.0', '172.31.255.255'), ('192.0.2.0', '192.0.2.255'),
    ('192.88.99.0', '192.88.99.255'), ('192.168.0.0', '192.168.255.255'),
    ('198.18.0.0', '198.19.255.255'), ('224.0.0.0', '239.255.255.255'),
    ('240.0.0.0', '255.255.255.255'));

var
  GlobalAppName, GlobalAppPath, GlobalAppExe: string;

function AppPath: string;
begin
  Result := GlobalAppPath;
end;

function IsReservedOrPrivateIP(const AIP: string): Boolean;
var
  I: Integer;
  IntIP: Cardinal;
begin
  Result := False;
  IntIP := IP2Long(AIP);
  for I := low(RESERVED_IPS) to high(RESERVED_IPS) do
    if (IntIP >= IP2Long(RESERVED_IPS[I][1])) and (IntIP <= IP2Long(RESERVED_IPS[I][2])) then
      Exit(True)
end;

function IP2Long(const AIP: string): UInt32;
begin
  Result := IdGlobal.IPv4ToUInt32(AIP);
end;

function B64Encode(const AValue: string): string; overload;
begin
  //Do not use TNetEncoding
  Result := TIdEncoderMIME.EncodeString(AValue);
end;

function B64Encode(const AValue: TBytes): string; overload;
begin
  //Do not use TNetEncoding
  Result := TIdEncoderMIME.EncodeBytes(TIdBytes(AValue));
end;

function B64Decode(const AValue: string): string;
begin
  //Do not use TNetEncoding
  Result := TIdDecoderMIME.DecodeString(AValue);
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

{ EMVCException }

constructor EMVCException.Create(const AMsg: string);
begin
  inherited Create(AMsg);
  FHttpErrorCode := HTTP_STATUS.InternalServerError;
  FDetailedMessage := EmptyStr;
  FAppErrorCode := 0;
end;

constructor EMVCException.Create(const AMsg, ADetailedMessage: string; const AAppErrorCode, AHttpErrorCode: UInt16);
begin
  Create(AMsg);
  FHttpErrorCode := AHttpErrorCode;
  FAppErrorCode := AAppErrorCode;
  FDetailedMessage := ADetailedMessage;
end;

constructor EMVCException.Create(const AHttpErrorCode: UInt16; const AMsg: string);
begin
  Create(AMsg);
  FHttpErrorCode := AHttpErrorCode;
end;

constructor EMVCException.CreateFmt(const AMsg: string; const AArgs: array of const);
begin
  inherited CreateFmt(AMsg, AArgs);
  FHttpErrorCode := HTTP_STATUS.InternalServerError;
  FDetailedMessage := EmptyStr;
  FAppErrorCode := 0;
end;

{ TMVCViewDataObject }

constructor TMVCViewDataObject.Create;
begin
  inherited Create([doOwnsValues]);
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

function TMVCCriticalSectionHelper.DoWithLockTimeout(const AAction: TProc; const ATimeOut: UInt32): TWaitResult;
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

constructor TMVCConfig.Create;
begin
  inherited Create;
  FConfig := TDictionary<string, string>.Create;
end;

destructor TMVCConfig.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
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
  Result := FConfig.Keys.ToArray;
end;

procedure TMVCConfig.LoadFromFile(const AFileName: string);
var
  S: string;
  Jo: TJSONObject;
  P: TJSONPair;
  Jv: TJSONValue;
  I: Integer;
begin
  { TODO -oEzequiel -cRefactoring : Replace for custom serializers }
  S := TFile.ReadAllText(AFileName);
  Jv := TJSONObject.ParseJSONValue(S);
  if Assigned(Jv) then
  begin
    if Jv is TJSONObject then
    begin
      Jo := TJSONObject(Jv);
      for I := 0 to Jo.Count - 1 do
      begin
        P := Jo.Pairs[I];
        FConfig.AddOrSetValue(P.JsonString.Value, P.JsonValue.Value);
      end
    end
    else
      raise EMVCConfigException.Create('DMVCFramework configuration file [' + AFileName + '] does not contain a valid JSONObject');
  end
  else
    raise EMVCConfigException.Create('Cannot load DMVCFramework configuration file [' + AFileName + ']');
end;

procedure TMVCConfig.SaveToFile(const AFileName: string);
begin
  TFile.WriteAllText(AFileName, ToString, TEncoding.ASCII);
end;

procedure TMVCConfig.SetValue(const AIndex, AValue: string);
begin
  FConfig.AddOrSetValue(AIndex, AValue);
end;

function TMVCConfig.ToString: string;
var
  S: string;
  Jo: TJSONObject;
begin
  { TODO -oEzequiel -cRefactoring : Replace for custom serializers }
  Jo := TJSONObject.Create;
  try
    for S in FConfig.Keys do
      Jo.AddPair(S, FConfig[S]);
    Result := Jo.ToString;
  finally
    Jo.Free;
  end;
end;

{ TMVCStringDictionary }

function TMVCStringDictionary.AddProperty(const Name,
  Value: string): TMVCStringDictionary;
begin
  FDict.AddOrSetValue(name, Value);
  Result := Self;
end;

procedure TMVCStringDictionary.Clear;
begin
  FDict.Clear;
end;

function TMVCStringDictionary.Count: Integer;
begin
  Result := FDict.Count;
end;

constructor TMVCStringDictionary.Create;
begin
  inherited;
  FDict := TDictionary<string, string>.Create;
end;

destructor TMVCStringDictionary.Destroy;
begin
  FDict.Free;
  inherited;
end;

function TMVCStringDictionary.GetEnumerator: TDictionary<string, string>.TPairEnumerator;
begin
  Result := FDict.GetEnumerator;
end;

function TMVCStringDictionary.TryGetValue(const Name: string;
  out Value: string): Boolean;
begin
  Result := FDict.TryGetValue(name, Value);
end;

{ TMVCStringObjectDictionary }

procedure TMVCStringObjectDictionary<T>.Add(const Name: string; Value: T);
begin
  FMREWS.BeginWrite;
  try
    if not FDict.ContainsKey(name) then
      FDict.Add(name, Value);
  finally
    FMREWS.EndWrite;
  end;
end;

constructor TMVCStringObjectDictionary<T>.Create;
begin
  inherited;
  FDict := TObjectDictionary<string, T>.Create([doOwnsValues]);
  FMREWS := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TMVCStringObjectDictionary<T>.Destroy;
begin
  FDict.Free;
  FMREWS.Free;
  inherited;
end;

function TMVCStringObjectDictionary<T>.TryGetValue(const Name: string; out Value: T): Boolean;
begin
  FMREWS.BeginRead;
  try
    Result := FDict.TryGetValue(name, Value);
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
  GURLSafeBase64CodeTable: string =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';    {Do not Localize}


procedure TURLSafeEncode.InitComponent;
begin
  inherited;
  FCodingTable := ToBytes(GURLSafeBase64CodeTable);
  FFillChar := '=';  {Do not Localize}
end;

procedure TURLSafeDecode.InitComponent;
begin
  inherited;
  FDecodeTable := GSafeBaseBase64DecodeTable;
  FCodingTable := ToBytes(GURLSafeBase64CodeTable);
  FFillChar := '=';  {Do not Localize}
end;

function URLSafeB64encode(const Value: string; IncludePadding: Boolean): String; overload;
begin
  if IncludePadding then
    Result := TURLSafeEncode.EncodeString(Value)
  else
    Result := TURLSafeEncode.EncodeString(Value).Replace('=', '', [rfReplaceAll]);
end;

/// <summary>
///   Remove "trimmed" character from the end of the string passed as parameter
/// </summary>
/// <param name="Value">Original string</param>
/// <param name="TrimmedChar">Character to remove</param>
/// <returns>Resulting string</returns>
function RTrim(const Value: string; TrimmedChar: char): string;
var
  Strlen: Integer;
begin
  Strlen := Length(Value);
  while (Strlen>0) and (Value[Strlen]=TrimmedChar) do
    dec(StrLen);
  result := copy(value, 1, StrLen)
end;

function URLSafeB64encode(const Value: TBytes; IncludePadding: Boolean): String;  overload;
begin

  if IncludePadding then
    Result := TURLSafeEncode.EncodeBytes(TIdBytes(Value))
  else
    Result := RTrim(TURLSafeEncode.EncodeBytes(TIdBytes(Value)), '=');
end;

function URLSafeB64Decode(const Value: string): String;
begin
  // SGR 2017-07-03 : b64url might not include padding. Need to add it before decoding
  case Length(value) mod 4 of
    0:
    begin
      Result := TURLSafeDecode.DecodeString(Value);
    end;
    2:
      Result := TURLSafeDecode.DecodeString(Value + '==');
    3:
      Result := TURLSafeDecode.DecodeString(Value + '=');
  else
    raise EExternalException.Create('Illegal base64url length');
  end;
end;

initialization

Lock := TObject.Create;

// SGR 2017-07-03 : Initialize decoding table for URLSafe Gb64 encoding
TURLSafeDecode.ConstructDecodeTable(GURLSafeBase64CodeTable, TURLSafeDecode.GSafeBaseBase64DecodeTable);

GlobalAppExe := ExtractFileName(GetModuleName(HInstance));
GlobalAppName := ChangeFileExt(GlobalAppExe, EmptyStr);
GlobalAppPath := IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleName(HInstance)));

finalization

FreeAndNil(Lock);

end.
