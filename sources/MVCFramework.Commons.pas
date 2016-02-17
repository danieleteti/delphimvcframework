unit MVCFramework.Commons;

interface

uses
	System.SysUtils
	, Generics.Collections
{$IF CompilerVersion < 27 }
		, Data.DBXJSON
{$ELSE}
		, System.JSON
{$IFEND}
		, System.Generics.Collections;

type
	TMVCMimeType = class sealed
	public const
		APPLICATION_JSON = 'application/json';
		TEXT_HTML = 'text/html';
		TEXT_PLAIN = 'text/plain';
		TEXT_XML = 'text/xml';
		TEXT_CSS = 'text/css';
		TEXT_JAVASCRIPT = 'text/javascript';
		IMAGE_JPEG = 'image/jpeg';
		IMAGE_PNG = 'image/x-png';
		TEXT_CACHEMANIFEST = 'text/cache-manifest';
		APPLICATION_OCTETSTREAM = 'application/octet-stream';
		TEXT_EVENTSTREAM = 'text/event-stream';
	end;

	TMVCConstants = class sealed
	public const
		SESSION_TOKEN_NAME = 'dtsessionid';
		DEFAULT_CONTENT_CHARSET = 'UTF-8';
		DEFAULT_CONTENT_TYPE = TMVCMimeType.APPLICATION_JSON;
		CURRENT_USER_SESSION_KEY = '__DMVC_CURRENT_USER__';
		LAST_AUTHORIZATION_HEADER_VALUE = '__DMVC_LAST_AUTHORIZATION_HEADER_VALUE_';
	end;

	EMVCException = class(Exception)
	private
		FHTTPErrorCode: UInt16;
		FApplicationErrorCode: UInt16;
		procedure SetDetailedMessage(const Value: string);

	strict protected
		FDetailedMessage: string;

	public
		constructor Create(const Msg: string); overload; virtual;
		constructor Create(const Msg: string; const DetailedMessage: string;
			const ApplicationErrorCode: UInt16; const HTTPErrorCode: UInt16 = 500);
			overload; virtual;
		constructor CreateFmt(const Msg: string; const Args: array of const);
		property HTTPErrorCode: UInt16 read FHTTPErrorCode;
		property DetailedMessage: string read FDetailedMessage
			write SetDetailedMessage;
		property ApplicationErrorCode: UInt16 read FApplicationErrorCode
			write FApplicationErrorCode;
	end;

	EMVCSessionExpiredException = class(EMVCException)

	end;

	EMVCConfigException = class(EMVCException)

	end;

	EMVCFrameworkView = class(EMVCException)

	end;

	TMVCRequestParamsTable = class(TDictionary<string, string>)

	end;

	TMVCDataObjects = class(TObjectDictionary<string, TJSONValue>)
		constructor Create;
	end;

	TMVCConfig = class sealed
	private
		FConfig: TDictionary<string, string>;
		function GetValue(AIndex: string): string;
		procedure SetValue(AIndex: string; const Value: string);
		function GetValueAsInt64(AIndex: string): Int64;

	public
		constructor Create;
		destructor Destroy; override;
		function Keys: TArray<string>;
		property Value[AIndex: string]: string read GetValue
			write SetValue; default;
		property AsInt64[AIndex: string]: Int64 read GetValueAsInt64;
		function ToString: string; override;
		procedure SaveToFile(const AFileName: string);
		procedure LoadFromFile(const AFileName: string);
	end;

	IMVCAuthenticationHandler = interface
		['{19B580EA-8A47-4364-A302-EEF3C6207A9F}']
		procedure OnRequest(const ControllerQualifiedClassName, ActionName: string;
			var AuthenticationRequired: Boolean);
		procedure OnAuthentication(const UserName, Password: string;
			UserRoles: TList<string>;
			var IsValid: Boolean);
		procedure OnAuthorization(UserRoles: TList<string>;
			const ControllerQualifiedClassName: string;
			const ActionName: string; var IsAuthorized: Boolean);

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

{$SCOPEDENUMS ON}


type
	THttpMethod = (GET, POST, PUT, DELETE, HEAD);

function AppPath: string;
function IsReservedOrPrivateIP(const IP: string): Boolean;
function IP2Long(IP: string): UInt32;

var
	Lock: TObject;

implementation

{$WARN SYMBOL_DEPRECATED OFF}


uses
	System.IOUtils,
	idGlobal,
	System.StrUtils,
	uGlobalVars;

const
	ReservedIPs: array [1 .. 11] of array [1 .. 2] of string =
		(('0.0.0.0', '0.255.255.255'),
		('10.0.0.0', '10.255.255.255'), ('127.0.0.0', '127.255.255.255'),
		('169.254.0.0', '169.254.255.255'), ('172.16.0.0', '172.31.255.255'),
		('192.0.2.0', '192.0.2.255'), ('192.88.99.0', '192.88.99.255'),
		('192.168.0.0', '192.168.255.255'), ('198.18.0.0', '198.19.255.255'),
		('224.0.0.0', '239.255.255.255'), ('240.0.0.0', '255.255.255.255'));

function IP2Long(IP: string): UInt32;
begin
	Result := idGlobal.IPv4ToDWord(IP);
end;

function IsReservedOrPrivateIP(const IP: string): Boolean;
var
	i: Integer;
	IntIP: Cardinal;
begin
	Result := False;
	IntIP := IP2Long(IP);
	for i := low(ReservedIPs) to high(ReservedIPs) do
	begin
		if (IntIP >= IP2Long(ReservedIPs[i][1])) and
			(IntIP <= IP2Long(ReservedIPs[i][2])) then
		begin
			Exit(True)
		end;
	end;
end;

function AppPath: string;
begin
	Result := gAppPath; // TPath.GetDirectoryName(GetModuleName(HInstance));
end;

{ TMVCDataObjects }

constructor TMVCDataObjects.Create;
begin
	inherited Create([doOwnsValues]);
end;

{ TMVCConfig }

constructor TMVCConfig.Create;
begin
	inherited;
	FConfig := TDictionary<string, string>.Create;
end;

destructor TMVCConfig.Destroy;
begin
	FConfig.Free;
	inherited;
end;

function TMVCConfig.GetValue(AIndex: string): string;
begin
	if FConfig.ContainsKey(AIndex) then
		Result := FConfig.Items[AIndex]
	else
		raise EMVCConfigException.CreateFmt('Invalid config key [%s]', [AIndex]);
end;

function TMVCConfig.GetValueAsInt64(AIndex: string): Int64;
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
	jobj: TJSONObject;
	p: TJSONPair;
	JSON: TJSONValue;
	i: Integer;
begin
	S := TFile.ReadAllText(AFileName);
	JSON := TJSONObject.ParseJSONValue(S);
	if Assigned(JSON) then
	begin
		if JSON is TJSONObject then
		begin
			jobj := TJSONObject(JSON);
			for i := 0 to jobj.Size - 1 do
			begin
				p := jobj.GET(i);
				FConfig.AddOrSetValue(p.JsonString.Value, p.JsonValue.Value);
			end
		end
		else
			raise EMVCConfigException.Create('DMVCFramework configuration file [' +
				AFileName +
				'] does not contain a valid JSONObject');
	end
	else
		raise EMVCConfigException.Create
			('Cannot load DMVCFramework configuration file [' +
			AFileName + ']');
end;

procedure TMVCConfig.SaveToFile(const AFileName: string);
begin
	TFile.WriteAllText(AFileName, ToString, TEncoding.ASCII);
end;

procedure TMVCConfig.SetValue(AIndex: string; const Value: string);
begin
	FConfig.AddOrSetValue(AIndex, Value);
end;

function TMVCConfig.ToString: string;
var
	k: string;
	JSON: TJSONObject;
begin
	JSON := TJSONObject.Create;
	try
		for k in FConfig.Keys do
			JSON.AddPair(k, FConfig[k]);
		Result := JSON.ToString;
	finally
		JSON.Free;
	end;
end;

{ EMVCException }

constructor EMVCException.Create(const Msg: string);
begin
	inherited Create(Msg);
	FHTTPErrorCode := 500;
	FDetailedMessage := 'N.A.';
	FApplicationErrorCode := 0;
end;

constructor EMVCException.Create(const Msg, DetailedMessage: string;
	const ApplicationErrorCode: UInt16; const HTTPErrorCode: UInt16);
begin
	Create(Msg);
	FHTTPErrorCode := HTTPErrorCode;
	FApplicationErrorCode := ApplicationErrorCode;
	FDetailedMessage := DetailedMessage;
end;

constructor EMVCException.CreateFmt(const Msg: string;
	const Args: array of const);
begin
	inherited;
	FHTTPErrorCode := 500;
	FDetailedMessage := 'N.A.';
	FApplicationErrorCode := 0;
end;

procedure EMVCException.SetDetailedMessage(const Value: string);
begin
	FDetailedMessage := Value;
end;

initialization

Lock := TObject.Create;

finalization

FreeAndNil(Lock);

end.
