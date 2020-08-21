// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file:
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

unit MVCFramework.RESTClient;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  REST.Client,
  REST.Types,
  MVCFramework.RESTClient.Intf,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Commons,
  MVCFramework.RESTClient.Indy,
  Data.DB,
  System.Rtti,
  System.TypInfo,
  System.Net.HttpClient;

type
  /// <summary>
  /// Alias for the Indy-based TRESTClient. The implementation of TRESTClient has been discontinued, it remains for
  /// compatibility only.
  /// </summary>
  TRESTClient = MVCFramework.RESTClient.Indy.TRESTClient deprecated
    'Moved to the MVCFramework.RESTClient.Indy unit. It is highly recommended to migrate to the TMVCRESTClient implementation.';

  IRESTResponse = MVCFramework.RESTClient.Indy.IRESTResponse deprecated
    'Moved to the MVCFramework.RESTClient.Indy unit. It is highly recommended to migrate to the TMVCRESTClient implementation.';

  /// <summary>
  /// Provides access to MVCRESTClient interfaces without the need to use the MVCFramework.RESTClient.Intf unit
  /// </summary>
  IMVCRESTClient = MVCFramework.RESTClient.Intf.IMVCRESTClient;
  IMVCRESTResponse = MVCFramework.RESTClient.Intf.IMVCRESTResponse;

  /// <summary>
  /// Provides access to delphi RESTClient library types without the need to use the REST.Types unit.
  /// </summary>
  TRESTContentType = REST.Types.TRESTContentType;

  /// <summary>
  /// Encapsulates the methods of the delphi native RESTClient library.
  /// </summary>
  TMVCRESTClient = class(TInterfacedObject, IMVCRESTClient)
  private
    fRttiContext: TRttiContext;
    fRESTClient: TCustomRESTClient;
    fRESTRequest: TCustomRESTRequest;
    fRESTResponse: TCustomRESTResponse;
    fSerializer: IMVCSerializer;
    fNextRequestIsAsync: Boolean;
    fAsyncCompletionHandler: TProc<IMVCRESTResponse>;
    fAsyncCompletionHandlerWithError: TProc<Exception>;
    fAsyncSynchronized: Boolean;

    procedure ClearRESTParams(const aRESTParamKind: TRESTRequestParameterKind);
    function ConvertMVCPathParamsToRESTParams(const aResource: string): string;
    function ObjectIsList(aObject: TObject): Boolean;
    function SerializeObject(aObject: TObject): string;
{$IF not defined(SYDNEYORBETTER)}
    function GetResponseCookies: TArray<TCookie>;
{$ENDIF}
    procedure ExecuteAsyncRESTRequest;
    function ExecuteRESTRequest(const aMethod: TRESTRequestMethod): IMVCRESTResponse;
  public
    constructor Create;
    destructor Destroy; override;

    class function New: IMVCRESTClient;

    { IMVCRESTClient }

    function BaseURL(const aHost: string; const aPort: Integer): IMVCRESTClient; overload;
    function BaseURL(const aBaseURL: string): IMVCRESTClient; overload;
    function BaseURL: string; overload;

    function RaiseExceptionOn500(const aRaiseExceptionOn500: Boolean): IMVCRESTClient; overload;
    function RaiseExceptionOn500: Boolean; overload;

    function ProxyServer(const aProxyServer: string): IMVCRESTClient; overload;
    function ProxyServer: string; overload;
    function ProxyPort(const aProxyPort: Integer): IMVCRESTClient; overload;
    function ProxyPort: Integer; overload;
    function ProxyUsername(const aProxyUsername: string): IMVCRESTClient; overload;
    function ProxyUsername: string; overload;
    function ProxyPassword(const aProxyPassword: string): IMVCRESTClient; overload;
    function ProxyPassword: string; overload;

    function UserAgent(const aUserAgent: string): IMVCRESTClient; overload;
    function UserAgent: string; overload;

    /// <summary>
    /// Clears all parameters, except authorization headers. This method is executed after each request is completed.
    /// </summary>
    function ClearAllParams: IMVCRESTClient;

    /// <summary>
    /// Connection timeout in milliseconds to be used for the requests.
    /// </summary>
    function ConnectTimeout(const aConnectTimeout: Integer): IMVCRESTClient; overload;
    function ConnectTimeout: Integer; overload;

    /// <summary>
    /// Response reading timeout in milliseconds to be used for the requests.
    /// </summary>
    function ReadTimeout(const aReadTimeout: Integer): IMVCRESTClient; overload;
    function ReadTimeout: Integer; overload;

    /// <summary>
    /// Add basic authorization header. Authorization = Basic &lt;Username:Password&gt; (encoded in Base64)
    /// </summary>
    function SetBasicAuthorization(const aUsername, aPassword: string): IMVCRESTClient;

    /// <summary>
    /// Add bearer authorization header. Authorization = Bearer &lt;Token&gt;
    /// </summary>
    function SetBearerAuthorization(const aToken: string): IMVCRESTClient;

    /// <summary>
    /// Add a header.
    /// </summary>
    /// <param name="aName">
    /// Header name
    /// </param>
    /// <param name="aValue">
    /// Header value
    /// </param>
    /// <param name="aDoNotEncode">
    /// Indicates whether the value of this header should be used as is (True), or encoded by the component (False)
    /// </param>
    function AddHeader(const aName, aValue: string; const aDoNotEncode: Boolean = False): IMVCRESTClient; overload;

    /// <summary>
    /// Clears all headers.
    /// </summary>
    function ClearHeaders: IMVCRESTClient;

    function AllowCookies(const aAllowCookies: Boolean): IMVCRESTClient; overload;
    function AllowCookies: Boolean; overload;

    /// <summary>
    /// Add a cookie header.
    /// </summary>
    function AddCookie(const aName, aValue: string): IMVCRESTClient;
    /// <summary>
    /// Clear all cookie headers.
    /// </summary>
    function ClearCookies: IMVCRESTClient;

    /// <summary>
    /// Add a URL segment parameter. The parameters of your url path may be enclosed in braces or in
    /// parentheses starting with a money sign. <c>/api/{param1}/($param2)</c>
    /// </summary>
    /// <param name="aName">
    /// Parameter name
    /// </param>
    /// <param name="aValue">
    /// Parameter value
    /// </param>
    function AddPathParam(const aName, aValue: string): IMVCRESTClient; overload;
    function AddPathParam(const aName: string; aValue: Integer): IMVCRESTClient; overload;
    function AddPathParam(const aName: string; aValue: Int64): IMVCRESTClient; overload;
    function AddPathParam(const aName: string; aValue: TGUID): IMVCRESTClient; overload;
    function AddPathParam(const aName: string; aValue: TDateTime): IMVCRESTClient; overload;
    function AddPathParam(const aName: string; aValue: TDate): IMVCRESTClient; overload;
    function AddPathParam(const aName: string; aValue: TTime): IMVCRESTClient; overload;
    function AddPathParam(const aName: string; aValue: Double): IMVCRESTClient; overload;
    function ClearPathParams: IMVCRESTClient;

    /// <summary>
    /// Add a QueryString parameter. <c>/api/person?para1=value&amp;param2=value</c>
    /// </summary>
    function AddQueryStringParam(const aName, aValue: string): IMVCRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: Integer): IMVCRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: Int64): IMVCRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: TGUID): IMVCRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: TDateTime): IMVCRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: TDate): IMVCRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: TTime): IMVCRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: Double): IMVCRESTClient; overload;
    function ClearQueryParams: IMVCRESTClient;

    function Accept(const aAccept: string): IMVCRESTClient; overload;
    function Accept: string; overload;
    function AcceptCharset(const aAcceptCharset: string): IMVCRESTClient; overload;
    function AcceptCharset: string; overload;
    function AcceptEncoding(const aAcceptEncoding: string): IMVCRESTClient; overload;
    function AcceptEncoding: string; overload;
    function HandleRedirects(const aHandleRedirects: Boolean): IMVCRESTClient; overload;
    function HandleRedirects: Boolean; overload;

    function FallbackCharsetEncoding(const aFallbackCharsetEncoding: string): IMVCRESTClient; overload;
    function FallbackCharsetEncoding: string; overload;

    function Resource(const aResource: string): IMVCRESTClient; overload;
    function Resource: string; overload;

    function URLAlreadyEncoded(const aURLAlreadyEncoded: Boolean): IMVCRESTClient; overload;
    function URLAlreadyEncoded: Boolean; overload;

    /// <summary>
    /// Add a body to the requisition.
    /// </summary>
    /// <param name="aBody">
    /// Body in string format.
    /// </param>
    /// <param name="aContentType">
    /// Body content type.
    /// </param>
    function AddBody(const aBody: string; const aDoNotEncode: Boolean = False;
      const aContentType: TRESTContentType = TRESTContentType.ctNone): IMVCRESTClient; overload;
    /// <summary>
    /// Add a body to the requisition
    /// </summary>
    /// <param name="aBodyStream">
    /// Body in Stream format
    /// </param>
    /// <param name="aContentType">
    /// Body content type
    /// </param>
    /// <param name="aOwnsStream">
    /// If OwnsStream is true, Stream will be destroyed by IMVCRESTClient.
    /// </param>
    function AddBody(aBodyStream: TStream; const aContentType: TRESTContentType = TRESTContentType.ctNone;
      const aOwnsStream: Boolean = True): IMVCRESTClient; overload;
    /// <summary>
    /// Add a body to the requisition
    /// </summary>
    /// <param name="aBodyObject">
    /// Body in Object format. The object will be serialized to a JSON string.
    /// </param>
    /// <param name="aOwnsObject">
    /// If OwnsObject is true, BodyObject will be destroyed by IMVCRESTClient.
    /// </param>
    function AddBody(aBodyObject: TObject; const aOwnsObject: Boolean = True): IMVCRESTClient; overload;
    function ClearBody: IMVCRESTClient;

    /// <summary>
    /// Adds a file as the request body. Several files can be added in the same request. In this case the request
    /// will be of the multipart/form-data type
    /// </summary>
    /// <param name="aName">
    /// Field name
    /// </param>
    /// <param name="aFileName">
    /// File path
    /// </param>
    /// <param name="aContentType">
    /// File content type
    /// </param>
    function AddFile(const aName, aFileName: string;
      const aContentType: TRESTContentType = TRESTContentType.ctNone): IMVCRESTClient; overload;
    function AddFile(const aFileName: string;
      const aContentType: TRESTContentType = TRESTContentType.ctNone): IMVCRESTClient; overload;
    function ClearFiles: IMVCRESTClient;

    /// <summary>
    /// Executes the next request asynchronously.
    /// </summary>
    /// <param name="aCompletionHandler">
    /// An anonymous method that will be run after the execution completed.
    /// </param>
    /// <param name="aSynchronized">
    /// Specifies if aCompletioHandler will be run in the main thread's (True) or execution thread's (False) context.
    /// </param>
    /// <param name="aCompletionHandlerWithError">
    /// An anonymous method that will be run if an exception is raised during execution.
    /// </param>
    function Async(aCompletionHandler: TProc<IMVCRESTResponse>; aCompletionHandlerWithError: TProc<Exception> = nil;
      const aSynchronized: Boolean = True): IMVCRESTClient;

    /// <summary>
    /// Execute a Get request.
    /// </summary>
    function Get(const aResource: string): IMVCRESTResponse; overload;
    function Get: IMVCRESTResponse; overload;

    /// <summary>
    /// Execute a Post request.
    /// </summary>
    function Post(const aResource: string; aBody: TObject; const aOwnsBody: Boolean = True): IMVCRESTResponse; overload;
    function Post(const aResource: string; const aBody: string = ''): IMVCRESTResponse; overload;
    function Post: IMVCRESTResponse; overload;

    /// <summary>
    /// Execute a Patch request.
    /// </summary>
    function Patch(const aResource: string; aBody: TObject;
      const aOwnsBody: Boolean = True): IMVCRESTResponse; overload;
    function Patch(const aResource: string; const aBody: string = ''): IMVCRESTResponse; overload;
    function Patch: IMVCRESTResponse; overload;

    /// <summary>
    /// Execute a Put request.
    /// </summary>
    function Put(const aResource: string; aBody: TObject; const aOwnsBody: Boolean = True): IMVCRESTResponse; overload;
    function Put(const aResource: string; const aBody: string = ''): IMVCRESTResponse; overload;
    function Put: IMVCRESTResponse; overload;

    /// <summary>
    /// Execute a Delete request.
    /// </summary>
    function Delete(const aResource: string): IMVCRESTResponse; overload;
    function Delete: IMVCRESTResponse; overload;

    /// <summary>
    /// Serialize the current dataset record and execute a POST request.
    /// </summary>
    function DataSetInsert(const aResource: string; aDataSet: TDataSet; const aIgnoredFields: TMVCIgnoredList = [];
      const aNameCase: TMVCNameCase = ncAsIs): IMVCRESTResponse;
    /// <summary>
    /// Serialize the current dataset record and execute a PUT request.
    /// </summary>
    function DataSetUpdate(const aResource: string; aDataSet: TDataSet; const aIgnoredFields: TMVCIgnoredList = [];
      const aNameCase: TMVCNameCase = ncAsIs): IMVCRESTResponse;
    /// <summary>
    /// Delete the current dataset record by executing a delete request.
    /// </summary>
    function DataSetDelete(const aResource: string): IMVCRESTResponse;

    /// <summary>
    /// Register a custom serializer to the RESTClient serializer.
    /// </summary>
    function RegisterTypeSerializer(const aTypeInfo: PTypeInfo; aInstance: IMVCTypeSerializer): IMVCRESTClient;

    /// <summary>
    /// Creates a new instance of RESTClient with all parameters of the current RESTClient.
    /// </summary>
    function CloneRESTClient: IMVCRESTClient;
  end;

  /// <summary>
  /// Provides access to the REST request response.
  /// </summary>
  TMVCRESTResponse = class(TInterfacedObject, IMVCRESTResponse)
  private
    fSuccess: Boolean;
    fStatusCode: Integer;
    fStatusText: string;
    fErrorMessage: string;
    fHeaders: TStrings;
    fCookies: TCookies;
    fServer: string;
    fFullRequestURI: string;
    fContentType: string;
    fContentEncoding: string;
    fContentLength: Integer;
    fContent: string;
    fRawBytes: TBytes;

    procedure FillRESTResponse(aRESTResponse: TCustomRESTResponse);
  public
    constructor Create(aRESTResponse: TCustomRESTResponse);
    destructor Destroy; override;
{$IF not defined(SYDNEYORBETTER)}
    procedure SetCookies(aCookies: TArray<TCookie>);
{$ENDIF}

    { IMVCRESTResponse }
    function Success: Boolean;
    function StatusCode: Integer;
    function StatusText: string;
    function ErrorMessage: string;
    function Headers: TStrings;
    function Cookies: TCookies;
    function HeaderByName(const aName: string): string;
    function Server: string;
    function FullRequestURI: string;
    function ContentType: string;
    function ContentEncoding: string;
    function ContentLength: Integer;
    function Content: string;
    function RawBytes: TBytes;
    procedure SaveContentToStream(aStream: TStream);
    procedure SaveContentToFile(const aFileName: string);
  end;

  EMVCRESTClientException = class(Exception);

implementation

uses
  MVCFramework.Serializer.JsonDataObjects,
  System.NetEncoding,
  System.RegularExpressions,
  REST.HttpClient;

const
  DEFAULT_ACCEPT_ENCODING = 'gzip, deflate';
  DEFAULT_BODY_NAME = 'body';
  DEFAULT_FILE_NAME = 'file';
  AUTHORIZATION_HEADER = 'Authorization';
  BASIC_AUTH_PREFIX = 'Basic ';
  BEARER_AUTH_PREFIX = 'Bearer ';
  HEADER_RESPONSE_COOKIES = 'Cookies';

{ TMVCRESTClient }

function TMVCRESTClient.Accept(const aAccept: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.Accept := aAccept;
end;

function TMVCRESTClient.Accept: string;
begin
  Result := fRESTRequest.Accept;
end;

function TMVCRESTClient.AcceptCharset: string;
begin
  Result := fRESTRequest.AcceptCharset;
end;

function TMVCRESTClient.AcceptCharset(const aAcceptCharset: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.AcceptCharset := aAcceptCharset;
end;

function TMVCRESTClient.AcceptEncoding: string;
begin
  Result := fRESTRequest.AcceptEncoding;
end;

function TMVCRESTClient.AcceptEncoding(const aAcceptEncoding: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.AcceptEncoding := aAcceptEncoding;
end;

function TMVCRESTClient.AddBody(const aBody: string; const aDoNotEncode: Boolean;
  const aContentType: TRESTContentType): IMVCRESTClient;
var
  lBodyName: string;
  lOptions: TRESTRequestParameterOptions;
begin
  Result := Self;

  // A body does not have a specific name, but as names need to be unique, we are using a GUID here
  lBodyName := TGUID.NewGuid.ToString;
  lBodyName := lBodyName.Replace('{', '', [rfReplaceAll]);
  lBodyName := lBodyName.Replace('}', '', [rfReplaceAll]);
  lBodyName := lBodyName.Replace('-', '', [rfReplaceAll]);
  lBodyName := DEFAULT_BODY_NAME + lBodyName;

  if aDoNotEncode then
    lOptions := [TRESTRequestParameterOption.poDoNotEncode]
  else
    lOptions := [];

  fRESTRequest.AddParameter(lBodyName, aBody, TRESTRequestParameterKind.pkREQUESTBODY, lOptions);
end;

function TMVCRESTClient.AddBody(aBodyStream: TStream; const aContentType: TRESTContentType;
  const aOwnsStream: Boolean): IMVCRESTClient;
var
  lOwnsStream: TRESTObjectOwnership;
begin
  if aBodyStream = nil then
    raise EMVCRESTClientException.Create('You need a valid body!');

  Result := Self;

  if aOwnsStream then
    lOwnsStream := TRESTObjectOwnership.ooREST
  else
    lOwnsStream := TRESTObjectOwnership.ooApp;

  fRESTRequest.AddBody(aBodyStream, aContentType, lOwnsStream);
end;

function TMVCRESTClient.AddBody(aBodyObject: TObject; const aOwnsObject: Boolean): IMVCRESTClient;
begin
  if aBodyObject = nil then
    raise EMVCRESTClientException.Create('You need a valid body!');

  Result := Self;

  AddBody(SerializeObject(aBodyObject), False, TRESTContentType.ctAPPLICATION_JSON);

  if aOwnsObject then
    aBodyObject.Free;
end;

function TMVCRESTClient.AddCookie(const aName, aValue: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.AddParameter(aName, aValue, TRESTRequestParameterKind.pkCOOKIE);
end;

function TMVCRESTClient.AddFile(const aName, aFileName: string; const aContentType: TRESTContentType): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.AddFile(aName, aFileName, aContentType);
end;

function TMVCRESTClient.AddFile(const aFileName: string; const aContentType: TRESTContentType): IMVCRESTClient;
begin
  Result := AddFile(DEFAULT_FILE_NAME, aFileName, aContentType);
end;

function TMVCRESTClient.AddHeader(const aName, aValue: string; const aDoNotEncode: Boolean): IMVCRESTClient;
var
  lOptions: TRESTRequestParameterOptions;
begin
  Result := Self;

  if aDoNotEncode then
    lOptions := [TRESTRequestParameterOption.poDoNotEncode]
  else
    lOptions := [];

  fRESTRequest.AddParameter(aName, aValue, TRESTRequestParameterKind.pkHTTPHEADER, lOptions);
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: TGUID): IMVCRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: Int64): IMVCRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: Integer): IMVCRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddPathParam(const aName, aValue: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.AddParameter(aName, aValue);
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: Double): IMVCRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: TTime): IMVCRESTClient;
begin
  Result := AddPathParam(aName, TimeToISOTime(aValue));
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: TDateTime): IMVCRESTClient;
begin
  Result := AddPathParam(aName, DateTimeToISOTimeStamp(aValue));
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: TDate): IMVCRESTClient;
begin
  Result := AddPathParam(aName, DateToISODate(aValue));
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: TDate): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, DateToISODate(aValue));
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: TDateTime): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, DateTimeToISOTimeStamp(aValue));
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: TTime): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, TimeToISOTime(aValue));
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: Double): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AllowCookies: Boolean;
begin
  Result := fRESTClient.AllowCookies;
end;

function TMVCRESTClient.AllowCookies(const aAllowCookies: Boolean): IMVCRESTClient;
begin
  Result := Self;
  fRESTClient.AllowCookies := aAllowCookies;
end;

function TMVCRESTClient.Async(aCompletionHandler: TProc<IMVCRESTResponse>; aCompletionHandlerWithError: TProc<Exception>;
  const aSynchronized: Boolean): IMVCRESTClient;
begin
  Result := Self;
  fNextRequestIsAsync := True;
  fAsyncCompletionHandler := aCompletionHandler;
  fAsyncCompletionHandlerWithError := aCompletionHandlerWithError;
  fAsyncSynchronized := aSynchronized;
end;

function TMVCRESTClient.BaseURL(const aHost: string; const aPort: Integer): IMVCRESTClient;
begin
  Result := BaseURL(aHost + ':' + aPort.ToString);
end;

function TMVCRESTClient.BaseURL(const aBaseURL: string): IMVCRESTClient;
var
  lBaseURL: string;
begin
  Result := Self;

  lBaseURL := aBaseURL;
  if not lBaseURL.Contains('://') then
    lBaseURL := 'http://' + lBaseURL;

  fRESTClient.BaseURL := lBaseURL;
end;

function TMVCRESTClient.BaseURL: string;
begin
  Result := fRESTClient.BaseURL;
end;

function TMVCRESTClient.AddQueryStringParam(const aName, aValue: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.AddParameter(aName, aValue);
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: Integer): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: TGUID): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: Int64): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, aValue.ToString);
end;

function TMVCRESTClient.ClearAllParams: IMVCRESTClient;
var
  lAuthHeader: TRESTRequestParameter;
begin
  Result := Self;

  fRESTClient.HandleRedirects := True;
  fRESTClient.RaiseExceptionOn500 := False;
  fRESTClient.AllowCookies := True;

  // If the authorization header exists, it will be extracted from the list of parameters to be added later.
  lAuthHeader := fRESTRequest.Params.ParameterByName(AUTHORIZATION_HEADER);
  if Assigned(lAuthHeader) then
    lAuthHeader.Collection := nil;

  fRESTRequest.ResetToDefaults;
  fRESTRequest.AutoCreateParams := False;
  fRESTRequest.AcceptEncoding := DEFAULT_ACCEPT_ENCODING;

  if Assigned(lAuthHeader) then
    lAuthHeader.Collection := fRESTRequest.Params;

  fNextRequestIsAsync := False;;
  fAsyncCompletionHandler := nil;
  fAsyncCompletionHandlerWithError := nil;
  fAsyncSynchronized := False;
end;

function TMVCRESTClient.ClearBody: IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.ClearBody;
end;

function TMVCRESTClient.ClearCookies: IMVCRESTClient;
begin
  Result := Self;
  ClearRESTParams(TRESTRequestParameterKind.pkCOOKIE);
end;

function TMVCRESTClient.ClearFiles: IMVCRESTClient;
begin
  Result := Self;
  ClearRESTParams(TRESTRequestParameterKind.pkFILE);
end;

function TMVCRESTClient.ClearHeaders: IMVCRESTClient;
begin
  Result := Self;
  ClearRESTParams(TRESTRequestParameterKind.pkHTTPHEADER);
end;

function TMVCRESTClient.ClearPathParams: IMVCRESTClient;
begin
  Result := Self;
  ClearRESTParams(TRESTRequestParameterKind.pkURLSEGMENT);
end;

function TMVCRESTClient.ClearQueryParams: IMVCRESTClient;
begin
  Result := Self;
  ClearRESTParams(TRESTRequestParameterKind.pkQUERY);
end;

procedure TMVCRESTClient.ClearRESTParams(const aRESTParamKind: TRESTRequestParameterKind);
var
  I: Integer;
begin
  for I := Pred(fRESTRequest.Params.Count) downto 0 do
  begin
    if (fRESTRequest.Params[I].Kind = aRESTParamKind) then
      fRESTRequest.Params.Delete(I);
  end;
end;

function TMVCRESTClient.CloneRESTClient: IMVCRESTClient;
begin
  Result := TMVCRESTClient.New
    .BaseURL(BaseURL)
    .RaiseExceptionOn500(RaiseExceptionOn500)
    .ProxyServer(ProxyServer)
    .ProxyPort(ProxyPort)
    .ProxyUsername(ProxyUsername)
    .ProxyPassword(ProxyPassword)
    .ProxyServer(ProxyServer)
    .UserAgent(UserAgent)
    .ConnectTimeout(ConnectTimeout)
    .ReadTimeout(ReadTimeout)
    .Accept(Accept)
    .AcceptCharset(AcceptCharset)
    .AcceptEncoding(AcceptEncoding)
    .HandleRedirects(HandleRedirects)
    .AllowCookies(AllowCookies)
    .Resource(Resource)
    .URLAlreadyEncoded(URLAlreadyEncoded);
  TMVCRESTClient(Result).fRESTRequest.Params.Assign(fRESTRequest.Params);
end;

function TMVCRESTClient.ConnectTimeout(const aConnectTimeout: Integer): IMVCRESTClient;
begin
  Result := Self;
{$IF defined(SYDNEYORBETTER)}
  fRESTRequest.ConnectTimeout := aConnectTimeout;
{$ELSE}
  fRESTRequest.Timeout := aConnectTimeout;
{$ENDIF}
end;

function TMVCRESTClient.ConnectTimeout: Integer;
begin
{$IF defined(SYDNEYORBETTER)}
  Result := fRESTRequest.ConnectTimeout;
{$ELSE}
  Result := fRESTRequest.Timeout;
{$ENDIF}
end;

function TMVCRESTClient.ConvertMVCPathParamsToRESTParams(const aResource: string): string;
begin
  Result := TRegEx.Replace(aResource, '(\([($])([\w_]+)([)])', '{\2}', [roIgnoreCase]);
end;

constructor TMVCRESTClient.Create;
begin
  inherited Create;

  fRESTClient := TCustomRESTClient.Create(nil);
  fRESTRequest := TCustomRESTRequest.Create(nil);
  fRESTResponse := TCustomRESTResponse.Create(nil);

  fRESTRequest.Client := fRESTClient;
  fRESTRequest.Response := fRESTResponse;

  fSerializer := TMVCJsonDataObjectsSerializer.Create;
  fRttiContext := TRttiContext.Create;

  ClearAllParams;
end;

function TMVCRESTClient.DataSetDelete(const aResource: string): IMVCRESTResponse;
begin
  Result := Delete(aResource);
end;

function TMVCRESTClient.DataSetInsert(const aResource: string; aDataSet: TDataSet; const aIgnoredFields: TMVCIgnoredList;
  const aNameCase: TMVCNameCase): IMVCRESTResponse;
begin
  Result := Post(aResource, fSerializer.SerializeDataSetRecord(aDataSet, aIgnoredFields, aNameCase));
end;

function TMVCRESTClient.DataSetUpdate(const aResource: string; aDataSet: TDataSet; const aIgnoredFields: TMVCIgnoredList;
  const aNameCase: TMVCNameCase): IMVCRESTResponse;
begin
  Result := Put(aResource, fSerializer.SerializeDataSetRecord(aDataSet, aIgnoredFields, aNameCase));
end;

function TMVCRESTClient.Delete: IMVCRESTResponse;
begin
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmDELETE);
end;

function TMVCRESTClient.Delete(const aResource: string): IMVCRESTResponse;
begin
  Resource(aResource);
  Result := Delete;
end;

destructor TMVCRESTClient.Destroy;
begin
  fRESTResponse.Free;
  fRESTRequest.Free;
  fRESTClient.Free;
  fSerializer := nil;
  fRttiContext.Free;

  inherited Destroy;
end;

procedure TMVCRESTClient.ExecuteAsyncRESTRequest;
var
  lMVCRESTClient: IMVCRESTClient;
  lAsyncCompletionHandler: TProc<IMVCRESTResponse>;
  lAsyncSynchronized: Boolean;
  lAsyncCompletionHandlerWithError: TProc<Exception>;
begin
  // For asynchronous execution, a clone of RESTClient is created to be executed in an anonymous thread
  lMVCRESTClient := CloneRESTClient;

  lAsyncCompletionHandler := fAsyncCompletionHandler;
  lAsyncSynchronized := fAsyncSynchronized;
  lAsyncCompletionHandlerWithError := fAsyncCompletionHandlerWithError;

  TThread.CreateAnonymousThread(
    procedure
    var
      lMVCRESTResponse: IMVCRESTResponse;
    begin
      try
        TMVCRESTClient(lMVCRESTClient).fRESTRequest.Execute;
        lMVCRESTResponse := TMVCRESTResponse.Create(TMVCRESTClient(lMVCRESTClient).fRESTResponse);
{$IF not defined(SYDNEYORBETTER)}
        TMVCRESTResponse(lMVCRESTResponse).SetCookies(TMVCRESTClient(lMVCRESTClient).GetResponseCookies);
{$ENDIF}

        if Assigned(lAsyncCompletionHandler) then
        begin
          if lAsyncSynchronized then
            TThread.Synchronize(nil,
              procedure
              begin
                lAsyncCompletionHandler(lMVCRESTResponse)
              end
              )
          else
            lAsyncCompletionHandler(lMVCRESTResponse);
        end;
      except
        on E: Exception do
        begin
          if Assigned(lAsyncCompletionHandlerWithError) then
          begin
            if fAsyncSynchronized then
              TThread.Synchronize(nil,
                procedure
                begin
                  lAsyncCompletionHandlerWithError(E)
                end
                )
            else
              lAsyncCompletionHandlerWithError(E);
          end;
        end;
      end;
    end).Start;
end;

function TMVCRESTClient.ExecuteRESTRequest(const aMethod: TRESTRequestMethod): IMVCRESTResponse;
begin
  fRESTRequest.Method := aMethod;

  if fNextRequestIsAsync then
  begin
    Result := nil;
    ExecuteAsyncRESTRequest;
  end
  else
  begin
    fRESTRequest.Execute;
    Result := TMVCRESTResponse.Create(fRESTResponse);
{$IF not defined(SYDNEYORBETTER)}
    TMVCRESTResponse(Result).SetCookies(GetResponseCookies);
{$ENDIF}
  end;
  ClearAllParams;
end;

function TMVCRESTClient.FallbackCharsetEncoding: string;
begin
  Result := fRESTClient.FallbackCharsetEncoding;
end;

function TMVCRESTClient.FallbackCharsetEncoding(const aFallbackCharsetEncoding: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTClient.FallbackCharsetEncoding := aFallbackCharsetEncoding;
end;

function TMVCRESTClient.Get(const aResource: string): IMVCRESTResponse;
begin
  Resource(aResource);
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmGET);
end;

function TMVCRESTClient.HandleRedirects(const aHandleRedirects: Boolean): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.HandleRedirects := aHandleRedirects;
end;

function TMVCRESTClient.HandleRedirects: Boolean;
begin
  Result := fRESTRequest.HandleRedirects;
end;

class function TMVCRESTClient.New: IMVCRESTClient;
begin
  Result := TMVCRESTClient.Create;
end;

function TMVCRESTClient.ObjectIsList(aObject: TObject): Boolean;
begin
  Result := fRttiContext.GetType(aObject.ClassType).GetMethod('GetEnumerator') <> nil;
end;

function TMVCRESTClient.Get: IMVCRESTResponse;
begin
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmGET);
end;

function TMVCRESTClient.Patch(const aResource, aBody: string): IMVCRESTResponse;
begin
  Resource(aResource);
  if not aBody.isEmpty then
  begin
    ClearBody;
    AddBody(aBody, False, TRESTContentType.ctAPPLICATION_JSON);
  end;

  Result := Patch;
end;

function TMVCRESTClient.Patch: IMVCRESTResponse;
begin
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmPATCH);
end;

function TMVCRESTClient.Patch(const aResource: string; aBody: TObject; const aOwnsBody: Boolean): IMVCRESTResponse;
begin
  if aBody = nil then
    raise EMVCRESTClientException.Create('You need a valid body!');

  Result := Patch(aResource, SerializeObject(aBody));

  if aOwnsBody then
    aBody.Free;
end;

function TMVCRESTClient.Post(const aResource: string; aBody: TObject; const aOwnsBody: Boolean): IMVCRESTResponse;
begin
  if aBody = nil then
    raise EMVCRESTClientException.Create('You need a valid body!');

  Result := Post(aResource, SerializeObject(aBody));

  if aOwnsBody then
    aBody.Free;
end;

function TMVCRESTClient.Post(const aResource, aBody: string): IMVCRESTResponse;
begin
  Resource(aResource);
  if not aBody.IsEmpty then
  begin
    ClearBody;
    AddBody(aBody, False, TRESTContentType.ctAPPLICATION_JSON);
  end;
  Result := Post;
end;

function TMVCRESTClient.Post: IMVCRESTResponse;
begin
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmPOST);
end;

function TMVCRESTClient.ProxyPassword(const aProxyPassword: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTClient.ProxyPassword := aProxyPassword;
end;

function TMVCRESTClient.ProxyPassword: string;
begin
  Result := fRESTClient.ProxyPassword;
end;

function TMVCRESTClient.ProxyPort: Integer;
begin
  Result := fRESTClient.ProxyPort;
end;

function TMVCRESTClient.ProxyPort(const aProxyPort: Integer): IMVCRESTClient;
begin
  Result := Self;
  fRESTClient.ProxyPort := aProxyPort;
end;

function TMVCRESTClient.ProxyServer(const aProxyServer: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTClient.ProxyServer := aProxyServer;
end;

function TMVCRESTClient.ProxyServer: string;
begin
  Result := fRESTClient.ProxyServer;
end;

function TMVCRESTClient.ProxyUsername: string;
begin
  Result := fRESTClient.ProxyUsername;
end;

function TMVCRESTClient.ProxyUsername(const aProxyUsername: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTClient.ProxyUsername := aProxyUsername;
end;

function TMVCRESTClient.Put(const aResource: string; aBody: TObject; const aOwnsBody: Boolean): IMVCRESTResponse;
begin
  if aBody = nil then
    raise EMVCRESTClientException.Create('You need a valid body!');

  Result := Put(aResource, SerializeObject(aBody));

  if aOwnsBody then
    aBody.Free;
end;

function TMVCRESTClient.Put(const aResource, aBody: string): IMVCRESTResponse;
begin
  Resource(aResource);
  if not aBody.IsEmpty then
  begin
    ClearBody;
    AddBody(aBody, False, TRESTContentType.ctAPPLICATION_JSON);
  end;
  Result := Put;
end;

function TMVCRESTClient.Put: IMVCRESTResponse;
begin
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmPUT);
end;

function TMVCRESTClient.RaiseExceptionOn500(const aRaiseExceptionOn500: Boolean): IMVCRESTClient;
begin
  Result := Self;
  fRESTClient.RaiseExceptionOn500 := aRaiseExceptionOn500;
end;

function TMVCRESTClient.Resource: string;
begin
  Result := fRESTRequest.Resource;
end;

{$IF not defined(SYDNEYORBETTER)}
function TMVCRESTClient.GetResponseCookies: TArray<TCookie>;
var
  lRttiType: TRttiType;
  lRttiField: TRttiField;
  lRestHttp: TRESTHTTP;
  lHttpResponse: IHTTPResponse;
begin
  SetLength(Result, 0);
  lRttiType := fRttiContext.GetType(fRESTClient.ClassType);
  lRttiField := lRttiType.GetField('FHttpClient');
  if not Assigned(lRttiField) then
    Exit;

  lRestHttp :=  lRttiField.GetValue(fRESTClient).AsObject as TRESTHTTP;
  lRttiType := fRttiContext.GetType(lRestHttp.ClassType);
  lRttiField := lRttiType.GetField('FHTTPResponse');

  if not Assigned(lRttiField) then
    Exit;

  lHttpResponse := lRttiField.GetValue(lRestHttp).AsInterface as IHTTPResponse;

  Result := lHttpResponse.Cookies.ToArray;
end;
{$ENDIF}

function TMVCRESTClient.RegisterTypeSerializer(const aTypeInfo: PTypeInfo;
aInstance: IMVCTypeSerializer): IMVCRESTClient;
begin
  Result := Self;
  fSerializer.RegisterTypeSerializer(aTypeInfo, aInstance);
end;

function TMVCRESTClient.Resource(const aResource: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.Resource := ConvertMVCPathParamsToRESTParams(aResource);

end;

function TMVCRESTClient.RaiseExceptionOn500: Boolean;
begin
  Result := fRESTClient.RaiseExceptionOn500;
end;

function TMVCRESTClient.ReadTimeout: Integer;
begin
{$IF defined(SYDNEYORBETTER)}
  Result := fRESTRequest.ReadTimeout;
{$ELSE}
  Result := fRESTRequest.Timeout;
{$ENDIF}
end;

function TMVCRESTClient.ReadTimeout(const aReadTimeout: Integer): IMVCRESTClient;
begin
  Result := Self;
{$IF defined(SYDNEYORBETTER)}
  fRESTRequest.ReadTimeout := aReadTimeout;
{$ELSE}
  fRESTRequest.Timeout := aReadTimeout;
{$ENDIF}
end;

function TMVCRESTClient.URLAlreadyEncoded(const aURLAlreadyEncoded: Boolean): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.URLAlreadyEncoded := aURLAlreadyEncoded;
end;

function TMVCRESTClient.UserAgent(const aUserAgent: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTClient.UserAgent := aUserAgent;
end;

function TMVCRESTClient.UserAgent: string;
begin
  Result := fRESTClient.UserAgent;
end;

function TMVCRESTClient.URLAlreadyEncoded: Boolean;
begin
  Result := fRESTRequest.URLAlreadyEncoded;
end;

function TMVCRESTClient.SerializeObject(aObject: TObject): string;
begin
  if ObjectIsList(aObject) then
    Result := fSerializer.SerializeCollection(aObject)
  else
    Result := fSerializer.SerializeObject(aObject);
end;

function TMVCRESTClient.SetBasicAuthorization(const aUsername, aPassword: string): IMVCRESTClient;
var
  LBase64: TNetEncoding;
  LAuthValue: string;
begin
  Result := Self;
  // Do not use TNetEncoding.Base64 here, because it may break long line
  LBase64 := TBase64Encoding.Create(0, '');
  try
    LAuthValue := BASIC_AUTH_PREFIX + LBase64.Encode(aUsername + ':' + aPassword);
  finally
    LBase64.Free;
  end;
  AddHeader(AUTHORIZATION_HEADER, LAuthValue, True)
end;

function TMVCRESTClient.SetBearerAuthorization(const aToken: string): IMVCRESTClient;
begin
  Result := Self;
  AddHeader(AUTHORIZATION_HEADER, BEARER_AUTH_PREFIX + aToken, True);
end;

{ TMVCRESTResponse }

function TMVCRESTResponse.Content: string;
begin
  Result := fContent;
end;

function TMVCRESTResponse.ContentEncoding: string;
begin
  Result := fContentEncoding;
end;

function TMVCRESTResponse.ContentLength: Integer;
begin
  Result := fContentLength;
end;

function TMVCRESTResponse.ContentType: string;
begin
  Result := fContentType;
end;

function TMVCRESTResponse.Cookies: TCookies;
begin
  Result := fCookies;
end;

constructor TMVCRESTResponse.Create(aRESTResponse: TCustomRESTResponse);
begin
  inherited Create;
  fHeaders := TStringList.Create;
  SetLength(fRawBytes, 0);
  fCookies := TCookies.Create;

  FillRESTResponse(aRESTResponse);
end;

destructor TMVCRESTResponse.Destroy;
begin
  SetLength(fRawBytes, 0);
  fHeaders.Free;
  fCookies.Free;
  inherited Destroy;
end;

function TMVCRESTResponse.ErrorMessage: string;
begin
  Result := fErrorMessage;
end;

procedure TMVCRESTResponse.FillRESTResponse(aRESTResponse: TCustomRESTResponse);
begin
  fSuccess := aRESTResponse.Status.Success;
  fStatusCode := aRESTResponse.StatusCode;
  fStatusText := aRESTResponse.StatusText;
  fErrorMessage := aRESTResponse.ErrorMessage;
  fHeaders.Assign(aRESTResponse.Headers);
{$IF defined(SYDNEYORBETTER)}
  fCookies.AddRange(aRESTResponse.Cookies.ToArray);
{$ENDIF}
  fServer := aRESTResponse.Server;
  fFullRequestURI := aRESTResponse.FullRequestURI;
  fContent := aRESTResponse.Content;
  fRawBytes := aRESTResponse.RawBytes;
  fContentType := aRESTResponse.ContentType;
  fContentEncoding := aRESTResponse.ContentEncoding;
  fContentLength := aRESTResponse.ContentLength;
end;

function TMVCRESTResponse.FullRequestURI: string;
begin
  Result := fFullRequestURI;
end;

function TMVCRESTResponse.HeaderByName(const aName: string): string;
begin
  Result := fHeaders.Values[aName];
end;

function TMVCRESTResponse.Headers: TStrings;
begin
  Result := fHeaders;
end;

function TMVCRESTResponse.RawBytes: TBytes;
begin
  Result := fRawBytes;
end;

procedure TMVCRESTResponse.SaveContentToFile(const aFileName: string);
var
  lStream: TMemoryStream;
begin
  lStream := TMemoryStream.Create;
  try
    lStream.Write(fRawBytes, Length(fRawBytes));
    lStream.Position := 0;
    lStream.SaveToFile(aFileName);
  finally
    lStream.Free;
  end;
end;

procedure TMVCRESTResponse.SaveContentToStream(aStream: TStream);
begin
  if aStream = nil then
    raise EMVCRESTClientException.Create('Stream not assigned!');

  aStream.Write(fRawBytes, Length(fRawBytes));
end;

function TMVCRESTResponse.Server: string;
begin
  Result := fServer;
end;

{$IF not defined(SYDNEYORBETTER)}
procedure TMVCRESTResponse.SetCookies(aCookies: TArray<TCookie>);
var
  i: Integer;
  lCookies: string;
begin
  fCookies.AddRange(aCookies);

  if (fHeaders.IndexOfName(HEADER_RESPONSE_COOKIES) = -1) and (fCookies.Count > 0) then
  begin
    lCookies := '';
    for i := 0 to fCookies.Count - 1 do
      lCookies := lCookies + '; ' + fCookies[i].ToString;
    fHeaders.Add(HEADER_RESPONSE_COOKIES + '=' + lCookies.Substring(2));
  end;
end;
{$ENDIF}

function TMVCRESTResponse.StatusCode: Integer;
begin
  Result := fStatusCode;
end;

function TMVCRESTResponse.StatusText: string;
begin
  Result := fStatusText;
end;

function TMVCRESTResponse.Success: Boolean;
begin
  Result := fSuccess;
end;

end.
