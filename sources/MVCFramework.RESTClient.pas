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

interface

uses
  System.Classes,
  System.SysUtils,
  System.Net.HttpClient,
  System.Net.Mime,
  System.Net.URLClient,
  System.Generics.Collections,
  System.Rtti,
  System.TypInfo,
  MVCFramework.Commons,
  MVCFramework.RESTClient.Indy,
  MVCFramework.RESTClient.Intf,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Commons,
  Data.DB;

  { REST.Client }
  { MVCFramework.JSONRPC.Client }

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
  /// Provides access to delphi RESTClient library types without the need to use the REST.Types unit.
  /// </summary>
  TCookie = System.Net.HttpClient.TCookie;
  TCookies = System.Net.HttpClient.TCookies;
  {$IF defined(TOKYOORBETTER)}
  THTTPSecureProtocol = System.Net.HttpClient.THTTPSecureProtocol;
  THTTPSecureProtocols = System.Net.HttpClient.THTTPSecureProtocols;
  {$ENDIF}

  TMVCRESTClient = class(TInterfacedObject, IMVCRESTClient)
  private
    fHTTPClient: THTTPClient;
    fBaseURL: string;
    fResource: string;
    fProxySettings: TProxySettings;
    fHeaders: TList<TNetHeader>;
    fPathParams: TList<TNameValuePair>;
    fQueryStringParams: TList<TNameValuePair>;
    fRawBody: TStringStream;
    fBodyURLEncoded: TList<TNameValuePair>;
    fBodyFormData: TMultipartFormData;
    fSerializer: IMVCSerializer;
    fRttiContext: TRttiContext;
    function GetBodyFormData: TMultipartFormData;
    function GetContentTypeCharset(const aContentType: string): string;
    function ObjectIsList(aObject: TObject): Boolean;
    function SerializeObject(aObject: TObject): string;
    procedure SetContentType(const aContentType: string);
    function GetFullURL: string;
    procedure DoApplyPathParams(var aURL: string);
    procedure DoApplyQueryParams(var aURL: string);
    procedure DoApplyHeaders;
    procedure DoEncodeURL(var aURL: string);

    function ExecuteRequest(const aMethod: TMVCHTTPMethodType): IMVCRESTResponse;
  public
    constructor Create;
    destructor Destroy; override;

    class function New: IMVCRESTClient;

    { IMVCRESTClient }

    function BaseURL(const aBaseURL: string): IMVCRESTClient; overload;
    function BaseURL(const aHost: string; const aPort: Integer): IMVCRESTClient; overload;
    function BaseURL: string; overload;

    function ProxyServer(const aProxyServer: string): IMVCRESTClient; overload;
    function ProxyServer: string; overload;
    function ProxyPort(const aProxyPort: Integer): IMVCRESTClient; overload;
    function ProxyPort: Integer; overload;
    function ProxyUsername(const aProxyUsername: string): IMVCRESTClient; overload;
    function ProxyUsername: string; overload;
    function ProxyPassword(const aProxyPassword: string): IMVCRESTClient; overload;
    function ProxyPassword: string; overload;

// {$IF defined(TOKYOORBETTER)}
    function SecureProtocols(const aSecureProtocols: THTTPSecureProtocols): IMVCRESTClient; overload;
    function SecureProtocols: THTTPSecureProtocols; overload;
// {$ENDIF}

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
    function AddHeader(const aName, aValue: string): IMVCRESTClient; overload;
    function HeaderValue(const aName: string): string;

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
    function MaxRedirects(const aMaxRedirects: Integer): IMVCRESTClient; overload;
    function MaxRedirects: Integer; overload;

    function Resource(const aResource: string): IMVCRESTClient; overload;
    function Resource: string; overload;

    /// <summary>
    /// Add a body to the requisition.
    /// </summary>
    /// <param name="aBody">
    /// Body in string format.
    /// </param>
    /// <param name="aContentType">
    /// Body content type.
    /// </param>
    function AddBody(const aBody: string; const aContentType: string = ''): IMVCRESTClient; overload;
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
    function AddBody(aBodyStream: TStream; const aOwnsStream: Boolean = True;
      const aContentType: string = ''): IMVCRESTClient; overload;
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
    function AddFile(const aName, aFileName: string; const aContentType: string = ''): IMVCRESTClient; overload;
    function AddFile(const aFileName: string; const aContentType: string = ''): IMVCRESTClient; overload;

    function AddBodyFieldFormData(const aName, aValue: string): IMVCRESTClient; overload;
    function AddBodyFieldFormData(const aName: string; aStreamValue: TStream;
      const aContentType: string = ''): IMVCRESTClient; overload;

    /// <summary>
    ///   Add a field to the x-www-form-urlencoded body. You must set ContentType to application/x-www-form-urlencoded
    /// </summary>
    function AddBodyFieldURLEncoded(const aName, aValue: string): IMVCRESTClient;

    function ClearBody: IMVCRESTClient;

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
      const aSynchronized: Boolean = False): IMVCRESTClient;

    /// <summary>
    /// Execute a Get request.
    /// </summary>
    function Get(const aResource: string): IMVCRESTResponse; overload;
    function Get: IMVCRESTResponse; overload;

    /// <summary>
    /// Execute a Post request.
    /// </summary>
    /// <param name="aResource">
    /// Resource path
    /// </param>
    /// <param name="aBody">
    /// Object to be serialized. It can be a simple object or a list of objects (TObjectList &lt;T&gt;)
    /// </param>
    /// <param name="aOwnsBody">
    /// If OwnsBody is true, Body will be destroyed by IMVCRESTClient. <br />
    /// </param>
    function Post(const aResource: string; aBody: TObject; const aOwnsBody: Boolean = True): IMVCRESTResponse; overload;
    function Post(const aResource: string; const aBody: string = ''; const aDoNotEncode: Boolean = False;
      const aContentType: string = TMVCMediaType.APPLICATION_JSON): IMVCRESTResponse; overload;
    function Post: IMVCRESTResponse; overload;

    /// <summary>
    /// Execute a Patch request.
    /// </summary>
    function Patch(const aResource: string; aBody: TObject;
      const aOwnsBody: Boolean = True): IMVCRESTResponse; overload;
    function Patch(const aResource: string; const aBody: string = ''; const aDoNotEncode: Boolean = False;
      const aContentType: string = TMVCMediaType.APPLICATION_JSON): IMVCRESTResponse; overload;
    function Patch: IMVCRESTResponse; overload;

    /// <summary>
    /// Execute a Put request.
    /// </summary>
    function Put(const aResource: string; aBody: TObject; const aOwnsBody: Boolean = True): IMVCRESTResponse; overload;
    function Put(const aResource: string; const aBody: string = ''; const aDoNotEncode: Boolean = False;
      const aContentType: string = TMVCMediaType.APPLICATION_JSON): IMVCRESTResponse; overload;
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
    function DataSetUpdate(const aResource, aKeyValue: string; aDataSet: TDataSet;
      const aIgnoredFields: TMVCIgnoredList = []; const aNameCase: TMVCNameCase = ncAsIs): IMVCRESTResponse;
    /// <summary>
    /// Delete the current dataset record by executing a delete request.
    /// </summary>
    function DataSetDelete(const aResource, aKeyValue: string): IMVCRESTResponse;

    /// <summary>
    /// Register a custom serializer to the RESTClient serializer.
    /// </summary>
    function RegisterTypeSerializer(const aTypeInfo: PTypeInfo; aInstance: IMVCTypeSerializer): IMVCRESTClient;
  end;

implementation

uses
  System.NetConsts,
  System.NetEncoding,
  MVCFramework.Serializer.JsonDataObjects;

const
  PATH_UNSAFE_CHARS: TURLEncoding.TUnsafeChars = [Ord('"'), Ord('<'), Ord('>'), Ord('^'), Ord('`'), Ord('{'),
    Ord('}'), Ord('|'), Ord('/'), Ord('\'), Ord('?'), Ord('#'), Ord('+'), Ord('.')];
  QUERY_NAME_UNSAFE_CHARS: TURLEncoding.TUnsafeChars = [Ord('"'), Ord(''''), Ord(':'), Ord(';'), Ord('<'), Ord('='),
    Ord('>'), Ord('@'), Ord('['), Ord(']'), Ord('^'), Ord('`'), Ord('{'), Ord('}'), Ord('|'), Ord('/'), Ord('\'),
    Ord('?'), Ord('#'), Ord('&'), Ord('!'), Ord('$'), Ord('('), Ord(')'), Ord(','), Ord('~'), Ord(' '), Ord('*'),
    Ord('+')];


{ TMVCRESTClient }

function TMVCRESTClient.Accept: string;
begin

end;

function TMVCRESTClient.Accept(const aAccept: string): IMVCRESTClient;
begin
  Result := Self;
  fHTTPClient.Accept := aAccept;
end;

function TMVCRESTClient.AcceptCharset: string;
begin
  Result := fHTTPClient.AcceptCharSet;
end;

function TMVCRESTClient.AcceptCharset(const aAcceptCharset: string): IMVCRESTClient;
begin
  Result := Self;
  fHTTPClient.AcceptCharSet := aAcceptCharset;
end;

function TMVCRESTClient.AcceptEncoding(const aAcceptEncoding: string): IMVCRESTClient;
begin
  Result := Self;
  fHTTPClient.AcceptEncoding := aAcceptEncoding;
end;

function TMVCRESTClient.AcceptEncoding: string;
begin
  Result := fHTTPClient.AcceptEncoding;
end;

function TMVCRESTClient.AddBody(const aBody: string; const aContentType: string): IMVCRESTClient;
var
  lContentCharset: string;
  lEncoding: TEncoding;
  lBytes: TArray<Byte>;
begin
  Result := Self;

  SetContentType(aContentType);

  lContentCharset := GetContentTypeCharset(aContentType);
  if lContentCharset.IsEmpty then
  begin
    lContentCharset := TMVCCharSet.UTF_8;
  end;
  lEncoding := TEncoding.GetEncoding(lContentCharset);

  fRawBody.Clear;
  lBytes := TEncoding.Convert(TEncoding.Default, lEncoding, TEncoding.Default.GetBytes(aBody));
  fRawBody.WriteData(lBytes, Length(lBytes));
end;

function TMVCRESTClient.AddBody(aBodyStream: TStream; const aOwnsStream: Boolean;
  const aContentType: string): IMVCRESTClient;
begin
  Result := Self;

  Assert(ABodyStream is TStringStream);

  SetContentType(aContentType);

  fRawBody.Clear;
  fRawBody.CopyFrom(aBodyStream, 0);

  if aOwnsStream then
    aBodyStream.Free;
end;

function TMVCRESTClient.AddBody(aBodyObject: TObject; const aOwnsObject: Boolean): IMVCRESTClient;
begin
  Result := AddBody(SerializeObject(aBodyObject), TMVCMediaType.APPLICATION_JSON);

  if aOwnsObject then
    aBodyObject.Free;
end;

function TMVCRESTClient.AddBodyFieldFormData(const aName, aValue: string): IMVCRESTClient;
begin
  Result := Self;
  GetBodyFormData.AddField(aName, aValue);
  SetContentType(TMVCMediaType.MULTIPART_FORM_DATA);
end;

function TMVCRESTClient.AddBodyFieldFormData(const aName: string; aStreamValue: TStream;
  const aContentType: string): IMVCRESTClient;
begin
  Result := Self;
  GetBodyFormData.AddStream(aName, aStreamValue, aContentType);
  SetContentType(TMVCMediaType.MULTIPART_FORM_DATA);
end;

function TMVCRESTClient.AddBodyFieldURLEncoded(const aName, aValue: string): IMVCRESTClient;
begin
  Result := Self;
  fBodyURLEncoded.Add(TNameValuePair.Create(aName, aValue));
  SetContentType(TMVCMediaType.APPLICATION_FORM_URLENCODED);
end;

function TMVCRESTClient.AddCookie(const aName, aValue: string): IMVCRESTClient;
begin

end;

function TMVCRESTClient.AddFile(const aFileName, aContentType: string): IMVCRESTClient;
begin
  Result := AddFile('file', aFileName, aContentType);
end;

function TMVCRESTClient.AddFile(const aName, aFileName, aContentType: string): IMVCRESTClient;
begin
  Result := Self;
  GetBodyFormData.AddFile(aName, aFileName, aContentType);
  SetContentType(TMVCMediaType.MULTIPART_FORM_DATA);
end;

function TMVCRESTClient.AddHeader(const aName, aValue: string): IMVCRESTClient;
begin
  Result := Self;
  fHeaders.Add(TNetHeader.Create(aName, aValue));
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: Double): IMVCRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddPathParam(const aName, aValue: string): IMVCRESTClient;
begin
  Result := Self;
  fPathParams.Add(TNameValuePair.Create(aName, aValue));
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: TDate): IMVCRESTClient;
begin
  Result := AddPathParam(aName, DateToISODate(aValue));
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: TTime): IMVCRESTClient;
begin
  Result := AddPathParam(aName, TimeToISOTime(aValue));
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: TDateTime): IMVCRESTClient;
begin
  Result := AddPathParam(aName, DateTimeToISOTimeStamp(aValue));
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: Int64): IMVCRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: TGUID): IMVCRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: Integer): IMVCRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: Double): IMVCRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: TTime): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, TimeToISOTime(aValue));
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: TDate): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, DateToISODate(aValue));
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: TDateTime): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, DateTimeToISOTimeStamp(aValue));
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: TGUID): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: Int64): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddQueryStringParam(const aName, aValue: string): IMVCRESTClient;
begin
  Result := Self;
  fQueryStringParams.Add(TNameValuePair.Create(aName, aValue));
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: Integer): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AllowCookies: Boolean;
begin
  Result := fHTTPClient.AllowCookies;
end;

function TMVCRESTClient.AllowCookies(const aAllowCookies: Boolean): IMVCRESTClient;
begin
  Result := Self;
  fHTTPClient.AllowCookies := aAllowCookies;
end;

function TMVCRESTClient.Async(aCompletionHandler: TProc<IMVCRESTResponse>;
  aCompletionHandlerWithError: TProc<Exception>; const aSynchronized: Boolean): IMVCRESTClient;
begin

end;

function TMVCRESTClient.BaseURL(const aBaseURL: string): IMVCRESTClient;
begin
  Result := Self;

  fBaseURL := aBaseURL;
  if not fBaseURL.Contains('://') then
    fBaseURL := 'http://' + fBaseURL;
end;

function TMVCRESTClient.BaseURL(const aHost: string; const aPort: Integer): IMVCRESTClient;
begin
  Result := BaseURL(aHost + ':' + aPort.ToString);
end;

function TMVCRESTClient.BaseURL: string;
begin
  Result := fBaseURL;
end;

function TMVCRESTClient.ClearAllParams: IMVCRESTClient;
begin
  Result := Self;
  ClearHeaders;
  ClearPathParams;
  ClearQueryParams;
  ClearBody;
end;

function TMVCRESTClient.ClearBody: IMVCRESTClient;
begin
  fRawBody.Clear;
  if Assigned(fBodyFormData) then
    FreeAndNil(fBodyFormData);

  fBodyURLEncoded.Clear;
end;

function TMVCRESTClient.ClearCookies: IMVCRESTClient;
begin
  fHTTPClient.CookieManager.Clear;
end;

function TMVCRESTClient.ClearHeaders: IMVCRESTClient;
begin
  Result := Self;
  fHeaders.Clear;
end;

function TMVCRESTClient.ClearPathParams: IMVCRESTClient;
begin
  Result := Self;
  fPathParams.Clear;
end;

function TMVCRESTClient.ClearQueryParams: IMVCRESTClient;
begin
  Result := Self;
  fQueryStringParams.Clear;
end;

function TMVCRESTClient.ConnectTimeout: Integer;
begin
  Result := fHTTPClient.ConnectionTimeout;
end;

function TMVCRESTClient.ConnectTimeout(const aConnectTimeout: Integer): IMVCRESTClient;
begin
  Result := Self;
  fHTTPClient.ConnectionTimeout := aConnectTimeout;
end;

constructor TMVCRESTClient.Create;
begin
  inherited Create;

  fHTTPClient := THTTPClient.Create;
  fBaseURL := '';
  fResource := '';
  fProxySettings := TProxySettings.Create('', 0);
  fHeaders := TList<TNetHeader>.Create;
  fPathParams := TList<TNameValuePair>.Create;
  fQueryStringParams := TList<TNameValuePair>.Create;
  fRawBody := TStringStream.Create;
  fBodyURLEncoded := TList<TNameValuePair>.Create;
  fBodyFormData := nil;
  fSerializer := TMVCJsonDataObjectsSerializer.Create;
  fRttiContext := TRttiContext.Create;
end;

function TMVCRESTClient.DataSetDelete(const aResource, aKeyValue: string): IMVCRESTResponse;
begin

end;

function TMVCRESTClient.DataSetInsert(const aResource: string; aDataSet: TDataSet;
  const aIgnoredFields: TMVCIgnoredList; const aNameCase: TMVCNameCase): IMVCRESTResponse;
begin

end;

function TMVCRESTClient.DataSetUpdate(const aResource, aKeyValue: string; aDataSet: TDataSet;
  const aIgnoredFields: TMVCIgnoredList; const aNameCase: TMVCNameCase): IMVCRESTResponse;
begin

end;

function TMVCRESTClient.Delete(const aResource: string): IMVCRESTResponse;
begin

end;

function TMVCRESTClient.Delete: IMVCRESTResponse;
begin

end;

destructor TMVCRESTClient.Destroy;
begin
  FreeAndNil(fHTTPClient);
  FreeAndNil(fHeaders);
  FreeAndNil(fPathParams);
  FreeAndNil(fQueryStringParams);
  FreeAndNil(fBodyURLEncoded);
  if Assigned(fBodyFormData) then
    FreeAndNil(fBodyFormData);

  fSerializer := nil;
  fRttiContext.Free;
  inherited;
end;

procedure TMVCRESTClient.DoApplyHeaders;
var
  lHeader: TNetHeader;
begin
  for lHeader in fHeaders do
  begin
    fHTTPClient.CustomHeaders[lHeader.Name] := lHeader.Value;
  end;
end;

procedure TMVCRESTClient.DoApplyPathParams(var aURL: string);
var
  lPathParam: TNameValuePair;
  lReplace: string;
  lEncodedParam: string;
begin
  for lPathParam in fPathParams do
  begin
    lReplace := '{' + lPathParam.Name + '}';
    lEncodedParam := TNetEncoding.URL.Encode(lPathParam.Value, PATH_UNSAFE_CHARS,
      [TURLEncoding.TEncodeOption.EncodePercent]);

    aURL := aURL.Replace(lReplace, lEncodedParam, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

procedure TMVCRESTClient.DoApplyQueryParams(var aURL: string);
var
  lQueryParam: TNameValuePair;
  lName: string;
  lValue: string;
  lConcat: string;
begin
  for lQueryParam in fQueryStringParams do
  begin
    lName := TNetEncoding.URL.Encode(lQueryParam.Name, QUERY_NAME_UNSAFE_CHARS,
      [TURLEncoding.TEncodeOption.EncodePercent]);
    lValue := TNetEncoding.URL.EncodeForm(lQueryParam.Value);

    if aURL.Contains('?') then
      lConcat := '&'
    else
      lConcat := '?';

    aURL := aURL + lConcat + lName + '=' + lValue;
  end;
end;

procedure TMVCRESTClient.DoEncodeURL(var aURL: string);
begin
  // It is necessary to encode the dots because the HTTPClient removes dotted URL segments.
  // See https://tools.ietf.org/html/rfc3986#section-5.2.4
  aURL := aURL.Replace('\', '/', [rfReplaceAll]);
  aURL := aURL.Replace('../', '%2E%2E/', [rfReplaceAll]);
  aURL := aURL.Replace('./', '%2E/', [rfReplaceAll]);

  aURL := TURI.Create(aURL).Encode;
end;

function TMVCRESTClient.ExecuteRequest(const aMethod: TMVCHTTPMethodType): IMVCRESTResponse;
var
  lURL: string;
begin
  lURL := GetFullURL;
  DoApplyPathParams(lURL);
  DoApplyQueryParams(lURL);
  DoEncodeURL(lURL);
  DoApplyHeaders;


  case aMethod of
    httpGET: ;
    httpPOST: ;
    httpPUT: ;
    httpDELETE: ;
    httpHEAD: ;
    httpOPTIONS: ;
    httpPATCH: ;
    httpTRACE: ;
  end;
end;

function TMVCRESTClient.Get: IMVCRESTResponse;
begin

end;

function TMVCRESTClient.GetBodyFormData: TMultipartFormData;
begin
  if not Assigned(fBodyFormData) then
  begin
    fBodyFormData := TMultipartFormData.Create;
  end;
  Result := fBodyFormData;
end;

function TMVCRESTClient.GetContentTypeCharset(const aContentType: string): string;
var
  lContentType: string;
begin
  SplitContentMediaTypeAndCharset(aContentType, lContentType, Result);
end;

function TMVCRESTClient.GetFullURL: string;
var
  lResource: string;
begin
  Result := fBaseURL;

  lResource := fResource;
  if not lResource.IsEmpty then
  begin
    if not Result.EndsWith('/') and
      not (lResource.StartsWith('/') or lResource.StartsWith('?') or lResource.StartsWith('#')) then
    begin
      Result := Result + '/';
    end;

    Result := Result + lResource;
  end;
end;

function TMVCRESTClient.Get(const aResource: string): IMVCRESTResponse;
begin

end;

function TMVCRESTClient.HandleRedirects(const aHandleRedirects: Boolean): IMVCRESTClient;
begin
  Result := Self;
  fHTTPClient.HandleRedirects := aHandleRedirects;
end;

function TMVCRESTClient.HandleRedirects: Boolean;
begin
  Result := fHTTPClient.HandleRedirects;
end;

function TMVCRESTClient.HeaderValue(const aName: string): string;
var
  lHeader: TNetHeader;
begin
  Result := '';
  for lHeader in fHeaders do
  begin
    if SameText(lHeader.Name, aName) then
      Exit(lHeader.Value)
  end;
end;

function TMVCRESTClient.MaxRedirects(const aMaxRedirects: Integer): IMVCRESTClient;
begin
  Result := Self;
  fHTTPClient.MaxRedirects := aMaxRedirects;
end;

function TMVCRESTClient.MaxRedirects: Integer;
begin
  Result := fHTTPClient.MaxRedirects;
end;

class function TMVCRESTClient.New: IMVCRESTClient;
begin
  Result := TMVCRESTClient.Create;
end;

function TMVCRESTClient.ObjectIsList(aObject: TObject): Boolean;
begin
  Result := fRttiContext.GetType(aObject.ClassType).GetMethod('GetEnumerator') <> nil;
end;

function TMVCRESTClient.Patch(const aResource, aBody: string; const aDoNotEncode: Boolean;
  const aContentType: string): IMVCRESTResponse;
begin

end;

function TMVCRESTClient.Patch: IMVCRESTResponse;
begin

end;

function TMVCRESTClient.Patch(const aResource: string; aBody: TObject; const aOwnsBody: Boolean): IMVCRESTResponse;
begin

end;

function TMVCRESTClient.Post(const aResource, aBody: string; const aDoNotEncode: Boolean;
  const aContentType: string): IMVCRESTResponse;
begin

end;

function TMVCRESTClient.Post(const aResource: string; aBody: TObject; const aOwnsBody: Boolean): IMVCRESTResponse;
begin

end;

function TMVCRESTClient.Post: IMVCRESTResponse;
begin

end;

function TMVCRESTClient.ProxyPassword: string;
begin
  Result := fProxySettings.Password;
end;

function TMVCRESTClient.ProxyPassword(const aProxyPassword: string): IMVCRESTClient;
begin
  Result := Self;
  fProxySettings.Password := aProxyPassword;
end;

function TMVCRESTClient.ProxyPort: Integer;
begin
  Result := fProxySettings.Port;
end;

function TMVCRESTClient.ProxyPort(const aProxyPort: Integer): IMVCRESTClient;
begin
  Result := Self;
  fProxySettings.Port := aProxyPort;
end;

function TMVCRESTClient.ProxyServer(const aProxyServer: string): IMVCRESTClient;
begin
  Result := Self;
  fProxySettings.Host := aProxyServer;
end;

function TMVCRESTClient.ProxyServer: string;
begin
  Result := fProxySettings.Host;
end;

function TMVCRESTClient.ProxyUsername(const aProxyUsername: string): IMVCRESTClient;
begin
  Result := Self;
  fProxySettings.UserName := aProxyUsername;
end;

function TMVCRESTClient.ProxyUsername: string;
begin
  Result := fProxySettings.UserName;
end;

function TMVCRESTClient.Put: IMVCRESTResponse;
begin

end;

function TMVCRESTClient.Put(const aResource, aBody: string; const aDoNotEncode: Boolean;
  const aContentType: string): IMVCRESTResponse;
begin

end;

function TMVCRESTClient.Put(const aResource: string; aBody: TObject; const aOwnsBody: Boolean): IMVCRESTResponse;
begin

end;

function TMVCRESTClient.ReadTimeout: Integer;
begin
  Result := fHTTPClient.ResponseTimeout;
end;

function TMVCRESTClient.ReadTimeout(const aReadTimeout: Integer): IMVCRESTClient;
begin
  Result := Self;
  fHTTPClient.ResponseTimeout := aReadTimeout;
end;

function TMVCRESTClient.RegisterTypeSerializer(const aTypeInfo: PTypeInfo;
  aInstance: IMVCTypeSerializer): IMVCRESTClient;
begin
  Result := Self;
  fSerializer.RegisterTypeSerializer(aTypeInfo, aInstance);
end;

function TMVCRESTClient.Resource(const aResource: string): IMVCRESTClient;
begin
  Result := Self;
  fResource := aResource;
end;

function TMVCRESTClient.Resource: string;
begin
  Result := fResource;
end;

function TMVCRESTClient.SecureProtocols: THTTPSecureProtocols;
begin
  Result := fHTTPClient.SecureProtocols;
end;

function TMVCRESTClient.SerializeObject(aObject: TObject): string;
begin
  if ObjectIsList(aObject) then
    Result := fSerializer.SerializeCollection(aObject)
  else
    Result := fSerializer.SerializeObject(aObject);
end;

function TMVCRESTClient.SecureProtocols(const aSecureProtocols: THTTPSecureProtocols): IMVCRESTClient;
begin
  Result := Self;
  fHTTPClient.SecureProtocols := aSecureProtocols;
end;

function TMVCRESTClient.SetBasicAuthorization(const aUsername, aPassword: string): IMVCRESTClient;
begin

end;

function TMVCRESTClient.SetBearerAuthorization(const aToken: string): IMVCRESTClient;
begin

end;

procedure TMVCRESTClient.SetContentType(const aContentType: string);
begin
  fHTTPClient.ContentType := aContentType;
end;

function TMVCRESTClient.UserAgent(const aUserAgent: string): IMVCRESTClient;
begin
  Result := Self;
  fHTTPClient.UserAgent := aUserAgent;
end;

function TMVCRESTClient.UserAgent: string;
begin
  Result := fHTTPClient.UserAgent;
end;

end.
