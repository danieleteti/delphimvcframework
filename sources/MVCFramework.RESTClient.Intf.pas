// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.RESTClient.Intf;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.TypInfo,
  System.Net.HttpClient,
  System.Net.URLClient,
  MVCFramework.Serializer.Intf,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  Data.DB,
  JsonDataObjects;

type
  IMVCRESTResponse = interface;

  TNeedClientCertificateProc = reference to procedure(const aSender: TObject; const aRequest: TURLRequest;
    const aCertificateList: TCertificateList; var aIndex: Integer);
  TValidateServerCertificateProc = reference to procedure(const aSender: TObject; const aRequest: TURLRequest;
    const aCertificate: TCertificate; var aAccepted: Boolean);
  TBeforeRequestProc = reference to procedure (aRequest: IHTTPRequest);
  TRequestCompletedProc = reference to procedure (aResponse: IHTTPResponse; var aHandled: Boolean);
  TResponseCompletedProc = reference to procedure(aResponse: IMVCRESTResponse);

  IMVCRESTClient = interface
    ['{592BC90F-B825-4B3B-84A7-6CA3927BAD69}']

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
    function ProxyScheme(const aProxyScheme: string): IMVCRESTClient; overload;
    function ProxyScheme: string; overload;


{$IF defined(TOKYOORBETTER)}
    function SecureProtocols(const aSecureProtocols: THTTPSecureProtocols): IMVCRESTClient; overload;
    function SecureProtocols: THTTPSecureProtocols; overload;
{$ENDIF}

    /// <summary>
    /// Method called when a ClientCertificate is needed.
    /// </summary>
    function SetNeedClientCertificateProc(aNeedClientCertificateProc: TNeedClientCertificateProc): IMVCRESTClient;

    /// <summary>
    /// Add a custom SSL certificate validation. By default all certificates are accepted.
    /// </summary>
    function SetValidateServerCertificateProc(aValidateCertificateProc: TValidateServerCertificateProc): IMVCRESTClient;

    /// <summary>
    /// Executes before send the request
    /// </summary>
    function SetBeforeRequestProc(aBeforeRequestProc: TBeforeRequestProc): IMVCRESTClient;

    /// <summary>
    /// Executes after send the request
    /// </summary>
    function SetRequestCompletedProc(aRequestCompletedProc: TRequestCompletedProc): IMVCRESTClient;

    /// <summary>
    /// Executes after the response is processed.
    /// </summary>
    function SetResponseCompletedProc(aResponseCompletedProc: TResponseCompletedProc): IMVCRESTClient;

    ///<summary>
    /// Set the client certificate for the request</summary>
    /// </summary>
    function SetClientCertificate(const aCertStream: TStream; const aPassword: string): IMVCRESTClient; overload;

{$IF defined(TOKYOORBETTER)}
    /// <summary>
    /// Set the path containing a client certificate for the request (iOS, Linux, Windows, Android).
    /// Note, on Android the Path is certificate fingerprint or imported name, not a file path.
    /// Password is not used.
    /// </summary>
    function SetClientCertificate(const aCertPath, aPassword: string): IMVCRESTClient; overload;
{$ENDIF}

    /// <summary>
    /// Clears all parameters (headers, body, path params and query params). This method is executed after each
    /// request is completed.
    /// </summary>
    function ClearAllParams: IMVCRESTClient;

{$IF defined(BERLINORBETTER)}
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
{$ENDIF}

    /// <summary>
    /// Add basic authorization header. Authorization = Basic &lt;Username:Password&gt; (encoded in Base64)
    /// </summary>
    function SetBasicAuthorization(const aUsername, aPassword: string): IMVCRESTClient;

    /// <summary>
    /// Add bearer authorization header. Authorization = Bearer &lt;Token&gt;
    /// </summary>
    function SetBearerAuthorization(const aAccessToken: string): IMVCRESTClient;

    /// <summary>
    /// Returns the stored authorization. Includes Basic or Bearer prefix
    /// </summary>
    function Authorization: string;

    /// <summary>
    /// Removes the authorization header defined in <see cref="MVCFramework.RESTClient.Intf|IMVCRESTClient.SetBasicAuthorization(string,string)">
    /// SetBasicAuthorization</see> or <see cref="MVCFramework.RESTClient.Intf|IMVCRESTClient.SetBearerAuthorization(string)">
    /// SetBearerAuthorization</see>
    /// </summary>
    function ClearAuthorization: IMVCRESTClient;

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
    function Headers: TNameValueArray;

    /// <summary>
    /// Clears all headers.
    /// </summary>
    function ClearHeaders: IMVCRESTClient;

    function AllowCookies(const aAllowCookies: Boolean): IMVCRESTClient; overload;
    function AllowCookies: Boolean; overload;

    /// <summary>
    /// Add DMVC session cookie
    /// </summary>
    function SessionId(const aSessionId: string): IMVCRESTClient; overload;
    function SessionId: string; overload;

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

    function UserAgent(const aUserAgent: string): IMVCRESTClient; overload;
    function UserAgent: string; overload;

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
{$IF defined(RIOORBETTER)}
    function AddFile(const aName: string; aFileStreamValue: TStream; const aFileName: string = '';
      const aContentType: string = ''): IMVCRESTClient; overload;
{$ENDIF}
    function AddBodyFieldFormData(const aName, aValue: string): IMVCRESTClient; overload;
{$IF defined(RIOORBETTER)}
    function AddBodyFieldFormData(const aName: string; aStreamValue: TStream;
      const aContentType: string = ''): IMVCRESTClient; overload;
{$ENDIF}
/// <summary>
    /// Add a field to the x-www-form-urlencoded body. You must set ContentType to application/x-www-form-urlencoded
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
    /// Execute a Get request. The GET method requests a representation of the specified resource.
    /// Requests using GET should only retrieve data.
    /// Sending body/payload in a GET request may cause some existing implementations to
    /// reject the request — while not prohibited by the specification, the semantics
    /// are undefined. It is better to just avoid sending payloads in GET requests.
    /// </summary>
    function Get(const aResource: string): IMVCRESTResponse; overload;
    /// <summary>
    /// Execute a Get request. The GET method requests a representation of the specified resource.
    /// Requests using GET should only retrieve data.
    /// Sending body/payload in a GET request may cause some existing implementations to
    /// reject the request — while not prohibited by the specification, the semantics
    /// are undefined. It is better to just avoid sending payloads in GET requests.
    /// </summary>
    function Get: IMVCRESTResponse; overload;

    /// <summary>
    /// Execute a Head request. The HEAD method asks for a response identical
    /// to that of a GET request, but without the response body.
    /// </summary>
    function Head(const aResource: string): IMVCRESTResponse; overload;
    /// <summary>
    /// Execute a Head request. The HEAD method asks for a response identical
    /// to that of a GET request, but without the response body.
    /// </summary>
    function Head: IMVCRESTResponse; overload;

    /// <summary>
    /// Execute a Options request. The OPTIONS method is used to describe the communication options for the target resource.
    /// </summary>
    function Options(const aResource: string): IMVCRESTResponse; overload;
    /// <summary>
    /// Execute a Options request. The OPTIONS method is used to describe the communication options for the target resource.
    /// </summary>
    function Options: IMVCRESTResponse; overload;

    /// <summary>
    /// Execute a Post request. The POST method is used to submit an entity to the specified resource, often causing a change in state or side effects on the server.
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
    /// <summary>
    /// Execute a Post request. The POST method is used to submit an entity to the specified resource, often causing a change in state or side effects on the server.
    /// </summary>
    /// <param name="aResource">
    /// Resource path
    /// </param>
    /// <param name="aBody">
    /// Serialized data sent as body request. It can be a simple object or a list of objects (TObjectList &lt;T&gt;)
    /// </param>
    /// <param name="aContentType">
    /// Format of the body data. Must be one of the allowed media-types <br />
    /// </param>
    function Post(const aResource: string; const aBody: string = '';
      const aContentType: string = TMVCMediaType.APPLICATION_JSON): IMVCRESTResponse; overload;
    /// <summary>
    /// Execute a Post request. The POST method is used to submit an entity to the specified resource, often causing a change in state or side effects on the server.
    /// </summary>
    function Post: IMVCRESTResponse; overload;

    /// <summary>
    /// Execute a Patch request. The PATCH method is used to apply partial modifications to a resource.
    /// </summary>
    function Patch(const aResource: string; aBody: TObject;
      const aOwnsBody: Boolean = True): IMVCRESTResponse; overload;
    /// <summary>
    /// Execute a Patch request. The PATCH method is used to apply partial modifications to a resource.
    /// </summary>
    function Patch(const aResource: string; const aBody: string = '';
      const aContentType: string = TMVCMediaType.APPLICATION_JSON): IMVCRESTResponse; overload;
    /// <summary>
    /// Execute a Patch request. The PATCH method is used to apply partial modifications to a resource.
    /// </summary>
    function Patch: IMVCRESTResponse; overload;

    /// <summary>
    /// Execute a Put request. The PUT method replaces all current representations of the target resource with the request payload.
    /// </summary>
    function Put(const aResource: string; aBody: TObject; const aOwnsBody: Boolean = True): IMVCRESTResponse; overload;
    /// <summary>
    /// Execute a Put request. The PUT method replaces all current representations of the target resource with the request payload.
    /// </summary>
    function Put(const aResource: string; const aBody: string = '';
      const aContentType: string = TMVCMediaType.APPLICATION_JSON): IMVCRESTResponse; overload;
    /// <summary>
    /// Execute a Put request. The PUT method replaces all current representations of the target resource with the request payload.
    /// </summary>
    function Put: IMVCRESTResponse; overload;

    /// <summary>
    /// Execute a Delete request. The DELETE method deletes the specified resource.
    /// </summary>
    function Delete(const aResource: string): IMVCRESTResponse; overload;
    function Delete: IMVCRESTResponse; overload;

    /// <summary>
    /// Executes any type of HTTP request
    /// </summary>
    function Execute(const aMethod: TMVCHTTPMethodType; const aResource: string): IMVCRESTResponse; overload;
    function Execute(const aMethod: TMVCHTTPMethodType): IMVCRESTResponse; overload;

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
    /// Access the RESTClient serializer
    /// </summary>
    function Serializer: IMVCSerializer; overload;
    /// <summary>
    /// Add a serializer to the RESTClient
    /// </summary>
    function Serializer(const aSerializer: IMVCSerializer): IMVCRESTClient; overload;

    /// <summary>
    /// Register a custom serializer to the RESTClient serializer.
    /// </summary>
    function RegisterTypeSerializer(const aTypeInfo: PTypeInfo; aInstance: IMVCTypeSerializer): IMVCRESTClient;
  end;

  IMVCRESTResponse = interface
    ['{BF611B46-CCD1-47C7-8D8B-82EA0518896B}']

    /// <summary>
    ///   Success if StatusCode is &gt;= 200 and &lt; 300
    /// </summary>
    function Success: Boolean;
    function StatusCode: Integer;
    function StatusText: string;
    function Headers: TStrings;
    function HeaderValue(const aName: string): string;
    function Cookies: TCookies;
    function CookieByName(const aName: string; const RaiseExceptionIfNotFound: Boolean = False): TCookie;
    function Server: string;
    function ContentType: string;
    function ContentEncoding: string;
    function ContentLength: Integer;
    function Content: string;
    function ContentRawBytes: TBytes;
    procedure SaveContentToStream(aStream: TStream);
    procedure SaveContentToFile(const aFileName: string);
    function ToJSONObject: TJDOJsonObject;
    function ToJSONArray: TJDOJsonArray;
    procedure BodyFor(const aObject: TObject; const aRootNode: string = '');
    procedure BodyForListOf(const aObjectList: TObject; const aObjectClass: TClass; const aRootNode: string = '');
  end;

implementation

end.
