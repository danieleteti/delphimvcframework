// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
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

unit MVCFramework.HttpSysApi;

/// <summary>
/// Windows HTTP Server API (httpapi.dll) type translations and function imports.
/// This unit provides Delphi bindings for the Windows kernel-mode HTTP server stack.
/// All types and functions are Windows-only.
/// </summary>

{$I dmvcframework.inc}

interface

{$IFDEF MSWINDOWS}

uses
  Winapi.Windows, Winapi.WinSock;

const
  HTTPAPI_DLL = 'httpapi.dll';
  HTTP_INITIALIZE_SERVER = 1;
  HTTP_INITIALIZE_CONFIG = 2;
  HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY = 1;
  HTTP_SEND_RESPONSE_FLAG_DISCONNECT = $00000001;
  HTTP_SEND_RESPONSE_FLAG_MORE_DATA = $00000002;

type
  HTTP_SERVER_PROPERTY = (
    HttpServerAuthenticationProperty,
    HttpServerLoggingProperty,
    HttpServerQosProperty,
    HttpServerTimeoutsProperty,
    HttpServerQueueLengthProperty,
    HttpServerStateProperty,
    HttpServer503VerbosityProperty,
    HttpServerBindingProperty,
    HttpServerExtendedAuthenticationProperty,
    HttpServerListenEndpointProperty,
    HttpServerChannelBindProperty,
    HttpServerProtectionLevelProperty
  );

  HTTP_VERB = (
    HttpVerbUnparsed = 0,
    HttpVerbUnknown,
    HttpVerbInvalid,
    HttpVerbOPTIONS,
    HttpVerbGET,
    HttpVerbHEAD,
    HttpVerbPOST,
    HttpVerbPUT,
    HttpVerbDELETE,
    HttpVerbTRACE,
    HttpVerbCONNECT,
    HttpVerbTRACK,
    HttpVerbMOVE,
    HttpVerbCOPY,
    HttpVerbPROPFIND,
    HttpVerbPROPPATCH,
    HttpVerbMKCOL,
    HttpVerbLOCK,
    HttpVerbUNLOCK,
    HttpVerbSEARCH,
    HttpVerbMaximum
  );

  /// <summary>
  /// Known request header IDs matching HttpHeaderXxx constants in http.h
  /// </summary>
  HTTP_HEADER_ID = (
    HttpHeaderCacheControl = 0,
    HttpHeaderConnection,
    HttpHeaderDate,
    HttpHeaderKeepAlive,
    HttpHeaderPragma,
    HttpHeaderTrailer,
    HttpHeaderTransferEncoding,
    HttpHeaderUpgrade,
    HttpHeaderVia,
    HttpHeaderWarning,
    HttpHeaderAllow,
    HttpHeaderContentLength,
    HttpHeaderContentType,
    HttpHeaderContentEncoding,
    HttpHeaderContentLanguage,
    HttpHeaderContentLocation,
    HttpHeaderContentMd5,
    HttpHeaderContentRange,
    HttpHeaderExpires,
    HttpHeaderLastModified,
    HttpHeaderAccept,
    HttpHeaderAcceptCharset,
    HttpHeaderAcceptEncoding,
    HttpHeaderAcceptLanguage,
    HttpHeaderAuthorization,
    HttpHeaderCookie,
    HttpHeaderExpect,
    HttpHeaderFrom,
    HttpHeaderHost,
    HttpHeaderIfMatch,
    HttpHeaderIfModifiedSince,
    HttpHeaderIfNoneMatch,
    HttpHeaderIfRange,
    HttpHeaderIfUnmodifiedSince,
    HttpHeaderMaxForwards,
    HttpHeaderProxyAuthorization,
    HttpHeaderReferer,
    HttpHeaderRange,
    HttpHeaderTe,
    HttpHeaderTranslate,
    HttpHeaderUserAgent,
    HttpHeaderRequestMaximum
  );

  /// <summary>
  /// Known response header IDs
  /// </summary>
  HTTP_RESPONSE_HEADER_ID = (
    HttpHeaderResponseCacheControl = 0,
    HttpHeaderResponseConnection,
    HttpHeaderResponseDate,
    HttpHeaderResponseKeepAlive,
    HttpHeaderResponsePragma,
    HttpHeaderResponseTrailer,
    HttpHeaderResponseTransferEncoding,
    HttpHeaderResponseUpgrade,
    HttpHeaderResponseVia,
    HttpHeaderResponseWarning,
    HttpHeaderResponseAllow,
    HttpHeaderResponseContentLength,
    HttpHeaderResponseContentType,
    HttpHeaderResponseContentEncoding,
    HttpHeaderResponseContentLanguage,
    HttpHeaderResponseContentLocation,
    HttpHeaderResponseContentMd5,
    HttpHeaderResponseContentRange,
    HttpHeaderResponseExpires,
    HttpHeaderResponseLastModified,
    HttpHeaderResponseAcceptRanges,
    HttpHeaderResponseAge,
    HttpHeaderResponseEtag,
    HttpHeaderResponseLocation,
    HttpHeaderResponseProxyAuthenticate,
    HttpHeaderResponseRetryAfter,
    HttpHeaderResponseServer,
    HttpHeaderResponseSetCookie,
    HttpHeaderResponseVary,
    HttpHeaderResponseWwwAuthenticate,
    HttpHeaderResponseMaximum
  );

  HTTP_VERSION = record
    MajorVersion: USHORT;
    MinorVersion: USHORT;
  end;

  HTTP_KNOWN_HEADER = record
    RawValueLength: USHORT;
    pRawValue: PAnsiChar;
  end;

  HTTP_UNKNOWN_HEADER = record
    NameLength: USHORT;
    RawValueLength: USHORT;
    pName: PAnsiChar;
    pRawValue: PAnsiChar;
  end;
  PHTTP_UNKNOWN_HEADER = ^HTTP_UNKNOWN_HEADER;

  /// <summary>
  /// Array type for accessing unknown headers by index
  /// </summary>
  HTTP_UNKNOWN_HEADER_ARRAY = array[0..0] of HTTP_UNKNOWN_HEADER;
  PHTTP_UNKNOWN_HEADER_ARRAY = ^HTTP_UNKNOWN_HEADER_ARRAY;

  HTTP_REQUEST_HEADERS = record
    UnknownHeaderCount: USHORT;
    pUnknownHeaders: PHTTP_UNKNOWN_HEADER;
    TrailerCount: USHORT;
    pTrailers: PHTTP_UNKNOWN_HEADER;
    KnownHeaders: array[0..Ord(HttpHeaderRequestMaximum) - 1] of HTTP_KNOWN_HEADER;
  end;

  HTTP_RESPONSE_HEADERS = record
    UnknownHeaderCount: USHORT;
    pUnknownHeaders: PHTTP_UNKNOWN_HEADER;
    TrailerCount: USHORT;
    pTrailers: PHTTP_UNKNOWN_HEADER;
    KnownHeaders: array[0..Ord(HttpHeaderResponseMaximum) - 1] of HTTP_KNOWN_HEADER;
  end;

  HTTP_COOKED_URL = record
    FullUrlLength: USHORT;
    HostLength: USHORT;
    AbsPathLength: USHORT;
    QueryStringLength: USHORT;
    pFullUrl: PWideChar;
    pHost: PWideChar;
    pAbsPath: PWideChar;
    pQueryString: PWideChar;
  end;

  HTTP_TRANSPORT_ADDRESS = record
    pRemoteAddress: Pointer; // PSOCKADDR - use Pointer to avoid WinSock/WinSock2 conflicts
    pLocalAddress: Pointer;
  end;

  HTTP_DATA_CHUNK_TYPE = (
    HttpDataChunkFromMemory = 0,
    HttpDataChunkFromFileHandle,
    HttpDataChunkFromFragmentCache,
    HttpDataChunkFromFragmentCacheEx,
    HttpDataChunkTrailers,
    HttpDataChunkMaximum
  );

  HTTP_DATA_CHUNK = record
    DataChunkType: HTTP_DATA_CHUNK_TYPE;
    case Integer of
      0: (FromMemory: record
            pBuffer: Pointer;
            BufferLength: ULONG;
          end);
      1: (FromFileHandle: record
            ByteRange_StartingOffset: ULARGE_INTEGER;
            ByteRange_Length: ULARGE_INTEGER;
            FileHandle: THandle;
          end);
  end;
  PHTTP_DATA_CHUNK = ^HTTP_DATA_CHUNK;

  HTTP_SSL_INFO = record
    ServerCertKeySize: USHORT;
    ConnectionKeySize: USHORT;
    ServerCertIssuerSize: ULONG;
    ServerCertSubjectSize: ULONG;
    pServerCertIssuer: PAnsiChar;
    pServerCertSubject: PAnsiChar;
    pClientCertInfo: Pointer;
    SslClientCertNegotiated: ULONG;
  end;
  PHTTP_SSL_INFO = ^HTTP_SSL_INFO;

  HTTP_REQUEST_ID = ULONGLONG;
  HTTP_CONNECTION_ID = ULONGLONG;
  HTTP_URL_CONTEXT = ULONGLONG;
  HTTP_RAW_CONNECTION_ID = ULONGLONG;

  HTTP_REQUEST_V1 = record
    Flags: ULONG;
    ConnectionId: HTTP_CONNECTION_ID;
    RequestId: HTTP_REQUEST_ID;
    UrlContext: HTTP_URL_CONTEXT;
    Version: HTTP_VERSION;
    Verb: HTTP_VERB;
    UnknownVerbLength: USHORT;
    RawUrlLength: USHORT;
    pUnknownVerb: PAnsiChar;
    pRawUrl: PAnsiChar;
    CookedUrl: HTTP_COOKED_URL;
    Address: HTTP_TRANSPORT_ADDRESS;
    Headers: HTTP_REQUEST_HEADERS;
    BytesReceived: ULONGLONG;
    EntityChunkCount: USHORT;
    pEntityChunks: PHTTP_DATA_CHUNK;
    RawConnectionId: HTTP_RAW_CONNECTION_ID;
    pSslInfo: PHTTP_SSL_INFO;
  end;
  PHTTP_REQUEST_V1 = ^HTTP_REQUEST_V1;

  /// <summary>
  /// HTTP_REQUEST is an alias for HTTP_REQUEST_V1 (HTTP API version 1.0)
  /// </summary>
  HTTP_REQUEST = HTTP_REQUEST_V1;
  PHTTP_REQUEST = PHTTP_REQUEST_V1;

  HTTP_RESPONSE_V1 = record
    Flags: ULONG;
    Version: HTTP_VERSION;
    StatusCode: USHORT;
    ReasonLength: USHORT;
    pReason: PAnsiChar;
    Headers: HTTP_RESPONSE_HEADERS;
    EntityChunkCount: USHORT;
    pEntityChunks: PHTTP_DATA_CHUNK;
  end;
  PHTTP_RESPONSE_V1 = ^HTTP_RESPONSE_V1;

  /// <summary>
  /// HTTP_RESPONSE is an alias for HTTP_RESPONSE_V1 (HTTP API version 1.0)
  /// </summary>
  HTTP_RESPONSE = HTTP_RESPONSE_V1;
  PHTTP_RESPONSE = PHTTP_RESPONSE_V1;

/// <summary>Initialize the HTTP Server API</summary>
function HttpInitialize(Version: HTTP_VERSION; Flags: ULONG; pReserved: Pointer): ULONG; stdcall;

/// <summary>Terminate the HTTP Server API</summary>
function HttpTerminate(Flags: ULONG; pReserved: Pointer): ULONG; stdcall;

/// <summary>Create a request queue handle</summary>
function HttpCreateHttpHandle(var pReqQueueHandle: THandle; Reserved: ULONG): ULONG; stdcall;

/// <summary>Register a URL prefix on a request queue</summary>
function HttpAddUrl(ReqQueueHandle: THandle; pFullyQualifiedUrl: PWideChar; pReserved: Pointer): ULONG; stdcall;

/// <summary>Unregister a URL prefix from a request queue</summary>
function HttpRemoveUrl(ReqQueueHandle: THandle; pFullyQualifiedUrl: PWideChar): ULONG; stdcall;

/// <summary>Receive an HTTP request from the request queue</summary>
function HttpReceiveHttpRequest(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID; Flags: ULONG;
  pRequestBuffer: PHTTP_REQUEST; RequestBufferLength: ULONG; var pBytesReceived: ULONG;
  pOverlapped: POverlapped): ULONG; stdcall;

/// <summary>Receive the entity body of an HTTP request</summary>
function HttpReceiveRequestEntityBody(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID; Flags: ULONG;
  pBuffer: Pointer; BufferLength: ULONG; var pBytesReceived: ULONG;
  pOverlapped: POverlapped): ULONG; stdcall;

/// <summary>Send an HTTP response</summary>
function HttpSendHttpResponse(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID; Flags: ULONG;
  pHttpResponse: PHTTP_RESPONSE; pCachePolicy: Pointer; var pBytesSent: ULONG;
  pReserved1: Pointer; Reserved2: ULONG; pOverlapped: POverlapped;
  pLogData: Pointer): ULONG; stdcall;

/// <summary>Close a request queue handle</summary>
function HttpCloseRequestQueue(ReqQueueHandle: THandle): ULONG;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

function HttpInitialize; external HTTPAPI_DLL;
function HttpTerminate; external HTTPAPI_DLL;
function HttpCreateHttpHandle; external HTTPAPI_DLL;
function HttpAddUrl; external HTTPAPI_DLL;
function HttpRemoveUrl; external HTTPAPI_DLL;
function HttpReceiveHttpRequest; external HTTPAPI_DLL;
function HttpReceiveRequestEntityBody; external HTTPAPI_DLL;
function HttpSendHttpResponse; external HTTPAPI_DLL;

function HttpCloseRequestQueue(ReqQueueHandle: THandle): ULONG;
begin
  if ReqQueueHandle <> 0 then
  begin
    if Winapi.Windows.CloseHandle(ReqQueueHandle) then
      Result := NO_ERROR
    else
      Result := GetLastError;
  end
  else
    Result := NO_ERROR;
end;

{$ENDIF}

end.
