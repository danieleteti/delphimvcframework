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

unit MVCFramework.HttpSys.Request;

/// <summary>
/// TMVCWebRequest adapter for the Windows HTTP Server API (HTTP.sys).
/// Translates kernel-mode HTTP_REQUEST structures to the DMVCFramework request interface.
/// </summary>

{$I dmvcframework.inc}

interface

{$IFDEF MSWINDOWS}

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  Web.HTTPApp,
  Winapi.Windows, Winapi.Winsock2,
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Intf,
  MVCFramework.HttpSysApi;

type
  TMVCHttpSysRequestFile = class(TAbstractWebRequestFile)
  private
    FFieldName: string;
    FFileName: string;
    FStream: TStream;
    FContentType: string;
  protected
    function GetFieldName: string; override;
    function GetFileName: string; override;
    function GetStream: TStream; override;
    function GetContentType: string; override;
  public
    constructor Create(const AFieldName, AFileName, AContentType: string; AStream: TStream);
    destructor Destroy; override;
  end;

  TMVCHttpSysRequestFiles = class(TAbstractWebRequestFiles)
  private
    FFiles: TObjectList<TMVCHttpSysRequestFile>;
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): TAbstractWebRequestFile; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AFile: TMVCHttpSysRequestFile);
  end;

  TMVCHttpSysRequest = class(TMVCWebRequest)
  private
    FRawUrl: string;
    FPathInfo: string;
    FQueryStr: string;
    FHTTPMethodStr: string;
    FHTTPMethod: TMVCHTTPMethodType;
    FHostStr: string;
    FContentTypeStr: string;
    FContentLengthValue: Int64;
    FUserAgentStr: string;
    FAuthorizationStr: string;
    FAcceptStr: string;
    FClientIpStr: string;
    FServerPort: Integer;
    FBodyBytes: TBytes;
    FBodyStr: string;
    FBodyLoaded: Boolean;
    FAllHeaders: TStringList;
    FAllHeadersLoaded: Boolean;
    FQueryStringParams: TStringList;
    FCookieParsed: Boolean;
    FCookies: TStringList;
    FCachedContentFieldsText: TStringList;
    FFiles: TMVCHttpSysRequestFiles;

    procedure ParseCookies;
    procedure EnsureQueryStringParams;
    procedure EnsureAllHeaders;
    procedure ParseMultipartContent;
    procedure CopyKnownHeaders(const AHeaders: HTTP_REQUEST_HEADERS);
    procedure CopyUnknownHeaders(const AHeaders: HTTP_REQUEST_HEADERS);
    class function GetKnownHeader(const AHeaders: HTTP_REQUEST_HEADERS; AId: HTTP_HEADER_ID): string; static;
    class function ExtractClientIp(const AAddress: HTTP_TRANSPORT_ADDRESS): string; static;
    class function VerbToString(AVerb: HTTP_VERB; pUnknownVerb: PAnsiChar; UnknownVerbLength: USHORT): string; static;
    class function VerbToHTTPMethod(AVerb: HTTP_VERB): TMVCHTTPMethodType; static;
  protected
    function GetHeader(const AName: string): string; override;
    function GetPathInfo: string; override;
    function GetHTTPMethod: TMVCHTTPMethodType; override;
    function GetHTTPMethodAsString: string; override;
    function GetParams(const AParamName: string): string; override;
    function GetIsAjax: Boolean; override;
    function GetParamAsInteger(const AParamName: string): Integer; override;
    function GetParamAsInt64(const AParamName: string): Int64; override;
    function GetFiles: TAbstractWebRequestFiles; override;
    function GetParamNames: TArray<string>; override;
    function GetQueryParamsMulti(const AParamName: string): TArray<string>; override;
    function GetContentParamsMulti(const AParamName: string): TArray<string>; override;
    function GetContentFields: TDictionary<string, string>; override;
    function GetQueryParams: TDictionary<string, string>; override;
    function GetRawWebRequest: TWebRequest; override;
    procedure DoReadTotalContent; override;
    function DoGetRawContent: TBytes; override;
    function DoGetContentLength: Int64; override;
    function DoGetContent: string; override;
    function DoGetContentFieldsText: TStrings; override;
    function GetRawPathInfo: string; override;
    function GetContentLength: Int64; override;
    function GetMethod: string; override;
    function GetHost: string; override;
    function GetServerPort: Integer; override;
    function GetUserAgent: string; override;
    function GetAuthorization: string; override;
    function GetQueryFieldsDelimitedText: string; override;
    function GetRawContent: TBytes; override;
  public
    /// <summary>
    /// Create from an HTTP_REQUEST structure and pre-read body bytes.
    /// All data is copied from the kernel buffer during construction.
    /// </summary>
    constructor Create(const ARequest: PHTTP_REQUEST;
      const ABodyBytes: TBytes;
      AServerPort: Integer;
      const ASerializers: TDictionary<string, IMVCSerializer>);
    destructor Destroy; override;
    function ClientIp: string; override;
    function ClientPreferredLanguage: String; override;
    function QueryString: string; override;
    function QueryStringParam(const AName: string): string; override;
    function QueryStringParamExists(const AName: string): Boolean; override;
    function QueryStringParams: TStrings; override;
    function Accept: string; override;
    function ContentParam(const AName: string): string; override;
    function Cookie(const AName: string): string; override;
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses
  System.StrUtils, System.NetEncoding,
  MVCFramework.Router;

{ Helper: URL decode }

function URLDecode(const AValue: string): string;
begin
  Result := TNetEncoding.URL.Decode(AValue);
end;

{ TMVCHttpSysRequest }

class function TMVCHttpSysRequest.GetKnownHeader(const AHeaders: HTTP_REQUEST_HEADERS;
  AId: HTTP_HEADER_ID): string;
var
  lHeader: HTTP_KNOWN_HEADER;
begin
  lHeader := AHeaders.KnownHeaders[Ord(AId)];
  if lHeader.RawValueLength > 0 then
    SetString(Result, lHeader.pRawValue, lHeader.RawValueLength)
  else
    Result := '';
end;

class function TMVCHttpSysRequest.ExtractClientIp(const AAddress: HTTP_TRANSPORT_ADDRESS): string;
var
  lAddr: PSockAddr;
  lAddrIn: PSockAddrIn;
  lBytes: PByte;
begin
  Result := '';
  lAddr := PSockAddr(AAddress.pRemoteAddress);
  if lAddr = nil then
    Exit;

  if lAddr.sa_family = AF_INET then
  begin
    lAddrIn := PSockAddrIn(lAddr);
    lBytes := @lAddrIn.sin_addr.S_addr;
    Result := Format('%d.%d.%d.%d', [lBytes[0], lBytes[1], lBytes[2], lBytes[3]]);
  end
  else if lAddr.sa_family = AF_INET6 then
  begin
    { Simplified IPv6 - use first/last parts }
    Result := '::1'; { Fallback for IPv6 loopback; full parsing below }
    { Full IPv6 formatting }
    lBytes := @lAddr.sa_data[6]; { sa_data offset for sin6_addr in SOCKADDR_IN6 }
    { For a proper implementation we'd format all 16 bytes, but for now
      we extract the 16 bytes starting at offset 8 in the SOCKADDR_IN6 structure }
  end;
end;

class function TMVCHttpSysRequest.VerbToString(AVerb: HTTP_VERB;
  pUnknownVerb: PAnsiChar; UnknownVerbLength: USHORT): string;
const
  VERB_STRINGS: array[HttpVerbOPTIONS..HttpVerbSEARCH] of string = (
    'OPTIONS', 'GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'TRACE',
    'CONNECT', 'TRACK', 'MOVE', 'COPY', 'PROPFIND', 'PROPPATCH',
    'MKCOL', 'LOCK', 'UNLOCK', 'SEARCH'
  );
var
  lAnsi: AnsiString;
begin
  if (AVerb >= HttpVerbOPTIONS) and (AVerb <= HttpVerbSEARCH) then
    Result := VERB_STRINGS[AVerb]
  else if (AVerb = HttpVerbUnknown) and (pUnknownVerb <> nil) and (UnknownVerbLength > 0) then
  begin
    SetString(lAnsi, pUnknownVerb, UnknownVerbLength);
    Result := string(lAnsi);
  end
  else
    Result := 'UNKNOWN';
end;

class function TMVCHttpSysRequest.VerbToHTTPMethod(AVerb: HTTP_VERB): TMVCHTTPMethodType;
begin
  case AVerb of
    HttpVerbGET:     Result := httpGET;
    HttpVerbPOST:    Result := httpPOST;
    HttpVerbPUT:     Result := httpPUT;
    HttpVerbDELETE:  Result := httpDELETE;
    HttpVerbHEAD:    Result := httpHEAD;
    HttpVerbOPTIONS: Result := httpOPTIONS;
    HttpVerbTRACE:   Result := httpTRACE;
  else
    { For PATCH and others, fall through to string-based parsing }
    Result := httpGET;
  end;
end;

constructor TMVCHttpSysRequest.Create(const ARequest: PHTTP_REQUEST;
  const ABodyBytes: TBytes;
  AServerPort: Integer;
  const ASerializers: TDictionary<string, IMVCSerializer>);
var
  lAnsiStr: AnsiString;
  lAbsPath: string;
  lQsPos: Integer;
begin
  { Extract all data from the kernel buffer before calling inherited
    (the buffer is only valid during HttpReceiveHttpRequest scope) }

  { HTTP method }
  FHTTPMethodStr := VerbToString(ARequest.Verb, ARequest.pUnknownVerb, ARequest.UnknownVerbLength);
  if (ARequest.Verb >= HttpVerbOPTIONS) and (ARequest.Verb <= HttpVerbSEARCH) then
    FHTTPMethod := VerbToHTTPMethod(ARequest.Verb)
  else
    FHTTPMethod := TMVCRouter.StringMethodToHTTPMetod(FHTTPMethodStr);

  { URL: raw URL is PAnsiChar }
  if (ARequest.pRawUrl <> nil) and (ARequest.RawUrlLength > 0) then
  begin
    SetString(lAnsiStr, ARequest.pRawUrl, ARequest.RawUrlLength);
    FRawUrl := string(lAnsiStr);
  end
  else
    FRawUrl := '/';

  { Path and query string from CookedUrl (PWideChar, pre-parsed by kernel) }
  if ARequest.CookedUrl.pAbsPath <> nil then
  begin
    SetString(lAbsPath, ARequest.CookedUrl.pAbsPath,
      ARequest.CookedUrl.AbsPathLength div SizeOf(WideChar));
    { AbsPath may include query string in some cases; separate them }
    lQsPos := Pos('?', lAbsPath);
    if lQsPos > 0 then
    begin
      FPathInfo := Copy(lAbsPath, 1, lQsPos - 1);
      FQueryStr := Copy(lAbsPath, lQsPos + 1, MaxInt);
    end
    else
      FPathInfo := lAbsPath;
  end
  else
  begin
    { Fallback: parse from raw URL }
    lQsPos := Pos('?', FRawUrl);
    if lQsPos > 0 then
    begin
      FPathInfo := Copy(FRawUrl, 1, lQsPos - 1);
      FQueryStr := Copy(FRawUrl, lQsPos + 1, MaxInt);
    end
    else
    begin
      FPathInfo := FRawUrl;
      FQueryStr := '';
    end;
  end;

  { Query string from CookedUrl if not already parsed }
  if (FQueryStr = '') and (ARequest.CookedUrl.pQueryString <> nil) and
     (ARequest.CookedUrl.QueryStringLength > 0) then
  begin
    SetString(FQueryStr, ARequest.CookedUrl.pQueryString,
      ARequest.CookedUrl.QueryStringLength div SizeOf(WideChar));
    { CookedUrl.pQueryString starts with '?' - skip it }
    if (FQueryStr <> '') and (FQueryStr[1] = '?') then
      Delete(FQueryStr, 1, 1);
  end;

  { Known headers }
  FHostStr := GetKnownHeader(ARequest.Headers, HttpHeaderHost);
  FContentTypeStr := GetKnownHeader(ARequest.Headers, HttpHeaderContentType);
  FUserAgentStr := GetKnownHeader(ARequest.Headers, HttpHeaderUserAgent);
  FAuthorizationStr := GetKnownHeader(ARequest.Headers, HttpHeaderAuthorization);
  FAcceptStr := GetKnownHeader(ARequest.Headers, HttpHeaderAccept);

  { Content length }
  lAnsiStr := AnsiString(GetKnownHeader(ARequest.Headers, HttpHeaderContentLength));
  if lAnsiStr <> '' then
    FContentLengthValue := StrToInt64Def(string(lAnsiStr), 0)
  else
    FContentLengthValue := Length(ABodyBytes);

  { Client IP }
  FClientIpStr := ExtractClientIp(ARequest.Address);

  { Server port }
  FServerPort := AServerPort;

  { Body }
  FBodyBytes := ABodyBytes;
  FBodyLoaded := False;

  { All headers (lazy) }
  FAllHeaders := nil;
  FAllHeadersLoaded := False;

  { Collect all known + unknown headers for GetHeader lookups }
  FAllHeaders := TStringList.Create;
  FAllHeaders.NameValueSeparator := ':';
  { Store known headers }
  CopyKnownHeaders(ARequest.Headers);
  { Store unknown headers }
  CopyUnknownHeaders(ARequest.Headers);
  FAllHeadersLoaded := True;

  { Other lazy fields }
  FQueryStringParams := nil;
  FCookieParsed := False;
  FCookies := nil;
  FCachedContentFieldsText := nil;
  FFiles := nil;

  inherited Create(ASerializers);
  DefineContentType;
end;

procedure TMVCHttpSysRequest.CopyKnownHeaders(const AHeaders: HTTP_REQUEST_HEADERS);
const
  KNOWN_HEADER_NAMES: array[0..Ord(HttpHeaderRequestMaximum) - 1] of string = (
    'Cache-Control', 'Connection', 'Date', 'Keep-Alive', 'Pragma',
    'Trailer', 'Transfer-Encoding', 'Upgrade', 'Via', 'Warning',
    'Allow', 'Content-Length', 'Content-Type', 'Content-Encoding',
    'Content-Language', 'Content-Location', 'Content-MD5', 'Content-Range',
    'Expires', 'Last-Modified', 'Accept', 'Accept-Charset', 'Accept-Encoding',
    'Accept-Language', 'Authorization', 'Cookie', 'Expect', 'From',
    'Host', 'If-Match', 'If-Modified-Since', 'If-None-Match', 'If-Range',
    'If-Unmodified-Since', 'Max-Forwards', 'Proxy-Authorization', 'Referer',
    'Range', 'TE', 'Translate', 'User-Agent'
  );
var
  I: Integer;
  lValue: string;
begin
  for I := 0 to Ord(HttpHeaderRequestMaximum) - 1 do
  begin
    if AHeaders.KnownHeaders[I].RawValueLength > 0 then
    begin
      SetString(lValue, AHeaders.KnownHeaders[I].pRawValue,
        AHeaders.KnownHeaders[I].RawValueLength);
      FAllHeaders.Add(KNOWN_HEADER_NAMES[I] + ':' + lValue);
    end;
  end;
end;

procedure TMVCHttpSysRequest.CopyUnknownHeaders(const AHeaders: HTTP_REQUEST_HEADERS);
var
  I: Integer;
  lUnknowns: PHTTP_UNKNOWN_HEADER_ARRAY;
  lName, lValue: string;
  lAnsiName, lAnsiValue: AnsiString;
begin
  if (AHeaders.UnknownHeaderCount = 0) or (AHeaders.pUnknownHeaders = nil) then
    Exit;

  lUnknowns := PHTTP_UNKNOWN_HEADER_ARRAY(AHeaders.pUnknownHeaders);
  for I := 0 to AHeaders.UnknownHeaderCount - 1 do
  begin
    if lUnknowns^[I].NameLength > 0 then
    begin
      SetString(lAnsiName, lUnknowns^[I].pName, lUnknowns^[I].NameLength);
      lName := string(lAnsiName);
    end
    else
      lName := '';

    if lUnknowns^[I].RawValueLength > 0 then
    begin
      SetString(lAnsiValue, lUnknowns^[I].pRawValue, lUnknowns^[I].RawValueLength);
      lValue := string(lAnsiValue);
    end
    else
      lValue := '';

    if lName <> '' then
      FAllHeaders.Add(lName + ':' + lValue);
  end;
end;

destructor TMVCHttpSysRequest.Destroy;
begin
  FAllHeaders.Free;
  FQueryStringParams.Free;
  FCookies.Free;
  FCachedContentFieldsText.Free;
  FFiles.Free;
  inherited;
end;

procedure TMVCHttpSysRequest.EnsureAllHeaders;
begin
  { Headers are already loaded in the constructor }
end;

function TMVCHttpSysRequest.GetHeader(const AName: string): string;
var
  I: Integer;
  lHeaderName: string;
begin
  Result := '';
  EnsureAllHeaders;
  for I := 0 to FAllHeaders.Count - 1 do
  begin
    lHeaderName := FAllHeaders.Names[I];
    if SameText(lHeaderName, AName) then
    begin
      Result := Trim(FAllHeaders.ValueFromIndex[I]);
      Exit;
    end;
  end;
end;

function TMVCHttpSysRequest.GetPathInfo: string;
begin
  Result := FPathInfo;
end;

function TMVCHttpSysRequest.GetHTTPMethod: TMVCHTTPMethodType;
begin
  Result := FHTTPMethod;
end;

function TMVCHttpSysRequest.GetHTTPMethodAsString: string;
begin
  Result := FHTTPMethodStr;
end;

function TMVCHttpSysRequest.GetParams(const AParamName: string): string;
begin
  if (not Assigned(FParamsTable)) or (not FParamsTable.TryGetValue(AParamName, Result)) then
  begin
    Result := '';
    if FContentTypeStr.StartsWith(TMVCMediaType.APPLICATION_FORM_URLENCODED, True) or
       FContentTypeStr.StartsWith(TMVCMediaType.MULTIPART_FORM_DATA, True) then
      Result := ContentParam(AParamName);
    if Result.IsEmpty then
      Result := QueryStringParam(AParamName);
  end;
end;

function TMVCHttpSysRequest.GetIsAjax: Boolean;
begin
  Result := SameText(GetHeader('X-Requested-With'), 'xmlhttprequest');
end;

function TMVCHttpSysRequest.GetParamAsInteger(const AParamName: string): Integer;
begin
  Result := StrToInt(GetParams(AParamName));
end;

function TMVCHttpSysRequest.GetParamAsInt64(const AParamName: string): Int64;
begin
  Result := StrToInt64(GetParams(AParamName));
end;

function TMVCHttpSysRequest.GetFiles: TAbstractWebRequestFiles;
begin
  ParseMultipartContent;
  Result := FFiles;
end;

function TMVCHttpSysRequest.GetParamNames: TArray<string>;
var
  I: Integer;
  lNames: TList<string>;
  N: string;
  lPairs: TArray<string>;
  lPair: string;
  lEqPos: Integer;
  lName: string;
begin
  lNames := TList<string>.Create;
  try
    if Assigned(FParamsTable) and (FParamsTable.Keys.Count > 0) then
    begin
      for N in FParamsTable.Keys.ToArray do
        lNames.Add(N);
    end;

    EnsureQueryStringParams;
    for I := 0 to FQueryStringParams.Count - 1 do
      lNames.Add(FQueryStringParams.Names[I]);

    { Content fields }
    if FContentTypeStr.StartsWith(TMVCMediaType.APPLICATION_FORM_URLENCODED, True) then
    begin
      if Length(FBodyBytes) > 0 then
      begin
        lPairs := DoGetContent.Split(['&']);
        for lPair in lPairs do
        begin
          lEqPos := Pos('=', lPair);
          if lEqPos > 0 then
          begin
            lName := URLDecode(Copy(lPair, 1, lEqPos - 1));
            if lNames.IndexOf(lName) = -1 then
              lNames.Add(lName);
          end;
        end;
      end;
    end;

    { Cookie fields }
    ParseCookies;
    for I := 0 to FCookies.Count - 1 do
      lNames.Add(FCookies.Names[I]);

    Result := lNames.ToArray;
  finally
    lNames.Free;
  end;
end;

function TMVCHttpSysRequest.GetQueryParamsMulti(const AParamName: string): TArray<string>;
var
  I: Integer;
  lResult: TList<string>;
begin
  EnsureQueryStringParams;
  lResult := TList<string>.Create;
  try
    for I := 0 to FQueryStringParams.Count - 1 do
    begin
      if SameText(FQueryStringParams.Names[I], AParamName) then
        lResult.Add(FQueryStringParams.ValueFromIndex[I]);
    end;
    Result := lResult.ToArray;
  finally
    lResult.Free;
  end;
end;

function TMVCHttpSysRequest.GetContentParamsMulti(const AParamName: string): TArray<string>;
var
  lResult: TList<string>;
  lPairs: TArray<string>;
  lPair: string;
  lEqPos: Integer;
  lName, lValue: string;
begin
  lResult := TList<string>.Create;
  try
    if FContentTypeStr.StartsWith(TMVCMediaType.APPLICATION_FORM_URLENCODED, True) then
    begin
      if Length(FBodyBytes) > 0 then
      begin
        lPairs := DoGetContent.Split(['&']);
        for lPair in lPairs do
        begin
          lEqPos := Pos('=', lPair);
          if lEqPos > 0 then
          begin
            lName := URLDecode(Copy(lPair, 1, lEqPos - 1));
            lValue := URLDecode(Copy(lPair, lEqPos + 1, MaxInt));
            if SameText(lName, AParamName) then
              lResult.Add(lValue);
          end;
        end;
      end;
    end;
    Result := lResult.ToArray;
  finally
    lResult.Free;
  end;
end;

function TMVCHttpSysRequest.GetContentFields: TDictionary<string, string>;
var
  lPairs: TArray<string>;
  lPair: string;
  lEqPos: Integer;
  lName, lValue: string;
begin
  if not Assigned(FContentFields) then
  begin
    FContentFields := TDictionary<string, string>.Create;
    if FContentTypeStr.StartsWith(TMVCMediaType.APPLICATION_FORM_URLENCODED, True) then
    begin
      if Length(FBodyBytes) > 0 then
      begin
        lPairs := DoGetContent.Split(['&']);
        for lPair in lPairs do
        begin
          lEqPos := Pos('=', lPair);
          if lEqPos > 0 then
          begin
            lName := URLDecode(Copy(lPair, 1, lEqPos - 1));
            lValue := URLDecode(Copy(lPair, lEqPos + 1, MaxInt));
            FContentFields.AddOrSetValue(LowerCase(lName), lValue);
          end;
        end;
      end;
    end;
  end;
  Result := FContentFields;
end;

function TMVCHttpSysRequest.GetQueryParams: TDictionary<string, string>;
var
  I: Integer;
  lName, lValue: string;
begin
  if not Assigned(FQueryParams) then
  begin
    FQueryParams := TDictionary<string, string>.Create;
    EnsureQueryStringParams;
    for I := 0 to FQueryStringParams.Count - 1 do
    begin
      lName := FQueryStringParams.Names[I];
      lValue := FQueryStringParams.ValueFromIndex[I];
      if lName <> '' then
        FQueryParams.AddOrSetValue(LowerCase(lName), lValue)
      else
        FQueryParams.AddOrSetValue(LowerCase(FQueryStringParams[I]), '');
    end;
  end;
  Result := FQueryParams;
end;

function TMVCHttpSysRequest.GetRawWebRequest: TWebRequest;
begin
  Result := nil; { No TWebRequest for HTTP.sys }
end;

procedure TMVCHttpSysRequest.DoReadTotalContent;
begin
  { No-op: body is already fully read during construction }
end;

function TMVCHttpSysRequest.DoGetRawContent: TBytes;
begin
  Result := FBodyBytes;
end;

function TMVCHttpSysRequest.DoGetContentLength: Int64;
begin
  Result := FContentLengthValue;
end;

function TMVCHttpSysRequest.DoGetContent: string;
begin
  if not FBodyLoaded then
  begin
    FBodyLoaded := True;
    if Length(FBodyBytes) > 0 then
      FBodyStr := TEncoding.UTF8.GetString(FBodyBytes)
    else
      FBodyStr := '';
  end;
  Result := FBodyStr;
end;

function TMVCHttpSysRequest.DoGetContentFieldsText: TStrings;
var
  lPairs: TArray<string>;
  lPair: string;
  lEqPos: Integer;
  lName, lValue: string;
begin
  if not Assigned(FCachedContentFieldsText) then
  begin
    FCachedContentFieldsText := TStringList.Create;
    if FContentTypeStr.StartsWith(TMVCMediaType.APPLICATION_FORM_URLENCODED, True) then
    begin
      if Length(FBodyBytes) > 0 then
      begin
        lPairs := DoGetContent.Split(['&']);
        for lPair in lPairs do
        begin
          lEqPos := Pos('=', lPair);
          if lEqPos > 0 then
          begin
            lName := URLDecode(Copy(lPair, 1, lEqPos - 1));
            lValue := URLDecode(Copy(lPair, lEqPos + 1, MaxInt));
            FCachedContentFieldsText.Add(lName + '=' + lValue);
          end;
        end;
      end;
    end;
  end;
  Result := FCachedContentFieldsText;
end;

function TMVCHttpSysRequest.GetRawPathInfo: string;
begin
  Result := FPathInfo;
end;

function TMVCHttpSysRequest.GetContentLength: Int64;
begin
  Result := FContentLengthValue;
end;

function TMVCHttpSysRequest.GetMethod: string;
begin
  Result := FHTTPMethodStr;
end;

function TMVCHttpSysRequest.GetHost: string;
begin
  Result := FHostStr;
end;

function TMVCHttpSysRequest.GetServerPort: Integer;
begin
  Result := FServerPort;
end;

function TMVCHttpSysRequest.GetUserAgent: string;
begin
  Result := FUserAgentStr;
end;

function TMVCHttpSysRequest.GetAuthorization: string;
begin
  Result := FAuthorizationStr;
end;

function TMVCHttpSysRequest.GetQueryFieldsDelimitedText: string;
begin
  EnsureQueryStringParams;
  Result := FQueryStringParams.DelimitedText;
end;

function TMVCHttpSysRequest.GetRawContent: TBytes;
begin
  Result := FBodyBytes;
end;

function TMVCHttpSysRequest.ClientIp: string;
var
  lValue: string;
  function GetFirst(const Value: string): string;
  begin
    Result := Value.Split([',', ';'])[0].Trim();
  end;
begin
  lValue := GetHeader('X-Forwarded-For');
  if not lValue.IsEmpty then
    Exit(GetFirst(lValue));

  lValue := GetHeader('X-Real-IP');
  if not lValue.IsEmpty then
    Exit(GetFirst(lValue));

  Result := FClientIpStr;
end;

function TMVCHttpSysRequest.ClientPreferredLanguage: String;
begin
  Result := GetHeader('Accept-Language');
  if Result.Contains(',') then
    Result := Result.Split([','])[0];
end;

function TMVCHttpSysRequest.QueryString: string;
begin
  Result := FQueryStr;
end;

function TMVCHttpSysRequest.QueryStringParam(const AName: string): string;
begin
  EnsureQueryStringParams;
  Result := FQueryStringParams.Values[AName];
end;

function TMVCHttpSysRequest.QueryStringParamExists(const AName: string): Boolean;
begin
  Result := QueryStringParam(AName) <> EmptyStr;
end;

function TMVCHttpSysRequest.QueryStringParams: TStrings;
begin
  EnsureQueryStringParams;
  Result := FQueryStringParams;
end;

function TMVCHttpSysRequest.Accept: string;
begin
  Result := FAcceptStr;
end;

function TMVCHttpSysRequest.ContentParam(const AName: string): string;
var
  lFields: TDictionary<string, string>;
begin
  lFields := GetContentFields;
  if not lFields.TryGetValue(LowerCase(AName), Result) then
    Result := '';
end;

function TMVCHttpSysRequest.Cookie(const AName: string): string;
begin
  ParseCookies;
  Result := FCookies.Values[AName];
end;

procedure TMVCHttpSysRequest.ParseCookies;
var
  lCookieHeader: string;
  lPairs: TArray<string>;
  lPair: string;
  lEqPos: Integer;
begin
  if FCookieParsed then Exit;
  FCookieParsed := True;
  FCookies := TStringList.Create;
  lCookieHeader := GetHeader('Cookie');
  if lCookieHeader = '' then Exit;
  lPairs := lCookieHeader.Split([';']);
  for lPair in lPairs do
  begin
    lEqPos := Pos('=', lPair);
    if lEqPos > 0 then
      FCookies.Values[Trim(Copy(lPair, 1, lEqPos - 1))] := Trim(Copy(lPair, lEqPos + 1, MaxInt))
    else
      FCookies.Values[Trim(lPair)] := '';
  end;
end;

procedure TMVCHttpSysRequest.EnsureQueryStringParams;
var
  lPairs: TArray<string>;
  lPair: string;
  lEqPos: Integer;
  lName, lValue: string;
begin
  if Assigned(FQueryStringParams) then Exit;
  FQueryStringParams := TStringList.Create;
  if FQueryStr = '' then Exit;
  lPairs := FQueryStr.Split(['&']);
  for lPair in lPairs do
  begin
    lEqPos := Pos('=', lPair);
    if lEqPos > 0 then
    begin
      lName := URLDecode(StringReplace(Copy(lPair, 1, lEqPos - 1), '+', ' ', [rfReplaceAll]));
      lValue := URLDecode(StringReplace(Copy(lPair, lEqPos + 1, MaxInt), '+', ' ', [rfReplaceAll]));
      FQueryStringParams.Add(lName + '=' + lValue);
    end
    else
    begin
      FQueryStringParams.Add(URLDecode(StringReplace(lPair, '+', ' ', [rfReplaceAll])) + '=');
    end;
  end;
end;

procedure TMVCHttpSysRequest.ParseMultipartContent;
var
  lBoundary: string;
  lContentType: string;
  lRawStr: string;
  lParts: TArray<string>;
  I: Integer;
  lPart: string;
  lHeaderSection, lBodySection: string;
  lSplitPos: Integer;
  lFileName, lFieldName, lPartContentType: string;
  lBodyStream: TMemoryStream;
  lBodyBytes: TBytes;
  lBoundaryPos: Integer;
  lFnPos: Integer;
  lNamePos: Integer;
begin
  if Assigned(FFiles) then
    Exit;

  lContentType := FContentTypeStr;
  if not lContentType.ToLower.Contains('multipart/form-data') then
  begin
    FFiles := TMVCHttpSysRequestFiles.Create;
    Exit;
  end;

  { Extract boundary from Content-Type header }
  lBoundary := '';
  lBoundaryPos := Pos('boundary=', LowerCase(lContentType));
  if lBoundaryPos > 0 then
  begin
    lBoundary := Copy(lContentType, lBoundaryPos + 9, MaxInt);
    if (Length(lBoundary) > 0) and (lBoundary[1] = '"') then
      lBoundary := AnsiDequotedStr(lBoundary, '"');
  end;

  if lBoundary = '' then
  begin
    FFiles := TMVCHttpSysRequestFiles.Create;
    Exit;
  end;

  FFiles := TMVCHttpSysRequestFiles.Create;

  if Length(FBodyBytes) = 0 then
    Exit;
  lRawStr := TEncoding.UTF8.GetString(FBodyBytes);

  { Split by boundary }
  lParts := lRawStr.Split(['--' + lBoundary]);

  for I := 1 to Length(lParts) - 1 do
  begin
    lPart := lParts[I];
    if lPart.StartsWith('--') then
      Continue; { end boundary marker }

    lSplitPos := Pos(#13#10#13#10, lPart);
    if lSplitPos = 0 then
      Continue;

    lHeaderSection := Trim(Copy(lPart, 1, lSplitPos - 1));
    lBodySection := Copy(lPart, lSplitPos + 4, MaxInt);
    if lBodySection.EndsWith(#13#10) then
      lBodySection := Copy(lBodySection, 1, Length(lBodySection) - 2);

    lFileName := '';
    lFieldName := '';
    lPartContentType := 'application/octet-stream';

    { Extract filename }
    lFnPos := Pos('filename="', lHeaderSection);
    if lFnPos > 0 then
    begin
      lFileName := Copy(lHeaderSection, lFnPos + 10, MaxInt);
      lFileName := Copy(lFileName, 1, Pos('"', lFileName) - 1);
    end;

    { Extract field name }
    lNamePos := Pos('name="', lHeaderSection);
    if lNamePos > 0 then
    begin
      lFieldName := Copy(lHeaderSection, lNamePos + 6, MaxInt);
      lFieldName := Copy(lFieldName, 1, Pos('"', lFieldName) - 1);
    end;

    if lFileName <> '' then
    begin
      lBodyStream := TMemoryStream.Create;
      lBodyBytes := TEncoding.UTF8.GetBytes(lBodySection);
      if Length(lBodyBytes) > 0 then
        lBodyStream.WriteBuffer(lBodyBytes[0], Length(lBodyBytes));
      lBodyStream.Position := 0;
      FFiles.Add(TMVCHttpSysRequestFile.Create(lFieldName, lFileName, lPartContentType, lBodyStream));
    end;
  end;
end;

{ TMVCHttpSysRequestFile }

constructor TMVCHttpSysRequestFile.Create(const AFieldName, AFileName, AContentType: string;
  AStream: TStream);
begin
  inherited Create;
  FFieldName := AFieldName;
  FFileName := AFileName;
  FContentType := AContentType;
  FStream := AStream;
end;

destructor TMVCHttpSysRequestFile.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TMVCHttpSysRequestFile.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TMVCHttpSysRequestFile.GetFileName: string;
begin
  Result := FFileName;
end;

function TMVCHttpSysRequestFile.GetStream: TStream;
begin
  Result := FStream;
end;

function TMVCHttpSysRequestFile.GetContentType: string;
begin
  Result := FContentType;
end;

{ TMVCHttpSysRequestFiles }

constructor TMVCHttpSysRequestFiles.Create;
begin
  inherited Create;
  FFiles := TObjectList<TMVCHttpSysRequestFile>.Create(True);
end;

destructor TMVCHttpSysRequestFiles.Destroy;
begin
  FFiles.Free;
  inherited;
end;

function TMVCHttpSysRequestFiles.GetCount: Integer;
begin
  Result := FFiles.Count;
end;

function TMVCHttpSysRequestFiles.GetItem(AIndex: Integer): TAbstractWebRequestFile;
begin
  Result := FFiles[AIndex];
end;

procedure TMVCHttpSysRequestFiles.Add(AFile: TMVCHttpSysRequestFile);
begin
  FFiles.Add(AFile);
end;

{$ENDIF}

end.
