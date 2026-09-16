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

unit MVCFramework.Indy.Request;

{$I dmvcframework.inc}

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  Web.HTTPApp,
  IdCustomHTTPServer, IdContext,
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Intf;

type
  TMVCIndyRequestFile = class(TAbstractWebRequestFile)
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

  TMVCIndyRequestFiles = class(TAbstractWebRequestFiles)
  private
    FFiles: TObjectList<TMVCIndyRequestFile>;
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): TAbstractWebRequestFile; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AFile: TMVCIndyRequestFile);
  end;

  TMVCIndyDirectRequest = class(TMVCWebRequest)
  private
    FRequestInfo: TIdHTTPRequestInfo;
    FContext: TIdContext;
    FQueryStringParams: TStringList;
    FCookieParsed: Boolean;
    FCookies: TStringList;
    FCachedBody: string;
    FCachedBodyLoaded: Boolean;
    FCachedRawContent: TBytes;
    FCachedRawContentLoaded: Boolean;
    FCachedContentFieldsText: TStringList;
    FFiles: TMVCIndyRequestFiles;
    FMultipartParsed: Boolean;
    { Cached like FCachedBody/FCachedRawContent: the verb is read at least twice
      per request (routing, plus the router log, whose parameters are built
      eagerly) and each read would otherwise allocate twice. }
    FRealHTTPCommand: string;
    FRealHTTPCommandLoaded: Boolean;
    procedure ParseCookies;
    procedure EnsureQueryStringParams;
    procedure LoadBody;
    procedure LoadRawContent;
    procedure ParseMultipartContent;
    function GetRealHTTPCommand: string;
  protected
    function GetHeader(const AName: string): string; override;
    function IsDuplicatedHeader(const AName: string): Boolean; override;
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
    function GetClientConnection: TObject; override;
    constructor Create(const AContext: TIdContext;
      const ARequestInfo: TIdHTTPRequestInfo;
      const ASerializers: TDictionary<string, IMVCSerializer>);
    destructor Destroy; override;
    function ClientIp: string; override;
    function PeerIp: string; override;
    function ClientPreferredLanguage: String; override;
    function QueryString: string; override;
    function QueryStringParam(const AName: string): string; override;
    function QueryStringParamExists(const AName: string): Boolean; override;
    function QueryStringParams: TStrings; override;
    function Accept: string; override;
    function ContentParam(const AName: string): string; override;
    function Cookie(const AName: string): string; override;
  end;

implementation

uses
  System.StrUtils,
  System.NetEncoding,
  IdURI,
  MVCFramework.Router;

{ TMVCIndyDirectRequest }

constructor TMVCIndyDirectRequest.Create(const AContext: TIdContext;
  const ARequestInfo: TIdHTTPRequestInfo;
  const ASerializers: TDictionary<string, IMVCSerializer>);
begin
  FRequestInfo := ARequestInfo;
  FContext := AContext;
  FQueryStringParams := nil;
  FCookieParsed := False;
  FCookies := nil;
  FCachedBodyLoaded := False;
  FCachedRawContentLoaded := False;
  FCachedContentFieldsText := nil;
  FFiles := nil;
  FMultipartParsed := False;
  inherited Create(ASerializers);
  DefineContentType;
end;

destructor TMVCIndyDirectRequest.Destroy;
begin
  FQueryStringParams.Free;
  FCookies.Free;
  FCachedContentFieldsText.Free;
  FFiles.Free;
  inherited;
end;

procedure TMVCIndyDirectRequest.ParseCookies;
var
  lCookieHeader: string;
  lPairs: TArray<string>;
  lPair: string;
  lEqPos: Integer;
begin
  if FCookieParsed then Exit;
  FCookieParsed := True;
  FCookies := TStringList.Create;
  lCookieHeader := FRequestInfo.RawHeaders.Values['Cookie'];
  if lCookieHeader = '' then Exit;
  lPairs := lCookieHeader.Split([';']);
  for lPair in lPairs do
  begin
    lEqPos := Pos('=', lPair);
    { Decoded because the response side percent-encodes name and value, the same
      way WebBroker does on both sides (TCookie.GetHeaderValue writes them
      encoded, TWebRequest.ExtractCookieFields reads them decoded). Encoding on
      the way out and not decoding on the way in would turn a value with a space
      or an '=' into a different value on the next request, on these two hosts
      only. }
    if lEqPos > 0 then
      FCookies.Values[TNetEncoding.URL.Decode(Trim(Copy(lPair, 1, lEqPos - 1)))] :=
        TNetEncoding.URL.Decode(Trim(Copy(lPair, lEqPos + 1, MaxInt)))
    else
      FCookies.Values[Trim(lPair)] := '';
  end;
end;

procedure TMVCIndyDirectRequest.EnsureQueryStringParams;
var
  lQueryStr: string;
  lPairs: TArray<string>;
  lPair: string;
  lEqPos: Integer;
  lName, lValue: string;
begin
  if Assigned(FQueryStringParams) then Exit;
  FQueryStringParams := TStringList.Create;
  lQueryStr := FRequestInfo.QueryParams;
  if lQueryStr = '' then Exit;
  lPairs := lQueryStr.Split(['&']);
  for lPair in lPairs do
  begin
    lEqPos := Pos('=', lPair);
    if lEqPos > 0 then
    begin
      { TNetEncoding, not TIdURI, and the difference is the point. TIdURI.URLDecode
        accepts the non-standard IIS form %uXXXX - so %u003Cscript%u003E arrives
        as <script> with no '<' anywhere in the bytes a WAF inspected - and it
        silently drops a malformed escape, turning ad%zzmin into admin. The other
        two hosts use TNetEncoding, which leaves both forms literal. The host was
        the only thing deciding which of the two an action received.
        '+' is already turned into a space above, so nothing changes there. }
      lName := TNetEncoding.URL.Decode(StringReplace(Copy(lPair, 1, lEqPos - 1), '+', ' ', [rfReplaceAll]));
      lValue := TNetEncoding.URL.Decode(StringReplace(Copy(lPair, lEqPos + 1, MaxInt), '+', ' ', [rfReplaceAll]));
      FQueryStringParams.Add(lName + '=' + lValue);
    end
    else
    begin
      FQueryStringParams.Add(TNetEncoding.URL.Decode(StringReplace(lPair, '+', ' ', [rfReplaceAll])) + '=');
    end;
  end;
end;

procedure TMVCIndyDirectRequest.LoadBody;
var
  lStream: TStream;
  lBytes: TBytes;
begin
  if FCachedBodyLoaded then Exit;
  FCachedBodyLoaded := True;
  lStream := FRequestInfo.PostStream;
  if Assigned(lStream) and (lStream.Size > 0) then
  begin
    lStream.Position := 0;
    SetLength(lBytes, lStream.Size);
    lStream.ReadBuffer(lBytes[0], lStream.Size);
    lStream.Position := 0;
    FCachedBody := TEncoding.UTF8.GetString(lBytes);
  end
  else
  begin
    FCachedBody := FRequestInfo.UnparsedParams;
  end;
end;

procedure TMVCIndyDirectRequest.LoadRawContent;
var
  lStream: TStream;
begin
  if FCachedRawContentLoaded then Exit;
  FCachedRawContentLoaded := True;
  lStream := FRequestInfo.PostStream;
  if Assigned(lStream) and (lStream.Size > 0) then
  begin
    lStream.Position := 0;
    SetLength(FCachedRawContent, lStream.Size);
    lStream.ReadBuffer(FCachedRawContent[0], lStream.Size);
    lStream.Position := 0;
  end
  else if FRequestInfo.UnparsedParams <> '' then
  begin
    FCachedRawContent := TEncoding.UTF8.GetBytes(FRequestInfo.UnparsedParams);
  end
  else
  begin
    SetLength(FCachedRawContent, 0);
  end;
end;

function TMVCIndyDirectRequest.GetHeader(const AName: string): string;
begin
  Result := FRequestInfo.RawHeaders.Values[AName];
end;

function TMVCIndyDirectRequest.IsDuplicatedHeader(const AName: string): Boolean;
var
  I, lCount, lNameLen: Integer;
  lLine: string;
begin
  { Compared against the raw line rather than RawHeaders.Names[I]: TIdHeaderList
    overrides Names and allocates twice per header to produce it, and this runs
    three times on every request. A folded continuation line starts with
    whitespace, so it cannot match a name either way. }
  lNameLen := Length(AName);
  lCount := 0;
  for I := 0 to FRequestInfo.RawHeaders.Count - 1 do
  begin
    lLine := FRequestInfo.RawHeaders[I];
    if (Length(lLine) > lNameLen) and (lLine[lNameLen + 1] = ':') and
      (StrLIComp(PChar(lLine), PChar(AName), lNameLen) = 0) then
    begin
      Inc(lCount);
      if lCount > 1 then
        Exit(True);
    end;
  end;
  Result := False;
end;

function TMVCIndyDirectRequest.GetPathInfo: string;
begin
  Result := FRequestInfo.Document;
end;

function TMVCIndyDirectRequest.GetRealHTTPCommand: string;
var
  lSpacePos: Integer;
begin
  if FRealHTTPCommandLoaded then
    Exit(FRealHTTPCommand);
  { Indy rewrites RequestInfo.Command on POST from any of X-HTTP-Method-Override,
    X-HTTP-Method or X-METHOD-OVERRIDE (IdCustomHTTPServer.pas, "check for
    overrides when LCmd is 'POST'"). Routing must never depend on a header a
    client can write, and the other two hosts honour no such thing - a verb that
    changes on one host only is a way past whatever sits in front of the server.
    RawHTTPCommand is captured before the rewrite, so it still holds the request
    line as it arrived. }
  Result := FRequestInfo.RawHTTPCommand;
  if Result = '' then
    Result := FRequestInfo.Command
  else
  begin
    lSpacePos := Pos(' ', Result);
    if lSpacePos > 0 then
      SetLength(Result, lSpacePos - 1);
    Result := UpperCase(Result);
  end;
  FRealHTTPCommand := Result;
  FRealHTTPCommandLoaded := True;
end;

function TMVCIndyDirectRequest.GetHTTPMethod: TMVCHTTPMethodType;
begin
  Result := TMVCRouter.StringMethodToHTTPMetod(GetRealHTTPCommand);
end;

function TMVCIndyDirectRequest.GetHTTPMethodAsString: string;
begin
  Result := GetRealHTTPCommand;
end;

function TMVCIndyDirectRequest.GetParams(const AParamName: string): string;
begin
  if (not Assigned(FParamsTable)) or (not FParamsTable.TryGetValue(AParamName, Result)) then
  begin
    Result := '';
    if string(FRequestInfo.ContentType).StartsWith(TMVCMediaType.APPLICATION_FORM_URLENCODED, True) or
      string(FRequestInfo.ContentType).StartsWith(TMVCMediaType.MULTIPART_FORM_DATA, True) then
      Result := ContentParam(AParamName);
    if Result.IsEmpty then
      Result := QueryStringParam(AParamName);
  end;
end;

function TMVCIndyDirectRequest.GetIsAjax: Boolean;
begin
  Result := LowerCase(FRequestInfo.RawHeaders.Values['X-Requested-With']) = 'xmlhttprequest';
end;

function TMVCIndyDirectRequest.GetParamAsInteger(const AParamName: string): Integer;
begin
  Result := StrToInt(GetParams(AParamName));
end;

function TMVCIndyDirectRequest.GetParamAsInt64(const AParamName: string): Int64;
begin
  Result := StrToInt64(GetParams(AParamName));
end;

function TMVCIndyDirectRequest.GetFiles: TAbstractWebRequestFiles;
begin
  ParseMultipartContent;
  Result := FFiles;
end;

function TMVCIndyDirectRequest.GetParamNames: TArray<string>;
var
  I: Integer;
  Names: TList<string>;
  N: string;
  lPairs: TArray<string>;
  lPair: string;
  lEqPos: Integer;
  lName: string;
begin
  Names := TList<string>.Create;
  try
    if Assigned(FParamsTable) and (FParamsTable.Keys.Count > 0) then
    begin
      for N in FParamsTable.Keys.ToArray do
        Names.Add(N);
    end;

    EnsureQueryStringParams;
    for I := 0 to FQueryStringParams.Count - 1 do
      Names.Add(FQueryStringParams.Names[I]);

    // Content fields
    if string(FRequestInfo.ContentType).StartsWith(TMVCMediaType.APPLICATION_FORM_URLENCODED, True) then
    begin
      LoadBody;
      if FCachedBody <> '' then
      begin
        lPairs := FCachedBody.Split(['&']);
        for lPair in lPairs do
        begin
          lEqPos := Pos('=', lPair);
          if lEqPos > 0 then
          begin
            lName := TIdURI.URLDecode(StringReplace(Copy(lPair, 1, lEqPos - 1), '+', ' ', [rfReplaceAll]));
            if Names.IndexOf(lName) = -1 then
              Names.Add(lName);
          end;
        end;
      end;
    end;

    // Cookie fields
    ParseCookies;
    for I := 0 to FCookies.Count - 1 do
      Names.Add(FCookies.Names[I]);

    Result := Names.ToArray;
  finally
    Names.Free;
  end;
end;

function TMVCIndyDirectRequest.GetQueryParamsMulti(const AParamName: string): TArray<string>;
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

function TMVCIndyDirectRequest.GetContentParamsMulti(const AParamName: string): TArray<string>;
var
  lResult: TList<string>;
  lPairs: TArray<string>;
  lPair: string;
  lEqPos: Integer;
  lName, lValue: string;
  I: Integer;
begin
  lResult := TList<string>.Create;
  try
    if string(FRequestInfo.ContentType).ToLower.Contains('multipart/form-data') then
    begin
      // Multipart: ParseMultipartContent preserves duplicate field names in
      // FCachedContentFieldsText (one "name=value" line per occurrence), the
      // only source that keeps multi-value text parts. The urlencoded branch
      // below cannot see these (the body is not '&'-delimited).
      ParseMultipartContent;
      if Assigned(FCachedContentFieldsText) then
      begin
        for I := 0 to FCachedContentFieldsText.Count - 1 do
        begin
          lPair := FCachedContentFieldsText[I];
          lEqPos := Pos('=', lPair);
          if lEqPos > 0 then
          begin
            lName := Copy(lPair, 1, lEqPos - 1);
            lValue := Copy(lPair, lEqPos + 1, MaxInt);
            if SameText(lName, AParamName) then
              lResult.Add(lValue);
          end;
        end;
      end;
    end
    else if string(FRequestInfo.ContentType).StartsWith(TMVCMediaType.APPLICATION_FORM_URLENCODED, True) then
    begin
      LoadBody;
      if FCachedBody <> '' then
      begin
        lPairs := FCachedBody.Split(['&']);
        for lPair in lPairs do
        begin
          lEqPos := Pos('=', lPair);
          if lEqPos > 0 then
          begin
            lName := TIdURI.URLDecode(StringReplace(Copy(lPair, 1, lEqPos - 1), '+', ' ', [rfReplaceAll]));
            lValue := TIdURI.URLDecode(StringReplace(Copy(lPair, lEqPos + 1, MaxInt), '+', ' ', [rfReplaceAll]));
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

function TMVCIndyDirectRequest.GetContentFields: TDictionary<string, string>;
var
  lPairs: TArray<string>;
  lPair: string;
  lEqPos: Integer;
  lName, lValue: string;
begin
  // For multipart bodies, defer to ParseMultipartContent which populates
  // FContentFields with the text-only parts. ParseMultipartContent is
  // idempotent (FMultipartParsed flag), so calling it from multiple
  // accessors costs nothing after the first request.
  if string(FRequestInfo.ContentType).ToLower.Contains('multipart/form-data') then
    ParseMultipartContent;

  if not Assigned(FContentFields) then
  begin
    FContentFields := TDictionary<string, string>.Create;
    if string(FRequestInfo.ContentType).StartsWith(TMVCMediaType.APPLICATION_FORM_URLENCODED, True) then
    begin
      LoadBody;
      if FCachedBody <> '' then
      begin
        lPairs := FCachedBody.Split(['&']);
        for lPair in lPairs do
        begin
          lEqPos := Pos('=', lPair);
          if lEqPos > 0 then
          begin
            lName := TIdURI.URLDecode(StringReplace(Copy(lPair, 1, lEqPos - 1), '+', ' ', [rfReplaceAll]));
            lValue := TIdURI.URLDecode(StringReplace(Copy(lPair, lEqPos + 1, MaxInt), '+', ' ', [rfReplaceAll]));
            FContentFields.AddOrSetValue(LowerCase(lName), lValue);
          end;
        end;
      end;
    end;
  end;
  Result := FContentFields;
end;

function TMVCIndyDirectRequest.GetQueryParams: TDictionary<string, string>;
var
  I: Integer;
  lName, lValue, lKey: string;
begin
  if not Assigned(FQueryParams) then
  begin
    FQueryParams := TDictionary<string, string>.Create;
    EnsureQueryStringParams;
    for I := 0 to FQueryStringParams.Count - 1 do
    begin
      lName := FQueryStringParams.Names[I];
      lValue := FQueryStringParams.ValueFromIndex[I];
      { First value wins, so that QueryParams and QueryStringParam agree on a
        repeated key: QueryStringParam reads TStringList.Values, which returns
        the first match. Two accessors disagreeing on ?role=user&role=admin is
        a validate-with-one, use-the-other bypass waiting to happen, and most
        proxies and WAFs in front of us also take the first. }
      if lName <> '' then
        lKey := LowerCase(lName)
      else
      begin
        lKey := LowerCase(FQueryStringParams[I]);
        lValue := '';
      end;
      if not FQueryParams.ContainsKey(lKey) then
        FQueryParams.Add(lKey, lValue);
    end;
  end;
  Result := FQueryParams;
end;

function TMVCIndyDirectRequest.GetRawWebRequest: TWebRequest;
begin
  Result := nil; // No TWebRequest for direct Indy
end;

procedure TMVCIndyDirectRequest.DoReadTotalContent;
begin
  // No-op: Indy already has the full content available
end;

function TMVCIndyDirectRequest.DoGetRawContent: TBytes;
begin
  LoadRawContent;
  Result := FCachedRawContent;
end;

function TMVCIndyDirectRequest.DoGetContentLength: Int64;
begin
  Result := FRequestInfo.ContentLength;
end;

function TMVCIndyDirectRequest.DoGetContent: string;
begin
  LoadBody;
  Result := FCachedBody;
end;

function TMVCIndyDirectRequest.DoGetContentFieldsText: TStrings;
var
  lPairs: TArray<string>;
  lPair: string;
  lEqPos: Integer;
  lName, lValue: string;
begin
  // Same dual-path approach as GetContentFields: ParseMultipartContent
  // populates FCachedContentFieldsText for multipart text parts; the
  // urlencoded path below handles application/x-www-form-urlencoded.
  if string(FRequestInfo.ContentType).ToLower.Contains('multipart/form-data') then
    ParseMultipartContent;

  if not Assigned(FCachedContentFieldsText) then
  begin
    FCachedContentFieldsText := TStringList.Create;
    if string(FRequestInfo.ContentType).StartsWith(TMVCMediaType.APPLICATION_FORM_URLENCODED, True) then
    begin
      LoadBody;
      if FCachedBody <> '' then
      begin
        lPairs := FCachedBody.Split(['&']);
        for lPair in lPairs do
        begin
          lEqPos := Pos('=', lPair);
          if lEqPos > 0 then
          begin
            lName := TIdURI.URLDecode(StringReplace(Copy(lPair, 1, lEqPos - 1), '+', ' ', [rfReplaceAll]));
            lValue := TIdURI.URLDecode(StringReplace(Copy(lPair, lEqPos + 1, MaxInt), '+', ' ', [rfReplaceAll]));
            FCachedContentFieldsText.Add(lName + '=' + lValue);
          end;
        end;
      end;
    end;
  end;
  Result := FCachedContentFieldsText;
end;

function TMVCIndyDirectRequest.GetRawPathInfo: string;
begin
  Result := FRequestInfo.URI;
end;

function TMVCIndyDirectRequest.GetContentLength: Int64;
begin
  Result := FRequestInfo.ContentLength;
end;

function TMVCIndyDirectRequest.GetMethod: string;
begin
  Result := GetRealHTTPCommand;
end;

function TMVCIndyDirectRequest.GetHost: string;
begin
  Result := FRequestInfo.Host;
end;

function TMVCIndyDirectRequest.GetServerPort: Integer;
begin
  Result := FContext.Binding.Port;
end;

function TMVCIndyDirectRequest.GetUserAgent: string;
begin
  Result := FRequestInfo.UserAgent;
end;

function TMVCIndyDirectRequest.GetAuthorization: string;
begin
  Result := FRequestInfo.RawHeaders.Values['Authorization'];
end;

function TMVCIndyDirectRequest.GetQueryFieldsDelimitedText: string;
begin
  EnsureQueryStringParams;
  Result := FQueryStringParams.DelimitedText;
end;

function TMVCIndyDirectRequest.GetRawContent: TBytes;
begin
  Result := DoGetRawContent;
end;

function TMVCIndyDirectRequest.GetClientConnection: TObject;
begin
  // Expose the TIdContext so streaming writers (TMVCSSEWriter,
  // TMVCJSONLWriter, TMVCJSONArrayWriter) can grab the raw Indy socket
  // IOHandler and emit chunked output without buffering in the
  // framework response.
  Result := FContext;
end;

function TMVCIndyDirectRequest.ClientIp: string;
begin
  Result := MVCResolveClientIP(
    FRequestInfo.RawHeaders.Values['X-Forwarded-For'],
    FRequestInfo.RawHeaders.Values['X-Real-IP'],
    FContext.Binding.PeerIP,
    MVCTrustProxyForwardedHeaders);
end;

function TMVCIndyDirectRequest.PeerIp: string;
begin
  Result := FContext.Binding.PeerIP;
end;

function TMVCIndyDirectRequest.ClientPreferredLanguage: String;
begin
  Result := FRequestInfo.RawHeaders.Values['Accept-Language'];
  if Result.Contains(',') then
    Result := Result.Split([','])[0];
end;

function TMVCIndyDirectRequest.QueryString: string;
begin
  Result := FRequestInfo.QueryParams;
end;

function TMVCIndyDirectRequest.QueryStringParam(const AName: string): string;
begin
  EnsureQueryStringParams;
  Result := FQueryStringParams.Values[AName];
end;

function TMVCIndyDirectRequest.QueryStringParamExists(const AName: string): Boolean;
begin
  Result := QueryStringParam(AName) <> EmptyStr;
end;

function TMVCIndyDirectRequest.QueryStringParams: TStrings;
begin
  EnsureQueryStringParams;
  Result := FQueryStringParams;
end;

function TMVCIndyDirectRequest.Accept: string;
begin
  Result := FRequestInfo.Accept;
end;

function TMVCIndyDirectRequest.ContentParam(const AName: string): string;
var
  lFields: TDictionary<string, string>;
begin
  lFields := GetContentFields;
  if not lFields.TryGetValue(LowerCase(AName), Result) then
    Result := '';
end;

function TMVCIndyDirectRequest.Cookie(const AName: string): string;
begin
  ParseCookies;
  Result := FCookies.Values[AName];
end;

function IndexOfBytes(const AHaystack, ANeedle: TBytes; const AStart: Integer;
  const AEnd: Integer = -1): Integer;
var
  I, J: Integer;
  lHayLen, lNeedleLen: Integer;
  lMatch: Boolean;
begin
  Result := -1;
  lHayLen := Length(AHaystack);
  { AEnd bounds the scan. Without it a search inside one part runs to the end of
    the whole body, so a body made of boundaries with no header separator costs
    one full scan per part: quadratic, and a 5 MiB request keeps a connection
    thread at 100% for tens of minutes. }
  if (AEnd >= 0) and (AEnd < lHayLen) then
    lHayLen := AEnd;
  lNeedleLen := Length(ANeedle);
  if (lNeedleLen = 0) or (lNeedleLen > lHayLen) then
    Exit;
  I := AStart;
  if I < 0 then
    I := 0;
  while I <= lHayLen - lNeedleLen do
  begin
    lMatch := True;
    for J := 0 to lNeedleLen - 1 do
      if AHaystack[I + J] <> ANeedle[J] then
      begin
        lMatch := False;
        Break;
      end;
    if lMatch then
      Exit(I);
    Inc(I);
  end;
end;

procedure TMVCIndyDirectRequest.ParseMultipartContent;
//
// Strategy: the Indy Direct backend has no WebBroker layer parsing the body
// for it, so this routine is the *single* place that walks a
// multipart/form-data payload. It must therefore populate BOTH:
//   - FFiles: parts that carry a filename="..." attribute
//   - FContentFields / FCachedContentFieldsText: text-only parts (i.e. parts
//     without a filename), so ContentParam('field') returns the value
//     (issue #758 - older fix populated only FFiles).
//
// The body is walked at the BYTE level (TBytes), never round-tripped through a
// UTF-8 string: a multipart payload can carry arbitrary binary file parts
// (images, zips, ...) and decoding those as UTF-8 corrupts the bytes (invalid
// sequences become U+FFFD) and changes their length, which both fails the
// upload and previously raised a 500. Only the header section and text-only
// field values - which are text by definition - are decoded to string.
//
// Idempotent via FMultipartParsed: GetFiles, GetContentFields and
// DoGetContentFieldsText all call this method, but the body is split exactly
// once. FFiles is allocated unconditionally so callers always see a valid
// (possibly empty) collection; the dictionary/stringlist for text fields are
// allocated lazily, only when the body actually contains text parts, to keep
// non-multipart requests cheap.
var
  lBoundary: string;
  lContentType: string;
  lRaw: TBytes;
  lBoundaryBytes: TBytes;
  lCRLF2: TBytes;
  lRawLen, lBoundaryLen: Integer;
  lPos, lPartStart, lPartEnd, lNextBoundary: Integer;
  lHeaderEnd, lBodyStart, lBodyLen: Integer;
  lHeaderSection, lFieldValue: string;
  lFileName, lFieldName, lPartContentType: string;
  lBodyStream: TMemoryStream;
  lBoundaryPos, lFnPos, lNamePos: Integer;
begin
  if FMultipartParsed then
    Exit;
  FMultipartParsed := True;

  if not Assigned(FFiles) then
    FFiles := TMVCIndyRequestFiles.Create;

  lContentType := FRequestInfo.ContentType;
  if not lContentType.ToLower.Contains('multipart/form-data') then
    Exit;

  // Extract boundary from Content-Type header
  lBoundary := '';
  lBoundaryPos := Pos('boundary=', LowerCase(lContentType));
  if lBoundaryPos > 0 then
  begin
    lBoundary := Copy(lContentType, lBoundaryPos + 9, MaxInt);
    // Remove quotes if present
    if (Length(lBoundary) > 0) and (lBoundary[1] = '"') then
      lBoundary := AnsiDequotedStr(lBoundary, '"');
  end;

  { RFC 2046 caps a boundary at 70 characters. The cap matters here because the
    boundary is the needle of a naive matcher: a 16 KB boundary (Indy's line
    limit) against a 5 MiB body is ~8e10 byte comparisons on a connection
    thread. Anything longer is not a legal multipart body. }
  if (lBoundary = '') or (Length(lBoundary) > 70) then
    Exit;

  // Read raw content (bytes, never decoded as a whole)
  LoadRawContent;
  lRaw := FCachedRawContent;
  lRawLen := Length(lRaw);
  if lRawLen = 0 then
    Exit;

  lBoundaryBytes := TEncoding.ASCII.GetBytes('--' + lBoundary);
  lBoundaryLen := Length(lBoundaryBytes);
  lCRLF2 := TEncoding.ASCII.GetBytes(#13#10#13#10);

  lPos := IndexOfBytes(lRaw, lBoundaryBytes, 0);
  while lPos >= 0 do
  begin
    lPartStart := lPos + lBoundaryLen;

    // Closing boundary marker "--boundary--"
    if (lPartStart + 1 < lRawLen) and (lRaw[lPartStart] = Byte('-')) and (lRaw[lPartStart + 1] = Byte('-')) then
      Break;

    // Skip the CRLF that follows the boundary line
    if (lPartStart + 1 < lRawLen) and (lRaw[lPartStart] = 13) and (lRaw[lPartStart + 1] = 10) then
      Inc(lPartStart, 2);

    lNextBoundary := IndexOfBytes(lRaw, lBoundaryBytes, lPartStart);
    if lNextBoundary < 0 then
      lPartEnd := lRawLen
    else
      lPartEnd := lNextBoundary;

    // Header/body separator (double CRLF) inside this part
    lHeaderEnd := IndexOfBytes(lRaw, lCRLF2, lPartStart, lPartEnd);
    if (lHeaderEnd < 0) or (lHeaderEnd >= lPartEnd) then
    begin
      lPos := lNextBoundary;
      Continue;
    end;

    lHeaderSection := TEncoding.UTF8.GetString(lRaw, lPartStart, lHeaderEnd - lPartStart);
    lBodyStart := lHeaderEnd + 4;
    lBodyLen := lPartEnd - lBodyStart;
    // Strip the trailing CRLF that precedes the next boundary delimiter
    if (lBodyLen >= 2) and (lRaw[lBodyStart + lBodyLen - 2] = 13) and (lRaw[lBodyStart + lBodyLen - 1] = 10) then
      Dec(lBodyLen, 2);
    if lBodyLen < 0 then
      lBodyLen := 0;

    // Parse Content-Disposition for filename and field name
    lFileName := '';
    lFieldName := '';
    lPartContentType := 'application/octet-stream';

    lFnPos := Pos('filename="', lHeaderSection);
    if lFnPos > 0 then
    begin
      lFileName := Copy(lHeaderSection, lFnPos + 10, MaxInt);
      lFileName := Copy(lFileName, 1, Pos('"', lFileName) - 1);
    end;

    lNamePos := Pos('name="', lHeaderSection);
    if lNamePos > 0 then
    begin
      lFieldName := Copy(lHeaderSection, lNamePos + 6, MaxInt);
      lFieldName := Copy(lFieldName, 1, Pos('"', lFieldName) - 1);
    end;

    if lFileName <> '' then
    begin
      // File part - copy raw bytes verbatim, no encoding round-trip
      lBodyStream := TMemoryStream.Create;
      if lBodyLen > 0 then
        lBodyStream.WriteBuffer(lRaw[lBodyStart], lBodyLen);
      lBodyStream.Position := 0;
      FFiles.Add(TMVCIndyRequestFile.Create(lFieldName, lFileName, lPartContentType, lBodyStream));
    end
    else if lFieldName <> '' then
    begin
      // Text field part - decode the (text) value and populate ContentFields
      // so ContentParam('field') returns the value
      lFieldValue := TEncoding.UTF8.GetString(lRaw, lBodyStart, lBodyLen);
      if not Assigned(FContentFields) then
        FContentFields := TDictionary<string, string>.Create;
      if not Assigned(FCachedContentFieldsText) then
        FCachedContentFieldsText := TStringList.Create;
      FContentFields.AddOrSetValue(LowerCase(lFieldName), lFieldValue);
      FCachedContentFieldsText.Add(lFieldName + '=' + lFieldValue);
    end;

    lPos := lNextBoundary;
  end;
end;

{ TMVCIndyRequestFile }

constructor TMVCIndyRequestFile.Create(const AFieldName, AFileName, AContentType: string; AStream: TStream);
begin
  inherited Create;
  FFieldName := AFieldName;
  FFileName := AFileName;
  FContentType := AContentType;
  FStream := AStream;
end;

destructor TMVCIndyRequestFile.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TMVCIndyRequestFile.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TMVCIndyRequestFile.GetFileName: string;
begin
  Result := FFileName;
end;

function TMVCIndyRequestFile.GetStream: TStream;
begin
  Result := FStream;
end;

function TMVCIndyRequestFile.GetContentType: string;
begin
  Result := FContentType;
end;

{ TMVCIndyRequestFiles }

constructor TMVCIndyRequestFiles.Create;
begin
  inherited Create;
  FFiles := TObjectList<TMVCIndyRequestFile>.Create(True);
end;

destructor TMVCIndyRequestFiles.Destroy;
begin
  FFiles.Free;
  inherited;
end;

function TMVCIndyRequestFiles.GetCount: Integer;
begin
  Result := FFiles.Count;
end;

function TMVCIndyRequestFiles.GetItem(AIndex: Integer): TAbstractWebRequestFile;
begin
  Result := FFiles[AIndex];
end;

procedure TMVCIndyRequestFiles.Add(AFile: TMVCIndyRequestFile);
begin
  FFiles.Add(AFile);
end;

end.
