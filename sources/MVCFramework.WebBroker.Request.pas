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

unit MVCFramework.WebBroker.Request;

{$I dmvcframework.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Web.HTTPApp,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Intf;

type
  TMVCWebBrokerRequest = class(TMVCWebRequest)
  private
    FWebRequest: TWebRequest;
    FMultipartFallback: TStringList;
    FMultipartFallbackTried: Boolean;
    { The verb is read at least twice per request, and under ISAPI every read of
      TWebRequest.Method is a GetServerVariable round trip. }
    FRealHTTPCommand: string;
    FRealHTTPCommandLoaded: Boolean;
    procedure EnsureMultipartFallback;
    function GetRealHTTPCommand: string;
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
    function GetRawPathInfo: string; override;
    function GetContentLength: Int64; override;
    function GetMethod: string; override;
    function GetHost: string; override;
    function GetServerPort: Integer; override;
    function GetUserAgent: string; override;
    function GetAuthorization: string; override;
    function GetQueryFieldsDelimitedText: string; override;
    function GetRawContent: TBytes; override;
    { Abstract helper overrides for Body }
    procedure DoReadTotalContent; override;
    function DoGetRawContent: TBytes; override;
    function DoGetContentLength: Int64; override;
    function DoGetContent: string; override;
    function DoGetContentFieldsText: TStrings; override;
  public
    constructor Create(const AWebRequest: TWebRequest;
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
    function GetClientConnection: TObject; override;
    property WebRequest: TWebRequest read FWebRequest;
  end;

implementation

uses
  System.Rtti,
  MVCFramework.Router;

{ TIdHTTPAppRequest is reached by RTTI, never by a unit reference: pulling in
  IdHTTPWebBrokerBridge would link a full Indy HTTP server into every ISAPI and
  Apache binary, and that unit's initialization claims WebRequestHandlerProc and
  TWebResponseStream.FGetResponseStream - two process-wide slots the ISAPI and
  Apache hosts claim too. See GetClientConnection below, which has done it this
  way for the same reason. }
function IsOrInheritsFromTIdHTTPAppRequest(AClass: TClass): Boolean;
begin
  while AClass <> nil do
  begin
    if AClass.ClassName = 'TIdHTTPAppRequest' then
      Exit(True);
    AClass := AClass.ClassParent;
  end;
  Result := False;
end;

{ TMVCWebBrokerRequest }

constructor TMVCWebBrokerRequest.Create(const AWebRequest: TWebRequest;
  const ASerializers: TDictionary<string, IMVCSerializer>);
begin
  FWebRequest := AWebRequest;
  FMultipartFallback := nil;
  FMultipartFallbackTried := False;
  inherited Create(ASerializers);
  DefineContentType;
end;

destructor TMVCWebBrokerRequest.Destroy;
begin
  FMultipartFallback.Free;
  inherited;
end;

procedure TMVCWebBrokerRequest.EnsureMultipartFallback;
//
// Strategy: WebBroker delegates request parsing to its host (Indy bridge,
// Apache, ISAPI). The host normally populates TWebRequest.ContentFields with
// every multipart text field. Some host versions — notably older Indy bridges
// when the part carries Content-Transfer-Encoding: 8bit (issue #758) — skip
// those fields, leaving ContentFields empty for multipart bodies.
//
// To avoid breaking installations that depend on the *current* behaviour
// (which works correctly on Delphi 13 + current Indy), this fallback runs
// only when the host produced zero ContentFields for a multipart request.
// In that narrow scenario we parse the raw body ourselves and stash the
// text-only parts in FMultipartFallback. The accessors merge that view on
// top of whatever the host already produced; it never overrides values that
// the host parsed correctly.
//
// Tried at most once per request (FMultipartFallbackTried) to keep the cost
// at zero on the happy path.
var
  lContentType, lBoundary, lRawStr, lPart, lHeaderSection, lBodySection: string;
  lParts: TArray<string>;
  I, lBoundaryPos, lSplitPos, lFnPos, lNamePos: Integer;
  lFieldName, lFileName: string;
  lRawBytes: TBytes;
begin
  if FMultipartFallbackTried then
    Exit;
  FMultipartFallbackTried := True;

  lContentType := string(FWebRequest.ContentType);
  if not lContentType.ToLower.Contains('multipart/form-data') then
    Exit;
  // Skip fallback when the host already parsed text fields — avoids any risk
  // of double-counting and keeps the well-known happy path untouched.
  if FWebRequest.ContentFields.Count > 0 then
    Exit;

  lBoundary := '';
  lBoundaryPos := Pos('boundary=', LowerCase(lContentType));
  if lBoundaryPos > 0 then
  begin
    lBoundary := Copy(lContentType, lBoundaryPos + 9, MaxInt);
    if (Length(lBoundary) > 0) and (lBoundary[1] = '"') then
      lBoundary := AnsiDequotedStr(lBoundary, '"');
  end;
  if lBoundary = '' then
    Exit;

  lRawBytes := DoGetRawContent;
  if Length(lRawBytes) = 0 then
    Exit;
  lRawStr := TEncoding.UTF8.GetString(lRawBytes);

  FMultipartFallback := TStringList.Create;
  lParts := lRawStr.Split(['--' + lBoundary]);
  for I := 1 to Length(lParts) - 1 do
  begin
    lPart := lParts[I];
    if lPart.StartsWith('--') then
      Continue;
    lSplitPos := Pos(#13#10#13#10, lPart);
    if lSplitPos = 0 then
      Continue;
    lHeaderSection := Trim(Copy(lPart, 1, lSplitPos - 1));
    lBodySection := Copy(lPart, lSplitPos + 4, MaxInt);
    if lBodySection.StartsWith(#13#10) then
      lBodySection := Copy(lBodySection, 3, MaxInt);
    if lBodySection.EndsWith(#13#10) then
      lBodySection := Copy(lBodySection, 1, Length(lBodySection) - 2);

    lFileName := '';
    lFieldName := '';
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
    if (lFileName = '') and (lFieldName <> '') then
      FMultipartFallback.Values[lFieldName] := lBodySection;
  end;
end;

function TMVCWebBrokerRequest.Accept: string;
begin
  Result := FWebRequest.Accept;
end;

function TMVCWebBrokerRequest.PeerIp: string;
begin
  Result := FWebRequest.RemoteAddr;
end;

function TMVCWebBrokerRequest.ClientIp: string;
begin
  // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Forwarded-For
  Result := MVCResolveClientIP(
    String(FWebRequest.GetFieldByName('X-Forwarded-For')),
    String(FWebRequest.GetFieldByName('X-Real-IP')),
    FWebRequest.RemoteAddr,
    MVCTrustProxyForwardedHeaders);
end;

function TMVCWebBrokerRequest.ClientPreferredLanguage: String;
begin
  Result := FWebRequest.GetFieldByName('Accept-Language');
  if Result.Contains(',') then
  begin
    Result := Result.Split([','])[0];
  end;
end;

function TMVCWebBrokerRequest.ContentParam(const AName: string): string;
begin
  Result := FWebRequest.ContentFields.Values[AName];
  if Result = '' then
  begin
    EnsureMultipartFallback;
    if Assigned(FMultipartFallback) then
      Result := FMultipartFallback.Values[AName];
  end;
end;

function TMVCWebBrokerRequest.Cookie(const AName: string): string;
begin
  Result := FWebRequest.CookieFields.Values[AName];
end;

function TMVCWebBrokerRequest.DoGetContent: string;
begin
  Result := FWebRequest.Content;
end;

function TMVCWebBrokerRequest.DoGetContentFieldsText: TStrings;
var
  I: Integer;
begin
  // When the host already populated ContentFields, return it as-is to
  // preserve original casing/order.
  if FWebRequest.ContentFields.Count > 0 then
    Exit(FWebRequest.ContentFields);
  // Otherwise (multipart with parts skipped by old Indy bridge — #758),
  // surface the manually-parsed text fields so callers iterating over
  // ContentFieldsText still see them.
  EnsureMultipartFallback;
  if Assigned(FMultipartFallback) and (FMultipartFallback.Count > 0) then
  begin
    for I := 0 to FMultipartFallback.Count - 1 do
      if FWebRequest.ContentFields.IndexOf(FMultipartFallback[I]) < 0 then
        FWebRequest.ContentFields.Add(FMultipartFallback[I]);
  end;
  Result := FWebRequest.ContentFields;
end;

function TMVCWebBrokerRequest.DoGetContentLength: Int64;
begin
  Result := FWebRequest.ContentLength;
end;

function TMVCWebBrokerRequest.DoGetRawContent: TBytes;
{$IF not Defined(BERLINORBETTER)}
var
  lLen: Int64;
{$ENDIF}
begin
{$IF Defined(BERLINORBETTER)}
  Result := FWebRequest.RawContent;
{$ELSE}
  lLen := FWebRequest.ContentLength;
  SetLength(Result, lLen);
  if lLen > 0 then
    FWebRequest.ReadClient(Result[0], lLen);
{$ENDIF}
end;

procedure TMVCWebBrokerRequest.DoReadTotalContent;
begin
{$IF Defined(BERLINORBETTER)}
  FWebRequest.ReadTotalContent;
{$ENDIF}
end;

function TMVCWebBrokerRequest.GetContentFields: TDictionary<string, string>;
var
  I: Integer;
begin
  if not Assigned(FContentFields) then
  begin
    FContentFields := TDictionary<string, string>.Create;
    // Primary source: WebBroker host (Indy/Apache/ISAPI) parsed ContentFields.
    for I := 0 to Pred(FWebRequest.ContentFields.Count) do
    begin
      FContentFields.AddOrSetValue(LowerCase(FWebRequest.ContentFields.Names[I]),
        FWebRequest.ContentFields.ValueFromIndex[I]);
    end;
    // Fallback: parts that the host skipped (e.g. older Indy ignoring
    // Content-Transfer-Encoding: 8bit — issue #758).
    EnsureMultipartFallback;
    if Assigned(FMultipartFallback) then
      for I := 0 to Pred(FMultipartFallback.Count) do
        FContentFields.AddOrSetValue(LowerCase(FMultipartFallback.Names[I]),
          FMultipartFallback.ValueFromIndex[I]);
  end;
  Result := FContentFields;
end;

function TMVCWebBrokerRequest.GetContentParamsMulti(const AParamName: string): TArray<string>;
begin
  Result := GetMultiParamsAsArray(AParamName, FWebRequest.ContentFields);
end;

function TMVCWebBrokerRequest.GetFiles: TAbstractWebRequestFiles;
begin
  Result := FWebRequest.Files;
end;

function TMVCWebBrokerRequest.GetHeader(const AName: string): string;
begin
  Result := FWebRequest.GetFieldByName(AName);
end;

function TMVCWebBrokerRequest.GetRealHTTPCommand: string;
var
  lCtx: TRttiContext;
  lField: TRttiField;
  lRequestInfo: TObject;
  lRawCommand: string;
  lSpacePos: Integer;
begin
  if FRealHTTPCommandLoaded then
    Exit(FRealHTTPCommand);
  Result := FWebRequest.Method;
  { When this WebBroker application is hosted by TIdHTTPWebBrokerBridge, Indy has
    already rewritten the verb of a POST from any of X-HTTP-Method-Override,
    X-HTTP-Method or X-METHOD-OVERRIDE (IdCustomHTTPServer.pas, "check for
    overrides when LCmd is 'POST'") - and TWebRequest keeps no trace of the
    request line. Routing must never depend on a header a client can write, and
    ISAPI, Apache and HTTP.sys honour none of them: a verb that changes on one
    host only is a way past whatever sits in front of the server. RawHTTPCommand
    is captured before the rewrite, so it still holds the line as it arrived.
    Under any other WebBroker host the class check fails and nothing here
    applies. }
  if (FWebRequest <> nil) and IsOrInheritsFromTIdHTTPAppRequest(FWebRequest.ClassType) then
  begin
    lCtx := TRttiContext.Create;
    try
      lField := lCtx.GetType(FWebRequest.ClassType).GetField('FRequestInfo');
      if Assigned(lField) then
      begin
        lRequestInfo := lField.GetValue(FWebRequest).AsObject;
        if Assigned(lRequestInfo) then
        begin
          lField := lCtx.GetType(lRequestInfo.ClassType).GetField('FRawHTTPCommand');
          if Assigned(lField) then
          begin
            lRawCommand := lField.GetValue(lRequestInfo).AsString;
            if lRawCommand <> '' then
            begin
              lSpacePos := Pos(' ', lRawCommand);
              if lSpacePos > 0 then
                SetLength(lRawCommand, lSpacePos - 1);
              Result := UpperCase(lRawCommand);
            end;
          end;
        end;
      end;
    finally
      lCtx.Free;
    end;
  end;
  FRealHTTPCommand := Result;
  FRealHTTPCommandLoaded := True;
end;

function TMVCWebBrokerRequest.GetHTTPMethod: TMVCHTTPMethodType;
begin
  Result := TMVCRouter.StringMethodToHTTPMetod(GetRealHTTPCommand);
end;

function TMVCWebBrokerRequest.GetHTTPMethodAsString: string;
begin
  Result := GetRealHTTPCommand;
end;

function TMVCWebBrokerRequest.GetIsAjax: Boolean;
begin
  Result := LowerCase(FWebRequest.GetFieldByName('X-Requested-With')) = 'xmlhttprequest';
end;

function TMVCWebBrokerRequest.GetParamAsInt64(const AParamName: string): Int64;
begin
  Result := StrToInt64(GetParams(AParamName));
end;

function TMVCWebBrokerRequest.GetParamAsInteger(const AParamName: string): Integer;
begin
  Result := StrToInt(GetParams(AParamName));
end;

function TMVCWebBrokerRequest.GetParamNames: TArray<string>;
var
  I: Integer;
  Names: TList<string>;
  N: string;
begin
  Names := TList<string>.Create;
  try
    if Assigned(FParamsTable) and (FParamsTable.Keys.Count > 0) then
    begin
      for N in FParamsTable.Keys.ToArray do
      begin
        Names.Add(N);
      end;
    end;

    if (FWebRequest.QueryFields.Count > 0) then
    begin
      for I := 0 to FWebRequest.QueryFields.Count - 1 do
      begin
        Names.Add(FWebRequest.QueryFields.Names[I]);
      end;
    end;

    if (FWebRequest.ContentFields.Count > 0) then
    begin
      for I := 0 to FWebRequest.ContentFields.Count - 1 do
      begin
        if Names.IndexOf(FWebRequest.ContentFields.Names[I]) = -1 then
        begin
          Names.Add(FWebRequest.ContentFields.Names[I]);
        end;
      end;
    end;

    if (FWebRequest.CookieFields.Count > 0) then
    begin
      for I := 0 to FWebRequest.CookieFields.Count - 1 do
      begin
        Names.Add(FWebRequest.CookieFields.Names[I]);
      end;
    end;

    Result := Names.ToArray;
  finally
    Names.Free;
  end;
end;

function TMVCWebBrokerRequest.GetParams(const AParamName: string): string;
begin
  if (not Assigned(FParamsTable)) or (not FParamsTable.TryGetValue(AParamName, Result)) then
  begin
    Result := '';
    if string(FWebRequest.ContentType).StartsWith(TMVCMediaType.APPLICATION_FORM_URLENCODED, True) or
      string(FWebRequest.ContentType).StartsWith(TMVCMediaType.MULTIPART_FORM_DATA, True) then
      Result := FWebRequest.ContentFields.Values[AParamName];
    if Result.IsEmpty then
      Result := FWebRequest.QueryFields.Values[AParamName];
  end;
end;

function TMVCWebBrokerRequest.GetPathInfo: string;
var
  LQueryPos: Integer;
begin
  // Apache (Web.HTTPD24Impl) splits the URI CGI-style — the first
  // matching segment goes to ScriptName and the rest into PathInfo, and
  // the split point is not always on a '/' boundary (colons, dots and
  // other characters can confuse it). ScriptName+PathInfo is therefore
  // not safe to concatenate.
  // TWebRequest.URL maps to Apache's unparsed_uri (and to the full URL
  // under the Indy bridge), so stripping the query string yields the
  // same original path in every WebBroker host.
  Result := FWebRequest.URL;
  LQueryPos := Pos('?', Result);
  if LQueryPos > 0 then
    Result := Copy(Result, 1, LQueryPos - 1);
  if Result = '' then
    Result := FWebRequest.PathInfo;
end;

function TMVCWebBrokerRequest.GetQueryParams: TDictionary<string, string>;
var
  I: Integer;
  lRow: String;
  lName: String;
begin
  if not Assigned(FQueryParams) then
  begin
    FQueryParams := TDictionary<string, string>.Create;
    for I := 0 to Pred(FWebRequest.QueryFields.Count) do
    begin
      { First value wins, matching QueryStringParam (TStringList.Values returns
        the first match) and the other two hosts. Add() alone would also raise
        on a repeated key, turning ?a=1&a=2 into a 500 on this host only. }
      lRow := FWebRequest.QueryFields[i];
      if lRow.Contains('=') then
      begin
        lName := LowerCase(Trim(FWebRequest.QueryFields.Names[I]));
        if not FQueryParams.ContainsKey(lName) then
          FQueryParams.Add(lName, FWebRequest.QueryFields.ValueFromIndex[I]);
      end
      else
      begin
        { A bare key: WebBroker keeps the token without a '=', so QueryStringParam
          (TStrings.Values) skips this row and finds a LATER "a=1" instead, while
          this dictionary answers ''. The other two hosts append the '=' when they
          parse, so they agree; here the first-wins entry has to be recorded for
          both readers. }
        lName := LowerCase(lRow);
        if not FQueryParams.ContainsKey(lName) then
          FQueryParams.Add(lName, '');
      end;
    end;
  end;
  Result := FQueryParams;
end;

function TMVCWebBrokerRequest.GetQueryParamsMulti(
  const AParamName: string): TArray<string>;
begin
  Result := GetMultiParamsAsArray(AParamName, FWebRequest.QueryFields);
end;

function TMVCWebBrokerRequest.GetRawWebRequest: TWebRequest;
begin
  Result := FWebRequest;
end;

function TMVCWebBrokerRequest.QueryString: string;
begin
  Result := FWebRequest.Query;
end;

function TMVCWebBrokerRequest.QueryStringParam(const AName: string): string;
var
  I, lNameLen: Integer;
  lRow: string;
begin
  { First match wins, bare keys included. TStrings.Values skips a row with no
    '=', so ?a&a=1 answered '1' here while QueryParams answered '' - the two
    accessors disagreeing on the same request is how a value gets validated
    through one and used through the other. }
  lNameLen := Length(AName);
  for I := 0 to FWebRequest.QueryFields.Count - 1 do
  begin
    lRow := FWebRequest.QueryFields[I];
    if SameText(lRow, AName) then
      Exit('');
    if (Length(lRow) > lNameLen) and (lRow[lNameLen + 1] = '=') and
      SameText(Copy(lRow, 1, lNameLen), AName) then
      Exit(Copy(lRow, lNameLen + 2, MaxInt));
  end;
  Result := '';
end;

function TMVCWebBrokerRequest.QueryStringParamExists(const AName: string): Boolean;
begin
  Result := QueryStringParam(AName) <> EmptyStr;
end;

function TMVCWebBrokerRequest.QueryStringParams: TStrings;
begin
  Result := FWebRequest.QueryFields;
end;

function TMVCWebBrokerRequest.GetRawPathInfo: string;
begin
  // See comment on GetPathInfo: TWebRequest.URL is the only field that
  // carries the full original path intact under every WebBroker host.
  Result := GetPathInfo;
end;

function TMVCWebBrokerRequest.GetClientConnection: TObject;
var
  LCtx: TRttiContext;
  LField: TRttiField;
begin
  // TIdHTTPAppRequest (IdHTTPWebBrokerBridge) holds the TIdContext in its
  // FThread field. Reach it via RTTI so SSE and other streaming features
  // work under WebBroker backed by Indy, without adding a compile-time
  // dependency on IdHTTPWebBrokerBridge here. Subclasses are supported
  // by walking the class hierarchy instead of matching ClassName exactly.
  // Under ISAPI/Apache (TISAPIRequest / TApacheRequest) the check fails
  // and the caller (SSE writer) correctly falls back to "not supported".
  Result := nil;
  if FWebRequest = nil then
    Exit;
  if not IsOrInheritsFromTIdHTTPAppRequest(FWebRequest.ClassType) then
    Exit;
  LCtx := TRttiContext.Create;
  try
    LField := LCtx.GetType(FWebRequest.ClassType).GetField('FThread');
    if Assigned(LField) then
      Result := LField.GetValue(FWebRequest).AsObject;
  finally
    LCtx.Free;
  end;
end;

function TMVCWebBrokerRequest.GetContentLength: Int64;
begin
  Result := FWebRequest.ContentLength;
end;

function TMVCWebBrokerRequest.GetMethod: string;
begin
  Result := GetRealHTTPCommand;
end;

function TMVCWebBrokerRequest.GetHost: string;
begin
  Result := FWebRequest.Host;
end;

function TMVCWebBrokerRequest.GetServerPort: Integer;
begin
  Result := FWebRequest.ServerPort;
end;

function TMVCWebBrokerRequest.GetUserAgent: string;
begin
  Result := FWebRequest.GetFieldByName('User-Agent');
end;

function TMVCWebBrokerRequest.GetAuthorization: string;
begin
  Result := FWebRequest.Authorization;
end;

function TMVCWebBrokerRequest.GetQueryFieldsDelimitedText: string;
begin
  Result := FWebRequest.QueryFields.DelimitedText;
end;

function TMVCWebBrokerRequest.GetRawContent: TBytes;
begin
  Result := DoGetRawContent;
end;

end.
