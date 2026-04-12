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
    function ClientIp: string; override;
    function ClientPreferredLanguage: String; override;
    function QueryString: string; override;
    function QueryStringParam(const AName: string): string; override;
    function QueryStringParamExists(const AName: string): Boolean; override;
    function QueryStringParams: TStrings; override;
    function Accept: string; override;
    function ContentParam(const AName: string): string; override;
    function Cookie(const AName: string): string; override;
    property WebRequest: TWebRequest read FWebRequest;
  end;

implementation

uses
  MVCFramework.Router;

{ TMVCWebBrokerRequest }

constructor TMVCWebBrokerRequest.Create(const AWebRequest: TWebRequest;
  const ASerializers: TDictionary<string, IMVCSerializer>);
begin
  FWebRequest := AWebRequest;
  inherited Create(ASerializers);
  DefineContentType;
end;

function TMVCWebBrokerRequest.Accept: string;
begin
  Result := FWebRequest.Accept;
end;

function TMVCWebBrokerRequest.ClientIp: string;
var
  lValue: string;
  function GetFirst(const Value: String): String; inline;
  begin
    Result := Value.Split([',',';'])[0].Trim();
  end;
begin
  // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Forwarded-For
  lValue := String(FWebRequest.GetFieldByName('X-Forwarded-For'));
  if not lValue.IsEmpty then
  begin
    Exit(GetFirst(lValue));
  end;

  lValue := String(FWebRequest.GetFieldByName('X-Real-IP'));
  if not lValue.IsEmpty then
  begin
    Exit(GetFirst(lValue));
  end;

  Result := FWebRequest.RemoteAddr;
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
begin
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
    for I := 0 to Pred(FWebRequest.ContentFields.Count) do
    begin
      FContentFields.AddOrSetValue(LowerCase(FWebRequest.ContentFields.Names[I]),
        FWebRequest.ContentFields.ValueFromIndex[I]);
    end;
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

function TMVCWebBrokerRequest.GetHTTPMethod: TMVCHTTPMethodType;
begin
  Result := TMVCRouter.StringMethodToHTTPMetod(FWebRequest.Method);
end;

function TMVCWebBrokerRequest.GetHTTPMethodAsString: string;
begin
  Result := FWebRequest.Method;
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
begin
  // Apache (Web.HTTPD24Impl) splits the URI CGI-style: the first segment
  // lands in ScriptName and the rest in PathInfo (often empty). Classic
  // Indy bridge (IdHTTPWebBrokerBridge) keeps the full path in PathInfo
  // with ScriptName empty. Concatenating the two yields the full URI in
  // both environments.
  Result := FWebRequest.ScriptName + FWebRequest.PathInfo;
  if Result = '' then
    Result := FWebRequest.PathInfo;
end;

function TMVCWebBrokerRequest.GetQueryParams: TDictionary<string, string>;
var
  I: Integer;
  lRow: String;
begin
  if not Assigned(FQueryParams) then
  begin
    FQueryParams := TDictionary<string, string>.Create;
    for I := 0 to Pred(FWebRequest.QueryFields.Count) do
    begin
      lRow := FWebRequest.QueryFields[i];
      if lRow.Contains('=') then
      begin
        FQueryParams.Add(
          LowerCase(Trim(FWebRequest.QueryFields.Names[I])),
          FWebRequest.QueryFields.ValueFromIndex[I]);
      end
      else
      begin
        FQueryParams.AddOrSetValue(LowerCase(lRow), '');
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
begin
  Result := FWebRequest.QueryFields.Values[AName];
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
  // See comment on GetPathInfo for why ScriptName+PathInfo is used.
  // RawPathInfo in TWebRequest falls back to PathInfo (same source under
  // Apache), so the same reconstruction applies.
  Result := FWebRequest.ScriptName + FWebRequest.PathInfo;
  if Result = '' then
    Result := FWebRequest.RawPathInfo;
end;

function TMVCWebBrokerRequest.GetContentLength: Int64;
begin
  Result := FWebRequest.ContentLength;
end;

function TMVCWebBrokerRequest.GetMethod: string;
begin
  Result := FWebRequest.Method;
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
