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
    procedure ParseCookies;
    procedure EnsureQueryStringParams;
    procedure LoadBody;
    procedure LoadRawContent;
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
    constructor Create(const AContext: TIdContext;
      const ARequestInfo: TIdHTTPRequestInfo;
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

implementation

uses
  System.StrUtils,
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
  inherited Create(ASerializers);
  DefineContentType;
end;

destructor TMVCIndyDirectRequest.Destroy;
begin
  FQueryStringParams.Free;
  FCookies.Free;
  FCachedContentFieldsText.Free;
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
    if lEqPos > 0 then
      FCookies.Values[Trim(Copy(lPair, 1, lEqPos - 1))] := Trim(Copy(lPair, lEqPos + 1, MaxInt))
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
      lName := TIdURI.URLDecode(Copy(lPair, 1, lEqPos - 1));
      lValue := TIdURI.URLDecode(Copy(lPair, lEqPos + 1, MaxInt));
      FQueryStringParams.Add(lName + '=' + lValue);
    end
    else
    begin
      FQueryStringParams.Add(TIdURI.URLDecode(lPair) + '=');
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

function TMVCIndyDirectRequest.GetPathInfo: string;
begin
  Result := FRequestInfo.Document;
end;

function TMVCIndyDirectRequest.GetHTTPMethod: TMVCHTTPMethodType;
begin
  Result := TMVCRouter.StringMethodToHTTPMetod(FRequestInfo.Command);
end;

function TMVCIndyDirectRequest.GetHTTPMethodAsString: string;
begin
  Result := FRequestInfo.Command;
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
  Result := nil; // TODO: implement multipart file upload parsing for Indy direct
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
            lName := TIdURI.URLDecode(Copy(lPair, 1, lEqPos - 1));
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
begin
  lResult := TList<string>.Create;
  try
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
            lName := TIdURI.URLDecode(Copy(lPair, 1, lEqPos - 1));
            lValue := TIdURI.URLDecode(Copy(lPair, lEqPos + 1, MaxInt));
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
            lName := TIdURI.URLDecode(Copy(lPair, 1, lEqPos - 1));
            lValue := TIdURI.URLDecode(Copy(lPair, lEqPos + 1, MaxInt));
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
  lName, lValue: string;
  lEqPos: Integer;
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
            lName := TIdURI.URLDecode(Copy(lPair, 1, lEqPos - 1));
            lValue := TIdURI.URLDecode(Copy(lPair, lEqPos + 1, MaxInt));
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
  Result := FRequestInfo.Command;
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

function TMVCIndyDirectRequest.ClientIp: string;
var
  lValue: string;
  function GetFirst(const Value: String): String; inline;
  begin
    Result := Value.Split([',',';'])[0].Trim();
  end;
begin
  lValue := FRequestInfo.RawHeaders.Values['X-Forwarded-For'];
  if not lValue.IsEmpty then
    Exit(GetFirst(lValue));

  lValue := FRequestInfo.RawHeaders.Values['X-Real-IP'];
  if not lValue.IsEmpty then
    Exit(GetFirst(lValue));

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

end.
