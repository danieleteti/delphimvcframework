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
    procedure ParseCookies;
    procedure EnsureQueryStringParams;
    procedure LoadBody;
    procedure LoadRawContent;
    procedure ParseMultipartContent;
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
  FFiles := nil;
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
      lName := TIdURI.URLDecode(StringReplace(Copy(lPair, 1, lEqPos - 1), '+', ' ', [rfReplaceAll]));
      lValue := TIdURI.URLDecode(StringReplace(Copy(lPair, lEqPos + 1, MaxInt), '+', ' ', [rfReplaceAll]));
      FQueryStringParams.Add(lName + '=' + lValue);
    end
    else
    begin
      FQueryStringParams.Add(TIdURI.URLDecode(StringReplace(lPair, '+', ' ', [rfReplaceAll])) + '=');
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

procedure TMVCIndyDirectRequest.ParseMultipartContent;
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

  lContentType := FRequestInfo.ContentType;
  if not lContentType.ToLower.Contains('multipart/form-data') then
  begin
    FFiles := TMVCIndyRequestFiles.Create;
    Exit;
  end;

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

  if lBoundary = '' then
  begin
    FFiles := TMVCIndyRequestFiles.Create;
    Exit;
  end;

  FFiles := TMVCIndyRequestFiles.Create;

  // Read raw content
  LoadRawContent;
  if Length(FCachedRawContent) = 0 then
    Exit;
  lRawStr := TEncoding.UTF8.GetString(FCachedRawContent);

  // Split by boundary
  lParts := lRawStr.Split(['--' + lBoundary]);

  for I := 1 to Length(lParts) - 1 do // skip first empty part
  begin
    lPart := lParts[I];
    if lPart.StartsWith('--') then
      Continue; // end boundary marker

    // Split headers from body (separated by double CRLF)
    lSplitPos := Pos(#13#10#13#10, lPart);
    if lSplitPos = 0 then
      Continue;

    lHeaderSection := Trim(Copy(lPart, 1, lSplitPos - 1));
    lBodySection := Copy(lPart, lSplitPos + 4, MaxInt);
    // Remove trailing CRLF
    if lBodySection.EndsWith(#13#10) then
      lBodySection := Copy(lBodySection, 1, Length(lBodySection) - 2);

    // Parse Content-Disposition for filename and field name
    lFileName := '';
    lFieldName := '';
    lPartContentType := 'application/octet-stream';

    // Extract filename
    lFnPos := Pos('filename="', lHeaderSection);
    if lFnPos > 0 then
    begin
      lFileName := Copy(lHeaderSection, lFnPos + 10, MaxInt);
      lFileName := Copy(lFileName, 1, Pos('"', lFileName) - 1);
    end;

    // Extract field name
    lNamePos := Pos('name="', lHeaderSection);
    if lNamePos > 0 then
    begin
      lFieldName := Copy(lHeaderSection, lNamePos + 6, MaxInt);
      lFieldName := Copy(lFieldName, 1, Pos('"', lFieldName) - 1);
    end;

    // Only add as file if it has a filename
    if lFileName <> '' then
    begin
      lBodyStream := TMemoryStream.Create;
      lBodyBytes := TEncoding.UTF8.GetBytes(lBodySection);
      if Length(lBodyBytes) > 0 then
        lBodyStream.WriteBuffer(lBodyBytes[0], Length(lBodyBytes));
      lBodyStream.Position := 0;
      FFiles.Add(TMVCIndyRequestFile.Create(lFieldName, lFileName, lPartContentType, lBodyStream));
    end;
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
