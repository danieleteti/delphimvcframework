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

unit MVCFramework.CrossSocket.Request;

{$I dmvcframework.inc}

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  Web.HTTPApp,
  Net.CrossHttpServer, Net.CrossHttpParams,
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Intf;

type
  TMVCCrossSocketRequestFile = class(TAbstractWebRequestFile)
  private
    FFieldName: string;
    FFileName: string;
    FStream: TStream;
    FContentType: string;
    FOwnsStream: Boolean;
  protected
    function GetFieldName: string; override;
    function GetFileName: string; override;
    function GetStream: TStream; override;
    function GetContentType: string; override;
  public
    constructor Create(const AFieldName, AFileName, AContentType: string;
      AStream: TStream; AOwnsStream: Boolean = True);
    destructor Destroy; override;
  end;

  TMVCCrossSocketRequestFiles = class(TAbstractWebRequestFiles)
  private
    FFiles: TObjectList<TMVCCrossSocketRequestFile>;
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): TAbstractWebRequestFile; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AFile: TMVCCrossSocketRequestFile);
  end;

  TMVCCrossSocketRequest = class(TMVCWebRequest)
  private
    FRequest: ICrossHttpRequest;
    FConnection: ICrossHttpConnection;
    FQueryStringParams: TStringList;
    FCachedRawContent: TBytes;
    FCachedRawContentLoaded: Boolean;
    FCachedContentFieldsText: TStringList;
    FFiles: TMVCCrossSocketRequestFiles;
    procedure EnsureQueryStringParams;
    procedure LoadRawContent;
    procedure ParseFiles;
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
    function GetClientConnection: TObject; override;
    constructor Create(const AConnection: ICrossHttpConnection;
      const ARequest: ICrossHttpRequest;
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
  MVCFramework.Router, IdURI;

{ TMVCCrossSocketRequestFile }

constructor TMVCCrossSocketRequestFile.Create(const AFieldName, AFileName, AContentType: string;
  AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;
  FFieldName := AFieldName;
  FFileName := AFileName;
  FContentType := AContentType;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
end;

destructor TMVCCrossSocketRequestFile.Destroy;
begin
  if FOwnsStream then
    FStream.Free;
  inherited;
end;

function TMVCCrossSocketRequestFile.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TMVCCrossSocketRequestFile.GetFileName: string;
begin
  Result := FFileName;
end;

function TMVCCrossSocketRequestFile.GetStream: TStream;
begin
  Result := FStream;
end;

function TMVCCrossSocketRequestFile.GetContentType: string;
begin
  Result := FContentType;
end;

{ TMVCCrossSocketRequestFiles }

constructor TMVCCrossSocketRequestFiles.Create;
begin
  inherited Create;
  FFiles := TObjectList<TMVCCrossSocketRequestFile>.Create(True);
end;

destructor TMVCCrossSocketRequestFiles.Destroy;
begin
  FFiles.Free;
  inherited;
end;

function TMVCCrossSocketRequestFiles.GetCount: Integer;
begin
  Result := FFiles.Count;
end;

function TMVCCrossSocketRequestFiles.GetItem(AIndex: Integer): TAbstractWebRequestFile;
begin
  Result := FFiles[AIndex];
end;

procedure TMVCCrossSocketRequestFiles.Add(AFile: TMVCCrossSocketRequestFile);
begin
  FFiles.Add(AFile);
end;

{ TMVCCrossSocketRequest }

constructor TMVCCrossSocketRequest.Create(const AConnection: ICrossHttpConnection;
  const ARequest: ICrossHttpRequest;
  const ASerializers: TDictionary<string, IMVCSerializer>);
begin
  FRequest := ARequest;
  FConnection := AConnection;
  FQueryStringParams := nil;
  FCachedRawContentLoaded := False;
  FCachedContentFieldsText := nil;
  FFiles := nil;
  inherited Create(ASerializers);
  DefineContentType;
end;

destructor TMVCCrossSocketRequest.Destroy;
begin
  FQueryStringParams.Free;
  FCachedContentFieldsText.Free;
  FFiles.Free;
  inherited;
end;

function TMVCCrossSocketRequest.GetHeader(const AName: string): string;
begin
  Result := FRequest.Header[AName];
end;

function TMVCCrossSocketRequest.GetPathInfo: string;
begin
  Result := FRequest.Path;
end;

function TMVCCrossSocketRequest.GetHTTPMethod: TMVCHTTPMethodType;
begin
  Result := TMVCRouter.StringMethodToHTTPMetod(FRequest.Method);
end;

function TMVCCrossSocketRequest.GetHTTPMethodAsString: string;
begin
  Result := FRequest.Method;
end;

function TMVCCrossSocketRequest.GetParams(const AParamName: string): string;
begin
  if (not Assigned(FParamsTable)) or (not FParamsTable.TryGetValue(AParamName, Result)) then
  begin
    Result := '';
    if FRequest.ContentType.StartsWith(TMVCMediaType.APPLICATION_FORM_URLENCODED, True) or
       FRequest.ContentType.StartsWith(TMVCMediaType.MULTIPART_FORM_DATA, True) then
      Result := ContentParam(AParamName);
    if Result.IsEmpty then
      Result := QueryStringParam(AParamName);
  end;
end;

function TMVCCrossSocketRequest.GetIsAjax: Boolean;
begin
  Result := LowerCase(FRequest.Header['X-Requested-With']) = 'xmlhttprequest';
end;

function TMVCCrossSocketRequest.GetParamAsInteger(const AParamName: string): Integer;
begin
  Result := StrToInt(GetParams(AParamName));
end;

function TMVCCrossSocketRequest.GetParamAsInt64(const AParamName: string): Int64;
begin
  Result := StrToInt64(GetParams(AParamName));
end;

function TMVCCrossSocketRequest.GetFiles: TAbstractWebRequestFiles;
begin
  ParseFiles;
  Result := FFiles;
end;

procedure TMVCCrossSocketRequest.ParseFiles;
var
  lMultiPart: THttpMultiPartFormData;
  I: Integer;
  lField: TFormField;
  lStream: TMemoryStream;
begin
  if Assigned(FFiles) then Exit;
  FFiles := TMVCCrossSocketRequestFiles.Create;

  if FRequest.BodyType <> btMultiPart then Exit;

  lMultiPart := THttpMultiPartFormData(FRequest.Body);
  if lMultiPart = nil then Exit;

  for I := 0 to lMultiPart.Count - 1 do
  begin
    lField := lMultiPart.Items[I];
    if lField.FileName <> '' then
    begin
      lStream := TMemoryStream.Create;
      if (lField.Value <> nil) and (lField.Value.Size > 0) then
      begin
        lField.Value.Position := 0;
        lStream.CopyFrom(lField.Value, lField.Value.Size);
        lStream.Position := 0;
      end;
      FFiles.Add(TMVCCrossSocketRequestFile.Create(
        lField.Name, lField.FileName, lField.ContentType, lStream, True));
    end;
  end;
end;

function TMVCCrossSocketRequest.GetParamNames: TArray<string>;
var
  I: Integer;
  Names: TList<string>;
  N: string;
begin
  Names := TList<string>.Create;
  try
    if Assigned(FParamsTable) then
      for N in FParamsTable.Keys.ToArray do
        Names.Add(N);

    EnsureQueryStringParams;
    for I := 0 to FQueryStringParams.Count - 1 do
      Names.Add(FQueryStringParams.Names[I]);

    Result := Names.ToArray;
  finally
    Names.Free;
  end;
end;

procedure TMVCCrossSocketRequest.EnsureQueryStringParams;
var
  I: Integer;
  lQuery: THttpUrlParams;
begin
  if Assigned(FQueryStringParams) then Exit;
  FQueryStringParams := TStringList.Create;
  lQuery := FRequest.Query;
  if lQuery = nil then Exit;
  for I := 0 to lQuery.Count - 1 do
    FQueryStringParams.Add(lQuery.Items[I].Name + '=' + lQuery.Items[I].Value);
end;

function TMVCCrossSocketRequest.GetQueryParamsMulti(const AParamName: string): TArray<string>;
var
  I: Integer;
  lResult: TList<string>;
begin
  EnsureQueryStringParams;
  lResult := TList<string>.Create;
  try
    for I := 0 to FQueryStringParams.Count - 1 do
      if SameText(FQueryStringParams.Names[I], AParamName) then
        lResult.Add(FQueryStringParams.ValueFromIndex[I]);
    Result := lResult.ToArray;
  finally
    lResult.Free;
  end;
end;

function TMVCCrossSocketRequest.GetContentParamsMulti(const AParamName: string): TArray<string>;
begin
  // CrossSocket handles content params via Body object
  Result := nil;
end;

function TMVCCrossSocketRequest.GetContentFields: TDictionary<string, string>;
var
  lUrlEncoded: THttpUrlParams;
  I: Integer;
begin
  if not Assigned(FContentFields) then
  begin
    FContentFields := TDictionary<string, string>.Create;
    if FRequest.BodyType = btUrlEncoded then
    begin
      lUrlEncoded := THttpUrlParams(FRequest.Body);
      if lUrlEncoded <> nil then
        for I := 0 to lUrlEncoded.Count - 1 do
          FContentFields.AddOrSetValue(LowerCase(lUrlEncoded.Items[I].Name),
            lUrlEncoded.Items[I].Value);
    end;
  end;
  Result := FContentFields;
end;

function TMVCCrossSocketRequest.GetQueryParams: TDictionary<string, string>;
var
  I: Integer;
begin
  if not Assigned(FQueryParams) then
  begin
    FQueryParams := TDictionary<string, string>.Create;
    EnsureQueryStringParams;
    for I := 0 to FQueryStringParams.Count - 1 do
    begin
      if FQueryStringParams.Names[I] <> '' then
        FQueryParams.AddOrSetValue(LowerCase(FQueryStringParams.Names[I]),
          FQueryStringParams.ValueFromIndex[I])
      else
        FQueryParams.AddOrSetValue(LowerCase(FQueryStringParams[I]), '');
    end;
  end;
  Result := FQueryParams;
end;

function TMVCCrossSocketRequest.GetRawWebRequest: TWebRequest;
begin
  Result := nil;
end;

procedure TMVCCrossSocketRequest.DoReadTotalContent;
begin
  // CrossSocket already has full content available
end;

procedure TMVCCrossSocketRequest.LoadRawContent;
var
  lStream: TStream;
begin
  if FCachedRawContentLoaded then Exit;
  FCachedRawContentLoaded := True;
  if FRequest.Body = nil then
  begin
    FCachedRawContent := nil;
    Exit;
  end;
  case FRequest.BodyType of
    btBinary:
    begin
      lStream := TStream(FRequest.Body);
      lStream.Position := 0;
      SetLength(FCachedRawContent, lStream.Size);
      if lStream.Size > 0 then
        lStream.ReadBuffer(FCachedRawContent[0], lStream.Size);
      lStream.Position := 0;
    end;
  else
    FCachedRawContent := TEncoding.UTF8.GetBytes(DoGetContent);
  end;
end;

function TMVCCrossSocketRequest.DoGetRawContent: TBytes;
begin
  LoadRawContent;
  Result := FCachedRawContent;
end;

function TMVCCrossSocketRequest.DoGetContentLength: Int64;
begin
  Result := FRequest.ContentLength;
end;

function TMVCCrossSocketRequest.DoGetContent: string;
begin
  if FRequest.Body is TStream then
  begin
    LoadRawContent;
    Result := TEncoding.UTF8.GetString(FCachedRawContent);
  end
  else
    Result := '';
end;

function TMVCCrossSocketRequest.DoGetContentFieldsText: TStrings;
var
  lUrlEncoded: THttpUrlParams;
  I: Integer;
begin
  if not Assigned(FCachedContentFieldsText) then
  begin
    FCachedContentFieldsText := TStringList.Create;
    if FRequest.BodyType = btUrlEncoded then
    begin
      lUrlEncoded := THttpUrlParams(FRequest.Body);
      if lUrlEncoded <> nil then
        for I := 0 to lUrlEncoded.Count - 1 do
          FCachedContentFieldsText.Add(lUrlEncoded.Items[I].Name + '=' + lUrlEncoded.Items[I].Value);
    end;
  end;
  Result := FCachedContentFieldsText;
end;

function TMVCCrossSocketRequest.GetRawPathInfo: string;
begin
  Result := FRequest.RawPathAndParams;
end;

function TMVCCrossSocketRequest.GetContentLength: Int64;
begin
  Result := FRequest.ContentLength;
end;

function TMVCCrossSocketRequest.GetMethod: string;
begin
  Result := FRequest.Method;
end;

function TMVCCrossSocketRequest.GetHost: string;
begin
  Result := FRequest.HostName;
end;

function TMVCCrossSocketRequest.GetServerPort: Integer;
begin
  Result := FRequest.HostPort;
end;

function TMVCCrossSocketRequest.GetUserAgent: string;
begin
  Result := FRequest.UserAgent;
end;

function TMVCCrossSocketRequest.GetAuthorization: string;
begin
  Result := FRequest.Authorization;
end;

function TMVCCrossSocketRequest.GetQueryFieldsDelimitedText: string;
begin
  EnsureQueryStringParams;
  Result := FQueryStringParams.DelimitedText;
end;

function TMVCCrossSocketRequest.GetRawContent: TBytes;
begin
  Result := DoGetRawContent;
end;

function TMVCCrossSocketRequest.GetClientConnection: TObject;
begin
  Result := Pointer(FConnection);
end;

function TMVCCrossSocketRequest.ClientIp: string;
var
  lValue: string;
begin
  lValue := FRequest.XForwardedFor;
  if not lValue.IsEmpty then
    Exit(lValue.Split([',', ';'])[0].Trim);

  lValue := FRequest.Header['X-Real-IP'];
  if not lValue.IsEmpty then
    Exit(lValue.Split([',', ';'])[0].Trim);

  Result := FConnection.PeerAddr;
end;

function TMVCCrossSocketRequest.ClientPreferredLanguage: String;
begin
  Result := FRequest.AcceptLanguage;
  if Result.Contains(',') then
    Result := Result.Split([','])[0];
end;

function TMVCCrossSocketRequest.QueryString: string;
var
  lFull: string;
  lQPos: Integer;
begin
  lFull := FRequest.RawPathAndParams;
  lQPos := Pos('?', lFull);
  if lQPos > 0 then
    Result := Copy(lFull, lQPos + 1, MaxInt)
  else
    Result := '';
end;

function TMVCCrossSocketRequest.QueryStringParam(const AName: string): string;
begin
  EnsureQueryStringParams;
  Result := FQueryStringParams.Values[AName];
end;

function TMVCCrossSocketRequest.QueryStringParamExists(const AName: string): Boolean;
begin
  Result := QueryStringParam(AName) <> EmptyStr;
end;

function TMVCCrossSocketRequest.QueryStringParams: TStrings;
begin
  EnsureQueryStringParams;
  Result := FQueryStringParams;
end;

function TMVCCrossSocketRequest.Accept: string;
begin
  Result := FRequest.Accept;
end;

function TMVCCrossSocketRequest.ContentParam(const AName: string): string;
var
  lFields: TDictionary<string, string>;
begin
  lFields := GetContentFields;
  if not lFields.TryGetValue(LowerCase(AName), Result) then
    Result := '';
end;

function TMVCCrossSocketRequest.Cookie(const AName: string): string;
begin
  Result := FRequest.Cookies[AName];
end;

end.
