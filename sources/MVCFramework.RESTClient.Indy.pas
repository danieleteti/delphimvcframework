// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.RESTClient.Indy;

{$I dmvcframework.inc}

interface

uses
  System.Classes,
  IdHTTP,
  IdURI,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  IdMultipartFormData,
  System.SysUtils,
  Data.DB,
  IdIOHandler,
  IdCompressorZLib,
  IdSSLOpenSSL,
  System.Generics.Collections,
  System.StrUtils,
  Web.HTTPApp,
  IdCookie,
  MVCFramework.Serializer.Intf;

type
  ERESTClientException = class(Exception);

  TArrayOfString = array of string;
  // THTTPCommand = (httpGET, httpPOST, httpPUT, httpDELETE, httpPATCH, httpTRACE);

  [MVCNameCaseAttribute(ncLowerCase)]
  TMVCExceptionObj = class(TObject)
  private
    FStatus: string;
    Fclassname: string;
    FMessage: string;
    FHttp_error: Integer;
    FErrorNumber: Integer;
  public
    [MVCNameAs('reasonstring')]
    property Status: string read FStatus write FStatus;
    [MVCNameAs('classname')]
    property ExceptionClassname: string read Fclassname write Fclassname;
    [MVCNameAs('message')]
    property ExceptionMessage: string read FMessage write FMessage;
    [MVCNameAs('statuscode')]
    property HTTPError: Integer read FHttp_error write FHttp_error;
    [MVCNameAs('errornumber')]
    property ErrorNumber: Integer read FErrorNumber write FErrorNumber;
  end;

  IRESTResponse = interface
    ['{E96178DE-79D4-4EF6-88F6-1A677207265A}']
    function Body: TStream;
    function BodyAsString: string;
    // function BodyAsJSONValue: TJSONValue;
    // function BodyAsJSONObject: TJSONObject;
    // function BodyAsJSONArray: TJSONArray;

    procedure UpdateResponseCode(const AResponseCode: Word);
    procedure UpdateResponseText(const AResponseText: string);
    procedure UpdateHeaders(AHeaders: TStrings);

    function ResponseCode: Word;
    function ResponseText: string;

    function Headers: TStringlist;
    function HeaderValue(const AName: string): string;

    function ContentType: string;
    function ContentTypeCharset: string;
    function ContentEncoding: string;

    function GetCookies: TIdCookies;
    procedure SetCookies(aCookie: TIdCookies);

    function GetHasError: Boolean;
    procedure SetHasError(const aHasError: Boolean);

    function Error: TMVCExceptionObj;

    property Cookies: TIdCookies read GetCookies write SetCookies;
    property HasError: Boolean read GetHasError write SetHasError;
  end;

  TRESTClient = class(TInterfacedObject)
  strict private
    FHost: string;
    FPort: Word;
    FBodyParams: TStringlist;
    FQueryStringParams: TStringlist;
    FAccept: string;
    FRawBody: TStringStream;
    FHTTP: TIdHTTP;
    FContentType: string;
    FPrimaryThread: TThread;
    FProtocol: string;
    FRequestHeaders: TStringlist;
    FResource: string;
    FParams: array of string;
    FLastSessionID: string;
    FNextRequestIsAsynch: Boolean;
    FAsynchProc: TProc<IRESTResponse>;
    FAsynchProcErr: TProc<Exception>;
    FAsynchProcAlways: TProc;
    FMultiPartFormData: TIdMultiPartFormDataStream;
    FSynchronized: Boolean;
    FContentEncoding: string;
    function GetRawBody(): TStringStream;
    function GetMultiPartFormData(): TIdMultiPartFormDataStream;
    function GetSessionID(): string;
    function GetBasicAuth(): Boolean;
    function GetPassword(): string;
    function GetUserName(): string;
    function GetBodyParams(): TStringlist;
    function GetQueryStringParams(): TStringlist;
    procedure SetBasicAuth(const AValue: Boolean);
    procedure SetPassword(const AValue: string);
    procedure SetUserName(const AValue: string);
    procedure SetSessionID(const AValue: string);
    procedure SetProxyServer(const AValue: string);
    procedure SetProxyPort(const AValue: Integer);
    procedure SetProxyPassword(const AValue: string);
    procedure SetProxyUsername(const AValue: string);
  private
    FSerializer: IMVCSerializer;
    FURL: string;

    function GetURL: string;
  strict protected
    procedure HandleRequestCookies();
    procedure HandleCookies(aCookies: TIdCookies; aRESTResponse: IRESTResponse);

    function EncodeQueryStringParams(const AParams: TStrings; AIncludeQuestionMark: Boolean = True): string;
    function EncodeResourceParams(const AResourceParams: array of string): string;

    procedure StartAsynchRequest(const ACommand: TMVCHTTPMethodType; const AResource, ABody: string); overload;
    procedure StartAsynchRequest(const ACommand: TMVCHTTPMethodType; const AResource: string); overload;

    function HTTPCommandToString(const ACommand: TMVCHTTPMethodType): string;

    function SendHTTPCommand(const ACommand: TMVCHTTPMethodType; const AAccept, AContentMediaType, AResource: string;
      ABodyParams: TStrings): IRESTResponse;

    function SendHTTPCommandWithBody(const ACommand: TMVCHTTPMethodType;
      const AAccept, AContentMediaType, AContentCharset, AResource, ABody: string): IRESTResponse;

    procedure OnHTTPRedirect(Sender: TObject; var dest: string; var NumRedirect: Integer; var Handled: Boolean;
      var VMethod: TIdHTTPMethod);
  public
    constructor Create(const AHost: string; const APort: Word = 80; AIOHandler: TIdIOHandler = nil); virtual;
    destructor Destroy; override;

    function ReadTimeOut(const AValue: Integer): TRESTClient; overload;
    function ConnectionTimeOut(const AValue: Integer): TRESTClient; overload;
    function Authentication(const AUsername, APassword: string; const ABasicAuth: Boolean = True): TRESTClient;
    function ClearHeaders(): TRESTClient;
    function Header(const AField, AValue: string): TRESTClient;
    function Accept(const AValue: string): TRESTClient; overload;
    function AcceptCharSet(const AValue: string): TRESTClient;
    function ContentType(const AValue: string): TRESTClient; overload;
    function ContentCharSet(const AValue: string): TRESTClient;
    function ContentEncoding(const AValue: string): TRESTClient; overload;
    function Resource(const AValue: string): TRESTClient;
    function Params(const AValues: array of string): TRESTClient;
    function ClearAllParams(): TRESTClient;
    function SSL(const AEnabled: Boolean = True): TRESTClient;
    function Compression(const AEnabled: Boolean = True): TRESTClient;
    function ResetSession(): TRESTClient;

    function AddFile(const AFieldName, AFileName: string; const AContentType: string = ''): TRESTClient;

    function Asynch(AProc: TProc<IRESTResponse>; AProcErr: TProc<Exception> = nil; AProcAlways: TProc = nil;
      ASynchronized: Boolean = False): TRESTClient;

    function doGET(): IRESTResponse; overload;
    function doGET(const AResource: string; const AParams: array of string; const aQueryStringParams: TStrings = nil)
      : IRESTResponse; overload;
    function doGET(const AResource: string; const AParams: array of string;
      const aQueryStringParamNames: array of string;
      const aQueryStringParamValues: array of string): IRESTResponse; overload;
    function doPOST(const ABody: string): IRESTResponse; overload;
    function doPOST<TBodyType: class>(ABody: TBodyType; const AOwnsBody: Boolean = True): IRESTResponse; overload;
    function doPOST<TBodyType: class>(ABody: TObjectList<TBodyType>; const AOwnsBody: Boolean = True)
      : IRESTResponse; overload;
    function doPOST(const AResource: string; const AParams: array of string): IRESTResponse; overload;
    function doPOST(const AResource: string; const AParams: array of string; const ABody: string)
      : IRESTResponse; overload;

    function doPATCH(const ABody: string): IRESTResponse; overload;
    function doPATCH<TBodyType: class>(ABody: TBodyType; const AOwnsBody: Boolean = True): IRESTResponse; overload;
    function doPATCH<TBodyType: class>(ABody: TObjectList<TBodyType>; const AOwnsBody: Boolean = True)
      : IRESTResponse; overload;
    function doPATCH(const AResource: string; const AParams: array of string; const ABody: string)
      : IRESTResponse; overload;

    function doPUT(const ABody: string): IRESTResponse; overload;
    function doPUT<TBodyType: class>(ABody: TBodyType; const AOwnsBody: Boolean = True): IRESTResponse; overload;
    function doPUT<TBodyType: class>(ABody: TObjectList<TBodyType>; const AOwnsBody: Boolean = True)
      : IRESTResponse; overload;
    function doPUT(const AResource: string; const AParams: array of string): IRESTResponse; overload;
    function doPUT(const AResource: string; const AParams: array of string; const ABody: string)
      : IRESTResponse; overload;

    function doDELETE(): IRESTResponse; overload;
    function doDELETE(const AResource: string; const AParams: array of string): IRESTResponse; overload;

    function DataSetUpdate(const AResource: string; ADataSet: TDataSet; const AKeyValue: string): IRESTResponse;
    function DataSetInsert(const AResource: string; ADataSet: TDataSet): IRESTResponse;
    function DataSetDelete(const AResource: string; const AKeyValue: string): IRESTResponse;

    function DSUpdate(const AResource: string; ADataSet: TDataSet; const AKeyValue: string): IRESTResponse;
      deprecated 'use method DataSetUpdate';
    function DSInsert(const AResource: string; ADataSet: TDataSet): IRESTResponse;
      deprecated 'use method DataSetInsert';
    function DSDelete(const AResource: string; const AKeyValue: string): IRESTResponse;
      deprecated 'use method DataSetDelete';

    function Accept(): string; overload;
    function ContentType(): string; overload;
    function ContentEncoding(): string; overload;
    function ConnectionTimeOut(): Integer; overload;
    function ReadTimeOut(): Integer; overload;
    function HasSSL(): Boolean;
    function HasCompression(): Boolean;

    property RawBody: TStringStream read GetRawBody;
    property MultiPartFormData: TIdMultiPartFormDataStream read GetMultiPartFormData;
    property BodyParams: TStringlist read GetBodyParams;
    property SessionID: string read GetSessionID write SetSessionID;
    property Username: string read GetUserName write SetUserName;
    property Password: string read GetPassword write SetPassword;
    property UseBasicAuthentication: Boolean read GetBasicAuth write SetBasicAuth;
    property RequestHeaders: TStringlist read FRequestHeaders;
    property QueryStringParams: TStringlist read GetQueryStringParams;
    property ProxyServer: string write SetProxyServer;
    property ProxyPort: Integer write SetProxyPort;
    property ProxyUsername: string write SetProxyUsername;
    property ProxyPassword: string write SetProxyPassword;
    property URL: string read GetURL write FURL;
  end;

implementation

uses
  MVCFramework.Serializer.Defaults,
  MVCFramework.DataSet.Utils,
  System.ZLib

{$IFNDEF ANDROID OR IOS}
{$IFDEF BERLINORBETTER}
{$IFNDEF LINUX}
    ,
  System.AnsiStrings

{$ENDIF}
{$ENDIF}
{$ENDIF}
    ;

type
  TRESTResponse = class(TInterfacedObject, IRESTResponse)
  strict private
    FBody: TMemoryStream;
    FResponseCode: Word;
    FResponseText: string;
    FHeaders: TStringlist;
    // FBodyAsJSONValue: TJSONValue;
    FContentType: string;
    FContentTypeCharset: string;
    function GetHeader(const AValue: string): string;
  private
    FCookies: TIdCookies;
    FHasError: Boolean;
    FErrorObject: TMVCExceptionObj;
    FContentEncoding: string;
  protected
    function GetHasError: Boolean;
    procedure SetHasError(const aHasError: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetResponseCode(const AResponseCode: Word);
    procedure SetResponseText(const AResponseText: string);
    procedure SetHeaders(AHeaders: TStrings);

    function Body(): TStream;
    function BodyAsString(): string;

    procedure UpdateResponseCode(const AResponseCode: Word);
    procedure UpdateResponseText(const AResponseText: string);
    procedure UpdateHeaders(AHeaders: TStrings);

    function ResponseCode(): Word;
    function ResponseText(): string;

    function Headers: TStringlist;
    function HeaderValue(const AName: string): string;

    function ContentType(): string;
    function ContentTypeCharset(): string;
    function ContentEncoding(): string;

    function Error: TMVCExceptionObj;

    function GetCookies: TIdCookies;
    procedure SetCookies(aCookie: TIdCookies);
    property Cookies: TIdCookies read GetCookies write SetCookies;
  end;

  { TRESTResponse }

function TRESTResponse.Body: TStream;
begin
  Result := FBody;
end;

function TRESTResponse.BodyAsString: string;
var
  ss: TStringStream;
begin
  if (FContentTypeCharset = '') then
    FContentTypeCharset := 'utf-8';
  ss := TStringStream.Create('', TEncoding.GetEncoding(FContentTypeCharset.ToLower));
  try
    FBody.Position := 0;
    FBody.SaveToStream(ss);
    Result := ss.DataString;
  finally
    FreeAndNil(ss);
  end;
end;

function TRESTResponse.ContentTypeCharset: string;
begin
  Result := FContentTypeCharset;
end;

function TRESTResponse.ContentEncoding: string;
begin
  Result := FContentEncoding;
end;

function TRESTResponse.ContentType: string;
begin
  Result := FContentType;
end;

constructor TRESTResponse.Create;
begin
  FHeaders := TStringlist.Create;
  FCookies := TIdCookies.Create(nil);
  FBody := TStringStream.Create('', TEncoding.UTF8);
  // FBodyAsJSONValue := nil;
  FHasError := False;
end;

destructor TRESTResponse.Destroy;
begin
  // if Assigned(FBodyAsJSONValue) then
  // FreeAndNil(FBodyAsJSONValue);
  FreeAndNil(FHeaders);
  FreeAndNil(FBody);
  FreeAndNil(FCookies);
  FreeAndNil(FErrorObject);
  inherited;
end;

function TRESTResponse.Error: TMVCExceptionObj;
var
  lSerializer: IMVCSerializer;
begin
  if not FHasError then
    Exit(nil);
  if not Assigned(FErrorObject) then
  begin
    FErrorObject := TMVCExceptionObj.Create;
    { if content-type is json then we can use a more evoluted deserialization, otherwise
      we just copy some http information into FErrorObject }
    if Self.ContentType.StartsWith('application/json', True) then
    begin
      lSerializer := GetDefaultSerializer;
      lSerializer.DeserializeObject(Self.BodyAsString, FErrorObject);
    end
    else
    begin
      FErrorObject.Status := Self.ResponseText;
      FErrorObject.ExceptionMessage := Self.BodyAsString;
      FErrorObject.HTTPError := Self.ResponseCode;
    end;
  end;
  Result := FErrorObject;
end;

function TRESTResponse.GetCookies: TIdCookies;
begin
  Result := FCookies;
end;

function TRESTResponse.GetHasError: Boolean;
begin
  Result := FHasError;
end;

function TRESTResponse.GetHeader(const AValue: string): string;
var
  s: string;
begin
  Result := '';
  if Assigned(FHeaders) and (FHeaders.Count > 0) then
  begin
    for s in FHeaders do
      if s.StartsWith(AValue + ':', True) then
        Exit(s);
  end;
end;

function TRESTResponse.Headers: TStringlist;
begin
  Result := FHeaders;
end;

function TRESTResponse.HeaderValue(const AName: string): string;
var
  s: string;
  arr: TArray<string>;
begin
  Result := '';
  for s in Self.Headers do
  begin
    arr := s.Split([':'], 2);
    if SameText(arr[0].Trim, AName) then
    begin
      Result := arr[1].Trim;
      Break;
    end;
  end;
end;

function TRESTResponse.ResponseCode: Word;
begin
  Result := FResponseCode;
end;

function TRESTResponse.ResponseText: string;
begin
  Result := FResponseText;
end;

procedure TRESTResponse.SetCookies(aCookie: TIdCookies);
begin
  FCookies := aCookie;
end;

procedure TRESTResponse.SetHasError(const aHasError: Boolean);
begin
  FHasError := aHasError;
end;

procedure TRESTResponse.SetHeaders(AHeaders: TStrings);
begin
  UpdateHeaders(AHeaders);
end;

procedure TRESTResponse.SetResponseCode(const AResponseCode: Word);
begin
  UpdateResponseCode(AResponseCode);
end;

procedure TRESTResponse.SetResponseText(const AResponseText: string);
begin
  UpdateResponseText(AResponseText);
end;

procedure TRESTResponse.UpdateHeaders(AHeaders: TStrings);
var
  CT: TArray<string>;
  C: string;
begin
  FHeaders.Assign(AHeaders);
  FHeaders.NameValueSeparator := ':';

  C := GetHeader('content-type');
  if not C.IsEmpty then
  begin
    CT := C.Split([':'])[1].Split([';']);
    FContentType := Trim(CT[0]);
  end
  else
  begin
    SetLength(CT, 0);
    FContentType := TMVCConstants.DEFAULT_CONTENT_TYPE;
  end;

  FContentTypeCharset := 'UTF-8';
  if Length(CT) > 1 then
    if CT[1].Trim.StartsWith('charset', True) then
      FContentTypeCharset := CT[1].Trim.Split(['='])[1].Trim;

  FContentEncoding := '';
  C := GetHeader('content-encoding');
  if not C.IsEmpty then
  begin
    CT := C.Split([':']);
    if Length(CT) <> 2 then
      raise EMVCException.Create('Invalid Content-Encoding response header');
    FContentEncoding := Trim(CT[1]);
  end;
end;

procedure TRESTResponse.UpdateResponseCode(const AResponseCode: Word);
begin
  FResponseCode := AResponseCode;
end;

procedure TRESTResponse.UpdateResponseText(const AResponseText: string);
begin
  FResponseText := AResponseText;
end;

{ TJSONObjectResponseHelper }

// function TJSONObjectResponseHelper.AsObject<T>: T;
// var
// lSerializer: IMVCSerializer;
// begin
// lSerializer := GetDefaultSerializer;
// Result := T.Create;
// try
// lSerializer.DeserializeObject(Self.ToJSON, Result);
// except
// FreeAndNil(Result);
// raise;
// end;
// // Result := Mapper.JSONObjectToObject<T>(self);
// end;

{ TJSONArrayResponseHelper }

// function TJSONArrayResponseHelper.AsObjectList<T>: TObjectList<T>;
// begin
// raise Exception.Create('Not Implemented');
// // Result := Mapper.JSONArrayToObjectList<T>(self, False, True);
// end;

{ TRESTClient }

function TRESTClient.Accept(const AValue: string): TRESTClient;
begin
  FAccept := AValue;
  Result := Self;
end;

function TRESTClient.Accept: string;
begin
  Result := FAccept;
end;

function TRESTClient.AcceptCharSet(const AValue: string): TRESTClient;
begin
  if (FAccept = '') then
    raise ERESTClientException.Create('First set the Accept property!');

  if not AnsiContainsText(FAccept, 'charset') then
    Self.Accept(FAccept + ';charset=' + AValue);

  Result := Self;
end;

function TRESTClient.AddFile(const AFieldName, AFileName, AContentType: string): TRESTClient;
begin
  MultiPartFormData.AddFile(AFieldName, AFileName, AContentType);
  Result := Self;
end;

function TRESTClient.Asynch(AProc: TProc<IRESTResponse>; AProcErr: TProc<Exception>; AProcAlways: TProc;
  ASynchronized: Boolean): TRESTClient;
begin
  FNextRequestIsAsynch := True;
  FAsynchProc := AProc;
  FAsynchProcErr := AProcErr;
  FAsynchProcAlways := AProcAlways;
  FSynchronized := ASynchronized;
  Result := Self;
end;

function TRESTClient.Authentication(const AUsername, APassword: string; const ABasicAuth: Boolean): TRESTClient;
begin
  FHTTP.Request.Username := AUsername;
  FHTTP.Request.Password := APassword;
  FHTTP.Request.BasicAuthentication := ABasicAuth;
  Result := Self;
end;

function TRESTClient.ClearHeaders: TRESTClient;
begin
  FRequestHeaders.Clear;
  Result := Self;
end;

function TRESTClient.ClearAllParams: TRESTClient;
begin
  RawBody.Size := 0;
  RawBody.Position := 0;
  BodyParams.Clear;
  QueryStringParams.Clear;

  FNextRequestIsAsynch := False;
  FAsynchProc := nil;
  FAsynchProcErr := nil;
  FAsynchProcAlways := nil;
  FSynchronized := False;

  FHTTP.Request.Username := '';
  FHTTP.Request.Password := '';
  FHTTP.Request.BasicAuthentication := True;

  SetLength(FParams, 0);

  Result := Self;
end;

function TRESTClient.Compression(const AEnabled: Boolean): TRESTClient;
begin
  if AEnabled then
  begin
    if not Assigned(FHTTP.Compressor) then
      FHTTP.Compressor := TIdCompressorZLib.Create(FHTTP);
  end
  else
  begin
    if (FHTTP.Compressor <> nil) then
    begin

{$HINTS OFF}
      FHTTP.Compressor.Free;
      FHTTP.Compressor := nil;

{$HINTS ON}
    end;
  end;
  Result := Self;
end;

function TRESTClient.ConnectionTimeOut(const AValue: Integer): TRESTClient;
begin
  FHTTP.ConnectTimeout := AValue;
  Result := Self;
end;

function TRESTClient.ConnectionTimeOut: Integer;
begin
  Result := FHTTP.ConnectTimeout;
end;

function TRESTClient.ContentCharSet(const AValue: string): TRESTClient;
begin
  if (FContentType = '') then
    raise ERESTClientException.Create('First set the ContentType property!');

  if not AnsiContainsText(FContentType, 'charset') then
    Self.ContentType(FContentType + ';charset=' + AValue);

  Result := Self;
end;

function TRESTClient.ContentEncoding: string;
begin
  Result := FContentEncoding;
end;

function TRESTClient.ContentEncoding(const AValue: string): TRESTClient;
begin
  FContentEncoding := AValue;
  Result := Self;
end;

function TRESTClient.ContentType: string;
begin
  Result := FContentType;
end;

function TRESTClient.ContentType(const AValue: string): TRESTClient;
begin
  FContentType := AValue;
  Result := Self;
end;

constructor TRESTClient.Create(const AHost: string; const APort: Word; AIOHandler: TIdIOHandler);
var
  Pieces: TArray<string>;
begin
  inherited Create;
  FHost := AHost;
  FPort := APort;
  FPrimaryThread := TThread.CurrentThread;
  FBodyParams := nil;
  FQueryStringParams := nil;
  FRawBody := nil;
  FAccept := 'application/json';
  FContentType := BuildContentType(TMVCMediaType.APPLICATION_JSON, TMVCCharset.UTF_8);
  FResource := '';
  FContentEncoding := '';
  FRequestHeaders := TStringlist.Create;
  FLastSessionID := '';
  FNextRequestIsAsynch := False;
  FAsynchProc := nil;
  FAsynchProcErr := nil;
  FAsynchProcAlways := nil;
  FMultiPartFormData := nil;
  FSynchronized := False;
  SetLength(FParams, 0);

  if FHost.Contains('://') then
  begin
    Pieces := FHost.Split(['://'], 2, TStringSplitOptions.ExcludeEmpty);
    FProtocol := Pieces[0];
    FHost := Pieces[1];
  end
  else
    FProtocol := 'http';

  FHTTP := TIdHTTP.Create(nil);
  FHTTP.HandleRedirects := False; // DT 2016/09/16
  FHTTP.OnRedirect := OnHTTPRedirect; // DT 2016/09/16
  FHTTP.ReadTimeOut := 20000;
  FHTTP.Request.UserAgent := 'Mozilla/3.0 (compatible; IndyLibrary)'; // Resolve 403 Forbidden error in REST API SSL

  if (AIOHandler <> nil) then
    FHTTP.IOHandler := AIOHandler
  else
    SSL(False);

  Compression(False);

  FHTTP.HandleRedirects := True;
  FHTTP.Request.CustomHeaders.FoldLines := False;
  FHTTP.Request.BasicAuthentication := False; // DT 2018/07/24

  // https://www.indyproject.org/2016/01/10/new-tidhttp-flags-and-onchunkreceived-event/
  FHTTP.HTTPOptions := FHTTP.HTTPOptions + [hoWantProtocolErrorContent, hoNoProtocolErrorException]; //DT 2022/05/24
  FSerializer := GetDefaultSerializer;
end;

function TRESTClient.DataSetDelete(const AResource, AKeyValue: string): IRESTResponse;
begin
  Result := doDELETE(AResource, [AKeyValue]);
end;

function TRESTClient.DataSetInsert(const AResource: string; ADataSet: TDataSet): IRESTResponse;
begin
  Result := doPOST(AResource, [], ADataSet.AsJSONObject);
end;

function TRESTClient.DataSetUpdate(const AResource: string; ADataSet: TDataSet; const AKeyValue: string): IRESTResponse;
begin
  Result := doPUT(AResource, [AKeyValue], ADataSet.AsJSONObject);
end;

destructor TRESTClient.Destroy;
begin
  if Assigned(FBodyParams) then
    FreeAndNil(FBodyParams);

  if Assigned(FQueryStringParams) then
    FreeAndNil(FQueryStringParams);

  if Assigned(FRawBody) then
    FreeAndNil(FRawBody);

  if Assigned(FMultiPartFormData) then
    FreeAndNil(FMultiPartFormData);

  FreeAndNil(FRequestHeaders);
  FreeAndNil(FHTTP);
  inherited;
end;

function TRESTClient.doDELETE(const AResource: string; const AParams: array of string): IRESTResponse;
var
  URL: string;
begin
  URL := Self.URL + AResource + EncodeResourceParams(AParams) +
    EncodeQueryStringParams(QueryStringParams);

  if FNextRequestIsAsynch then
  begin
    Result := nil;
    StartAsynchRequest(httpDELETE, URL);
  end
  else
  begin
    Result := SendHTTPCommand(httpDELETE, FAccept, FContentType, URL, nil);
    ClearAllParams;
  end;
end;

function TRESTClient.doGET(const AResource: string; const AParams,
  aQueryStringParamNames,
  aQueryStringParamValues: array of string): IRESTResponse;
var
  lParams: TStringlist;
  lName: string;
  I: Integer;
begin
  Assert(Length(aQueryStringParamNames) = Length(aQueryStringParamValues));
  lParams := TStringlist.Create;
  try
    I := 0;
    for lName in aQueryStringParamNames do
    begin
      lParams.Values[lName] := aQueryStringParamValues[I];
      inc(I);
    end;
    Result := doGET(AResource, AParams, lParams);
  finally
    lParams.Free;
  end;
end;

function TRESTClient.doDELETE: IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  Result := doDELETE(FResource, FParams);
end;

function TRESTClient.doGET: IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  Result := doGET(FResource, FParams);
end;

function TRESTClient.doGET(const AResource: string; const AParams: array of string; const aQueryStringParams: TStrings)
  : IRESTResponse;
var
  URL: string;
begin
  URL := Self.URL + AResource + EncodeResourceParams(AParams);
  if aQueryStringParams = nil then
    URL := URL + EncodeQueryStringParams(FQueryStringParams)
  else
    URL := URL + EncodeQueryStringParams(aQueryStringParams);

  if FNextRequestIsAsynch then
  begin
    Result := nil;
    StartAsynchRequest(httpGET, URL);
  end
  else
  begin
    Result := SendHTTPCommand(httpGET, FAccept, FContentType, URL, nil);
    ClearAllParams;
  end;
end;

function TRESTClient.doPOST(const AResource: string; const AParams: array of string): IRESTResponse;
var
  s: string;
begin
  try
    Result := SendHTTPCommand(httpPOST, FAccept, FContentType, Self.URL +
      AResource + EncodeResourceParams(AParams) + EncodeQueryStringParams(FQueryStringParams), FBodyParams);
  except
    on E: EIdHTTPProtocolException do
      s := E.Message;
  end;
  ClearAllParams;
end;

function TRESTClient.doPATCH(const AResource: string; const AParams: array of string; const ABody: string)
  : IRESTResponse;
var
  URL: string;
begin
  URL := Self.URL + AResource + EncodeResourceParams(AParams) +
    EncodeQueryStringParams(QueryStringParams);

  if FNextRequestIsAsynch then
  begin
    Result := nil;
    StartAsynchRequest(httpPOST, URL, ABody);
  end
  else
  begin
    Result := SendHTTPCommandWithBody(httpPATCH, FAccept, FContentType, FContentEncoding, URL, ABody);
    ClearAllParams;
  end;
end;

function TRESTClient.doPATCH(const ABody: string): IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if (ABody = '') then
    raise ERESTClientException.Create('You must enter the Body!');

  Result := doPATCH(FResource, FParams, ABody);
end;

function TRESTClient.doPATCH<TBodyType>(ABody: TBodyType; const AOwnsBody: Boolean): IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if not Assigned(ABody) then
    raise ERESTClientException.Create('You must enter the Body!');

  Result := doPATCH(FResource, FParams, FSerializer.SerializeObject(ABody));

  if AOwnsBody then
    TObject(ABody).Free;
end;

function TRESTClient.doPATCH<TBodyType>(ABody: TObjectList<TBodyType>; const AOwnsBody: Boolean): IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if not Assigned(ABody) then
    raise ERESTClientException.Create('You must enter the Body!');

  ABody.OwnsObjects := AOwnsBody;
  Result := doPATCH(FResource, FParams, FSerializer.SerializeCollection(ABody));
end;

function TRESTClient.doPOST(const AResource: string; const AParams: array of string; const ABody: string)
  : IRESTResponse;
var
  URL { , lContentTypeWithCharset } : string;
begin
  URL := Self.URL + AResource + EncodeResourceParams(AParams) +
    EncodeQueryStringParams(QueryStringParams);

  if FNextRequestIsAsynch then
  begin
    Result := nil;
    StartAsynchRequest(httpPOST, URL, ABody);
  end
  else
  begin
    // lContentTypeWithCharset := FContentType;
    // if FContentEncoding = '' then
    // FContentEncoding := 'UTF-8';
    // lContentTypeWithCharset := FContentType + ';charset=' + FContentEncoding;

    Result := SendHTTPCommandWithBody(httpPOST, FAccept, FContentType, FContentEncoding, URL, ABody);
    ClearAllParams;
  end;
end;

function TRESTClient.doPOST(const ABody: string): IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if (ABody = '') then
    raise ERESTClientException.Create('You must enter the Body!');

  Result := doPOST(FResource, FParams, ABody);
end;

function TRESTClient.doPOST<TBodyType>(ABody: TBodyType; const AOwnsBody: Boolean): IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if not Assigned(ABody) then
    raise ERESTClientException.Create('You must enter the Body!');

  Result := doPOST(FResource, FParams, FSerializer.SerializeObject(ABody));

  if AOwnsBody then
    TObject(ABody).Free;
end;

function TRESTClient.doPOST<TBodyType>(ABody: TObjectList<TBodyType>; const AOwnsBody: Boolean): IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if not Assigned(ABody) then
    raise ERESTClientException.Create('You must enter the Body!');

  ABody.OwnsObjects := AOwnsBody;

  Result := doPOST(FResource, FParams, FSerializer.SerializeCollection(ABody));
end;

function TRESTClient.doPUT(const AResource: string; const AParams: array of string): IRESTResponse;
begin
  Result := SendHTTPCommand(httpPUT, FAccept, FContentType, Self.URL +
    AResource + EncodeResourceParams(AParams) + EncodeQueryStringParams(QueryStringParams), FBodyParams);
  ClearAllParams;
end;

function TRESTClient.doPUT(const AResource: string; const AParams: array of string; const ABody: string): IRESTResponse;
var
  URL: string;
begin
  URL := Self.URL + AResource + EncodeResourceParams(AParams) +
    EncodeQueryStringParams(QueryStringParams);

  if FNextRequestIsAsynch then
  begin
    Result := nil;
    StartAsynchRequest(httpPUT, URL, ABody);
  end
  else
  begin
    Result := SendHTTPCommandWithBody(httpPUT, FAccept, FContentType, FContentEncoding, URL, ABody);
    ClearAllParams;
  end;
end;

function TRESTClient.doPUT(const ABody: string): IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if (ABody = '') then
    raise ERESTClientException.Create('You must enter the Body!');

  Result := doPUT(FResource, FParams, ABody);
end;

function TRESTClient.doPUT<TBodyType>(ABody: TBodyType; const AOwnsBody: Boolean): IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if not Assigned(ABody) then
    raise ERESTClientException.Create('You must enter the Body!');

  Result := doPUT(FResource, FParams, FSerializer.SerializeObject(ABody));

  if AOwnsBody then
    TObject(ABody).Free;
end;

function TRESTClient.doPUT<TBodyType>(ABody: TObjectList<TBodyType>; const AOwnsBody: Boolean): IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if not Assigned(ABody) then
    raise ERESTClientException.Create('You must enter the Body!');

  ABody.OwnsObjects := AOwnsBody;

  Result := doPUT(FResource, FParams, FSerializer.SerializeCollection(ABody));
end;

function TRESTClient.DSDelete(const AResource, AKeyValue: string): IRESTResponse;
begin
  Result := DataSetDelete(AResource, AKeyValue);
end;

function TRESTClient.DSInsert(const AResource: string; ADataSet: TDataSet): IRESTResponse;
begin
  Result := DataSetInsert(AResource, ADataSet);
end;

function TRESTClient.DSUpdate(const AResource: string; ADataSet: TDataSet; const AKeyValue: string): IRESTResponse;
begin
  Result := DataSetUpdate(AResource, ADataSet, AKeyValue);
end;

function TRESTClient.EncodeQueryStringParams(const AParams: TStrings; AIncludeQuestionMark: Boolean): string;
var
  I: Integer;
begin
  Result := '';

  if not Assigned(AParams) or (AParams.Count = 0) then
    Exit;

  if AIncludeQuestionMark then
    Result := '?';

  for I := 0 to AParams.Count - 1 do
  begin
    if I > 0 then
      Result := Result + '&';
    Result := Result + AParams.Names[I] + '=' + TIdURI.ParamsEncode(AParams.ValueFromIndex[I]);
  end;
end;

function TRESTClient.EncodeResourceParams(const AResourceParams: array of string): string;
var
  I: Integer;
begin
  Result := '';
  for I := low(AResourceParams) to high(AResourceParams) do
    Result := Result + '/' + TIdURI.ParamsEncode(AResourceParams[I]);
end;

function TRESTClient.GetBasicAuth: Boolean;
begin
  Result := FHTTP.Request.BasicAuthentication;
end;

function TRESTClient.GetBodyParams: TStringlist;
begin
  if not Assigned(FBodyParams) then
    FBodyParams := TStringlist.Create;
  Result := FBodyParams;
end;

function TRESTClient.GetMultiPartFormData: TIdMultiPartFormDataStream;
begin
  if not Assigned(FMultiPartFormData) then
    FMultiPartFormData := TIdMultiPartFormDataStream.Create;
  Result := FMultiPartFormData;
end;

function TRESTClient.GetPassword: string;
begin
  Result := FHTTP.Request.Password;
end;

function TRESTClient.GetQueryStringParams: TStringlist;
begin
  if not Assigned(FQueryStringParams) then
    FQueryStringParams := TStringlist.Create;
  Result := FQueryStringParams;
end;

function TRESTClient.GetRawBody: TStringStream;
begin
  if not Assigned(FRawBody) then
    FRawBody := TStringStream.Create('');
  Result := FRawBody;
end;

function TRESTClient.GetSessionID: string;
begin
  Result := FLastSessionID;
end;

function TRESTClient.GetURL: string;
begin
  if FURL = EmptyStr then
    Result := FProtocol + '://' + FHost + ':' + IntToStr(FPort)
  else
    Result := FURL;
end;

function TRESTClient.GetUserName: string;
begin
  Result := FHTTP.Request.Username;
end;

procedure TRESTClient.HandleCookies(aCookies: TIdCookies; aRESTResponse: IRESTResponse);
var
  s: string;
  arr: TArray<string>;
begin
  aCookies.LockCookieList(caReadWrite);
  try
    aRESTResponse.Cookies.Clear;
    aRESTResponse.Cookies.AddCookies(aCookies);
  finally
    aCookies.UnlockCookieList(caReadWrite);
  end;

  for s in FHTTP.Response.RawHeaders do
  begin
    if s.StartsWith('Set-Cookie', True) then
    begin
      arr := s.Split([':'], 2);
      if arr[1].Trim.StartsWith('dtsessionid') then
      begin
        arr := arr[1].Split(['='], 2);
        FLastSessionID := TIdURI.URLDecode(arr[1].Split([';'])[0]);
        if FLastSessionID.Contains('invalid') then
          FLastSessionID := '';
      end;
      Break;
    end;
  end;
end;

procedure TRESTClient.HandleRequestCookies;
var
  I: Integer;
begin
  if Assigned(FHTTP.CookieManager) then
    FHTTP.CookieManager.CookieCollection.Clear;

  if not FLastSessionID.Trim.IsEmpty then
    FHTTP.Request.CustomHeaders.AddValue('Cookie', 'dtsessionid=' + FLastSessionID);

  for I := 0 to FRequestHeaders.Count - 1 do
  begin
    FHTTP.Request.CustomHeaders.AddValue(FRequestHeaders.Names[I], FRequestHeaders.ValueFromIndex[I]);
  end;
end;

function TRESTClient.HasCompression: Boolean;
begin
  Result := (FHTTP.Compressor <> nil);
end;

function TRESTClient.HasSSL: Boolean;
begin
  Result := (FHTTP.IOHandler <> nil);
end;

function TRESTClient.Header(const AField, AValue: string): TRESTClient;
begin
  FRequestHeaders.Add(AField + '=' + AValue);
  Result := Self;
end;

function TRESTClient.HTTPCommandToString(const ACommand: TMVCHTTPMethodType): string;
begin
  case ACommand of
    httpGET:
      Result := 'GET';
    httpPOST:
      Result := 'POST';
    httpPUT:
      Result := 'PUT';
    httpDELETE:
      Result := 'DELETE';
  else
    raise ERESTClientException.Create('Unknown HTTPCommand in TRESTClient.HTTPCommandToString');
  end;
end;

procedure TRESTClient.OnHTTPRedirect(Sender: TObject; var dest: string; var NumRedirect: Integer; var Handled: Boolean;
  var VMethod: TIdHTTPMethod);
begin
  Handled := False;
end;

function TRESTClient.Params(const AValues: array of string): TRESTClient;
var
  I: Integer;
begin
  SetLength(FParams, Length(AValues));
  for I := low(AValues) to high(AValues) do
    FParams[I] := AValues[I];
  Result := Self;
end;

function TRESTClient.ReadTimeOut(const AValue: Integer): TRESTClient;
begin
  FHTTP.ReadTimeOut := AValue;
  Result := Self;
end;

function TRESTClient.ReadTimeOut: Integer;
begin
  Result := FHTTP.ReadTimeOut;
end;

function TRESTClient.ResetSession: TRESTClient;
begin
  SessionID := '';
  if Assigned(FHTTP.CookieManager) then
    FHTTP.CookieManager.CookieCollection.Clear;
  FHTTP.Request.RawHeaders.Clear;
  Result := Self;
end;

function TRESTClient.Resource(const AValue: string): TRESTClient;
begin
  FResource := AValue;
  Result := Self;
end;

function TRESTClient.SendHTTPCommand(const ACommand: TMVCHTTPMethodType;
  const AAccept, AContentMediaType, AResource: string; ABodyParams: TStrings): IRESTResponse;
var
  lTmp: TMemoryStream;
  lDecomp: TZDecompressionStream;
  lCompressionType: TMVCCompressionType;
begin
  FHTTP.Request.BasicAuthentication := not Username.IsEmpty; // DT 2019/08/23

  FContentEncoding := '';
  Result := TRESTResponse.Create;

  FHTTP.Request.RawHeaders.Clear;
  FHTTP.Request.CustomHeaders.Clear;
  FHTTP.Request.Accept := AAccept;
  if ACommand in MVC_HTTP_METHODS_WITH_CONTENT then
    FHTTP.Request.ContentType := AContentMediaType;

  HandleRequestCookies;
  try
    case ACommand of
      httpGET:
        begin
          Result.Body.Position := 0;
          FHTTP.Get(AResource, Result.Body);
        end;

      httpPOST:
        begin
          if (MultiPartFormData.Size = 0) then
          begin
            Result.Body.Position := 0;
            FHTTP.Post(AResource, RawBody, Result.Body);
          end
          else
          begin
            FHTTP.Post(AResource, MultiPartFormData, Result.Body);
            MultiPartFormData.Clear;
          end;
        end;

      httpPUT:
        begin
          if (MultiPartFormData.Size <> 0) then { TODO -oDaniele -cGeneral : Rework please!!! }
            raise ERESTClientException.Create('Only POST can Send Files');

          Result.Body.Position := 0;

          if Assigned(ABodyParams) and (ABodyParams.Count > 0) then
          begin
            RawBody.Size := 0;
            RawBody.WriteString(EncodeQueryStringParams(ABodyParams, False));
          end;

          FHTTP.Put(AResource, RawBody, Result.Body);
        end;

      httpDELETE:
        begin
          Result.Body.Position := 0;
          FHTTP.Delete(AResource);
          RawBody.Size := 0;
        end;
    end;
  except
    on E: EIdHTTPProtocolException do
    begin
      Result.HasError := True;
      Result.Body.WriteUTF8(E.ErrorMessage);
    end
    else
      raise;
  end;

  HandleCookies(FHTTP.CookieManager.CookieCollection, Result);

  Result.UpdateResponseCode(FHTTP.Response.ResponseCode);
  Result.UpdateResponseText(FHTTP.Response.ResponseText);
  Result.UpdateHeaders(FHTTP.Response.RawHeaders);

  if Result.ContentEncoding.IsEmpty or (Result.ContentEncoding = 'identity') then
    Exit;

  if Result.ContentEncoding = 'deflate' then
  begin
    lCompressionType := TMVCCompressionType.ctDeflate;
  end
  else if Result.ContentEncoding = 'gzip' then
  begin
    lCompressionType := TMVCCompressionType.ctGZIP;
  end
  else
    raise EMVCException.CreateFmt('Content-Encoding not supported [%s]', [Result.ContentEncoding]);

  lTmp := TMemoryStream.Create;
  try
    Result.Body.Position := 0;
{$IF Defined(BerlinOrBetter)}
    lDecomp := TZDecompressionStream.Create(Result.Body, MVC_COMPRESSION_ZLIB_WINDOW_BITS[lCompressionType], False);
{$ELSE}
    lDecomp := TZDecompressionStream.Create(Result.Body, MVC_COMPRESSION_ZLIB_WINDOW_BITS[lCompressionType]);
{$ENDIF}
    try
      lTmp.CopyFrom(lDecomp, 0);
      Result.Body.Size := 0;
      Result.Body.CopyFrom(lTmp, 0);
    finally
      lDecomp.Free;
    end;
  finally
    lTmp.Free;
  end;
end;

function TRESTClient.SendHTTPCommandWithBody(const ACommand: TMVCHTTPMethodType;
  const AAccept, AContentMediaType, AContentCharset, AResource, ABody: string): IRESTResponse;
var
  lContentCharset: string;
  lEncoding: TEncoding;
  lTmpStrStream: TStringStream;
begin
  Result := TRESTResponse.Create;

  FHTTP.Request.RawHeaders.Clear;
  FHTTP.Request.CustomHeaders.Clear;
  FHTTP.Request.Accept := AAccept;

  lContentCharset := 'UTF-8';
  if AContentCharset <> '' then
    lContentCharset := AContentCharset;

  FHTTP.Request.ContentType := BuildContentType(AContentMediaType, lContentCharset);

  HandleRequestCookies;
  try
    case ACommand of
      httpGET:
        begin
          FHTTP.Get(AResource, Result.Body);
        end;

      httpPOST, httpPUT:
        begin
          if (MultiPartFormData.Size <> 0) then
            raise ERESTClientException.Create('This method cannot send files');

          RawBody.Position := 0;
          RawBody.Size := 0;

          lEncoding := TEncoding.GetEncoding(lContentCharset);
          try
            lTmpStrStream := TStringStream.Create(ABody, lEncoding, False);
            try
              RawBody.LoadFromStream(lTmpStrStream);
            finally
              lTmpStrStream.Free;
            end
          finally
            lEncoding.Free;
          end;

          if ACommand = httpPOST then
            FHTTP.Post(AResource, RawBody, Result.Body)
          else
            FHTTP.Put(AResource, RawBody, Result.Body);
        end;

      httpPATCH:
        begin
          raise ERESTClientException.Create
            ('Sorry, PATCH is not supported by the RESTClient because is not supportd by the TidHTTP');
        end;

      httpDELETE:
        begin
          FHTTP.Delete(AResource);
          RawBody.Size := 0;
        end;
    end;
  except
    on E: EIdHTTPProtocolException do
    begin
      Result.HasError := True;
      Result.Body.WriteUTF8(E.ErrorMessage);
    end
    else
      raise;
  end;

  HandleCookies(FHTTP.CookieManager.CookieCollection, Result);

  Result.UpdateResponseCode(FHTTP.Response.ResponseCode);
  Result.UpdateResponseText(FHTTP.Response.ResponseText);
  Result.UpdateHeaders(FHTTP.Response.RawHeaders);
end;

procedure TRESTClient.SetBasicAuth(const AValue: Boolean);
begin
  FHTTP.Request.BasicAuthentication := AValue;
end;

procedure TRESTClient.SetPassword(const AValue: string);
begin
  FHTTP.Request.Password := AValue;
end;

procedure TRESTClient.SetProxyPort(const AValue: Integer);
begin
  FHTTP.ProxyParams.ProxyPort := AValue;
end;

procedure TRESTClient.SetProxyServer(const AValue: string);
begin
  FHTTP.ProxyParams.ProxyServer := AValue;
end;

procedure TRESTClient.SetProxyUsername(const AValue: string);
begin
  FHTTP.ProxyParams.ProxyUsername := AValue;
end;

procedure TRESTClient.SetProxyPassword(const AValue: string);
begin
  FHTTP.ProxyParams.ProxyPassword := AValue;
end;

procedure TRESTClient.SetSessionID(const AValue: string);
begin
  FLastSessionID := AValue;
  if Assigned(FHTTP.CookieManager) then
    FHTTP.CookieManager.CookieCollection.Clear;
end;

procedure TRESTClient.SetUserName(const AValue: string);
begin
  FHTTP.Request.Username := AValue;
end;

function TRESTClient.SSL(const AEnabled: Boolean): TRESTClient;
begin
  if AEnabled then
  begin
    if not Assigned(FHTTP.IOHandler) then
    begin
      FHTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
      TIdSSLIOHandlerSocketOpenSSL(FHTTP.IOHandler).SSLOptions.SSLVersions :=
        [sslvSSLv2, sslvSSLv23, sslvSSLv3, sslvTLSv1,sslvTLSv1_1,sslvTLSv1_2];
    end;
  end
  else
  begin
    if (FHTTP.IOHandler <> nil) then
    begin

{$HINTS OFF}
      FHTTP.IOHandler.Free;
      FHTTP.IOHandler := nil;

{$HINTS ON}
    end;
  end;
  Result := Self;
end;

procedure TRESTClient.StartAsynchRequest(const ACommand: TMVCHTTPMethodType; const AResource: string);
begin
  StartAsynchRequest(ACommand, AResource, '');
end;

procedure TRESTClient.StartAsynchRequest(const ACommand: TMVCHTTPMethodType; const AResource, ABody: string);
var
  th: TThread;
begin
  th := TThread.CreateAnonymousThread(
    procedure
    var
      R: IRESTResponse;
    begin
      try
        R := SendHTTPCommandWithBody(ACommand, FAccept, FContentType, FContentEncoding, AResource, ABody);
        TMonitor.Enter(TObject(R));
        try
          if FSynchronized then
            TThread.Synchronize(nil,
              procedure
              begin
                FAsynchProc(R);
              end)
          else
            FAsynchProc(R);
        finally
          TMonitor.Exit(TObject(R));
        end;
      except
        on E: Exception do
        begin
          if not Assigned(FAsynchProcErr) then
            raise;
          if FSynchronized then
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                FAsynchProcErr(E);
              end)
          end
          else
          begin
            FAsynchProcErr(E);
          end;
        end;
      end; { except }
      if Assigned(FAsynchProcAlways) then
      begin
        if FSynchronized then
          TThread.Synchronize(nil,
            procedure
            begin
              FAsynchProcAlways();
            end)
        else
          FAsynchProcAlways();
      end;
      ClearAllParams;
    end);
  th.Start;
end;

end.
