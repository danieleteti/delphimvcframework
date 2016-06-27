// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2016 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.RESTClient;

interface

uses
  System.Classes,
  IdHTTP,
  IdURI,
  ObjectsMappers,

{$IF CompilerVersion < 27}
  Data.DBXJSON,

{$ELSE}
  System.JSON,

{$ENDIF}
  IdMultipartFormData,
  System.SysUtils,
  Data.DB,
  IdIOHandler,
  IdCompressorZLib,
  IdSSLOpenSSL,
  System.Generics.Collections,
  System.StrUtils, Web.HTTPApp, IdCookie;

type
  ERESTClientException = class(Exception);

  TArrayOfString = array of string;
  THTTPCommand = (httpGET, httpPOST, httpPUT, httpDELETE, httpPATCH, httpTRACE);

  [MapperJSONNaming(JSONNameLowerCase)]
  TMVCExceptionObj = class(TObject)
  private
    FStatus: string;
    Fclassname: string;
    FMessage: string;
    FHttp_error: Integer;
  public
    property Status: string read FStatus write FStatus;
    [MapperJSONSer('classname')]
    property ExceptionClassname: string read Fclassname write Fclassname;
    [MapperJSONSer('message')]
    property ExceptionMessage: string read FMessage write FMessage;
    [MapperJSONSer('http_error')]
    property HTTPError: Integer read FHttp_error write FHttp_error;
  end;

  IRESTResponse = interface
    ['{E96178DE-79D4-4EF6-88F6-1A677207265A}']
    function GetContentType: string; deprecated 'use method ContentType';
    function GetContentEncoding: string;
      deprecated 'use method ContentEncoding';
    function GetHeaderValue(const AName: string): string;
      deprecated 'use method HeaderValue';

    procedure SetResponseCode(const AResponseCode: Word);
      deprecated 'use method UpdateResponseCode';
    procedure SetResponseText(const AResponseText: string);
      deprecated 'use method UpdateResponseText';
    procedure SetHeaders(AHeaders: TStrings);
      deprecated 'use method UpdateHeaders';

    function Body: TStream;
    function BodyAsString: string;
    function BodyAsJSONValue: TJSONValue;
    function BodyAsJSONObject: TJSONObject;
    function BodyAsJSONArray: TJSONArray;

    procedure UpdateResponseCode(const AResponseCode: Word);
    procedure UpdateResponseText(const AResponseText: string);
    procedure UpdateHeaders(AHeaders: TStrings);

    function ResponseCode: Word;
    function ResponseText: string;

    function Headers: TStringlist;
    function HeaderValue(const AName: string): string;

    function ContentType: string;
    function ContentEncoding: string;

    function GetCookies: TIdCookies;
    procedure SetCookies(aCookie: TIdCookies);

    function GetHasError: Boolean;
    procedure SetHasError(const aHasError: Boolean);

    function Error: TMVCExceptionObj;

    property Cookies: TIdCookies read GetCookies write SetCookies;
    property HasError: Boolean read GetHasError write SetHasError;
  end;

  TJSONObjectResponseHelper = class helper for TJSONObject
  public
    function AsObject<T: class, constructor>(): T;
  end;

  TJSONArrayResponseHelper = class helper for TJSONArray
  public
    function AsObjectList<T: class, constructor>(): TObjectList<T>;
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
  strict protected
    procedure HandleRequestCookies();
    procedure HandleCookies(aCookies: TIdCookies;
      aRESTResponse: IRESTResponse);

    function EncodeQueryStringParams(const AParams: TStrings;
      AIncludeQuestionMark: Boolean = True): string;
    function EncodeResourceParams(const AResourceParams
      : array of string): string;

    procedure StartAsynchRequest(const ACommand: THTTPCommand;
      const AResource, ABody: string); overload;
    procedure StartAsynchRequest(const ACommand: THTTPCommand;
      const AResource: string); overload;

    function HTTPCommandToString(const ACommand: THTTPCommand): string;

    function SendHTTPCommand(const ACommand: THTTPCommand;
      const AAccept, AContentType, AResource: string; ABodyParams: TStrings)
      : IRESTResponse;

    function SendHTTPCommandWithBody(const ACommand: THTTPCommand;
      const AAccept, AContentType, AResource, ABody: string): IRESTResponse;
  public
    constructor Create(const AHost: string; const APort: Word = 80;
      AIOHandler: TIdIOHandler = nil); virtual;
    destructor Destroy; override;

    function ReadTimeOut(const AValue: Integer): TRESTClient; overload;
    function ConnectionTimeOut(const AValue: Integer): TRESTClient; overload;
    function Authentication(const AUsername, APassword: string;
      const ABasicAuth: Boolean = True): TRESTClient;
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

    function AddFile(const AFieldName, AFileName: string;
      const AContentType: string = ''): TRESTClient;

    function Asynch(AProc: TProc<IRESTResponse>;
      AProcErr: TProc<Exception> = nil; AProcAlways: TProc = nil;
      ASynchronized: Boolean = False): TRESTClient;

    function doGET(): IRESTResponse; overload;
    function doGET(const AResource: string; const AParams: array of string;
      const aQueryStringParams: TStrings = nil)
      : IRESTResponse; overload;

    function doPOST(const ABody: string): IRESTResponse; overload;
    function doPOST(ABody: TJSONValue; const AOwnsBody: Boolean = True)
      : IRESTResponse; overload;
    function doPOST<TBodyType: class>(ABody: TBodyType;
      const AOwnsBody: Boolean = True): IRESTResponse; overload;
    function doPOST<TBodyType: class>(ABody: TObjectList<TBodyType>;
      const AOwnsBody: Boolean = True): IRESTResponse; overload;
    function doPOST(const AResource: string; const AParams: array of string)
      : IRESTResponse; overload;
    function doPOST(const AResource: string; const AParams: array of string;
      ABody: TJSONValue; const AOwnsBody: Boolean = True)
      : IRESTResponse; overload;
    function doPOST(const AResource: string; const AParams: array of string;
      const ABody: string): IRESTResponse; overload;

    function doPATCH(const ABody: string): IRESTResponse; overload;
    function doPATCH(ABody: TJSONValue; const AOwnsBody: Boolean = True)
      : IRESTResponse; overload;
    function doPATCH<TBodyType: class>(ABody: TBodyType;
      const AOwnsBody: Boolean = True): IRESTResponse; overload;
    function doPATCH<TBodyType: class>(ABody: TObjectList<TBodyType>;
      const AOwnsBody: Boolean = True): IRESTResponse; overload;
    function doPATCH(const AResource: string; const AParams: array of string;
      ABody: TJSONValue; const AOwnsBody: Boolean = True)
      : IRESTResponse; overload;
    function doPATCH(const AResource: string; const AParams: array of string;
      const ABody: string): IRESTResponse; overload;

    function doPUT(const ABody: string): IRESTResponse; overload;
    function doPUT(ABody: TJSONValue; const AOwnsBody: Boolean = True)
      : IRESTResponse; overload;
    function doPUT<TBodyType: class>(ABody: TBodyType;
      const AOwnsBody: Boolean = True): IRESTResponse; overload;
    function doPUT<TBodyType: class>(ABody: TObjectList<TBodyType>;
      const AOwnsBody: Boolean = True): IRESTResponse; overload;
    function doPUT(const AResource: string; const AParams: array of string)
      : IRESTResponse; overload;
    function doPUT(const AResource: string; const AParams: array of string;
      ABody: TJSONValue; const AOwnsBody: Boolean = True)
      : IRESTResponse; overload;
    function doPUT(const AResource: string; const AParams: array of string;
      const ABody: string): IRESTResponse; overload;

    function doDELETE(): IRESTResponse; overload;
    function doDELETE(const AResource: string; const AParams: array of string)
      : IRESTResponse; overload;

    function DataSetUpdate(const AResource: string; ADataSet: TDataSet;
      const AKeyValue: string): IRESTResponse;
    function DataSetInsert(const AResource: string; ADataSet: TDataSet)
      : IRESTResponse;
    function DataSetDelete(const AResource: string; const AKeyValue: string)
      : IRESTResponse;

    function DSUpdate(const AResource: string; ADataSet: TDataSet;
      const AKeyValue: string): IRESTResponse;
      deprecated 'use method DataSetUpdate';
    function DSInsert(const AResource: string; ADataSet: TDataSet)
      : IRESTResponse; deprecated 'use method DataSetInsert';
    function DSDelete(const AResource: string; const AKeyValue: string)
      : IRESTResponse; deprecated 'use method DataSetDelete';

    function Accept(): string; overload;
    function ContentType(): string; overload;
    function ContentEncoding(): string; overload;
    function ConnectionTimeOut(): Integer; overload;
    function ReadTimeOut(): Integer; overload;
    function HasSSL(): Boolean;
    function HasCompression(): Boolean;

    property RawBody: TStringStream read GetRawBody;
    property MultiPartFormData: TIdMultiPartFormDataStream
      read GetMultiPartFormData;
    property BodyParams: TStringlist read GetBodyParams;
    property SessionID: string read GetSessionID write SetSessionID;
    property Username: string read GetUserName write SetUserName;
    property Password: string read GetPassword write SetPassword;
    property UseBasicAuthentication: Boolean read GetBasicAuth
      write SetBasicAuth;
    property RequestHeaders: TStringlist read FRequestHeaders;
    property QueryStringParams: TStringlist read GetQueryStringParams;
    property ProxyServer: string write SetProxyServer;
    property ProxyPort: Integer write SetProxyPort;
  end;

implementation

uses
  AnsiStrings;

type
  TRESTResponse = class(TInterfacedObject, IRESTResponse)
  strict private
    FBody: TMemoryStream;
    FResponseCode: Word;
    FResponseText: string;
    FHeaders: TStringlist;
    FBodyAsJSONValue: TJSONValue;
    FContentType: string;
    FContentEncoding: string;
    function GetHeader(const AValue: string): string;
  private
    FCookies: TIdCookies;
    FHasError: Boolean;
    FErrorObject: TMVCExceptionObj;
  protected
    function GetHasError: Boolean;
    procedure SetHasError(const aHasError: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetContentType: string;
    function GetContentEncoding: string;
    function GetHeaderValue(const AName: string): string;

    procedure SetResponseCode(const AResponseCode: Word);
    procedure SetResponseText(const AResponseText: string);
    procedure SetHeaders(AHeaders: TStrings);

    function Body(): TStream;
    function BodyAsString(): string;
    function BodyAsJSONValue(): TJSONValue;
    function BodyAsJSONObject(): TJSONObject;
    function BodyAsJSONArray(): TJSONArray;

    procedure UpdateResponseCode(const AResponseCode: Word);
    procedure UpdateResponseText(const AResponseText: string);
    procedure UpdateHeaders(AHeaders: TStrings);

    function ResponseCode(): Word;
    function ResponseText(): string;

    function Headers: TStringlist;
    function HeaderValue(const AName: string): string;

    function ContentType(): string;
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

function TRESTResponse.BodyAsJSONArray: TJSONArray;
begin
  Result := BodyAsJSONValue as TJSONArray;
end;

function TRESTResponse.BodyAsJSONObject: TJSONObject;
begin
  Result := BodyAsJSONValue as TJSONObject;
end;

function TRESTResponse.BodyAsJSONValue: TJSONValue;
begin
  try
    if not Assigned(FBodyAsJSONValue) then
    begin
      if (BodyAsString = '') then
        FBodyAsJSONValue := nil
      else
      begin
        try
          FBodyAsJSONValue := TJSONObject.ParseJSONValue(BodyAsString);
        except
          FBodyAsJSONValue := nil;
        end;
      end;
    end;
    Result := FBodyAsJSONValue;
  except
    on E: Exception do
      raise ERESTClientException.Create(E.Message);
  end;
end;

function TRESTResponse.BodyAsString: string;
var
  ss: TStringStream;
begin
  if (FContentEncoding = '') then
    FContentEncoding := 'UTF-8';
  ss := TStringStream.Create('', TEncoding.GetEncoding(FContentEncoding));
  try
    FBody.Position := 0;
    FBody.SaveToStream(ss);
    Result := ss.DataString;
  finally
    FreeAndNil(ss);
  end;
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
  FBodyAsJSONValue := nil;
  FHasError := False;
end;

destructor TRESTResponse.Destroy;
begin
  if Assigned(FBodyAsJSONValue) then
    FreeAndNil(FBodyAsJSONValue);
  FreeAndNil(FHeaders);
  FreeAndNil(FBody);
  FreeAndNil(FCookies);
  FreeAndNil(FErrorObject);
  inherited;
end;

function TRESTResponse.Error: TMVCExceptionObj;
begin
  if not FHasError then
    Exit(nil);
  if not Assigned(FErrorObject) then
  begin
    FErrorObject := Mapper.JSONObjectToObject<TMVCExceptionObj>(self.BodyAsJSONObject);
  end;
  Result := FErrorObject;
end;

function TRESTResponse.GetContentEncoding: string;
begin
  Result := ContentEncoding;
end;

function TRESTResponse.GetContentType: string;
begin
  Result := ContentType;
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
  if Assigned(FHeaders) and (FHeaders.Count > 0) then
  begin
    for s in FHeaders do
      if s.StartsWith(AValue + ':', True) then
        Exit(s);
  end
  else
    Result := '';
end;

function TRESTResponse.GetHeaderValue(const AName: string): string;
begin
  Result := HeaderValue(AName);
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
  for s in self.Headers do
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

  C := GetHeader('content-type');

  CT := C.Split([':'])[1].Split([';']);
  FContentType := Trim(CT[0]);
  FContentEncoding := 'UTF-8';
  if Length(CT) > 1 then
    if CT[1].Trim.StartsWith('charset', True) then
      FContentEncoding := CT[1].Trim.Split(['='])[1].Trim;
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

function TJSONObjectResponseHelper.AsObject<T>: T;
begin
  Result := Mapper.JSONObjectToObject<T>(self);
end;

{ TJSONArrayResponseHelper }

function TJSONArrayResponseHelper.AsObjectList<T>: TObjectList<T>;
begin
  Result := Mapper.JSONArrayToObjectList<T>(self, False, True);
end;

{ TRESTClient }

function TRESTClient.Accept(const AValue: string): TRESTClient;
begin
  FAccept := AValue;
  Result := self;
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
    self.Accept(FAccept + ';charset=' + AValue);

  Result := self;
end;

function TRESTClient.AddFile(const AFieldName, AFileName, AContentType: string)
  : TRESTClient;
begin
  MultiPartFormData.AddFile(AFieldName, AFileName, AContentType);
  Result := self;
end;

function TRESTClient.Asynch(AProc: TProc<IRESTResponse>;
  AProcErr: TProc<Exception>; AProcAlways: TProc; ASynchronized: Boolean)
  : TRESTClient;
begin
  FNextRequestIsAsynch := True;
  FAsynchProc := AProc;
  FAsynchProcErr := AProcErr;
  FAsynchProcAlways := AProcAlways;
  FSynchronized := ASynchronized;
  Result := self;
end;

function TRESTClient.Authentication(const AUsername, APassword: string;
  const ABasicAuth: Boolean): TRESTClient;
begin
  FHTTP.Request.Username := AUsername;
  FHTTP.Request.Password := APassword;
  FHTTP.Request.BasicAuthentication := ABasicAuth;
  Result := self;
end;

function TRESTClient.ClearHeaders: TRESTClient;
begin
  FRequestHeaders.Clear;
  Result := self;
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

  Result := self;
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
      FHTTP.Compressor.Free;
      FHTTP.Compressor := nil;
    end;
  end;
  Result := self;
end;

function TRESTClient.ConnectionTimeOut(const AValue: Integer): TRESTClient;
begin
  FHTTP.ConnectTimeout := AValue;
  Result := self;
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
    self.ContentType(FContentType + ';charset=' + AValue);

  Result := self;
end;

function TRESTClient.ContentEncoding: string;
begin
  Result := FContentEncoding;
end;

function TRESTClient.ContentEncoding(const AValue: string): TRESTClient;
begin
  FContentEncoding := AValue;
  Result := self;
end;

function TRESTClient.ContentType: string;
begin
  Result := FContentType;
end;

function TRESTClient.ContentType(const AValue: string): TRESTClient;
begin
  FContentType := AValue;
  Result := self;
end;

constructor TRESTClient.Create(const AHost: string; const APort: Word;
  AIOHandler: TIdIOHandler);
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
  FContentType := 'application/json; charset=utf-8';
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
  FHTTP.ReadTimeOut := 20000;

  if (AIOHandler <> nil) then
    FHTTP.IOHandler := AIOHandler
  else
    SSL(False);

  Compression(False);

  FHTTP.HandleRedirects := True;
  FHTTP.Request.CustomHeaders.FoldLines := False;
  FHTTP.Request.BasicAuthentication := True;
end;

function TRESTClient.DataSetDelete(const AResource, AKeyValue: string)
  : IRESTResponse;
begin
  Result := doDELETE(AResource, [AKeyValue]);
end;

function TRESTClient.DataSetInsert(const AResource: string; ADataSet: TDataSet)
  : IRESTResponse;
begin
  Result := doPOST(AResource, [], ADataSet.AsJSONObjectString);
end;

function TRESTClient.DataSetUpdate(const AResource: string; ADataSet: TDataSet;
  const AKeyValue: string): IRESTResponse;
begin
  Result := doPUT(AResource, [AKeyValue], ADataSet.AsJSONObjectString);
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

function TRESTClient.doDELETE(const AResource: string;
  const AParams: array of string): IRESTResponse;
var
  URL: string;
begin
  URL := FProtocol + '://' + FHost + ':' + IntToStr(FPort) + AResource +
    EncodeResourceParams(AParams) + EncodeQueryStringParams(QueryStringParams);

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

function TRESTClient.doGET(const AResource: string;
  const AParams: array of string; const aQueryStringParams: TStrings): IRESTResponse;
var
  URL: string;
begin
  URL := FProtocol + '://' + FHost + ':' + IntToStr(FPort) + AResource +
    EncodeResourceParams(AParams);
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

function TRESTClient.doPOST(const AResource: string;
  const AParams: array of string): IRESTResponse;
var
  s: string;
begin
  try
    Result := SendHTTPCommand(httpPOST, FAccept, FContentType,
      FProtocol + '://' + FHost + ':' + IntToStr(FPort) + AResource +
      EncodeResourceParams(AParams) + EncodeQueryStringParams
      (FQueryStringParams), FBodyParams);
  except
    on E: EIdHTTPProtocolException do
      s := E.Message;
  end;
  ClearAllParams;
end;

function TRESTClient.doPOST(const AResource: string;
  const AParams: array of string; ABody: TJSONValue; const AOwnsBody: Boolean)
  : IRESTResponse;
begin
  if not Assigned(ABody) then
    raise ERESTClientException.Create('ABody is nil JSONValue');

  try
    Result := doPOST(AResource, AParams,

{$IF CompilerVersion >= 28}
      ABody.ToJSON

{$ELSE}
      ABody.ToString

{$ENDIF});
  finally
    if AOwnsBody then
      FreeAndNil(ABody);
  end;
end;

function TRESTClient.doPATCH(const AResource: string;
  const AParams: array of string; ABody: TJSONValue; const AOwnsBody: Boolean)
  : IRESTResponse;
begin
  if not Assigned(ABody) then
    raise ERESTClientException.Create('ABody is nil JSONValue');

  try
    Result := doPATCH(AResource, AParams,

{$IF CompilerVersion >= 28}
      ABody.ToJSON

{$ELSE}
      ABody.ToString

{$ENDIF});
  finally
    if AOwnsBody then
      FreeAndNil(ABody);
  end;
end;

function TRESTClient.doPATCH(const AResource: string;
  const AParams: array of string; const ABody: string): IRESTResponse;
var
  URL: string;
begin
  URL := FProtocol + '://' + FHost + ':' + IntToStr(FPort) + AResource +
    EncodeResourceParams(AParams) + EncodeQueryStringParams(QueryStringParams);

  if FNextRequestIsAsynch then
  begin
    Result := nil;
    StartAsynchRequest(httpPOST, URL, ABody);
  end
  else
  begin
    Result := SendHTTPCommandWithBody(httpPATCH, FAccept, FContentType,
      URL, ABody);
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

function TRESTClient.doPATCH(ABody: TJSONValue; const AOwnsBody: Boolean)
  : IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if not Assigned(ABody) then
    raise ERESTClientException.Create('You must enter the Body!');

  Result := doPATCH(FResource, FParams, ABody, AOwnsBody);
end;

function TRESTClient.doPATCH<TBodyType>(ABody: TBodyType;
  const AOwnsBody: Boolean): IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if not Assigned(ABody) then
    raise ERESTClientException.Create('You must enter the Body!');

  Result := doPATCH(FResource, FParams, Mapper.ObjectToJSONObject(ABody)
    as TJSONValue, True);

  if AOwnsBody then
    TObject(ABody).Free;
end;

function TRESTClient.doPATCH<TBodyType>(ABody: TObjectList<TBodyType>;
  const AOwnsBody: Boolean): IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if not Assigned(ABody) then
    raise ERESTClientException.Create('You must enter the Body!');

  ABody.OwnsObjects := AOwnsBody;

  Result := doPATCH(FResource, FParams, Mapper.ObjectListToJSONArray<TBodyType>
    (ABody, AOwnsBody) as TJSONValue, True);
end;

function TRESTClient.doPOST(const AResource: string;
  const AParams: array of string; const ABody: string): IRESTResponse;
var
  URL: string;
begin
  URL := FProtocol + '://' + FHost + ':' + IntToStr(FPort) + AResource +
    EncodeResourceParams(AParams) + EncodeQueryStringParams(QueryStringParams);

  if FNextRequestIsAsynch then
  begin
    Result := nil;
    StartAsynchRequest(httpPOST, URL, ABody);
  end
  else
  begin
    Result := SendHTTPCommandWithBody(httpPOST, FAccept, FContentType,
      URL, ABody);
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

function TRESTClient.doPOST(ABody: TJSONValue; const AOwnsBody: Boolean)
  : IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if not Assigned(ABody) then
    raise ERESTClientException.Create('You must enter the Body!');

  Result := doPOST(FResource, FParams, ABody, AOwnsBody);
end;

function TRESTClient.doPOST<TBodyType>(ABody: TBodyType;
  const AOwnsBody: Boolean): IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if not Assigned(ABody) then
    raise ERESTClientException.Create('You must enter the Body!');

  Result := doPOST(FResource, FParams, Mapper.ObjectToJSONObject(ABody)
    as TJSONValue, True);

  if AOwnsBody then
    TObject(ABody).Free;
end;

function TRESTClient.doPOST<TBodyType>(ABody: TObjectList<TBodyType>;
  const AOwnsBody: Boolean): IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if not Assigned(ABody) then
    raise ERESTClientException.Create('You must enter the Body!');

  ABody.OwnsObjects := AOwnsBody;

  Result := doPOST(FResource, FParams, Mapper.ObjectListToJSONArray<TBodyType>
    (ABody, AOwnsBody) as TJSONValue, True);
end;

function TRESTClient.doPUT(const AResource: string;
  const AParams: array of string): IRESTResponse;
begin
  Result := SendHTTPCommand(httpPUT, FAccept, FContentType,
    FProtocol + '://' + FHost + ':' + IntToStr(FPort) + AResource +
    EncodeResourceParams(AParams) + EncodeQueryStringParams(QueryStringParams),
    FBodyParams);
  ClearAllParams;
end;

function TRESTClient.doPUT(const AResource: string;
  const AParams: array of string; ABody: TJSONValue; const AOwnsBody: Boolean)
  : IRESTResponse;
begin
  if not Assigned(ABody) then
    raise ERESTClientException.Create('ABody is nil JSONValue');

  try
    Result := doPUT(AResource, AParams,

{$IF CompilerVersion >= 28}
      ABody.ToJSON

{$ELSE}
      ABody.ToString

{$ENDIF});
  finally
    if AOwnsBody then
      FreeAndNil(ABody);
  end;
end;

function TRESTClient.doPUT(const AResource: string;
  const AParams: array of string; const ABody: string): IRESTResponse;
var
  URL: string;
begin
  URL := FProtocol + '://' + FHost + ':' + IntToStr(FPort) + AResource +
    EncodeResourceParams(AParams) + EncodeQueryStringParams(QueryStringParams);

  if FNextRequestIsAsynch then
  begin
    Result := nil;
    StartAsynchRequest(httpPUT, URL, ABody);
  end
  else
  begin
    Result := SendHTTPCommandWithBody(httpPUT, FAccept, FContentType,
      URL, ABody);
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

function TRESTClient.doPUT(ABody: TJSONValue; const AOwnsBody: Boolean)
  : IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if not Assigned(ABody) then
    raise ERESTClientException.Create('You must enter the Body!');

  Result := doPUT(FResource, FParams, ABody, AOwnsBody);
end;

function TRESTClient.doPUT<TBodyType>(ABody: TBodyType;
  const AOwnsBody: Boolean): IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if not Assigned(ABody) then
    raise ERESTClientException.Create('You must enter the Body!');

  Result := doPUT(FResource, FParams, Mapper.ObjectToJSONObject(ABody)
    as TJSONValue, True);

  if AOwnsBody then
    TObject(ABody).Free;
end;

function TRESTClient.doPUT<TBodyType>(ABody: TObjectList<TBodyType>;
  const AOwnsBody: Boolean): IRESTResponse;
begin
  if (FResource = '') then
    raise ERESTClientException.Create('You must enter the Resource!');

  if not Assigned(ABody) then
    raise ERESTClientException.Create('You must enter the Body!');

  ABody.OwnsObjects := AOwnsBody;

  Result := doPUT(FResource, FParams, Mapper.ObjectListToJSONArray<TBodyType>
    (ABody, AOwnsBody) as TJSONValue, True);
end;

function TRESTClient.DSDelete(const AResource, AKeyValue: string)
  : IRESTResponse;
begin
  Result := DataSetDelete(AResource, AKeyValue);
end;

function TRESTClient.DSInsert(const AResource: string; ADataSet: TDataSet)
  : IRESTResponse;
begin
  Result := DataSetInsert(AResource, ADataSet);
end;

function TRESTClient.DSUpdate(const AResource: string; ADataSet: TDataSet;
  const AKeyValue: string): IRESTResponse;
begin
  Result := DataSetUpdate(AResource, ADataSet, AKeyValue);
end;

function TRESTClient.EncodeQueryStringParams(const AParams: TStrings;
  AIncludeQuestionMark: Boolean): string;
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
    Result := Result + AParams.Names[I] + '=' + TIdURI.ParamsEncode
      (AParams.ValueFromIndex[I]);
  end;
end;

function TRESTClient.EncodeResourceParams(const AResourceParams
  : array of string): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(AResourceParams) to High(AResourceParams) do
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

function TRESTClient.GetUserName: string;
begin
  Result := FHTTP.Request.Username;
end;

procedure TRESTClient.HandleCookies(aCookies: TIdCookies;
  aRESTResponse: IRESTResponse);
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
    FHTTP.Request.CustomHeaders.AddValue('Cookie',
      'dtsessionid=' + FLastSessionID);

  for I := 0 to FRequestHeaders.Count - 1 do
  begin
    FHTTP.Request.CustomHeaders.AddValue(FRequestHeaders.Names[I],
      FRequestHeaders.ValueFromIndex[I]);
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
  Result := self;
end;

function TRESTClient.HTTPCommandToString(const ACommand: THTTPCommand): string;
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
    raise ERESTClientException.Create
      ('Unknown HTTPCommand in TRESTClient.HTTPCommandToString');
  end;
end;

function TRESTClient.Params(const AValues: array of string): TRESTClient;
var
  I: Integer;
begin
  SetLength(FParams, Length(AValues));
  for I := Low(AValues) to High(AValues) do
    FParams[I] := AValues[I];
  Result := self;
end;

function TRESTClient.ReadTimeOut(const AValue: Integer): TRESTClient;
begin
  FHTTP.ReadTimeOut := AValue;
  Result := self;
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
  Result := self;
end;

function TRESTClient.Resource(const AValue: string): TRESTClient;
begin
  FResource := AValue;
  Result := self;
end;

function TRESTClient.SendHTTPCommand(const ACommand: THTTPCommand;
  const AAccept, AContentType, AResource: string; ABodyParams: TStrings)
  : IRESTResponse;
begin
  Result := TRESTResponse.Create;

  FHTTP.Request.RawHeaders.Clear;
  FHTTP.Request.CustomHeaders.Clear;
  FHTTP.Request.Accept := AAccept;
  FHTTP.Request.ContentType := AContentType;

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
          if (MultiPartFormData.Size <> 0)
          then { TODO -oDaniele -cGeneral : Rework please!!! }
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
      Result.Body.Write(UTF8Encode(E.ErrorMessage)[1],
        AnsiStrings.ElementToCharLen(UTF8Encode(E.ErrorMessage), Length(E.ErrorMessage) * 2));
    end
    else
      raise;
  end;

  HandleCookies(FHTTP.CookieManager.CookieCollection, Result);

  Result.UpdateResponseCode(FHTTP.Response.ResponseCode);
  Result.UpdateResponseText(FHTTP.Response.ResponseText);
  Result.UpdateHeaders(FHTTP.Response.RawHeaders);
end;

function TRESTClient.SendHTTPCommandWithBody(const ACommand: THTTPCommand;
  const AAccept, AContentType, AResource, ABody: string): IRESTResponse;
begin
  Result := TRESTResponse.Create;

  FHTTP.Request.RawHeaders.Clear;
  FHTTP.Request.CustomHeaders.Clear;
  FHTTP.Request.Accept := AAccept;
  FHTTP.Request.ContentType := AContentType;

  HandleRequestCookies;
  try
    case ACommand of
      httpGET:
        begin
          FHTTP.Get(AResource, Result.Body);
        end;

      httpPOST:
        begin
          if (MultiPartFormData.Size <> 0) then
            raise ERESTClientException.Create('This method cannot send files');

          RawBody.Position := 0;
          RawBody.Size := 0;

{$WARNINGS OFF}
          if (LowerCase(FHTTP.Request.CharSet) = 'utf-8') then
            RawBody.WriteString(UTF8ToString(ABody))
          else
            RawBody.WriteString(ABody);

{$WARNINGS ON}
          FHTTP.Post(AResource, RawBody, Result.Body);
        end;

      httpPATCH:
        begin
          raise ERESTClientException.Create
            ('Sorry, PATCH is not supported by the RESTClient because is not supportd by the TidHTTP');
        end;

      httpPUT:
        begin
          RawBody.Position := 0;
          RawBody.Size := 0;

{$WARNINGS OFF}
          if (LowerCase(FHTTP.Request.CharSet) = 'utf-8') then
            RawBody.WriteString(UTF8ToString(ABody))
          else
            RawBody.WriteString(ABody);

{$WARNINGS ON}
          FHTTP.Put(AResource, RawBody, Result.Body);
        end;

      httpDELETE:
        begin
          FHTTP.Delete(AResource);
          RawBody.Size := 0;
        end;
    end;
  except
    on E: EIdHTTPProtocolException do
      Result.Body.Write(UTF8Encode(E.ErrorMessage)[1],
        AnsiStrings.ElementToCharLen(UTF8Encode(E.ErrorMessage), Length(E.ErrorMessage) * 2));
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
      FHTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(FHTTP);
  end
  else
  begin
    if (FHTTP.IOHandler <> nil) then
    begin
      FHTTP.IOHandler.Free;
      FHTTP.IOHandler := nil;
    end;
  end;
  Result := self;
end;

procedure TRESTClient.StartAsynchRequest(const ACommand: THTTPCommand;
  const AResource: string);
begin
  StartAsynchRequest(ACommand, AResource, '');
end;

procedure TRESTClient.StartAsynchRequest(const ACommand: THTTPCommand;
  const AResource, ABody: string);
var
  th: TThread;
begin
  th := TThread.CreateAnonymousThread(
    procedure
    var
      R: IRESTResponse;
    begin
      try
        R := SendHTTPCommandWithBody(ACommand, FAccept, FContentType,
          AResource, ABody);
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
          if FSynchronized then
            TThread.Synchronize(nil,
              procedure
              begin
                FAsynchProcErr(E);
              end)
          else
            FAsynchProcErr(E);
        end;
      end;
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
