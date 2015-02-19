unit MVCFramework.RESTClient;

interface

uses
  Classes,
  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdHTTP,
  idURI,
{$IF CompilerVersion < 27}
  Data.DBXJSON,
{$ELSE}
  System.JSON,
{$IFEND}
  IdMultipartFormData,
  System.SysUtils, Data.DB, IdIOHandler;

type
  TArrayOfString = array of string;
  THttpCommand = (httpGET, httpPOST, httpPUT, httpDELETE, httpPATCH, httpTRACE);

  IRESTResponse = interface
    function BodyAsString: string;
    function BodyAsJsonObject: TJSONObject;
    function BodyAsJsonValue: TJSONValue;
    function ResponseCode: Word;
    function ResponseText: string;
    function Headers: TStringlist;
    function GetContentType: string;
    function GetContentEncoding: string;
    function Body: TStringStream;
    function GetHeaderValue(const Name: string): string;
    procedure SetResponseCode(AResponseCode: Word);
    procedure SetResponseText(AResponseText: string);
    procedure SetHeaders(AHeaders: TStrings);
  end;

  TRESTResponse = class(TInterfacedObject, IRESTResponse)
  private
    FBody: TStringStream;
    FResponseCode: Word;
    FResponseText: string;
    FHeaders: TStringlist;
    FBodyAsJSONValue: TJSONValue;
    FContentType: string;
    FContentEncoding: string;
    function GetHeader(const Value: string): string;
  public

    function BodyAsString: string;
    function BodyAsJsonObject: TJSONObject;
    function BodyAsJsonValue: TJSONValue;
    function ResponseCode: Word;
    function ResponseText: string;
    function Headers: TStringlist;
    function Body: TStringStream;
    function GetContentType: string;
    function GetContentEncoding: string;

    procedure SetResponseCode(AResponseCode: Word);
    procedure SetResponseText(AResponseText: string);
    procedure SetHeaders(AHeaders: TStrings);
    constructor Create; virtual;
    destructor Destroy; override;
    function GetHeaderValue(const Name: string): string;
  end;

  TRESTClient = class(TInterfacedObject)
  private
    FServerName: string;
    FServerPort: Word;
    FBodyParams: TStringlist;
    FQueryStringParams: TStringlist;
    FAccept: string;
    FRawBody: TStringStream;
    FHTTP: TIdHTTP;
    FContentType: string;
    FLastSessionID: string;
    FNextRequestIsAsynch: Boolean;
    FAsynchProc: TProc<IRESTResponse>;
    FAsynchProcErr: TProc<Exception>;
    FPrimaryThread: TThread;
    FMultiPartFormData: TIdMultiPartFormDataStream;
    FAsynchProcAlways: TProc;
    FProtocol: string;
    FSynchronized: Boolean;
    FContentEncoding: string;
    function EncodeQueryStringParams(const AQueryStringParams: TStrings;
      IncludeQuestionMark: Boolean = true): string;
    procedure SetBodyParams(const Value: TStringlist);
    procedure SetQueryStringParams(const Value: TStringlist);
    procedure SetAccept(const Value: string);
    procedure SetContentType(const Value: string);
    function GetSessionID: string;
    procedure SetSessionID(const Value: string);
    function GetBodyParams: TStringlist;
    function GetQueryStringParams: TStringlist;
    function GetRawBody: TStringStream;
    procedure SetReadTimeout(const Value: Integer);
    function GetReadTimeout: Integer;
    procedure StartAsynchRequest(AHTTPMethod: THttpCommand; AUrl: string;
      ABodyString: string); overload;
    procedure StartAsynchRequest(AHTTPMethod: THttpCommand; AUrl: string); overload;
    procedure SetConnectionTimeout(const Value: Integer);
    function GetConnectionTimeout: Integer;
    procedure SetRequestHeaders(const Value: TStringlist);
    procedure SetPassword(const Value: string);
    procedure SetUserName(const Value: string);
    function GetPassword: string;
    function GetUserName: string;
    function GetUseBasicAuthentication: Boolean;
    procedure SetUseBasicAuthentication(const Value: Boolean);

  strict protected
    FRequestHeaders: TStringlist;

  protected
    procedure HandleCookies;
    function EncodeResourceParams(AResourceParams: array of string): string;
    function HttpCommandToString(const AHttpCommand: THttpCommand): string;
    function SendHTTPCommand(const ACommand: THttpCommand;
      const AAccept, AContentType, AUrl: string; ABodyParams: TStrings): IRESTResponse;
    function SendHTTPCommandWithBody(const ACommand: THttpCommand;
      const AAccept, AContentType, AUrl: string; ABodyString: string): IRESTResponse;
    procedure HandleRequestCookies;
    function GetMultipartFormData: TIdMultiPartFormDataStream;

  public
    constructor Create(const AServerName: string; AServerPort: Word = 80;
      AIOHandler: TIdIOHandler = nil); virtual;
    destructor Destroy; override;

    function AddFile(const FieldName, FileName: string; const ContentType: string = '')
      : TRESTClient;

    function Asynch(AProc: TProc<IRESTResponse>; AProcErr: TProc<Exception> = nil;
      AProcAlways: TProc = nil; ASynchronized: Boolean = false): TRESTClient;
    function ClearAllParams: TRESTClient;
    function ResetSession: TRESTClient;
    function Accept(const AcceptHeader: string): TRESTClient; overload;
    function Accept: string; overload;
    function ContentType(const ContentTypeHeader: string): TRESTClient; overload;
    function ContentType: string; overload;
    function ContentEncoding(const ContentEncodingHeader: string): TRESTClient; overload;
    function ContentEncoding: string; overload;
    // requests
    function doGET(AResource: string; AResourceParams: array of string): IRESTResponse;
    function doPOST(AResource: string; AResourceParams: array of string): IRESTResponse; overload;
    function doPOST(AResource: string; AResourceParams: array of string; AJSONValue: TJSONValue;
      AOwnsJSONBody: Boolean = true): IRESTResponse; overload;
    function doPOST(AResource: string; AResourceParams: array of string; ABodyString: string)
      : IRESTResponse; overload;
    function doPATCH(AResource: string; AResourceParams: array of string; AJSONValue: TJSONValue;
      AOwnsJSONBody: Boolean = true): IRESTResponse; overload;
    function doPATCH(AResource: string; AResourceParams: array of string; ABodyString: string)
      : IRESTResponse; overload;
    function doPUT(AResource: string; AResourceParams: array of string): IRESTResponse; overload;
    function doPUT(AResource: string; AResourceParams: array of string; AJSONValue: TJSONValue;
      AOwnsJSONBody: Boolean = true): IRESTResponse; overload;
    function doPUT(AResource: string; AResourceParams: array of string; ABodyString: string)
      : IRESTResponse; overload;
    function doDELETE(AResource: string; AResourceParams: array of string): IRESTResponse;
    property UserName: string read GetUserName write SetUserName;
    property Password: string read GetPassword write SetPassword;
    property UseBasicAuthentication: Boolean read GetUseBasicAuthentication
      write SetUseBasicAuthentication;
    property BodyParams: TStringlist read GetBodyParams write SetBodyParams;
    property RawBody: TStringStream read GetRawBody;
    property QueryStringParams: TStringlist read GetQueryStringParams write SetQueryStringParams;
    property SessionID: string read GetSessionID write SetSessionID;
    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout;
    property RequestHeaders: TStringlist read FRequestHeaders write SetRequestHeaders;
    // dataset specific methods
    function DSUpdate(const URL: string; DataSet: TDataSet; const KeyValue: string): IRESTResponse;
    function DSInsert(const URL: string; DataSet: TDataSet): IRESTResponse;
    function DSDelete(const URL: string; const KeyValue: string): IRESTResponse;
  end;

function StringsToArrayOfString(const AStrings: TStrings): TArrayOfString;

implementation

uses
  ObjectsMappers;

{ TRSF }

function StringsToArrayOfString(const AStrings: TStrings): TArrayOfString;

var
  i: Integer;
begin
  SetLength(Result, AStrings.Count);
  if AStrings.Count = 0 then
    Exit;
  for i := 0 to AStrings.Count - 1 do
    Result[i] := AStrings[i];
end;

function TRESTClient.Accept(const AcceptHeader: string): TRESTClient;
begin
  SetAccept(AcceptHeader);
  Result := Self;
end;

function TRESTClient.Accept: string;
begin
  Result := FAccept;
end;

function TRESTClient.AddFile(const FieldName, FileName, ContentType: string): TRESTClient;
begin
  GetMultipartFormData.AddFile(FieldName, FileName, ContentType);
  Result := Self;
end;

function TRESTClient.Asynch(AProc: TProc<IRESTResponse>; AProcErr: TProc<Exception>;
  AProcAlways: TProc; ASynchronized: Boolean): TRESTClient;
begin
  FNextRequestIsAsynch := true;
  FAsynchProc := AProc;
  FAsynchProcErr := AProcErr;
  FAsynchProcAlways := AProcAlways;
  FSynchronized := ASynchronized;
  Result := Self;
end;

function TRESTClient.ClearAllParams: TRESTClient;
begin
  if Assigned(FRawBody) then
  begin
    FRawBody.Size := 0;
    FRawBody.Position := 0;
  end;
  if Assigned(FBodyParams) then
    FBodyParams.Clear;
  if Assigned(FQueryStringParams) then
    FQueryStringParams.Clear;
  Result := Self;
  FNextRequestIsAsynch := false;
  FAsynchProc := nil;
  FAsynchProcErr := nil;
  FAsynchProcAlways := nil;
  UseBasicAuthentication := false;
end;

function TRESTClient.ContentEncoding(const ContentEncodingHeader: string): TRESTClient;
begin
  FContentEncoding := ContentEncodingHeader;
  Result := Self;
end;

function TRESTClient.ContentEncoding: string;
begin
  Result := FContentEncoding;
end;

function TRESTClient.ContentType: string;
begin
  Result := FContentType;
end;

function TRESTClient.ContentType(const ContentTypeHeader: string): TRESTClient;
begin
  SetContentType(ContentTypeHeader);
  Result := Self;
end;

constructor TRESTClient.Create(const AServerName: string; AServerPort: Word;
  AIOHandler: TIdIOHandler);
var
  Pieces: TArray<string>;
begin
  inherited Create;
  FPrimaryThread := TThread.CurrentThread;
  FServerName := AServerName;
  FServerPort := AServerPort;
  FBodyParams := nil; // TStringlist.Create;
  FQueryStringParams := nil; // TStringlist.Create;
  FRawBody := nil; // TStringStream.Create('');
  FAccept := 'application/json';
  FContentType := 'application/json; charset=utf-8';
  FRequestHeaders := TStringlist.Create;
  if AServerName.Contains('://') then
  begin
    Pieces := FServerName.Split(['://'], 2, TStringSplitOptions.ExcludeEmpty);
    FProtocol := Pieces[0];
    FServerName := Pieces[1];
  end
  else
    FProtocol := 'http';

  FHTTP := TIdHTTP.Create(nil);
  FHTTP.ReadTimeout := 20000;
  FHTTP.IOHandler := AIOHandler;
  FHTTP.Request.BasicAuthentication := True;
  // FHTTP.AllowCookies := true;
end;

destructor TRESTClient.Destroy;
begin
  FreeAndNil(FBodyParams);
  FreeAndNil(FQueryStringParams);
  FreeAndNil(FRawBody);
  FreeAndNil(FRequestHeaders);
  FreeAndNil(FMultiPartFormData);
  FHTTP.Free;
  inherited;
end;

function TRESTClient.doDELETE(AResource: string; AResourceParams: array of string): IRESTResponse;
var
  URL: string;
begin
  URL := FProtocol + '://' + FServerName + ':' + inttostr(FServerPort) + AResource +
    EncodeResourceParams(AResourceParams) + EncodeQueryStringParams(FQueryStringParams);

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

procedure TRESTClient.StartAsynchRequest(AHTTPMethod: THttpCommand; AUrl: string);
begin
  StartAsynchRequest(AHTTPMethod, AUrl, '');
end;

procedure TRESTClient.StartAsynchRequest(AHTTPMethod: THttpCommand; AUrl: string;
  ABodyString: string);
var
  th: TThread;
begin
  th := TThread.CreateAnonymousThread(
    procedure
    var
      R: IRESTResponse;
    begin
      try
        R := SendHTTPCommandWithBody(AHTTPMethod, FAccept, FContentType, AUrl, ABodyString);
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

function TRESTClient.doGET(AResource: string; AResourceParams: array of string): IRESTResponse;
var
  URL: string;
begin
  URL := FProtocol + '://' + FServerName + ':' + inttostr(FServerPort) + AResource +
    EncodeResourceParams(AResourceParams) + EncodeQueryStringParams(FQueryStringParams);

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

function TRESTClient.doPOST(AResource: string; AResourceParams: array of string): IRESTResponse;
var
  s: string;
begin
  try
    Result := SendHTTPCommand(httpPOST, FAccept, FContentType, FProtocol + '://' + FServerName + ':'
      + inttostr(FServerPort) + AResource + EncodeResourceParams(AResourceParams) +
      EncodeQueryStringParams(FQueryStringParams), FBodyParams);
  except
    on E: EIdHTTPProtocolException do
    begin
      s := E.Message;
    end;
  end;
  ClearAllParams;
end;

function TRESTClient.doPOST(AResource: string; AResourceParams: array of string;
AJSONValue: TJSONValue; AOwnsJSONBody: Boolean): IRESTResponse;
begin
  if not Assigned(AJSONValue) then
    raise Exception.Create('AJSONValue is nil');
  try
    Result := doPOST(AResource, AResourceParams,
{$IF CompilerVersion >= 28}
    AJSONValue.ToJSON
{$ELSE}
    AJSONValue.ToString
{$ENDIF});
  finally
    if AOwnsJSONBody then
      FreeAndNil(AJSONValue);
  end;
end;

function TRESTClient.doPATCH(AResource: string; AResourceParams: array of string;
ABodyString: string): IRESTResponse;
var
  URL: string;
begin
  URL := FProtocol + '://' + FServerName + ':' + inttostr(FServerPort) + AResource +
    EncodeResourceParams(AResourceParams) + EncodeQueryStringParams(FQueryStringParams);

  if FNextRequestIsAsynch then
  begin
    Result := nil;
    StartAsynchRequest(httpPOST, URL, ABodyString);
  end
  else
  begin
    Result := SendHTTPCommandWithBody(httpPATCH, FAccept, FContentType, URL, ABodyString);
    ClearAllParams;
  end;
end;

function TRESTClient.doPATCH(AResource: string; AResourceParams: array of string;
AJSONValue: TJSONValue; AOwnsJSONBody: Boolean): IRESTResponse;
begin
  if not Assigned(AJSONValue) then
    raise Exception.Create('AJSONValue is nil');
  try
    Result := doPATCH(AResource, AResourceParams,
{$IF CompilerVersion >= 28}
    AJSONValue.ToJSON
{$ELSE}
    AJSONValue.ToString
{$ENDIF});
  finally
    if AOwnsJSONBody then
      FreeAndNil(AJSONValue);
  end;
end;

function TRESTClient.doPOST(AResource: string; AResourceParams: array of string;
ABodyString: string): IRESTResponse;
var
  URL: string;
begin
  URL := FProtocol + '://' + FServerName + ':' + inttostr(FServerPort) + AResource +
    EncodeResourceParams(AResourceParams) + EncodeQueryStringParams(FQueryStringParams);

  if FNextRequestIsAsynch then
  begin
    Result := nil;
    StartAsynchRequest(httpPOST, URL, ABodyString);
  end
  else
  begin
    Result := SendHTTPCommandWithBody(httpPOST, FAccept, FContentType, URL, ABodyString);
    ClearAllParams;
  end;
end;

function TRESTClient.doPUT(AResource: string; AResourceParams: array of string; ABodyString: string)
  : IRESTResponse;
var
  URL: string;
begin
  URL := FProtocol + '://' + FServerName + ':' + inttostr(FServerPort) + AResource +
    EncodeResourceParams(AResourceParams) + EncodeQueryStringParams(FQueryStringParams);

  if FNextRequestIsAsynch then
  begin
    Result := nil;
    StartAsynchRequest(httpPUT, URL, ABodyString);
  end
  else
  begin
    Result := SendHTTPCommandWithBody(httpPUT, FAccept, FContentType, URL, ABodyString);
    ClearAllParams;
  end;

end;

function TRESTClient.DSDelete(const URL, KeyValue: string): IRESTResponse;
begin
  Result := doDELETE(URL, [KeyValue]);
end;

function TRESTClient.DSInsert(const URL: string; DataSet: TDataSet): IRESTResponse;
begin
  Result := doPOST(URL, [], DataSet.AsJSONObjectString);
end;

function TRESTClient.DSUpdate(const URL: string; DataSet: TDataSet; const KeyValue: string)
  : IRESTResponse;
begin
  Result := doPUT(URL, [KeyValue], DataSet.AsJSONObjectString);
end;

function TRESTClient.doPUT(AResource: string; AResourceParams: array of string;
AJSONValue: TJSONValue; AOwnsJSONBody: Boolean = true): IRESTResponse;
begin
  if not Assigned(AJSONValue) then
    raise Exception.Create('AJSONValue is nil');

  try
    Result := doPUT(AResource, AResourceParams,
{$IF CompilerVersion >= 28}
    AJSONValue.ToJSON
{$ELSE}
    AJSONValue.ToString
{$ENDIF});
  finally
    if AOwnsJSONBody then
      FreeAndNil(AJSONValue);
  end;
end;

function TRESTClient.doPUT(AResource: string; AResourceParams: array of string): IRESTResponse;
begin
  Result := SendHTTPCommand(httpPUT, FAccept, FContentType, FProtocol + '://' + FServerName + ':' +
    inttostr(FServerPort) + AResource + EncodeResourceParams(AResourceParams) +
    EncodeQueryStringParams(FQueryStringParams), FBodyParams);
  ClearAllParams;
end;

function TRESTClient.EncodeResourceParams(AResourceParams: array of string): string;
var
  i: Integer;
begin
  Result := '';
  for i := low(AResourceParams) to high(AResourceParams) do
    Result := Result + '/' + TIdURI.ParamsEncode(AResourceParams[i]);
end;

function TRESTClient.GetBodyParams: TStringlist;
begin
  if not Assigned(FBodyParams) then
    FBodyParams := TStringlist.Create;
  Result := FBodyParams;
end;

function TRESTClient.GetConnectionTimeout: Integer;
begin
  Result := FHTTP.ConnectTimeout;
end;

function TRESTClient.GetMultipartFormData: TIdMultiPartFormDataStream;
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
  Result := Self.FLastSessionID;
end;

function TRESTClient.GetUseBasicAuthentication: Boolean;
begin
  Result := FHTTP.Request.BasicAuthentication;
end;

function TRESTClient.GetUserName: string;
begin
  Result := FHTTP.Request.Password;
end;

function TRESTClient.GetReadTimeout: Integer;
begin
  Result := FHTTP.ReadTimeout;
end;

function TRESTClient.EncodeQueryStringParams(const AQueryStringParams: TStrings;
IncludeQuestionMark: Boolean = true): string;
var
  i: Integer;
begin
  Result := '';
  if not Assigned(AQueryStringParams) or (AQueryStringParams.Count = 0) then
    Exit;
  if IncludeQuestionMark then
    Result := '?';

  for i := 0 to AQueryStringParams.Count - 1 do
  begin
    if i > 0 then
      Result := Result + '&';
    Result := Result + AQueryStringParams.Names[i] + '=' +
      TIdURI.ParamsEncode(AQueryStringParams.ValueFromIndex[i]);
  end;
end;

procedure TRESTClient.HandleCookies;
var
  s: string;
  arr: TArray<string>;
begin
  // Log('Received cookies', FHTTP.Response.RawHeaders.Text);
  for s in FHTTP.Response.RawHeaders do
  begin
    if s.StartsWith('Set-Cookie', true) then
    begin
      arr := s.Split([':'], 2);
      if arr[1].trim.StartsWith('dtsessionid') then
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
  i: Integer;
begin
  if Assigned(FHTTP.CookieManager) then
    FHTTP.CookieManager.CookieCollection.Clear;
  if not FLastSessionID.trim.IsEmpty then
    FHTTP.Request.CustomHeaders.AddValue('Cookie', 'dtsessionid=' + FLastSessionID);

  for i := 0 to FRequestHeaders.Count - 1 do
  begin
    FHTTP.Request.CustomHeaders.AddValue(FRequestHeaders.Names[i],
      FRequestHeaders.ValueFromIndex[i]);
  end;
end;

function TRESTClient.HttpCommandToString(const AHttpCommand: THttpCommand): string;
begin
  case AHttpCommand of
    httpGET:
      Result := 'GET';
    httpPOST:
      Result := 'POST';
    httpPUT:
      Result := 'PUT';
    httpDELETE:
      Result := 'DELETE';
  else
    raise Exception.Create('Unknown HttpCommand in TRSF.HttpCommandToString');
  end;
end;

function TRESTClient.ResetSession: TRESTClient;
begin
  SessionID := '';
  if Assigned(FHTTP.CookieManager) then
    FHTTP.CookieManager.CookieCollection.Clear;
  FHTTP.Request.RawHeaders.Clear;
  Result := Self;
end;

function TRESTClient.SendHTTPCommand(const ACommand: THttpCommand;
const AAccept, AContentType, AUrl: string; ABodyParams: TStrings): IRESTResponse;
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
          FHTTP.Get(AUrl, Result.Body);
        end;

      httpPOST:
        begin
          if GetMultipartFormData.Size = 0 then
          begin
            Result.Body.Position := 0;
            GetRawBody; // creates the body
            FHTTP.Post(AUrl, FRawBody, Result.Body);
          end
          else
          begin
            FHTTP.Post(AUrl, GetMultipartFormData, Result.Body);
            GetMultipartFormData.Clear;
          end;
        end;

      httpPUT:
        begin
          if GetMultipartFormData.Size <> 0 then { TODO -oDaniele -cGeneral : Rework please!!! }
            raise Exception.Create('Only POST can Send Files');
          Result.Body.Position := 0;
          if Assigned(ABodyParams) and (ABodyParams.Count > 0) then
          begin
            GetRawBody.Size := 0;
            GetRawBody.WriteString(EncodeQueryStringParams(ABodyParams, false));
          end;
          FHTTP.Put(AUrl, FRawBody, Result.Body);
        end;

      httpDELETE:
        begin
          Result.Body.Position := 0;
          FHTTP.Delete(AUrl);
          GetRawBody.Size := 0;
        end;
    end;
  except
    on E: EIdHTTPProtocolException do
    begin
      Result.Body.WriteString(E.ErrorMessage);
    end;
  end;
  HandleCookies;
  Result.SetResponseCode(FHTTP.Response.ResponseCode);
  Result.SetResponseText(FHTTP.Response.ResponseText);
  Result.SetHeaders(FHTTP.Response.RawHeaders);
end;

function TRESTClient.SendHTTPCommandWithBody(const ACommand: THttpCommand;
const AAccept, AContentType, AUrl: string; ABodyString: string): IRESTResponse;
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
          FHTTP.Get(AUrl, Result.Body);
        end;

      httpPOST:
        begin
          if GetMultipartFormData.Size <> 0 then
            raise Exception.Create('This method cannot send files');

          RawBody.Position := 0;
          FRawBody.Size := 0;
          FRawBody.WriteString(UTF8Encode(ABodyString));
          FHTTP.Post(AUrl, FRawBody, Result.Body);
        end;

      httpPATCH:
        begin
          raise Exception.Create
            ('Sorry, PATCH is not supported by the RESTClient because is not supportd by the TidHTTP');
        end;

      httpPUT:
        begin
          RawBody.Position := 0;
          FRawBody.Size := 0;
          FRawBody.WriteString(UTF8Encode(ABodyString));
          FHTTP.Put(AUrl, FRawBody, Result.Body);
        end;

      httpDELETE:
        begin
          FHTTP.Delete(AUrl);
          RawBody.Size := 0;
        end;
    end;
  except
    on E: EIdHTTPProtocolException do
    begin
      Result.Body.WriteString(E.ErrorMessage);
    end;
  end;
  HandleCookies;
  Result.SetResponseCode(FHTTP.Response.ResponseCode);
  Result.SetResponseText(FHTTP.Response.ResponseText);
  Result.SetHeaders(FHTTP.Response.RawHeaders);
end;

procedure TRESTClient.SetAccept(const Value: string);
begin
  FAccept := Value;
end;

procedure TRESTClient.SetBodyParams(const Value: TStringlist);
begin
  FBodyParams := Value;
end;

procedure TRESTClient.SetConnectionTimeout(const Value: Integer);
begin
  FHTTP.ConnectTimeout := Value;
end;

procedure TRESTClient.SetContentType(const Value: string);
begin
  FContentType := Value;
end;

procedure TRESTClient.SetPassword(const Value: string);
begin
  FHTTP.Request.Password := Value;
end;

procedure TRESTClient.SetQueryStringParams(const Value: TStringlist);
begin
  FQueryStringParams := Value;
end;

procedure TRESTClient.SetSessionID(const Value: string);
begin
  FLastSessionID := Value;
  if Assigned(FHTTP.CookieManager) then
    FHTTP.CookieManager.CookieCollection.Clear;
end;

procedure TRESTClient.SetUseBasicAuthentication(const Value: Boolean);
begin
  FHTTP.Request.BasicAuthentication := Value;
end;

procedure TRESTClient.SetUserName(const Value: string);
begin
  FHTTP.Request.UserName := Value;
end;

procedure TRESTClient.SetReadTimeout(const Value: Integer);
begin
  FHTTP.ReadTimeout := Value;
end;

procedure TRESTClient.SetRequestHeaders(const Value: TStringlist);
begin
  FRequestHeaders.Assign(Value);
end;

{ TRESTResponse }

function TRESTResponse.Body: TStringStream;
begin
  Result := FBody;
end;

function TRESTResponse.BodyAsJsonObject: TJSONObject;
begin
  Result := BodyAsJsonValue as TJSONObject;
end;

function TRESTResponse.BodyAsJsonValue: TJSONValue;
begin
  try
    if not Assigned(FBodyAsJSONValue) then
    begin
      if BodyAsString = '' then
        FBodyAsJSONValue := nil
      else
      begin
        try
          FBodyAsJSONValue := TJSONObject.ParseJSONValue(BodyAsString);
          // if Assigned(V) then
          // FBodyAsJSONObject := V as TJSONObject;
        except
          FBodyAsJSONValue := nil;
        end;
      end;
    end;
    Result := FBodyAsJSONValue;
  except
    on E: Exception do
    begin
      raise;
    end;
  end;
end;

function TRESTResponse.BodyAsString: string;
var
  ss: TStringStream;
begin
  if FContentEncoding.IsEmpty then
    FContentEncoding := 'UTF-8';
  ss := TStringStream.Create('', TEncoding.GetEncoding(FContentEncoding));
  try
    FBody.Position := 0;
    FBody.SaveToStream(ss);
    Result := ss.DataString;
  finally
    ss.Free;
  end;
end;

constructor TRESTResponse.Create;
begin
  inherited;
  FHeaders := TStringlist.Create;
  FBody := TStringStream.Create('', TEncoding.UTF8);
  FBodyAsJSONValue := nil;
end;

destructor TRESTResponse.Destroy;
begin
  FreeAndNil(FBodyAsJSONValue);
  FHeaders.Free;
  FBody.Free;
  inherited;
end;

function TRESTResponse.GetContentEncoding: string;
begin
  Result := FContentEncoding;
end;

function TRESTResponse.GetContentType: string;
begin
  Result := FContentType;
end;

function TRESTResponse.GetHeader(const Value: string): string;
var
  s: string;
begin
  if Assigned(FHeaders) and (FHeaders.Count > 0) then
  begin
    for s in FHeaders do
    begin
      if s.StartsWith(Value + ':', true) then
      begin
        Exit(s);
      end;
    end;
  end
  else
  begin
    Result := '';
  end;
end;

function TRESTResponse.GetHeaderValue(const Name: string): string;
var
  s: string;
  arr: TArray<string>;
begin
  Result := '';
  for s in Self.Headers do
  begin
    arr := s.Split([':'], 2);
    if SameText(arr[0].trim, name) then
    begin
      Result := arr[1].trim;
      Break;
    end;
  end;
end;

function TRESTResponse.Headers: TStringlist;
begin
  Result := FHeaders;
end;

function TRESTResponse.ResponseCode: Word;
begin
  Result := FResponseCode;
end;

function TRESTResponse.ResponseText: string;
begin
  Result := FResponseText;
end;

procedure TRESTResponse.SetHeaders(AHeaders: TStrings);
var
  CT: TArray<string>;
  C: string;
begin
  FHeaders.Assign(AHeaders);

  C := GetHeader('content-type');

  CT := C.Split([':'])[1].Split([';']);
  FContentType := trim(CT[0]);
  FContentEncoding := 'UTF-8'; // default encoding
  if Length(CT) > 1 then
  begin
    if CT[1].trim.StartsWith('charset', true) then
    begin
      FContentEncoding := CT[1].trim.Split(['='])[1].trim;
    end;
  end;
end;

procedure TRESTResponse.SetResponseCode(AResponseCode: Word);
begin
  FResponseCode := AResponseCode;
end;

procedure TRESTResponse.SetResponseText(AResponseText: string);
begin
  FResponseText := AResponseText;
end;

end.
