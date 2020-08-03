unit MVCFramework.RESTClient;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  IPPeerClient,
  REST.Client,
  REST.Types,
  MVCFramework.Serializer.Intf,
  MVCFramework.RESTClient.Intf;

type
  TRESTClient = class(TInterfacedObject, IRESTClient)
  private
    fRESTClient: TCustomRESTClient;
    fRESTRequest: TCustomRESTRequest;
    fRESTResponse: TCustomRESTResponse;
    fSerializer: IMVCSerializer;
  public
    constructor Create;
    destructor Destroy; override;

    function BaseURL: string; overload;
    function BaseURL(const ABaseURL: string): IRESTClient; overload;
    function ReadTimeout: Integer; overload;
    function ReadTimeout(const AReadTimeout: Integer): IRESTClient; overload;

    function ProxyServer: string; overload;
    function ProxyServer(const AProxyServer: string): IRESTClient; overload;
    function ProxyPort: Integer; overload;
    function ProxyPort(const AProxyPort: Integer): IRESTClient; overload;
    function ProxyUsername: string; overload;
    function ProxyUsername(const AProxyUsername: string): IRESTClient; overload;
    function ProxyPassword: string; overload;
    function ProxyPassword(const AProxyPassword: string): IRESTClient; overload;

    function SetBasicAuthorization(const AUsername, APassword: string): IRESTClient;
    function SetBearerAuthorization(const AToken: string): IRESTClient;

    function AddHeader(const AName, AValue: string): IRESTClient; overload;
    function AddHeader(const AName: string; AValue: Integer): IRESTClient; overload;
    function AddHeader(const AName: string; AValue: Int64): IRESTClient; overload;
    function AddHeader(const AName: string; AValue: TGUID): IRESTClient; overload;
    function AddHeader(const AName: string; AValue: TDateTime): IRESTClient; overload;
    function AddHeader(const AName: string; AValue: TDate): IRESTClient; overload;
    function AddHeader(const AName: string; AValue: TTime): IRESTClient; overload;
    function AddHeader(const AName: string; AValue: Double): IRESTClient; overload;
    function ClearHeaders: IRESTClient;

    function AddPathParam(const AName, AValue: string): IRESTClient; overload;
    function AddPathParam(const AName: string; AValue: Integer): IRESTClient; overload;
    function AddPathParam(const AName: string; AValue: Int64): IRESTClient; overload;
    function AddPathParam(const AName: string; AValue: TGUID): IRESTClient; overload;
    function AddPathParam(const AName: string; AValue: TDateTime): IRESTClient; overload;
    function AddPathParam(const AName: string; AValue: TDate): IRESTClient; overload;
    function AddPathParam(const AName: string; AValue: TTime): IRESTClient; overload;
    function AddPathParam(const AName: string; AValue: Double): IRESTClient; overload;
    function ClearPathParams: IRESTClient;

    function AddQueryStringParam(const AName, AValue: string): IRESTClient; overload;
    function AddQueryStringParam(const AName: string; AValue: Integer): IRESTClient; overload;
    function AddQueryStringParam(const AName: string; AValue: Int64): IRESTClient; overload;
    function AddQueryStringParam(const AName: string; AValue: TGUID): IRESTClient; overload;
    function AddQueryStringParam(const AName: string; AValue: TDateTime): IRESTClient; overload;
    function AddQueryStringParam(const AName: string; AValue: TDate): IRESTClient; overload;
    function AddQueryStringParam(const AName: string; AValue: TTime): IRESTClient; overload;
    function AddQueryStringParam(const AName: string; AValue: Double): IRESTClient; overload;
    function ClearQueryParams: IRESTClient;

    function Accept: string; overload;
    function Accept(const AAccept: string): IRESTClient; overload;
    function AcceptCharset: string; overload;
    function AcceptCharset(const AAcceptCharset: string): IRESTClient; overload;
    function AcceptEncoding: string; overload;
    function AcceptEncoding(const AAcceptEncoding: string): IRESTClient; overload;
  end;

  TRESTResponse = class(TInterfacedObject, IRESTResponse)
  private
    fSuccess: Boolean;
    fStatusCode: Integer;
    fStatusText: string;
    fHeaders: TStrings;
    fContentType: string;
    fContentEncoding: string;
    fContentLength: Integer;
    fContent: string;
    fRawBytes: TBytes;
  public
    constructor Create;
    destructor Destroy; override;

    function Success: Boolean; overload;
    function Success(const ASuccess: Boolean): IRESTResponse; overload;

    function StatusCode: Integer; overload;
    function StatusCode(const AStatusCode: Integer): IRESTResponse; overload;
    function StatusText: string; overload;
    function StatusText(const AStatusText: string): IRESTResponse; overload;

    function Headers: TStrings; overload;
    function Headers(const AHeaders: TStrings): IRESTResponse; overload;

    function ContentType: string; overload;
    function ContentType(const AContentType: string): IRESTResponse; overload;
    function ContentEncoding: string; overload;
    function ContentEncoding(const AContentEncoding: string): IRESTResponse; overload;
    function ContentLength: Integer; overload;
    function ContentLength(const AContentLength: Integer): IRESTResponse; overload;

    function Content: string; overload;
    function Content(const AContent: string): IRESTResponse; overload;
    function RawBytes: TBytes; overload;
    function RawBytes(const ARawBytes: TBytes): IRESTResponse; overload;
  end;

implementation

uses
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.JsonDataObjects,
  System.NetEncoding;

{ TRESTClient }

function TRESTClient.AddHeader(const AName: string; AValue: TDateTime): IRESTClient;
begin
  Result := AddHeader(AName, DateTimeToISOTimeStamp(AValue));
end;

function TRESTClient.AddHeader(const AName: string; AValue: TDate): IRESTClient;
begin
  Result := AddHeader(AName, DateToISODate(AValue));
end;

function TRESTClient.AddHeader(const AName: string; AValue: TTime): IRESTClient;
begin
  Result := AddHeader(AName, TimeToISOTime(AValue));
end;

function TRESTClient.Accept(const AAccept: string): IRESTClient;
begin
  Result := Self;
  fRESTRequest.Accept := AAccept;
end;

function TRESTClient.Accept: string;
begin
  Result := fRESTRequest.Accept;
end;

function TRESTClient.AcceptCharset: string;
begin
  Result := fRESTRequest.AcceptCharset;
end;

function TRESTClient.AcceptCharset(const AAcceptCharset: string): IRESTClient;
begin
  Result := Self;
  fRESTRequest.AcceptCharset := AAcceptCharset;
end;

function TRESTClient.AcceptEncoding: string;
begin
  Result := fRESTRequest.AcceptEncoding;
end;

function TRESTClient.AcceptEncoding(const AAcceptEncoding: string): IRESTClient;
begin
  Result := Self;
  fRESTRequest.AcceptEncoding := AAcceptEncoding;
end;

function TRESTClient.AddHeader(const AName: string; AValue: Double): IRESTClient;
begin
  Result := AddHeader(AName, AValue.ToString);
end;

function TRESTClient.AddHeader(const AName, AValue: string): IRESTClient;
begin
  Result := Self;
  fRESTRequest.AddParameter(AName, AValue, TRESTRequestParameterKind.pkHTTPHEADER);
end;

function TRESTClient.AddHeader(const AName: string; AValue: Integer): IRESTClient;
begin
  Result := AddHeader(AName, AValue.ToString);
end;

function TRESTClient.AddHeader(const AName: string; AValue: Int64): IRESTClient;
begin
  Result := AddHeader(AName, AValue.ToString);
end;

function TRESTClient.AddHeader(const AName: string; AValue: TGUID): IRESTClient;
begin
  Result := AddHeader(AName, AValue.ToString);
end;

function TRESTClient.AddPathParam(const AName: string; AValue: TGUID): IRESTClient;
begin
  Result := AddPathParam(AName, AValue.ToString);
end;

function TRESTClient.AddPathParam(const AName: string; AValue: Int64): IRESTClient;
begin
  Result := AddPathParam(AName, AValue.ToString);
end;

function TRESTClient.AddPathParam(const AName: string; AValue: Integer): IRESTClient;
begin
  Result := AddPathParam(AName, AValue.ToString);
end;

function TRESTClient.AddPathParam(const AName, AValue: string): IRESTClient;
begin
  Result := Self;
  fRESTRequest.AddParameter(AName, AValue);
end;

function TRESTClient.AddPathParam(const AName: string; AValue: Double): IRESTClient;
begin
  Result := AddPathParam(AName, AValue.ToString);
end;

function TRESTClient.AddPathParam(const AName: string; AValue: TTime): IRESTClient;
begin
  Result := AddPathParam(AName, TimeToISOTime(AValue));
end;

function TRESTClient.AddPathParam(const AName: string; AValue: TDateTime): IRESTClient;
begin
  Result := AddPathParam(AName, DateTimeToISOTimeStamp(AValue));
end;

function TRESTClient.AddPathParam(const AName: string; AValue: TDate): IRESTClient;
begin
  Result := AddPathParam(AName, DateToISODate(AValue));
end;

function TRESTClient.AddQueryStringParam(const AName: string; AValue: TDate): IRESTClient;
begin
  Result := AddQueryStringParam(AName, DateToISODate(AValue));
end;

function TRESTClient.AddQueryStringParam(const AName: string; AValue: TDateTime): IRESTClient;
begin
  Result := AddQueryStringParam(AName, DateTimeToISOTimeStamp(AValue));
end;

function TRESTClient.AddQueryStringParam(const AName: string; AValue: TTime): IRESTClient;
begin
  Result := AddQueryStringParam(AName, TimeToISOTime(AValue));
end;

function TRESTClient.AddQueryStringParam(const AName: string; AValue: Double): IRESTClient;
begin
  Result := AddQueryStringParam(AName, AValue.ToString);
end;

function TRESTClient.AddQueryStringParam(const AName, AValue: string): IRESTClient;
begin
  Result := Self;
  fRESTRequest.AddParameter(AName, AValue);
end;

function TRESTClient.AddQueryStringParam(const AName: string; AValue: Integer): IRESTClient;
begin
  Result := AddQueryStringParam(AName, AValue.ToString);
end;

function TRESTClient.AddQueryStringParam(const AName: string; AValue: TGUID): IRESTClient;
begin
  Result := AddQueryStringParam(AName, AValue.ToString);
end;

function TRESTClient.AddQueryStringParam(const AName: string; AValue: Int64): IRESTClient;
begin
  Result := AddQueryStringParam(AName, AValue.ToString);
end;

function TRESTClient.BaseURL: string;
begin
  Result := fRESTClient.BaseURL;
end;

function TRESTClient.BaseURL(const ABaseURL: string): IRESTClient;
begin
  Result := Self;
  fRESTClient.BaseURL := ABaseURL;
end;

function TRESTClient.ClearHeaders: IRESTClient;
var
  I: Integer;
begin
  Result := Self;
  for I := Pred(fRESTRequest.Params.Count) downto 0 do
  begin
    if (fRESTRequest.Params[I].Kind = TRESTRequestParameterKind.pkHTTPHEADER) then
      fRESTRequest.Params.Delete(I);
  end;
end;

function TRESTClient.ClearPathParams: IRESTClient;
var
  I: Integer;
begin
  Result := Self;
  for I := Pred(fRESTRequest.Params.Count) downto 0 do
  begin
    if (fRESTRequest.Params[I].Kind = TRESTRequestParameterKind.pkURLSEGMENT) then
      fRESTRequest.Params.Delete(I);
  end;
end;

function TRESTClient.ClearQueryParams: IRESTClient;
var
  I: Integer;
begin
  Result := Self;
  for I := Pred(fRESTRequest.Params.Count) downto 0 do
  begin
    if (fRESTRequest.Params[I].Kind = TRESTRequestParameterKind.pkQUERY) then
      fRESTRequest.Params.Delete(I);
  end;
end;

constructor TRESTClient.Create;
begin
  inherited Create;

  fRESTClient := TCustomRESTClient.Create(nil);
  fRESTRequest := TCustomRESTRequest.Create(nil);
  fRESTResponse := TCustomRESTResponse.Create(nil);

  fRESTClient.HandleRedirects := True;
  fRESTClient.RaiseExceptionOn500 := False;

  fRESTRequest.Client := fRESTClient;
  fRESTRequest.Response := fRESTResponse;
  fRESTRequest.SynchronizedEvents := False;
  fRESTRequest.AutoCreateParams := False;

  fSerializer := TMVCJsonDataObjectsSerializer.Create;
end;

destructor TRESTClient.Destroy;
begin
  fRESTResponse.Free;
  fRESTRequest.Free;
  fRESTClient.Free;
  fSerializer := nil;

  inherited Destroy;
end;

function TRESTClient.ProxyPassword(const AProxyPassword: string): IRESTClient;
begin
  Result := Self;
  fRESTClient.ProxyPassword := AProxyPassword;
end;

function TRESTClient.ProxyPassword: string;
begin
  Result := fRESTClient.ProxyPassword;
end;

function TRESTClient.ProxyPort: Integer;
begin
  Result := fRESTClient.ProxyPort;
end;

function TRESTClient.ProxyPort(const AProxyPort: Integer): IRESTClient;
begin
  Result := Self;
  fRESTClient.ProxyPort := AProxyPort;
end;

function TRESTClient.ProxyServer(const AProxyServer: string): IRESTClient;
begin
  Result := Self;
  fRESTClient.ProxyServer := AProxyServer;
end;

function TRESTClient.ProxyServer: string;
begin
  Result := fRESTClient.ProxyServer;
end;

function TRESTClient.ProxyUsername: string;
begin
  Result := fRESTClient.ProxyUsername;
end;

function TRESTClient.ProxyUsername(const AProxyUsername: string): IRESTClient;
begin
  Result := Self;
  fRESTClient.ProxyUsername := AProxyUsername;
end;

function TRESTClient.ReadTimeout: Integer;
begin
  Result := fRESTRequest.Timeout;
end;

function TRESTClient.ReadTimeout(const AReadTimeout: Integer): IRESTClient;
begin
  Result := Self;
  fRESTRequest.Timeout := AReadTimeout;
end;

function TRESTClient.SetBasicAuthorization(const AUsername, APassword: string): IRESTClient;
var
  LBase64: TNetEncoding;
  LAuthValue: string;
begin
  Result := Self;
  // Do not use TNetEncoding.Base64 here, because it may break long line
  LBase64 := TBase64Encoding.Create(0, '');
  try
    LAuthValue := 'Basic ' + LBase64.Encode(AUsername + ':' + APassword); // do not translate
  finally
    LBase64.Free;
  end;
  fRESTRequest.AddParameter('Authorization', LAuthValue, TRESTRequestParameterKind.pkHTTPHEADER,
    [TRESTRequestParameterOption.poDoNotEncode]);
end;

function TRESTClient.SetBearerAuthorization(const AToken: string): IRESTClient;
begin
  Result := Self;
  fRESTRequest.AddParameter('Authorization', 'Bearer ' + AToken, TRESTRequestParameterKind.pkHTTPHEADER,
    [TRESTRequestParameterOption.poDoNotEncode]);
end;

{ TRESTResponse }

function TRESTResponse.Content(const AContent: string): IRESTResponse;
begin
  Result := Self;
  fContent := AContent;
end;

function TRESTResponse.Content: string;
begin
  Result := fContent;
end;

function TRESTResponse.ContentEncoding(const AContentEncoding: string): IRESTResponse;
begin
  Result := Self;
  fContentEncoding := AContentEncoding;
end;

function TRESTResponse.ContentEncoding: string;
begin
  Result := fContentEncoding;
end;

function TRESTResponse.ContentLength(const AContentLength: Integer): IRESTResponse;
begin
  Result := Self;
  fContentLength := AContentLength;
end;

function TRESTResponse.ContentLength: Integer;
begin
  Result := fContentLength;
end;

function TRESTResponse.ContentType: string;
begin
  Result := fContentType;
end;

function TRESTResponse.ContentType(const AContentType: string): IRESTResponse;
begin
  Result := Self;
  fContentType := AContentType;
end;

constructor TRESTResponse.Create;
begin
  inherited Create;
  fHeaders := TStringList.Create;
  SetLength(fRawBytes, 0);
end;

destructor TRESTResponse.Destroy;
begin
  SetLength(fRawBytes, 0);
  fHeaders.Free;
  inherited Destroy;
end;

function TRESTResponse.Headers(const AHeaders: TStrings): IRESTResponse;
begin
  Result := Self;
  fHeaders.Assign(AHeaders);
end;

function TRESTResponse.Headers: TStrings;
begin
  Result := fHeaders;
end;

function TRESTResponse.RawBytes: TBytes;
begin
  Result := fRawBytes;
end;

function TRESTResponse.RawBytes(const ARawBytes: TBytes): IRESTResponse;
begin
  Result := Self;
  fRawBytes := ARawBytes;
end;

function TRESTResponse.StatusCode(const AStatusCode: Integer): IRESTResponse;
begin
  Result := Self;
  fStatusCode := AStatusCode;
end;

function TRESTResponse.StatusCode: Integer;
begin
  Result := fStatusCode;
end;

function TRESTResponse.StatusText: string;
begin
  Result := fStatusText;
end;

function TRESTResponse.StatusText(const AStatusText: string): IRESTResponse;
begin
  Result := Self;
  fStatusText := AStatusText;
end;

function TRESTResponse.Success: Boolean;
begin
  Result := fSuccess;
end;

function TRESTResponse.Success(const ASuccess: Boolean): IRESTResponse;
begin
  Result := Self;
  fSuccess := ASuccess;
end;

end.
