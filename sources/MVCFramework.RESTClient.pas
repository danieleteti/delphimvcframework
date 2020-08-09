unit MVCFramework.RESTClient;

interface

uses
  System.SysUtils,
  System.Classes,
  REST.Client,
  REST.Types,
  MVCFramework.RESTClient.Intf,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Commons,
  Data.DB,
  System.Rtti;

type
  TRESTClient = class(TInterfacedObject, IRESTClient)
  private
    fRttiContext: TRttiContext;
    fRESTClient: TCustomRESTClient;
    fRESTRequest: TCustomRESTRequest;
    fRESTResponse: TCustomRESTResponse;
    fSerializer: IMVCSerializer;
    procedure ClearRESTParams(const aRESTParamKind: TRESTRequestParameterKind);
    function ConvertMVCPathParamsToRESTParams(const aResource: string): string;
    function ExecuteRESTRequest(const aMethod: TRESTRequestMethod): IRESTResponse;
    function ObjectIsList(aObject: TObject): Boolean;
    function SerializeObject(aObject: TObject): string;
  public
    constructor Create;
    destructor Destroy; override;

    function BaseURL: string; overload;
    function BaseURL(const aBaseURL: string): IRESTClient; overload;
    function Timeout: Integer; overload;
    function Timeout(const aTimeout: Integer): IRESTClient; overload;
    function RaiseExceptionOn500: Boolean; overload;
    function RaiseExceptionOn500(const aRaiseExceptionOn500: Boolean): IRESTClient; overload;

    function ProxyServer: string; overload;
    function ProxyServer(const aProxyServer: string): IRESTClient; overload;
    function ProxyPort: Integer; overload;
    function ProxyPort(const aProxyPort: Integer): IRESTClient; overload;
    function ProxyUsername: string; overload;
    function ProxyUsername(const aProxyUsername: string): IRESTClient; overload;
    function ProxyPassword: string; overload;
    function ProxyPassword(const aProxyPassword: string): IRESTClient; overload;

    function SetBasicAuthorization(const aUsername, aPassword: string): IRESTClient;
    function SetBearerAuthorization(const aToken: string): IRESTClient;

    function AddHeader(const aName, aValue: string): IRESTClient; overload;
    function ClearHeaders: IRESTClient;

    function AddCookie(const aName, aValue: string): IRESTClient;
    function ClearCookies: IRESTClient;

    function AddPathParam(const aName, aValue: string): IRESTClient; overload;
    function AddPathParam(const aName: string; aValue: Integer): IRESTClient; overload;
    function AddPathParam(const aName: string; aValue: Int64): IRESTClient; overload;
    function AddPathParam(const aName: string; aValue: TGUID): IRESTClient; overload;
    function AddPathParam(const aName: string; aValue: TDateTime): IRESTClient; overload;
    function AddPathParam(const aName: string; aValue: TDate): IRESTClient; overload;
    function AddPathParam(const aName: string; aValue: TTime): IRESTClient; overload;
    function AddPathParam(const aName: string; aValue: Double): IRESTClient; overload;
    function ClearPathParams: IRESTClient;

    function AddQueryStringParam(const aName, aValue: string): IRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: Integer): IRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: Int64): IRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: TGUID): IRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: TDateTime): IRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: TDate): IRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: TTime): IRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: Double): IRESTClient; overload;
    function ClearQueryParams: IRESTClient;

    function Accept: string; overload;
    function Accept(const aAccept: string): IRESTClient; overload;
    function AcceptCharset: string; overload;
    function AcceptCharset(const aAcceptCharset: string): IRESTClient; overload;
    function AcceptEncoding: string; overload;
    function AcceptEncoding(const aAcceptEncoding: string): IRESTClient; overload;

    function Resource: string; overload;
    function Resource(const aResource: string): IRESTClient; overload;

    function AddBody(const aBody: string;
      const aContentType: TRESTContentType = ctAPPLICATION_JSON): IRESTClient; overload;
    function AddBody(aBody: TStream; const aContentType: TRESTContentType = ctNone;
      aOwnsStream: Boolean = True): IRESTClient; overload;
    function AddBody(aBody: TObject; const aOwnsObject: Boolean = True): IRESTClient; overload;
    function ClearBody: IRESTClient;

    function AddFile(const aName, aFileName: string;
      const aContentType: TRESTContentType = ctNone): IRESTClient; overload;
    function AddFile(const aFileName: string; const aContentType: TRESTContentType = ctNone): IRESTClient; overload;
    function ClearFiles: IRESTClient;

    function Get: IRESTResponse; overload;
    function Get(const aResource: string): IRESTResponse; overload;

    function Post: IRESTResponse; overload;
    function Post(const aResource: string; const aBody: string = ''): IRESTResponse; overload;
    function Post(const aResource: string; aBody: TObject; const aOwnsBody: Boolean = True): IRESTResponse; overload;

    function Patch: IRESTResponse; overload;
    function Patch(const aResource: string; const aBody: string = ''): IRESTResponse; overload;
    function Patch(const aResource: string; aBody: TObject; const aOwnsBody: Boolean = True): IRESTResponse; overload;

    function Put: IRESTResponse; overload;
    function Put(const aResource: string; const aBody: string = ''): IRESTResponse; overload;
    function Put(const aResource: string; aBody: TObject; const aOwnsBody: Boolean = True): IRESTResponse; overload;

    function Delete: IRESTResponse; overload;
    function Delete(const aResource: string): IRESTResponse; overload;

    function DataSetInsert(const aResource: string; aDataSet: TDataSet; const aIgnoredFields: TMVCIgnoredList = [];
      const aNameCase: TMVCNameCase = ncAsIs): IRESTResponse;
    function DataSetUpdate(const aResource: string; aDataSet: TDataSet; const aIgnoredFields: TMVCIgnoredList = [];
      const aNameCase: TMVCNameCase = ncAsIs): IRESTResponse;
    function DataSetDelete(const aResource: string): IRESTResponse;

    function Serializer: IMVCSerializer;
  end;

  TRESTResponse = class(TInterfacedObject, IRESTResponse)
  private
    fSuccess: Boolean;
    fStatusCode: Integer;
    fStatusText: string;
    fErrorMessage: string;
    fHeaders: TStrings;
    fServer: string;
    fFullRequestURI: string;
    fContentType: string;
    fContentEncoding: string;
    fContentLength: Integer;
    fContent: string;
    fRawBytes: TBytes;
    procedure FillRESTResponse(aRESTResponse: TCustomRESTResponse);
  public
    constructor Create(aRESTResponse: TCustomRESTResponse);
    destructor Destroy; override;

    function Success: Boolean;
    function StatusCode: Integer;
    function StatusText: string;
    function ErrorMessage: string;
    function Headers: TStrings;
    function HeaderByName(const aName: string): string;
    function Server: string;
    function FullRequestURI: string;
    function ContentType: string;
    function ContentEncoding: string;
    function ContentLength: Integer;
    function Content: string;
    function RawBytes: TBytes;
    procedure SaveContentToStream(aStream: TStream);
    procedure SaveContentToFile(const aFileName: string);
  end;

  ERESTClientException = class(Exception);

implementation

uses
  MVCFramework.Serializer.JsonDataObjects,
  System.NetEncoding,
  System.RegularExpressions;

{ TRESTClient }

function TRESTClient.Accept(const aAccept: string): IRESTClient;
begin
  Result := Self;
  fRESTRequest.Accept := aAccept;
end;

function TRESTClient.Accept: string;
begin
  Result := fRESTRequest.Accept;
end;

function TRESTClient.AcceptCharset: string;
begin
  Result := fRESTRequest.AcceptCharset;
end;

function TRESTClient.AcceptCharset(const aAcceptCharset: string): IRESTClient;
begin
  Result := Self;
  fRESTRequest.AcceptCharset := aAcceptCharset;
end;

function TRESTClient.AcceptEncoding: string;
begin
  Result := fRESTRequest.AcceptEncoding;
end;

function TRESTClient.AcceptEncoding(const aAcceptEncoding: string): IRESTClient;
begin
  Result := Self;
  fRESTRequest.AcceptEncoding := aAcceptEncoding;
end;

function TRESTClient.AddBody(const aBody: string; const aContentType: TRESTContentType): IRESTClient;
begin
  Result := Self;
  fRESTRequest.AddBody(aBody, aContentType);
end;

function TRESTClient.AddBody(aBody: TStream; const aContentType: TRESTContentType;
  aOwnsStream: Boolean): IRESTClient;
var
  lOwnsStream: TRESTObjectOwnership;
begin
  if aBody = nil then
    raise ERESTClientException.Create('You need a valid body!');

  Result := Self;

  if aOwnsStream then
    lOwnsStream := TRESTObjectOwnership.ooREST
  else
    lOwnsStream := TRESTObjectOwnership.ooApp;

  fRESTRequest.AddBody(aBody, aContentType, lOwnsStream);
end;

function TRESTClient.AddBody(aBody: TObject; const aOwnsObject: Boolean): IRESTClient;
begin
  if aBody = nil then
    raise ERESTClientException.Create('You need a valid body!');

  Result := Self;

  AddBody(SerializeObject(aBody), TRESTContentType.ctAPPLICATION_JSON);

  if aOwnsObject then
    aBody.Free;
end;

function TRESTClient.AddCookie(const aName, aValue: string): IRESTClient;
begin
  Result := Self;
  fRESTRequest.AddParameter(aName, aValue, TRESTRequestParameterKind.pkCOOKIE);
end;

function TRESTClient.AddFile(const aName, aFileName: string; const aContentType: TRESTContentType): IRESTClient;
begin
  Result := Self;
  fRESTRequest.AddFile(aName, aFileName, aContentType);
end;

function TRESTClient.AddFile(const aFileName: string; const aContentType: TRESTContentType): IRESTClient;
begin
  Result := AddFile('file', aFileName, aContentType);
end;

function TRESTClient.AddHeader(const aName, aValue: string): IRESTClient;
begin
  Result := Self;
  fRESTRequest.AddParameter(aName, aValue, TRESTRequestParameterKind.pkHTTPHEADER);
end;

function TRESTClient.AddPathParam(const aName: string; aValue: TGUID): IRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TRESTClient.AddPathParam(const aName: string; aValue: Int64): IRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TRESTClient.AddPathParam(const aName: string; aValue: Integer): IRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TRESTClient.AddPathParam(const aName, aValue: string): IRESTClient;
begin
  Result := Self;
  fRESTRequest.AddParameter(aName, aValue);
end;

function TRESTClient.AddPathParam(const aName: string; aValue: Double): IRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TRESTClient.AddPathParam(const aName: string; aValue: TTime): IRESTClient;
begin
  Result := AddPathParam(aName, TimeToISOTime(aValue));
end;

function TRESTClient.AddPathParam(const aName: string; aValue: TDateTime): IRESTClient;
begin
  Result := AddPathParam(aName, DateTimeToISOTimeStamp(aValue));
end;

function TRESTClient.AddPathParam(const aName: string; aValue: TDate): IRESTClient;
begin
  Result := AddPathParam(aName, DateToISODate(aValue));
end;

function TRESTClient.AddQueryStringParam(const aName: string; aValue: TDate): IRESTClient;
begin
  Result := AddQueryStringParam(aName, DateToISODate(aValue));
end;

function TRESTClient.AddQueryStringParam(const aName: string; aValue: TDateTime): IRESTClient;
begin
  Result := AddQueryStringParam(aName, DateTimeToISOTimeStamp(aValue));
end;

function TRESTClient.AddQueryStringParam(const aName: string; aValue: TTime): IRESTClient;
begin
  Result := AddQueryStringParam(aName, TimeToISOTime(aValue));
end;

function TRESTClient.AddQueryStringParam(const aName: string; aValue: Double): IRESTClient;
begin
  Result := AddQueryStringParam(aName, aValue.ToString);
end;

function TRESTClient.AddQueryStringParam(const aName, aValue: string): IRESTClient;
begin
  Result := Self;
  fRESTRequest.AddParameter(aName, aValue);
end;

function TRESTClient.AddQueryStringParam(const aName: string; aValue: Integer): IRESTClient;
begin
  Result := AddQueryStringParam(aName, aValue.ToString);
end;

function TRESTClient.AddQueryStringParam(const aName: string; aValue: TGUID): IRESTClient;
begin
  Result := AddQueryStringParam(aName, aValue.ToString);
end;

function TRESTClient.AddQueryStringParam(const aName: string; aValue: Int64): IRESTClient;
begin
  Result := AddQueryStringParam(aName, aValue.ToString);
end;

function TRESTClient.BaseURL: string;
begin
  Result := fRESTClient.BaseURL;
end;

function TRESTClient.BaseURL(const aBaseURL: string): IRESTClient;
begin
  Result := Self;
  fRESTClient.BaseURL := aBaseURL;
end;

function TRESTClient.ClearBody: IRESTClient;
begin
  Result := Self;
  fRESTRequest.ClearBody;
end;

function TRESTClient.ClearCookies: IRESTClient;
begin
  Result := Self;
  ClearRESTParams(TRESTRequestParameterKind.pkCOOKIE);
end;

function TRESTClient.ClearFiles: IRESTClient;
begin
  Result := Self;
  ClearRESTParams(TRESTRequestParameterKind.pkFILE);
end;

function TRESTClient.ClearHeaders: IRESTClient;
begin
  Result := Self;
  ClearRESTParams(TRESTRequestParameterKind.pkHTTPHEADER);
end;

function TRESTClient.ClearPathParams: IRESTClient;
begin
  Result := Self;
  ClearRESTParams(TRESTRequestParameterKind.pkURLSEGMENT);
end;

function TRESTClient.ClearQueryParams: IRESTClient;
begin
  Result := Self;
  ClearRESTParams(TRESTRequestParameterKind.pkQUERY);
end;

procedure TRESTClient.ClearRESTParams(const aRESTParamKind: TRESTRequestParameterKind);
var
  I: Integer;
begin
  for I := Pred(fRESTRequest.Params.Count) downto 0 do
  begin
    if (fRESTRequest.Params[I].Kind = aRESTParamKind) then
      fRESTRequest.Params.Delete(I);
  end;
end;

function TRESTClient.ConvertMVCPathParamsToRESTParams(const aResource: string): string;
begin
  Result := TRegEx.Replace(aResource, '(\([($])([\w_]+)([)])', '{\2}', [roIgnoreCase]);
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
  fRttiContext := TRttiContext.Create;
end;

function TRESTClient.DataSetDelete(const aResource: string): IRESTResponse;
begin
  Result := Delete(aResource);
end;

function TRESTClient.DataSetInsert(const aResource: string; aDataSet: TDataSet; const aIgnoredFields: TMVCIgnoredList;
  const aNameCase: TMVCNameCase): IRESTResponse;
begin
  Result := Post(aResource, fSerializer.SerializeDataSetRecord(aDataSet, aIgnoredFields, aNameCase));
end;

function TRESTClient.DataSetUpdate(const aResource: string; aDataSet: TDataSet; const aIgnoredFields: TMVCIgnoredList;
  const aNameCase: TMVCNameCase): IRESTResponse;
begin
  Result := Put(aResource, fSerializer.SerializeDataSetRecord(aDataSet, aIgnoredFields, aNameCase));
end;

function TRESTClient.Delete: IRESTResponse;
begin
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmDELETE);
end;

function TRESTClient.Delete(const aResource: string): IRESTResponse;
begin
  Resource(aResource);
  Result := Delete;
end;

destructor TRESTClient.Destroy;
begin
  fRESTResponse.Free;
  fRESTRequest.Free;
  fRESTClient.Free;
  fSerializer := nil;
  fRttiContext.Free;

  inherited Destroy;
end;

function TRESTClient.ExecuteRESTRequest(const aMethod: TRESTRequestMethod): IRESTResponse;
begin
  fRESTRequest.Method := aMethod;

  fRESTRequest.Execute;

  Result := TRESTResponse.Create(fRESTResponse);
end;

function TRESTClient.Get(const aResource: string): IRESTResponse;
begin
  Resource(aResource);
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmGET);
end;

function TRESTClient.ObjectIsList(aObject: TObject): Boolean;
begin
  Result := fRttiContext.GetType(aObject.ClassType).GetMethod('GetEnumerator') <> nil;
end;

function TRESTClient.Get: IRESTResponse;
begin
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmGET);
end;

function TRESTClient.Patch(const aResource, aBody: string): IRESTResponse;
begin
  Resource(aResource);
  if not aBody.isEmpty then
  begin
    ClearBody;
    AddBody(aBody, TRESTContentType.ctAPPLICATION_JSON);
  end;

  Result := Patch;
end;

function TRESTClient.Patch: IRESTResponse;
begin
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmPATCH);
end;

function TRESTClient.Patch(const aResource: string; aBody: TObject; const aOwnsBody: Boolean): IRESTResponse;
begin
  if aBody = nil then
    raise ERESTClientException.Create('You need a valid body!');

  Result := Patch(aResource, SerializeObject(aBody));

  if aOwnsBody then
    aBody.Free;
end;

function TRESTClient.Post(const aResource: string; aBody: TObject; const aOwnsBody: Boolean): IRESTResponse;
begin
  if aBody = nil then
    raise ERESTClientException.Create('You need a valid body!');

  Result := Post(aResource, SerializeObject(aBody));

  if aOwnsBody then
    aBody.Free;
end;

function TRESTClient.Post(const aResource, aBody: string): IRESTResponse;
begin
  Resource(aResource);
  if not aBody.IsEmpty then
  begin
    ClearBody;
    AddBody(aBody, TRESTContentType.ctAPPLICATION_JSON);
  end;
  Result := Post;
end;

function TRESTClient.Post: IRESTResponse;
begin
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmPOST);
end;

function TRESTClient.ProxyPassword(const aProxyPassword: string): IRESTClient;
begin
  Result := Self;
  fRESTClient.ProxyPassword := aProxyPassword;
end;

function TRESTClient.ProxyPassword: string;
begin
  Result := fRESTClient.ProxyPassword;
end;

function TRESTClient.ProxyPort: Integer;
begin
  Result := fRESTClient.ProxyPort;
end;

function TRESTClient.ProxyPort(const aProxyPort: Integer): IRESTClient;
begin
  Result := Self;
  fRESTClient.ProxyPort := aProxyPort;
end;

function TRESTClient.ProxyServer(const aProxyServer: string): IRESTClient;
begin
  Result := Self;
  fRESTClient.ProxyServer := aProxyServer;
end;

function TRESTClient.ProxyServer: string;
begin
  Result := fRESTClient.ProxyServer;
end;

function TRESTClient.ProxyUsername: string;
begin
  Result := fRESTClient.ProxyUsername;
end;

function TRESTClient.ProxyUsername(const aProxyUsername: string): IRESTClient;
begin
  Result := Self;
  fRESTClient.ProxyUsername := aProxyUsername;
end;

function TRESTClient.Put(const aResource: string; aBody: TObject; const aOwnsBody: Boolean): IRESTResponse;
begin
  if aBody = nil then
    raise ERESTClientException.Create('You need a valid body!');

  Result := Put(aResource, SerializeObject(aBody));

  if aOwnsBody then
    aBody.Free;
end;

function TRESTClient.Put(const aResource, aBody: string): IRESTResponse;
begin
  Resource(aResource);
  if not aBody.IsEmpty then
  begin
    ClearBody;
    AddBody(aBody, TRESTContentType.ctAPPLICATION_JSON);
  end;
  Result := Put;
end;

function TRESTClient.Put: IRESTResponse;
begin
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmPUT);
end;

function TRESTClient.RaiseExceptionOn500(const aRaiseExceptionOn500: Boolean): IRESTClient;
begin
  Result := Self;
  fRESTClient.RaiseExceptionOn500 := aRaiseExceptionOn500;
end;

function TRESTClient.Resource: string;
begin
  Result := fRESTRequest.Resource;
end;

function TRESTClient.Resource(const aResource: string): IRESTClient;
begin
  Result := Self;
  fRESTRequest.Resource := ConvertMVCPathParamsToRESTParams(aResource);
end;

function TRESTClient.RaiseExceptionOn500: Boolean;
begin
  Result := fRESTClient.RaiseExceptionOn500;
end;

function TRESTClient.Timeout: Integer;
begin
  Result := fRESTRequest.Timeout;
end;

function TRESTClient.Timeout(const aTimeout: Integer): IRESTClient;
begin
  Result := Self;
  fRESTRequest.Timeout := aTimeout;
end;

function TRESTClient.SerializeObject(aObject: TObject): string;
begin
  if ObjectIsList(aObject) then
    Result := fSerializer.SerializeCollection(aObject)
  else
    Result := fSerializer.SerializeObject(aObject);
end;

function TRESTClient.Serializer: IMVCSerializer;
begin
  Result := fSerializer;
end;

function TRESTClient.SetBasicAuthorization(const aUsername, aPassword: string): IRESTClient;
var
  LBase64: TNetEncoding;
  LAuthValue: string;
begin
  Result := Self;
  // Do not use TNetEncoding.Base64 here, because it may break long line
  LBase64 := TBase64Encoding.Create(0, '');
  try
    LAuthValue := 'Basic ' + LBase64.Encode(aUsername + ':' + aPassword);
  finally
    LBase64.Free;
  end;
  fRESTRequest.AddParameter('Authorization', LAuthValue, TRESTRequestParameterKind.pkHTTPHEADER,
    [TRESTRequestParameterOption.poDoNotEncode]);
end;

function TRESTClient.SetBearerAuthorization(const aToken: string): IRESTClient;
begin
  Result := Self;
  fRESTRequest.AddParameter('Authorization', 'Bearer ' + aToken, TRESTRequestParameterKind.pkHTTPHEADER,
    [TRESTRequestParameterOption.poDoNotEncode]);
end;

{ TRESTResponse }

function TRESTResponse.Content: string;
begin
  Result := fContent;
end;

function TRESTResponse.ContentEncoding: string;
begin
  Result := fContentEncoding;
end;

function TRESTResponse.ContentLength: Integer;
begin
  Result := fContentLength;
end;

function TRESTResponse.ContentType: string;
begin
  Result := fContentType;
end;

constructor TRESTResponse.Create(aRESTResponse: TCustomRESTResponse);
begin
  inherited Create;
  fHeaders := TStringList.Create;
  SetLength(fRawBytes, 0);

  FillRESTResponse(aRESTResponse);
end;

destructor TRESTResponse.Destroy;
begin
  SetLength(fRawBytes, 0);
  fHeaders.Free;
  inherited Destroy;
end;

function TRESTResponse.ErrorMessage: string;
begin
  Result := fErrorMessage;
end;

procedure TRESTResponse.FillRESTResponse(aRESTResponse: TCustomRESTResponse);
begin
  fSuccess := aRESTResponse.Status.Success;
  fStatusCode := aRESTResponse.StatusCode;
  fStatusText := aRESTResponse.StatusText;
  fErrorMessage := aRESTResponse.ErrorMessage;
  fHeaders := aRESTResponse.Headers;
  fServer := aRESTResponse.Server;
  fFullRequestURI := aRESTResponse.FullRequestURI;
  fContent := aRESTResponse.Content;
  fRawBytes := aRESTResponse.RawBytes;
  fContentType := aRESTResponse.ContentType;
  fContentEncoding := aRESTResponse.ContentEncoding;
  fContentLength := aRESTResponse.ContentLength;
end;

function TRESTResponse.FullRequestURI: string;
begin
  Result := fFullRequestURI;
end;

function TRESTResponse.HeaderByName(const aName: string): string;
begin
  Result := fHeaders.Values[aName];
end;

function TRESTResponse.Headers: TStrings;
begin
  Result := fHeaders;
end;

function TRESTResponse.RawBytes: TBytes;
begin
  Result := fRawBytes;
end;

procedure TRESTResponse.SaveContentToFile(const aFileName: string);
var
  lStream: TMemoryStream;
begin
  lStream := TMemoryStream.Create;
  try
    lStream.Write(fRawBytes, Length(fRawBytes));
    lStream.Position := 0;
    lStream.SaveToFile(aFileName);
  finally
    lStream.Free;
  end;
end;

procedure TRESTResponse.SaveContentToStream(aStream: TStream);
begin
  if aStream = nil then
    raise ERESTClientException.Create('Stream not assigned!');

  aStream.Write(fRawBytes, Length(fRawBytes));
end;

function TRESTResponse.Server: string;
begin
  Result := fServer;
end;

function TRESTResponse.StatusCode: Integer;
begin
  Result := fStatusCode;
end;

function TRESTResponse.StatusText: string;
begin
  Result := fStatusText;
end;

function TRESTResponse.Success: Boolean;
begin
  Result := fSuccess;
end;

end.
