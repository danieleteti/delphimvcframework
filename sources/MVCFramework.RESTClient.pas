// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file:
// João Antônio Duarte (https://github.com/joaoduarte19)
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

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  REST.Client,
  REST.Types,
  MVCFramework.RESTClient.Intf,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.Commons,
  MVCFramework.RESTClient.Indy,
  Data.DB,
  System.Rtti,
  System.TypInfo;

type
  /// <summary>
  /// Alias for the Indy-based TRESTClient. The implementation of TRESTClient has been discontinued, it remains for
  /// compatibility only.
  /// </summary>
  TRESTClient = MVCFramework.RESTClient.Indy.TRESTClient deprecated
    'Moved to the MVCFramework.RESTClient.Indy unit. It is highly recommended to migrate to the TMVCRESTClient implementation.';

  IRESTResponse = MVCFramework.RESTClient.Indy.IRESTResponse deprecated
    'Moved to the MVCFramework.RESTClient.Indy unit. It is highly recommended to migrate to the TMVCRESTClient implementation.';

  /// <summary>
  /// Provides access to MVCRESTClient interfaces without the need to use the MVCFramework.RESTClient.Intf unit
  /// </summary>
  IMVCRESTClient = MVCFramework.RESTClient.Intf.IMVCRESTClient;
  IMVCRESTResponse = MVCFramework.RESTClient.Intf.IMVCRESTResponse;

  /// <summary>
  /// Provides access to delphi RESTClient library types without the need to use the REST.Types unit.
  /// </summary>
  TRESTContentType = REST.Types.TRESTContentType;

  /// <summary>
  /// Encapsulates the methods of the delphi native RESTClient library.
  /// </summary>
  TMVCRESTClient = class(TInterfacedObject, IMVCRESTClient)
  private
    fRttiContext: TRttiContext;
    fRESTClient: TCustomRESTClient;
    fRESTRequest: TCustomRESTRequest;
    fRESTResponse: TCustomRESTResponse;
    fSerializer: IMVCSerializer;
    fNextRequestIsAsync: Boolean;
    fAsyncCompletionHandler: TProc<IMVCRESTResponse>;
    fAsyncSynchronized: Boolean;
    fAsyncCompletionHandlerWithError: TProc<Exception>;

    procedure ClearRESTParams(const aRESTParamKind: TRESTRequestParameterKind);
    function ConvertMVCPathParamsToRESTParams(const aResource: string): string;
    function ObjectIsList(aObject: TObject): Boolean;
    function SerializeObject(aObject: TObject): string;
    procedure ExecuteAsyncRESTRequest;
    function ExecuteRESTRequest(const aMethod: TRESTRequestMethod): IMVCRESTResponse;
  public
    constructor Create;
    destructor Destroy; override;

    class function New: IMVCRESTClient;

    { IMVCRESTClient }

    function BaseURL: string; overload;
    function BaseURL(const aBaseURL: string): IMVCRESTClient; overload;
    function RaiseExceptionOn500: Boolean; overload;
    function RaiseExceptionOn500(const aRaiseExceptionOn500: Boolean): IMVCRESTClient; overload;
    function ProxyServer: string; overload;
    function ProxyServer(const aProxyServer: string): IMVCRESTClient; overload;
    function ProxyPort: Integer; overload;
    function ProxyPort(const aProxyPort: Integer): IMVCRESTClient; overload;
    function ProxyUsername: string; overload;
    function ProxyUsername(const aProxyUsername: string): IMVCRESTClient; overload;
    function ProxyPassword: string; overload;
    function ProxyPassword(const aProxyPassword: string): IMVCRESTClient; overload;

    function UserAgent: string; overload;
    function UserAgent(const aUserAgent: string): IMVCRESTClient; overload;

    function ClearAllParams: IMVCRESTClient;

    function Timeout: Integer; overload;
    function Timeout(const aTimeout: Integer): IMVCRESTClient; overload;
    function SetBasicAuthorization(const aUsername, aPassword: string): IMVCRESTClient;
    function SetBearerAuthorization(const aToken: string): IMVCRESTClient;

    function AddHeader(const aName, aValue: string; const aDoNotEncode: Boolean = False): IMVCRESTClient; overload;
    function ClearHeaders: IMVCRESTClient;

    function AddCookie(const aName, aValue: string): IMVCRESTClient;
    function ClearCookies: IMVCRESTClient;

    function AddPathParam(const aName, aValue: string): IMVCRESTClient; overload;
    function AddPathParam(const aName: string; aValue: Integer): IMVCRESTClient; overload;
    function AddPathParam(const aName: string; aValue: Int64): IMVCRESTClient; overload;
    function AddPathParam(const aName: string; aValue: TGUID): IMVCRESTClient; overload;
    function AddPathParam(const aName: string; aValue: TDateTime): IMVCRESTClient; overload;
    function AddPathParam(const aName: string; aValue: TDate): IMVCRESTClient; overload;
    function AddPathParam(const aName: string; aValue: TTime): IMVCRESTClient; overload;
    function AddPathParam(const aName: string; aValue: Double): IMVCRESTClient; overload;
    function ClearPathParams: IMVCRESTClient;

    function AddQueryStringParam(const aName, aValue: string): IMVCRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: Integer): IMVCRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: Int64): IMVCRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: TGUID): IMVCRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: TDateTime): IMVCRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: TDate): IMVCRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: TTime): IMVCRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: Double): IMVCRESTClient; overload;
    function ClearQueryParams: IMVCRESTClient;

    function Accept: string; overload;
    function Accept(const aAccept: string): IMVCRESTClient; overload;
    function AcceptCharset: string; overload;
    function AcceptCharset(const aAcceptCharset: string): IMVCRESTClient; overload;
    function AcceptEncoding: string; overload;
    function AcceptEncoding(const aAcceptEncoding: string): IMVCRESTClient; overload;
    function HandleRedirects: Boolean; overload;
    function HandleRedirects(const aHandleRedirects: Boolean): IMVCRESTClient; overload;

    function Resource: string; overload;
    function Resource(const aResource: string): IMVCRESTClient; overload;
    function URLAlreadyEncoded: Boolean; overload;
    function URLAlreadyEncoded(const aURLAlreadyEncoded: Boolean): IMVCRESTClient; overload;

    function AddBody(const aBody: string; const aDoNotEncode: Boolean = False;
      const aContentType: TRESTContentType = TRESTContentType.ctNone): IMVCRESTClient; overload;
    function AddBody(aBody: TStream; const aContentType: TRESTContentType = TRESTContentType.ctNone;
      const aOwnsStream: Boolean = True): IMVCRESTClient; overload;
    function AddBody(aBody: TObject; const aOwnsObject: Boolean = True): IMVCRESTClient; overload;
    function ClearBody: IMVCRESTClient;

    function AddFile(const aName, aFileName: string;
      const aContentType: TRESTContentType = TRESTContentType.ctNone): IMVCRESTClient; overload;
    function AddFile(const aFileName: string;
      const aContentType: TRESTContentType = TRESTContentType.ctNone): IMVCRESTClient; overload;
    function ClearFiles: IMVCRESTClient;

    function Async(aCompletionHandler: TProc<IMVCRESTResponse>; const aSynchronized: Boolean = True;
      aCompletionHandlerWithError: TProc<Exception> = nil): IMVCRESTClient;

    function Get: IMVCRESTResponse; overload;
    function Get(const aResource: string): IMVCRESTResponse; overload;

    function Post: IMVCRESTResponse; overload;
    function Post(const aResource: string; const aBody: string = ''): IMVCRESTResponse; overload;
    function Post(const aResource: string; aBody: TObject; const aOwnsBody: Boolean = True): IMVCRESTResponse; overload;

    function Patch: IMVCRESTResponse; overload;
    function Patch(const aResource: string; const aBody: string = ''): IMVCRESTResponse; overload;
    function Patch(const aResource: string; aBody: TObject;
      const aOwnsBody: Boolean = True): IMVCRESTResponse; overload;

    function Put: IMVCRESTResponse; overload;
    function Put(const aResource: string; const aBody: string = ''): IMVCRESTResponse; overload;
    function Put(const aResource: string; aBody: TObject; const aOwnsBody: Boolean = True): IMVCRESTResponse; overload;

    function Delete: IMVCRESTResponse; overload;
    function Delete(const aResource: string): IMVCRESTResponse; overload;

    function DataSetInsert(const aResource: string; aDataSet: TDataSet; const aIgnoredFields: TMVCIgnoredList = [];
      const aNameCase: TMVCNameCase = ncAsIs): IMVCRESTResponse;
    function DataSetUpdate(const aResource: string; aDataSet: TDataSet; const aIgnoredFields: TMVCIgnoredList = [];
      const aNameCase: TMVCNameCase = ncAsIs): IMVCRESTResponse;
    function DataSetDelete(const aResource: string): IMVCRESTResponse;

    function RegisterTypeSerializer(const aTypeInfo: PTypeInfo; aInstance: IMVCTypeSerializer): IMVCRESTClient;
  end;

  /// <summary>
  /// Provides access to the REST request response.
  /// </summary>
  TMVCRESTResponse = class(TInterfacedObject, IMVCRESTResponse)
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

    { IMVCRESTResponse }
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

  EMVCRESTClientException = class(Exception);

implementation

uses
  MVCFramework.Serializer.JsonDataObjects,
  System.NetEncoding,
  System.RegularExpressions;

{ TMVCRESTClient }

function TMVCRESTClient.Accept(const aAccept: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.Accept := aAccept;
end;

function TMVCRESTClient.Accept: string;
begin
  Result := fRESTRequest.Accept;
end;

function TMVCRESTClient.AcceptCharset: string;
begin
  Result := fRESTRequest.AcceptCharset;
end;

function TMVCRESTClient.AcceptCharset(const aAcceptCharset: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.AcceptCharset := aAcceptCharset;
end;

function TMVCRESTClient.AcceptEncoding: string;
begin
  Result := fRESTRequest.AcceptEncoding;
end;

function TMVCRESTClient.AcceptEncoding(const aAcceptEncoding: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.AcceptEncoding := aAcceptEncoding;
end;

function TMVCRESTClient.AddBody(const aBody: string; const aDoNotEncode: Boolean;
  const aContentType: TRESTContentType): IMVCRESTClient;
var
  lBodyName: string;
  lOptions: TRESTRequestParameterOptions;
begin
  Result := Self;

  // A body does not have a specific name, but as names need to be unique, we are using a GUID here
  lBodyName := TGUID.NewGuid.ToString;
  lBodyName := lBodyName.Replace('{', '', [rfReplaceAll]);
  lBodyName := lBodyName.Replace('}', '', [rfReplaceAll]);
  lBodyName := lBodyName.Replace('-', '', [rfReplaceAll]);
  lBodyName := 'body' + lBodyName;

  if aDoNotEncode then
    lOptions := [TRESTRequestParameterOption.poDoNotEncode]
  else
    lOptions := [];

  fRESTRequest.AddParameter(lBodyName, aBody, TRESTRequestParameterKind.pkREQUESTBODY, lOptions);
end;

function TMVCRESTClient.AddBody(aBody: TStream; const aContentType: TRESTContentType;
  const aOwnsStream: Boolean): IMVCRESTClient;
var
  lOwnsStream: TRESTObjectOwnership;
begin
  if aBody = nil then
    raise EMVCRESTClientException.Create('You need a valid body!');

  Result := Self;

  if aOwnsStream then
    lOwnsStream := TRESTObjectOwnership.ooREST
  else
    lOwnsStream := TRESTObjectOwnership.ooApp;

  fRESTRequest.AddBody(aBody, aContentType, lOwnsStream);
end;

function TMVCRESTClient.AddBody(aBody: TObject; const aOwnsObject: Boolean): IMVCRESTClient;
begin
  if aBody = nil then
    raise EMVCRESTClientException.Create('You need a valid body!');

  Result := Self;

  AddBody(SerializeObject(aBody), False, TRESTContentType.ctAPPLICATION_JSON);

  if aOwnsObject then
    aBody.Free;
end;

function TMVCRESTClient.AddCookie(const aName, aValue: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.AddParameter(aName, aValue, TRESTRequestParameterKind.pkCOOKIE);
end;

function TMVCRESTClient.AddFile(const aName, aFileName: string; const aContentType: TRESTContentType): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.AddFile(aName, aFileName, aContentType);
end;

function TMVCRESTClient.AddFile(const aFileName: string; const aContentType: TRESTContentType): IMVCRESTClient;
begin
  Result := AddFile('file', aFileName, aContentType);
end;

function TMVCRESTClient.AddHeader(const aName, aValue: string; const aDoNotEncode: Boolean): IMVCRESTClient;
var
  lOptions: TRESTRequestParameterOptions;
begin
  Result := Self;

  if aDoNotEncode then
    lOptions := [TRESTRequestParameterOption.poDoNotEncode]
  else
    lOptions := [];

  fRESTRequest.AddParameter(aName, aValue, TRESTRequestParameterKind.pkHTTPHEADER, lOptions);
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: TGUID): IMVCRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: Int64): IMVCRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: Integer): IMVCRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddPathParam(const aName, aValue: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.AddParameter(aName, aValue);
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: Double): IMVCRESTClient;
begin
  Result := AddPathParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: TTime): IMVCRESTClient;
begin
  Result := AddPathParam(aName, TimeToISOTime(aValue));
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: TDateTime): IMVCRESTClient;
begin
  Result := AddPathParam(aName, DateTimeToISOTimeStamp(aValue));
end;

function TMVCRESTClient.AddPathParam(const aName: string; aValue: TDate): IMVCRESTClient;
begin
  Result := AddPathParam(aName, DateToISODate(aValue));
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: TDate): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, DateToISODate(aValue));
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: TDateTime): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, DateTimeToISOTimeStamp(aValue));
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: TTime): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, TimeToISOTime(aValue));
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: Double): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, aValue.ToString);
end;

function TMVCRESTClient.Async(aCompletionHandler: TProc<IMVCRESTResponse>; const aSynchronized: Boolean;
  aCompletionHandlerWithError: TProc<Exception>): IMVCRESTClient;
begin
  Result := Self;
  fNextRequestIsAsync := True;
  fAsyncCompletionHandler := aCompletionHandler;
  fAsyncSynchronized := aSynchronized;
  fAsyncCompletionHandlerWithError := aCompletionHandlerWithError;
end;

function TMVCRESTClient.AddQueryStringParam(const aName, aValue: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.AddParameter(aName, aValue);
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: Integer): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: TGUID): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, aValue.ToString);
end;

function TMVCRESTClient.AddQueryStringParam(const aName: string; aValue: Int64): IMVCRESTClient;
begin
  Result := AddQueryStringParam(aName, aValue.ToString);
end;

function TMVCRESTClient.BaseURL: string;
begin
  Result := fRESTClient.BaseURL;
end;

function TMVCRESTClient.BaseURL(const aBaseURL: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTClient.BaseURL := aBaseURL;
end;

function TMVCRESTClient.ClearAllParams: IMVCRESTClient;
begin
  Result := Self;

  fRESTClient.HandleRedirects := True;
  fRESTClient.RaiseExceptionOn500 := False;

  fRESTRequest.ResetToDefaults;
  fRESTRequest.AutoCreateParams := False;
  fNextRequestIsAsync := False;
  fAsyncCompletionHandler := nil;
  fAsyncSynchronized := False;
  fAsyncCompletionHandlerWithError := nil;
end;

function TMVCRESTClient.ClearBody: IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.ClearBody;
end;

function TMVCRESTClient.ClearCookies: IMVCRESTClient;
begin
  Result := Self;
  ClearRESTParams(TRESTRequestParameterKind.pkCOOKIE);
end;

function TMVCRESTClient.ClearFiles: IMVCRESTClient;
begin
  Result := Self;
  ClearRESTParams(TRESTRequestParameterKind.pkFILE);
end;

function TMVCRESTClient.ClearHeaders: IMVCRESTClient;
begin
  Result := Self;
  ClearRESTParams(TRESTRequestParameterKind.pkHTTPHEADER);
end;

function TMVCRESTClient.ClearPathParams: IMVCRESTClient;
begin
  Result := Self;
  ClearRESTParams(TRESTRequestParameterKind.pkURLSEGMENT);
end;

function TMVCRESTClient.ClearQueryParams: IMVCRESTClient;
begin
  Result := Self;
  ClearRESTParams(TRESTRequestParameterKind.pkQUERY);
end;

procedure TMVCRESTClient.ClearRESTParams(const aRESTParamKind: TRESTRequestParameterKind);
var
  I: Integer;
begin
  for I := Pred(fRESTRequest.Params.Count) downto 0 do
  begin
    if (fRESTRequest.Params[I].Kind = aRESTParamKind) then
      fRESTRequest.Params.Delete(I);
  end;
end;

function TMVCRESTClient.ConvertMVCPathParamsToRESTParams(const aResource: string): string;
begin
  Result := TRegEx.Replace(aResource, '(\([($])([\w_]+)([)])', '{\2}', [roIgnoreCase]);
end;

constructor TMVCRESTClient.Create;
begin
  inherited Create;

  fRESTClient := TCustomRESTClient.Create(nil);
  fRESTRequest := TCustomRESTRequest.Create(nil);
  fRESTResponse := TCustomRESTResponse.Create(nil);

  fRESTRequest.Client := fRESTClient;
  fRESTRequest.Response := fRESTResponse;

  fSerializer := TMVCJsonDataObjectsSerializer.Create;
  fRttiContext := TRttiContext.Create;

  ClearAllParams;
end;

function TMVCRESTClient.DataSetDelete(const aResource: string): IMVCRESTResponse;
begin
  Result := Delete(aResource);
end;

function TMVCRESTClient.DataSetInsert(const aResource: string; aDataSet: TDataSet; const aIgnoredFields: TMVCIgnoredList;
  const aNameCase: TMVCNameCase): IMVCRESTResponse;
begin
  Result := Post(aResource, fSerializer.SerializeDataSetRecord(aDataSet, aIgnoredFields, aNameCase));
end;

function TMVCRESTClient.DataSetUpdate(const aResource: string; aDataSet: TDataSet; const aIgnoredFields: TMVCIgnoredList;
  const aNameCase: TMVCNameCase): IMVCRESTResponse;
begin
  Result := Put(aResource, fSerializer.SerializeDataSetRecord(aDataSet, aIgnoredFields, aNameCase));
end;

function TMVCRESTClient.Delete: IMVCRESTResponse;
begin
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmDELETE);
end;

function TMVCRESTClient.Delete(const aResource: string): IMVCRESTResponse;
begin
  Resource(aResource);
  Result := Delete;
end;

destructor TMVCRESTClient.Destroy;
begin
  fRESTResponse.Free;
  fRESTRequest.Free;
  fRESTClient.Free;
  fSerializer := nil;
  fRttiContext.Free;

  inherited Destroy;
end;

procedure TMVCRESTClient.ExecuteAsyncRESTRequest;
begin
  fRESTRequest.ExecuteAsync(
// procedure
// var
// lMVCRESTResponse: IMVCRESTResponse;
// begin
// lMVCRESTResponse := TMVCRESTResponse.Create(fRESTResponse);
// if Assigned(fAsyncCompletionHandler) then
// fAsyncCompletionHandler(lMVCRESTResponse);
// ClearAllParams;
// end
  );
end;

function TMVCRESTClient.ExecuteRESTRequest(const aMethod: TRESTRequestMethod): IMVCRESTResponse;
begin
  fRESTRequest.Method := aMethod;

  if fNextRequestIsAsync then
  begin
    Result := nil;
    ExecuteAsyncRESTRequest;
  end
  else
  begin
    fRESTRequest.Execute;
    Result := TMVCRESTResponse.Create(fRESTResponse);

    ClearAllParams;
  end;
end;

function TMVCRESTClient.Get(const aResource: string): IMVCRESTResponse;
begin
  Resource(aResource);
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmGET);
end;

function TMVCRESTClient.HandleRedirects(const aHandleRedirects: Boolean): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.HandleRedirects := aHandleRedirects;
end;

function TMVCRESTClient.HandleRedirects: Boolean;
begin
  Result := fRESTRequest.HandleRedirects;
end;

class function TMVCRESTClient.New: IMVCRESTClient;
begin
  Result := TMVCRESTClient.Create;
end;

function TMVCRESTClient.ObjectIsList(aObject: TObject): Boolean;
begin
  Result := fRttiContext.GetType(aObject.ClassType).GetMethod('GetEnumerator') <> nil;
end;

function TMVCRESTClient.Get: IMVCRESTResponse;
begin
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmGET);
end;

function TMVCRESTClient.Patch(const aResource, aBody: string): IMVCRESTResponse;
begin
  Resource(aResource);
  if not aBody.isEmpty then
  begin
    ClearBody;
    AddBody(aBody, False, TRESTContentType.ctAPPLICATION_JSON);
  end;

  Result := Patch;
end;

function TMVCRESTClient.Patch: IMVCRESTResponse;
begin
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmPATCH);
end;

function TMVCRESTClient.Patch(const aResource: string; aBody: TObject; const aOwnsBody: Boolean): IMVCRESTResponse;
begin
  if aBody = nil then
    raise EMVCRESTClientException.Create('You need a valid body!');

  Result := Patch(aResource, SerializeObject(aBody));

  if aOwnsBody then
    aBody.Free;
end;

function TMVCRESTClient.Post(const aResource: string; aBody: TObject; const aOwnsBody: Boolean): IMVCRESTResponse;
begin
  if aBody = nil then
    raise EMVCRESTClientException.Create('You need a valid body!');

  Result := Post(aResource, SerializeObject(aBody));

  if aOwnsBody then
    aBody.Free;
end;

function TMVCRESTClient.Post(const aResource, aBody: string): IMVCRESTResponse;
begin
  Resource(aResource);
  if not aBody.IsEmpty then
  begin
    ClearBody;
    AddBody(aBody, False, TRESTContentType.ctAPPLICATION_JSON);
  end;
  Result := Post;
end;

function TMVCRESTClient.Post: IMVCRESTResponse;
begin
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmPOST);
end;

function TMVCRESTClient.ProxyPassword(const aProxyPassword: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTClient.ProxyPassword := aProxyPassword;
end;

function TMVCRESTClient.ProxyPassword: string;
begin
  Result := fRESTClient.ProxyPassword;
end;

function TMVCRESTClient.ProxyPort: Integer;
begin
  Result := fRESTClient.ProxyPort;
end;

function TMVCRESTClient.ProxyPort(const aProxyPort: Integer): IMVCRESTClient;
begin
  Result := Self;
  fRESTClient.ProxyPort := aProxyPort;
end;

function TMVCRESTClient.ProxyServer(const aProxyServer: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTClient.ProxyServer := aProxyServer;
end;

function TMVCRESTClient.ProxyServer: string;
begin
  Result := fRESTClient.ProxyServer;
end;

function TMVCRESTClient.ProxyUsername: string;
begin
  Result := fRESTClient.ProxyUsername;
end;

function TMVCRESTClient.ProxyUsername(const aProxyUsername: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTClient.ProxyUsername := aProxyUsername;
end;

function TMVCRESTClient.Put(const aResource: string; aBody: TObject; const aOwnsBody: Boolean): IMVCRESTResponse;
begin
  if aBody = nil then
    raise EMVCRESTClientException.Create('You need a valid body!');

  Result := Put(aResource, SerializeObject(aBody));

  if aOwnsBody then
    aBody.Free;
end;

function TMVCRESTClient.Put(const aResource, aBody: string): IMVCRESTResponse;
begin
  Resource(aResource);
  if not aBody.IsEmpty then
  begin
    ClearBody;
    AddBody(aBody, False, TRESTContentType.ctAPPLICATION_JSON);
  end;
  Result := Put;
end;

function TMVCRESTClient.Put: IMVCRESTResponse;
begin
  Result := ExecuteRESTRequest(TRESTRequestMethod.rmPUT);
end;

function TMVCRESTClient.RaiseExceptionOn500(const aRaiseExceptionOn500: Boolean): IMVCRESTClient;
begin
  Result := Self;
  fRESTClient.RaiseExceptionOn500 := aRaiseExceptionOn500;
end;

function TMVCRESTClient.Resource: string;
begin
  Result := fRESTRequest.Resource;
end;

function TMVCRESTClient.RegisterTypeSerializer(const aTypeInfo: PTypeInfo;
  aInstance: IMVCTypeSerializer): IMVCRESTClient;
begin
  Result := Self;
  fSerializer.RegisterTypeSerializer(aTypeInfo, aInstance);
end;

function TMVCRESTClient.Resource(const aResource: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.Resource := ConvertMVCPathParamsToRESTParams(aResource);

end;

function TMVCRESTClient.RaiseExceptionOn500: Boolean;
begin
  Result := fRESTClient.RaiseExceptionOn500;
end;

function TMVCRESTClient.Timeout: Integer;
begin
  Result := fRESTRequest.Timeout;
end;

function TMVCRESTClient.Timeout(const aTimeout: Integer): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.Timeout := aTimeout;
end;

function TMVCRESTClient.URLAlreadyEncoded(const aURLAlreadyEncoded: Boolean): IMVCRESTClient;
begin
  Result := Self;
  fRESTRequest.URLAlreadyEncoded := aURLAlreadyEncoded;
end;

function TMVCRESTClient.UserAgent(const aUserAgent: string): IMVCRESTClient;
begin
  Result := Self;
  fRESTClient.UserAgent := aUserAgent;
end;

function TMVCRESTClient.UserAgent: string;
begin
  Result := fRESTClient.UserAgent;
end;

function TMVCRESTClient.URLAlreadyEncoded: Boolean;
begin
  Result := fRESTRequest.URLAlreadyEncoded;
end;

function TMVCRESTClient.SerializeObject(aObject: TObject): string;
begin
  if ObjectIsList(aObject) then
    Result := fSerializer.SerializeCollection(aObject)
  else
    Result := fSerializer.SerializeObject(aObject);
end;

function TMVCRESTClient.SetBasicAuthorization(const aUsername, aPassword: string): IMVCRESTClient;
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
  AddHeader('Authorization', LAuthValue, True)
end;

function TMVCRESTClient.SetBearerAuthorization(const aToken: string): IMVCRESTClient;
begin
  Result := Self;
  AddHeader('Authorization', 'Bearer ' + aToken, True);
end;

{ TMVCRESTResponse }

function TMVCRESTResponse.Content: string;
begin
  Result := fContent;
end;

function TMVCRESTResponse.ContentEncoding: string;
begin
  Result := fContentEncoding;
end;

function TMVCRESTResponse.ContentLength: Integer;
begin
  Result := fContentLength;
end;

function TMVCRESTResponse.ContentType: string;
begin
  Result := fContentType;
end;

constructor TMVCRESTResponse.Create(aRESTResponse: TCustomRESTResponse);
begin
  inherited Create;
  fHeaders := TStringList.Create;
  SetLength(fRawBytes, 0);

  FillRESTResponse(aRESTResponse);
end;

destructor TMVCRESTResponse.Destroy;
begin
  SetLength(fRawBytes, 0);
  fHeaders.Free;
  inherited Destroy;
end;

function TMVCRESTResponse.ErrorMessage: string;
begin
  Result := fErrorMessage;
end;

procedure TMVCRESTResponse.FillRESTResponse(aRESTResponse: TCustomRESTResponse);
begin
  fSuccess := aRESTResponse.Status.Success;
  fStatusCode := aRESTResponse.StatusCode;
  fStatusText := aRESTResponse.StatusText;
  fErrorMessage := aRESTResponse.ErrorMessage;
  fHeaders.Assign(aRESTResponse.Headers);
  fServer := aRESTResponse.Server;
  fFullRequestURI := aRESTResponse.FullRequestURI;
  fContent := aRESTResponse.Content;
  fRawBytes := aRESTResponse.RawBytes;
  fContentType := aRESTResponse.ContentType;
  fContentEncoding := aRESTResponse.ContentEncoding;
  fContentLength := aRESTResponse.ContentLength;
end;

function TMVCRESTResponse.FullRequestURI: string;
begin
  Result := fFullRequestURI;
end;

function TMVCRESTResponse.HeaderByName(const aName: string): string;
begin
  Result := fHeaders.Values[aName];
end;

function TMVCRESTResponse.Headers: TStrings;
begin
  Result := fHeaders;
end;

function TMVCRESTResponse.RawBytes: TBytes;
begin
  Result := fRawBytes;
end;

procedure TMVCRESTResponse.SaveContentToFile(const aFileName: string);
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

procedure TMVCRESTResponse.SaveContentToStream(aStream: TStream);
begin
  if aStream = nil then
    raise EMVCRESTClientException.Create('Stream not assigned!');

  aStream.Write(fRawBytes, Length(fRawBytes));
end;

function TMVCRESTResponse.Server: string;
begin
  Result := fServer;
end;

function TMVCRESTResponse.StatusCode: Integer;
begin
  Result := fStatusCode;
end;

function TMVCRESTResponse.StatusText: string;
begin
  Result := fStatusText;
end;

function TMVCRESTResponse.Success: Boolean;
begin
  Result := fSuccess;
end;

end.
