// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2022 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.JSONRPC.Client;

interface

uses
  MVCFramework.JSONRPC,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.Generics.Collections,
  System.SysUtils,
  MVCFramework.Commons,
  JsonDataObjects;

type
  IMVCJSONRPCExecutor = interface
    ['{55415094-9D28-4707-AEC5-5FCF925E82BC}']
    function ExecuteRequest(const aJSONRPCRequest: IJSONRPCRequest; const UseVerb: TJSONRPCHTTPVerb = jrpcDefault)
      : IJSONRPCResponse; overload;
    function ExecuteRequest(const aLastEndPointSegment: string; const aJSONRPCRequest: IJSONRPCRequest;
      const UseVerb: TJSONRPCHTTPVerb = jrpcDefault): IJSONRPCResponse; overload;
    function ExecuteNotification(const aJSONRPCNotification: IJSONRPCNotification;
      const UseVerb: TJSONRPCHTTPVerb = jrpcDefault): IJSONRPCResponse; overload;
    function ExecuteNotification(const aLastEndPointSegment: string; const aJSONRPCNotification: IJSONRPCNotification;
      const UseVerb: TJSONRPCHTTPVerb = jrpcDefault): IJSONRPCResponse; overload;
    function HTTPResponse: IHTTPResponse;
    // Http headers handling
    procedure AddHTTPHeader(const aNetHeader: TNetHeader);
    procedure ClearHTTPHeaders;
    function HTTPHeadersCount: Integer;
    function SetOnReceiveData(const aOnReceiveData: TReceiveDataEvent): IMVCJSONRPCExecutor;
    function SetOnReceiveResponse(const aOnReceiveResponseProc: TProc<IJSONRPCObject, IJSONRPCObject>)
      : IMVCJSONRPCExecutor;
    function SetOnSendCommand(const aOnSendCommandProc: TProc<IJSONRPCObject>): IMVCJSONRPCExecutor;
    function SetOnNeedClientCertificate(const aOnNeedClientCertificate: TNeedClientCertificateEvent)
      : IMVCJSONRPCExecutor;
    function SetOnValidateServerCertificate(const aOnValidateServerCertificate: TValidateCertificateEvent)
      : IMVCJSONRPCExecutor;
    function SetOnReceiveHTTPResponse(const aOnReceiveHTTPResponse: TProc<IHTTPResponse>): IMVCJSONRPCExecutor;
    function ConfigureHTTPClient(const aConfigProc: TProc<THTTPClient>): IMVCJSONRPCExecutor;
  end;

  TMVCJSONRPCExecutor = class(TInterfacedObject, IMVCJSONRPCExecutor)
  private
    fDefaultHTTPVerb: TJSONRPCHTTPVerb;
    fURL: string;
    fHTTP: THTTPClient;
    fRaiseExceptionOnError: Boolean;
    fHTTPRequestHeaders: TList<TNetHeader>;
    fHTTPResponse: IHTTPResponse;
    fOnReceiveResponse: TProc<IJSONRPCObject, IJSONRPCObject>;
    fOnReceiveHTTPResponse: TProc<IHTTPResponse>;
    fOnSendCommand: TProc<IJSONRPCObject>;
    function GetHTTPRequestHeaders: TList<TNetHeader>;
  protected
    function GetQueryStringParameters(const aJSONRPCObject: IJSONRPCObject): String;
    function HTTPResponse: IHTTPResponse;
    function InternalExecute(const aEndPoint: string; const aJSONRPCObject: IJSONRPCObject;
      const UseVerb: TJSONRPCHTTPVerb): IJSONRPCResponse;
    function ExecuteRequest(const aJSONRPCRequest: IJSONRPCRequest; const UseVerb: TJSONRPCHTTPVerb)
      : IJSONRPCResponse; overload;
    function ExecuteRequest(const aLastEndPointSegment: string; const aJSONRPCRequest: IJSONRPCRequest;
      const UseVerb: TJSONRPCHTTPVerb): IJSONRPCResponse; overload;
    function ExecuteNotification(const aJSONRPCNotification: IJSONRPCNotification; const UseVerb: TJSONRPCHTTPVerb)
      : IJSONRPCResponse; overload;
    function ExecuteNotification(const aLastEndPointSegment: string; const aJSONRPCNotification: IJSONRPCNotification;
      const UseVerb: TJSONRPCHTTPVerb): IJSONRPCResponse; overload;
    // Http headers handling
    procedure AddHTTPHeader(const aNetHeader: TNetHeader);
    procedure ClearHTTPHeaders;
    function HTTPHeadersCount: Integer;
    function SetOnReceiveData(const aOnReceiveData: TReceiveDataEvent): IMVCJSONRPCExecutor;
    function SetOnReceiveResponse(const aOnReceiveResponseProc: TProc<IJSONRPCObject, IJSONRPCObject>)
      : IMVCJSONRPCExecutor;
    function SetOnSendCommand(const aOnSendCommandProc: TProc<IJSONRPCObject>): IMVCJSONRPCExecutor;
    function SetOnNeedClientCertificate(const aOnNeedClientCertificate: TNeedClientCertificateEvent)
      : IMVCJSONRPCExecutor;
    function SetOnValidateServerCertificate(const aOnValidateServerCertificate: TValidateCertificateEvent)
      : IMVCJSONRPCExecutor;
    function SetOnReceiveHTTPResponse(const aOnReceiveHTTPResponse: TProc<IHTTPResponse>): IMVCJSONRPCExecutor;
    function ConfigureHTTPClient(const aConfigProc: TProc<THTTPClient>): IMVCJSONRPCExecutor;
  public
    constructor Create(const aURL: string; const aRaiseExceptionOnError: Boolean = True;
      const aDefaultHTTPVerb: TJSONRPCHTTPVerb = jrpcDefault); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  System.Classes;

procedure JSONRPCExec(const aJSONRPCURL: string; const aJSONRPCRequest: IJSONRPCRequest;
  out aJSONRPCResponse: IJSONRPCResponse);
var
  lSS: TStringStream;
  lHttpResp: IHTTPResponse;
  lHTTP: THTTPClient;
begin
  lSS := TStringStream.Create(aJSONRPCRequest.AsJSONString);
  try
    lSS.Position := 0;
    lHTTP := THTTPClient.Create;
    try
      lHttpResp := lHTTP.Post(aJSONRPCURL, lSS, nil, [TNetHeader.Create('content-type', 'application/json'),
        TNetHeader.Create('accept', 'application/json')]);
      if (lHttpResp.StatusCode <> 204) then
      begin
        aJSONRPCResponse := TJSONRPCResponse.Create;
        aJSONRPCResponse.AsJSONString := lHttpResp.ContentAsString;
        if Assigned(aJSONRPCResponse.Error) then
          raise Exception.CreateFmt('Error [%d]: %s', [aJSONRPCResponse.Error.Code, aJSONRPCResponse.Error.ErrMessage]);
      end;
    finally
      lHTTP.Free;
    end;
  finally
    lSS.Free;
  end;
end;

{ TMVCJSONRPCExecutor }

procedure TMVCJSONRPCExecutor.AddHTTPHeader(const aNetHeader: TNetHeader);
begin
  GetHTTPRequestHeaders.Add(aNetHeader);
end;

procedure TMVCJSONRPCExecutor.ClearHTTPHeaders;
begin
  if Assigned(fHTTPRequestHeaders) then
  begin
    fHTTPRequestHeaders.Clear;
  end;
end;

function TMVCJSONRPCExecutor.ConfigureHTTPClient(const aConfigProc: TProc<THTTPClient>): IMVCJSONRPCExecutor;
begin
  aConfigProc(fHTTP);
end;

constructor TMVCJSONRPCExecutor.Create(const aURL: string; const aRaiseExceptionOnError: Boolean = True;
  const aDefaultHTTPVerb: TJSONRPCHTTPVerb = jrpcDefault);
begin
  inherited Create;
  fRaiseExceptionOnError := aRaiseExceptionOnError;
  fURL := aURL;
  fDefaultHTTPVerb := aDefaultHTTPVerb;
  fHTTP := THTTPClient.Create;
{$IF defined(BERLINORBETTER)}
  fHTTP.ResponseTimeout := MaxInt;
{$ENDIF}
  fHTTPRequestHeaders := nil;
  SetOnReceiveResponse(nil).SetOnReceiveData(nil).SetOnNeedClientCertificate(nil).SetOnValidateServerCertificate(nil);
end;

destructor TMVCJSONRPCExecutor.Destroy;
begin
  fHTTP.Free;
  fHTTPRequestHeaders.Free;
  inherited;
end;

function TMVCJSONRPCExecutor.ExecuteNotification(const aJSONRPCNotification: IJSONRPCNotification;
  const UseVerb: TJSONRPCHTTPVerb): IJSONRPCResponse;
begin
  Result := ExecuteNotification('', aJSONRPCNotification, UseVerb);
end;

function TMVCJSONRPCExecutor.ExecuteRequest(const aJSONRPCRequest: IJSONRPCRequest; const UseVerb: TJSONRPCHTTPVerb)
  : IJSONRPCResponse;
begin
  Result := ExecuteRequest('', aJSONRPCRequest, UseVerb);
end;

function TMVCJSONRPCExecutor.GetHTTPRequestHeaders: TList<TNetHeader>;
begin
  if not Assigned(fHTTPRequestHeaders) then
  begin
    fHTTPRequestHeaders := TList<TNetHeader>.Create;
  end;
  Result := fHTTPRequestHeaders;
end;

function TMVCJSONRPCExecutor.GetQueryStringParameters(const aJSONRPCObject: IJSONRPCObject): String;
var
  lJObj: TJDOJsonObject;
begin
  lJObj := aJSONRPCObject.AsJSON;
  try
    Result := 'jsonrpc=' + URLEncode(lJObj.S['jsonrpc']);
    Result := Result + '&method=' + URLEncode(lJObj.S['method']);
    if lJObj.Contains('id') then
    begin
      Result := Result + '&id=' + URLEncode(lJObj.S['id']);
    end;

    if lJObj.Contains('params') then
    begin
      if lJObj.Types['params'] = jdtArray then
      begin
        Result := Result + '&params=' + URLEncode(lJObj.A['params'].ToJSON());
      end
      else
      begin
        Result := Result + '&params=' + URLEncode(lJObj.O['params'].ToJSON());
      end;
    end;
  finally
    lJObj.Free;
  end;
end;

function TMVCJSONRPCExecutor.HTTPHeadersCount: Integer;
begin
  if Assigned(fHTTPRequestHeaders) then
  begin
    Result := fHTTPRequestHeaders.Count;
  end
  else
  begin
    Result := 0;
  end;
end;

function TMVCJSONRPCExecutor.InternalExecute(const aEndPoint: string; const aJSONRPCObject: IJSONRPCObject;
  const UseVerb: TJSONRPCHTTPVerb): IJSONRPCResponse;
var
  lSS: TStringStream;
  lHttpResp: IHTTPResponse;
  lJSONRPCResponse: IJSONRPCResponse;
  lCustomHeaders: TNetHeaders;
begin
  lCustomHeaders := [];
  if Assigned(fHTTPRequestHeaders) then
  begin
    lCustomHeaders := fHTTPRequestHeaders.ToArray;
  end;

  Result := nil;
  lSS := TStringStream.Create(aJSONRPCObject.AsJSONString, TEncoding.UTF8);
  try
    lSS.Position := 0;
    if Assigned(fOnSendCommand) then
    begin
      fOnSendCommand(aJSONRPCObject);
    end;
    fHTTPResponse := nil;

    case UseVerb of
      jrpcPOST, jrpcDefault:
        begin
          lHttpResp := fHTTP.Post(fURL + aEndPoint, lSS, nil,
            [TNetHeader.Create('content-type', 'application/json;charset=utf8'), TNetHeader.Create('accept',
            'application/json;charset=utf8')] + lCustomHeaders);
        end;
      jrpcGET:
        begin
          lHttpResp := fHTTP.Get(fURL + aEndPoint + '?' + GetQueryStringParameters(aJSONRPCObject), nil,
            [TNetHeader.Create('accept', 'application/json;charset=utf8')] + lCustomHeaders);
        end;
    end;

    fHTTPResponse := lHttpResp;
    if Assigned(fOnReceiveHTTPResponse) then
    begin
      fOnReceiveHTTPResponse(fHTTPResponse);
    end;
    lJSONRPCResponse := nil;
    if lHttpResp.StatusCode = HTTP_STATUS.NoContent then
    begin
      lJSONRPCResponse := TJSONRPCNullResponse.Create;
    end
    else
    begin
      if fHTTPResponse.HeaderValue['content-type'].Contains(TMVCMediaType.APPLICATION_JSON) then
      begin
        lJSONRPCResponse := TJSONRPCResponse.Create;
        lJSONRPCResponse.AsJSONString := lHttpResp.ContentAsString;
      end;
    end;
    fHTTPResponse := lHttpResp;
    if Assigned(fOnReceiveResponse) then
    begin
      fOnReceiveResponse(aJSONRPCObject, lJSONRPCResponse);
    end;

    if lJSONRPCResponse = nil then
    begin
      raise EMVCJSONRPCException.CreateFmt('[PROTOCOL EXCEPTION][%d: %s]: %s',
        [fHTTPResponse.StatusCode,
        fHTTPResponse.StatusText,
        fHTTPResponse.ContentAsString()]);
    end;

    if Assigned(lJSONRPCResponse.Error) and fRaiseExceptionOnError then
      raise EMVCJSONRPCRemoteException.Create(
        lJSONRPCResponse.Error.Code,
        lJSONRPCResponse.Error.ErrMessage,
        lJSONRPCResponse.Error.Data
        );
    Result := lJSONRPCResponse;
  finally
    lSS.Free;
  end;
end;

function TMVCJSONRPCExecutor.HTTPResponse: IHTTPResponse;
begin
  Result := fHTTPResponse;
end;

function TMVCJSONRPCExecutor.SetOnNeedClientCertificate(const aOnNeedClientCertificate: TNeedClientCertificateEvent)
  : IMVCJSONRPCExecutor;
begin
  fHTTP.OnNeedClientCertificate := aOnNeedClientCertificate;
  Result := Self;
end;

function TMVCJSONRPCExecutor.SetOnReceiveData(const aOnReceiveData: TReceiveDataEvent): IMVCJSONRPCExecutor;
begin
  fHTTP.OnReceiveData := aOnReceiveData;
  Result := Self;
end;

function TMVCJSONRPCExecutor.SetOnReceiveHTTPResponse(const aOnReceiveHTTPResponse: TProc<IHTTPResponse>)
  : IMVCJSONRPCExecutor;
begin
  fOnReceiveHTTPResponse := aOnReceiveHTTPResponse;
  Result := Self;
end;

function TMVCJSONRPCExecutor.SetOnReceiveResponse(const aOnReceiveResponseProc: TProc<IJSONRPCObject, IJSONRPCObject>)
  : IMVCJSONRPCExecutor;
begin
  fOnReceiveResponse := aOnReceiveResponseProc;
  Result := Self;
end;

function TMVCJSONRPCExecutor.SetOnSendCommand(const aOnSendCommandProc: TProc<IJSONRPCObject>): IMVCJSONRPCExecutor;
begin
  fOnSendCommand := aOnSendCommandProc;
  Result := Self;
end;

function TMVCJSONRPCExecutor.SetOnValidateServerCertificate(const aOnValidateServerCertificate
  : TValidateCertificateEvent): IMVCJSONRPCExecutor;
begin
  fHTTP.OnValidateServerCertificate := aOnValidateServerCertificate;
  Result := Self;
end;

function TMVCJSONRPCExecutor.ExecuteNotification(const aLastEndPointSegment: string;
  const aJSONRPCNotification: IJSONRPCNotification; const UseVerb: TJSONRPCHTTPVerb): IJSONRPCResponse;
begin
  Result := InternalExecute(aLastEndPointSegment, aJSONRPCNotification as TJSONRPCObject, UseVerb);
end;

function TMVCJSONRPCExecutor.ExecuteRequest(const aLastEndPointSegment: string; const aJSONRPCRequest: IJSONRPCRequest;
  const UseVerb: TJSONRPCHTTPVerb): IJSONRPCResponse;
begin
  Result := InternalExecute(aLastEndPointSegment, aJSONRPCRequest, UseVerb);
end;

end.
