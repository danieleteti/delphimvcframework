// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2020 Daniele Teti and the DMVCFramework Team
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
  MVCFramework.Commons;

type
  IMVCJSONRPCExecutor = interface
    ['{55415094-9D28-4707-AEC5-5FCF925E82BC}']
    function ExecuteRequest(const aJSONRPCRequest: IJSONRPCRequest): IJSONRPCResponse;
    function ExecuteNotification(const aJSONRPCNotification: IJSONRPCNotification): IJSONRPCResponse;
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
    function ConfigureHTTPClient(const aConfigProc: TProc<THTTPClient>): IMVCJSONRPCExecutor;
  end;

  TMVCJSONRPCExecutor = class(TInterfacedObject, IMVCJSONRPCExecutor)
  private
    fURL: string;
    fHTTP: THTTPClient;
    fRaiseExceptionOnError: Boolean;
    fHTTPRequestHeaders: TList<TNetHeader>;
    fHTTPResponse: IHTTPResponse;
    fOnReceiveResponse: TProc<IJSONRPCObject, IJSONRPCObject>;
    fOnSendCommand: TProc<IJSONRPCObject>;
    function GetHTTPRequestHeaders: TList<TNetHeader>;
  protected
    function HTTPResponse: IHTTPResponse;
    function InternalExecute(const aJSONRPCObject: IJSONRPCObject): IJSONRPCResponse;
    function ExecuteRequest(const aJSONRPCRequest: IJSONRPCRequest): IJSONRPCResponse;
    function ExecuteNotification(const aJSONRPCNotification: IJSONRPCNotification): IJSONRPCResponse;
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
    function ConfigureHTTPClient(const aConfigProc: TProc<THTTPClient>): IMVCJSONRPCExecutor;
  public
    constructor Create(const aURL: string; const aRaiseExceptionOnError: Boolean = True); virtual;
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
      lHttpResp := lHTTP.Post(aJSONRPCURL, lSS, nil,
        [TNetHeader.Create('content-type', 'application/json'), TNetHeader.Create('accept', 'application/json')]);
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

function TMVCJSONRPCExecutor.ConfigureHTTPClient(
  const aConfigProc: TProc<THTTPClient>): IMVCJSONRPCExecutor;
begin
  aConfigProc(fHTTP);
end;

constructor TMVCJSONRPCExecutor.Create(const aURL: string; const aRaiseExceptionOnError: Boolean = True);
begin
  inherited Create;
  fRaiseExceptionOnError := aRaiseExceptionOnError;
  fURL := aURL;
  fHTTP := THTTPClient.Create;
  fHTTP.ResponseTimeout := MaxInt;
  fHTTPRequestHeaders := nil;
  SetOnReceiveResponse(nil)
    .SetOnReceiveData(nil)
    .SetOnNeedClientCertificate(nil)
    .SetOnValidateServerCertificate(nil);
end;

destructor TMVCJSONRPCExecutor.Destroy;
begin
  fHTTP.Free;
  fHTTPRequestHeaders.Free;
  inherited;
end;

function TMVCJSONRPCExecutor.ExecuteNotification(const aJSONRPCNotification: IJSONRPCNotification): IJSONRPCResponse;
// var
// lResp: IJSONRPCResponse;
begin
  Result := InternalExecute(aJSONRPCNotification as TJSONRPCObject);
  // if Assigned(lResp) then
  // begin
  //
  // end;
  // if InternalExecute(aJSONRPCNotification as TJSONRPCObject) <> nil then
  // begin
  // raise EMVCJSONRPCException.Create('A "notification" cannot returns a response. Use ExecuteRequest instead.');
  // end;
end;

function TMVCJSONRPCExecutor.ExecuteRequest(const aJSONRPCRequest: IJSONRPCRequest): IJSONRPCResponse;
begin
  Result := InternalExecute(aJSONRPCRequest);
end;

function TMVCJSONRPCExecutor.GetHTTPRequestHeaders: TList<TNetHeader>;
begin
  if not Assigned(fHTTPRequestHeaders) then
  begin
    fHTTPRequestHeaders := TList<TNetHeader>.Create;
  end;
  Result := fHTTPRequestHeaders;
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

function TMVCJSONRPCExecutor.InternalExecute(const aJSONRPCObject: IJSONRPCObject): IJSONRPCResponse;
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
    lHttpResp := fHTTP.Post(fURL, lSS, nil, [TNetHeader.Create('content-type', 'application/json;charset=utf8'),
      TNetHeader.Create('accept', 'application/json;charset=utf8')] + lCustomHeaders);
    fHTTPResponse := lHttpResp;
    if lHttpResp.StatusCode = HTTP_STATUS.NoContent then
    begin
      lJSONRPCResponse := TJSONRPCNullResponse.Create;
    end
    else
    begin
      lJSONRPCResponse := TJSONRPCResponse.Create;
      lJSONRPCResponse.AsJSONString := lHttpResp.ContentAsString;
    end;
    if Assigned(fOnReceiveResponse) then
    begin
      fOnReceiveResponse(aJSONRPCObject, lJSONRPCResponse);
    end;
    fHTTPResponse := lHttpResp;
    if Assigned(lJSONRPCResponse.Error) and fRaiseExceptionOnError then
      raise EMVCJSONRPCException.CreateFmt('[REMOTE EXCEPTION][%d]: %s',
        [lJSONRPCResponse.Error.Code, lJSONRPCResponse.Error.ErrMessage]);
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

function TMVCJSONRPCExecutor.SetOnReceiveData(
  const aOnReceiveData: TReceiveDataEvent): IMVCJSONRPCExecutor;
begin
  fHTTP.OnReceiveData := aOnReceiveData;
  Result := Self;
end;

function TMVCJSONRPCExecutor.SetOnReceiveResponse(const aOnReceiveResponseProc: TProc<IJSONRPCObject, IJSONRPCObject>)
  : IMVCJSONRPCExecutor;
begin
  fOnReceiveResponse := aOnReceiveResponseProc;
  Result := Self;
end;

function TMVCJSONRPCExecutor.SetOnSendCommand(
  const aOnSendCommandProc: TProc<IJSONRPCObject>): IMVCJSONRPCExecutor;
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

end.
