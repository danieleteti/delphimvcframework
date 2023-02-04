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
  TJSONRPCResponseHandlerProc = reference to procedure(JSONRPCResponse: IJSONRPCResponse);
  TJSONRPCErrorHandlerProc = reference to procedure(Exc: Exception);

  IMVCJSONRPCExecutor = interface
    ['{55415094-9D28-4707-AEC5-5FCF925E82BC}']
    //sync
    function ExecuteRequest(
      const aJSONRPCRequest: IJSONRPCRequest;
      const UseVerb: TJSONRPCHTTPVerb = jrpcDefault): IJSONRPCResponse; overload;
    function ExecuteRequest(
      const aLastEndPointSegment: string;
      const aJSONRPCRequest: IJSONRPCRequest;
      const UseVerb: TJSONRPCHTTPVerb = jrpcDefault): IJSONRPCResponse; overload;
    function ExecuteNotification(
      const aJSONRPCNotification: IJSONRPCNotification;
      const UseVerb: TJSONRPCHTTPVerb = jrpcDefault): IJSONRPCResponse; overload;
    function ExecuteNotification(
      const aLastEndPointSegment: string;
      const aJSONRPCNotification: IJSONRPCNotification;
      const UseVerb: TJSONRPCHTTPVerb = jrpcDefault): IJSONRPCResponse; overload;
    function HTTPResponse: IHTTPResponse;
    // Http headers handling
    procedure AddHTTPHeader(const aNetHeader: TNetHeader);
    procedure ClearHTTPHeaders;
    function HTTPHeadersCount: Integer;
    function SetOnNeedClientCertificate(const aOnNeedClientCertificate: TNeedClientCertificateEvent)
      : IMVCJSONRPCExecutor;
    function SetOnValidateServerCertificate(const aOnValidateServerCertificate: TValidateCertificateEvent)
      : IMVCJSONRPCExecutor;
    function ConfigureHTTPClient(const aConfigProc: TProc<THTTPClient>): IMVCJSONRPCExecutor;
    //events
    //sync
    function SetOnReceiveData(const aOnReceiveData: TReceiveDataEvent): IMVCJSONRPCExecutor;
    function SetOnReceiveResponse(const aOnReceiveResponseProc: TProc<IJSONRPCObject, IJSONRPCObject>)
      : IMVCJSONRPCExecutor;
    function SetOnSendCommand(const aOnSendCommandProc: TProc<IJSONRPCObject>): IMVCJSONRPCExecutor;
    function SetOnReceiveHTTPResponse(const aOnReceiveHTTPResponse: TProc<IHTTPResponse>): IMVCJSONRPCExecutor;
    //end events

  end;

  IMVCJSONRPCExecutorAsync = interface
    ['{16E930C2-0318-48A3-9633-614FB5BF8BAE}']
    //async
    procedure ExecuteRequestAsync(
      const aJSONRPCRequest: IJSONRPCRequest;
      const AJSONRPCResponseHandler: TJSONRPCResponseHandlerProc;
      const AJSONRPCErrorHandler: TJSONRPCErrorHandlerProc = nil;
      const UseVerb: TJSONRPCHTTPVerb = jrpcDefault); overload;
    procedure ExecuteRequestAsync(
      const aLastEndPointSegment: string;
      const aJSONRPCRequest: IJSONRPCRequest;
      const AJSONRPCResponseHandler: TJSONRPCResponseHandlerProc;
      const AJSONRPCErrorHandler: TJSONRPCErrorHandlerProc = nil;
      const UseVerb: TJSONRPCHTTPVerb = jrpcDefault); overload;
    procedure ExecuteNotificationAsync(
      const aJSONRPCNotification: IJSONRPCNotification;
      const AJSONRPCErrorHandler: TJSONRPCErrorHandlerProc = nil;
      const UseVerb: TJSONRPCHTTPVerb = jrpcDefault); overload;
    procedure ExecuteNotificationAsync(
      const aLastEndPointSegment: string;
      const aJSONRPCNotification: IJSONRPCNotification;
      const AJSONRPCErrorHandler: TJSONRPCErrorHandlerProc = nil;
      const UseVerb: TJSONRPCHTTPVerb = jrpcDefault); overload;
    // end async
    // http headers handling
    procedure AddHTTPHeader(const aNetHeader: TNetHeader);
    procedure ClearHTTPHeaders;
    function HTTPHeadersCount: Integer;
    function SetOnNeedClientCertificate(const aOnNeedClientCertificate: TNeedClientCertificateEvent)
      : IMVCJSONRPCExecutor;
    function SetOnValidateServerCertificate(const aOnValidateServerCertificate: TValidateCertificateEvent)
      : IMVCJSONRPCExecutor;
    //events
    //async
    function SetOnReceiveResponseAsync(const aOnReceiveResponseAsyncProc: TProc<IJSONRPCObject, IJSONRPCObject>)
      : IMVCJSONRPCExecutor;
    function SetOnSendCommandAsync(const aOnSendCommandAsyncProc: TProc<IJSONRPCObject>): IMVCJSONRPCExecutor;
    function SetOnReceiveHTTPResponseAsync(const aOnReceiveHTTPResponseAsync: TProc<IHTTPResponse>): IMVCJSONRPCExecutor;
    function SetOnBeginAsyncRequest(const Proc: TProc): IMVCJSONRPCExecutor;
    function SetOnEndAsyncRequest(const Proc: TProc): IMVCJSONRPCExecutor;
    /// <summary>
    ///   Invoked internally just before each async requests/notifications.
    ///   Use it to customize properties and events of HTTP client used in async operations.
    /// </summary>
    function SetConfigureHTTPClientAsync(const aConfigProcAsync: TProc<THTTPClient>): IMVCJSONRPCExecutor;
    //end events
  end;


  TMVCJSONRPCExecutor = class(TInterfacedObject, IMVCJSONRPCExecutor, IMVCJSONRPCExecutorAsync)
  private
    fDefaultHTTPVerb: TJSONRPCHTTPVerb;
    fURL: string;
    fHTTP: THTTPClient;
    fRaiseExceptionOnError: Boolean;
    fHTTPRequestHeaders: TList<TNetHeader>;
    fHTTPResponse: IHTTPResponse;

    //the following sync events are used only with sync execution
    fOnReceiveResponse: TProc<IJSONRPCObject, IJSONRPCObject>;
    fOnReceiveHTTPResponse: TProc<IHTTPResponse>;
    fOnSendCommand: TProc<IJSONRPCObject>;
    //end sync events

    //the following async events are used only with async execution
    fOnReceiveHTTPResponseAsync: TProc<IHTTPResponse>;
    fOnSendCommandAsync: TProc<IJSONRPCObject>;
    fOnReceiveResponseAsync: TProc<IJSONRPCObject, IJSONRPCObject>;
    fOnBeginAsyncRequest: TProc;
    fOnEndAsyncRequest: TProc;
    fConfigProcAsync: TProc<THTTPClient>;
    //end async events
    function GetHTTPRequestHeaders: TList<TNetHeader>;
    procedure DoBeginAsyncRequest;
    procedure DoEndAsyncRequest;
  protected
    function GetQueryStringParameters(const aJSONRPCObject: IJSONRPCObject): String;
    function HTTPResponse: IHTTPResponse;

    //sync
    function InternalExecute(
      const aEndPoint: string;
      const aJSONRPCObject: IJSONRPCObject;
      const UseVerb: TJSONRPCHTTPVerb): IJSONRPCResponse;
    function ExecuteRequest(
      const aJSONRPCRequest: IJSONRPCRequest;
      const UseVerb: TJSONRPCHTTPVerb): IJSONRPCResponse; overload;
    function ExecuteRequest(
      const aLastEndPointSegment: string;
      const aJSONRPCRequest: IJSONRPCRequest;
      const UseVerb: TJSONRPCHTTPVerb): IJSONRPCResponse; overload;
    function ExecuteNotification(const aJSONRPCNotification: IJSONRPCNotification; const UseVerb: TJSONRPCHTTPVerb)
      : IJSONRPCResponse; overload;
    function ExecuteNotification(const aLastEndPointSegment: string; const aJSONRPCNotification: IJSONRPCNotification;
      const UseVerb: TJSONRPCHTTPVerb): IJSONRPCResponse; overload;
    //async
    procedure InternalExecuteAsync(
      const AEndPoint: string;
      const AJSONRPCObject: IJSONRPCObject;
      const AJSONRPCResponseHandler: TJSONRPCResponseHandlerProc;
      const AJSONRPCErrorHandler: TJSONRPCErrorHandlerProc;
      const UseVerb: TJSONRPCHTTPVerb);
    procedure ExecuteRequestAsync(
      const aJSONRPCRequest: IJSONRPCRequest;
      const AJSONRPCResponseHandler: TJSONRPCResponseHandlerProc;
      const AJSONRPCErrorHandler: TJSONRPCErrorHandlerProc;
      const UseVerb: TJSONRPCHTTPVerb); overload;
    procedure ExecuteRequestAsync(
      const aLastEndPointSegment: string;
      const AJSONRPCRequest: IJSONRPCRequest;
      const AJSONRPCResponseHandler: TJSONRPCResponseHandlerProc;
      const AJSONRPCErrorHandler: TJSONRPCErrorHandlerProc;
      const UseVerb: TJSONRPCHTTPVerb); overload;
    procedure ExecuteNotificationAsync(
      const AJSONRPCNotification: IJSONRPCNotification;
      const AJSONRPCErrorHandler: TJSONRPCErrorHandlerProc;
      const UseVerb: TJSONRPCHTTPVerb); overload;
    procedure ExecuteNotificationAsync(
      const ALastEndPointSegment: string;
      const AJSONRPCNotification: IJSONRPCNotification;
      const AJSONRPCErrorHandler: TJSONRPCErrorHandlerProc;
      const UseVerb: TJSONRPCHTTPVerb); overload;
    function SetOnBeginAsyncRequest(const Proc: TProc): IMVCJSONRPCExecutor;
    function SetOnEndAsyncRequest(const Proc: TProc): IMVCJSONRPCExecutor;
    // Http headers handling
    procedure AddHTTPHeader(const aNetHeader: TNetHeader);
    procedure ClearHTTPHeaders;
    function HTTPHeadersCount: Integer;
    //events
    //sync
    function SetOnReceiveData(const aOnReceiveData: TReceiveDataEvent): IMVCJSONRPCExecutor;
    function SetOnReceiveResponse(const aOnReceiveResponseProc: TProc<IJSONRPCObject, IJSONRPCObject>)
      : IMVCJSONRPCExecutor;
    function SetOnSendCommand(const aOnSendCommandProc: TProc<IJSONRPCObject>): IMVCJSONRPCExecutor;
    function SetOnNeedClientCertificate(const aOnNeedClientCertificate: TNeedClientCertificateEvent)
      : IMVCJSONRPCExecutor;
    function SetOnValidateServerCertificate(const aOnValidateServerCertificate: TValidateCertificateEvent)
      : IMVCJSONRPCExecutor;
    function SetOnReceiveHTTPResponse(const aOnReceiveHTTPResponse: TProc<IHTTPResponse>): IMVCJSONRPCExecutor;
    //async
    function SetOnReceiveResponseAsync(const aOnReceiveResponseAsyncProc: TProc<IJSONRPCObject, IJSONRPCObject>): IMVCJSONRPCExecutor;
    function SetOnSendCommandAsync(const aOnSendCommandAsyncProc: TProc<IJSONRPCObject>): IMVCJSONRPCExecutor;
    function SetOnReceiveHTTPResponseAsync(const aOnReceiveHTTPResponseAsync: TProc<IHTTPResponse>): IMVCJSONRPCExecutor;
    function SetConfigureHTTPClientAsync(const aConfigProcAsync: TProc<THTTPClient>): IMVCJSONRPCExecutor;
    //end events
    function ConfigureHTTPClient(const aConfigProc: TProc<THTTPClient>): IMVCJSONRPCExecutor;
  public
    constructor Create(const aURL: string;
      const aRaiseExceptionOnError: Boolean = True;
      const aDefaultHTTPVerb: TJSONRPCHTTPVerb = jrpcDefault); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  System.Classes, System.Types, MVCFramework.AsyncTask;

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

procedure TMVCJSONRPCExecutor.DoBeginAsyncRequest;
begin
  if Assigned(fOnBeginAsyncRequest) then
  begin
    TThread.Queue(nil,
      procedure
      begin
        fOnBeginAsyncRequest();
      end);
  end;
end;

procedure TMVCJSONRPCExecutor.DoEndAsyncRequest;
begin
  if Assigned(fOnEndAsyncRequest) then
  begin
    TThread.Queue(nil,
      procedure
      begin
        fOnEndAsyncRequest();
      end);
  end;
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

function TMVCJSONRPCExecutor.InternalExecute(
  const aEndPoint: string;
  const aJSONRPCObject: IJSONRPCObject;
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

procedure TMVCJSONRPCExecutor.InternalExecuteAsync(
  const AEndPoint: string;
  const AJSONRPCObject: IJSONRPCObject;
  const AJSONRPCResponseHandler: TJSONRPCResponseHandlerProc;
  const AJSONRPCErrorHandler: TJSONRPCErrorHandlerProc;
  const UseVerb: TJSONRPCHTTPVerb);
var
  lCustomHeaders: TNetHeaders;
  lProc: TProc;
begin
  lCustomHeaders := [];
  if Assigned(fHTTPRequestHeaders) then
  begin
    lCustomHeaders := fHTTPRequestHeaders.ToArray;
  end;

  lProc := procedure
  var
    lSS: TStringStream;
    lHttpResp: IHTTPResponse;
    lJSONRPCResponse: IJSONRPCResponse;
    lEx: Pointer;
    lExceptionAddress: Pointer;
    lHTTP: THTTPClient;
  begin
    lHTTP := THTTPClient.Create;
    try
      if Assigned(fConfigProcAsync) then
      begin
        fConfigProcAsync(lHttp);
      end;
      lHTTP.OnNeedClientCertificate := fHTTP.OnNeedClientCertificate;
      lHTTP.OnReceiveData := fHTTP.OnReceiveData;
      lHTTP.OnValidateServerCertificate := fHTTP.OnValidateServerCertificate;
      lSS := TStringStream.Create(AJSONRPCObject.AsJSONString, TEncoding.UTF8);
      try
        lSS.Position := 0;
        fHTTPResponse := nil;
        try
          DoBeginAsyncRequest;
          try
            if Assigned(fOnSendCommandAsync) then
            begin
              fOnSendCommandAsync(AJSONRPCObject);
            end;
            case UseVerb of
              jrpcPOST, jrpcDefault:
                begin
                  lHttpResp := lHTTP.Post(fURL + aEndPoint, lSS, nil,
                    [TNetHeader.Create('content-type', 'application/json;charset=utf8'), TNetHeader.Create('accept',
                    'application/json;charset=utf8')] + lCustomHeaders);
                end;
              jrpcGET:
                begin
                  lHttpResp := lHTTP.Get(fURL + aEndPoint + '?' + GetQueryStringParameters(AJSONRPCObject), nil,
                    [TNetHeader.Create('accept', 'application/json;charset=utf8')] + lCustomHeaders);
                end;
            end;

            if Assigned(fOnReceiveHTTPResponseAsync) then
            begin
              fOnReceiveHTTPResponseAsync(lHttpResp);
            end;
            lJSONRPCResponse := nil;

            if lHttpResp.StatusCode = HTTP_STATUS.NoContent then
            begin
              lJSONRPCResponse := TJSONRPCNullResponse.Create;
            end
            else
            begin
              if lHttpResp.HeaderValue['content-type'].Contains(TMVCMediaType.APPLICATION_JSON) then
              begin
                lJSONRPCResponse := TJSONRPCResponse.Create;
                lJSONRPCResponse.AsJSONString := lHttpResp.ContentAsString;
              end
              else
              begin
                raise EMVCJSONRPCProtocolException.CreateFmt(
                  'Expected content-type "%s", got "%s"',
                  [
                    TMVCMediaType.APPLICATION_JSON,
                    lHttpResp.HeaderValue['content-type']
                  ]);
              end;
            end;

            if Assigned(fOnReceiveResponseAsync) then
            begin
              fOnReceiveResponseAsync(aJSONRPCObject, lJSONRPCResponse);
            end;

            if Assigned(lJSONRPCResponse.Error) then
            begin
              raise EMVCJSONRPCRemoteException.Create(
                lJSONRPCResponse.Error.Code,
                lJSONRPCResponse.Error.ErrMessage,
                lJSONRPCResponse.Error.Data
                );
            end;
          finally
            DoEndAsyncRequest;
          end;
          if Assigned(AJSONRPCResponseHandler) then
          begin
            TThread.Queue(nil,
              procedure
              begin
                AJSONRPCResponseHandler(lJSONRPCResponse);
              end);
          end;
        except
          lEx := AcquireExceptionObject;
          lExceptionAddress := ExceptAddr;
          TThread.Queue(nil,
            procedure
            var
              lCurrException: Exception;
            begin
              lCurrException := Exception(lEx);
              try
                if Assigned(AJSONRPCErrorHandler) then
                  AJSONRPCErrorHandler(lCurrException)
                else
                  gDefaultTaskErrorHandler(lCurrException, lExceptionAddress);
              finally
                FreeAndNil(lCurrException);
              end;
            end);
        end;
      finally
        lSS.Free;
      end;
    finally
      lHTTP.Free;
    end;
  end; //end proc
  TThread.CreateAnonymousThread(lProc).Start;
end;

function TMVCJSONRPCExecutor.HTTPResponse: IHTTPResponse;
begin
  Result := fHTTPResponse;
end;

function TMVCJSONRPCExecutor.SetConfigureHTTPClientAsync(
  const aConfigProcAsync: TProc<THTTPClient>): IMVCJSONRPCExecutor;
begin
  fConfigProcAsync := aConfigProcAsync;
end;

function TMVCJSONRPCExecutor.SetOnBeginAsyncRequest(
  const Proc: TProc): IMVCJSONRPCExecutor;
begin
  fOnBeginAsyncRequest := Proc;
  Result := Self;
end;

function TMVCJSONRPCExecutor.SetOnEndAsyncRequest(
  const Proc: TProc): IMVCJSONRPCExecutor;
begin
  fOnEndAsyncRequest := Proc;
  Result := Self;
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

function TMVCJSONRPCExecutor.SetOnReceiveHTTPResponseAsync(
  const aOnReceiveHTTPResponseAsync: TProc<IHTTPResponse>): IMVCJSONRPCExecutor;
begin
  fOnReceiveHTTPResponseAsync := aOnReceiveHTTPResponseAsync;
  Result := Self;
end;

function TMVCJSONRPCExecutor.SetOnReceiveResponse(const aOnReceiveResponseProc: TProc<IJSONRPCObject, IJSONRPCObject>)
  : IMVCJSONRPCExecutor;
begin
  fOnReceiveResponse := aOnReceiveResponseProc;
  Result := Self;
end;

function TMVCJSONRPCExecutor.SetOnReceiveResponseAsync(
  const aOnReceiveResponseAsyncProc: TProc<IJSONRPCObject, IJSONRPCObject>): IMVCJSONRPCExecutor;
begin
  fOnReceiveResponseAsync := aOnReceiveResponseAsyncProc;
  Result := Self;
end;

function TMVCJSONRPCExecutor.SetOnSendCommand(const aOnSendCommandProc: TProc<IJSONRPCObject>): IMVCJSONRPCExecutor;
begin
  fOnSendCommand := aOnSendCommandProc;
  Result := Self;
end;

function TMVCJSONRPCExecutor.SetOnSendCommandAsync(
  const aOnSendCommandAsyncProc: TProc<IJSONRPCObject>): IMVCJSONRPCExecutor;
begin
  fOnSendCommandAsync := aOnSendCommandAsyncProc;
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

procedure TMVCJSONRPCExecutor.ExecuteNotificationAsync(
  const aJSONRPCNotification: IJSONRPCNotification;
  const AJSONRPCErrorHandler: TJSONRPCErrorHandlerProc;
  const UseVerb: TJSONRPCHTTPVerb);
begin
  ExecuteNotificationAsync('', aJSONRPCNotification, AJSONRPCErrorHandler, UseVerb);
end;

procedure TMVCJSONRPCExecutor.ExecuteNotificationAsync(
  const aLastEndPointSegment: string;
  const aJSONRPCNotification: IJSONRPCNotification;
  const AJSONRPCErrorHandler: TJSONRPCErrorHandlerProc;
  const UseVerb: TJSONRPCHTTPVerb);
begin
  InternalExecuteAsync(aLastEndPointSegment, aJSONRPCNotification as TJSONRPCObject, nil, AJSONRPCErrorHandler, UseVerb);
end;

function TMVCJSONRPCExecutor.ExecuteRequest(const aLastEndPointSegment: string; const aJSONRPCRequest: IJSONRPCRequest;
  const UseVerb: TJSONRPCHTTPVerb): IJSONRPCResponse;
begin
  Result := InternalExecute(aLastEndPointSegment, aJSONRPCRequest, UseVerb);
end;

procedure TMVCJSONRPCExecutor.ExecuteRequestAsync(
  const aLastEndPointSegment: string; const aJSONRPCRequest: IJSONRPCRequest;
  const AJSONRPCResponseHandler: TJSONRPCResponseHandlerProc;
  const AJSONRPCErrorHandler: TJSONRPCErrorHandlerProc;
  const UseVerb: TJSONRPCHTTPVerb);
begin
  InternalExecuteAsync(aLastEndPointSegment, aJSONRPCRequest, AJSONRPCResponseHandler, AJSONRPCErrorHandler, UseVerb);
end;

procedure TMVCJSONRPCExecutor.ExecuteRequestAsync(
  const AJSONRPCRequest: IJSONRPCRequest;
  const AJSONRPCResponseHandler: TJSONRPCResponseHandlerProc;
  const AJSONRPCErrorHandler: TJSONRPCErrorHandlerProc;
  const UseVerb: TJSONRPCHTTPVerb);
begin
  ExecuteRequestAsync('', aJSONRPCRequest, AJSONRPCResponseHandler, AJSONRPCErrorHandler, UseVerb);
end;

end.
