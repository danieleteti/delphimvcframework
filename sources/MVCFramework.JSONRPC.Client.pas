// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2018 Daniele Teti and the DMVCFramework Team
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
  System.Generics.Collections, MVCFramework.Commons;

type
  IMVCJSONRPCExecutor = interface
    ['{55415094-9D28-4707-AEC5-5FCF925E82BC}']
    function ExecuteRequest(const aJSONRPCRequest: IJSONRPCRequest): IJSONRPCResponse;
    procedure ExecuteNotification(const aJSONRPCNotification: IJSONRPCNotification);
    // Http headers handling
    procedure AddHTTPHeader(const aNetHeader: TNetHeader);
    procedure ClearHTTPHeaders;
    function HTTPHeadersCount: Integer;
  end;

  TMVCJSONRPCExecutor = class(TInterfacedObject, IMVCJSONRPCExecutor)
  private
    FURL: string;
    FHTTP: THTTPClient;
    FRaiseExceptionOnError: Boolean;
    FHTTPRequestHeaders: TList<TNetHeader>;
    function GetHTTPRequestHeaders: TList<TNetHeader>;
  protected
    function InternalExecute(const aJSONRPCObject: IJSONRPCObject): IJSONRPCResponse;
  public
    constructor Create(const aURL: string; const aRaiseExceptionOnError: Boolean = True); virtual;
    destructor Destroy; override;
    function ExecuteRequest(const aJSONRPCRequest: IJSONRPCRequest): IJSONRPCResponse;
    procedure ExecuteNotification(const aJSONRPCNotification: IJSONRPCNotification);
    // Http headers handling
    procedure AddHTTPHeader(const aNetHeader: TNetHeader);
    procedure ClearHTTPHeaders;
    function HTTPHeadersCount: Integer;
  end;

implementation

uses
  System.Classes,
  System.SysUtils;

procedure JSONRPCExec(const aJSONRPCURL: string; const aJSONRPCRequest: IJSONRPCRequest; out aJSONRPCResponse: IJSONRPCResponse);
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
      lHttpResp := lHTTP.Post('http://localhost:8080/jsonrpc', lSS, nil, [TNetHeader.Create('content-type', 'application/json'),
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
  if Assigned(FHTTPRequestHeaders) then
  begin
    FHTTPRequestHeaders.Clear;
  end;
end;

constructor TMVCJSONRPCExecutor.Create(const aURL: string; const aRaiseExceptionOnError: Boolean = True);
begin
  inherited Create;
  FRaiseExceptionOnError := aRaiseExceptionOnError;
  FURL := aURL;
  FHTTP := THTTPClient.Create;
  FHTTPRequestHeaders := nil;
end;

destructor TMVCJSONRPCExecutor.Destroy;
begin
  FHTTP.Free;
  FHTTPRequestHeaders.Free;
  inherited;
end;

procedure TMVCJSONRPCExecutor.ExecuteNotification(const aJSONRPCNotification: IJSONRPCNotification);
begin
  if InternalExecute(aJSONRPCNotification as TJSONRPCObject) <> nil then
    raise EMVCJSONRPCException.Create('A "notification" cannot returns a response. Use ExecuteRequest instead.');
end;

function TMVCJSONRPCExecutor.ExecuteRequest(const aJSONRPCRequest: IJSONRPCRequest): IJSONRPCResponse;
begin
  Result := InternalExecute(aJSONRPCRequest);
end;

function TMVCJSONRPCExecutor.GetHTTPRequestHeaders: TList<TNetHeader>;
begin
  if not Assigned(FHTTPRequestHeaders) then
  begin
    FHTTPRequestHeaders := TList<TNetHeader>.Create;
  end;
  Result := FHTTPRequestHeaders;
end;

function TMVCJSONRPCExecutor.HTTPHeadersCount: Integer;
begin
  if Assigned(FHTTPRequestHeaders) then
  begin
    Result := FHTTPRequestHeaders.Count;
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
  if Assigned(FHTTPRequestHeaders) then
  begin
    lCustomHeaders := FHTTPRequestHeaders.ToArray;
  end;

  Result := nil;
  lSS := TStringStream.Create(aJSONRPCObject.AsJSONString);
  try
    lSS.Position := 0;
    lHttpResp := FHTTP.Post(FURL, lSS, nil, [TNetHeader.Create('content-type', 'application/json'),
      TNetHeader.Create('accept', 'application/json')] + lCustomHeaders);
    if (lHttpResp.StatusCode <> HTTP_STATUS.NoContent) then
    begin
      lJSONRPCResponse := TJSONRPCResponse.Create;
      lJSONRPCResponse.AsJSONString := lHttpResp.ContentAsString;
      if Assigned(lJSONRPCResponse.Error) and FRaiseExceptionOnError then
        raise Exception.CreateFmt('Error [%d]: %s', [lJSONRPCResponse.Error.Code, lJSONRPCResponse.Error.ErrMessage]);
      Result := lJSONRPCResponse;
    end;
  finally
    lSS.Free;
  end;
end;

end.
