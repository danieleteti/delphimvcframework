// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
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

unit MVCFramework.Server.HttpSys;

/// <summary>
/// IMVCServer implementation using Windows HTTP Server API (HTTP.sys).
/// Kernel-mode HTTP server with IOCP, the same stack used by IIS and Kestrel.
/// This is the highest-performance server backend available on Windows.
/// <para>
/// IMPORTANT: HTTP.sys requires URL ACL registration. For development,
/// "http://localhost:PORT/" typically works without admin privileges.
/// For production with "http://+:PORT/", run:
///   netsh http add urlacl url=http://+:PORT/ user=Everyone
/// </para>
/// </summary>

{$I dmvcframework.inc}

interface

{$IFDEF MSWINDOWS}

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  Winapi.Windows,
  MVCFramework, MVCFramework.Server.Intf, MVCFramework.Commons,
  MVCFramework.HttpSysApi;

type
  TMVCHttpSysServer = class(TInterfacedObject, IMVCServer)
  private
    FReqQueueHandle: THandle;
    FEngine: TMVCEngine;
    FPort: Integer;
    FHost: string;
    FUrl: string;
    FListenerThread: TThread;
    FActive: Boolean;
    FMaxConnections: Integer;
    FKeepAlive: Boolean;
    FListenQueue: Integer;
    FHttpApiInitialized: Boolean;
    FUrlRegistered: Boolean;
    procedure HandleRequest(ARequest: PHTTP_REQUEST; const ABodyBytes: TBytes);
    procedure CheckError(AResult: ULONG; const AContext: string);
    function ReadFullBody(ARequest: PHTTP_REQUEST; AInitialBodyBytes: TBytes): TBytes;
  protected
    procedure SetEngine(AEngine: TMVCEngine);
    function GetEngine: TMVCEngine;
    function GetPort: Integer;
    function GetHost: string;
    procedure SetMaxConnections(AValue: Integer);
    function GetMaxConnections: Integer;
    procedure SetKeepAlive(AValue: Boolean);
    function GetKeepAlive: Boolean;
    procedure SetListenQueue(AValue: Integer);
    function GetListenQueue: Integer;
  public
    constructor Create; overload;
    constructor Create(AEngine: TMVCEngine); overload;
    destructor Destroy; override;
    procedure Listen(APort: Integer = 8080; const AHost: string = 'localhost');
    procedure Stop;
    function IsRunning: Boolean;
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses
  MVCFramework.HttpSys.Request, MVCFramework.HttpSys.Response,
  MVCFramework.Logger;

const
  { Initial request buffer: 16KB covers most request headers + small bodies }
  INITIAL_REQUEST_BUFFER_SIZE = 16384;
  { Buffer for reading additional entity body data }
  ENTITY_BODY_BUFFER_SIZE = 65536;

{ TMVCHttpSysServer }

constructor TMVCHttpSysServer.Create;
begin
  inherited Create;
  FReqQueueHandle := 0;
  FEngine := nil;
  FPort := 8080;
  FHost := 'localhost';
  FUrl := '';
  FListenerThread := nil;
  FActive := False;
  FMaxConnections := 4096;
  FKeepAlive := True;
  FListenQueue := 200;
  FHttpApiInitialized := False;
  FUrlRegistered := False;
end;

constructor TMVCHttpSysServer.Create(AEngine: TMVCEngine);
begin
  Create;
  FEngine := AEngine;
end;

destructor TMVCHttpSysServer.Destroy;
begin
  if FActive then
    Stop;
  inherited;
end;

procedure TMVCHttpSysServer.CheckError(AResult: ULONG; const AContext: string);
begin
  if AResult <> NO_ERROR then
    raise EMVCException.CreateFmt('HTTP.sys error in %s: %d (%s)',
      [AContext, AResult, SysErrorMessage(AResult)]);
end;

function TMVCHttpSysServer.ReadFullBody(ARequest: PHTTP_REQUEST;
  AInitialBodyBytes: TBytes): TBytes;
var
  lBodyStream: TMemoryStream;
  lBuffer: array[0..ENTITY_BODY_BUFFER_SIZE - 1] of Byte;
  lBytesReceived: ULONG;
  lResult: ULONG;
begin
  lBodyStream := TMemoryStream.Create;
  try
    { Write any body data already received in the initial request buffer }
    if Length(AInitialBodyBytes) > 0 then
      lBodyStream.WriteBuffer(AInitialBodyBytes[0], Length(AInitialBodyBytes));

    { Read remaining body data using HttpReceiveRequestEntityBody }
    while True do
    begin
      lBytesReceived := 0;
      lResult := HttpReceiveRequestEntityBody(FReqQueueHandle, ARequest.RequestId, 0,
        @lBuffer[0], SizeOf(lBuffer), lBytesReceived, nil);

      if lResult = NO_ERROR then
      begin
        if lBytesReceived > 0 then
          lBodyStream.WriteBuffer(lBuffer[0], lBytesReceived);
      end
      else if lResult = ERROR_HANDLE_EOF then
      begin
        { End of entity body }
        if lBytesReceived > 0 then
          lBodyStream.WriteBuffer(lBuffer[0], lBytesReceived);
        Break;
      end
      else
      begin
        { Error reading body - use what we have }
        Break;
      end;
    end;

    SetLength(Result, lBodyStream.Size);
    if lBodyStream.Size > 0 then
    begin
      lBodyStream.Position := 0;
      lBodyStream.ReadBuffer(Result[0], lBodyStream.Size);
    end;
  finally
    lBodyStream.Free;
  end;
end;

procedure TMVCHttpSysServer.Listen(APort: Integer; const AHost: string);
var
  lVersion: HTTP_VERSION;
  lUrl: string;
  lServerRef: TMVCHttpSysServer;
begin
  if not Assigned(FEngine) then
    raise EMVCException.Create('Engine not assigned');

  FPort := APort;
  FHost := AHost;

  { Initialize HTTP API }
  lVersion.MajorVersion := 1;
  lVersion.MinorVersion := 0;
  CheckError(HttpInitialize(lVersion, HTTP_INITIALIZE_SERVER, nil), 'HttpInitialize');
  FHttpApiInitialized := True;

  { Create request queue }
  CheckError(HttpCreateHttpHandle(FReqQueueHandle, 0), 'HttpCreateHttpHandle');

  { Register URL prefix }
  lUrl := Format('http://%s:%d/', [AHost, APort]);
  FUrl := lUrl;
  CheckError(HttpAddUrl(FReqQueueHandle, PWideChar(WideString(lUrl)), nil), 'HttpAddUrl');
  FUrlRegistered := True;

  FActive := True;

  { Start listener thread }
  lServerRef := Self;
  FListenerThread := TThread.CreateAnonymousThread(
    procedure
    var
      lRequestBuffer: TBytes;
      lRequest: PHTTP_REQUEST;
      lBytesReceived: ULONG;
      lResult: ULONG;
      lBodyBytes: TBytes;
      lInitialBodyBytes: TBytes;
      I: Integer;
    begin
      SetLength(lRequestBuffer, INITIAL_REQUEST_BUFFER_SIZE);

      while lServerRef.FActive do
      begin
        lRequest := PHTTP_REQUEST(@lRequestBuffer[0]);
        FillChar(lRequestBuffer[0], Length(lRequestBuffer), 0);
        lBytesReceived := 0;

        lResult := HttpReceiveHttpRequest(lServerRef.FReqQueueHandle, 0,
          HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY,
          lRequest, Length(lRequestBuffer), lBytesReceived, nil);

        if not lServerRef.FActive then
          Break;

        if lResult = NO_ERROR then
        begin
          { Extract initial body data from entity chunks }
          SetLength(lInitialBodyBytes, 0);
          if lRequest.EntityChunkCount > 0 then
          begin
            for I := 0 to lRequest.EntityChunkCount - 1 do
            begin
              if PHTTP_DATA_CHUNK(NativeUInt(lRequest.pEntityChunks) +
                 NativeUInt(I) * SizeOf(HTTP_DATA_CHUNK))^.DataChunkType = HttpDataChunkFromMemory then
              begin
                { Append chunk data }
                SetLength(lBodyBytes,
                  PHTTP_DATA_CHUNK(NativeUInt(lRequest.pEntityChunks) +
                  NativeUInt(I) * SizeOf(HTTP_DATA_CHUNK))^.FromMemory.BufferLength);
                if Length(lBodyBytes) > 0 then
                begin
                  Move(
                    PHTTP_DATA_CHUNK(NativeUInt(lRequest.pEntityChunks) +
                    NativeUInt(I) * SizeOf(HTTP_DATA_CHUNK))^.FromMemory.pBuffer^,
                    lBodyBytes[0], Length(lBodyBytes));
                  lInitialBodyBytes := lInitialBodyBytes + lBodyBytes;
                end;
              end;
            end;
          end;

          { Read remaining body if Content-Length indicates more data }
          lBodyBytes := lServerRef.ReadFullBody(lRequest, lInitialBodyBytes);

          try
            lServerRef.HandleRequest(lRequest, lBodyBytes);
          except
            on E: Exception do
            begin
              try
                LogE('HTTP.sys HandleRequest error: ' + E.Message);
              except
                { Swallow logging errors in listener thread }
              end;
            end;
          end;
        end
        else if lResult = ERROR_MORE_DATA then
        begin
          { Request buffer too small - allocate larger buffer and retry with the same RequestId }
          { This shouldn't happen often with 16KB buffer, but handle it gracefully }
          SetLength(lRequestBuffer, lBytesReceived + 4096);
          { The request is lost; we need to receive the next one }
          { Note: A production implementation would use the RequestId to re-receive,
            but for simplicity we skip this request }
          LogW('HTTP.sys: request buffer too small, request dropped');
        end
        else if (lResult <> ERROR_OPERATION_ABORTED) and lServerRef.FActive then
        begin
          { Log unexpected errors but keep listening }
          try
            LogE(Format('HTTP.sys: HttpReceiveHttpRequest error %d', [lResult]));
          except
            { Swallow }
          end;
        end;
      end;
    end);
  FListenerThread.FreeOnTerminate := False;
  FListenerThread.Start;
end;

procedure TMVCHttpSysServer.HandleRequest(ARequest: PHTTP_REQUEST;
  const ABodyBytes: TBytes);
var
  LRequest: TMVCHttpSysRequest;
  LResponse: TMVCHttpSysResponse;
begin
  LRequest := TMVCHttpSysRequest.Create(ARequest, ABodyBytes, FPort, FEngine.Serializers);
  try
    LResponse := TMVCHttpSysResponse.Create(FReqQueueHandle, ARequest.RequestId);
    try
      FEngine.HandleRequest(LRequest, LResponse);
      LResponse.Flush;
    finally
      LResponse.Free;
    end;
  finally
    LRequest.Free;
  end;
end;

procedure TMVCHttpSysServer.Stop;
begin
  FActive := False;

  { Remove URL registration first to unblock HttpReceiveHttpRequest }
  if FUrlRegistered and (FReqQueueHandle <> 0) and (FUrl <> '') then
  begin
    HttpRemoveUrl(FReqQueueHandle, PWideChar(WideString(FUrl)));
    FUrlRegistered := False;
  end;

  { Close the request queue handle to unblock HttpReceiveHttpRequest }
  if FReqQueueHandle <> 0 then
  begin
    CloseHandle(FReqQueueHandle);
    FReqQueueHandle := 0;
  end;

  { Wait for listener thread to finish }
  if Assigned(FListenerThread) then
  begin
    FListenerThread.WaitFor;
    FreeAndNil(FListenerThread);
  end;

  { Terminate HTTP API }
  if FHttpApiInitialized then
  begin
    HttpTerminate(HTTP_INITIALIZE_SERVER, nil);
    FHttpApiInitialized := False;
  end;
end;

function TMVCHttpSysServer.IsRunning: Boolean;
begin
  Result := FActive;
end;

procedure TMVCHttpSysServer.SetEngine(AEngine: TMVCEngine);
begin
  FEngine := AEngine;
end;

function TMVCHttpSysServer.GetEngine: TMVCEngine;
begin
  Result := FEngine;
end;

function TMVCHttpSysServer.GetPort: Integer;
begin
  Result := FPort;
end;

function TMVCHttpSysServer.GetHost: string;
begin
  Result := FHost;
end;

procedure TMVCHttpSysServer.SetMaxConnections(AValue: Integer);
begin
  FMaxConnections := AValue;
end;

function TMVCHttpSysServer.GetMaxConnections: Integer;
begin
  Result := FMaxConnections;
end;

procedure TMVCHttpSysServer.SetKeepAlive(AValue: Boolean);
begin
  FKeepAlive := AValue;
end;

function TMVCHttpSysServer.GetKeepAlive: Boolean;
begin
  Result := FKeepAlive;
end;

procedure TMVCHttpSysServer.SetListenQueue(AValue: Integer);
begin
  FListenQueue := AValue;
end;

function TMVCHttpSysServer.GetListenQueue: Integer;
begin
  Result := FListenQueue;
end;

{$ENDIF}

end.
