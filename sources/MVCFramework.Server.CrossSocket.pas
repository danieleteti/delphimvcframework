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

unit MVCFramework.Server.CrossSocket;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  Net.CrossHttpServer,
  MVCFramework, MVCFramework.Server.Intf, MVCFramework.Commons;

type
  /// <summary>
  /// IMVCServer implementation using Delphi-Cross-Socket.
  /// High performance HTTP server with IOCP (Windows), epoll (Linux),
  /// kqueue (macOS) backends. Native keep-alive, SSL support.
  /// </summary>
  TMVCCrossSocketServer = class(TInterfacedObject, IMVCServer)
  private
    FHttpServer: ICrossHttpServer;
    FEngine: TMVCEngine;
    FPort: Integer;
    FHost: string;
    FMaxConnections: Integer;
    FKeepAlive: Boolean;
    FListenQueue: Integer;
    procedure HandleRequest(const Sender: TObject;
      const ARequest: ICrossHttpRequest;
      const AResponse: ICrossHttpResponse;
      var AHandled: Boolean);
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
    procedure Listen(APort: Integer = 8080; const AHost: string = '0.0.0.0');
    procedure Stop;
    function IsRunning: Boolean;
  end;

implementation

uses
  MVCFramework.CrossSocket.Request, MVCFramework.CrossSocket.Response;

{ TMVCCrossSocketServer }

constructor TMVCCrossSocketServer.Create;
begin
  inherited Create;
  FHttpServer := TCrossHttpServer.Create(0, False);
  FHttpServer.OnRequest := HandleRequest;
  FEngine := nil;
  FPort := 8080;
  FHost := '0.0.0.0';
  FMaxConnections := 0;
  FKeepAlive := True;
  FListenQueue := 200;
end;

constructor TMVCCrossSocketServer.Create(AEngine: TMVCEngine);
begin
  Create;
  FEngine := AEngine;
end;

destructor TMVCCrossSocketServer.Destroy;
begin
  if IsRunning then
    Stop;
  FHttpServer := nil;
  inherited;
end;

procedure TMVCCrossSocketServer.Listen(APort: Integer; const AHost: string);
begin
  FPort := APort;
  FHost := AHost;

  if not Assigned(FEngine) then
    raise EMVCException.Create('Engine not assigned');

  FHttpServer.Addr := FHost;
  FHttpServer.Port := Word(FPort);
  FHttpServer.Active := True;
end;

procedure TMVCCrossSocketServer.Stop;
begin
  FHttpServer.Active := False;
end;

function TMVCCrossSocketServer.IsRunning: Boolean;
begin
  Result := FHttpServer.Active;
end;

procedure TMVCCrossSocketServer.HandleRequest(const Sender: TObject;
  const ARequest: ICrossHttpRequest;
  const AResponse: ICrossHttpResponse;
  var AHandled: Boolean);
var
  LRequest: TMVCCrossSocketRequest;
  LResponse: TMVCCrossSocketResponse;
begin
  AHandled := True;
  LRequest := TMVCCrossSocketRequest.Create(ARequest.Connection, ARequest, FEngine.Serializers);
  try
    LResponse := TMVCCrossSocketResponse.Create(ARequest.Connection, AResponse);
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

procedure TMVCCrossSocketServer.SetEngine(AEngine: TMVCEngine);
begin
  FEngine := AEngine;
end;

function TMVCCrossSocketServer.GetEngine: TMVCEngine;
begin
  Result := FEngine;
end;

function TMVCCrossSocketServer.GetPort: Integer;
begin
  Result := FPort;
end;

function TMVCCrossSocketServer.GetHost: string;
begin
  Result := FHost;
end;

procedure TMVCCrossSocketServer.SetMaxConnections(AValue: Integer);
begin
  FMaxConnections := AValue;
end;

function TMVCCrossSocketServer.GetMaxConnections: Integer;
begin
  Result := FMaxConnections;
end;

procedure TMVCCrossSocketServer.SetKeepAlive(AValue: Boolean);
begin
  FKeepAlive := AValue;
end;

function TMVCCrossSocketServer.GetKeepAlive: Boolean;
begin
  Result := FKeepAlive;
end;

procedure TMVCCrossSocketServer.SetListenQueue(AValue: Integer);
begin
  FListenQueue := AValue;
end;

function TMVCCrossSocketServer.GetListenQueue: Integer;
begin
  Result := FListenQueue;
end;

end.
