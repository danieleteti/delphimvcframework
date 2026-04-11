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

unit MVCFramework.Server.Indy;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils, System.Classes,
  IdHTTPServer, IdContext, IdCustomHTTPServer, IdSocketHandle,
  MVCFramework, MVCFramework.Server.Intf, MVCFramework.Commons;

type
  TMVCIndyServer = class(TInterfacedObject, IMVCServer)
  private
    FHTTPServer: TIdHTTPServer;
    FEngine: TMVCEngine;
    FPort: Integer;
    FHost: string;
    FMaxConnections: Integer;
    FKeepAlive: Boolean;
    FListenQueue: Integer;
    procedure OnCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure OnCommandOther(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure InternalHandleRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure OnParseAuthentication(AContext: TIdContext;
      const AAuthType, AAuthData: String;
      var VUsername, VPassword: String; var VHandled: Boolean);
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
  MVCFramework.Indy.Request, MVCFramework.Indy.Response,
  MVCFramework.Logger;

{ TMVCIndyServer }

constructor TMVCIndyServer.Create;
begin
  inherited Create;
  FHTTPServer := TIdHTTPServer.Create(nil);
  FHTTPServer.OnCommandGet := OnCommandGet;
  FHTTPServer.OnCommandOther := OnCommandOther;
  FHTTPServer.OnParseAuthentication := OnParseAuthentication;
  FEngine := nil;
  FPort := 8080;
  FHost := '0.0.0.0';
  FMaxConnections := 4096;
  FKeepAlive := True;
  FListenQueue := 200;
end;

constructor TMVCIndyServer.Create(AEngine: TMVCEngine);
begin
  Create;
  FEngine := AEngine;
end;

destructor TMVCIndyServer.Destroy;
begin
  if FHTTPServer.Active then
    Stop;
  FHTTPServer.Free;
  inherited;
end;

procedure TMVCIndyServer.Listen(APort: Integer; const AHost: string);
begin
  FPort := APort;
  FHost := AHost;

  FHTTPServer.DefaultPort := FPort;
  FHTTPServer.MaxConnections := FMaxConnections;
  FHTTPServer.KeepAlive := FKeepAlive;
  FHTTPServer.ListenQueue := FListenQueue;

  FHTTPServer.Active := True;
end;

procedure TMVCIndyServer.Stop;
begin
  FHTTPServer.Active := False;
end;

function TMVCIndyServer.IsRunning: Boolean;
begin
  Result := FHTTPServer.Active;
end;

procedure TMVCIndyServer.OnCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  InternalHandleRequest(AContext, ARequestInfo, AResponseInfo);
end;

procedure TMVCIndyServer.OnCommandOther(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  InternalHandleRequest(AContext, ARequestInfo, AResponseInfo);
end;

procedure TMVCIndyServer.OnParseAuthentication(AContext: TIdContext;
  const AAuthType, AAuthData: String;
  var VUsername, VPassword: String; var VHandled: Boolean);
begin
  // Let the framework handle authentication via middleware
  VHandled := True;
end;

procedure TMVCIndyServer.InternalHandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  LRequest: TMVCIndyDirectRequest;
  LResponse: TMVCIndyDirectResponse;
begin
  if not Assigned(FEngine) then
    raise EMVCException.Create('Engine not assigned');

  LRequest := TMVCIndyDirectRequest.Create(AContext, ARequestInfo, FEngine.Serializers);
  try
    LResponse := TMVCIndyDirectResponse.Create(AContext, AResponseInfo);
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

procedure TMVCIndyServer.SetEngine(AEngine: TMVCEngine);
begin
  FEngine := AEngine;
end;

function TMVCIndyServer.GetEngine: TMVCEngine;
begin
  Result := FEngine;
end;

function TMVCIndyServer.GetPort: Integer;
begin
  Result := FPort;
end;

function TMVCIndyServer.GetHost: string;
begin
  Result := FHost;
end;

procedure TMVCIndyServer.SetMaxConnections(AValue: Integer);
begin
  FMaxConnections := AValue;
end;

function TMVCIndyServer.GetMaxConnections: Integer;
begin
  Result := FMaxConnections;
end;

procedure TMVCIndyServer.SetKeepAlive(AValue: Boolean);
begin
  FKeepAlive := AValue;
end;

function TMVCIndyServer.GetKeepAlive: Boolean;
begin
  Result := FKeepAlive;
end;

procedure TMVCIndyServer.SetListenQueue(AValue: Integer);
begin
  FListenQueue := AValue;
end;

function TMVCIndyServer.GetListenQueue: Integer;
begin
  Result := FListenQueue;
end;

end.
