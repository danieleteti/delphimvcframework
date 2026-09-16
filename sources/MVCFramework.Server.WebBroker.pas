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

unit MVCFramework.Server.WebBroker;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils, System.Classes,
  IdContext, IdGlobal,
  MVCFramework, MVCFramework.Server.Intf, MVCFramework.Commons;

type
  /// <summary>
  /// IMVCServer implementation using WebBroker (TIdHTTPWebBrokerBridge).
  /// WebBroker manages the TWebModule lifecycle internally. This server
  /// accepts two separate callbacks per request cycle:
  ///   AConfigAction   - called during TMVCEngine.Create to set config values
  ///                     (before the config is frozen); may be nil for defaults.
  ///   AEngineConfigProc - called after Create to add controllers/middleware.
  /// </summary>
  TMVCWebBrokerServer = class(TInterfacedObject, IMVCServer)
  private
    FBridge: TObject; // TIdHTTPWebBrokerBridge — kept opaque to avoid implicit package import
    FEngine: TMVCEngine;
    FPort: Integer;
    FHost: string;
    FMaxConnections: Integer;
    FKeepAlive: Boolean;
    FListenQueue: Integer;
    FUseHTTPS: Boolean;
    FCertFile: string;
    FKeyFile: string;
    FRootCertFile: string;
    FCertPassword: string;
    FHTTPSConfigurator: TMVCHTTPSConfigurator;
    procedure OnParseAuthentication(AContext: TIdContext;
      const AAuthType, AAuthData: String;
      var VUsername, VPassword: String; var VHandled: Boolean);
    procedure OnQuerySSLPort(APort: TIdPort; var VUseSSL: Boolean);
    procedure ConfigureHTTPS;
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
    procedure SetUseHTTPS(AValue: Boolean);
    function GetUseHTTPS: Boolean;
    procedure SetCertFile(const AValue: string);
    function GetCertFile: string;
    procedure SetKeyFile(const AValue: string);
    function GetKeyFile: string;
    procedure SetRootCertFile(const AValue: string);
    function GetRootCertFile: string;
    procedure SetCertPassword(const AValue: string);
    function GetCertPassword: string;
    procedure SetHTTPSConfigurator(AValue: TMVCHTTPSConfigurator);
    function GetHTTPSConfigurator: TMVCHTTPSConfigurator;
  public
    constructor Create(AConfigAction: TProc<TMVCConfig>; AEngineConfigProc: TMVCEngineConfigProc = nil);
    destructor Destroy; override;
    procedure Listen(APort: Integer = 8080; const AHost: string = '0.0.0.0');
    procedure Stop;
    procedure RunAndWait(APort: Integer = 8080; const AHost: string = '0.0.0.0');
    function IsRunning: Boolean;
    /// <summary>
    /// Underlying TIdHTTPWebBrokerBridge as TObject. HTTPS providers cast it
    /// to TIdHTTPWebBrokerBridge to access engine-specific knobs — application
    /// code should not touch it directly.
    /// </summary>
    property Bridge: TObject read FBridge;
  end;

implementation

{$R MVCFramework.Server.WebBroker.dfm}

uses
  IdHTTPWebBrokerBridge,
  Web.HTTPApp, Web.WebReq,
  MVCFramework.Signal;

type
  TMVCAutoWebModule = class(TWebModule)
  private
    FEngine: TMVCEngine;
  protected
    procedure Loaded; override;
  public
    destructor Destroy; override;
  end;

var
  _ConfigAction: TProc<TMVCConfig>;
  _EngineConfigProc: TMVCEngineConfigProc;

{ Typed accessor — avoids repeating the cast at every FBridge access site. }
function BridgeOf(AServer: TMVCWebBrokerServer): TIdHTTPWebBrokerBridge; inline;
begin
  Result := TIdHTTPWebBrokerBridge(AServer.Bridge);
end;

procedure TMVCAutoWebModule.Loaded;
begin
  inherited;
  FEngine := TMVCEngine.Create(Self, _ConfigAction);
  if Assigned(_EngineConfigProc) then
    _EngineConfigProc(FEngine);
end;

destructor TMVCAutoWebModule.Destroy;
begin
  FEngine.Free;
  inherited;
end;

{ TMVCWebBrokerServer }

constructor TMVCWebBrokerServer.Create(AConfigAction: TProc<TMVCConfig>; AEngineConfigProc: TMVCEngineConfigProc);
var
  LBridge: TIdHTTPWebBrokerBridge;
begin
  inherited Create;
  _ConfigAction := AConfigAction;
  _EngineConfigProc := AEngineConfigProc;
  LBridge := TIdHTTPWebBrokerBridge.Create(nil);
  LBridge.OnParseAuthentication := OnParseAuthentication;
  FBridge := LBridge;
  FEngine := nil;
  FPort := 8080;
  FHost := '0.0.0.0';
  FMaxConnections := 4096;
  FKeepAlive := True;
  FListenQueue := 200;
  FUseHTTPS := False;
end;

destructor TMVCWebBrokerServer.Destroy;
begin
  if BridgeOf(Self).Active then
    Stop;
  FBridge.Free;
  _ConfigAction := nil;
  _EngineConfigProc := nil;
  inherited;
end;

procedure TMVCWebBrokerServer.ConfigureHTTPS;
begin
  if not FUseHTTPS then
    Exit;
  if (FCertFile = '') or (FKeyFile = '') then
    raise EMVCException.Create(
      'TMVCWebBrokerServer: HTTPS enabled but CertFile/KeyFile not set');
  if not Assigned(FHTTPSConfigurator) then
    raise EMVCException.Create(
      'TMVCWebBrokerServer: HTTPS requested but HTTPSConfigurator not assigned. ' +
      'Use a provider unit, e.g.:' + sLineBreak +
      '  uses MVCFramework.Server.HTTPS.TaurusTLS;' + sLineBreak +
      '  LServer.HTTPSConfigurator := TaurusTLSWebBrokerConfigurator;');
  FHTTPSConfigurator(Self);
  // See TMVCIndyServer.ConfigureHTTPS: force TLS on every port, not just 443.
  BridgeOf(Self).OnQuerySSLPort := OnQuerySSLPort;
end;

procedure TMVCWebBrokerServer.Listen(APort: Integer; const AHost: string);
var
  LBridge: TIdHTTPWebBrokerBridge;
begin
  FPort := APort;
  FHost := AHost;

  if not Assigned(_EngineConfigProc) then
    raise EMVCException.Create('Engine configuration procedure not assigned');

  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := TMVCAutoWebModule;
  WebRequestHandler.CacheConnections := True;
  WebRequestHandler.MaxConnections := FMaxConnections;

  LBridge := BridgeOf(Self);
  LBridge.DefaultPort := FPort;
  LBridge.MaxConnections := FMaxConnections;
  LBridge.ListenQueue := FListenQueue;

  ConfigureHTTPS;

  LBridge.Active := True;
end;

procedure TMVCWebBrokerServer.Stop;
begin
  BridgeOf(Self).Active := False;
end;

function TMVCWebBrokerServer.IsRunning: Boolean;
begin
  Result := BridgeOf(Self).Active;
end;

procedure TMVCWebBrokerServer.RunAndWait(APort: Integer; const AHost: string);
begin
  Listen(APort, AHost);
  try
    WaitForTerminationSignal;
    EnterInShutdownState;
  finally
    Stop;
  end;
end;

procedure TMVCWebBrokerServer.OnParseAuthentication(AContext: TIdContext;
  const AAuthType, AAuthData: String;
  var VUsername, VPassword: String; var VHandled: Boolean);
begin
  VHandled := True;
end;

procedure TMVCWebBrokerServer.OnQuerySSLPort(APort: TIdPort; var VUseSSL: Boolean);
begin
  // HTTPS enabled for the whole server: negotiate TLS on every port, not just 443.
  VUseSSL := True;
end;

procedure TMVCWebBrokerServer.SetEngine(AEngine: TMVCEngine);
begin
  FEngine := AEngine;
end;

function TMVCWebBrokerServer.GetEngine: TMVCEngine;
begin
  Result := FEngine;
end;

function TMVCWebBrokerServer.GetPort: Integer;
begin
  Result := FPort;
end;

function TMVCWebBrokerServer.GetHost: string;
begin
  Result := FHost;
end;

procedure TMVCWebBrokerServer.SetMaxConnections(AValue: Integer);
begin
  FMaxConnections := AValue;
end;

function TMVCWebBrokerServer.GetMaxConnections: Integer;
begin
  Result := FMaxConnections;
end;

procedure TMVCWebBrokerServer.SetKeepAlive(AValue: Boolean);
begin
  FKeepAlive := AValue;
end;

function TMVCWebBrokerServer.GetKeepAlive: Boolean;
begin
  Result := FKeepAlive;
end;

procedure TMVCWebBrokerServer.SetListenQueue(AValue: Integer);
begin
  FListenQueue := AValue;
end;

function TMVCWebBrokerServer.GetListenQueue: Integer;
begin
  Result := FListenQueue;
end;

procedure TMVCWebBrokerServer.SetUseHTTPS(AValue: Boolean);
begin
  FUseHTTPS := AValue;
end;

function TMVCWebBrokerServer.GetUseHTTPS: Boolean;
begin
  Result := FUseHTTPS;
end;

procedure TMVCWebBrokerServer.SetCertFile(const AValue: string);
begin
  FCertFile := AValue;
end;

function TMVCWebBrokerServer.GetCertFile: string;
begin
  Result := FCertFile;
end;

procedure TMVCWebBrokerServer.SetKeyFile(const AValue: string);
begin
  FKeyFile := AValue;
end;

function TMVCWebBrokerServer.GetKeyFile: string;
begin
  Result := FKeyFile;
end;

procedure TMVCWebBrokerServer.SetRootCertFile(const AValue: string);
begin
  FRootCertFile := AValue;
end;

function TMVCWebBrokerServer.GetRootCertFile: string;
begin
  Result := FRootCertFile;
end;

procedure TMVCWebBrokerServer.SetCertPassword(const AValue: string);
begin
  FCertPassword := AValue;
end;

function TMVCWebBrokerServer.GetCertPassword: string;
begin
  Result := FCertPassword;
end;

procedure TMVCWebBrokerServer.SetHTTPSConfigurator(AValue: TMVCHTTPSConfigurator);
begin
  FHTTPSConfigurator := AValue;
end;

function TMVCWebBrokerServer.GetHTTPSConfigurator: TMVCHTTPSConfigurator;
begin
  Result := FHTTPSConfigurator;
end;

end.
