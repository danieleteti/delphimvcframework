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
// ***************************************************************************
//
// SSL provider that wires TaurusTLS (OpenSSL 1.1.1+) into IMVCServer
// instances backed by Indy or WebBroker.
//
// Add this unit to the application's uses clause ONLY when HTTPS is needed.
// Server units (MVCFramework.Server.Indy / WebBroker) have no compile-time
// dependency on TaurusTLS — projects that don't use HTTPS don't pull it.
//
// Usage from the program:
//
//   uses
//     MVCFramework.Server.HTTPS.TaurusTLS;
//   ...
//   LServer := TMVCServerFactory.Build(sktIndyDirect, LEngine);
//   LServer.HTTPSConfigurator := TaurusTLSIndyConfigurator;   // <-- visible
//   LServer.UseHTTPS := True;
//   LServer.CertFile := '...';
//   LServer.KeyFile  := '...';
//   LServer.Listen(443);
//
// To swap TLS stacks (e.g. a future MVCFramework.Server.HTTPS.OpenSSL3),
// just change the configurator assignment — server units stay untouched.
//
// ***************************************************************************

unit MVCFramework.Server.HTTPS.TaurusTLS;

{$I dmvcframework.inc}

interface

uses
  MVCFramework.Server.Intf;

/// <summary>
/// Configurator for TMVCIndyServer-backed IMVCServer instances.
/// Reads CertFile / KeyFile / RootCertFile / CertPassword from AServer and
/// installs a TaurusTLSServerIOHandler on the underlying TIdHTTPServer.
/// </summary>
function TaurusTLSIndyConfigurator: TMVCHTTPSConfigurator;

/// <summary>
/// Configurator for TMVCWebBrokerServer-backed IMVCServer instances.
/// Same behavior as TaurusTLSIndyConfigurator but targets the
/// TIdHTTPWebBrokerBridge owned by the server.
/// </summary>
function TaurusTLSWebBrokerConfigurator: TMVCHTTPSConfigurator;

implementation

uses
  System.Classes, System.SysUtils,
  IdHTTPServer, IdHTTPWebBrokerBridge, IdSSLOpenSSL,
  TaurusTLS,
  MVCFramework.Commons,
  MVCFramework.Server.Indy,
  MVCFramework.Server.WebBroker;

type
  // Owned by the IOHandler (TComponent), so its lifetime tracks the
  // IOHandler lifetime which itself tracks the HTTPServer/Bridge lifetime.
  TSSLPasswordHelper = class(TComponent)
  private
    FPassword: string;
  public
    procedure SetPassword(const APassword: string);
    procedure OnGetSSLPassword(aSender: TObject; var aPassword: string;
      const aIsWrite: Boolean; var aOk: Boolean);
  end;

procedure TSSLPasswordHelper.SetPassword(const APassword: string);
begin
  FPassword := APassword;
end;

procedure TSSLPasswordHelper.OnGetSSLPassword(aSender: TObject;
  var aPassword: string; const aIsWrite: Boolean; var aOk: Boolean);
begin
  aPassword := FPassword;
  aOk := True;
end;

function BuildTaurusTLSHandler(AOwner: TComponent;
  const ACertFile, AKeyFile, ARootCertFile, ACertPassword: string): TTaurusTLSServerIOHandler;
var
  LHelper: TSSLPasswordHelper;
begin
  Result := TTaurusTLSServerIOHandler.Create(AOwner);
  Result.SSLOptions.Mode := sslmServer;
  Result.DefaultCert.PublicKey := ACertFile;
  Result.DefaultCert.PrivateKey := AKeyFile;
  if ARootCertFile <> '' then
    Result.DefaultCert.RootKey := ARootCertFile;
  LHelper := TSSLPasswordHelper.Create(Result);
  LHelper.SetPassword(ACertPassword);
  Result.OnGetPassword := LHelper.OnGetSSLPassword;
end;

function TaurusTLSIndyConfigurator: TMVCHTTPSConfigurator;
begin
  Result :=
    procedure(AServer: IMVCServer)
    var
      LIndy: TMVCIndyServer;
    begin
      LIndy := AServer as TMVCIndyServer;
      LIndy.HTTPServer.IOHandler := BuildTaurusTLSHandler(
        LIndy.HTTPServer,
        AServer.CertFile, AServer.KeyFile,
        AServer.RootCertFile, AServer.CertPassword);
    end;
end;

function TaurusTLSWebBrokerConfigurator: TMVCHTTPSConfigurator;
begin
  Result :=
    procedure(AServer: IMVCServer)
    var
      LWB: TMVCWebBrokerServer;
    begin
      LWB := AServer as TMVCWebBrokerServer;
      LWB.Bridge.IOHandler := BuildTaurusTLSHandler(
        LWB.Bridge,
        AServer.CertFile, AServer.KeyFile,
        AServer.RootCertFile, AServer.CertPassword);
    end;
end;

end.
