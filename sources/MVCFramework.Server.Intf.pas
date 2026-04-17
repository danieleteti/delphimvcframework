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

unit MVCFramework.Server.Intf;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils, MVCFramework;

type
  IMVCServer = interface;

  /// <summary>
  /// Per-server callback that wires a TLS IOHandler/binding into the
  /// underlying engine. Templates assign this from a provider unit, e.g.:
  ///   LServer.HTTPSConfigurator := TaurusTLSIndyConfigurator;
  /// Implementations cast AServer to the concrete server class to access
  /// engine-specific knobs (TIdHTTPServer, TIdHTTPWebBrokerBridge, ...).
  /// </summary>
  TMVCHTTPSConfigurator = reference to procedure(AServer: IMVCServer);

  IMVCServer = interface
    ['{A8D7B2C1-E5F4-4A3B-9C6D-1F2E3A4B5C6D}']
    procedure SetEngine(AEngine: TMVCEngine);
    function GetEngine: TMVCEngine;
    procedure Listen(APort: Integer = 8080; const AHost: string = '0.0.0.0');
    procedure Stop;
    function IsRunning: Boolean;
    function GetPort: Integer;
    function GetHost: string;
    procedure SetMaxConnections(AValue: Integer);
    function GetMaxConnections: Integer;
    procedure SetKeepAlive(AValue: Boolean);
    function GetKeepAlive: Boolean;
    procedure SetListenQueue(AValue: Integer);
    function GetListenQueue: Integer;
    // HTTPS configuration. Each implementation handles SSL internally:
    //   - TMVCIndyServer/TMVCWebBrokerServer: TaurusTLS using the cert
    //     properties below.
    //   - TMVCHttpSysServer: SSL bound externally via "netsh http add
    //     sslcert"; UseHTTPS only flips the registered URL prefix to
    //     "https://", cert properties are ignored.
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
    property Engine: TMVCEngine read GetEngine write SetEngine;
    property Port: Integer read GetPort;
    property Host: string read GetHost;
    property MaxConnections: Integer read GetMaxConnections write SetMaxConnections;
    property KeepAlive: Boolean read GetKeepAlive write SetKeepAlive;
    property ListenQueue: Integer read GetListenQueue write SetListenQueue;
    property UseHTTPS: Boolean read GetUseHTTPS write SetUseHTTPS;
    property CertFile: string read GetCertFile write SetCertFile;
    property KeyFile: string read GetKeyFile write SetKeyFile;
    property RootCertFile: string read GetRootCertFile write SetRootCertFile;
    property CertPassword: string read GetCertPassword write SetCertPassword;
    property HTTPSConfigurator: TMVCHTTPSConfigurator
      read GetHTTPSConfigurator write SetHTTPSConfigurator;
  end;

implementation

end.
