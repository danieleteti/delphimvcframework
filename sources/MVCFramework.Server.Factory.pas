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
// Single entry point for building IMVCServer instances regardless of the
// underlying engine. One Create* method per engine type (compile-time
// safety: an "unknown engine" runtime case is impossible).
//
// HTTPS is configured via the IMVCServer interface (UseHTTPS + cert
// properties + HTTPSConfigurator); see MVCFramework.Server.HTTPS.TaurusTLS
// for the TaurusTLS provider.
//
// ***************************************************************************

unit MVCFramework.Server.Factory;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  MVCFramework, MVCFramework.Server.Intf;

type
  TMVCEngineConfigProc = reference to procedure(AEngine: TMVCEngine);

  TMVCServerFactory = class
  public
    /// <summary>
    /// Build an Indy Direct server bound to an already-configured engine.
    /// Returns IMVCIndyServer so callers can opt into Indy-specific knobs
    /// (e.g. SingleFlushResponse). The result is also assignable to a
    /// plain IMVCServer variable, preserving backward compatibility.
    /// </summary>
    class function CreateIndyDirect(AEngine: TMVCEngine): IMVCIndyServer;

    /// <summary>
    /// Build a HTTP.sys (Windows kernel-mode) server bound to an
    /// already-configured engine.
    /// </summary>
    class function CreateHttpSys(AEngine: TMVCEngine): IMVCServer;

    /// <summary>
    /// Build a WebBroker server. WebBroker creates a TMVCEngine for each
    /// TWebModule instance, so the caller passes a configuration procedure
    /// invoked once per WebModule.
    /// </summary>
    class function CreateWebBroker(AEngineConfig: TMVCEngineConfigProc): IMVCServer;
  end;

implementation

uses
  MVCFramework.Server.Indy,
  MVCFramework.Server.HttpSys,
  MVCFramework.Server.WebBroker;

class function TMVCServerFactory.CreateIndyDirect(AEngine: TMVCEngine): IMVCIndyServer;
begin
  Result := TMVCIndyServer.Create(AEngine);
end;

class function TMVCServerFactory.CreateHttpSys(AEngine: TMVCEngine): IMVCServer;
begin
  Result := TMVCHttpSysServer.Create(AEngine);
end;

class function TMVCServerFactory.CreateWebBroker(
  AEngineConfig: TMVCEngineConfigProc): IMVCServer;
begin
  Result := TMVCWebBrokerServer.Create(
    procedure(AEngine: TMVCEngine)
    begin
      if Assigned(AEngineConfig) then
        AEngineConfig(AEngine);
    end);
end;

end.
