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
    property Engine: TMVCEngine read GetEngine write SetEngine;
    property Port: Integer read GetPort;
    property Host: string read GetHost;
    property MaxConnections: Integer read GetMaxConnections write SetMaxConnections;
    property KeepAlive: Boolean read GetKeepAlive write SetKeepAlive;
    property ListenQueue: Integer read GetListenQueue write SetListenQueue;
  end;

implementation

end.
