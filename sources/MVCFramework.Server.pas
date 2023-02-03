// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators with this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
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

unit MVCFramework.Server;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  MVCFramework.Commons, MVCFramework;

type

  EMVCServerException = class(Exception);

  IMVCListenerProperties = interface
    ['{82721C88-A308-4B2E-B94A-8E7CEEC4721F}']
    function GetName: string;
    function SetName(const AValue: string): IMVCListenerProperties;

    function GetPort: Integer;
    function SetPort(AValue: Integer): IMVCListenerProperties;

    function GetMaxConnections: Integer;
    function SetMaxConnections(AValue: Integer): IMVCListenerProperties;

    function GetWebModuleClass: TComponentClass;
    function SetWebModuleClass(AValue: TComponentClass): IMVCListenerProperties;

    function GetSSLOptions(out SSLCertFile, SSLRootCertFile, SSLKeyFile, SSLPassword: String): Boolean;
    function SetSSLOptions(const SSLCertFile, SSLRootCertFile, SSLKeyFile, SSLPassword: String): IMVCListenerProperties;
  end;

  IMVCListener = interface
    ['{127A5E5D-D968-4409-BE9A-8D3AE08E6009}']
    function GetActive: Boolean;

    procedure Start;
    procedure Stop;

    property Active: Boolean read GetActive;
  end;

  IMVCListenersContext = interface
    ['{9EA6BBDB-B5C1-462E-BBF4-AA30A4317F54}']
    function Add(const AName: string; AListener: IMVCListener): IMVCListenersContext; overload;
    function Add(AProperties: IMVCListenerProperties): IMVCListenersContext; overload;
    function Remove(const AListenerName: string): IMVCListenersContext;

    procedure StartAll;
    procedure StopAll;

    function FindByName(const AListenerName: string): IMVCListener;

    procedure ForEach(AProc: TProc<string, IMVCListener>);
    function Count: Integer;
  end;

  TMVCRequestDelegate = reference to procedure(const AControllerQualifiedClassName, AActionName: string;
    var AAuthenticationRequired: Boolean);

  TMVCAuthenticationDelegate = reference to procedure(const AUserName, APassword: string; AUserRoles: TList<string>;
    var IsValid: Boolean; const ASessionData: TDictionary<String, String>);

  TMVCAuthorizationDelegate = reference to procedure(AUserRoles: TList<string>; const AControllerQualifiedClassName: string;
    const AActionName: string; var IsAuthorized: Boolean);

  IMVCDefaultAuthenticationHandler = interface(IMVCAuthenticationHandler)
    ['{0B292EEF-B871-4FA9-81AC-FED633C3A238}']
    function SetOnRequest(AMethod: TMVCRequestDelegate): IMVCDefaultAuthenticationHandler;
    function SetOnAuthentication(AMethod: TMVCAuthenticationDelegate): IMVCDefaultAuthenticationHandler;
    function SetOnAuthorization(AMethod: TMVCAuthorizationDelegate): IMVCDefaultAuthenticationHandler;
  end;

implementation

end.
