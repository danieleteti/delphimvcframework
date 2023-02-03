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

unit MVCFramework.Server.Impl;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  IdHTTPWebBrokerBridge,
  IdSSLOpenSSL,
  IdSSL,
  IdContext,
  MVCFramework.Server,
  MVCFramework;

type
  TMVCListenerProperties = class(TInterfacedObject, IMVCListenerProperties)
  private
    FName: string;
    FPort: Integer;
    FMaxConnections: Integer;
    FWebModuleClass: TComponentClass;
    FSSLCertFile: String;
    FSSLRootCertFile: String;
    FSSLKeyFile: String;
    FSSLPassword: String;
  protected
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
  public
    constructor Create;
    class function New: IMVCListenerProperties; static;
  end;

  TMVCListener = class(TInterfacedObject, IMVCListener)
  private
    FBridge: TIdHTTPWebBrokerBridge;
    FBridgeSSLHandler: TIdServerIOHandlerSSLOpenSSL;
    FBridgeSSLPassword: String;
    procedure OnParseAuthentication(AContext: TIdContext; const AAuthType,
      AAuthData: String; var VUsername, VPassword: String;
      var VHandled: Boolean);
    procedure OnGetSSLPassword(var APassword: string);
{$IF Defined(RIOORBETTER)}
    procedure QuerySSLPort(APort: Word; var VUseSSL: boolean);
{$ENDIF}
  protected
    function GetActive: Boolean;

    procedure Start;
    procedure Stop;
  public
    constructor Create(AProperties: IMVCListenerProperties);
    destructor Destroy; override;
  end;

  TMVCListenersContext = class(TInterfacedObject, IMVCListenersContext)
  private
    FListeners: TDictionary<string, IMVCListener>;
  protected
    function Add(const AName: string; AListener: IMVCListener): IMVCListenersContext; overload;
    function Add(AProperties: IMVCListenerProperties): IMVCListenersContext; overload;
    function Remove(const AListenerName: string): IMVCListenersContext;

    procedure StartAll;
    procedure StopAll;

    function FindByName(const AListenerName: string): IMVCListener;

    procedure ForEach(AProc: TProc<string, IMVCListener>);
    function Count: Integer;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TMVCDefaultAuthenticationHandler = class(TInterfacedObject, IMVCDefaultAuthenticationHandler)
  private
    FRequestDelegate: TMVCRequestDelegate;
    FAuthenticationDelegate: TMVCAuthenticationDelegate;
    FAuthorizationDelegate: TMVCAuthorizationDelegate;
  protected
    procedure OnRequest(
      const AContext: TWebContext;
      const AControllerQualifiedClassName, AActionName: string;
      var AAuthenticationRequired: Boolean);

    procedure OnAuthentication(
      const AContext: TWebContext;
      const AUserName, APassword: string;
      AUserRoles: TList<string>;
      var IsValid: Boolean;
      const ASessionData: TDictionary<string, string>);

    procedure OnAuthorization(
      const AContext: TWebContext;
      AUserRoles: TList<string>;
      const AControllerQualifiedClassName: string;
      const AActionName: string;
      var IsAuthorized: Boolean);
  public
    class function New: IMVCDefaultAuthenticationHandler; static;

    function SetOnRequest(AMethod: TMVCRequestDelegate): IMVCDefaultAuthenticationHandler;
    function SetOnAuthentication(AMethod: TMVCAuthenticationDelegate): IMVCDefaultAuthenticationHandler;
    function SetOnAuthorization(AMethod: TMVCAuthorizationDelegate): IMVCDefaultAuthenticationHandler;
  end;

implementation

{ TMVCListenerProperties }

constructor TMVCListenerProperties.Create;
begin
  inherited Create;
  FName := '';
  FPort := 8080;
  FMaxConnections := 1024;
  FWebModuleClass := nil;
  FSSLCertFile := '';
  FSSLRootCertFile := '';
  FSSLKeyFile := '';
  FSSLPassword := '';
end;

function TMVCListenerProperties.GetMaxConnections: Integer;
begin
  Result := FMaxConnections;
end;

function TMVCListenerProperties.GetName: string;
begin
  Result := FName;
end;

function TMVCListenerProperties.GetPort: Integer;
begin
  Result := FPort;
end;

function TMVCListenerProperties.GetSSLOptions(out SSLCertFile, SSLRootCertFile,
  SSLKeyFile, SSLPassword: String): Boolean;
begin
  SSLCertFile := FSSLCertFile;
  SSLRootCertFile := FSSLRootCertFile;
  SSLKeyFile := FSSLKeyFile;
  SSLPassword := FSSLPassword;
  Result := not (FSSLCertFile.IsEmpty and FSSLKeyFile.IsEmpty);
end;

function TMVCListenerProperties.GetWebModuleClass: TComponentClass;
begin
  Result := FWebModuleClass;
end;

class function TMVCListenerProperties.New: IMVCListenerProperties;
begin
  Result := TMVCListenerProperties.Create;
end;

function TMVCListenerProperties.SetMaxConnections(AValue: Integer): IMVCListenerProperties;
begin
  FMaxConnections := AValue;
  Result := Self;
end;

function TMVCListenerProperties.SetName(const AValue: string): IMVCListenerProperties;
begin
  FName := AValue;
  Result := Self;
end;

function TMVCListenerProperties.SetPort(AValue: Integer): IMVCListenerProperties;
begin
  FPort := AValue;
  Result := Self;
end;

function TMVCListenerProperties.SetSSLOptions(const SSLCertFile,
  SSLRootCertFile, SSLKeyFile, SSLPassword: String): IMVCListenerProperties;
begin
  FSSLCertFile := SSLCertFile;
  FSSLRootCertFile := SSLRootCertFile;
  FSSLKeyFile := SSLKeyFile;
  FSSLPassword := SSLPassword;
  Result := Self;
end;

function TMVCListenerProperties.SetWebModuleClass(AValue: TComponentClass): IMVCListenerProperties;
begin
  FWebModuleClass := AValue;
  Result := Self;
end;

{ TMVCListener }

constructor TMVCListener.Create(AProperties: IMVCListenerProperties);
var
  lSSLCertFile: String;
  lSSLRootCertFile: String;
  lSSLKeyFile: String;
begin
  inherited Create;

  if not Assigned(AProperties) then
    raise EMVCServerException.Create('Listener properties was not informed.');

  if AProperties.GetName.IsEmpty then
    raise EMVCServerException.Create('Listener name was not informed.');

  FBridge := TIdHTTPWebBrokerBridge.Create(nil);
  FBridge.DefaultPort := AProperties.GetPort;
  FBridge.MaxConnections := AProperties.GetMaxConnections;
  FBridge.OnParseAuthentication := OnParseAuthentication;
  FBridge.RegisterWebModuleClass(AProperties.GetWebModuleClass);

  if AProperties.GetSSLOptions(lSSLCertFile,lSSLRootCertFile,lSSLKeyFile,FBridgeSSLPassword) then
  begin
    //notice for client implementations
    //OpenSSL currently don't support perfect forward security by default
    FBridgeSSLHandler := TIdServerIOHandlerSSLOpenSSL.Create(FBridge);
    FBridgeSSLHandler.SSLOptions.Method := sslvTLSv1_2;
    FBridgeSSLHandler.SSLOptions.SSLVersions := [sslvSSLv23, sslvSSLv3, sslvTLSv1,sslvTLSv1_1,sslvTLSv1_2];
    FBridgeSSLHandler.SSLOptions.Mode := sslmServer;
    FBridgeSSLHandler.SSLOptions.CertFile := lSSLCertFile;
    FBridgeSSLHandler.SSLOptions.RootCertFile := lSSLRootCertFile; //should be empty, currently broken in 10.3.3
    FBridgeSSLHandler.SSLOptions.KeyFile := lSSLKeyFile;
    FBridgeSSLHandler.OnGetPassword := OnGetSSLPassword;
    FBridge.IOHandler := FBridgeSSLHandler;
    {$IF Defined(RIOORBETTER)}
    FBridge.OnQuerySSLPort := QuerySSLPort;
    {$ENDIF}
  end;
end;

destructor TMVCListener.Destroy;
begin
  if Assigned(FBridge) then
    FBridge.Free;
  inherited Destroy;
end;

procedure TMVCListener.OnGetSSLPassword(var APassword: string);
begin
  APassword := FBridgeSSLPassword;
end;

procedure TMVCListener.OnParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
begin
  vhandled := True;
end;


{$IF Defined(RIOORBETTER)}
procedure TMVCListener.QuerySSLPort(APort: Word; var VUseSSL: boolean);
begin
  VUseSSL := true;
end;
{$ENDIF}

function TMVCListener.GetActive: Boolean;
begin
  Result := FBridge.Active;
end;

procedure TMVCListener.Start;
begin
  FBridge.Active := True;
end;

procedure TMVCListener.Stop;
begin
  FBridge.Active := False;
end;

{ TMVCListenersContext }

function TMVCListenersContext.Add(const AName: string; AListener: IMVCListener)
  : IMVCListenersContext;
begin
  FListeners.AddOrSetValue(AName, AListener);
  Result := Self;
end;

function TMVCListenersContext.Add(AProperties: IMVCListenerProperties): IMVCListenersContext;
var
  Listener: IMVCListener;
begin
  Listener := TMVCListener.Create(AProperties);
  Result := Add(AProperties.GetName, Listener);
end;

function TMVCListenersContext.Count: Integer;
begin
  Result := FListeners.Count;
end;

constructor TMVCListenersContext.Create;
begin
  inherited Create;
  FListeners := TDictionary<string, IMVCListener>.Create;
end;

destructor TMVCListenersContext.Destroy;
begin
  StopAll;
  FListeners.Free;
  inherited Destroy;
end;

function TMVCListenersContext.FindByName(const AListenerName: string): IMVCListener;
begin
  Result := FListeners.Items[AListenerName];
end;

procedure TMVCListenersContext.ForEach(AProc: TProc<string, IMVCListener>);
var
  Pair: TPair<string, IMVCListener>;
begin
  for Pair in FListeners do
    AProc(Pair.Key, Pair.Value);
end;

function TMVCListenersContext.Remove(const AListenerName: string): IMVCListenersContext;
begin
  if (FListeners.ContainsKey(AListenerName)) then
    FListeners.Remove(AListenerName)
  else
    raise EMVCServerException.Create('Listener ' + AListenerName + ' not found.');
  Result := Self;
end;

procedure TMVCListenersContext.StartAll;
begin
  ForEach(
    procedure(AName: string; AListener: IMVCListener)
    begin
      AListener.Start;
    end
    );
end;

procedure TMVCListenersContext.StopAll;
begin
  ForEach(
    procedure(AName: string; AListener: IMVCListener)
    begin
      AListener.Stop;
    end
    );
end;

{ TMVCDefaultAuthenticationHandler }

class function TMVCDefaultAuthenticationHandler.New: IMVCDefaultAuthenticationHandler;
begin
  Result := TMVCDefaultAuthenticationHandler.Create;
end;

procedure TMVCDefaultAuthenticationHandler.OnAuthentication(
      const AContext: TWebContext;
      const AUserName, APassword: string;
      AUserRoles: TList<string>;
      var IsValid: Boolean;
      const ASessionData: TDictionary<string, string>);
begin
  IsValid := True;
  if Assigned(FAuthenticationDelegate) then
    FAuthenticationDelegate(AUserName, APassword, AUserRoles, IsValid, ASessionData);
end;

procedure TMVCDefaultAuthenticationHandler.OnAuthorization(
      const AContext: TWebContext;
      AUserRoles: TList<string>;
      const AControllerQualifiedClassName: string;
      const AActionName: string;
      var IsAuthorized: Boolean);
begin
  IsAuthorized := True;
  if Assigned(FAuthorizationDelegate) then
    FAuthorizationDelegate(AUserRoles, AControllerQualifiedClassName, AActionName, IsAuthorized);
end;

procedure TMVCDefaultAuthenticationHandler.OnRequest(const AContext: TWebContext; const AControllerQualifiedClassName, AActionName: string;
      var AAuthenticationRequired: Boolean);
begin
  AAuthenticationRequired := True;
  if Assigned(FRequestDelegate) then
    FRequestDelegate(AControllerQualifiedClassName, AActionName, AAuthenticationRequired);
end;

function TMVCDefaultAuthenticationHandler.SetOnAuthentication(AMethod: TMVCAuthenticationDelegate)
  : IMVCDefaultAuthenticationHandler;
begin
  FAuthenticationDelegate := AMethod;
  Result := Self;
end;

function TMVCDefaultAuthenticationHandler.SetOnAuthorization(AMethod: TMVCAuthorizationDelegate)
  : IMVCDefaultAuthenticationHandler;
begin
  FAuthorizationDelegate := AMethod;
  Result := Self;
end;

function TMVCDefaultAuthenticationHandler.SetOnRequest(AMethod: TMVCRequestDelegate)
  : IMVCDefaultAuthenticationHandler;
begin
  FRequestDelegate := AMethod;
  Result := Self;
end;

end.
