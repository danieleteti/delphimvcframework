// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2018 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators with this file: Ezequiel Juliano M�ller (ezequieljuliano@gmail.com)
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
  MVCFramework.Server;

type

  TMVCListenerProperties = class(TInterfacedObject, IMVCListenerProperties)
  private
    FName: string;
    FPort: Integer;
    FMaxConnections: Integer;
    FWebModuleClass: TComponentClass;
  protected
    function GetName: string;
    function SetName(const AValue: string): IMVCListenerProperties;

    function GetPort: Integer;
    function SetPort(AValue: Integer): IMVCListenerProperties;

    function GetMaxConnections: Integer;
    function SetMaxConnections(AValue: Integer): IMVCListenerProperties;

    function GetWebModuleClass: TComponentClass;
    function SetWebModuleClass(AValue: TComponentClass): IMVCListenerProperties;
  public
    constructor Create;
    class function New: IMVCListenerProperties; static;
  end;

  TMVCListener = class(TInterfacedObject, IMVCListener)
  private
    FBridge: TIdHTTPWebBrokerBridge;
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
    procedure OnRequest(const AControllerQualifiedClassName, AActionName: string;
      var AAuthenticationRequired: Boolean);

    procedure OnAuthentication(
      const AUserName, APassword: string;
      AUserRoles: TList<string>;
      var IsValid: Boolean;
      const ASessionData: TDictionary<string, string>);

    procedure OnAuthorization(
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

function TMVCListenerProperties.SetWebModuleClass(AValue: TComponentClass): IMVCListenerProperties;
begin
  FWebModuleClass := AValue;
  Result := Self;
end;

{ TMVCListener }

constructor TMVCListener.Create(AProperties: IMVCListenerProperties);
begin
  inherited Create;

  if not Assigned(AProperties) then
    raise EMVCServerException.Create('Listener properties was not informed.');

  if AProperties.GetName.IsEmpty then
    raise EMVCServerException.Create('Listener name was not informed.');

  FBridge := TIdHTTPWebBrokerBridge.Create(nil);
  FBridge.DefaultPort := AProperties.GetPort;
  FBridge.MaxConnections := AProperties.GetMaxConnections;
  FBridge.RegisterWebModuleClass(AProperties.GetWebModuleClass);
end;

destructor TMVCListener.Destroy;
begin
  if Assigned(FBridge) then
    FBridge.Free;
  inherited Destroy;
end;

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

procedure TMVCDefaultAuthenticationHandler.OnAuthentication(const AUserName, APassword: string;
AUserRoles: TList<string>;
var IsValid: Boolean; const ASessionData: TDictionary<string, string>);
begin
  IsValid := True;
  if Assigned(FAuthenticationDelegate) then
    FAuthenticationDelegate(AUserName, APassword, AUserRoles, IsValid, ASessionData);
end;

procedure TMVCDefaultAuthenticationHandler.OnAuthorization(AUserRoles: TList<string>;
const AControllerQualifiedClassName,
  AActionName: string; var IsAuthorized: Boolean);
begin
  IsAuthorized := True;
  if Assigned(FAuthorizationDelegate) then
    FAuthorizationDelegate(AUserRoles, AControllerQualifiedClassName, AActionName, IsAuthorized);
end;

procedure TMVCDefaultAuthenticationHandler.OnRequest(const AControllerQualifiedClassName,
  AActionName: string;
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
