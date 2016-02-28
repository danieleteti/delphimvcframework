{***************************************************************************}
{                                                                           }
{                      Delphi MVC Framework                                 }
{                                                                           }
{     Copyright (c) 2010-2015 Daniele Teti and the DMVCFramework Team       }
{                                                                           }
{           https://github.com/danieleteti/delphimvcframework               }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit MVCFramework.Server;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.SyncObjs,
  System.Classes

  {$IFDEF IOCP},

  Iocp.DSHTTPWebBroker

  {$ELSE},

  IdHTTPWebBrokerBridge

  {$ENDIF},

  MVCFramework.Commons;

type

  EMVCServerException = class(Exception);

  IMVCSecurity = MVCFramework.Commons.IMVCAuthenticationHandler;

  TMVCBaseSecurity = class abstract(TInterfacedObject)
  strict private
    { private declarations }
  strict protected
    { protected declarations }
  public
    { public declarations }
  end;

  TMVCAuthenticationDelegate = reference to procedure(const AUserName, APassword: string;
    AUserRoles: TList<string>; var AIsValid: Boolean);

  TMVCAuthorizationDelegate = reference to procedure(AUserRoles: TList<string>;
    const AControllerQualifiedClassName: string; const AActionName: string; var AIsAuthorized: Boolean);

  TMVCDefaultSecurity = class(TMVCBaseSecurity, IMVCSecurity)
  strict private
    FAuthenticationDelegate: TMVCAuthenticationDelegate;
    FAuthorizationDelegate: TMVCAuthorizationDelegate;
  public
    constructor Create(AAuthenticationDelegate: TMVCAuthenticationDelegate;
      AAuthorizationDelegate: TMVCAuthorizationDelegate);

    procedure OnRequest(const AControllerQualifiedClassName, AActionName: string;
      var AAuthenticationRequired: Boolean);

    procedure OnAuthorization(AUserRoles: TList<string>; const AControllerQualifiedClassName: string;
      const AActionName: string; var AIsAuthorized: Boolean);

    procedure OnAuthentication(const UserName: string; const Password: string;
      UserRoles: System.Generics.Collections.TList<System.string>;
      var IsValid: Boolean;
      const SessionData: TDictionary<string,string>);
  end;

  IMVCServerInfo = interface
    ['{3A328987-2485-4660-BB9B-B8AFFF47E4BA}']
    function GetServerName(): string;
    procedure SetServerName(const AValue: string);

    function GetPort(): Integer;
    procedure SetPort(const AValue: Integer);

    function GetMaxConnections(): Integer;
    procedure SetMaxConnections(const AValue: Integer);

    function GetWebModuleClass(): TComponentClass;
    procedure SetWebModuleClass(AValue: TComponentClass);

    function GetSecurity(): IMVCSecurity;
    procedure SetSecurity(AValue: IMVCSecurity);

    property ServerName: string read GetServerName write SetServerName;
    property Port: Integer read GetPort write SetPort;
    property MaxConnections: Integer read GetMaxConnections write SetMaxConnections;
    property WebModuleClass: TComponentClass read GetWebModuleClass write SetWebModuleClass;
    property Security: IMVCSecurity read GetSecurity write SetSecurity;
  end;

  TMVCServerInfoFactory = class sealed
  strict private

    {$HINTS OFF}

    constructor Create;

    {$HINTS ON}

  public
    class function Build(): IMVCServerInfo; static;
  end;

  IMVCServer = interface
    ['{95E91DF0-6ABF-46B1-B995-FC748BC54568}']
    function GetActive(): Boolean;
    function GetInfo(): IMVCServerInfo;

    procedure Start();
    procedure Stop();

    property Active: Boolean read GetActive;
    property Info: IMVCServerInfo read GetInfo;
  end;

  TMVCServerFactory = class sealed
  strict private

    {$HINTS OFF}

    constructor Create;

    {$HINTS ON}

  public
    class function Build(AServerInfo: IMVCServerInfo): IMVCServer; static;
  end;

  IMVCServerContainer = interface
    ['{B20796A0-CB07-4D16-BEAB-4F0B10880318}']
    function GetServers(): TDictionary<string, IMVCServer>;

    procedure CreateServer(AServerInfo: IMVCServerInfo);
    procedure DestroyServer(const AServerName: string);

    procedure StartServers();
    procedure StopServers();

    function FindServerByName(const AServerName: string): IMVCServer;

    property Servers: TDictionary<string, IMVCServer> read GetServers;
  end;

  TMVCServerContainerFactory = class sealed
  strict private

    {$HINTS OFF}

    constructor Create;

    {$HINTS ON}

  public
    class function Build(): IMVCServerContainer; static;
  end;

  MVCServerDefault = class sealed
  strict private

    {$HINTS OFF}

    constructor Create;

    {$HINTS ON}

  public
    class function Container(): IMVCServerContainer; static;
  end;

implementation

const
  _CanNotBeInstantiatedException = 'This class can not be instantiated!';

type

  TMVCServerInfo = class(TInterfacedObject, IMVCServerInfo)
  strict private
    FServerName: string;
    FPort: Integer;
    FMaxConnections: Integer;
    FWebModuleClass: TComponentClass;
    FSecurity: IMVCSecurity;
  strict private
    function GetServerName(): string;
    procedure SetServerName(const AValue: string);

    function GetPort(): Integer;
    procedure SetPort(const AValue: Integer);

    function GetMaxConnections(): Integer;
    procedure SetMaxConnections(const AValue: Integer);

    function GetWebModuleClass(): TComponentClass;
    procedure SetWebModuleClass(AValue: TComponentClass);

    function GetSecurity(): IMVCSecurity;
    procedure SetSecurity(AValue: IMVCSecurity);

  public
    constructor Create();
    destructor Destroy(); override;

    property ServerName: string read GetServerName write SetServerName;
    property Port: Integer read GetPort write SetPort;
    property MaxConnections: Integer read GetMaxConnections write SetMaxConnections;
    property WebModuleClass: TComponentClass read GetWebModuleClass write SetWebModuleClass;
    property Security: IMVCSecurity read GetSecurity write SetSecurity;
  end;

  TMVCServer = class(TInterfacedObject, IMVCServer)
  strict private

    {$IFDEF IOCP}

    FBridge: TIocpWebBrokerBridge;

    {$ELSE}

    FBridge: TIdHTTPWebBrokerBridge;

    {$ENDIF}

    FInfo: IMVCServerInfo;
  strict private
    function GetActive(): Boolean;
    function GetInfo(): IMVCServerInfo;
    procedure Configuration(AServerInfo: IMVCServerInfo);
  public
    constructor Create(AServerInfo: IMVCServerInfo);
    destructor Destroy(); override;

    procedure Start();
    procedure Stop();

    property Active: Boolean read GetActive;
    property Info: IMVCServerInfo read GetInfo;
  end;

  TMVCServerContainer = class(TInterfacedObject, IMVCServerContainer)
  strict private
    FServers: TDictionary<string, IMVCServer>;
  strict private
    function GetServers(): TDictionary<string, IMVCServer>;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure CreateServer(AServerInfo: IMVCServerInfo);
    procedure DestroyServer(const AServerName: string);

    procedure StartServers();
    procedure StopServers();

    function FindServerByName(const AServerName: string): IMVCServer;

    property Servers: TDictionary<string, IMVCServer> read GetServers;
  end;

  TMVCSingletonServerContainer = class sealed
  strict private
    class var CriticalSection: TCriticalSection;
    class var ServerContainer: IMVCServerContainer;

    class constructor Create;
    class destructor Destroy;
  public
    class function GetInstance(): IMVCServerContainer; static;
  end;

  { TMVCServerInfo }

constructor TMVCServerInfo.Create;
begin
  FServerName := EmptyStr;
  FPort := 0;
  FMaxConnections := 0;
  FWebModuleClass := nil;
  FSecurity := nil;
end;

destructor TMVCServerInfo.Destroy;
begin

  inherited;
end;

function TMVCServerInfo.GetMaxConnections: Integer;
begin
  if (FMaxConnections = 0) then
    raise EMVCServerException.Create('MaxConnections was not informed!');
  Result := FMaxConnections;
end;

function TMVCServerInfo.GetPort: Integer;
begin
  if (FPort = 0) then
    raise EMVCServerException.Create('Port was not informed!');
  Result := FPort;
end;

function TMVCServerInfo.GetSecurity: IMVCSecurity;
begin
  Result := FSecurity;
end;

function TMVCServerInfo.GetServerName: string;
begin
  if (FServerName = EmptyStr) then
    raise EMVCServerException.Create('ServerName was not informed!');
  Result := FServerName;
end;

function TMVCServerInfo.GetWebModuleClass: TComponentClass;
begin
  if (FWebModuleClass = nil) then
    raise EMVCServerException.Create('WebModuleClass was not informed!');
  Result := FWebModuleClass;
end;

procedure TMVCServerInfo.SetMaxConnections(const AValue: Integer);
begin
  FMaxConnections := AValue;
end;

procedure TMVCServerInfo.SetPort(const AValue: Integer);
begin
  FPort := AValue;
end;

procedure TMVCServerInfo.SetSecurity(AValue: IMVCSecurity);
begin
  FSecurity := AValue;
end;

procedure TMVCServerInfo.SetServerName(const AValue: string);
begin
  FServerName := AValue;
end;

procedure TMVCServerInfo.SetWebModuleClass(AValue: TComponentClass);
begin
  FWebModuleClass := AValue;
end;

{ TMVCServerInfoFactory }

class function TMVCServerInfoFactory.Build: IMVCServerInfo;
begin
  Result := TMVCServerInfo.Create;
end;

constructor TMVCServerInfoFactory.Create;
begin
  raise EMVCServerException.Create(_CanNotBeInstantiatedException);
end;

{ TMVCServer }

procedure TMVCServer.Configuration(AServerInfo: IMVCServerInfo);
begin
  if (AServerInfo = nil) then
    raise EMVCServerException.Create('ServerInfo was not informed!');

  FInfo := AServerInfo;

  {$IFDEF IOCP}

  FBridge := TIocpWebBrokerBridge.Create(nil);
  Stop();
  FBridge.Port := FInfo.Port;
  FBridge.MaxClients := FInfo.MaxConnections;
  FBridge.RegisterWebModuleClass(FInfo.WebModuleClass);

  {$ELSE}

  FBridge := TIdHTTPWebBrokerBridge.Create(nil);
  Stop();
  FBridge.DefaultPort := FInfo.Port;
  FBridge.MaxConnections := FInfo.MaxConnections;
  FBridge.RegisterWebModuleClass(FInfo.WebModuleClass);

  {$ENDIF}

end;

constructor TMVCServer.Create(AServerInfo: IMVCServerInfo);
begin
  Configuration(AServerInfo);
end;

destructor TMVCServer.Destroy;
begin
  if (FBridge <> nil) then
    FreeAndNil(FBridge);
  inherited;
end;

function TMVCServer.GetActive: Boolean;
begin
  Result := FBridge.Active;
end;

function TMVCServer.GetInfo: IMVCServerInfo;
begin
  if (FInfo = nil) then
    raise EMVCServerException.Create('Server Info was not informed!');

  Result := FInfo;
end;

procedure TMVCServer.Start;
begin
  FBridge.Active := True;
end;

procedure TMVCServer.Stop;
begin
  FBridge.Active := False;
end;

{ TMVCServerFactory }

class function TMVCServerFactory.Build(AServerInfo: IMVCServerInfo): IMVCServer;
begin
  Result := TMVCServer.Create(AServerInfo);
end;

constructor TMVCServerFactory.Create;
begin
  raise EMVCServerException.Create(_CanNotBeInstantiatedException);
end;

{ TMVCServerContainer }

constructor TMVCServerContainer.Create;
begin
  FServers := TDictionary<string, IMVCServer>.Create;
end;

procedure TMVCServerContainer.CreateServer(AServerInfo: IMVCServerInfo);
var
  vServer: IMVCServer;
  vPair: TPair<string, IMVCServer>;
begin
  if not(FServers.ContainsKey(AServerInfo.ServerName)) then
  begin
    for vPair in FServers do
      if (vPair.Value.Info.WebModuleClass = AServerInfo.WebModuleClass) then
        raise EMVCServerException.Create('Server List already contains ' + AServerInfo.WebModuleClass.ClassName + '!');

    vServer := TMVCServerFactory.Build(AServerInfo);
    FServers.Add(AServerInfo.ServerName, vServer);
  end;
end;

destructor TMVCServerContainer.Destroy;
begin
  StopServers();
  FreeAndNil(FServers);
  inherited;
end;

procedure TMVCServerContainer.DestroyServer(const AServerName: string);
begin
  if (FServers.ContainsKey(AServerName)) then
    FServers.Remove(AServerName)
  else
    raise EMVCServerException.Create('Server ' + AServerName + ' not found!');
end;

function TMVCServerContainer.FindServerByName(const AServerName: string): IMVCServer;
begin
  try
    Result := FServers.Items[AServerName];
  except
    Result := nil;
  end;
end;

function TMVCServerContainer.GetServers: TDictionary<string, IMVCServer>;
begin
  Result := FServers;
end;

procedure TMVCServerContainer.StartServers;
var
  vPair: TPair<string, IMVCServer>;
begin
  for vPair in FServers do
    vPair.Value.Start();
end;

procedure TMVCServerContainer.StopServers;
var
  vPair: TPair<string, IMVCServer>;
begin
  for vPair in FServers do
    vPair.Value.Stop();
end;

{ TMVCServerContainerFactory }

class function TMVCServerContainerFactory.Build: IMVCServerContainer;
begin
  Result := TMVCServerContainer.Create;
end;

constructor TMVCServerContainerFactory.Create;
begin
  raise EMVCServerException.Create(_CanNotBeInstantiatedException);
end;

{ TMVCDefaultSecurity }

constructor TMVCDefaultSecurity.Create(AAuthenticationDelegate: TMVCAuthenticationDelegate;
  AAuthorizationDelegate: TMVCAuthorizationDelegate);
begin
  FAuthenticationDelegate := AAuthenticationDelegate;
  FAuthorizationDelegate := AAuthorizationDelegate;
end;

procedure TMVCDefaultSecurity.OnAuthentication(const UserName: string; const Password: string;
      UserRoles: System.Generics.Collections.TList<System.string>;
      var IsValid: Boolean;
      const SessionData: TDictionary<string,string>);
begin
  IsValid := True;
  if Assigned(FAuthenticationDelegate) then
    FAuthenticationDelegate(UserName, Password, UserRoles, IsValid);
end;

procedure TMVCDefaultSecurity.OnAuthorization(AUserRoles: TList<string>;
  const AControllerQualifiedClassName, AActionName: string; var AIsAuthorized: Boolean);
begin
  AIsAuthorized := True;
  if Assigned(FAuthorizationDelegate) then
    FAuthorizationDelegate(AUserRoles, AControllerQualifiedClassName, AActionName, AIsAuthorized);
end;

procedure TMVCDefaultSecurity.OnRequest(const AControllerQualifiedClassName, AActionName: string;
  var AAuthenticationRequired: Boolean);
begin
  AAuthenticationRequired := True;
end;

{ TMVCSingletonServerContainer }

class constructor TMVCSingletonServerContainer.Create;
begin
  CriticalSection := TCriticalSection.Create();
  ServerContainer := nil;
end;

class destructor TMVCSingletonServerContainer.Destroy;
begin
  ServerContainer := nil;
  FreeAndNil(CriticalSection);
end;

class function TMVCSingletonServerContainer.GetInstance: IMVCServerContainer;
begin
  if (ServerContainer = nil) then
  begin
    CriticalSection.Enter;
    try
      ServerContainer := TMVCServerContainerFactory.Build();
    finally
      CriticalSection.Leave;
    end;
  end;
  Result := ServerContainer;
end;

{ MVCServerDefault }

class function MVCServerDefault.Container: IMVCServerContainer;
begin
  Result := TMVCSingletonServerContainer.GetInstance;
end;

constructor MVCServerDefault.Create;
begin
  raise EMVCServerException.Create(_CanNotBeInstantiatedException);
end;

end.
