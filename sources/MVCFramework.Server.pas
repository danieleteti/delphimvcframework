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

  EMVCSeverException = class(Exception);

  IMVCSecurity = MVCFramework.Commons.IMVCAuthenticationHandler;

  TMVCBaseSecurity = class abstract(TInterfacedObject)
  strict private
    { private declarations }
  strict protected
    { protected declarations }
  public
    { public declarations }
  end;

  TMVCAuthenticationDelegate = reference to procedure(const pUserName, pPassword: string;
    pUserRoles: TList<string>; var pIsValid: Boolean);

  TMVCAuthorizationDelegate = reference to procedure(pUserRoles: TList<string>;
    const pControllerQualifiedClassName: string; const pActionName: string; var pIsAuthorized: Boolean);

  TMVCDefaultSecurity = class(TMVCBaseSecurity, IMVCSecurity)
  strict private
    FAuthenticationDelegate: TMVCAuthenticationDelegate;
    FAuthorizationDelegate: TMVCAuthorizationDelegate;
  public
    constructor Create(pAuthenticationDelegate: TMVCAuthenticationDelegate;
      pAuthorizationDelegate: TMVCAuthorizationDelegate);

    procedure OnRequest(const ControllerQualifiedClassName, ActionName: string;
      var AuthenticationRequired: Boolean);

    procedure OnAuthentication(const UserName, Password: string; UserRoles: TList<string>;
      var IsValid: Boolean);

    procedure OnAuthorization(UserRoles: TList<string>; const ControllerQualifiedClassName: string;
      const ActionName: string; var IsAuthorized: Boolean);
  end;

  IMVCServerInfo = interface
    ['{3A328987-2485-4660-BB9B-B8AFFF47E4BA}']
    function GetServerName(): string;
    procedure SetServerName(const pValue: string);

    function GetPort(): Integer;
    procedure SetPort(const pValue: Integer);

    function GetMaxConnections(): Integer;
    procedure SetMaxConnections(const pValue: Integer);

    function GetWebModuleClass(): TComponentClass;
    procedure SetWebModuleClass(pValue: TComponentClass);

    function GetSecurity(): IMVCSecurity;
    procedure SetSecurity(pValue: IMVCSecurity);

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
    function GetInfo(): IMVCServerInfo;

    procedure Start();
    procedure Stop();

    property Info: IMVCServerInfo read GetInfo;
  end;

  TMVCServerFactory = class sealed
  strict private

    {$HINTS OFF}

    constructor Create;

    {$HINTS ON}

  public
    class function Build(pServerInfo: IMVCServerInfo): IMVCServer; static;
  end;

  IMVCServerContainer = interface
    ['{B20796A0-CB07-4D16-BEAB-4F0B10880318}']
    function GetServers(): TDictionary<string, IMVCServer>;

    procedure CreateServer(pServerInfo: IMVCServerInfo);
    procedure DestroyServer(const pServerName: string);

    procedure StartServers();
    procedure StopServers();

    function FindServerByName(const pServerName: string): IMVCServer;

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
    procedure SetServerName(const pValue: string);

    function GetPort(): Integer;
    procedure SetPort(const pValue: Integer);

    function GetMaxConnections(): Integer;
    procedure SetMaxConnections(const pValue: Integer);

    function GetWebModuleClass(): TComponentClass;
    procedure SetWebModuleClass(pValue: TComponentClass);

    function GetSecurity(): IMVCSecurity;
    procedure SetSecurity(pValue: IMVCSecurity);

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
    function GetInfo(): IMVCServerInfo;
    procedure Configuration(pServerInfo: IMVCServerInfo);
  public
    constructor Create(pServerInfo: IMVCServerInfo);
    destructor Destroy(); override;

    procedure Start();
    procedure Stop();

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

    procedure CreateServer(pServerInfo: IMVCServerInfo);
    procedure DestroyServer(const pServerName: string);

    procedure StartServers();
    procedure StopServers();

    function FindServerByName(const pServerName: string): IMVCServer;

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
    raise EMVCSeverException.Create('MaxConnections was not informed!');
  Result := FMaxConnections;
end;

function TMVCServerInfo.GetPort: Integer;
begin
  if (FPort = 0) then
    raise EMVCSeverException.Create('Port was not informed!');
  Result := FPort;
end;

function TMVCServerInfo.GetSecurity: IMVCSecurity;
begin
  Result := FSecurity;
end;

function TMVCServerInfo.GetServerName: string;
begin
  if (FServerName = EmptyStr) then
    raise EMVCSeverException.Create('ServerName was not informed!');
  Result := FServerName;
end;

function TMVCServerInfo.GetWebModuleClass: TComponentClass;
begin
  if (FWebModuleClass = nil) then
    raise EMVCSeverException.Create('WebModuleClass was not informed!');
  Result := FWebModuleClass;
end;

procedure TMVCServerInfo.SetMaxConnections(const pValue: Integer);
begin
  FMaxConnections := pValue;
end;

procedure TMVCServerInfo.SetPort(const pValue: Integer);
begin
  FPort := pValue;
end;

procedure TMVCServerInfo.SetSecurity(pValue: IMVCSecurity);
begin
  FSecurity := pValue;
end;

procedure TMVCServerInfo.SetServerName(const pValue: string);
begin
  FServerName := pValue;
end;

procedure TMVCServerInfo.SetWebModuleClass(pValue: TComponentClass);
begin
  FWebModuleClass := pValue;
end;

{ TMVCServerInfoFactory }

class function TMVCServerInfoFactory.Build: IMVCServerInfo;
begin
  Result := TMVCServerInfo.Create;
end;

constructor TMVCServerInfoFactory.Create;
begin
  raise EMVCSeverException.Create(_CanNotBeInstantiatedException);
end;

{ TMVCServer }

procedure TMVCServer.Configuration(pServerInfo: IMVCServerInfo);
begin
  if (pServerInfo = nil) then
    raise EMVCSeverException.Create('ServerInfo was not informed!');

  FInfo := pServerInfo;

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

constructor TMVCServer.Create(pServerInfo: IMVCServerInfo);
begin
  Configuration(pServerInfo);
end;

destructor TMVCServer.Destroy;
begin
  if (FBridge <> nil) then
    FreeAndNil(FBridge);
  inherited;
end;

function TMVCServer.GetInfo: IMVCServerInfo;
begin
  if (FInfo = nil) then
    raise EMVCSeverException.Create('Server Info was not informed!');

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

class function TMVCServerFactory.Build(pServerInfo: IMVCServerInfo): IMVCServer;
begin
  Result := TMVCServer.Create(pServerInfo);
end;

constructor TMVCServerFactory.Create;
begin
  raise EMVCSeverException.Create(_CanNotBeInstantiatedException);
end;

{ TMVCServerContainer }

constructor TMVCServerContainer.Create;
begin
  FServers := TDictionary<string, IMVCServer>.Create;
end;

procedure TMVCServerContainer.CreateServer(pServerInfo: IMVCServerInfo);
var
  vServer: IMVCServer;
  vPair: TPair<string, IMVCServer>;
begin
  if not(FServers.ContainsKey(pServerInfo.ServerName)) then
  begin
    for vPair in FServers do
      if (vPair.Value.Info.WebModuleClass = pServerInfo.WebModuleClass) then
        raise EMVCSeverException.Create('Server List already contains ' + pServerInfo.WebModuleClass.ClassName + '!');

    vServer := TMVCServerFactory.Build(pServerInfo);
    FServers.Add(pServerInfo.ServerName, vServer);
  end;
end;

destructor TMVCServerContainer.Destroy;
begin
  StopServers();
  FreeAndNil(FServers);
  inherited;
end;

procedure TMVCServerContainer.DestroyServer(const pServerName: string);
begin
  if (FServers.ContainsKey(pServerName)) then
    FServers.Remove(pServerName)
  else
    raise EMVCSeverException.Create('Server ' + pServerName + ' not found!');
end;

function TMVCServerContainer.FindServerByName(const pServerName: string): IMVCServer;
begin
  try
    Result := FServers.Items[pServerName];
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
  raise EMVCSeverException.Create(_CanNotBeInstantiatedException);
end;

{ TMVCDefaultSecurity }

constructor TMVCDefaultSecurity.Create(pAuthenticationDelegate: TMVCAuthenticationDelegate;
  pAuthorizationDelegate: TMVCAuthorizationDelegate);
begin
  FAuthenticationDelegate := pAuthenticationDelegate;
  FAuthorizationDelegate := pAuthorizationDelegate;
end;

procedure TMVCDefaultSecurity.OnAuthentication(const UserName, Password: string;
  UserRoles: TList<string>; var IsValid: Boolean);
begin
  IsValid := True;
  if Assigned(FAuthenticationDelegate) then
    FAuthenticationDelegate(UserName, Password, UserRoles, IsValid);
end;

procedure TMVCDefaultSecurity.OnAuthorization(UserRoles: TList<string>;
  const ControllerQualifiedClassName, ActionName: string; var IsAuthorized: Boolean);
begin
  IsAuthorized := True;
  if Assigned(FAuthorizationDelegate) then
    FAuthorizationDelegate(UserRoles, ControllerQualifiedClassName, ActionName, IsAuthorized);
end;

procedure TMVCDefaultSecurity.OnRequest(const ControllerQualifiedClassName, ActionName: string;
  var AuthenticationRequired: Boolean);
begin
  AuthenticationRequired := True;
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
  raise EMVCSeverException.Create(_CanNotBeInstantiatedException);
end;

end.
