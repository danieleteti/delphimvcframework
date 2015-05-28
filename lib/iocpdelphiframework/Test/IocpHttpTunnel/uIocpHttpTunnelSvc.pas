unit uIocpHttpTunnelSvc;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs,
  Iocp.TcpSocket, Iocp.HttpTunnel, uIocpHttpTunnelConfig;

type
  TIocpMultiHttpTunnel = class(Iocp.HttpTunnel.TIocpHttpTunnel)
  private
    function GetDstHost(const Host: string; out DstHost: THostEntry): Boolean;
  protected
    function TriggerConfirmForward(Client: TIocpHttpTunnelConnection;
      out ServerAddr: string; out ServerPort: Word): Boolean; override;
  end;

  TIocpHttpTunnel = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FTunnelServer: TIocpMultiHttpTunnel;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  IocpHttpTunnel: TIocpHttpTunnel;

implementation

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  IocpHttpTunnel.Controller(CtrlCode);
end;

function TIocpHttpTunnel.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

{ TIocpMultiHttpTunnel }

function TIocpMultiHttpTunnel.GetDstHost(const Host: string; out DstHost: THostEntry): Boolean;
var
  Tunnel: TTunnelEntry;
begin
  try
    IocpHttpTunnelConfig.ReadLock;
    for Tunnel in IocpHttpTunnelConfig.TunnelList do
    begin
      if SameText(Tunnel.SrcHost.Host, Host) then
      begin
        DstHost := Tunnel.DstHost;
        Exit(True);
      end;
    end;
  finally
    IocpHttpTunnelConfig.ReadUnlock;
  end;

  Result := False;
end;

function TIocpMultiHttpTunnel.TriggerConfirmForward(
  Client: TIocpHttpTunnelConnection;
  out ServerAddr: string; out ServerPort: Word): Boolean;
var
  DstHost: THostEntry;
begin
  Result := GetDstHost(Client.RequestHostName, DstHost);
  if Result then
  begin
    ServerAddr := DstHost.Host;
    ServerPort := DstHost.Port;
  end else
  begin
    Client.Answer503;
  end;
end;

procedure TIocpHttpTunnel.ServiceCreate(Sender: TObject);
begin
  FTunnelServer := TIocpMultiHttpTunnel.Create(nil);

  try
    IocpHttpTunnelConfig.ReadLock;
    FTunnelServer.Port := IocpHttpTunnelConfig.Port;
    FTunnelServer.Timeout := IocpHttpTunnelConfig.Timeout;
    FTunnelServer.ClientLife := IocpHttpTunnelConfig.Lifeout;
  finally
    IocpHttpTunnelConfig.ReadUnlock;
  end;
end;

procedure TIocpHttpTunnel.ServiceDestroy(Sender: TObject);
begin
  FTunnelServer.Free;
end;

procedure TIocpHttpTunnel.ServiceStart(Sender: TService; var Started: Boolean);
begin
  Started := FTunnelServer.Start;
end;

procedure TIocpHttpTunnel.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  Stopped := FTunnelServer.Stop;
end;

end.
