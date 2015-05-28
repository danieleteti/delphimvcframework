unit Iocp.SimpleServer;

{$i Iocp.Ext.inc}

interface

uses
  System.Classes, Winapi.Windows, Iocp.TcpSocket{$ifdef __IOCP_SSL__},Iocp.SSLSocket{$endif};

type
  TSimpleIocpTcpServer = class({$ifdef __IOCP_SSL__}TIocpSSLSocket{$else}TIocpTcpSocket{$endif})
  private
    FAddr: string;
    FPort: Word;
    FListened: Boolean;
    FInitAcceptNum: Integer;
    FStartTick: DWORD;
    FActive: Boolean;
    procedure SetActive(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent; IoThreadsNumber: Integer); override;
    destructor Destroy; override;

    function Start: Boolean;
    function Stop: Boolean;

    property StartTick: DWORD read FStartTick;
  published
    property Addr: string read FAddr write FAddr;
    property Port: Word read FPort write FPort;
    property InitAcceptNum: Integer read FInitAcceptNum write FInitAcceptNum default INIT_ACCEPTEX_NUM;
    property Active: Boolean read FActive write SetActive;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Iocp', [TSimpleIocpTcpServer]);
end;

{ TSimpleIocpTcpServer }

constructor TSimpleIocpTcpServer.Create(AOwner: TComponent;
  IoThreadsNumber: Integer);
begin
  inherited Create(AOwner, IoThreadsNumber);
  FListened := False;

  FAddr := '';
  FInitAcceptNum := INIT_ACCEPTEX_NUM;
  FStartTick := 0;
end;

destructor TSimpleIocpTcpServer.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TSimpleIocpTcpServer.SetActive(const Value: Boolean);
begin
  if (FActive = Value) then Exit;

  FActive := Value;
  if FActive then
    Start
  else
    Stop;
end;

function TSimpleIocpTcpServer.Start: Boolean;
var
  LPort: Word;
begin
  if FListened then Exit(True);

  StartupWorkers;
  LPort := inherited Listen(FAddr, FPort, FInitAcceptNum);
  FListened := (LPort <> 0);
  Result := FListened;
  if Result then
  begin
    FStartTick := GetTickCount;
    FPort := LPort;
  end;
end;

function TSimpleIocpTcpServer.Stop: Boolean;
begin
  if not FListened then Exit(True);

  ShutdownWorkers;
  FListened := False;
  Result := True;
  FStartTick := 0;
end;

end.
