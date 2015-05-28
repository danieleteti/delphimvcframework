unit IocpDsServer;

{$define __IOCP_DATASNAP__}

interface

uses
  {$ifdef __IOCP_DATASNAP__}
  Iocp.DSHTTPWebBroker
  {$else}
  IdHTTPWebBrokerBridge
  {$endif};

type
  TDataSnapServer = class
  private
    {$ifdef __IOCP_DATASNAP__}
    class var FServer: TIocpWebBrokerBridge;
    {$else}
    class var FServer: TIdHTTPWebBrokerBridge;
    {$endif}
    class function GetActive: Boolean; static;
    class procedure SetActive(const Value: Boolean); static;
  public
    class procedure StartServer(Port: Word);
    class procedure StopServer;
    class property Active: Boolean read GetActive write SetActive;
  end;

implementation

uses
  Datasnap.DSSession;

{ TDataSnapServer }

class function TDataSnapServer.GetActive: Boolean;
begin
  Result := FServer.Active;
end;

class procedure TDataSnapServer.SetActive(const Value: Boolean);
begin
  FServer.Active := Value;
end;

class procedure TDataSnapServer.StartServer(Port: Word);
begin
  if not FServer.Active then
  begin
    {$ifdef __IOCP_DATASNAP__}
    FServer.Port := Port;
    {$else}
    FServer.Bindings.Clear;
    FServer.DefaultPort := Port;
    {$endif}
    FServer.Active := True;
  end;
end;

class procedure TDataSnapServer.StopServer;
begin
  if (TDSSessionManager.Instance <> nil) then
    TDSSessionManager.Instance.TerminateAllSessions;

  if FServer.Active then
  begin
    FServer.Active := False;
  end;
end;

initialization
  {$ifdef __IOCP_DATASNAP__}
  TDataSnapServer.FServer := TIocpWebBrokerBridge.Create(nil);
  {$else}
  TDataSnapServer.FServer := TIdHTTPWebBrokerBridge.Create(nil);
  {$endif}

finalization
  TDataSnapServer.FServer.Free;

end.
