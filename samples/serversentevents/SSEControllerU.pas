unit SSEControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.SSEController, MVCFramework.SSE;

type
  [MVCPath('/stocks')]
  TMySSEController = class(TMVCSSEController)
  protected
    procedure OnClientConnected(const AConnection: TSSEConnection); override;
    procedure OnClientDisconnected(const AConnection: TSSEConnection); override;
  end;

implementation

uses
  MVCFramework.Logger, System.SysUtils;

{ TMySSEController }

procedure TMySSEController.OnClientConnected(const AConnection: TSSEConnection);
begin
  LogI('SSE client connected: %s (channel: %s)',
    [AConnection.ClientId, ChannelName], 'sse');
end;

procedure TMySSEController.OnClientDisconnected(const AConnection: TSSEConnection);
begin
  LogI('SSE client disconnected: %s (channel: %s)',
    [AConnection.ClientId, ChannelName], 'sse');
end;

end.
