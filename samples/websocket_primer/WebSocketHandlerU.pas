unit WebSocketHandlerU;

interface

uses
  MVCFramework.WebSocketServer;

type
  TWebSocketHandler = class
  public
    procedure Connect(WSContext: TMVCWebSocketIOHandler);
    procedure Disconnect(WSContext: TMVCWebSocketIOHandler);
    procedure MessageArrived(WSContext: TMVCWebSocketIOHandler);
  end;

implementation

uses
  MVCFramework.Logger;

{ TWebSocketHandler }

procedure TWebSocketHandler.Connect(WSContext: TMVCWebSocketIOHandler);
begin
  Log.Debug('TWebSocketHandler.Connect', 'wslog');
end;

procedure TWebSocketHandler.Disconnect(WSContext: TMVCWebSocketIOHandler);
begin
  Log.Debug('TWebSocketHandler.Disconnect', 'wslog');
end;

procedure TWebSocketHandler.MessageArrived(WSContext: TMVCWebSocketIOHandler);
var
  lData: string;
begin
  Log.Debug('TWebSocketHandler.MessageArrived', 'wslog');
  lData := WSContext.ReadString;
  if lData <> '' then
  begin
    LogD(lData);
    WSContext.WriteString(lData);
  end;
end;

end.
