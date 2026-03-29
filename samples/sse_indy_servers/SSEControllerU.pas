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
    /// <summary>
    /// Called at every interval. Generates a random stock price update
    /// and sends it to the connected client.
    /// This demonstrates the "notify on change" pattern where the SSE
    /// controller itself checks for new data (or generates it) and pushes
    /// events without needing an external thread.
    /// </summary>
    procedure OnInterval(const AConnection: TSSEConnection;
      var ANextIntervalMS: Integer); override;
    function Interval: Integer; override;
  end;

implementation

uses
  MVCFramework.Logger, System.SysUtils, System.JSON;

const
  STOCK_NAMES: array[0..3] of string = ('IBM', 'AAPL', 'GOOG', 'MSFT');

{ TMySSEController }

procedure TMySSEController.OnClientConnected(const AConnection: TSSEConnection);
begin
  LogI('SSE client connected: %s (channel: %s)',
    [AConnection.ClientId, ChannelName], 'sse');
  // Store an initial event counter in CustomData
  AConnection.CustomData := TObject(0); // Use as Integer via pointer cast
end;

procedure TMySSEController.OnClientDisconnected(const AConnection: TSSEConnection);
begin
  LogI('SSE client disconnected: %s (channel: %s)',
    [AConnection.ClientId, ChannelName], 'sse');
end;

function TMySSEController.Interval: Integer;
begin
  // Base interval: generate a stock update roughly every second
  Result := 1000;
end;

procedure TMySSEController.OnInterval(const AConnection: TSSEConnection;
  var ANextIntervalMS: Integer);
var
  LEventId: NativeInt;
  LStockIdx: Integer;
  LJSON: TJSONObject;
begin
  // Increment the per-connection event counter
  LEventId := NativeInt(AConnection.CustomData) + 1;
  AConnection.CustomData := TObject(LEventId);

  LStockIdx := Random(Length(STOCK_NAMES));

  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('stock', STOCK_NAMES[LStockIdx]);
    LJSON.AddPair('value', TJSONNumber.Create((500 + Random(200)) + (Random(50) / 100)));

    AConnection.Send(
      TSSEMessage.Create('stockupdate', LJSON.ToJSON, LEventId.ToString));
  finally
    LJSON.Free;
  end;

  // Adaptive interval: randomize between 1000-1500ms for a realistic feel
  ANextIntervalMS := 1000 + Random(500);
end;

end.
