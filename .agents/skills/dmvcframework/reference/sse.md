# DMVCFramework — Server-Sent Events

`TMVCSSEController` and the global SSE broker.

---

## Server-Sent Events (SSE)

Do **not** hand-roll the stream. Inherit `TMVCSSEController` and override its hooks — the base class owns
the connection, the heartbeat and the `text/event-stream` framing.

```delphi
uses MVCFramework, MVCFramework.Commons, MVCFramework.SSEController, MVCFramework.SSE;

type
  [MVCPath('/stocks')]
  TMySSEController = class(TMVCSSEController)
  protected
    procedure OnClientConnected(const AConnection: TSSEConnection); override;
    procedure OnClientDisconnected(const AConnection: TSSEConnection); override;
    procedure OnInterval(const AConnection: TSSEConnection; var ANextIntervalMS: Integer); override;
    function Interval: Integer; override;   // default tick, in ms
  end;

procedure TMySSEController.OnInterval(const AConnection: TSSEConnection;
  var ANextIntervalMS: Integer);
begin
  AConnection.Send(TSSEMessage.Create('stockupdate', lJSON.ToJSON, lEventID.ToString));
  ANextIntervalMS := 500;   // optional: adapt the next tick for this connection only
end;
```

- Register it like any other controller: `AEngine.AddController(TMySSEController)`.
- `TSSEConnection`: `Send(TSSEMessage)`, `SendComment(text)`, `ClientId`, `LastEventId`,
  `CustomData: TObject` for per-connection state. (`ChannelName` is a method of `TMVCSSEController`,
  not of the connection.)
- **Push from outside the controller** (e.g. from a POST action) via the global broker:
  `SSEBroker.Broadcast('/chat', TSSEMessage.Create(...))`, or `SSEBroker.SendTo(channel, clientId, msg)`.

Samples: `samples/sse/`, `samples/sse_chat/`.

---
