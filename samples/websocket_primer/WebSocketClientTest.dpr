program WebSocketClientTest;

uses
  Vcl.Forms,
  WebSocketClientTestU in 'WebSocketClientTestU.pas' {FormWebSocketClient};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormWebSocketClient, FormWebSocketClient);
  Application.Run;
end.
