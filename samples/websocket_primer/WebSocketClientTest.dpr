program WebSocketClientTest;

uses
  Vcl.Forms,
  WebSocketClientTestU in 'WebSocketClientTestU.pas' {MainClientForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainClientForm, MainClientForm);
  Application.Run;
end.
