program jsonrpcclient;

uses
  Vcl.Forms,
  MainClientFormU in 'MainClientFormU.pas' {Form10};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm10, Form10);
  Application.Run;
end.
