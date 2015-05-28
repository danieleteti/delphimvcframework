program IocpPacketServer;

uses
  Vcl.Forms,
  IocpServerMain in 'IocpServerMain.pas' {fmIocpServer};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmIocpServer, fmIocpServer);
  Application.Run;
end.
