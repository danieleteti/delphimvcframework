program IocpPacketClient;

uses
  Vcl.Forms,
  IocpClientMain in 'IocpClientMain.pas' {fmIocpClient};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmIocpClient, fmIocpClient);
  Application.Run;
end.
