program SslClient;

uses
  Vcl.Forms,
  SslClientMain in 'SslClientMain.pas' {fmIocpClient};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmIocpClient, fmIocpClient);
  Application.Run;
end.
