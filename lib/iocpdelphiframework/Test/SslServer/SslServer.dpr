program SslServer;

uses
  Vcl.Forms,
  SslServerMain in 'SslServerMain.pas' {fmIocpServer};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmIocpServer, fmIocpServer);
  Application.Run;
end.
