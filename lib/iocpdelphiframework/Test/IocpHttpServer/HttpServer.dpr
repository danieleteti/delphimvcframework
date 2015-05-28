program HttpServer;

uses
  Vcl.Forms,
  IocpHttpServerMain in 'IocpHttpServerMain.pas' {fmIocpHttpServer};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmIocpHttpServer, fmIocpHttpServer);
  Application.Run;
end.
