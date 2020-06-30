program JWTClient;

uses
  Vcl.Forms,
  MainClientFormU in 'MainClientFormU.pas' {Form5};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
