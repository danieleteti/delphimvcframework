program jwtplainclient;

uses
  Vcl.Forms,
  MainClientFormU in 'MainClientFormU.pas' {Form7};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
