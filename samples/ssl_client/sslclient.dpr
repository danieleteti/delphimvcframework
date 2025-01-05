program sslclient;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {Form9};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
