program JWTBlacklistClientSample;

uses
  Vcl.Forms,
  MainClientFormU in 'MainClientFormU.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
