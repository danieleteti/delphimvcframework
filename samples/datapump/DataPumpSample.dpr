program DataPumpSample;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {MainForm},
  FDConnectionConfigU in 'FDConnectionConfigU.pas',
  CustomerEntityU in 'CustomerEntityU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
