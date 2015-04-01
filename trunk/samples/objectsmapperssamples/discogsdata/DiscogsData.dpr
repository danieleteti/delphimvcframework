program DiscogsData;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {MainForm},
  DiscogsClasses in 'DiscogsClasses.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
