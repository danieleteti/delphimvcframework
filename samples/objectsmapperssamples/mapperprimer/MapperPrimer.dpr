program MapperPrimer;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {MainForm},
  PersonU in 'PersonU.pas',
  BusinessObjectsU in 'BusinessObjectsU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
