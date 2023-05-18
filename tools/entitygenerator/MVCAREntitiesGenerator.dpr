program MVCAREntitiesGenerator;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {MainForm},
  UtilsU in 'UtilsU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
