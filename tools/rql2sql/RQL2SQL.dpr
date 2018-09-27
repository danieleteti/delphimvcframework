program RQL2SQL;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {MainForm},
  MVCFramework.RQL2SQL in '..\..\sources\MVCFramework.RQL2SQL.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
