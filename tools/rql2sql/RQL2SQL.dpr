program RQL2SQL;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {MainForm},
  MVCFramework.RQL.AST2FirebirdSQL in '..\..\sources\MVCFramework.RQL.AST2FirebirdSQL.pas',
  MVCFramework.RQL.Parser in '..\..\sources\MVCFramework.RQL.Parser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
