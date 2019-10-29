program RQL2SQL;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {MainForm},
  MVCFramework.RQL.AST2FirebirdSQL in '..\..\sources\MVCFramework.RQL.AST2FirebirdSQL.pas',
  MVCFramework.RQL.Parser in '..\..\sources\MVCFramework.RQL.Parser.pas',
  MVCFramework.RQL.AST2MySQL in '..\..\sources\MVCFramework.RQL.AST2MySQL.pas',
  MVCFramework.RQL.AST2InterbaseSQL in '..\..\sources\MVCFramework.RQL.AST2InterbaseSQL.pas',
  MVCFramework.RQL.AST2PostgreSQL in '..\..\sources\MVCFramework.RQL.AST2PostgreSQL.pas',
  MVCFramework.RQL.AST2MSSQL in '..\..\sources\MVCFramework.RQL.AST2MSSQL.pas',
  MVCFramework.RQL.AST2SQLite in '..\..\sources\MVCFramework.RQL.AST2SQLite.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
