program unidac_activerecord_showcase;

uses
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {MainForm},
  EntitiesU in 'EntitiesU.pas',
  MVCFramework.ActiveRecord.UniDAC in '..\..\sources\MVCFramework.ActiveRecord.UniDAC.pas',
  MVCFramework.ActiveRecord in '..\..\sources\MVCFramework.ActiveRecord.pas',
  MVCFramework.Commons in '..\..\sources\MVCFramework.Commons.pas',
  MVCFramework.RQL.Parser in '..\..\sources\MVCFramework.RQL.Parser.pas',
  MVCFramework.RQL.AST in '..\..\sources\MVCFramework.RQL.AST.pas',
  MVCFramework.RQL.Token in '..\..\sources\MVCFramework.RQL.Token.pas',
  MVCFramework.SQLGenerators.Interfaces in '..\..\sources\MVCFramework.SQLGenerators.Interfaces.pas',
  MVCFramework.SQLGenerators.Factory in '..\..\sources\MVCFramework.SQLGenerators.Factory.pas',
  MVCFramework.SQLGenerators.MSSQL in '..\..\sources\MVCFramework.SQLGenerators.MSSQL.pas',
  MVCFramework.SQLGenerators.MySQL in '..\..\sources\MVCFramework.SQLGenerators.MySQL.pas',
  MVCFramework.SQLGenerators.PostgreSQL in '..\..\sources\MVCFramework.SQLGenerators.PostgreSQL.pas',
  MVCFramework.SQLGenerators.Firebird in '..\..\sources\MVCFramework.SQLGenerators.Firebird.pas',
  MVCFramework.SQLGenerators.SQLite in '..\..\sources\MVCFramework.SQLGenerators.SQLite.pas',
  MVCFramework.SQLGenerators.Interbase in '..\..\sources\MVCFramework.SQLGenerators.Interbase.pas',
  MVCFramework.Nullables in '..\..\sources\MVCFramework.Nullables.pas',
  MVCFramework.Log.Logger in '..\..\sources\MVCFramework.Log.Logger.pas',
  MVCFramework.Log.Appenders.File in '..\..\sources\MVCFramework.Log.Appenders.File.pas',
  MVCFramework.Log.Appenders.Console in '..\..\sources\MVCFramework.Log.Appenders.Console.pas',
  MVCFramework.Log.Appenders.OutputDebugString in '..\..\sources\MVCFramework.Log.Appenders.OutputDebugString.pas',
  MVCFramework.DuckTyping in '..\..\sources\MVCFramework.DuckTyping.pas',
  MVCFramework.Helpers.JSON in '..\..\sources\MVCFramework.Helpers.JSON.pas',
  MVCFramework.Serializer.Defaults in '..\..\sources\MVCFramework.Serializer.Defaults.pas',
  MVCFramework.Serializer.Intf in '..\..\sources\MVCFramework.Serializer.Intf.pas',
  MVCFramework.Serializer.JsonDataObjects in '..\..\sources\MVCFramework.Serializer.JsonDataObjects.pas',
  MVCFramework.ActiveRecord.Intf in '..\..\sources\MVCFramework.ActiveRecord.Intf.pas',
  MVCFramework.ActiveRecord.Mapper in '..\..\sources\MVCFramework.ActiveRecord.Mapper.pas',
  MVCFramework.ActiveRecord.Stub in '..\..\sources\MVCFramework.ActiveRecord.Stub.pas',
  MVCFramework.ActiveRecord.ConnectionManager in '..\..\sources\MVCFramework.ActiveRecord.ConnectionManager.pas',
  MVCFramework.ActiveRecord.Cache in '..\..\sources\MVCFramework.ActiveRecord.Cache.pas',
  MVCFramework.UniDAC.Utils in '..\..\sources\MVCFramework.UniDAC.Utils.pas',
  UniConnectionConfigU in 'UniConnectionConfigU.pas',
  EngineChoiceFormU in 'EngineChoiceFormU.pas' {EngineChoiceForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TEngineChoiceForm, EngineChoiceForm);
  Application.Run;
end.
