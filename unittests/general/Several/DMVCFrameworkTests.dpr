program DMVCFrameworkTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}


uses
  System.SysUtils,
  System.IOUtils,
  DUnitX.TestFramework,
  DUnitX.Loggers.XML.NUnit,
  {$IFDEF CONSOLE_TESTRUNNER}
  DUnitX.Loggers.Console,
  {$ENDIF }
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  FrameworkTestsU in 'FrameworkTestsU.pas',
  LiveServerTestU in 'LiveServerTestU.pas',
  BOs in 'BOs.pas',
  TestServerControllerU in '..\TestServer\TestServerControllerU.pas',
  RESTAdapterTestsU in 'RESTAdapterTestsU.pas',
  MVCFramework.Tests.WebModule1 in '..\RESTClient\MVCFramework.Tests.WebModule1.pas' {TestWebModule1: TWebModule},
  MVCFramework.Tests.RESTClient in '..\RESTClient\MVCFramework.Tests.RESTClient.pas',
  MVCFramework.Tests.AppController in '..\RESTClient\MVCFramework.Tests.AppController.pas',
  BusinessObjectsU in '..\..\..\samples\commons\BusinessObjectsU.pas',
  MVCFramework.SystemJSONUtils in '..\..\..\sources\MVCFramework.SystemJSONUtils.pas',
  MVCFramework.Serializer.Commons in '..\..\..\sources\MVCFramework.Serializer.Commons.pas',
  MVCFramework.Patches in '..\..\..\sources\MVCFramework.Patches.pas',
  JSONRPCTestsU in 'JSONRPCTestsU.pas',
  MVCFramework.JSONRPC in '..\..\..\sources\MVCFramework.JSONRPC.pas',
  RandomUtilsU in '..\..\..\samples\commons\RandomUtilsU.pas',
  MVCFramework.Serializer.JsonDataObjects in '..\..\..\sources\MVCFramework.Serializer.JsonDataObjects.pas',
  JsonDataObjects in '..\..\..\sources\JsonDataObjects.pas',
  Serializers.JsonDataObjectsTestU in 'Serializers.JsonDataObjectsTestU.pas',
  MVCFramework.Tests.Serializer.Entities in '..\..\common\MVCFramework.Tests.Serializer.Entities.pas',
  MVCFramework.Tests.Serializer.EntitiesModule in '..\..\common\MVCFramework.Tests.Serializer.EntitiesModule.pas' {EntitiesModule: TDataModule},
  MVCFramework.Tests.Serializer.Intf in '..\..\common\MVCFramework.Tests.Serializer.Intf.pas',
  MVCFramework.Serializer.JsonDataObjects.OptionalCustomTypes in '..\..\..\sources\MVCFramework.Serializer.JsonDataObjects.OptionalCustomTypes.pas',
  ActiveRecordTestsU in 'ActiveRecordTestsU.pas',
  TestConstsU in 'TestConstsU.pas',
  MVCFramework.RESTClient.Indy in '..\..\..\sources\MVCFramework.RESTClient.Indy.pas',
  MVCFramework.RESTClient.Intf in '..\..\..\sources\MVCFramework.RESTClient.Intf.pas',
  MVCFramework.RESTClient.Commons in '..\..\..\sources\MVCFramework.RESTClient.Commons.pas',
  MVCFramework.RESTClient in '..\..\..\sources\MVCFramework.RESTClient.pas',
  PGUtilsU in 'PGUtilsU.pas',
  MVCFramework.ActiveRecord in '..\..\..\sources\MVCFramework.ActiveRecord.pas',
  MVCFramework.ActiveRecordController in '..\..\..\sources\MVCFramework.ActiveRecordController.pas',
  ActiveRecordControllerTestU in 'ActiveRecordControllerTestU.pas',
  ActiveRecordControllerWebModuleU in 'webmodules\ActiveRecordControllerWebModuleU.pas' {ActiveRecordControllerWebModule: TWebModule},
  FDConnectionConfigU in '..\..\common\FDConnectionConfigU.pas',
  StandaloneServerTestU in 'StandaloneServerTestU.pas',
  StandAloneServerWebModuleTest in 'webmodules\StandAloneServerWebModuleTest.pas' {TestWebModule2: TWebModule},
  MVCFramework.Commons in '..\..\..\sources\MVCFramework.Commons.pas',
  MVCFramework.Serializer.JsonDataObjects.CustomTypes in '..\..\..\sources\MVCFramework.Serializer.JsonDataObjects.CustomTypes.pas',
  MVCFramework.SQLGenerators.Firebird in '..\..\..\sources\MVCFramework.SQLGenerators.Firebird.pas',
  MVCFramework.Utils in '..\..\..\sources\MVCFramework.Utils.pas',
  MVCFramework.SQLGenerators.Interbase in '..\..\..\sources\MVCFramework.SQLGenerators.Interbase.pas',
  MVCFramework.SQLGenerators.MSSQL in '..\..\..\sources\MVCFramework.SQLGenerators.MSSQL.pas',
  MVCFramework.SQLGenerators.MySQL in '..\..\..\sources\MVCFramework.SQLGenerators.MySQL.pas',
  MVCFramework.SQLGenerators.PostgreSQL in '..\..\..\sources\MVCFramework.SQLGenerators.PostgreSQL.pas',
  MVCFramework.SQLGenerators.Sqlite in '..\..\..\sources\MVCFramework.SQLGenerators.Sqlite.pas',
  MVCFramework.RQL.AST2FirebirdSQL in '..\..\..\sources\MVCFramework.RQL.AST2FirebirdSQL.pas',
  MVCFramework.RQL.AST2InterbaseSQL in '..\..\..\sources\MVCFramework.RQL.AST2InterbaseSQL.pas',
  MVCFramework.RQL.AST2MSSQL in '..\..\..\sources\MVCFramework.RQL.AST2MSSQL.pas',
  MVCFramework.RQL.AST2MySQL in '..\..\..\sources\MVCFramework.RQL.AST2MySQL.pas',
  MVCFramework.RQL.AST2PostgreSQL in '..\..\..\sources\MVCFramework.RQL.AST2PostgreSQL.pas',
  MVCFramework.RQL.AST2SQLite in '..\..\..\sources\MVCFramework.RQL.AST2SQLite.pas',
  MVCFramework.RQL.Parser in '..\..\..\sources\MVCFramework.RQL.Parser.pas',
  Entities in 'Entities.pas',
  EntitiesProcessors in 'EntitiesProcessors.pas',
  MVCFramework.Nullables in '..\..\..\sources\MVCFramework.Nullables.pas',
  IntfObjectPoolTestU in 'IntfObjectPoolTestU.pas',
  ObjectPoolTestU in 'ObjectPoolTestU.pas',
  MVCFramework.DotEnv.Parser in '..\..\..\sources\MVCFramework.DotEnv.Parser.pas',
  MVCFramework.DotEnv in '..\..\..\sources\MVCFramework.DotEnv.pas';

{$R *.RES}

{$IFDEF CONSOLE_TESTRUNNER}


procedure MainConsole();
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  OutputNUnitFolder: String;
begin
  try
    // Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    // Create the test runner
    runner := TDUnitX.CreateRunner;
    // Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    // tell the runner how we will log things
    // Log to the console window
    logger := TDUnitXConsoleLogger.Create(True);
    runner.AddLogger(logger);

    // Generate an NUnit compatible XML File
    if TDUnitX.Options.XMLOutputFile.IsEmpty then
    begin
      OutputNUnitFolder := TPath.Combine(
        TDirectory.GetParent(TDirectory.GetParent(TDirectory.GetParent(AppPath))), 'UnitTestReports');
      TDirectory.CreateDirectory(OutputNUnitFolder);
      {$if defined(win32)}
      TDUnitX.Options.XMLOutputFile := TPath.Combine(OutputNUnitFolder,'dmvcframework_nunit_win32.xml');
      {$endif}
      {$if defined(win64)}
      TDUnitX.Options.XMLOutputFile := TPath.Combine(OutputNUnitFolder, 'dmvcframework_nunit_win64.xml');
      {$endif}
      {$if defined(linux64)}
      TDUnitX.Options.XMLOutputFile := TPath.Combine(OutputNUnitFolder, 'dmvcframework_nunit_linux64.xml');
      {$endif}
    end;

    runner.AddLogger(TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile));
    runner.FailsOnNoAsserts := False; // When true, Assertions must be made during tests;

    // Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

{$IFNDEF CI}
    // We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
{$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end;
{$ENDIF}


begin
  ReportMemoryLeaksOnShutdown := True;
{$IF Defined(CONSOLE_TESTRUNNER)}
  MainConsole();
{$ELSE}
{$IF Defined(TESTINSIGHT)}
  TestInsight.DUnitX.RunRegisteredTests();
{$ELSE}
  raise Exception.Create('No Runner defined');
{$ENDIF}
{$ENDIF}

end.
