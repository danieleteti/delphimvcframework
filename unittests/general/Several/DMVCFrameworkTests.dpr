program DMVCFrameworkTests;

{$IFNDEF TESTINSIGHT}
{$IFNDEF GUI_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}{$ENDIF}{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  {$IFDEF GUI_TESTRUNNER}
  Vcl.Forms,
  DUnitX.Loggers.GUI.Vcl,
  {$ENDIF }
  {$IFDEF CONSOLE_TESTRUNNER}
  DUnitX.Loggers.Console,
  {$ENDIF }
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  FrameworkTestsU in 'FrameworkTestsU.pas',
  LiveServerTestU in 'LiveServerTestU.pas',
  MessagingExtensionsTestU in 'MessagingExtensionsTestU.pas',
  BOs in 'BOs.pas',
  TestServerControllerU in '..\TestServer\TestServerControllerU.pas',
  RESTAdapterTestsU in 'RESTAdapterTestsU.pas',
  MVCFramework.Tests.WebModule2 in '..\StandaloneServer\MVCFramework.Tests.WebModule2.pas' {TestWebModule2: TWebModule},
  MVCFramework.Tests.StandaloneServer in '..\StandaloneServer\MVCFramework.Tests.StandaloneServer.pas',
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
  MVCFramework.Serializer.JsonDataObjects.OptionalCustomTypes in '..\..\..\sources\MVCFramework.Serializer.JsonDataObjects.OptionalCustomTypes.pas';

{$R *.RES}
{$IFDEF CONSOLE_TESTRUNNER}

procedure MainConsole();
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger: ITestLogger;
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
    // nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    // runner.AddLogger(nunitLogger);
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
{$IFDEF GUI_TESTRUNNER}

procedure MainGUI;
begin
  Application.Initialize;
  Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
  Application.Run;
end;
{$ENDIF}

begin
  ReportMemoryLeaksOnShutdown := True;
{$IFDEF CONSOLE_TESTRUNNER}
  MainConsole();
{$ENDIF}
{$IFDEF GUI_TESTRUNNER}
  MainGUI();
{$ENDIF}

end.
