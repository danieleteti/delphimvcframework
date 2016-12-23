program DMVCFrameworkTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}


uses
  DUnitTestRunner,
  FrameworkTestsU in 'FrameworkTestsU.pas',
  LiveServerTestU in 'LiveServerTestU.pas',
  MessagingExtensionsTestU in 'MessagingExtensionsTestU.pas',
  BusinessObjectsU in '..\..\samples\commons\BusinessObjectsU.pas',
  BOs in 'BOs.pas',
  TestServerControllerU in '..\TestServer\TestServerControllerU.pas',
  RESTAdapterTestsU in 'RESTAdapterTestsU.pas',
  MVCFramework.HMAC in '..\..\sources\MVCFramework.HMAC.pas',
  MVCFramework.Tests.WebModule2 in '..\StandaloneServer\MVCFramework.Tests.WebModule2.pas' {TestWebModule2: TWebModule},
  MVCFramework.Tests.StandaloneServer in '..\StandaloneServer\MVCFramework.Tests.StandaloneServer.pas',
  MVCFramework.Tests.WebModule1 in '..\RESTClient\MVCFramework.Tests.WebModule1.pas' {TestWebModule1: TWebModule},
  MVCFramework.Tests.RESTClient in '..\RESTClient\MVCFramework.Tests.RESTClient.pas',
  MVCFramework.Tests.AppController in '..\RESTClient\MVCFramework.Tests.AppController.pas';

{$R *.RES}


begin

  ReportMemoryLeaksOnShutdown := True;

  DUnitTestRunner.RunRegisteredTests;

{$IFDEF CONSOLE_TESTRUNNER}
  write('Press return to continue...');
  ReadLn;
{$ENDIF}

end.
