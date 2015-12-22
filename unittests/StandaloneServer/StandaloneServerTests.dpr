program StandaloneServerTests;
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
  MVCFramework.Tests.StandaloneServer in 'MVCFramework.Tests.StandaloneServer.pas',
  MVCFramework.Tests.WebModule in 'MVCFramework.Tests.WebModule.pas' {TestWebModule: TWebModule};

{$R *.RES}

begin

  ReportMemoryLeaksOnShutdown := True;

  DUnitTestRunner.RunRegisteredTests;

end.
