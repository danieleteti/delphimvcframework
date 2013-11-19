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
  TestControllersU in 'TestControllersU.pas',
  MVCFramework.RESTClient in '..\..\sources\MVCFramework.RESTClient.pas',
  BusinessObjectsU in '..\..\samples\commons\BusinessObjectsU.pas',
  ObjectsMappers in '..\..\sources\ObjectsMappers.pas',
  BOs in 'BOs.pas';

{$R *.RES}


begin
  DUnitTestRunner.RunRegisteredTests;

end.
