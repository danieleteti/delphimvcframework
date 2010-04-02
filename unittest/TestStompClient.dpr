program TestStompClient;
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
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestStompClientU in 'TestStompClientU.pas',
  XMLTestRunner in 'C:\Program Files\Embarcadero\RAD Studio\7.0\source\dUnit\Contrib\XMLReporting\XMLTestRunner.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with XMLTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

