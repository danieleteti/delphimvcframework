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
  HudsonTestRunner in
  'C:\Program Files\Embarcadero\RAD Studio\7.0\source\dUnit\Contrib\HudsonReporting\HudsonTestRunner.pas';
{$R *.RES}

var
  ExCode: Cardinal;
begin
  Application.Initialize;
  if IsConsole then
    with HudsonTestRunner.RunRegisteredTests do
    begin
      ExCode := THudsonTestListener.GetErrorCount;
      Free
    end
    else
      GUITestRunner.RunRegisteredTests;
  if ExCode > 0 then
    ExitCode := 1;
end.
