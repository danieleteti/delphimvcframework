program TestStompClient;

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
    ExitCode := -1;
end.
