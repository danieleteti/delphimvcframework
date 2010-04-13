program TestStompClient;
{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestStompClientU in 'TestStompClientU.pas';

{$R *.RES}

var
  ExCode: Integer;
  TestResult: TTestResult;

begin
  ExCode := 0;
  Application.Initialize;
  if IsConsole then
  begin
    TestResult := TextTestRunner.RunRegisteredTests;
    try
      ExCode := TestResult.ErrorCount + TestResult.FailureCount;
    finally
      TestResult.Free;
    end;
  end
  else
    GUITestRunner.RunRegisteredTests;
  Halt(ExCode);
end.
