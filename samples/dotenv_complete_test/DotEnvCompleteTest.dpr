program DotEnvCompleteTest;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  MVCFramework.DotEnv.Parser,
  TestFramework in 'TestFramework.pas',
  DotEnvTestSuite in 'DotEnvTestSuite.pas';

var
  TestSuite: TDotEnvTestSuite;
  Results: TTestResults;

begin
  try
    WriteLn('=========================================');
    WriteLn('DMVCFramework DotEnv Complete Test Suite');
    WriteLn('=========================================');
    WriteLn('');

    TestSuite := TDotEnvTestSuite.Create;
    try
      Results := TestSuite.RunAllTests;

      WriteLn('');
      WriteLn('=========================================');
      WriteLn('TEST RESULTS SUMMARY');
      WriteLn('=========================================');
      WriteLn(Format('Total Tests: %d', [Results.TotalTests]));
      WriteLn(Format('Passed: %d', [Results.PassedTests]));
      WriteLn(Format('Failed: %d', [Results.FailedTests]));
      WriteLn(Format('Success Rate: %.1f%%', [Results.SuccessRate]));
      WriteLn('');

      if Results.FailedTests > 0 then
      begin
        WriteLn('FAILED TESTS:');
        WriteLn('-------------');
        for var FailedTest in Results.FailedTestNames do
          WriteLn('- ' + FailedTest);
        WriteLn('');
      end;

      if Results.FailedTests = 0 then
      begin
        WriteLn('🎉 ALL TESTS PASSED! DotEnv implementation is working correctly.');
        ExitCode := 0;
      end
      else
      begin
        WriteLn('❌ Some tests failed. Please check the implementation.');
        ExitCode := 1;
      end;

    finally
      TestSuite.Free;
    end;

    WriteLn('');
    WriteLn('Press Enter to exit...');
    ReadLn;

  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 2;
    end;
  end;
end.