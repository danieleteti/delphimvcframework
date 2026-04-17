program ParityCheck;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  ParityTestsU in 'ParityTestsU.pas';

begin
  try
    ExitCode := RunParityTests;
  except
    on E: Exception do
    begin
      Writeln('FATAL: ', E.ClassName, ': ', E.Message);
      ExitCode := 2;
    end;
  end;
end.
