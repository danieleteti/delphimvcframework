program teststompclient2007;

{$APPTYPE CONSOLE}

uses
  StompTypes in 'StompTypes.pas',
  StompClient in 'StompClient.pas',
  MainU in 'MainU.pas',
  SysUtils,
  StopWatch in 'StopWatch.pas';

var
  address: string;
begin
  if ParamCount = 1 then
    address := paramstr(1)
  else
    address := 'localhost';
  try
    Main(address);
  except
    on E: Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.

