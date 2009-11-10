program teststompclient;

{$APPTYPE CONSOLE}

uses
  StompTypes in 'StompTypes.pas',
  StompClient in 'StompClient.pas',
  MainU in 'MainU.pas',
  SysUtils,
  StopWatch in 'StopWatch.pas';

begin
  try
//    Main;
    MainWithTransaction;
//    Test_Unicode_Chars; //Non passa
    Writeln('DONE');
  except
    on E: Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  readln;
end.

