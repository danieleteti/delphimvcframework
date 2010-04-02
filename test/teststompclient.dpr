program teststompclient;

{$APPTYPE CONSOLE}

uses
  MainU in 'MainU.pas',
  SysUtils,
  StompClient in '..\StompClient.pas',
  StompTypes in '..\StompTypes.pas';

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

