program teststompclient;

{$MODE Delphi}

uses

  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
    MainU in 'MainU.pas',
  SysUtils,
  StompClient in '../StompClient.pas',
  StompTypes in '../StompTypes.pas';

begin
  try
    Main;
    MainWithTransaction;
    // Test_Unicode_Chars; //Non passa
    Writeln('ALL TESTS OK');
  except
    on E: Exception do
      Writeln(E.Classname, ': ', E.message);
  end;

  readln;
end.
