program teststompclient;
{$APPTYPE CONSOLE}


uses
  MainU in 'MainU.pas',
  SysUtils,
  StompClient in '..\StompClient.pas',
  StompTypes in '..\StompTypes.pas';

const
  SERVERNAME =
    'localhost';
//    '192.168.3.72';

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    Main(SERVERNAME, STOMP_Version_1_0);
    //Main(SERVERNAME, STOMP_Version_1_1); // Your STOMP server supports protocol 1.1 ?
    //MainWithTransaction;
    //Test_Unicode_Chars;
    Writeln('ALL TESTS OK');
  except
    on E: Exception do
      Writeln(E.Classname, ': ', E.message);
  end;

  readln;

end.
