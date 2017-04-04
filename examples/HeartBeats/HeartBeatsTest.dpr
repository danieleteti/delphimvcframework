program HeartBeatsTest;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  StompTypes in '..\..\StompTypes.pas',
  StompClient in '..\..\StompClient.pas', System.Classes, System.SysUtils;

procedure Main;
var
  lSTOMP: IStompClient;
  lFrame: IStompFrame;
const
  DESTINATION = '/topic/pippo';
begin
  lSTOMP := TStompClient.Create;
  lSTOMP
    .SetHeartBeat(500, 0) // very low outgoing heartbeat interval
    .SetUserName('guest')
    .SetPassword('guest')
    .Connect('127.0.0.1', 61613, '', TStompAcceptProtocol.Ver_1_1);
  WriteLn('PROTOCOL: ', lSTOMP.GetProtocolVersion);
  WriteLn('SERVER  : ', lSTOMP.GetServer);
  WriteLn('SESSION : ', lSTOMP.GetSession);
  lSTOMP.Subscribe(DESTINATION);

  // let's create a thread to send 10+1 messages to the already defined subscriber
  TThread.CreateAnonymousThread(
    procedure
    var
      lSTOMP: IStompClient;
      I: Integer;
    begin
      lSTOMP := TStompClient.Create;
      lSTOMP
        .SetUserName('guest')
        .SetPassword('guest')
        .Connect('127.0.0.1', 61613, '', TStompAcceptProtocol.Ver_1_1);
      for I := 1 to 10 do
      begin
        lSTOMP.Send(DESTINATION, TGuid.NewGuid.ToString + ' ' + DateTimeToStr(now));
        Sleep(500 + Random(1500));
      end;
      lSTOMP.Send(DESTINATION, 'FINISHED');
    end).Start;

  // start to receive messages
  WriteLn;
  WriteLn('Reading messages...');;
  while True do
  begin
    try
      lFrame := lSTOMP.Receive(5000);
    except
      on E: Exception do
      begin
        WriteLn(E.ClassName, ' ', E.Message);
        Break;
      end;
    end;
    if Assigned(lFrame) then
    begin
      WriteLn(lFrame.Body);
      if lFrame.Body = 'FINISHED' then
      begin
        WriteLn('Bye bye...');
        Break;
      end;
    end;
  end;
end;

begin
  try
    Main;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
  Readln;

end.
