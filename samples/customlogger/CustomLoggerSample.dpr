program CustomLoggerSample;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  MVCFramework.Logger,

  {$IFDEF MSWINDOWS}

  Winapi.Windows,
  Winapi.ShellAPI,

  {$ENDIF}

  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  MyControllerU in 'MyControllerU.pas',
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule} ,
  CustomLoggerConfigU in 'CustomLoggerConfigU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln('** DMVCFramework Server **');
  Writeln('WARNING! Run this program in debug and check the Delphi "Event" debug window to see the custom logs');
  Writeln('WARNING! Also, the log file are generated in the custom path "MyFolder\MyLogs"');
  Writeln(Format('Starting HTTP Server on port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;

    {$IFDEF MSWINDOWS}

    ShellExecute(0, 'open', pChar('http://localhost:' + inttostr(APort)), nil, nil,
      SW_SHOWMAXIMIZED);

    {$ENDIF}

    Writeln('Press RETURN to stop the server');
    ReadLn;
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
