program BasicDemo;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  Winapi.Windows,
  Winapi.ShellAPI,
  Web.WebReq,
  Web.WebBroker,
  MVCFramework.Console,
  MVCFramework.Server,
  MVCFramework.Server.Impl,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule} ,
  App1MainControllerU in 'App1MainControllerU.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServerListenerCtx: IMVCListenersContext;
begin
  Writeln(Format('Starting HTTP Server or port %d', [APort]));

  LServerListenerCtx := TMVCListenersContext.Create;

  LServerListenerCtx.Add(TMVCListenerProperties.New
    .SetName('BasicDemo')
    .SetPort(APort)
    .SetMaxConnections(1024)
    .SetWebModuleClass(WebModuleClass)
    );

  LServerListenerCtx.StartAll;

{$IFNDEF LINUX}
  ShellExecute(0, 'open', pChar('http://localhost:' + inttostr(APort) +
    '/div/10/20'), nil, nil, SW_SHOWMAXIMIZED);

{$ENDIF}
  Writeln('Press RETURN to stop the server');
  ReadLn;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    RunServer(3000);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
