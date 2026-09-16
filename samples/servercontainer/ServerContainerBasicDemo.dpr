program ServerContainerBasicDemo;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  Winapi.Windows,
  Winapi.ShellAPI,
  MVCFramework,
  MVCFramework.Console,
  MVCFramework.DotEnv,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.Server,
  MVCFramework.Server.Impl,
  App1MainControllerU in 'App1MainControllerU.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  lServerListeners: IMVCListenersContext;
  lEngineConfig: TProc<TMVCEngine>;
begin
  dotEnvConfigure(
    function: IMVCDotEnv
    begin
      Result := NewDotEnv
               .UseStrategy(TMVCDotEnvPriority.FileThenEnv)
                                     //if available, by default, loads default environment (.env)
               .UseProfile('test') //if available loads the test environment (.env.test)
               .UseProfile('prod') //if available loads the prod environment (.env.prod)
               .UseLogger(procedure(LogItem: String)
                          begin
                            LogW('dotEnv: ' + LogItem);
                          end)
               .Build();             //uses the executable folder to look for .env* files
    end);

  // All four listeners share the same engine configuration: a TMVCListener is
  // now an Indy Direct server (no WebBroker), so the controller stack is wired
  // through SetEngineConfig instead of a TWebModule class.
  lEngineConfig :=
    procedure(AEngine: TMVCEngine)
    begin
      AEngine.AddController(TApp1MainController);
    end;

  Writeln(Format('Starting HTTP Server or port %d, %d, %d and %d',
    [APort, APort + 10, APort + 20, APort + 30]));

  lServerListeners := TMVCListenersContext.Create;

  lServerListeners.Add(TMVCListenerProperties.New
    .SetName('BasicDemo1')
    .SetPort(APort)
    .SetMaxConnections(1024)
    .SetEngineConfig(lEngineConfig)
    );

  lServerListeners.Add(TMVCListenerProperties.New
    .SetName('BasicDemo2')
    .SetPort(APort + 10)
    .SetMaxConnections(1024)
    .SetEngineConfig(lEngineConfig)
    );

  lServerListeners.Add(TMVCListenerProperties.New
    .SetName('BasicDemo3')
    .SetPort(APort + 20)
    .SetMaxConnections(1024)
    .SetEngineConfig(lEngineConfig)
    );

  lServerListeners.Add(TMVCListenerProperties.New
    .SetName('BasicDemo4')
    .SetPort(APort + 30)
    .SetMaxConnections(1024)
    .SetEngineConfig(lEngineConfig)
    );

  lServerListeners.StartAll;

{$IFNDEF LINUX}
  ShellExecute(0, 'open', PChar('http://localhost:' + inttostr(APort) +
    '/div/10/10'), nil, nil, SW_SHOWMAXIMIZED);
  ShellExecute(0, 'open', PChar('http://localhost:' + inttostr(APort + 10) +
    '/div/10/20'), nil, nil, SW_SHOWMAXIMIZED);
  ShellExecute(0, 'open', PChar('http://localhost:' + inttostr(APort + 20) +
    '/div/10/30'), nil, nil, SW_SHOWMAXIMIZED);
  ShellExecute(0, 'open', PChar('http://localhost:' + inttostr(APort + 30) +
    '/div/10/40'), nil, nil, SW_SHOWMAXIMIZED);
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
