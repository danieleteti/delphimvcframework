program ConsoleApp;
{$APPTYPE CONSOLE}


uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.DotEnv,
  MVCFramework.Logger,
  System.SysUtils,
  Winapi.Windows,
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,
  WinesBO in '..\..\WineCellarSample\winecellarserver\WinesBO.pas',
  WineCellarAppControllerU in '..\..\WineCellarSample\winecellarserver\WineCellarAppControllerU.pas',
  MainWebModuleUnit in '..\..\WineCellarSample\winecellarserver\MainWebModuleUnit.pas' {wm: TWebModule},
  MainDataModuleUnit in '..\..\WineCellarSample\winecellarserver\MainDataModuleUnit.pas' {WineCellarDataModule: TDataModule};

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln(Format('Starting HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    Write('CTRL+C to stop the server');
    WaitForTerminationSignal;
    EnterInShutdownState;
  finally
    LServer.Free;
  end;
end;

begin
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    dotEnvConfigure(
      function: IMVCDotEnv
      begin
        Result := NewDotEnv
                 .WithStrategy(TMVCDotEnvPriority.FileThenEnv)
                                     //if available, by default, loads default environment (.env)
                 .UseProfile('test') //if available loads the test environment (.env.test)
                 .UseProfile('prod') //if available loads the prod environment (.env.prod)
                 .UseLogger(procedure(LogItem: String)
                            begin
                              LogW('dotEnv: ' + LogItem);
                            end)
                 .Build();             //uses the executable folder to look for .env* files
      end);
    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end

end.
