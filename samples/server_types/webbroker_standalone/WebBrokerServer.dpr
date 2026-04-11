program WebBrokerServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Logger,
  ControllersU in '..\commons\ControllersU.pas',
  EngineConfigU in '..\commons\EngineConfigU.pas',
  WebModuleU in 'WebModuleU.pas' {SampleWebModule: TWebModule};

{$R *.res}

var
  LServer: TIdHTTPWebBrokerBridge;
begin
  ReportMemoryLeaksOnShutdown := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    LServer := TIdHTTPWebBrokerBridge.Create(nil);
    try
      LServer.DefaultPort := 8080;
      LServer.MaxConnections := 0;
      LServer.ListenQueue := 200;
      LServer.Active := True;
      LogI('WebBroker Standalone Server started on port 8080');
      LogI('Server type: WebBroker (TIdHTTPWebBrokerBridge)');
      LogI('Press CTRL+C to stop');
      WaitForTerminationSignal;
      EnterInShutdownState;
    finally
      LServer.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
