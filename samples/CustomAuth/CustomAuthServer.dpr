program CustomAuthServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  PublicControllerU in 'PublicControllerU.pas',
  MyWebModuleU in 'MyWebModuleU.pas' {MyWebModule: TWebModule} ,
  PrivateControllerU in 'PrivateControllerU.pas',
  AuthHandlerU in 'AuthHandlerU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln('** DMVCFramework Server **');
  Writeln(Format('Starting HTTP Server on port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    // ShellExecute(0, 'open', pChar('http://localhost:' + inttostr(APort)), nil, nil, SW_SHOWMAXIMIZED);
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
