program JWTServer;

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
  IdContext,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  AppControllerU in 'AppControllerU.pas',
  AuthenticationU in 'AuthenticationU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.KeepAlive := True;
    LServer.Active := True;
    LogI('Listening on http://localhost:' + APort.ToString);
    LogI('Press Ctrl+C to shut down.');
    WaitForTerminationSignal;
    EnterInShutdownState;
    LServer.Active := False;
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  UseConsoleLogger := True;

  LogI('** JWT Server Sample ** build ' + DMVCFRAMEWORK_VERSION);

  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(8080);
  except
    on E: Exception do
      LogF(E.ClassName + ': ' + E.Message);
  end;
end.
