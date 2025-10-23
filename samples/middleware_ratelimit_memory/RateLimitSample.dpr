program RateLimitSample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Web.WebReq,
  Web.WebBroker,
  MVCFramework.Logger,
  MVCFramework.Commons,
  IdHTTPWebBrokerBridge,
  WebModuleU in 'WebModuleU.pas' {WebModule1: TWebModule},
  MainControllerU in 'MainControllerU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LogI(Format('** DMVCFramework Rate Limit Middleware Sample **', []));
  LogI(Format('Starting HTTP Server on port %d', [APort]));
  LogI('');
  LogI('Rate Limit Configuration:');
  LogI('  - Limit: 10 requests per 60 seconds (1 minute)');
  LogI('  - Strategy: Fixed Window');
  LogI('  - Key: IP Address');
  LogI('');
  LogI('Try these commands:');
  LogI('  curl http://localhost:8080/api/hello');
  LogI('  curl http://localhost:8080/api/protected');
  LogI('  curl http://localhost:8080/health (excluded from rate limit)');
  LogI('');
  LogI('To test rate limiting, run this command multiple times:');
  LogI('  for /L %i in (1,1,15) do @curl -s http://localhost:8080/api/hello && @echo.');
  LogI('');

  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    LogI('Press RETURN to stop the server');
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
      LogException(E, E.Message);
  end;
end.
