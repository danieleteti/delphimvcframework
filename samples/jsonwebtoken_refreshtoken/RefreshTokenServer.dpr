program RefreshTokenServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  MVCFramework.Commons,
  WebModuleU in 'WebModuleU.pas' {WebModule1: TWebModule},
  AuthControllerU in 'AuthControllerU.pas',
  AuthenticationU in 'AuthenticationU.pas',
  SharedU in 'SharedU.pas';

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln(Format('Starting HTTP Server on port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.Active := True;
    Writeln('');
    Writeln('JWT ACCESS + REFRESH TOKEN SAMPLE');
    Writeln('  POST /auth/login    {"username":"u","password":"u"}  -> {access_token, refresh_token}');
    Writeln('  GET  /api/profile   Authorization: Bearer <access_token>');
    Writeln('  POST /auth/refresh  {"refresh_token":"..."}          -> new {access_token, refresh_token}');
    Writeln('');
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
