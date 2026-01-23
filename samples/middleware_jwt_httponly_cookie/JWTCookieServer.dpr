program JWTCookieServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Winapi.Windows,
  Winapi.ShellAPI,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  MVCFramework.Commons,
  IdContext,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  AppControllerU in 'AppControllerU.pas',
  AuthenticationU in 'AuthenticationU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln('========================================');
  Writeln(' JWT Cookie Authentication Sample');
  Writeln(' Secure HTTP-Only Cookie Demo');
  Writeln('========================================');
  Writeln;
  Writeln(Format('Starting HTTP Server on port %d', [APort]));
  Writeln;
  Writeln('NOTE: For production, use HTTPS!');
  Writeln('      Cookie has Secure=False for this HTTP demo.');
  Writeln;
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.Active := True;
    Writeln('Server started successfully!');
    Writeln;
    Writeln('Test endpoints:');
    Writeln('  POST /login     - Login with JSON body {"username":"user1","password":"user1"}');
    Writeln('  GET  /logout    - Logout (invalidates cookie)');
    Writeln('  GET  /public    - Public endpoint (no auth required)');
    Writeln('  GET  /admin/role1 - Protected endpoint (requires role1)');
    Writeln('  GET  /admin/role2 - Protected endpoint (requires role2)');
    Writeln;
    Writeln('Users for testing:');
    Writeln('  user1/user1 -> has role1');
    Writeln('  user2/user2 -> has role2');
    Writeln('  user3/user3 -> has role1 and role2');
    Writeln;
    Writeln('Press RETURN to stop the server');
    ShellExecute(0, 'open', PChar('http://localhost:' + IntToStr(APort)), nil, nil, SW_SHOW);
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
  end
end.
