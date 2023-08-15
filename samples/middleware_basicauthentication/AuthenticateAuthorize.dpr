program AuthenticateAuthorize;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
{$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.ShellAPI,
{$ENDIF}
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule} ,
  AppControllerU in 'AppControllerU.pas',
  MVCFramework.Middleware.Authentication
    in '..\..\sources\MVCFramework.Middleware.Authentication.pas',
  AuthenticationU in 'AuthenticationU.pas';

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
    Writeln('Press RETURN to stop the server');
		{$IFDEF MSWINDOWS}
    ShellExecute(0, 'open', PChar('http://localhost:' + IntToStr(APort) + '/static'), nil, nil, SW_SHOW);
		{$ENDIF}
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
