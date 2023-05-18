program SwaggerDocApiExtended;

{$APPTYPE CONSOLE}

uses
{$IF Defined(MSWINDOWS)}
  Winapi.ShellAPI,
  Winapi.Windows,
{$ENDIF }
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.Signal,
  MVCFramework.Middleware.Redirect,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdContext,
  IdHTTPWebBrokerBridge,
  WebModuleMainU in 'WebModuleMainU.pas' {WebModule1: TWebModule} ,
  ControllersU in 'ControllersU.pas',
  EntitiesU in 'EntitiesU.pas',
  BaseControllerU in 'BaseControllerU.pas',
  AuthHandler in 'AuthHandler.pas',
  MVCFramework.Swagger.Commons in '..\..\sources\MVCFramework.Swagger.Commons.pas',
  MVCFramework.Middleware.Swagger in '..\..\sources\MVCFramework.Middleware.Swagger.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
  LCmd: string;
  lURL: string;
begin
  Writeln('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  LCmd := 'start';
  if ParamCount >= 1 then
    LCmd := ParamStr(1);

  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.KeepAlive := True;

    { more info about MaxConnections
      http://ww2.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=index.html }
    LServer.MaxConnections := 0;

    { more info about ListenQueue
      http://ww2.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=index.html }
    LServer.ListenQueue := 200;
    lURL := Format('http://localhost:%d', [APort]);
    Writeln('SWAGGER UI available at ', lURL);
    LServer.Active := True;
    Write('CTRL+C to Quit');
{$IF Defined(MSWINDOWS)}
    ShellExecute(0, nil, PChar(lURL), nil, nil, SW_SHOWNOACTIVATE);
{$ENDIF}
    WaitForTerminationSignal;
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
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
