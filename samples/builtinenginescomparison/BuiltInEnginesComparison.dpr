program BuiltInEnginesComparison;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.REPLCommandsHandlerU,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdContext,
  {$IF Defined(INDYENGINE)}
  IdHTTPWebBrokerBridge,
  {$ENDIF }
  {$IF Defined(HTTPSYSENGINE)}
  MVCFramework.HTTPSys.WebBrokerBridge,
  {$ENDIF }
  MyControllerU in 'MyControllerU.pas',
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule};

{$R *.res}
{$IF Defined(INDYENGINE)}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
  LCustomHandler: TMVCCustomREPLCommandsHandler;
  LCmd: string;
begin
  Writeln('** INDY - DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  LCmd := 'start';
  if ParamCount >= 1 then
    LCmd := ParamStr(1);

  LCustomHandler :=
      function(const Value: String; const Server: TIdHTTPWebBrokerBridge; out Handled: Boolean): THandleCommandResult
    begin
      Handled := False;
      Result := THandleCommandResult.Unknown;
    end;

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
    { required if you use JWT middleware }
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.Active := True;
    Writeln('Press [Enter] to shutdown the server');
    ReadLn;
  finally
    LServer.Free;
  end;
end;

{$ENDIF}
{$IF Defined(HTTPSYSENGINE)}

procedure RunServer(APort: Integer);
var
  LServer: TMVCHTTPSysWebBrokerBridge;
begin
  Writeln('** HTTP.Sys - DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  LServer := TMVCHTTPSysWebBrokerBridge.Create;
  try
    LServer.Port := APort;
    LServer.UseSSL := False;
    LServer.UseCompression := True;
    LServer.Active := True;
    Writeln('Press [Enter] to shutdown the server');
    ReadLn;
  finally
    LServer.Free;
  end;
end;
{$ENDIF}

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
