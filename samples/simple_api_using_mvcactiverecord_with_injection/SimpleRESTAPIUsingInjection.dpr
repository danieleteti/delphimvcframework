program SimpleRESTAPIUsingInjection;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Container,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.Signal,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdContext,
  IdHTTPWebBrokerBridge,
  CustomersControllerU in 'CustomersControllerU.pas',
  MainWM in 'MainWM.pas' {TMunicipalLibraryWebModule: TWebModule},
  EntitiesU in 'EntitiesU.pas',
  CustomerServiceU in 'CustomerServiceU.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LogI('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);

  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;

    { more info about MaxConnections
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_MaxConnections.html }
    LServer.MaxConnections := 0;

    { more info about ListenQueue
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_ListenQueue.html }
    LServer.ListenQueue := 200;
    LServer.Active := True;
    LogI('Listening on port ' + APort.ToString);
    LogI('CTRL+C to shutdown the server');
    WaitForTerminationSignal;
    EnterInShutdownState;
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  MVCSerializeNulls := False;
  UseConsoleLogger := True;

  TMVCSqids.SQIDS_ALPHABET := dotEnv.Env('dmvc.sqids.alphabet', 'N7h4vc3nMiAaQEDFr5HUXuwy19qLmt8jGkTKlz60pRSJs2oIxZPbgYfBVOeWdC');
  TMVCSqids.SQIDS_MIN_LENGTH := dotEnv.Env('dmvc.sqids.min_length', 6);

  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;

{$IF CompilerVersion >= 34} //SYDNEY+
    if dotEnv.Env('dmvc.profiler.enabled', false) then
    begin
      Profiler.ProfileLogger := Log;
      Profiler.WarningThreshold := dotEnv.Env('dmvc.profiler.warning_threshold', 2000);
    end;
{$ENDIF}

    RegisterServices(DefaultMVCServiceContainer);
    DefaultMVCServiceContainer.Build;

    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
