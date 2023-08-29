program articles_crud_service_with_prometheus;

{$APPTYPE CONSOLE}



{----------------------------------------------------------------}
{- THIS SAMPLE REQUIRES -----------------------------------------}
{- https://github.com/marcobreveglieri/prometheus-client-delphi -}
{----------------------------------------------------------------}

uses
  Prometheus.Registry,
  Prometheus.Collectors.Counter,
  Prometheus.Collectors.Gauge,
  MVCFramework.Signal,
  System.SysUtils,
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  Controllers.Base in 'Controllers.Base.pas',
  Controllers.Articles in 'Controllers.Articles.pas',
  BusinessObjects in '..\..\articles_crud_server\BusinessObjects.pas',
  Commons in '..\..\articles_crud_server\Commons.pas',
  MainDM in '..\..\articles_crud_server\MainDM.pas',
  Services in '..\..\articles_crud_server\Services.pas',
  MVCFramework.Middleware.Prometheus in 'MVCFramework.Middleware.Prometheus.pas',
  UtilsU in 'UtilsU.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  WriteLn('ARTICLES CRUD Sample. Use articles_crud_vcl_client.dproj to manage data');
  WriteLn(Format('Starting HTTP Server on port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    WriteLn('CTRL+C to stop the server');
    WaitForTerminationSignal;
    ReadLn;
  finally
    LServer.Free;
  end;
end;


procedure ConfigurePrometheusMetrics;
var
  lGauge: TGauge;
begin
  // Configure some sample metrics... add metrics as you wish
  TCounter
    .Create('http_requests_count', 'Received HTTP request count')
    .Register();

  TCounter
    .Create('new_article', 'Articles Created')
    .Register();

  TCounter
    .Create('searches', 'Searches Count')
    .Register();

  TCounter
    .Create('http_requests_handled', 'HTTP handled request count',
      ['path', 'status'])
    .Register();

  lGauge := TGauge.Create('memory_allocated_total', 'Total memory allocated by the process');
  lGauge.SetTo(GetMemoryUsed);
  lGauge.Register();
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    ConfigurePrometheusMetrics;
    RunServer(8080);
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end

end.
