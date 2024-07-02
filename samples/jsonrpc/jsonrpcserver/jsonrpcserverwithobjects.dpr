program jsonrpcserverwithobjects;

 {$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.Console,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  MVCFramework.Signal,
  MyObjectU in 'MyObjectU.pas',
  MainWebModuleU in 'MainWebModuleU.pas' {MyWebModule: TWebModule},
  BusinessObjectsU in '..\..\commons\BusinessObjectsU.pas',
  RandomUtilsU in '..\..\commons\RandomUtilsU.pas',
  MainDM in '..\..\articles_crud_server\MainDM.pas' {dmMain: TDataModule},
  CommonTypesU in '..\CommonTypesU.pas',
  Services in '..\..\articles_crud_server\Services.pas',
  BusinessObjects in '..\..\articles_crud_server\BusinessObjects.pas',
  Commons in '..\..\articles_crud_server\Commons.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  lServer: TIdHTTPWebBrokerBridge;
begin
  LogI('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  LogI('JSON-RPC Server with Published Objects');
  LogI('Listening on port ' + APort.ToString);

  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    lServer.Active := True;
    LogI('CTRL+C to quit...');
    WaitForTerminationSignal;
  finally
    LServer.Free;
  end;
end;

begin
  UseConsoleLogger := True;
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  TextColor(TConsoleColor.White);
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(8080);
  except
    on E: Exception do
      LogE(E.ClassName + ': ' + E.Message);
  end;
end.
