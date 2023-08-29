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
  CommonTypesU in '..\CommonTypesU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  lServer: TIdHTTPWebBrokerBridge;
begin
  Writeln('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  Writeln('JSON-RPC Server with Published Objects');
  Writeln('Listening on port ', APort);

  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;

    { more info about MaxConnections
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_MaxConnections.html }
    LServer.MaxConnections := 0;

    { more info about ListenQueue
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_ListenQueue.html }
    LServer.ListenQueue := 200;

    lServer.Active := True;
    Write('CTRL+C to quit...');
    WaitForTerminationSignal;
  finally
    LServer.Free;
  end;
end;

begin
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
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
