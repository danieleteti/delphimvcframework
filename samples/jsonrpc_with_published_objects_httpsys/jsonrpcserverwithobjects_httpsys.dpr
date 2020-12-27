program jsonrpcserverwithobjects_httpsys;

 {$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.Console,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  MyObjectU in 'MyObjectU.pas',
  MainWebModuleU in 'MainWebModuleU.pas' {MyWebModule: TWebModule},
  MVCFramework.JSONRPC in '..\..\sources\MVCFramework.JSONRPC.pas',
  BusinessObjectsU in '..\commons\BusinessObjectsU.pas',
  RandomUtilsU in '..\commons\RandomUtilsU.pas',
  MainDM in '..\articles_crud_server\MainDM.pas' {dmMain: TDataModule},
  MVCFramework.HTTPSys.WebBrokerBridge in '..\..\sources\MVCFramework.HTTPSys.WebBrokerBridge.pas',
  MVCFramework.HTTPSys.Core in '..\..\sources\MVCFramework.HTTPSys.Core.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  lServer: TMVCHTTPSysWebBrokerBridge;
begin
  Writeln('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  LServer := TMVCHTTPSysWebBrokerBridge.Create;
  try
    LServer.Port := APort;
    lServer.Active := True;
    SaveColors;
    TextColor(Yellow);
    WriteLn('Press [Enter] to shutdown the server');
    RestoreSavedColors;
    ReadLn;
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
    WebRequestHandlerProc.MaxConnections := 0;
    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
