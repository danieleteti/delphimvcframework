program masterdetailssample;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,
  MVCFramework,
  MVCFramework.Signal,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  Controllers.Base in 'Controllers.Base.pas',
  Controllers.Orders in 'Controllers.Orders.pas',
  BusinessObjects in 'BusinessObjects.pas',
  Commons in 'Commons.pas',
  FDConnectionConfigU in 'FDConnectionConfigU.pas';

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
    WriteLn('Press Ctrl+C to stop the server');
    WaitForTerminationSignal;
    EnterInShutdownState;
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    CreatePostgresqlPrivateConnDef(True);
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(8080);
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end

end.
