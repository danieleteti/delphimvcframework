program articles_crud;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  Controllers.Base in 'Controllers.Base.pas',
  Controllers.Articles in 'Controllers.Articles.pas',
  Services in 'Services.pas',
  BusinessObjects in 'BusinessObjects.pas',
  MainDM in 'MainDM.pas' {dmMain: TDataModule},
  Commons in 'Commons.pas';

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
    WriteLn('Press RETURN to stop the server');
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
    RunServer(8080);
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end

end.
