program ServerSideViewsTemplatePro;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Signal,
  MVCFramework.Logger,
  {$IFDEF MSWINDOWS}
  Winapi.ShellAPI,
  Winapi.Windows,
  {$ENDIF }
  IdHTTPWebBrokerBridge,
  TemplatePro,
  MVCFramework.Commons,
  MVCFramework.View.Renderers.TemplatePro,
  Web.WebReq,
  Web.WebBroker,
  WebModuleU in 'WebModuleU.pas' {WebModule1: TWebModule},
  WebSiteControllerU in 'WebSiteControllerU.pas',
  DAL in 'DAL.pas',
  MyDataModuleU in '..\renders\MyDataModuleU.pas' {MyDataModule: TDataModule},
  CustomTemplateProFiltersU in 'CustomTemplateProFiltersU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  ReportMemoryLeaksOnShutdown := True;
  LogI(Format('Starting HTTP Server on http://localhost:%d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
{$IFDEF MSWINDOWS}
    ShellExecute(0, 'open', PChar('http://localhost:' + inttostr(APort)), nil, nil, SW_SHOW);
{$ENDIF}
    LogI('Ctrl+C to stop the server');
    WaitForTerminationSignal;
    EnterInShutdownState;
    LServer.Active := False;
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    TemplateProContextConfigure;
    RunServer(8080);
  except
    on E: Exception do
      LogE(E.ClassName + ': ' + E.Message);
  end;

end.
