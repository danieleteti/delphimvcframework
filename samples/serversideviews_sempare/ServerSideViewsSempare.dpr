program ServerSideViewsSempare;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Signal,
{$IFDEF MSWINDOWS}
  Winapi.ShellAPI,
  Winapi.Windows,
{$ENDIF }
  Sempare.Template,
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,
  WebModuleU in 'WebModuleU.pas' {WebModule1: TWebModule},
  WebSiteControllerU in 'WebSiteControllerU.pas',
  DAL in 'DAL.pas',
  MyDataModuleU in '..\renders\MyDataModuleU.pas' {MyDataModule: TDataModule},
  CustomSempareHelpersU in 'CustomSempareHelpersU.pas',
  MVCFramework.Serializer.URLEncoded in '..\..\sources\MVCFramework.Serializer.URLEncoded.pas',
  MVCFramework.View.Renderers.Sempare in '..\..\contrib\MVCFramework.View.Renderers.Sempare.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  TTemplateRegistry.Instance.LoadStrategy := [tlsLoadFile];
  ReportMemoryLeaksOnShutdown := True;
  Writeln(Format('Starting HTTP Server on port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
{$IFDEF MSWINDOWS}
    ShellExecute(0, 'open', pchar('http://localhost:' + inttostr(APort)), nil, nil, SW_SHOW);
{$ENDIF}
    Write('Ctrl+C  to stop the server');
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

    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
