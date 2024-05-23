program ServerSideViewsMustache;

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
  MVCFramework.View.Renderers.Mustache,
  Web.WebReq,
  Web.WebBroker,
  WebModuleU in 'WebModuleU.pas' {WebModule1: TWebModule},
  WebSiteControllerU in 'WebSiteControllerU.pas',
  DAL in 'DAL.pas',
  MyDataModuleU in '..\renders\MyDataModuleU.pas' {MyDataModule: TDataModule},
  CustomMustacheHelpersU in 'CustomMustacheHelpersU.pas',
  MVCFramework.Serializer.URLEncoded in '..\..\sources\MVCFramework.Serializer.URLEncoded.pas',
  mormot.core.mustache;

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  ReportMemoryLeaksOnShutdown := True;
  LogI(Format('Starting HTTP Server on port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
{$IFDEF MSWINDOWS}
    ShellExecute(0, 'open', PChar('http://localhost:' + inttostr(APort)), nil, nil, SW_SHOW);
{$ENDIF}
    LogI('Ctrl+C  to stop the server');
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

    // these helpers will be available to the mustache views as if they were the standard ones
    TMVCMustacheHelpers.OnLoadCustomHelpers := procedure(var MustacheHelpers: TSynMustacheHelpers)
      begin
        TSynMustache.HelperAdd(MustacheHelpers, 'MyHelper1', TMyMustacheHelpers.MyHelper1);
        TSynMustache.HelperAdd(MustacheHelpers, 'MyHelper2', TMyMustacheHelpers.MyHelper2);
      end;

    RunServer(8080);
  except
    on E: Exception do
      LogE(E.ClassName + ': ' + E.Message);
  end;

end.
