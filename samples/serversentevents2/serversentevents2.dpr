program serversentevents2;


{$APPTYPE CONSOLE}

uses
  MVCFramework,
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Commons,
  ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
{$IFDEF MSWINDOWS}
  WinAPI.Windows,
  WinAPI.ShellAPI,
{$ENDIF}
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule} ,
  SysConstantsU in 'SysConstantsU.pas',
  MainDMU in 'MainDMU.pas' {dmMain: TDataModule} ,
  StatusControllerU in 'StatusControllerU.pas',
  EntitiesU in 'entities\EntitiesU.pas',
  StatusesServiceU in 'services\StatusesServiceU.pas',
  BaseServiceU in 'services\BaseServiceU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  Writeln(Format('Starting HTTP Server on port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.KeepAlive := True;
    LServer.DefaultPort := APort;
    LServer.Active := True;
    LogI(Format('Server started on port 8080', [APort]));
    { more info about MaxConnections
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_MaxConnections.html }
    LServer.MaxConnections := 0;
    { more info about ListenQueue
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_ListenQueue.html }
    LServer.ListenQueue := 200;
{$IFDEF MSWINDOWS}
    ShellExecute(0, 'open', PChar('http://localhost:' + inttostr(APort) +
      '/static'), nil, nil, SW_SHOWMAXIMIZED);
{$ENDIF}
    Writeln('Press RETURN to stop the server');
    ReadLn;
    EnterInShutdownState; { WARNING! }
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
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

