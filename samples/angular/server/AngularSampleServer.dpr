program AngularSampleServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,

  {$IFDEF MSWINDOWS}

  Winapi.Windows,
  Winapi.ShellAPI,

  {$ENDIF}

  Web.WebReq,
  Web.WebBroker,
  MVCFramework.Commons,
  IdHTTPWebBrokerBridge,
  CustomersControllerU in 'CustomersControllerU.pas',
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule} ,
  CustomersTDGU in 'CustomersTDGU.pas' {CustomersTDG: TDataModule};

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln('** DMVCFramework Server ** ' + DMVCFRAMEWORK_VERSION);
  Writeln(Format('Starting HTTP Server on port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    { more info about MaxConnections
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_MaxConnections.html }
    LServer.MaxConnections := 0;
    { more info about ListenQueue
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_ListenQueue.html }
    LServer.ListenQueue := 200;

    {$IFDEF MSWINDOWS}

    { Comment the next line to avoid the default browser startup }
    ShellExecute(0, 'open', PChar('http://localhost:' + inttostr(APort)), nil, nil, SW_SHOWMAXIMIZED);

    {$ENDIF}

    Writeln('Press RETURN to stop the server');
    ReadLn;
  finally
    LServer.Free;
  end;
end;

begin
  SetupFireDACConnection;
  ReportMemoryLeaksOnShutdown := True;
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
