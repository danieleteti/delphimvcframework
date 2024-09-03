program WineCellarServer;

{$APPTYPE CONSOLE}


uses
  WinApi.ShellAPI,
  System.SysUtils,

  {$IFDEF MSWINDOWS}

  WinApi.Windows,

  {$ENDIF}

  Web.WebReq,
  Web.WebBroker,
  MainWebModuleUnit in 'MainWebModuleUnit.pas' {wm: TWebModule} ,
  WineCellarAppControllerU in 'WineCellarAppControllerU.pas',
  MainDataModuleUnit in 'MainDataModuleUnit.pas' {WineCellarDataModule: TDataModule} ,
  WinesBO in 'WinesBO.pas',
  IdHTTPWebBrokerBridge;

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln(Format('Starting HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    Writeln('Press RETURN to stop the server');

    {$IFDEF MSWINDOWS}

    // Just to start the WEB client
    ShellExecute(0, 'open', 'http://localhost:3000/app', nil, nil, SW_SHOW);

    {$ENDIF}

    ReadLn;
  finally
    LServer.Free;
  end;
end;

begin
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024; // daniele teti
    RunServer(3000);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end

end.
