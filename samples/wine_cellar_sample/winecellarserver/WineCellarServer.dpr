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
  MVCFramework.Logger,
  IdHTTPWebBrokerBridge;

{$R *.res}


procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LogI(Format('Starting HTTP Server or port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    LogI('Press RETURN to stop the server');

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
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(3000);
  except
    on E: Exception do
      LogException(E, E.Message);
  end

end.
