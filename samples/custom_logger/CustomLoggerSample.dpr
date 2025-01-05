program CustomLoggerSample;

{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  MVCFramework.Logger,
  LoggerPro.Proxy,

  {$IFDEF MSWINDOWS}

  Winapi.Windows,
  Winapi.ShellAPI,

  {$ENDIF}

  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  MyControllerU in 'MyControllerU.pas',
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule} ,
  CustomLoggerConfigU in 'CustomLoggerConfigU.pas', LoggerPro;

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LogI('** DMVCFramework Server **');
  LogI('WARNING! Run this program in debug and check the Delphi "Event" debug window to see the custom logs');
  LogI('WARNING! Also, the log file are generated in the custom path "MyFolder\MyLogs"');
  LogI(Format('Starting HTTP Server on port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;

    {$IFDEF MSWINDOWS}

    ShellExecute(0, 'open', pChar('http://localhost:' + inttostr(APort)), nil, nil,
      SW_SHOWMAXIMIZED);

    {$ENDIF}

    LogI('Press RETURN to stop the server');
    ReadLn;
  finally
    LServer.Free;
  end;
end;

begin
  //Option 1
  //You can customize the logger providing a complete new one
  //SetDefaultLogger(GetLogger);



  //Option 2
  //If you want to sligthly change the behaviour of the default logger
  //you can retrive the "default configuration" and then apply a decorator
  //with a filter function which "decides" if the logitem must be go through
  //the appenders chain or not (so, discarded)
  SetDefaultLogger(TLogWriterDecorator.Build(CreateLoggerWithDefaultConfiguration,
    function (const aType: TLogType; const aMessage, aTag: string): Boolean
    begin
      Result := True;
    end));


  ReportMemoryLeaksOnShutdown := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(8080);
  except
    on E: Exception do
      LogException(E, 'Closing service');
  end;

end.
