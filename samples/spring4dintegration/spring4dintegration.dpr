program spring4dintegration;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.REPLCommandsHandlerU,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  Controller1U in 'Controller1U.pas',
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule},
  DIConfiguration in 'DIConfiguration.pas',
  Controller2U in 'Controller2U.pas',
  Service1U in 'Service1U.pas',
  Service2U in 'Service2U.pas',
  ServicesInterfaceU in 'ServicesInterfaceU.pas',
  Service3U in 'Service3U.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  lServer: TIdHTTPWebBrokerBridge;
  lCustomHandler: TMVCCustomREPLCommandsHandler;
  lCmd: string;
begin
  Writeln('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  if ParamCount >= 1 then
    lCmd := ParamStr(1)
  else
    lCmd := 'start';

  lCustomHandler := function(const Value: string; const Server: TIdHTTPWebBrokerBridge; out Handled: Boolean): THandleCommandResult
    begin
      Handled := False;
      Result := THandleCommandResult.Unknown;

      // Write here your custom command for the REPL using the following form...
      // ***
      // Handled := False;
      // if (Value = 'apiversion') then
      // begin
      // REPLEmit('Print my API version number');
      // Result := THandleCommandResult.Continue;
      // Handled := True;
      // end
      // else if (Value = 'datetime') then
      // begin
      // REPLEmit(DateTimeToStr(Now));
      // Result := THandleCommandResult.Continue;
      // Handled := True;
      // end;
    end;

  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;

    { more info about MaxConnections
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_MaxConnections.html }
    LServer.MaxConnections := 0;

    { more info about ListenQueue
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_ListenQueue.html }
    LServer.ListenQueue := 200;

    WriteLn('Write "quit" or "exit" to shutdown the server');
    repeat
      if lCmd.IsEmpty then
      begin
        write('-> ');
        ReadLn(lCmd)
      end;
      try
        case HandleCommand(lCmd.ToLower, LServer, lCustomHandler) of
          THandleCommandResult.Continue:
            begin
              Continue;
            end;
          THandleCommandResult.Break:
            begin
              Break;
            end;
          THandleCommandResult.Unknown:
            begin
              REPLEmit('Unknown command: ' + lCmd);
            end;
        end;
      finally
        lCmd := '';
      end;
    until false;

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
    BuildContainer;
    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
