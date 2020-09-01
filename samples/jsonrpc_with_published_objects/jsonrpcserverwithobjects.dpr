program jsonrpcserverwithobjects;

 {$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.Console,
  MVCFramework.REPLCommandsHandlerU,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  MyObjectU in 'MyObjectU.pas',
  MainWebModuleU in 'MainWebModuleU.pas' {MyWebModule: TWebModule},
  MVCFramework.JSONRPC in '..\..\sources\MVCFramework.JSONRPC.pas',
  BusinessObjectsU in '..\commons\BusinessObjectsU.pas',
  RandomUtilsU in '..\commons\RandomUtilsU.pas',
  MainDM in '..\articles_crud_server\MainDM.pas' {dmMain: TDataModule};

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

  lCustomHandler := function(const Value: String; const Server: TIdHTTPWebBrokerBridge; out Handled: Boolean): THandleCommandResult
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
    SaveColors;
    TextColor(Yellow);
    WriteLn('Write "quit" or "exit" to shutdown the server');
    RestoreSavedColors;
    repeat
      if lCmd.IsEmpty then
      begin
        SaveColors;
        TextColor(Green);
        Write('-> ');
        RestoreSavedColors;
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
              SaveColors;
              TextColor(Red);
              REPLEmit('Unknown command: ' + lCmd);
              RestoreSavedColors;
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
  TextColor(TConsoleColor.White);
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
