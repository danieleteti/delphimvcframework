program activerecord_crud;

{$APPTYPE CONSOLE}


uses
//  FastMM4,
  FireDAC.Phys.FB,
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.REPLCommandsHandlerU,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule} ,
  Entities in 'Entities.pas',
  FDConnectionConfigU in 'FDConnectionConfigU.pas',
  MVCFramework.ActiveRecordController in '..\..\sources\MVCFramework.ActiveRecordController.pas',
  MVCFramework.ActiveRecord in '..\..\sources\MVCFramework.ActiveRecord.pas',
  MVCFramework.RQL.AST2MySQL in '..\..\sources\MVCFramework.RQL.AST2MySQL.pas',
  MVCFramework.RQL.AST2FirebirdSQL in '..\..\sources\MVCFramework.RQL.AST2FirebirdSQL.pas',
  EntitiesProcessors in 'EntitiesProcessors.pas';

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
      if (Value = '/firebird') then
      begin
        REPLEmit('Using FirebirdSQL');
        Result := THandleCommandResult.Continue;
        ConnectionDefinitionName := CON_DEF_NAME_FIREBIRD;
        Handled := True;
        Server.Active := True;
      end
      else if (Value = '/mysql') then
      begin
        REPLEmit('Using MySQL');
        Result := THandleCommandResult.Continue;
        ConnectionDefinitionName := CON_DEF_NAME_MYSQL;
        Handled := True;
        Server.Active := True;
      end;
    end;

  lServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    lServer.DefaultPort := APort;

    { more info about MaxConnections
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_MaxConnections.html }
    lServer.MaxConnections := 0;

    { more info about ListenQueue
      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_ListenQueue.html }
    lServer.ListenQueue := 200;

    Writeln('Write "quit" or "exit" to shutdown the server');
    repeat
      if lCmd.IsEmpty then
      begin
        write('-> ');
        ReadLn(lCmd)
      end;
      try
        case HandleCommand(lCmd.ToLower, lServer, lCustomHandler) of
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
    until False;

  finally
    lServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  try
    CreateFirebirdPrivateConnDef(True);
    CreateMySQLPrivateConnDef(True);

    // if ParamCount = 0 then
    // begin
    // ConnectionDefinitionName := CON_DEF_NAME_MYSQL;
    // end
    // else if ParamCount = 1 then
    // begin
    // if FindCmdLineSwitch('firebird', ['/', '-'], True) then
    // begin
    // ConnectionDefinitionName := CON_DEF_NAME_FIREBIRD;
    // end
    // else if FindCmdLineSwitch('mysql', ['/', '-'], True) then
    // begin
    // ConnectionDefinitionName := CON_DEF_NAME_MYSQL;
    // end
    // else
    // raise Exception.Create('Unknown option in command line');
    // end
    // else
    // begin
    // raise Exception.Create('Unknown options in command line');
    // end;


    // To use MySQL decomment the following line
    // ConnectionDefinitionName := CON_DEF_NAME_MYSQL;

    // To use FirebirdSQL decomment the following line
    // ConnectionDefinitionName := CON_DEF_NAME_FIREBIRD;

    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
