program activerecord_crud;

{$APPTYPE CONSOLE}

uses
  FireDAC.Phys.FB,
  FireDAC.Phys.PG,
  FireDAC.Phys.MySQL,
  FireDAC.Phys.SQLite,
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.REPLCommandsHandlerU,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  MVCFramework.SQLGenerators.PostgreSQL,
  MVCFramework.SQLGenerators.Firebird,
  MVCFramework.SQLGenerators.Interbase,
  MVCFramework.SQLGenerators.MSSQL,
  MVCFramework.SQLGenerators.MySQL,
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule} ,
  Entities in 'Entities.pas',
  MVCFramework.ActiveRecordController
    in '..\..\sources\MVCFramework.ActiveRecordController.pas',
  MVCFramework.ActiveRecord in '..\..\sources\MVCFramework.ActiveRecord.pas',
  EntitiesProcessors in 'EntitiesProcessors.pas',
  FDConnectionConfigU in '..\activerecord_showcase\FDConnectionConfigU.pas';

{$R *.res}

procedure RunServer(APort: Integer);
var
  lServer: TIdHTTPWebBrokerBridge;
  lCustomHandler: TMVCCustomREPLCommandsHandler;
  lCmd: string;
begin
  ConnectionDefinitionName := CON_DEF_NAME;
  Writeln('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);
  if ParamCount >= 1 then
    lCmd := ParamStr(1)
  else
    lCmd := 'start';

  lCustomHandler :=
      function(const Value: string; const Server: TIdHTTPWebBrokerBridge;
      out Handled: Boolean): THandleCommandResult
    begin
      Handled := False;
      Result := THandleCommandResult.Continue;
      if (Value = '/firebird') then
      begin
        REPLEmit('Using FirebirdSQL');
        Result := THandleCommandResult.Continue;
        CreateFirebirdPrivateConnDef(True);
        Handled := True;
        Server.Active := True;
        Writeln('Server listening on port ', Server.DefaultPort);
      end
      else if (Value = '/mysql') then
      begin
        REPLEmit('Using MySQL');
        Result := THandleCommandResult.Continue;
        CreateMySQLPrivateConnDef(True);
        Handled := True;
        Server.Active := True;
        Writeln('Server listening on port ', Server.DefaultPort);
      end
      else if (Value = 'start') or (Value = '/postgresql') then
      begin
        REPLEmit('Using PostgreSQL');
        Result := THandleCommandResult.Continue;
        CreatePostgreSQLPrivateConnDef(True);
        Handled := True;
        Server.Active := True;
        Writeln('Server listening on port ', Server.DefaultPort);
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
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
