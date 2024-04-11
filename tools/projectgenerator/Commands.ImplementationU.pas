unit Commands.ImplementationU;

interface

uses CommonsU, System.SysUtils, MVCFramework.Commons, JsonDataObjects;

type
  TUnitFooterCommand = class(TCustomCommand)
  public
    procedure ExecuteImplementation(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
    procedure ExecuteInterface(
      Section: TStringBuilder;
      Model: TJsonObject); override;
  end;

  TUnitMainBeginEndCommand = class(TCustomCommand)
  public
    procedure ExecuteImplementation(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
    procedure ExecuteInterface(Section: TStringBuilder; Model: TJsonObject); override;
  end;

  TUnitRunServerProcBody = class(TCustomCommand)
  public
    procedure ExecuteImplementation(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
    procedure ExecuteInterface(Section: TStringBuilder; Model: TJsonObject); override;
  end;

  TUnitControllerEntityImplementationCommand = class(TCustomCommand)
  public
    procedure ExecuteImplementation(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
  end;

  TUnitControllerControllerImplementationCommand = class(TCustomCommand)
  public
    procedure ExecuteImplementation(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
  end;

implementation

{ TUnitFooterCommand }

procedure TUnitFooterCommand.ExecuteImplementation(Section: TStringBuilder;
  Model: TJSONObject);
begin
  inherited;
  Section.AppendLine.AppendLine('end.');
end;

procedure TUnitFooterCommand.ExecuteInterface(Section: TStringBuilder;
  Model: TJsonObject);
begin
  inherited;

end;

{ TUnitMainBeginEndCommand }

procedure TUnitMainBeginEndCommand.ExecuteImplementation(Section: TStringBuilder;
  Model: TJSONObject);
begin
  inherited;
  Section
    .AppendLine
    .AppendLine('begin')
    .AppendLine('  { Enable ReportMemoryLeaksOnShutdown during debug }')
    .AppendLine('  // ReportMemoryLeaksOnShutdown := True;')
    .AppendLine('  IsMultiThread := True;'' + sLineBreak + sLineBreak +')
    .AppendLine('  // DMVCFramework Specific Configuration ')
    .AppendLine('  // When MVCSerializeNulls = True empty nullables and nil are serialized as json null.')
    .AppendLine('  // When MVCSerializeNulls = False empty nullables and nil are not serialized at all.')
    .AppendLine('  MVCSerializeNulls := True;')
    .AppendLine('  UseConsoleLogger := True;')
    .AppendLine('  LogI(''** DMVCFramework Server ** build '' + DMVCFRAMEWORK_VERSION);')
    .AppendLine('  try')
    .AppendLine('    if WebRequestHandler <> nil then')
    .AppendLine('      WebRequestHandler.WebModuleClass := WebModuleClass;')
    .AppendLine
    .AppendLine('    dotEnvConfigure(')
    .AppendLine('      function: IMVCDotEnv')
    .AppendLine('      begin')
    .AppendLine('        Result := NewDotEnv')
    .AppendLine('                 .UseStrategy(TMVCDotEnvPriority.FileThenEnv)')
    .AppendLine('                                       //if available, by default, loads default environment (.env)')
    .AppendLine('                 .UseProfile(''test'') //if available loads the test environment (.env.test)')
    .AppendLine('                 .UseProfile(''prod'') //if available loads the prod environment (.env.prod)')
    .AppendLine('                 .UseLogger(procedure(LogItem: String)')
    .AppendLine('                            begin')
    .AppendLine('                              LogD(''dotEnv: '' + LogItem);')
    .AppendLine('                            end)')
    .AppendLine('                 .Build();             //uses the executable folder to look for .env* files')
    .AppendLine('      end);')
    .AppendLine
    .AppendLine('    WebRequestHandlerProc.MaxConnections := dotEnv.Env(''dmvc.handler.max_connections'', 1024);')
    .AppendLine
    .AppendLine('{$IF Defined(SYDNEYORBETTER)}')
    .AppendLine('    if dotEnv.Env(''dmvc.profiler.enabled'', false) then')
    .AppendLine('    begin')
    .AppendLine('      Profiler.ProfileLogger := Log;')
    .AppendLine('      Profiler.WarningThreshold := dotEnv.Env(''dmvc.profiler.warning_threshold'', 2000);')
    .AppendLine('    end;')
    .AppendLine('{$ENDIF}')
    .AppendLine
    .AppendLine('    RunServer(dotEnv.Env(''dmvc.server.port'', %1:d));')
    .AppendLine('  except')
    .AppendLine('    on E: Exception do')
    .AppendLine('      LogF(E.ClassName + '': '' + E.Message);')
    .AppendLine('  end;')
    .AppendLine('end.')
end;

procedure TUnitMainBeginEndCommand.ExecuteInterface(Section: TStringBuilder;
  Model: TJsonObject);
begin
  inherited;

end;

{ TUnitRunServerProcBody }

procedure TUnitRunServerProcBody.ExecuteImplementation(Section: TStringBuilder;
  Model: TJSONObject);
begin
  inherited;
  Section
    .AppendLine('procedure RunServer(APort: Integer);')
    .AppendLine('var')
    .AppendLine('  LServer: TIdHTTPWebBrokerBridge;')
    .AppendLine('begin')
    .AppendLine('  LServer := TIdHTTPWebBrokerBridge.Create(nil);')
    .AppendLine('  try')
    .AppendLine('    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;')
    .AppendLine('    LServer.DefaultPort := APort;')
    .AppendLine('    LServer.KeepAlive := True;')
    .AppendLine('    LServer.MaxConnections := dotEnv.Env(''dmvc.webbroker.max_connections'', 0);')
    .AppendLine('    LServer.ListenQueue := dotEnv.Env(''dmvc.indy.listen_queue'', 500);')
    .AppendLine('    LServer.Active := True;')
    .AppendLine('    LogI(''Listening on port '' + APort.ToString);')
    .AppendLine('    LogI(''Application started. Press Ctrl+C to shut down.'');')
    .AppendLine('    WaitForTerminationSignal;')
    .AppendLine('    EnterInShutdownState;')
    .AppendLine('    LServer.Active := False;')
    .AppendLine('  finally')
    .AppendLine('    LServer.Free;')
    .AppendLine('  end;')
    .AppendLine('end;')
end;

procedure TUnitRunServerProcBody.ExecuteInterface(Section: TStringBuilder;
  Model: TJsonObject);
begin
  inherited;

end;

{ TUnitControllerEntityImplementationCommand }

procedure TUnitControllerEntityImplementationCommand.ExecuteImplementation(
  Section: TStringBuilder; Model: TJSONObject);
begin
  inherited;
  if not Model.B['entity.generate'] then Exit;
  CheckFor('entity.class_name', Model);
  Section
    .AppendLine('constructor ' + Model['entity.class_name'] + '.Create(FirstName, LastName: String; DOB: TDate);')
    .AppendLine('begin')
    .AppendLine('  inherited Create;')
    .AppendLine('  fFirstName := FirstName;')
    .AppendLine('  fLastName := LastName;')
    .AppendLine('  fDOB := DOB;')
    .AppendLine('end;')
end;

{ TUnitControllerControllerImplementationCommand }

procedure TUnitControllerControllerImplementationCommand.ExecuteImplementation(
  Section: TStringBuilder; Model: TJSONObject);
begin
  inherited;
  CheckFor('controller.name', Model);

  Section
    .AppendLine('[MVCPath(''/api'')]')
    .AppendLine(Model['controller.name'] + ' = class(TMVCController)')
    .AppendLine('  public');

  if Model.B['controller.index_methods.generate'] then
  begin
    Section
      .AppendLine('    [MVCPath]')
      .AppendLine('    [MVCHTTPMethod([httpGET])]')
      .AppendLine('    function Index: String;')
      .AppendLine('    [MVCPath(''/reversedstrings/($Value)'')]')
      .AppendLine('    [MVCHTTPMethod([httpGET])]')
      .AppendLine('    [MVCProduces(TMVCMediaType.TEXT_PLAIN)]')
      .AppendLine('    function GetReversedString(const Value: String): String;')
  end;

  if Model.B['controller.action_filters.generate'] then
  begin
    Section
      .AppendLine('  protected')
      .AppendLine('    procedure OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean); override;')
      .AppendLine
      .AppendLine('    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;')
  end;

  if Model.B['controller.crud_methods.generate'] then
  begin
    Section
      .AppendLine('  public')
      .AppendLine('    //Sample CRUD Actions for a "People" entity')
      .AppendLine('    [MVCPath(''/people'')]')
      .AppendLine('    [MVCHTTPMethod([httpGET])]')
      .AppendLine('    function GetPeople: TObjectList<TPerson>;')
      .AppendLine('    [MVCPath(''/people/($ID)'')]')
      .AppendLine('    [MVCHTTPMethod([httpGET])]')
      .AppendLine('    function GetPerson(ID: Integer): TPerson;')
      .AppendLine('    [MVCPath(''/people'')]')
      .AppendLine('    [MVCHTTPMethod([httpPOST])]')
      .AppendLine('    function CreatePerson([MVCFromBody] Person: TPerson): IMVCResponse;')
      .AppendLine('    [MVCPath(''/people/($ID)'')]')
      .AppendLine('    [MVCHTTPMethod([httpPUT])]')
      .AppendLine('    function UpdatePerson(ID: Integer; [MVCFromBody] Person: TPerson): IMVCResponse;')
      .AppendLine('    [MVCPath(''/people/($ID)'')]')
      .AppendLine('    [MVCHTTPMethod([httpDELETE])]')
      .AppendLine('    function DeletePerson(ID: Integer): IMVCResponse;')
  end;

  Section
    .AppendLine('end;')
    .AppendLine

end;

end.
