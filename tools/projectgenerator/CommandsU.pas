unit CommandsU;

interface

uses
  CommonsU, System.SysUtils, MVCFramework.Commons, System.DateUtils,
  JsonDataObjects;

type
  TUnitInterfaceKeywordCommand = class(TCustomCommand)
  public
    procedure ExecuteInterface(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
     procedure ExecuteImplementation(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
  end;

  TUnitDMVCLicenseCommand = class(TCustomCommand)
  public
    procedure ExecuteInterface(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
  end;

  TUnitControllerCommand = class(TCustomCommand)
  public
    procedure ExecuteInterface(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
    procedure ExecuteImplementation(
      Section: TStringBuilder;
      Model: TJsonObject
      ); override;
  end;

  TUnitControllerEntityDeclarationCommand = class(TCustomCommand)
  public
    procedure ExecuteInterface(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
    procedure ExecuteImplementation(
      Section: TStringBuilder;
      Model: TJsonObject
      ); override;
  end;


  TUnitControllerDeclarationCommand = class(TCustomCommand)
  public
    procedure ExecuteInterface(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
  end;

  TUnitControllerControllerDeclarationCommand = class(TCustomCommand)
  public
    procedure ExecuteInterface(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
    procedure ExecuteImplementation(
      Section: TStringBuilder;
      Model: TJsonObject
      ); override;
  end;

  TUnitProgramCommand = class(TCustomCommand)
  public
    procedure ExecuteInterface(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
    procedure ExecuteImplementation(Section: TStringBuilder; Model: TJsonObject); override;
  end;

  TUnitUsesCommand = class(TCustomCommand)
  public
    procedure ExecuteInterface(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
  end;

  TWebModuleDFMCommand = class(TCustomCommand)
  public
    procedure ExecuteInterface(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
     procedure ExecuteImplementation(
        Section: TStringBuilder;
        Model: TJsonObject); override;
  end;

  TUnitWebModuleDeclarationCommand = class(TCustomCommand)
  public
    procedure ExecuteInterface(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
    procedure ExecuteImplementation(
      Section: TStringBuilder;
      Model: TJsonObject
      ); override;
  end;

  TUnitJSONRPCDeclarationCommand = class(TCustomCommand)
  public
    procedure ExecuteInterface(
      Section: TStringBuilder;
      Model: TJSONObject
      ); override;
    procedure ExecuteImplementation(
      Section: TStringBuilder;
      Model: TJsonObject
      ); override;
  end;

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


{ TUnitUsesCommand }

procedure TUnitUsesCommand.ExecuteInterface(Section: TStringBuilder;
  Model: TJSONObject);
begin
  inherited;
  Section
    .AppendLine('uses')
    .AppendLine('  System.SysUtils')
    .AppendLine('  ;')
    .AppendLine
end;

{ TUnitDMVCLicenseCommand }

procedure TUnitDMVCLicenseCommand.ExecuteInterface(Section: TStringBuilder;
  Model: TJSONObject);
begin
  inherited;
  Section
    .AppendLine('// ***************************************************************************')
    .AppendLine('//')
    .AppendLine('// Delphi MVC Framework')
    .AppendLine('//')
    .AppendLine('// Copyright (c) 2010-' + YearOf(Date).ToString + ' Daniele Teti and the DMVCFramework Team')
    .AppendLine('//')
    .AppendLine('// https://github.com/danieleteti/delphimvcframework')
    .AppendLine('//')
    .AppendLine('// ***************************************************************************')
    .AppendLine('//')
    .AppendLine('// Licensed under the Apache License, Version 2.0 (the "License");')
    .AppendLine('// you may not use this file except in compliance with the License.')
    .AppendLine('// You may obtain a copy of the License at')
    .AppendLine('//')
    .AppendLine('// http://www.apache.org/licenses/LICENSE-2.0')
    .AppendLine('//')
    .AppendLine('// Unless required by applicable law or agreed to in writing, software')
    .AppendLine('// distributed under the License is distributed on an "AS IS" BASIS,')
    .AppendLine('// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.')
    .AppendLine('// See the License for the specific language governing permissions and')
    .AppendLine('// limitations under the License.')
    .AppendLine('//')
    .AppendLine('// This IDE expert is based off of the one included with the DUnitX')
    .AppendLine('// project.  Original source by Robert Love.  Adapted by Nick Hodges.')
    .AppendLine('//')
    .AppendLine('// The DUnitX project is run by Vincent Parrett and can be found at:')
    .AppendLine('//')
    .AppendLine('// https://github.com/VSoftTechnologies/DUnitX')
    .AppendLine('// ***************************************************************************')
    .AppendLine;
end;

{ TUnitProgramCommand }

procedure TUnitProgramCommand.ExecuteImplementation(Section: TStringBuilder;
  Model: TJsonObject);
begin
  inherited;

end;

procedure TUnitProgramCommand.ExecuteInterface(
  Section: TStringBuilder;
  Model: TJSONObject);
begin
  inherited;
  CheckFor('program.name', Model);
  Section
    .AppendLine('program ' + Model[TConfigKey.program_name] + ';')
    .AppendLine
    .AppendLine('{$APPTYPE CONSOLE}')
    .AppendLine
    .AppendLine('uses');

  if Model[TConfigKey.program_msheap] then
  begin
    Section.AppendLine('  MSHeap,');
  end;

  Section
    .AppendLine('  System.SysUtils,')
    .AppendLine('  Web.ReqMulti,')
    .AppendLine('  Web.WebReq,')
    .AppendLine('  Web.WebBroker,')
    .AppendLine('  IdContext,')
    .AppendLine('  IdHTTPWebBrokerBridge,')
    .AppendLine('  MVCFramework,')
    .AppendLine('  MVCFramework.Logger,')
    .AppendLine('  MVCFramework.DotEnv,')
    .AppendLine('  MVCFramework.Commons,')
    .AppendLine('  MVCFramework.Signal;')
    .AppendLine()
    .AppendLine('{$R *.res}')
    .AppendLine
end;

{ TUnitControllerCommand }

procedure TUnitControllerCommand.ExecuteImplementation(Section: TStringBuilder;
  Model: TJsonObject);
begin
  inherited;
  Section
    .AppendLine('implementation')
    .AppendLine
    .AppendLine('uses')
    .AppendLine('  System.StrUtils, System.SysUtils, MVCFramework.Logger;')
    .AppendLine;
end;

procedure TUnitControllerCommand.ExecuteInterface(Section: TStringBuilder;
  Model: TJSONObject);
begin
  inherited;
  Section
    .AppendLine('unit ' + Model[TConfigKey.controller_unit_name] + ';')
    .AppendLine
    .AppendLine('interface')
    .AppendLine
    .AppendLine('uses')
    .AppendLine('  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons, System.Generics.Collections;')
    .AppendLine
    .AppendLine('type')
end;

{ TUnitControllerDeclarationCommand }

procedure TUnitControllerDeclarationCommand.ExecuteInterface(Section: TStringBuilder;
  Model: TJSONObject);
begin
  inherited;


end;

procedure TUnitControllerEntityDeclarationCommand.ExecuteImplementation(
  Section: TStringBuilder; Model: TJsonObject);
begin
  inherited;
  if not Model.B[TConfigKey.entity_generate] then Exit;
  Section
    .AppendLine('constructor ' + Model[TConfigKey.entity_classname] + '.Create(FirstName, LastName: String; DOB: TDate);')
    .AppendLine('begin')
    .AppendLine('  inherited Create;')
    .AppendLine('  fFirstName := FirstName;')
    .AppendLine('  fLastName := LastName;')
    .AppendLine('  fDOB := DOB;')
    .AppendLine('end;')
end;

{ TUnitControllerEntitiesCommand }

procedure TUnitControllerEntityDeclarationCommand.ExecuteInterface(Section: TStringBuilder;
  Model: TJSONObject);
begin
  inherited;
  if not Model.B[TConfigKey.entity_generate] then Exit;

  CheckFor('entity.classname', Model);
  Section
    .AppendLine('  [MVCNameCase(ncCamelCase)]')
    .AppendLine('  ' + Model[TConfigKey.entity_classname] + ' = class')
    .AppendLine('  private')
    .AppendLine('    fFirstName: String;')
    .AppendLine('    fLastName: String;')
    .AppendLine('    fDOB: TDate;')
    .AppendLine('  public')
    .AppendLine('    property FirstName: String read fFirstName write fFirstName;')
    .AppendLine('    property LastName: String read fLastName write fLastName;')
    .AppendLine('    property DOB: TDate read fDOB write fDOB;  ')
    .AppendLine('    constructor Create(FirstName, LastName: String; DOB: TDate);')
    .AppendLine('  end;')
    .AppendLine
end;

{ TUnitInterfaceKeywordCommand }

procedure TUnitInterfaceKeywordCommand.ExecuteImplementation(
  Section: TStringBuilder; Model: TJSONObject);
begin
  inherited;
  Section.AppendLine('implementation').AppendLine;
end;

procedure TUnitInterfaceKeywordCommand.ExecuteInterface(Section: TStringBuilder;
  Model: TJSONObject);
begin
  inherited;
  Section.AppendLine('interface').AppendLine;
end;

{ TUnitControllerControllerDeclarationCommand }

procedure TUnitControllerControllerDeclarationCommand.ExecuteImplementation(
  Section: TStringBuilder; Model: TJsonObject);
begin
  inherited;
  CheckFor(TConfigKey.controller_classname, Model);

  if Model.B[TConfigKey.controller_action_filters_generate] then
  begin
    Section
      .AppendLine
      .AppendLine('procedure ' + Model[TConfigKey.controller_classname] + '.OnAfterAction(Context: TWebContext; const AActionName: string);')
      .AppendLine('begin')
      .AppendLine('  { Executed after each action }')
      .AppendLine('  inherited;')
      .AppendLine('end;')
      .AppendLine
      .AppendLine('procedure ' + Model[TConfigKey.controller_classname] + '.OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean);')
      .AppendLine('begin')
      .AppendLine('  { Executed before each action')
      .AppendLine('    if handled is true (or an exception is raised) the actual')
      .AppendLine('    action will not be called }')
      .AppendLine('  inherited;')
      .AppendLine('end;')
  end;

  if Model.B[TConfigKey.controller_index_methods_generate] then
  begin
    Section
      .AppendLine
      .AppendLine('function ' + Model[TConfigKey.controller_classname] + '.Index: String;')
      .AppendLine('begin')
      .AppendLine('  //use Context property to access to the HTTP request and response')
      .AppendLine('  Result := ''Hello DelphiMVCFramework World'';')
      .AppendLine('end;')
      .AppendLine
      .AppendLine('function ' + Model[TConfigKey.controller_classname] + '.GetReversedString(const Value: String): String;')
      .AppendLine('begin')
      .AppendLine('  Result := System.StrUtils.ReverseString(Value.Trim);')
      .AppendLine('end;')
  end;

  if Model.B[TConfigKey.controller_crud_methods_generate] then
  begin
    Section
      .AppendLine
      .AppendLine('//Sample CRUD Actions for a "People" entity')
      .AppendLine('function ' + Model[TConfigKey.controller_classname] + '.GetPeople: TObjectList<TPerson>;')
      .AppendLine('var')
      .AppendLine('  lPeople: TObjectList<TPerson>;')
      .AppendLine('begin')
      .AppendLine('  lPeople := TObjectList<TPerson>.Create(True);')
      .AppendLine('  try')
      .AppendLine('    lPeople.Add(TPerson.Create(''Peter'',''Parker'', EncodeDate(1965, 10, 4)));')
      .AppendLine('    lPeople.Add(TPerson.Create(''Bruce'',''Banner'', EncodeDate(1945, 9, 6)));')
      .AppendLine('    lPeople.Add(TPerson.Create(''Reed'',''Richards'', EncodeDate(1955, 3, 7)));')
      .AppendLine('    Result := lPeople;')
      .AppendLine('  except')
      .AppendLine('    lPeople.Free;')
      .AppendLine('    raise;')
      .AppendLine('  end;')
      .AppendLine('end;')
      .AppendLine
      .AppendLine('function ' + Model[TConfigKey.controller_classname] + '.GetPerson(ID: Integer): TPerson;')
      .AppendLine('var')
      .AppendLine('  lPeople: TObjectList<TPerson>;')
      .AppendLine('begin')
      .AppendLine('  lPeople := GetPeople;')
      .AppendLine('  try')
      .AppendLine('    Result := lPeople.ExtractAt(ID mod lPeople.Count);')
      .AppendLine('  finally')
      .AppendLine('    lPeople.Free;')
      .AppendLine('  end;')
      .AppendLine('end;')
      .AppendLine
      .AppendLine('function ' + Model[TConfigKey.controller_classname] + '.CreatePerson([MVCFromBody] Person: TPerson): IMVCResponse;')
      .AppendLine('begin')
      .AppendLine('  LogI(''Created '' + Person.FirstName + '' '' + Person.LastName);')
      .AppendLine('  Result := MVCResponseBuilder')
      .AppendLine('      .StatusCode(HTTP_STATUS.Created)')
      .AppendLine('      .Body(''Person created'')')
      .AppendLine('      .Build;')
      .AppendLine('end;')
      .AppendLine
      .AppendLine('function ' + Model[TConfigKey.controller_classname] + '.UpdatePerson(ID: Integer; [MVCFromBody] Person: TPerson): IMVCResponse;')
      .AppendLine('begin')
      .AppendLine('  LogI(''Updated '' + Person.FirstName + '' '' + Person.LastName);')
      .AppendLine('  Result := MVCResponseBuilder')
      .AppendLine('    .StatusCode(HTTP_STATUS.NoContent)')
      .AppendLine('    .Build;')
      .AppendLine('end;')
      .AppendLine
      .AppendLine('function ' + Model[TConfigKey.controller_classname] + '.DeletePerson(ID: Integer): IMVCResponse;')
      .AppendLine('begin')
      .AppendLine('  LogI(''Deleted person with id '' + ID.ToString);')
      .AppendLine('  Result := MVCResponseBuilder')
      .AppendLine('    .StatusCode(HTTP_STATUS.NoContent)')
      .AppendLine('    .Build;')
      .AppendLine('end;')

  end;
end;

procedure TUnitControllerControllerDeclarationCommand.ExecuteInterface(
  Section: TStringBuilder; Model: TJSONObject);
begin
  inherited;
  CheckFor('controller.classname', Model);

  Section
    .AppendLine('  [MVCPath(''/api'')]')
    .AppendLine('  ' + Model[TConfigKey.controller_classname] + ' = class(TMVCController)');

  if Model.B[TConfigKey.controller_action_filters_generate] then
  begin
    Section
      .AppendLine('  protected')
      .AppendLine('    procedure OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean); override;')
      .AppendLine('    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;')
  end;

  if Model.B[TConfigKey.controller_index_methods_generate] then
  begin
    Section
      .AppendLine('  public')
      .AppendLine('    [MVCPath]')
      .AppendLine('    [MVCHTTPMethod([httpGET])]')
      .AppendLine('    function Index: String;')
      .AppendLine('    [MVCPath(''/reversedstrings/($Value)'')]')
      .AppendLine('    [MVCHTTPMethod([httpGET])]')
      .AppendLine('    [MVCProduces(TMVCMediaType.TEXT_PLAIN)]')
      .AppendLine('    function GetReversedString(const Value: String): String;')
  end;

  if Model.B[TConfigKey.controller_crud_methods_generate] then
  begin
    if not Model.B[TConfigKey.controller_index_methods_generate] then
    begin
      Section
        .AppendLine('  public')
    end;
    Section
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
    .AppendLine('  end;')
    .AppendLine

end;

{ TWebModuleDFMCommand }

procedure TWebModuleDFMCommand.ExecuteImplementation(Section: TStringBuilder;
  Model: TJsonObject);
begin
  inherited;
  Section
    .AppendLine('object ' + Model.S[TConfigKey.webmodule_classname].Substring(1) + ': ' + Model[TConfigKey.webmodule_classname])
    .AppendLine('  OldCreateOrder = False')
    .AppendLine('  OnCreate = WebModuleCreate')
    .AppendLine('  OnDestroy = WebModuleDestroy')
    .AppendLine('  Height = 230')
    .AppendLine('  Width = 415')
    .AppendLine('end')
end;

procedure TWebModuleDFMCommand.ExecuteInterface(Section: TStringBuilder;
  Model: TJSONObject);
begin
  inherited;

end;

{ TUnitWebModuleDeclarationCommand }

procedure TUnitWebModuleDeclarationCommand.ExecuteImplementation(
  Section: TStringBuilder; Model: TJsonObject);
var
  activerecord_con_def_name: string;
  activerecord_con_def_filename: string;
begin
  inherited;
  Section
    .AppendLine
    .AppendLine('implementation')
    .AppendLine
    .AppendLine('{$R *.dfm}')
    .AppendLine
    .AppendLine('uses')
    .AppendLine('  ' + Model[TConfigKey.controller_unit_name] + ',');

  if Model.B[TConfigKey.jsonrpc_generate] then
  begin
    Section.AppendLine('  ' + Model[TConfigKey.jsonrpc_unit_name] + ',')
  end;

  Section
    .AppendLine('  System.IOUtils,')
    .AppendLine('  MVCFramework.Commons,')
    .AppendLine('  MVCFramework.Middleware.ActiveRecord,')
    .AppendLine('  MVCFramework.Middleware.StaticFiles,')
    .AppendLine('  MVCFramework.Middleware.Analytics,')
    .AppendLine('  MVCFramework.Middleware.Trace,')
    .AppendLine('  MVCFramework.Middleware.CORS,')
    .AppendLine('  MVCFramework.Middleware.ETag,')
    .AppendLine('  MVCFramework.Middleware.Compression;')
    .AppendLine
    .AppendLine('procedure ' + Model.S[TConfigKey.webmodule_classname] + '.WebModuleCreate(Sender: TObject);')
    .AppendLine('begin')
    .AppendLine('  FMVC := TMVCEngine.Create(Self,')
    .AppendLine('    procedure(Config: TMVCConfig)')
    .AppendLine('    begin')
    .AppendLine('      Config.dotEnv := dotEnv; ')
    .AppendLine('      // session timeout (0 means session cookie)')
    .AppendLine('      Config[TMVCConfigKey.SessionTimeout] := dotEnv.Env(''dmvc.session_timeout'', ''0'');')
    .AppendLine('      //default content-type')
    .AppendLine('      Config[TMVCConfigKey.DefaultContentType] := dotEnv.Env(''dmvc.default.content_type'', TMVCConstants.DEFAULT_CONTENT_TYPE);')
    .AppendLine('      //default content charset')
    .AppendLine('      Config[TMVCConfigKey.DefaultContentCharset] := dotEnv.Env(''dmvc.default.content_charset'', TMVCConstants.DEFAULT_CONTENT_CHARSET);')
    .AppendLine('      //unhandled actions are permitted?')
    .AppendLine('      Config[TMVCConfigKey.AllowUnhandledAction] := dotEnv.Env(''dmvc.allow_unhandled_actions'', ''false'');')
    .AppendLine('      //enables or not system controllers loading (available only from localhost requests)')
    .AppendLine('      Config[TMVCConfigKey.LoadSystemControllers] := dotEnv.Env(''dmvc.load_system_controllers'', ''true'');')
    .AppendLine('      //default view file extension')
    .AppendLine('      Config[TMVCConfigKey.DefaultViewFileExtension] := dotEnv.Env(''dmvc.default.view_file_extension'', ''html'');')
    .AppendLine('      //view path')
    .AppendLine('      Config[TMVCConfigKey.ViewPath] := dotEnv.Env(''dmvc.view_path'', ''templates'');')
    .AppendLine('      //use cache for server side views (use "false" in debug and "true" in production for faster performances')
    .AppendLine('      Config[TMVCConfigKey.ViewCache] := dotEnv.Env(''dmvc.view_cache'', ''false'');')
    .AppendLine('      //Max Record Count for automatic Entities CRUD')
    .AppendLine('      Config[TMVCConfigKey.MaxEntitiesRecordCount] := dotEnv.Env(''dmvc.max_entities_record_count'', IntToStr(TMVCConstants.MAX_RECORD_COUNT));')
    .AppendLine('      //Enable Server Signature in response')
    .AppendLine('      Config[TMVCConfigKey.ExposeServerSignature] := dotEnv.Env(''dmvc.expose_server_signature'', ''false'');')
    .AppendLine('      //Enable X-Powered-By Header in response')
    .AppendLine('      Config[TMVCConfigKey.ExposeXPoweredBy] := dotEnv.Env(''dmvc.expose_x_powered_by'', ''true'');')
    .AppendLine('      // Max request size in bytes')
    .AppendLine('      Config[TMVCConfigKey.MaxRequestSize] := dotEnv.Env(''dmvc.max_request_size'', IntToStr(TMVCConstants.DEFAULT_MAX_REQUEST_SIZE));')
    .AppendLine('    end);')
    .AppendLine
    .AppendLine('  // Controllers')
    .AppendLine('  FMVC.AddController(' + Model[TConfigKey.controller_classname] + ');')
    .AppendLine('  // Controllers - END')
    .AppendLine
    .AppendLine('  // Middleware');

    if Model.B[TConfigKey.webmodule_middleware_analytics] then
    begin
      Section.AppendLine('  fMVC.AddMiddleware(TMVCAnalyticsMiddleware.Create(GetAnalyticsDefaultLogger));')
    end;

    if Model.B[TConfigKey.webmodule_middleware_staticfiles] then
    begin
      Section.AppendLine('  fMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create(''/static'', TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), ''www'')));')
    end;

    if Model.B[TConfigKey.webmodule_middleware_trace] then
    begin
      Section.AppendLine('  fMVC.AddMiddleware(TMVCTraceMiddleware.Create);')
    end;

    if Model.B[TConfigKey.webmodule_middleware_compression] then
    begin
      Section.AppendLine('  fMVC.AddMiddleware(TMVCCompressionMiddleware.Create);')
    end;

    if Model.B[TConfigKey.webmodule_middleware_etag] then
    begin
      Section.AppendLine('  fMVC.AddMiddleware(TMVCETagMiddleware.Create);')
    end;

    if Model.B[TConfigKey.webmodule_middleware_cors] then
    begin
      Section.AppendLine('  fMVC.AddMiddleware(TMVCCORSMiddleware.Create);')
    end;

    if Model.B[TConfigKey.webmodule_middleware_activerecord] then
    begin
      activerecord_con_def_name := Model[TConfigKey.webmodule_middleware_activerecord_con_def_name];
      activerecord_con_def_filename := Model[TConfigKey.webmodule_middleware_activerecord_con_def_filename];
      Section
        .AppendLine('  fMVC.AddMiddleware(TMVCActiveRecordMiddleware.Create(')
        .AppendLine('    dotEnv.Env(''firedac.connection_definition_name'', ''' +  activerecord_con_def_name + '''),')
        .AppendLine('    dotEnv.Env(''firedac.connection_definitions_filename'', ''' +  activerecord_con_def_filename + ''')')
        .AppendLine('  ));')
    end;

    Section
      .AppendLine('  // Middleware - END');

    if Model.B[TConfigKey.jsonrpc_generate] then
    begin
      Section
        .AppendLine
        .AppendLine('  // JSONRPC')
        .AppendLine('  fMVC.PublishObject(')
        .AppendLine('    function : TObject')
        .AppendLine('    begin')
        .AppendLine('      Result := ' + Model[TConfigKey.jsonrpc_classname] + '.Create;')
        .AppendLine('    end, ''/jsonrpc'');')
        .AppendLine('  // JSONRPC - END')
    end;

    Section
      .AppendLine
      .AppendLine('end;')
      .AppendLine
      .AppendLine('procedure ' + Model.S[TConfigKey.webmodule_classname] + '.WebModuleDestroy(Sender: TObject);')
      .AppendLine('begin')
      .AppendLine('  FMVC.Free;')
      .AppendLine('end;')
      .AppendLine
      .AppendLine('end.')
end;

procedure TUnitWebModuleDeclarationCommand.ExecuteInterface(
  Section: TStringBuilder; Model: TJSONObject);
begin
  inherited;
  CheckFor(TConfigKey.webmodule_unit_name, Model);
  CheckFor(TConfigKey.webmodule_classname, Model);
  Section
    .AppendLine('unit ' + Model.S[TConfigKey.webmodule_unit_name] + ';')
    .AppendLine('')
    .AppendLine('interface')
    .AppendLine
    .AppendLine('uses ')
    .AppendLine('  System.SysUtils,')
    .AppendLine('  System.Classes,')
    .AppendLine('  Web.HTTPApp,')
    .AppendLine('  MVCFramework;')
    .AppendLine
    .AppendLine('type')
    .AppendLine('  ' + Model.S[TConfigKey.webmodule_classname] + ' = class(TWebModule)')
    .AppendLine('    procedure WebModuleCreate(Sender: TObject);')
    .AppendLine('    procedure WebModuleDestroy(Sender: TObject);')
    .AppendLine('  private')
    .AppendLine('    fMVC: TMVCEngine;')
    .AppendLine('  end;')
    .AppendLine
    .AppendLine('var')
    .AppendLine('  WebModuleClass: TComponentClass = ' + Model.S[TConfigKey.webmodule_classname] + ';')
end;

{ TUnitJSONRPCDeclarationCommand }

procedure TUnitJSONRPCDeclarationCommand.ExecuteImplementation(
  Section: TStringBuilder; Model: TJsonObject);
begin
  inherited;
  if not Model.B[TConfigKey.jsonrpc_generate] then
  begin
    Exit;
  end;
  Section
    .AppendLine('implementation')
    .AppendLine
    .AppendLine('uses')
    .AppendLine('  System.StrUtils;')
    .AppendLine
    .AppendLine('function ' + Model.S[TConfigKey.jsonrpc_classname] + '.ReverseString(const Value: String): String;')
    .AppendLine('begin')
    .AppendLine('  Result := System.StrUtils.ReverseString(Value);')
    .AppendLine('end;')
    .AppendLine
    .AppendLine('end.')
end;

procedure TUnitJSONRPCDeclarationCommand.ExecuteInterface(
  Section: TStringBuilder; Model: TJSONObject);
begin
  inherited;
  CheckFor(TConfigKey.jsonrpc_generate, Model);
  if not Model.B[TConfigKey.jsonrpc_generate] then
  begin
    Exit;
  end;

  CheckFor(TConfigKey.jsonrpc_unit_name, Model);
  CheckFor(TConfigKey.jsonrpc_classname, Model);
  Section
    .AppendLine('unit ' + Model[TConfigKey.jsonrpc_unit_name] + ';')
    .AppendLine
    .AppendLine('interface')
    .AppendLine
    .AppendLine('type')
    .AppendLine('  ' + Model[TConfigKey.jsonrpc_classname] + ' = class')
    .AppendLine('  public')
    .AppendLine('    function ReverseString(const Value: String): String;')
    .AppendLine('  end;')
    .AppendLine;
end;

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
    .AppendLine('  IsMultiThread := True;')
    .AppendLine('  // DMVCFramework Specific Configuration ')
    .AppendLine('  // When MVCSerializeNulls = True empty nullables and nil are serialized as json null.')
    .AppendLine('  // When MVCSerializeNulls = False empty nullables and nil are not serialized at all.')
    .AppendLine('  MVCSerializeNulls := True;')
    .AppendLine('  UseConsoleLogger := True;')
    .AppendLine
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
    .AppendLine('    RunServer(dotEnv.Env(''dmvc.server.port'', ' + Model[TConfigKey.program_default_server_port] + '));')
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
  if not Model.B[TConfigKey.entity_generate] then Exit;
  CheckFor(TConfigKey.entity_classname, Model);
  Section
    .AppendLine('constructor ' + Model[TConfigKey.entity_classname] + '.Create(FirstName, LastName: String; DOB: TDate);')
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
  CheckFor(TConfigKey.controller_classname, Model);

  Section
    .AppendLine('[MVCPath(''/api'')]')
    .AppendLine(Model[TConfigKey.controller_classname] + ' = class(TMVCController)')
    .AppendLine('  public');

  if Model.B[TConfigKey.controller_index_methods_generate] then
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

  if Model.B[TConfigKey.controller_action_filters_generate] then
  begin
    Section
      .AppendLine('  protected')
      .AppendLine('    procedure OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean); override;')
      .AppendLine
      .AppendLine('    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;')
  end;

  if Model.B[TConfigKey.controller_crud_methods_generate] then
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
