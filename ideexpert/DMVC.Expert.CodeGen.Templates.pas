// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// This IDE expert is based off of the one included with the DUnitX
// project.  Original source by Robert Love.  Adapted by Nick Hodges.
//
// The DUnitX project is run by Vincent Parrett and can be found at:
//
// https://github.com/VSoftTechnologies/DUnitX
// ***************************************************************************

unit DMVC.Expert.CodeGen.Templates;

interface

{$I ..\sources\dmvcframework.inc}

resourcestring

  { Delphi template code }
  // 0 - project name
  // 1 - http/s port
  sDMVCDPR =
    'program %0:s;' + sLineBreak +
    sLineBreak +
    '{$APPTYPE CONSOLE}' + sLineBreak +
    sLineBreak +
    'uses' + sLineBreak +
    '  System.SysUtils,' + sLineBreak +
    '  MVCFramework,' + sLineBreak +
    '  MVCFramework.Logger,' + sLineBreak +
    '  MVCFramework.DotEnv,' + sLineBreak +
    '  MVCFramework.Commons,' + sLineBreak +
    '  MVCFramework.Signal,' + sLineBreak +
    {$IF Defined(SeattleOrBetter)}
    '  Web.ReqMulti, //If you have problem with this unit, see https://quality.embarcadero.com/browse/RSP-17216' + sLineBreak +
    '  Web.WebReq,' + sLineBreak +
    '  Web.WebBroker,' + sLineBreak +
    {$ELSE}
    '  ReqMulti, //If you have problem with this unit, see https://quality.embarcadero.com/browse/RSP-17216' + sLineBreak +
    '  WebReq,' + sLineBreak +
    '  WebBroker,' + sLineBreak +
    {$ENDIF}
    '  IdContext,' + sLineBreak +
    '  IdHTTPWebBrokerBridge;' + sLineBreak +
    sLineBreak +
    '{$R *.res}' + sLineBreak +
    sLineBreak +
    sLineBreak +
    'procedure RunServer(APort: Integer);' + sLineBreak +
    'var' + sLineBreak +
    '  LServer: TIdHTTPWebBrokerBridge;' + sLineBreak +
    'begin' + sLineBreak +
    '  Writeln(''** DMVCFramework Server ** build '' + DMVCFRAMEWORK_VERSION);' + sLineBreak +
    '  LServer := TIdHTTPWebBrokerBridge.Create(nil);' + sLineBreak +
    '  try' + sLineBreak +
    '    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;' + sLineBreak +
    '    LServer.DefaultPort := APort;' + sLineBreak +
    '    LServer.KeepAlive := True;' + sLineBreak +
    '    LServer.MaxConnections := dotEnv.Env(''dmvc.webbroker.max_connections'', 0);' + sLineBreak +
    '    LServer.ListenQueue := dotEnv.Env(''dmvc.indy.listen_queue'', 500);' + sLineBreak + sLineBreak +
    '    LServer.Active := True;' + sLineBreak +
    '    WriteLn(''Listening on port '', APort);' + sLineBreak +
    '    Write(''CTRL+C to shutdown the server'');' + sLineBreak +
    '    WaitForTerminationSignal; ' + sLineBreak +
    '    EnterInShutdownState; ' + sLineBreak +
    '    LServer.Active := False; ' + sLineBreak +
    '  finally' + sLineBreak +
    '    LServer.Free;' + sLineBreak +
    '  end;' + sLineBreak +
    'end;' + sLineBreak +
    sLineBreak +
    'begin' + sLineBreak +
    '  { Enable ReportMemoryLeaksOnShutdown during debug }' + sLineBreak +
	'  // ReportMemoryLeaksOnShutdown := True;' + sLineBreak +
    '  IsMultiThread := True;' + sLineBreak + sLineBreak +	
    '  // DMVCFramework Specific Configuration ' + sLineBreak +
    '  // When MVCSerializeNulls = True empty nullables and nil are serialized as json null.' + sLineBreak +
    '  // When MVCSerializeNulls = False empty nullables and nil are not serialized at all.' + sLineBreak +
    '  MVCSerializeNulls := True;' + sLineBreak + sLineBreak +
    '  try' + sLineBreak +
    '    if WebRequestHandler <> nil then' + sLineBreak +
    '      WebRequestHandler.WebModuleClass := WebModuleClass;' + sLineBreak +
    '' + sLineBreak +
    '    dotEnvConfigure(' + sLineBreak +
    '      function: IMVCDotEnv' + sLineBreak +
    '      begin' + sLineBreak +
    '        Result := NewDotEnv' + sLineBreak +
    '                 .WithStrategy(TMVCDotEnvPriority.FileThenEnv)' + sLineBreak +
    '                                       //if available, by default, loads default environment (.env)' + sLineBreak +
    '                 .UseProfile(''test'') //if available loads the test environment (.env.test)' + sLineBreak +
    '                 .UseProfile(''prod'') //if available loads the prod environment (.env.prod)' + sLineBreak +
    '                 .UseLogger(procedure(LogItem: String)' + sLineBreak +
    '                            begin' + sLineBreak +
    '                              LogW(''dotEnv: '' + LogItem);' + sLineBreak +
    '                            end)' + sLineBreak +
    '                 .Build();             //uses the executable folder to look for .env* files' + sLineBreak +
    '      end);' + sLineBreak +
    '' + sLineBreak +
    '    WebRequestHandlerProc.MaxConnections := dotEnv.Env(''dmvc.handler.max_connections'', 1024);' + sLineBreak +	
    '    RunServer(dotEnv.Env(''dmvc.server.port'', %1:d));' + sLineBreak +
    '  except' + sLineBreak +
    '    on E: Exception do' + sLineBreak +
    '      Writeln(E.ClassName, '': '', E.Message);' + sLineBreak +
    '  end;' + sLineBreak +
    'end.' + sLineBreak;

  // 0 - Unit Name
  // 1 - Class Name
  // 2 - Sample Methods - Interface
  // 3 - Sample Methods - Implementation
  // 4 - Action Filters - Interface
  // 5 - Action Filters - Implementation
  // 8 - BO - Interface
  // 9 - BO - Implementation

  sControllerUnit = 'unit %0:s;' + sLineBreak +
    sLineBreak +
    'interface' + sLineBreak +
    sLineBreak +
    'uses' + sLineBreak +
    '  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons, System.Generics.Collections;' + sLineBreak +
    sLineBreak +
    'type' + sLineBreak +
    '%8:s' + sLineBreak +
    '  [MVCPath(''/api'')]' + sLineBreak +
    '  %1:s = class(TMVCController) ' + sLineBreak +
    '  public' + sLineBreak +
    '%2:s' +
    '%4:s' +
    '%6:s' +
    '  end;' + sLineBreak +
    sLineBreak +
    'implementation' + sLineBreak + sLineBreak +
    'uses' + sLineBreak +
    '  System.SysUtils, MVCFramework.Logger, System.StrUtils;' + sLineBreak +
    sLineBreak +
    '%3:s' + sLineBreak +
    '%5:s' + sLineBreak +
    '%7:s' + sLineBreak +
    sLineBreak +
    '%9:s' + sLineBreak +
    'end.' + sLineBreak;

  sIndexMethodIntf =
    '    [MVCPath]' + sLineBreak +
    '    [MVCHTTPMethod([httpGET])]' + sLineBreak +
    '    procedure Index;' + sLineBreak + sLineBreak +
    '    [MVCPath(''/reversedstrings/($Value)'')]' + sLineBreak +
    '    [MVCHTTPMethod([httpGET])]' + sLineBreak +
    '    [MVCProduces(TMVCMediaType.TEXT_PLAIN)]' + sLineBreak +
    '    procedure GetReversedString(const Value: String);' + sLineBreak;

  // 0 - Class Name
  sIndexMethodImpl =
    'procedure %0:s.Index;' + sLineBreak +
    'begin' + sLineBreak +
    '  //use Context property to access to the HTTP request and response ' + sLineBreak +
    '  Render(''Hello DelphiMVCFramework World'');' + sLineBreak +
    'end;' + sLineBreak + sLineBreak +
    'procedure %0:s.GetReversedString(const Value: String);' + sLineBreak +
    'begin' + sLineBreak +
    '  Render(System.StrUtils.ReverseString(Value.Trim));' + sLineBreak +
    'end;' + sLineBreak;

  sCRUDMethodsIntf =
    sLineBreak +
		'  public' + sLineBreak + 
    '    //Sample CRUD Actions for a "People" entity' + sLineBreak +
    '    [MVCPath(''/people'')]' + sLineBreak +
    '    [MVCHTTPMethod([httpGET])]' + sLineBreak +
    '    function GetPeople: TObjectList<TPerson>;' + sLineBreak + sLineBreak +
    '    [MVCPath(''/people/($ID)'')]' + sLineBreak +
    '    [MVCHTTPMethod([httpGET])]' + sLineBreak +
    '    function GetPerson(ID: Integer): TPerson;' + sLineBreak + sLineBreak +
    '    [MVCPath(''/people'')]' + sLineBreak +
    '    [MVCHTTPMethod([httpPOST])]' + sLineBreak +
    '    function CreatePerson([MVCFromBody] Person: TPerson): TMVCResponse;' + sLineBreak + sLineBreak +
    '    [MVCPath(''/people/($ID)'')]' + sLineBreak +
    '    [MVCHTTPMethod([httpPUT])]' + sLineBreak +
    '    function UpdatePerson(ID: Integer; [MVCFromBody] Person: TPerson): TMVCResponse;' + sLineBreak + sLineBreak +
    '    [MVCPath(''/people/($ID)'')]' + sLineBreak +
    '    [MVCHTTPMethod([httpDELETE])]' + sLineBreak +
    '    function DeletePerson(ID: Integer): TMVCResponse;' + sLineBreak + sLineBreak;

  sCRUDMethodsImpl =
    '//Sample CRUD Actions for a "People" entity' + sLineBreak +
    'function %0:s.GetPeople: TObjectList<TPerson>;' + sLineBreak +
    'var' + sLineBreak +
    '  lPeople: TObjectList<TPerson>;' + sLineBreak +
    'begin' + sLineBreak +
    '  lPeople := TObjectList<TPerson>.Create(True);' + sLineBreak +
    '  try' + sLineBreak +
    '    lPeople.Add(TPerson.Create(''Peter'',''Parker'', EncodeDate(1965, 10, 4)));' + sLineBreak +
    '    lPeople.Add(TPerson.Create(''Bruce'',''Banner'', EncodeDate(1945, 9, 6)));' + sLineBreak +
    '    lPeople.Add(TPerson.Create(''Reed'',''Richards'', EncodeDate(1955, 3, 7)));' + sLineBreak +
    '    Result := lPeople;' + sLineBreak +
    '  except' + sLineBreak +
    '    lPeople.Free;' + sLineBreak +
    '    raise;' + sLineBreak +
    '  end;' + sLineBreak +
    'end;' + sLineBreak + sLineBreak +
    'function %0:s.GetPerson(ID: Integer): TPerson;' + sLineBreak +
    'var' + sLineBreak +
    '  lPeople: TObjectList<TPerson>;' + sLineBreak +
    'begin' + sLineBreak +
    '  lPeople := GetPeople;' + sLineBreak +
    '  try' + sLineBreak +
    '    Result := lPeople.ExtractAt(ID mod lPeople.Count);' + sLineBreak +
    '  finally' + sLineBreak +
    '    lPeople.Free;' + sLineBreak +
    '  end;' + sLineBreak +
    'end;' + sLineBreak + sLineBreak +
    'function %0:s.CreatePerson([MVCFromBody] Person: TPerson): TMVCResponse;' + sLineBreak + 
    'begin' + sLineBreak +
	'  LogI(''Created '' + Person.FirstName + '' '' + Person.LastName);' + sLineBreak +
    '  Result := TMVCResponse.Create(HTTP_STATUS.Created, ''Person created'');' + sLineBreak +
    'end;' + sLineBreak + sLineBreak +
    'function %0:s.UpdatePerson(ID: Integer; [MVCFromBody] Person: TPerson): TMVCResponse;' + sLineBreak +
    'begin' + sLineBreak +
    '  LogI(''Updated '' + Person.FirstName + '' '' + Person.LastName);' + sLineBreak +
    '  Result := TMVCResponse.Create(HTTP_STATUS.OK, ''Person updated'');' + sLineBreak +
    'end;' + sLineBreak + sLineBreak +
    'function %0:s.DeletePerson(ID: Integer): TMVCResponse;' + sLineBreak +
    'begin' + sLineBreak +
    '  LogI(''Deleted person with id '' + ID.ToString);' + sLineBreak +
    '  Result := TMVCResponse.Create(HTTP_STATUS.OK, ''Person deleted'');' + sLineBreak +
    'end;' + sLineBreak;

  sActionFiltersIntf =
    '  protected' + sLineBreak +
    '    procedure OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean); override;'
    + sLineBreak +
    '    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;' +
    sLineBreak;

  sActionFiltersImpl =
    'procedure %0:s.OnAfterAction(Context: TWebContext; const AActionName: string); ' + sLineBreak +
    'begin' + sLineBreak +
    '  { Executed after each action }' + sLineBreak +
    '  inherited;' + sLineBreak +
    'end;' + sLineBreak +
    sLineBreak +
    'procedure %0:s.OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean);'
    + sLineBreak +
    'begin' + sLineBreak +
    '  { Executed before each action' + sLineBreak +
    '    if handled is true (or an exception is raised) the actual' + sLineBreak +
    '    action will not be called }' + sLineBreak +
    '  inherited;' + sLineBreak +
    'end;' + sLineBreak;

  sBOClassesIntf =    
     '  [MVCNameCase(ncCamelCase)]' + sLineBreak +
     '  TPerson = class' + sLineBreak +
     '  private' + sLineBreak +
     '    fFirstName: String;' + sLineBreak +
     '    fLastName: String;' + sLineBreak +
     '    fDOB: TDate;' + sLineBreak +
     '  public' + sLineBreak +
     '    property FirstName: String read fFirstName write fFirstName;' + sLineBreak +
     '    property LastName: String read fLastName write fLastName;' + sLineBreak +
     '    property DOB: TDate read fDOB write fDOB;  ' + sLineBreak +
     '    constructor Create(FirstName, LastName: String; DOB: TDate);' + sLineBreak +
     '  end;' + sLineBreak;

  sBOClassesImpl =
    sLineBreak +
	'constructor %0:s.Create(FirstName, LastName: String; DOB: TDate);' + sLineBreak +
	'begin' + sLineBreak +
	'  inherited Create;' + sLineBreak +
	'  fFirstName := FirstName;' + sLineBreak +
	'  fLastName := LastName;' + sLineBreak +
	'  fDOB := DOB;' + sLineBreak +
	'end;' + sLineBreak;

  sDefaultControllerName = 'TMyController';
  sDefaultWebModuleName = 'TMyWebModule';
  sDefaultServerPort = '8080';

  // 0 = unit name
  // 1 = webmodule classname
  // 2 = controller unit
  // 3 - controller class name
  // 4 - middlewares
  // 5 - jsonrpc registration code
  sWebModuleUnit =
    'unit %0:s;' + sLineBreak +
    '' + sLineBreak +
    'interface' + sLineBreak +
    sLineBreak +
    'uses ' + sLineBreak + 
   	'  System.SysUtils,' + sLineBreak +
    '  System.Classes,' + sLineBreak +
    '  Web.HTTPApp,' + sLineBreak +
    '  MVCFramework;' + sLineBreak +
    sLineBreak +
    'type' + sLineBreak +
    '  %1:s = class(TWebModule)' + sLineBreak +
    '    procedure WebModuleCreate(Sender: TObject);' + sLineBreak +
    '    procedure WebModuleDestroy(Sender: TObject);' + sLineBreak +
    '  private' + sLineBreak +
    '    FMVC: TMVCEngine;' + sLineBreak +
    '  public' + sLineBreak +
    '    { Public declarations }' + sLineBreak +
    '  end;' + sLineBreak +
    sLineBreak +
    'var' + sLineBreak +
    '  WebModuleClass: TComponentClass = %1:s;' + sLineBreak +
    sLineBreak +
    'implementation' + sLineBreak +
    sLineBreak +
    '{$R *.dfm}' + sLineBreak +
    sLineBreak +
    'uses ' + sLineBreak +
  	'  %2:s, ' + sLineBreak +
	  '  %6:s ' + sLineBreak +
	  '  System.IOUtils, ' + sLineBreak +
    '  MVCFramework.Commons, ' + sLineBreak +
  '  MVCFramework.Middleware.ActiveRecord, ' + sLineBreak +
	'  MVCFramework.Middleware.StaticFiles, ' + sLineBreak +
	'  MVCFramework.Middleware.Analytics, ' + sLineBreak +
  '  MVCFramework.Middleware.Trace, ' + sLineBreak +
  '  MVCFramework.Middleware.CORS, ' + sLineBreak +
  '  MVCFramework.Middleware.ETag, ' + sLineBreak +
	'  MVCFramework.Middleware.Compression;' + sLineBreak +
    sLineBreak +
    'procedure %1:s.WebModuleCreate(Sender: TObject);' + sLineBreak +
    'begin' + sLineBreak +
    '  FMVC := TMVCEngine.Create(Self,' + sLineBreak +
    '    procedure(Config: TMVCConfig)' + sLineBreak +
    '    begin' + sLineBreak +
	'      Config.dotEnv := dotEnv; ' + sLineBreak +
    '      // session timeout (0 means session cookie)' + sLineBreak +
    '      Config[TMVCConfigKey.SessionTimeout] := dotEnv.Env(''dmvc.session_timeout'', ''0'');' + sLineBreak +
    '      //default content-type' + sLineBreak +
    '      Config[TMVCConfigKey.DefaultContentType] := dotEnv.Env(''dmvc.default.content_type'', TMVCConstants.DEFAULT_CONTENT_TYPE);' +
    sLineBreak +
    '      //default content charset' + sLineBreak +
    '      Config[TMVCConfigKey.DefaultContentCharset] := dotEnv.Env(''dmvc.default.content_charset'', TMVCConstants.DEFAULT_CONTENT_CHARSET);' +
    sLineBreak +
    '      //unhandled actions are permitted?' + sLineBreak +
    '      Config[TMVCConfigKey.AllowUnhandledAction] := dotEnv.Env(''dmvc.allow_unhandled_actions'', ''false'');' + sLineBreak +
    '      //enables or not system controllers loading (available only from localhost requests)' + sLineBreak +
    '      Config[TMVCConfigKey.LoadSystemControllers] := dotEnv.Env(''dmvc.load_system_controllers'', ''true'');' + sLineBreak +
    '      //default view file extension' + sLineBreak +
    '      Config[TMVCConfigKey.DefaultViewFileExtension] := dotEnv.Env(''dmvc.default.view_file_extension'', ''html'');' + sLineBreak +
    '      //view path' + sLineBreak +
    '      Config[TMVCConfigKey.ViewPath] := dotEnv.Env(''dmvc.view_path'', ''templates'');' + sLineBreak +
    '      //Max Record Count for automatic Entities CRUD' + sLineBreak +
    '      Config[TMVCConfigKey.MaxEntitiesRecordCount] := dotEnv.Env(''dmvc.max_entities_record_count'', IntToStr(TMVCConstants.MAX_RECORD_COUNT));' + sLineBreak +
	  '      //Enable Server Signature in response' + sLineBreak +
    '      Config[TMVCConfigKey.ExposeServerSignature] := dotEnv.Env(''dmvc.expose_server_signature'', ''false'');' + sLineBreak +
  	'      //Enable X-Powered-By Header in response' + sLineBreak +
    '      Config[TMVCConfigKey.ExposeXPoweredBy] := dotEnv.Env(''dmvc.expose_x_powered_by'', ''true'');' + sLineBreak +
    '      // Max request size in bytes' + sLineBreak +
    '      Config[TMVCConfigKey.MaxRequestSize] := dotEnv.Env(''dmvc.max_request_size'', IntToStr(TMVCConstants.DEFAULT_MAX_REQUEST_SIZE));' + sLineBreak +
    '    end);' + sLineBreak +
    '  FMVC.AddController(%3:s);' + sLineBreak + sLineBreak +    
    '  %4:s ' + sLineBreak +
    '  %5:s ' + sLineBreak +
    '  ' + sLineBreak +
    '  {' + sLineBreak +
    '  FMVC.OnWebContextCreate( ' + sLineBreak +
    '    procedure(const Context: TWebContext) ' + sLineBreak +
    '    begin ' + sLineBreak +
    '      // Initialize services to make them accessibile from Context ' + sLineBreak +
    '      // Context.CustomIntfObject := TMyService.Create; ' + sLineBreak +
    '    end); ' + sLineBreak +
    '  ' + sLineBreak +
    '  FMVC.OnWebContextDestroy(' + sLineBreak +
    '    procedure(const Context: TWebContext)' + sLineBreak +
    '    begin' + sLineBreak +
    '      //Cleanup services, if needed' + sLineBreak +
    '    end);' + sLineBreak +
    '  }' + sLineBreak +
    'end;' + sLineBreak +
    sLineBreak +
    'procedure %1:s.WebModuleDestroy(Sender: TObject);' + sLineBreak +
    'begin' + sLineBreak +
    '  FMVC.Free;' + sLineBreak +
    'end;' + sLineBreak +
    sLineBreak +
    'end.' + sLineBreak;

  sWebModuleDFM =
    'object %0:s: %1:s' + sLineBreak +
    '  OldCreateOrder = False' + sLineBreak +
    '  OnCreate = WebModuleCreate' + sLineBreak +
    '  OnDestroy = WebModuleDestroy' + sLineBreak +
    '  Height = 230' + sLineBreak +
    '  Width = 415' + sLineBreak +
    'end';


  //0 - unit name
  //1 - class name
  sJSONRPCUnit =
    'unit %0:s; ' + sLineBreak + sLineBreak +
    'interface' + sLineBreak + sLineBreak +
    'type ' + sLineBreak +
    '  %1:s = class' + sLineBreak +
    '  public' + sLineBreak +
    '    function ReverseString(const Value: String): String;' + sLineBreak +
    '  end;' + sLineBreak + sLineBreak +
    'implementation' + sLineBreak + sLineBreak +
    'uses System.StrUtils;' + sLineBreak + sLineBreak +
    'function %1:s.ReverseString(const Value: String): String;' + sLineBreak +
    'begin' + sLineBreak +
    '  Result := System.StrUtils.ReverseString(Value);' + sLineBreak +
    'end;' + sLineBreak + sLineBreak +
    'end.' + sLineBreak;

implementation

end.
