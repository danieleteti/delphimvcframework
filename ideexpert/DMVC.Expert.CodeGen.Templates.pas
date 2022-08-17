// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2022 Daniele Teti and the DMVCFramework Team
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
    '  MVCFramework.Commons,' + sLineBreak +
    '  MVCFramework.Signal,' + sLineBreak +
//    '  MVCFramework.REPLCommandsHandlerU,' + sLineBreak +
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
//    '  LCustomHandler: TMVCCustomREPLCommandsHandler;' + sLineBreak +
//    '  LCmd: string;' + sLineBreak +
    'begin' + sLineBreak +
    '  Writeln(''** DMVCFramework Server ** build '' + DMVCFRAMEWORK_VERSION);' + sLineBreak +
    '  LServer := TIdHTTPWebBrokerBridge.Create(nil);' + sLineBreak +
    '  try' + sLineBreak +
    '    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;' + sLineBreak +
    '    LServer.DefaultPort := APort;' + sLineBreak +
    '    LServer.KeepAlive := True;' + sLineBreak + sLineBreak +
    '    { more info about MaxConnections' + sLineBreak +
    '      http://ww2.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=index.html }' + sLineBreak +
    '    LServer.MaxConnections := 0;' + sLineBreak +
    sLineBreak +
    '    { more info about ListenQueue' + sLineBreak +
    '      http://ww2.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=index.html }' + sLineBreak +
    '    LServer.ListenQueue := 200;' + sLineBreak +
    sLineBreak +
//    '    WriteLn(''Write "quit" or "exit" to shutdown the server'');' + sLineBreak +
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
    '  ReportMemoryLeaksOnShutdown := True;' + sLineBreak +
    '  IsMultiThread := True;' + sLineBreak +
    '  try' + sLineBreak +
    '    if WebRequestHandler <> nil then' + sLineBreak +
    '      WebRequestHandler.WebModuleClass := WebModuleClass;' + sLineBreak +
    '    WebRequestHandlerProc.MaxConnections := 1024;' + sLineBreak +
    '    RunServer(%1:d);' + sLineBreak +
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
  sControllerUnit = 'unit %0:s;' + sLineBreak +
    sLineBreak +
    'interface' + sLineBreak +
    sLineBreak +
    'uses' + sLineBreak +
    '  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons;' + sLineBreak +
    sLineBreak +
    'type' + sLineBreak +
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
    'end.' + sLineBreak;

  sIndexMethodIntf =
    '    [MVCPath]' + sLineBreak +
    '    [MVCHTTPMethod([httpGET])]' + sLineBreak +
    '    procedure Index;' + sLineBreak + sLineBreak +
    '    [MVCPath(''/reversedstrings/($Value)'')]' + sLineBreak +
    '    [MVCHTTPMethod([httpGET])]' + sLineBreak +
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
    '    //Sample CRUD Actions for a "Customer" entity' + sLineBreak +
    '    [MVCPath(''/customers'')]' + sLineBreak +
    '    [MVCHTTPMethod([httpGET])]' + sLineBreak +
    '    procedure GetCustomers;' + sLineBreak + sLineBreak +
    '    [MVCPath(''/customers/($id)'')]' + sLineBreak +
    '    [MVCHTTPMethod([httpGET])]' + sLineBreak +
    '    procedure GetCustomer(id: Integer);' + sLineBreak + sLineBreak +
    '    [MVCPath(''/customers'')]' + sLineBreak +
    '    [MVCHTTPMethod([httpPOST])]' + sLineBreak +
    '    procedure CreateCustomer;' + sLineBreak + sLineBreak +
    '    [MVCPath(''/customers/($id)'')]' + sLineBreak +
    '    [MVCHTTPMethod([httpPUT])]' + sLineBreak +
    '    procedure UpdateCustomer(id: Integer);' + sLineBreak + sLineBreak +
    '    [MVCPath(''/customers/($id)'')]' + sLineBreak +
    '    [MVCHTTPMethod([httpDELETE])]' + sLineBreak +
    '    procedure DeleteCustomer(id: Integer);' + sLineBreak + sLineBreak;

  sCRUDMethodsImpl =
    '//Sample CRUD Actions for a "Customer" entity' + sLineBreak +
    'procedure %0:s.GetCustomers;' + sLineBreak +
    'begin' + sLineBreak +
    '  //todo: render a list of customers' + sLineBreak +
    'end;' + sLineBreak + sLineBreak +
    'procedure %0:s.GetCustomer(id: Integer);' + sLineBreak +
    'begin' + sLineBreak +
    '  //todo: render the customer by id' + sLineBreak +
    'end;' + sLineBreak + sLineBreak +
    'procedure %0:s.CreateCustomer;' + sLineBreak + 
    'begin' + sLineBreak +
    '  //todo: create a new customer' + sLineBreak +
    'end;' + sLineBreak + sLineBreak +
    'procedure %0:s.UpdateCustomer(id: Integer);' + sLineBreak +
    'begin' + sLineBreak +
    '  //todo: update customer by id' + sLineBreak +
    'end;' + sLineBreak + sLineBreak +
    'procedure %0:s.DeleteCustomer(id: Integer);' + sLineBreak +
    'begin' + sLineBreak +
    '  //todo: delete customer by id' + sLineBreak +
    'end;' + sLineBreak + sLineBreak;

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
    '      // session timeout (0 means session cookie)' + sLineBreak +
    '      Config[TMVCConfigKey.SessionTimeout] := ''0'';' + sLineBreak +
    '      //default content-type' + sLineBreak +
    '      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;' +
    sLineBreak +
    '      //default content charset' + sLineBreak +
    '      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;' +
    sLineBreak +
    '      //unhandled actions are permitted?' + sLineBreak +
    '      Config[TMVCConfigKey.AllowUnhandledAction] := ''false'';' + sLineBreak +
    '      //enables or not system controllers loading (available only from localhost requests)' + sLineBreak +
    '      Config[TMVCConfigKey.LoadSystemControllers] := ''true'';' + sLineBreak +
    '      //default view file extension' + sLineBreak +
    '      Config[TMVCConfigKey.DefaultViewFileExtension] := ''html'';' + sLineBreak +
    '      //view path' + sLineBreak +
    '      Config[TMVCConfigKey.ViewPath] := ''templates'';' + sLineBreak +
    '      //Max Record Count for automatic Entities CRUD' + sLineBreak +
    '      Config[TMVCConfigKey.MaxEntitiesRecordCount] := ''20'';' + sLineBreak +   
	'      //Enable Server Signature in response' + sLineBreak +
    '      Config[TMVCConfigKey.ExposeServerSignature] := ''true'';' + sLineBreak +
	'      //Enable X-Powered-By Header in response' + sLineBreak +
    '      Config[TMVCConfigKey.ExposeXPoweredBy] := ''true'';' + sLineBreak +	
//    '      // Define a default URL for requests that don''t map to a route or a file (useful for client side web app)' + sLineBreak +
//    '      Config[TMVCConfigKey.FallbackResource] := ''index.html'';' + sLineBreak +
    '      // Max request size in bytes' + sLineBreak +
    '      Config[TMVCConfigKey.MaxRequestSize] := IntToStr(TMVCConstants.DEFAULT_MAX_REQUEST_SIZE);' + sLineBreak +	
    '    end);' + sLineBreak +
    '  FMVC.AddController(%3:s);' + sLineBreak + sLineBreak +    
    '  %4:s ' + sLineBreak +
    '  %5:s ' + sLineBreak +
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
