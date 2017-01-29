// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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

resourcestring

  { Delphi template code }
  // 0 - project name
  // 1 - http/s port
  sDMVCDPR = 'program %0:s;' + sLineBreak +
    sLineBreak +
    ' {$APPTYPE CONSOLE}' + sLineBreak +
    '' + sLineBreak +
    'uses' + sLineBreak +
    '  System.SysUtils,' + sLineBreak +
    '  MVCFramework.Logger,' + sLineBreak +
    '  MVCFramework.Commons,' + sLineBreak +
    '  Winapi.Windows,' + sLineBreak +
    '  Winapi.ShellAPI,' + sLineBreak +
    '  ReqMulti, {enables files upload}' + sLineBreak +
    '  Web.WebReq,' + sLineBreak +
    '  Web.WebBroker,' + sLineBreak +
    '  IdHTTPWebBrokerBridge;' + sLineBreak +
    '' + sLineBreak +
    '{$R *.res}' + sLineBreak +
    sLineBreak +
    'procedure RunServer(APort: Integer);' + sLineBreak +
    'var' + sLineBreak +
    '  LInputRecord: TInputRecord;' + sLineBreak +
    '  LEvent: DWord;' + sLineBreak +
    '  LHandle: THandle;' + sLineBreak +
    '  LServer: TIdHTTPWebBrokerBridge;' + sLineBreak +
    'begin' + sLineBreak +
    '  Writeln(''** DMVCFramework Server ** build '' + DMVCFRAMEWORK_VERSION);' + sLineBreak +
    '  Writeln(Format(''Starting HTTP Server on port %%d'', [APort]));' + sLineBreak +
    '  LServer := TIdHTTPWebBrokerBridge.Create(nil);' + sLineBreak +
    '  try' + sLineBreak +
    '    LServer.DefaultPort := APort;' + sLineBreak +
    '    LServer.Active := True;' + sLineBreak +
    '    LogI(Format(''Server started on port %d'', [APort]));' + sLineBreak +
    '    { more info about MaxConnections ' + sLineBreak +
    '      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_MaxConnections.html}'
    + sLineBreak +
    '    LServer.MaxConnections := 0;' + sLineBreak +
    '    { more info about ListenQueue ' + sLineBreak +
    '      http://www.indyproject.org/docsite/html/frames.html?frmname=topic&frmfile=TIdCustomTCPServer_ListenQueue.html}'
    + sLineBreak +
    '    LServer.ListenQueue := 200;' + sLineBreak +
    '    { Comment the next line to avoid the default browser startup }' + sLineBreak +
    '    ShellExecute(0, ''open'', PChar(''http://localhost:'' + inttostr(APort)), nil, nil, SW_SHOWMAXIMIZED);'
    + sLineBreak +
    '    Writeln(''Press ESC to stop the server'');' + sLineBreak +
    '    LHandle := GetStdHandle(STD_INPUT_HANDLE);' + sLineBreak +
    '    while True do' + sLineBreak +
    '    begin' + sLineBreak +
    '      Win32Check(ReadConsoleInput(LHandle, LInputRecord, 1, LEvent));' + sLineBreak +
    '      if (LInputRecord.EventType = KEY_EVENT) and' + sLineBreak +
    '        LInputRecord.Event.KeyEvent.bKeyDown and' + sLineBreak +
    '        (LInputRecord.Event.KeyEvent.wVirtualKeyCode = VK_ESCAPE) then' + sLineBreak +
    '        break;' + sLineBreak +
    '    end;' + sLineBreak +
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
    '  MVCFramework, MVCFramework.Commons;' + sLineBreak +
    sLineBreak +
    'type' + sLineBreak +
    sLineBreak +
    '  [MVCPath(''/'')]' + sLineBreak +
    '  %1:s = class(TMVCController) ' + sLineBreak +
    '  public' + sLineBreak +
    '%2:s' +
    '%4:s' +
    '  end;' + sLineBreak +
    sLineBreak +
    'implementation' + sLineBreak + sLineBreak +
    'uses' + sLineBreak +
    '  MVCFramework.Logger;' + sLineBreak +
    sLineBreak +
    '%3:s' + sLineBreak +
    '%5:s' + sLineBreak +
    sLineBreak +
    'end.' + sLineBreak;

  sIndexMethodIntf =
    '    [MVCPath(''/'')]' + sLineBreak +
    '    [MVCHTTPMethod([httpGET])]' + sLineBreak +
    '    procedure Index;' + sLineBreak + sLineBreak +
    '    [MVCPath(''/hellos/($FirstName)'')]' + sLineBreak +
    '    [MVCHTTPMethod([httpGET])]' + sLineBreak +
    '    procedure GetSpecializedHello(const FirstName: String);' + sLineBreak;

  // 0 - Class Name
  sIndexMethodImpl =
    'procedure %0:s.Index;' + sLineBreak +
    'begin' + sLineBreak +
    '  //use Context property to access to the HTTP request and response ' + sLineBreak +
    '  Render(''Hello DelphiMVCFramework World'');' + sLineBreak +
    'end;' + sLineBreak + sLineBreak +
    'procedure %0:s.GetSpecializedHello(const FirstName: String);' + sLineBreak +
    'begin' + sLineBreak +
    '  Render(''Hello '' + FirstName);' + sLineBreak +
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

  sDefaultControllerName = 'TMyController';
  sDefaultWebModuleName = 'TMyWebModule';
  sDefaultServerPort = '8080';

  // 0 = unit name
  // 1 = webmodule classname
  // 2 = controller unit
  // 3 - controller class name
  sWebModuleUnit =
    'unit %0:s;' + sLineBreak +
    '' + sLineBreak +
    'interface' + sLineBreak +
    sLineBreak +
    'uses System.SysUtils,' + sLineBreak +
    '     System.Classes,' + sLineBreak +
    '     Web.HTTPApp,' + sLineBreak +
    '     MVCFramework;' + sLineBreak +
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
    'uses %2:s, MVCFramework.Commons;' + sLineBreak +
    sLineBreak +
    'procedure %1:s.WebModuleCreate(Sender: TObject);' + sLineBreak +
    'begin' + sLineBreak +
    '  FMVC := TMVCEngine.Create(Self,' + sLineBreak +
    '    procedure(Config: TMVCConfig)' + sLineBreak +
    '    begin' + sLineBreak +
    '      //enable static files' + sLineBreak +
    '      Config[TMVCConfigKey.DocumentRoot] := ExtractFilePath(GetModuleName(HInstance)) + ''\www'';'
    + sLineBreak +
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
    '      //default view file extension' + sLineBreak +
    '      Config[TMVCConfigKey.DefaultViewFileExtension] := ''html'';' + sLineBreak +
    '      //view path' + sLineBreak +
    '      Config[TMVCConfigKey.ViewPath] := ''templates'';' + sLineBreak +
    '      //Enable STOMP messaging controller' + sLineBreak +
    '      Config[TMVCConfigKey.Messaging] := ''false'';' + sLineBreak +
    '      //Enable Server Signature in response' + sLineBreak +
    '      Config[TMVCConfigKey.ExposeServerSignature] := ''true'';' + sLineBreak +
    '      // Define a default URL for requests that don''t map to a route or a file (useful for client side web app)' +
    sLineBreak +
    '      Config[TMVCConfigKey.FallbackResource] := ''index.html'';' + sLineBreak +
    '    end);' + sLineBreak +
    '  FMVC.AddController(%3:s);' + sLineBreak +
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

implementation

end.
