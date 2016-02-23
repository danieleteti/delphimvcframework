{***************************************************************************}
{                                                                           }
{                      Delphi MVC Framework                                 }
{                                                                           }
{     Copyright (c) 2010-2015 Daniele Teti and the DMVCFramework Team       }
{                                                                           }
{           https://github.com/danieleteti/delphimvcframework               }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{  This IDE expert is based off of the one included with the DUnitX         }
{  project.  Original source by Robert Love.  Adapted by Nick Hodges.       }
{                                                                           }
{  The DUnitX project is run by Vincent Parrett and can be found at:        }
{                                                                           }
{            https://github.com/VSoftTechnologies/DUnitX                    }
{***************************************************************************}

unit DMVC.Expert.CodeGen.Templates;

interface

resourcestring

 { Delphi template code }

 sDMVCDPR = 'program %0:s;'#13#10 +
#13#10 +
' {$APPTYPE CONSOLE}'#13#10  +
''#13#10 +
'uses'#13#10 +
'  System.SysUtils,'#13#10 +
'  Winapi.Windows,'#13#10 +
'  Winapi.ShellAPI,'#13#10 +
'  Web.WebReq,'#13#10 +
'  Web.WebBroker,'#13#10 +
'  IdHTTPWebBrokerBridge;'#13#10 +
''#13#10 +
'{$R *.res}'#13#10 +
#13#10 +
'procedure RunServer(APort: Integer);'#13#10 +
'var'#13#10 +
'  LInputRecord: TInputRecord;'#13#10 +
'  LEvent: DWord;'#13#10 +
'  LHandle: THandle;'#13#10 +
'  LServer: TIdHTTPWebBrokerBridge;'#13#10 +
'begin'#13#10 +
'  Writeln(Format(''Starting HTTP Server or port %%d'', [APort]));'#13#10 +
'  LServer := TIdHTTPWebBrokerBridge.Create(nil);'#13#10 +
'  try'#13#10 +
'    LServer.DefaultPort := APort;'#13#10 +
'    LServer.Active := True;'#13#10 +
'    ShellExecute(0, ''open'', pChar(''http://localhost:'' + inttostr(APort)), nil, nil, SW_SHOWMAXIMIZED);'#13#10 +
'    Writeln(''Press ESC to stop the server'');'#13#10 +
'    LHandle := GetStdHandle(STD_INPUT_HANDLE);'#13#10 +
'    while True do'#13#10 +
'    begin'#13#10 +
'      Win32Check(ReadConsoleInput(LHandle, LInputRecord, 1, LEvent));'#13#10 +
'      if (LInputRecord.EventType = KEY_EVENT) and'#13#10 +
'        LInputRecord.Event.KeyEvent.bKeyDown and'#13#10 +
'        (LInputRecord.Event.KeyEvent.wVirtualKeyCode = VK_ESCAPE) then'#13#10 +
'        break;'#13#10 +
'    end;'#13#10 +
'  finally'#13#10 +
'    LServer.Free;'#13#10 +
'  end;'#13#10 +
'end;'#13#10 +
#13#10 +
'begin'#13#10 +
'  ReportMemoryLeaksOnShutdown := True;'#13#10 +
'  try'#13#10 +
'    if WebRequestHandler <> nil then'#13#10 +
'      WebRequestHandler.WebModuleClass := WebModuleClass;'#13#10 +
'    WebRequestHandlerProc.MaxConnections := 1024;'#13#10 +
'    RunServer(3000);'#13#10 +
'  except'#13#10 +
'    on E: Exception do'#13#10 +
'      Writeln(E.ClassName, '': '', E.Message);'#13#10 +
'  end;'#13#10 +
'end.'#13#10;


 // 0 - Unit Name
 // 1 - Class Name
 // 2 - Index Method - Interface
 // 3 - Index Method - Implementation
 sControllerUnit = 'unit %0:s;'#13#10 +
 #13#10 +
 'interface'#13#10 +
 #13#10 +
 'uses'#13#10 +
 '  MVCFramework;'#13#10 +
 #13#10 +
 'type'#13#10 +
 #13#10 +
 '  %1:s = class(TMVCController) '#13#10 +
 '  public'#13#10 +
 '%2:s' +
 '  end;'#13#10 +
 #13#10 +
 'implementation'#13#10 +
 #13#10 +
 '%3:s'#13#10 +
 #13#10 +
 'end.'#13#10;

 sIndexMethodIntf =
'    [MVCPath(''/'')]'#13#10 +
'    [MVCHTTPMethod([httpGET])]'#13#10 +
'    procedure Index(ctx: TWebContext);'#13#10;

 // 0 - Class Name
 sIndexMethodImpl =
 'procedure %0:s.Index(ctx: TWebContext);'#13#10 +
 'begin'#13#10 +
 #13#10 +
 'end;';


 sDefaultControllerName = 'TMyController';
 sDefaultWebModuleName = 'TMyWebModule';


 // 0 = unit name
 // 1 = webmodule classname
 // 2 = controller unit
 // 3 - controller class name
 sWebModuleUnit =
'unit %0:s;'#13#10 +
''#13#10 +
'interface'#13#10 +
#13#10 +
'uses System.SysUtils,'#13#10 +
'     System.Classes,'#13#10 +
'     Web.HTTPApp,'#13#10 +
'     MVCFramework;'#13#10 +
#13#10 +
'type'#13#10 +
'  %1:s = class(TWebModule)'#13#10 +
'    procedure WebModuleCreate(Sender: TObject);'#13#10 +
'    procedure WebModuleDestroy(Sender: TObject);'#13#10 +
'  private'#13#10 +
'    MVC: TMVCEngine;'#13#10 +
'  public'#13#10 +
'    { Public declarations }'#13#10 +
'  end;'#13#10 +
#13#10 +
'var'#13#10 +
'  WebModuleClass: TComponentClass = %1:s;'#13#10 +
#13#10 +
'implementation'#13#10 +
#13#10 +
'{$R *.dfm}'#13#10 +
#13#10 +
'uses %2:s;'#13#10 +
#13#10 +
'procedure %1:s.WebModuleCreate(Sender: TObject);'#13#10 +
'begin'#13#10 +
'  MVC := TMVCEngine.Create(Self);'#13#10 +
'  MVC.AddController(%3:s);'#13#10 +
'end;'#13#10 +
#13#10 +
'procedure %1:s.WebModuleDestroy(Sender: TObject);'#13#10 +
'begin'#13#10 +
'  MVC.Free;'#13#10 +
'end;'#13#10 +
#13#10 +
'end.'#13#10;


sWebModuleDFM =
'object %0:s: %1:s'#13#10 +
'  OldCreateOrder = False'#13#10 +
'  Height = 230'#13#10 +
'  Width = 415'#13#10 +
'end';

implementation

end.
