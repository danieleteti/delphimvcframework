// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
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
// ***************************************************************************

unit DMVC.Expert.CodeGen.Commands;

interface

uses
  System.SysUtils, MVCFramework.Commons, System.DateUtils,
  JsonDataObjects, DMVC.Expert.Commons;

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

  TUnitServicesDeclarationCommand = class(TCustomCommand)
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

  TUnitMustacheHelpersDeclarationCommand = class(TCustomCommand)
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

  TUnitTemplateProHelpersDeclarationCommand = class(TCustomCommand)
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

  TUnitWebStencilsHelpersDeclarationCommand = class(TCustomCommand)
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
  private
    function GetScrambledAlphabet: String;
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
    .AppendLine('// project.  Original source by Robert Love.  Adapted by Nick Hodges and Daniele Teti.')
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
    Section.AppendLine('  {$IF Defined(MSWINDOWS)}MSHeap,{$ENDIF}');
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
    .AppendLine('  MVCFramework.Serializer.Commons,');

  if Model.B[TConfigKey.program_ssv_mustache] then
  begin
    {The Mustache units are required in the "program" because the mustache helpers are declared there.
     Should we create an external unit as we do in templatepro and webstencils }
    Section
      .AppendLine('  MVCFramework.View.Renderers.Mustache,')
      .AppendLine('  mormot.core.mustache,');
  end;
  if Model.B[TConfigKey.program_service_container_generate] then
  begin
    Section
      .AppendLine('  MVCFramework.Container,')
  end;
  Section
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
    .Append('  MVCFramework, MVCFramework.Commons, ');
  if Model.B[TConfigKey.entity_generate] then
  begin
    Section
      .Append('MVCFramework.Nullables, ')
      .Append(Model[TConfigKey.entity_unit_name] + ', ');
  end;
  if Model.B[TConfigKey.program_service_container_generate] then
  begin
    Section
      .Append(Model[TConfigKey.program_service_container_unit_name] + ', ');
  end;
  Section
    .AppendLine('MVCFramework.Serializer.Commons, System.Generics.Collections;')
    .AppendLine
    .AppendLine('type')
end;

procedure TUnitControllerEntityDeclarationCommand.ExecuteImplementation(
  Section: TStringBuilder; Model: TJsonObject);
begin
  inherited;
  if not Model.B[TConfigKey.entity_generate] then Exit;
  Section
    .AppendLine('implementation')
    .AppendLine
    .AppendLine('constructor ' + Model[TConfigKey.entity_classname] + '.Create(ID: Integer; FirstName, LastName: String; DOB: TDate);')
    .AppendLine('begin')
    .AppendLine('  inherited Create;')
    .AppendLine('  fID := ID;')
    .AppendLine('  fFirstName := FirstName;')
    .AppendLine('  fLastName := LastName;')
    .AppendLine('  fDOB := DOB;')
    .AppendLine('end;')
    .AppendLine
    .AppendLine
    .AppendLine('end.')
end;

{ TUnitControllerEntitiesCommand }

procedure TUnitControllerEntityDeclarationCommand.ExecuteInterface(Section: TStringBuilder;
  Model: TJSONObject);
begin
  inherited;
  if not Model.B[TConfigKey.entity_generate] then Exit;

  CheckFor('entity.classname', Model);
  Section
    .AppendLine('unit ' + Model[TConfigKey.entity_unit_name] + ';')
    .AppendLine
    .AppendLine('interface')
    .AppendLine
    .AppendLine('uses')
    .AppendLine('  MVCFramework.Nullables, MVCFramework.Serializer.Commons;')
    .AppendLine
    .AppendLine('type')
    .AppendLine('  ' + Model[TConfigKey.entity_classname] + ' = class')
    .AppendLine('  private')
    .AppendLine('    fID: NullableInt32;')
    .AppendLine('    fFirstName: String;')
    .AppendLine('    fLastName: String;')
    .AppendLine('    fDOB: TDate;')
    .AppendLine('  public');

    if Model.B[TConfigKey.program_sqids] then
    begin
      Section.AppendLine('    [MVCSerializeAsSqids]')
    end;

  Section
    .AppendLine('    property ID: NullableInt32 read fID write fID;')
    .AppendLine('    property FirstName: String read fFirstName write fFirstName;')
    .AppendLine('    property LastName: String read fLastName write fLastName;')
    .AppendLine('    property DOB: TDate read fDOB write fDOB;  ')
    .AppendLine('    constructor Create(ID: Integer; FirstName, LastName: String; DOB: TDate);')
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
      .AppendLine('begin');
    if Model.B[TConfigKey.controller_actions_profiling_generate] then
    begin
      Section
        .AppendLine('{$IF CompilerVersion >= 34} //SYDNEY+')
        .AppendLine('  var lProf := Profiler.Start(Context.ActionQualifiedName);')
        .AppendLine('{$ENDIF}')
        .AppendLine;
    end;
    Section
      .AppendLine('  //use Context property to access to the HTTP request and response')
      .AppendLine('  Result := ''<p>Hello <strong>DelphiMVCFramework</strong> World</p>'' + ')
	  .AppendLine('            ''<p><small>dmvcframework-'' + DMVCFRAMEWORK_VERSION + ''</small></p>'';')
      .AppendLine('end;')
      .AppendLine
      .AppendLine('function ' + Model[TConfigKey.controller_classname] + '.GetReversedString(const Value: String): String;')
      .AppendLine('begin');
    if Model.B[TConfigKey.controller_actions_profiling_generate] then
    begin
      Section
        .AppendLine('{$IF CompilerVersion >= 34} //SYDNEY+')
        .AppendLine('  var lProf := Profiler.Start(Context.ActionQualifiedName);')
        .AppendLine('{$ENDIF}')
        .AppendLine;
    end;
    Section
      .AppendLine('  Result := System.StrUtils.ReverseString(Value.Trim);')
      .AppendLine('end;')
  end;

  if Model.B[TConfigKey.controller_crud_methods_generate] then
  begin
    if Model.B[TConfigKey.program_service_container_generate] then
    begin
      Section
        .AppendLine
        .AppendLine('//Sample CRUD Actions for a "People" entity (with service injection)')
        .AppendLine('function ' + Model[TConfigKey.controller_classname] + '.GetPeople(PeopleService: IPeopleService): IMVCResponse;')
        .AppendLine('begin');
      if Model.B[TConfigKey.controller_actions_profiling_generate] then
      begin
        Section
          .AppendLine('{$IF CompilerVersion >= 34} //SYDNEY+')
          .AppendLine('  var lProf := Profiler.Start(Context.ActionQualifiedName);')
          .AppendLine('{$ENDIF}')
          .AppendLine;
      end;
      Section
        .AppendLine('  Result := OkResponse(PeopleService.GetAll);')
        .AppendLine('end;')
    end
    else
    begin
      Section
        .AppendLine
        .AppendLine('//Sample CRUD Actions for a "People" entity (no service injection)')
        .AppendLine('function ' + Model[TConfigKey.controller_classname] + '.GetPeople: IMVCResponse;')
        .AppendLine('var')
        .AppendLine('  lPeople: TObjectList<TPerson>;')
        .AppendLine('begin');
      if Model.B[TConfigKey.controller_actions_profiling_generate] then
      begin
        Section
          .AppendLine('{$IF CompilerVersion >= 34} //SYDNEY+')
          .AppendLine('  var lProf := Profiler.Start(Context.ActionQualifiedName);')
          .AppendLine('{$ENDIF}')
          .AppendLine;
      end;
      Section
        .AppendLine('  lPeople := TObjectList<TPerson>.Create(True);')
        .AppendLine('  try')
        .AppendLine('    lPeople.Add(TPerson.Create(1, ''Peter'',''Parker'', EncodeDate(1965, 10, 4)));')
        .AppendLine('    lPeople.Add(TPerson.Create(2, ''Bruce'',''Banner'', EncodeDate(1945, 9, 6)));')
        .AppendLine('    lPeople.Add(TPerson.Create(3, ''Reed'',''Richards'', EncodeDate(1955, 3, 7)));')
        .AppendLine('    Result := OkResponse(lPeople);')
        .AppendLine('  except')
        .AppendLine('    lPeople.Free;')
        .AppendLine('    raise;')
        .AppendLine('  end;')
        .AppendLine('end;')
    end;

    Section
      .AppendLine
      .AppendLine('function ' + Model[TConfigKey.controller_classname] + '.GetPerson(ID: Integer): TPerson;')
      .AppendLine('begin');
    if Model.B[TConfigKey.controller_actions_profiling_generate] then
    begin
      Section
        .AppendLine('{$IF CompilerVersion >= 34} //SYDNEY+')
        .AppendLine('  var lProf := Profiler.Start(Context.ActionQualifiedName);')
        .AppendLine('{$ENDIF}')
        .AppendLine;
    end;
    Section
      .AppendLine('  Result := TPerson.Create(ID, ''Daniele'', ''Teti'', EncodeDate(1979, 11, 4));')
      .AppendLine('end;')
      .AppendLine
      .AppendLine('function ' + Model[TConfigKey.controller_classname] + '.CreatePerson([MVCFromBody] Person: TPerson): IMVCResponse;')
      .AppendLine('begin');
    if Model.B[TConfigKey.controller_actions_profiling_generate] then
    begin
      Section
        .AppendLine('{$IF CompilerVersion >= 34} //SYDNEY+')
        .AppendLine('  var lProf := Profiler.Start(Context.ActionQualifiedName);')
        .AppendLine('{$ENDIF}')
        .AppendLine;
    end;
    Section
      .AppendLine('  LogI(''Created '' + Person.FirstName + '' '' + Person.LastName);')
      .AppendLine('  Result := CreatedResponse('''', ''Person created'');')
      .AppendLine('end;')
      .AppendLine
      .AppendLine('function ' + Model[TConfigKey.controller_classname] + '.UpdatePerson(ID: Integer; [MVCFromBody] Person: TPerson): IMVCResponse;')
      .AppendLine('begin');
    if Model.B[TConfigKey.controller_actions_profiling_generate] then
    begin
      Section
        .AppendLine('{$IF CompilerVersion >= 34} //SYDNEY+')
        .AppendLine('  var lProf := Profiler.Start(Context.ActionQualifiedName);')
        .AppendLine('{$ENDIF}')
        .AppendLine;
    end;
    Section
      .AppendLine('  LogI(''Updated '' + Person.FirstName + '' '' + Person.LastName);')
      .AppendLine('  Result := NoContentResponse();')
      .AppendLine('end;')
      .AppendLine
      .AppendLine('function ' + Model[TConfigKey.controller_classname] + '.DeletePerson(ID: Integer): IMVCResponse;')
      .AppendLine('begin');
    if Model.B[TConfigKey.controller_actions_profiling_generate] then
    begin
      Section
        .AppendLine('{$IF CompilerVersion >= 34} //SYDNEY+')
        .AppendLine('  var lProf := Profiler.Start(Context.ActionQualifiedName);')
        .AppendLine('{$ENDIF}')
        .AppendLine;
    end;
    Section
      .AppendLine('  LogI(''Deleted person with id '' + ID.ToString);')
      .AppendLine('  Result := NoContentResponse();')
      .AppendLine('end;')

  end;
end;

procedure TUnitControllerControllerDeclarationCommand.ExecuteInterface(
  Section: TStringBuilder; Model: TJSONObject);
var
  lConverter: string;
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
	  .AppendLine('    [MVCProduces(TMVCMediaType.TEXT_HTML)]')
      .AppendLine('    function Index: String;')
      .AppendLine
      .AppendLine('    [MVCPath(''/reversedstrings/($Value)'')]')
      .AppendLine('    [MVCHTTPMethod([httpGET])]')
      .AppendLine('    [MVCProduces(TMVCMediaType.TEXT_PLAIN)]')
      .AppendLine('    function GetReversedString(const Value: String): String;')
      .AppendLine
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
      .AppendLine('    [MVCHTTPMethod([httpGET])]');

    if Model.B[TConfigKey.program_service_container_generate] then
    begin
      Section
        .AppendLine('    function GetPeople([MVCInject] PeopleService: IPeopleService): IMVCResponse;');
    end
    else
    begin
      Section
        .AppendLine('    function GetPeople: IMVCResponse;')
    end;

    lConverter := '';
    if Model.B[TConfigKey.program_sqids] then
    begin
      lConverter := ':sqids';
    end;

    Section
      .AppendLine
      .AppendLine('    [MVCPath(''/people/($ID' + lConverter + ')'')]')
      .AppendLine('    [MVCHTTPMethod([httpGET])]')
      .AppendLine('    function GetPerson(ID: Integer): TPerson;')
      .AppendLine
      .AppendLine('    [MVCPath(''/people'')]')
      .AppendLine('    [MVCHTTPMethod([httpPOST])]')
      .AppendLine('    function CreatePerson([MVCFromBody] Person: TPerson): IMVCResponse;')
      .AppendLine
      .AppendLine('    [MVCPath(''/people/($ID' + lConverter + ')'')]')
      .AppendLine('    [MVCHTTPMethod([httpPUT])]')
      .AppendLine('    function UpdatePerson(ID: Integer; [MVCFromBody] Person: TPerson): IMVCResponse;')
      .AppendLine
      .AppendLine('    [MVCPath(''/people/($ID' + lConverter + ')'')]')
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
    .AppendLine('  MVCFramework.Commons,');

  if Model.B[TConfigKey.program_ssv_templatepro] then
  begin
    Section
      .AppendLine('  MVCFramework.View.Renderers.TemplatePro,')
  end;

  if Model.B[TConfigKey.program_ssv_webstencils] then
  begin
    Section
      .AppendLine('  MVCFramework.View.Renderers.WebStencils,')
  end;


  if Model.B[TConfigKey.program_ssv_mustache] then
  begin
    Section
      .AppendLine('  MVCFramework.View.Renderers.Mustache,')
  end;

  Section
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
    .AppendLine;

    if Model.B[TConfigKey.program_ssv_templatepro] then
    begin
      Section
        .AppendLine('  // Server Side View')
        .AppendLine('  FMVC.SetViewEngine(TMVCTemplateProViewEngine);')
        .AppendLine('  // Server Side View - END')
        .AppendLine;
    end;

    if Model.B[TConfigKey.program_ssv_webstencils] then
    begin
      Section
        .AppendLine('  // Server Side View')
        .AppendLine('  FMVC.SetViewEngine(TMVCWebStencilsViewEngine);')
        .AppendLine('  // Server Side View - END')
        .AppendLine;
    end;

    if Model.B[TConfigKey.program_ssv_mustache] then
    begin
      Section
        .AppendLine('  // Server Side View')
        .AppendLine('  FMVC.SetViewEngine(TMVCMustacheViewEngine);')
        .AppendLine('  // Server Side View - END')
        .AppendLine;
    end;


    Section
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
    .AppendLine()
    .AppendLine('  // DMVCFramework Specific Configurations ')
    .AppendLine('  //   When MVCSerializeNulls = True empty nullables and nil are serialized as json null.')
    .AppendLine('  //   When MVCSerializeNulls = False empty nullables and nil are not serialized at all.')
    .AppendLine('  MVCSerializeNulls := True;')
    .AppendLine()
    .AppendLine('  // MVCNameCaseDefault defines the name case of property names generated by the serializers.')
    .AppendLine('  //   Possibile values are: ncAsIs, ncUpperCase, ncLowerCase (default), ncCamelCase, ncPascalCase, ncSnakeCase')
    .AppendLine('  MVCNameCaseDefault := TMVCNameCase.' + Model.S[TConfigKey.serializer_name_case] + ';')
    .AppendLine()
    .AppendLine('  // UseConsoleLogger defines if logs must be emitted to also the console (if available).')
    .AppendLine('  UseConsoleLogger := True;')
    .AppendLine()
    .AppendLine()
    .AppendLine('  LogI(''** DMVCFramework Server ** build '' + DMVCFRAMEWORK_VERSION);');

  if Model.B[TConfigKey.program_dotenv] then
  begin
    Section
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
      .AppendLine;
  end;

  if Model.B[TConfigKey.program_sqids] then
  begin
    Section
      .AppendLine
      .AppendLine('  TMVCSqids.SQIDS_ALPHABET := dotEnv.Env(''dmvc.sqids.alphabet'', ''' + GetScrambledAlphabet + ''');')
      .AppendLine('  TMVCSqids.SQIDS_MIN_LENGTH := dotEnv.Env(''dmvc.sqids.min_length'', 6);')
  end;

  Section
    .AppendLine
    .AppendLine('  try')
    .AppendLine('    if WebRequestHandler <> nil then')
    .AppendLine('      WebRequestHandler.WebModuleClass := WebModuleClass;')
    .AppendLine;
  Section
    .AppendLine('    WebRequestHandlerProc.MaxConnections := dotEnv.Env(''dmvc.handler.max_connections'', 1024);')
    .AppendLine
    .AppendLine('{$IF CompilerVersion >= 34} //SYDNEY+')
    .AppendLine('    if dotEnv.Env(''dmvc.profiler.enabled'', ' + Model.S[TConfigKey.controller_actions_profiling_generate].ToLower + ') then')
    .AppendLine('    begin')
    .AppendLine('      Profiler.ProfileLogger := Log;')
    .AppendLine('      Profiler.WarningThreshold := dotEnv.Env(''dmvc.profiler.warning_threshold'', 2000);')
    .AppendLine('    end;')
    .AppendLine('{$ENDIF}')
    .AppendLine;

  if Model.B[TConfigKey.program_ssv_templatepro] then
  begin
    Section
      .AppendLine('    TemplateProContextConfigure;');
  end;

  if Model.B[TConfigKey.program_ssv_webstencils] then
  begin
    Section
      .AppendLine('    WebStencilsProcessorConfigure;');
  end;

  if Model.B[TConfigKey.program_ssv_mustache] then
  begin
    Section
      .AppendLine('  // Project specific Mustache helpers')
      .AppendLine('  TMVCMustacheHelpers.OnLoadCustomHelpers := procedure(var MustacheHelpers: TSynMustacheHelpers)')
      .AppendLine('  begin')
      .AppendLine('    TSynMustache.HelperAdd(MustacheHelpers, ''MyHelper1'', TMyMustacheHelpers.MyHelper1);')
      .AppendLine('    TSynMustache.HelperAdd(MustacheHelpers, ''MyHelper2'', TMyMustacheHelpers.MyHelper2);')
      .AppendLine('  end;')
      .AppendLine;
  end;

  if Model.B[TConfigKey.program_service_container_generate] then
  begin
    Section
      .AppendLine('    RegisterServices(DefaultMVCServiceContainer);')
      .AppendLine('    DefaultMVCServiceContainer.Build;')
      .AppendLine;
  end;

  Section
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

function TUnitMainBeginEndCommand.GetScrambledAlphabet: String;
const
  DEFAULT_ALPHABET = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
var
  I: Integer;
  lIdx1: Integer;
  lSize: Integer;
  lIdx2: Integer;
  lTmp: Char;
begin
  Randomize;
  Result := DEFAULT_ALPHABET;
  lSize := Length(Result);
  for I := 1 to 100 do
  begin
    lIdx1 := Random(lSize) + 1;
    lIdx2 := Random(lSize) + 1;
    lTmp := Result[lIdx1];
    Result[lIdx1] := Result[lIdx2];
    Result[lIdx2] := lTmp;
  end;
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
    .AppendLine('    LServer.KeepAlive := dotEnv.Env(''dmvc.indy.keep_alive'', True);')
    .AppendLine('    LServer.MaxConnections := dotEnv.Env(''dmvc.webbroker.max_connections'', 0);')
    .AppendLine('    LServer.ListenQueue := dotEnv.Env(''dmvc.indy.listen_queue'', 500);')
    .AppendLine('    LServer.Active := True;')
    .AppendLine('    LogI(''Listening on http://localhost:'' + APort.ToString);')
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
    .AppendLine('constructor ' + Model[TConfigKey.entity_classname] + '.Create(ID: Integer; FirstName, LastName: String; DOB: TDate);')
    .AppendLine('begin')
    .AppendLine('  inherited Create;')
    .AppendLine('  fID := ID;')
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

{ TUnitMustacheHelpersDeclarationCommand }

procedure TUnitMustacheHelpersDeclarationCommand.ExecuteImplementation(
  Section: TStringBuilder; Model: TJsonObject);
begin
  inherited;
  Section
    .AppendLine('implementation')
    .AppendLine
    .AppendLine('uses')
    .AppendLine('  MVCFramework.View.Renderers.Mustache, System.SysUtils;')
    .AppendLine
    .AppendLine('{ TMyMustacheHelpers }')
    .AppendLine
    .AppendLine('class procedure TMyMustacheHelpers.MyHelper1(const Value: variant; out Result: variant);')
    .AppendLine('begin')
    .AppendLine('  Result := Value +  '' (I''''m The MyHelper1)'';')
    .AppendLine('end;')
    .AppendLine
    .AppendLine('class procedure TMyMustacheHelpers.MyHelper2(const Value: variant; out Result: variant);')
    .AppendLine('begin')
    .AppendLine('  Result := Value +  '' (I''''m The MyHelper2)'';')
    .AppendLine('end;')
    .AppendLine
    .AppendLine
    .AppendLine('end.');
end;

procedure TUnitMustacheHelpersDeclarationCommand.ExecuteInterface(
  Section: TStringBuilder; Model: TJSONObject);
begin
  inherited;
  CheckFor(TConfigKey.program_ssv_mustache, Model);
  CheckFor(TConfigKey.mustache_helpers_unit_name, Model);
  Section
    .AppendLine('unit ' + Model[TConfigKey.mustache_helpers_unit_name] + ';')
    .AppendLine
    .AppendLine('interface')
    .AppendLine
    .AppendLine
    .AppendLine('type')
    .AppendLine('  TMyMustacheHelpers = class sealed')
    .AppendLine('  public')
    .AppendLine('    class procedure MyHelper1(const Value: variant; out Result: variant);')
    .AppendLine('    class procedure MyHelper2(const Value: variant; out Result: variant);')
    .AppendLine('  end;')
    .AppendLine;
end;

{ TUnitServicesDeclarationCommand }

procedure TUnitServicesDeclarationCommand.ExecuteImplementation(
  Section: TStringBuilder; Model: TJsonObject);
begin
  Section
    .AppendLine('implementation')
    .AppendLine
    .AppendLine('uses')
    .AppendLine('  System.SysUtils;')
    .AppendLine
    .AppendLine('procedure RegisterServices(Container: IMVCServiceContainer);')
    .AppendLine('begin')
    .AppendLine('  Container.RegisterType(TPeopleService, IPeopleService, TRegistrationType.SingletonPerRequest);')
    .AppendLine('  // Register other services here')
    .AppendLine('end;')
    .AppendLine
    .AppendLine('function TPeopleService.GetAll: TObjectList<TPerson>;')
    .AppendLine('begin')
    .AppendLine('  Result := TObjectList<TPerson>.Create;')
    .AppendLine('  Result.AddRange([')
    .AppendLine('    TPerson.Create(1, ''Henry'', ''Ford'', EncodeDate(1863, 7, 30)),')
    .AppendLine('    TPerson.Create(2, ''Guglielmo'', ''Marconi'', EncodeDate(1874, 4, 25)),')
    .AppendLine('    TPerson.Create(3, ''Antonio'', ''Meucci'', EncodeDate(1808, 4, 13)),')
    .AppendLine('    TPerson.Create(4, ''Michael'', ''Faraday'', EncodeDate(1867, 9, 22))')
    .AppendLine('  ]);')
    .AppendLine('end;')
    .AppendLine
    .AppendLine
    .AppendLine('end.');
end;

procedure TUnitServicesDeclarationCommand.ExecuteInterface(
  Section: TStringBuilder; Model: TJSONObject);
begin
  Section
    .AppendLine('unit ' +  Model[TConfigKey.program_service_container_unit_name] + ';')
    .AppendLine
    .AppendLine('interface')
    .AppendLine
    .AppendLine('uses')
    .AppendLine('  ' + Model[TConfigKey.entity_unit_name] + ',')
    .AppendLine('  MVCFramework.Container, System.Generics.Collections;')
    .AppendLine
    .AppendLine('type')
    .AppendLine('  IPeopleService = interface')
    .AppendLine('    [''' + TGUID.NewGuid.ToString + ''']')
    .AppendLine('    function GetAll: TObjectList<TPerson>;')
    .AppendLine('  end;')
    .AppendLine
    .AppendLine('  TPeopleService = class(TInterfacedObject, IPeopleService)')
    .AppendLine('  protected')
    .AppendLine('    function GetAll: TObjectList<TPerson>;')
    .AppendLine('  end;')
    .AppendLine
    .AppendLine('procedure RegisterServices(Container: IMVCServiceContainer);')
    .AppendLine;

end;

{ TUnitTemplateProHelpersDeclarationCommand }

procedure TUnitTemplateProHelpersDeclarationCommand.ExecuteImplementation(
  Section: TStringBuilder; Model: TJsonObject);
begin
  inherited;
  Section
    .AppendLine('implementation')
    .AppendLine
    .AppendLine('uses')
    .AppendLine('  TemplatePro, System.SysUtils;')
    .AppendLine
    .AppendLine
    .AppendLine('function MyHelper1(const Value: TValue; const Parameters: TArray<string>): TValue;')
    .AppendLine('begin')
    .AppendLine('  Result := Value.ToString +  '' (I''''m The MyHelper1)'';')
    .AppendLine('end;')
    .AppendLine
    .AppendLine('function MyHelper2(const Value: TValue; const Parameters: TArray<string>): TValue;')
    .AppendLine('begin')
    .AppendLine('  Result := Value.ToString +  '' (I''''m The MyHelper2)'';')
    .AppendLine('end;')
    .AppendLine
    .AppendLine
    .AppendLine('procedure TemplateProContextConfigure;')
    .AppendLine('begin')
    .AppendLine('  TTProConfiguration.OnContextConfiguration := procedure(const CompiledTemplate: ITProCompiledTemplate)')
    .AppendLine('  begin')
    .AppendLine('    // These filters will be available to the TemplatePro views as if they were the standard ones')
    .AppendLine('    CompiledTemplate.AddFilter(''MyHelper1'', MyHelper1);')
    .AppendLine('    CompiledTemplate.AddFilter(''MyHelper2'', MyHelper2);')
    .AppendLine
    .AppendLine('    CompiledTemplate.OnGetValue :=')
    .AppendLine('      procedure(const DataSource, Members: string; var Value: TValue; var Handled: Boolean)')
    .AppendLine('      begin')
    .AppendLine('        if SameText(DataSource, ''ext1'') then')
    .AppendLine('        begin')
    .AppendLine('          if Members.IsEmpty then')
    .AppendLine('          begin')
    .AppendLine('            Value := ''External Value Ext1''')
    .AppendLine('          end')
    .AppendLine('          else')
    .AppendLine('          begin')
    .AppendLine('            Value := ''Reading ext1.'' + Members;')
    .AppendLine('          end;')
    .AppendLine('          Handled := True;')
    .AppendLine('        end;')
    .AppendLine('      end')
    .AppendLine('  end;')
    .AppendLine('end;')
    .AppendLine
    .AppendLine
    .AppendLine('end.');
end;

procedure TUnitTemplateProHelpersDeclarationCommand.ExecuteInterface(
  Section: TStringBuilder; Model: TJSONObject);
begin
  inherited;
  CheckFor(TConfigKey.program_ssv_templatepro, Model);
  CheckFor(TConfigKey.templatepro_helpers_unit_name, Model);
  Section
    .AppendLine('unit ' + Model[TConfigKey.templatepro_helpers_unit_name] + ';')
    .AppendLine
    .AppendLine('interface')
    .AppendLine
    .AppendLine('uses')
    .AppendLine('  System.Rtti;')
    .AppendLine
    .AppendLine('function MyHelper1(const Value: TValue; const Parameters: TArray<string>): TValue;')
    .AppendLine('function MyHelper2(const Value: TValue; const Parameters: TArray<string>): TValue;')
    .AppendLine
    .AppendLine
    .AppendLine('procedure TemplateProContextConfigure;')
    .AppendLine;
end;

{ TUnitWebStencilsHelpersDeclarationCommand }

procedure TUnitWebStencilsHelpersDeclarationCommand.ExecuteImplementation(Section: TStringBuilder; Model: TJsonObject);
begin
  inherited;
  Section
    .AppendLine('implementation')
    .AppendLine('')
    .AppendLine('uses')
    .AppendLine('  System.SysUtils, MVCFramework.View.Renderers.WebStencils, System.Bindings.Methods, Web.Stencils;')
    .AppendLine('')
    .AppendLine('')
    .AppendLine('function MyHelper1(const Parameters: TArray<IValue>): TValue;')
    .AppendLine('begin')
    .AppendLine('  Result := Parameters[0].GetValue.ToString +  '' (I''''m The MyHelper1)'';')
    .AppendLine('end;')
    .AppendLine('')
    .AppendLine('function MyHelper2(const Parameters: TArray<IValue>): TValue;')
    .AppendLine('begin')
    .AppendLine('  Result := Parameters[0].GetValue.ToString +  '' (I''''m The MyHelper2)'';')
    .AppendLine('end;')
    .AppendLine('')
    .AppendLine('procedure WebStencilsProcessorConfigure;')
    .AppendLine('begin')
    .AppendLine('  TBindingMethodsFactory.RegisterMethod(')
    .AppendLine('   TMethodDescription.Create(')
    .AppendLine('    MakeInvokable(function(Args: TArray<IValue>): IValue')
    .AppendLine('    begin')
    .AppendLine('      Result := TValueWrapper.Create(MyHelper1(Args));')
    .AppendLine('    end),')
    .AppendLine('    ''MyHelper1'', ''MyHelper1'', '''', True, ''MyHelper1 is just a sample'', nil));')
    .AppendLine('')
    .AppendLine('')
    .AppendLine('  TBindingMethodsFactory.RegisterMethod(')
    .AppendLine('   TMethodDescription.Create(')
    .AppendLine('    MakeInvokable(function(Args: TArray<IValue>): IValue')
    .AppendLine('    begin')
    .AppendLine('      Result := TValueWrapper.Create(MyHelper2(Args));')
    .AppendLine('    end),')
    .AppendLine('    ''MyHelper2'', ''MyHelper2'', '''', True, ''MyHelper2 is just a sample'', nil));')
    .AppendLine('')
    .AppendLine('  TMVCWebStencilsConfiguration.OnProcessorConfiguration :=')
    .AppendLine('    procedure(const WebStencilsProcessor: TWebStencilsProcessor)')
    .AppendLine('    begin')
    .AppendLine('      //custom configuration for TWebStencilsProcessor (executed for each view)')
    .AppendLine('    end;')
    .AppendLine('')
    .AppendLine('end;')
    .AppendLine('')
    .AppendLine('end.')
end;

procedure TUnitWebStencilsHelpersDeclarationCommand.ExecuteInterface(Section: TStringBuilder; Model: TJSONObject);
begin
  inherited;
  CheckFor(TConfigKey.program_ssv_webstencils, Model);
  CheckFor(TConfigKey.webstencils_helpers_unit_name, Model);
  Section
    .AppendLine('unit ' + Model[TConfigKey.webstencils_helpers_unit_name] + ';')
    .AppendLine
    .AppendLine('interface')
    .AppendLine
    .AppendLine('uses')
    .AppendLine('  System.Rtti, System.Bindings.EvalProtocol;')
    .AppendLine
    .AppendLine('function MyHelper1(const Parameters: TArray<IValue>): TValue;')
    .AppendLine('function MyHelper2(const Parameters: TArray<IValue>): TValue;')
    .AppendLine
    .AppendLine
    .AppendLine('procedure WebStencilsProcessorConfigure;')
    .AppendLine;
end;

end.
