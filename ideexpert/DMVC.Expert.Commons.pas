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

unit DMVC.Expert.Commons;

interface

uses
  MVCFramework.Commons, System.SysUtils, JsonDataObjects, ToolsAPI;

type
  IGenCommand = interface
    ['{B5F6B048-FB5A-48EA-80F9-D8395B4DE40D}']
    procedure ExecuteInterface(Section: TStringBuilder;  Model: TJSONObject);
    procedure ExecuteImplementation(Section: TStringBuilder;  Model: TJSONObject);
  end;

  TCustomCommand = class abstract(TInterfacedObject, IGenCommand)
  protected
    procedure CheckFor(const Key: String; Model: TJSONObject);
  public
    procedure ExecuteInterface(Section: TStringBuilder;  Model: TJSONObject); virtual; abstract;
    procedure ExecuteImplementation(Section: TStringBuilder;  Model: TJSONObject); virtual; abstract;
  end;

  TDefaultValues = class sealed
  public
    const sDefaultControllerName = 'TMyController';
    const sDefaultWebModuleName  = 'TMyWebModule';
    const sDefaultServerPort = '8080';
  end;

  TConfigKey = class sealed
  public const
     program_name = 'program.name';
     program_default_server_port= 'program.default_server_port';
     program_msheap='program.msheap';
     program_sqids='program.sqids';
     program_dotenv='program.dotenv';
     program_ssv_templatepro='program.ssv.templatepro';
     program_ssv_webstencils='program.ssv.webstencils';
     program_ssv_mustache='program.ssv.mustache';
     program_service_container_generate = 'program.service.container.generate';
     program_service_container_unit_name = 'program.service.container.unit_name';
     mustache_helpers_unit_name = 'mustache.helpers_unit_name';
     templatepro_helpers_unit_name = 'templatepro.helpers_unit_name';
     webstencils_helpers_unit_name = 'webstencils.helpers_unit_name';
     controller_unit_name='controller.unit_name';
     controller_classname= 'controller.classname';
     controller_index_methods_generate= 'controller.index_methods.generate';
     controller_action_filters_generate= 'controller.action_filters.generate';
     controller_crud_methods_generate= 'controller.crud_methods.generate';
     controller_actions_profiling_generate= 'controller.actions.profiling.generate';
     entity_generate= 'entity.generate';
     entity_classname= 'entity.classname';
     entity_unit_name='entity.unit_name';
     jsonrpc_generate= 'jsonrpc.generate';
     jsonrpc_classname= 'jsonrpc.classname';
     jsonrpc_unit_name='jsonrpc.unit_name';
     serializer_name_case= 'serializer.name_case';
     webmodule_classname= 'webmodule.classname';
     webmodule_unit_name= 'webmodule.unit_name';
     webmodule_middleware_analytics= 'webmodule.middleware.analytics';
     webmodule_middleware_staticfiles= 'webmodule.middleware.staticfiles';
     webmodule_middleware_trace= 'webmodule.middleware.trace';
     webmodule_middleware_compression= 'webmodule.middleware.compression';
     webmodule_middleware_etag= 'webmodule.middleware.etag';
     webmodule_middleware_cors= 'webmodule.middleware.cors';
     webmodule_middleware_activerecord= 'webmodule.middleware.activerecord';
     webmodule_middleware_activerecord_con_def_name= 'webmodule.middleware.activerecord.con_def_name';
     webmodule_middleware_activerecord_con_def_filename= 'webmodule.middleware.activerecord.con_def_filename';
  end;

  procedure ChangeIOTAModuleFileNamePrefix(const IOTA: IOTAModule; const FileNamePrefix: String);

implementation

uses
  System.IOUtils;

{ TCustomCommand }

procedure TCustomCommand.CheckFor(const Key: String;
  Model: TJSONObject);
begin
  if (not Model.Contains(Key)) or Model.S[Key].IsEmpty then
  begin
    raise Exception.CreateFmt('Required key "%s" not found or empty while processing %s', [Key, ClassName]);
  end;
end;

procedure ChangeIOTAModuleFileNamePrefix(const IOTA: IOTAModule; const FileNamePrefix: String);
var
  lDirName: string;
  lFileName: string;
  lFileExt: string;
begin
  lDirName := TPath.GetDirectoryName(IOTA.FileName);
  lFileName := TPath.GetFileNameWithoutExtension(IOTA.FileName);
  lFileExt :=  TPath.GetExtension(IOTA.FileName);
  lFileName := FileNamePrefix;
//  IOTA.FileName := TPath.Combine(lDirName, lFileName + lFileExt);
//  IOTA.Refresh(False);
end;

end.
