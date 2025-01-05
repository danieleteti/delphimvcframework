unit CommonsU;

interface

uses
  MVCFramework.Commons, System.SysUtils, JsonDataObjects;

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

  TConfigKey = class sealed
  public const
     program_name = 'program.name';
     program_default_server_port= 'program.default_server_port';
     program_msheap='program.msheap';
     controller_unit_name='controller.unit_name';
     controller_classname= 'controller.classname';
     controller_index_methods_generate= 'controller.index_methods.generate';
     controller_action_filters_generate= 'controller.action_filters.generate';
     controller_crud_methods_generate= 'controller.crud_methods.generate';
     entity_generate= 'entity.generate';
     entity_classname= 'entity.classname';
     jsonrpc_generate= 'jsonrpc.generate';
     jsonrpc_classname= 'jsonrpc.classname';
     jsonrpc_unit_name='jsonrpc.unit_name';
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

implementation

{ TCustomCommand }

procedure TCustomCommand.CheckFor(const Key: String;
  Model: TJSONObject);
begin
  if (not Model.Contains(Key)) or Model.S[Key].IsEmpty then
  begin
    raise Exception.CreateFmt('Required key "%s" not found or empty while processing %s', [Key, ClassName]);
  end;
end;

end.
