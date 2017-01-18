unit ToDoControllerU;

interface

uses MVCFramework,
 MVCFramework.Commons,
  MVCFramework.Logger,
  dorm, //this sample requires DORM
  dorm.Mappings,
  dorm.loggers,
  Web.HTTPApp;

type

  [MVCPath('/todo')]
  TToDoController = class(TMVCController)
  strict private
    FdormSession: TSession;

  private
    function GetdormSession: dorm.TSession;

  protected
    property dormSession: dorm.TSession read GetdormSession;

  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure GetAllToDo(ctx: TWebContext);

    [MVCPath('/($todoid)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetToDo(ctx: TWebContext);

    [MVCPath]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateToDo(ctx: TWebContext);

    destructor Destroy; override;

  end;

implementation

uses ObjectsMappers,
  Generics.Collections,
  dorm.ObjectStatus,
  dorm.Utils,
  dorm.Commons,
  ToDoBO,
{$IF CompilerVersion <= 27}
  DATA.DBXJSON,
{$ELSE}
  System.JSON,
{$ENDIF}
  System.SysUtils;

{ TApp1MainController }

destructor TToDoController.Destroy;
begin
  FdormSession.Free;
  inherited;
end;

procedure TToDoController.GetAllToDo(ctx: TWebContext);
var
  ToDos: TObjectList<TToDo>;
begin
  ToDos := dormSession.LoadList<TToDo>();
  try
    RenderListAsProperty<TToDo>('todos', ToDos, false);
  finally
    ToDos.Free;
  end;
end;

function TToDoController.GetdormSession: dorm.TSession;
begin
  if not Assigned(FdormSession) then
  begin
    FdormSession := TSession.CreateConfigured('dorm.conf',
      // {$IFDEF TEST}TdormEnvironment.deTest{$ENDIF}
      TdormEnvironment.deDevelopment
      // {$IFDEF RELEASE}TdormEnvironment.deRelease{$ENDIF}
      );
  end;
  Result := FdormSession;
end;

procedure TToDoController.GetToDo(ctx: TWebContext);
var
  Todo: TToDo;
  ID: Integer;
begin
  ID := ctx.Request.Params['todoid'].ToInteger;
  Todo := dormSession.Load<TToDo>(ID);
  try
    Render(Todo, false);
  finally
    Todo.Free;
  end;
end;

procedure TToDoController.UpdateToDo(ctx: TWebContext);
var
  ToDos: TObjectList<TToDo>;
begin
  if not Context.Request.ThereIsRequestBody then
    raise Exception.Create('Invalid request: Expected request body');
  if Context.Request.BodyAsJSONObject = nil then
    raise Exception.Create('Invalid request, Missing JSON object in parameter');

  ToDos := Mapper.JSONArrayToObjectList<TToDo>(ctx.Request.BodyAsJSONObject.Get('todos')
    .JsonValue as TJSONArray, false);
  try
    dormSession.PersistCollection(ToDos);
    Render(200, 'update ok');
  finally
    ToDos.Free;
  end;
end;

end.
