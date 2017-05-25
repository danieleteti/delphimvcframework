unit ToDoBO;

interface

uses
  dorm.Mappings,
  MVCFramework.Commons,
  system.Rtti,
  dorm.ObjectStatus,
  system.SysUtils,
	MVCFramework.Serializer.Commons,
  Generics.Collections;

type
  TBaseBO = class abstract
  private
    FObjVersion: Int64;
    FID        : Integer;
    FObjStatus : TdormObjectStatus;
    procedure SetID(const Value: Integer);
    procedure SetObjStatus(const Value: TdormObjectStatus);

  protected
    procedure ThrowException(
      const AMessage        : string;
      const ADetailedMessage: string = '');

  public
    constructor Create; virtual;
    destructor Destroy; override;
    property ID: Integer read FID write SetID;
    [Transient]
    property ObjStatus: TdormObjectStatus read FObjStatus write SetObjStatus;
  end;

  [MapperJSONNaming(JSONNameLowerCase)]
  [Entity('TODO')]
  TToDo = class(TBaseBO)
  private
    FDescription: string;
    FDateTime   : TDateTime;
    procedure SetDateTime(const Value: TDateTime);
    procedure SetDescription(const Value: string);

  public
    [Column('DESCRIPTION')]
    property Description: string read FDescription write SetDescription;
    [Column('DATETIME')]
    property DateTime: TDateTime read FDateTime write SetDateTime;
  end;

implementation

{ TBPBaseBO }

constructor TBaseBO.Create;
begin
  inherited;
end;

destructor TBaseBO.Destroy;
begin

  inherited;
end;

procedure TBaseBO.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TBaseBO.SetObjStatus(const Value: TdormObjectStatus);
begin
  FObjStatus := Value;
end;

procedure TBaseBO.ThrowException(const AMessage, ADetailedMessage: string);
begin

end;

{ TToDoBO }

procedure TToDo.SetDateTime(const Value: TDateTime);
begin
  FDateTime := Value;
end;

procedure TToDo.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

end.
