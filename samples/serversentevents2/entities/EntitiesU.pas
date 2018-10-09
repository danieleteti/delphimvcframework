unit EntitiesU;

interface

uses
  MVCFramework.Serializer.Commons, System.SysUtils;

type
  TBaseEntity = class
    constructor Create; virtual;
  end;

  [MVCNameCase(ncLowerCase)]
  TNotificationEntity = class(TBaseEntity)
  private
    FValue: string;
    procedure SetValue(const Value: string);
  public
    constructor Create(const aValue: string); reintroduce; virtual;
    property Value: string read FValue write SetValue;
  end;

  [MVCNameCase(ncLowerCase)]
  TFullStatusEntity = class(TBaseEntity)
  private
    FId: Integer;
    FValue: string;
    FPushedAt: string;
    procedure SetId(const Value: Integer);
    procedure SetValue(const Value: string);
    procedure SetPushedAt(const Value: string);
  public
    property Id: Integer read FId write SetId;
    property Value: string read FValue write SetValue;
    property PushedAt: string read FPushedAt write SetPushedAt;
  end;

  /// <summary>
  /// Sinchronized access to status. Instances of this class are thread safe.
  /// </summary>
  TCurrentStatusEntity = class(TBaseEntity)
  private
    fStatus: TFullStatusEntity;
    class var Instance: TCurrentStatusEntity;
    class var CanBeCreated: string;
  protected
    procedure Lock;
    procedure UnLock;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetStatus(const aStatus: TFullStatusEntity);
    function GetStatus: Integer;
    class function GetInstance(const aInitializer
      : TFunc<TFullStatusEntity> = nil): TCurrentStatusEntity;
    class destructor Destroy;
  end;

implementation

var
  GLock: TObject;

  { TChangeStatus }

constructor TNotificationEntity.Create(const aValue: string);
begin
  inherited Create;
  FValue := aValue;
end;

procedure TNotificationEntity.SetValue(const Value: string);
begin
  FValue := Value;
end;

{ TCurrentStatusEntity }

constructor TCurrentStatusEntity.Create;
begin
  if CanBeCreated = '' then
    raise Exception.Create
      ('Cannot be created directly. Use the GetInstance static method.');
  inherited;
end;

class destructor TCurrentStatusEntity.Destroy;
begin
  FreeAndNil(Instance);
end;

destructor TCurrentStatusEntity.Destroy;
begin
  FreeAndNil(fStatus);
  inherited;
end;

class function TCurrentStatusEntity.GetInstance(const aInitializer
  : TFunc<TFullStatusEntity> = nil): TCurrentStatusEntity;
begin
  if not Assigned(Instance) then
  begin
    TMonitor.Enter(GLock);
    try
      if not Assigned(Instance) then // "double check" here
      begin
        CanBeCreated := 'X';
        Instance := TCurrentStatusEntity.Create;
        CanBeCreated := '';
        if Assigned(aInitializer) then
        begin
          Instance.SetStatus(aInitializer());
        end;
      end;
    finally
      TMonitor.Exit(GLock);
    end;
  end;
  Result := Instance;
end;

function TCurrentStatusEntity.GetStatus: Integer;
begin
  Lock;
  try
    Result := fStatus.Id;
  finally
    UnLock;
  end;
end;

procedure TCurrentStatusEntity.Lock;
begin
  TMonitor.Enter(GLock);
end;

procedure TCurrentStatusEntity.SetStatus(const aStatus: TFullStatusEntity);
begin
  Lock;
  try
    FreeAndNil(fStatus);
    fStatus := aStatus;
  finally
    UnLock;
  end;
end;

procedure TCurrentStatusEntity.UnLock;
begin
  TMonitor.Exit(GLock);
end;

{ TBaseEntity }

constructor TBaseEntity.Create;
begin
  inherited;
end;

{ TFullStatusEntity }

procedure TFullStatusEntity.SetId(const Value: Integer);
begin
  FId := Value;
end;

procedure TFullStatusEntity.SetPushedAt(const Value: string);
begin
  FPushedAt := Value;
end;

procedure TFullStatusEntity.SetValue(const Value: string);
begin
  FValue := Value;
end;

initialization

GLock := TObject.Create;

finalization

FreeAndNil(GLock);

end.
