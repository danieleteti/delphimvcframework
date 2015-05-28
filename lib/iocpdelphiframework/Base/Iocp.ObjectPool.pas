unit Iocp.ObjectPool;

interface

uses
  Classes, Types, SysUtils, SyncObjs;

type
  TIocpObject = class
  protected
    FOwner: TObject;
  protected
    procedure Initialize; virtual;
    procedure Finalize; virtual;
  public
    constructor Create(AOwner: TObject); virtual;

    property Owner: TObject read FOwner;
  end;
  
  TIocpObjectClass = class of TIocpObject;

  TIocpObjectPool = class
  private
    FOwner: TObject;
    FObjectClass: TIocpObjectClass;
    FMaxFreeObjects: Integer;
    FFreeObjectList: TList;
    FUsedObjectList: TList;
    FLocker: TCriticalSection;

    function GetFreeObjects: Integer;
    function GetUsedObjects: Integer;
    procedure SetMaxFreeObjects(MaxFreeObjects: Integer);
    function GetFreeObjectsSize: Integer;
    function GetUsedObjectsSize: Integer;
    procedure SetObjectClass(const Value: TIocpObjectClass);
  public
    constructor Create(AOwner: TObject; AObjectClass: TIocpObjectClass; AMaxFreeObjects: Integer); virtual;
    destructor Destroy; override;

    procedure Lock; inline;
    procedure Unlock; inline;
    function GetObject: TIocpObject;
    procedure FreeObject(AObj: TIocpObject);
    procedure Clear;

    property ObjectClass: TIocpObjectClass read FObjectClass write SetObjectClass;
    property MaxFreeObjects: Integer read FMaxFreeObjects write SetMaxFreeObjects;
    property FreeObjectList: TList read FFreeObjectList;
    property UsedObjectList: TList read FUsedObjectList;
    property FreeObjects: Integer read GetFreeObjects;
    property FreeObjectsSize: Integer read GetFreeObjectsSize;
    property UsedObjects: Integer read GetUsedObjects;
    property UsedObjectsSize: Integer read GetUsedObjectsSize;
  end;

implementation

{ TIocpObjectPool }

procedure TIocpObjectPool.Clear;
var
  Obj: TIocpObject;
begin
  Lock;
  try
    while (FFreeObjectList.Count > 0) do
    begin
      Obj := FFreeObjectList[FFreeObjectList.Count - 1];
      if (Obj <> nil) then
        Obj.Free;
      FFreeObjectList.Delete(FFreeObjectList.Count - 1);
    end;

    while (FUsedObjectList.Count > 0) do
    begin
      Obj := FUsedObjectList[FUsedObjectList.Count - 1];
      if (Obj <> nil) then
        Obj.Free;
      FUsedObjectList.Delete(FUsedObjectList.Count - 1);
    end;
  finally
    Unlock;
  end;
end;

constructor TIocpObjectPool.Create(AOwner: TObject; AObjectClass: TIocpObjectClass; AMaxFreeObjects: Integer);
begin
  FOwner := AOwner;
  FObjectClass := AObjectClass;
  FMaxFreeObjects := AMaxFreeObjects;
  FFreeObjectList := TList.Create;
  FUsedObjectList := TList.Create;
  FLocker := TCriticalSection.Create;
end;

destructor TIocpObjectPool.Destroy;
begin
  Clear;

  FFreeObjectList.Free;
  FUsedObjectList.Free;
  FLocker.Free;
  
  inherited Destroy;
end;

procedure TIocpObjectPool.FreeObject(AObj: TIocpObject);
begin
  if (AObj = nil) then Exit;

  Lock;
  try
    if (FUsedObjectList.Extract(AObj) = nil) then Exit;

    if (FFreeObjectList.Count < FMaxFreeObjects) then
    begin
      AObj.Finalize;
      FFreeObjectList.Add(AObj);
    end else
      FreeAndNil(AObj);
  finally
    Unlock;
  end;
end;

function TIocpObjectPool.GetFreeObjects: Integer;
begin
  Result := FFreeObjectList.Count;
end;

function TIocpObjectPool.GetFreeObjectsSize: Integer;
begin
  Result := FFreeObjectList.Count * FObjectClass.InstanceSize;
end;

function TIocpObjectPool.GetObject: TIocpObject;
begin
  Result := nil;

  Lock;
  try
    if (FFreeObjectList.Count > 0) then
    begin
      Result := FFreeObjectList[FFreeObjectList.Count - 1];
      FFreeObjectList.Delete(FFreeObjectList.Count - 1);
    end;

    if (Result = nil) then
      Result := FObjectClass.Create(FOwner);

    if (Result <> nil) then
    begin
      Result.Initialize;
      FUsedObjectList.Add(Result);
    end;
  finally
    Unlock;
  end;

  if (Result = nil) then
    raise Exception.CreateFmt('∑÷≈‰∂‘œÛ ß∞‹(%s)', [FObjectClass.ClassName]);
end;

function TIocpObjectPool.GetUsedObjects: Integer;
begin
  Result := FUsedObjectList.Count;
end;

function TIocpObjectPool.GetUsedObjectsSize: Integer;
begin
  Result := FUsedObjectList.Count * FObjectClass.InstanceSize;
end;

procedure TIocpObjectPool.Lock;
begin
  FLocker.Enter;
end;

procedure TIocpObjectPool.SetMaxFreeObjects(MaxFreeObjects: Integer);
begin
  Lock;
  FMaxFreeObjects := MaxFreeObjects;
  Unlock;
end;

procedure TIocpObjectPool.SetObjectClass(const Value: TIocpObjectClass);
begin
  Lock;
  FObjectClass := Value;
  Unlock;
end;

procedure TIocpObjectPool.Unlock;
begin
  FLocker.Leave;
end;

{ TIocpObject }

constructor TIocpObject.Create(AOwner: TObject);
begin
  FOwner := AOwner;
end;

procedure TIocpObject.Finalize;
begin
end;

procedure TIocpObject.Initialize;
begin
end;

end.
