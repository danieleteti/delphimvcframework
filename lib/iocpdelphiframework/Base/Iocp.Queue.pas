unit Iocp.Queue;

interface

uses
  Classes, SyncObjs;

type
  {
    *** 指针队列 ***
  }
  TIocpPointerQueue = class
  private
    FQueue: TList;
  protected
    function GetCount: Integer; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Push(P: Pointer): Boolean; virtual;
    function Pop(out P: Pointer): Boolean; virtual;
    procedure Clear; virtual;

    property Count: Integer read GetCount;
  end;

  {
    *** 带锁的指针队列 ***
  }
  TIocpQueue = class(TIocpPointerQueue)
  private
    FLocker: TCriticalSection;
  protected
    function GetCount: Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;

    function Push(P: Pointer): Boolean; override;
    function Pop(out P: Pointer): Boolean; override;
    procedure Clear; override;
  end;

  {
    *** 文本队列 ***
  }
  TTextQueue = class(TIocpQueue)
  public
    destructor Destroy; override;

    function Push(S: AnsiString): Boolean; reintroduce;
    function Pop(out S: AnsiString): Boolean; reintroduce;
  end;

implementation

{ TIocpPointerQueue }

procedure TIocpPointerQueue.Clear;
begin
  FQueue.Clear;
end;

constructor TIocpPointerQueue.Create;
begin
  FQueue := TList.Create;
end;

destructor TIocpPointerQueue.Destroy;
begin
  FQueue.Free;

  inherited Destroy;
end;

function TIocpPointerQueue.GetCount: Integer;
begin
  Result := FQueue.Count;
end;

function TIocpPointerQueue.Pop(out P: Pointer): Boolean;
begin
  if (FQueue.Count <= 0) then
  begin
    Result := False;
    Exit;
  end;

  P := FQueue[FQueue.Count - 1];
  FQueue.Delete(FQueue.Count - 1);
  Result := True;
end;

function TIocpPointerQueue.Push(P: Pointer): Boolean;
begin
  FQueue.Add(P);
  Result := True;
end;

{ TIocpQueue }

constructor TIocpQueue.Create;
begin
  inherited Create;

  FLocker := TCriticalSection.Create;
end;

destructor TIocpQueue.Destroy;
begin
  FLocker.Free;

  inherited Destroy;
end;

procedure TIocpQueue.Clear;
begin
  try
    Lock;
    inherited Clear;
  finally
    Unlock;
  end;
end;

function TIocpQueue.GetCount: Integer;
begin
  try
    Lock;
    Result := inherited GetCount;
  finally
    Unlock;
  end;
end;

procedure TIocpQueue.Lock;
begin
  FLocker.Enter;
end;

function TIocpQueue.Pop(out P: Pointer): Boolean;
begin
  try
    Lock;
    Result := inherited Pop(P);
  finally
    Unlock;
  end;
end;

function TIocpQueue.Push(P: Pointer): Boolean;
begin
  try
    Lock;
    Result := inherited Push(P);
  finally
    Unlock;
  end;
end;

procedure TIocpQueue.Unlock;
begin
  FLocker.Leave;
end;

{ TTextQueue }

destructor TTextQueue.Destroy;
var
  S: AnsiString;
begin
  while Pop(S) do;

  inherited Destroy;
end;

function TTextQueue.Pop(out S: AnsiString): Boolean;
var
  ps: PAnsiString;
begin
  Result := inherited Pop(Pointer(ps));
  if not Result then Exit;

  S := ps^;
  System.Dispose(ps);
end;

function TTextQueue.Push(S: AnsiString): Boolean;
var
  ps: PAnsiString;
begin
  System.New(ps);
  ps^ := S;
  Result := inherited Push(ps);
end;

end.
