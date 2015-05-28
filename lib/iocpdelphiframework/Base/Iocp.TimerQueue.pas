unit Iocp.TimerQueue;

{基于Win32系统的时钟队列}

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs, System.Generics.Collections,
  Winapi.Windows;

type
  TTimerProc = reference to procedure;
  TTimerMethod = procedure of object;

  PTimer = ^TTimer;
  TTimer = record
    Handle: THandle;
    Interval: Cardinal;
    Proc: TTimerProc;
    Method: TTimerMethod;
  end;

  TTimerQueue = class
  public type
    TTimerQueueTimerList = class(TList<PTimer>)
    protected
      procedure Notify(const Item: PTimer; Action: TCollectionNotification); override;
    end;
  private class var
    FTimerQueueHandle: THandle;
    FTimerList: TTimerQueueTimerList;
    FLocker: TCriticalSection;
  private
    class constructor Create;
    class destructor Destroy;

    class function NewTimer(Timer: PTimer): PTimer; overload;
  public
    class function NewTimer(Interval: Cardinal; Proc: TTimerProc): PTimer; overload;
    class function NewTimer(Interval: Cardinal; Method: TTimerMethod): PTimer; overload;
    class procedure RemoveTimer(Timer: PTimer);
    class procedure SetInterval(Timer: PTimer; Interval: Cardinal);

    class property Handle: THandle read FTimerQueueHandle;
  end;

implementation

procedure WaitOrTimerCallback(Timer: PTimer; TimerOrWaitFired: ByteBool); stdcall;
begin
  if not Assigned(Timer) then Exit;

  try
    if Assigned(Timer.Proc) then
      Timer.Proc()
    else if Assigned(Timer.Method) then
      Timer.Method();
  except
  end;
end;

{ TTimerQueue.TTimerQueueTimerList }

procedure TTimerQueue.TTimerQueueTimerList.Notify(const Item: PTimer;
  Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  if (Action = cnRemoved) and Assigned(Item) then
    System.Dispose(Item);
end;

{ TTimerQueue }

class constructor TTimerQueue.Create;
begin
  FTimerQueueHandle := CreateTimerQueue();
  FTimerList := TTimerQueueTimerList.Create;
  FLocker := TCriticalSection.Create;
end;

class destructor TTimerQueue.Destroy;
begin
  DeleteTimerQueueEx(FTimerQueueHandle, 0);
  FTimerQueueHandle := INVALID_HANDLE_VALUE;

  try
    FLocker.Enter;
    FTimerList.Clear;
  finally
    FLocker.Leave;
  end;

  FTimerList.Free;
  FLocker.Free;
end;

class function TTimerQueue.NewTimer(Timer: PTimer): PTimer;
begin
  if not Assigned(Timer) then Exit(nil);

  if CreateTimerQueueTimer(Timer.Handle, FTimerQueueHandle, @WaitOrTimerCallback, Timer, 100, Timer.Interval, 0) then
  begin
    FLocker.Enter;
    FTimerList.Add(Timer);
    FLocker.Leave;
    Result := Timer;
  end else
  begin
    System.Dispose(Timer);
    Result := nil;
  end;
end;

class function TTimerQueue.NewTimer(Interval: Cardinal;
  Proc: TTimerProc): PTimer;
var
  LTimer: PTimer;
begin
  System.New(LTimer);
  LTimer.Interval := Interval;
  LTimer.Proc := Proc;
  LTimer.Method := nil;
  Result := NewTimer(LTimer);
end;

class function TTimerQueue.NewTimer(Interval: Cardinal;
  Method: TTimerMethod): PTimer;
var
  LTimer: PTimer;
begin
  System.New(LTimer);
  LTimer.Interval := Interval;
  LTimer.Proc := nil;
  LTimer.Method := Method;
  Result := NewTimer(LTimer);
end;

class procedure TTimerQueue.RemoveTimer(Timer: PTimer);
begin
  if not Assigned(Timer) then Exit;
  
  FLocker.Enter;
  try
    DeleteTimerQueueTimer(FTimerQueueHandle, Timer.Handle, 0);
    FTimerList.Remove(Timer);
  finally
    FLocker.Leave;
  end;
end;

class procedure TTimerQueue.SetInterval(Timer: PTimer;
  Interval: Cardinal);
begin
  if not Assigned(Timer) then Exit;

  FLocker.Enter;
  try
    ChangeTimerQueueTimer(FTimerQueueHandle, Timer.Handle, 0, Interval);
    Timer.Interval := Interval;
  finally
    FLocker.Leave;
  end;
end;

end.

