unit Iocp.ReadWriteLocker;

{ 用原子操作加临界区实现的读写锁，性能比Delphi自带的高很多

ZY 2011.1.4

测试代码：

var
  L: TIocpReadWriteLocker;
  L2: TMultiReadExclusiveWriteSynchronizer;
  t: DWORD;
  i: Integer;
begin
  L := TIocpReadWriteLocker.Create;
  t := GetTickCount;
  for i := 1 to 10000000 do
  begin
    L.ReadLock;
    L.WriteLock;
    L.WriteUnlock;
    L.ReadUnlock;
  end;
  t := CalcTickDiff(t, GetTickCount);
  Memo1.Lines.Add('TIocpReadWriteLocker: ' + TickToTimeStr(t));
  L.Free;

  L2 := TMultiReadExclusiveWriteSynchronizer.Create;
  t := GetTickCount;
  for i := 1 to 10000000 do
  begin
    L2.BeginRead;
    L2.BeginWrite;
    L2.EndWrite;
    L2.EndRead;
  end;
  t := CalcTickDiff(t, GetTickCount);
  Memo1.Lines.Add('TMultiReadExclusiveWriteSynchronizer: ' + TickToTimeStr(t));
  L2.Free;
end;

测试结果：

TIocpReadWriteLocker: 625毫秒
TMultiReadExclusiveWriteSynchronizer: 17秒235毫秒
}

interface

uses
  Windows;

type
  TIocpReadWriteLocker = class
  private
    FReaderCount: Integer;
//    FCriRead: TRTLCriticalSection;
    FCriWrite: TRTLCriticalSection;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure ReadLock;
    procedure ReadUnlock;
    procedure WriteLock;
    procedure WriteUnlock;

    procedure BeginRead;
    procedure EndRead;
    procedure BeginWrite;
    procedure EndWrite;
  end;

implementation

{ TIocpReadWriteLocker }

constructor TIocpReadWriteLocker.Create;
begin
//  InitializeCriticalSection(FCriRead);
  InitializeCriticalSection(FCriWrite);
  FReaderCount := 0;
end;

destructor TIocpReadWriteLocker.Destroy;
begin
//  DeleteCriticalSection(FCriRead);
  DeleteCriticalSection(FCriWrite);

  inherited Destroy;
end;

procedure TIocpReadWriteLocker.ReadLock;
begin
{  EnterCriticalSection(FCriRead);
  Inc(FReaderCount);
  if (FReaderCount = 1) then
    WriteLock;
  LeaveCriticalSection(FCriRead);}

  if (InterlockedIncrement(FReaderCount) = 1) then
    WriteLock;
end;

procedure TIocpReadWriteLocker.ReadUnlock;
begin
{  EnterCriticalSection(FCriRead);
  Dec(FReaderCount);
  if (FReaderCount = 0) then
    WriteUnlock;
  LeaveCriticalSection(FCriRead);}

  if (InterlockedDecrement(FReaderCount) = 0) then
    WriteUnlock;
end;

procedure TIocpReadWriteLocker.WriteLock;
begin
  EnterCriticalSection(FCriWrite);
end;

procedure TIocpReadWriteLocker.WriteUnlock;
begin
  LeaveCriticalSection(FCriWrite);
end;

procedure TIocpReadWriteLocker.BeginRead;
begin
  ReadLock;
end;

procedure TIocpReadWriteLocker.BeginWrite;
begin
  WriteLock;
end;

procedure TIocpReadWriteLocker.EndRead;
begin
  ReadUnlock;
end;

procedure TIocpReadWriteLocker.EndWrite;
begin
  WriteUnlock;
end;

end.
