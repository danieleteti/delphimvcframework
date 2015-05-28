unit Iocp.MemoryPool;

{* 实际测试发现,32位下小块内存(<4K)的分配,Delphi自带的GetMem比HeapAlloc效率高很多
 * 即使GetMem + ZeroMemory也比HeapAlloc带$08(清0内存)标志快
 * 但是64位下或者大块内存(>4K)的分配HeapAlloc/HealFree性能略高
 *}
{$define __HEAP_ALLOC__}

interface

uses
  Windows, Types, Classes, SysUtils, SyncObjs, Iocp.Logger;

type
  TIocpMemoryPool = class
  private const
    {$ifdef __HEAP_ALLOC__}
    HEAP_ALLOC_FLAG: array [Boolean] of DWORD = ($00, $08);
    {$endif}
  private
    {$ifdef __HEAP_ALLOC__}
    FHeapHandle: THandle;
    {$endif}
    FRefCount: Integer;
    FBlockSize, FMaxFreeBlocks: Integer;
    FFreeMemoryBlockList: TList; // 经过实际测试，使用Classes.TList比Collections.TList<>效率更高
    FUsedMemoryBlockList: TList;
    FLocker: TCriticalSection;

    function GetFreeBlocks: Integer;
    function GetFreeBlocksSize: Integer;
    function GetUsedBlocks: Integer;
    function GetUsedBlocksSize: Integer;
    procedure SetMaxFreeBlocks(MaxFreeBlocks: Integer);
  protected
    function RealAlloc(Size: Integer; Zero: Boolean): Pointer; inline;
    procedure RealFree(P: Pointer); inline;
  public
    constructor Create(BlockSize, MaxFreeBlocks: Integer); virtual;
    destructor Destroy; override;

    procedure Lock; inline;
    procedure Unlock; inline;
    function GetMemory(Zero: Boolean): Pointer;
    procedure FreeMemory(P: Pointer);
    procedure Clear;

    property MaxFreeBlocks: Integer read FMaxFreeBlocks write SetMaxFreeBlocks;

    property FreeMemoryBlockList: TList read FFreeMemoryBlockList;
    property UsedMemoryBlockList: TList read FUsedMemoryBlockList;
    property BlockSize: Integer read FBlockSize;
    property FreeBlocks: Integer read GetFreeBlocks;
    property FreeBlocksSize: Integer read GetFreeBlocksSize;
    property UsedBlocks: Integer read GetUsedBlocks;
    property UsedBlocksSize: Integer read GetUsedBlocksSize;
  end;

implementation

{ TIocpMemoryPool }

constructor TIocpMemoryPool.Create(BlockSize, MaxFreeBlocks: Integer);
begin
  // 块大小以64字节对齐，这样的执行效率最高
  if (BlockSize mod 64 = 0) then
    FBlockSize := BlockSize
  else
    FBlockSize := (BlockSize div 64) * 64 + 64;
    
  FMaxFreeBlocks := MaxFreeBlocks;
  FFreeMemoryBlockList := TList.Create;
  FUsedMemoryBlockList := TList.Create;
  FLocker := TCriticalSection.Create;
  {$ifdef __HEAP_ALLOC__}
  FHeapHandle := GetProcessHeap;
  {$endif}
  FRefCount := 1;
end;

destructor TIocpMemoryPool.Destroy;
begin
  Clear;

  FFreeMemoryBlockList.Free;
  FUsedMemoryBlockList.Free;
  FLocker.Free;
  
  inherited Destroy;
end;

procedure TIocpMemoryPool.Lock;
begin
  FLocker.Enter;
end;

procedure TIocpMemoryPool.Unlock;
begin
  FLocker.Leave;
end;

function TIocpMemoryPool.RealAlloc(Size: Integer; Zero: Boolean): Pointer;
begin
  {$ifdef __HEAP_ALLOC__}
  Result := HeapAlloc(FHeapHandle, HEAP_ALLOC_FLAG[Zero], Size);
  {$else}
  GetMem(Result, Size);
  if (Result <> nil) and Zero then
    FillChar(Result^, Size, 0);
  {$endif}
end;

procedure TIocpMemoryPool.RealFree(P: Pointer);
begin
  {$ifdef __HEAP_ALLOC__}
  HeapFree(FHeapHandle, 0, P);
  {$else}
  FreeMem(P);
  {$endif}
end;

function TIocpMemoryPool.GetMemory(Zero: Boolean): Pointer;
begin
  Result := nil;

  Lock;
  try
    // 从空闲内存块列表中取一块
    if (FFreeMemoryBlockList.Count > 0) then
    begin
      Result := FFreeMemoryBlockList[FFreeMemoryBlockList.Count - 1];
      FFreeMemoryBlockList.Delete(FFreeMemoryBlockList.Count - 1);
    end;

    // 如果没有空闲内存块，分配新的内存块
    if (Result = nil) then
      Result := RealAlloc(FBlockSize, Zero);

    // 将取得的内存块放入已使用内存块列表
    if (Result <> nil) then
      FUsedMemoryBlockList.Add(Result);
  finally
    Unlock;
  end;

  if (Result = nil) then
    raise Exception.CreateFmt('分配内存块失败，块大小: %d', [FBlockSize]);
end;

procedure TIocpMemoryPool.FreeMemory(P: Pointer);
begin
  if (P = nil) then Exit;

  Lock;
  try
    // 从已使用内存块列表中移除内存块
    if (FUsedMemoryBlockList.Extract(P) = nil) then Exit;

    // 如果最大空闲内存块没有超标，将内存块放到空闲内存块列表中
    if (FFreeMemoryBlockList.Count < FMaxFreeBlocks) then
      FFreeMemoryBlockList.Add(P)
    // 否则释放内存
    else
      RealFree(P);
  finally
    Unlock;
  end;
end;

procedure TIocpMemoryPool.Clear;
var
  P: Pointer;
begin
  Lock;

  try
    // 清空空闲内存
    while (FFreeMemoryBlockList.Count > 0) do
    begin
      P := FFreeMemoryBlockList[FFreeMemoryBlockList.Count - 1];
      if (P <> nil) then
        RealFree(P);
      FFreeMemoryBlockList.Delete(FFreeMemoryBlockList.Count - 1);
    end;

    // 清空已使用内存
    while (FUsedMemoryBlockList.Count > 0) do
    begin
      P := FUsedMemoryBlockList[FUsedMemoryBlockList.Count - 1];
      if (P <> nil) then
        RealFree(P);
      FUsedMemoryBlockList.Delete(FUsedMemoryBlockList.Count - 1);
    end;
  finally
    Unlock;
  end;
end;

function TIocpMemoryPool.GetFreeBlocks: Integer;
begin
  Result := FFreeMemoryBlockList.Count;
end;

function TIocpMemoryPool.GetFreeBlocksSize: Integer;
begin
  Result := FFreeMemoryBlockList.Count * FBlockSize;
end;

function TIocpMemoryPool.GetUsedBlocks: Integer;
begin
  Result := FUsedMemoryBlockList.Count;
end;

function TIocpMemoryPool.GetUsedBlocksSize: Integer;
begin
  Result := FUsedMemoryBlockList.Count * FBlockSize;
end;

procedure TIocpMemoryPool.SetMaxFreeBlocks(MaxFreeBlocks: Integer);
begin
  Lock;
  FMaxFreeBlocks := MaxFreeBlocks;
  Unlock;
end;

end.
