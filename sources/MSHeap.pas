unit MSHeap;

// Delphi memory-manager override that routes GetMem/FreeMem/ReallocMem/AllocMem
// to the Windows Heap API on the process heap.
//
// Roberto Della Pasqua www.dellapasqua.com
// 10 sept 2022 added inline directive, zeromemory and freemem return value
// 21 jun 2023 addition by Daniele Teti https://github.com/danieleteti/delphimvcframework
//
// ============================================================================
//   WARNING: DO NOT USE FOR CONCURRENT HTTP SERVERS
// ============================================================================
//
// On DMVCFramework Indy-direct, HTTP.sys and WebBroker backends this unit
// produces a MEASURED PERFORMANCE REGRESSION against the default FastMM
// memory manager at every concurrency level:
//
//   c=100, 15 s bench, Win64 Release
//     scenario     | FastMM    | MSHeap  | delta
//     -------------|-----------|---------|-------
//     health       | 4136 rps  |  716    |  -82%
//     json/small   | 3761      |  708    |  -81%
//     pods/small   | 3615      |  703    |  -81%
//     json/large   | 1229      |  289    |  -76%
//     heavy chain  | 3495      |  667    |  -81%
//
//   c=1, steady state
//     scenario     | FastMM    | MSHeap  | delta
//     -------------|-----------|---------|-------
//     health       | 1596 rps  |  841    |  -47%
//     json/small   | 1575      |  838    |  -47%
//
// Root cause: the Windows process heap serialises allocations through a
// process-wide lock (even with LFH, bucket selection still contends). Indy
// uses one thread per connection, so every request allocates from the same
// contended heap. FastMM has per-thread small-block arenas and avoids the
// contention entirely.
//
// When this unit IS useful: single-threaded console tools, long-running
// workers that allocate rarely, or processes where measured numbers show a
// win on the specific workload. Benchmark your own scenario before enabling
// — do not assume it is faster because it is the OS allocator.
//
// The DMVC project wizard no longer offers MSHeap as a checkbox option
// (removed after the benchmark above). This unit stays in the tree so
// existing projects that already list it in their .dpr keep compiling; to
// opt in manually, add `MSHeap,` as the FIRST unit in the .dpr uses clause.
// ============================================================================

{$O+}

interface

uses Windows;

implementation

var
  ProcessHeap: THandle;

function SysGetMem(Size: NativeInt): Pointer; inline;
begin
  Result := HeapAlloc(ProcessHeap, 0, Size);
end;

function SysFreeMem(P: Pointer): Integer; inline; //thanks Daniele Teti delphimvc
begin
  // HeapFree(NULL) is documented undefined behaviour on Windows; FastMM's
  // FreeMem(nil) is a no-op, and downstream code relies on that contract.
  if P = nil then
    Exit(0);
  if HeapFree(ProcessHeap, 0, P) then
    Result := 0
  else
    Result := -1;
end;

function SysReallocMem(P: Pointer; Size: NativeInt): Pointer; inline;
begin
  // HeapReAlloc's contract differs from FastMM's ReallocMem in two edge cases
  // that callers legitimately rely on:
  //   - P = nil: HeapReAlloc fails and returns nil. FastMM treats this as
  //     "allocate fresh", so we route through HeapAlloc.
  //   - Size = 0: HeapReAlloc keeps the block alive at zero bytes. FastMM
  //     frees the block and returns nil.
  if P = nil then
    Result := HeapAlloc(ProcessHeap, 0, Size)
  else if Size = 0 then
  begin
    HeapFree(ProcessHeap, 0, P);
    Result := nil;
  end
  else
    Result := HeapReAlloc(ProcessHeap, 0, P, Size);
end;

function SysAllocMem(Size: NativeInt): Pointer; inline;
begin
  Result := HeapAlloc(ProcessHeap, 8, Size); // zeromemory in dwflags api call
end;

function SysRegisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  Result := False;
end;

function SysUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  Result := False;
end;

const
  MemoryManager: TMemoryManagerEx =
  (
  GetMem: SysGetmem;
  FreeMem: SysFreeMem;
  ReallocMem: SysReAllocMem;
  AllocMem: SysAllocMem;
  RegisterExpectedMemoryLeak: SysRegisterExpectedMemoryLeak;
  UnregisterExpectedMemoryLeak: SysUnregisterExpectedMemoryLeak
  );

initialization

ProcessHeap := GetProcessHeap;
SetMemoryManager(MemoryManager);

end.
