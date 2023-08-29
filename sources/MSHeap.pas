unit MSHeap;

// default MM override using Windows Heap API
// Roberto Della Pasqua www.dellapasqua.com
// 10 sept 2022 added inline directive, zeromemory and freemem return value
// 21 jun 2023 addition by Daniele Teti https://github.com/danieleteti/delphimvcframework

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
  Result := 0;
  if not HeapFree(ProcessHeap, 0, P) then Result := -1;
end;

function SysReallocMem(P: Pointer; Size: NativeInt): Pointer; inline;
begin
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
