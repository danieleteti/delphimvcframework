unit UtilsU;

interface

function GetMemoryUsed: UInt64;

implementation

uses
  System.SysUtils;

function GetMemoryUsed: UInt64;
{$IF Defined(MSWINDOWS)}
{$WARN SYMBOL_PLATFORM OFF}
var
  st: TMemoryManagerState;
  sb: TSmallBlockTypeState;
begin
  GetMemoryManagerState(st);
  result :=  st.TotalAllocatedMediumBlockSize
           + st.TotalAllocatedLargeBlockSize;
  for sb in st.SmallBlockTypeStates do
  begin
    Result := Result + sb.UseableBlockSize * sb.AllocatedBlockCount;
  end;
end;
{$WARN SYMBOL_PLATFORM ON}
{$ELSE}
begin
  raise Exception.Create('GetMemoryUsed is not supported on current platform');
end;
{$ENDIF}

end.
