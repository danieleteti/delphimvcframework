unit LoggerPro.Utils;

interface

uses
  System.SysUtils;

function AppPath: string;
{$IFDEF MSWINDOWS}
function WUserName: string;
function FileVersion(const FileName: TFileName): string;
{$ENDIF}

implementation

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.IOUtils;

var
  GAppPath: string = '';

{$IFDEF MSWINDOWS}
function WUserName: string;
var
  nSize: DWord;
begin
  nSize := 1024;
  SetLength(Result, nSize);
  if GetUserName(PChar(Result), nSize) then
    SetLength(Result, nSize - 1)
  else
    RaiseLastOSError;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function FileVersion(const FileName: TFileName): string;
var
  VerInfoSize: Cardinal;
  VerValueSize: Cardinal;
  Dummy: Cardinal;
  PVerInfo: Pointer;
  PVerValue: PVSFixedFileInfo;
begin
  Result := '';
  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
  GetMem(PVerInfo, VerInfoSize);
  try
    if GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, PVerInfo) then
      if VerQueryValue(PVerInfo, '\', Pointer(PVerValue), VerValueSize) then
        with PVerValue^ do
          Result := Format('v%d.%d.%d build %d', [
            HiWord(dwFileVersionMS), // Major
            LoWord(dwFileVersionMS), // Minor
            HiWord(dwFileVersionLS), // Release
            LoWord(dwFileVersionLS)]); // Build
  finally
    FreeMem(PVerInfo, VerInfoSize);
  end;
end;
{$ENDIF}

function AppPath: string;
begin
  Result := GAppPath;
end;

initialization

GAppPath := IncludeTrailingPathDelimiter(TPath.GetDirectoryName(GetModuleName(HInstance)));


end.
