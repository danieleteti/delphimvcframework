unit Iocp.Utils;

interface

uses
  Windows, SysUtils;

function CalcTickDiff(const StartTick, EndTick: LongWord): LongWord;
function ThreadFormat(const Fmt: string; const Args: array of const): string;
function ThreadWideFormat(const Fmt: WideString; const Args: array of const): WideString;
function ThreadFormatDateTime(const Fmt: string; DateTime: TDateTime): string;
procedure zMsgInfo(Handle: THandle; const AMsg: string; const ATitle: string = 'message');
procedure zMsgError(Handle: THandle; const AMsg: string; const ATitle: string = 'error message');
function zMsgQuest(Handle: THandle; const AMsg: string; const ATitle: string = 'confirmation'): Integer;
procedure zMsgWarning(Handle: THandle; const AMsg: string; const ATitle: string = 'warning message');
function SysErrInfo(ErrCode: DWORD): string;
function TickToTimeStr(t: Int64): string;
function RandRange(RangeFrom, RangeTo: Int64): Int64;
procedure StrResetLength(var S: string);
function GetWindowsTempPath: string;
function GetTempFileName(Path, Prefix, Ext: string): string;
function GetTempPath(const ParentPath: string): string;
function WaitForObject(Obj: THandle; Timeout: Integer): DWORD;
function WaitForObjects(const Obj: array of THandle; WaitAll: Boolean; Timeout: Integer): DWORD;
function SetFileDate(const FileName: string; const FileTime: TDateTime): Boolean;
function ExpandFullFileName(const FileName: string): string;

implementation

uses
  uGlobalVars;

function CalcTickDiff(const StartTick, EndTick: LongWord): LongWord;
begin
  if EndTick >= StartTick then
    Result := EndTick - StartTick
  else
    Result := High(LongWord) - StartTick + EndTick;
end;

function ThreadFormat(const Fmt: string; const Args: array of const): string;
var
  Fms: TFormatSettings;
begin
  {$if COMPILERVERSION >= 20}
  Fms := TFormatSettings.Create;
  {$else}
  GetLocaleFormatSettings(GetThreadLocale, Fms);
  {$ifend}
  Result := Format(Fmt, Args, Fms);
end;

function ThreadWideFormat(const Fmt: WideString; const Args: array of const): WideString;
var
  Fms: TFormatSettings;
begin
  {$if COMPILERVERSION >= 20}
  Fms := TFormatSettings.Create;
  {$else}
  GetLocaleFormatSettings(GetThreadLocale, Fms);
  {$ifend}
  Result := WideFormat(Fmt, Args, Fms);
end;

function ThreadFormatDateTime(const Fmt: string; DateTime: TDateTime): string;
var
  Fms: TFormatSettings;
begin
  {$if COMPILERVERSION >= 20}
  Fms := TFormatSettings.Create;
  {$else}
  GetLocaleFormatSettings(GetThreadLocale, Fms);
  {$ifend}
  Result := FormatDateTime(Fmt, DateTime, Fms);
end;

procedure zMsgInfo(Handle: THandle; const AMsg: string; const ATitle: string = 'message');
begin
  MessageBox(Handle, PChar(AMsg), PChar(ATitle), MB_OK or MB_ICONINFORMATION);
end;

procedure zMsgError(Handle: THandle; const AMsg: string; const ATitle: string = 'error message');
begin
  MessageBox(Handle, PChar(AMsg), PChar(ATitle), MB_OK or MB_ICONERROR);
end;

function zMsgQuest(Handle: THandle; const AMsg: string; const ATitle: string = 'confirmation'): Integer;
begin
  Result := MessageBox(Handle, PChar(AMsg), PChar(ATitle), MB_YESNO or MB_ICONQUESTION);
end;

procedure zMsgWarning(Handle: THandle; const AMsg: string; const ATitle: string = 'warning message');
begin
  MessageBox(Handle, PChar(AMsg), PChar(ATitle), MB_OK or MB_ICONWARNING);
end;

function SysErrInfo(ErrCode: DWORD): string;
begin
  Result := Format('System error: %d=%s', [ErrCode, SysErrorMessage(ErrCode)]);
end;

function TickToTimeStr(t: Int64): string;
begin
  if (t = 0) then
    Result := ''
  else if (t < 1000) then
    Result := Format('%dms', [t])
  else if (t < 60000) then
    Result := Format('%ds', [t div 1000]) + TickToTimeStr(t mod 1000)
  else if (t < 3600000) then
    Result := Format('%dm', [t div 60000]) + TickToTimeStr(t mod 60000)
  else
    Result := Format('%dh', [t div 3600000]) + TickToTimeStr(t mod 3600000);
end;

function RandRange(RangeFrom, RangeTo: Int64): Int64;
begin
  Result := RangeFrom + Random(RangeTo - RangeFrom + 1);
end;

procedure StrResetLength(var S: string);
begin
  SetLength(S, StrLen(PChar(S)));
end;

function GetWindowsTempPath: string;
var
  Required: Cardinal;
begin
  Result := '';
  Required := Windows.GetTempPath(0, nil);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    Windows.GetTempPath(Required, PChar(Result));
    StrResetLength(Result);
    Result := IncludeTrailingPathDelimiter(Result);
  end;
end;

function GetTempFileName(Path, Prefix, Ext: string): string;
begin
  if (Path = '') then
    Path := GetWindowsTempPath;
  Path := IncludeTrailingPathDelimiter(Path);
  repeat
    Result := Path + Prefix + IntToStr(Random(MaxInt)) + Ext;
  until not FileExists(Result);
end;

function GetTempPath(const ParentPath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempFileName(ParentPath, '', ''));
end;

function WaitForObject(Obj: THandle; Timeout: Integer): DWORD;
var
  t: DWORD;
begin
  if (Timeout > 0) then
    t := Timeout
  else
    t := INFINITE;
  Result := WaitforSingleObject(Obj, t);
end;

function WaitForObjects(const Obj: array of THandle; WaitAll: Boolean; Timeout: Integer): DWORD;
var
  t: DWORD;
begin
  if (Timeout > 0) then
    t := Timeout
  else
    t := INFINITE;
  Result := WaitForMultipleObjects(Length(Obj), @Obj[Low(Obj)], WaitAll, t);
end;

function SetFileDate(const FileName: string; const FileTime: TDateTime): Boolean;
var
  FileHandle: Integer;
  SysTime: TSystemTime;
  Time, LocalTime: TFileTime;
begin
  FileHandle := FileOpen(FileName, fmOpenWrite or fmShareDenyNone);
  if FileHandle > 0 then
  begin
    DateTimeToSystemTime(FileTime, SysTime);
    SystemTimeToFileTime(SysTime, LocalTime);
    LocalFileTimeToFileTime(LocalTime, Time);
    SetFileTime(FileHandle, @Time, @Time, @Time);
    FileClose(FileHandle);
    Result := True;
  end
  else
    Result := False;
end;

function ExpandFullFileName(const FileName: string): string;
begin
  Result := FileName;
  if (Result <> '') then
  begin
    if (ExtractFileDrive(Result) = '') then
      Result := ExpandFileName(gAppPath + Result);
  end;
end;

end.
