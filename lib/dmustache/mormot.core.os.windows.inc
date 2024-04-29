{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

  Windows API calls for FPC/Delphi, as used by mormot.core.os.pas
}


{ ****************** Unicode, Time, File process }

const
  /// Windows file APIs have hardcoded MAX_PATH = 260 :(
  // - but more than 260 chars are possible with the \\?\..... prefix
  // or by disabling the limitation in registry since Windows 10, version 1607
  // https://learn.microsoft.com/en-us/windows/win32/fileio/maximum-file-path-limitation
  // - extended-length path allows up to 32,767 widechars
  // - but 2047 chars seems big enough in practice e.g. due to NTFS - POSIX uses 4096
  W32_MAX = 2047;

type
  /// 4KB stack buffer for no heap allocation during UTF-16 encoding or
  // switch to extended-length path
  TW32Temp = array[0..W32_MAX] of WideChar;

// W32() gets a PWideChar buffer from a TFileName using TW32Temp static buffer
// W32Copy() forces a temporary copy - as used by GetFileVersionInfo()

{$ifdef UNICODE}

function W32Copy(const FileName: TFileName; var Temp: TW32Temp): PWideChar;
var
  len: PtrInt;
begin
  len := length(FileName) * 2 + 2; // +2 to include ending #0
  if len > SizeOf(Temp) then
    Temp[0] := #0 // avoid buffer overflow
  else
    MoveFast(pointer(FileName)^, Temp, len);
  result := @Temp;
end;

// UnicodeString may need to be converted to extended-length path
function W32(const FileName: TFileName; var Temp: TW32Temp): PWideChar;
var
  U: pointer; // hold a UnicodeString but with no try..finally
begin
  if (length(FileName) > MAX_PATH) and
     (ord(FileName[1]) in [ord('A')..ord('Z'), ord('a')..ord('z')]) and
     (FileName[2] = ':') then
  begin
    // convert to extended-length path
    U := nil;
    TFileName(U) := '\\?\' + FileName;
    // move to TW32Temp stack buffer
    result := W32Copy(TFileName(U), Temp);
    // release temp memory and return the generated static buffer
    TFileName(U) := '';
  end
  else
    // no conversion nor allocation needed
    result := pointer(FileName);
end;

{$else}

// AnsiString is converted to UTF-16, potentially with extended-length path
procedure W32Convert(const FileName: TFileName; var Temp: TW32Temp);
var
  U: SynUnicode;
  len: PtrInt;
begin
  // identical to FPC RTL, which converts to UnicodeString before Wide API call 
  U := SynUnicode(FileName); // let the RTL + OS do the conversion
  // switch to extended-length path if needed, allowing up to 32,767 widechars
  len := length(U);
  if (len > MAX_PATH) and
     (FileName[1] in ['A'..'Z', 'a'..'z']) and
     (FileName[2] = ':') then
    U := '\\?\' + U;
  // move to TW32Temp stack buffer
  len := length(U) * 2 + 2;  // +2 to include ending #0
  if len > SizeOf(Temp) then
    Temp[0] := #0 // avoid buffer overflow (rejected by Windows anyway)
  else
    MoveFast(pointer(U)^, Temp, len);
end;

function W32(const FileName: TFileName; var Temp: TW32Temp): PWideChar;
var
  i, len: PtrInt;
begin
  len := length(FileName);
  if len = 0 then
    result := nil
  else
  begin
    if (len < MAX_PATH) and
       IsAnsiCompatible(pointer(FileName), len) then
      // most common cases do not need any Unicode conversion
      for i := 0 to len do // include trailing #0
        PWordArray(@Temp)[i] := PByteArray(FileName)[i]
    else
      // use a temporary SynUnicode variable for complex UTF-16 conversion
      // or if MAX_PATH is reached and \\?\ prefix is needed for extended length
      W32Convert(FileName, Temp);
    result := @Temp;
  end;
end;

function W32Copy(const FileName: TFileName; var Temp: TW32Temp): PWideChar;
begin
  result := W32(FileName, Temp); // from AnsiString: name is always copied
end;

{$endif UNICODE}

function _fmt(const Fmt: string; const Args: array of const): RawUtf8; overload;
begin
  result := RawUtf8(format(Fmt, Args)); // good enough (seldom called)
end;

procedure _fmt(const Fmt: string; const Args: array of const;
  var result: RawUtf8); overload;
begin
  result := RawUtf8(format(Fmt, Args)); // good enough (seldom called)
end;

procedure DoWin32PWideCharToUtf8(P: PWideChar; Len: PtrInt; var res: RawUtf8);
var
  tmp: TSynTempBuffer;
begin
  tmp.Init(Len * 3);
  Len := UnicodeToUtf8(tmp.Buf, Len * 3, P, Len); // use RTL if complex
  if Len > 0 then
    dec(Len); // UnicodeToUtf8() result includes the null terminator
  FastSetString(res, tmp.buf, Len);
  tmp.Done;
end;

// local RTL wrapper functions to avoid linking mormot.core.unicode.pas
procedure Win32PWideCharToUtf8(P: PWideChar; Len: PtrInt; out res: RawUtf8);
var
  i: PtrInt;
begin
  if Len > 0 then
    if IsAnsiCompatibleW(P, Len) then
    begin
      FastSetString(res, Len);
      for i := 0 to Len - 1 do
        PByteArray(res)[i] := PWordArray(P)[i]; // fast direct conversion
    end
    else
      DoWin32PWideCharToUtf8(P, Len, res);
end;

procedure Win32PWideCharToUtf8(P: PWideChar; out res: RawUtf8);
begin
  if P <> nil then
    Win32PWideCharToUtf8(P, StrLenW(P), res);
end;

function Utf8ToWin32PWideChar(const Text: RawUtf8;
  var dest: TSynTempBuffer): PWideChar;
var
  TextLen, i: PtrInt;
begin
  result := nil;
  TextLen := length(Text);
  dest.Init(TextLen * 2);
  if dest.len = 0 then
    exit;
  result := dest.buf;
  if IsAnsiCompatible(PAnsiChar(pointer(Text)), TextLen) then
  begin
    dest.len := TextLen;
    for i := 0 to TextLen do // include trailing #0
      PWordArray(result)[i] := PByteArray(Text)[i];
  end
  else
  begin
    dest.len := Utf8ToUnicode(result, dest.Len + 16, pointer(Text), TextLen);
    if dest.len <= 0 then
      dest.len := 0
    else
    begin
      dec(dest.len); // Utf8ToUnicode() returned length includes trailing #0
      result[dest.len] := #0; // missing on FPC
    end;
  end;
end;

const
  DefaultCharVar: AnsiChar = '?';

function Unicode_AnsiToWide(A: PAnsiChar; W: PWideChar; LA, LW, CodePage: PtrInt): integer;
begin
  result := MultiByteToWideChar(CodePage, MB_PRECOMPOSED, A, LA, W, LW);
end;

function Unicode_WideToAnsi(W: PWideChar; A: PAnsiChar; LW, LA, CodePage: PtrInt): integer;
begin
  result := WideCharToMultiByte(CodePage, 0, W, LW, A, LA, @DefaultCharVar, nil);
end;

function LibraryOpen(const LibraryName: TFileName): TLibHandle;
var
  tmp: TW32Temp;
  err: DWord;
  {$ifdef CPUX86}
  x87cw: word;
  {$endif CPUX86}
begin
  // note: GetErrorMode() is not available on XP
  err := SetErrorMode(SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS);
  {$ifdef CPUX86}
  asm
      fnstcw x87cw // save x87 flags
  end;
  {$endif CPUX86}
  result := Windows.LoadLibraryW(W32(LibraryName, tmp));
  {$ifdef CPUX86}
  asm
      fnclex       // clear pending x87 exceptions
      fldcw x87cw  // restore flags (Visual C++ librairies usually change them)
  end;
  {$endif CPUX86}
  SetErrorMode(err);
end;

procedure LibraryClose(Lib: TLibHandle);
begin
  if pointer(Lib) <> nil then
    Windows.FreeLibrary(Lib);
end;

// Delphi Unicode has an ambiguous GetProcAddress() overload with PWideChar
function LibraryResolve(Lib: TLibHandle; ProcName: PAnsiChar): pointer;
  external kernel32 name 'GetProcAddress'; // this is an Ansi-only API

function LibraryError: string;
begin
  result := IntToStr(GetLastError); // enough for basic troubleshouting
end;

procedure FileTimeToInt64(const FT: TFileTime; out I64: Int64);
  {$ifdef HASINLINE} inline; {$endif} 
begin
  PInt64Rec(@I64)^.Lo := FT.dwLowDateTime; // Delphi 2007 bug with PInt64()
  PInt64Rec(@I64)^.Hi := FT.dwHighDateTime;
end;

const
  SecsPerFileTime = 10000000;
  MilliSecsPerFileTime = 10000;

procedure UnixTimeToFileTime(I64: TUnixTime; out FT: TFileTime);
begin
  I64 := (I64 * SecsPerFileTime) + UnixFileTimeDelta;
  FT.dwLowDateTime  := PInt64Rec(@I64)^.Lo; // Delphi 2007 bug with PInt64()
  FT.dwHighDateTime := PInt64Rec(@I64)^.Hi;
end;

procedure UnixMSTimeToFileTime(I64: TUnixMSTime; out FT: TFileTime);
begin
  I64 := (I64 * MilliSecsPerFileTime) + UnixFileTimeDelta;
  FT.dwLowDateTime  := PInt64Rec(@I64)^.Lo; // Delphi 2007 bug with PInt64()
  FT.dwHighDateTime := PInt64Rec(@I64)^.Hi;
end;

procedure UnixTimeToLocalTime(I64: TUnixTime; out Local: TSystemTime);
var
  ft, lt: TFileTime;
begin
  UnixTimeToFileTime(I64, ft);
  FileTimeToLocalFileTime(ft, lt);
  FileTimeToSystemTime(lt, Local);
end;

function FileTimeToUnixTime(const FT: TFileTime): TUnixTime;
{$ifdef CPU64}
var
  nano100: Int64; // TFileTime is in 100 ns unit
{$endif CPU64}
begin
  if PInt64(@FT)^ = 0 then
  begin
    result := 0;
    exit;
  end;
  {$ifdef CPU64}
  FileTimeToInt64(ft, nano100);
  result := (nano100 - UnixFileTimeDelta) div SecsPerFileTime;
  {$else} // use PInt64 to avoid URW699 with Delphi 6 / Kylix
  result := (PInt64(@ft)^ - UnixFileTimeDelta) div SecsPerFileTime;
  {$endif CPU64}
end;

function FileTimeToUnixMSTime(const FT: TFileTime): TUnixMSTime;
{$ifdef CPU64}
var
  nano100: Int64; // TFileTime is in 100 ns unit
{$endif CPU64}
begin
  if PInt64(@FT)^ = 0 then
  begin
    result := 0;
    exit;
  end;
  {$ifdef CPU64}
  FileTimeToInt64(ft, nano100);
  result := (nano100 - UnixFileTimeDelta) div MilliSecsPerFileTime;
  {$else} 
  result := (PInt64(@ft)^ - UnixFileTimeDelta) div MilliSecsPerFileTime;
  {$endif CPU64}
end;

function FileTimeToDateTime(const FT: TFileTime): TDateTime;
begin
  if PInt64(@FT)^ = 0 then
    result := 0
  else // inlined UnixTimeToDateTime()
    result := FileTimeToUnixMSTime(FT) / MSecsPerDay + UnixDateDelta;
end;

procedure DateTimeToFileTime(dt: TDateTime; out FT: TFileTime);
begin
  if dt = 0 then
    PInt64(@FT)^ := 0
  else // inlined DateTimeToUnixTime()
    UnixTimeToFileTime(Round((dt - UnixDateDelta) * SecsPerDay), FT);
end;

function UnixTimeUtc: TUnixTime;
var
  ft: TFileTime;
begin
  GetSystemTimeAsFileTime(ft); // fast (HW resolution is < TUnixTime second)
  result := FileTimeToUnixTime(ft);
end;

var
  // redirect to a slower but more accurate API available since Windows 8
  // - points to GetSystemTimeAsFileTime() before Windows 8
  GetSystemTimePreciseAsFileTime: procedure(var ft: TFILETIME); stdcall;

function UnixMSTimeUtc: TUnixMSTime;
var
  ft: TFileTime;
begin
  GetSystemTimePreciseAsFileTime(ft); // slower, but try to achieve ms resolution
  result := FileTimeToUnixMSTime(ft);
end;

function UnixMSTimeUtcFast: TUnixMSTime;
var
  ft: TFileTime;
begin
  GetSystemTimeAsFileTime(ft); // faster, but with HW interupt resolution
  result := FileTimeToUnixMSTime(ft);
end;

procedure GetSystemTime;              external kernel32;
procedure GetLocalTime;               external kernel32;
procedure InitializeCriticalSection;  external kernel32;
procedure EnterCriticalSection;       external kernel32;
procedure LeaveCriticalSection;       external kernel32;
procedure DeleteCriticalSection;      external kernel32;
function  TryEnterCriticalSection;    external kernel32;
function  CloseHandle;                external kernel32;
procedure FileClose;                  external kernel32 name 'CloseHandle';
function  GetCurrentThreadId;         external kernel32;
procedure SwitchToThread;             external kernel32;
function  GetCurrentProcessId;        external kernel32;
function  GetCurrentProcess;          external kernel32;
function  WaitForSingleObject;        external kernel32;
function  GetEnvironmentStringsW;     external kernel32;
function  FreeEnvironmentStringsW;    external kernel32;
function  RtlCaptureStackBackTrace;   external kernel32;
function  IsDebuggerPresent;          external kernel32;
procedure SetEndOfFile;               external kernel32;
procedure FlushFileBuffers;           external kernel32;
function  GetLastError;               external kernel32;
procedure SetLastError;               external kernel32;
function  IocpCreate;                 external kernel32 name 'CreateIoCompletionPort';
function  IocpGetQueuedStatus;        external kernel32 name 'GetQueuedCompletionStatus';
function  IocpPostQueuedStatus;       external kernel32 name 'PostQueuedCompletionStatus';
function  GetDesktopWindow;           external user32;
function  Unicode_InPlaceUpper;       external user32 name 'CharUpperBuffW';
function  Unicode_InPlaceLower;       external user32 name 'CharLowerBuffW';

function HasConsole: boolean;
begin
  if StdOut = 0 then
    StdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  result := (StdOut <> 0) and
            (StdOut <> INVALID_HANDLE_VALUE);
end;

procedure AllocConsole;
begin
  Windows.AllocConsole;
  if (StdOut = 0) or
     (StdOut = INVALID_HANDLE_VALUE) then
    // force setup StdOut global variable
    StdOut := GetStdHandle(STD_OUTPUT_HANDLE);
end;

{$I-}
procedure DisplayFatalError(const title, msg: RawUtf8);
begin
  // better than a MessageBox() especially for services
  AllocConsole; // will create one black window console if none
  if title <> '' then
  begin
    TextColor(ccWhite);
    writeln(#13#10, title);
    writeln(StringOfChar('-', length(title) + 1), #13#10);
    TextColor(ccLightRed);
    writeln(msg);
    TextColor(ccLightGray);
  end
  else
    writeln(msg);
  ioresult;
end;
{$I+}

function IsSharedViolation(ErrorCode: integer): boolean;
begin
  if ErrorCode = 0 then
    ErrorCode := GetLastError;
  result := ErrorCode in [ERROR_SHARING_VIOLATION, ERROR_LOCK_VIOLATION];
end;

function GetModuleHandle(lpModuleName: PChar): HMODULE;
begin
  result := Windows.GetModuleHandle(lpModuleName); // call either A or W API
end;

function SetSystemTime(const utctime: TSystemTime): boolean;
var
  privileges: TSynWindowsPrivileges;
begin
  try
    privileges.Init;
    try
      privileges.Enable(wspSystemTime); // ensure has SE_SYSTEMTIME_NAME
      result := Windows.SetSystemTime(PSystemTime(@utctime)^);
    finally
      privileges.Done;
    end;
    if result then
      PostMessage(HWND_BROADCAST, WM_TIMECHANGE, 0, 0); // notify the apps
  except
    result := false;
  end;
end;

const
  // https://learn.microsoft.com/en-us/windows/win32/debug/system-error-codes
  ERROR_STANDARD1: array[0..39] of PUtf8Char = (
    'SUCCESS', 'INVALID_FUNCTION', 'FILE_NOT_FOUND', 'PATH_NOT_FOUND',
    'TOO_MANY_OPEN_FILES', 'ACCESS_DENIED', 'INVALID_HANDLE', 'ARENA_TRASHED',
    'NOT_ENOUGH_MEMORY', 'INVALID_BLOCK', 'BAD_ENVIRONMENT', 'BAD_FORMAT',
    'INVALID_ACCESS', 'INVALID_DATA', 'OUTOFMEMORY', 'INVALID_DRIVE',
    'CURRENT_DIRECTORY', 'NOT_SAME_DEVICE', 'NO_MORE_FILES', 'WRITE_PROTECT',
    'BAD_UNIT', 'NOT_READY', 'BAD_COMMAND', 'CRC', 'BAD_LENGTH', 'SEEK',
    'NOT_DOS_DISK', 'SECTOR_NOT_FOUND', 'OUT_OF_PAPER', 'WRITE_FAULT',
    'READ_FAULT', 'GEN_FAILURE', 'SHARING_VIOLATION', 'LOCK_VIOLATION',
    'WRONG_DISK', '35', 'SHARING_BUFFER_EXCEEDED', '37', 'HANDLE_EOF',
    'HANDLE_DISK_FULL');
  ERROR_STANDARD2: array[50..55] of PUtf8Char = (
    'NOT_SUPPORTED', 'REM_NOT_LIST', 'DUP_NAME', 'BAD_NETPATH',
    'NETWORK_BUSY', 'DEV_NOT_EXIST');
  ERROR_STANDARD3: array[80..89] of PUtf8Char = (
    'FILE_EXISTS', '81', 'CANNOT_MAKE', 'FAIL_I24', 'OUT_OF_STRUCTURES',
    'ALREADY_ASSIGNED', 'INVALID_PASSWORD', 'INVALID_PARAMETER',
    'NET_WRITE_FAULT', 'NO_PROC_SLOTS');
  ERROR_STANDARD4: array[108..129] of PUtf8Char = (
    'DRIVE_LOCKED', 'BROKEN_PIPE', 'OPEN_FAILED', 'BUFFER_OVERFLOW',
    'DISK_FULL', 'NO_MORE_SEARCH_HANDLES', 'INVALID_TARGET_HANDLE', '115',
    '116', 'INVALID_CATEGORY', 'INVALID_VERIFY_SWITCH', 'BAD_DRIVER_LEVEL',
    'CALL_NOT_IMPLEMENTED', 'SEM_TIMEOUT', 'INSUFFICIENT_BUFFER',
    'INVALID_NAME', 'INVALID_LEVEL', 'NO_VOLUME_LABEL', 'MOD_NOT_FOUND',
    'PROC_NOT_FOUND', 'WAIT_NO_CHILDREN', 'CHILD_NOT_COMPLETE');
  ERROR_STANDARD5: array[995..1013] of PUtf8Char = (
    'OPERATION_ABORTED', 'IO_INCOMPLETE', 'IO_PENDING', 'NOACCESS', 'SWAPERROR',
    '1000', 'STACK_OVERFLOW', 'INVALID_MESSAGE', 'CAN_NOT_COMPLETE',
    'INVALID_FLAGS', 'UNRECOGNIZED_VOLUME', 'FILE_INVALID', 'FULLSCREEN_MODE',
    'NO_TOKEN', 'BADDB', 'BADKEY', 'CANTOPEN', 'CANTREAD', 'CANTWRITE');
  ERROR_STANDARD6: array[1051..1079] of PUtf8Char = (
    'DEPENDENT_SERVICES_RUNNING', 'INVALID_SERVICE_CONTROL',
    'SERVICE_REQUEST_TIMEOUT', 'SERVICE_NO_THREAD', 'SERVICE_DATABASE_LOCKED',
    'SERVICE_ALREADY_RUNNING', 'INVALID_SERVICE_ACCOUNT', 'SERVICE_DISABLED',
    'CIRCULAR_DEPENDENCY', 'SERVICE_DOES_NOT_EXIST', 'SERVICE_CANNOT_ACCEPT_CTRL',
    'SERVICE_NOT_ACTIVE', 'FAILED_SERVICE_CONTROLLER_CONNECT',
    'EXCEPTION_IN_SERVICE', 'DATABASE_DOES_NOT_EXIST', 'SERVICE_SPECIFIC_ERROR',
    'PROCESS_ABORTED', 'SERVICE_DEPENDENCY_FAIL', 'SERVICE_LOGON_FAILED',
    'SERVICE_START_HANG', 'INVALID_SERVICE_LOCK', 'SERVICE_MARKED_FOR_DELETE',
    'SERVICE_EXISTS', 'ALREADY_RUNNING_LKG', 'SERVICE_DEPENDENCY_DELETED',
    'BOOT_ALREADY_ACCEPTED', 'SERVICE_NEVER_STARTED', 'DUPLICATE_SERVICE_NAME',
    'DIFFERENT_SERVICE_ACCOUNT');
  ERROR_STANDARD7: array[1200..1246] of PUtf8Char = (
    'BAD_DEVICE', 'CONNECTION_UNAVAIL', 'DEVICE_ALREADY_REMEMBERED',
    'NO_NET_OR_BAD_PATH', 'BAD_PROVIDER', 'CANNOT_OPEN_PROFILE', 'BAD_PROFILE',
    'NOT_CONTAINER', 'EXTENDED_ERROR', 'INVALID_GROUPNAME', 'INVALID_COMPUTERNAME',
    'INVALID_EVENTNAME', 'INVALID_DOMAINNAME', 'INVALID_SERVICENAME',
    'INVALID_NETNAME', 'INVALID_SHARENAME', 'INVALID_PASSWORDNAME',
    'INVALID_MESSAGENAME', 'INVALID_MESSAGEDEST', 'SESSION_CREDENTIAL_CONFLICT',
    'REMOTE_SESSION_LIMIT_EXCEEDED', 'DUP_DOMAINNAME', 'NO_NETWORK', 'CANCELLED',
    'USER_MAPPED_FILE', 'CONNECTION_REFUSED', 'GRACEFUL_DISCONNECT',
    'ADDRESS_ALREADY_ASSOCIATED', 'ADDRESS_NOT_ASSOCIATED', 'CONNECTION_INVALID',
    'CONNECTION_ACTIVE', 'NETWORK_UNREACHABLE', 'HOST_UNREACHABLE',
    'PROTOCOL_UNREACHABLE', 'PORT_UNREACHABLE', 'REQUEST_ABORTED',
    'CONNECTION_ABORTED', 'RETRY', 'CONNECTION_COUNT_LIMIT',
    'LOGIN_TIME_RESTRICTION', 'LOGIN_WKSTA_RESTRICTION', 'INCORRECT_ADDRESS',
    'ALREADY_REGISTERED', 'SERVICE_NOT_FOUND', 'NOT_AUTHENTICATED',
    'NOT_LOGGED_ON', 'CONTINUE');

function WinErrorConstant(Code: cardinal): PUtf8Char;
begin
  if Code <= high(ERROR_STANDARD1) then
    result := ERROR_STANDARD1[Code]
  else if Code in [low(ERROR_STANDARD2)..high(ERROR_STANDARD2)] then
    result := ERROR_STANDARD2[Code]
  else if Code in [low(ERROR_STANDARD3)..high(ERROR_STANDARD3)] then
    result := ERROR_STANDARD3[Code]
  else if Code in [low(ERROR_STANDARD4)..high(ERROR_STANDARD4)] then
    result := ERROR_STANDARD4[Code]
  else if (Code >= low(ERROR_STANDARD5)) and
          (Code <= high(ERROR_STANDARD5)) then
    result := ERROR_STANDARD5[Code]
  else if (Code >= low(ERROR_STANDARD6)) and
          (Code <= high(ERROR_STANDARD6)) then
    result := ERROR_STANDARD6[Code]
  else if (Code >= low(ERROR_STANDARD7)) and
          (Code <= high(ERROR_STANDARD7)) then
    result := ERROR_STANDARD7[Code]
  else
    case Code of
      ERROR_ALREADY_EXISTS:
        result := 'ALREADY_EXISTS';
      ERROR_MORE_DATA:
        result := 'MORE_DATA';
      ERROR_NO_SYSTEM_RESOURCES:
        result := 'NO_SYSTEM_RESOURCES';
      ERROR_WINHTTP_CANNOT_CONNECT:
        result := 'WINHTTP_CANNOT_CONNECT';
      ERROR_WINHTTP_TIMEOUT:
        result := 'WINHTTP_TIMEOUT';
      ERROR_WINHTTP_INVALID_SERVER_RESPONSE:
        result := 'WINHTTP_INVALID_SERVER_RESPONSE';
      10014:
        result := 'WSAEFAULT';
      10022:
        result := 'WSAEINVAL';
      10024:
        result := 'WSAEMFILE';
      10035:
        result := 'WSAEWOULDBLOCK';
      10038:
        result := 'WSAENOTSOCK';
      10053:
        result := 'WSAECONNABORTED';
      10054:
        result := 'WSAECONNRESET';
      10055:
        result := 'WSAENOBUFS';
      10060:
        result := 'WSAETIMEDOUT';
      10061:
        result := 'WSAECONNREFUSED';
      11003:
        result := 'WSATRY_AGAIN';
      else
        result := nil;
    end;
end;

function WinErrorText(Code: cardinal; ModuleName: PChar): RawUtf8;
var
  bak: integer;
  flags, len: PtrUInt;
  src: pointer;
  cod: PUtf8Char;
  tmp: array[0..511] of WideChar;
begin
  bak := GetLastError;
  src := nil;
  flags := FORMAT_MESSAGE_FROM_SYSTEM;
  if ModuleName = nil then
  begin
    // system error codes
    cod := WinErrorConstant(Code);
    if cod <> nil then
    begin
      // we can return directly the standard system error code constant
      _fmt('ERROR_%s', [cod], result);
      exit;
    end;
  end
  else
  begin
    // module specific error codes
    src := pointer(GetModuleHandle(ModuleName));
    if src <> nil then
      flags := FORMAT_MESSAGE_FROM_HMODULE;
  end;
  // first try if there is an English message version of this error code
  len := FormatMessageW(flags, src, Code, ENGLISH_LANGID, @tmp, SizeOf(tmp), nil);
  if len = 0 then
    // typically ERROR_RESOURCE_LANG_NOT_FOUND or ERROR_MUI_FILE_NOT_FOUND
    len := FormatMessageW(flags, src, Code, 0, @tmp, SizeOf(tmp), nil);
  if (len = 0) and
     (src <> nil) then
  begin
    // fallback to the system error message if this module as no such code
    SetLastError(bak);
    result := WinErrorText(Code, nil);
    exit;
  end;
  while (len > 0) and
        (ord(tmp[len - 1]) in [0..32, ord('.')]) do
    dec(len); // trim right
  Win32PWideCharToUtf8(@tmp, len, result);
  SetLastError(bak);
end;

function GetErrorText(error: integer): RawUtf8;
begin
  result := WinErrorText(error, nil);
end;

procedure RaiseLastModuleError(ModuleName: PChar; ModuleException: ExceptClass);
var
  code: integer;
begin
  code := GetLastError;
  raise ModuleException.CreateFmt('%s error %x (%s)',
    [ModuleName, code, string(WinErrorText(code, ModuleName))]);
end;

procedure RaiseLastError(const Context: shortstring; RaisedException: ExceptClass);
var
  code: integer;
begin
  code := GetLastError;
  if RaisedException = nil then
    RaisedException := EOSException;
  raise RaisedException.CreateFmt('%s error %x (%s)',
    [Context, code, string(WinErrorText(code, nil))])
end;

function PostMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL;
begin
  result := Windows.PostMessage(hWnd, Msg, wParam, lParam); // call either A or W API
end;

function ExpandEnvVars(const aStr: string): string;
// adapted from http://delphidabbler.com/articles?article=6
var
  size: integer;
begin
  // Get required buffer size
  size := ExpandEnvironmentStrings(pointer(aStr), nil, 0);
  if size > 0 then
  begin
    // Read expanded string into result string
    SetString(result, nil, size - 1);
    ExpandEnvironmentStrings(pointer(aStr), pointer(result), size);
  end
  else
    result := aStr; // return the original file name
end;

function IsInitializedCriticalSection(var cs: TRTLCriticalSection): boolean;
begin
  result := not IsZero(@cs, SizeOf(cs));
end;

var
  // value is documented as stable after boot, so we get it at startup
  _QueryPerformanceFrequency: QWord;
  // from HyperV or if HPET disabled e.g. -> direct division
  _QueryPerformanceFrequencyPer10: boolean;

procedure QueryPerformanceMicroSeconds(out Value: Int64);
var
  v: Int64; // for proper alignment on some old Delphi revisions + Win32
begin
  QueryPerformanceCounter(v);
  if _QueryPerformanceFrequencyPer10 then
    Value := QWord(v) div 10 // faster div by a constant (especially on FPC_64)
  else
    Value := QWord((QWord(v) * 1000000) div _QueryPerformanceFrequency);
end;

var
  shlwapiDll: THandle; // lazy loading (only by TNetClientProtocolFile)
  PathCreateFromUrl: function(pszUrl, pszPath: PChar; var pcchPath: cardinal;
    dwFlags: cardinal): HRESULT; stdcall;

function GetFileNameFromUrl(const Uri: string): TFileName;
var
  len: DWORD;
  tmp: array[0..MAX_PATH] of char;
begin
  result := '';
  len := MAX_PATH;
  if DelayedProc(PathCreateFromUrl, shlwapiDll, 'shlwapi.dll',
       'PathCreateFromUrl' + _AW) and
     (PathCreateFromUrl(pointer(Uri), @tmp, len, 0) = S_OK) then
    result := tmp;
end;

const
  faInvalidFile = faDirectory + faVolumeID{%H-} + faSysFile{%H-} + faHidden{%H-};
  faDirectoryMask = faDirectory + faHidden{%H-};

function FileDateToDateTime(const FileDate: TFileAge): TDateTime;
begin
  result := WindowsFileTimeToDateTime(FileDate);
end;

// some definitions missing on oldest Delphi
const
  FILE_ATTRIBUTE_REPARSE_POINT = $0000400;

function FindFirstFileExW(lpfilename: PWideChar; fInfoLevelId: FINDEX_INFO_LEVELS;
   lpFindFileData: pointer; fSearchOp: FINDEX_SEARCH_OPS;
   lpSearchFilter: pointer = nil; dwAdditionalFlags: cardinal = 0): THandle;
  stdcall; external kernel32;

// an alternative to GetFileAttributesExW() with fallback to FindFirstFileEx API
function GetFileAttributesRaw(fn: PWideChar;
  out Attr: WIN32_FILE_ATTRIBUTE_DATA): boolean;
var
  h: THandle;
  fd: TWin32FindDataW;
begin
  // this API is much faster than CreateFile/GetFileTime/GetFileSize/CloseHandle
  result := GetFileAttributesExW(fn, GetFileExInfoStandard, @Attr);
  if result or
     (GetLastError in [ERROR_FILE_NOT_FOUND, ERROR_PATH_NOT_FOUND,
        ERROR_INVALID_NAME, ERROR_INVALID_DRIVE, ERROR_NOT_READY,
        ERROR_INVALID_PARAMETER, ERROR_BAD_PATHNAME, ERROR_BAD_NETPATH,
        ERROR_BAD_NET_NAME]) then
    exit;
  // access denied, or locked file: fallback to slower but regular API
  h := FindFirstFileExW(fn, FindExInfoStandard, @fd, FindExSearchNameMatch);
  if not ValidHandle(h) then
    exit;
  windows.FindClose(h);
  Attr.dwFileAttributes := fd.dwFileAttributes;
  Attr.ftCreationTime   := fd.ftCreationTime;
  Attr.ftLastAccessTime := fd.ftLastAccessTime;
  Attr.ftLastWriteTime  := fd.ftLastWriteTime;
  Attr.nFileSizeHigh    := fd.nFileSizeHigh;
  Attr.nFileSizeLow     := fd.nFileSizeLow;
  result := true;
end;

function GetFileAttributesInternal(const FileName: TFileName;
  out Attr: WIN32_FILE_ATTRIBUTE_DATA; FollowLink: boolean = true): boolean;
var
  fn: PWideChar;
  h: THandle;
  f: cardinal;
  lp: TByHandleFileInformation;
  tmp: TW32Temp;
begin
  result := false;
  if FileName = '' then
    exit;
  fn := W32(FileName, tmp);
  result := GetFileAttributesRaw(fn, Attr);
  if result and
     FollowLink and
     (Attr.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT <> 0) then
  begin
    // we need to follow a symbolic link
    f := 0;
    if Attr.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY <> 0 then
      f := FILE_FLAG_BACKUP_SEMANTICS; // to access folder handle
    FillCharFast(Attr, SizeOf(Attr), 0); // enough for FileExists()
    // raw file access seems better than FileGetSymLinkTarget() in our case
    // and it will be consistent on both FPC and Delphi (including pre-Unicode)
    // - if we require file information, it is likely we would like to access it
    // - note that FPC and Delphi RTL seems overcomplicated and non-consistent
    // about symbolic links: mORMot will share this function everywhere
    h := CreateFileW(fn, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, f, 0);
    if ValidHandle(h) then
    begin
      if GetFileInformationByHandle(h, lp) then
      begin
        Attr.dwFileAttributes := lp.dwFileAttributes;
        Attr.ftCreationTime   := lp.ftCreationTime;
        Attr.ftLastAccessTime := lp.ftLastAccessTime;
        Attr.ftLastWriteTime  := lp.ftLastWriteTime;
        Attr.nFileSizeHigh    := lp.nFileSizeHigh;
        Attr.nFileSizeLow     := lp.nFileSizeLow;
      end
      else
        result := false;
      CloseHandle(h);
    end
    else
      result := IsSharedViolation;
  end;
end;

function FileAgeToDateTime(const FileName: TFileName): TDateTime;
var
  FA: WIN32_FILE_ATTRIBUTE_DATA;
  ST, LT: TSystemTime;
begin
  if (FileName <> '') and
     GetFileAttributesInternal(FileName, FA) and
     FileTimeToSystemTime({%H-}FA.ftLastWriteTime, ST) and
     SystemTimeToTzSpecificLocalTime(nil, ST, LT) then
    result := SystemTimeToDateTime(LT)
  else
    result := 0;
end;

function FileAgeToUnixTimeUtc(const FileName: TFileName; AllowDir: boolean): TUnixTime;
var
  FA: WIN32_FILE_ATTRIBUTE_DATA;
begin
  if (FileName <> '') and
     GetFileAttributesInternal(FileName, FA) and
     (AllowDir or ({%H-}FA.dwFileAttributes and faDirectory = 0)) then
    result := FileTimeToUnixTime(FA.ftLastWriteTime) // no local time conversion
  else
    result := 0;
end;

function FileAgeToWindowsTime(const FileName: TFileName): integer;
var
  FA: WIN32_FILE_ATTRIBUTE_DATA;
  ft: TFileTime;
begin
  result := 0;
  if (FileName <> '') and
     GetFileAttributesInternal(FileName, FA) and
     ({%H-}FA.dwFileAttributes and faDirectory = 0) and
     FileTimeToLocalFileTime(FA.ftLastWriteTime, ft) and
     not FileTimeToDosDateTime(ft, LongRec(result).Hi, LongRec(result).Lo) then
    result := 0;
end;

function FileSetDateFromWindowsTime(const Dest: TFileName; WinTime: integer): boolean;
begin
  result := FileSetDate(Dest, WinTime) = 0; // we already are on Windows
end;

function FileSetDateFromUnixUtc(const Dest: TFileName; Time: TUnixTime): boolean;
var
  D: THandle;
  ft: TFileTime;
begin
  result := false;
  if (Dest = '') or
     (Time = 0) then
    exit;
  D := FileOpen(Dest, fmOpenWrite);
  if not ValidHandle(D) then
    exit;
  UnixTimeToFileTime(Time, ft);
  result := SetFileTime(D, nil, nil, @ft);
  FileClose(D);
end;

function SearchRecToWindowsTime(const F: TSearchRec): integer;
begin
  result := F.Time; // already in the expected legacy format
end;

function SearchRecToUnixTimeUtc(const F: TSearchRec): TUnixTime;
begin // return the search record timestamp with no local time conversion
  result := FileTimeToUnixTime(F.FindData.ftLastWriteTime);
end;

function FileInfoByHandle(aFileHandle: THandle; FileId, FileSize: PInt64;
  LastWriteAccess, FileCreateDateTime: PUnixMSTime): boolean;
var
  mtime, atime, ctime: Int64;
  lp: TByHandleFileInformation;
begin
  result := GetFileInformationByHandle(aFileHandle, lp);
  if not result then
    exit;
  if FileId <> nil then
  begin
    PInt64Rec(FileId)^.lo := lp.nFileIndexLow;
    PInt64Rec(FileId)^.hi := lp.nFileIndexHigh;
  end;
  if FileSize <> nil then
  begin
    PInt64Rec(FileSize)^.lo := lp.nFileSizeLow;
    PInt64Rec(FileSize)^.hi := lp.nFileSizeHigh;
  end;
  if (LastWriteAccess = nil) and
     (FileCreateDateTime = nil) then
    exit;
  mtime := FileTimeToUnixMSTime(lp.ftLastWriteTime);
  if LastWriteAccess <> nil then
    LastWriteAccess^ := mtime;
  if FileCreateDateTime = nil then
    exit;
  atime := FileTimeToUnixMSTime(lp.ftLastAccessTime);
  ctime := FileTimeToUnixMSTime(lp.ftCreationTime);
  if mtime <> 0 then
    if (ctime = 0) or
       (ctime > mtime) then
      ctime := mtime;
  if atime <> 0 then
    if (ctime = 0) or
       (ctime > atime) then
      ctime := atime;
  FileCreateDateTime^ := ctime;
end;

function FileIsExecutable(const FileName: TFileName): boolean;
var
  h: THandle;
  header: word;
begin
  result := false;
  h := FileOpen(FileName, fmOpenReadShared);
  if not ValidHandle(h) then
    exit;
  result := (FileRead(h, header, 2) = 2) and
            (header = $5A4D); // DOS Magic Number
  FileClose(h);
end;

function GetModuleHandleExA(dwFlags: cardinal; lpModuleName: pointer;
 var phModule: HMODULE): BOOL; stdcall; external kernel32;

const
  GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT = $00000002;
  GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS       = $00000004;

function GetExecutableName(aAddress: pointer): TFileName;
var
  tmp: array[byte] of WideChar;
  hm: HMODULE;
begin
  result := '';
  FillcharFast(tmp, SizeOf(tmp), 0);
  hm := 0;
  if not GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT or
           GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS, aAddress, hm) then
    exit;
  GetModuleFileNameW(hm, tmp, SizeOf(tmp));
  result := string(SynUnicode(tmp));
end;

function FileIsWritable(const FileName: TFileName): boolean;
var
  FA: WIN32_FILE_ATTRIBUTE_DATA;
begin
  result := (FileName <> '') and
            GetFileAttributesInternal(FileName, FA) and
            (FA.dwFileAttributes and faReadOnly = 0);
end;

function FileExists(const FileName: TFileName; FollowLink, CheckAsDir: boolean): boolean;
var
  FA: WIN32_FILE_ATTRIBUTE_DATA;
begin
  result := (FileName <> '') and
            GetFileAttributesInternal(FileName, FA, FollowLink) and
            ((FA.dwFileAttributes and faDirectory <> 0) = CheckAsDir);
end;

function DirectoryExists(const FileName: TFileName; FollowLink: boolean): boolean;
var
  L: integer;
begin
  L := length(FileName);
  if L = 0 then
    result := false
  else if (L = 1) and
          (FileName[1] = '.') then
    result := true
  else if FileName[L] <> '\' then
    result := FileExists(FileName, FollowLink, {checkasdir=}true)
  else
    result := FileExists(copy(FileName, 1, L - 1), FollowLink, true);
end;

function FileSize(const FileName: TFileName): Int64;
var
  FA: WIN32_FILE_ATTRIBUTE_DATA;
begin
  if (FileName <> '') and
     GetFileAttributesInternal(FileName, FA) and
     (FA.dwFileAttributes and faDirectory = 0) then
    result := Qword(FA.nFileSizeHigh) shl 32 + FA.nFileSizeLow
  else
    result := 0;
end;

function FileInfoByName(const FileName: TFileName; out FileSize: Int64;
  out FileTimestampUtc: TUnixMSTime): boolean;
var
  FA: WIN32_FILE_ATTRIBUTE_DATA;
begin
  result := (FileName <> '') and
            GetFileAttributesInternal(FileName, FA);
  if not result then
    exit;
  PInt64Rec(@FileSize)^.Lo := FA.nFileSizeLow;
  PInt64Rec(@FileSize)^.Hi := FA.nFileSizeHigh;
  FileTimestampUtc := FileTimeToUnixMSTime(FA.ftLastWriteTime) // no local time
end;

function GetFileSizeEx(hFile: THandle; var FileSize: Int64): BOOL;
  stdcall; external kernel32;

function FileSize(F: THandle): Int64;
begin
  if (PtrInt(F) <= 0) or
     not GetFileSizeEx(F, result) then
    result := 0;
end;

function FileSeek64(Handle: THandle; const Offset: Int64;
  Origin: cardinal): Int64;
var
  r: TQWordRec;
begin
  r.V := Offset;
  r.L := SetFilePointer(Handle, r.L, @r.H, Origin);
  if (r.Li = -1) and
     (GetLastError <> 0) then
    result := -1
  else
    result := r.V;
end;

function DeleteFile(const aFileName: TFileName): boolean;
var
  tmp: TW32Temp;
begin
  if aFileName = '' then
    result := false
  else
    result := DeleteFileW(W32(aFileName, tmp));
end;

function FileShare(aMode: integer): DWord;
begin
  case (aMode and $f0) of
    fmShareRead:       // = fmShareDenyWrite
      result := FILE_SHARE_READ;
    fmShareWrite:      // = fmShareDenyRead
      result := FILE_SHARE_WRITE;
    fmShareReadWrite:  // = fmShareDenyNone
      result := FILE_SHARE_READ or FILE_SHARE_WRITE;
  else
    result := 0;
  end;
end;

function FileCreate(const aFileName: TFileName; aMode, aRights: integer): THandle;
var
  tmp: TW32Temp;
begin
  // aRights parameter is just ignored on Windows
  if aFileName = '' then
    result := 0
  else
    result := CreateFileW(W32(aFileName, tmp), GENERIC_READ or GENERIC_WRITE,
      FileShare(aMode), nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;

const
  FILE_WRITE_ATTRIBUTES = $0100; // not defined on oldest Delphi

// W32() will support length > MAX_PATH even if aFileName is UnicodeString
function FileOpen(const aFileName: TFileName; aMode: integer): THandle;
const
  ACCESS: array[0..2] of DWord = (
    GENERIC_READ,   // fmOpenRead  = $0000
    GENERIC_WRITE,  // fmOpenWrite = $0001
    GENERIC_READ or GENERIC_WRITE or FILE_WRITE_ATTRIBUTES); // fmOpenReadWrite
var
  tmp: TW32Temp;
begin
  if aFileName = '' then
    result := 0
  else
   result := CreateFileW(W32(aFileName, tmp), ACCESS[aMode and 3],
      FileShare(aMode), nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
end;

function FileSetDateFrom(const Dest: TFileName; SourceHandle: THandle): boolean;
var
  FileTime: TFileTime;
  D: THandle;
begin
  if (Dest = '') or
     not ValidHandle(SourceHandle) then
    result := false
  else
  begin
    D := FileOpen(Dest, fmOpenWrite);
    if ValidHandle(D) then
    begin
      result := GetFileTime(SourceHandle, nil, nil, @FileTime) and
                SetFileTime(D, nil, nil, @FileTime);
      FileClose(D);
    end
    else
      result := false;
  end;
end;

function FileSetDateFrom(const Dest, Source: TFileName): boolean;
var
  S: THandle;
begin
  result := false;
  if (Dest = '') or
     (Source = '') then
    exit;
  S := FileOpen(Source, fmOpenReadShared);
  if not ValidHandle(S) then
    exit;
  result := FileSetDateFrom(Dest, S);
  FileClose(S);
end;

procedure FileSetAttr(const FileName: TFileName; Attr: integer);
var
  tmp: TW32Temp;
begin
  if FileName <> '' then
    SetFileAttributesW(W32(FileName, tmp), Attr);
end;

procedure FileSetHidden(const FileName: TFileName; ReadOnly: boolean);
const
  FLAGS: array[boolean] of integer = (
    FILE_ATTRIBUTE_HIDDEN,
    FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_READONLY);
begin
  FileSetAttr(FileName, FLAGS[ReadOnly]);
end;

procedure FileSetSticky(const FileName: TFileName);
begin
  FileSetAttr(FileName, FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM);
end;

function RenameFile(const OldName, NewName: TFileName): boolean;
var
  o, n: TW32Temp;
begin
  if (OldName = '') or
     (NewName = '') then
    result := false
  else
    result := MoveFileW(W32(OldName, o), W32(NewName, n));
end;

function FileSetTime(const FileName: TFileName;
  const Created, Accessed, Written: Int64): boolean;
var
  tmp: TW32Temp;
  h: THandle;
  pct, pat, pwt: pointer;
begin
  result := false;
  h := CreateFileW(W32(FileName, tmp), FILE_WRITE_ATTRIBUTES,
    FILE_SHARE_READ, nil, OPEN_ALWAYS, 0, 0);
  if ValidHandle(h) then
    try
      // some input code may not set all properties: use what we got
      if Created <> 0 then
        pct := @Created
      else if Written <> 0 then
        pct := @Written
      else if Accessed <> 0 then
        pct := @Accessed
      else
        exit;
      if Accessed <> 0 then
        pat := @Accessed
      else if Written <> 0 then
        pat := @Written
      else
        pat := @Created;
      if Written <> 0 then
        pwt := @Written
      else if Created <> 0 then
        pwt := @Created
      else
        pwt := @Accessed;
      result := SetFileTime(h, pct, pat, pwt);
    finally
      CloseHandle(h);
    end;
end;

function CopyFile(const Source, Target: TFileName; FailIfExists: boolean): boolean;
var
  s, t: TW32Temp;
begin
  if (Source = '') or
     (Target = '') then
    result := false
  else
    result := Windows.CopyFileW(W32(Source, s), W32(Target, t), FailIfExists);
end;

function ValidHandle(Handle: THandle): boolean;
begin
  result := PtrInt(Handle) > 0;
end;

function FileOpenSequentialRead(const FileName: TFileName): integer;
var
  tmp: TW32Temp;
begin
  result := CreateFileW(W32(FileName, tmp), GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,
    FILE_FLAG_SEQUENTIAL_SCAN, 0);
end;

function FileIsReadable(const aFileName: TFileName): boolean;
var
  tmp: TW32Temp;
  h: THandle;
begin
  h := CreateFileW(W32(aFileName, tmp), GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  result := ValidHandle(h);
  if result then
    CloseHandle(h);
end;

threadvar // mandatory: GetTickCount seems per-thread on XP :(
  LastTickXP: TQWordRec;

function GetTickCount64ForXP: Int64; stdcall;
var
  t32: cardinal;
  p: PQWordRec;
begin
  // warning: GetSystemTimeAsFileTime() is fast, but not monotonic!
  t32 := Windows.GetTickCount; // we only have the 32-bit counter on XP
  p := @LastTickXP;
  inc(p^.H, ord(t32 < p^.L)); // wrap-up overflow after 49 days
  p^.L := t32;
  result := p^.V;
end; // warning: FPC's GetTickCount64 doesn't handle 49 days wrap on XP :(

procedure InitializeSRWLockForXP(var P: TOSLightMutex); stdcall;
begin
  TLightLock(P).Init; // TLightLock is good enough on XP
end;

procedure AcquireSRWLockExclusiveForXP(var P: TOSLightMutex); stdcall;
begin
  TLightLock(P).Lock;
end;

procedure ReleaseSRWLockExclusiveForXP(var P: TOSLightMutex); stdcall;
begin
  TLightLock(P).UnLock;
end;

function GetUptimeSec: cardinal;
begin
  result := GetTickCount64 div MSecsPerSec; // good enough
end;

procedure SleepHiRes(ms: cardinal);
begin
  if ms <> 0 then
    Windows.Sleep(ms) // follow the HW timer: typically up to 16ms on Windows
  else
    SwitchToThread;
end;


{ TOSLightLock }

procedure TOSLightLock.Init;
begin
  fMutex := nil;
  InitializeSRWLock(fMutex); // fallback to TLightLock on XP
end;

procedure TOSLightLock.Done;
begin // nothing needed
end;

procedure TOSLightLock.Lock;
begin
  AcquireSRWLockExclusive(fMutex);
end;

procedure TOSLightLock.UnLock;
begin
  ReleaseSRWLockExclusive(fMutex);
end;


{ TSynEvent }

constructor TSynEvent.Create;
begin
  fHandle := pointer(CreateEvent(nil, false, false, nil));
end;

destructor TSynEvent.Destroy;
begin
  CloseHandle(THandle(fHandle));
  inherited Destroy;
end;

procedure TSynEvent.ResetEvent;
begin
  Windows.ResetEvent(THandle(fHandle));
end;

procedure TSynEvent.SetEvent;
begin
  Windows.SetEvent(THandle(fHandle));
end;

procedure TSynEvent.WaitFor(TimeoutMS: integer);
begin
  WaitForSingleObject(THandle(fHandle), TimeoutMS);
end;

procedure TSynEvent.WaitForEver;
begin
  WaitForSingleObject(THandle(fHandle), INFINITE);
end;


{$ifdef FPC}
  {$define NOSETTHREADNAME} // only tested and supported on Delphi
{$endif FPC}

const
  // see http://msdn.microsoft.com/en-us/library/xcb2z8hs
  cSetThreadNameException = $406D1388;

{$ifdef NOSETTHREADNAME}

procedure RawSetThreadName(ThreadID: TThreadID; const Name: RawUtf8);
begin
end;

{$else}

procedure RawSetThreadName(ThreadID: TThreadID; const Name: RawUtf8);
var
  s: AnsiString;
  {$ifndef ISDELPHIXE2}
  info: record
    FType: LongWord;     // must be 0x1000
    FName: PAnsiChar;    // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
  {$endif ISDELPHIXE2}
begin
  if not IsDebuggerPresent then
    exit;
  s := AnsiString(Name);
  {$ifdef ISDELPHIXE2}
  TThread.NameThreadForDebugging(s, ThreadID); // use
  {$else}
  info.FType := $1000;
  info.FName := pointer(s);
  info.FThreadID := ThreadID;
  info.FFlags := 0;
  try
    RaiseException(
      cSetThreadNameException, 0, SizeOf(info) div SizeOf(LongWord), @info);
  except
    {ignore}
  end;
  {$endif ISDELPHIXE2}
end;

{$endif NOSETTHREADNAME}

function RawKillThread(Thread: TThread): boolean;
begin
  result := (Thread <> nil) and
            Windows.TerminateThread(Thread.Handle, 777);
end;

procedure ResetCpuSet(out CpuSet: TCpuSet);
begin
  CpuSet := 0;
end;

function SetThreadMaskAffinity(Thread: TThread; const Mask: TCpuSet): boolean;
begin
  result := (Thread <> nil) and
            (Windows.SetThreadAffinityMask(Thread.Handle, Mask) <> 0);
end;

function GetProcessAffinityMask(hProcess: THandle;
  var lpProcessAffinityMask, lpSystemAffinityMask: TCpuSet): BOOL;
    stdcall; external kernel32; // redefined for Delphi 7 compatibility

function GetMaskAffinity(out CpuSet: TCpuSet): boolean;
var
  process, thread: TCpuSet;
begin
  result := GetProcessAffinityMask(GetCurrentProcess, process, thread);
  if result then
    CpuSet := process;
end;

type
  // avoid linking of ComObj.pas just for EOleSysError
  EOleSysError = class(Exception)
  public
    ErrorCode: cardinal;
  end;

{$ifndef NOEXCEPTIONINTERCEPT}

const
  // https://docs.microsoft.com/en-us/archive/blogs/yizhang/interpreting-hresults-returned-from-netclr-0x8013xxxx
  // see also https://referencesource.microsoft.com/#mscorlib/system/__hresults.cs
  DOTNET_EXCEPTIONNAME: array[0..91] of PUtf8Char = (
    'Access',                            // $8013151A
    'AmbiguousMatch',                    // $8000211D
    'appdomainUnloaded',                 // $80131015
    'Application',                       // $80131600
    'Argument',                          // $80070057
    'ArgumentNull',                      // $80004003
    'ArgumentOutOfRange',                // $80131502
    'Arithmetic',                        // $80070216
    'ArrayTypeMismatch',                 // $80131503
    'BadImageFormat',                    // $8007000B
    'CannotUnloadappdomain',             // $80131015
    'ContextMarshal',                    // $80090020
    'Cryptographic',                     // $80004001
    'CryptographicUnexpectedOperation',  // $80131431
    'CustomAttributeFormat',             // $80131537
    'DirectoryNotFound',                 // $80070003
    'DirectoryNotFound',                 // $80030003
    'DivideByZero',                      // $80020012
    'DllNotFound',                       // $80131524
    'DuplicateWaitObject',               // $80131529
    'EndOfStream',                       // $00801338
    'EntryPointNotFound',                // $80131522
    '',                                  // $80131500 - name is plain Exception
    'ExecutionEngine',                   // $80131506
    'External',                          // $80004005
    'FieldAccess',                       // $80131507
    'FileLoad',                          // $80131621
    'FileLoad',                          // $80131018
    'FileNotFound',                      // $80070002
    'Format',                            // $80131537
    'IndexOutOfRange',                   // $80131508
    'InvalidCast',                       // $80004002
    'InvalidComObject',                  // $80131527
    'InvalidFilterCriteria',             // $80131601
    'InvalidOleVariantType',             // $80131531
    'InvalidOperation',                  // $80131509
    'InvalidProgram',                    // $8013153A
    'IO',                                // $80131620
    'IsolatedStorage',                   // $80131450
    'MarshalDirective',                  // $80131535
    'MethodAccess',                      // $80131510
    'MissingField',                      // $80131511
    'MissingManifestResource',           // $80131532
    'MissingMember',                     // $80131512
    'MissingMethod',                     // $80131513
    'MulticastNotSupported',             // $80131514
    'NotFiniteNumber',                   // $80131528
    'NotImplemented',                    // $80004001
    'NotSupported',                      // $80131515
    'NullReference',                     // $80004003
    'OutOfMemory',                       // $8007000E
    'Overflow',                          // $80131516
    'PlatformNotSupported',              // $80131539
    'Policy',                            // $80131416
    'Rank',                              // $80131517
    'ReflectionTypeLoad',                // $80131602
    'Remoting',                          // $8013150B
    'RemotingTimeout',                   // $8013150B
    'SafeArrayTypeMismatch',             // $80131533
    'SafeArrayRankMismatch',             // $80131538
    'Security',                          // $8013150A
    'SEH',                               // $80004005
    'Serialization',                     // $8013150C
    'Server',                            // $8013150E
    'StackOverflow',                     // $800703E9
    'SUDSGenerator',                     // $80131500
    'SUDSParser',                        // $80131500
    'SynchronizationLock',               // $80131518
    'System',                            // $80131501
    'Target',                            // $80131603
    'TargetInvocation',                  // $80131604
    'TargetParameterCount',              // $80138002
    'ThreadAbort',                       // $80131530
    'ThreadInterrupted',                 // $80131519
    'ThreadState',                       // $80131520
    'ThreadStop',                        // $80131521
    'TypeInitialization',                // $80131534
    'TypeLoad',                          // $80131522
    'TypeUnloaded',                      // $80131013
    'UnauthorizedAccess',                // $80070005
    'InClassConstructor',                // $80131543
    'KeyNotFound',                       // $80131577
    'InsufficientStack',                 // $80131578
    'InsufficientMemory',                // $8013153D
    'Verification',                      // $8013150D
    'HostProtection',                    // $80131640
    'MinGrantFailed',                    // $80131417
    'Crypto',                            // $80131430
    'CryptoUnexOper',                    // $80131431
    'Overflow',                          // $8002000a
    'InvalidName',                       // $80131047
    'TypeMismatch');                     // $80028ca0

  DOTNET_EXCEPTIONHRESULT: array[0..91] of cardinal = (
    $8013151A,
    $8000211D,
    $80131015,
    $80131600,
    $80070057,
    $80004003,
    $80131502,
    $80070216,
    $80131503,
    $8007000B,
    $80131015,
    $80090020,
    $80004001,
    $80131431,
    $80131537,
    $80070003,
    $80030003,
    $80020012,
    $80131524,
    $80131529,
    $00801338,
    $80131522,
    $80131500,
    $80131506,
    $80004005,
    $80131507,
    $80131621,
    $80131018,
    $80070002,
    $80131537,
    $80131508,
    $80004002,
    $80131527,
    $80131601,
    $80131531,
    $80131509,
    $8013153A,
    $80131620,
    $80131450,
    $80131535,
    $80131510,
    $80131511,
    $80131532,
    $80131512,
    $80131513,
    $80131514,
    $80131528,
    $80004001,
    $80131515,
    $80004003,
    $8007000E,
    $80131516,
    $80131539,
    $80131416,
    $80131517,
    $80131602,
    $8013150B,
    $8013150B,
    $80131533,
    $80131538,
    $8013150A,
    $80004005,
    $8013150C,
    $8013150E,
    $800703E9,
    $80131500,
    $80131500,
    $80131518,
    $80131501,
    $80131603,
    $80131604,
    $80138002,
    $80131530,
    $80131519,
    $80131520,
    $80131521,
    $80131534,
    $80131522,
    $80131013,
    $80070005,
    $80131543,
    $80131577,
    $80131578,
    $8013153D,
    $8013150D,
    $80131640,
    $80131417,
    $80131430,
    $80131431,
    $8002000a,
    $80131047,
    $80028ca0);

function ExceptionInheritsFrom(E: TClass; const Name: ShortString): boolean;
begin
  result := true;
  while (E <> nil) and
        (E <> Exception) do
    if PropNameEquals(PPointer(PtrInt(E) + vmtClassName)^, @Name) then
      exit
    else
      E := GetClassParent(E);
  result := false;
end;

function TSynLogExceptionContext.AdditionalInfo(
  out ExceptionNames: TPUtf8CharDynArray): cardinal;
var
  i: PtrInt;
begin
  if ExceptionInheritsFrom(EClass, 'EOleSysError') then
  begin
    result := EOleSysError(EInstance).ErrorCode;
    if result > $80000000 then
      for i := 0 to high(DOTNET_EXCEPTIONHRESULT) do
        // manual loop: the same error code can appear several times
        if DOTNET_EXCEPTIONHRESULT[i] = result then
          PtrArrayAdd(ExceptionNames, DOTNET_EXCEPTIONNAME[i]);
  end
  else
    result := 0;
end;

var
  _RawLogException: TOnRawLogException;

{$ifdef FPC}
  {$ifdef WIN64}
    {$define WITH_VECTOREXCEPT} // use AddVectoredExceptionHandler Win64 API
  {$else}
    {$ifdef FPC_USE_WIN32_SEH}
      {$define WITH_VECTOREXCEPT} // new since FPC 3.2
    {$else}
      // Win32, Linux: intercept via the RaiseProc global variable
      {$define WITH_RAISEPROC} // RaiseProc is set in main mormot.core.os.pas
    {$endif FPC_USE_WIN32_SEH}
  {$endif WIN64}
{$else}
  {$ifdef CPU64}
    {$define WITH_VECTOREXCEPT}
  {$else}
    {$define WITH_RTLUNWINDPROC} //  use x86_64 asm -> Win32 only
  {$endif CPU64}
{$endif FPC}

{$ifndef WITH_RAISEPROC}

type
  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = record
    ExceptionCode: DWord;
    ExceptionFlags: DWord;
    OuterException: PExceptionRecord;
    ExceptionAddress: PtrUInt;
    NumberParameters: integer;
    case {IsOsException:} boolean of
      true:
        (ExceptionInformation: array[0..14] of PtrUInt);
      false:
        (ExceptAddr: PtrUInt;
         ExceptObject: Exception);
  end;
  GetExceptionClass = function(const P: TExceptionRecord): ExceptClass;

const
  {$ifdef FPC}
  cRtlException = $E0465043; // $E0 F P C
  {$else}
  cRtlException = $0EEDFADE; // Delphi exception
  {$endif FPC}

procedure LogExcept(stack: PPtrUInt; const Exc: TExceptionRecord);
var
  ctxt: TSynLogExceptionContext;
  backuplasterror: DWord;
  backuphandler: TOnRawLogException;
begin
  if Exc.ExceptionCode = cSetThreadNameException then
    exit;
  backuplasterror := GetLastError;
  backuphandler := _RawLogException;
  if Assigned(backuphandler) then // paranoid check (tested before calling)
    try
      _RawLogException := nil; // disable nested exception
      ctxt.ECode := Exc.ExceptionCode;
      if (Exc.ExceptionCode = cRtlException) and
         (Exc.ExceptObject <> nil) then
      begin
        if Exc.ExceptObject.InheritsFrom(Exception) then
          ctxt.EClass := PPointer(Exc.ExceptObject)^
        else
          ctxt.EClass := EExternalException;
        ctxt.EInstance := Exc.ExceptObject;
        ctxt.ELevel := sllException;
        ctxt.EAddr := Exc.ExceptAddr;
      end
      else
      begin
        if Assigned(ExceptClsProc) then
          ctxt.EClass := GetExceptionClass(ExceptClsProc)(Exc)
        else
          ctxt.EClass := EExternal;
        ctxt.EInstance := nil;
        ctxt.ELevel := sllExceptionOS;
        ctxt.EAddr := Exc.ExceptionAddress;
      end;
      ctxt.EStack := pointer(stack);
      ctxt.EStackCount := 0;
      ctxt.ETimestamp := UnixTimeUtc; // fast API call
      backuphandler(ctxt);
    except
      { ignore any nested exception }
    end;
  _RawLogException := backuphandler;
  SetLastError(backuplasterror); // code above could have changed this
end;

{$ifdef WITH_VECTOREXCEPT}

type
  PExceptionInfo = ^TExceptionInfo;
  TExceptionInfo = packed record
    ExceptionRecord: PExceptionRecord;
    ContextRecord: pointer;
  end;

var
  AddVectoredExceptionHandler: function(FirstHandler: cardinal;
    VectoredHandler: pointer): PtrInt; stdcall;

function SynLogVectoredHandler(ExceptionInfo: PExceptionInfo): PtrInt; stdcall;
const
  EXCEPTION_CONTINUE_SEARCH = 0;
begin
  if Assigned(_RawLogException) then
    LogExcept(nil, ExceptionInfo^.ExceptionRecord^);
  result := EXCEPTION_CONTINUE_SEARCH;
end;

{$endif WITH_VECTOREXCEPT}

{$ifdef WITH_RTLUNWINDPROC}

var
  OldUnWindProc: pointer;

procedure SynRtlUnwind(TargetFrame, TargetIp: pointer;
  ExceptionRecord: PExceptionRecord; ReturnValue: Pointer); stdcall;
asm
        cmp     dword ptr _RawLogException, 0
        jz      @old
        pushad
        mov     eax, TargetFrame
        mov     edx, ExceptionRecord
        call    LogExcept
        popad
@old:   pop     ebp // hidden push ebp at asm level
        jmp     OldUnWindProc
end;

{$endif WITH_RTLUNWINDPROC}

{$endif WITH_RAISEPROC}

{$endif NOEXCEPTIONINTERCEPT}

{ TMemoryMap }

function TMemoryMap.DoMap(aCustomOffset: Int64): boolean;
begin
  with PInt64Rec(@fFileSize)^ do
    fMap := CreateFileMapping(fFile, nil, PAGE_READONLY, Hi, Lo, nil);
  if fMap = 0 then
    RaiseLastError('TMemoryMap.Map: CreateFileMapping');
  with PInt64Rec(@aCustomOffset)^ do
    fBuf := MapViewOfFile(fMap, FILE_MAP_READ, Hi, Lo, fBufSize);
  if fBuf = nil then
  begin
    // Windows failed to find a contiguous VA space -> fall back on direct read
    CloseHandle(fMap);
    fMap := 0;
  end;
  result := fMap <> 0;
end;

procedure TMemoryMap.DoUnMap;
begin
  if fMap <> 0 then
  begin
    UnmapViewOfFile(fBuf);
    CloseHandle(fMap);
    fMap := 0;
  end;
end;

const
  STUB_SIZE = 65536; // 16*4 KB (4 KB = memory granularity)

type
  TProcessMemoryCounters = record
    cb: DWord;
    PageFaultCount: DWord;
    PeakWorkingSetSize: PtrUInt;
    WorkingSetSize: PtrUInt;
    QuotaPeakPagedPoolUsage: PtrUInt;
    QuotaPagedPoolUsage: PtrUInt;
    QuotaPeakNonPagedPoolUsage: PtrUInt;
    QuotaNonPagedPoolUsage: PtrUInt;
    PagefileUsage: PtrUInt;
    PeakPagefileUsage: PtrUInt;
  end;

const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;

var
  // PROCESS_QUERY_INFORMATION (XP) / PROCESS_QUERY_LIMITED_INFORMATION (Vista+)
  OpenProcessAccess: DWord;

  // late-binding of Windows API entries not available on XP
  GetSystemTimes: function(
    var lpIdleTime, lpKernelTime, lpUserTime: TFileTime): BOOL; stdcall;
  GetProcessTimes: function(hProcess: THandle;
    var lpCreationTime, lpExitTime, lpKernelTime,
        lpUserTime: TFileTime): BOOL; stdcall;
  // Vista+/WS2008+ (use GetModuleFileNameEx on XP)
  QueryFullProcessImageNameW: function(hProcess: THandle; dwFlags: DWord;
    lpExeName: PWideChar; lpdwSize: PDWord): BOOL; stdcall;
  // PSAPI API late-binding via DelayedProc()
  GetProcessMemoryInfo: function(Process: THandle;
    var ppsmemCounters: TProcessMemoryCounters; cb: DWord): BOOL; stdcall;
  EnumProcesses: function(lpidProcess: PDWord; cb: DWord;
    var cbNeeded: DWord): BOOL; stdcall;
  GetModuleFileNameExW: function(hProcess: THandle; hModule: HMODULE;
    lpBaseName: PWideChar; nSize: DWord): DWord; stdcall;

function DelayedProc(var api; var lib: THandle;
  libname: PChar; procname: PAnsiChar): boolean;
var
  proc: pointer;
begin
  if pointer(api) = nil then
  begin
    proc := nil;
    GlobalLock; // avoid race condition
    if lib = 0 then
      lib := Windows.LoadLibrary(libname);
    if lib >= 32 then
      proc := Windows.GetProcAddress(lib, procname)
    else
      lib := 1; // try to load the library once
    if proc = nil then
      proc := pointer(1); // mark non available
    pointer(api) := proc; // set it last
    GlobalUnLock;
  end;
  result := pointer(api) <> pointer(1);
end;


function GetNextItem(var P: PAnsiChar): RawUtf8;
var
  S: PAnsiChar;
begin
  result := '';
  while P^ <= ' ' do
    if P^ = #0 then
      exit
    else
      inc(P);
  S := P;
  repeat
    inc(P);
  until P^ <= ' ';
  FastSetString(result, S, P - S);
end;

const
  PAGE_GUARD = $0100;
  PAGE_VALID = $00e6; // PAGE_READONLY or PAGE_READWRITE or PAGE_EXECUTE or
      // PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY

var
  LastMemInfo: TMemoryBasicInformation; // simple cache

function SeemsRealPointer(p: pointer): boolean;
var
  meminfo: TMemoryBasicInformation;
begin
  result := false;
  if PtrUInt(p) <= 65535 then
    exit; // first 64KB is not a valid pointer by definition
  if (LastMemInfo.State <> 0) and
     (PtrUInt(p) - PtrUInt(LastMemInfo.BaseAddress) <=
       PtrUInt(LastMemInfo.RegionSize)) then
    result := true // reuse last memory region information if we can
  else
  begin
    // VirtualQuery API is slow but better than raising an exception
    // see https://stackoverflow.com/a/37547837/458259
    FillCharFast(meminfo, SizeOf(meminfo), 0);
    result := (VirtualQuery(p, meminfo, SizeOf(meminfo)) = SizeOf(meminfo)) and
              (meminfo.RegionSize >= SizeOf(pointer)) and
              (meminfo.State = MEM_COMMIT) and
              (meminfo.Protect and PAGE_VALID <> 0) and
              (meminfo.Protect and PAGE_GUARD = 0);
    if result then
      LastMemInfo := meminfo;
  end;
end;

var
  PsapiDll: THandle;

function EnumAllProcesses: TCardinalDynArray;
var
  n, count: cardinal;
begin
  result := nil;
  if not DelayedProc(EnumProcesses, PsapiDll, 'Psapi.dll', 'EnumProcesses') then
    exit;
  count := 0;
  n := 2048; // retrieve 2KB of IDs, i.e. 512 processes, by default
  repeat
    SetLength(result, n);
    if EnumProcesses(pointer(result), n * 4, count) then
      count := count shr 2 // from bytes to count
    else
      count := 0; // error
    if count < n then
      break;
    // count=n if buffer was too small
    inc(n, 1024);
  until n > 8192;
  if count = 0 then
    result := nil // on error
  else
    DynArrayFakeLength(result, count); // no realloc
end;

function EnumProcessName(PID: cardinal): RawUtf8;
var
  h: THandle;
  len: DWord;
  name: array[0..4095] of WideChar;
begin
  result := '';
  if PID = 0 then
    exit;
  h := OpenProcess(OpenProcessAccess, false, PID);
  if h <> 0 then
    try
      if Assigned(QueryFullProcessImageNameW) then
      begin
        len := high(name);
        if QueryFullProcessImageNameW(h, 0, name, @len) then
          Win32PWideCharToUtf8(name, len, result);
      end
      else if DelayedProc(GetModuleFileNameExW, PsapiDll, 'Psapi.dll',
                'GetModuleFileNameExW') and
              (GetModuleFileNameExW(h, 0, name, high(name)) <> 0) then
        Win32PWideCharToUtf8(name, result);
    finally
      CloseHandle(h);
    end;
end;

// some definitions missing on Delphi and/or FPC
type
  TProcessEntry32 = record
    dwSize: DWORD;
    cntUsage: DWORD;
    th32ProcessID: DWORD;          // this process
    th32DefaultHeapID: PtrUInt;
    th32ModuleID:DWORD;            // associated exe
    cntThreads: DWORD;
    th32ParentProcessID: DWORD;    // this process's parent process
    pcPriClassBase: integer;          // Base priority of process's threads
    dwFlags: DWORD;
    szExeFile: array [0..MAX_PATH - 1] of WideChar;   // Path
  end;

  TThreadEntry32 = record
    dwSize: DWord;
    cntUsage: DWord;
    th32ThreadID: DWord;       // this thread
    th32OwnerProcessID: DWord; // Process this thread is associated with
    tpBasePri: integer;
    tpDeltaPri: integer;
    dwFlags: DWord;
  end;

const
  TH32CS_SNAPPROCESS  = $00000002;
  TH32CS_SNAPTHREAD = $00000004;

function AttachConsole(pid: cardinal): BOOL;
  stdcall; external kernel32;
function GetConsoleWindow: HWND;
  stdcall; external kernel32;
function CreateToolhelp32Snapshot(dwFlags, th32ProcessID: DWord): THandle;
  stdcall; external kernel32;
function Process32FirstW(hSnapshot: THandle; var lppe: TProcessEntry32): BOOL;
  stdcall; external kernel32;
function Process32NextW(hSnapshot: THandle; var lppe: TProcessEntry32): BOOL;
  stdcall; external kernel32;
function Thread32First(hSnapshot: THandle; var lpte: TThreadEntry32): BOOL;
  stdcall; external kernel32;
function Thread32Next(hSnapshot: THandle; var lpte: TThreadEntry32): BOOL;
  stdcall; external kernel32;

function PostThreadMessage(idThread: DWord; Msg: UINT; wParam: WPARAM;
    lParam: LPARAM): BOOL;
  stdcall; external user32 name 'PostThreadMessageW';

function RawProcessInfo(pid: cardinal; var e: TProcessEntry32): boolean;
var
  snap: THandle;
begin
  result := false;
  if integer(pid) <= 0 then
    pid := GetCurrentProcessId;
  snap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if snap <= 0 then
    exit;
  FillCharFast(e, SizeOf(e), 0);
  e.dwSize := SizeOf(e);
  result := true;
  if Process32FirstW(snap, e) then // loop over all processes of the system
    repeat
      if e.th32ProcessID = pid then
        exit;
    until not Process32NextW(snap, e);
  CloseHandle(snap);
  result := false;
end;

function GetParentProcess(PID: cardinal): cardinal;
var
  e: TProcessEntry32;
begin
  if RawProcessInfo(PID, e) then
    result := e.th32ParentProcessID
  else
    result := 0;
end;

function RetrieveSystemTimes(out IdleTime, KernelTime, UserTime: Int64): boolean;
var
  ftidl, ftkrn, ftusr: TFileTime;
begin
  result := Assigned(GetSystemTimes) and
            GetSystemTimes(ftidl, ftkrn, ftusr);
  if not result then
    exit;
  FileTimeToInt64(ftidl, IdleTime);
  FileTimeToInt64(ftkrn, KernelTime);
  FileTimeToInt64(ftusr, UserTime);
end;

function RetrieveLoadAvg: RawUtf8;
begin
  result := ''; // call RetrieveSystemTimes() instead
end;

function DelayedGetProcessMemoryInfo: boolean;
begin
  result:= DelayedProc(GetProcessMemoryInfo, PsapiDll, 'Psapi.dll',
    'GetProcessMemoryInfo');
end;

function RetrieveProcessInfo(PID: cardinal; out KernelTime, UserTime: Int64;
  out WorkKB, VirtualKB: cardinal): boolean;
var
  h: THandle;
  ftkrn, ftusr, ftp, fte: TFileTime;
  mem: TProcessMemoryCounters;
begin
  result := false;
  if (not Assigned(GetProcessTimes)) or
     (not DelayedGetProcessMemoryInfo) then
    exit;
  h := OpenProcess(OpenProcessAccess, false, PID);
  if h = 0 then
    exit;
  try
    if GetProcessTimes(h, ftp, fte, ftkrn, ftusr) then
    begin
      FileTimeToInt64(ftkrn, KernelTime);
      FileTimeToInt64(ftusr, UserTime);
      FillCharFast(mem, SizeOf(mem), 0);
      mem.cb := SizeOf(mem);
      if GetProcessMemoryInfo(h, mem, SizeOf(mem)) then
      begin
        WorkKB := mem.WorkingSetSize shr 10;
        VirtualKB := mem.PagefileUsage shr 10;
      end;
      result := true;
    end;
  finally
    CloseHandle(h);
  end;
end;

function CoCreateGuid(out guid: THash128Rec): HRESULT;
  stdcall; external 'ole32.dll';

procedure XorOSEntropy(var e: THash512Rec);
var
  h: THash128Rec;
  ft: packed record
    krn, usr, p, e: TFileTime;
  end;
  mem: TProcessMemoryCounters;
begin
  QueryPerformanceCounter(h.Lo); // e.h3 xored with raw timestamps
  e.i[6] := e.i[6] xor h.Lo;
  if Assigned(GetProcessTimes) then
    GetProcessTimes(GetCurrentProcess, ft.p, ft.e, ft.krn, ft.usr);
  DefaultHasher128(@e.h0, @ft, SizeOf(ft));
  if Assigned(GetProcessMemoryInfo) then // may have been delayed
  begin
    mem.cb := SizeOf(mem);
    GetProcessMemoryInfo(GetCurrentProcess, mem, SizeOf(mem));
    DefaultHasher128(@e.h1, @mem, SizeOf(mem));
  end;
  if Assigned(GetSystemTimes) then
    GetSystemTimes(ft.usr, ft.p, ft.e);
  DefaultHasher128(@e.h2, @ft, SizeOf(ft));
  CoCreateGuid(h); // very fast on Windows - used to obfuscate system info
  e.i[0] := e.i[0] xor h.Lo;
  e.i[1] := e.i[1] xor h.Hi;
  CoCreateGuid(h);
  e.i[2] := e.i[2] xor h.Lo;
  e.i[3] := e.i[3] xor h.Hi;
  CoCreateGuid(h);
  e.i[4] := e.i[4] xor h.Lo;
  e.i[5] := e.i[5] xor h.Hi;
  CoCreateGuid(h);
  e.i[6] := e.i[6] xor h.Lo;
  e.i[7] := e.i[7] xor h.Hi;
  QueryPerformanceCounter(h.Lo); // is likely to have changed in-between
  e.i[7] := e.i[7] xor h.Lo;     // e.h3 xored with raw timestamps
end;

function FillSystemRandom(Buffer: PByteArray; Len: integer;
  AllowBlocking: boolean): boolean;
var
  prov: HCRYPTPROV;
begin
  result := false;
  if Len <= 0 then
    exit;
  // warning: on some Windows versions, this could take up to 30 ms!
  if CryptoApi.Available then
    if CryptoApi.AcquireContextA(prov, nil, nil,
      PROV_RSA_FULL, CRYPT_VERIFYCONTEXT) then
    begin
      result := CryptoApi.GenRandom(prov, Len, Buffer);
      CryptoApi.ReleaseContext(prov, 0);
    end;
  if not result then
    // OS API call failed -> fallback to our Lecuyer's gsl_rng_taus2 generator
    RandomBytes(pointer(Buffer), Len);
end;

function TProcessInfo.Init: boolean;
begin
  FillCharFast(self, SizeOf(self), 0);
  // no monitoring API available under oldest Windows
  result := Assigned(GetSystemTimes) and
            Assigned(GetProcessTimes) and
            DelayedGetProcessMemoryInfo;
end;

function TProcessInfo.Start: boolean;
var
  ftidl, ftkrn, ftusr: TFileTime;
  sidl, skrn, susr: Int64;
begin
  result := Assigned(GetSystemTimes) and
            GetSystemTimes(ftidl, ftkrn, ftusr);
  if not result then
    exit;
  FileTimeToInt64(ftidl, sidl);
  FileTimeToInt64(ftkrn, skrn);
  FileTimeToInt64(ftusr, susr);
  fDiffIdle := sidl - fSysPrevIdle;
  fDiffKernel := skrn - fSysPrevKernel;
  fDiffUser := susr - fSysPrevUser;
  fDiffTotal := fDiffKernel + fDiffUser; // kernel time also includes idle time
  dec(fDiffKernel, fDiffIdle);
  fSysPrevIdle := sidl;
  fSysPrevKernel := skrn;
  fSysPrevUser := susr;
end;

function TProcessInfo.PerProcess(PID: cardinal; Now: PDateTime;
  out Data: TSystemUseData; var PrevKernel, PrevUser: Int64): boolean;
var
  h: THandle;
  ftkrn, ftusr, ftp, fte: TFileTime;
  pkrn, pusr: Int64;
  mem: TProcessMemoryCounters;
begin
  result := false;
  FillCharFast(Data, SizeOf(Data), 0);
  h := OpenProcess(OpenProcessAccess, false, PID);
  if h <> 0 then
  try
    if GetProcessTimes(h, ftp, fte, ftkrn, ftusr) then
    begin
      if Now <> nil then
        Data.Timestamp := Now^;
      FileTimeToInt64(ftkrn, pkrn);
      FileTimeToInt64(ftusr, pusr);
      if (PrevKernel <> 0) and
         (fDiffTotal > 0) then
      begin
        Data.Kernel := ((pkrn - PrevKernel) * 100) / fDiffTotal;
        Data.User   := ((pusr - PrevUser)   * 100) / fDiffTotal;
      end;
      PrevKernel := pkrn;
      PrevUser := pusr;
      FillCharFast(mem, SizeOf(mem), 0);
      mem.cb := SizeOf(mem);
      if GetProcessMemoryInfo(h, mem, SizeOf(mem)) then
      begin
        Data.WorkKB    := mem.WorkingSetSize shr 10;
        Data.VirtualKB := mem.PagefileUsage  shr 10;
      end;
      result := true;
    end;
  finally
    CloseHandle(h);
  end;
end;

function TProcessInfo.PerSystem(out Idle, Kernel, User: single): boolean;
begin
  if fDiffTotal <= 0 then
  begin
    Idle   := 0;
    Kernel := 0;
    User   := 0;
    result := false;
  end
  else
  begin
    Kernel := {%H-}SimpleRoundTo2Digits((fDiffKernel * 100) / fDiffTotal);
    User   := {%H-}SimpleRoundTo2Digits((fDiffUser * 100)   / fDiffTotal);
    Idle   := 100 - Kernel - User; // ensure sum is always 100%
    result := true;
  end;
end;

{$ifndef UNICODE} // 64-bit aware Windows API missing on FPC and oldest Delphi

type
  TMemoryStatusEx = record
    dwLength: DWord;
    dwMemoryLoad: DWord;
    ullTotalPhys: QWord;
    ullAvailPhys: QWord;
    ullTotalPageFile: QWord;
    ullAvailPageFile: QWord;
    ullTotalVirtual: QWord;
    ullAvailVirtual: QWord;
    ullAvailExtendedVirtual: QWord;
  end;

// information about the system's current usage of both physical and virtual memory
function GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx): BOOL;
  stdcall; external kernel32;

{$endif UNICODE}

function GetMemoryInfo(out info: TMemoryInfo; withalloc: boolean): boolean;
{$ifdef WITH_FASTMM4STATS}
var
  Heap: TMemoryManagerState;
  sb: PtrInt;
{$endif WITH_FASTMM4STATS}
var
  global: TMemoryStatusEx;
  mem: TProcessMemoryCounters;
begin
  FillCharFast(global, SizeOf(global), 0);
  global.dwLength := SizeOf(global);
  result := GlobalMemoryStatusEx(global);
  info.percent   := global.dwMemoryLoad;
  info.memtotal  := global.ullTotalPhys;
  info.memfree   := global.ullAvailPhys;
  info.filetotal := global.ullTotalPageFile;
  info.filefree  := global.ullAvailPageFile;
  info.vmtotal   := global.ullTotalVirtual;
  info.vmfree    := global.ullAvailVirtual;
  info.allocreserved := 0;
  info.allocused     := 0;
  if not withalloc then
    exit;
  {$ifdef WITH_FASTMM4STATS} // override OS information by actual FastMM4
  GetMemoryManagerState(Heap); // direct raw FastMM4 access
  info.allocused := Heap.TotalAllocatedMediumBlockSize +
                    Heap.TotalAllocatedLargeBlockSize;
  info.allocreserved := Heap.ReservedMediumBlockAddressSpace +
                        Heap.ReservedLargeBlockAddressSpace;
  for sb := 0 to high(Heap.SmallBlockTypeStates) do
    with Heap.SmallBlockTypeStates[sb] do
    begin
      inc(info.allocused, UseableBlockSize * AllocatedBlockCount);
      inc(info.allocreserved, ReservedAddressSpace);
    end;
  {$else}
  if not DelayedGetProcessMemoryInfo then
    exit;
  FillcharFast(mem, SizeOf(mem), 0);
  mem.cb := SizeOf(mem);
  GetProcessMemoryInfo(GetCurrentProcess, mem, SizeOf(mem));
  info.allocreserved := mem.PeakWorkingSetSize;
  info.allocused     := mem.WorkingSetSize;
  {$endif WITH_FASTMM4STATS}
end;

function GetDiskFreeSpaceExW(lpDirectoryName: PWideChar;
  var lpFreeBytesAvailableToCaller, lpTotalNumberOfBytes,
  lpTotalNumberOfFreeBytes: QWord): LongBool;
   stdcall; external kernel32;

{
// DeviceIoControl(IOCTL_DISK_GET_PARTITION_INFO) requires root -> not used
function DeviceIoControl(hDevice: THandle; dwIoControlCode: DWord;
  lpInBuffer: Pointer; nInBufferSize: DWord; lpOutBuffer: Pointer;
  nOutBufferSize: DWord; var lpBytesReturned: DWord;
  lpOverlapped: POverlapped): BOOL; stdcall; external kernel32;
}

function GetDiskInfo(var aDriveFolderOrFile: TFileName;
  out aAvailableBytes, aFreeBytes, aTotalBytes: QWord;
  aVolumeName: PSynUnicode): boolean;
var
  tmp: array[0..MAX_PATH - 1] of WideChar;
  dummy, flags: DWord;
  dn: SynUnicode;
begin
  if aDriveFolderOrFile = '' then
    aDriveFolderOrFile := SysUtils.UpperCase(
      ExtractFileDrive(Executable.ProgramFilePath));
  dn := SynUnicode(aDriveFolderOrFile); // use RTL for UTF-16 conversion
  if (dn <> '') and
     (dn[2] = ':') and
     (dn[3] = #0) then
    dn := dn + '\';
  if (aVolumeName <> nil) and
     (aVolumeName^ = '') then
  begin
    tmp[0] := #0;
    GetVolumeInformationW(pointer(dn), tmp, MAX_PATH, nil, dummy, flags, nil, 0);
    aVolumeName^ := tmp;
  end;
  result := GetDiskFreeSpaceExW(pointer(dn),
    aAvailableBytes, aTotalBytes, aFreeBytes);
end;

function GetDiskPartitions: TDiskPartitions;
var
  drives, drive, m, n: integer;
  fn: TFileName;
  volume: SynUnicode;
  av, fr, tot: QWord;
  p: ^TDiskPartition;
begin
  result := nil;
  n := 0;
  fn := '#:';
  drives := GetLogicalDrives;
  m := 1 shl 2; // bit 2 = drive C
  for drive := 3 to 26 do
  begin
    // retrieve partitions mounted as C..Z drives
    if drives and m <> 0 then
    begin
      fn[1] := char(64 + drive);
      if GetDiskInfo(fn, av, fr, tot, @volume) then
      begin
        SetLength(result, n + 1);
        p := @result[n];
        Win32PWideCharToUtf8(pointer(volume), length(volume), p^.name);
        p^.mounted := fn;
        p^.size := tot;
        volume := '';
        inc(n);
      end;
    end;
    m := m shl 1;
  end;
end;

var
  TextAttr: integer = ord(ccDarkGray);

procedure TextColor(Color: TConsoleColor);
var
  oldAttr: integer;
begin
  if not HasConsole then
    exit;
  oldAttr := TextAttr;
  TextAttr := (TextAttr and $F0) or ord(Color);
  if TextAttr <> oldAttr then
    SetConsoleTextAttribute(StdOut, TextAttr);
end;

procedure TextBackground(Color: TConsoleColor);
var
  oldAttr: integer;
begin
  if not HasConsole then
    exit;
  oldAttr := TextAttr;
  TextAttr := (TextAttr and $0F) or (ord(Color) shl 4);
  if TextAttr <> oldAttr then
    SetConsoleTextAttribute(StdOut, TextAttr);
end;

var
  ConsoleCriticalSection: TOSLock;

procedure ConsoleWrite(const Text: RawUtf8; Color: TConsoleColor;
  NoLineFeed, NoColor: boolean);
var
  txt: RawByteString;
  l: PtrInt;
  written: cardinal;
begin
  if not HasConsole then
    exit;
  txt := Utf8ToConsole(Text);
  l := length(txt);
  if not NoLineFeed then
  begin
    SetLength(txt, l + 2); // faster to reallocate than WriteFile() twice
    PWord(@PByteArray(txt)[l])^ := $0a0d; // CRLF
    inc(l, 2);
  end;
  ConsoleCriticalSection.Lock;
  try
    if not NoColor then
      TextColor(Color);
    WriteFile(StdOut, pointer(txt)^, l, written, nil);
    // FlushFileBuffers(StdOut); // don't: would block until read on the pipe
  finally
    ConsoleCriticalSection.UnLock;
  end;
end;

function ConsoleKeyPressed(ExpectedKey: Word): boolean;
var
  events, read: DWord;
  rec: TInputRecord;
  h: THandle;
begin
  result := false;
  h := GetStdHandle(STD_INPUT_HANDLE);
  events := 0;
  GetNumberOfConsoleInputEvents(h, events);
  if events <> 0 then
  begin
    PeekConsoleInput(h, rec, 1, read);
    if read <> 0 then
      if rec.EventType = KEY_EVENT then
        if rec.Event.KeyEvent.bKeyDown and
           ((ExpectedKey = 0) or
            (rec.Event.KeyEvent.wVirtualKeyCode = ExpectedKey)) then
          result := true
        else
          FlushConsoleInputBuffer(h)
      else
        FlushConsoleInputBuffer(h);
  end;
end;

type
  TConsoleHandleCtrlC = class
  private
    class procedure HandleCtrlC;
  end;
var
  ConsoleHandleCtrlCPressed: boolean;

class procedure TConsoleHandleCtrlC.HandleCtrlC;
begin
  ConsoleHandleCtrlCPressed := true;
end;

procedure ConsoleWaitForEnterKey;
var
  msg: TMsg;
begin
  ConsoleHandleCtrlCPressed := false;
  HandleCtrlC(TConsoleHandleCtrlC.HandleCtrlC);
  try
    if GetCurrentThreadID = MainThreadID then
      // process the messages from the main thread while waiting
      while not ConsoleKeyPressed(VK_RETURN) and
            not ConsoleHandleCtrlCPressed do
      begin
        if IsMultiThread then
          CheckSynchronize{$ifndef DELPHI6OROLDER}(100){$endif}
        else
          Sleep(100);
        while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do
          if msg.Message = WM_QUIT then
            exit // stop waiting when the process is gracefully closing
          else
          begin
            TranslateMessage(msg);
            DispatchMessage(msg);
          end;
      end
    else
      // just intercept any WM_QUIT message on this sub-thread
      while not ConsoleKeyPressed(VK_RETURN) and
            not ConsoleHandleCtrlCPressed do
      begin
        Sleep(100);
        if PeekMessage(msg, 0, 0, 0, PM_REMOVE) then
          if msg.Message = WM_QUIT then
            exit; // nothing to dispatch with PostThreadMessage()
      end;
  finally
    HandleCtrlC(nil);
  end;
end;
{$I+}

{$ifdef ISDELPHI}
var
  // Delphi doesn't define this global variable need by ConsoleReadBody
  StdInputHandle: THandle;
{$endif ISDELPHI}

function ConsoleStdInputLen: integer;
begin
  if StdInputHandle = 0 then
    StdInputHandle := GetStdHandle(STD_INPUT_HANDLE);
  if not PeekNamedPipe(StdInputHandle, nil, 0, nil, @result, nil) then
    result := 0;
end;

procedure Utf8ToConsoleDoConv(const Utf8: RawUtf8; var Console: RawByteString);
var
  tmp16, tmp: TSynTempBuffer;
begin
  Utf8ToWin32PWideChar(Utf8, tmp16);
  if tmp16.len = 0 then
  begin
    Console := Utf8; // input is not valid UTF-8 -> return without conversion
    exit;
  end;
  tmp.Init(tmp16.len * 3);
  CharToOemBuffW(tmp16.buf, tmp.buf, tmp16.len + 1); // +1 = ending #0
  tmp16.Done;
  FastSetStringCP(Console, tmp.buf, StrLen(tmp.buf), CP_OEMCP);
  tmp.Done;
end;

function Utf8ToConsole(const S: RawUtf8): RawByteString;
begin
  if IsAnsiCompatible(S) then
    result := S // no conversion needed
  else
    Utf8ToConsoleDoConv(S, result);
end;

function TFileVersion.RetrieveInformationFromFileName: boolean;
var
  Size, Size2: DWord;
  Pt: Pointer;
  Trans: PWordArray;
  LanguageInfo: RawUtf8;
  Info: ^TVSFixedFileInfo;
  FileTime: TFILETIME;
  SystemTime: TSYSTEMTIME;
  tmp: TW32Temp;

  function ReadResourceByName(const From: RawUtf8): RawUtf8;
  var
    StrValPt: pointer;
    sz: DWord;
    u: SynUnicode;
  begin
    u := Utf8Decode('\StringFileInfo\' + LanguageInfo + '\' + From);
    VerQueryValueW(Pt, pointer(u), StrValPt, sz);
    if sz > 0 then
      Win32PWideCharToUtf8(StrValPt, result)
    else
      result := '';
  end;

begin
  result := false;
  if fFileName = '' then
    exit;
  // GetFileVersionInfo() modifies the filename parameter data while parsing
  // -> copy the FileName into local tmp buffer to create a writable copy
  Size := GetFileVersionInfoSizeW(W32Copy(fFileName, tmp), Size2);
  if Size > 0 then
  begin
    GetMem(Pt, Size);
    try
      if GetFileVersionInfoW(W32Copy(fFileName, tmp), 0, Size, Pt) then
      begin
        VerQueryValueW(Pt, '\', pointer(Info), Size2);
        with Info^ do
        begin
          SetVersion({major=}   dwFileVersionMS shr 16,
                     {minor=}   word(dwFileVersionMS),
                     {release=} dwFileVersionLS shr 16,
                     {build=}   word(dwFileVersionLS));
          if (dwFileDateLS <> 0) and
             (dwFileDateMS <> 0) then
          begin
            FileTime.dwLowDateTime := dwFileDateLS; // built date from version info
            FileTime.dwHighDateTime := dwFileDateMS;
            FileTimeToSystemTime(FileTime, SystemTime);
            fBuildDateTime := EncodeDate(
              SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay);
          end;
        end;
        VerQueryValueW(Pt, '\VarFileInfo\Translation', pointer(Trans), Size2);
        if Size2 >= 4 then
        begin
          _fmt('%4.4x%4.4x', [Trans^[0], Trans^[1]], LanguageInfo);
          CompanyName      := ReadResourceByName('CompanyName');
          FileDescription  := ReadResourceByName('FileDescription');
          FileVersion      := ReadResourceByName('FileVersion');
          InternalName     := ReadResourceByName('InternalName');
          LegalCopyright   := ReadResourceByName('LegalCopyright');
          OriginalFilename := ReadResourceByName('OriginalFilename');
          ProductName      := ReadResourceByName('ProductName');
          ProductVersion   := ReadResourceByName('ProductVersion');
          Comments         := ReadResourceByName('Comments');
        end;
        result := true;
      end;
    finally
      Freemem(Pt);
    end;
  end;
end;

procedure GetUserHost(out User, Host: RawUtf8);
var
  tmp: array[byte] of WideChar;
  tmpsize: cardinal;
begin
  tmpsize := SizeOf(tmp);
  GetComputerNameW(tmp{%H-}, tmpsize);
  Win32PWideCharToUtf8(@tmp, Host);
  tmpsize := SizeOf(tmp);
  GetUserNameW(tmp, tmpsize);
  Win32PWideCharToUtf8(@tmp, User);
end;

var
  SHFolderDll: THandle;
  // avoid unneeded reference to ShlObj.pas
  // - late binding is mandatory to be used on WinPE which does NOT have this dll
  // - late binding also ensure that we load libraries only when needed
  SHGetFolderPath: function(hwnd: hwnd; csidl: integer; hToken: THandle;
    dwFlags: DWord; pszPath: PChar): HRESULT; stdcall;

const
  CSIDL_PERSONAL         = $0005;
  CSIDL_LOCAL_APPDATA    = $001C; // local non roaming user folder
  CSIDL_COMMON_APPDATA   = $0023;
  CSIDL_COMMON_DOCUMENTS = $002E;

  CSIDL: array[TSystemPath] of integer = (
    CSIDL_COMMON_APPDATA,   // spCommonData
                            // C:\ProgramData
    CSIDL_LOCAL_APPDATA,    // spUserData
                            // C:\Users\<user>\AppData\Local
    CSIDL_COMMON_DOCUMENTS, // spCommonDocuments
                            // C:\Users\Public\Documents
    CSIDL_PERSONAL,         // spUserDocuments
                            // C:\Users\<user>\Documents
    0,                      // spTemp
    0);                     // spLog
  // note: for SYSTEM user, got C:\Windows\System32\config\systemprofile\AppData
  // or C:\Windows\SysWOW64\config\systemprofile\AppData (on Win32 over Win64)

procedure _ComputeSystemPath(kind: TSystemPath; var result: TFileName);
const
  _ENV: array[TSystemPath] of TFileName = (
    'ALLUSERSAPPDATA', // spCommonData
    'LOCALAPPDATA',    // spUserData
    '',                // spCommonDocuments
    '',                // spUserDocuments
    'TEMP',            // spTemp
    '');               // spLog
var
  tmp: array[0..MAX_PATH] of char;
begin
  result := '';
  case kind of
    spLog:
      begin
        // try <exepath>\log - without [idwExcludeWinSys] (writable is enough)
        result := Executable.ProgramFilePath;
        if not IsDirectoryWritable(result) then
          // fallback to 'C:\Users\<user>\AppData\Local\<exename>-log'
          result := format('%s%s-',
                      [GetSystemPath(spUserData), Executable.ProgramName]);
        result := EnsureDirectoryExists(result + 'log');
        if IsDirectoryWritable(result) then
          exit; // found a folder able to receive new logs
        // 'C:\Users\<user>\AppData\Local\Temp\<exename>-log'
        result := EnsureDirectoryExists(format('%s%s-log',
                    [GetSystemPath(spTemp), Executable.ProgramName]));
      end;
    spTemp:
      begin
        // typically 'C:\Users\<user>\AppData\Local\Temp'
        if GetTempPath(MAX_PATH, @tmp) <> 0 then
          result := tmp; // retrieve from dedicated standard API
        if result = '' then
          result := GetEnvironmentVariable(_ENV[spTemp]); // fallback
      end;
  else
    if (CSIDL[kind] <> 0) and
       DelayedProc(SHGetFolderPath, SHFolderDll, 'SHFolder.dll',
         'SHGetFolderPath' + _AW) and
       (SHGetFolderPath(0, CSIDL[kind], 0, 0, @tmp) = S_OK) then
      // retrieved from official CSIDL
      result := tmp
    else
    begin
      // fallback to environment variables (very unlikely)
      result := GetEnvironmentVariable(_ENV[kind]);
      if result = '' then
      begin
        result := GetEnvironmentVariable('APPDATA');
        if result = '' then
          result := Executable.ProgramFilePath;
      end;
    end;
  end;
  if result <> '' then
    result := IncludeTrailingPathDelimiter(result); // no EnsureDirectoryExists
end;

procedure PatchCode(Old, New: pointer; Size: PtrInt; Backup: pointer;
  LeaveUnprotected: boolean);
var
  RestoreProtection, Ignore: DWord;
  i: PtrInt;
begin
  if VirtualProtect(Old, Size, PAGE_EXECUTE_READWRITE, RestoreProtection) then
  begin
    if Backup <> nil then
      for i := 0 to Size - 1 do  // do not use Move() here
        PByteArray(Backup)^[i] := PByteArray(Old)^[i];
    for i := 0 to Size - 1 do    // do not use Move() here
      PByteArray(Old)^[i] := PByteArray(New)^[i];
    if not LeaveUnprotected then
      VirtualProtect(Old, Size, RestoreProtection, Ignore);
    FlushInstructionCache(GetCurrentProcess, Old, Size);
    if not CompareMemFixed(Old, New, Size) then
      raise Exception.Create('PatchCode?');
  end;
end;


{ ****************** Operating System Specific Types (e.g. TWinRegistry) }

{ TWinRegistry }

const
  _HKEY: array[TWinRegistryRoot] of HKEY = (
    HKEY_CLASSES_ROOT,
    HKEY_CURRENT_USER,
    HKEY_LOCAL_MACHINE,
    HKEY_USERS);

function TWinRegistry.ReadOpen(root: TWinRegistryRoot; const keyname: RawUtf8;
  closefirst: boolean): boolean;
var
  tmp: TSynTempBuffer;
begin
  if closefirst then
    Close;
  key := 0;
  result := RegOpenKeyExW(
    _HKEY[root], Utf8ToWin32PWideChar(keyname, tmp), 0, KEY_READ, key) = 0;
  tmp.Done;
end;

procedure TWinRegistry.Close;
begin
  if key <> 0 then
    RegCloseKey(key);
end;

function TWinRegistry.ReadString(const entry: SynUnicode; andtrim: boolean): RawUtf8;
var
  rtype, rsize, res: DWord;
  tmp: TSynTempBuffer;
begin
  result := '';
  rsize := {%H-}tmp.Init; // most of the time, a single call is enough
  res := RegQueryValueExW(key, pointer(entry), nil, @rtype, tmp.buf, @rsize);
  if res <> 0 then
    if res = ERROR_MORE_DATA then
    begin
      tmp.Init(rsize); // more than 4KB of data (unlikely)
      res := RegQueryValueExW(key, pointer(entry), nil, nil, tmp.buf, @rsize);
    end
    else
      exit;
  if res = 0 then
  begin
    case rtype of
      REG_SZ,
      REG_EXPAND_SZ,
      REG_MULTI_SZ: // StrLen() will return the first value of REG_MULTI_SZ
        Win32PWideCharToUtf8(tmp.buf, result);
    end;
    if andtrim then
      TrimSelf(result);
  end;
  tmp.Done;
end;

function TWinRegistry.ReadData(const entry: SynUnicode): RawByteString;
var
  rsize: DWord;
begin
  result := '';
  if RegQueryValueExW(key, pointer(entry), nil, nil, nil, @rsize) <> 0 then
    exit;
  SetLength(result, rsize);
  if RegQueryValueExW(key, pointer(entry), nil, nil, pointer(result), @rsize) <> 0 then
    result := '';
end;

function TWinRegistry.ReadDword(const entry: SynUnicode): cardinal;
begin
  if not ReadBuffer(entry, @result, SizeOf(result)) then
    result := 0;
end;

function TWinRegistry.ReadQword(const entry: SynUnicode): QWord;
begin
  if not ReadBuffer(entry, @result, SizeOf(result)) then
    result := 0;
end;

function TWinRegistry.ReadBuffer(const entry: SynUnicode;
  Data: pointer; DataLen: DWord): boolean;
begin
  result := RegQueryValueExW(key, pointer(entry), nil, nil, Data, @DataLen) = 0;
end;

function TWinRegistry.ReadSize(const entry: SynUnicode): integer;
begin
  if RegQueryValueExW(key, pointer(entry), nil, nil, nil, @result) <> 0 then
    result := -1;
end;

function TWinRegistry.ReadEnumEntries: TRawUtf8DynArray;
var
  count, maxlen, i, len: DWord;
  tmp: TSynTempBuffer;
begin
  result := nil;
  count := 0;
  if (RegQueryInfoKeyW(key, nil, nil, nil, @count, @maxlen,
       nil, nil, nil, nil, nil, nil) <> 0) or
     (count = 0) then
    exit;
  SetLength(result, count);
  inc(maxlen);
  tmp.Init(maxlen * 2);
  for i := 0 to count - 1 do
  begin
    len := maxlen;
    if RegEnumKeyExW(key, i, tmp.buf, len, nil, nil, nil, nil) = 0 then
      Win32PWideCharToUtf8(tmp.buf, len, result[i]);
  end;
  tmp.Done;
end;


const
  _WSP: array[TWinSystemPrivilege] of string[32] = (
    // note: string[32] to ensure there is a trailing #0 for all items
    'SeCreateTokenPrivilege',          // wspCreateToken
    'SeAssignPrimaryTokenPrivilege',   // wspAssignPrimaryToken
    'SeLockMemoryPrivilege',           // wspLockMemory
    'SeIncreaseQuotaPrivilege',        // wspIncreaseQuota
    'SeUnsolicitedInputPrivilege',     // wspUnsolicitedInput
    'SeMachineAccountPrivilege',       // wspMachineAccount
    'SeTcbPrivilege',                  // wspTCP
    'SeSecurityPrivilege',             // wspSecurity
    'SeTakeOwnershipPrivilege',        // wspTakeOwnership
    'SeLoadDriverPrivilege',           // wspLoadDriver
    'SeSystemProfilePrivilege',        // wspSystemProfile
    'SeSystemtimePrivilege',           // wspSystemTime
    'SeProfileSingleProcessPrivilege', // wspProfSingleProcess
    'SeIncreaseBasePriorityPrivilege', // wspIncBasePriority
    'SeCreatePagefilePrivilege',       // wspCreatePageFile
    'SeCreatePermanentPrivilege',      // wspCreatePermanent
    'SeBackupPrivilege',               // wspBackup
    'SeRestorePrivilege',              // wspRestore
    'SeShutdownPrivilege',             // wspShutdown
    'SeDebugPrivilege',                // wspDebug
    'SeAuditPrivilege',                // wspAudit
    'SeSystemEnvironmentPrivilege',    // wspSystemEnvironment
    'SeChangeNotifyPrivilege',         // wspChangeNotify
    'SeRemoteShutdownPrivilege',       // wspRemoteShutdown
    'SeUndockPrivilege',               // wspUndock
    'SeSyncAgentPrivilege',            // wspSyncAgent
    'SeEnableDelegationPrivilege',     // wspEnableDelegation
    'SeManageVolumePrivilege',         // wspManageVolume
    'SeImpersonatePrivilege',          // wspImpersonate
    'SeCreateGlobalPrivilege',         // wspCreateGlobal
    'SeTrustedCredManAccessPrivilege', // wspTrustedCredmanAccess
    'SeRelabelPrivilege',              // wspRelabel
    'SeIncreaseWorkingSetPrivilege',   // wspIncWorkingSet
    'SeTimeZonePrivilege',             // wspTimeZone
    'SeCreateSymbolicLinkPrivilege');  // wspCreateSymbolicLink

  _TokenVirtualizationEnabled = TTokenInformationClass(24); // for oldest Delphi

type
  TOKEN_PRIVILEGES = packed record
    PrivilegeCount : DWord;
    Privileges : array[0..0] of LUID_AND_ATTRIBUTES;
  end;
  PTOKEN_PRIVILEGES = ^TOKEN_PRIVILEGES;

  TOKEN_USER = record
    User: SID_AND_ATTRIBUTES;
  end;
  PTOKEN_USER = ^TOKEN_USER;

  TOKEN_GROUPS = record
    GroupCount: DWord;
    Groups: array [0..0] of SID_AND_ATTRIBUTES;
  end;
  PTOKEN_GROUPS = ^TOKEN_GROUPS;

function OpenProcessToken(ProcessHandle: THandle; DesiredAccess: DWord;
  var TokenHandle: THandle): BOOL;
    stdcall; external advapi32;

function LookupPrivilegeValueA(lpSystemName, lpName: PAnsiChar;
  var lpLuid: TLargeInteger): BOOL;
    stdcall; external advapi32;

function LookupPrivilegeNameA(lpSystemName: PAnsiChar; var lpLuid: TLargeInteger;
  lpName: PAnsiChar; var cbName: DWord): BOOL;
    stdcall; external advapi32;

function LookupAccountSidW(lpSystemName: PWideChar; Sid: PSID; Name: PWideChar;
  var cchName: DWord; ReferencedDomainName: PAnsiChar;
  var cchReferencedDomainName: DWord; var peUse: DWord): BOOL;
    stdcall; external advapi32;

function AdjustTokenPrivileges(TokenHandle: THandle; DisableAllPrivileges: BOOL;
  const NewState: TOKEN_PRIVILEGES; BufferLength: DWord;
  PreviousState: PTokenPrivileges; ReturnLength: PDWord): BOOL;
    stdcall; external advapi32;

function IsSystemFolder(const Folder: TFileName): boolean;
begin
  if CompareText(copy(Folder, 2,  9), ':\windows') = 0 then
    result := ord(Folder[11]) in [0, ord('\')]
  else
    result := (CompareText(copy(Folder, 2, 15), ':\program files') = 0) and
              ((ord(Folder[17]) in [0, ord('\')]) or
               (CompareText(copy(Folder, 17, 6), ' (x86)') = 0));
end;

{$ifdef CPU32}
var
  IsUacEnabled: (iueUntested, iueDisabled, iueEnabled);

function IsUacVirtualizationEnabled: boolean;
var
  token: THandle;
  enabled, len: DWORD;
begin
  if IsUacEnabled = iueUntested then
    if OSVersion < wVista then
      IsUacEnabled := iueDisabled // no UAC on Windows XP
    else
    begin
      IsUacEnabled := iueEnabled; // enabled by default
      if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, token) then
      begin
        enabled := 1;
        len := SizeOf(enabled);
        if GetTokenInformation(token, _TokenVirtualizationEnabled,
             @enabled, SizeOf(enabled), len) and
           (enabled = 0) then
          // returns enabled=0 if mormot.win.default.manifest.res is included
          IsUacEnabled := iueDisabled;
        CloseHandle(token);
      end;
    end;
  result := IsUacEnabled = iueEnabled;
end;

function IsUacVirtualFolder(const Folder: TFileName): boolean;
begin
  // note: IsUacVirtualizationEnabled returns false if our manifest is included
  result := (OSVersion >= wVista) and // no UAC on Windows XP
            IsUacVirtualizationEnabled and
            IsSystemFolder(Folder);
end;

{$else}

function IsUacVirtualizationEnabled: boolean;
begin
  result := false; // never enabled for a Win64 process
end;

function IsUacVirtualFolder(const Folder: TFileName): boolean;
begin
  result := false; // never enabled for a Win64 process
end;

{$endif CPU32}

function RawTokenOpen(wtt: TWinTokenType; access: cardinal): THandle;
begin
  if wtt = wttProcess then
  begin
    if not OpenProcessToken(GetCurrentProcess, access, result) then
      RaiseLastError('OpenToken: OpenProcessToken');
  end
  else if not OpenThreadToken(GetCurrentThread, access, false, result) then
    if GetLastError = ERROR_NO_TOKEN then
    begin
      // try to impersonate the thread
      if not ImpersonateSelf(SecurityImpersonation) or
         not OpenThreadToken(GetCurrentThread, access, false, result) then
        RaiseLastError('OpenToken: ImpersonateSelf');
    end
    else
      RaiseLastError('OpenToken: OpenThreadToken');
end;

function RawTokenGetInfo(tok: THandle; tic: TTokenInformationClass;
  var buf: TSynTempBuffer): cardinal;
begin
  buf.Init; // stack-allocated buffer (always enough)
  result := 0; // error
  if (tok = INVALID_HANDLE_VALUE) or
     (tok = 0) or
     GetTokenInformation(tok, tic, buf.buf, buf.len, result) then
    exit; // we directly store the output buffer on buf stack
  if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
  begin
    result := 0;
    exit;
  end;
  buf.Done;
  buf.Init(result); // we need a bigger buffer
  if not GetTokenInformation(tok, tic, buf.buf, buf.len, result) then
    result := 0;
end;

function RawTokenSid(tok: THandle; var buf: TSynTempBuffer): PSid;
begin
  if RawTokenGetInfo(tok, TokenUser, buf) >= SizeOf(TOKEN_USER) then
    result := PSid(PTOKEN_USER(buf.buf)^.User.Sid) // within buf.buf/len
  else
    result := nil;
end;

function CurrentSid(wtt: TWinTokenType; name, domain: PRawUtf8): RawUtf8;
var
  sid: RawSid;
begin
  CurrentRawSid(sid, wtt, name, domain);
  result := RawSidToText(sid);
end;

procedure CurrentRawSid(out sid: RawSid; wtt: TWinTokenType;
  name, domain: PRawUtf8);
var
  h: THandle;
  p: PSid;
  n, d: RawUtf8;
  tmp: TSynTempBuffer;
begin
  h := RawTokenOpen(wtt, TOKEN_QUERY);
  p := RawTokenSid(h, tmp);
  if p <> nil then
  begin
    ToRawSid(p, sid);
    if (name <> nil) or
       (domain <> nil) then
    begin
      LookupSid(p, n, d);
      if name <> nil then
        name^ := n;
      if domain <> nil then
        domain^ := d;
    end;
  end;
  tmp.Done;
  CloseHandle(h);
end;

function ToText(p: TWinSystemPrivilege): PShortString;
begin
  result := @_WSP[p];
end;

function RawTokenGroups(tok: THandle; var buf: TSynTempBuffer): PSids;
var
  nfo: PTokenGroups;
  i: PtrInt;
begin
  result := nil;
  if RawTokenGetInfo(tok, TokenGroups, buf) < SizeOf({%H-}nfo^) then
    exit;
  nfo := buf.buf;
  if nfo.GroupCount = 0 then
    exit;
  SetLength(result, nfo.GroupCount);
  for i := 0 to nfo.GroupCount - 1 do
    result[i] := pointer(nfo.Groups[i].Sid); // within buf.buf/len
end;

function TokenGroupsText(tok: THandle): TRawUtf8DynArray;
var
  tmp: TSynTempBuffer;
begin
  result := SidsToText(RawTokenGroups(tok, tmp));
  tmp.Done;
end;

function TokenHasGroup(tok: THandle; sid: PSid): boolean;
var
  tmp: TSynTempBuffer;
  i: PtrInt;
begin
  result := false;
  if (sid <> nil) and
     (RawTokenGetInfo(tok, TokenGroups, tmp) <> 0) then
    with PTokenGroups(tmp.buf)^ do
      for i := 0 to GroupCount - 1 do
        if SidCompare(pointer(Groups[i].Sid), sid) = 0 then
        begin
          result := true;
          break;
        end;
  tmp.Done;
end;

function TokenHasAnyGroup(tok: THandle; const sid: RawSidDynArray): boolean;
var
  tmp: TSynTempBuffer;
begin
  result := HasAnySid(RawTokenGroups(tok, tmp), sid);
  tmp.Done;
end;

function CurrentGroups(wtt: TWinTokenType; var tmp: TSynTempBuffer): PSids;
var
  h: THandle;
begin
  h := RawTokenOpen(wtt, TOKEN_QUERY);
  result := RawTokenGroups(h, tmp);
  CloseHandle(h);
end;

function CurrentGroupsSid(wtt: TWinTokenType): TRawUtf8DynArray;
var
  tmp: TSynTempBuffer;
begin
  result := SidsToText(CurrentGroups(wtt, tmp));
  tmp.Done;
end;

function CurrentKnownGroups(wtt: TWinTokenType): TWellKnownSids;
var
  tmp: TSynTempBuffer;
begin
  result := SidToKnownGroups(CurrentGroups(wtt, tmp));
  tmp.Done;
end;

function CurrentUserHasGroup(sid: PSid; wtt: TWinTokenType): boolean;
var
  h: THandle;
begin
  h := RawTokenOpen(wtt, TOKEN_QUERY);
  result := TokenHasGroup(h, sid);
  CloseHandle(h);
end;

function CurrentUserHasGroup(wks: TWellKnownSid; wtt: TWinTokenType): boolean;
begin
  result := (wks <> wksNull) and
            CurrentUserHasGroup(pointer(KnownRawSid(wks)), wtt);
end;

function CurrentUserHasGroup(const sid: RawUtf8; wtt: TWinTokenType): boolean;
var
  s: TSid;
begin
  result := TextToSid(pointer(sid), s) and
            CurrentUserHasGroup(@s, wtt);
end;

function CurrentUserHasAnyGroup(const sid: RawSidDynArray; wtt: TWinTokenType): boolean;
var
  tmp: TSynTempBuffer;
begin
  result := HasAnySid(CurrentGroups(wtt, tmp), sid);
  tmp.Done;
end;

function CurrentUserHasGroup(const name, domain, server: RawUtf8;
  wtt: TWinTokenType): boolean;
var
  i: PtrInt;
  sids: PSids;
  n, d: RawUtf8;
  tmp: TSynTempBuffer;
begin
  result := false;
  sids := CurrentGroups(wtt, tmp);
  for i := 0 to length(sids) - 1 do
    if (SidToKnown(sids[i]) = wksNull) and
       (LookupSid(sids[i], n, d, server) = stTypeGroup) then
      if PropNameEquals(n, name) and
         ((domain = '') or
          PropNameEquals(d, domain)) then
      begin
        result := true;
        break;
      end;
  tmp.Done;
end;

function CurrentUserIsAdmin: boolean;
begin
  result := CurrentUserHasGroup(wksBuiltinAdministrators);
end;

function LookupSid(sid: PSid; out name, domain: RawUtf8;
  const server: RawUtf8): TSidType;
var
  n, d: array[byte] of WideChar;
  s: TSynTempBuffer;
  nl, dl, use: cardinal;
begin
  result := stUndefined;
  if sid = nil then
    exit;
  nl := SizeOf(n);
  dl := SizeOf(d);
  if LookupAccountSidW(Utf8ToWin32PWideChar(server, s), sid, @n, nl, @d, dl, use) then
  begin
    Win32PWideCharToUtf8(@n, name);
    Win32PWideCharToUtf8(@d, domain);
    if use <= byte(high(TSidType)) then
      result := TSidType(use);
  end;
  s.Done;
end;

function LookupSid(const sid: RawUtf8; out name, domain: RawUtf8;
  const server: RawUtf8): TSidType;
var
  s: TSid;
begin
  if TextToSid(pointer(sid), s) then
    result := LookupSid(@s, name, domain, server)
  else
    result := stUndefined;
end;

function LookupToken(tok: THandle; out name, domain: RawUtf8;
  const server: RawUtf8): boolean;
var
  tmp: TSynTempBuffer;
begin
  result := LookupSid(RawTokenSid(tok, tmp) ,name, domain, server) <> stUndefined;
  tmp.Done;
end;

function LookupToken(tok: THandle; const server: RawUtf8): RawUtf8;
var
  name, domain: RawUtf8;
begin
  if LookupToken(tok, name, domain, server) then
    result := domain + '\' + name
  else
    result := '';
end;


{ TSynWindowsPrivileges }

procedure TSynWindowsPrivileges.Init(aTokenPrivilege: TWinTokenType;
  aLoadPrivileges: boolean);
begin
  fAvailable := [];
  fEnabled := [];
  fDefEnabled := [];
  fToken := RawTokenOpen(aTokenPrivilege, TOKEN_QUERY or TOKEN_ADJUST_PRIVILEGES);
  if aLoadPrivileges then
    LoadPrivileges;
end;

procedure TSynWindowsPrivileges.Done(aRestoreInitiallyEnabled: boolean);
var
  p: TWinSystemPrivilege;
  new: TWinSystemPrivileges;
begin
  if aRestoreInitiallyEnabled then
  begin
    new := fEnabled - fDefEnabled;
    for p := low(p) to high(p) do
      if p in new then
        Disable(p);
  end;
  CloseHandle(fToken);
  fToken := 0;
end;

function TSynWindowsPrivileges.Enable(aPrivilege: TWinSystemPrivilege): boolean;
begin
  result := aPrivilege in fEnabled;
  if result or
     not (aPrivilege in fAvailable) or
     not SetPrivilege(aPrivilege, true) then
    exit;
  Include(fEnabled, aPrivilege);
  result := true;
end;

function TSynWindowsPrivileges.Disable(
  aPrivilege: TWinSystemPrivilege): boolean;
begin
  result := not (aPrivilege in fEnabled);
  if result or
     not (aPrivilege in fAvailable) or
     not SetPrivilege(aPrivilege, false) then
    exit;
  Exclude(fEnabled, aPrivilege);
  result := true;
end;

procedure TSynWindowsPrivileges.LoadPrivileges;
var
  buf: TSynTempBuffer;
  name: string[127];
  tp: PTOKEN_PRIVILEGES;
  i: PtrInt;
  len: cardinal;
  p: TWinSystemPrivilege;
  priv: PLUIDANDATTRIBUTES;
begin
  if Token = 0 then
    raise EOSException.Create('LoadPriviledges: no token');
  fAvailable := [];
  fEnabled := [];
  fDefEnabled := [];
  try
    if RawTokenGetInfo(Token, TokenPrivileges, buf) = 0 then
      RaiseLastError('LoadPriviledges: GetTokenInformation');
    tp := buf.buf;
    priv := @tp.Privileges;
    for i := 1 to tp.PrivilegeCount do
    begin
      len := high(name);
      if not LookupPrivilegeNameA(nil, priv.Luid, @name[1], len) or
         (len = 0) then
         RaiseLastError('LoadPriviledges: LookupPrivilegeNameA');
      name[0] := AnsiChar(len);
      for p := low(p) to high(p) do
        if not (p in fAvailable) and
           PropNameEquals(PShortString(@name), PShortString(@_WSP[p])) then
        begin
          include(fAvailable, p);
          if priv.Attributes and SE_PRIVILEGE_ENABLED <> 0 then
            include(fDefEnabled, p);
          break;
        end;
      inc(priv);
    end;
    fEnabled := fDefEnabled;
  finally
    buf.Done;
  end;
end;

function TSynWindowsPrivileges.SetPrivilege(
  wsp: TWinSystemPrivilege; on: boolean): boolean;
var
  tp: TOKEN_PRIVILEGES;
  id: TLargeInteger;
  tpprev: TOKEN_PRIVILEGES;
  cbprev: DWord;
begin
  result := false;
  if not LookupPrivilegeValueA(nil, @_WSP[wsp][1], id) then
    exit;
  tp.PrivilegeCount := 1;
  tp.Privileges[0].Luid := PInt64(@id)^;
  tp.Privileges[0].Attributes := 0;
  cbprev := SizeOf(TOKEN_PRIVILEGES);
  AdjustTokenPrivileges(
    Token, false, tp, SizeOf(TOKEN_PRIVILEGES), @tpprev, @cbprev);
  if GetLastError <> ERROR_SUCCESS then
    exit;
  tpprev.PrivilegeCount := 1;
  tpprev.Privileges[0].Luid := PInt64(@id)^;
  with tpprev.Privileges[0] do
    if on then
      Attributes := Attributes or SE_PRIVILEGE_ENABLED
    else
      Attributes := Attributes xor (SE_PRIVILEGE_ENABLED and Attributes);
  AdjustTokenPrivileges(
    Token, false, tpprev, cbprev, nil, nil);
  if GetLastError <> ERROR_SUCCESS then
    exit;
  result := true;
end;

const
  ntdll = 'NTDLL.DLL';

type
  _PPS_POST_PROCESS_INIT_ROUTINE = ULONG;

  PUNICODE_STRING = ^UNICODE_STRING;
  UNICODE_STRING = packed record
    Length: word;
    MaximumLength: word;
    {$ifdef CPUX64}
    _align: array[0..3] of byte;
    {$endif CPUX64}
    Buffer: PWideChar;
  end;

  PMS_PEB_LDR_DATA = ^MS_PEB_LDR_DATA;
  MS_PEB_LDR_DATA = packed record
    Reserved1: array[0..7] of byte;
    Reserved2: array[0..2] of pointer;
    InMemoryOrderModuleList: LIST_ENTRY;
  end;

  PMS_RTL_USER_PROCESS_PARAMETERS = ^MS_RTL_USER_PROCESS_PARAMETERS;
  MS_RTL_USER_PROCESS_PARAMETERS = packed record
    Reserved1: array[0..15] of byte;
    Reserved2: array[0..9] of pointer;
    ImagePathName: UNICODE_STRING;
    CommandLine: UNICODE_STRING ;
  end;

  PMS_PEB = ^MS_PEB;
  MS_PEB = packed record
    Reserved1: array[0..1] of byte;
    BeingDebugged: BYTE;
    Reserved2: array[0..0] of byte;
    {$ifdef CPUX64}
    _align1: array[0..3] of byte;
    {$endif CPUX64}
    Reserved3: array[0..1] of pointer;
    Ldr: PMS_PEB_LDR_DATA;
    ProcessParameters: PMS_RTL_USER_PROCESS_PARAMETERS;
    Reserved4: array[0..103] of byte;
    Reserved5: array[0..51] of pointer;
    PostProcessInitRoutine: _PPS_POST_PROCESS_INIT_ROUTINE;
    Reserved6: array[0..127] of byte;
    {$ifdef CPUX64}
    _align2: array[0..3] of byte;
    {$endif CPUX64}
    Reserved7: array[0..0] of pointer;
    SessionId: ULONG;
    {$ifdef CPUX64}
    _align3: array[0..3] of byte;
    {$endif CPUX64}
  end;

  PMS_PROCESS_BASIC_INFORMATION = ^MS_PROCESS_BASIC_INFORMATION;
  MS_PROCESS_BASIC_INFORMATION = packed record
    ExitStatus: integer;
    {$ifdef CPUX64}
    _align1: array[0..3] of byte;
    {$endif CPUX64}
    PebBaseAddress: PMS_PEB;
    AffinityMask: PtrUInt;
    BasePriority: integer;
    {$ifdef CPUX64}
    _align2: array[0..3] of byte;
    {$endif CPUX64}
    UniqueProcessId: PtrUInt;
    InheritedFromUniqueProcessId: PtrUInt;
  end;

  {$Z4}
  PROCESSINFOCLASS = (
    ProcessBasicInformation = 0,
    ProcessDebugPort = 7,
    ProcessWow64Information = 26,
    ProcessImageFileName = 27,
    ProcessBreakOnTermination = 29,
    ProcessSubsystemInformation = 75);
  {$Z1}

  NTSTATUS = integer;
  PVOID = pointer;
  PPVOID = ^PVOID;

  OBJECT_ATTRIBUTES = record
    Length: ULONG;
    RootDirectory: THandle;
    ObjectName: PUNICODE_STRING;
    Attributes: ULONG;
    SecurityDescriptor: Pointer;       // Points to type SECURITY_DESCRIPTOR
    SecurityQualityOfService: Pointer; // Points to type SECURITY_QUALITY_OF_SERVICE
  end;
  POBJECT_ATTRIBUTES = ^OBJECT_ATTRIBUTES;

var
  // low-level (undocumented) ntdll.dll functions - accessed via late-binding
  NtQueryInformationProcess: function(ProcessHandle: THandle;
    ProcessInformationClass: PROCESSINFOCLASS; ProcessInformation: pointer;
    ProcessInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;
  RtlInitUnicodeString: function(var DestinationString: UNICODE_STRING;
    const SourceString: PWideChar): NTSTATUS; stdcall;
  NtOpenSection: function (SectionHandle: PHANDLE; DesiredAccess: ACCESS_MASK;
    ObjectAttributes: POBJECT_ATTRIBUTES): NTSTATUS; stdcall;
  NtMapViewOfSection: function (SectionHandle, ProcessHandle: THandle;
    BaseAddress: PPVOID; ZeroBits: ULONG; CommitSize: ULONG;
    var SectionOffset: TLargeInteger; ViewSize: PULONG; InheritDisposition: DWord;
    AllocationType: ULONG; Protect: ULONG): NTSTATUS; stdcall;
  NtUnmapViewOfSection: function (ProcessHandle: THandle;
    BaseAddress: PVOID): NTSTATUS; stdcall;

function ReadSystemMemory(address, size: PtrUInt): RawByteString;
var
  memfile: UNICODE_STRING;
  att: OBJECT_ATTRIBUTES;
  mem: THandle;
  add: TLargeInteger;
  virt: pointer;
begin
  result := '';
  if (size <= 4 shl 20) and // map up to 4MB
     Assigned(RtlInitUnicodeString) and
     Assigned(NtOpenSection) and
     Assigned(NtMapViewOfSection) and
     Assigned(NtUnmapViewOfSection) then
  begin
    RtlInitUnicodeString(memfile, '\device\physicalmemory');
    FillCharFast(att, SizeOf(att), 0);
    att.Length := SizeOf(att);
    att.ObjectName := @memfile;
    att.Attributes := $40; // OBJ_CASE_INSENSITIVE
    if NtOpenSection(@mem, SECTION_MAP_READ, @att) <> 0 then
      exit;
    add := address;
    virt := nil;
    if NtMapViewOfSection(mem, INVALID_HANDLE_VALUE, @virt, 0, size, add, @size,
         1, 0, PAGE_READONLY) = 0 then
    begin
      FastSetRawByteString(result, virt, size);
      NtUnmapViewOfSection(INVALID_HANDLE_VALUE, virt);
    end;
    CloseHandle(mem);
  end;
end;

function ReadProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer;
  lpBuffer: Pointer; nSize: PtrUInt; var lpNumberOfBytesRead: PtrUInt): BOOL;
    stdcall; external kernel32;

function InternalGetProcessInfo(aPID: DWord; out aInfo: TWinProcessInfo): boolean;
var
  bytesread: PtrUInt;
  sizeneeded: DWord;
  pbi: MS_PROCESS_BASIC_INFORMATION;
  peb: MS_PEB;
  peb_upp: MS_RTL_USER_PROCESS_PARAMETERS;
  prochandle: THandle;
begin
  result := false;
  Finalize(aInfo);
  FillCharFast(aInfo, SizeOf(aInfo), 0);
  if (APID = 0) and
     Assigned(NtQueryInformationProcess) then
    exit;
  prochandle := OpenProcess(
    PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, FALSE, aPid);
  if prochandle = INVALID_HANDLE_VALUE then
    exit;
  Include(aInfo.AvailableInfo, wpaiPID);
  aInfo.PID := aPid;
  try
    // read PBI (Process Basic Information)
    sizeneeded := 0;
    FillCharFast(pbi, SizeOf(pbi), 0);
    if NtQueryInformationProcess(prochandle, ProcessBasicInformation,
         @pbi, Sizeof(pbi), @sizeneeded) < 0 then
      exit;
    with aInfo do
    begin
      Include(AvailableInfo, wpaiBasic);
      PID := pbi.UniqueProcessId;
      ParentPID := pbi.InheritedFromUniqueProcessId;
      BasePriority := pbi.BasePriority;
      ExitStatus := pbi.ExitStatus;
      PEBBaseAddress := pbi.PebBaseAddress;
      AffinityMask := pbi.AffinityMask;
    end;
    // read PEB (Process Environment Block)
    if not Assigned(pbi.PebBaseAddress) then
      exit;
    bytesread := 0;
    FillCharFast(peb, SizeOf(peb), 0);
    if not ReadProcessMemory(prochandle, pbi.PebBaseAddress,
             @peb, SizeOf(peb), bytesread) then
      exit;
    Include(aInfo.AvailableInfo, wpaiPEB);
    aInfo.SessionID := peb.SessionId;
    aInfo.BeingDebugged := peb.BeingDebugged;
    FillCharFast(peb_upp, SizeOf(MS_RTL_USER_PROCESS_PARAMETERS), 0);
    bytesread := 0;
    if not ReadProcessMemory(prochandle, peb.ProcessParameters,
         @peb_upp, SizeOf(MS_RTL_USER_PROCESS_PARAMETERS), bytesread) then
      exit;
    // command line info
    if peb_upp.CommandLine.Length > 0 then
    begin
      SetLength(aInfo.CommandLine, peb_upp.CommandLine.Length shr 1);
      bytesread := 0;
      if not ReadProcessMemory(prochandle, peb_upp.CommandLine.Buffer,
           pointer(aInfo.CommandLine), peb_upp.CommandLine.Length, bytesread) then
        exit;
      Include(aInfo.AvailableInfo, wpaiCommandLine);
    end;
    // image info
    if peb_upp.ImagePathName.Length > 0 then
    begin
      SetLength(aInfo.ImagePath, peb_upp.ImagePathName.Length shr 1);
      bytesread := 0;
      if not ReadProcessMemory(prochandle, peb_upp.ImagePathName.Buffer,
           pointer(aInfo.ImagePath), peb_upp.ImagePathName.Length, bytesread) then
        exit;
      Include(aInfo.AvailableInfo, wpaiImagePath);
    end;
    result := true;
  finally
    CloseHandle(prochandle);
  end;
end;

procedure GetProcessInfo(aPid: cardinal; out aInfo: TWinProcessInfo);
var
  privileges: TSynWindowsPrivileges;
begin
  privileges.Init(wttThread);
  try
    privileges.Enable(wspDebug);
    InternalGetProcessInfo(aPid, aInfo);
  finally
    privileges.Done;
  end;
end;

procedure GetProcessInfo(const aPidList: TCardinalDynArray;
  out aInfo: TWinProcessInfoDynArray);
var
  privileges: TSynWindowsPrivileges;
  i: PtrInt;
begin
  SetLength(aInfo, Length(aPidList));
  privileges.Init(wttThread);
  try
    privileges.Enable(wspDebug);
    for i := 0 to High(aPidList) do
      InternalGetProcessInfo(aPidList[i], aInfo[i]);
  finally
    privileges.Done;
  end;
end;

function ReadRegString(Key: THandle; const Path, Value: string): string;
var
  siz, typ: DWord;
  tmp: array[byte] of char;
  k: HKey;
begin
  result := '';
  if RegOpenKeyEx(Key, pointer(Path), 0, KEY_QUERY_VALUE, k) <> ERROR_SUCCESS then
    exit;
  siz := 250;
  typ := REG_SZ;
  if RegQueryValueEx(k, pointer(Value), nil, @typ, @tmp, @siz) = ERROR_SUCCESS then
    result := tmp;
  RegCloseKey(k);
end;


{ TWinCryptoApi }

function TWinCryptoApi.Available: boolean;
begin
  if not Tested then
    Resolve;
  result := Assigned(AcquireContextA);
end;

procedure TWinCryptoApi.Resolve;
const
  NAMES: array[0..7] of PAnsiChar = (
    'CryptAcquireContextA',
    'CryptReleaseContext',
    'CryptImportKey',
    'CryptSetKeyParam',
    'CryptDestroyKey',
    'CryptEncrypt',
    'CryptDecrypt',
    'CryptGenRandom');
var
  P: PPointer;
  i: PtrInt;
begin
  Tested := true;
  Handle := GetModuleHandle('advapi32.dll');
  if Handle <> 0 then
  begin
    P := @@AcquireContextA;
    for i := 0 to high(NAMES) do
    begin
      P^ := LibraryResolve(Handle, NAMES[i]);
      if P^ = nil then
      begin
        PPointer(@@AcquireContextA)^ := nil;
        break;
      end;
      inc(P);
    end;
  end;
  // note: CryptSignMessage and CryptVerifyMessageSignature are in crypt32.dll
end;

type
  {$ifdef FPC}
  {$packrecords C} // mandatory under Win64
  {$endif FPC}
  DATA_BLOB = record
    cbData: DWord;
    pbData: PAnsiChar;
  end;
  PDATA_BLOB = ^DATA_BLOB;
  {$ifdef FPC}
  {$packrecords DEFAULT}
  {$endif FPC}

const
  crypt32 = 'Crypt32.dll';
  CRYPTPROTECT_UI_FORBIDDEN = 1;
  CRYPT_STRING_BASE64HEADER = 0; // = PEM textual format

function CryptProtectData(const DataIn: DATA_BLOB; szDataDescr: PWideChar;
  OptionalEntropy: PDATA_BLOB; Reserved, PromptStruct: Pointer; dwFlags: DWord;
  var DataOut: DATA_BLOB): BOOL;
    stdcall; external crypt32;

function CryptUnprotectData(const DataIn: DATA_BLOB; szDataDescr: PWideChar;
  OptionalEntropy: PDATA_BLOB; Reserved, PromptStruct: Pointer; dwFlags: DWord;
  var DataOut: DATA_BLOB): BOOL;
    stdcall; external crypt32;

function CryptDataForCurrentUserDPAPI(const Data, AppSecret: RawByteString;
  Encrypt: boolean): RawByteString;
var
  src, dst, ent: DATA_BLOB;
  e: PDATA_BLOB;
  ok: boolean;
begin
  src.pbData := pointer(Data);
  src.cbData := length(Data);
  if AppSecret <> '' then
  begin
    ent.pbData := pointer(AppSecret);
    ent.cbData := length(AppSecret);
    e := @ent;
  end
  else
    e := nil;
  if Encrypt then
    ok := CryptProtectData(
      src, nil, e, nil, nil, CRYPTPROTECT_UI_FORBIDDEN, dst)
  else
    ok := CryptUnprotectData(
      src, nil, e, nil, nil, CRYPTPROTECT_UI_FORBIDDEN, dst);
  if ok then
  begin
    FastSetRawByteString(result, dst.pbData, dst.cbData);
    LocalFree(HLOCAL(dst.pbData));
  end
  else
    result := '';
end;

function CertOpenSystemStoreW(hProv: HCRYPTPROV;
  szSubsystemProtocol: PWideChar): HCERTSTORE ;
    stdcall; external crypt32;

function CertEnumCertificatesInStore(hCertStore: HCERTSTORE;
  pPrevCertContext: PCCERT_CONTEXT): PCCERT_CONTEXT;
  stdcall; external crypt32;

function CryptBinaryToStringA(pBinary: PByte; cbBinary, dwFlags: DWord;
  pszString: PAnsiChar; var pchString: DWord): BOOL;
    stdcall; external crypt32;

function CertCloseStore(hCertStore: HCERTSTORE; dwFlags: DWord): BOOL;
    stdcall; external crypt32;

function _GetSystemStoreAsPem(CertStore: TSystemCertificateStore): RawUtf8;
var
  store: HCERTSTORE;
  ctx: PCCERT_CONTEXT;
  resultlen, certlen: DWord;
  tmp: TSynTempBuffer;
begin
  // call the Windows API to retrieve the System certificates
  result := '';
  resultlen := 0;
  store := CertOpenSystemStoreW(nil, WINDOWS_CERTSTORE[CertStore]);
  try
    ctx := CertEnumCertificatesInStore(store, nil);
    while ctx <> nil do
    begin
      certlen := 0;
      if not CryptBinaryToStringA(ctx^.pbCertEncoded, ctx^.cbCertEncoded,
          CRYPT_STRING_BASE64HEADER, nil, certlen) then
        break;
      tmp.Init(certlen); // a PEM is very likely to be < 8KB so will be on stack
      if CryptBinaryToStringA(ctx^.pbCertEncoded, ctx^.cbCertEncoded,
          CRYPT_STRING_BASE64HEADER, tmp.buf, certlen) then
      begin
        SetLength(result, resultlen + certlen);
        MoveFast(tmp.buf^, PByteArray(result)[resultlen], certlen);
        inc(resultlen, certlen);
      end;
      tmp.Done;
      ctx := CertEnumCertificatesInStore(store, ctx); // next certificate
    end;
  finally
    CertCloseStore(store, 0);
  end;
end;

function SearchSmbios(const mem: RawByteString; var info: TRawSmbiosInfo): PtrUInt;
  forward; // implemented later in mormot.core.os.pas

const
  _RSMB_ = $52534D42;
  // potential location of the SMBIOS buffer pointers within a 64KB fixed frame
  SMB_START  = $000f0000;
  SMB_STOP   = $00100000;

function _GetRawSmbios(var info: TRawSmbiosInfo): boolean;
var
  siz: DWord;
  tmp: RawByteString;
  addr: PtrUInt;
  get: function(sig, id: DWord; buf: pointer; siz: DWord): PtrUInt; stdcall;
begin
  // first try to use Vista+ API which supports EFI
  get := GetProcAddress(GetModuleHandle(kernel32), 'GetSystemFirmwareTable');
  if Assigned(get) then
  begin
    siz := get(_RSMB_, 0, nil, 0); // first call to retrieve the full size
    if siz > SizeOf(info) then
    begin
      FastNewRawByteString(tmp, siz);
      get(_RSMB_, 0, pointer(tmp), siz);
      PInt64(@info)^ := PInt64(tmp)^; // header fields = 64-bit
      FastSetRawByteString(info.data, @PInt64Array(tmp)[1], siz - SizeOf(Int64));
      result := true;
      exit;
    end;
  end;
  // on XP, read directly from physical memory via ntdll.dll low-level API
  result := false;
  tmp := ReadSystemMemory(SMB_START, SMB_STOP - SMB_START);
  if tmp = '' then
    exit;
  addr := SearchSmbios(tmp, info);
  if addr = 0 then
    exit;
  info.data := ReadSystemMemory(addr, info.Length);
  result := info.data <> '';
end;

procedure DirectSmbiosInfo(out info: TSmbiosBasicInfos);
begin
  // not needed - GetRawSmbios() is likely to work with no administrator rights
end;

threadvar // do not publish for compilation within Delphi packages
  CoInitCounter: integer;

// avoid including ActiveX unit
function CoInitialize(_para1: pointer): HRESULT;
  stdcall; external 'ole32.dll';
procedure CoUninitialize;
  stdcall; external 'ole32.dll';

procedure CoInit;
begin
  inc(CoInitCounter); // is a threadvar: no InterlockedIncrement() needed
  if CoInitCounter = 1 then
    CoInitialize(nil);
end;

procedure CoUninit;
begin
  if CoInitCounter <= 0 then
    raise EOleSysError.Create('You should call TOleDBConnection.Free from the same ' +
      'thread which called its Create: i.e. call MyProps.EndCurrentThread from an ' +
      'THttpServerGeneric.OnHttpThreadTerminate event - see ticket 213544b2f5');
  dec(CoInitCounter);
  if CoInitCounter = 0 then
    CoUninitialize;
end;


{ ****************** Unix Daemon and Windows Service Support }

function OpenServiceManager(const TargetComputer, DatabaseName: RawUtf8;
  dwDesiredAccess: cardinal): SC_HANDLE;
var
  t1, t2: TSynTempBuffer;
begin
  result := OpenSCManagerW(
    Utf8ToWin32PWideChar(TargetComputer, t1),
    Utf8ToWin32PWideChar(DatabaseName, t2), dwDesiredAccess);
  t1.Done;
  t2.Done;
end;

function OpenServiceInstance(hSCManager: SC_HANDLE; const ServiceName: RawUtf8;
  dwDesiredAccess: cardinal): SC_HANDLE;
var
  t: TSynTempBuffer;
begin
  result := OpenServiceW(
    hSCManager, Utf8ToWin32PWideChar(ServiceName, t), dwDesiredAccess);
  t.Done;
end;


{ TServiceController }

type
  EService = class(Exception);

constructor TServiceController.CreateNewService(const TargetComputer,
  DatabaseName, Name, DisplayName: RawUtf8; const Path: TFileName;
  const OrderGroup: RawUtf8; const Dependencies: RawUtf8;
  const Username: RawUtf8; const Password: RawUtf8; DesiredAccess: cardinal;
  ServiceType: cardinal; StartType: cardinal; ErrorControl: cardinal);
var
  exeName: TFileName;
  exeNameW: SynUnicode;
  depW: PWideChar;
  i: PtrInt;
  t0, t1, t2, t3, t4, t5: TSynTempBuffer;
begin
  inherited Create;
  if Path = '' then
  begin
    TService.DoLog(sllError,
      'CreateNewService(''%'',''%'') with no Path', [Name, DisplayName], self);
    exit;
  end;
  if TargetComputer = '' then
    if GetDriveType(pointer(ExtractFileDrive(Path))) = DRIVE_REMOTE then
    begin
      exeName := ExpandUNCFileName(Path);
      if (copy(exeName, 1, 12) <> '\\localhost\') or
         (exeName[14] <> '$') then
        raise EService.CreateFmt(
          '%s.CreateNewService(''%s'',''%s'') on remote drive: Path=%s is %s',
          [ClassNameShort(self)^, Name, DisplayName, Path, exeName]);
      system.delete(exeName, 1, 12); // \\localhost\c$\... -> c:\...
      exeName[2] := ':';
    end
    else
      exeName := Path;
  exeNameW := SynUnicode(exeName); // use RTL for TFileName to UTF-16
  fName := Name;
  fSCHandle := OpenServiceManager(
    TargetComputer, DatabaseName, SC_MANAGER_ALL_ACCESS);
  if fSCHandle = 0 then
    RaiseLastError('TServiceController.CreateService: OpenServiceManager', EService);
  depW := Utf8ToWin32PWideChar(Dependencies, t0);
  if depW <> nil then
  begin
    for i := 0 to t0.len - 1 do
      if depW[i] = ';' then
        depW[i] := #0; // as expected by CreateServiceW() API
    depW[t0.len + 1] := #0; // should end with #0#0
  end;
  fHandle := CreateServiceW(fSCHandle,
    Utf8ToWin32PWideChar(Name, t1), Utf8ToWin32PWideChar(DisplayName, t2),
    DesiredAccess, ServiceType, StartType,
    ErrorControl, pointer(exeNameW), Utf8ToWin32PWideChar(OrderGroup, t3),
    nil, depW, Utf8ToWin32PWideChar(Username, t4),
    Utf8ToWin32PWideChar(Password, t5));
  t0.Done;
  t1.Done;
  t2.Done;
  t3.Done;
  t4.Done;
  t5.Done;
  if fHandle = 0 then
    RaiseLastError('TServiceController.CreateService:', EService);
  TService.DoLog(sllInfo, 
    'CreateService(''%'',''%'',''%'')', [Name, DisplayName, exeName], self);
end;

constructor TServiceController.CreateOpenService(
  const TargetComputer, DataBaseName, Name: RawUtf8; DesiredAccess: cardinal);
begin
  inherited Create;
  fName := RawUtf8(Name);
  fSCHandle := OpenServiceManager(TargetComputer, DataBaseName, GENERIC_READ);
  if fSCHandle = 0 then
  begin
    TService.DoLog(sllLastError, 'OpenSCManager(''%'',''%'') for [%]',
      [TargetComputer, DataBaseName, fName], self);
    exit;
  end;
  fHandle := OpenServiceInstance(fSCHandle, Name, DesiredAccess);
  if fHandle = 0 then
    TService.DoLog(sllLastError, 'OpenService(%)', [Name], self);
end;

function TServiceController.Delete: boolean;
begin
  result := false;
  if fHandle <> 0 then
    if DeleteService(fHandle) then
    begin
      result := CloseServiceHandle(fHandle);
      fHandle := 0;
    end
    else
      TService.DoLog(sllLastError, 'DeleteService(%)', [fName], self);
end;

destructor TServiceController.Destroy;
begin
  if fHandle <> 0 then
  begin
    CloseServiceHandle(fHandle);
    fHandle := 0;
  end;
  if fSCHandle <> 0 then
  begin
    CloseServiceHandle(fSCHandle);
    fSCHandle := 0;
  end;
  inherited;
end;

function TServiceController.GetState: TServiceState;
begin
  if (self = nil) or
     (fSCHandle = 0) or
     (fHandle = 0) then
    result := ssNotInstalled
  else
    result := CurrentStateToServiceState(GetStatus.dwCurrentState);
  TService.DoLog(sllTrace, 'GetState(%)=%', [fName, ToText(result)^], self);
end;

function TServiceController.GetStatus: TServiceStatus;
begin
  FillCharFast(fStatus, SizeOf(fStatus), 0);
  QueryServiceStatus(fHandle, fStatus);
  result := fStatus;
end;

function TServiceController.Pause: boolean;
begin
  if fHandle = 0 then
    result := false
  else
    result := ControlService(fHandle, SERVICE_CONTROL_PAUSE, fStatus);
end;

function TServiceController.Refresh: boolean;
begin
  if fHandle = 0 then
    result := false
  else
    result := ControlService(fHandle, SERVICE_CONTROL_INTERROGATE, fStatus);
end;

function TServiceController.Resume: boolean;
begin
  if fHandle = 0 then
    result := false
  else
    result := ControlService(fHandle, SERVICE_CONTROL_CONTINUE, fStatus);
end;

function TServiceController.Shutdown: boolean;
begin
  if fHandle = 0 then
    result := false
  else
    result := ControlService(fHandle, SERVICE_CONTROL_SHUTDOWN, fStatus);
end;

function TServiceController.Start(const Args: array of PWideChar): boolean;
begin
  TService.DoLog(sllDebug, 'Start(%) Args=% Handle=%',
    [fName, length(Args), fHandle], self);
  if fHandle = 0 then
  begin
    TService.DoLog(sllError, 'Start(%): no Service', [fName], self);
    result := false;
    exit;
  end;
  if length(Args) = 0 then
    result := StartServiceW(fHandle, 0, nil)
  else
    result := StartServiceW(fHandle, length(Args), @Args[0]);
  if not result then
    TService.DoLog(sllLastError, 'Start(%) failed', [fName], self);
end;

function TServiceController.Stop: boolean;
begin
  if fHandle = 0 then
    result := false
  else
    result := ControlService(fHandle, SERVICE_CONTROL_STOP, fStatus);
end;

function TServiceController.SetDescription(const Description: RawUtf8): boolean;
var
  sd: TServiceDescription;
  t: TSynTempBuffer;
begin
  if Description = '' then
  begin
    result := false;
    exit;
  end;
  sd.lpDestription := Utf8ToWin32PWideChar(Description, t);
  result := ChangeServiceConfig2W(fHandle, SERVICE_CONFIG_DESCRIPTION, @sd);
  t.Done;
end;

class procedure TServiceController.CheckParameters(
  const ExeFileName: TFileName; const ServiceName, DisplayName,
  Description: RawUtf8; const Dependencies: RawUtf8);
var
  param: string;
  i: integer;

  procedure ShowError(Msg: RawUtf8);
  begin
    Msg := _fmt('%s: "%s" failed for %s', [ServiceName, Msg, param]);
    TService.DoLog(sllLastError, '%', [Msg], nil);
    ConsoleWrite(Msg, ccLightRed);
  end;

begin
  for i := 1 to ParamCount do
  begin
    param := SysUtils.LowerCase(paramstr(i));
    TService.DoLog(sllInfo,
      'Controling % with command [%]', [ServiceName, param], nil);
    if param = '/install' then
      TServiceController.Install(
        ServiceName, DisplayName, Description, true, ExeFileName, Dependencies)
    else
      with TServiceController.CreateOpenService('', '', ServiceName) do
      try
        if State = ssErrorRetrievingState then
          ShowError('State')
        else if param = '/uninstall' then
        begin
          if not Stop then
            ShowError('Stop');
          if not Delete then
            ShowError('Delete');
        end
        else if param = '/stop' then
        begin
          if not Stop then
            ShowError('Stop');
        end
        else if param = '/start' then
        begin
          if not Start([]) then
            ShowError('Start');
        end;
      finally
        Free;
      end;
  end;
end;

class function TServiceController.Install(
  const Name, DisplayName, Description: RawUtf8; AutoStart: boolean;
  ExeName: TFileName; const Dependencies, Username, Password: RawUtf8): TServiceState;
var
  ctrl: TServiceController;
  start: cardinal;
begin
  if AutoStart then
    start := SERVICE_AUTO_START
  else
    start := SERVICE_DEMAND_START;
  if ExeName = '' then
    ExeName := Executable.ProgramFileName;
  ctrl := TServiceController.CreateNewService(
    '', '', Name, DisplayName, ExeName, '', Dependencies, UserName, Password,
    SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS, start);
  try
    result := ctrl.State;
    if result <> ssNotInstalled then
      ctrl.SetDescription(Description);
    TService.DoLog(sllDebug,
      'Install(%)=%', [Name, ToText(result)^], ctrl);
  finally
    ctrl.Free;
  end;
end;

class function TServiceController.CurrentState(const Name: RawUtf8): TServiceState;
begin
  try
    with CreateOpenService('', '', Name, SERVICE_QUERY_STATUS) do
      try
        result := GetState;
      finally
        Free;
      end;
  except
    result := ssErrorRetrievingState;
  end;
end;


{ TService }

class procedure TService.DoLog(Level: TSynLogLevel; const Fmt: RawUtf8;
  const Args: array of const; Instance: TObject);
begin
  if Assigned(WindowsServiceLog) then
    WindowsServiceLog(Level, Fmt, Args, Instance);
end;

constructor TService.Create(const aServiceName, aDisplayName: RawUTf8);
begin
  fServiceName := aServiceName;
  if aDisplayName = '' then
    fDisplayName := aServiceName
  else
    fDisplayName := aDisplayName;
  fServiceType := SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS;
  fStartType := SERVICE_AUTO_START;
  fStatusRec.dwServiceType := fServiceType;
  fStatusRec.dwCurrentState := SERVICE_STOPPED;
  fStatusRec.dwControlsAccepted := 31;
  fStatusRec.dwWin32ExitCode := NO_ERROR;
  DoLog(sllDebug, 'Create: % (%) running as [%]',
    [ServiceName, aDisplayName, Executable.ProgramFullSpec], self);
end;

procedure TService.CtrlHandle(Code: cardinal);
begin
  DoCtrlHandle(Code);
end;

const
  _CMD: array[0.. 5] of string[11] = (
    'UNKNOWN', 'STOP', 'PAUSE', 'CONTINUE', 'INTERROGATE', 'SHUTDOWN');

procedure TService.DoCtrlHandle(Code: cardinal);
var
  c: PShortString;
begin
  if Code <= high(_CMD) then
    c := @_CMD[Code]
  else
    c := @_CMD[0];
  DoLog(sllTrace, '% DoCtrlHandle(SERVICE_CONTROL_%=%)',
    [ServiceName, c^, Code], self);
  try
    case Code of
      SERVICE_CONTROL_STOP:
        begin
          ReportStatus(SERVICE_STOP_PENDING, NO_ERROR, 0);
          try
            if Assigned(fOnStop) then
              fOnStop(Self);
            ReportStatus(SERVICE_STOPPED, NO_ERROR, 0);
          except
            ReportStatus(SERVICE_STOPPED, ERROR_CAN_NOT_COMPLETE, 0);
          end;
        end;
      SERVICE_CONTROL_PAUSE:
        begin
          ReportStatus(SERVICE_PAUSE_PENDING, NO_ERROR, 0);
          try
            if Assigned(fOnPause) then
              fOnPause(Self);
            ReportStatus(SERVICE_PAUSED, NO_ERROR, 0)
          except
            ReportStatus(SERVICE_PAUSED, ERROR_CAN_NOT_COMPLETE, 0)
          end;
        end;
      SERVICE_CONTROL_CONTINUE:
        begin
          ReportStatus(SERVICE_CONTINUE_PENDING, NO_ERROR, 0);
          try
            if Assigned(fOnResume) then
              fOnResume(Self);
            ReportStatus(SERVICE_RUNNING, NO_ERROR, 0);
          except
            ReportStatus(SERVICE_RUNNING, ERROR_CAN_NOT_COMPLETE, 0);
          end;
        end;
      SERVICE_CONTROL_SHUTDOWN:
        begin
          if Assigned(fOnShutdown) then
            fOnShutdown(Self);
          Code := 0;
        end;
      SERVICE_CONTROL_INTERROGATE:
        begin
          SetServiceStatus(fStatusHandle, fStatusRec);
          if Assigned(fOnInterrogate) then
            fOnInterrogate(Self);
        end;
    end;
    if Assigned(fOnControl) then
      fOnControl(Self, Code);
  except
  end;
  DoLog(sllTrace, '% DoCtrlHandle(SERVICE_CONTROL_%=%) finished',
    [ServiceName, c^, Code], self);
end;

procedure TService.Execute;
begin
  try
    if Assigned(fOnStart) then
      fOnStart(Self);
    ReportStatus(SERVICE_RUNNING, NO_ERROR, 0);
    if Assigned(fOnExecute) then
      fOnExecute(Self);
  except
    ReportStatus(SERVICE_RUNNING, ERROR_CAN_NOT_COMPLETE, 0);
  end;
  DoLog(sllTrace, '% Execute finished', [ServiceName], self);
end;

function TService.GetArgCount: Integer;
begin
  result := length(fArgsList);
end;

function TService.GetArgs(Idx: Integer): RawUtf8;
begin
  if cardinal(Idx) > cardinal(high(fArgsList)) then
    result := ''
  else
    // avoid GPF
    result := fArgsList[Idx];
end;

function TService.GetControlHandler: TServiceControlHandler;
begin
  result := fControlHandler;
  if not Assigned(result) then
    DoLog(sllError, '% GetControlHandler=nil: use TServiceSingle or ' +
      'assign a custom ControlHandler', [ServiceName], self);
end;

function TService.GetInstalled: boolean;
begin
  with TServiceController.CreateOpenService(
    '', '', fServiceName, SERVICE_QUERY_STATUS) do
  try
    result := Handle <> 0;
  finally
    Free;
  end;
end;

function TService.Install(const Params: TFileName): boolean;
var
  schService: SC_HANDLE;
  schSCManager: SC_HANDLE;
  ServicePath: TFileName;
  p: SynUnicode;
  t1, t2: TSynTempBuffer;
begin
  result := false;
  if installed then
    exit;
  ServicePath := Executable.ProgramFileName;
  if Params <> '' then
    ServicePath := ServicePath + ' ' + Params;
  p := SynUnicode(ServicePath); // use RTL for TFileName to UTF-16 conversion
  schSCManager := OpenSCManagerW(nil, nil, SC_MANAGER_ALL_ACCESS);
  if schSCManager <= 0 then
    exit;
  schService := CreateServiceW(schSCManager,
    Utf8ToWin32PWideChar(fServiceName, t1),
    Utf8ToWin32PWideChar(fDisplayName, t2),
    SERVICE_ALL_ACCESS, fServiceType, fStartType, SERVICE_ERROR_NORMAL,
    pointer(p), nil, nil, nil, nil, nil);
  t1.Done;
  t2.Done;
  if schService > 0 then
  begin
    result := true;
    CloseServiceHandle(schService);
  end;
  CloseServiceHandle(schSCManager);
end;

procedure TService.Remove;
begin
  with TServiceController.CreateOpenService(
    '', '', fServiceName, SERVICE_ALL_ACCESS) do
  try
    if Handle = 0 then
      exit;
    Stop;
    Delete;
  finally
    Free;
  end;
end;

function TService.ReportStatus(dwState, dwExitCode, dwWait: cardinal): BOOL;
var
  status: PShortString;
begin
  status := ToText(CurrentStateToServiceState(dwState));
  DoLog(sllTrace, '% ReportStatus(%=%,%)=%', [ServiceName,
    WinErrorConstant(dwExitCode), dwExitCode, dwWait, status^], self);
  if dwState = SERVICE_START_PENDING then
    fStatusRec.dwControlsAccepted := 0
  else
    fStatusRec.dwControlsAccepted := 31;
  fStatusRec.dwCurrentState := dwState;
  fStatusRec.dwWin32ExitCode := dwExitCode;
  fStatusRec.dwWaitHint := dwWait;
  if (dwState = SERVICE_RUNNING) or
     (dwState = SERVICE_STOPPED) then
    fStatusRec.dwCheckPoint := 0
  else
    inc(fStatusRec.dwCheckPoint);
  result := SetServiceStatus(fStatusHandle, fStatusRec);
  if not result then
    DoLog(sllLastError, '% ReportStatus(%,%)=% SetServiceStatus() failed',
      [ServiceName, dwExitCode, dwWait, status^], self);
end;

procedure TService.SetControlHandler(const Value: TServiceControlHandler);
begin
  fControlHandler := Value;
end;

procedure TService.SetStatus(const Value: TServiceStatus);
begin
  fStatusRec := Value;
  if fStatusHandle <> 0 then
    SetServiceStatus(fStatusHandle, fStatusRec);
end;

procedure TService.Start;
begin
  with TServiceController.CreateOpenService(
    '', '', fServiceName, SERVICE_ALL_ACCESS) do
  try
    Start([]);
  finally
    Free;
  end;
end;

procedure TService.Stop;
begin
  with TServiceController.CreateOpenService(
    '', '', fServiceName, SERVICE_ALL_ACCESS) do
  try
    Stop;
  finally
    Free;
  end;
end;

procedure TService.ServiceProc(ArgCount: integer; Args: PPWideChar);
var
  i: PtrInt;
  t: TSynTempBuffer;
begin
  SetCurrentThreadName('ServiceProc');
  DoLog(sllTrace, 'ServiceProc: ArgCount=% ServiceSingle=%',
    [ArgCount, self], self);
  if self = nil then
    exit;
  dec(ArgCount); // first argument is the service name to be ignored
  if (Args = nil) or
     (ArgCount <= 0) then
    fArgsList := nil // no argument
  else
  begin
    SetLength(fArgsList, ArgCount);
    for i := 0 to ArgCount - 1 do
    begin
      inc(Args); // first was service name
      Win32PWideCharToUtf8(Args^, fArgsList[i]); // to string
    end;
  end;
  fStatusHandle := RegisterServiceCtrlHandlerW(
    Utf8ToWin32PWideChar(fServiceName, t), @ControlHandler);
  t.Done;
  if fStatusHandle = 0 then
  begin
    ReportStatus(SERVICE_STOPPED, GetLastError, 0);
    exit;
  end;
  ReportStatus(SERVICE_START_PENDING, 0, 0);
  Execute;
end;


function CurrentStateToServiceState(CurrentState: cardinal): TServiceState;
begin
  case CurrentState of
    SERVICE_STOPPED:
      result := ssStopped;
    SERVICE_START_PENDING:
      result := ssStarting;
    SERVICE_STOP_PENDING:
      result := ssStopping;
    SERVICE_RUNNING:
      result := ssRunning;
    SERVICE_CONTINUE_PENDING:
      result := ssResuming;
    SERVICE_PAUSE_PENDING:
      result := ssPausing;
    SERVICE_PAUSED:
      result := ssPaused;
  else
    // e.g. SERVICE_CONTROL_SHUTDOWN
    result := ssNotInstalled;
  end;
end;

function GetServicePid(const aServiceName: RawUtf8;
  aServiceState: PServiceState): cardinal;
var
  ss: TServiceState;
  st: TServiceStatus;
  ssp: TServiceStatusProcess;
  scm: THandle;
  svc: THandle;
  size: cardinal;
begin
  result := 0;
  ss := ssErrorRetrievingState;
  scm := OpenSCManagerW(nil, nil, SC_MANAGER_CONNECT);
  if scm <> 0 then
  try
    svc := OpenServiceInstance(scm, aServiceName, SERVICE_QUERY_STATUS);
    if svc <> 0 then
    try
      if QueryServiceStatusEx(svc, SC_STATUS_PROCESS_INFO,
          @ssp, SizeOf(TServiceStatusProcess), size) then
      begin
        result := ssp.dwProcessId;
        if aServiceState <> nil then
        begin
          FillCharFast(st, SizeOf(st), 0);
          QueryServiceStatus(svc, st);
          ss := CurrentStateToServiceState(st.dwCurrentState);
        end;
      end
      else
        TService.DoLog(sllLastError, 'GetServicePid(%)', [aServiceName], nil);
    finally
      CloseServiceHandle(svc);
    end
    else
      ss := ssNotInstalled;
  finally
    CloseServiceHandle(scm);
  end;
  if aServiceState <> nil then
    aServiceState^ := ss;
end;

{  function that a service process specifies as the entry point function
   of a particular service. The function can have any application-defined name
  - Args points to an array of pointers that point to null-terminated
    argument strings. The first argument in the array is the name of the service,
    and subsequent arguments are any strings passed to the service by the process
    that called the StartService function to start the service. Args can
    be nil if there are no arguments. }

procedure ServiceProc(ArgCount: cardinal; Args: PPWideChar); stdcall;
begin
  ServiceSingle.ServiceProc(ArgCount, Args);
end;

function ServiceSingleRun: boolean;
var
  S: array[0..1] of TServiceTableEntry;
  t: TSynTempBuffer;
begin
  if ServiceSingle = nil then
  begin
    result := false;
    exit;
  end;
  S[0].lpServiceName := Utf8ToWin32PWideChar(ServiceSingle.ServiceName, t);
  S[0].lpServiceProc := ServiceProc;
  S[1].lpServiceName := nil;
  S[1].lpServiceProc := nil;
  { TODO : disable EExternal exception logging in ServicesSingleRun? }
  result := StartServiceCtrlDispatcherW(@S);
  t.Done;
end;


{ TServiceSingle }

procedure SingleServiceControlHandler(Opcode: LongWord); stdcall;
begin
  if ServiceSingle <> nil then
    ServiceSingle.DoCtrlHandle(Opcode);
end;

constructor TServiceSingle.Create(const aServiceName, aDisplayName: RawUtf8);
begin
  if ServiceSingle <> nil then
    raise EOSException.Create('Only one TServiceSingle is allowed at a time');
  inherited Create(aServiceName, aDisplayName);
  ServiceSingle := self;
  SetControlHandler(SingleServiceControlHandler);
end;

destructor TServiceSingle.Destroy;
begin
  try
    inherited;
  finally
    if ServiceSingle = self then
      ServiceSingle := nil;
  end;
end;


function WaitProcess(pid: cardinal; waitseconds: integer): boolean;
var
  ph: THandle;
begin
  result := false;
  ph := OpenProcess(SYNCHRONIZE, false, pid);
  if ph = 0 then
    exit;
  result := WaitForSingleObject(ph, waitseconds * 1000) = WAIT_OBJECT_0;
  CloseHandle(ph);
end;

function CancelProcess(pid: cardinal; waitseconds: integer): boolean;
begin
  result := false;
  if integer(pid) <= 0 then
    exit;
  if GetConsoleWindow <> 0 then              // can attach to a single console
    FreeConsole;
  if not AttachConsole(pid) then             // attach to the pid console
    exit;
  SetConsoleCtrlHandler(nil, true);          // nil=ignore the event ourself
  GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0); // send Ctrl+C event
  FreeConsole;                               // detach
  SetConsoleCtrlHandler(nil, false);         // remove our nil=ignore handler
  result := WaitProcess(pid, waitseconds);
end;

function QuitProcess(pid: cardinal; waitseconds: integer): boolean;
var
  snap: THandle;
  e: TThreadEntry32;
begin
  result := false;
  if integer(pid) <= 0 then
    exit;
  snap := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if snap <= 0 then
    exit;
  FillCharFast(e, SizeOf(e), 0);
  e.dwSize := SizeOf(e);
  if Thread32First(snap, e) then // loop over all threads of the system
    repeat
      if e.th32OwnerProcessID = pid then
        if PostThreadMessage(e.th32ThreadID, WM_QUIT, 0, 0) then
          result := true; // at least one thread found
    until not Thread32Next(snap, e);
  CloseHandle(snap);
  if result and
     (waitseconds <> 0) then
    result := WaitProcess(pid, waitseconds);
end;

function KillProcess(pid: cardinal; waitseconds: integer): boolean;
var
  ph: THandle;
begin
  result := false;
  if integer(pid) <= 0 then
    exit;
  ph := OpenProcess(PROCESS_TERMINATE or SYNCHRONIZE, false, pid);
  if ph = 0 then
    exit;
  result := TerminateProcess(ph, 0) and
            (WaitForSingleObject(ph, waitseconds * 1000) <> WAIT_TIMEOUT);
  CloseHandle(ph);
end;

var
  OnHandleCtrlC: TThreadMethod;

function ConsoleCtrlHandler(typ : dword) : BOOL; stdcall;
begin
  result := false;
  if Assigned(OnHandleCtrlC) then
    case typ of
      CTRL_C_EVENT,
      CTRL_CLOSE_EVENT,
      CTRL_LOGOFF_EVENT,
      CTRL_SHUTDOWN_EVENT:
        begin
          OnHandleCtrlC();
          result := true;
        end;
    end;
end;

function HandleCtrlC(const OnClose: TThreadMethod): boolean;
begin
  result := SetConsoleCtrlHandler(@ConsoleCtrlHandler, Assigned(OnClose));
  if result then
    OnHandleCtrlC := OnClose;
end;

function DropPriviledges(const UserName: RawUtf8): boolean;
begin
  result := false;
end;

function ChangeRoot(const FolderName: RawUtf8): boolean;
begin
  result := false;
end;

type
  TJobObjectInfoClass = (
    BasicLimitInformation = 2,
    JobObjectBasicProcessIdList = 3,
    BasicUIRestrictions = 4,
    SecurityLimitInformation = 5,
    EndOfJobTimeInformation = 6,
    AssociateCompletionPortInformation = 7,
    ExtendedLimitInformation = 9,
    GroupInformation = 11);

  TJobObjectBasicLimitInformation = record
    PerProcessUserTimeLimit: LARGE_INTEGER;
    PerJobUserTimeLimit: LARGE_INTEGER;
    LimitFlags: DWord;
    MinimumWorkingSetSize: PtrUInt;
    MaximumWorkingSetSize: PtrUInt;
    ActiveProcessLimit: DWord;
    Affinity: PtrUInt;
    PriorityClass: DWord;
    SchedulingClass: DWord;
  end;

  TIOCounter = record
    ReadOperationCount: QWord;
    WriteOperationCount: QWord;
    OtherOperationCount: QWord;
    ReadTransferCount: QWord;
    WriteTransferCount: QWord;
    OtherTransferCount: QWord;
  end;

  TJobObjectExtendedLimitInformation = record
    BasicLimitInformation: TJobObjectBasicLimitInformation;
    IoInfo: TIOCounter;
    ProcessMemoryLimit: PtrUInt;
    JobMemoryLimit: PtrUInt;
    PeakProcessMemoryUsed: PtrUInt;
    PeakJobMemoryUsed: PtrUInt;
  end;

const
  // to create a child process in a new job object
  // https://learn.microsoft.com/en-us/windows/win32/procthread/job-objects
  CREATE_BREAKAWAY_FROM_JOB =  $1000000;

  JOB_OBJECT_LIMIT_PROCESS_MEMORY             = $00000100;
  JOB_OBJECT_LIMIT_JOB_MEMORY                 = $00000200;
  JOB_OBJECT_LIMIT_DIE_ON_UNHANDLED_EXCEPTION = $00000400;
  JOB_OBJECT_LIMIT_BREAKAWAY_OK               = $00000800;
  JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK        = $00001000;
  JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE          = $00002000;

function CreateJobObjectA(lpJobAttributes: PSecurityAttributes;
   lpName: PWideChar): THandle;
  stdcall; external kernel32;
function SetInformationJobObject(hJob: THandle;
   JobObjectInformationClass: TJobObjectInfoClass; lpJobObjectInformation: pointer;
   cbJobObjectInformationLength: DWord): BOOL;
  stdcall; external kernel32;
function AssignProcessToJobObject(hJob, hProcess: THandle): BOOL;
  stdcall; external kernel32;

// redefined here so that we can share code with FPC and Delphi
function CreateProcessW(lpApplicationName: PWideChar; lpCommandLine: PWideChar;
   lpProcessAttributes, lpThreadAttributes: PSecurityAttributes;
   bInheritHandles: BOOL; dwCreationFlags: cardinal; lpEnvironment: Pointer;
   lpCurrentDirectory: PWideChar; const lpStartupInfo: TStartupInfo;
   out lpProcessInformation: TProcessInformation): BOOL;
  stdcall; external kernel32;

function GetExitCodeProcess(hProcess: THandle; out lpExitCode: cardinal): BOOL;
  stdcall; external kernel32;

function CreateJobToClose(parentpid: cardinal): THandle;
var
  security: TSecurityAttributes;
  limits: TJobObjectExtendedLimitInformation;
  jobname: RawUtf8;
begin
  security.nLength := SizeOf(security);
  security.bInheritHandle := false; // should be false
  security.lpSecurityDescriptor := nil;
  _fmt('AutoCloseChild%d', [parentpid], jobname);
  result := CreateJobObjectA(@security, pointer(jobname));
  if result = 0 then
    exit;
  FillCharFast(limits, SizeOf(limits), 0);
  limits.BasicLimitInformation.LimitFlags :=
    JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE or
    JOB_OBJECT_LIMIT_BREAKAWAY_OK;
  if SetInformationJobObject(result, ExtendedLimitInformation,
       @limits, SizeOf(limits)) then
    exit;
  CloseHandle(result); // error initializing the job (too old or too new OS?)
  result := 0;
end;

function AssignJobToProcess(job, process: THandle; const ctxt: ShortString): boolean;
begin
  result := (job <> 0) and
            AssignProcessToJobObject(job, process);
  if result then
    TService.DoLog(sllTrace, 'RunCommand: % AssignProcessToJobObject success',
      [ctxt], nil)
  else
    TService.DoLog(sllDebug, 'RunCommand: % AssignProcessToJobObject failed % %',
      [ctxt, GetLastError, WinErrorConstant(GetLastError)], nil);
end;

function RunProcess(const path, arg1: TFileName; waitfor: boolean;
  const arg2, arg3, arg4, arg5, env: TFileName; options: TRunOptions): integer;
begin
  result := RunCommand(Format('"%s" %s %s %s %s %s',
    [path, arg1, arg2, arg3, arg4, arg5]), waitfor, env, options);
end;

var
  EnvironmentCache: SynUnicode;
  EnvironmentCacheLock: TLightLock; // just set once

procedure GetEnvironmentCache;
var
  e, p: PWideChar;
begin
  EnvironmentCacheLock.Lock;
  if EnvironmentCache = '' then
  begin
    e := GetEnvironmentStringsW;
    p := e;
    while p^ <> #0 do
      inc(p, StrLenW(p) + 1); // go to name=value#0 pairs end
    SetString(EnvironmentCache, e, (PtrUInt(p) - PtrUInt(e)) shr 1);
    FreeEnvironmentStringsW(e);
  end;
  EnvironmentCacheLock.UnLock;
end;

function RunCommand(const cmd: TFileName; waitfor: boolean; const env: TFileName;
  options: TRunOptions; waitfordelayms: cardinal; processhandle: PHandle;
  redirected: PRawByteString; const onoutput: TOnRedirect;
  const wrkdir: TFileName): integer;
var
  startupinfo: TStartupInfo; // _STARTUPINFOW or _STARTUPINFOA is equal here
  processinfo: TProcessInformation;
  security: TSecurityAttributes;
  exe, path: TFileName;
  rd, wr, job: THandle;
  // CreateProcess can alter the strings -> use local SynUnicode temp variables
  wcmd, wenv, wpath: SynUnicode;
  endtix: Int64;
  flags, exitcode, res: cardinal;
  ram: TRunAbortMethods;
  created, terminated: boolean;
  i, l: PtrInt;

  procedure RedirectOutput(flush: boolean);
  var
    new: RawByteString;
    pending, n: cardinal;
    tmp: TSynTempBuffer;
  begin
    repeat
      pending := 0;
      if not PeekNamedPipe(rd, nil, 0, nil, @pending, nil) or
         (pending = 0) then
      begin
        if (not flush) and
           Assigned(onoutput) and
           onoutput('', processinfo.dwProcessId) then
            exitcode := WAIT_OBJECT_0; // onoutput() returned true to abort
        break;
      end;
      if pending > SizeOf(tmp) then
        pending := SizeOf(tmp);
      n := 0;
      Win32Check(ReadFile(rd, tmp, pending, n, nil));
      if n <= 0 then
        break;
      if redirected <> nil then
      begin
        SetLength(redirected^, l + PtrInt(n));
        MoveFast(tmp, PByteArray(redirected^)[l], n); // append without convert
        inc(l, n);
      end;
      if Assigned(onoutput) then
      begin
        SetString(new, PAnsiChar(@tmp), n);
        if onoutput(new, processinfo.dwProcessId) then // notify new content
          // onoutput() callback returned true to stop the execution
          if not flush then
          begin
            exitcode := WAIT_OBJECT_0;
            break;
          end;
      end;
    until false;
  end;

begin
  // https://support.microsoft.com/en-us/help/175986/info-understanding-createprocess-and-command-line-arguments
  result := -1;
  // extract path and exe from cmd input
  if cmd = '' then
    exit;
  if cmd[1] = '"' then
  begin
    exe := copy(cmd, 2, maxInt);
    i := Pos('"', exe);
    if i = 0 then
      exit;
    SetLength(exe, i - 1); // unquote "exe" string
  end
  else
  begin
    i := Pos(' ', cmd);
    if i = 0 then
      exe := cmd // no parameter
    else
      exe := copy(cmd, 1, i - 1); // split exe and parameter(s)
  end;
  path := wrkdir;
  if (path = '') and
     (exe <> '') then
    path := ExtractFilePath(ExpandFileName(exe));
  if (path = '') and
     FileExists(Executable.ProgramFilePath + exe) then
    path := Executable.ProgramFilePath; // prefers the current folder
  // prepare the CreateProcess arguments
  wcmd := SynUnicode(cmd);
  UniqueString(wcmd);
  wpath := SynUnicode(path);
  if env <> '' then
  begin
    wenv := SynUnicode(env);
    if roEnvAddExisting in options then
    begin
      if EnvironmentCache = '' then
        GetEnvironmentCache;
      wenv := EnvironmentCache + wenv;
    end
    else
      UniqueString(wenv);
  end;
  security.nLength := SizeOf(security);
  security.bInheritHandle := true;
  security.lpSecurityDescriptor := nil;
  // launch the process
  FillCharFast(startupinfo, SizeOf(startupinfo), 0);
  startupinfo.cb := SizeOf(startupinfo);
  ram := RunAbortMethods;
  l := 0;
  rd := 0;
  job := 0;
  if Assigned(onoutput) or
     (redirected <> nil) then
    if CreatePipe(rd, wr, @security, 0) then
    begin
      SetHandleInformation(rd, HANDLE_FLAG_INHERIT, 0);
      startupinfo.wShowWindow := SW_HIDE;
      startupinfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
      startupinfo.hStdOutput := wr;
      startupinfo.hStdError := wr;
      startupinfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      if redirected <> nil then
        redirected^ := '';
      exclude(ram, ramCtrlC); // not compatible with redirection
    end
    else
      rd := 0;
  // https://docs.microsoft.com/en-en/windows/desktop/ProcThread/process-creation-flags
  FillCharFast(processinfo, SizeOf(processinfo), 0);
  flags := CREATE_UNICODE_ENVIRONMENT or CREATE_DEFAULT_ERROR_MODE;
  if not (roWinNoProcessDetach in options) then
    flags := flags or (DETACHED_PROCESS or CREATE_NEW_PROCESS_GROUP);
  if roWinJobCloseChildren in options then
    // create the child process in a new job object
    flags := flags or CREATE_BREAKAWAY_FROM_JOB;
  // actually create the new process
  created := CreateProcessW(nil, pointer(wcmd), @security, @security, true,
    flags, pointer({%H-}wenv), pointer(wpath), startupinfo, processinfo);
  if (not created) and
     (flags and CREATE_BREAKAWAY_FROM_JOB <> 0) then
  begin
    TService.DoLog(sllTrace,
      'RunCommand: unsupported CREATE_BREAKAWAY_FROM_JOB = % %',
      [GetLastError, WinErrorConstant(GetLastError)], nil);
    flags := flags and (not CREATE_BREAKAWAY_FROM_JOB);
    wcmd := SynUnicode(cmd); // CreateProcesW() modified wcmd content: recreate
    UniqueString(wcmd);
    created := CreateProcessW(nil, pointer(wcmd), @security, @security, true,
      flags, pointer({%H-}wenv), pointer(wpath), startupinfo, processinfo);
  end;
  if not created then
  begin
    result := -GetLastError; // returns CreateProcessW() error as negative
    TService.DoLog(sllTrace,  'RunCommand: CreateProcess = % %',
      [-result, WinErrorConstant(-result)], nil);
    exit;
  end;
  // setup the newly created process
  if processhandle <> nil then
    processhandle^ := processinfo.hProcess;
  if flags and CREATE_BREAKAWAY_FROM_JOB <> 0 then
  begin
    job := CreateJobToClose(processinfo.dwProcessID);
    if (job <> 0) and
       not AssignJobToProcess(job, processinfo.hProcess, 'CloseChildren') then
    begin
      CloseHandle(job);
      job := 0;
    end;
  end;
  if Assigned(onoutput) then
    onoutput('', processinfo.dwProcessId);
  // main wait (and redirect) until the process is finished (or not)
  if rd <> 0 then
  begin
    // wait and redirect - see https://stackoverflow.com/a/25725197/458259
    CloseHandle(wr);
    if waitfordelayms = INFINITE then
      endtix := 0
    else
      endtix := GetTickCount64 + waitfordelayms;
    repeat
      exitcode := WaitForSingleObject(processinfo.hProcess, 50);
      // note: WaitForMultipleObjects() with rd burns 100% of one core :(
      Win32Check(exitcode <> WAIT_FAILED);
      RedirectOutput({flush=}false);
    until (exitcode = WAIT_OBJECT_0) or
          ((endtix <> 0) and
           (GetTickCount64 > endtix));
    if GetExitCodeProcess(processinfo.hProcess, exitcode) and
       (exitcode <> STILL_ACTIVE) then
      result := exitcode // process ended from natural death -> return code
    else
    begin
      result := -GetLastError; // not able to retrieve exit code
      // e.g. -STILL_ACTIVE if aborted by onoutput()=true above
      terminated := false;
      if RunAbortTimeoutSecs > 0 then
      begin
        if ramCtrlC in ram then  // try Ctrl+C (disabled above)
        begin
          terminated := CancelProcess(processinfo.dwProcessId, RunAbortTimeoutSecs);
          TService.DoLog(sllTrace, 'RunCommand: CancelProcess(%)=%',
            [processinfo.dwProcessId, ord(terminated)], nil);
        end;
        if (not terminated) and
           (ramQuit in ram) then
        begin // try WM_QUIT
          terminated := QuitProcess(processinfo.dwProcessId, 0);
          TService.DoLog(sllTrace, 'RunCommand: QuitProcess(%)=%',
            [processinfo.dwProcessId, ord(terminated)], nil);
          if terminated then
          begin
            endtix := GetTickCount64 + RunAbortTimeoutSecs * 1000; // wait ended
            repeat
              res := WaitForSingleObject(processinfo.hProcess, 10);
              RedirectOutput({flush=}true); // mandatory to unlock pipe
            until (res <> WAIT_TIMEOUT) or
                  (GetTickCount64 > endtix);
            terminated := res = WAIT_OBJECT_0;
          end;
        end;
      end;
      RedirectOutput({flush=}true); // ensure there is no pending data
      if terminated and
         // gracefully ended -> try to retrieve the exit code
         GetExitCodeProcess(processinfo.hProcess, exitcode) then
        result := exitcode
      else
      begin
        TerminateProcess(processinfo.hProcess, result); // forced kill
        TService.DoLog(sllTrace, 'RunCommand: TerminateProcess(%)=%',
          [processinfo.dwProcessId, result], nil);
      end;
    end;
  end
  else if waitfor then
    if WaitForSingleObject(processinfo.hProcess, waitfordelayms) = WAIT_FAILED then
      if waitfordelayms <> INFINITE then
        result := -1 // still runing after waitfordelayms
      else
        result := -GetLastError // failed to wait
    else if GetExitCodeProcess(processinfo.hProcess, exitcode) then
        result := exitcode      // waited for process to end -> return code
      else
        result := -GetLastError // was not able to retrieve exit code
  else
    // waitfor is false: asynchronous process launch
    result := 0;
  // release the handles created for this process
  CloseHandle(processinfo.hProcess);
  CloseHandle(processinfo.hThread);
  if rd <> 0 then // CloseHandle(wr) has already be done
    CloseHandle(rd);
  if job <> 0 then
    CloseHandle(job);
end;

function RunRedirect(const cmd: TFileName; exitcode: PInteger;
  const onoutput: TOnRedirect; waitfordelayms: cardinal; setresult: boolean;
  const env, wrkdir: TFileName; options: TRunOptions): RawByteString;
var
  res: integer;
  redir: PRawByteString;
begin
  result := '';
  if setresult then
    redir := @result
  else
    redir := nil;
  res := RunCommand(cmd, true, env, options, waitfordelayms, nil,
    redir, onoutput, wrkdir);
  if exitcode <> nil then
    exitcode^ := res;
end;


{ ****************** Gather Operating System Information }

const
  // lpMinimumApplicationAddress retrieved from Windows is very low ($10000)
  // - i.e. maximum number of ID per table would be 65536 in TOrm.GetID
  // - so we'll force an higher and almost "safe" value as 1,048,576
  // (real value from runnning Windows is greater than $400000)
  MIN_PTR_VALUE = $100000;

  // see http://msdn.microsoft.com/en-us/library/ms724833(v=vs.85).aspx
  VER_NT_WORKSTATION = 1;
  VER_NT_DOMAIN_CONTROLLER = 2;
  VER_NT_SERVER = 3;
  SM_SERVERR2 = 89;
  PROCESSOR_ARCHITECTURE_AMD64 = 9;

type
  TSystemLogicalProcessorRelation = (
    RelationProcessorCore,
    RelationNumaNode,
    RelationCache,
    RelationProcessorPackage,
    RelationGroup);
  TSystemLogicalProcessorCache = (
    CacheUnified,
    CacheInstruction,
    CacheData,
    CacheTrace);

  {$ifdef CPU64}
  {$A8}
  {$else}
  {$A4}
  {$endif CPU64}
  TSystemLogicalProcessorInformation = record
    ProcessorMask: PtrUInt;
    case Relationship: TSystemLogicalProcessorRelation of
      RelationProcessorCore: (
        ProcessorCoreFlags: BYTE);
      RelationNumaNode: (
        NumaNodeNumber: DWord);
      RelationCache: (
        Cache: record
          Level: BYTE;
          Associativity: BYTE;
          LineSize: WORD;
          Size: DWord;
          CacheType: TSystemLogicalProcessorCache;
        end);
      RelationGroup: (
        Reserved: array [0..1] of QWord); // to define the actual struct size
  end;
  {$A+}


{$ifndef UNICODE}
function GetVersionEx(var lpVersionInformation: TOSVersionInfoEx): BOOL;
  stdcall; external kernel32 name 'GetVersionExA';
{$endif UNICODE}

function GetLocalTimeOffset: Integer; // not defined in oldest Delphi
var
  tzi: TTimeZoneInformation;
begin
   case GetTimeZoneInformation(tzi) of
     TIME_ZONE_ID_UNKNOWN:
       result := tzi.Bias;
     TIME_ZONE_ID_STANDARD:
       result := tzi.Bias + tzi.StandardBias;
     TIME_ZONE_ID_DAYLIGHT:
       result := tzi.Bias + tzi.DaylightBias;
   else
     result := 0;
   end;
end;

function UUID_CACHE: TFileName;
begin // where to cache our computed UUID as a local file
  result := GetSystemPath(spCommonData) + 'synopse.uuid';
end;

procedure InitializeSpecificUnit;
var
  h: THandle;
  IsWow64Process: function(Handle: THandle; var Res: BOOL): BOOL; stdcall;
  GetNativeSystemInfo: procedure(var SystemInfo: TSystemInfo); stdcall;
  GetLogicalProcessorInformation: function(
   var Info: TSystemLogicalProcessorInformation; Len: PDWord): BOOL; stdcall;
  wine_get_version: function: PAnsiChar; stdcall;
  mem: TMemoryStatusEx;
  Res: BOOL;
  P: pointer;
  Vers: TWindowsVersion;
  cpu, manuf, prod, prodver: RawUtf8;
  reg: TWinRegistry;
  proc: array of TSystemLogicalProcessorInformation;
  i: integer;
  siz: DWord;
begin
  {$ifdef ASMX86}
  {$ifndef HASNOSSE2}
  if not (cfSSE2 in CpuFeatures) then
  begin
    // avoid illegal opcode in MoveFast() and SynLZ functions
    {$ifdef ISDELPHI} // FPC_X86 already redirect to FastCode RTL Move()
    RedirectCode(@MoveFast, @System.Move);
    {$endif ISDELPHI}
    RedirectCode(@SynLZcompress1, @SynLZcompress1Pas);
    RedirectCode(@SynLZdecompress1, @SynLZdecompress1Pas);
    ConsoleWrite('WARNING: too old CPU - recompile with HASNOSSE2', ccLightRed);
    // note: FillCharFast is handled by mormot.core.base via ERMSB
    // and Byte/Word/IntegerScanIndex() are likely to GPF at runtime
  end;
  {$endif HASNOSSE2}
  {$endif ASMX86}
  // late-binding of newest Windows APIs
  h := GetModuleHandle(kernel32);
  GetTickCount64 := GetProcAddress(h, 'GetTickCount64');
  if not Assigned(GetTickCount64) then // WinXP+
    GetTickCount64 := @GetTickCount64ForXP;
  GetSystemTimePreciseAsFileTime :=
    GetProcAddress(h, 'GetSystemTimePreciseAsFileTime');
  if not Assigned(GetSystemTimePreciseAsFileTime) then // Win8+
    GetSystemTimePreciseAsFileTime := @GetSystemTimeAsFileTime;
  {$ifdef WITH_VECTOREXCEPT}
  AddVectoredExceptionHandler :=
    GetProcAddress(h, 'AddVectoredExceptionHandler');
  {$endif WITH_VECTOREXCEPT}
  QueryPerformanceFrequency(PInt64(@_QueryPerformanceFrequency)^);
  if _QueryPerformanceFrequency = 0 then
    raise Exception.Create('QueryPerformanceFrequency=0'); // paranoid
  _QueryPerformanceFrequencyPer10 := _QueryPerformanceFrequency = 10000000;
  IsWow64Process := GetProcAddress(h, 'IsWow64Process');
  Res := false;
  IsWow64 := Assigned(IsWow64Process) and
             IsWow64Process(GetCurrentProcess, Res) and
             Res;
  if IsWow64 then
    // see http://msdn.microsoft.com/en-us/library/ms724381(v=VS.85).aspx
    GetNativeSystemInfo := GetProcAddress(h, 'GetNativeSystemInfo')
  else
    @GetNativeSystemInfo := nil;
  GetSystemTimes := GetProcAddress(h, 'GetSystemTimes');
  GetProcessTimes := GetProcAddress(h, 'GetProcessTimes');
  QueryFullProcessImageNameW := GetProcAddress(h, 'QueryFullProcessImageNameW');
  GetLogicalProcessorInformation := GetProcAddress(h, 'GetLogicalProcessorInformation');
  InitializeSRWLock := GetProcAddress(h, 'InitializeSRWLock');
  AcquireSRWLockExclusive := GetProcAddress(h, 'AcquireSRWLockExclusive');
  ReleaseSRWLockExclusive := GetProcAddress(h, 'ReleaseSRWLockExclusive');
  if not Assigned(InitializeSRWLock) or
     not Assigned(AcquireSRWLockExclusive) or
     not Assigned(ReleaseSRWLockExclusive) then
  begin // SRW was introduced with Vista: on XP, fallback to our TLightLock
    InitializeSRWLock := @InitializeSRWLockForXP;
    AcquireSRWLockExclusive := @AcquireSRWLockExclusiveForXP;
    ReleaseSRWLockExclusive := @ReleaseSRWLockExclusiveForXP;
  end;
  // retrieve system information
  TimeZoneLocalBias := -GetLocalTimeOffset;
  FillcharFast(SystemInfo, SizeOf(SystemInfo), 0);
  if Assigned(GetNativeSystemInfo) then
    GetNativeSystemInfo(SystemInfo)
  else
    Windows.GetSystemInfo(SystemInfo);
  GetMem(P, 10); // ensure that using MIN_PTR_VALUE won't break anything
  if (PtrUInt(P) > MIN_PTR_VALUE) and
     (PtrUInt(SystemInfo.lpMinimumApplicationAddress) <= MIN_PTR_VALUE) then
    PtrUInt(SystemInfo.lpMinimumApplicationAddress) := MIN_PTR_VALUE;
  Freemem(P);
  FillCharFast(mem, SizeOf(mem), 0);
  mem.dwLength := SizeOf(mem);
  if GlobalMemoryStatusEx(mem) then
    SystemMemorySize := mem.ullTotalPhys;
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  Vers := wUnknown;
  with OSVersionInfo do
    // see https://msdn.microsoft.com/en-us/library/windows/desktop/ms724833
    case dwMajorVersion of
      5:
        case dwMinorVersion of
          0:
            Vers := w2000;
          1:
            Vers := wXP;
          2:
            if (wProductType = VER_NT_WORKSTATION) and
               (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
              Vers := wXP_64
            else if GetSystemMetrics(SM_SERVERR2) = 0 then
              Vers := wServer2003
            else
              Vers := wServer2003_R2;
        end;
      6:
        case dwMinorVersion of
          0:
            Vers := wVista;
          1:
            Vers := wSeven;
          2:
            Vers := wEight;
          3:
            Vers := wEightOne;
          4:
            Vers := wTen;
        end;
      10:
        Vers := wTen;
    end;
  if Vers >= wVista then
  begin
    // see https://en.wikipedia.org/wiki/List_of_Microsoft_Windows_versions
    if OSVersionInfo.wProductType <> VER_NT_WORKSTATION then
    begin
      // Server edition
      inc(Vers, 2); // e.g. wEight -> wServer2012
      if Vers = wServer2016 then
        // we identify only LTSC server versions
        if OSVersionInfo.dwBuildNumber >= 17763 then
          if OSVersionInfo.dwBuildNumber >= 20285 then // released as 20348
            Vers := wServer2022_64
          else
            Vers := wServer2019_64;
    end
    else if (Vers = wTen) and
            (OSVersionInfo.dwBuildNumber >= 22000) then
      // Windows 11 has always 22000.###
      Vers := wEleven;
    if (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) and
       (Vers < wServer2019_64) then
      inc(Vers);   // e.g. wEight -> wEight64
  end;
  OSVersion := Vers;
  OpenProcessAccess := PROCESS_QUERY_LIMITED_INFORMATION;
  if Vers < wVista then
    OpenProcessAccess := PROCESS_QUERY_INFORMATION or PROCESS_VM_READ;
  OSVersion32.os := osWindows;
  OSVersion32.win := Vers;
  OSVersion32.winbuild := OSVersionInfo.dwBuildNumber;
  h := GetModuleHandle(ntdll);
  if h > 0 then
  begin
    wine_get_version := GetProcAddress(h, 'wine_get_version');
    if Assigned(wine_get_version) then
    begin
      OSVersionInfoEx := wine_get_version;
      OSVersionInfoEx := TrimU('Wine ' + TrimU(OSVersionInfoEx));
    end;
    NtQueryInformationProcess := GetProcAddress(h, 'NtQueryInformationProcess');
    RtlInitUnicodeString      := GetProcAddress(h, 'RtlInitUnicodeString');
    NtOpenSection             := GetProcAddress(h, 'NtOpenSection');
    NtMapViewOfSection        := GetProcAddress(h, 'NtMapViewOfSection');
    NtUnmapViewOfSection      := GetProcAddress(h, 'NtUnmapViewOfSection');
  end;
  // retrieve Software/Hardware information from Registry
  if reg.ReadOpen(wrLocalMachine, 'Software\Microsoft\Windows NT\CurrentVersion') then
  begin
    WindowsUbr := reg.ReadDword('UBR');
    WindowsProductName := reg.ReadString('ProductName');
    WindowsDisplayVersion := reg.ReadString('DisplayVersion');
  end;
  with OSVersionInfo do
  begin
    _fmt('Windows %s (%d.%d.%d)', [WINDOWS_NAME[Vers],
      dwMajorVersion, dwMinorVersion, dwBuildNumber], OSVersionText);
    if wServicePackMajor <> 0 then
      insert(_fmt('SP%d ', [wServicePackMajor]), OSVersionText, PosExChar('(', OSVersionText));
  end;
  if WindowsUbr <> 0 then
    insert(_fmt('.%d', [WindowsUbr]), OSVersionText, length(OSVersionText));
  if WindowsDisplayVersion <> '' then
    insert(WindowsDisplayVersion + ' ', OSVersionText, PosExChar('(', OSVersionText));
  if OSVersionInfoEx <> '' then
    OSVersionText := OSVersionText + ' - ' + OSVersionInfoEx;
  if reg.ReadOpen(wrLocalMachine, 'Hardware\Description\System\CentralProcessor\0', true) then
  begin
    cpu := reg.ReadString('ProcessorNameString');
    if cpu = '' then
      cpu := reg.ReadString('Identifier');
  end;
  if reg.ReadOpen(wrLocalMachine, 'Hardware\Description\System\BIOS', true) then
  begin
    manuf := reg.ReadString('SystemManufacturer');
    if manuf <> '' then
      manuf := manuf + ' ';
    prod := reg.ReadString('SystemProductName');
    prodver := reg.ReadString('SystemVersion');
    if prodver = '' then
      prodver := reg.ReadString('BIOSVersion');
  end;
  if ({%H-}prod = '') or
     ({%H-}prodver = '') then
  begin
    if reg.ReadOpen(wrLocalMachine, 'Hardware\Description\System', true) then
    begin
      if prod = '' then
        prod := reg.ReadString('SystemBiosVersion');
      if prodver = '' then
        prodver := reg.ReadString('VideoBiosVersion');
    end;
  end;
  reg.Close;
  BiosInfoText := manuf{%H-} + prod;
  if prodver <> '' then
    BiosInfoText := BiosInfoText + ' ' + prodver;
  if {%H-}cpu = '' then
    cpu := RawUtf8(GetEnvironmentVariable('PROCESSOR_IDENTIFIER'));
  if Assigned(GetLogicalProcessorInformation) then
  begin
    SetLength(proc, 1024);
    siz := SizeOf(proc[0]) * length(proc);
    if GetLogicalProcessorInformation(proc[0], @siz) then
    begin
      for i := 0 to (siz div SizeOf(proc[0])) - 1 do
        with proc[i] do
          case Relationship of
            RelationProcessorPackage: // physical processor socket
              AddPtrUInt(TPtrUIntDynArray(CpuSocketsMask), CpuSockets, ProcessorMask);
            RelationCache:            // raw cache information
              if Cache.CacheType in [CacheUnified, CacheData] then
                if (Cache.Level >= low(CpuCache)) and
                   (Cache.Level <= high(CpuCache)) then
                  with CpuCache[Cache.Level] do
                    if (Count = 0) or
                       (Cache.CacheType <> CacheUnified) then
                    begin
                      inc(Count);
                      Size := Cache.Size;
                      LineSize := Cache.LineSize;
                    end;
          end;
      for i := high(CpuCache) downto low(CpuCache) do
      begin
        CpuCacheSize := CpuCache[i].Size;
        if CpuCacheSize <> 0 then // append the biggest level Cache size
        begin
          cpu := _fmt('%s [%s]', [cpu, _oskb(CpuCacheSize)]);
          break;
        end;
      end;
      for i := low(CpuCache) to high(CpuCache) do
        with CpuCache[i] do
          if Count <> 0 then
            if Count = 1 then
              CpuCacheText :=
                _fmt('%s L%d=%s ', [CpuCacheText, i, _oskb(Size)])
            else
              CpuCacheText :=
                _fmt('%s L%d=%d*%s ', [CpuCacheText, i, Count, _oskb(Size)]);
      TrimSelf(CpuCacheText);
    end;
  end;
  if CpuSockets = 0 then
    CpuSockets := 1; // e.g. on XP prior to SP3
  _fmt('%d x %s (' + CPU_ARCH_TEXT + ')',
    [SystemInfo.dwNumberOfProcessors, cpu], CpuInfoText);
  // writeln(CpuInfoText); writeln(CpuCacheText);
end;

procedure FinalizeSpecificUnit;
begin
  if CryptoApi.Handle <> 0 then
    Windows.FreeLibrary(CryptoApi.Handle);
  if CoInitCounter <> 0 then
    ConsoleWrite('Missing CoUninit (e.g. TOleDBConnection.Destroy call)');
end;


