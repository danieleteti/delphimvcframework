{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   POSIX API calls for FPC, as used by mormot.core.os.pas
}

uses
  baseunix,
  unix,
  unixcp,
  unixtype,
  unixutil, // for TZSeconds - as used by sysutils anyway
  {$ifdef OSBSDDARWIN}
  sysctl,
  {$else}
  linux,
  syscall,
  {$endif OSBSDDARWIN}
  {$ifdef FPCUSEVERSIONINFO} // to be enabled in mormot.defines.inc
    fileinfo,        // FPC 3.0 and up
    {$ifdef OSDARWIN}
      machoreader,   // MACH-O executables
    {$else}
      elfreader,     // ELF executables
    {$endif OSDARWIN}
  {$endif FPCUSEVERSIONINFO}
  errors,
  termio,
  dl,
  initc; // we statically link the libc for some raw calls

{$ifdef UNICODE}
  'mORMot assumes no UNICODE on POSIX, i.e. as TFileName = PChar = PUtf8Char'
{$endif UNICODE}


// define some raw text functions, to avoid linking mormot.core.text

function IdemPChar(p, up: PUtf8Char): boolean; inline;
var
  c, u: AnsiChar;
begin
  // we know that p<>nil and up<>nil within this unit
  result := false;
  repeat
    u := up^;
    if u = #0 then
      break;
    inc(up);
    c := p^;
    inc(p);
    if c <> u then
      if (c >= 'a') and
         (c <= 'z') then
      begin
        dec(c, 32);
        if c <> u then
          exit;
      end
      else
        exit;
  until false;
  result := true;
end;

function IdemPChars(const s: RawUtf8; const up: array of PUtf8Char): boolean;
var
  i: PtrInt;
begin
  if s <> '' then
  begin
    result := true;
    for i := 0 to high(up) do
      if IdemPChar(pointer(s), up[i]) then
        exit;
  end;
  result := false;
end;

procedure FindNameValue(const s, up: RawUtf8; var res: RawUtf8);
var
  p: PUtf8Char;
  L: PtrInt;
begin
  p := pointer(s);
  while p <> nil do
  begin
    if IdemPChar(p, pointer(up)) then
    begin
      inc(p, length(up));
      while (p^ <= ' ') and
            (p^ <> #0) do
        inc(p); // trim left
      L := 0;
      while p[L] > #13 do
        inc(L);
      while p[L - 1] = ' ' do
        dec(L); // trim right
      FastSetString(res, p, L);
      exit;
    end;
    p := GotoNextLine(p);
  end;
  res := '';
end;

function GetNextCardinal(var P: PAnsiChar): PtrUInt;
var
  c: cardinal;
begin
  result := 0;
  while not (P^ in ['0'..'9']) do
    if P^ = #0 then
      exit
    else
      inc(P);
  repeat
    c := ord(P^) - 48;
    if c > 9 then
      break;
    result := result * 10 + c;
    inc(P);
  until false;
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

procedure RawUtf8Append(var s: RawUtf8; p: PUtf8Char; l: PtrInt);
var
  n: PtrInt;
begin
  if l = 0 then
    exit;
  n := length(s);
  SetLength(s, n + l);
  MoveFast(p^, PByteArray(s)[n], l);
end;

function _fmt(const Fmt: string; const Args: array of const): RawUtf8; overload;
begin
  result := RawUtf8(format(Fmt, Args)); // good enough (seldom called)
end;

procedure _fmt(const Fmt: string; const Args: array of const;
  var result: RawUtf8); overload;
begin
  result := RawUtf8(format(Fmt, Args)); // good enough (seldom called)
end;


{ ****************** Unicode, Time, File process }

function LibraryOpen(const LibraryName: TFileName): TLibHandle;
begin
  result := TLibHandle(dlopen(pointer(LibraryName), RTLD_LAZY));
end;

procedure LibraryClose(Lib: TLibHandle);
begin
  dlclose(pointer(Lib));
end;

function LibraryResolve(Lib: TLibHandle; ProcName: PAnsiChar): pointer;
begin
  result := dlsym(pointer(Lib), ProcName);
end;

function LibraryError: string;
begin
  result := dlerror;
end;


{ TIcuLibrary }

procedure TIcuLibrary.Done;
begin
  if icui18n <> nil then
    dlclose(icui18n);
  if icu <> nil then
    dlclose(icu);
  if icudata <> nil then
    dlclose(icudata);
  icu := nil;
  icudata := nil;
  icui18n := nil;
  @ucnv_open := nil;
end;

function TIcuLibrary.IsAvailable: boolean;
begin
  if not Loaded then
    DoLoad;
  result := Assigned(ucnv_open);
end;

function IsWideStringManagerProperlyInstalled: boolean;
const
  u: WideChar = #$2020; // should convert to dagger glyph #134 in CP 1252
var
  d: RawByteString;
begin
  try
    widestringmanager.Unicode2AnsiMoveProc(@u, d, 1252, 1);
    result := (length(d) = 1) and
              (d[1] = #134);
    // the default RTL handler would just return '?'
  except
    result := false;
  end;
end;

procedure TIcuLibrary.DoLoad(const LibName: TFileName; Version: string);
const
  NAMES: array[0..12] of string = (
    'ucnv_open',
    'ucnv_close',
    'ucnv_setSubstChars',
    'ucnv_setFallback',
    'ucnv_fromUChars',
    'ucnv_toUChars',
    'u_strToUpper',
    'u_strToLower',
    'u_strCompare',
    'u_strCaseCompare',
    'u_getDataDirectory',
    'u_setDataDirectory',
    'u_init');
  {$ifdef OSANDROID}
  // https://developer.android.com/guide/topics/resources/internationalization
  ICU_VER: array[1..15] of string = (
    '_3_8', '_4_2', '_44', '_46', '_48', '_50', '_51', '_53', '_55',
    '_56', '_58', '_60', '_63', '_66', '_68');
  ICU_MAX = 80;
  ICU_MIN = 69; // previous versions are known and listed within ICU_VER[]
  SYSDATA: PAnsiChar = '/system/usr/icu';
  {$else}
  ICU_MAX = 80;
  ICU_MIN = 44;
  SYSDATA: PAnsiChar = '';
  {$endif OSANDROID}
var
  i: integer;
  err: SizeInt;
  P: PPointer;
  {$ifndef OSDARWIN}
  so: string;
  {$endif OSDARWIN}
  v, vers: string;
  data: PAnsiChar;
begin
  GlobalLock;
  try
    if Loaded then
      exit;
    Loaded := true;
    if LibName <> '' then
    begin
      icu := dlopen(pointer(LibName), RTLD_LAZY);
      if icu = nil then
        exit;
    end
    else
    begin
      {$ifdef OSDARWIN}
      // Mach OS has its own ICU set of libraries
      icu := dlopen('libicuuc.dylib', RTLD_LAZY);
      if icu <> nil then
        icui18n := dlopen('libicui18n.dylib', RTLD_LAZY);
      {$else}
      // libicudata should be loaded first because other two depend on it
      icudata := dlopen('libicudata.so', RTLD_LAZY);
      {$ifndef OSANDROID}
      if icudata = nil then
      begin
        // there is no link to the library -> try e.g. 'libicudata.so.67'
        if Version <> '' then
          icudata := dlopen(pointer('libicudata.so.' + Version), RTLD_LAZY);
        if icudata = nil then
          for i := ICU_MAX downto ICU_MIN do
          begin
            str(i, v);
            icudata := dlopen(pointer('libicudata.so.' + v), RTLD_LAZY);
            if icudata <> nil then
            begin
              Version := v;
              break;
            end;
          end;
        if icudata <> nil then
          so := '.' + Version;
      end;
      {$endif OSANDROID}
      if icudata <> nil then
      begin
        icu := dlopen(pointer('libicuuc.so' + so), RTLD_LAZY);
        if icu <> nil then
          icui18n := dlopen(pointer('libicui18n.so' + so), RTLD_LAZY);
      end;
      {$endif OSDARWIN}
      if icui18n = nil then
      begin
        // we did not find any ICU installed -> ensure iconv/RTL fallback is ok
        if not IsWideStringManagerProperlyInstalled then
          DisplayFatalError('ICU ' + CPU_ARCH_TEXT + ' is not available',
            'Either install it or put cwstring in your uses clause as fallback');
        Done;
        exit;
      end
    end;
    // ICU append a version prefix to all its functions e.g. ucnv_open_66
    if (Version <> '') and
       (dlsym(icu, pointer('ucnv_open_' + Version)) <> nil) then
      vers := '_' + Version // matched the explicit version
    else
    begin
      {$ifdef OSANDROID}
      for i := high(ICU_VER) downto 1 do
      begin
        if dlsym(icu, pointer(NAMES[0] + ICU_VER[i])) <> nil then
        begin
          vers := ICU_VER[i];
          break;
        end;
      end;
      if vers <> '' then
      {$endif OSANDROID}
      if dlsym(icu, 'ucnv_open') = nil then
        for i := ICU_MAX downto ICU_MIN do
        begin
          str(i, v);
          if dlsym(icu, pointer('ucnv_open_' + v)) <> nil then
          begin
            vers := '_' + v;
            break;
          end;
        end;
    end;
    P := @@ucnv_open;
    for i := 0 to high(NAMES) do
    begin
      P[i] := dlsym(icu, pointer(NAMES[i] + vers));
      if P[i] = nil then
      begin
        @ucnv_open := nil;
        exit;
      end;
    end;
    data := u_getDataDirectory;
    if (data = nil) or
       (data^ = #0) then
      if SYSDATA <> '' then
        u_setDataDirectory(SYSDATA);
    err := 0;
    u_init(err);
  finally
    GlobalUnLock;
  end;
end;

function TIcuLibrary.ForceLoad(const LibName: TFileName; const Version: string): boolean;
begin
  Done;
  Loaded := false;
  DoLoad(LibName, Version);
  result := Assigned(ucnv_open);
end;

function TIcuLibrary.ucnv(codepage: cardinal): pointer;
var
  s: ShortString;
  err: SizeInt;
  {$ifdef CPUINTEL}
  mask: cardinal;
  {$endif CPUINTEL}
begin
  if not IsAvailable then
    exit(nil);
  str(codepage, s);
  MoveFast(s[1], s[3], ord(s[0]));
  PWord(@s[1])^ := ord('c') + ord('p') shl 8;
  inc(s[0], 3);
  s[ord(s[0])] := #0;
  {$ifdef CPUINTEL}
  mask := GetMXCSR;
  SetMXCSR(mask or $0080 {MM_MaskInvalidOp} or $1000 {MM_MaskPrecision});
  {$endif CPUINTEL}
  err := 0;
  result := ucnv_open(@s[1], err);
  if result <> nil then
  begin
    err := 0;
    ucnv_setSubstChars(result, '?', 1, err);
    ucnv_setFallback(result, true);
  end;
  {$ifdef CPUINTEL}
  SetMXCSR(mask);
  {$endif CPUINTEL}
end;


const
  // for CompareStringW()
  LOCALE_USER_DEFAULT = $400;
  NORM_IGNORECASE = 1 shl ord(coIgnoreCase); // [widestringmanager.coIgnoreCase]

function CompareStringRTL(A, B: PWideChar; AL, BL, flags: integer): integer;
var
  U1, U2: UnicodeString; // allocate two temporary strings
begin
  // cwstring as fallback, using iconv on systems where ICU is not available
  SetString(U1, A, AL);
  SetString(U2, B, BL);
  result := widestringmanager.CompareUnicodeStringProc(
    U1, U2, TCompareOptions(flags));
end;

function CompareStringW(locale, flags: DWORD;
  A: PWideChar; AL: integer; B: PWideChar; BL: integer): PtrInt;
const
  CODE_POINT_ORDER = $8000;
var
  err: SizeInt;
begin
  if AL < 0 then
    AL := StrLenW(A);
  if BL < 0 then
    BL := StrLenW(B);
  err := 0;
  if icu.IsAvailable then
    if flags and NORM_IGNORECASE <> 0 then
      result := icu.u_strCaseCompare(A, AL, B, BL, CODE_POINT_ORDER, err)
    else
      result := icu.u_strCompare(A, AL, B, BL, true)
  else
    result := CompareStringRTL(A, B, AL, BL, flags);
  inc(result, 2); // caller would make -2 to get regular -1/0/1 comparison values
end;

function AnsiToWideRTL(CodePage: cardinal; A: PAnsiChar; W: PWideChar;
  AL, WL: PtrInt): PtrInt;
var
  tmp: UnicodeString;
begin
  // cwstring as fallback, using iconv on systems where ICU is not available
  widestringmanager.Ansi2UnicodeMoveProc(A, CodePage, tmp, AL);
  result := length(tmp);
  if result > WL then
    result := WL;
  MoveFast(pointer(tmp)^, W^, result * 2);
end;

function Unicode_AnsiToWide(A: PAnsiChar; W: PWideChar;
  LA, LW, CodePage: PtrInt): integer;
var
  cnv: pointer;
  err: SizeInt;
begin
  if CodePage = CP_UTF8 then
    exit(Utf8ToUnicode(W, A, LA));
  cnv := icu.ucnv(CodePage);
  if cnv = nil then
    exit(AnsiToWideRTL(CodePage, A, W, LA, LW)); // fallback to cwstring/iconv
  err := 0;
  result := icu.ucnv_toUChars(cnv, W, LW, A, LA, err);
  if result < 0 then
    result := 0;
  icu.ucnv_close(cnv);
end;

function WideToAnsiRTL(CodePage: cardinal; W: PWideChar; A: PAnsiChar;
  WL, AL: PtrInt): PtrInt;
var
  tmp: RawByteString;
begin
  // cwstring as fallback, using iconv on systems where ICU is not available
  widestringmanager.Unicode2AnsiMoveProc(W, tmp, CodePage, WL);
  result := length(tmp);
  if result > AL then
    result := AL;
  MoveFast(pointer(tmp)^, A^, result);
end;

function Unicode_WideToAnsi(W: PWideChar; A: PAnsiChar;
  LW, LA, CodePage: PtrInt): integer;
var
  cnv: pointer;
  err: SizeInt;
begin
  if CodePage = CP_UTF8 then
    exit(UnicodeToUtf8(A, W, LW));
  cnv := icu.ucnv(CodePage);
  if cnv = nil then
    exit(WideToAnsiRTL(CodePage, W, A, LW, LA)); // fallback to cwstring/iconv
  err := 0;
  result := icu.ucnv_fromUChars(cnv, A, LA, W, LW, err);
  if result < 0 then
    result := 0;
  icu.ucnv_close(cnv);
end;

function Unicode_InPlaceUpper(W: PWideChar; WLen: integer): integer;
var
  err, i: SizeInt;
begin
  if icu.IsAvailable then
  begin
    // call the accurate ICU library
    err := 0;
    result := icu.u_strToUpper(W, WLen, W, WLen, nil, err);
  end
  else
  begin
    // simple fallback code only handling 'a'..'z' -> 'A'..'Z' basic conversion
    for i := 0 to WLen - 1 do
      if ord(W[i]) in [ord('a')..ord('z')] then
        dec(W[i], 32);
    result := WLen;
  end;
end;

function Unicode_InPlaceLower(W: PWideChar; WLen: integer): integer;
var
  err, i: SizeInt;
begin
  if icu.IsAvailable then
  begin
    // call the accurate ICU library
    err := 0;
    result := icu.u_strToLower(W, WLen, W, WLen, nil, err);
  end
  else
  begin
    // simple fallback code only handling 'A'..'Z' -> 'a'..'z' basic conversion
    for i := 0 to WLen - 1 do
      if ord(W[i]) in [ord('A')..ord('Z')] then
        inc(W[i], 32);
    result := WLen;
  end;
end;

function GetDesktopWindow: PtrInt;
begin
  result := 0; // fixed result on a window-abstracted system
end;


{$ifdef NODIRECTTHREADMANAGER} // try to stabilize MacOS/BSD pthreads API calls

function GetCurrentThreadId: TThreadID;
begin
  result := system.GetCurrentThreadID();
end;

function TryEnterCriticalSection(var cs: TRTLCriticalSection): integer;
begin
  result := system.TryEnterCriticalSection(cs);
end;

procedure EnterCriticalSection(var cs: TRTLCriticalSection);
begin
  system.EnterCriticalSection(cs);
end;

procedure LeaveCriticalSection(var cs: TRTLCriticalSection);
begin
  system.LeaveCriticalSection(cs);
end;

{$endif NODIRECTTHREADMANAGER}

const
  HoursPerDay = 24;
  MinsPerHour = 60;
  SecsPerMin  = 60;
  MinsPerDay  = HoursPerDay * MinsPerHour;
  SecsPerDay  = MinsPerDay  * SecsPerMin;
  SecsPerHour = MinsPerHour * SecsPerMin;
  MilliSecsPerSec = 1000;
  MicroSecsPerSec = 1000000;
  MicroSecsPerMilliSec = 1000;
  NanoSecsPerMicroSec  = 1000;
  NanoSecsPerMilliSec  = 1000000;
  NanoSecsPerSec       = 1000000000;

const
  // Date Translation - see http://en.wikipedia.org/wiki/Julian_day
  D0 = 1461;
  D1 = 146097;
  D2 = 1721119;
  C1970 = 2440588;

procedure JulianToGregorian(JulianDN: PtrUInt; out result: TSystemTime);
  {$ifdef HASINLINE}inline;{$endif}
var
  YYear, XYear, Temp, TempMonth: PtrUInt;
begin
  Temp := ((JulianDN - D2) * 4) - 1;
  JulianDN := Temp div D1;
  XYear := (Temp - (JulianDN * D1)) or 3;
  YYear := XYear div D0;
  Temp := (((XYear - (YYear * D0) + 4) shr 2) * 5) - 3;
  TempMonth := Temp div 153;
  result.Day := ((Temp - (TempMonth * 153)) + 5) div 5;
  if TempMonth >= 10 then
  begin
    inc(YYear);
    dec(TempMonth, 12 - 3);
  end
  else
    inc(TempMonth, 3);
  result.Month := TempMonth;
  result.Year := YYear + (JulianDN * 100);
  // initialize fake dayOfWeek - as used by FromGlobalTime()
  result.DayOfWeek := 0;
end;

procedure EpochToSystemTime(epoch: PtrUInt; out result: TSystemTime);
var
  t: PtrUInt;
begin
  t := epoch div SecsPerDay;
  JulianToGregorian(t + C1970, result);
  dec(epoch, t * SecsPerDay);
  t := epoch div SecsPerHour;
  result.Hour := t;
  dec(epoch, t * SecsPerHour);
  t := epoch div SecsPerMin;
  result.Minute := t;
  result.Second := epoch - t * SecsPerMin;
end;

{$ifdef OSDARWIN} // OSX has no clock_gettime() API

type
  TTimebaseInfoData = record
    Numer: cardinal;
    Denom: cardinal;
  end;

function mach_absolute_time: UInt64;
  cdecl external clib name 'mach_absolute_time';

function mach_continuous_time: UInt64;
  cdecl external clib name 'mach_continuous_time';

function mach_timebase_info(var TimebaseInfoData: TTimebaseInfoData): integer;
  cdecl external clib name 'mach_timebase_info';

var
  mach_timeinfo: TTimebaseInfoData;
  mach_timecoeff: double;
  mach_timenanosecond: boolean; // very likely to be TRUE on Intel CPUs

procedure machtimetonanosec(var Value: Int64); inline;
begin
  if not mach_timenanosecond then
    if mach_timeinfo.Denom = 1 then
      // integer resolution is enough
      Value := Value * mach_timeinfo.Numer
    else
      // use floating point to avoid potential overflow
      Value := round(Value * mach_timecoeff);
end;

procedure QueryPerformanceMicroSeconds(out Value: Int64);
begin
  Value := mach_absolute_time;
  machtimetonanosec(Value);
  Value := Value div NanoSecsPerMicroSec; // ns to us
end;

function GetTickCount64: Int64;
begin
  result := mach_absolute_time;
  machtimetonanosec(result);
  result := result div NanoSecsPerMilliSec; // ns to ms
end;

function GetUptimeSec: cardinal;
var
  v: Int64;
begin
  v := mach_continuous_time;
  machtimetonanosec(v);
  result := v div NanoSecsPerSec; // ns to s
end;

function UnixTimeUtc: TUnixTime;
var
  tz: timeval;
begin
  fpgettimeofday(@tz, nil); // from libc
  result := tz.tv_sec;
end;

function UnixMSTimeUtc: TUnixMSTime;
var
  tz: timeval;
begin
  fpgettimeofday(@tz, nil);
  result := (Int64(tz.tv_sec) * MilliSecsPerSec) +
            tz.tv_usec div MicroSecsPerMilliSec; // in milliseconds
end;

procedure GetSystemTime(out result: TSystemTime);
var
  tz: timeval;
begin
  fpgettimeofday(@tz, nil);
  EpochToSystemTime(tz.tv_sec, result);
  result.MilliSecond := tz.tv_usec div MicroSecsPerMilliSec;
end;

procedure GetLocalTime(out result: TSystemTime);
var
  tz: timeval;
begin
  fpgettimeofday(@tz, nil);
  // + unixutil.TZSeconds = UTC to local time conversion
  EpochToSystemTime(tz.tv_sec + TZSeconds, result);
  result.MilliSecond := tz.tv_usec div MicroSecsPerMilliSec;
end;

{$else}


{$ifdef OSBSD}

const
  {$ifdef OSFREEBSD}
  // see https://github.com/freebsd/freebsd/blob/master/sys/sys/time.h
  CLOCK_REALTIME  = 0;
  CLOCK_MONOTONIC = 4;
  CLOCK_BOOTTIME  = 5;
  CLOCK_REALTIME_COARSE  = 10; // named CLOCK_REALTIME_FAST in FreeBSD 8.1+
  CLOCK_MONOTONIC_COARSE = 12;
  {$else}
  // see https://github.com/openbsd/src/blob/master/sys/sys/_time.h#L63
  CLOCK_REALTIME  = 0;
  CLOCK_MONOTONIC = 3;
  CLOCK_BOOTTIME  = 6;
  CLOCK_REALTIME_COARSE  = CLOCK_REALTIME; // no FAST/COARSE version
  CLOCK_MONOTONIC_COARSE = CLOCK_MONOTONIC;
  {$endif OSFREEBSD}

function clock_gettime(clk_id: cardinal; tp: ptimespec): integer;
  cdecl external clib name 'clock_gettime';

function clock_getres(clk_id: cardinal; tp: ptimespec): integer;
  cdecl external clib name 'clock_getres';

{$else}

const
  CLOCK_REALTIME         = 0;
  CLOCK_MONOTONIC        = 1;
  CLOCK_REALTIME_COARSE  = 5; // see http://lwn.net/Articles/347811
  CLOCK_MONOTONIC_COARSE = 6;
  CLOCK_BOOTTIME         = 7; // includes asleep time (2.6.39+)

// libc's clock_gettime function uses vDSO (avoid syscall) while FPC by default
// is compiled without FPC_USE_LIBC defined and do a syscall each time
//   GetTickCount64 fpc    2 494 563 op/sec
//   GetTickCount64 libc 119 919 893 op/sec
function clock_gettime(clk_id: clockid_t; tp: ptimespec): cint;
  cdecl external clib name 'clock_gettime'; // LIBC_SUFFIX fails on CentOS 7

function gettimeofday(tp: ptimeval; tzp: ptimezone): cint;
  cdecl external clib name 'gettimeofday' + LIBC_SUFFIX;

{$endif OSBSD}

var
  // contains CLOCK_REALTIME_COARSE since kernel 2.6.32
  CLOCK_REALTIME_FAST: integer = CLOCK_REALTIME;

  // contains CLOCK_MONOTONIC_COARSE since kernel 2.6.32
  CLOCK_MONOTONIC_FAST: integer = CLOCK_MONOTONIC;

  // contains CLOCK_MONOTONIC_RAW since kernel 2.6.28
  // - so that QueryPerformanceMicroSeconds() is not subject to NTP adjustments
  CLOCK_MONOTONIC_HIRES: integer = CLOCK_MONOTONIC;

  // contains CLOCK_BOOTTIME since kernel 2.6.39
  CLOCK_UPTIME: integer = CLOCK_MONOTONIC;

function UnixMSTimeUtc: TUnixMSTime;
var
  r: timespec;
begin
  clock_gettime(CLOCK_REALTIME_FAST, @r); // likely = CLOCK_REALTIME_COARSE
  // convert from nanoseconds into milliseconds
  result := QWord(PtrUInt(r.tv_nsec) div PtrUInt(NanoSecsPerMilliSec)) +
            QWord(r.tv_sec) * MilliSecsPerSec;
end;

function UnixTimeUtc: TUnixTime;
var
  r: timespec;
begin
  clock_gettime(CLOCK_REALTIME_FAST, @r);
  result := r.tv_sec;
end;

procedure QueryPerformanceMicroSeconds(out Value: Int64);
var
  r : TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC_HIRES, @r);
  // convert from nanoseconds into microseconds
  Value := QWord(PtrUInt(r.tv_nsec) div PtrUInt(NanoSecsPerMicroSec)) +
           QWord(r.tv_sec) * MicroSecsPerSec;
end;

procedure GetSystemTime(out result: TSystemTime);
var
  r: timespec;
begin
  // faster than fpgettimeofday() which makes a syscall and don't use vDSO
  clock_gettime(CLOCK_REALTIME_FAST, @r);
  EpochToSystemTime(r.tv_sec, result);
  result.MilliSecond := PtrUInt(r.tv_nsec) div PtrUInt(NanoSecsPerMilliSec);
end;

// c_timezone: longint external 'c' name 'timezone'; is broken and returns 0

procedure GetLocalTime(out result: TSystemTime);
var
  r: timespec;
begin
  // faster than fpgettimeofday() which makes a syscall and don't use vDSO
  clock_gettime(CLOCK_REALTIME_FAST, @r);
  // + unixutil.TZSeconds = UTC to local time conversion
  EpochToSystemTime(r.tv_sec + TZSeconds, result);
  result.MilliSecond := PtrUInt(r.tv_nsec) div PtrUInt(NanoSecsPerMilliSec);
end;

function GetTickCount64: Int64;
var
  tp: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC_FAST, @tp); // likely = CLOCK_MONOTONIC_COARSE
  // convert from nanoseconds into milliseconds
  result := QWord(PtrUInt(tp.tv_nsec) div PtrUInt(NanoSecsPerMilliSec)) +
            QWord(tp.tv_sec) * MilliSecsPerSec;
end;

function GetUptimeSec: cardinal;
var
  tp: timespec;
begin
  tp.tv_sec := 0;
  clock_gettime(CLOCK_UPTIME, @tp);
  // convert from nanoseconds into milliseconds
  result := tp.tv_sec;
end;

{$endif OSDARWIN}

function SetSystemTime(utctime: TUnixTime): boolean;
var
  u: timeval;
begin
  u.tv_sec := utctime;
  u.tv_usec := 0;
  result := fpsettimeofday(@u, nil) = 0;
end;

function UnixMSTimeUtcFast: TUnixMSTime;
begin
  result := UnixMSTimeUtc;
end;

{$undef OSPTHREADS}
{$undef HAS_PTHREADSETNAMENP}
{$undef HAS_PTHREADSETAFFINITY}

{$ifdef OSPTHREADSLIB}
  {$define OSPTHREADS}
var
  {$ifdef OSLINUX}
  // pthread_setname_np for Linux https://stackoverflow.com/a/7989973/458259
  {$define HAS_PTHREADSETNAMENP}
  pthread_setname_np: function(thread: pointer; name: PAnsiChar): integer; cdecl;
  // pthread_setaffinity_np has been tested on Linux only
  {$define HAS_PTHREADSETAFFINITY}
  pthread_setaffinity_np: function(thread: pointer;
    cpusetsize: SizeUInt; cpuset: pointer): integer; cdecl;
  {$endif OSLINUX}
  pthread_cancel: function(thread: pointer): integer; cdecl;
  pthread_mutex_init: function(mutex, attr: pointer): integer; cdecl;
  pthread_mutex_destroy: function(mutex: pointer): integer; cdecl;
{$endif OSPTHREADSLIB}

{$ifdef OSPTHREADSSTATIC}
  // note: pthread_setname_np() has no consistent API across POSIX systems
  {$define OSPTHREADS}

{$ifdef OSDARWIN}
// we specify link to clib='c' as in rtl/darwin/pthread.inc
function pthread_cancel(thread: pointer): integer;
  cdecl; external clib name 'pthread_cancel';
function pthread_mutex_init(mutex, attr: pointer): integer;
  cdecl; external clib name 'pthread_mutex_init';
function pthread_mutex_destroy(mutex: pointer): integer;
  cdecl; external clib name 'pthread_mutex_destroy';
function pthread_mutex_lock(mutex: pointer): integer;
  cdecl; external clib name 'pthread_mutex_lock';
function pthread_mutex_trylock(mutex: pointer): integer;
  cdecl; external clib name 'pthread_mutex_trylock';
function pthread_mutex_unlock(mutex: pointer): integer;
  cdecl; external clib name 'pthread_mutex_unlock';
{$else}
// just "external" without clib='c' as in rtl/*bsd/pthread.inc
function pthread_cancel(thread: pointer): integer;           cdecl; external;
function pthread_mutex_init(mutex, attr: pointer): integer;  cdecl; external;
function pthread_mutex_destroy(mutex: pointer): integer;     cdecl; external;
function pthread_mutex_lock(mutex: pointer): integer;        cdecl; external;
function pthread_mutex_trylock(mutex: pointer): integer;     cdecl; external;
function pthread_mutex_unlock(mutex: pointer): integer;      cdecl; external;
{$endif OSDARWIN}
{$endif OSPTHREADSSTATIC}


function IsInitializedCriticalSection(var cs: TRTLCriticalSection): boolean;
begin
  {$ifdef OSLINUX}
  result := cs.__m_kind <> 0;
  {$else}
  result := not IsZero(@cs, SizeOf(cs));
  {$endif OSLINUX}
end;

{$ifdef OSPTHREADS}

{ TOSLightLock }

procedure TOSLightLock.Init;
begin
  FillCharFast(self, SizeOf(self), 0); // may be bigger than pthread struct
  {$ifdef OSPTHREADSLIB}
  if not Assigned(pthread_mutex_init) then
    EOSException.Create('TOSLightLock.Init: no pthread_mutex_init')
  else // no recursive attribute -> fast mutex
  {$endif OSPTHREADSLIB}
    pthread_mutex_init(@fMutex, nil);
end;

procedure TOSLightLock.Done;
begin
  if IsInitializedCriticalSection(fMutex) then
    pthread_mutex_destroy(@fMutex);
end;

procedure TOSLightLock.Lock;
begin
  pthread_mutex_lock(@fMutex);
end;

function TOSLightLock.TryLock: boolean;
begin
  result := pthread_mutex_trylock(@fMutex) = 0;
end;

procedure TOSLightLock.UnLock;
begin
  pthread_mutex_unlock(@fMutex);
end;

{$else} // fallback to plain recursive TRTLCriticalSection

{ TOSLightLock }

procedure TOSLightLock.Init;
begin
  InitCriticalSection(fMutex);
end;

procedure TOSLightLock.Done;
begin
  DeleteCriticalSectionIfNeeded(fMutex);
end;

procedure TOSLightLock.Lock;
begin
  EnterCriticalSection(fMutex);
end;

function TOSLightLock.TryLock: boolean;
begin
  result := TryEnterCriticalSection(fMutex) <> 0;
end;

procedure TOSLightLock.UnLock;
begin
  LeaveCriticalSection(fMutex);
end;

{$endif OSPTHREADS}


procedure SetUnixThreadName(ThreadID: TThreadID; const Name: RawByteString);
var
  // truncated to 16 non space chars (including #0)
  {%H-}trunc: array[0..15] of AnsiChar;
  i, L, c4: integer;
begin
  if Name = '' then
    exit;
  L := 0; // trim unrelevant spaces and prefixes when filling the 16 chars
  i := 1;
  if Name[1] = 'T' then
  begin
    c4 := PCardinal(Name)^ and $dfdfdfdf;
    if (c4 = ord('T') + ord('S') shl 8 + ord('Q') shl 16 + ord('L') shl 24) or
       (c4 = ord('T') + ord('O') shl 8 + ord('R') shl 16 + ord('M') shl 24) then
      i := 5
    else
      i := 2;
  end;
  while i <= length(Name) do
  begin
    if Name[i] > ' ' then
    begin
      trunc[L] := Name[i];
      inc(L);
      if L = high(trunc) then
        break;
    end;
    inc(i);
  end;
  if L = 0 then
    exit;
  trunc[L] := #0;
  {$ifdef HAS_PTHREADSETNAMENP} // see https://stackoverflow.com/a/7989973
  {$ifdef OSPTHREADSLIB}
  if Assigned(pthread_setname_np) then
    try
      pthread_setname_np(pointer(ThreadID), @trunc[0]);
    except
      // ignore any exception (pthread confusion with its static version?)
      @pthread_setname_np := nil; // don't continue that way
    end;
  {$endif OSPTHREADSLIB}
  {$endif HAS_PTHREADSETNAMENP}
end;

procedure RawSetThreadName(ThreadID: TThreadID; const Name: RawUtf8);
begin
  if ThreadID <> MainThreadID then // don't change the main process name
    SetUnixThreadName(ThreadID, Name); // call pthread_setname_np()
end;

function RawKillThread(Thread: TThread): boolean;
begin
  result := false;
  {$ifdef OSPTHREADSLIB}
  if Assigned(pthread_cancel) then
    try
      result := pthread_cancel(pointer(Thread.ThreadID)) = 0;
    except
      // ignore any exception (pthread confusion with its static version?)
      @pthread_cancel := nil; // don't continue that way
    end;
  {$endif OSPTHREADSLIB}
  {$ifdef OSPTHREADSSTATIC}
  result := pthread_cancel(pointer(Thread.ThreadID)) = 0;
  {$endif OSPTHREADSSTATIC}
end;

procedure ResetCpuSet(out CpuSet: TCpuSet);
begin
  FillCharFast(CpuSet, SizeOf(CpuSet), 0);
end;

function SetThreadMaskAffinity(Thread: TThread; const Mask: TCpuSet): boolean;
begin
  result := false;
  {$ifdef HAS_PTHREADSETAFFINITY}
  {$ifdef OSPTHREADSLIB}
  if (Thread <> nil) and
     Assigned(pthread_setaffinity_np) then
    try
      result := pthread_setaffinity_np(
        pointer(Thread.ThreadID), SizeOf(Mask), @Mask) = 0;
    except
      // ignore any exception (pthread confusion with its static version?)
      @pthread_setaffinity_np := nil; // don't continue that way
    end;
  {$endif OSPTHREADSLIB}
  {$endif HAS_PTHREADSETAFFINITY}
end;

{$ifdef HAS_PTHREADSETAFFINITY}
function sched_getaffinity(pid: integer;
  cpusetsize: SizeUInt; cpuset: pointer): integer;
    cdecl external clib name 'sched_getaffinity';
{$endif HAS_PTHREADSETAFFINITY}

function GetMaskAffinity(out CpuSet: TCpuSet): boolean;
begin
  {$ifdef HAS_PTHREADSETAFFINITY}
  result := sched_getaffinity(0, SizeOf(CpuSet), @CpuSet) = 0;
  {$else}
  result := false; // unsupported by now
  {$endif HAS_PTHREADSETAFFINITY}
end;


{$ifndef NOEXCEPTIONINTERCEPT}

function TSynLogExceptionContext.AdditionalInfo(
  out ExceptionNames: TPUtf8CharDynArray): cardinal;
begin
  result := 0; // Windows/CLR specific by now
end;

var
  _RawLogException: TOnRawLogException;

// FPC: intercept via the RaiseProc global variable
{$define WITH_RAISEPROC}
// RaiseProc redirection is implemented in main mormot.core.os.pas

{$endif NOEXCEPTIONINTERCEPT}

function GetFileNameFromUrl(const Uri: string): TFileName;
begin
  result := ''; // no such native API on POSIX
end;

const
  faInvalidFile   = faDirectory;
  faDirectoryMask = faDirectory;

function FileDateToDateTime(const FileDate: TFileAge): TDateTime;
begin
  if FileDate <= 0 then
    result := 0
  else
    // + unixutil.TZSeconds = UTC to local time conversion
    result := Int64(FileDate + TZSeconds) / Int64(SecsPerDay) + Int64(UnixDelta);
end;

function FileAgeToDateTime(const FileName: TFileName): TDateTime;
begin
  // faster to use POSIX time than RTL FileDateToDateTime(FileAge())
  result := FileDateToDateTime(FileAgeToUnixTimeUtc(FileName)); // UTC to local
end;

function FileAgeToUnixTimeUtc(const FileName: TFileName; AllowDir: boolean): TUnixTime;
var
  st: TStat;
begin
  result := 0;
  if (FileName <> '') and
     (fpStat(pointer(FileName), st) = 0) and
     (AllowDir or
      (not FpS_ISDIR(st.st_mode))) then
    result := st.st_mtime; // as TUnixTime seconds, with no local conversion
end;

function FileHandleToUnixTimeUtc(F: THandle): TUnixTime;
var
  st: TStat;
begin
  result := 0;
  if ValidHandle(F) and
     (FpFStat(F, st) = 0) then
    result := st.st_mtime;
end;

function FileSetDateFromUnixUtc(const Dest: TFileName; Time: TUnixTime): boolean;
var
  t: TUtimBuf;
begin
  result := false;
  if (Dest = '') or
     (Time = 0) then
    exit;
  t.actime := Time;
  t.modtime := Time;
  result := FpUtime(pointer(Dest), @t) = 0; // direct syscall
end;

function FileSetDateFrom(const Dest: TFileName; SourceHandle: THandle): boolean;
begin
  result := FileSetDateFromUnixUtc(Dest, FileHandleToUnixTimeUtc(SourceHandle));
end;

function FileSetDateFrom(const Dest, Source: TFileName): boolean;
begin
  result := FileSetDateFromUnixUtc(Dest, FileAgeToUnixTimeUtc(Source));
end;

function FileSetDateFromWindowsTime(const Dest: TFileName;
  WinTime: integer): boolean;
var
  dt: TDateTime;
begin
  dt := WindowsFileTimeToDateTime(WinTime);
  result := (Dest <> '') and
            (dt <> 0) and
            (FileSetDate(Dest, DateTimeToFileDate(dt)) = 0); // with LocalToEpoch()
end;

function SearchRecToWindowsTime(const F: TSearchRec): integer;
begin
  result := DateTimeToWindowsFileTime(FileDateToDateTime(F.Time));
end;

function SearchRecToUnixTimeUtc(const F: TSearchRec): TUnixTime;
begin
  result := F.Time; // raw POSIX FileDate is already in UTC seconds
end;

function FileAgeToWindowsTime(const FileName: TFileName): integer;
begin
  result := DateTimeToWindowsFileTime(FileAgeToDateTime(FileName));
end;

function FileIsWritable(const FileName: TFileName): boolean;
begin
  result := (FileName <> '') and
            (fpaccess(pointer(FileName), W_OK) = 0);
end;

procedure FileSetHidden(const FileName: TFileName; ReadOnly: boolean);
begin
  if FileName <> '' then
    if ReadOnly then
      fpchmod(pointer(FileName), S_IRUSR)
    else
      fpchmod(pointer(FileName), S_IRUSR or S_IWUSR);
end;

procedure FileSetSticky(const FileName: TFileName);
begin
  fpchmod(FileName, S_IRUSR or S_IWUSR or S_IRGRP or S_IROTH or S_ISVTX);
end;

function FileSize(const FileName: TFileName): Int64;
var
  st: TStat;
begin
  if (FileName = '') or
     (fpStat(pointer(FileName), st) <> 0) or
     FpS_ISDIR(st.st_mode) then
    result := 0
  else
    result := st.st_size;
end;

function FileExists(const FileName: TFileName): boolean;
var
  st: TStat;
begin
  result := (FileName <> '') and
            (fpStat(pointer(FileName), st) = 0) and
            not FpS_ISDIR(st.st_mode);
end;

function FileCreate(const aFileName: TFileName; aMode, aRights: integer): THandle;
begin
  if aFileName = '' then
    result := 0
  else if (aMode = 0) and
          (aRights = 0) then
    result := sysutils.FileCreate(aFileName) // direct call of the FPC RTL
  else
  begin
    if aRights = 0 then // use 644 / '-rw-r-r--' default POSIX file attributes
      aRights := S_IRUSR or S_IWUSR or S_IRGRP or S_IROTH;
    result := sysutils.FileCreate(aFileName, aMode, aRights);
  end;
end;

procedure StatTimeMS(const st: TStat; out time: TUnixMSTime); inline;
begin
  time := QWord(st.st_mtime) * MilliSecsPerSec + // no local conversion needed
          // include milliseconds information
          {$ifdef OSLINUXANDROID}
          st.st_mtime_nsec div NanoSecsPerMilliSec;
          {$else}
          st.st_mtimensec div NanoSecsPerMilliSec;
          {$endif OSLINUXANDROID}
end;

function FileInfoByName(const FileName: TFileName; out FileSize: Int64;
  out FileTimestampUtc: TUnixMSTime): boolean;
var
  st: TStat;
begin
  result := fpStat(pointer(FileName), st) = 0;
  if not result then
    exit;
  FileSize := st.st_size;
  StatTimeMS(st, FileTimestampUtc);
end;

function FileSize(F: THandle): Int64;
var
  st: TStat;
begin
  if fpFstat(F, st) <> 0 then
    result := 0
  else
    result := st.st_size;
end;

function FileSeek64(Handle: THandle; const Offset: Int64; Origin: cardinal): Int64;
begin
  result := FPLSeek(Handle, Offset, Origin);
end;

function FileInfoByHandle(aFileHandle: THandle; FileId, FileSize: PInt64;
  LastWriteAccess, FileCreateDateTime: PUnixMSTime): boolean;
var
  mtime, atime, ctime: Int64;
  lp: TStat;
  r: integer;
begin
  r := FpFStat(aFileHandle, lp);
  result := r >= 0;
  if not result then
    exit;
  if FileId <> nil then
    FileId^ := lp.st_ino;
  if FileSize <> nil then
    FileSize^ := lp.st_size;
  if (LastWriteAccess = nil) and
     (FileCreateDateTime = nil) then
    exit;
  StatTimeMS(lp, mtime);
  if LastWriteAccess <> nil then
    LastWriteAccess^ := mtime;
  if FileCreateDateTime = nil then
    exit;
  // some FS don't populate all fields, so we use what we actually got
  {$ifdef OSOPENBSD}
  if (lp.st_birthtime <> 0) and
     (lp.st_birthtime < lp.st_ctime) then
    lp.st_ctime := lp.st_birthtime;
  {$endif OSOPENBSD}
  // ignore nanoseconds/Milliseconds for FileCreateDateTime
  ctime := Int64(lp.st_ctime) * MilliSecsPerSec;
  atime := Int64(lp.st_atime) * MilliSecsPerSec;
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
  st: TStat;
begin
  result := (fpStat(pointer(FileName), st) = 0) and
            (st.st_mode and (S_IXUSR or S_IXGRP or S_IXOTH) <> 0) and
            not FpS_ISDIR(st.st_mode);
end;

function GetExecutableName(aAddress: pointer): TFileName;
var
  dlinfo: dl_info;
begin
  FillCharFast(dlinfo, sizeof(dlinfo), 0);
  dladdr(aAddress, @dlinfo);
  result := ExpandFileName(string(dlinfo.dli_fname));
end;

function CopyFile(const Source, Target: TFileName;
  FailIfExists: boolean): boolean;
var
  src, dst: THandleStream;
begin
  result := false;
  if FileExists(Target) then
    if FailIfExists then
      exit
    else
      DeleteFile(Target);
  try
    src := TFileStreamEx.Create(Source, fmOpenReadShared);
    try
      dst := TFileStreamEx.Create(Target, fmCreate);
      try
        StreamCopyUntilEnd(src, dst); // faster than dst.CopyFrom()
      finally
        dst.Free;
      end;
      FileSetDateFrom(Target, src.Handle);
    finally
      src.Free;
    end;
    result := true;
  except
    result := false;
  end;
end;

function ValidHandle(Handle: THandle): boolean;
begin
  result := PtrInt(Handle) >= 0; // 0=StdIn is a valid POSIX file descriptor
end;

function WaitReadPending(fd, timeout: integer): boolean;
var
  p: TPollFD; // select() limits process to 1024 sockets in POSIX -> use poll()
  // https://moythreads.com/wordpress/2009/12/22/select-system-call-limitation
begin
  p.fd := fd;
  p.events := POLLIN or POLLPRI;
  p.revents := 0;
  result := FpPoll(@p, 1, timeout) > 0;
end;

{$I-}
procedure DisplayFatalError(const title, msg: RawUtf8);
var
  err: ^Text;
begin
  err := @StdErr;
  if title <> '' then
    writeln(err^, Executable.ProgramName, ': ', title);
  writeln(err^, Executable.ProgramName, ': ', msg);
  ioresult;
end;
{$I+}

function FileOpenSequentialRead(const FileName: TFileName): integer;
begin
  // SysUtils.FileOpen = fpOpen + fpFlock
  result := fpOpen(pointer(FileName), O_RDONLY); // no fpFlock() call
end;

function FileIsReadable(const aFileName: TFileName): boolean;
var
  fd: integer;
begin
  fd := fpOpen(pointer(aFileName), O_RDONLY); // no fpFlock() call
  result := ValidHandle(fd);
  if result then
    FpClose(fd);
end;

procedure SetEndOfFile(F: THandle);
begin
  FpFtruncate(F, FPLseek(F, 0, SEEK_CUR));
end;

procedure FlushFileBuffers(F: THandle);
begin
  FpFsync(F);
end;

function GetLastError: integer;
begin
  result := fpgeterrno;
end;

function IsSharedViolation(ErrorCode: integer): boolean;
begin
  if ErrorCode = 0 then
    ErrorCode := fpgeterrno;
  result := ErrorCode = ESysEACCES;
end;

procedure SetLastError(error: integer);
begin
  fpseterrno(error);
end;

function GetErrorText(error: integer): RawUtf8;
begin
  result := StrError(error); // from FPC RTL: using a OS-specific array of const
end;

function TMemoryMap.DoMap(aCustomOffset: Int64): boolean;
begin
  if aCustomOffset <> 0 then
    if (aCustomOffset and (SystemInfo.dwPageSize - 1)) <> 0 then
      raise EOSException.CreateFmt(
        'DoMap(aCustomOffset=%d) incompatible with dwPageSize=%d',
        [aCustomOffset, SystemInfo.dwPageSize]);
  fBuf := fpmmap(nil, fBufSize, PROT_READ, MAP_SHARED, fFile, aCustomOffset);
  if fBuf = MAP_FAILED then
  begin
    fBuf := nil;
    result := false;
  end
  else
    result := true;
end;

procedure TMemoryMap.DoUnMap;
begin
  if (fBuf <> nil) and
     (fBufSize > 0) and
     (fFile <> 0) then
    fpmunmap(fBuf, fBufSize);
end;

procedure SleepHiRes(ms: cardinal);
var
  timeout: TTimespec;
  s: cardinal;
begin
  if ms = 0 then
    // handle SleepHiRes(0) special case
    if SleepHiRes0Yield then
    begin
      // warning: reported as buggy by Alan on POSIX, and despitable by Linus
      // - from our testing, it gives worse performance than fpnanosleep()
      ThreadSwitch; // call e.g. POSIX libc's sched_yield API
      exit;
    end
    else
    begin
      timeout.tv_sec := 0;
      timeout.tv_nsec := 10000; // 10us is around timer resolution on modern HW
    end
  else
  begin
    s := ms div MilliSecsPerSec;
    timeout.tv_sec := s;
    timeout.tv_nsec := (ms - s * MilliSecsPerSec) * NanoSecsPerMilliSec;
  end;
  fpnanosleep(@timeout, nil)
  // no retry loop on ESysEINTR (as with regular RTL's Sleep)
end;

procedure SwitchToThread;
var
  timeout: Ttimespec;
begin
  // nanosleep() seems better than FPC RTL ThreadSwitch = POSIX libc sched_yield
  timeout.tv_sec := 0;
  timeout.tv_nsec := 10; // empirically identified on a recent Linux Kernel
  // note: nanosleep() adds a few dozen of microsecs for context switching
  fpnanosleep(@timeout, nil);
end;


{$undef HASEVENTFD}
{$ifdef OSLINUX}
  {$ifdef CPUX64}
    {$define HASEVENTFD}
  {$endif CPUX64}
  {$ifdef CPUX86}
    {.$define HASEVENTFD} // untested
  {$endif CPUX86}
  {$ifdef CPUAARCH64}
    {.$define HASEVENTFD} // untested
  {$endif CPUAARCH64}
{$endif OSLINUX}


{ TSynEvent }

constructor TSynEvent.Create;
begin
  {$ifdef HASEVENTFD}
  fFD := LinuxEventFD({nonblocking=}false, {semaphore=}false);
  if fFD = 0 then // fallback to PRTLEvent on oldest kernel
  {$endif HASEVENTFD}
    fHandle := RTLEventCreate;
end;

destructor TSynEvent.Destroy;
begin
  {$ifdef HASEVENTFD}
  if fFD <> 0 then
  begin
    LinuxEventFDWrite(fFD, 1); // release the lock or do nothing
    fpClose(fFD);
  end
  else
  {$endif HASEVENTFD}
    RTLEventDestroy(fHandle);
  inherited Destroy;
end;

procedure TSynEvent.ResetEvent;
begin
  {$ifdef HASEVENTFD}
  if fFD = 0 then // no need to reset the eventfd() handle
  {$endif HASEVENTFD}
    RTLEventResetEvent(fHandle);
end;

procedure TSynEvent.SetEvent;
begin
  {$ifdef HASEVENTFD}
  if fFD <> 0 then
    LinuxEventFDWrite(fFD, 1)
  else
  {$endif HASEVENTFD}
    RTLEventSetEvent(fHandle);
end;

procedure TSynEvent.WaitFor(TimeoutMS: integer);
begin
  {$ifdef HASEVENTFD}
  if fFD <> 0 then
  begin
    if WaitReadPending(fFD, TimeoutMS) then // = LinuxEventFDWait()
      LinuxEventFDRead(fFD);
  end
  else
  {$endif HASEVENTFD}
    RTLEventWaitFor(fHandle, TimeoutMS);
end;

procedure TSynEvent.WaitForEver;
begin
  {$ifdef HASEVENTFD}
  if fFD <> 0 then
    LinuxEventFDRead(fFD)
  else
  {$endif HASEVENTFD}
    RTLEventWaitFor(fHandle);
end;


procedure InitializeCriticalSection(var cs : TRTLCriticalSection);
begin
  InitCriticalSection(cs);
end;

procedure DeleteCriticalSection(var cs : TRTLCriticalSection);
begin
  DoneCriticalSection(cs);
end;

function GetFileOpenLimit(hard: boolean): integer;
var
  limit: TRLIMIT;
begin
  if fpgetrlimit(RLIMIT_NOFILE, @limit) = 0 then
    if hard then
      result := limit.rlim_max
    else
      result := limit.rlim_cur
  else
    result := -1;
end;

function SetFileOpenLimit(max: integer; hard: boolean): integer;
var
  limit: TRLIMIT;
begin
  result := -1;
  if fpgetrlimit(RLIMIT_NOFILE, @limit) <> 0 then
    exit;
  if (hard and
      (integer(limit.rlim_max) = max)) or
     (not hard and
      (integer(limit.rlim_cur) = max)) then
    exit(max); // already to the expected value
  if hard then
    limit.rlim_max := max
  else
    limit.rlim_cur := max;
  if fpsetrlimit(RLIMIT_NOFILE, @limit) = 0 then
    result := GetFileOpenLimit(hard);
end;


{$ifdef OSLINUX} { the systemd API is Linux-specific }

{ TSystemD }

procedure TSystemD.DoLoad;
var
  p: PPointer;
  i, j: PtrInt;
const
  NAMES: array[0..5] of PAnsiChar = (
    'sd_listen_fds',
    'sd_is_socket_unix',
    'sd_journal_print',
    'sd_journal_sendv',
    'sd_notify',
    'sd_watchdog_enabled');
begin
  GlobalLock;
  if not tested then
  begin
    systemd := dlopen(LIBSYSTEMD_PATH, RTLD_LAZY);
    if systemd <> nil then
    begin
      p := @@listen_fds;
      for i := 0 to high(NAMES) do
      begin
        p^ := dlsym(systemd, NAMES[i]);
        if p^ = nil then
        begin
          p := @@listen_fds;
          for j := 0 to i do
          begin
            p^ := nil;
            inc(p);
          end;
          break;
        end;
        inc(p);
      end;
    end;
    tested := true;
  end;
  GlobalUnLock;
end;

function TSystemD.IsAvailable: boolean;
begin
  if not tested then
    DoLoad;
  result := Assigned(listen_fds);
end;

function TSystemD.ProcessIsStartedBySystemd: boolean;
begin
  result := IsAvailable and
    // note: for example on Ubuntu 20.04 INVOCATION_ID is always defined
    // from the other side PPID 1 can be in case we run under docker of started
    // by init.d so let's verify both
    (fpgetppid() = 1) and
    (fpGetenv(ENV_INVOCATION_ID) <> nil);
end;

procedure TSystemD.Done;
begin
  if systemd <> nil then
  begin
    dlclose(systemd);
    systemd := nil;
  end;
end;

{$ifdef HASEVENTFD}

const
  EFD_SEMAPHORE = $00000001;
  EFD_NONBLOCK  = O_NONBLOCK;
  EFD_CLOEXEC   = O_CLOEXEC;

  // exists since Kernel 2.6.27
  {$ifdef CPUX64}
  syscall_nr_eventfd2 = 290;
  {$endif CPUX64}
  {$ifdef CPUX86}
  syscall_nr_eventfd2 = 328;
  {$endif CPUX86}
  {$ifdef CPUAARCH64}
  syscall_nr_eventfd2 = 356;
  {$endif CPUAARCH64}

function eventfd(initval, flags: cardinal): integer; inline;
begin
  result := do_syscall(syscall_nr_eventfd2, TSysParam(initval), TSysParam(flags));
end;

function LinuxEventFD(nonblocking, semaphore: boolean): integer;
var
  flags: cardinal;
begin
  result := 0;
  if KernelRevision < $02061b then
    exit; // not available prior to kernel 2.6.27
  flags := 0;
  if nonblocking then
    flags := EFD_NONBLOCK;
  if semaphore then
    flags := flags or EFD_SEMAPHORE;
  result := eventfd(0, flags);
  if not ValidHandle(result) then
    result := 0;
end;

{$else}

function LinuxEventFD(nonblocking, semaphore: boolean): integer;
begin
  result := 0; // non implemented (not tested, infact) on this CPU
end;

{$endif HASEVENTFD}

function LinuxEventFDRead(fd: integer): Int64;
begin
  { If EFD_SEMAPHORE was specified and the eventfd counter has a nonzero value,
    then a read returns 8 bytes containing the value 1, and the counter's value
    is decremented by 1 }
  result := 0;
  if do_syscall(syscall_nr_read, fd, TSysParam(@result), 8) <> 8 then
    result := -1;
end;

procedure LinuxEventFDWrite(fd: integer; count: QWord);
begin
  if count <> 0 then
    do_syscall(syscall_nr_write, fd, TSysParam(@count), SizeOf(count));
end;

function LinuxEventFDWait(fd: integer; ms: integer): boolean;
begin
  result := WaitReadPending(fd, ms);
end;

{$endif OSLINUX}


// we bypass crt.pp since this unit cancels the SIGINT signal

procedure AllocConsole;
begin
  StdOut := StdOutputHandle;
end;

var
  TextAttr: integer = 255; // always change the color at startup

procedure TextColorCmd(Color: TConsoleColor; var s: TShort8);
const
  TERM_CTRL: string[8] = '04261537';
begin
  s[0] := #0;
  if (ord(Color) = TextAttr) or
     not StdOutIsTTY then
    exit;
  TextAttr := ord(Color);
  s := #27'[0;3#m';
  if ord(Color) >= 8 then
    s[3] := '1';
  s[6] := TERM_CTRL[(ord(Color) and 7) + 1];
end;

procedure TextColorAppend(Color: TConsoleColor; var s: RawUtf8);
var
  c: TShort8;
begin
  TextColorCmd(Color, c);
  RawUtf8Append(s, @c[1], ord(c[0]));
end;

procedure TextColor(Color: TConsoleColor);
var
  c: TShort8;
begin
  TextColorCmd(Color, c);
  if c[0] <> #0 then
    fpwrite(StdOutputHandle, @c[1], ord(c[0])); // single syscall
end;

procedure TextBackground(Color: TConsoleColor);
begin
  // not implemented yet - but not much needed either
end;

var
  ConsoleCriticalSection: TOSLock;
const
  PosixLineFeed: AnsiChar = #10;

procedure ConsoleWrite(const Text: RawUtf8; Color: TConsoleColor;
  NoLineFeed, NoColor: boolean);
var
  s: RawUtf8;
begin
  // pre-compute the whole chars to be sent to the console
  if not NoColor then
    TextColorAppend(Color, s);
  RawUtf8Append(s, pointer(Text), length(Text));
  if not NoLineFeed then
    RawUtf8Append(s, @PosixLineFeed, 1);
  if not NoColor then
    TextColorAppend(ccLightGray, s);
  // display whole line in a single syscall
  ConsoleCriticalSection.Lock;
  FileWriteAll(StdOutputHandle, pointer(s), length(s)); // UTF-8 console
  ConsoleCriticalSection.UnLock;
end;

function UnixKeyPending: boolean;
begin
  result := WaitReadPending(StdInputHandle, 0);
end;

{$I-}
procedure ConsoleWaitForEnterKey;
var
  c: AnsiChar;
begin
  if GetCurrentThreadID = MainThreadID then
  begin
    SynDaemonIntercept; // intercept ^C and SIGQUIT - do nothing if already set
    repeat
      if IsMultiThread then
        CheckSynchronize(100)
      else
        Sleep(100);
      if SynDaemonTerminated <> 0 then
        break;
      if UnixKeyPending then
        repeat
          c := #0;
          if FpRead(StdInputHandle, c, 1) <> 1 then
            break;
          if c in [#10, #13] then
            exit;
        until false;
    until false;
  end
  else
    ReadLn;
  ioresult;
end;
{$I+}

function ConsoleStdInputLen: integer;
begin
  if fpioctl(StdInputHandle, FIONREAD, @result) < 0 then
    result := 0;
end;

function Utf8ToConsole(const S: RawUtf8): RawByteString;
begin
  result := S; // expect a UTF-8 console under Linux/BSD
end;


{$ifdef FPCUSEVERSIONINFO} // FPC 3.0+ if enabled in .inc / project options
function TFileVersion.RetrieveInformationFromFileName: boolean;
var
  VI: TVersionInfo;
  TI: integer;
begin
  result := false;
  if fFileName = '' then
    exit;
  VI := TVersionInfo.Create;
  try
    try
      // extract information - VI.Load() may raise EResNotFound
      if (fFileName <> '') and
         (fFileName <> ParamStr(0)) then
        VI.Load(fFileName)
      else
        VI.Load(HInstance); // load info for currently running program
      result := VI.FixedInfo.FileVersion[0] <> 0;
      // set extracted version numbers
      SetVersion(VI.FixedInfo.FileVersion[0],
                 VI.FixedInfo.FileVersion[1],
                 VI.FixedInfo.FileVersion[2],
                 VI.FixedInfo.FileVersion[3]);
      // detect translation
      if VI.VarFileInfo.Count > 0 then
        with VI.VarFileInfo.Items[0] do
          LanguageInfo := _fmt('%.4x%.4x', [language, codepage]);
      if LanguageInfo = '' then
      begin
        // take first language
        TI := 0;
        if VI.StringFileInfo.Count > 0 then
          LanguageInfo := VI.StringFileInfo.Items[0].Name
      end
      else
      begin
        // look for language index
        TI := VI.StringFileInfo.Count - 1;
        while (TI >= 0) and
              (CompareText(VI.StringFileInfo.Items[TI].Name, LanguageInfo) <> 0) do
          dec(TI);
        if TI < 0 then
        begin
          TI := 0; // revert to first translation
          LanguageInfo := VI.StringFileInfo.Items[TI].Name;
        end;
      end;
      with VI.StringFileInfo.Items[TI] do
      begin
        CompanyName      := Values['CompanyName'];
        FileDescription  := Values['FileDescription'];
        FileVersion      := Values['FileVersion'];
        InternalName     := Values['InternalName'];
        LegalCopyright   := Values['LegalCopyright'];
        OriginalFilename := Values['OriginalFilename'];
        ProductName      := Values['ProductName'];
        ProductVersion   := Values['ProductVersion'];
        Comments         := Values['Comments'];
      end;
    except
      // trap EResNotFound exception from VI.Load()
    end;
  finally
    VI.Free;
  end;
end;
{$else}
function TFileVersion.RetrieveInformationFromFileName: boolean;
begin
  result := false; // nothing to be done
end;
{$endif FPCUSEVERSIONINFO}

procedure GetUserHost(out User, Host: RawUtf8);
begin
  Host := RawUtf8(GetHostName);
  if Host = '' then
    Host := RawUtf8(GetEnvironmentVariable('HOSTNAME'));
  User := RawUtf8(GetEnvironmentVariable('LOGNAME')); // POSIX
  if User = '' then
    User := RawUtf8(GetEnvironmentVariable('USER'));
end;

function GetEnvFolder(const name: string; var folder: TFileName;
  writable: boolean): boolean;
begin
  folder := GetEnvironmentVariable(name);
  if folder <> '' then
    if writable and
       not IsDirectoryWritable(folder) then
      folder := ''
    else
      folder := IncludeTrailingPathDelimiter(folder);
  result := folder <> '';
end;

function WritableFolder(const parent, sub: TFileName; var folder: TFileName): boolean;
begin
  result := false;
  if not IsDirectoryWritable(parent) then
    exit;
  folder := EnsureDirectoryExists(parent + sub);
  if folder = '' then
    exit;
  if IsDirectoryWritable(folder) then
    result := true
  else
    folder := '';
end;

procedure _ComputeSystemPath(kind: TSystemPath; var result: TFileName);
begin
  result := ''; // "out result" param is not enough for FPC
  case kind of
    spLog:
         // try '/var/log/<exename>'
      if not WritableFolder('/var/log/', TFileName(Executable.ProgramName), result) and
         // try '<exepath>/log'
         not WritableFolder(Executable.ProgramFilePath, 'log', result) then
        // fallback to '$TMP/<exename>-log' - spUserData/$HOME is no option
        result := EnsureDirectoryExists(format('%s%s-log',
          [GetSystemPath(spTemp), Executable.ProgramName]));
    // warning: $HOME is reported wrong with sudo for spUserData/spUserDocuments
    spUserData:
         // try $XDG_CACHE_HOME
      if not GetEnvFolder('XDG_CACHE_HOME', result, {writable=}true) and
         // try '$HOME/.cache'
         not WritableFolder(GetSystemPath(spUserDocuments), '.cache', result) then
        // fallback to '$TMP/<user>'
        WritableFolder(GetSystemPath(spTemp), TFileName(Executable.User), result);
    spTemp:
      begin
        // try $TMPDIR (POSIX standard) and $TMP and $TEMP
        if GetEnvFolder('TMPDIR', result, {writable=}true) or
           GetEnvFolder('TMP',    result, {writable=}true) or
           GetEnvFolder('TEMP',   result, {writable=}true) then
          exit;
        // try /tmp
        result := '/tmp/';
        if not IsDirectoryWritable(result) then
          // fallback to /var/tmp
          result := '/var/tmp/';
      end;
  else
    // POSIX requires a value for the $HOME environment variable
    GetEnvFolder('HOME', result, {writable=}false);
  end;
end;

function _GetSystemStoreAsPem(CertStore: TSystemCertificateStore): RawUtf8;
var
  files: TRawUtf8DynArray;
  f: PtrInt;
begin
  // see https://go.dev/src/crypto/x509/root_unix.go as reference
  case CertStore of
    scsRoot:
      result := StringFromFirstFile([
        {$ifdef OSLINUXANDROID}
          '/etc/ssl/certs/ca-certificates.crt',                // Debian/Gentoo
      	  '/etc/pki/tls/certs/ca-bundle.crt',                  // Fedora/RHEL 6
          '/etc/ssl/ca-bundle.pem',                            // OpenSUSE
          '/etc/pki/tls/cacert.pem',                           // OpenELEC
          '/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem', // CentOS/RHEL 7
          '/etc/ssl/cert.pem'                                  // Alpine Linux
        {$else}
      	  '/usr/local/etc/ssl/cert.pem',            // FreeBSD
      	  '/etc/ssl/cert.pem',                      // OpenBSD
      	  '/usr/local/share/certs/ca-root-nss.crt', // DragonFly
      	  '/etc/openssl/certs/ca-certificates.crt'  // NetBSD
        {$endif OSLINUXANDROID}
        ]);
    scsCA:
      begin
        files := StringFromFolders([
          {$ifdef OSLINUXANDROID}
            '/etc/ssl/certs',               // Debian/SLES10/SLES11
            '/etc/pki/tls/certs',           // Fedora/RHEL
      	    '/system/etc/security/cacerts'  // Android
          {$else}
            '/etc/ssl/certs',         // FreeBSD 12.2+
            '/usr/local/share/certs', // FreeBSD
            '/etc/openssl/certs'      // NetBSD
          {$endif OSLINUXANDROID}
          ]);
        for f := 0 to length(files) - 1 do
          if (PosEx('-----BEGIN', files[f]) <> 0) and
             IsAnsiCompatible(files[f]) and
             (PosEx(files[f], result) = 0) then // append PEM files once
            result := result + #10 + files[f];
      end;
  end;
end;


const
  // on POSIX, we store the SMBIOS data in a local cache for non-root users
  SMB_CACHE = '/var/tmp/.synopse.smb';
  SMB_FLAGS = $010003ff; // assume 3.0 SMB is good enough
  // local storage of fallback UUID
  UUID_CACHE = '/var/tmp/.synopse.uid';
  // note: /var/tmp is cleaned up by systemd after 30 days so we set S_ISVTX
  //   see https://systemd.io/TEMPORARY_DIRECTORIES

{$ifdef CPUINTEL}
const
  // potential location of the SMBIOS buffer pointers within a 64KB fixed frame
  SMB_START  = $000f0000;
  SMB_STOP   = $00100000;

function GetSmbEfiMem: RawByteString; forward; // Linux/BSD dedicated versions
function SearchSmbios(const mem: RawByteString; var info: TRawSmbiosInfo): PtrUInt;
  forward; // implemented later in mormot.core.os.pas

function GetRawSmbiosFromMem(var info: TRawSmbiosInfo): boolean;
var
  mem: RawByteString;
  addr: PtrUInt;
  {$ifdef OSLINUX}
  fromsysfs: boolean;
  {$endif OSLINUX}
begin
  result := false;
  {$ifdef OSLINUX}
  // on Linux, first try from sysfs tables
  fromsysfs := false;
  mem := StringFromFile('/sys/firmware/dmi/tables/smbios_entry_point', true);
  if mem <> '' then
    fromsysfs := true
  else
  {$endif OSLINUX}
    // then try to read system EFI entries
    mem := GetSmbEfiMem;
  if mem = '' then
    // last fallback to raw memory reading (won't work on modern/EFI systems)
    mem := ReadSystemMemory(SMB_START, SMB_STOP - SMB_START);
  if mem = '' then
    exit;
  addr := SearchSmbios(mem, info);
  if addr = 0 then
    exit;
  {$ifdef OSLINUX}
  if fromsysfs then
    info.data := StringFromFile('/sys/firmware/dmi/tables/DMI', {nosize=}true)
  else
  {$endif OSLINUX}
    info.data := ReadSystemMemory(addr, info.Length);
  result := info.data <> '';
end;
{$endif CPUINTEL}

function _GetRawSmbios(var info: TRawSmbiosInfo): boolean;
begin
  {$ifdef CPUINTEL}
  result := GetRawSmbiosFromMem(info);
  if result then
    exit;
  {$else} // do not mess with low-level RAM buffer scanning on ARM/AARCH64
  result := false; // untested and reported as clearly faulty on some platforms
  {$endif CPUINTEL}
  PCardinal(@info)^ := SMB_FLAGS; // mark as retrieved from cache
  info.Data := StringFromFile(SMB_CACHE); // cache is better than PosixInject
  if (info.Data <> '') and
     (CompressSynLZ(info.Data, false) = '') then
    info.Data := ''; // tampered file
  if info.Data = '' then
    if Assigned(PosixInject.GetSmbiosData) then
    begin
      info.Data := PosixInject.GetSmbiosData; // e.g. from mormot.core.os.mac
      if info.Data <> '' then
        PCardinal(@info)^ := SMB_FLAGS - 1; // mark retrieved from PosixInject
    end;
  if info.Data = '' then
    exit;
  info.Length := length(info.Data);
  result := true;
end;

procedure PosixInjectSmbiosInfo(var info: TSmbiosBasicInfos);
var
  i: TSmbiosBasicInfo;
begin
  if Assigned(PosixInject.GetSmbios) then // e.g. from mormot.core.os.mac
    for i := succ(low(i)) to high(i) do
      if info[i] = '' then
        info[i] := PosixInject.GetSmbios(i);
end;

procedure _AfterDecodeSmbios(var info: TRawSmbiosInfo);
var
  s: RawByteString;
begin
  // complete information e.g. from mormot.core.os.mac
  if Assigned(PosixInject.GetSmbios) then
    PosixInjectSmbiosInfo(_Smbios);
  // check if require persistence after some HW changes
  if (PCardinal(@info)^ = SMB_FLAGS) or
     (CompressSynLZGetHash32(StringFromFile(SMB_CACHE)) = Hash32(info.Data)) then
    exit;
  // cache raw SMBIOS data for non-root users
  s := info.Data;
  CompressSynLZ(s, true); // SynLZ + Hash32 to avoid tampered file
  FileFromString(s, SMB_CACHE);
  FileSetSticky(SMB_CACHE);
  DeleteFile(UUID_CACHE); // this file is now superfluous and maybe inconsistent
end;

function SeemsRealPointer(p: pointer): boolean;
begin
  // let the GPF happen silently in the kernel (validated on Linux only)
  result := (PtrUInt(p) > 65535) and
            (fpaccess(p, F_OK) <> 0) and
            (fpgeterrno <> ESysEFAULT);
end;

const
  DT_UNKNOWN  = 0; // need to call fpstat() if this is returned (depends on FS)
  DT_FIFO     = 1;
  DT_CHR      = 2;
  DT_DIR      = 4;
  DT_BLK      = 6;
  DT_REG      = 8;
  DT_LNK      = 10;
  DT_SOCK     = 12;
  DT_WHT      = 14;

function PosixFileNames(const Folder: TFileName; Recursive: boolean): TRawUtf8DynArray;
var
  n: PtrInt;
  root: TFileName;

  procedure DoFolder(const subpath: TFileName);
  var
    d: pDir;
    e: pDirent;
    pl, el: PtrInt;
    fn: RawUtf8;
  begin
    d := FpOpendir(root + subpath); // (much) faster alternative to FindFirst()
    if d = nil then
      exit;
    pl := length(subpath);
    if pl <> 0 then
      inc(pl);
    repeat
      e := FpReaddir(d^); // FPC RTL use getdents64 syscall on Linux and BSD :)
      if e = nil then
        break;
      // fn := [subpath + '/'] + e^.d_name
      el := StrLen(@e^.d_name);
      FastSetString(fn, pl + el);
      if pl <> 0 then
      begin
        MoveFast(pointer(subpath)^, pointer(fn)^, pl - 1);
        PByteArray(fn)[pl - 1] := ord('/');
      end;
      MoveFast(e^.d_name, PByteArray(fn)[pl], el);
      // handle this entry
      case e.d_type of
        DT_UNKNOWN, // assume modern FS over BSD or Linux Kernel >= 2.6.4
        DT_REG:
          begin
            if n = 0 then // generous initial result capacity
              SetLength(result, 128)
            else if n = length(result) then
              SetLength(result, NextGrow(n));
            result[n] := fn;
            inc(n);
          end;
        DT_DIR:
          if Recursive and
             (e^.d_name[0] <> '.')  then
            DoFolder(fn);
      end;
    until false;
    FpClosedir(d^);
  end;

begin
  result := nil;
  n := 0;
  root := IncludeTrailingPathDelimiter(Folder);
  DoFolder('');
  if n <> 0 then
    DynArrayFakeLength(result, n);
end;

{$ifdef OSBSDDARWIN}

function IsValidPid(pid: cardinal): boolean;
begin
  result := pid <> 0;
end;

function EnumAllProcesses: TCardinalDynArray;
begin
  result := nil;
  // not implemented yet on BSD/Darwin
  // - fpsysctl with CTL_KERN + KERN_PROC + KERN_PROC_ALL is highly OS dependent
  // and headers are over-complicated so almost impossible to safely use in FPC:
  // https://github.com/apple-opensource/xnu/blob/master/bsd/sys/sysctl.h#L975
  // https://stackoverflow.com/a/6945542/458259
  // - kvm_openfiles / kvm_getprocs may be a good option:
  // https://kaashif.co.uk/2015/06/18/how-to-get-a-list-of-processes-on-openbsd-in-c
end;

function EnumProcessName(PID: cardinal): RawUtf8;
begin
  result := ''; // not implemented yet on BSD/Darwin
  // use fpsysctl with CTL_KERN + KERN_PROC + KERN_PROC_PID
  //  https://man.openbsd.org/sysctl.2#KERN_PROC_PID
  // another trouble is that the name is likely to be truncated to 16 chars
  // because it is defined p_comm[MAXCOMLEN + 1] in the very complex headers
end;

function _IsDebuggerPresent: boolean;
begin
  // rough detection for FPC on BSD (not yet working because of EnumProcessName)
  result := PosEx('lazarus', LowerCase(EnumProcessName(FpGetppid))) <> 0;
end;

function GetParentProcess(PID: cardinal): cardinal;
begin
  if PID = 0 then
    result := FpGetppid // we have a system call for the currrent process :)
  else
    result := 0; // not implemented yet on BSD/Darwin
end;

function fpsysctlhwint(hwid: cint): Int64;
var
  mib: array[0..1] of cint;
  len: cint;
begin
  result := 0;
  mib[0] := CTL_HW;
  mib[1] := hwid;
  len := SizeOf(result);
  fpsysctl(pointer(@mib), 2, @result, @len, nil, 0);
end;

function fpsysctlhwstr(hwid: cint; var temp: ShortString): PUtf8Char;
var
  mib: array[0..1] of cint;
  len: cint;
begin
  mib[0] := CTL_HW;
  mib[1] := hwid;
  FillCharFast(temp, SizeOf(temp), 0); // ShortString as 0-terminated buffer
  len := SizeOf(temp);
  fpsysctl(pointer(@mib), 2, @temp, @len, nil, 0);
  if temp[0] <> #0 then
    result := @temp
  else
    result := nil;
end;

function fpsysctlbynamehwstr(name: PAnsiChar; var temp: ShortString): PUtf8Char;
var
  len: cint;
begin
  FillCharFast(temp, SizeOf(temp), 0); // ShortString as 0-terminated buffer
  len := SizeOf(temp);
  FPsysctlbyname(name, @temp, @len, nil, 0);
  if temp[0] <> #0 then
    result := @temp
  else
    result := nil;
end;

type
  TLoadAvg = array[0..2] of double;

function getloadavg(var loadavg: TLoadAvg; nelem: integer): integer;
  cdecl external clib name 'getloadavg';

function RetrieveLoadAvg: RawUtf8;
var
  avg: TLoadAvg;
begin
  if getloadavg(avg, 3) = 3 then
    result := _fmt('%g %g %g', [avg[0], avg[1], avg[2]])
  else
    result := '';
end;

{$ifdef OSFREEBSD}

const
  KENV_GET = 0;
  KENV_SET = 1;

function kenv(action: integer; name, value: PAnsiChar; len: integer): integer;
  cdecl external clib name 'kenv';

function GetSmbEfiMem: RawByteString;
var
  tmp: array[byte] of AnsiChar;
  xaddr: PtrUInt;
begin
  result := '';
  if kenv(KENV_GET, 'hint.smbios.0.mem', @tmp, SizeOf(tmp)) < 0 then
    exit;
  xaddr := PosixParseHex32(@tmp); // typical value is '0xf05b0'
  if xaddr <> 0 then
    result := ReadSystemMemory(xaddr, 1024); // 32 bytes is enough
end;

const
  _KNOWN: array[0..14] of record
    id: TSmbiosBasicInfo;
    fn: RawUtf8;
  end = (
    (id: sbiBiosVendor;        fn: 'bios.vendor'),
    (id: sbiBiosVersion;       fn: 'bios.version'),
    (id: sbiBiosDate;          fn: 'bios.reldate'),
    (id: sbiManufacturer;      fn: 'system.maker'),
    (id: sbiProductName;       fn: 'system.product'),
    (id: sbiVersion;           fn: 'system.version'),
    (id: sbiSerial;            fn: 'system.serial'),
    (id: sbiUuid;              fn: 'system.uuid'),
    (id: sbiSku;               fn: 'system.sku'),
    (id: sbiFamily;            fn: 'system.family'),
    (id: sbiBoardManufacturer; fn: 'planar.maker'),
    (id: sbiBoardProductName;  fn: 'planar.product'),
    (id: sbiBoardVersion;      fn: 'planar.version'),
    (id: sbiBoardSerial;       fn: 'planar.serial'),
    (id: sbiBoardAssetTag;     fn: 'planar.tag')
  );

procedure _DirectSmbiosInfo(out info: TSmbiosBasicInfos);
var
  i: PtrInt;
  tmp: array[byte] of AnsiChar;
begin
  for i := 0 to high(_KNOWN) do
    with _KNOWN[i] do
      if kenv(KENV_GET, PAnsiChar('smbios.' + fn), @tmp, SizeOf(tmp)) >= 0 then
        info[id] := TrimU(tmp);
end;

{$else}

// help is needed to implement those on buggy Mac OS
// may fallback to PosixInject wrappers from mormot.core.os.mac

function GetSmbEfiMem: RawByteString;
begin
  result := '';
end;

{$ifdef OSDARWIN}
function ReadSystemMemory(address, size: PtrUInt): RawByteString;
begin
  result := '';
end;
{$endif OSDARWIN}

procedure _DirectSmbiosInfo(out info: TSmbiosBasicInfos);
begin
end;

{$endif OSFREEBSD}

function GetMemoryInfo(out info: TMemoryInfo; withalloc: boolean): boolean;
begin
  FillCharFast(info, SizeOf(info), 0);
  info.memtotal := SystemMemorySize; // retrieved at startup
  info.memfree := info.memtotal - fpsysctlhwint(HW_USERMEM);
  result := info.memtotal <> 0;// avoid div per 0 exception
  if result then
    info.percent := ((info.memtotal - info.memfree) * 100) div info.memtotal;
end;

procedure XorOSEntropy(var e: THash512Rec);
var
  mem: TMemoryInfo;
  avg: TLoadAvg absolute mem;
  us: Int64 absolute mem.vmtotal; // =0 after above GetMemoryInfo()
  guid: THash128Rec absolute mem.filetotal; // also = 0
begin
  //some minimal OS entropy we could get for BSD/Darwin
  QueryPerformanceMicroSeconds(us);
  e.i[0] := e.i[0] xor us;
  e.i[1] := e.i[1] xor GetTickCount64;
  getloadavg(avg, 3);
  DefaultHasher128(@e.h1, @avg, SizeOf(avg)); // may be AesNiHash128
  GetMemoryInfo(mem, {withalloc=}false);
  DefaultHasher128(@e.h2, @mem, SizeOf(mem));
  {$ifdef OSDARWIN} // FPC CreateGuid calls /dev/urandom which is not advised
  guid.Lo := mach_absolute_time; // monotonic clock in nanoseconds
  guid.Hi := mach_continuous_time;
  {$else}
  CreateGuid(guid.guid); // use e.g. FreeBSD syscall or /dev/urandom
  {$endif OSDARWIN}
  QueryPerformanceMicroSeconds(us); // should have changed in-between
  crcblocks(@e.h3, @mem, SizeOf(mem) shr 4); // another algo
end;

{$else} // Linux-specific code

function IsValidPid(pid: cardinal): boolean;
var
  status, tgid: RawUtf8;
begin
  result := false;
  if pid = 0 then
    exit;
  status := StringFromFile('/proc/' + IntToStr(pid) + '/status', {nosize=}true);
  // ensure is a real process, not a thread
  // https://www.memsql.com/blog/the-curious-case-of-thread-groups-identifiers
  FindNameValue(status, 'TGID:', tgid);
  result := GetCardinal(pointer(tgid)) = pid;
end;

function EnumAllProcesses: TCardinalDynArray;
var
  d: pDir;
  e: pDirent;
  n: integer;
  pid: cardinal;
begin
  result := nil;
  d := FpOpendir('/proc'); // (much) faster alternative to FindFirst()
  if d = nil then
    exit;
  n := 0;
  SetLength(result, 128);
  repeat
    e := FpReaddir(d^); // FPC RTL uses direct getdents syscall on Linux/BSD :)
    if e = nil then
      break;
    if (e.d_type in [DT_UNKNOWN, DT_DIR]) and
       (e.d_name[0] in ['1'..'9']) then
    begin
      pid := GetCardinal(@e.d_name[0]);
      if (pid <> 0) and
         IsValidPid(pid) then
        AddInteger(TIntegerDynArray(result), n, pid);
    end;
  until false;
  FpClosedir(d^);
  if n = 0 then
    result := nil
  else
    DynArrayFakeLength(result, n);
end;

var
  tryprocexe: boolean = true;

function EnumProcessName(PID: cardinal): RawUtf8;
var
  proc: TFileName;
  cmdline: RawUtf8;
begin
  proc := '/proc/' + IntToStr(PID);
  if tryprocexe then
  begin
    // need to be root to follow /proc/[pid]/exe
    result := fpReadLink(proc + '/exe');
    if result <> '' then
      exit;
  end;
  cmdline := StringFromFile(proc + '/cmdline', {nosize=}true);
  // set of strings separated by null bytes -> exe is the first argument
  FastSetString(result, pointer(cmdline), StrLen(pointer(cmdline)));
  if result <> '' then
    tryprocexe := false; // no need to try again next time
end;

function GetParentProcess(PID: cardinal): cardinal;
var
  status, ppid: RawUtf8;
begin
  if PID = 0 then
    result := FpGetppid // we have a system call for the current process :)
  else
  begin
    result := 0;
    status := StringFromFile('/proc/' + IntToStr(PID) + '/status', {nosize=}true);
    if status = '' then
      exit; // no such process
    FindNameValue(status, 'PPID:', ppid);
    result := GetCardinal(pointer(ppid));
  end;
end;

function _IsDebuggerPresent: boolean;
var
  status, tracerpid: RawUtf8;
begin
  status := StringFromFile('/proc/self/status', {nosize=}true);
  FindNameValue(status, 'TRACERPID:', tracerpid);
  result := (tracerpid <> '0');
end;

function RetrieveLoadAvg: RawUtf8;
begin
  // the libc parses this file anyway :)
  result := TrimU(StringFromFile('/proc/loadavg', {nosize=}true));
end;

function FindMemInfo(const meminfo, up: RawUtf8): PtrUInt;
var
  v: RawUtf8;
begin
  FindNameValue(meminfo, up, v);
  result := GetCardinal(pointer(v)) shl 10; // from KB to bytes
end;

function GetMemoryInfo(out info: TMemoryInfo; withalloc: boolean): boolean;
var
  proc: RawUtf8;
  P: PUtf8Char;
begin
  result := false;
  FillCharFast(info, SizeOf(info), 0);
  // sysinfo() syscall has not enough information: use /proc sysfiles
  proc := StringFromFile('/proc/meminfo', {hasnosize=}true);
  if proc = '' then
    exit;
  info.memtotal  := FindMemInfo(proc, 'MEMTOTAL:');
  info.memfree   := FindMemInfo(proc, 'MEMAVAILABLE:'); // MemFree is too low
  info.filetotal := FindMemInfo(proc, 'SWAPTOTAL:');
  info.filefree  := FindMemInfo(proc, 'SWAPFREE:');
  // note: Windows-like virtual memory information is not available under Linux
  info.vmtotal   := FindMemInfo(proc, 'COMMITLIMIT:');
  info.vmfree    := FindMemInfo(proc, 'MEMFREE:');
  if info.memfree = 0 then // kernel < 3.14 may not have the MemAvailable field
    info.memfree  := info.vmfree +
                     FindMemInfo(proc, 'BUFFERS:') +
                     FindMemInfo(proc, 'CACHED:')  +
                     FindMemInfo(proc, 'SRECLAIMABLE:') -
                     FindMemInfo(proc, 'SHMEM:');
  if info.memtotal <> 0 then
  begin
    info.percent := ((info.memtotal - info.memfree) * 100) div info.memtotal;
    result := true;
  end;
  if not withalloc then
    exit;
  // GetHeapStatus is only about current thread -> use /proc/[pid]/statm
  proc := StringFromFile('/proc/self/statm', {hasnosize=}true);
  P := pointer(proc);
  info.allocreserved := GetNextCardinal(P) * SystemInfo.dwPageSize; // VmSize
  info.allocused     := GetNextCardinal(P) * SystemInfo.dwPageSize; // VmRSS
end;

procedure DoHash128File(var h: THash128; const filename: TFileName);
var
  s: RawByteString;
begin
  s := StringFromFile(filename, {nosize=}true);
  DefaultHasher128(@h, pointer(s), length(s)); // maybe AesNiHash128
end;

procedure XorOSEntropy(var e: THash512Rec);
var
  si: TSysInfo;  // Linuxism
  rt: TTimeSpec; // with nanoseconds resolution
begin
  clock_gettime(CLOCK_MONOTONIC_HIRES, @rt);
  DefaultHasher128(@e.h0, @rt, SizeOf(rt)); // maybe AesNiHash128
  SysInfo(@si); // uptime + loadavg + meminfo + numprocess
  DefaultHasher128(@e.h0, @si, SizeOf(si));
  // detailed CPU execution context and timing from Linux kernel
  DoHash128File(e.h0, '/proc/self/statm');
  DoHash128File(e.h0, '/proc/self/stat');
  DoHash128File(e.h1, '/proc/stat');
  clock_gettime(CLOCK_UPTIME, @rt);
  DefaultHasher128(@e.h2, @rt, SizeOf(rt)); // maybe AesNiHash128
  // read-only 122-bit random UUID text '6fd5a44b-35f4-4ad4-a9b9-6b9be13e1fe9'
  DoHash128File(e.h2, '/proc/sys/kernel/random/uuid');
  DoHash128File(e.h3, '/proc/sys/kernel/random/boot_id');
  clock_gettime(CLOCK_MONOTONIC_HIRES, @rt); // should have changed in-between
  DefaultHasher128(@e.h3, @rt, SizeOf(rt));
end;

{$ifdef OSANDROID}

procedure _DirectSmbiosInfo(out info: TSmbiosBasicInfos);
begin
end;

{$else} // pure Linux

const
  // note: reading some of the /sys/class/dmi/id/* files may require root access
  _KNOWN: array[0..15] of record
    id: TSmbiosBasicInfo;
    fn: string;
  end = (
    (id: sbiBiosVendor;        fn: 'bios_vendor'),
    (id: sbiBiosVersion;       fn: 'bios_version'),
    (id: sbiBiosDate;          fn: 'bios_date'),
    (id: sbiBiosRelease;       fn: 'bios_release'),
    (id: sbiManufacturer;      fn: 'sys_vendor'),
    (id: sbiProductName;       fn: 'product_name'),
    (id: sbiVersion;           fn: 'product_version'),
    (id: sbiSerial;            fn: 'product_serial'),
    (id: sbiUuid;              fn: 'product_uuid'),
    (id: sbiSku;               fn: 'product_sku'),
    (id: sbiFamily;            fn: 'product_family'),
    (id: sbiBoardManufacturer; fn: 'board_vendor'),
    (id: sbiBoardProductName;  fn: 'board_name'),
    (id: sbiBoardVersion;      fn: 'board_version'),
    (id: sbiBoardSerial;       fn: 'board_serial'),
    (id: sbiBoardAssetTag;     fn: 'board_asset_tag')
  );

procedure _DirectSmbiosInfo(out info: TSmbiosBasicInfos);
var
  i: PtrInt;
begin
  for i := 0 to high(_KNOWN) do
    with _KNOWN[i] do
      info[id] := TrimU(StringFromFile('/sys/class/dmi/id/' + fn, {nosize=}true));
  // note: /var/lib/dbus/machine-id and /etc/machine-id are SW generated from
  // random at system install so do NOT match sbiUuid HW DMI value - see
  // https://www.freedesktop.org/software/systemd/man/machine-id.html
end;

{$endif OSANDROID}

{$endif OSBSDDARWIN}

var
  __IsDebuggerPresent: (idpUntested, idpNone, idpPresent);

function IsDebuggerPresent: boolean;
begin
  if __IsDebuggerPresent = idpUntested then
    if _IsDebuggerPresent then
      __IsDebuggerPresent := idpPresent
    else
      __IsDebuggerPresent := idpNone;
  result := __IsDebuggerPresent = idpPresent;
end;

{$ifndef OSDARWIN}
// on POSIX systems, /dev/mem may be available from root
// but sometimes even root can't access it on hardened systems
function ReadSystemMemory(address, size: PtrUInt): RawByteString;
var
  mem: cInt;
  map: PAnsiChar;
  off: PtrUInt;
begin
  result := '';
  if size > 4 shl 20 then
    exit; // read up to 4MB
  mem := FpOpen('/dev/mem', O_RDONLY, 0);
  if mem <= 0 then
    exit;
  // Fpmmap() is more complex but works around problems using plain read() calls
  off := address mod SystemInfo.dwPageSize;
  map := Fpmmap(nil, off + size, PROT_READ, MAP_SHARED, mem, address - off);
  if map <> MAP_FAILED then
  begin
    FastSetRawByteString(result, map + off, size);
    Fpmunmap(map, off + size);
  end;
  FpClose(mem);
end;
{$endif OSDARWIN}

procedure DirectSmbiosInfo(out info: TSmbiosBasicInfos);
begin
  // retrieve OS-dependent information
  _DirectSmbiosInfo(info);
  // normalize some entries
  info[sbiUuid] := LowerCase(info[sbiUuid]);
  // some missing info may have retrieved at startup of this unit
  if info[sbiCpuVersion] = '' then
    info[sbiCpuVersion] := CpuInfoText;
  // e.g. from mormot.core.os.mac
  if Assigned(PosixInject.GetSmbios) then
    PosixInjectSmbiosInfo(info);
end;

function RetrieveSystemTimes(out IdleTime, KernelTime, UserTime: Int64): boolean;
begin
  result := false; // should call e.g. RetrieveLoadAvg() instead
end;

function RetrieveProcessInfo(PID: cardinal; out KernelTime, UserTime: Int64;
  out WorkKB, VirtualKB: cardinal): boolean;
begin
  result := false;
end;

function TProcessInfo.Init: boolean;
begin
  FillCharFast(self, SizeOf(self), 0);
  result := false;
end;

function TProcessInfo.Start: boolean;
begin
  result := false;
end;

function TProcessInfo.PerProcess(PID: cardinal; Now: PDateTime;
  out Data: TSystemUseData; var PrevKernel, PrevUser: Int64): boolean;
begin
  result := false;
end;

function TProcessInfo.PerSystem(out Idle, Kernel, User: single): boolean;
var
  P: PUtf8Char;
  U, K, I, S: cardinal;
begin
  // see http://www.linuxhowtos.org/System/procstat.htm
  result := false;
  P := pointer(StringFromFile('/proc/stat', {nosize=}true));
  if P = nil then
    exit;
  // e.g. 'cpu  3418147 18140 265232 6783435 12184 0 34219 0 0 0'
  U := GetNextCardinal(P){=user} + GetNextCardinal(P){=nice};
  K := GetNextCardinal(P){=system};
  I := GetNextCardinal(P){=idle};
  S := U + K + I;
  result := S <> 0;
  if not result then
    exit;
  Kernel := {%H-}SimpleRoundTo2Digits((K * 100) / S);
  User   := {%H-}SimpleRoundTo2Digits((U * 100) / S);
  Idle   := 100 - Kernel - User; // ensure sum is always 100%
end; { TODO : use a diff approach for TProcessInfo.PerSystem on Linux? }


function FillSystemRandom(Buffer: PByteArray; Len: integer;
  AllowBlocking: boolean): boolean;
var
  rd, dev: integer;
begin
  result := false;
  if Len <= 0 then
    exit;
  dev := FileOpenSequentialRead('/dev/urandom');  // non blocking on Linux + BSD
  if (dev <= 0) and
     AllowBlocking then
    dev := FileOpenSequentialRead('/dev/random'); // may block until got entropy
  if dev > 0 then
    try
      rd := 32; // read up to 256 bits - see "man urandom" Usage paragraph
      if Len <= 32 then
        rd := Len;
      result := (FileRead(dev, Buffer[0], rd) = rd);
      if result and
         (Len > 32) then
        RandomBytes(@Buffer[32], Len - 32); // simple gsl_rng_taus2 padding
    finally
      FileClose(dev);
    end;
  if not result then
    // OS API call failed -> fallback to our Lecuyer's gsl_rng_taus2 generator
    RandomBytes(pointer(Buffer), Len);
end;

function GetDiskInfo(var aDriveFolderOrFile: TFileName;
  out aAvailableBytes, aFreeBytes, aTotalBytes: QWord): boolean;
var
  fs: tstatfs;
begin
  if aDriveFolderOrFile = '' then
    aDriveFolderOrFile := '.';
  FillCharFast(fs, SizeOf(fs), 0);
  result := fpStatFS(aDriveFolderOrFile, @fs) = 0;
  aAvailableBytes := QWord(fs.bavail) * QWord(fs.bsize);
  aFreeBytes := aAvailableBytes; // no user Quota involved here
  aTotalBytes := QWord(fs.blocks) * QWord(fs.bsize);
end;

function GetDiskPartitions: TDiskPartitions;
var
  mounts, fs, mnt, typ: RawUtf8;
  p: PUtf8Char;
  fn: TFileName;
  n: integer;
  av, fr, tot: QWord;
begin
  // see https://github.com/gagern/gnulib/blob/master/lib/mountlist.c
  result := nil;
  {$ifdef OSLINUXANDROID}
  mounts := StringFromFile('/proc/self/mounts', {hasnosize=}true);
  if mounts = '' then
  {$endif OSLINUXANDROID}
    mounts := StringFromFile('/etc/mtab', {hasnosize=}true);
  n := 0;
  p := pointer(mounts);
  if p <> nil then // e.g. Darwin has no /etc/mtab :(
  repeat
    fs :=  GetNextItem(p);
    mnt := GetNextItem(p);
    typ := GetNextItem(p);
    if (fs <> '') and
       (fs <> 'rootfs') and
       not IdemPChar(pointer(fs), '/DEV/LOOP') and
       (mnt <> '') and
       (mnt <> '/mnt') and
       (typ <> '') and
       not IdemPChars(mnt, ['/PROC/', '/SYS/', '/RUN/']) and
       not IdemPChars(typ, ['AUTOFS', 'PROC', 'SUBFS', 'DEBUGFS', 'DEVPTS',
        'FUSECTL', 'MQUEUE', 'RPC-PIPEFS', 'SYSFS', 'DEVFS', 'KERNFS',
        'IGNORE', 'NONE', 'TMPFS', 'SECURITYFS', 'RAMFS', 'ROOTFS', 'DEVTMPFS',
        'HUGETLBFS', 'ISO9660']) then
    begin
      fn := mnt;
      if GetDiskInfo(fn, av, fr, tot) and
         (tot > 1 shl 20) then
      begin
  //writeln('fs=',fs,' mnt=',mnt,' typ=',typ, ' av=',av,' fr=',fr,' tot=',tot);
        if n = length(result) then
          SetLength(result, NextGrow(n));
        if length(fs) > 24 then
          fs := copy(fs, 1, 24) + '..';
        result[n].name := fs;
        result[n].mounted := fn;
        result[n].size := tot;
        inc(n);
      end;
    end;
    p := GotoNextLine(p);
  until p = nil;
  SetLength(result, n);
end;

{$ifdef OSBSDDARWIN}
  {$define USEMPROTECT}
{$else}
  {$ifdef OSANDROID}
    {$define USEMPROTECT}
  {$endif OSANDROID}
{$endif OSBSDDARWIN}

{$ifdef USEMPROTECT}
function mprotect(Addr: Pointer; Len: size_t; Prot: integer): integer;
  cdecl external clib name 'mprotect';
{$endif USEMPROTECT}

function SynMProtect(addr: pointer; size: size_t; prot: integer): integer;
begin
  result := -1;
  {$ifdef UNIX}
    {$ifdef USEMPROTECT}
    result := mprotect(addr, size, prot);
    {$else}
    if Do_SysCall(syscall_nr_mprotect, TSysParam(addr), size, prot) >= 0 then
      result := 0;
    {$endif USEMPROTECT}
  {$endif UNIX}
end;

procedure PatchCode(Old, New: pointer; Size: PtrInt; Backup: pointer;
  LeaveUnprotected: boolean);
var
  PageSize: PtrUInt;
  AlignedAddr: pointer;
  i: PtrInt;
  ProtectedResult, ProtectedMemory: boolean;
begin
  if Backup <> nil then
    for i := 0 to Size - 1 do // do not use Move() here
      PByteArray(Backup)^[i] := PByteArray(Old)^[i];
  PageSize := SystemInfo.dwPageSize;
  AlignedAddr :=
    Pointer((PtrUInt(Old) div SystemInfo.dwPageSize) * SystemInfo.dwPageSize);
  while PtrUInt(Old) + PtrUInt(Size) >= PtrUInt(AlignedAddr) + PageSize do
    inc(PageSize, SystemInfo.dwPageSize);
  ProtectedResult := SynMProtect(
    AlignedAddr, PageSize, PROT_READ or PROT_WRITE or PROT_EXEC) = 0;
  ProtectedMemory := not ProtectedResult;
  if ProtectedMemory then
    ProtectedResult := SynMProtect(
      AlignedAddr, PageSize, PROT_READ or PROT_WRITE) = 0;
  if ProtectedResult then
    try
      for i := 0 to Size - 1 do // do not use Move() here
        PByteArray(Old)^[i] := PByteArray(New)^[i];
      if not LeaveUnprotected and
         ProtectedMemory then
        SynMProtect(AlignedAddr, PageSize, PROT_READ or PROT_EXEC);
    except
      // we ignore any exception here - it should work anyway
    end;
end;

const
  STUB_SIZE = 65536; // 16*4 KB (4 KB = memory granularity)
  // on most platforms, Compute_FAKEVMT is run once with all JITted stubs
  // on i386, it needs ArgsSizeInStack adjustement, but only 24 bytes per method

{$ifdef CPUARM}

var
  StubCallAllocMemLastStart: PtrUInt; // avoid unneeded fpmmap() calls

function StubCallAllocMem(const Size, flProtect: DWORD): pointer;
const
  STUB_RELJMP = {$ifdef CPUARM} $7fffff {$else} $7fffffff {$endif}; // rel jmp
  STUB_INTERV = STUB_RELJMP + 1; // try to reserve in closed stub interval
  STUB_ALIGN = QWord($ffffffffffff0000); // align to STUB_SIZE
var
  start, stop, stub, dist: PtrUInt;
begin
  stub := PtrUInt(ArmFakeStubAddr); // = @TInterfacedObjectFake.ArmFakeStub
  if StubCallAllocMemLastStart <> 0 then
    start := StubCallAllocMemLastStart
  else
  begin
    start := stub - STUB_INTERV;
    if start > stub then
      start := 0; // avoid range overflow
    start := start and STUB_ALIGN;
  end;
  stop := stub + STUB_INTERV;
  if stop < stub then
    stop := high(PtrUInt);
  stop := stop and STUB_ALIGN;
  while start < stop do
  begin
    // try whole -STUB_INTERV..+STUB_INTERV range
    inc(start, STUB_SIZE);
    result := fpmmap(pointer(start), STUB_SIZE,
      flProtect, MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
    if result <> MAP_FAILED then
    begin
      // close enough for a 24/32-bit relative jump?
      dist := abs(stub - PtrUInt(result));
      if dist < STUB_RELJMP then
      begin
        StubCallAllocMemLastStart := start;
        exit;
      end
      else
        fpmunmap(result, STUB_SIZE);
    end;
  end;
  result := MAP_FAILED; // error
end;

{$else}

// other platforms (Intel+Arm64) use plain Kernel call and PtrInt jump
function StubCallAllocMem(const Size, flProtect: DWORD): pointer;
begin
  result := fpmmap(nil, STUB_SIZE,
    flProtect, MAP_PRIVATE OR MAP_ANONYMOUS, -1, 0);
end;

{$endif CPUARM}


{ ****************** Unix Daemon and Windows Service Support }

// Linux/POSIX signal interception

var
  SynDaemonIntercepted: boolean;
  SynDaemonInterceptLog: TSynLogProc;

procedure DoShutDown(Sig: integer; Info: PSigInfo; Context: PSigContext); cdecl;
var
  level: TSynLogLevel;
  si_code: integer;
  text: string[4]; // code below has no memory allocation
begin
  if Assigned(SynDaemonInterceptLog) then
  begin
    case Sig of
      SIGQUIT:
        text := 'QUIT';
      SIGTERM:
        text := 'TERM';
      SIGINT:
        text := 'INT';
      SIGABRT:
        text := 'ABRT';
    else
      text := 'SIG';
    end;
    if Sig = SIGTERM then
      // polite quit
      level := sllInfo
    else
       // abort after panic
      level := sllExceptionOS;
    if Info = nil then
      si_code := 0
    else
      si_code := Info^.si_code;
    SynDaemonInterceptLog(level,
      'SynDaemonIntercepted received SIG%=% si_code=%',
      [text, Sig, si_code], nil);
  end;
  SynDaemonTerminated := Sig;
end;

procedure SynDaemonIntercept(const onlog: TSynLogProc);
var
  sa: SigactionRec;
begin
  // note: SIGFPE/SIGSEGV/SIGBUS/SIGILL are handled by the RTL
  if SynDaemonIntercepted then
    exit;
  GlobalLock;
  try
    if SynDaemonIntercepted then
      exit;
    SynDaemonInterceptLog := onlog;
    FillCharFast(sa, SizeOf(sa), 0);
    sa.sa_handler := @DoShutDown;
    fpSigaction(SIGQUIT, @sa, nil);
    fpSigaction(SIGTERM, @sa, nil);
    fpSigaction(SIGINT,  @sa, nil);
    fpSigaction(SIGABRT, @sa, nil);
    SynDaemonIntercepted := true; // flag set AFTER interception
  finally
    GlobalUnLock;
  end;
end;

var
  SigPipeDisabled: boolean; // process-wide thread-safe flag

// TO INVESTIGATE: we may use per-thread signal masking instead
// http://www.microhowto.info/howto/ignore_sigpipe_without_affecting_other_threads_in_a_process.html

procedure DoNothing(Sig: integer; Info: PSigInfo; Context: PSigContext); cdecl;
begin
end;

procedure SigPipeIntercept;
var
  sa: SigactionRec;
begin
  if SigPipeDisabled then
    exit; // quickly return if already done
  GlobalLock;
  try
    if SigPipeDisabled then
      exit;
    FillCharFast(sa, SizeOf(sa), 0);
    sa.sa_handler := @DoNothing;
    fpSigaction(SIGPIPE, @sa, nil);
    SigPipeDisabled := true; // flag set AFTER disabling it
  finally
    GlobalUnLock;
  end;
end;

type
  TPasswd = record
    pw_name: PAnsiChar;    // user name
    pw_passwd: PAnsiChar;  // encrypted password
    pw_uid: TUid;	   // user uid
    pw_gid: TGid;	   // user gid
    // following fields are not consistent on BSD or Linux, but not needed
  end;
  PPasswd  = ^TPasswd;

// retrieve information of a given user by name
function getpwnam(name: PAnsiChar): PPasswd;
  cdecl external clib name 'getpwnam';

// sets the supplementary group IDs for the calling process
function setgroups(n: size_t; groups: PGid): integer;
  cdecl external clib name 'setgroups';

function setuid(uid: TUid): integer;
  cdecl external clib name 'setuid';
function setgid(gid: TGid): integer;
  cdecl external clib name 'setgid';

// changes the root directory of the calling process
function chroot(rootpath: PAnsiChar): integer;
  cdecl external clib name 'chroot';

function DropPriviledges(const UserName: RawUtf8): boolean;
var
  pwnam: PPasswd;
begin
  result := false;
  pwnam := getpwnam(pointer(UserName));
  if (pwnam = nil) or
     ((setgid(pwnam.pw_gid) <> 0) and
      (fpgeterrno <> ESysEPERM)) or
     ((setuid(pwnam.pw_uid) <> 0) and
      (fpgeterrno <> ESysEPERM)) then
    exit;
  result := true;
end;

function ChangeRoot(const FolderName: RawUtf8): boolean;
begin
  result := (FolderName <> '') and
            (FpChdir(pointer(FolderName)) = 0) and
            (chroot('.') = 0);
end;

function RunUntilSigTerminatedPidFile(ensureWritable: boolean): TFileName;
var
  pidpath: TFileName;
begin
  pidpath := RunUntilSigTerminatedPidFilePath;
  if pidpath = '' then
    pidpath := Executable.ProgramFilePath;
  if not ensureWritable then
  begin
    result := Format('%s.%s.pid', [pidpath, Executable.ProgramName]);
    if FileExists(result) then
      exit;
  end;
  if not IsDirectoryWritable(pidpath) then
    // if the executable folder is not writable, use the temporary folder
    pidpath := GetSystemPath(spTemp);
  result := Format('%s.%s.pid', [pidpath, Executable.ProgramName]);
end;

function RunUntilSigTerminatedState: TServiceState;
begin
  if FileExists(RunUntilSigTerminatedPidFile(false)) then
    result := ssRunning
  else
    result := ssStopped;
end;

function RunUntilSigTerminatedForKill(waitseconds: integer): boolean;
var
  pid: PtrInt;
  pidfilename: TFileName;
  tix: Int64;
begin
  result := false;
  pidfilename := RunUntilSigTerminatedPidFile;
  pid := GetInteger(pointer(StringFromFile(pidfilename)));
  if pid <= 0 then
    exit;
  if fpkill(pid, SIGTERM) <> 0 then // polite quit
    if fpgeterrno <> ESysESRCH then
      exit
    else
      // ESysESRCH = no such process -> try to delete the .pid file
      if DeleteFile(pidfilename) then
      begin
        result := true; // process crashed or hard reboot -> nothing to kill
        exit;
      end;
  if waitseconds <= 0 then
  begin
    result := true;
    exit;
  end;
  tix := GetTickCount64 + waitseconds * MilliSecsPerSec;
  repeat
    // RunUntilSigTerminated() below should delete the .pid file
    sleep(10);
    if not FileExists(pidfilename) then
      result := true;
  until result or
        (GetTickCount64 > tix);
  if not result then
    fpkill(pid, SIGKILL); // murder with finesse
end;

procedure CleanAfterFork;
begin
  fpUMask(0); // reset file mask
  chdir('/'); // avoid locking current directory
  Close(input);
  AssignFile(input, '/dev/null');
  ReWrite(input);
  Close(output);
  AssignFile(output, '/dev/null');
  ReWrite(output);
  Close(stderr);
end;

procedure RunUntilSigTerminated(daemon: TObject; dofork: boolean;
  const start, stop: TThreadMethod; const onlog: TSynLogProc;
  const servicename: string);
var
  pid, sid: TPID;
  pidfilename: TFileName;
  s: AnsiString;
const
  TXT: array[boolean] of string[4] = ('run', 'fork');
begin
  SynDaemonIntercept(onlog);
  if dofork then
  begin
    pidfilename := RunUntilSigTerminatedPidFile;
    pid := GetInteger(pointer(StringFromFile(pidfilename)));
    if pid > 0 then
      if (fpkill(pid, 0) = 0) or
         not DeleteFile(pidfilename) then
        raise EOSException.CreateFmt(
          '%s.CommandLine Fork failed: %s is already forked as pid=%d',
          [ClassNameShort(daemon)^, Executable.ProgramName, PtrInt(pid)]);
    pid := fpFork;
    if pid < 0 then
      raise EOSException.CreateFmt(
        '%s.CommandLine Fork failed', [ClassNameShort(daemon)^]);
    if pid > 0 then  // main program - just terminate
      exit;
    // clean forked instance
    sid := fpSetSID;
    if sid < 0 then // new session (process group) created?
      raise EOSException.CreateFmt(
        '%s.CommandLine SetSID failed', [ClassNameShort(daemon)^]);
    CleanAfterFork;
    // create local .[Executable.ProgramName].pid file
    pid := fpgetpid;
    str(pid, s);
    FileFromString(s, pidfilename);
  end;
  try
    if Assigned(onlog) then
      onlog(sllNewRun, 'Start % /% %',
        [servicename, TXT[dofork], Executable.Version.DetailedOrVoid], nil);
    Start;
    while SynDaemonTerminated = 0 do
      if GetCurrentThreadID = MainThreadID then
        CheckSynchronize(100)
      else
        Sleep(100);
  finally
    if Assigned(onlog) then
      onlog(sllNewRun, 'Stop /% from Sig=%',
        [TXT[dofork], SynDaemonTerminated], nil);
    try
      Stop;
    finally
      if dofork and
         (pidfilename <> '') then
      begin
        DeleteFile(pidfilename);
        if Assigned(onlog) then
          onlog(sllTrace, 'RunUntilSigTerminated: deleted file %',
            [pidfilename], nil);
      end;
    end;
  end;
end;

function RunInternal(args: PPAnsiChar; waitfor: boolean; const env: TFileName;
  options: TRunOptions): integer;
var
  pid: TPID;
  e: array[0..511] of PAnsiChar; // max 512 environment variables
  envpp: PPAnsiChar;
  P: PAnsiChar;
  n: PtrInt;
begin
  {$ifdef FPC}
  {$if (defined(BSD) or defined(SUNOS)) and defined(FPC_USE_LIBC)}
  pid := FpvFork;
  {$else}
  pid := FpFork;
  {$ifend}
  {$else}
  'only FPC is supported yet';
  {$endif FPC}
  if pid < 0 then
  begin
    // fork failed
    result := -1;
    exit;
  end;
  if pid = 0 then
  begin
    // we are in child process -> switch to new executable
    if not waitfor then
      // don't share the same console
      CleanAfterFork;
    envpp := envp;
    if env <> '' then
    begin
      n := 0;
      result := -ESysE2BIG;
      if (roEnvAddExisting in options) and
         (envpp <> nil) then
      begin
        while envpp^ <> nil do
        begin
          if PosChar(envpp^, #10) = nil then
          begin
            // filter to add only single-line variables
            if n = high(e) - 1 then
              exit;
            e[n] := envpp^;
            inc(n);
          end;
          inc(envpp);
        end;
      end;
      P := pointer(env); // env follows Windows layout 'n1=v1'#0'n2=v2'#0#0
      while P^ <> #0 do
      begin
        if n = high(e) - 1 then
          exit;
        e[n] := P; // makes POSIX compatible
        inc(n);
        inc(P, StrLen(P) + 1);
      end;
      e[n] := nil; // end with null
      envpp := @e;
    end;
    FpExecve(args^, args, envpp);
    FpExit(127);
  end;
  if waitfor then
  begin
    result := WaitProcess(pid);
    if result = 127 then
      // execv() failed in child process
      result := -result;
  end
  else
    // fork success (don't wait for the child process to fail)
    result := 0;
end;

function RunProcess(const path, arg1: TFileName; waitfor: boolean;
  const arg2, arg3, arg4, arg5, env: TFileName;
  options: TRunOptions): integer;
var
  a: array[0..6] of PAnsiChar; // assume no UNICODE on POSIX, i.e. as TFileName
begin
  a[0] := pointer(path);
  a[1] := pointer(arg1);
  a[2] := pointer(arg2);
  a[3] := pointer(arg3);
  a[4] := pointer(arg4);
  a[5] := pointer(arg5);
  a[6] := nil; // end pointer list with null
  result := RunInternal(@a, waitfor, env, options);
end;

function RunCommand(const cmd: TFileName; waitfor: boolean;
  const env: TFileName; options: TRunOptions; parsed: PParseCommands): integer;
var
  temp: RawUtf8;
  err: TParseCommands;
  a: TParseCommandsArgs;
begin
  err := ParseCommandArgs(cmd, @a, nil, @temp);
  if parsed <> nil then
    parsed^ := err;
  if err = [] then
    // no need to spawn the shell for simple commands
    result := RunInternal(a, waitfor, env, options)
  else if err * PARSECOMMAND_ERROR <> [] then
    // no system call for clearly invalid command line
    result := -ESysEPERM
  else
  begin
    // execute complex commands via the shell
    a[0] := '/bin/sh';
    a[1] := '-c';
    a[2] := pointer(cmd);
    a[3] := nil;
    result := RunInternal(@a, waitfor, env, options);
  end;
end;

function RunRedirect(const cmd: TFileName; exitcode: PInteger;
  const onoutput: TOnRedirect; waitfordelayms: cardinal;
  setresult: boolean; const env, wrkdir: TFileName; options: TRunOptions): RawByteString;
var
  // notes: - FPC popen() allows access to the pid whereas clib popen() won't
  //        - env and options params are not supported by popen() so are ignored
  f: file;
  fd: THandle;
  pid, res, wr: cint;
  n, l: TSsize;
  wait: cardinal;
  endtix: Int64;
  tmp: array[word] of byte; // 64KB stack buffer

  function RedirectOutput(flush: boolean; var redir: RawByteString): boolean;
  var
    u: RawUtf8;
  begin
    result := false; // return false on pipe closed
    if WaitReadPending(fd, wait) then
    begin
      n := fpread(fd, tmp, SizeOf(tmp));
      if n < 0 then
        exit; // pipe closed = execution finished
      if setresult and
         (n <> 0) then
      begin
        if redir = '' then
          FastSetString(RawUtf8(redir), @tmp, n) // assume CP_UTF8
        else
        begin
          SetLength(redir, l + n); // append
          MoveFast(tmp, PByteArray(redir)[l], n);
        end;
        inc(l, n);
      end;
      if Assigned(onoutput) then
      begin
        FastSetString(u, @tmp, n); // console output is likely UTF-8 on POSIX
        if onoutput(u, pid) and
           not flush then
          endtix := 1; // returned true: force kill() on abort
      end;
    end
    else if Assigned(onoutput) and // idle
            onoutput('', pid) and
            not flush then
      endtix := 1; // returned true to abort -> kill()
    result := true;
  end;

begin
  result := '';
  if wrkdir <> '' then
    ChDir(wrkdir);
  if popen(f, cmd, 'r') <> 0 then // fork and launch cmd - env is ignored by now
    exit;
  fd := TFileRec(f).Handle;
  pid := pcint(@TFileRec(f).userdata[2])^; // see popen() from Unix.pp
  if Assigned(onoutput) then
    onoutput('', pid);
  wait := 200;
  if waitfordelayms = INFINITE then
    endtix := 0
  else
  begin
    endtix := GetTickCount64 + waitfordelayms;
    if waitfordelayms < wait then
      wait := waitfordelayms;
  end;
  l := 0;
  repeat
    if not RedirectOutput({flush=}false, result) then
      break; // pipe closed = execution finished
    if (endtix <> 0) and
       (GetTickCount64 > endtix) then
    begin
      // abort process execution after timeout or onoutput()=true
      if RunAbortTimeoutSecs > 0 then
      begin
        // try gracefull death
        if (ramSigTerm in RunAbortMethods) and
           (fpkill(pid, SIGTERM) = 0) then
        begin
          endtix := GetTickCount64 + RunAbortTimeoutSecs * 1000;
          repeat
            wr := FpWaitPid(pid, @res, WNOHANG);  // 0 = no state change
            RedirectOutput({flush=}true, result); // continue redirection
            if (wr <> 0) or
               (GetTickCount64 > endtix) then
              break;
            SleepHiRes(5);
          until false;
          if wr = pid then // <0 for error
            break; // gracefully ended
        end;
      end;
      // force process termination
      fpkill(pid, SIGKILL);
      pid := 0;
      break;
    end;
  until false;
  res := pclose(f);
  if exitcode <> nil then
    if pid = 0 then
      exitcode^ := -1
    else
      exitcode^ := res;
end;


{ ****************** Gather Operating System Information }

{$ifdef OSANDROID}

function GetSmbEfiMem: RawByteString;
begin
  result := '';
end;

const
  getpagesize = 4096;

{$else}
function getpagesize: integer;
  cdecl external clib name 'getpagesize';
{$endif OSANDROID}

{$ifdef OSLINUX}

function get_nprocs: integer;
  cdecl external clib name 'get_nprocs';

procedure SetLinuxDistrib(const release: RawUtf8);
var
  distrib: TOperatingSystem;
  rel, dist: RawUtf8;
begin
  rel := UpperCase(release);
  for distrib := osArch to high(distrib) do
  begin
    dist := UpperCase(OS_NAME[distrib]);
    if PosEx(dist, rel) > 0 then
    begin
      OS_KIND := distrib;
      break;
    end;
  end;
end;

function clock_gettime_c(clk_id: clockid_t; tp: ptimespec): cint;
begin
  // FPC only knows the regular clocks: convert to the *_FAST version
  case clk_id of
    // 1 ms resolution is good enough for milliseconds-based RTL functions
    CLOCK_REALTIME:
      clk_id := CLOCK_REALTIME_FAST;
    CLOCK_MONOTONIC:
      clk_id := CLOCK_MONOTONIC_FAST;
    // no CLOCK_MONOTONIC_RAW redirect because it doesn't match CLOCK_MONOTONIC
    // and cthreads.pp forces pthread_condattr_setclock(CLOCK_MONOTONIC_RAW)
  end;
  // it is much faster to not use the Linux syscall but the libc vDSO call
  result := clock_gettime(clk_id, tp);
end;

function gettimeofday_c(tp: ptimeval; tzp: ptimezone): cint;
begin
  // it is much faster to not use the Linux syscall but the libc vDSO call
  result := gettimeofday(tp, tzp);
end;

function GetSmbEfiMem: RawByteString;
var
  efi, addr: RawUtf8;
  xaddr: cardinal;
begin
  // retrieve raw EFI information from systab
  result := '';
  xaddr := 0;
  efi := StringFromFile('/sys/firmware/efi/systab', {nosize=}true);
  if efi = '' then
    efi := StringFromFile('/proc/efi/systab', {nosize=}true); // old Linux<2.6.6
  if efi = '' then
    exit;
  FindNameValue(efi, 'SMBIOS', addr);
  xaddr := PosixParseHex32(pointer(addr));
  if xaddr <> 0 then
    result := ReadSystemMemory(xaddr, 32); // 32 bytes is enough
end;

// on Android, /sys/class/net is not readable from the standard user :(
function _GetSystemMacAddress: TRawUtf8DynArray;
var
  SR: TSearchRec;
  fn: TFileName;
  f: RawUtf8;
begin
  result := nil;
  if FindFirst('/sys/class/net/*', faDirectory, SR) <> 0 then
    exit;
  repeat
    if (SR.Name <> 'lo') and
       not IdemPChar(pointer(SR.Name), 'DOCKER') and
       SearchRecValidFolder(SR) then
    begin
      fn := '/sys/class/net/' + SR.Name;
      f := StringFromFile(fn + '/flags', {nosize=}true);
      if (length(f) > 2) and // e.g. '0x40' or '0x1043'
         (PosixParseHex32(pointer(f)) and {IFF_LOOPBACK:}8 = 0) then
      begin
        f := TrimU(StringFromFile(fn + '/address', {nosize=}true));
        if f <> '' then
        begin
          SetLength(result, length(result) + 1);
          result[high(result)] := f;
        end;
      end;
    end;
  until FindNext(SR) <> 0;
  FindClose(SR);
end;

{$endif OSLINUX}

{$ifdef CPUARM3264} // POSIX libc is faster than FPC RTL or our pascal code

function libc_strlen(s: PAnsiChar): SizeInt;
  cdecl external clib name 'strlen';

function libc_memmove(dst, src: pointer; n: SizeInt): pointer;
  cdecl external clib name 'memmove';

function libc_memset(dst: pointer; c: integer; n: SizeInt): pointer;
  cdecl external clib name 'memset';

function StrLenLibc(s: PAnsiChar): SizeInt;
begin
  if s = nil then
    result := PtrUInt(s)
  else
    result := libc_strlen(s);
end;

procedure MoveFastLibC(const source; var dest; count: SizeInt);
begin
  if (@dest <> @source) and
     (count > 0) then
    libc_memmove(@dest, @source, count);
end;

procedure FillCharLibC(var dest; count: PtrInt; value: byte);
begin
  if (@dest <> nil) and
     (count > 0) then
    libc_memset(@dest, value, count);
end;

{$ifdef OSLINUXANDROID}

procedure RetrieveCpuInfoArm;
begin
  if CpuFeatures = [] then
  begin
    // fallback to /proc/cpuinfo "Features:" text
    if PosEx(' aes', CpuInfoFeatures) >= 0 then
      include(CpuFeatures, ahcAes);
    if PosEx(' pmull', CpuInfoFeatures) >= 0 then
      include(CpuFeatures, ahcPmull);
    if PosEx(' sha1', CpuInfoFeatures) >= 0 then
      include(CpuFeatures, ahcSha1);
    if PosEx(' sha2', CpuInfoFeatures) >= 0 then
      include(CpuFeatures, ahcSha2);
    if PosEx(' crc32', CpuInfoFeatures) >= 0 then
      include(CpuFeatures, ahcCrc32);
  end;
end;

{$endif OSLINUXANDROID}

{$endif CPUARM3264}

function Hex2Dec(c: AnsiChar): integer; inline;
begin
  result := ord(c);
  case c of
    '0'..'9':
      dec(result, ord('0'));
    'A'..'Z':
      dec(result, ord('A') - 10);
    'a'..'z':
      dec(result, ord('a') - 10);
  else
    result := -1;
  end;
end;

// this function is published in interface section for mormot.net.sock.posix.inc
function PosixParseHex32(p: PAnsiChar): integer;
var
  v0, v1: integer;
begin
  result := 0;
  p := StrScan(p, 'x');
  if p = nil then
    exit;
  repeat
    inc(p); // points to trailing 'x' at start
    v0 := Hex2Dec(p^);
    if v0 < 0 then
      break; // not in '0'..'9','a'..'f'
    inc(p);
    v1 := Hex2Dec(p^);
    if v1 < 0 then
    begin
      result := (result shl 4) or v0; // only one char left
      break;
    end;
    result := (result shl 8) or (v0 shl 4) or v1;
  until false;
end;

procedure ParseHex(p: PAnsiChar; b: PByte; n: integer);
var
  v0, v1: integer;
begin
  repeat // caller ensured p<>nil and b<>nil and n>0
    v0 := Hex2Dec(p^);
    if v0 < 0 then
      break; // not in '0'..'9','a'..'f'
    inc(p);
    v1 := Hex2Dec(p^);
    if v1 < 0 then
      break;
    inc(p);
    b^ := (v0 shl 4) or v1;
    inc(b);
    dec(n);
  until n = 0;
end;

procedure ParseHex32Add(p: PAnsiChar; var result: TIntegerDynArray);
var
  v: integer;
begin
  v := PosixParseHex32(p);
  if v <> 0 then
    AddInteger(result, v, {nodup=}true);
end;

function ParseLine(P: PUtf8Char): PUtf8Char;
begin
  if P <> nil then
    P := strscan(P, ':');
  result := P;
  if P = nil then
    exit;
  repeat
    inc(P);
  until (P^ = #0) or
        (P^ > ' ');
  result := P;
  while not (ord(P^) in [0, 10, 13]) do
  begin
    if P^ < ' ' then
      P^ := ' '; // change any tab into space
    inc(P);
  end;
  P^ := #0; // make asciiz
end;

function ParseInt(P: PUtf8Char): integer;
begin
  P := ParseLine(P);
  if (P <> nil) and
     (P^ in ['0'..'9']) then
    result := GetCardinal(P)
  else
    result := -1;
end;

{$ifdef CPUAARCH64}
{$ifdef OSLINUXANDROID}
// AARCH64 armv8.o is only validated on Linux
// (it should work on other POSIX ABI, but was reported to fail)

{$define ARMV8STATIC}
{$L ..\..\static\aarch64-linux\armv8.o} // ARMv8 crc32c Linux code

function crc32carm64(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal; external;
function crc32arm64(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal; external;
function crc32cby4arm64(crc, value: cardinal): cardinal; external;
procedure crc32blockarm64(crc128, data128: PBlock128); external;
procedure crc32blocksarm64(crc128, data128: PBlock128; count: integer); external;

{$endif OSLINUXANDROID}
{$endif CPUAARCH64}

procedure InitializeSpecificUnit;
var
  P: PAnsiChar;
  modname, beg: PUtf8Char;
  uts: UtsName;
  {$ifndef NODIRECTTHREADMANAGER}
  tm: TThreadManager;
  {$endif NODIRECTTHREADMANAGER}
  {$ifndef OSDARWIN}
  {$ifdef CPUARM3264}
  act, aci: TIntegerDynArray;
  i: PtrInt;
  {$endif CPUARM3264}
  tp: timespec;
  {$endif OSDARWIN}
  {$ifdef OSBSDDARWIN}
  temp1, temp2: ShortString;
  {$else}
  hw, cache, cpuinfo: PUtf8Char;
  proccpuinfo, release, prod: RawUtf8;
  procid, phyid, phyndx: integer;
  phy: TIntegerDynArray;
  {$ifdef OSLINUX}
  prodver, dist: RawUtf8;
  SR: TSearchRec;
  si: TSysInfo;  // Linuxism

  function GetSysFile(const fn: TFileName): RawUtf8;
  begin
    result := TrimU(StringFromFile(fn, true));
    if result = 'Default string' then // e.g. on ProxMox containers or VMs
      result := '';
  end;
  {$endif OSLINUX}
  {$endif OSBSDDARWIN}
begin
  // retrieve Kernel and Hardware information
  StdOutIsTTY := not IsDebuggerPresent and
                 (IsATTY(StdOutputHandle) = 1) and
                 IdemPChars(RawUtf8(GetEnvironmentVariable('TERM')), [
                   'XTERM', 'SCREEN', 'TMUX', 'RXVT', 'LINUX', 'CYGWIN']);
  modname := nil;
  fpuname(uts);
  {$ifdef OSBSDDARWIN}
  // pure FreeBSD NetBSD MacOS branch
  SystemInfo.dwNumberOfProcessors := fpsysctlhwint(HW_NCPU);
  beg := fpsysctlhwstr(HW_MACHINE, temp1);
  {$ifdef OSDARWIN}
  if strscan(beg, ' ') = nil then // e.g. from a Parallels VM
    beg := fpsysctlhwstr(HW_MODEL, temp1);
  modname := fpsysctlbynamehwstr('machdep.cpu.brand_string', temp2);
  {$endif OSDARWIN}
  FastSetString(BiosInfoText, beg, StrLen(beg));
  if modname = nil then
    modname := fpsysctlhwstr(HW_MODEL, temp2);
  with uts do
    OSVersionText := sysname + '-' + release + ' ' + version;
  {$ifdef OSDARWIN}
  // pure MACOS branch
  CpuCache[1].LineSize := fpsysctlhwint(HW_CACHELINE);
  CpuCache[1].Size     := fpsysctlhwint(HW_L1DCACHESIZE);
  CpuCache[2].LineSize := fpsysctlhwint(HW_CACHELINE);
  CpuCache[2].Size     := fpsysctlhwint(HW_L2CACHESIZE);
  CpuCache[3].LineSize := fpsysctlhwint(HW_CACHELINE);
  CpuCache[3].Size     := fpsysctlhwint(HW_L3CACHESIZE);
  CpuCacheSize := CpuCache[3].Size;
  if CpuCacheSize = 0 then
    CpuCacheSize := CpuCache[2].Size;
  if CpuCacheSize = 0 then
    CpuCacheSize := CpuCache[1].Size;
  if CpuCacheSize <> 0 then
    _fmt('L1=%s L2=%s L3=%s', [_oskb(CpuCache[1].Size),
      _oskb(CpuCache[2].Size), _oskb(CpuCache[3].Size)], CpuCacheText);
  SystemMemorySize := fpsysctlhwint(HW_MEMSIZE);
  {$else}
  SystemMemorySize := fpsysctlhwint(HW_PHYSMEM);
  {$endif OSDARWIN}
  {$else}
  {$ifdef OSANDROID}
  // pure ANDROID branch
  release := GetSystemProperty('ro.build.version.release');
  prod := TrimU(RawUtf8(GetSystemProperty('ro.product.brand') + ' ' +
                        GetSystemProperty('ro.product.name') + ' ' +
                        GetSystemProperty('ro.product.device')));
  {$else}
  // pure LINUX branch
  GetSystemMacAddress := _GetSystemMacAddress;
  if Sysinfo(@si) = 0 then
    SystemMemorySize := si.totalram * si.mem_unit;
  prod := TrimU(GetSysFile('/sys/class/dmi/id/sys_vendor') + ' ' +
                GetSysFile('/sys/class/dmi/id/product_name'));
  if prod <> '' then
  begin // e.g. 'QEMU KVM Virtual Machine' or 'LENOVO 20HES23B0U'
    prodver := GetSysFile('/sys/class/dmi/id/product_version');
    if prodver <> '' then
      prod := prod + ' ' + prodver;
  end
  else
    // return e.g. 'Raspberry Pi 3 Model B Rev 1.2'
    prod := GetSysFile('/proc/device-tree/model');
  FindNameValue(StringFromFile('/etc/os-release', true),
    'PRETTY_NAME=', release);
  if (release <> '') and
     (release[1] = '"') then
    release := copy(release, 2, length(release) - 2);
  TrimSelf(release);
  if release = '' then
  begin
    FindNameValue(StringFromFile('/etc/lsb-release', true),
      'DISTRIB_DESCRIPTION=', release);
    if (release <> '') and
       (release[1] = '"') then
      release := copy(release, 2, length(release) - 2);
  end;
  if (release = '') and
     (FindFirst('/etc/*-release', faAnyFile, SR) = 0) then
  begin
    release := SR.Name; // 'redhat-release' 'SuSE-release'
    if IdemPChar(pointer(release), 'LSB-') and
       (FindNext(SR) = 0) then
      release := SR.Name;
    release := split(release, '-');
    dist := split(StringFromFile('/etc/' + SR.Name, true), #10);
    if (dist <> '') and
       (PosExChar('=', dist) = 0) and
       (PosExChar(' ', dist) > 0) then
      // e.g. 'Red Hat Enterprise Linux Server release 6.7 (Santiago)'
      SetLinuxDistrib(dist)
    else
      dist := '';
    FindClose(SR);
  end;
  if (release <> '') and
     (OS_KIND = osLinux) then
  begin
    SetLinuxDistrib(release);
    if (OS_KIND = osLinux) and
       ({%H-}dist <> '') then
    begin
      SetLinuxDistrib(dist);
      release := dist;
    end;
    if (OS_KIND = osLinux) and
       ((PosEx('RH', release) > 0) or
        (PosEx('Red Hat', release) > 0)) then
      OS_KIND := osRedHat;
  end;
  {$endif OSANDROID}
  BiosInfoText := prod;
  hw := nil;
  cache := nil;
  SystemInfo.dwNumberOfProcessors := 0;
  proccpuinfo := StringFromFile('/proc/cpuinfo', true);
  procid := -1;
  cpuinfo := pointer(proccpuinfo);
  while cpuinfo <> nil do
  begin
    beg := cpuinfo;
    cpuinfo := GotoNextLine(cpuinfo);
    if IdemPChar(beg, 'PROCESSOR') then
      if beg^ = 'P' then
        modname := ParseLine(beg) // Processor : ARMv7
      else
      begin
        // loop over all "processor : 0 .. 1 .. 2" lines to count the CPUs
        inc(SystemInfo.dwNumberOfProcessors);
        procid := ParseInt(beg);
        if procid >= integer(SystemInfo.dwNumberOfProcessors) then
          procid := -1; // paranoid
      end
    else if IdemPChar(beg, 'MODEL NAME') then
      modname := ParseLine(beg)
    else if IdemPChar(beg, 'FEATURES') or
            IdemPChar(beg, 'FLAGS') then
      CpuInfoFeatures := ParseLine(beg)
    else if IdemPChar(beg, 'HARDWARE') then
      hw := ParseLine(beg)
    else if IdemPChar(beg, 'CACHE SIZE') then
      cache := ParseLine(beg)
    {$ifdef CPUARM3264}
    else if IdemPChar(beg, 'CPU IMPLEMENTER') then
      ParseHex32Add(beg, aci)
    else if IdemPChar(beg, 'CPU PART') then
      ParseHex32Add(beg, act)
    {$endif CPUARM3264}
    else if IdemPChar(beg, 'PHYSICAL ID') then
    begin
      phyid := ParseInt(beg); // in practice, may be 0,3,... and not 0,1,...
      if phyid < 0 then
        continue;
      phyndx := IntegerScanIndex(pointer(phy), CpuSockets, phyid);
      if phyndx < 0 then
      begin
        AddInteger(phy, CpuSockets, phyid);
        SetLength(CpuSocketsMask, CpuSockets);
        phyndx := CpuSockets - 1;
      end;
      if (procid >= 0) and
         (procid < SizeOf(TCpuSet) shl 3) then
        SetBitPtr(@CpuSocketsMask[phyndx], procid);
    end;
  end;
  {$ifdef CPUARM3264}
  if act <> nil then // CPU part/implementer are more detailed than model name
  begin
    proccpuinfo := '';
    for i := 0 to high(aci) do // there should be a single implementer
      proccpuinfo := proccpuinfo +
        ArmCpuImplementerName(ArmCpuImplementer(aci[i]), aci[i]) + ' ';
    proccpuinfo := proccpuinfo + ArmCpuTypeName(ArmCpuType(act[0]), act[0]);
    for i := 1 to high(act) do // but there may be several parts/models
      proccpuinfo := proccpuinfo + ' / ' + ArmCpuTypeName(ArmCpuType(act[i]), act[i]);
    modname := pointer(proccpuinfo);
  end;
  RetrieveCpuInfoArm;
  {$endif CPUARM3264}
  if hw <> nil then
  begin
    prod := hw;
    if BiosInfoText = '' then
      BiosInfoText := prod
    else
      BiosInfoText := BiosInfoText + ' (' + prod + ')';
  end else if BiosInfoText = '' then
    BiosInfoText := 'Generic ' + CPU_ARCH_TEXT + ' system'; // noname computer
  if cache <> nil then
  begin
    CpuCacheText := TrimU(cache);
    CpuCacheSize := GetNextCardinal(cache);
    while cache^ = ' ' do
      inc(cache);
    case upcase(cache^) of
      'K':
        CpuCacheSize := CpuCacheSize shl 10;
      'M':
        CpuCacheSize := CpuCacheSize shl 20;
      'G':
        CpuCacheSize := CpuCacheSize shl 30;
    end;
  end;
  SystemInfo.release := release;
  {$endif OSBSDDARWIN}
  SystemInfo.dwPageSize := getpagesize; // call libc API
  if CpuCacheSize <> 0 then
    _fmt('[%s]', [_oskb(CpuCacheSize)], CpuInfoText);
  if CpuSockets = 0 then
    CpuSockets := 1;
  SystemInfo.uts.release := uts.Release;
  SystemInfo.uts.sysname := uts.Sysname;
  SystemInfo.uts.version := uts.Version;
  P := @uts.release[0];
  KernelRevision := GetNextCardinal(P) shl 16 +
                    GetNextCardinal(P) shl 8 +
                    GetNextCardinal(P);
  OSVersion32.os := OS_KIND;
  MoveByOne(@KernelRevision, @OSVersion32.utsrelease, 3); // 24-bit
  with SystemInfo.uts do
    OSVersionText := sysname + ' ' + release;
  if SystemInfo.release <> '' then
    OSVersionText := SystemInfo.release + ' - ' + OSVersionText;
  {$ifdef OSANDROID}
  OSVersionText := 'Android (' + OSVersionText + ')';
  {$else}
  {$ifdef OSLINUX}
  if SystemInfo.dwNumberOfProcessors = 0 then // e.g. QEMU limited /proc/cpuinfo
    SystemInfo.dwNumberOfProcessors := get_nprocs;
  {$endif OSLINUX}
  {$endif OSANDROID}
  if SystemInfo.dwNumberOfProcessors = 0 then
    SystemInfo.dwNumberOfProcessors := 1;
  if modname = nil then
    CpuInfoText := _fmt('%d x generic ' + CPU_ARCH_TEXT + ' cpu %s',
      [SystemInfo.dwNumberOfProcessors, CpuInfoText])
  else
    CpuInfoText := _fmt('%d x %s %s (' + CPU_ARCH_TEXT + ')',
      [SystemInfo.dwNumberOfProcessors, modname, CpuInfoText]);
  // intialize supported APIs
  TimeZoneLocalBias := -GetLocalTimeOffset;
  {$ifndef NODIRECTTHREADMANAGER}
  // for inlined RTL calls (avoid one level of redirection)
  GetThreadManager(tm);
  @GetCurrentThreadId      := @tm.GetCurrentThreadId;
  @TryEnterCriticalSection := @tm.TryEnterCriticalSection;
  @EnterCriticalSection    := @tm.EnterCriticalSection;
  @LeaveCriticalSection    := @tm.LeaveCriticalSection;
  {$endif NODIRECTTHREADMANAGER}
  {$ifdef OSDARWIN}
  OSVersionText := ToText(OSVersion32) + ' (' + OSVersionText + ')';
  mach_timebase_info(mach_timeinfo);
  mach_timecoeff := mach_timeinfo.Numer / mach_timeinfo.Denom;
  mach_timenanosecond := (mach_timeinfo.Numer = 1) and
                         (mach_timeinfo.Denom = 1);
  {$else}
  // try Linux kernel 2.6.32+ or FreeBSD 8.1+ fastest clocks
  if clock_gettime(CLOCK_REALTIME_COARSE, @tp) = 0 then
    CLOCK_REALTIME_FAST := CLOCK_REALTIME_COARSE;
  if clock_gettime(CLOCK_MONOTONIC_COARSE, @tp) = 0 then
    CLOCK_MONOTONIC_FAST := CLOCK_MONOTONIC_COARSE;
  {$ifdef OSLINUX}
  if clock_gettime(CLOCK_MONOTONIC_RAW, @tp) = 0 then
    CLOCK_MONOTONIC_HIRES := CLOCK_MONOTONIC_RAW;
  {$endif OSLINUX}
  if clock_gettime(CLOCK_BOOTTIME, @tp) = 0 then
    CLOCK_UPTIME := CLOCK_BOOTTIME;
  if (clock_gettime(CLOCK_REALTIME_FAST, @tp) <> 0) or // paranoid check
     (clock_gettime(CLOCK_MONOTONIC_FAST, @tp) <> 0) or
     (clock_gettime(CLOCK_MONOTONIC_HIRES, @tp) <> 0) then
    raise EOSException.CreateFmt(
      'clock_gettime() not supported by %s kernel - errno=%d',
      [PAnsiChar(@uts.release), fpgeterrno]);
  // direct access to the pthread library if possible (Linux only)
  {$ifdef OSPTHREADSLIB}
  // mutex_lock() is blocking when dlopen run from a .so: cthreads uses both
  // static and dynamic linking, which is really confusing to our code
  // -> we don't open libpthread but we get its symbol
  pthread := dlopen('libpthread.so.0', RTLD_LAZY);
  if pthread <> nil then
  begin
    {$ifdef HAS_PTHREADSETNAMENP}
    @pthread_setname_np := dlsym(pthread, 'pthread_setname_np');
    {$endif HAS_PTHREADSETNAMENP}
    {$ifdef HAS_PTHREADSETAFFINITY}
    @pthread_setaffinity_np := dlsym(pthread, 'pthread_setaffinity_np');
    {$endif HAS_PTHREADSETAFFINITY}
    @pthread_cancel := dlsym(pthread, 'pthread_cancel');
    @pthread_mutex_init := dlsym(pthread, 'pthread_mutex_init');
    @pthread_mutex_destroy := dlsym(pthread, 'pthread_mutex_destroy');
    @pthread_mutex_trylock := dlsym(pthread, 'pthread_mutex_trylock');
    @pthread_mutex_lock    := dlsym(pthread, 'pthread_mutex_lock');
    @pthread_mutex_unlock  := dlsym(pthread, 'pthread_mutex_unlock');
  end;
  {$endif OSPTHREADSLIB}
  // some ARM/AARCH64 specific initialization
  {$ifdef CPUARM3264}
  StrLen := @StrLenLibc; // libc version is faster than plain pascal or RTL code
  MoveFast := @MoveFastLibC;
  FillCharFast := @FillCharLibC;
  {$ifdef ARMV8STATIC}
  if ahcCrc32 in CpuFeatures then
    try
      if (crc32cby4arm64(0, 1) = 3712330424) and
         (crc32carm64(0, @SystemInfo, SizeOf(SystemInfo)) =
           crc32cfast(0, @SystemInfo, SizeOf(SystemInfo))) then
      begin
        crc32c := @crc32carm64;
        DefaultHasher := @crc32carm64;
        InterningHasher := @crc32carm64;
        crc32cby4 := @crc32cby4arm64;
        crcblock := @crc32blockarm64;
        crcblocks := @crc32blocksarm64;
      end;
      if crc32arm64(0, @SystemInfo, SizeOf(SystemInfo)) =
          crc32fast(0, @SystemInfo, SizeOf(SystemInfo)) then
        crc32 := @crc32arm64;
    except
      exclude(CpuFeatures, ahcCrc32); // crc32 was actually not supported
    end;
  {$endif ARMV8STATIC}
  {$endif CPUARM3264}
  {$ifdef CPUX64}
  {$ifdef OSLINUX}
  {$ifndef NOPATCHRTL}
  // redirect some syscall FPC RTL functions to faster vDSO libc variant
  {$ifndef FPC_USE_LIBC}
  RedirectCode(@Linux.clock_gettime, @clock_gettime_c);
  // will avoid syscall e.g. for events timeout in cthreads.pp
  RedirectCode(@fpgettimeofday, @gettimeofday_c);
  {$endif FPC_USE_LIBC}
  {$endif NOPATCHRTL}
  {$endif OSLINUX}
  {$endif CPUX64}
  {$endif OSDARWIN}
end;

procedure FinalizeSpecificUnit;
begin
  {$ifdef OSPTHREADSLIB}
  if pthread <> nil then
    dlclose(pthread);
  {$endif OSPTHREADSLIB}
  {$ifdef OSLINUX} // systemd API is Linux-specific
  sd.Done;
  {$endif OSLINUX}
  icu.Done;
  SynDaemonInterceptLog := nil;
end;


