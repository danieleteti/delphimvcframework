/// wrapper of some Windows-like functions translated to Linux/BSD for FPC
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynFPCLinux;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2022 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Alfred Glaenzer.

  Portions created by the Initial Developer are Copyright (C) 2022
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Alan Chate
  - Arnaud Bouchez


  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. if you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. if you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

}

interface


{$ifndef FPC}
  'this unit is for FPC only - do not include it in any Delphi project!'
{$endif FPC}


{$I Synopse.inc} // set proper flags, and define LINUX for BSD and ANDROID

uses
  {$ifdef LINUX}
  BaseUnix,
  UnixType,
  {$endif LINUX}
  SysUtils;

const
  { HRESULT codes, delphi-like }
  NOERROR = 0;
  NO_ERROR = 0;
  INVALID_HANDLE_VALUE = THandle(-1);

  LOCALE_USER_DEFAULT = $400;

  // for CompareStringW()
  NORM_IGNORECASE = 1 shl ord(coIgnoreCase); // [widestringmanager.coIgnoreCase]

/// compatibility function, wrapping Win32 API mutex initialization
procedure InitializeCriticalSection(var cs : TRTLCriticalSection); inline;

/// compatibility function, wrapping Win32 API mutex finalization
procedure DeleteCriticalSection(var cs : TRTLCriticalSection); inline;

{$ifdef LINUX}

/// used by SynCommons to compute the sizes in byte
function getpagesize: Integer; cdecl; external 'c';

/// compatibility function, wrapping Win32 API high resolution timer
// - returns nanoseconds resolution, calling e.g. CLOCK_MONOTONIC on Linux/BSD
procedure QueryPerformanceCounter(out Value: Int64);

/// slightly faster than QueryPerformanceCounter() div 1000 - but not for Windows
// - returns microseconds resolution, calling e.g. CLOCK_MONOTONIC on Linux/BSD
procedure QueryPerformanceMicroSeconds(out Value: Int64); inline;

/// compatibility function, wrapping Win32 API high resolution timer
// - hardcoded to 1e9 for clock_gettime() nanoseconds resolution on Linux/BSD
function QueryPerformanceFrequency(out Value: Int64): boolean;

/// compatibility function, wrapping Win32 API file position change
function SetFilePointer(hFile: cInt; lDistanceToMove: TOff;
  lpDistanceToMoveHigh: Pointer; dwMoveMethod: cint): TOff; inline;

/// compatibility function, wrapping Win32 API file size retrieval
function GetFileSize(hFile: cInt; lpFileSizeHigh: PDWORD): DWORD;

/// compatibility function, wrapping Win32 API file truncate at current position
procedure SetEndOfFile(hFile: cInt); inline;

/// compatibility function, wrapping Win32 API file flush to disk
procedure FlushFileBuffers(hFile: cInt); inline;

/// compatibility function, wrapping Win32 API last error code
function GetLastError: longint; inline;

/// compatibility function, wrapping Win32 API last error code
procedure SetLastError(error: longint); inline;

/// compatibility function, wrapping Win32 API text comparison
// - will use the system ICU library if available, or the widestringmanager
// - seldom called, unless our proprietary WIN32CASE collation is used in SynSQLite3
function CompareStringW(GetThreadLocale: DWORD; dwCmpFlags: DWORD; lpString1: PWideChar;
  cchCount1: integer; lpString2: PWideChar; cchCount2: integer): integer;

/// compatibility function, wrapping Win32 API text case conversion
function CharUpperBuffW(W: PWideChar; WLen: integer): integer;

/// compatibility function, wrapping Win32 API text case conversion
function CharLowerBuffW(W: PWideChar; WLen: integer): integer;

/// compatibility function, wrapping Win32 MultiByteToWideChar API conversion
// - will use the system ICU library for efficient conversion
function AnsiToWideICU(codepage: cardinal; Source: PAnsiChar; Dest: PWideChar;
  SourceChars: PtrInt): PtrInt;

/// compatibility function, wrapping Win32 WideCharToMultiByte API conversion
// - will use the system ICU library for efficient conversion
function WideToAnsiICU(codepage: cardinal; Source: PWideChar; Dest: PAnsiChar;
  SourceChars: PtrInt): PtrInt;

/// returns the current UTC time
// - will convert from clock_gettime(CLOCK_REALTIME_COARSE) if available
function GetNowUTC: TDateTime;

/// returns the current UTC time, as Unix Epoch seconds
// - will call clock_gettime(CLOCK_REALTIME_COARSE) if available
function GetUnixUTC: Int64;

/// returns the current UTC time, as Unix Epoch milliseconds
// - will call clock_gettime(CLOCK_REALTIME_COARSE) if available
function GetUnixMSUTC: Int64;

/// returns the current UTC time as TSystemTime
// - will convert from clock_gettime(CLOCK_REALTIME_COARSE) if available
procedure GetNowUTCSystem(out result: TSystemTime);

var
  /// will contain the current Linux kernel revision, as one 24-bit integer
  // - e.g. $030d02 for 3.13.2, or $020620 for 2.6.32
  KernelRevision: cardinal;

/// calls the pthread_setname_np() function, if available on this system
// - under Linux/FPC, this API truncates the name to 16 chars
procedure SetUnixThreadName(ThreadID: TThreadID; const Name: RawByteString);

/// calls mprotect() syscall or clib
function SynMProtect(addr:pointer; size:size_t; prot:integer): longint;

{$ifdef BSD}
function fpsysctlhwint(hwid: cint): Int64;
function fpsysctlhwstr(hwid: cint; var temp: shortstring): pointer;
{$endif BSD}

{$ifndef DARWIN} // OSX has no clock_gettime() API

{$ifdef BSD}
const // see https://github.com/freebsd/freebsd/blob/master/sys/sys/time.h
  CLOCK_REALTIME = 0;
{$ifdef OpenBSD}
  CLOCK_MONOTONIC = 3;
  CLOCK_REALTIME_COARSE = CLOCK_REALTIME; // no faster alternative
  CLOCK_MONOTONIC_COARSE = CLOCK_MONOTONIC;
{$else}
  CLOCK_MONOTONIC = 4;
  CLOCK_REALTIME_COARSE = 10; // named CLOCK_REALTIME_FAST in FreeBSD 8.1+
  CLOCK_MONOTONIC_COARSE = 12;
{$endif OPENBSD}

{$else}
const
  CLOCK_REALTIME = 0;
  CLOCK_MONOTONIC = 1;
  CLOCK_REALTIME_COARSE = 5; // see http://lwn.net/Articles/347811
  CLOCK_MONOTONIC_COARSE = 6;
{$endif BSD}

var
  // contains CLOCK_REALTIME_COARSE since kernel 2.6.32
  CLOCK_REALTIME_FAST: integer = CLOCK_REALTIME;
  // contains CLOCK_MONOTONIC_COARSE since kernel 2.6.32
  CLOCK_MONOTONIC_FAST: integer = CLOCK_MONOTONIC;

{$endif DARWIN}
{$endif LINUX}

/// compatibility function, to be implemented according to the running OS
// - expect more or less the same result as the homonymous Win32 API function
// - will call clock_gettime(CLOCK_MONOTONIC_COARSE) if available
function GetTickCount64: Int64;

/// compatibility function, to be implemented according to the running OS
// - expect more or less the same result as the homonymous Win32 API function
// - will call clock_gettime(CLOCK_MONOTONIC_COARSE) if available
function GetTickCount: cardinal;

var
  /// could be set to TRUE to force SleepHiRes(0) to call the sched_yield API
  // - in practice, it has been reported as buggy under POSIX systems
  // - even Linus Torvald himself raged against its usage - see e.g.
  // https://www.realworldtech.com/forum/?threadid=189711&curpostid=189752
  // - you may tempt the devil and try it by yourself
  SleepHiRes0Yield: boolean = false;

/// similar to Windows sleep() API call, to be truly cross-platform
// - using millisecond resolution
// - SleepHiRes(0) calls ThreadSwitch on windows, but this POSIX version will
// wait 10 microsecond unless SleepHiRes0Yield is forced to true (bad idea)
// - in respect to RTL's Sleep() function, it will return on ESysEINTR
procedure SleepHiRes(ms: cardinal);

/// check if any char is pending from StdInputHandle file descriptor
function UnixKeyPending: boolean;


{$ifdef LINUX}

type
  /// the libraries supported by TExternalLibrariesAPI
  TExternalLibrary = (
    elPThread, elICU {$ifdef LINUXNOTBSD} , elSystemD {$endif});
  /// set of libraries supported by TExternalLibrariesAPI
  TExternalLibraries = set of TExternalLibrary;

  /// implements late-binding of system libraries
  // - about systemd: see https://www.freedesktop.org/wiki/Software/systemd
  // and http://0pointer.de/blog/projects/socket-activation.html - to get headers
  // on debian: `sudo apt install libsystemd-dev && cd /usr/include/systemd`
  TExternalLibrariesAPI = object
  private
    Lock: TRTLCriticalSection;
    Loaded: TExternalLibraries;
    {$ifdef LINUX}
    pthread: pointer;
    {$ifdef LINUXNOTBSD}
    systemd: pointer;
    {$endif LINUXNOTBSD}
    {$endif LINUX}
    icu, icudata, icui18n: pointer;
    procedure LoadIcuWithVersion;
    procedure Done;
  public
    {$ifdef LINUXNOTBSD}
    /// customize the name of a thread (truncated to 16 bytes)
    // - see https://stackoverflow.com/a/7989973
    pthread_setname_np: function(thread: pointer; name: PAnsiChar): longint; cdecl;
    /// systemd: returns how many file descriptors have been passed to process
    // - if result=1 then socket for accepting connection is SD_LISTEN_FDS_START
    sd_listen_fds: function(unset_environment: integer): integer; cdecl;
    /// systemd: returns 1 if the file descriptor is an AF_UNIX socket of the specified type and path
    sd_is_socket_unix: function(fd, typr, listening: integer;
      var path: TFileName; pathLength: PtrUInt): integer; cdecl;
    /// systemd: submit simple, plain text log entries to the system journal
    // - priority value can be obtained using longint(LOG_TO_SYSLOG[logLevel])
    // - WARNING: args strings processed using C printf semantic, so % is a printf
    // placeholder and should be either escaped using %% or all formatting args must be passed
    sd_journal_print: function(priority: longint; args: array of const): longint; cdecl;
    /// systemd: submit array of iov structures instead of the format string to the system journal.
    //  - each structure should reference one field of the entry to submit.
    //  - the second argument specifies the number of structures in the array.
    sd_journal_sendv: function(const iov: Piovec; n: longint): longint; cdecl;
    /// systemd: sends notification to systemd
    // - see https://www.freedesktop.org/software/systemd/man/sd_notify.html
    // status notification sample: sd.notify(0, 'READY=1');
    // watchdog notification: sd.notify(0, 'WATCHDOG=1');
    sd_notify: function(unset_environment: longint; state: PAnsiChar): longint; cdecl;
    /// systemd: check whether the service manager expects watchdog keep-alive
    // notifications from a service
    // - if result > 0 then usec contains the notification interval (app should
    // notify every usec/2)
    sd_watchdog_enabled: function(unset_environment: longint; usec: Puint64): longint; cdecl;
    {$endif LINUXNOTBSD}
    /// Initialize an ICU text converter for a given encoding
    ucnv_open: function (converterName: PAnsiChar; var err: SizeInt): pointer; cdecl;
    /// finalize the ICU text converter for a given encoding
    ucnv_close: procedure (converter: pointer); cdecl;
    /// customize the ICU text converter substitute char
    ucnv_setSubstChars: procedure (converter: pointer;
      subChars: PAnsiChar; len: byte; var err: SizeInt); cdecl;
    /// enable the ICU text converter fallback
    ucnv_setFallback: procedure (cnv: pointer; usesFallback: LongBool); cdecl;
    /// ICU text conversion from UTF-16 to a given encoding
    ucnv_fromUChars: function (cnv: pointer; dest: PAnsiChar; destCapacity: cardinal;
      src: PWideChar; srcLength: cardinal; var err: SizeInt): cardinal; cdecl;
    /// ICU text conversion from a given encoding to UTF-16
    ucnv_toUChars: function (cnv: pointer; dest: PWideChar; destCapacity: cardinal;
      src: PAnsiChar; srcLength: cardinal; var err: SizeInt): cardinal; cdecl;
    /// ICU UTF-16 text conversion to uppercase
    u_strToUpper: function (dest: PWideChar; destCapacity: cardinal;
      src: PWideChar; srcLength: cardinal; locale: PAnsiChar;
      var err: SizeInt): cardinal; cdecl;
    /// ICU UTF-16 text conversion to lowercase
    u_strToLower: function (dest: PWideChar; destCapacity: cardinal;
      src: PWideChar; srcLength: cardinal; locale: PAnsiChar;
      var err: SizeInt): cardinal; cdecl;
    /// ICU UTF-16 text comparison
    u_strCompare: function (s1: PWideChar; length1: cardinal;
      s2: PWideChar; length2: cardinal; codePointOrder: LongBool): cardinal; cdecl;
    /// ICU UTF-16 text comparison with options, e.g. for case-insensitive
    u_strCaseCompare: function (s1: PWideChar; length1: cardinal;
      s2: PWideChar; length2: cardinal; options: cardinal;
      var err: SizeInt): cardinal; cdecl;
    /// get the ICU data folder
    u_getDataDirectory: function: PAnsiChar; cdecl;
    /// set the ICU data folder
    u_setDataDirectory: procedure(directory: PAnsiChar); cdecl;
    /// initialize the ICU library
    u_init: procedure(var status: SizeInt); cdecl;
    /// Initialize an ICU text converter for a given codepage
    // - returns nil if ICU is not available on this system
    function ucnv(codepage: cardinal): pointer;
    /// thread-safe loading of a system library
    // - caller should then check the API function to be not nil
    procedure EnsureLoaded(lib: TExternalLibrary);
  end;

var
  /// late-binding of system libraries
  ExternalLibraries: TExternalLibrariesAPI;

{$ifdef LINUXNOTBSD} { the systemd API is Linux-specific }

const
  /// The first passed file descriptor is fd 3
  SD_LISTEN_FDS_START = 3;

  /// low-level libcurl library file name, depending on the running OS
  LIBSYSTEMD_PATH = 'libsystemd.so.0';

  ENV_INVOCATION_ID: PAnsiChar = 'INVOCATION_ID';

type
  /// low-level exception raised during systemd library access
  ESystemd = class(Exception);

/// returns true in case process is started by systemd
// - For systemd v232+
function ProcessIsStartedBySystemd: boolean;

/// initialize the libsystemd API
// - do nothing if the library has already been loaded
// - will raise ESsytemd exception on any loading issue
procedure LibSystemdInitialize;

/// returns TRUE if a systemd library is available
// - will load and initialize it, calling LibSystemdInitialize if necessary,
// catching any exception during the process
function SystemdIsAvailable: boolean; inline;

{$endif LINUXNOTBSD}

{$endif LINUX}


implementation

{$ifdef LINUX}
uses
  Classes,
  Unix,
  {$ifdef BSD}
  sysctl,
  {$else}
  Linux,
  SysCall,
  {$endif BSD}
  dl;
{$endif LINUX}

procedure InitializeCriticalSection(var cs : TRTLCriticalSection);
begin
  InitCriticalSection(cs);
end;

procedure DeleteCriticalSection(var cs : TRTLCriticalSection);
begin
  {$ifdef LINUXNOTBSD}
  if cs.__m_kind<>0 then
  {$else}
  {$ifdef BSD}
  {$ifdef Darwin}
  if cs.sig<>0 then
  {$else}
  if Assigned(cs) then
  {$endif Darwin}
  {$endif BSD}
  {$endif LINUXNOTBSD}
    DoneCriticalSection(cs);
end;

function UnixKeyPending: boolean;
var
  fdsin: tfdSet;
begin
  fpFD_ZERO(fdsin);
  fpFD_SET(StdInputHandle,fdsin);
  result := fpSelect(StdInputHandle+1,@fdsin,nil,nil,0)>0;
end;

{$ifdef LINUX}

const // Date Translation - see http://en.wikipedia.org/wiki/Julian_day
  HoursPerDay = 24;
  MinsPerHour = 60;
  SecsPerMin  = 60;
  MinsPerDay  = HoursPerDay*MinsPerHour;
  SecsPerDay  = MinsPerDay*SecsPerMin;
  SecsPerHour = MinsPerHour*SecsPerMin;
  C1970       = 2440588;
  D0          = 1461;
  D1          = 146097;
  D2          = 1721119;
  UnixDelta   = 25569;

  C_THOUSAND = Int64(1000);
  C_MILLION  = Int64(C_THOUSAND * C_THOUSAND);
  C_BILLION  = Int64(C_THOUSAND * C_THOUSAND * C_THOUSAND);

procedure JulianToGregorian(JulianDN: PtrUInt; out result: TSystemTime);
  {$ifdef HASINLINE}inline;{$endif}
var YYear,XYear,Temp,TempMonth: PtrUInt;
begin
  Temp := ((JulianDN-D2)*4)-1;
  JulianDN := Temp div D1;
  XYear := (Temp-(JulianDN*D1)) or 3;
  YYear := XYear div D0;
  Temp := (((XYear-(YYear*D0)+4) shr 2)*5)-3;
  TempMonth := Temp div 153;
  result.Day := ((Temp-(TempMonth*153))+5) div 5;
  if TempMonth>=10 then begin
    inc(YYear);
    dec(TempMonth,12-3);
  end else
    inc(TempMonth,3);
  result.Month := TempMonth;
  result.Year := YYear+(JulianDN*100);
  // initialize fake dayOfWeek - as used by SynCommons.FromGlobalTime RCU128
  result.DayOfWeek := 0;
end;

procedure EpochToSystemTime(epoch: PtrUInt; out result: TSystemTime);
var t: PtrUInt;
begin
  t := epoch div SecsPerDay;
  JulianToGregorian(t+C1970,result);
  dec(epoch,t*SecsPerDay);
  t := epoch div SecsPerHour;
  result.Hour := t;
  dec(epoch,t*SecsPerHour);
  t := epoch div SecsPerMin;
  result.Minute := t;
  result.Second := epoch-t*SecsPerMin;
end;

function GetTickCount: cardinal;
begin
  result := cardinal(GetTickCount64);
end;

{$ifdef DARWIN}
// clock_gettime() is not implemented: http://stackoverflow.com/a/5167506

type
  TTimebaseInfoData = record
    Numer: cardinal;
    Denom: cardinal;
  end;

function mach_absolute_time: UInt64;
  cdecl external 'libc.dylib' name 'mach_absolute_time';
function mach_timebase_info(var TimebaseInfoData: TTimebaseInfoData): Integer;
  cdecl external 'libc.dylib' name 'mach_timebase_info';

var
  mach_timeinfo: TTimebaseInfoData;
  mach_timecoeff: double;
  mach_timenanosecond: boolean; // very likely to be TRUE on Intel CPUs

procedure QueryPerformanceCounter(out Value: Int64);
begin // returns time in nano second resolution
  Value := mach_absolute_time;
  if mach_timeinfo.Denom=1 then
    if mach_timeinfo.Numer=1 then
      // seems to be the case on Intel CPUs
      exit else
      Value := Value*mach_timeinfo.Numer else
    // use floating point to avoid potential overflow
    Value := round(Value*mach_timecoeff);
end;

procedure QueryPerformanceMicroSeconds(out Value: Int64);
begin
  if mach_timenanosecond then
    Value := mach_absolute_time div C_THOUSAND else begin
    QueryPerformanceCounter(Value);
    Value := Value div C_THOUSAND; // ns to us
  end;
end;

function GetTickCount64: Int64;
begin
  if mach_timenanosecond then
    result := mach_absolute_time else
    QueryPerformanceCounter(result);
  result := result div C_MILLION; // ns to ms
end;

function GetUnixUTC: Int64;
var tz: timeval;
begin
  fpgettimeofday(@tz,nil);
  result := tz.tv_sec;
end;

function GetUnixMSUTC: Int64;
var tz: timeval;
begin
  fpgettimeofday(@tz,nil);
  result := (tz.tv_sec*C_THOUSAND)+tz.tv_usec div C_THOUSAND; // in milliseconds
end;

procedure GetNowUTCSystem(out result: TSystemTime);
var tz: timeval;
begin
  fpgettimeofday(@tz,nil);
  EpochToSystemTime(tz.tv_sec,result);
  result.MilliSecond := tz.tv_usec div C_THOUSAND;
end;

{$else}

{$ifdef BSD}

function clock_gettime(ID: cardinal; r: ptimespec): Integer;
  cdecl external 'libc.so' name 'clock_gettime';
function clock_getres(ID: cardinal; r: ptimespec): Integer;
  cdecl external 'libc.so' name 'clock_getres';

{$else}

// libc's clock_gettime function uses vDSO (avoid syscall) while FPC by default
// is compiled without FPC_USE_LIBC defined and do a syscall each time
//   GetTickCount64 fpc    2 494 563 op/sec
//   GetTickCount64 libc 119 919 893 op/sec
// note: for high-resolution QueryPerformanceMicroSeconds, calling the kernel
// is also slower
function clock_gettime(clk_id : clockid_t; tp: ptimespec) : cint;
  cdecl; external 'c' name 'clock_gettime';

{$endif BSD}

function GetTickCount64: Int64;
var tp: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC_FAST,@tp); // likely = CLOCK_MONOTONIC_COARSE
  Result := (Int64(tp.tv_sec) * C_THOUSAND) + (tp.tv_nsec div C_MILLION); // in ms
end;

function GetUnixMSUTC: Int64;
var r: timespec;
begin
  clock_gettime(CLOCK_REALTIME_FAST,@r); // likely = CLOCK_REALTIME_COARSE
  result := (Int64(r.tv_sec) * C_THOUSAND) + (r.tv_nsec div C_MILLION); // in ms
end;

function GetUnixUTC: Int64;
var r: timespec;
begin
  clock_gettime(CLOCK_REALTIME_FAST,@r);
  result := r.tv_sec;
end;

procedure QueryPerformanceCounter(out Value: Int64);
var r : TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC, @r);
  value := r.tv_nsec+r.tv_sec*C_BILLION; // returns nanoseconds resolution
end;

procedure QueryPerformanceMicroSeconds(out Value: Int64);
var r : TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC, @r);
  value := PtrUInt(r.tv_nsec) div C_THOUSAND+r.tv_sec*C_MILLION; // as microseconds
end;

procedure GetNowUTCSystem(out result: TSystemTime);
var r: timespec;
begin
  clock_gettime(CLOCK_REALTIME_FAST,@r); // faster than fpgettimeofday()
  EpochToSystemTime(r.tv_sec,result);
  result.MilliSecond := r.tv_nsec div C_MILLION;
end;

{$endif DARWIN}

{$ifdef BSD}
function fpsysctlhwint(hwid: cint): Int64;
var mib: array[0..1] of cint;
    len: cint;
begin
  result := 0;
  mib[0] := CTL_HW;
  mib[1] := hwid;
  len := SizeOf(result);
  fpsysctl(pointer(@mib),2,@result,@len,nil,0);
end;

function fpsysctlhwstr(hwid: cint; var temp: shortstring): pointer;
var mib: array[0..1] of cint;
    len: cint;
begin
  mib[0] := CTL_HW;
  mib[1] := hwid;
  FillChar(temp,SizeOf(temp),0); // use shortstring as temp 0-terminated buffer
  len := SizeOf(temp);
  fpsysctl(pointer(@mib),2,@temp,@len,nil,0);
  if temp[0]<>#0 then
    result := @temp else
    result := nil;
end;
{$endif BSD}

function GetNowUTC: TDateTime;
begin
  result := GetUnixMSUTC / MSecsPerDay + UnixDelta;
end;

function QueryPerformanceFrequency(out Value: Int64): boolean;
begin
  Value := C_BILLION; // 1 second = 1e9 nanoseconds
  result := true;
end;

function SetFilePointer(hFile: cInt; lDistanceToMove: TOff;
  lpDistanceToMoveHigh: Pointer; dwMoveMethod: cint): TOff;
var offs: Int64;
begin
  Int64Rec(offs).Lo := lDistanceToMove;
  if lpDistanceToMoveHigh=nil then
    Int64Rec(offs).Hi := 0 else
    Int64Rec(offs).Hi := PDWord(lpDistanceToMoveHigh)^;
  offs := FpLseek(hFile,offs,dwMoveMethod);
  result := Int64Rec(offs).Lo;
  if lpDistanceToMoveHigh<>nil then
    PDWord(lpDistanceToMoveHigh)^ := Int64Rec(offs).Hi;
end;

procedure SetEndOfFile(hFile: cInt);
begin
  FpFtruncate(hFile,FPLseek(hFile,0,SEEK_CUR));
end;

procedure FlushFileBuffers(hFile: cInt);
begin
  FpFsync(hFile);
end;

function GetLastError: longint;
begin
  result := fpgeterrno;
end;

procedure SetLastError(error: longint);
begin
  fpseterrno(error);
end;

function CompareStringRTL(a, b: PWideChar; al, bl, flags: integer): integer;
var
  U1, U2: UnicodeString;
begin
  SetString(U1,a,al);
  SetString(U2,b,bl);
  result := widestringmanager.CompareUnicodeStringProc(U1,U2,TCompareOptions(flags));
end;

function CompareStringW(GetThreadLocale: DWORD; dwCmpFlags: DWORD; lpString1: PWideChar;
  cchCount1: integer; lpString2: PWideChar; cchCount2: integer): integer;
const
  U_COMPARE_CODE_POINT_ORDER = $8000;
var
  err: SizeInt;
begin
  if cchCount1 < 0 then
    cchCount1 := StrLen(lpString1);
  if cchCount2 < 0 then
    cchCount2 := StrLen(lpString2);
  with ExternalLibraries do
  begin
    if not (elICU in Loaded) then
      EnsureLoaded(elICU);
    if Assigned(ucnv_open) then
    begin
      err := 0;
      if dwCmpFlags and NORM_IGNORECASE <> 0 then
        result := u_strCaseCompare(lpString1, cchCount1, lpString2, cchCount2,
          U_COMPARE_CODE_POINT_ORDER, err)
      else
        result := u_strCompare(lpString1, cchCount1, lpString2, cchCount2, true);
    end
    else
      result := CompareStringRTL(lpString1, lpString2, cchCount1, cchCount2, dwCmpFlags);
  end;
  inc(result, 2); // caller would make -2 to get regular -1/0/1 comparison values
end;

function CharUpperBuffW(W: PWideChar; WLen: integer): integer;
var
  err: SizeInt;
begin
  with ExternalLibraries do
  begin
    if not (elICU in Loaded) then
      EnsureLoaded(elICU);
    if Assigned(ucnv_open) then
    begin
      err := 0;
      result := u_strToUpper(W, WLen, W, WLen, nil, err);
    end
    else
      result := WLen;
  end;
end;

function CharLowerBuffW(W: PWideChar; WLen: integer): integer;
var
  err: SizeInt;
begin
  with ExternalLibraries do
  begin
    if not (elICU in Loaded) then
      EnsureLoaded(elICU);
    if Assigned(ucnv_open) then
    begin
      err := 0;
      result := u_strToLower(W, WLen, W, WLen, nil, err);
    end
    else
      result := WLen;
  end;
end;

function AnsiToWideRTL(codepage: cardinal; Source: PAnsiChar; Dest: PWideChar;
  SourceChars: PtrInt): PtrInt;
var
  tmp: UnicodeString;
begin
  widestringmanager.Ansi2UnicodeMoveProc(Source, codepage, tmp, SourceChars);
  result := length(tmp);
  Move(pointer(tmp)^, Dest^, result * 2);
end;

function AnsiToWideICU(codepage: cardinal; Source: PAnsiChar; Dest: PWideChar;
  SourceChars: PtrInt): PtrInt;
var
  cnv: pointer;
  err: SizeInt;
begin
  if codepage = CP_UTF8 then
    exit(Utf8ToUnicode(Dest, Source, SourceChars));
  cnv := ExternalLibraries.ucnv(codepage);
  if cnv = nil then
    exit(AnsiToWideRTL(codepage, Source, Dest, SourceChars));
  err := 0;
  result := ExternalLibraries.ucnv_toUChars(
    cnv, Dest, SourceChars, Source, SourceChars, err);
  if result < 0 then
    result := 0;
  ExternalLibraries.ucnv_close(cnv);
end;

function WideToAnsiRTL(codepage: cardinal; Source: PWideChar; Dest: PAnsiChar;
  SourceChars: PtrInt): PtrInt;
var
  tmp: RawByteString;
begin
  widestringmanager.Unicode2AnsiMoveProc(Source, tmp, codepage, SourceChars);
  result := length(tmp);
  Move(pointer(tmp)^, Dest^, result);
end;

function WideToAnsiICU(codepage: cardinal; Source: PWideChar; Dest: PAnsiChar;
  SourceChars: PtrInt): PtrInt;
var
  cnv: pointer;
  err: SizeInt;
begin
  if codepage = CP_UTF8 then
    // fallback to RTL
    exit(UnicodeToUTF8(Dest, Source, SourceChars));
  cnv := ExternalLibraries.ucnv(codepage);
  if cnv = nil then
    exit(WideToAnsiRTL(codepage, Source, Dest, SourceChars));
  err := 0;
  result := ExternalLibraries.ucnv_fromUChars(
    cnv, Dest, SourceChars * 3, Source, SourceChars, err);
  if result < 0 then
    result := 0;
  ExternalLibraries.ucnv_close(cnv);
end;


function GetFileSize(hFile: cInt; lpFileSizeHigh: PDWORD): DWORD;
var FileInfo: TStat;
begin
  if fpFstat(hFile,FileInfo)<>0 then
    FileInfo.st_Size := 0; // returns 0 on error
  result := Int64Rec(FileInfo.st_Size).Lo;
  if lpFileSizeHigh<>nil then
    lpFileSizeHigh^ := Int64Rec(FileInfo.st_Size).Hi;
end;

procedure SleepHiRes(ms: cardinal);
var timeout: TTimespec;
begin
  if ms=0 then // handle SleepHiRes(0) special case
    if SleepHiRes0Yield then begin // reported as buggy by Alan on POSIX
      ThreadSwitch; // call e.g. pthread's sched_yield API
      exit;
    end else begin
      timeout.tv_sec := 0;
      timeout.tv_nsec := 10000; // 10us is around timer resolution on modern HW
    end else begin
    timeout.tv_sec := ms div 1000;
    timeout.tv_nsec := 1000000*(ms mod 1000);
  end;
  fpnanosleep(@timeout,nil)
  // no retry loop on ESysEINTR (as with regular RTL's Sleep)
end;

{$ifdef BSD}
function mprotect(Addr: Pointer; Len: size_t; Prot: Integer): Integer;
  {$ifdef Darwin} cdecl external 'libc.dylib' name 'mprotect';
  {$else} cdecl external 'libc.so' name 'mprotect'; {$endif}
{$endif BSD}

function SynMProtect(addr: pointer; size: size_t; prot: integer): longint;
begin
  result := -1;
  {$ifdef UNIX}
    {$ifdef BSD}
    result := mprotect(addr, size, prot);
    {$else}
    if Do_SysCall(syscall_nr_mprotect, PtrUInt(addr), size, prot) >= 0 then
      result := 0;
    {$endif BSD}
  {$endif UNIX}
end;

procedure GetKernelRevision;
var uts: UtsName;
    P: PAnsiChar;
    tp: timespec;
  function GetNext: cardinal;
  var c: cardinal;
  begin
    result := 0;
    repeat
      c := ord(P^)-48;
      if c>9 then
        break else
        result := result*10+c;
      inc(P);
    until false;
    if P^ in ['.','-',' '] then
      inc(P);
  end;
begin
  if fpuname(uts)=0 then begin
    P := @uts.release[0];
    KernelRevision := GetNext shl 16+GetNext shl 8+GetNext;
  end else
    uts.release[0] := #0;
  {$ifdef DARWIN}
  mach_timebase_info(mach_timeinfo);
  mach_timecoeff := mach_timeinfo.Numer/mach_timeinfo.Denom;
  mach_timenanosecond := (mach_timeinfo.Numer=1) and (mach_timeinfo.Denom=1);
  {$else}
  {$ifdef LINUX}
  // try Linux kernel 2.6.32+ or FreeBSD 8.1+ fastest clocks
  if (CLOCK_REALTIME_COARSE <> CLOCK_REALTIME_FAST) and
     (clock_gettime(CLOCK_REALTIME_COARSE, @tp) = 0) then
    CLOCK_REALTIME_FAST := CLOCK_REALTIME_COARSE;
  if (CLOCK_MONOTONIC_COARSE <> CLOCK_MONOTONIC_FAST) and
     (clock_gettime(CLOCK_MONOTONIC_COARSE, @tp) = 0) then
    CLOCK_MONOTONIC_FAST := CLOCK_MONOTONIC_COARSE;
  if (clock_gettime(CLOCK_REALTIME_FAST,@tp)<>0) or // paranoid check
     (clock_gettime(CLOCK_MONOTONIC_FAST,@tp)<>0) then
    raise Exception.CreateFmt('clock_gettime() not supported by %s kernel - errno=%d',
      [PAnsiChar(@uts.release),GetLastError]);
  {$endif LINUX}
  {$endif DARWIN}
end;


{ TExternalLibrariesAPI }

procedure TExternalLibrariesAPI.LoadIcuWithVersion;
const
  NAMES: array[0..12] of string = (
    'ucnv_open', 'ucnv_close', 'ucnv_setSubstChars', 'ucnv_setFallback',
    'ucnv_fromUChars', 'ucnv_toUChars', 'u_strToUpper', 'u_strToLower',
    'u_strCompare', 'u_strCaseCompare', 'u_getDataDirectory',
    'u_setDataDirectory', 'u_init');
{$ifdef ANDROID}
// from https://developer.android.com/guide/topics/resources/internationalization
  ICU_VER: array[1..13] of string = (
    '_3_8', '_4_2', '_44', '_46', '_48', '_50', '_51', '_53', '_55', '_56', '_58', '_60', '_63');
  SYSDATA: PAnsiChar = '/system/usr/icu';
{$else}
  SYSDATA: PAnsiChar = '';
{$endif ANDROID}
var
  i, j: integer;
  err: SizeInt;
  P: PPointer;
  v, vers: string;
  data: PAnsiChar;
begin
  {$ifdef ANDROID}
  for i := high(ICU_VER) downto 1 do
  begin
    if dlsym(icu, pointer(NAMES[0] + ICU_VER[i])) <> nil then
    begin
      vers := ICU_VER[i];
      break;
    end;
  end;
  if vers <> '' then
  {$endif ANDROID}
  if dlsym(icu, 'ucnv_open') = nil then
    for i := 80 downto 44 do
    begin
      str(i, v);
      if dlsym(icu, pointer('ucnv_open_' + v)) <> nil then
      begin
        vers := '_' + v;
        break;
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
  if (data = nil) or (data^ = #0) then
    if SYSDATA <> '' then
      u_setDataDirectory(SYSDATA);
  err := 0;
  u_init(err);
end;

function TExternalLibrariesAPI.ucnv(codepage: cardinal): pointer;
var
  s: shortstring;
  err: SizeInt;
  {$ifdef CPUINTEL}
  mask: cardinal;
  {$endif CPUINTEL}
begin
  if not (elICU in Loaded) then
    EnsureLoaded(elICU);
  if not Assigned(ucnv_open) then
    exit(nil);
  str(codepage, s);
  Move(s[1], s[3], ord(s[0]));
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

procedure TExternalLibrariesAPI.EnsureLoaded(lib: TExternalLibrary);
var
  p: PPointer;
  i, j: integer;
const
  NAMES: array[0..5] of PAnsiChar = (
    'sd_listen_fds', 'sd_is_socket_unix', 'sd_journal_print', 'sd_journal_sendv',
    'sd_notify', 'sd_watchdog_enabled');
begin
  if lib in Loaded then
    exit;
  EnterCriticalSection(Lock);
  if not (lib in Loaded) then
  case lib of
    elPThread:
      begin
        {$ifdef LINUX}
        pthread := dlopen({$ifdef ANDROID}'libc.so'{$else}'libpthread.so.0'{$endif}, RTLD_LAZY);
        if pthread <> nil then
        begin
          {$ifdef LINUXNOTBSD}
          @pthread_setname_np := dlsym(pthread, 'pthread_setname_np');
          {$endif LINUXNOTBSD}
        end;
        {$endif LINUX}
        include(Loaded, elPThread);
      end;
    elICU:
      begin
        {$ifdef DARWIN}
        icu := dlopen('libicuuc.dylib', RTLD_LAZY);
        if icu <> nil then
          icui18n := dlopen('libicui18n.dylib', RTLD_LAZY);
        {$else}
        // libicudata should be loaded first because other two depend on it
        icudata := dlopen('libicudata.so', RTLD_LAZY);
        if icudata <> nil then
        begin
          icu := dlopen('libicuuc.so', RTLD_LAZY);
          if icu <> nil then
            icui18n := dlopen('libicui18n.so', RTLD_LAZY);
        end;
        {$endif DARWIN}
        if icui18n = nil then
        begin
          if icu <> nil then
            dlclose(icu);
          if icudata <> nil then
            dlclose(icudata);
        end
        else
          // ICU append a version prefix to all its functions e.g. ucnv_open_66
          LoadIcuWithVersion;
        include(Loaded, elICU);
      end;
  {$ifdef LINUXNOTBSD}
    elSystemD:
      begin
        systemd := dlopen(LIBSYSTEMD_PATH, RTLD_LAZY);
        if systemd <> nil then
        begin
          p := @@sd_listen_fds;
          for i := 0 to high(NAMES) do
          begin
            p^ := dlsym(systemd, NAMES[i]);
            if p^ = nil then
            begin
              p := @@sd_listen_fds;
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
        include(Loaded, elSystemD);
      end;
  {$endif LINUXNOTBSD}
  end;
  LeaveCriticalSection(Lock);
end;

procedure TExternalLibrariesAPI.Done;
begin
  EnterCriticalSection(Lock);
  if elPThread in Loaded then
  begin
    {$ifdef LINUX}
    {$ifdef LINUXNOTBSD}
    @pthread_setname_np := nil;
    {$endif LINUXNOTBSD}
    if pthread <> nil then
      dlclose(pthread);
    {$endif LINUX}
  end;
  if elICU in Loaded then
  begin
    if icui18n <> nil then
      dlclose(icui18n);
    if icu <> nil then
      dlclose(icu);
    if icudata <> nil then
      dlclose(icudata);
    @ucnv_open := nil;
  end;
  {$ifdef LINUXNOTBSD}
  if (elSystemD in Loaded) and (systemd <> nil) then
    dlclose(systemd);
  {$endif LINUXNOTBSD}
  LeaveCriticalSection(Lock);
  DeleteCriticalSection(Lock);
end;

procedure SetUnixThreadName(ThreadID: TThreadID; const Name: RawByteString);
var trunc: array[0..15] of AnsiChar; // truncated to 16 bytes (including #0)
    i,L: integer;
begin
  {$ifdef LINUXNOTBSD}
  if not(elPThread in ExternalLibraries.Loaded) then
    ExternalLibraries.EnsureLoaded(elPThread);
  if not Assigned(ExternalLibraries.pthread_setname_np) then
    exit;
  if Name = '' then
    exit;
  L := 0; // trim unrelevant spaces and prefixes when filling the 16 chars 
  i := 1;
  if Name[1] = 'T' then
    if PCardinal(Name)^ = ord('T') + ord('S') shl 8 + ord('Q') shl 16 + ord('L') shl 24 then
      i := 5
    else
      i := 2;
  while i <= length(Name) do begin
    if Name[i]>' ' then begin
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
  ExternalLibraries.pthread_setname_np(pointer(ThreadID), @trunc[0]);
  {$endif LINUXNOTBSD}
end;


{$ifdef LINUXNOTBSD}

function SystemdIsAvailable: boolean;
begin
  if not(elSystemD in ExternalLibraries.Loaded) then
    ExternalLibraries.EnsureLoaded(elSystemD);
  result := Assigned(ExternalLibraries.sd_listen_fds);
end;

function ProcessIsStartedBySystemd: boolean;
begin
  result := SystemdIsAvailable and
    // note: for example on Ubuntu 20.04 INVOCATION_ID is always defined
    // from the other side PPID 1 can be set if we run under docker and started
    // by init.d so let's verify both
    (fpgetppid() = 1) and (fpGetenv(ENV_INVOCATION_ID) <> nil);
end;

procedure LibSystemdInitialize;
begin
  if not SystemdIsAvailable then
    raise ESystemd.Create('Impossible to load ' + LIBSYSTEMD_PATH);
end;

{$endif LINUXNOTBSD}


initialization
  GetKernelRevision;
  InitializeCriticalSection(ExternalLibraries.Lock);

finalization
  ExternalLibraries.Done;
{$endif LINUX}
end.
