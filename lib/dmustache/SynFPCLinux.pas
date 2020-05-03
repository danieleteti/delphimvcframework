/// wrapper of some Windows-like functions translated to Linux/BSD for FPC
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynFPCLinux;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2020 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2020
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

{$I Synopse.inc} // set proper flags, and define LINUX for BSD and ANDROID

uses
  {$ifdef LINUX}
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
// - somewhat slow by using two temporary UnicodeString - but seldom called,
// unless our proprietary WIN32CASE collation is used in SynSQLite3
function CompareStringW(GetThreadLocale: DWORD; dwCmpFlags: DWORD; lpString1: Pwidechar;
  cchCount1: longint; lpString2: Pwidechar; cchCount2: longint): longint;

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

{$ifdef BSD}
function fpsysctlhwint(hwid: cint): Int64;
function fpsysctlhwstr(hwid: cint; var temp: shortstring): pointer;
{$endif BSD}

{$ifndef DARWIN} // OSX has no clock_gettime() API

{$ifdef BSD}
const // see https://github.com/freebsd/freebsd/blob/master/sys/sys/time.h
  CLOCK_REALTIME = 0;
  CLOCK_MONOTONIC = 4;
  CLOCK_REALTIME_COARSE = 10; // named CLOCK_REALTIME_FAST in FreeBSD 8.1+
  CLOCK_MONOTONIC_COARSE = 12;
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


implementation

{$ifdef LINUX}
uses
  Classes,
  Unix,
  BaseUnix,
  {$ifdef BSD}
  sysctl,
  {$else}
  Linux,
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
  clock_gettime(CLOCK_MONOTONIC,@r);
  value := r.tv_nsec+r.tv_sec*C_BILLION; // returns nanoseconds resolution
end;

procedure QueryPerformanceMicroSeconds(out Value: Int64);
var r : TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC,@r);
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

function CompareStringW(GetThreadLocale: DWORD; dwCmpFlags: DWORD; lpString1: Pwidechar;
  cchCount1: longint; lpString2: Pwidechar; cchCount2: longint): longint;
var U1,U2: UnicodeString; // (may be?) faster than WideString
begin // not inlined to avoid try..finally UnicodeString protection
  if cchCount1<0 then
    cchCount1 := StrLen(lpString1);
  SetString(U1,lpString1,cchCount1);
  if cchCount2<0 then
    cchCount2 := StrLen(lpString2);
  SetString(U2,lpString2,cchCount2);
  result := widestringmanager.CompareUnicodeStringProc(U1,U2,TCompareOptions(dwCmpFlags));
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
  if clock_gettime(CLOCK_REALTIME_COARSE, @tp) = 0 then
    CLOCK_REALTIME_FAST := CLOCK_REALTIME_COARSE;
  if clock_gettime(CLOCK_MONOTONIC_COARSE, @tp) = 0 then
    CLOCK_MONOTONIC_FAST := CLOCK_MONOTONIC_COARSE;
  if (clock_gettime(CLOCK_REALTIME_FAST,@tp)<>0) or // paranoid check
     (clock_gettime(CLOCK_MONOTONIC_FAST,@tp)<>0) then
    raise Exception.CreateFmt('clock_gettime() not supported by %s kernel - errno=%d',
      [PAnsiChar(@uts.release),GetLastError]);
  {$endif LINUX}
  {$endif DARWIN}
end;


type
  TExternalLibraries = object
    Lock: TRTLCriticalSection;
    Loaded: boolean;
    {$ifdef LINUX}
    pthread: pointer;
    {$ifdef LINUXNOTBSD} // see https://stackoverflow.com/a/7989973
    pthread_setname_np: function(thread: pointer; name: PAnsiChar): LongInt; cdecl;
    {$endif LINUXNOTBSD}
    {$endif LINUX}
    procedure EnsureLoaded;
    procedure Done;
  end;
var
  ExternalLibraries: TExternalLibraries;

procedure TExternalLibraries.EnsureLoaded;
begin
  EnterCriticalSection(Lock);
  if not Loaded then begin
    {$ifdef LINUX}
    pthread := dlopen({$ifdef ANDROID}'libc.so'{$else}'libpthread.so.0'{$endif}, RTLD_LAZY);
    if pthread <> nil then begin
      {$ifdef LINUXNOTBSD}
      @pthread_setname_np := dlsym(pthread, 'pthread_setname_np');
      {$endif LINUXNOTBSD}
    end;
    {$endif LINUX}
    Loaded := true;
  end;
  LeaveCriticalSection(Lock);
end;

procedure TExternalLibraries.Done;
begin
  EnterCriticalSection(Lock);
  if Loaded then begin
    {$ifdef LINUX}
    {$ifdef LINUXNOTBSD}
    @pthread_setname_np := nil;
    {$endif LINUXNOTBSD}
    if pthread <> nil then
      dlclose(pthread);
    {$endif LINUX}
  end;
  LeaveCriticalSection(Lock);
  DeleteCriticalSection(Lock);
end;

procedure SetUnixThreadName(ThreadID: TThreadID; const Name: RawByteString);
var trunc: array[0..15] of AnsiChar; // truncated to 16 bytes (including #0)
    i,L: integer;
begin
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
  {$ifdef LINUXNOTBSD}
  ExternalLibraries.EnsureLoaded;
  if Assigned(ExternalLibraries.pthread_setname_np) then
    ExternalLibraries.pthread_setname_np(pointer(ThreadID), @trunc[0]);
  {$endif LINUXNOTBSD}
end;

initialization
  GetKernelRevision;
  InitializeCriticalSection(ExternalLibraries.Lock);

finalization
  ExternalLibraries.Done;
{$endif LINUX}
end.
