/// wrapper for Windows functions translated to Linux for FPC
unit SynFPCLinux;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2019 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2019
  the Initial Developer. All Rights Reserved.

  Contributor(s):
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


  Version 1.18
  - initial revision

}

interface

{$I Synopse.inc} // set proper flags, and define LINUX for BSD and ANDROID

uses
  SysUtils
  {$ifdef Linux}
  ,UnixType
  {$endif};

const
  { HRESULT codes, delphi-like }
  NOERROR = 0;
  NO_ERROR = 0;
  INVALID_HANDLE_VALUE = THandle(-1);

  LOCALE_USER_DEFAULT = $400;
  NORM_IGNORECASE = 1;

/// compatibility function, wrapping Win32 API mutex initialization
procedure InitializeCriticalSection(var cs : TRTLCriticalSection); inline;

/// compatibility function, wrapping Win32 API mutex finalization
procedure DeleteCriticalSection(var cs : TRTLCriticalSection); inline;

{$ifdef LINUX}

{$ifdef LINUXNOTBSD}
const
  CLOCK_REALTIME = 0;
  CLOCK_MONOTONIC = 1;
  CLOCK_REALTIME_COARSE = 5; // see http://lwn.net/Articles/347811
  CLOCK_MONOTONIC_COARSE = 6;

var
  // contains CLOCK_REALTIME_COARSE since kernel 2.6.32
  CLOCK_REALTIME_TICKCOUNT: integer = CLOCK_REALTIME;
  // contains CLOCK_MONOTONIC_COARSE since kernel 2.6.32
  CLOCK_MONOTONIC_TICKCOUNT: integer = CLOCK_MONOTONIC;

{$endif LINUXNOTBSD}

/// used by TSynMonitorMemory.RetrieveMemoryInfo to compute the sizes in byte
function getpagesize: Integer; cdecl; external 'c';

/// compatibility function, wrapping Win32 API high resolution timer
procedure QueryPerformanceCounter(out Value: Int64); inline;

/// compatibility function, wrapping Win32 API high resolution timer
function QueryPerformanceFrequency(out Value: Int64): boolean; inline;

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
function CompareStringW(GetThreadLocale: DWORD; dwCmpFlags: DWORD; lpString1: Pwidechar;
  cchCount1: longint; lpString2: Pwidechar; cchCount2: longint): longint;

/// returns the current UTC time
function GetNowUTC: TDateTime;

/// returns the current UTC time, as Unix Epoch seconds
function GetUnixUTC: Int64; inline;

/// returns the current UTC time, as Unix Epoch milliseconds
// - will call clock_gettime(CLOCK_REALTIME_COARSE) if available
function GetUnixMSUTC: Int64; inline;

/// returns the current UTC time as TSystemTime
procedure GetNowUTCSystem(out result: TSystemTime);

var
  /// will contain the current Linux kernel revision, as one integer
  // - e.g. $030d02 for 3.13.2, or $020620 for 2.6.32
  KernelRevision: cardinal;

/// calls the pthread_setname_np() function, if available on this system
// - under Linux/FPC, this API truncates the name to 16 chars
procedure SetUnixThreadName(ThreadID: TThreadID; const Name: RawByteString);

{$endif LINUX}

/// compatibility function, to be implemented according to the running OS
// - expect more or less the same result as the homonymous Win32 API function
function GetTickCount64: Int64; inline;

/// compatibility function, to be implemented according to the running OS
// - expect more or less the same result as the homonymous Win32 API function
function GetTickCount: cardinal; inline;

/// similar to Windows sleep() API call, to be truly cross-platform
// - it should have a millisecond resolution, and handle ms=0 as a switch to
// another pending thread, i.e. call sched_yield() API
procedure SleepHiRes(ms: cardinal); inline;


implementation

{$ifdef LINUX}
uses
  Classes, Unix, BaseUnix, {$ifdef LINUXNOTBSD}linux,{$endif} dl;
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

procedure JulianToGregorian(JulianDN: PtrUInt; out result: TSystemTime);
var YYear,XYear,Temp,TempMonth: PtrUInt;
begin
  Temp := ((JulianDN-D2) shl 2)-1;
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

procedure EpochToLocal(epoch: PtrUInt; out result: TSystemTime);
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

function GetNowUTC: TDateTime;
var SystemTime: TSystemTime;
begin
  GetNowUTCSystem(SystemTime);
  result := SystemTimeToDateTime(SystemTime);
end;

procedure GetNowUTCSystem(out result: TSystemTime);
var tz: timeval;
begin
  fpgettimeofday(@tz,nil);
  EpochToLocal(tz.tv_sec,result);
  result.MilliSecond := tz.tv_usec div 1000;
end;

function GetTickCount: cardinal;
begin
  result := cardinal(GetTickCount64);
end;

const
  C_THOUSAND = Int64(1000);
  C_MILLION  = Int64(C_THOUSAND * C_THOUSAND);
  C_BILLION  = Int64(C_THOUSAND * C_THOUSAND * C_THOUSAND);

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

procedure QueryPerformanceCounter(var Value: Int64);
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

function GetTickCount64: Int64;
begin
  QueryPerformanceCounter(result);
  result := result div C_MILLION; // 1 millisecond = 1e6 nanoseconds
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
  result := (tz.tv_sec*1000)+tz.tv_usec div 1000;
end;

{$else}

{$ifdef BSD}
function clock_gettime(ID: cardinal; r: ptimespec): Integer;
  cdecl external 'libc.so' name 'clock_gettime';
function clock_getres(ID: cardinal; r: ptimespec): Integer;
  cdecl external 'libc.so' name 'clock_getres';
const
  CLOCK_REALTIME = 0;
  CLOCK_MONOTONIC = 4;
  CLOCK_MONOTONIC_FAST = 12; // FreeBSD specific
  CLOCK_MONOTONIC_TICKCOUNT = CLOCK_MONOTONIC;
  CLOCK_REALTIME_TICKCOUNT = CLOCK_REALTIME;
{$endif BSD}

function GetTickCount64: Int64;
var tp: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC_TICKCOUNT,@tp);
  Result := (Int64(tp.tv_sec) * C_THOUSAND) + (tp.tv_nsec div 1000000); // in ms
end;

function GetUnixMSUTC: Int64;
var r: timespec;
begin
  clock_gettime(CLOCK_REALTIME_TICKCOUNT,@r);
  result := (Int64(r.tv_sec) * C_THOUSAND) + (r.tv_nsec div 1000000); // in ms
end;

function GetUnixUTC: Int64;
var r: timespec;
begin
  clock_gettime(CLOCK_REALTIME_TICKCOUNT,@r);
  result := r.tv_sec;
end;

procedure QueryPerformanceCounter(out Value: Int64);
var r : TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC,@r);
  value := r.tv_nsec+r.tv_sec*C_BILLION;
end;

{$endif DARWIN}

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
var W1,W2: WideString;
begin // not inlined to avoid stack unicodestring allocation
  W1 := lpString1;
  W2 := lpString2;
  if dwCmpFlags and NORM_IGNORECASE<>0 then
    result := WideCompareText(W1,W2) else
    result := WideCompareStr(W1,W2);
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
begin
  SysUtils.Sleep(ms);
end;

procedure GetKernelRevision;
var uts: UtsName;
    P: PAnsiChar;
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
    {$ifdef LINUXNOTBSD}
    if KernelRevision>=$020620 then begin // expects kernel 2.6.32 or higher
      CLOCK_MONOTONIC_TICKCOUNT := CLOCK_MONOTONIC_COARSE;
      CLOCK_REALTIME_TICKCOUNT := CLOCK_REALTIME_COARSE;
    end;
    {$endif LINUXNOTBSD}
  end;
  {$ifdef DARWIN}
  mach_timebase_info(mach_timeinfo);
  mach_timecoeff := mach_timeinfo.Numer/mach_timeinfo.Denom;
  {$endif DARWIN}
end;


type
  TExternalLibraries = object
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
  if Loaded then
    exit;
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

procedure TExternalLibraries.Done;
begin
  if not Loaded then
    exit;
  {$ifdef LINUX}
  if pthread <> nil then
    dlclose(pthread);
  {$endif LINUX}
  Loaded := false;
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

finalization
  ExternalLibraries.Done;
{$endif LINUX}
end.
