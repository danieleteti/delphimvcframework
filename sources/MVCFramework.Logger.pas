// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// *************************************************************************** }

unit MVCFramework.Logger;

{$I dmvcframework.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  MVCFramework.Commons,
  System.Diagnostics,
  LoggerPro,
  LoggerPro.FileAppender;

const
  LOGGERPRO_TAG = 'dmvcframework';

type
  TLogLevel = (levDebug = 0, levNormal = 1, levWarning = 2, levError = 3, levException = 4);

{$IF Defined(SYDNEYORBETTER)}
const
  PROFILER_LOG_TYPE: array [false..true] of TLogType = (TLogType.Info, TLogType.Warning);

type
  Profiler = record
  private
    fMessage: string;
    fStopWatch: TStopWatch;
    fIndent: string;
  public
    class operator Finalize(var Dest: Profiler);
    constructor Start(const Message: string); overload;
    constructor Start(const Message: string; const Params: array of TVarRec); overload;
    constructor Start(const Message: string; const Params: array of TVarRec; const TAG: String); overload;
    class var ProfileLogger: ILogWriter;
    class var LoggerTag: String;
    class var WarningThreshold: UInt32;
    class var LogsOnlyIfOverThreshold: Boolean;
    // Trace
    class procedure Trace(const Message: String; Proc: TProc; const WarningThreshold: UInt32); overload; static;
    class function Trace<T>(const Message: String; Func: TFunc<T>; const WarningThreshold: UInt32): T; overload; static;
  end;
{$ENDIF}

function LogLevelAsString(ALogLevel: TLogLevel): string;
procedure Log(AMessage: string); overload;
procedure Log(AObject: TObject); overload;

procedure LogD(AMessage: string); overload;
procedure LogD(AMessage: TObject); overload;

procedure LogI(AMessage: string); overload;
procedure LogI(AObject: TObject); overload;

procedure LogW(AMessage: string); overload;
procedure LogW(AObject: TObject); overload;

procedure LogE(AMessage: string);

procedure LogF(AMessage: string);

procedure Log(LogLevel: TLogLevel; const AMessage: string); overload;

procedure LogException(const E: Exception; const AMessage: String);

procedure LogEnterMethod(const AMethodName: string);
procedure LogExitMethod(const AMethodName: string);

// direct access to loggerpro logger
function Log: ILogWriter; overload;

procedure SetDefaultLogger(const aLogWriter: ILogWriter);
//procedure InitializeDefaultLogger;
function CreateLoggerWithDefaultConfiguration: ILogWriter;

{ @abstract(Use only inside DLL because dll unloading is not a safe place to shutdown threads, so call this before unload DLL)
  Use this also in ISAPI dll. Check the @code(loggerproisapisample.dll) sample
}
procedure ReleaseGlobalLogger;

procedure InitThreadVars;

var
  LogLevelLimit: TLogLevel = TLogLevel.levNormal;
  UseConsoleLogger: Boolean = True;

implementation

uses
  {$IF Defined(MSWINDOWS)}
  LoggerPro.ConsoleAppender,
  {$ELSE}
  {$IF Not Defined(MOBILE)}
  LoggerPro.SimpleConsoleAppender, //only for linux
  {$ENDIF}
  {$ENDIF}
  LoggerPro.Renderers,
  System.IOUtils,
  MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.DuckTyping;

{$IF Defined(SYDNEYORBETTER)}
threadvar
  gIndent: NativeUInt;
  gReqNr: NativeUInt;

{$ENDIF}

var
  gLock: TObject;
  gDefaultLogger: ILogWriter;
  gLevelsMap: array [TLogLevel.levDebug .. TLogLevel.levException] of LoggerPro.TLogType = (
    (
      TLogType.Debug
    ),
    (
      TLogType.Info
    ),
    (
      TLogType.Warning
    ),
    (
      TLogType.Error
    ),
    (
      TLogType.Error
    )
  );

function Log: ILogWriter;
begin
    if gDefaultLogger = nil then
    begin
      SetDefaultLogger(nil);
    end;

    Result := gDefaultLogger;
end;

function LogLevelAsString(ALogLevel: TLogLevel): string;
begin
    case ALogLevel of
      levNormal:
        Result := ''; // normal is '' because is more readable
      levWarning:
        Result := 'WARNING';
      levError:
        Result := 'ERROR';
      levException:
        Result := 'EXCEPTION';
    else
      Result := 'UNKNOWN';
    end;
end;

function ObjectToJSON(const AObject: TObject): String;
var
  lSer: TMVCJsonDataObjectsSerializer;
begin
    lSer := TMVCJsonDataObjectsSerializer.Create;
    try
      if TDuckTypedList.CanBeWrappedAsList(AObject) then
      begin
        Result := '[' + AObject.QualifiedClassName + '] ' + lSer.SerializeCollection(AObject);
      end
      else
      begin
        Result := '[' + AObject.QualifiedClassName + '] ' + lSer.SerializeObject(AObject);
      end;
    finally
      lSer.Free;
    end;
end;

procedure LogW(AMessage: string);
begin
    Log.Warn(AMessage, LOGGERPRO_TAG);
end;

procedure LogE(AMessage: string);
begin
    Log.Error(AMessage, LOGGERPRO_TAG);
end;

procedure LogF(AMessage: string);
begin
    Log.Fatal(AMessage, LOGGERPRO_TAG);
end;

procedure LogException(const E: Exception; const AMessage: String);
begin
    LogE(E.ClassName + ': ' + E.Message + ' - (Custom Message: ' + AMessage + ')');
end;

procedure LogEnterMethod(const AMethodName: string);
begin
    LogI('>> ' + AMethodName);
end;

procedure LogExitMethod(const AMethodName: string);
begin
    LogI('<< ' + AMethodName);
end;

procedure Log(LogLevel: TLogLevel; const AMessage: string);
begin
    case gLevelsMap[LogLevel] of
      TLogType.Debug:
        Log.Debug(AMessage, LOGGERPRO_TAG);
      TLogType.Info:
        Log.Info(AMessage, LOGGERPRO_TAG);
      TLogType.Warning:
        Log.Warn(AMessage, LOGGERPRO_TAG);
      TLogType.Error:
        Log.Error(AMessage, LOGGERPRO_TAG);
    else
      raise Exception.Create('Invalid LOG LEVEL! Original message was: ' + AMessage);
    end;

end;

procedure Log(AMessage: string); overload;
begin
    LogI(AMessage);
end;

procedure Log(AObject: TObject); overload;
begin
    Log(ObjectToJSON(AObject));
end;

procedure LogI(AMessage: string); overload;
begin
    Log.Info(AMessage, LOGGERPRO_TAG);
end;

procedure LogD(AMessage: string); overload;
begin
    Log.Debug(AMessage, LOGGERPRO_TAG);
end;

procedure LogD(AMessage: TObject); overload;
begin
    LogD(ObjectToJSON(AMessage));
end;

procedure LogI(AObject: TObject); overload;
begin
    LogI(ObjectToJSON(AObject));
end;

procedure LogW(AObject: TObject); overload;
begin
    LogW(ObjectToJSON(AObject));
end;

procedure InitializeDefaultLogger;
begin
  { This procedure must be called in a synchronized context
    (Normally only SetDefaultLogger should be the caller) }
  if not Assigned(gDefaultLogger) then
  begin
    gDefaultLogger := CreateLoggerWithDefaultconfiguration;
  end;
end;

procedure SetDefaultLogger(const aLogWriter: ILogWriter);
begin
  if gDefaultLogger = nil then
  begin
    TMonitor.Enter(gLock); // double check here
    try
      if gDefaultLogger = nil then
      begin
        if aLogWriter <> nil then
        begin
          gDefaultLogger := aLogWriter;
          Log.Info('Custom Logger initialized', LOGGERPRO_TAG);
        end
        else
        begin
          InitializeDefaultLogger;
        end;
      end;
    finally
      TMonitor.Exit(gLock);
    end;
  end;
end;


function CreateLoggerWithDefaultConfiguration: ILogWriter;
var
  lLogsFolder: String;
  lFileAppender, lConsoleAppender: ILogAppender;
  lAppenders: TArray<ILogAppender>;
begin
{$IF NOT DEFINED(MOBILE)}
  lLogsFolder := AppPath + 'logs';
{$ELSE}
  lLogsFolder := TPath.Combine(TPath.GetDocumentsPath, 'logs');
{$ENDIF}
  lFileAppender := TLoggerProFileAppender.Create(5, 10000, lLogsFolder);
  if IsConsole and UseConsoleLogger then
  begin
    {$IF Defined(MSWINDOWS)}
    lConsoleAppender := TLoggerProConsoleAppender.Create(TLogItemRendererNoTag.Create);
    {$ELSE}
    {$IF Not Defined(MOBILE)}
    lConsoleAppender := TLoggerProSimpleConsoleAppender.Create(TLogItemRendererNoTag.Create);
    {$ENDIF}
    {$ENDIF}
    lAppenders := [lFileAppender, lConsoleAppender];
  end
  else
  begin
    lAppenders := [lFileAppender];
  end;
  Result := BuildLogWriter(lAppenders);
end;

procedure ReleaseGlobalLogger;
begin
    if gDefaultLogger <> nil then
    begin
      TMonitor.Enter(gLock);
      try
        if gDefaultLogger <> nil then // double check
        begin
          gDefaultLogger := nil;
        end;
      finally
        TMonitor.Exit(gLock);
      end;
    end;
end;


{ ****************************************** }
{ *************** PROFILER ***************** }
{ ****************************************** }
{$IF Defined(SYDNEYORBETTER)}

constructor Profiler.Start(const Message: string; const Params: array of TVarRec);
begin
  Start(Message, Params, LoggerTag);
end;

constructor Profiler.Start(const Message: string; const Params: array of TVarRec; const TAG: String);
begin
  if Profiler.ProfileLogger = nil then
    Exit;
  fMessage := Format(Message, Params);
  fStopWatch := TStopWatch.StartNew;
  if not LogsOnlyIfOverThreshold then
  begin
    Inc(gReqNr);
    fIndent := StringOfChar(' ', gIndent);
    ProfileLogger.Info('[%s>>][%6d][%s]', [
      fIndent,
      gReqNr,
      fMessage], TAG);
    Inc(gIndent);
  end;
end;

class operator Profiler.Finalize(var Dest: Profiler);
begin
  if Profiler.ProfileLogger = nil then
    Exit;
  Dest.fStopWatch.Stop;
  if not LogsOnlyIfOverThreshold then
  begin
    ProfileLogger.Log(
      PROFILER_LOG_TYPE[Dest.fStopWatch.ElapsedMilliseconds >= WarningThreshold],
      '[%s<<][%6d][%s][ELAPSED: %s]',
      [
        Dest.fIndent,
        gReqNr,
        Dest.fMessage,
        Dest.fStopWatch.Elapsed.ToString
      ], LoggerTag);
    Dec(gIndent);
    Dec(gReqNr);
  end
  else
  begin
    if Dest.fStopWatch.ElapsedMilliseconds >= WarningThreshold then
    begin
      ProfileLogger.Log(
        PROFILER_LOG_TYPE[True],
        '[%s][ELAPSED: %s][THRESHOLD %d ms]',
        [
          Dest.fMessage,
          Dest.fStopWatch.Elapsed.ToString,
          WarningThreshold
        ], LoggerTag);
    end;
  end;
end;

constructor Profiler.Start(const Message: string);
begin
  Start(Message, []);
end;

class function Profiler.Trace<T>(const Message: String; Func: TFunc<T>; const WarningThreshold: UInt32): T;
var
  lStopWatch: TStopWatch;
begin
  lStopWatch := TStopWatch.StartNew;
  Result := Func(); //do not put try/except here. If exception raises the timing is a nonsense
  lStopWatch.Stop;
  if lStopWatch.ElapsedMilliseconds >= WarningThreshold then
  begin
    ProfileLogger.Log(
      PROFILER_LOG_TYPE[True],
      '[%s][ELAPSED: %s][TRACE][THRESHOLD %d ms]',
      [
        Message,
        lStopWatch.Elapsed.ToString,
        WarningThreshold
      ], LoggerTag);
  end;
end;

class procedure Profiler.Trace(const Message: String; Proc: TProc; const WarningThreshold: UInt32);
var
  lStopWatch: TStopWatch;
begin
  lStopWatch := TStopWatch.StartNew;
  Proc(); //do not put try/except here. If exception raises the timing is a nonsense
  lStopWatch.Stop;
  if lStopWatch.ElapsedMilliseconds >= WarningThreshold then
  begin
    ProfileLogger.Log(
      PROFILER_LOG_TYPE[True],
      '[%s][ELAPSED: %s][TRACE][THRESHOLD %d ms]',
      [
        Message,
        lStopWatch.Elapsed.ToString,
        WarningThreshold
      ], LoggerTag);
  end;
end;

{$ENDIF}

procedure InitThreadVars;
begin
{$IF Defined(SYDNEYORBETTER)}
  gIndent := 0;
  gReqNr := 0;
{$ENDIF}
end;

initialization

  gLock := TObject.Create;

{$IF Defined(SYDNEYORBETTER)}
  Profiler.LoggerTag := 'profiler';
  Profiler.WarningThreshold := 1000; //one sec
{$ENDIF}
  { The TLoggerProFileAppender has its defaults defined as follows:
    DEFAULT_LOG_FORMAT = '%0:s [TID %1:-8d][%2:-10s] %3:s [%4:s]';
    DEFAULT_MAX_BACKUP_FILE_COUNT = 5;
    DEFAULT_MAX_FILE_SIZE_KB = 1000;

    You can override these dafaults passing parameters to the constructor.
    Here's some configuration examples:
    @longcode(#
    // Creates log in the same exe folder without PID in the filename
    _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
    [TFileAppenderOption.LogsInTheSameFolder])]);

    // Creates log in the AppData/Roaming with PID in the filename
    _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
    [TFileAppenderOption.IncludePID])]);

    // Creates log in the same folder with PID in the filename
    _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
    [TFileAppenderOption.IncludePID])]);
    #)
  }

  // Creates log in the ..\..\ folder without PID in the filename

  // DefaultDMVCFrameworkLogger := BuildLogWriter([TLoggerProFileAppender.Create(10, 5)]);
  // Create logs in the exe' same folder
  // _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5)]);

  // Creates log in the AppData/Roaming with PID in the filename
  // _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
  // [TFileAppenderOption.IncludePID])]);

  // Creates log in the same folder with PID in the filename
  // _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
  // [TFileAppenderOption.IncludePID])]);


finalization

  gLock.Free;


end.
