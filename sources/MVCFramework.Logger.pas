// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
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
  LoggerPro.Builder,
  LoggerPro.FileAppender;

const
  LOGGERPRO_TAG = 'dmvcframework';

type
  TLogLevel = (levDebug = 0, levNormal = 1, levWarning = 2, levError = 3, levException = 4, levFatal = 5);

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

function LogLevelToStr(ALogLevel: TLogLevel): string;
function StrToLogLevel(ALogLevelString: String): TLogLevel;

procedure Log(AMessage: string); overload;
procedure Log(AObject: TObject); overload;

procedure LogD(AMessage: string); overload;
procedure LogD(AMessage: TObject); overload;
procedure LogD(AMessage: string; const AContext: array of LogParam); overload;
procedure LogD(const AMessage: string; const Args: array of const); overload;
procedure LogD(const AMessage: string; const ATag: string); overload;
procedure LogD(const AMessage: string; const Args: array of const; const ATag: string); overload;
procedure LogD(const AMessage: string; AObject: TObject); overload;

procedure LogI(AMessage: string); overload;
procedure LogI(AObject: TObject); overload;
procedure LogI(AMessage: string; const AContext: array of LogParam); overload;
procedure LogI(const AMessage: string; const Args: array of const); overload;
procedure LogI(const AMessage: string; const ATag: string); overload;
procedure LogI(const AMessage: string; const Args: array of const; const ATag: string); overload;
procedure LogI(const AMessage: string; AObject: TObject); overload;

procedure LogW(AMessage: string); overload;
procedure LogW(AObject: TObject); overload;
procedure LogW(AMessage: string; const AContext: array of LogParam); overload;
procedure LogW(const AMessage: string; const Args: array of const); overload;
procedure LogW(const AMessage: string; const ATag: string); overload;
procedure LogW(const AMessage: string; const Args: array of const; const ATag: string); overload;
procedure LogW(const AMessage: string; AObject: TObject); overload;

procedure LogE(AMessage: string); overload;
procedure LogE(AMessage: string; const AContext: array of LogParam); overload;
procedure LogE(const AMessage: string; const Args: array of const); overload;
procedure LogE(const AMessage: string; const ATag: string); overload;
procedure LogE(const AMessage: string; const Args: array of const; const ATag: string); overload;
procedure LogE(const AMessage: string; AObject: TObject); overload;
procedure LogE(const E: Exception); overload;
procedure LogE(const E: Exception; const AMessage: string); overload;

procedure LogF(AMessage: string); overload;
procedure LogF(AMessage: string; const AContext: array of LogParam); overload;
procedure LogF(const AMessage: string; const Args: array of const); overload;
procedure LogF(const AMessage: string; const ATag: string); overload;
procedure LogF(const AMessage: string; const Args: array of const; const ATag: string); overload;
procedure LogF(const AMessage: string; AObject: TObject); overload;

procedure Log(LogLevel: TLogLevel; const AMessage: string); overload;

procedure LogException(const E: Exception); overload;
procedure LogException(const E: Exception; const AMessage: String); overload;

procedure LogEnterMethod(const AMethodName: string);
procedure LogExitMethod(const AMethodName: string);

// Log level check helpers - useful to avoid expensive message construction
function IsDebugEnabled: Boolean; inline;
function IsInfoEnabled: Boolean; inline;
function IsWarningEnabled: Boolean; inline;
function IsErrorEnabled: Boolean; inline;

// direct access to loggerpro logger
function Log: ILogWriter; overload;

procedure SetDefaultLogger(const aLogWriter: ILogWriter);
//procedure InitializeDefaultLogger;
function CreateLoggerWithDefaultConfiguration: ILogWriter;
function CreateLogBuilderWithDefaultConfiguration: ILoggerProBuilder;
function CreateNullLogger: ILogWriter;

{ @abstract(Use only inside DLL because dll unloading is not a safe place to shutdown threads, so call this before unload DLL)
  Use this also in ISAPI dll. Check the @code(loggerproisapisample.dll) sample
}
procedure ReleaseGlobalLogger;

procedure InitThreadVars;

var
  UseConsoleLogger: Boolean = True;
  UseLoggerVerbosityLevel: TLogLevel = TLogLevel.levDebug;

implementation

uses
  LoggerPro.ConsoleAppender,
  LoggerPro.CallbackAppender,
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
  gLevelsMap: array [TLogLevel.levDebug .. TLogLevel.levFatal] of LoggerPro.TLogType = (
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
    ),
    (
      TLogType.Fatal
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

function StrToLogLevel(ALogLevelString: String): TLogLevel;
begin
  ALogLevelString := ALogLevelString.ToLower;
  if ALogLevelString.StartsWith('lev') then
  begin
    ALogLevelString := ALogLevelString.Remove(0, 3);
  end;
  if ALogLevelString.IsEmpty or (ALogLevelString = 'debug') then
    Exit(levDebug);
  if (ALogLevelString = 'info') or (ALogLevelString = 'normal') then
    Exit(levNormal);
  if ALogLevelString = 'warning' then
    Exit(levWarning);
  if ALogLevelString = 'error' then
    Exit(levError);
  if ALogLevelString = 'exception' then
    Exit(levException);
  if ALogLevelString = 'fatal' then
    Exit(levFatal);
  raise EMVCConfigException.Create('Invalid log level: ' + ALogLevelString);
end;

function LogLevelToStr(ALogLevel: TLogLevel): string;
begin
    case ALogLevel of
      levDebug :
        Result := 'DEBUG';
      levNormal:
        Result := ''; // normal is '' because is more readable
      levWarning:
        Result := 'WARNING';
      levError:
        Result := 'ERROR';
      levException:
        Result := 'EXCEPTION';
      levFatal:
        Result := 'FATAL';
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

procedure LogException(const E: Exception);
begin
  Log.LogException(E, '', LOGGERPRO_TAG);
end;

procedure LogException(const E: Exception; const AMessage: String);
begin
  Log.LogException(E, AMessage, LOGGERPRO_TAG);
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
    TLogType.Fatal:
      Log.Fatal(AMessage, LOGGERPRO_TAG);
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

procedure LogD(AMessage: string; const AContext: array of LogParam);
begin
  Log.Debug(AMessage, LOGGERPRO_TAG, AContext);
end;

procedure LogI(AMessage: string; const AContext: array of LogParam);
begin
  Log.Info(AMessage, LOGGERPRO_TAG, AContext);
end;

procedure LogW(AMessage: string; const AContext: array of LogParam);
begin
  Log.Warn(AMessage, LOGGERPRO_TAG, AContext);
end;

procedure LogE(AMessage: string; const AContext: array of LogParam);
begin
  Log.Error(AMessage, LOGGERPRO_TAG, AContext);
end;

procedure LogF(AMessage: string; const AContext: array of LogParam);
begin
  Log.Fatal(AMessage, LOGGERPRO_TAG, AContext);
end;

{ Format String overloads }

procedure LogD(const AMessage: string; const Args: array of const);
begin
  Log.Debug(Format(AMessage, Args), LOGGERPRO_TAG);
end;

procedure LogI(const AMessage: string; const Args: array of const);
begin
  Log.Info(Format(AMessage, Args), LOGGERPRO_TAG);
end;

procedure LogW(const AMessage: string; const Args: array of const);
begin
  Log.Warn(Format(AMessage, Args), LOGGERPRO_TAG);
end;

procedure LogE(const AMessage: string; const Args: array of const);
begin
  Log.Error(Format(AMessage, Args), LOGGERPRO_TAG);
end;

procedure LogF(const AMessage: string; const Args: array of const);
begin
  Log.Fatal(Format(AMessage, Args), LOGGERPRO_TAG);
end;

{ Tag overloads }

procedure LogD(const AMessage: string; const ATag: string);
begin
  Log.Debug(AMessage, ATag);
end;

procedure LogI(const AMessage: string; const ATag: string);
begin
  Log.Info(AMessage, ATag);
end;

procedure LogW(const AMessage: string; const ATag: string);
begin
  Log.Warn(AMessage, ATag);
end;

procedure LogE(const AMessage: string; const ATag: string);
begin
  Log.Error(AMessage, ATag);
end;

procedure LogF(const AMessage: string; const ATag: string);
begin
  Log.Fatal(AMessage, ATag);
end;

{ Format String + Tag overloads }

procedure LogD(const AMessage: string; const Args: array of const; const ATag: string);
begin
  Log.Debug(Format(AMessage, Args), ATag);
end;

procedure LogI(const AMessage: string; const Args: array of const; const ATag: string);
begin
  Log.Info(Format(AMessage, Args), ATag);
end;

procedure LogW(const AMessage: string; const Args: array of const; const ATag: string);
begin
  Log.Warn(Format(AMessage, Args), ATag);
end;

procedure LogE(const AMessage: string; const Args: array of const; const ATag: string);
begin
  Log.Error(Format(AMessage, Args), ATag);
end;

procedure LogF(const AMessage: string; const Args: array of const; const ATag: string);
begin
  Log.Fatal(Format(AMessage, Args), ATag);
end;

{ Message + Object overloads }

procedure LogD(const AMessage: string; AObject: TObject);
begin
  Log.Debug(AMessage + ' ' + ObjectToJSON(AObject), LOGGERPRO_TAG);
end;

procedure LogI(const AMessage: string; AObject: TObject);
begin
  Log.Info(AMessage + ' ' + ObjectToJSON(AObject), LOGGERPRO_TAG);
end;

procedure LogW(const AMessage: string; AObject: TObject);
begin
  Log.Warn(AMessage + ' ' + ObjectToJSON(AObject), LOGGERPRO_TAG);
end;

procedure LogE(const AMessage: string; AObject: TObject);
begin
  Log.Error(AMessage + ' ' + ObjectToJSON(AObject), LOGGERPRO_TAG);
end;

procedure LogF(const AMessage: string; AObject: TObject);
begin
  Log.Fatal(AMessage + ' ' + ObjectToJSON(AObject), LOGGERPRO_TAG);
end;

{ LogE Exception overloads }

procedure LogE(const E: Exception);
begin
  LogException(E);
end;

procedure LogE(const E: Exception; const AMessage: string);
begin
  LogException(E, AMessage);
end;

{ Log level check helpers }

function IsDebugEnabled: Boolean;
begin
  Result := UseLoggerVerbosityLevel <= TLogLevel.levDebug;
end;

function IsInfoEnabled: Boolean;
begin
  Result := UseLoggerVerbosityLevel <= TLogLevel.levNormal;
end;

function IsWarningEnabled: Boolean;
begin
  Result := UseLoggerVerbosityLevel <= TLogLevel.levWarning;
end;

function IsErrorEnabled: Boolean;
begin
  Result := UseLoggerVerbosityLevel <= TLogLevel.levError;
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


function CreateNullLogger: ILogWriter;
begin
  Result := LoggerProBuilder
    .WithMinimumLevel(gLevelsMap[UseLoggerVerbosityLevel])
    .WriteToCallback
      .WithCallback(procedure(const aLogItem: TLogItem; const aFormattedMessage: string) begin end)
      .Done
    .Build;
end;

function CreateLogBuilderWithDefaultConfiguration: ILoggerProBuilder;
var
  lLogsFolder: String;
  lBuilder: ILoggerProBuilder;
begin
{$IF NOT DEFINED(MOBILE)}
  lLogsFolder := AppPath + 'logs';
{$ELSE}
  lLogsFolder := TPath.Combine(TPath.GetDocumentsPath, 'logs');
{$ENDIF}

  lBuilder := LoggerProBuilder
    .WithMinimumLevel(gLevelsMap[UseLoggerVerbosityLevel])
    .WriteToFile
      .WithLogsFolder(lLogsFolder)
      .WithMaxBackupFiles(5)
      .WithMaxFileSizeInKB(10000)
      .Done;

  if IsConsole and UseConsoleLogger then
  begin
    lBuilder.WriteToConsole.WithRenderer(TLogItemRendererNoTag.Create).Done;
  end;

  Result := lBuilder;
end;

function CreateLoggerWithDefaultConfiguration(): ILogWriter;
begin
  Result := CreateLogBuilderWithDefaultConfiguration.Build;
end;

procedure ReleaseGlobalLogger;
begin
    if gDefaultLogger <> nil then
    begin
      TMonitor.Enter(gLock);
      try
        if gDefaultLogger <> nil then // double check
        begin
          gDefaultLogger.Shutdown;  // Flush and stop logger thread
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
  if Profiler.ProfileLogger = nil then
    Exit;
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
  if Profiler.ProfileLogger = nil then
    Exit;
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

finalization

  ReleaseGlobalLogger;
  gLock.Free;

end.
