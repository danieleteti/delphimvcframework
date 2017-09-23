// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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
  LoggerPro,
  LoggerPro.FileAppender;

const
  LOGGERPRO_TAG = 'dmvcframework';

type
  TLogLevel = (levDebug = 0, levNormal = 1, levWarning = 2, levError = 3, levException = 4);

function LogLevelAsString(ALogLevel: TLogLevel): string;
procedure Log(AMessage: string); overload;
procedure LogD(AMessage: string); overload;
procedure LogI(AMessage: string);
procedure LogW(AMessage: string);
procedure LogE(AMessage: string);
procedure Log(LogLevel: TLogLevel; const AMessage: string); overload;
procedure LogEnterMethod(const AMethodName: string);
procedure LogExitMethod(const AMethodName: string);

// direct access to loggerpro logger
function Log: ILogWriter; overload;

procedure SetDefaultLogger(const aLogWriter: ILogWriter);
procedure InitializeDefaultLogger;
{ @abstract(Use only inside DLL because dll unloading is not a safe place to shutdown threads, so call this before unload DLL)
  Use this also in ISAPI dll. Check the @code(loggerproisapisample.dll) sample
}
procedure ReleaseGlobalLogger;

var
  LogLevelLimit: TLogLevel = TLogLevel.levNormal;

implementation

var
  _lock: TObject;
  _DefaultLogger: ILogWriter;
  _LevelsMap: array [TLogLevel.levDebug .. TLogLevel.levException] of LoggerPro.TLogType =
    (
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
  if _DefaultLogger = nil then
  begin
    SetDefaultLogger(nil);
  end;

  Result := _DefaultLogger;
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

procedure LogW(AMessage: string);
begin
  Log.Warn(AMessage, LOGGERPRO_TAG);
end;

procedure LogE(AMessage: string);
begin
  Log.Error(AMessage, LOGGERPRO_TAG);
end;

// procedure LogException(
// const AException: Exception;
// const AMessage: string);
// begin
// Log.Error(Format('[%s] %s (Custom message: "%s")', [AException.ClassName,
// AException.Message, AMessage]), LOGGERPRO_TAG);
// end;

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
  case _LevelsMap[LogLevel] of
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

procedure LogI(AMessage: string); overload;
begin
  Log.Info(AMessage, LOGGERPRO_TAG);
end;

procedure LogD(AMessage: string); overload;
begin
  Log.Debug(AMessage, LOGGERPRO_TAG);
end;

procedure SetDefaultLogger(const aLogWriter: ILogWriter);
begin
  if _DefaultLogger = nil then
  begin
    TMonitor.Enter(_lock); // double check here
    try
      if _DefaultLogger = nil then
      begin
        if aLogWriter <> nil then
        begin
          _DefaultLogger := aLogWriter;
          Log.Info('Custom Logger initialized', LOGGERPRO_TAG);
        end
        else
        begin
          InitializeDefaultLogger;
          Log.Info('Default Logger initialized', LOGGERPRO_TAG);
        end;
      end;
    finally
      TMonitor.Exit(_lock);
    end;
  end;
end;

procedure InitializeDefaultLogger;
begin
  { This procedure must be called in a synchronized context
    (Normally only SetDefaultLogger should be the caller) }
  if not Assigned(_DefaultLogger) then
  begin
    _DefaultLogger := BuildLogWriter([TLoggerProFileAppender.Create(5, 2000, AppPath + 'logs')]);
  end;
end;

procedure ReleaseGlobalLogger;
begin
  if _DefaultLogger <> nil then
  begin
    TMonitor.Enter(_lock);
    try
      if _DefaultLogger <> nil then // double check
      begin
        _DefaultLogger := nil;
      end;
    finally
      TMonitor.Exit(_lock);
    end;
  end;
end;

initialization

_lock := TObject.Create;

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

_lock.Free;

end.
