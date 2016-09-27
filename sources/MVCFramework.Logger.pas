// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2016 Daniele Teti and the DMVCFramework Team
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

interface

uses
  LoggerPro,
  System.SysUtils;

const
  LOGGERPRO_TAG = 'dmvcframework';

type
  TLogLevel = (levNormal = 1, levWar = 2, levError = 3, levException = 4);

function LogLevelAsString(ALogLevel: TLogLevel): string;
procedure Log(AMessage: string); overload; deprecated 'Use Log.Info';
procedure LogW(AMessage: string); deprecated 'Use Log.Warn';
procedure LogE(AMessage: string); deprecated 'Use Log.Error';
procedure LogEx(AException: Exception; AMessage: string = ''); deprecated 'Use Log.Error';
procedure Log(LogLevel: TLogLevel; const AMessage: string); overload;
  deprecated 'Use Log.Info, Log.Debug, Log.Warn or Log.Error';
procedure LogEnterMethod(const AMethodName: string);
procedure LogExitMethod(const AMethodName: string);
procedure LogException(AException: Exception; AMessage: string = '');
  deprecated 'Use Log.Error';

// direct access to loggerpro logger
function Log: ILogWriter; overload;

procedure SetDefaultLogger(const aLogWriter: ILogWriter);
procedure InitializeDefaultLogger;

var
  LogLevelLimit: TLogLevel = TLogLevel.levNormal;

implementation

uses
  System.Classes, LoggerPro.FileAppender;

var
  _lock: TObject;
  _DefaultLogger: ILogWriter;
  _LevelsMap: array [TLogLevel.levNormal .. TLogLevel.levException] of LoggerPro.TLogType =
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
    )
  );

function Log: ILogWriter;
begin
  Result := _DefaultLogger;
end;

function LogLevelAsString(ALogLevel: TLogLevel): string;
begin
  case ALogLevel of
    levNormal:
      Result := ''; // normal is '' because is more readable
    levWar:
      Result := 'WARNING';
    levError:
      Result := 'ERROR';
    levException:
      Result := 'EXCEPTION';
  else
    Result := 'UNKNOWN';
  end;
end;

procedure LogEx(AException: Exception; AMessage: string = '');
begin
  Log(TLogLevel.levException, Format('[%s] %s (Custom message: "%s")', [AException.ClassName,
    AException.Message, AMessage]));
end;

procedure LogW(AMessage: string);
begin
  Log(TLogLevel.levWar, AMessage);
end;

procedure LogE(AMessage: string);
begin
  Log(TLogLevel.levError, AMessage);
end;

procedure LogException(
  AException: Exception;
  AMessage: string);
begin
  LogEx(AException, AMessage);
end;

procedure LogEnterMethod(const AMethodName: string);
begin
  Log.Info('>> ' + AMethodName, LOGGERPRO_TAG);
  // Log(TLogLevel.levNormal, '>> ' + AMethodName);
end;

procedure LogExitMethod(const AMethodName: string);
begin
  Log.Info('<< ' + AMethodName, LOGGERPRO_TAG);
  // Log(TLogLevel.levNormal, '<< ' + AMethodName);
end;

procedure Log(LogLevel: TLogLevel; const AMessage: string);
// var
// Msg: string;
begin
  // if LogLevel < LogLevelLimit then
  // Exit;

  // Msg := Format('[%10s %5.5d] %s', [
  // LogLevelAsString(LogLevel),
  // TThread.CurrentThread.ThreadID,
  // AMessage]);

  case _LevelsMap[LogLevel] of
    TLogType.Debug:
      _DefaultLogger.Debug(AMessage, LOGGERPRO_TAG);
    TLogType.Info:
      _DefaultLogger.Info(AMessage, LOGGERPRO_TAG);
    TLogType.Warning:
      _DefaultLogger.Warn(AMessage, LOGGERPRO_TAG);
    TLogType.Error:
      _DefaultLogger.Error(AMessage, LOGGERPRO_TAG);
  else
    raise Exception.Create('Invalid LOG LEVEL! Original message was: ' + AMessage);
  end;

end;

procedure Log(AMessage: string); overload;
begin
  Log.Info(AMessage, LOGGERPRO_TAG);
end;

procedure SetDefaultLogger(const aLogWriter: ILogWriter);
begin
  if _DefaultLogger <> nil then
    Exit;
  TMonitor.Enter(_lock); // double check here
  try
    if not Assigned(_DefaultLogger) then
    begin
      if Assigned(aLogWriter) then
      begin
        _DefaultLogger := aLogWriter;
        Log.Info('Custom Logger initialized', LOGGERPRO_TAG);
      end
      else
      begin
        _DefaultLogger := BuildLogWriter([TLoggerProFileAppender.Create(5, 2000, 'logs')]);
        Log.Info('Default Logger initialized', LOGGERPRO_TAG);
      end;
    end;
  finally
    TMonitor.Exit(_lock);
  end;
end;

procedure InitializeDefaultLogger;
begin
  TMonitor.Enter(_lock);
  try
    if not Assigned(_DefaultLogger) then
    begin
      _DefaultLogger := BuildLogWriter([TLoggerProFileAppender.Create(10, 5)]);
      Log.Info('Default Logger initialized', LOGGERPRO_TAG);
    end;
  finally
    TMonitor.Exit(_lock);
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
