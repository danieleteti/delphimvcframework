unit LoggerProConfig;

interface

uses
  LoggerPro;

function Log: ILogWriter;

implementation

uses
  WinAPI.Windows, System.SysUtils,
  LoggerPro.FileAppender;

var
  _Log: ILogWriter;

function Log: ILogWriter;
begin
  Result := _Log;
end;

function GetFileNameFormat: string;
var
  lLogFileNameFormat: string;
begin
  lLogFileNameFormat := TLoggerProFileAppender.DEFAULT_FILENAME_FORMAT;
  // '%s.%2.2d.%s.log';

  var lClientName: string := GetEnvironmentVariable('CLIENTNAME');
  var lComputerName: string := GetEnvironmentVariable('COMPUTERNAME');
  if not lClientName.IsEmpty then
  begin
    Exit('LOG_' + lClientName + '_' + lLogFileNameFormat);
  end;
  if not lComputerName.IsEmpty then
  begin
    Exit('LOG_' + lComputerName + '_' + lLogFileNameFormat);
  end;
  Result := 'LOG_' + GetProcessId(HInstance).ToString + '_' + lLogFileNameFormat;
end;

initialization

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
_Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5, '.\logs', [], GetFileNameFormat())]);
// Create logs in the exe' same folder
// _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5)]);

// Creates log in the AppData/Roaming with PID in the filename
// _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
// [TFileAppenderOption.IncludePID])]);

// Creates log in the same folder with PID in the filename
// _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
// [TFileAppenderOption.IncludePID])]);

end.
