unit LoggerProConfig;

interface

uses
  LoggerPro,
  LoggerPro.Proxy;

function Log: ILogWriter;

implementation

uses
  LoggerPro.FileAppender;

var
  _Log: ILogWriter;

function Log: ILogWriter;
begin
  Result := _Log;
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

// Creates logs in the ..\..\ folder without PID in the filename
// The FilteringFileAppender selects the 'TAG1' and 'TAG2' log messages into a separate file
_Log := BuildLogWriter([
  TLoggerProFileAppender.Create(10, 5, '..\..', [],
    TLoggerProFileAppender.DEFAULT_FILENAME_FORMAT, DEFAULT_LOG_FORMAT),

  TLoggerProFilter.Build(
    TLoggerProSimpleFileAppender.Create(10, 5, '..\..'),
    function(ALogItem: TLogItem): boolean
    begin
      Result := (ALogItem.LogTag = 'TAG1') or (ALogItem.LogTag = 'TAG2');
    end)
  ]);
// Create logs in the exe' same folder
// _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5)]);

// Creates log in the AppData/Roaming with PID in the filename
// _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
// [TFileAppenderOption.IncludePID])]);

// Creates log in the same folder with PID in the filename
// _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
// [TFileAppenderOption.IncludePID])]);

end.
