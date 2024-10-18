unit LoggerProConfig;

interface

uses
  LoggerPro,
  LoggerPro.Proxy;

function Log: ILogWriter;

implementation

uses
  LoggerPro.FileAppender, LoggerPro.JSONLFileAppender, System.SysUtils;

var
  _Log: ILogWriter;

function Log: ILogWriter;
begin
  Result := _Log;
end;

initialization

//  The TLoggerProFileAppender has its defaults defined as follows:
//  TLoggerProJSONLFileAppender = '{module}.{number}.{tag}.log';
//  DEFAULT_MAX_BACKUP_FILE_COUNT = 5;
//  DEFAULT_MAX_FILE_SIZE_KB = 1000;

// Creates logs in the ..\logs folder without PID in the filename
_Log := BuildLogWriter([
  TLoggerProJSONLFileAppender.Create(10, 5, '..\logs',
    TLoggerProJSONLFileAppender.DEFAULT_FILENAME_FORMAT,
    TEncoding.UTF8
    )
  ]);

end.
