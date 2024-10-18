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

// Create up to 10 logs in the exe\logs folder, max 2MiB each, using DEFAULT_FILENAME_FORMAT = '{module}.{number}.log';

_Log := BuildLogWriter([
    TLoggerProSimpleFileAppender.Create(10, 2048, 'logs')
  ]);

end.
