unit LoggerProConfig;

interface

uses
  LoggerPro;

function Log: ILogWriter;

implementation

uses
  LoggerPro.FileAppender,
  LoggerPro.ConsoleAppender,
  LoggerPro.OutputDebugStringAppender;

var
  _Log: ILogWriter;

function Log: ILogWriter;
begin
  Result := _Log;
end;

procedure SetupLogger;
var
  lFileAppender, lErrorsFileAppender, lOutputDebugStringAppender: ILogAppender;
begin
  lFileAppender := TLoggerProFileAppender.Create(5, 1000, 'logs');
  lFileAppender.SetLogLevel(TLogType.Info);

  lErrorsFileAppender := TLoggerProFileAppender.Create(5, 1000, 'logs_errors');
  lErrorsFileAppender.SetLogLevel(TLogType.Error);

  lOutputDebugStringAppender := TLoggerProOutputDebugStringAppender.Create;
  // default TLogType.Debug

  _Log := BuildLogWriter([lFileAppender, lErrorsFileAppender, lOutputDebugStringAppender]);
end;

initialization

SetupLogger;

end.
