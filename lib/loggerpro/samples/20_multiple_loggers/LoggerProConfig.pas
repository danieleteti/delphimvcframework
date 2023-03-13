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
begin
  _Log := BuildLogWriter([TLoggerProFileAppender.Create,
    TLoggerProConsoleAppender.Create,
    TLoggerProOutputDebugStringAppender.Create]);
  // only errors on console
  _Log.Appenders[1].SetLogLevel(TLogType.Error);
  // only warnings or errors on outputdebugstring
  _Log.Appenders[2].SetLogLevel(TLogType.Warning);
end;

initialization

SetupLogger;

end.
