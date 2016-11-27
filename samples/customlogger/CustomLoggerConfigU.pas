unit CustomLoggerConfigU;

interface

uses
  LoggerPro; // loggerpro core

function GetLogger: ILogWriter;

implementation

uses
  LoggerPro.FileAppender, // loggerpro file appender (logs to file)
  LoggerPro.ConsoleAppender, // loggerpro console appender (logs to the console)
  LoggerPro.OutputdebugStringAppender; // loggerpro outputdebugstring appender (logs to the debugger)

function GetLogger: ILogWriter;
begin
  Result := BuildLogWriter([
    TLoggerProFileAppender.Create(10, 1000, 'MyFolder\MyLogs'),
    TLoggerProOutputDebugStringAppender.Create
    ], nil, TLogType.Debug);
end;

end.
