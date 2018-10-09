unit CustomLoggerConfigU;

interface

uses
  LoggerPro; // loggerpro core

function GetLogger: ILogWriter;

implementation

uses
  System.IOUtils
    , LoggerPro.FileAppender // loggerpro file appender (logs to file)

  {$IFDEF MSWINDOWS} , LoggerPro.OutputdebugStringAppender {$ENDIF} // loggerpro outputdebugstring appender (logs to the debugger)
    ;

function GetLogger: ILogWriter;
begin
  Result := BuildLogWriter([
    TLoggerProFileAppender.Create(10, 1000, TPath.Combine('MyFolder', 'MyLogs'))

    {$IFDEF MSWINDOWS}, TLoggerProOutputDebugStringAppender.Create{$ENDIF}

    ], nil, TLogType.Debug);
end;

end.
