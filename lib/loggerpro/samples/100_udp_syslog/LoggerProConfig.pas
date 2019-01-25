unit LoggerProConfig;

interface

uses
  LoggerPro, LoggerPro.UDPSyslogAppender;

var
  Log: ILogWriter;
  Appender: TLoggerProUDPSyslogAppender;

implementation

initialization

Appender := TLoggerProUDPSyslogAppender.Create(
    '127.0.0.1'
    , 5114 //UDPClientPort.Value
    , 'COMPUTER'
    , 'USER'
    , 'EXE'
    , '0.0.1'
    , ''
    , True
    , False
  );

Log := BuildLogWriter([Appender]);

end.
