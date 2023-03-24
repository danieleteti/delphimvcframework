unit LoggerProConfig;

interface

uses
  LoggerPro;

function Log: ILogWriter;

implementation

uses
  LoggerPro.FileAppender,
  System.Net.HttpClient,
  System.SysUtils,
  LoggerPro.ElasticSearchAppender;

var
  _Log: ILogWriter;
  _Events: TLoggerProEventsHandler;
  _RESTAppender: ILogAppender;

function Log: ILogWriter;
begin
  Result := _Log;
end;

initialization

_Events := TLoggerProEventsHandler.Create;
_Events.OnAppenderError :=
    procedure(const AppenderClassName: string; const aFailedLogItem: TLogItem; const Reason: TLogErrorReason; var Action: TLogErrorAction)
  begin
    Action := TLogErrorAction.SkipNewest;
  end;

DefaultLoggerProAppenderQueueSize := 100;
{$IF Defined(MSWINDOWS)}
_RESTAppender := TLoggerProElasticSearchAppender.Create('http://localhost', 9200, 'loggerpro');
{$ENDIF}
{$IF Defined(Android)}
_RESTAppender := TLoggerProElasticSearchAppender.Create('http://192.168.1.6:8080/api/logs');
{$ENDIF}
TLoggerProElasticSearchAppender(_RESTAppender).OnNetSendError :=
    procedure(const Sender: TObject; const LogItem: TLogItem; const NetError: Exception; var RetryCount: Integer)
  begin
    // retries to send log for 5 times, then discard the logitem
    if RetryCount = 5 then
    begin
      RetryCount := 0
    end
    else
    begin
      Inc(RetryCount);
    end;
  end;

_Log := BuildLogWriter([_RESTAppender, TLoggerProFileAppender.Create], _Events);

finalization

_Log := nil;
_Events.Free;

end.
