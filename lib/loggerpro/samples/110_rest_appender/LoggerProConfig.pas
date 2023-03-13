unit LoggerProConfig;

interface

uses
  LoggerPro;

const
{$IF Defined(MSWINDOWS)}
  REST_LOGS_COLLECTOR_URL = 'http://localhost:8080';
{$ENDIF}
{$IF Defined(Android)}
  REST_LOGS_COLLECTOR_URL = 'http://192.168.1.153:8080';
{$ENDIF}

function Log: ILogWriter;

implementation

uses
  LoggerPro.RESTAppender,
  LoggerPro.FileAppender,
  System.Net.HttpClient,
  System.SysUtils;

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
    procedure(const AppenderClassName: string; const aFailedLogItem: TLogItem;
    const Reason: TLogErrorReason; var Action: TLogErrorAction)
  begin
    Action := TLogErrorAction.DiscardOlder;
  end;

DefaultLoggerProAppenderQueueSize := 10;
_RESTAppender := TLoggerProRESTAppender.Create(REST_LOGS_COLLECTOR_URL + '/api/logs');
TLoggerProRESTAppender(_RESTAppender).OnNetSendError :=
    procedure(const Sender: TObject; const LogItem: TLogItem; const NetError: Exception;
    var RetryCount: Integer)
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
