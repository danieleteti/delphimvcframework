unit LoggerProConfig;

interface

uses
  LoggerPro;

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
    procedure(const AppenderClassName: string; const aFailedLogItem: TLogItem; const Reason: TLogErrorReason; var Action: TLogErrorAction)
  begin
    Action := TLogErrorAction.SkipNewest;
  end;

DefaultLoggerProAppenderQueueSize := 100;
{$IF Defined(MSWINDOWS)}
_RESTAppender := TLoggerProRESTAppender.Create;
{$ENDIF}
{$IF Defined(Android)}
_RESTAppender := TLoggerProRESTAppender.Create('http://192.168.1.6:8080/api/logs');
{$ENDIF}
TLoggerProRESTAppender(_RESTAppender).OnNetSendError :=
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
